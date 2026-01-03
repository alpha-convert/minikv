open! Core
open! Core_unix

module SplitResult = struct
  type t = NoSplit | Split of (int * Pageno.t)
end

module Header = struct
  type t =
    | Internal
    | Leaf

  let to_int (h : t) : int = Obj.magic h
  let of_int (i : int) : t = Obj.magic i

  let classify (page @ local) @ local = exclave_
    let buf = Page.underlying_read_only page in
    let header_byte = Off_heap_buffer.unsafe_get_int8 buf ~pos:0 in
    match of_int header_byte with
    | Internal -> Either.First page
    | Leaf -> Either.Second page

  let as_leaf (page @ local) @ local = exclave_ page
  let as_internal (page @ local) @ local = exclave_ page
end


module Internal = struct
  (* Internal node layout:
     - header: 8-bit node type
     - num_keys: 16-bit little endian
     - data: pageno, key, pageno, key, ..., pageno
       where pageno and key are both 64-bit little endian
     - keys are sorted in increasing order *)
  type t = Page.t

  let tbl_start = 3  (* 1 byte header + 2 bytes num_keys *)

  (* Max keys: for n keys we need n+1 children (8 bytes each) + n keys (8 bytes each)
     = 8(n+1) + 8n = 16n + 8 bytes
     So: 16n + 8 <= page_size - 3
     => n <= (page_size - 3 - 8) / 16 *)
  let max_keys = (Page.page_size - tbl_start - 8) / 16

  let num_keys t =
    let buf = Page.underlying_read_only t in
    (Off_heap_buffer.unsafe_get_int16_le buf ~pos:1 [@nontail])

  let set_num_keys t n =
    let buf = Page.underlying t in
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:1 n [@nontail])

  let is_full t =
    num_keys t >= max_keys

  let get_key t i =
    let buf = Page.underlying_read_only t in
    let pos = tbl_start + (i * 16) + 8 in
    (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos [@nontail])

  let get_child t i =
    let buf = Page.underlying_read_only t in
    let pos = tbl_start + (i * 16) in
    Pageno.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos)

  let set_key t i key =
    let buf = Page.underlying t in
    let pos = tbl_start + (i * 16) + 8 in
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos key [@nontail])

  let set_child t i pageno =
    let buf = Page.underlying t in
    let pos = tbl_start + (i * 16) in
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos (Pageno.to_int pageno) [@nontail])

  let lookup_key (t @ local) k =
    let buf = Page.underlying_read_only t in
    let n = num_keys t in
    let rec search lo hi =
      if lo >= hi then
        let pos = tbl_start + (lo * 16) in
        Pageno.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos)
      else
        let mid = (lo + hi) / 2 in
        let key_pos = tbl_start + (mid * 16) + 8 in
        let mid_key = Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:key_pos in
        if k < mid_key then
          search lo mid
        else
          search (mid + 1) hi
    in
    (search 0 n [@nontail])

  let init page =
    let buf = Page.underlying page in
    (Off_heap_buffer.unsafe_set_int8_exn buf ~pos:0 (Header.to_int Header.Internal) [@nontail]);
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:1 0 [@nontail])   (* num_keys = 0 *)

  (* Insert a key and right child into internal node, assumes not full *)
  let insert_with_space t ~key ~right_child =
    assert (num_keys t < max_keys);
    let n = num_keys t in

    (* Find position where key should go *)
    let rec find_pos i =
      if i >= n then i
      else if key < get_key t i then i
      else find_pos (i + 1)
    in
    let pos = find_pos 0 in

    (* Shift keys and children to make room *)
    for i = n - 1 downto pos do
      set_key t (i + 1) (get_key t i);
      set_child t (i + 2) (get_child t (i + 1))
    done;

    (* Insert new key and right child *)
    set_key t pos key;
    set_child t (pos + 1) right_child;
    set_num_keys t (n + 1)

  (* Split a full internal node *)
  let split t ~key ~right_child page_cache allocator =
    assert (num_keys t = max_keys);

    (* Build in-memory array of all keys and children *)
    let entries = Array.init (max_keys + 1) (fun i ->
      if i < max_keys then (get_key t i, get_child t (i + 1))
      else (key, right_child)
    ) in

    (* Sort by key *)
    Array.sort ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2) entries;

    (* Middle key gets promoted *)
    let mid = (max_keys + 1) / 2 in
    let (promoted_key, mid_right_child) = entries.(mid) in

    (* Write left half back to original node *)
    for i = 0 to mid - 1 do
      let (k, child) = entries.(i) in
      set_key t i k;
      set_child t (i + 1) child
    done;
    set_num_keys t mid;

    (* Create new right node and write right half *)
    let right_pageno = Page_allocator.allocate_page allocator in
    Page_cache.with_page page_cache right_pageno (fun right_page ->
      init right_page;
      (* First child of right node is the right child of the promoted key *)
      set_child right_page 0 mid_right_child;
      for i = mid + 1 to max_keys do
        let (k, child) = entries.(i) in
        set_key right_page (i - mid - 1) k;
        set_child right_page (i - mid) child
      done;
      set_num_keys right_page (max_keys - mid)
    );

    (promoted_key, right_pageno)

  let insert t ~key ~right_child page_cache allocator =
    if not (is_full t) then begin
      insert_with_space t ~key ~right_child;
      SplitResult.NoSplit
    end else
      SplitResult.Split (split t ~key ~right_child page_cache allocator)

end

module Leaf = struct
  (* Leaf node layout:
     - header: 8-bit node type
     - num_keys: 16-bit little endian
     - entries: (key, pageno) pairs (each 16 bytes: 8-byte key + 8-byte pageno)
       stored sequentially, sorted *)
  type t = Page.t

  let entries_start = 3  (* 1 byte header + 2 bytes num_keys *)

  (* Max keys = (page_size - 3) / 16 *)
  let max_keys = (Page.page_size - entries_start) / 16
  let degree = max_keys / 2

  let num_keys t =
    let buf = Page.underlying_read_only t in
    (Off_heap_buffer.unsafe_get_int16_le buf ~pos:1 [@nontail])

  let set_num_keys t n =
    let buf = Page.underlying t in
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:1 n [@nontail])

  let is_full t =
    num_keys t >= max_keys

  let get_entry t i =
    let buf = Page.underlying_read_only t in
    let pos = entries_start + (i * 16) in
    let key = Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos in
    let pageno = Pageno.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:(pos + 8)) in
    (key, pageno)

  let set_entry t i ~key ~pointer =
    let buf = Page.underlying t in
    let pos = entries_start + (i * 16) in
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos key [@nontail]);
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:(pos + 8) (Pageno.to_int pointer) [@nontail])
   
  let init page =
    let buf = Page.underlying page in
    (Off_heap_buffer.unsafe_set_int8_exn buf ~pos:0 (Header.to_int Header.Leaf) [@nontail]);
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:1 0 [@nontail])

  (* Binary search to find the position where key should be inserted.
     Returns the index where key is found or should be inserted. *)
  let find_key_pos (t @ local) k =
    let n = num_keys t in
    let rec search lo hi =
      if lo >= hi then
        if lo >= n then
          (assert (lo == n); `Not_found n)
        else
          let (k', pageno) = get_entry t lo in
          if Int.equal k k' then `Found_exact (~idx:lo,pageno) else `Found_other (~idx:lo,k',pageno)
      else
        let mid = (lo + hi) / 2 in
        let (mid_key, _) = get_entry t mid in
        if k <= mid_key then
          search lo mid
        else
          search (mid + 1) hi
    in
    (search 0 n [@nontail])

  let lookup_key (t @ local) k =
    match find_key_pos t k with
    | `Not_found _ -> None
    | `Found_exact (~idx:_,pageno) -> Some pageno
    | `Found_other _ -> None

  (* Insert key-value pair into leaf at given position, assumes not full and key doesn't exist *)
  let insert_with_space t ~idx ~key ~value =
    let n = num_keys t in
    assert (n < max_keys);
    (* Shift entries to make room *)
    for i = n - 1 downto idx do
      let (k, p) = get_entry t i in
      set_entry t (i + 1) ~key:k ~pointer:p
    done;
    (* Insert at the correct position *)
    set_entry t idx ~key ~pointer:value;
    set_num_keys t (n + 1)

  (* Split a full leaf page. The page must have max_keys many keys. *)
  let split t ~key ~value page_cache allocator =
    assert (num_keys t = max_keys);

    (* Build in-memory array of all entries *)
    let entries = Array.init (max_keys + 1) (fun i ->
      if i < max_keys then get_entry t i
      else (key, value)
    ) in

    (* Sort the array *)
    Array.sort ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2) entries;

    (* Find the middle element *)
    let mid = (max_keys + 1) / 2 in
    let (pivot_key, _) = entries.(mid) in

    (* Write left half back to original page *)
    for i = 0 to mid - 1 do
      let (k, v) = entries.(i) in
      set_entry t i ~key:k ~pointer:v
    done;
    set_num_keys t mid;

    (* Create new page and write right half *)
    let right_pageno = Page_allocator.allocate_page allocator in
    Page_cache.with_page page_cache right_pageno (fun right_page ->
      init right_page;
      for i = mid to max_keys do
        let (k, v) = entries.(i) in
        set_entry right_page (i - mid) ~key:k ~pointer:v
      done;
      set_num_keys right_page (max_keys + 1 - mid)
    );

    (pivot_key, right_pageno)

  let insert (t @ local) ~key ~value page_cache allocator =
    match find_key_pos t key with
    | `Found_exact (~idx,pageno:_) ->
        set_entry t idx ~key ~pointer:value;
        SplitResult.NoSplit
    | `Not_found idx | `Found_other (~idx,_,_) ->
        if not (is_full t) then begin
          insert_with_space t ~idx ~key ~value;
          SplitResult.NoSplit
        end else
          SplitResult.Split (split t ~key ~value page_cache allocator)
end

(* A Bplustree.t is just the page number of its root *)
type t = {
  mutable root : Pageno.t;
  cache : Page_cache.t;
  allocator : Page_allocator.t
}

let root_pageno t = t.root

let load cache allocator root =
  {root;cache;allocator}

let create cache allocator root =
  (* Create initial leaf child *)
  let leaf_pageno = Page_allocator.allocate_page allocator in
  Page_cache.with_page cache leaf_pageno (fun leaf ->
    Leaf.init leaf
  );
  (* Initialize root as internal with one child *)
  Page_cache.with_page cache root (fun page ->
    Internal.init page;
    Internal.set_child page 0 leaf_pageno
  );
  {root;cache;allocator}

let lookup_leaf_page root cache key =
  let rec loop pageno =
    let next =
      Page_cache.with_page cache pageno (fun page ->
        match Header.classify page with
        | Either.First node ->
            let next_pageno = Internal.lookup_key node key in
            `Continue next_pageno
        | Either.Second _ ->
            `Done pageno
      )
    in
    match next with
    | `Continue next_pageno -> loop next_pageno
    | `Done result -> result
  in
  loop root

let lookup t key =
  let {root;cache;allocator} = t in
  let leaf_pageno = lookup_leaf_page root cache key in
  Page_cache.with_page cache leaf_pageno (fun page ->
    let leaf = Header.as_leaf page in
    Leaf.lookup_key leaf key [@nontail])

let insert t ~key ~value =
  let {root;cache;allocator} = t in
  let rec insert_into_node pageno =
    (* First, determine if this is an internal or leaf node and get child pageno if internal *)
    let next_step =
      Page_cache.with_page cache pageno (fun page ->
        match Header.classify page with
        | Either.First internal_node ->
            let child_pageno = Internal.lookup_key internal_node key in
            `Internal child_pageno
        | Either.Second _ ->
            `Leaf (Leaf.insert (Header.as_leaf page) ~key ~value cache allocator [@nontail])
      )
    in
    match next_step with
    | `Leaf res -> res
    | `Internal child_pageno ->
        (match insert_into_node child_pageno with
        | NoSplit -> NoSplit
        | Split (pivot, right_child) ->
            Page_cache.with_page cache pageno (fun page ->
              Internal.insert (Header.as_internal page) ~key:pivot ~right_child cache allocator [@nontail]
            ))
  in

  let root =
    match insert_into_node root with
    | NoSplit -> root
    | Split (pivot, right_pageno) ->
        (* Root split: create new root with two children *)
        let new_root_pageno = Page_allocator.allocate_page allocator in
        Page_cache.with_page cache new_root_pageno (fun new_root ->
          Internal.init new_root;
          Internal.set_child new_root 0 root;
          Internal.set_key new_root 0 pivot;
          Internal.set_child new_root 1 right_pageno;
          Internal.set_num_keys new_root 1
        );
        new_root_pageno
  in
  t.root <- root
