open! Core
open! Core_unix

module SplitResult = struct
  type t = NoSplit | Split of (int * Pageno.t)
end

module rec Header : sig
  type nodety = Internal | Leaf
  val nodety_to_int : nodety -> int
  val classify : Page.t @ local -> (Internal.t,Leaf.t) Either.t @ local
  val get_parent : Page.t @ local -> Pageno.t option
  val set_parent : Page.t @ local -> Pageno.t -> unit

  (* Layout constants *)
  val nodety_offset : int
  val parent_offset : int
  val num_keys_offset : int
  val null_parent : int
end
  = struct
  type nodety =
    | Internal
    | Leaf

  let nodety_to_int (h : nodety) : int = Obj.magic h
  let nodety_of_int (i : int) : nodety = Obj.magic i

  (* Layout offsets *)
  let nodety_offset = 0
  let parent_offset = 1
  let num_keys_offset = 9
  let null_parent = -1

  let get_parent pg =
    let buf = Page.underlying_read_only pg in
    let parent = Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:parent_offset in
    if Int.equal parent null_parent then None
    else Some (Pageno.of_int parent)

  let set_parent pg pageno =
    let buf = Page.underlying pg in
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:parent_offset (Pageno.to_int pageno) [@nontail]
    
  let as_leaf (page : Page.t @ local) : Leaf.t @ local = exclave_ (Obj.magic page)
  let as_internal (page : Page.t @ local) : Internal.t @ local = exclave_ (Obj.magic page)

  let classify (page @ local) : (Internal.t,Leaf.t) Either.t @ local = exclave_
    let buf = Page.underlying_read_only page in
    let header_byte = Off_heap_buffer.unsafe_get_int8 buf ~pos:nodety_offset in
    match nodety_of_int header_byte with
    | Internal -> Either.First (as_internal page)
    | Leaf -> Either.Second (as_leaf page)
  
end

and Internal : sig
  type t
  val init : Page.t @ local -> parent:Pageno.t option -> t @ local
  val set_child : t @ local -> int -> Pageno.t -> unit
  val set_key : t @ local -> int -> int -> unit
  val set_num_keys : t @ local -> int -> unit
  val lookup_key : t @ local -> int -> Pageno.t
  val insert : t @ local -> key:int -> right_child:Pageno.t -> Page_cache.t -> Page_allocator.t -> SplitResult.t
end = struct
  (* Internal node layout:
     - header: 8-bit node type, 64 bit parent pointer
     - num_keys: 16-bit little endian
     - data: pageno, key, pageno, key, ..., pageno
       where pageno and key are both 64-bit little endian
     - keys are sorted in increasing order *)
  type t = Page.t

  let tbl_start = 11  (* 1 byte header + 8 bytes parent + 2 bytes num_keys *)

  let entry_size = 16
  let child_size = 8
  let key_size = 8

  let max_keys = (Page.page_size - tbl_start - child_size) / entry_size

  let num_keys t =
    let buf = Page.underlying_read_only t in
    (Off_heap_buffer.unsafe_get_int16_le buf ~pos:Header.num_keys_offset [@nontail])

  let set_num_keys t n =
    let buf = Page.underlying t in
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:Header.num_keys_offset n [@nontail])

  let is_full t =
    num_keys t >= max_keys

  let get_key t i =
    let buf = Page.underlying_read_only t in
    let pos = tbl_start + (i * entry_size) + child_size in
    (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos [@nontail])

  let get_child t i =
    let buf = Page.underlying_read_only t in
    let pos = tbl_start + (i * entry_size) in
    Pageno.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos)

  let set_key t i key =
    let buf = Page.underlying t in
    let pos = tbl_start + (i * entry_size) + child_size in
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos key [@nontail])

  let set_child (t @ local) i pageno =
    let buf = Page.underlying t in
    let pos = tbl_start + (i * entry_size) in
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos (Pageno.to_int pageno) [@nontail]

  let lookup_key (t @ local) k =
    let buf = Page.underlying_read_only t in
    let n = num_keys t in
    let rec search lo hi =
      if lo >= hi then
        let pos = tbl_start + (lo * entry_size) in
        Pageno.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos)
      else
        let mid = (lo + hi) / 2 in
        let key_pos = tbl_start + (mid * entry_size) + child_size in
        let mid_key = Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:key_pos in
        if k < mid_key then
          search lo mid
        else
          search (mid + 1) hi
    in
    (search 0 n [@nontail])

  let init page ~parent =
    let buf = Page.underlying page in
    (Off_heap_buffer.unsafe_set_int8_exn buf ~pos:Header.nodety_offset (Header.nodety_to_int Header.Internal));
    let parent_int = match parent with
      | None -> Header.null_parent
      | Some p -> Pageno.to_int p
    in
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:Header.parent_offset parent_int);
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:Header.num_keys_offset 0);
    exclave_ page

  (* Insert a key and right child into internal node, assumes not full *)
  let insert_with_space t ~key ~right_child =
    assert (num_keys t < max_keys);
    let n = num_keys t in

    let rec find_pos i =
      if i >= n then i
      else if key < get_key t i then i
      else find_pos (i + 1)
    in
    let pos = find_pos 0 in

    for i = n - 1 downto pos do
      set_key t (i + 1) (get_key t i);
      set_child t (i + 2) (get_child t (i + 1))
    done;

    set_key t pos key;
    set_child t (pos + 1) right_child;
    set_num_keys t (n + 1)

  (* Split a full internal node *)
  let split t ~key ~right_child page_cache allocator =
    assert (num_keys t = max_keys);

    (* Build in-memory array of all keys and children *)
    let entries = Array.init (max_keys + 1) ~f:(fun i ->
      if i < max_keys then (get_key t i, get_child t (i + 1))
      else (key, right_child)
    ) in

    (* Sort by key *)
    Array.sort ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2) entries;

    (* Middle key gets promoted *)
    let mid = (max_keys + 1) / 2 in
    let (promoted_key, mid_right_child) = entries.(mid) in
    set_num_keys t mid;

    (* Create new right node and write right half *)
    let right_pageno = Page_allocator.allocate_page allocator in
    let parent = Header.get_parent t in
    Page_cache.with_page page_cache right_pageno (fun right_page_raw ->
      let right_page = init right_page_raw ~parent in
      set_child right_page 0 mid_right_child;
      for i = mid + 1 to max_keys do
        let (k, child) = entries.(i) in
        set_key right_page (i - mid - 1) k;
        set_child right_page (i - mid) child
      done;
      set_num_keys right_page (max_keys - mid) [@nontail]
    );

    (promoted_key, right_pageno)

  let insert t ~key ~right_child page_cache allocator =
    if not (is_full t) then begin
      insert_with_space t ~key ~right_child;
      SplitResult.NoSplit
    end else
      SplitResult.Split (split t ~key ~right_child page_cache allocator)

end

and Leaf : sig
  type t
  val init : Page.t @ local -> parent:Pageno.t option -> t @ local
  val lookup_key : t @ local -> int -> Pageno.t option
  val insert : t @ local -> key:int -> value:Pageno.t -> Page_cache.t -> Page_allocator.t -> SplitResult.t
end = struct
  (* Leaf node layout:
     - header: 8-bit node type, 64 bit parent pointer
     - num_keys: 16-bit little endian
     - entries: (key, pageno) pairs (each 16 bytes: 8-byte key + 8-byte pageno)
       stored sequentially, sorted *)
  type t = Page.t

  let entries_start = 11  (* 1 byte header + 8 bytes parent + 2 bytes num_keys *)

  let entry_size = 16
  let key_size = 8
  let pageno_size = 8

  let max_keys = (Page.page_size - entries_start) / entry_size

  let num_keys t =
    let buf = Page.underlying_read_only t in
    (Off_heap_buffer.unsafe_get_int16_le buf ~pos:Header.num_keys_offset [@nontail])

  let set_num_keys t n =
    let buf = Page.underlying t in
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:Header.num_keys_offset n [@nontail])

  let is_full t =
    num_keys t >= max_keys

  let get_entry t i =
    let buf = Page.underlying_read_only t in
    let pos = entries_start + (i * entry_size) in
    let key = Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos in
    let pageno = Pageno.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:(pos + key_size)) in
    (key, pageno)

  let set_entry t i ~key ~pointer =
    let buf = Page.underlying t in
    let pos = entries_start + (i * entry_size) in
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos key [@nontail]);
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:(pos + key_size) (Pageno.to_int pointer) [@nontail])
   
  let init (page @ local) ~parent : t @ local =
    let buf = Page.underlying page in
    (Off_heap_buffer.unsafe_set_int8_exn buf ~pos:Header.nodety_offset (Header.nodety_to_int Header.Leaf));
    let parent_int = match parent with
      | None -> Header.null_parent
      | Some p -> Pageno.to_int p
    in
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:Header.parent_offset parent_int);
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:Header.num_keys_offset 0);
    exclave_ page


  let find_key_pos (t @ local) k =
    let n = num_keys t in
    let rec search lo hi =
      if lo >= hi then
        if lo >= n then
          (assert (Int.equal lo n); `Not_found n)
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
    for i = n - 1 downto idx do
      let (k, p) = get_entry t i in
      set_entry t (i + 1) ~key:k ~pointer:p
    done;
    set_entry t idx ~key ~pointer:value;
    set_num_keys t (n + 1)

  (* Split a full leaf page. The page must have max_keys many keys. *)
  let split t ~key ~value page_cache allocator =
    assert (num_keys t = max_keys);

    let entries = Array.init (max_keys + 1) ~f:(fun i ->
      if i < max_keys then get_entry t i
      else (key, value)
    ) in

    Array.sort ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2) entries;

    let mid = (max_keys + 1) / 2 in
    let (pivot_key, _) = entries.(mid) in

    for i = 0 to mid - 1 do
      let (k, v) = entries.(i) in
      set_entry t i ~key:k ~pointer:v
    done;
    set_num_keys t mid;

    let right_pageno = Page_allocator.allocate_page allocator in
    Page_cache.with_page page_cache right_pageno (fun right_page_raw ->
      let parent = Header.get_parent t in
      let right_page = init right_page_raw ~parent in
      for i = mid to max_keys do
        let (k, v) = entries.(i) in
        set_entry right_page (i - mid) ~key:k ~pointer:v
      done;
      set_num_keys right_page (max_keys + 1 - mid) [@nontail]
    );

    (pivot_key, right_pageno)

  let insert (t @ local) ~key ~value page_cache allocator =
    match find_key_pos t key with
    | `Found_exact (~idx,_) ->
        set_entry t idx ~key ~pointer:value;
        SplitResult.NoSplit
    | `Not_found idx | `Found_other (~idx,_,_) ->
        if not (is_full t) then begin
          insert_with_space t ~idx ~key ~value;
          SplitResult.NoSplit
        end else
          SplitResult.Split (split t ~key ~value page_cache allocator)
end

type t = {
  mutable root : Pageno.t;
  cache : Page_cache.t;
  allocator : Page_allocator.t
}

let root t = t.root

let load cache allocator root =
  {root;cache;allocator}

let create cache allocator =
  let leaf_pageno = Page_allocator.allocate_page allocator in
  let root_pageno = Page_allocator.allocate_page allocator in
  Page_cache.with_page cache leaf_pageno (fun page ->
    ignore (Leaf.init page ~parent:(Some root_pageno))
  );
  Page_cache.with_page cache root_pageno (fun page ->
    let page = Internal.init page ~parent:None in
    Internal.set_child page 0 leaf_pageno [@nontail]
  );
  {root = root_pageno;cache;allocator}

type cursor = {
  mutable leaf_pageno : Pageno.t;
  mutable key : int;
  t : t;
}

let seek t key = 
  let rec loop pageno =
    Page_cache.with_page t.t.cache pageno (fun page ->
      match Header.classify page with
      | Either.First node -> loop (Internal.lookup_key node key)
      | Either.Second _ -> pageno)
  in
  t.leaf_pageno <- loop t.t.root;
  t.key <- key

let create_cursor t key =
  let cursor = {leaf_pageno = Pageno.of_int (-1); key;t} in
  seek cursor key;
  cursor

let get {leaf_pageno;key;t} =
  Page_cache.with_page t.cache leaf_pageno (fun page ->
    match Header.classify page with
    | Either.First _ -> assert false
    | Either.Second leaf -> Leaf.lookup_key leaf key [@nontail])

let set {leaf_pageno;key;t} value =
  let {cache;allocator;_} = t in

  let split_result =
    Page_cache.with_page cache leaf_pageno (fun page ->
      match Header.classify page with
      | Either.First _ -> assert false
      | Either.Second leaf -> Leaf.insert leaf ~key ~value cache allocator [@nontail])
  in

  match split_result with
  | SplitResult.NoSplit -> ()
  | SplitResult.Split (pivot, right_pageno) ->
      let rec propagate_split current_pageno split_key split_right =
        Page_cache.with_page cache current_pageno (fun page ->
          match Header.get_parent page with
          | None ->
              (* This is the root node w'ere splitting *)
              let new_root_pageno = Page_allocator.allocate_page allocator in
              Page_cache.with_page cache new_root_pageno (fun new_root ->
                let new_root = Internal.init new_root ~parent:None in
                Internal.set_child new_root 0 current_pageno;
                Internal.set_key new_root 0 split_key;
                Internal.set_child new_root 1 split_right;
                Internal.set_num_keys new_root 1;
                Page_cache.with_page cache current_pageno (fun child ->
                  Header.set_parent child new_root_pageno
                );
                Page_cache.with_page cache split_right (fun child ->
                  Header.set_parent child new_root_pageno [@nontail]
                ) 
              );
              t.root <- new_root_pageno
          | Some parent_pageno ->
              Page_cache.with_page cache parent_pageno (fun parent_page ->
                match Header.classify parent_page with
                | Either.First internal ->
                    (match Internal.insert internal ~key:split_key ~right_child:split_right cache allocator with
                    | SplitResult.NoSplit -> ()
                    | SplitResult.Split (new_key, new_right) ->
                        propagate_split parent_pageno new_key new_right)
                | Either.Second _ -> assert false))
      in
      propagate_split leaf_pageno pivot right_pageno