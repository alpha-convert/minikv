(*
  B+ Tree Page Formats
  ====================

  Internal Node:
    ┌──────────────────┬──────────────┬──────────────┬──────────────────────────────────────────────────────┐
    │ 8 bytes          │ 8 bytes      │ 2 bytes      │                     entries                          │
    │ unified header   │ parent       │ num_keys     │  child₀ key₀ child₁ key₁ ... keyₙ₋₁ childₙ          │
    └──────────────────┴──────────────┴──────────────┴──────────────────────────────────────────────────────┘

    Physical layout of entries (for num_keys = n):
      ┌────────┬──────┬────────┬──────┬─────┬────────────┬────────┐
      │ child₀ │ key₀ │ child₁ │ key₁ │ ... │ keyₙ₋₁     │ childₙ  │
      │ 8b     │ 8b   │ 8b     │ 8b   │     │ 8b         │ 8b     │
      └────────┴──────┴────────┴──────┴─────┴────────────┴────────┘
      n keys and n+1 children (interleaved as child-key pairs, plus final child)

    - Keys are sorted in increasing order
    - child_i contains keys < key_i
    - child_{i+1} contains keys >= key_i
    - max_keys = (page_size - 18 - 8) / 16 = 1022

  Leaf Node:
    ┌──────────────────┬──────────────┬──────────────┬──────────────┬──────────────┬───────────────────────┐
    │ 8 bytes          │ 8 bytes      │ 2 bytes      │ 8 bytes      │ 8 bytes      │       entries         │
    │ unified header   │ parent       │ num_keys     │ left_sib     │ right_sib    │  key₀ ptr₀ key₁ ptr₁  │
    └──────────────────┴──────────────┴──────────────┴──────────────┴──────────────┴───────────────────────┘

    Entry layout (repeating):
      ┌──────────────┬──────────────┐
      │ 8 bytes      │ 8 bytes      │
      │ key          │ data pageno  │  × num_keys
      └──────────────┴──────────────┘

    - Keys are sorted in increasing order
    - left_sib/right_sib are Pageno.Or_null.t for leaf traversal
    - max_keys = (page_size - 34) / 16 = 1021
*)

open! Core
open! Core_unix

module SplitResult = struct
  type t = NoSplit | Split of #(int * Pageno.t)
end

let parent_offset = Page_header.size
let num_keys_offset = Page_header.size + parent_offset

let get_parent pg =
  let buf = Page.underlying_read_only pg in
  let parent = Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:parent_offset in
  Pageno.Or_null.of_int parent

let set_parent pg pageno =
  let buf = Page.underlying pg in
  Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:parent_offset (Pageno.to_int pageno) [@nontail]

let update_child_parent page_cache child_pageno new_parent =
  Page_cache.with_page page_cache child_pageno (fun child_page ->
    match Page.classify_as_bplustree_exn child_page with
    | Either.First internal -> set_parent internal new_parent [@nontail]
    | Either.Second leaf -> set_parent leaf new_parent [@nontail])

module Internal = struct
  type t = Page_header.bplustree_internal Page.t

  let tbl_start = Page_header.size + 8 + 2  (* header + parent + num_keys *)

  let child_size = 8
  let key_size = 8
  let entry_size = child_size + key_size

  let max_keys = (Page.page_size - tbl_start - child_size) / entry_size

  let num_keys t =
    let buf = Page.underlying_read_only t in
    (Off_heap_buffer.unsafe_get_int16_le buf ~pos:num_keys_offset [@nontail])

  let set_num_keys t n =
    let buf = Page.underlying t in
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:num_keys_offset n [@nontail])

  let is_full t =
    num_keys t >= max_keys

  let key_pos i =
    tbl_start + (i * entry_size) + child_size

  let child_pos i =
    tbl_start + (i * entry_size)

  let get_key t i =
    let buf = Page.underlying_read_only t in
    (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:(key_pos i) [@nontail])

  let get_child t i =
    let buf = Page.underlying_read_only t in
    Pageno.of_int_exn (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:(child_pos i))

  let set_key t i key =
    let buf = Page.underlying t in
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:(key_pos i) key [@nontail])

  let set_child (t @ local) i pageno =
    let buf = Page.underlying t in
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:(child_pos i) (Pageno.to_int pageno) [@nontail]

  let lookup_key (t @ local) k =
    let n = num_keys t in
    let rec search lo hi =
      if lo >= hi then
        get_child t lo
      else
        let mid = (lo + hi) / 2 in
        let mid_key = get_key t mid in
        if k < mid_key then
          search lo mid
        else
          search (mid + 1) hi
    in
    (search 0 n [@nontail])

  let init (page : Page.packed @ local) ~parent : t @ local = exclave_
    let page = Page.overwrite_as_bplustree_internal page in
    let buf = Page.underlying page in
    let parent_int = Pageno.Or_null.to_int parent in 
    (Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:parent_offset parent_int);
    (Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:num_keys_offset 0);
    page

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

    let rec find_pos i =
      if i >= max_keys then max_keys
      else if key < get_key t i then i
      else find_pos (i + 1)
    in
    let insert_pos = find_pos 0 in

    let mid = (max_keys + 1) / 2 in

    let right_pageno = Page_allocator.allocate_page allocator in
    let parent = get_parent t in

    (*  logical position (after insertion) *)
    let get_entry_at_logical i =
      if i < insert_pos then
        #(get_key t i, get_child t (i + 1))
      else if i = insert_pos then
        #(key, right_child)
      else
        #(get_key t (i - 1), get_child t i)
    in

    let #(promoted_key, mid_right_child) = get_entry_at_logical mid in

    (* Build right node with entries [mid+1..max_keys] *)
    Page_cache.with_page page_cache right_pageno (fun right_page_raw ->
      let right_page = init right_page_raw ~parent in
      set_child right_page 0 mid_right_child;
      for i = mid + 1 to max_keys do
        let #(k, c) = get_entry_at_logical i in
        set_key right_page (i - mid - 1) k;
        set_child right_page (i - mid) c
      done;
      set_num_keys right_page (max_keys - mid) [@nontail]);

    (* Update parent pointers for children that moved to right node *)
    update_child_parent page_cache mid_right_child right_pageno;
    for i = mid + 1 to max_keys do
      let #(_, c) = get_entry_at_logical i in
      update_child_parent page_cache c right_pageno
    done;

    (* Update left node with entries [0..mid-1] *)
    (* We need to move entries if insert_pos < mid *)
    if insert_pos < mid then begin
      (* Shift entries [insert_pos..mid-2] right by one *)
      for i = mid - 2 downto insert_pos do
        set_key t (i + 1) (get_key t i);
        set_child t (i + 2) (get_child t (i + 1))
      done;
      (* Insert new entry *)
      set_key t insert_pos key;
      set_child t (insert_pos + 1) right_child;
    end;
    (* If insert_pos >= mid, the new entry went to right node, so left is unchanged *)
    set_num_keys t mid;

    #(promoted_key, right_pageno)

  let insert t ~key ~right_child page_cache allocator =
    if not (is_full t) then begin
      insert_with_space t ~key ~right_child;
      SplitResult.NoSplit
    end else
      SplitResult.Split (split t ~key ~right_child page_cache allocator)

end

module Leaf = struct
  type t = Page_header.bplustree_leaf Page.t

  let left_sib_offset = Page_header.size + 8 + 2  (* header + parent + num_keys *)
  let right_sib_offset = left_sib_offset + 8
  let entries_start = right_sib_offset + 8
  let key_size = 8
  let pageno_size = 8
  let entry_size = key_size + pageno_size

  let max_keys = (Page.page_size - entries_start) / entry_size

  let num_keys t =
    let buf = Page.underlying_read_only t in
    Off_heap_buffer.unsafe_get_int16_le buf ~pos:num_keys_offset [@nontail]

  let set_num_keys t n =
    let buf = Page.underlying t in
    Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:num_keys_offset n [@nontail]
  
  let get_sibs t =
    let buf = Page.underlying_read_only t in
    let left = Pageno.Or_null.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:left_sib_offset) in
    let right = Pageno.Or_null.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:right_sib_offset) in
    #(~left,~right)
  
  let set_left_sib (t @ local) left =
    let buf = Page.underlying t in
    let left_sib_i = Pageno.Or_null.to_int left in
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:left_sib_offset left_sib_i [@nontail]

  let set_right_sib (t @ local) right =
    let buf = Page.underlying t in
    let right_sib_i = Pageno.Or_null.to_int right in
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:right_sib_offset right_sib_i [@nontail]

  let is_full t =
    num_keys t >= max_keys

  let get_entry t i =
    let buf = Page.underlying_read_only t in
    let pos = entries_start + (i * entry_size) in
    let key = Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos in
    let pageno = Pageno.of_int_exn (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:(pos + key_size)) in
    (key, pageno)

  let set_entry t i ~key ~pointer =
    let buf = Page.underlying t in
    let pos = entries_start + (i * entry_size) in
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos key;
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:(pos + key_size) (Pageno.to_int pointer) [@nontail]
   
  let init (page : Page.packed @ local) ~parent ~left ~right : t @ local = exclave_
    let page = Page.overwrite_as_bplustree_leaf page in
    let buf = Page.underlying page in
    let parent_int = Pageno.Or_null.to_int parent in
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:parent_offset parent_int;
    Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:num_keys_offset 0;
    set_left_sib page left;
    set_right_sib page right;
    page


  let find_key_pos (t @ local) k =
    let n = num_keys t in
    let rec search lo hi =
      if lo >= hi then
        if lo >= n then
          (assert (Int.equal lo n); `Not_found n)
        else
          let (k', pageno) = get_entry t lo in
          if Int.equal k k' then `Found_exact (~idx:lo,pageno) else `Found_other (~idx:lo,~key:k',pageno)
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
    | `Not_found _ -> Null
    | `Found_exact (~idx,pageno) -> This (idx,pageno)
    | `Found_other _ -> Null

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

    (* TODO: do this split in place... *)
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

    let #(~left:_,~right:t_right) = get_sibs t in
    let right_pageno = Page_allocator.allocate_page allocator in
    Page_cache.with_page page_cache right_pageno (fun right_page_raw ->
      let parent = get_parent t in
      let right_page = init right_page_raw ~parent ~left:(This (Page.pageno t)) ~right:t_right in
      for i = mid to max_keys do
        let (k, v) = entries.(i) in
        set_entry right_page (i - mid) ~key:k ~pointer:v
      done;
      set_num_keys right_page (max_keys + 1 - mid) [@nontail]
    );
    set_right_sib t (This right_pageno);

    (match%optional.Or_null t_right with
    | None -> ()
    | Some old_right_pageno ->
        Page_cache.with_page page_cache old_right_pageno (fun (P old_right) ->
          set_left_sib old_right (This right_pageno)));

    #(pivot_key, right_pageno)

  let insert (t @ local) ~key ~value page_cache allocator =
    match find_key_pos t key with
    | `Found_exact (~idx,_) ->
        set_entry t idx ~key ~pointer:value;
        SplitResult.NoSplit
    | `Not_found idx | `Found_other (~idx,~key:_,_) ->
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
    let _ : Leaf.t = Leaf.init page ~parent:(This root_pageno) ~left:Null ~right:Null in ()
  );
  Page_cache.with_page cache root_pageno (fun page ->
    let page = Internal.init page ~parent:Null in
    Internal.set_child page 0 leaf_pageno [@nontail]
  );
  {root = root_pageno;cache;allocator}

type cursor = {
  mutable pageno : Pageno.t;
  mutable idx : int Or_null.t;
  mutable key : int;
  t : t;
}

let find_leaf t key =
  let rec loop pageno =
    Page_cache.with_page t.cache pageno (fun page ->
      match Page.classify_as_bplustree_exn page with
      | Either.First node -> loop (Internal.lookup_key node key)
      | Either.Second _ -> pageno)
  in
  loop t.root

let seek cursor key =
  let leaf_pageno = find_leaf cursor.t key in
  cursor.key <- key;
  Page_cache.with_page cursor.t.cache leaf_pageno (fun page ->
    let leaf = Page.classify_as_bplustree_leaf_exn page in
    cursor.pageno <- leaf_pageno;
    match Leaf.find_key_pos leaf key with
    | `Found_exact (~idx, _) ->
        cursor.idx <- This idx
    | `Found_other _ | `Not_found _ -> ())


let shift_to_sibling dir cursor leaf =
  let #(~left, ~right) = Leaf.get_sibs leaf in
  let sib =
    match dir with
    | `Left -> left
    | `Right -> right
  in
  match%optional.Or_null sib with
  | None -> cursor.idx <- Null
  | Some sib_pageno ->
      Page_cache.with_page cursor.t.cache sib_pageno (fun sib_page ->
        let sib_leaf = Page.classify_as_bplustree_leaf_exn sib_page in
        let num_keys = Leaf.num_keys sib_leaf in
        assert (num_keys > 0);
        let sib_leaf_idx =
          match dir with
          | `Left -> num_keys - 1
          | `Right -> 0
        in
        let (k, _) = Leaf.get_entry sib_leaf sib_leaf_idx in
        cursor.pageno <- sib_pageno;
        cursor.idx <- This sib_leaf_idx;
        cursor.key <- k)

let step cursor dir =
  let incr = 
    match dir with
    | `Left -> -1
    | `Right -> 1
  in
  let next_key = cursor.key + incr in
  let leaf_pageno = cursor.pageno in
  Page_cache.with_page cursor.t.cache leaf_pageno (fun page ->
    let leaf = Page.classify_as_bplustree_leaf_exn page in
    match Leaf.find_key_pos leaf next_key with
    | `Found_exact (~idx, _) ->
        cursor.pageno <- leaf_pageno;
        cursor.idx <- This idx;
        cursor.key <- next_key
    | `Found_other _ ->
        cursor.pageno <- leaf_pageno;
        cursor.idx <- Null;
        cursor.key <- next_key 
    | `Not_found _ ->
        shift_to_sibling dir cursor leaf [@nontail])

let next cursor = step cursor `Right
let prev cursor = step cursor `Left
  
let create_cursor t key =
  let cursor = { pageno = Pageno.of_int_exn Int.max_value ; idx = Null; key; t } in
  seek cursor key;
  cursor

let get cursor =
  match%optional.Or_null cursor.idx with
  | None -> Null
  | Some idx ->
      Page_cache.with_page cursor.t.cache cursor.pageno (fun page ->
        let leaf = Page.classify_as_bplustree_leaf_exn page in
        let (_,pageno) = Leaf.get_entry leaf idx in
        This pageno)
      
let set cursor value =
  let t = cursor.t in
  let { cache; allocator; _ } = t in
  let rec propagate_split current_pageno split_key split_right =
    Page_cache.with_page cache current_pageno (fun page ->
      match Page.classify_as_bplustree_exn page with
      | Either.First current_page ->
        (match%optional.Or_null get_parent current_page with
        | None ->
            let new_root_pageno = Page_allocator.allocate_page allocator in
            Page_cache.with_page cache new_root_pageno (fun new_root ->
              let new_root = Internal.init new_root ~parent:Null in
              Internal.set_child new_root 0 current_pageno;
              Internal.set_key new_root 0 split_key;
              Internal.set_child new_root 1 split_right;
              Internal.set_num_keys new_root 1;
              Page_cache.with_page cache split_right (fun child ->
                let child = Page.classify_as_bplustree_internal_exn child in
                set_parent child new_root_pageno [@nontail]
              )
            );
            set_parent current_page new_root_pageno;
            t.root <- new_root_pageno
        | Some parent_pageno ->
            Page_cache.with_page cache parent_pageno (fun parent_page ->
              let parent_page = Page.classify_as_bplustree_internal_exn parent_page in
              match Internal.insert parent_page ~key:split_key ~right_child:split_right cache allocator with
              | SplitResult.NoSplit -> ()
              | SplitResult.Split #(new_key, new_right) ->
                  propagate_split parent_pageno new_key new_right))
      | Either.Second current_page ->
        (match%optional.Or_null get_parent current_page with
        | None ->
            let new_root_pageno = Page_allocator.allocate_page allocator in
            Page_cache.with_page cache new_root_pageno (fun new_root ->
              let new_root = Internal.init new_root ~parent:Null in
              Internal.set_child new_root 0 current_pageno;
              Internal.set_key new_root 0 split_key;
              Internal.set_child new_root 1 split_right;
              Internal.set_num_keys new_root 1;
              Page_cache.with_page cache split_right (fun child ->
                let child = Page.classify_as_bplustree_leaf_exn child in
                set_parent child new_root_pageno [@nontail]
              )
            );
            set_parent current_page new_root_pageno;
            t.root <- new_root_pageno
        | Some parent_pageno ->
            Page_cache.with_page cache parent_pageno (fun parent_page ->
              let parent_page = Page.classify_as_bplustree_internal_exn parent_page in
              match Internal.insert parent_page ~key:split_key ~right_child:split_right cache allocator with
              | SplitResult.NoSplit -> ()
              | SplitResult.Split #(new_key, new_right) ->
                  propagate_split parent_pageno new_key new_right)))
  in
  let key = cursor.key in
  let leaf_pageno = cursor.pageno in
  let split_result =
    Page_cache.with_page cache leaf_pageno (fun page ->
      let leaf = Page.classify_as_bplustree_leaf_exn page in
      Leaf.insert leaf ~key ~value cache allocator [@nontail])
  in
  let update_cursor_pos new_leaf_pageno =
    Page_cache.with_page cache new_leaf_pageno (fun page ->
      let leaf = Page.classify_as_bplustree_leaf_exn page in
      match%optional.Or_null Leaf.lookup_key leaf key with
      | Some pos -> cursor.idx <- This (fst pos)
      | None -> cursor.idx <- Null)
  in
  match split_result with
  | SplitResult.NoSplit -> update_cursor_pos leaf_pageno
  | SplitResult.Split #(pivot, new_right_pageno) ->
      let new_leaf = if key >= pivot then new_right_pageno else leaf_pageno in
      propagate_split leaf_pageno pivot new_right_pageno;
      update_cursor_pos new_leaf

module Valid = struct
  let check t =
    let cache = t.cache in

    (* Check that keys are sorted in a leaf *)
    let check_leaf_sorted leaf pageno =
      let n = Leaf.num_keys leaf in
      for i = 0 to n - 2 do
        let (k1, _) = Leaf.get_entry leaf i in
        let (k2, _) = Leaf.get_entry leaf (i + 1) in
        if k1 >= k2 then
          failwith (sprintf "Keys not sorted in leaf page %d: key[%d]=%d >= key[%d]=%d"
            (Pageno.to_int pageno) i k1 (i + 1) k2)
      done
    in

    (* Check that keys are sorted in an internal node *)
    let check_internal_sorted internal pageno =
      let n = Internal.num_keys internal in
      for i = 0 to n - 2 do
        let k1 = Internal.get_key internal i in
        let k2 = Internal.get_key internal (i + 1) in
        if k1 >= k2 then
          failwith (sprintf "Keys not sorted in internal page %d: key[%d]=%d >= key[%d]=%d"
            (Pageno.to_int pageno) i k1 (i + 1) k2)
      done
    in

    (* Recursively check the tree, returning (min_key, max_key, depth) for the subtree *)
    let rec check_node pageno ~expected_parent ~min_bound ~max_bound =
      Page_cache.with_page cache pageno (fun page ->
        match Page.classify_as_bplustree_exn page with
        | Either.Second leaf ->
            (* Check parent pointer *)
            let actual_parent = get_parent leaf in
            (match expected_parent, actual_parent with
            | Some exp, This act when not (Pageno.equal exp act) ->
                failwith (sprintf "Parent mismatch for leaf %d: expected %d, got %d"
                  (Pageno.to_int pageno) (Pageno.to_int exp) (Pageno.to_int act))
            | Some _, Null ->
                failwith (sprintf "Parent mismatch for leaf %d: expected parent, got null"
                  (Pageno.to_int pageno))
            | None, This act ->
                failwith (sprintf "Parent mismatch for leaf %d: expected null (root), got %d"
                  (Pageno.to_int pageno) (Pageno.to_int act))
            | _ -> ());

            check_leaf_sorted leaf pageno;

            let n = Leaf.num_keys leaf in
            (* Check key bounds *)
            for i = 0 to n - 1 do
              let (k, _) = Leaf.get_entry leaf i in
              (match min_bound with
              | Some min when k < min ->
                  failwith (sprintf "Key %d in leaf %d is below min bound %d"
                    k (Pageno.to_int pageno) min)
              | _ -> ());
              (match max_bound with
              | Some max when k >= max ->
                  failwith (sprintf "Key %d in leaf %d is at or above max bound %d"
                    k (Pageno.to_int pageno) max)
              | _ -> ())
            done;

            0 (* depth of leaf is 0 *)

        | Either.First internal ->
            (* Check parent pointer *)
            let actual_parent = get_parent internal in
            (match expected_parent, actual_parent with
            | Some exp, This act when not (Pageno.equal exp act) ->
                failwith (sprintf "Parent mismatch for internal %d: expected %d, got %d"
                  (Pageno.to_int pageno) (Pageno.to_int exp) (Pageno.to_int act))
            | Some _, Null ->
                failwith (sprintf "Parent mismatch for internal %d: expected parent, got null"
                  (Pageno.to_int pageno))
            | None, This act ->
                failwith (sprintf "Parent mismatch for internal %d: expected null (root), got %d"
                  (Pageno.to_int pageno) (Pageno.to_int act))
            | _ -> ());

            check_internal_sorted internal pageno;

            let n = Internal.num_keys internal in

            (* Check all children recursively *)
            let first_child = Internal.get_child internal 0 in
            let child_min = min_bound in
            let child_max = if n > 0 then Some (Internal.get_key internal 0) else max_bound in
            let first_depth = check_node first_child ~expected_parent:(Some pageno) ~min_bound:child_min ~max_bound:child_max in

            for i = 0 to n - 1 do
              let child = Internal.get_child internal (i + 1) in
              let child_min = Some (Internal.get_key internal i) in
              let child_max = if i + 1 < n then Some (Internal.get_key internal (i + 1)) else max_bound in
              let depth = check_node child ~expected_parent:(Some pageno) ~min_bound:child_min ~max_bound:child_max in
              if depth <> first_depth then
                failwith (sprintf "Inconsistent depth in internal %d: child 0 has depth %d, child %d has depth %d"
                  (Pageno.to_int pageno) first_depth (i + 1) depth)
            done;

            first_depth + 1)
    in

    (* Check sibling pointers by traversing leaves left-to-right *)
    let check_sibling_pointers () =
      (* Find leftmost leaf *)
      let rec find_leftmost pageno =
        Page_cache.with_page cache pageno (fun page ->
          match Page.classify_as_bplustree_exn page with
          | Either.Second _ -> pageno
          | Either.First internal -> find_leftmost (Internal.get_child internal 0))
      in
      let leftmost = find_leftmost t.root in

      (* Traverse all leaves checking sibling pointers *)
      let rec traverse prev_pageno current_pageno =
        Page_cache.with_page cache current_pageno (fun page ->
          let leaf = Page.classify_as_bplustree_leaf_exn page in
          let #(~left, ~right) = Leaf.get_sibs leaf in

          (* Check left sibling *)
          (match prev_pageno, left with
          | None, Null -> ()
          | None, This l ->
              failwith (sprintf "Leftmost leaf %d has non-null left sibling %d"
                (Pageno.to_int current_pageno) (Pageno.to_int l))
          | Some prev, Null ->
              failwith (sprintf "Leaf %d has null left sibling, expected %d"
                (Pageno.to_int current_pageno) (Pageno.to_int prev))
          | Some prev, This l when not (Pageno.equal prev l) ->
              failwith (sprintf "Leaf %d has wrong left sibling: expected %d, got %d"
                (Pageno.to_int current_pageno) (Pageno.to_int prev) (Pageno.to_int l))
          | Some _, This _ -> ());

          (* Continue to right sibling *)
          match%optional.Or_null right with
          | None -> ()
          | Some next -> traverse (Some current_pageno) next)
      in
      traverse None leftmost
    in

    let _ = check_node t.root ~expected_parent:None ~min_bound:None ~max_bound:None in
    check_sibling_pointers ()
end