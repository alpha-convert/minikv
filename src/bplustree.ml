open! Core
open! Core_unix

module Header = struct
  type t =
    | Internal
    | Leaf

  let to_int (h : t) : int = Obj.magic h
  let of_int (i : int) : t = Obj.magic i

  let classify (page @ local) @ local = exclave_
    let buf = Page.underlying_read_only page in
    let header_byte = OffHeapBuffer.unsafe_get_int8 buf ~pos:0 in
    match of_int header_byte with
    | Internal -> Either.First page
    | Leaf -> Either.Second page

  let as_leaf (page @ local) @ local = exclave_ page
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

  (* Max keys = (page_size - 3) / 16 *)
  let max_keys = (Page.page_size - 3) / 16

  let num_keys t =
    let buf = Page.underlying_read_only t in
    (OffHeapBuffer.unsafe_get_int16_le buf ~pos:1 [@nontail])

  let set_num_keys t n =
    let buf = Page.underlying t in
    (OffHeapBuffer.unsafe_set_int16_le_exn buf ~pos:1 n [@nontail])

  let is_full t =
    num_keys t >= max_keys

  let get_key t i =
    let buf = Page.underlying_read_only t in
    let pos = tbl_start + (i * 16) + 8 in
    (OffHeapBuffer.unsafe_get_int64_le_exn buf ~pos [@nontail])

  let get_child t i =
    let buf = Page.underlying_read_only t in
    let pos = tbl_start + (i * 16) in
    Pageno.of_int (OffHeapBuffer.unsafe_get_int64_le_exn buf ~pos)

  let set_key t i key =
    let buf = Page.underlying t in
    let pos = tbl_start + (i * 16) + 8 in
    (OffHeapBuffer.unsafe_set_int64_le_exn buf ~pos key [@nontail])

  let set_child t i pageno =
    let buf = Page.underlying t in
    let pos = tbl_start + (i * 16) in
    (OffHeapBuffer.unsafe_set_int64_le_exn buf ~pos (Pageno.to_int pageno) [@nontail])

  let lookup_key (t @ local) k =
    let buf = Page.underlying_read_only t in
    let n = num_keys t in
    let rec search lo hi =
      if lo >= hi then
        let pos = tbl_start + (lo * 16) in
        Pageno.of_int (OffHeapBuffer.unsafe_get_int64_le_exn buf ~pos)
      else
        let mid = (lo + hi) / 2 in
        let key_pos = tbl_start + (mid * 16) + 8 in
        let mid_key = OffHeapBuffer.unsafe_get_int64_le_exn buf ~pos:key_pos in
        if k < mid_key then
          search lo mid
        else
          search (mid + 1) hi
    in
    (search 0 n [@nontail])

  let init page =
    let buf = Page.underlying page in
    (OffHeapBuffer.unsafe_set_int8_le_exn buf ~pos:0 (Header.to_int Header.Internal) [@nontail]);
    (OffHeapBuffer.unsafe_set_int16_le_exn buf ~pos:1 0 [@nontail])   (* num_keys = 0 *)
end

module Leaf = struct
  (* Leaf node layout:
     - header: 8-bit node type
     - num_keys: 16-bit little endian
     - entries: (key, pageno) pairs (each 16 bytes: 8-byte key + 8-byte pageno)
       stored sequentially, unsorted *)
  type t = Page.t

  let entries_start = 3  (* 1 byte header + 2 bytes num_keys *)

  (* Max keys = (page_size - 3) / 16 *)
  let max_keys = (Page.page_size - 3) / 16

  let num_keys t =
    let buf = Page.underlying_read_only t in
    (OffHeapBuffer.unsafe_get_int16_le buf ~pos:1 [@nontail])

  let set_num_keys t n =
    let buf = Page.underlying t in
    (OffHeapBuffer.unsafe_set_int16_le_exn buf ~pos:1 n [@nontail])

  let is_full t =
    num_keys t >= max_keys

  let get_entry t i =
    let buf = Page.underlying_read_only t in
    let pos = entries_start + (i * 16) in
    let key = OffHeapBuffer.unsafe_get_int64_le_exn buf ~pos in
    let pageno = Pageno.of_int (OffHeapBuffer.unsafe_get_int64_le_exn buf ~pos:(pos + 8)) in
    (key, pageno)

  let set_entry t i ~key ~pointer =
    let buf = Page.underlying t in
    let pos = entries_start + (i * 16) in
    (OffHeapBuffer.unsafe_set_int64_le_exn buf ~pos key [@nontail]);
    (OffHeapBuffer.unsafe_set_int64_le_exn buf ~pos:(pos + 8) (Pageno.to_int pointer) [@nontail])

  let lookup_key (t @ local) k =
    let n = num_keys t in
    let rec scan i =
      if i >= n then
        None
      else
        let (key, pageno) = get_entry t i in
        if Int.equal key k then
          Some pageno
        else
          scan (i + 1)
    in
    (scan 0 [@nontail])

  (* Insert key-value pair into leaf, assumes not full *)
  let insert t ~key ~value =
    let n = num_keys t in
    set_entry t n ~key ~pointer:value;
    set_num_keys t (n + 1)

  let init page =
    let buf = Page.underlying page in
    (OffHeapBuffer.unsafe_set_int16_le_exn buf ~pos:0 (Header.to_int Header.Leaf) [@nontail]);
    (OffHeapBuffer.unsafe_set_int16_le_exn buf ~pos:1 0 [@nontail])   (* num_keys = 0 *)
end


(* A Bplustree.t is just the page number of its root *)
type t = Pageno.t

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

let lookup root cache key =
  let leaf_pageno = lookup_leaf_page root cache key in
  Page_cache.with_page cache leaf_pageno (fun page ->
    let leaf = Header.as_leaf page in
    Leaf.lookup_key leaf key [@nontail]
  )

(* Simple insert - for now, just insert into leaf without handling splits *)
let insert root cache allocator ~key ~value =
  let leaf_pageno = lookup_leaf_page root cache key in
  Page_cache.with_page cache leaf_pageno (fun page ->
    let leaf = Header.as_leaf page in
    if not (Leaf.is_full leaf) then begin
      Leaf.insert leaf ~key ~value;
      root
    end else
      failwith "TODO: handle leaf split"
  )