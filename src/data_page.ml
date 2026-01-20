(*
  Data Page Format
  ================

  Each data page has the 8-byte unified page header, which encodes whether
  this is a Packed or Linked data page. The rest of the page is type-specific.

  Packed:
    ┌──────────────────┬────────────────┬─────────────────────────┐
    │ 8 bytes          │   2 bytes      │  up to page_size - 10   │
    │ unified header   │  data length   │       data              │
    └──────────────────┴────────────────┴─────────────────────────┘

  Linked:
    ┌──────────────────┬────────────────┬────────────────┬─────────────────────────┐
    │ 8 bytes          │   4 bytes      │    4 bytes     │  up to page_size - 16   │
    │ unified header   │  next_pageno   │ remaining size │       data              │
    └──────────────────┴────────────────┴────────────────┴─────────────────────────┘

    remaining size = total bytes from this page onward (including this page's data)
*)

open! Core

module Packed = struct
  let len_offset = Page_header.size
  let data_offset = Page_header.size + 2
  let max_data = Page.page_size - data_offset

  let read pageno cache : Bytes.t =
    Page_cache.with_page cache pageno (fun page ->
      let page = Page.classify_as_data_packed_exn page in
      let buf = Page.underlying_read_only page in
      let len = Off_heap_buffer.unsafe_get_int16_le buf ~pos:len_offset in
      Off_heap_buffer.to_bytes buf ~pos:data_offset ~len [@nontail]
    )

  let write pageno cache (bytes : Bytes.t) : unit =
    let len = Bytes.length bytes in
    assert (len <= max_data);
    Page_cache.with_page cache pageno (fun page ->
      let page = Page.overwrite_as_data_packed page in
      let buf = Page.underlying page in
      Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:len_offset len;
      Off_heap_buffer.blit_from_bytes buf bytes ~src_pos:0 ~dst_pos:data_offset ~len [@nontail]
    )
end

module Linked = struct
  (* After 8-byte unified header: 4-byte next_pageno, 4-byte remaining_size, then data *)
  let next_pageno_offset = Page_header.size
  let remaining_size_offset = Page_header.size + 4
  let data_offset = Page_header.size + 8
  let max_data = Page.page_size - data_offset

  let get_next_pageno (page : Page_header.data_linked Page.t) : Pageno.t Or_null.t =
    let buf = Page.underlying_read_only page in
    let next = Off_heap_buffer.unsafe_get_int32_le buf ~pos:next_pageno_offset in
    Pageno.of_int next

  let get_remaining_size page : int =
    let buf = Page.underlying_read_only page in
    Off_heap_buffer.unsafe_get_int32_le buf ~pos:remaining_size_offset [@nontail]

  let init_page (page : Page_header.data_linked Page.t) ~next ~remaining_size : unit =
    let buf = Page.underlying page in
    let next_int = match%optional.Or_null next with
      | None -> -1
      | Some p -> Pageno.to_int p
    in
    Off_heap_buffer.unsafe_set_int32_le_exn buf ~pos:next_pageno_offset next_int;
    Off_heap_buffer.unsafe_set_int32_le_exn buf ~pos:remaining_size_offset remaining_size [@nontail]

  let read root cache : Bytes.t =
    let remaining,dst =
      Page_cache.with_page cache root (fun page ->
        let page = Page.classify_as_data_linked_exn page in
        let total_size = get_remaining_size page in
        let result = Bytes.create total_size in
        total_size,result)
    in
    let rec go pageno_or_null ~dst_pos ~remaining : unit =
      match%optional.Or_null pageno_or_null with
      | None -> ()
      | Some pageno ->
        let to_copy = min remaining max_data in
        let next =
          Page_cache.with_page cache pageno (fun page ->
            let page = Page.classify_as_data_linked_exn page in
            let buf = Page.underlying_read_only page in
            Off_heap_buffer.blit_to_bytes buf dst ~src_pos:data_offset ~dst_pos ~len:to_copy;
            get_next_pageno page [@nontail])
        in
        go next ~dst_pos:(dst_pos + max_data) ~remaining:(remaining - max_data)
    in
    go (This root) ~dst_pos:0 ~remaining;
    dst

  let write root cache allocator (bytes : Bytes.t) : unit =
    let rec go pageno ~src_pos ~remaining : unit =
      let next_or_null =
        Page_cache.with_page cache pageno (fun page ->
          let page = Page.overwrite_as_data_linked page in
          let to_write = min remaining max_data in
          let next =
            if remaining > max_data then begin
              match%optional.Or_null get_next_pageno page with
              | Some existing -> This existing
              | None -> This (Page_allocator.allocate_page allocator)
            end else Null
          in
          init_page page ~next ~remaining_size:remaining;
          let buf = Page.underlying page in
          Off_heap_buffer.blit_from_bytes buf bytes ~src_pos ~dst_pos:data_offset ~len:to_write;
          next)
      in
      match%optional.Or_null next_or_null with
      | None -> ()
      | Some next ->
        go next ~src_pos:(src_pos + max_data) ~remaining:(remaining - max_data)
    in
    go root ~src_pos:0 ~remaining:(Bytes.length bytes)
end

let read root ~cache =
  Page_cache.with_page cache root (fun page ->
    (* Blah, this is wasteful *)
    match Page.classify page with
    | Page.C #(Page_header.Data_packed_header, _) -> Packed.read root cache
    | Page.C #(Page_header.Data_linked_header, _) -> Linked.read root cache
    | _ -> assert false
  )

let write root ~cache ~allocator ~src =
  let src_length = Bytes.length src in
  assert (src_length <= 0xFFFFFFFF);
  if src_length <= Packed.max_data then
    Packed.write root cache src
  else
    Linked.write root cache allocator src
