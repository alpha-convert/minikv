(*
  Data Page Format
  ================

  Each data page consists of a header followed by data.

  Page Types (2-bit discriminator)
  --------------------------------
    00 = Packed    Single-page value; data fits entirely on this page
    01 = Linked    Page in a multi-page value chain

  Header Layouts
  --------------

  Packed (2 bytes total):
    ┌─────────┬────────────────┐
    │ 2 bits  │    14 bits     │
    │  0b00   │  data length   │
    └─────────┴────────────────┘

  Linked (8 bytes total):
    ┌─────────┬────────────────────────────┬─────────────────────────────────┐
    │ 2 bits  │          30 bits           │            32 bits              │
    │  0b01   │       next_pageno          │    remaining size (bytes)       │
    └─────────┴────────────────────────────┴─────────────────────────────────┘

    remaining size = total bytes from this page onward (including this page's data)

*)

open! Core

module Header = struct
  type page_type = Packed | Linked

  let type_packed = 0b00
  let type_linked = 0b01

  let decode_type hdr = hdr land 0b11

  let classify ~(cache : Page_cache.t) (pageno : Pageno.t) : page_type =
    Page_cache.with_page cache pageno (fun page ->
      let buf = Page.underlying_read_only page in
      let hdr = Off_heap_buffer.unsafe_get_int16_le buf ~pos:0 in
      match decode_type hdr with
      | 0b00 -> Packed
      | 0b01 -> Linked
      | _ -> assert false
    )
end

module Packed = struct
  type t = #{
    pageno : Pageno.t;
    cache : Page_cache.t;
  }

  let header_size = 2
  let max_data = Page.page_size - header_size

  let encode_header len = len lsl 2

  let decode_len hdr = hdr lsr 2

  let create pageno cache = #{ pageno; cache }

  let read t : Bytes.t =
    Page_cache.with_page t.#cache t.#pageno (fun page ->
      let buf = Page.underlying_read_only page in
      let hdr = Off_heap_buffer.unsafe_get_int16_le buf ~pos:0 in
      let len = decode_len hdr in
      Off_heap_buffer.to_bytes buf ~pos:header_size ~len [@nontail]
    )

  let write t (bytes : Bytes.t) : unit =
    let len = Bytes.length bytes in
    assert (len <= max_data);
    Page_cache.with_page t.#cache t.#pageno (fun page ->
      let buf = Page.underlying page in
      Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:0 (encode_header len);
      Off_heap_buffer.blit_from_bytes buf bytes ~src_pos:0 ~dst_pos:header_size ~len [@nontail]
    )
end

module Linked = struct
  type t = #{
    pageno : Pageno.t;
    cache : Page_cache.t;
    allocator : Page_allocator.t;
  }

  let header_size = 8
  let max_data = Page.page_size - header_size

  let encode_header next_pageno =
    (next_pageno lsl 2) lor Header.type_linked

  let decode_next_pageno hdr = hdr lsr 2

  let create pageno cache allocator = #{ pageno; cache; allocator }

  let get_next_pageno (page : Page.t) : Pageno.t Or_null.t =
    let buf = Page.underlying_read_only page in
    let hdr32 = Off_heap_buffer.unsafe_get_int32_le buf ~pos:0 in
    let next = decode_next_pageno hdr32 in
    Pageno.of_int next

  let get_remaining_size (page : Page.t) : int =
    let buf = Page.underlying_read_only page in
    Off_heap_buffer.unsafe_get_int32_le buf ~pos:4 [@nontail]

  let init_page (page : Page.t) ~next ~remaining_size : unit =
    let buf = Page.underlying page in
    let next_int = match%optional.Or_null next with
      | None -> -1
      | Some p -> Pageno.to_int p
    in
    Off_heap_buffer.unsafe_set_int32_le_exn buf ~pos:0 (encode_header next_int);
    Off_heap_buffer.unsafe_set_int32_le_exn buf ~pos:4 remaining_size [@nontail]

  let write_data (page : Page.t) (bytes : Bytes.t) ~src_pos ~len : unit =
    let buf = Page.underlying page in
    Off_heap_buffer.blit_from_bytes buf bytes ~src_pos ~dst_pos:header_size ~len [@nontail]

  let read t : Bytes.t =
    let cache = t.#cache in
    let remaining,dst = 
      Page_cache.with_page t.#cache t.#pageno (fun page ->
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
            let buf = Page.underlying_read_only page in
            Off_heap_buffer.blit_to_bytes buf dst ~src_pos:header_size ~dst_pos ~len:to_copy;
            get_next_pageno page)
        in
        go next ~dst_pos:(dst_pos + max_data) ~remaining:(remaining - max_data)
    in
    go (This t.#pageno) ~dst_pos:0 ~remaining;
    dst

  let write t (bytes : Bytes.t) : unit =
    let cache = t.#cache in
    let allocator = t.#allocator in
    let rec go pageno_or_null ~src_pos ~remaining : unit =
      match%optional.Or_null pageno_or_null with
      | None -> assert (remaining <= 0)
      | Some pageno ->
        let next =
          Page_cache.with_page cache pageno (fun page ->
            let to_write = min remaining max_data in
            let next =
              if remaining > max_data then begin
                match%optional.Or_null get_next_pageno page with
                | Some existing -> This existing
                | None -> This (Page_allocator.allocate_page allocator)
              end else Null
            in
            init_page page ~next ~remaining_size:remaining;
            write_data page bytes ~src_pos ~len:to_write;
            next)
        in
        go next ~src_pos:(src_pos + max_data) ~remaining:(remaining - max_data)
    in
    go (This t.#pageno) ~src_pos:0 ~remaining:(Bytes.length bytes)
end

type t = {
  root : Pageno.t;
  cache : Page_cache.t;
  allocator : Page_allocator.t;
}

let create root cache allocator = { root; cache; allocator }

let read t =
  match Header.classify ~cache:t.cache t.root with
  | Packed -> Packed.read (Packed.create t.root t.cache)
  | Linked -> Linked.read (Linked.create t.root t.cache t.allocator)

let write t bytes =
  if Bytes.length bytes <= Packed.max_data then
    Packed.write (Packed.create t.root t.cache) bytes
  else
    Linked.write (Linked.create t.root t.cache t.allocator) bytes
