(*
  Free Page Format
  =================

  Each free page has the 8-byte unified page header, followed by the next free pageno.

    ┌──────────────────┬────────────────────┐
    │ 8 bytes          │   8 bytes          │
    │ unified header   │  next_free_pageno  │
    └──────────────────┴────────────────────┘

  next_free_pageno is an Pageno.Or_null.t
*)

open! Core

let next_pageno_offset = Page_header.size

type t = {
  cache : Page_cache.t;
  mutable head : Pageno.t Or_null.t
}

module Expert = struct
  let get_head t = t.head
  let set_head t pageno = t.head <- This pageno

  let create cache = {cache; head = Null}
end

module Free_page = struct
  let get_next_pageno page =
    let page = Page.classify_as_free_page_exn page in
    let buf = Page.underlying_read_only page in
    Pageno.Or_null.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:next_pageno_offset)

  let set_next_pageno page next =
    let page = Page.classify_as_free_page_exn page in
    let buf = Page.underlying page in
    let next_int = Pageno.Or_null.to_int next in
    Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:next_pageno_offset next_int [@nontail]
end


let add t pageno =
  Page_cache.with_page t.cache pageno (fun page -> Free_page.set_next_pageno page t.head [@nontail]);
  t.head <- This pageno

let take t =
  match%optional.Or_null t.head with
  | None -> Null
  | Some head_pageno ->
    let next =
      Page_cache.with_page t.cache head_pageno (fun page ->
        Free_page.get_next_pageno page [@nontail])
    in
    t.head <- next;
    This head_pageno