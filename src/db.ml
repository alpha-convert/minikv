open! Core
open! Core_unix

type t = {
  fd : File_descr.t;
  bptree : Bplustree.t;
  cache : Page_cache.t;
  allocator : Page_allocator.t;
  mutable latest_root_pageno : Pageno.t;
}

(* Metadata page (page 0) layout:
   - offset 0: root_pageno (64-bit little endian)
   - rest: reserved for future metadata *)
module Metadata = struct
  let metadata_pageno = Pageno.of_int 0
  let root_pageno_offset = 0

  let read_root_pageno page_cache =
    Page_cache.with_page page_cache metadata_pageno (fun page ->
      let buf = Page.underlying_read_only page in
      Pageno.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:root_pageno_offset)
    )

  let write_root_pageno page_cache root_pageno =
    Page_cache.with_page page_cache metadata_pageno (fun page ->
      let buf = Page.underlying page in
      Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:root_pageno_offset (Pageno.to_int root_pageno) [@nontail]
    )
end

let flush_metadata_if_new_root t = 
  let root_pageno = Bplustree.root t.bptree in
  if not (Pageno.equal root_pageno t.latest_root_pageno) then (
    t.latest_root_pageno <- root_pageno;
    Metadata.write_root_pageno t.cache root_pageno
  )

let flush t =
  flush_metadata_if_new_root t;
  Page_cache.flush_all t.cache

let load str =
  let fd = openfile ~mode:[O_RDWR;O_CREAT] str in
  let stat = Core_unix.fstat fd in
  let cache = Page_cache.create fd ~size:256 in
  let allocator = Page_allocator.create fd in
  let bptree =
    if Int64.equal stat.st_size Int64.zero then begin
      let _metadata_page = Page_allocator.allocate_page allocator in
      let bptree = Bplustree.create cache allocator in
      Metadata.write_root_pageno cache (Bplustree.root bptree);
      bptree
    end else begin
      let root_pageno = Metadata.read_root_pageno cache in
      Bplustree.load cache allocator root_pageno
    end
  in
  {fd;bptree;cache;allocator; latest_root_pageno = Bplustree.root bptree}

let get t k =
  let cursor = Bplustree.create_cursor t.bptree k in
  match Bplustree.get cursor with
  | None -> None
  | Some pageno ->
    Page_cache.with_page t.cache pageno (fun pg ->
      let buf = Page.underlying_read_only pg in
      Some (Off_heap_buffer.to_bytes buf))
    
let put t k v =
  assert (Bytes.length v <= Page.page_size);
  let cursor = Bplustree.create_cursor t.bptree k in
  let pageno =
    match Bplustree.get cursor with
    | Some pageno -> pageno
    | None ->
      let pageno = Page_allocator.allocate_page t.allocator in
      Bplustree.set cursor pageno;
      flush_metadata_if_new_root t;
      pageno
  in
  Page_cache.with_page t.cache pageno (fun page ->
    let buf = Page.underlying page in
    Off_heap_buffer.blit_from_bytes buf v [@nontail]
  );