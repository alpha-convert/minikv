open! Core
open! Core_unix

type t = {
  fd : File_descr.t;
  bptree : Bplustree.t;
  cache : Page_cache.t;
  allocator : Page_allocator.t;
  free_list : Free_list.t;
  mutable latest_root_pageno : Pageno.t;
  mutable latest_freelist_head : Pageno.t Or_null.t;
}

(* Metadata page (page 0) layout:
   - offset 0: root_pageno (64-bit little endian)
   - offset 8: freelist_head (64-bit little endian)
   - rest: reserved for future metadata *)
module Metadata = struct
  let metadata_pageno = Pageno.of_int_exn 0
  let root_pageno_offset = 0
  let freelist_head_offset = 8

  let read_metadata page_cache offset = 
    Page_cache.with_page page_cache metadata_pageno (fun page ->
      let page = Page.classify_as_metadata_exn page in
      let buf = Page.underlying_read_only page in
      Pageno.of_int (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:offset)
    )

  let write_metadata page_cache pageno offset =
    Page_cache.with_page page_cache metadata_pageno (fun page ->
      let page = Page.classify_as_metadata_exn page in
      let buf = Page.underlying page in
      Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:offset (Pageno.to_int pageno) [@nontail])

  let read_root_pageno page_cache = Or_null.value_exn (read_metadata page_cache root_pageno_offset)
  let read_freelist_head page_cache = read_metadata page_cache freelist_head_offset


  let write_root_pageno page_cache root_pageno = write_metadata page_cache root_pageno root_pageno_offset
  let write_freelist_head page_cache root_pageno = write_metadata page_cache root_pageno freelist_head_offset
end

let flush_metadata_if_new_root_or_freelist t =
  let root_pageno = Bplustree.root t.bptree in
  if not (Pageno.equal root_pageno t.latest_root_pageno) then (
    t.latest_root_pageno <- root_pageno;
    Metadata.write_root_pageno t.cache root_pageno);
  let freelist_head = Free_list.get_head t.free_list in
  if not (Or_null.equal Pageno.equal freelist_head t.latest_freelist_head) then (
    t.latest_freelist_head <- freelist_head;
    match%optional.Or_null freelist_head with
    | None -> ()
    | Some head -> Metadata.write_freelist_head t.cache head)

let flush t =
  flush_metadata_if_new_root_or_freelist t;
  Page_cache.flush_all t.cache

let load str =
  let fd = openfile ~mode:[O_RDWR;O_CREAT] str in
  let cache = Page_cache.create fd ~size:256 in
  let free_list = Free_list.create cache in
  let allocator = Page_allocator.create free_list fd in
  let bptree,freelist_head =
    let stat = Core_unix.fstat fd in
    if Int64.equal stat.st_size Int64.zero then begin
      let _metadata_page = Page_allocator.allocate_page allocator in
      let bptree = Bplustree.create cache allocator in
      Metadata.write_root_pageno cache (Bplustree.root bptree);
      bptree,Null
    end else begin
      let root_pageno = Metadata.read_root_pageno cache in
      let freelist_head = Metadata.read_freelist_head cache in
      let bptree = Bplustree.load cache allocator root_pageno in
      (bptree,freelist_head)
    end
  in
  (match%optional.Or_null freelist_head with
  | None -> ()
  | Some head -> Free_list.set_head free_list head);
  {fd;bptree;cache;free_list;allocator; latest_root_pageno = Bplustree.root bptree; latest_freelist_head = freelist_head}

let get t k : Bytes.t Or_null.t =
  let cursor = Bplustree.create_cursor t.bptree k in
  let res = Or_null.map (Bplustree.get cursor) ~f:(Data_page.read ~cache:t.cache) in
  flush_metadata_if_new_root_or_freelist t;
  res
    
let put t k v =
  let cursor = Bplustree.create_cursor t.bptree k in
  let pageno =
    match%optional.Or_null Bplustree.get cursor with
    | Some pageno -> pageno
    | None ->
      let pageno = Page_allocator.allocate_page t.allocator in
      Bplustree.set cursor pageno;
      flush_metadata_if_new_root_or_freelist t;
      pageno
  in
  Data_page.write pageno ~cache:t.cache ~allocator:t.allocator ~src:v