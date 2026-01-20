open! Core
open! Core_unix

let page_size = 16384

type _ t = {
  raw : Off_heap_buffer.t;
  fd : File_descr.t;
  mutable pageno : Pageno.t;
  mutable dirty : bool
}

type packed = | P : _ t -> packed [@@unboxed]

type classified = | C : #('a Page_header.t * 'a t) -> classified

let underlying t = t.dirty <- true; t.raw [@@inline always]
let underlying_read_only (t @ local) = t.raw [@@inline always]
let pageno pg = pg.pageno
let is_dirty pg = pg.dirty

let page_pos pageno = Pageno.to_int pageno * page_size

let seek_to_page fd pageno =
  ignore (lseek fd (Int64.of_int (page_pos pageno)) ~mode:SEEK_SET)

let overwrite_with (P t) pageno =
  let buf = t.raw in
  seek_to_page t.fd pageno;
  let num_read = Off_heap_buffer.read t.fd buf in
  assert (Int.equal num_read page_size);
  t.pageno <- pageno;
  t.dirty <- false

let set_pageno t pageno =
  t.pageno <- pageno

let flush (pg @ local) =
  if pg.dirty then begin
    seek_to_page pg.fd pg.pageno;
    let num_written = Off_heap_buffer.write pg.fd pg.raw in
    assert (Int.equal num_written page_size);
    pg.dirty <- false
  end

let create fd pageno header =
  let raw = Off_heap_buffer.create page_size in
  Off_heap_buffer.memset raw ~pos:0 ~len:page_size (Char.of_int_exn 0);
  Page_header.write_to_page header raw;
  {fd;pageno;raw;dirty = false}


(* TODO: figure out how to do these without magics *)
let classify ((P pg) @ local) @ local = exclave_
  let buf = underlying_read_only pg in
  let tag @ local = Page_header.read_from_page buf in
  C #(tag,Obj.magic pg)

let classify_as_data_linked_exn page @ local = exclave_
  match classify page with
  | C #(Page_header.Data_linked_header,pg) -> Obj.magic pg
  | _ -> assert false

let classify_as_data_packed_exn page @ local = exclave_
  match classify page with
  | C #(Page_header.Data_packed_header,pg) -> Obj.magic pg
  | _ -> assert false

let classify_as_metadata_exn page @ local = exclave_
  match classify page with
  | C #(Page_header.Metadata_header,pg) -> Obj.magic pg
  | _ -> assert false

let classify_as_bplustree_exn page @ local = exclave_
  match classify page with
  | C #(Page_header.Bplustree_internal_header, pg) -> Either.First (Obj.magic pg)
  | C #(Page_header.Bplustree_leaf_header, pg) -> Either.Second (Obj.magic pg)
  | _ -> assert false

let classify_as_bplustree_leaf_exn page @ local = exclave_
  match classify page with
  | C #(Page_header.Bplustree_leaf_header, pg) -> Obj.magic pg
  | _ -> assert false

let classify_as_bplustree_internal_exn page @ local = exclave_
  match classify page with
  | C #(Page_header.Bplustree_internal_header, pg) -> Obj.magic pg
  | _ -> assert false

let classify_as_free_page_exn page @ local = exclave_
  match classify page with
  | C #(Page_header.Free_page_header, pg) -> Obj.magic pg
  | _ -> assert false

let overwrite_as (type a) (header : a Page_header.t) ((P pg) @ local) : a t @ local = exclave_
  let buf = underlying pg in
  Page_header.write_to_page header buf;
  Obj.magic pg

let overwrite_as_bplustree_internal page @ local = exclave_
  overwrite_as Page_header.Bplustree_internal_header page

let overwrite_as_bplustree_leaf page @ local = exclave_
  overwrite_as Page_header.Bplustree_leaf_header page

let overwrite_as_data_linked page @ local = exclave_
  overwrite_as Page_header.Data_linked_header page

let overwrite_as_data_packed page @ local = exclave_
  overwrite_as Page_header.Data_packed_header page

let overwrite_as_metadata page @ local = exclave_
  overwrite_as Page_header.Metadata_header page

let overwrite_as_free_page page @ local = exclave_
  overwrite_as Page_header.Free_page_header page
