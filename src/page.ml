open! Core
open! Core_unix

let page_size = 16384
(* let page_size = 4096 *)

type t = {
  raw : Off_heap_buffer.t;
  fd : File_descr.t;
  mutable pageno : Pageno.t;
  mutable dirty : bool
}

let underlying t = t.dirty <- true; t.raw
let underlying_read_only t = t.raw
let pageno pg = pg.pageno
let is_dirty pg = pg.dirty

let page_pos pageno = Pageno.to_int pageno * page_size

let seek_to_page fd pageno =
  ignore (lseek fd (Int64.of_int (page_pos pageno)) ~mode:SEEK_SET)

let load t pageno =
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

let create fd pageno =
  let raw = Off_heap_buffer.create page_size in
  Off_heap_buffer.memset raw ~pos:0 ~len:page_size (Char.of_int_exn 0);
  {fd;pageno;raw; dirty = false}

