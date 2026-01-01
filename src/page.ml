open! Core
open! Core_unix

let page_size = 4096




type t = {
  fd : File_descr.t;
  mutable pageno : int;
  raw : Bigstring.t;
  mutable dirty : bool
}

let underlying t = t.dirty <- true; t.raw
let underlying_read_only t = t.raw

let pageno pg = pg.pageno
let is_dirty pg = pg.dirty

let page_pos pageno = pageno * page_size

let seek_to_page fd pageno =
  ignore (lseek fd (Int64.of_int (page_pos pageno)) ~mode:SEEK_SET)

let load t ~pageno =
  let buf = t.raw in
  seek_to_page t.fd pageno;
  let num_read = Bigstring_unix.read t.fd buf in
  assert (Int.equal num_read page_size);
  t.pageno <- pageno;
  t.dirty <- false

let clear_bytes t =
  Bigstring_unix.memset t.raw ~pos:0 ~len:page_size (Char.of_int_exn 0)

let set_pageno t pageno =
  t.pageno <- pageno

let flush (pg @ local) =
  if pg.dirty then begin
    seek_to_page pg.fd pg.pageno;
    let num_written = write pg.fd ~buf:(Obj.magic Obj.magic pg.raw) in
    assert (Int.equal num_written page_size);
    pg.dirty <- false
  end

let create fd pageno =
  let raw = Bigstring_unix.create 4096 in
  Bigstring_unix.memset raw ~pos:0 ~len:page_size (Char.of_int_exn 0);
  let pg = {fd;pageno;raw; dirty = true} in
  pg

