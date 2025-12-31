open! Core
open! Core_unix

let page_size = 4096

let max_num_entries = 255

(**

The format of a page is:
N, a number of entries

K (as a 64-bit integer, little-endian)
V (as a 64-bit integer, little-endian)
repeated, N times.
*)

type t = {
  fd : File_descr.t;
  pageno : int;
  raw : Bytes.t;
  mutable dirty : bool
}

let pageno pg = pg.pageno

let is_dirty pg = pg.dirty

let page_pos pageno = pageno * page_size

let seek_to_page fd pageno =
  ignore (lseek fd (Int64.of_int (page_pos pageno)) ~mode:SEEK_SET)

let load fd pageno =
  let raw = Bytes.create 4096 in
  seek_to_page fd pageno;
  let num_read = read fd ~buf:raw in
  assert (Int.equal num_read page_size);
  { fd ; pageno; raw; dirty = false }

let flush (pg @ local) =
  if pg.dirty then (
    seek_to_page pg.fd pg.pageno;
    let num_written = write pg.fd ~buf:(Obj.magic Obj.magic pg.raw) in
    assert (Int.equal num_written page_size);
    pg.dirty <- false
  )

let alloc_page fd pageno =
  let raw = Bytes.make 4096 (Char.of_int_exn 0) in
  let pg = {fd;pageno;raw; dirty = false} in
  pg

let num_entries pg = 
  Int64.to_int_trunc (Bytes.unsafe_get_int64 pg.raw 0)

let set_num_entries pg n = 
  Bytes.unsafe_set_int64 pg.raw 0 (Int64.of_int n);
  pg.dirty <- true

let incr_num_entries pg = 
  let num_entries = Int64.to_int_trunc (Bytes.unsafe_get_int64 pg.raw 0) in
  Bytes.unsafe_set_int64 pg.raw 0 (Int64.of_int (num_entries + 1));
  pg.dirty <- true

let get_entry pg i =
  assert Int.(i < 512);
  let k = Int64.to_int_trunc (Bytes.unsafe_get_int64 pg.raw (16*i + 8)) in
  let v = Int64.to_int_trunc (Bytes.unsafe_get_int64 pg.raw (16*i + 16)) in
  (~k,~v)

let set_entry pg i ~k ~v  =
  assert Int.(i < 512);
  Bytes.unsafe_set_int64 pg.raw (16*i + 8) (Int64.of_int k);
  Bytes.unsafe_set_int64 pg.raw (16*i + 16) (Int64.of_int v);
  pg.dirty <- true