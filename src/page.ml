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
  raw : Bytes.t
}

let page_pos pageno = pageno * page_size


let load fd pageno =
  let raw = Bytes.create 4096 in
  let num_read = read fd ~pos:(page_pos pageno) ~len:page_size ~buf:raw in
  assert (Int.equal num_read page_size);
  { fd ; pageno; raw }

let save pg =
  let num_written = write pg.fd ~pos:(page_pos pg.pageno) ~len:page_size ~buf:pg.raw in
  assert (Int.equal num_written page_size)

let alloc_page fd pageno =
  (* just write zero bytes into this region. *)
  let raw = Bytes.make 4096 (Char.of_int_exn 0) in
  let pg = {fd;pageno;raw} in
  save pg;
  pg

let num_entries pg = 
  Int64.to_int_trunc (Bytes.unsafe_get_int64 pg.raw 0)

let incr_num_entries pg = 
  let num_entries = Int64.to_int_trunc (Bytes.unsafe_get_int64 pg.raw 0) in
  Bytes.unsafe_set_int64 pg.raw 0 (Int64.of_int (num_entries + 1))

let get_entry pg i =
  assert Int.(i < 512);
  let k = Int64.to_int_trunc (Bytes.unsafe_get_int64 pg.raw (16*i + 8)) in
  let v = Int64.to_int_trunc (Bytes.unsafe_get_int64 pg.raw (16*i + 16)) in
  (~k,~v)

let set_entry pg i ~k ~v  =
  assert Int.(i < 512);
  Bytes.unsafe_set_int64 pg.raw (16*i + 8) (Int64.of_int k);
  Bytes.unsafe_set_int64 pg.raw (16*i + 16) (Int64.of_int v)