open! Core
open! Core_unix

type t = {
  fd : File_descr.t;
  mutable last_pageno : Pageno.t Or_null.t;
}

let create fd =
  let sz = Int64.to_int_exn (fstat fd).st_size in
  {
    fd;
    last_pageno = Pageno.of_int (sz / Page.page_size - 1)
  }


let allocate_page t : Pageno.t =
  let pageno =
    match%optional (t.last_pageno : Pageno.t Or_null.t) with
    | None -> Pageno.of_int_exn 0
    | Some pageno -> Pageno.succ pageno
  in
  t.last_pageno <- This pageno;
  let sz = (fstat t.fd).st_size in
  ftruncate t.fd ~len:Int64.(sz + (Int64.of_int Page.page_size));
  (* Write zeros to the newly allocated page *)
  let zero_page = Bigstring.create Page.page_size in
  Bigstring.memset zero_page ~pos:0 ~len:Page.page_size (Char.of_int_exn 0);
  ignore (lseek t.fd (Int64.of_int (Pageno.to_int pageno * Page.page_size)) ~mode:SEEK_SET);
  let num_written = Bigstring_unix.write t.fd zero_page in
  assert (Int.equal num_written Page.page_size);
  pageno