open! Core
open! Core_unix

type t = {
  fd : File_descr.t;
  mutable last_pageno : Pageno.t;
}

let create fd =
  let sz = Int64.to_int_exn (fstat fd).st_size in
  {
    fd;
    last_pageno = Pageno.of_int (sz / Page.page_size - 1)
  }

let last_pageno t = t.last_pageno

let allocate_page t : Pageno.t =
  t.last_pageno <- Pageno.succ t.last_pageno;
  let sz = (fstat t.fd).st_size in
  ftruncate t.fd Int64.(sz + (Int64.of_int Page.page_size));
  t.last_pageno

