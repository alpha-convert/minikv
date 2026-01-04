type t

val create : Core_unix.File_descr.t -> t
val allocate_page : t -> Pageno.t
(* val last_pageno : t -> Pageno.t *)