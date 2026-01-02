open! Core
open! Core_unix

type t

val page_size : int

val load : t -> Pageno.t -> unit

val set_pageno : t -> Pageno.t -> unit
val clear_bytes : t -> unit

val flush : t @ local -> unit

val pageno : t @ local -> Pageno.t
val is_dirty : t @ local -> bool

val create : File_descr.t -> Pageno.t -> t

val underlying_read_only : t @ local -> OffHeapBuffer.t @ local read
val underlying : t @ local -> OffHeapBuffer.t @ local
