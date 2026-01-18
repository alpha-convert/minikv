open! Core
open! Core_unix

type t
val create : File_descr.t -> Pageno.t -> t
val load : t -> Pageno.t -> unit

val page_size : int

val set_pageno : t -> Pageno.t -> unit
val pageno : t @ local -> Pageno.t
val is_dirty : t @ local -> bool

val flush : t @ local -> unit


val underlying_read_only : t @ local -> Off_heap_buffer.t @ local read
val underlying : t @ local -> Off_heap_buffer.t @ local
