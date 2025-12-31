open! Core
open! Core_unix

type t

val load : t -> pageno:int -> unit

val set_pageno : t -> int -> unit
val clear_bytes : t -> unit

val flush : t @ local -> unit

val pageno : t @ local -> int
val is_dirty : t @ local -> bool

val create : File_descr.t -> int -> t

val max_num_entries : int
val num_entries : t @ local -> int
val set_num_entries : t @ local -> int -> unit
val incr_num_entries : t @ local -> unit
val get_entry : t @ local -> int -> (k:int*v:int)
val set_entry : t @ local -> int -> k:int -> v:int -> unit
