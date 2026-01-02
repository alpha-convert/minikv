open! Core
open! Core_unix

type t = Bigstring_unix.t

val unsafe_get_int8 : t @ read local -> pos:int -> int
val unsafe_get_int16_le : t @ read local -> pos:int -> int
val unsafe_get_int64_le_exn : t @ read local -> pos:int -> int

val unsafe_set_int16_le : t @ local -> pos:int -> int -> unit
val unsafe_set_int64_le_exn : t @ local -> pos:int -> int -> unit

val read : File_descr.t -> t @ local -> int