open! Core
open! Core_unix

type t

val create : int -> t
val write : File_descr.t -> t @ local -> int
val read : File_descr.t -> t @ local -> int

val unsafe_get_int8 : t @ read local -> pos:int -> int
val unsafe_get_int16_le : t @ read local -> pos:int -> int
val unsafe_get_int64_le_exn : t @ read local -> pos:int -> int

val unsafe_set_int8_exn : t @ local -> pos:int -> int -> unit
val unsafe_set_int16_le_exn : t @ local -> pos:int -> int -> unit
val unsafe_set_int64_le_exn : t @ local -> pos:int -> int -> unit


val to_bytes :  t @ local read -> Bytes.t
val blit_from_bytes : t @ local -> Bytes.t -> unit
val memset : t @ local -> pos:int -> len:int -> char -> unit

