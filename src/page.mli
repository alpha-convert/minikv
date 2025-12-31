open! Core
open! Core_unix

type t

val load : File_descr.t -> int -> t
val save : t -> unit

val alloc_page : File_descr.t -> int -> t

val max_num_entries : int
val num_entries : t -> int
val incr_num_entries : t -> unit
val get_entry : t -> int -> (k:int*v:int)
val set_entry : t -> int -> k:int -> v:int -> unit
