open! Core_unix
type t

val create : File_descr.t -> size:int -> t

val with_page : t -> ?alloc:bool -> ?force_flush:bool -> pageno:int -> (Page.t @ local -> 'a) @ local -> 'a

val flush_all : t -> unit