open! Core_unix
type t

val create : File_descr.t -> size:int -> t
val with_page : ('a : value_or_null). t -> Pageno.t -> (Page.t @ local -> 'a) @ local -> 'a
val flush_all : t -> unit