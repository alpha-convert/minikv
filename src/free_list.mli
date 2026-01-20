open! Core
type t

val create : unit -> t
val add : t -> Pageno.t -> unit
val take : t -> Pageno.t Or_null.t