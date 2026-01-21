open! Core

type t
val create : Page_cache.t -> Page_allocator.t -> t
val load : Page_cache.t -> Page_allocator.t -> Pageno.t -> t
val root : t -> Pageno.t

type cursor
val create_cursor : t -> int -> cursor
val get : cursor -> Pageno.t Or_null.t
val set : cursor -> Pageno.t -> unit
val seek : cursor -> int -> unit

module Valid : sig
  val check : t -> unit
end