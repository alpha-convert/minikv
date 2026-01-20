open! Core
type t

val add : t -> Pageno.t -> unit
val take : t -> Pageno.t Or_null.t

module Expert : sig
    val create : Page_cache.t -> t
    val set_head : t -> Pageno.t -> unit
    val get_head : t -> Pageno.t Or_null.t
end