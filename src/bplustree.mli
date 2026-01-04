type t
val create : Page_cache.t -> Page_allocator.t -> t
val load : Page_cache.t -> Page_allocator.t -> Pageno.t -> t

val lookup : t -> int -> Pageno.t option
val insert : t -> int -> Pageno.t -> unit

val root : t -> Pageno.t