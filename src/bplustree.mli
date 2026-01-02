type t

val lookup : t -> Page_cache.t -> int -> Pageno.t option
val insert : t -> Page_cache.t -> Page_allocator.t -> key:int -> value:Pageno.t -> t