type t

val lookup : t -> Page_cache.t -> int -> Pageno.t option
val insert : t -> Page_cache.t -> key:int -> value:Pageno.t -> t