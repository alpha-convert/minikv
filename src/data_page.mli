open! Core

val read : Pageno.t -> cache:Page_cache.t -> Bytes.t
val write : Pageno.t -> cache:Page_cache.t -> allocator:Page_allocator. t -> src:Bytes.t -> unit
