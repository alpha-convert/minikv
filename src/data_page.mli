open! Core

type t

val create : Pageno.t -> Page_cache.t -> Page_allocator.t -> t
val read : t -> Bytes.t
val write : t -> Bytes.t -> unit
