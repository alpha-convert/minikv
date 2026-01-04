open! Core

type t

val load : string -> t
val flush : t -> unit
val get : t -> int -> Bytes.t Or_null.t
val put : t -> int -> Bytes.t -> unit