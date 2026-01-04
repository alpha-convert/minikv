open! Core

type t

val load : string -> t
val flush : t -> unit
val get : t -> int -> Bytes.t option
val put : t -> int -> Bytes.t -> unit