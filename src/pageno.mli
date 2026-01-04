open! Core
type t : value mod everything [@@deriving sexp, compare, hash, quickcheck]

val to_int : t -> int
val of_int_exn : int -> t
val of_int : int -> t Or_null.t
val equal : t -> t -> bool
val succ : t -> t