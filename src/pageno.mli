open! Core
type t : value mod everything [@@deriving sexp, compare, hash, quickcheck]

val to_int : t -> int
val of_int_exn : int -> t
val equal : t -> t -> bool
val succ : t -> t

module Or_null : sig
    type nonrec t = t Or_null.t
    val to_int : t -> int
    val of_int : int -> t
end