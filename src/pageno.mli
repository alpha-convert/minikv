type t [@@deriving sexp, compare, hash, quickcheck]

val to_int : t -> int
val of_int : int -> t
val equal : t -> t -> bool
val succ : t -> t
val max : t -> t -> t