open! Core

type t = int [@@deriving sexp, compare, hash, quickcheck]

let to_int x = x
let of_int_exn x =
  assert (x >= 0);
  x

let of_int x =
  if x < 0 then Null else This x

let equal = Int.equal
let succ = Int.succ