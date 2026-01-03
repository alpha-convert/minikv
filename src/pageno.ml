open! Core

type t = int [@@deriving sexp, compare, hash, quickcheck]

let zero = 0
let to_int x = x
let of_int x = x
let equal = Int.equal
let succ = Int.succ
let max = Int.max