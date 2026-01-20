open! Core

type t = int [@@deriving sexp, compare, hash, quickcheck]

let to_int x = x
let of_int_exn x =
  assert (x >= 0);
  x

let equal = Int.equal
let succ = Int.succ

module Or_null = struct
  type nonrec t = t Or_null.t
  let to_int x =
      match%optional.Or_null x with
      | None -> -1
      | Some x -> x
  
  let of_int x =
    if x < 0 then Null else This x
end