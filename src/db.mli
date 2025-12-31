type t

val save : t -> unit
val load : string -> t

val get : t -> int -> int option
val put : t -> k:int -> v:int -> unit

val scan : t -> (int * int ) list