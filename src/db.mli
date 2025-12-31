type t

val load : string -> t

val get : t -> int -> int option
val put : t -> k:int -> v:int -> unit