open! Core

type t

val of_page : Page.t @ local -> t @ local
val read : t @ local -> Bytes.t
val write : t @ local -> Bytes.t -> unit