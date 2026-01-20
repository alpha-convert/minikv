type t = Pageno.t Vec.t

let create () = Vec.create ()

let add t p = Vec.push_back t p
let take t = Vec.pop_back t