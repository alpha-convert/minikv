open! Core
open! Core_unix

let unsafe_get_int64_le_exn (buf : Bigstring.t @ read) ~pos = Bigstring.unsafe_get_int64_le_exn (Obj.magic Obj.magic buf) ~pos

module Internal = struct
  type t = Page.t

  let num_keys t =
    let buf = Page.underlying_read_only t in
    let res = unsafe_get_int64_le_exn buf 8 in
    res

end