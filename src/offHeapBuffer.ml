open! Core
open! Core_unix

type t = Bigstring_unix.t

let unsafe_get_int8 (buf : t @ read local) ~pos =
  Char.to_int (Bigstring.unsafe_get (Obj.magic Obj.magic buf) pos)

let unsafe_get_int16_le (buf : t @ read local) ~pos =
  Bigstring.unsafe_get_int16_le (Obj.magic Obj.magic buf) ~pos

let unsafe_get_int64_le_exn (buf : t @ read local) ~pos =
  Bigstring.unsafe_get_int64_le_exn (Obj.magic Obj.magic buf) ~pos

let unsafe_set_int16_le (buf : t @ local) ~pos value =
  Bigstring.unsafe_set_int16_le (Obj.magic Obj.magic buf) ~pos value

let unsafe_set_int64_le_exn (buf : t @ local) ~pos value =
  Bigstring.unsafe_set_int64_le (Obj.magic Obj.magic buf) ~pos value

let read fd buf = Bigstring_unix.read fd (Obj.magic Obj.magic buf)