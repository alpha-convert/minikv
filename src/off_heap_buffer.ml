open! Core
open! Core_unix

type t = Bigstring_unix.t

let unsafe_get_int8 (buf : t @ read local) ~pos =
  Char.to_int (Bigstring.unsafe_get (Obj.magic Obj.magic buf) pos)

let unsafe_get_int16_le (buf : t @ read local) ~pos =
  Bigstring.unsafe_get_int16_le (Obj.magic Obj.magic buf) ~pos

let unsafe_get_int64_le_exn (buf : t @ read local) ~pos =
  Bigstring.unsafe_get_int64_le_exn (Obj.magic Obj.magic buf) ~pos

let unsafe_set_int8_exn (buf : t @ local) ~pos value =
  Bigstring.unsafe_set (Obj.magic Obj.magic buf) pos (Char.of_int_exn value)

let unsafe_set_int16_le_exn (buf : t @ local) ~pos value =
  Bigstring.unsafe_set_int16_le (Obj.magic Obj.magic buf) ~pos value

let unsafe_set_int64_le_exn (buf : t @ local) ~pos value =
  Bigstring.unsafe_set_int64_le (Obj.magic Obj.magic buf) ~pos value

let read fd buf = Bigstring_unix.read fd (Obj.magic Obj.magic buf)

let to_bytes buf = Bigstring.to_bytes (Obj.magic Obj.magic buf)

let blit_from_bytes buf bytes =
  Bigstring.From_bytes.blit ~src:bytes ~dst:(Obj.magic Obj.magic buf) ~src_pos:0 ~dst_pos:0 ~len:(Bytes.length bytes)