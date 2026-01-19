(*
Data page format:
2 bytes of length,
then page_size - 2 bytes of data
*)
type t = {
  mutable len : int;
  pg : Page.t
}

let header_pos = 0
let data_start_pos = 2

let max_data_size = Page.page_size - data_start_pos

let of_page (pg @ local) = exclave_
  let buf = Page.underlying_read_only pg in
  let len = Off_heap_buffer.unsafe_get_int16_le buf ~pos:header_pos in
  {len ; pg}

let read t =
  let buf = Page.underlying_read_only t.pg in
  Off_heap_buffer.to_bytes buf ~pos:data_start_pos ~len:t.len [@nontail]

let write t bytes =
  let bytes_len = Bytes.length bytes in
  assert (bytes_len <= max_data_size);
  let buf = Page.underlying t.pg in
  Off_heap_buffer.blit_from_bytes buf bytes ~src_pos:0 ~dst_pos:data_start_pos ~len:bytes_len;
  Off_heap_buffer.unsafe_set_int16_le_exn buf ~pos:header_pos bytes_len;
  t.len <- bytes_len


