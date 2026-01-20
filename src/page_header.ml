open! Core
type metadata_page
type bplustree_internal
type bplustree_leaf
type data_packed
type data_linked
type free_page

type 'a t =
| Metadata_header : metadata_page t
| Bplustree_internal_header : bplustree_internal t
| Bplustree_leaf_header : bplustree_leaf t
| Data_packed_header : data_packed t
| Data_linked_header : data_linked t
| Free_page_header : free_page t

let size = 8

let write_to_page x (buf @ local) =
  Off_heap_buffer.unsafe_set_int64_le_exn buf ~pos:0 (Obj.magic x)

let read_from_page (buf @ local) : 'a t =
  Obj.magic (Off_heap_buffer.unsafe_get_int64_le_exn buf ~pos:0)

