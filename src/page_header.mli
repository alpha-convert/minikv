type metadata_page
type bplustree_internal
type bplustree_leaf
type data_packed
type data_linked

type 'a t =
| Metadata_header : metadata_page t
| Bplustree_internal_header : bplustree_internal t
| Bplustree_leaf_header : bplustree_leaf t
| Data_packed_header : data_packed t
| Data_linked_header : data_linked t

val size : int

val write_to_page : 'a t -> Off_heap_buffer.t @ local -> unit
val read_from_page : Off_heap_buffer.t @ local -> 'a t