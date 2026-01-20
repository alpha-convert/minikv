open! Core
open! Core_unix

type 'a t : value mod non_float
type packed = | P : 'a t -> packed [@@unboxed]
type classified = | C : #('a Page_header.t * 'a t) -> classified
val create : File_descr.t -> Pageno.t -> 'a Page_header.t -> 'a t
val overwrite_with : packed -> Pageno.t -> unit

val classify : packed @ local -> classified @ local
val classify_as_data_linked_exn : packed @ local -> Page_header.data_linked t @ local
val classify_as_data_packed_exn : packed @ local -> Page_header.data_packed t @ local
val classify_as_metadata_exn : packed @ local -> Page_header.metadata_page t @ local
val classify_as_bplustree_exn : packed @ local -> (Page_header.bplustree_internal t, Page_header.bplustree_leaf t) Either.t @ local
val classify_as_bplustree_leaf_exn : packed @ local -> Page_header.bplustree_leaf t @ local
val classify_as_bplustree_internal_exn : packed @ local -> Page_header.bplustree_internal t @ local
val classify_as_free_page_exn : packed @ local -> Page_header.free_page t @ local

val overwrite_as_bplustree_internal : packed @ local -> Page_header.bplustree_internal t @ local
val overwrite_as_bplustree_leaf : packed @ local -> Page_header.bplustree_leaf t @ local
val overwrite_as_data_linked : packed @ local -> Page_header.data_linked t @ local
val overwrite_as_data_packed : packed @ local -> Page_header.data_packed t @ local
val overwrite_as_metadata : packed @ local -> Page_header.metadata_page t @ local
val overwrite_as_free_page : packed @ local -> Page_header.free_page t @ local

val page_size : int

val pageno : 'a t @ local -> Pageno.t
val is_dirty : 'a t @ local -> bool
val flush : 'a t @ local -> unit

val underlying_read_only : 'a t @ local -> Off_heap_buffer.t @ local read
val underlying : 'a t @ local -> Off_heap_buffer.t @ local
