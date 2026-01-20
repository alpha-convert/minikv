type t

val create : Free_list.t -> Core_unix.File_descr.t -> t
(** NOTE: Page may contain garbage! *)
val allocate_page : t -> Pageno.t