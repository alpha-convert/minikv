open! Core
open! Core_unix
(* minidb.ml - Stupidly simple key-value store
   
   No pages, no buffer pool, no WAL.
   Just an in-memory hashtable that serializes to disk.
   
   Your tasks:
   1. Add paging (fixed-size blocks on disk)
   2. Add a buffer pool (cache pages in memory)
   3. Add WAL (log before modifying pages)
*)

type t = {
  fd : File_descr.t;
  page_tbl : (int,int) Hashtbl.t;
  mutable next_pageno : int;
}

let alloc_new_page t = 
  let pageno = t.next_pageno in
  t.next_pageno <- pageno + 1;
  (Page.alloc_page t.fd pageno,pageno)


(* Serialize: write each record as "key\tvalue\n" *)
let save t = ()
  (* let oc = open_out t.path in
  Hashtbl.iteri ~f:(fun ~key ~data -> Printf.fprintf oc "%d\t%s\n" key data) t.data;
  close_out oc
 *)

let load str = 
  let fd = openfile ~mode:[O_RDWR] str in
  (* TODO: actually load this data from page 0 *)
  {
    fd;
    page_tbl = Int.Table.create ();
    next_pageno = 0
  }



let get t k =
  let rec loop pg i num_entries =
    if i >= num_entries then None
    else
      let (~k:k',~v) = Page.get_entry pg i in
      if Int.equal k' k then Some v
      else loop pg (i+1) num_entries
  in
  match Hashtbl.find t.page_tbl k  with
  | None -> None
  | Some pageno ->
    let pg = Page.load t.fd pageno in
    loop pg 0 (Page.num_entries pg)


let put t ~k ~v =
  let rec loop pg i num_entries =
    if i >= num_entries then 
      (* For now, we have the DB behavior be ``just silently fail to write if there's no space on the designated page *)
      if i >= Page.max_num_entries then ()
      else
        (Page.set_entry pg i ~k ~v;
        Page.incr_num_entries pg)
    else
      let (~k:k',~v:_) = Page.get_entry pg i in
      if Int.equal k' k then Page.set_entry pg i ~k ~v
      else loop pg (i+1) num_entries
  in
  let pg =
    match Hashtbl.find t.page_tbl k  with
    | None ->
      let pg,pageno = alloc_new_page t in
      Hashtbl.set t.page_tbl ~key:k ~data:pageno;
      pg
    | Some pageno -> Page.load t.fd pageno
  in
    loop pg 0 (Page.num_entries pg);
    Page.save pg

let scan t = []
  (* Hashtbl.to_alist t.data *)
