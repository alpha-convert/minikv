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
  mutable last_pageno : int;
}

let save_pagetbl_to_page0 t =
  let pg0 = Page.load t.fd 0 in
  List.iteri (Hashtbl.to_alist t.page_tbl) ~f:(fun i (k,pageno) ->
    Page.set_entry pg0 i ~k ~v:pageno
  );
  Page.set_num_entries pg0 (Hashtbl.length t.page_tbl);
  Page.save pg0

let alloc_new_page t = 
  t.last_pageno <- t.last_pageno + 1;
  (Page.alloc_page t.fd t.last_pageno,t.last_pageno)

let seek k pg =
  let rec loop i =
    if i >= Page.num_entries pg then None
    else
      let (~k:k',~v:_) = Page.get_entry pg i in
      if Int.equal k' k then Some i
      else loop (i+1)
  in
  loop 0

let prefill fd = 
  let two_empty_pages = Bytes.make 8192 (Char.of_int_exn 0) in
  ignore (Core_unix.lseek fd 0L ~mode:SEEK_SET);
  ignore (Core_unix.write fd ~buf:two_empty_pages)

let load str = 
  let fd = openfile ~mode:[O_RDWR;O_CREAT] str in
  let stat = Core_unix.fstat fd in
  if Int64.equal stat.st_size 0L then prefill fd;
  let pg0 = Page.load fd 0 in
  let pgtbl_num_entries = Page.num_entries pg0 in
  let page_tbl = Int.Table.create () in
  (* TODO: this assumes that the page table can fit on a single page! fix this in the future. *)
  (for i = 0 to pgtbl_num_entries - 1 do
    let (~k:key,~v:pageno) = Page.get_entry pg0 i in
    Hashtbl.set page_tbl key pageno
  done);
  Printf.printf "Loaded page table: [";
  Hashtbl.iteri page_tbl ~f:(fun ~key ~data -> Printf.printf "key: %d, pageno: %d" key data);
  Printf.printf "]\n";

  let last_pageno = Hashtbl.fold page_tbl ~init:1 ~f:(fun ~key:_ ~data:pageno acc -> Int.max pageno acc) in
  {
    fd;
    page_tbl;
    last_pageno = last_pageno
  }

let get t k =
  match Hashtbl.find t.page_tbl k  with
  | None -> None
  | Some pageno ->
    let pg = Page.load t.fd pageno in
    match seek k pg with
    | None -> failwith "Impossible --- if there's a pageno entry, it should be on that page"
    | Some i -> let (~k:_,~v) = Page.get_entry pg i in Some v


let put t ~k ~v =
  match Hashtbl.find t.page_tbl k  with
  | None ->
    let last_pg = Page.load t.fd t.last_pageno in
    let (pg,pageno,i) =
      if Page.num_entries last_pg < Page.max_num_entries then
        (last_pg,t.last_pageno,Page.num_entries last_pg)
      else
        let pg,pageno = alloc_new_page t in (pg,pageno,0)
    in
      Hashtbl.set t.page_tbl ~key:k ~data:pageno;
      save_pagetbl_to_page0 t; (* This is extremely eager. *)
      Page.set_entry pg i ~k ~v;
      Page.incr_num_entries pg;
      Page.save pg
  | Some pageno ->
      let pg = Page.load t.fd pageno in
      let (Some i) = seek k pg in
      Page.set_entry pg i ~k ~v;
      Page.save pg
