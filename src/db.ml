open! Core
open! Core_unix

type t = {
  fd : File_descr.t;
  page_tbl : (int,int) Hashtbl.t;
  page_cache : Page_cache.t;
  mutable last_pageno : int;
}

let unsafe_get_int64_le_exn (buf : Bigstring.t @ read) ~pos = Bigstring.unsafe_get_int64_le_exn (Obj.magic Obj.magic buf) ~pos

(**
The format of a page is:
N, a number of entries

K (as a 64-bit integer, little-endian)
V (as a 64-bit integer, little-endian)
repeated, N times.
*)

let max_num_entries = 255

let num_entries (buf @ read) = 
  unsafe_get_int64_le_exn buf ~pos:0

let set_num_entries buf n = 
  Bigstring.unsafe_set_int64_le buf ~pos:0 n

let incr_num_entries buf = 
  let num_entries = Bigstring.unsafe_get_int64_le_exn buf ~pos:0 in
  Bigstring.unsafe_set_int64_le buf~pos:0 (num_entries + 1)

let get_entry (buf @ read) i =
  assert Int.(i < 512);
  let k = unsafe_get_int64_le_exn buf ~pos:(16*i + 8) in
  let v = unsafe_get_int64_le_exn buf ~pos:(16*i + 16) in
  (~k,~v)

let set_entry buf i ~k ~v  =
  assert Int.(i < 512);
  Bigstring.unsafe_set_int64_le buf ~pos:(16*i + 8) k;
  Bigstring.unsafe_set_int64_le buf  ~pos:(16*i + 16) v

(* NOTE: this is kinda sketchy. The page cache manages writing back all pages except this one,w hich we maintain a separate copy of an occasionally re-write back. it's a little sketchy. Maybe we should modify the page cache to maintain a constant copy of page 0.*)
let save_pagetbl_to_page0 t =
  Page_cache.with_page ~force_flush:true ~pageno:0 t.page_cache (fun pg0 ->
    let buf = Page.underlying pg0 in
    List.iteri (Hashtbl.to_alist t.page_tbl) ~f:(fun i (k,pageno) ->
      set_entry buf i ~k ~v:pageno
    );
    set_num_entries buf (Hashtbl.length t.page_tbl); ()
  )

let flush t = 
  Page_cache.flush_all t.page_cache;
  save_pagetbl_to_page0 t

let alloc_new_page t f = 
  t.last_pageno <- t.last_pageno + 1;
  Page_cache.with_page t.page_cache ~new_page:true ~pageno:t.last_pageno f

let seek k (buf @ read) =
  let rec loop i =
    if i >= num_entries buf then None
    else
      let (~k:k',~v:_) = get_entry buf i in
      if Int.equal k' k then Some i
      else loop (i+1)
  in
  let res = loop 0 in res

let prefill fd = 
  let two_empty_pages = Bytes.make 8192 (Char.of_int_exn 0) in
  ignore (Core_unix.lseek fd 0L ~mode:SEEK_SET);
  ignore (Core_unix.write fd ~buf:two_empty_pages)

let load str = 
  let fd = openfile ~mode:[O_RDWR;O_CREAT] str in
  let stat = Core_unix.fstat fd in
  if Int64.equal stat.st_size 0L then prefill fd;
  let page_cache = Page_cache.create fd ~size:4 in
  Page_cache.with_page page_cache ~pageno:0 (fun pg0 ->
    let buf0 = Page.underlying pg0 in
    let pgtbl_num_entries = num_entries buf0 in
    let page_tbl = Int.Table.create () in
    (* TODO: this assumes that the page table can fit on a single page! fix this in the future. *)
    (for i = 0 to pgtbl_num_entries - 1 do
      let (~k:key,~v:pageno) = get_entry buf0 i in
      Hashtbl.set page_tbl ~key ~data:pageno
    done);
    let last_pageno = Hashtbl.fold page_tbl ~init:1 ~f:(fun ~key:_ ~data:pageno acc -> Int.max pageno acc) in
    {
      fd;
      page_tbl;
      page_cache;
      last_pageno = last_pageno
    }
  )

let get t k =
  match Hashtbl.find t.page_tbl k  with
  | None -> None
  | Some pageno ->
    Page_cache.with_page t.page_cache ~pageno (fun pg ->
      let buf = Page.underlying_read_only pg in
      match seek k buf with
      | None -> failwith "Impossible --- if there's a pageno entry, it should be on that page"
      | Some i -> let (~k:_,~v) = get_entry buf i in Some v
    )

let put t ~k ~v =
  match Hashtbl.find t.page_tbl k  with
  | None ->
    Page_cache.with_page t.page_cache ~pageno:t.last_pageno (
      fun last_pg ->
        let buf = Page.underlying last_pg in
        if num_entries buf < max_num_entries then
          let i = num_entries buf in
          Hashtbl.set t.page_tbl ~key:k ~data:t.last_pageno;
          set_entry buf i ~k ~v;
          incr_num_entries buf; ()
        else
          alloc_new_page t (fun pg ->
            let buf = Page.underlying pg in
            Hashtbl.set t.page_tbl ~key:k ~data:(Page.pageno pg);
            set_entry buf 0 ~k ~v;
            incr_num_entries buf; ()
          )
    )
  | Some pageno ->
      Page_cache.with_page t.page_cache ~pageno (fun pg ->
        let buf = Page.underlying pg in
        let (Some i) = seek k buf in
        set_entry buf i ~k ~v; ()
      )
      
