open! Core
open! Core_unix

type slot = { mutable pg : Page.t; mutable seqno : int }

let compare_seqno s s' = Int.compare s.seqno s'.seqno

type t =
  {
    fd : File_descr.t;
    slots : slot option Array.t;
    mutable latest_seqno : int
  }

let flush_all t =
  Array.iter t.slots ~f:(function
  | None -> ()
  | Some {pg;_} -> Page.flush pg)

let create fd ~size =
  {fd ; slots = Array.create ~len:size None; latest_seqno = 0}

let get_victim_or_empty t =
  let first_empty = Array.find_mapi t.slots ~f:(fun i pg_opt ->
      match pg_opt with
      | None -> Some i
      | _ -> None
    )
  in
  match first_empty with
  | Some i -> `Empty i
  | None -> `Victim (Option.value_exn (Array.min_elt (Array.filter_opt t.slots) ~compare:compare_seqno))


let find_slot_of t pageno =
  Array.find_map t.slots ~f:(
    fun pg_opt ->
      match pg_opt with
      | None -> None
      | Some ({pg;_} as slot) -> if Int.equal (Page.pageno pg) pageno then Some slot else None
    )


let with_page t ?(alloc = false) ?(force_flush = false) ~pageno (f : Page.t @ local -> 'a) =
  let slot_opt = find_slot_of t pageno in
  match slot_opt with
  | Some slot ->
      let res = f slot.pg in
      if force_flush then Page.flush slot.pg;
      slot.seqno <- t.latest_seqno;
      t.latest_seqno <- t.latest_seqno + 1;
      res
  | None ->
      let pg = if alloc then Page.alloc_page t.fd pageno else Page.load t.fd pageno in
      (match get_victim_or_empty t with
      | `Empty i -> t.slots.(i) <- Some {pg;seqno = t.latest_seqno}
      | `Victim victim_slot ->
          Page.flush victim_slot.pg;
          victim_slot.pg <- pg;
          victim_slot.seqno <- t.latest_seqno
      );
      t.latest_seqno <- t.latest_seqno + 1;
      let res = f pg in
      if force_flush then Page.flush pg;
      res