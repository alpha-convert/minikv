open! Core
open! Core_unix

type slot = {
  mutable in_use : bool;
  mutable pg : Page.t;
  mutable seqno : int
}

let compare_seqno s s' =
  (* Prefer clean pages over dirty pages for eviction *)
  match (Page.is_dirty s.pg, Page.is_dirty s'.pg) with
  | (false, true) -> -1
  | (true, false) -> 1
  | _ -> Int.compare s.seqno s'.seqno

type t =
  {
    fd : File_descr.t;
    slots : slot option Array.t;
    mutable latest_seqno : int
  }

let next_seqno t =
  let a = t.latest_seqno in
  t.latest_seqno <- a + 1;
  a

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
  | None ->
      let available_slots = Array.filter_opt t.slots |> Array.filter ~f:(fun slot -> not slot.in_use) in
      match Array.min_elt available_slots ~compare:compare_seqno with
      | Some victim -> `Victim victim
      | None -> failwith "No available slots."


let cache_lookup t pageno =
  Array.find_map t.slots ~f:(
    fun pg_opt ->
      match pg_opt with
      | None -> None
      | Some ({pg;_} as slot) ->
        if Pageno.equal (Page.pageno pg) pageno then Some slot else None
    )

let with_page t ?(force_flush = false) pageno (f : (Page.t @ local -> 'a) @ local) =
  let slot =
    match cache_lookup t pageno with
    | Some slot -> slot.seqno <- next_seqno t; slot
    | None ->
      (match get_victim_or_empty t with
       | `Empty i ->
          let pg = Page.create t.fd pageno in
          Printf.printf "loading pageno: %d" (Pageno.to_int pageno);
          Page.load pg pageno;
          let slot = {pg;seqno = next_seqno t; in_use = true} in
          t.slots.(i) <- Some slot;
          slot
       | `Victim victim_slot ->
          Page.flush victim_slot.pg;
          Page.load victim_slot.pg pageno;
          victim_slot.seqno <- next_seqno t;
          victim_slot)
  in
  slot.in_use <- true;
  let res = f slot.pg in
  slot.in_use <- false;
  if force_flush then Page.flush slot.pg;
  res