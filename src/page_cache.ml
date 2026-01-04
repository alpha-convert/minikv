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
    slots : slot Uniform_array.t;
    mutable latest_seqno : int;
    pageno_to_slot : (Pageno.t, int) Hashtbl.t
  }

let next_seqno t =
  let a = t.latest_seqno in
  t.latest_seqno <- a + 1;
  a

let flush_all t =
  Uniform_array.iter t.slots ~f:(function {pg;_} -> Page.flush pg)

let create fd ~size =
  let dummy_slot _ =
    let dummy_pg = Page.create fd (Pageno.of_int_exn Int.max_value) in
    { in_use = false; pg = dummy_pg; seqno = -1 }
  in
  {
    fd;
    slots = Uniform_array.init size ~f:dummy_slot ;
    latest_seqno = 0;
    pageno_to_slot = Hashtbl.create (module Pageno)
  }

let evict t =
  let res =
    Uniform_array.foldi t.slots ~init:None ~f:(fun i best slot ->
    if slot.in_use then best
    else
      match best with
      | None -> Some (i, slot)
      | Some (_, best_slot) ->
          if compare_seqno slot best_slot < 0
          then Some (i,slot)
          else best)
  in
  let (idx,slot) = Option.value_exn res ~message:"No available slots." in
  let old_pageno = Page.pageno slot.pg in
  Hashtbl.remove t.pageno_to_slot old_pageno;
  Page.flush slot.pg;
  (idx,slot)

let with_page t ?(force_flush = false) pageno f =
  let slot =
    match Hashtbl.find t.pageno_to_slot pageno with
    | Some slot_idx -> Uniform_array.get t.slots slot_idx
    | None -> let (i,slot) = evict t in
               Page.load slot.pg pageno;
               Hashtbl.set t.pageno_to_slot ~key:pageno ~data:i;
               slot
  in
  slot.seqno <- next_seqno t;
  slot.in_use <- true;
  let res = f slot.pg in
  slot.in_use <- false;
  if force_flush then Page.flush slot.pg;
  res