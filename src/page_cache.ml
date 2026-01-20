open! Stdlib_stable
open! Core
open! Core_unix

type slot = #{
  pg : Page.packed;
  in_use : bool;
  seqno : int
}

let compare_seqno s s' =
  (* Prefer clean pages over dirty pages for eviction *)
  let (Page.P pg) = s.#pg in
  let (Page.P pg') = s'.#pg in
  match (Page.Expert.is_dirty pg, Page.Expert.is_dirty pg') with
  | (false, true) -> -1
  | (true, false) -> 1
  | _ -> Int.compare s.#seqno s'.#seqno

type t =
  {
    fd : File_descr.t;
    slots : slot array;
    size : int;
    mutable latest_seqno : int;
    pageno_to_slot : (Pageno.t, int) Hashtbl.t
  }

let next_seqno t =
  let a = t.latest_seqno in
  t.latest_seqno <- a + 1;
  a

let flush_all t =
  for i = 0 to t.size - 1 do
    let slot = t.slots.(i) in
    (* don't flush pages that don't actually correspond to anything yet *)
    if slot.#seqno < 0 then ()
    else
      let (P pg) = slot.#pg in
      Page.Expert.flush pg
  done

let create fd ~size =
  let dummy_slot _ =
    let dummy_pg = Page.Expert.create fd (Pageno.of_int_exn Int.max_value) Page_header.Metadata_header in
    #{ in_use = false; pg = P dummy_pg; seqno = -1 }
  in
  (* This is among the most horrifying hacks i've ever pulled. *)
  let slots : slot array = Obj.magic Obj.magic (Array.create ~len:(3 * size) 0) in
  for i = 0 to size - 1 do
    slots.(i) <- dummy_slot ();
  done;
  {
    fd;
    slots; 
    size;
    latest_seqno = 0;
    pageno_to_slot = Hashtbl.create (module Pageno)
  }

let evict t =
  let mutable best_idx = -1 in
  for i = 0 to t.size - 1 do
    let slot = t.slots.(i) in
    if not slot.#in_use then
      if best_idx < 0 then best_idx <- i
      else
        let best = t.slots.(best_idx) in
        if compare_seqno slot best < 0 then best_idx <- i
  done;
  if best_idx < 0 then failwith "No slots availble";
  let slot = t.slots.(best_idx) in
  let (P pg) = slot.#pg in
  let old_pageno = Page.pageno pg in
  Hashtbl.remove t.pageno_to_slot old_pageno;
  Page.Expert.flush pg;
  best_idx

let with_page t pageno f =
  let slot_idx =
    match Hashtbl.find t.pageno_to_slot pageno with
    | Some slot_idx -> slot_idx
    | None -> let slot_idx = evict t in
              let slot = t.slots.(slot_idx) in
               Page.Expert.overwrite_with slot.#pg pageno;
               Hashtbl.set t.pageno_to_slot ~key:pageno ~data:slot_idx;
               slot_idx
  in
  let pg = t.slots.(slot_idx).#pg  in
  let slot_seqno = (.(slot_idx).#seqno) in
  let slot_in_use = (.(slot_idx).#in_use) in
  Idx_mut.unsafe_set t.slots slot_seqno (next_seqno t);
  Idx_mut.unsafe_set t.slots slot_in_use true; 
  let res = f pg in
  Idx_mut.unsafe_set t.slots slot_in_use false; 
  res