open! Core
open! Core_unix
open! Base_quickcheck
open! Minikv

let test_differential_insert_lookup () =

  let module T : sig
    type t = (int * Pageno.t) list [@@deriving sexp_of, quickcheck]
  end= struct
    type t = (int * Pageno.t) list [@@deriving sexp, quickcheck]
    let quickcheck_generator =
      Generator.list_with_length ~length:500000
      (Generator.both
         (Generator.int_uniform_inclusive 1 10000)
         (Generator.map ~f:Pageno.of_int (Generator.int_uniform_inclusive 1 1000000)))
  end in

  (* Run the test *)
  Test.run_exn ~config:{Test.default_config with test_count=1} (module T) ~f:(fun ops ->
    (* Set up B+ tree *)
    let (_, fd) = mkstemp "bptree_test" in
    let page_cache = Page_cache.create fd ~size:4 in
    let allocator = Page_allocator.create fd in
    
    (* Initialize root as empty internal *)
    let bptree = Bplustree.create page_cache allocator in

    let table = Int.Table.create () in

    List.iter ops ~f:(fun (key, value) ->
      Bplustree.insert bptree key value;
      Hashtbl.set table ~key ~data:value;
    );

    (* Verify all keys *)
    Hashtbl.iteri table ~f:(fun ~key ~data ->
      match Bplustree.lookup bptree key with
      | None ->
          failwith (sprintf "Key %d not found in B+ tree but present in table" key)
      | Some found_value ->
          if Pageno.to_int found_value <> Pageno.to_int data then
            failwith (sprintf "Key %d: B+ tree has value %d but table has %d"
                        key (Pageno.to_int found_value) (Pageno.to_int data))
    );

    ignore (close fd);
    ()
  );

  printf "Differential insert/lookup test passed!\n"

let () =
  printf "Running B+ tree differential tests...\n";
  test_differential_insert_lookup ();
  printf "All tests passed!\n"
