open! Core
open! Core_unix
open! Base_quickcheck
open! Minikv

module T : sig
    type t = (int * Pageno.t) list [@@deriving sexp, quickcheck]
  end = struct
    type t = (int * Pageno.t) list [@@deriving sexp, quickcheck]
    let quickcheck_generator =
      Generator.list_with_length ~length:5000000
      (Generator.both
         (Generator.int_uniform_inclusive 1 100000)
         (Generator.map ~f:Pageno.of_int_exn (Generator.int_uniform_inclusive 1 1000000)))
  end

let test_differential_insert_lookup () =
  Test.run_exn ~config:{Test.default_config with test_count=5} (module T) ~f:(fun ops ->
    let (_, fd) = mkstemp "bptree_test" in
    let page_cache = Page_cache.create fd ~size:256 in
    let free_list = Free_list.Expert.create page_cache in
    let allocator = Page_allocator.create free_list fd in
    
    let bptree = Bplustree.create page_cache allocator in
    let table = Int.Table.create () in
    let cursor = Bplustree.create_cursor bptree 0 in

    List.iter ops ~f:(fun (key, value) ->
      Bplustree.seek cursor key;
      Bplustree.set cursor value;
      Hashtbl.set table ~key ~data:value;
    );

    Hashtbl.iteri table ~f:(fun ~key ~data ->
      Bplustree.seek cursor key;
      match%optional.Or_null Bplustree.get cursor with
      | None ->
          failwith (sprintf "Key %d not found in B+ tree but present in table" key)
      | Some found_value ->
          if Pageno.to_int found_value <> Pageno.to_int data then
            failwith (sprintf "Key %d: B+ tree has value %d but table has %d"
                        key (Pageno.to_int found_value) (Pageno.to_int data))
    );

    Bplustree.Valid.check bptree;

    ignore (close fd);
    ()
  );

  printf "Differential insert/lookup test passed!\n"

let () =
  printf "Running B+ tree differential tests...\n";
  test_differential_insert_lookup ();
  printf "All tests passed!\n"
