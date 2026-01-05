open! Core
open! Core_unix
open Minikv

let benchmark_insert () =
  let n = 1_000_000 in
  Printf.printf "Benchmarking insertion of %d key-value pairs with random keys...\n" n;

  (* Generate random keys *)
  Random.init 42;
  let keys = Array.init n ~f:(fun _ -> Random.int 50_000) in

  (* Create a temporary database file *)
  let (temp_file, fd) = mkstemp "benchmark_db" in
  ignore (close fd);

  let db = Db.load temp_file in

  let start_time = Time_ns.now () in

  Array.iteri keys ~f:(fun i key ->
    let value = Bytes.of_string (sprintf "value_%d" i) in
    Db.put db key value
  );

  let end_time = Time_ns.now () in
  let elapsed = Time_ns.diff end_time start_time in
  let elapsed_ms = Time_ns.Span.to_ms elapsed in

  Printf.printf "Inserted %d records in %.2f ms\n" n elapsed_ms;
  Printf.printf "Throughput: %.2f inserts/sec\n" (Float.of_int n /. (elapsed_ms /. 1000.0));

  Db.flush db;

  (* Build expected hashtable *)
  Printf.printf "Building validation hashtable...\n";
  let expected = Int.Table.create () in
  Array.iteri keys ~f:(fun i key ->
    let value = Bytes.of_string (sprintf "value_%d" i) in
    Hashtbl.set expected ~key ~data:value
  );

  (* Validation: check that db matches hashtable *)
  Printf.printf "Validating results...\n";
  let mismatch_count = ref 0 in
  Hashtbl.iteri expected ~f:(fun ~key ~data:expected_value ->
    match%optional.Or_null Db.get db key with
    | None ->
        Printf.printf "ERROR: Key %d not found in DB but in hashtable\n" key;
        incr mismatch_count
    | Some db_value ->
        (* Trim db_value to expected length for comparison *)
        let expected_len = Bytes.length expected_value in
        let db_value_trimmed = Bytes.sub db_value ~pos:0 ~len:expected_len in
        if not (Bytes.equal db_value_trimmed expected_value) then begin
          Printf.printf "ERROR: Key %d mismatch - DB: %s, Expected: %s\n"
            key (Bytes.to_string db_value_trimmed) (Bytes.to_string expected_value);
          incr mismatch_count
        end
  );

  if !mismatch_count = 0 then
    Printf.printf "Validation passed! All %d unique keys match.\n" (Hashtbl.length expected)
  else
    Printf.printf "Validation FAILED with %d mismatches!\n" !mismatch_count;

  (* Clean up *)
  Sys_unix.remove temp_file

let () =
  benchmark_insert ()