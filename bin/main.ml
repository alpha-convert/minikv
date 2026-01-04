open! Core
open Minikv

let () =
  let db = Db.load "test.db" in
  Printf.printf "Database loaded.\n";
  Db.put db 1 (Bytes.of_string "sdf");
  Db.put db 2 (Bytes.of_string "ghkl");
  Db.put db 3 (Bytes.of_string "rhmw");
  Printf.printf "get 1: %s\n" (Bytes.to_string (Or_null.value_exn (Db.get db 1)));
  Printf.printf "get 2: %s\n" (Bytes.to_string (Or_null.value_exn (Db.get db 2)));
  Printf.printf "get 3: %s\n" (Bytes.to_string (Or_null.value_exn (Db.get db 3)));
  Printf.printf "put 2: 9999\n";
  Db.put db 2 (Bytes.of_string "9999") ;
  Printf.printf "get 2: %s\n" (Bytes.to_string (Or_null.value_exn (Db.get db 2)));
  Db.flush db;