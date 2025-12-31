open Minikv

let () =
  let db = Db.load "test.db" in
  Db.put db ~k:1 ~v:0;
  Db.put db ~k:2 ~v:5;
  Db.put db ~k:3 ~v:99;
  Printf.printf "get 1: %d\n" (Option.value (Db.get db 1) ~default:(-1));
  Printf.printf "get 2: %d\n" (Option.value (Db.get db 2) ~default:(-1));
  Printf.printf "get 3: %d\n" (Option.value (Db.get db 3) ~default:(-1));
  Printf.printf "put 2: 100\n";
  Db.put db ~k:2 ~v:100 ;
  Printf.printf "get 2: %d\n" (Option.value (Db.get db 2) ~default:(-1));