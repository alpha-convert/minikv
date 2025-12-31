open Minikv

let () =
  let db = Db.load "test.db" in
  Db.put db ~k:1 ~v:0;
  Db.put db ~k:2 ~v:5;
  Db.put db ~k:3 ~v:99;
  Printf.printf "get 1: %d\n" (Option.value (Db.get db 1) ~default:(-1));
  Printf.printf "get 2: %d\n" (Option.value (Db.get db 2) ~default:(-1));
  Printf.printf "scan: %s\n" 
    (String.concat ", " (List.map (fun (k,v) -> Printf.sprintf "%d->%d" k v) (Db.scan db)));
  Db.put db ~k:2 ~v:100 ;
  Printf.printf "final: %s\n"
    (String.concat ", " (List.map (fun (k,v) -> Printf.sprintf "%d->%d" k v) (Db.scan db)))