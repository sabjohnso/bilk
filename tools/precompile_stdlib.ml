open Wile

let () =
  let stdlib_dir = Sys.argv.(1) in
  let inst = Instance.create () in
  inst.fasl_cache := true;
  inst.search_paths := [stdlib_dir];
  let srfis =
    [ 1; 2; 8; 11; 16; 26; 28; 31; 41; 48;
      111; 113; 117; 125; 128; 132; 133; 145; 156; 158;
      162; 175; 189; 195; 210; 214; 219; 223; 228; 234; 235 ]
  in
  List.iter (fun n ->
    ignore (Instance.eval_string inst
      (Printf.sprintf "(import (srfi %d))" n))
  ) srfis
