external run_c_tests : unit -> int = "run_c_tests_stub"

let () =
  let rc = run_c_tests () in
  exit rc
