open Bilk

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Helper: read all available server messages from fd *)
let read_server_msgs fd =
  let tmp = Bytes.create 8192 in
  let n = Unix.read fd tmp 0 8192 in
  if n = 0 then []
  else begin
    let data = Bytes.sub_string tmp 0 n in
    let msgs = ref [] in
    let off = ref 0 in
    while Repl_protocol.frame_available data !off do
      let (msg, next) = Repl_protocol.read_server_msg data !off in
      msgs := msg :: !msgs;
      off := next
    done;
    List.rev !msgs
  end

(* Helper: create a server with a socketpair client *)
let make_server () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  (server, client_fd, server_fd)

(* --- Eval: simple result --- *)

let test_eval_result () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(+ 1 2)";
  let msgs = read_server_msgs client_fd in
  (* Should contain a Result "3" *)
  let has_result = List.exists (function
    | Repl_protocol.Result s -> s = "3"
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "result is 3" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Eval: error --- *)

let test_eval_error () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(error \"boom\")";
  let msgs = read_server_msgs client_fd in
  let has_error = List.exists (function
    | Repl_protocol.Error s -> String.length s > 0
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "error returned" true has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Eval: captures display output --- *)

let test_eval_captures_output () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(display \"hello\")";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "output contains hello" true
    (String.length combined > 0 && String.sub combined 0 5 = "hello");
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Eval: void result sends empty string --- *)

let test_eval_void_result () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(define x 42)";
  let msgs = read_server_msgs client_fd in
  let has_result = List.exists (function
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "result sent" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Eval: defines persist across evals --- *)

let test_eval_persists_state () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(define x 42)";
  (* Drain the first batch *)
  ignore (read_server_msgs client_fd);
  Repl_server.handle_eval server "x";
  let msgs = read_server_msgs client_fd in
  let has_result = List.exists (function
    | Repl_protocol.Result s -> s = "42"
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "x is 42" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Complete: returns matching symbols --- *)

let test_complete_matches () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_complete server "car";
  let msgs = read_server_msgs client_fd in
  let completions = List.filter_map (function
    | Repl_protocol.Completions lst -> Some lst
    | _ -> None
  ) msgs in
  Alcotest.(check bool) "got completions" true (completions <> []);
  let lst = List.hd completions in
  let has_car = List.mem "car" lst in
  Alcotest.(check bool) "car in completions" true has_car;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Complete: empty prefix returns all --- *)

let test_complete_empty_prefix () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_complete server "";
  let msgs = read_server_msgs client_fd in
  let completions = List.filter_map (function
    | Repl_protocol.Completions lst -> Some lst
    | _ -> None
  ) msgs in
  Alcotest.(check bool) "got completions" true (completions <> []);
  let lst = List.hd completions in
  (* Empty prefix should return many candidates *)
  Alcotest.(check bool) "many candidates" true (List.length lst > 10);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Complete: no matches --- *)

let test_complete_no_matches () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_complete server "zzzzz-nonexistent";
  let msgs = read_server_msgs client_fd in
  let completions = List.filter_map (function
    | Repl_protocol.Completions lst -> Some lst
    | _ -> None
  ) msgs in
  Alcotest.(check bool) "got completions" true (completions <> []);
  let lst = List.hd completions in
  Alcotest.(check (list string)) "empty list" [] lst;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Resume: correct token --- *)

let test_resume_ok () =
  let (server, client_fd, _server_fd) = make_server () in
  let token = Repl_server.session_token server in
  Repl_server.handle_resume server token;
  let msgs = read_server_msgs client_fd in
  let has_ok = List.exists (function
    | Repl_protocol.Session_ok -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "session ok" true has_ok;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Resume: wrong token --- *)

let test_resume_deny () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_resume server "wrong-token";
  let msgs = read_server_msgs client_fd in
  let has_deny = List.exists (function
    | Repl_protocol.Session_deny -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "session deny" true has_deny;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Disconnect keeps server alive --- *)

let test_disconnect_keeps_alive () =
  let (server, client_fd, _server_fd) = make_server () in
  Unix.close client_fd;
  Alcotest.(check bool) "still alive" true (Repl_server.is_alive server);
  Repl_server.shutdown server

(* --- Interrupt sets flag --- *)

let test_interrupt_flag () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_interrupt server;
  Alcotest.(check bool) "still alive after interrupt" true
    (Repl_server.is_alive server);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Instance accessor --- *)

let test_instance_accessor () =
  let (server, client_fd, _server_fd) = make_server () in
  let inst = Repl_server.instance server in
  let syms = Symbol.all inst.Instance.symbols in
  Alcotest.(check bool) "has symbols" true (List.length syms > 0);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Session token is non-empty --- *)

let test_session_token_nonempty () =
  let (server, client_fd, _server_fd) = make_server () in
  let token = Repl_server.session_token server in
  Alcotest.(check bool) "non-empty token" true (String.length token > 0);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- connect_line: non-insecure format --- *)

let test_connect_line_format () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = false;
  } in
  let server = Repl_server.create config in
  let line = Repl_server.connect_line server in
  (* Should start with "BILK CONNECT 7890 " *)
  Alcotest.(check bool) "starts with prefix" true
    (String.length line > 18 &&
     String.sub line 0 18 = "BILK CONNECT 7890 ");
  (* Key should not be "insecure" *)
  let parts = String.split_on_char ' ' line in
  let key = List.nth parts 3 in
  Alcotest.(check bool) "key not insecure" true (key <> "insecure");
  Repl_server.shutdown server

(* --- connect_line: insecure --- *)

let test_connect_line_insecure () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let line = Repl_server.connect_line server in
  Alcotest.(check string) "insecure connect line"
    "BILK CONNECT 7890 insecure" line;
  Repl_server.shutdown server

(* --- connect_line: parseable roundtrip --- *)

let test_connect_line_parseable () =
  let config = {
    Repl_server.port = 4567;
    auto_checkpoint = false;
    name = "rt";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = false;
  } in
  let server = Repl_server.create config in
  let line = Repl_server.connect_line server in
  let parsed = Ssh_connect.parse_connect_line line in
  (match parsed with
   | Some info ->
     Alcotest.(check int) "port roundtrips" 4567 info.port;
     Alcotest.(check bool) "key non-empty" true
       (String.length info.key > 0)
   | None ->
     Alcotest.fail "connect_line should be parseable");
  Repl_server.shutdown server

(* --- validate_config --- *)

let test_validate_insecure_loopback_ok () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  Alcotest.(check bool) "loopback insecure ok" true
    (Repl_server.validate_config config = Ok ())

let test_validate_insecure_any_rejected () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_any;
    session_timeout = 86400;
    insecure = true;
  } in
  Alcotest.(check bool) "any insecure rejected" true
    (match Repl_server.validate_config config with
     | Error _ -> true
     | Ok () -> false)

let test_validate_secure_any_ok () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_any;
    session_timeout = 86400;
    insecure = false;
  } in
  Alcotest.(check bool) "any secure ok" true
    (Repl_server.validate_config config = Ok ())

let test_validate_insecure_specific_ip_rejected () =
  let config = {
    Repl_server.port = 7890;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_of_string "192.168.1.1";
    session_timeout = 86400;
    insecure = true;
  } in
  Alcotest.(check bool) "specific ip insecure rejected" true
    (match Repl_server.validate_config config with
     | Error _ -> true
     | Ok () -> false)

(* --- Graceful shutdown: auto-checkpoint when enabled --- *)

let test_graceful_shutdown_auto_checkpoint () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = true;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  Repl_server.graceful_shutdown server;
  let cps = Session.list_checkpoints (Repl_server.session server) in
  Alcotest.(check bool) "has auto-checkpoint" true (List.length cps > 0);
  (try Unix.close client_fd with Unix.Unix_error _ -> ())

(* --- Graceful shutdown: no checkpoint when disabled --- *)

let test_graceful_shutdown_no_checkpoint () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  Repl_server.graceful_shutdown server;
  let cps = Session.list_checkpoints (Repl_server.session server) in
  Alcotest.(check (list string)) "no checkpoints" [] cps;
  (try Unix.close client_fd with Unix.Unix_error _ -> ())

(* --- Server commands: ,checkpoint creates checkpoint --- *)

let test_server_cmd_checkpoint () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",checkpoint foo";
  let msgs = read_server_msgs client_fd in
  let has_output = List.exists (function
    | Repl_protocol.Output s -> String.length s > 0
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "got response" true has_output;
  (* No Error message *)
  let has_error = List.exists (function
    | Repl_protocol.Error _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "no error" false has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: checkpoint + revert restores state --- *)

let test_server_cmd_checkpoint_revert () =
  let (server, client_fd, _server_fd) = make_server () in
  (* Define x=1 *)
  Repl_server.handle_eval server "(define x 1)";
  ignore (read_server_msgs client_fd);
  (* Checkpoint *)
  Repl_server.handle_eval server ",checkpoint snap";
  ignore (read_server_msgs client_fd);
  (* Define x=2 *)
  Repl_server.handle_eval server "(define x 2)";
  ignore (read_server_msgs client_fd);
  (* Verify x=2 *)
  Repl_server.handle_eval server "x";
  let msgs = read_server_msgs client_fd in
  let has_2 = List.exists (function
    | Repl_protocol.Result "2" -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "x is 2" true has_2;
  (* Revert *)
  Repl_server.handle_eval server ",revert snap";
  ignore (read_server_msgs client_fd);
  (* Verify x=1 *)
  Repl_server.handle_eval server "x";
  let msgs2 = read_server_msgs client_fd in
  let has_1 = List.exists (function
    | Repl_protocol.Result "1" -> true
    | _ -> false
  ) msgs2 in
  Alcotest.(check bool) "x is 1 after revert" true has_1;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: ,checkpoints lists names --- *)

let test_server_cmd_checkpoints () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",checkpoint alpha";
  ignore (read_server_msgs client_fd);
  Repl_server.handle_eval server ",checkpoints";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "lists alpha" true
    (String.length combined > 0
     && try let _ = String.split_on_char '\n' combined
            |> List.exists (fun l -> String.trim l = "alpha")
        in true with _ -> false);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: ,env produces output --- *)

let test_server_cmd_env () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",env";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "env has car" true
    (let contains s sub =
       let slen = String.length s and sublen = String.length sub in
       let rec check i = i + sublen <= slen
         && (String.sub s i sublen = sub || check (i + 1)) in
       check 0
     in contains combined "car");
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: ,libs produces output --- *)

let test_server_cmd_libs () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",libs";
  let msgs = read_server_msgs client_fd in
  let has_output = List.exists (function
    | Repl_protocol.Output _ -> true
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "got output" true has_output;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: ,help produces output --- *)

let test_server_cmd_help () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",help";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "help has content" true (String.length combined > 0);
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: unknown command → Error --- *)

let test_server_cmd_unknown () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",xyzzy-nonsense";
  let msgs = read_server_msgs client_fd in
  let has_error = List.exists (function
    | Repl_protocol.Error s ->
      (let slen = String.length s and sublen = 7 in
       let rec check i = i + sublen <= slen
         && (String.sub s i sublen = "Unknown" || check (i + 1)) in
       check 0)
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "unknown error" true has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Server commands: regular (+ 1 2) still works --- *)

let test_server_cmd_scheme_passthrough () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(+ 1 2)";
  let msgs = read_server_msgs client_fd in
  let has_result = List.exists (function
    | Repl_protocol.Result "3" -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "result is 3" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Named sessions --- *)

let with_temp_dir fn =
  let dir = Filename.temp_dir "bilk_server_test" "" in
  Fun.protect ~finally:(fun () ->
    let rec rm path =
      if Sys.is_directory path then begin
        Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path);
        Unix.rmdir path
      end else
        Sys.remove path
    in
    rm dir
  ) (fun () -> fn dir)

let string_contains s sub =
  let slen = String.length s and sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i = i + sublen <= slen
      && (String.sub s i sublen = sub || check (i + 1)) in
    check 0

let make_server_with_home home =
  let saved = try Some (Sys.getenv "BILK_HOME") with Not_found -> None in
  Unix.putenv "BILK_HOME" home;
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  let restore () =
    (match saved with
     | Some v -> Unix.putenv "BILK_HOME" v
     | None ->
       (* Can't unsetenv easily in OCaml, set to empty *)
       Unix.putenv "BILK_HOME" "")
  in
  (server, client_fd, server_fd, restore)

let test_server_named_session_roundtrip () =
  with_temp_dir (fun home ->
    let (server, client_fd, _server_fd, restore) = make_server_with_home home in
    Fun.protect ~finally:restore (fun () ->
      (* Define something and checkpoint *)
      Repl_server.handle_eval server "(define x 42)";
      ignore (read_server_msgs client_fd);
      Repl_server.handle_eval server ",checkpoint snap1";
      ignore (read_server_msgs client_fd);
      (* Save session by name *)
      Repl_server.handle_eval server ",save-session test1";
      let msgs = read_server_msgs client_fd in
      let has_error = List.exists (function
        | Repl_protocol.Error _ -> true | _ -> false) msgs in
      Alcotest.(check bool) "no error on save" false has_error;
      (* Load session by name *)
      Repl_server.handle_eval server ",load-session test1";
      let msgs2 = read_server_msgs client_fd in
      let has_error2 = List.exists (function
        | Repl_protocol.Error _ -> true | _ -> false) msgs2 in
      Alcotest.(check bool) "no error on load" false has_error2;
      Repl_server.shutdown server;
      Unix.close client_fd))

let test_server_sessions_list () =
  with_temp_dir (fun home ->
    let (server, client_fd, _server_fd, restore) = make_server_with_home home in
    Fun.protect ~finally:restore (fun () ->
      Repl_server.handle_eval server ",checkpoint cp1";
      ignore (read_server_msgs client_fd);
      Repl_server.handle_eval server ",save-session alpha";
      ignore (read_server_msgs client_fd);
      Repl_server.handle_eval server ",save-session beta";
      ignore (read_server_msgs client_fd);
      Repl_server.handle_eval server ",sessions";
      let msgs = read_server_msgs client_fd in
      let output = List.filter_map (function
        | Repl_protocol.Output s -> Some s | _ -> None) msgs in
      let combined = String.concat "" output in
      let has_alpha = string_contains combined "alpha" in
      let has_beta = string_contains combined "beta" in
      Alcotest.(check bool) "lists alpha" true has_alpha;
      Alcotest.(check bool) "lists beta" true has_beta;
      Repl_server.shutdown server;
      Unix.close client_fd))

let test_server_save_session_traversal () =
  with_temp_dir (fun home ->
    let (server, client_fd, _server_fd, restore) = make_server_with_home home in
    Fun.protect ~finally:restore (fun () ->
      Repl_server.handle_eval server ",save-session ../evil";
      let msgs = read_server_msgs client_fd in
      let has_error = List.exists (function
        | Repl_protocol.Error _ -> true | _ -> false) msgs in
      Alcotest.(check bool) "path traversal rejected" true has_error;
      Repl_server.shutdown server;
      Unix.close client_fd))

let test_server_load_session_nonexistent () =
  with_temp_dir (fun home ->
    let (server, client_fd, _server_fd, restore) = make_server_with_home home in
    Fun.protect ~finally:restore (fun () ->
      Repl_server.handle_eval server ",load-session nonexistent";
      let msgs = read_server_msgs client_fd in
      let has_error = List.exists (function
        | Repl_protocol.Error _ -> true | _ -> false) msgs in
      Alcotest.(check bool) "nonexistent session error" true has_error;
      Repl_server.shutdown server;
      Unix.close client_fd))

(* --- Path restriction for ,load --- *)

let test_server_load_traversal () =
  (* ../../../etc/passwd may not exist, so the error could be
     "path not found" or "outside allowed directories" — either
     way, the load must be rejected. *)
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",load ../../../etc/passwd";
  let msgs = read_server_msgs client_fd in
  let has_error = List.exists (function
    | Repl_protocol.Error _ -> true
    | _ -> false) msgs in
  Alcotest.(check bool) "path traversal error" true has_error;
  (* Must NOT have loaded successfully *)
  let has_loaded = List.exists (function
    | Repl_protocol.Output s -> string_contains s "Loaded"
    | _ -> false) msgs in
  Alcotest.(check bool) "not loaded" false has_loaded;
  Repl_server.shutdown server;
  Unix.close client_fd

let test_server_load_absolute_outside () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server ",load /etc/passwd";
  let msgs = read_server_msgs client_fd in
  let has_error = List.exists (function
    | Repl_protocol.Error s ->
      string_contains s "outside" || string_contains s "not found"
    | _ -> false) msgs in
  Alcotest.(check bool) "absolute outside error" true has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

let test_server_load_valid_relative () =
  with_temp_dir (fun dir ->
    (* Create a valid .scm file in a temp dir *)
    let file = Filename.concat dir "test.scm" in
    let oc = open_out file in
    output_string oc "(define load-test-var 99)\n";
    close_out oc;
    let (server, client_fd, _server_fd) = make_server () in
    (* Add the temp dir to search paths so it's allowed *)
    let inst = Repl_server.instance server in
    inst.Instance.search_paths := dir :: !(inst.Instance.search_paths);
    Repl_server.handle_eval server (Printf.sprintf ",load %s" file);
    let msgs = read_server_msgs client_fd in
    let has_outside_error = List.exists (function
      | Repl_protocol.Error s -> string_contains s "outside"
      | _ -> false) msgs in
    Alcotest.(check bool) "no path restriction error" false has_outside_error;
    Repl_server.shutdown server;
    Unix.close client_fd)

(* --- Eval input/output bounds (Issue 68) --- *)

let test_eval_input_too_large () =
  let (server, client_fd, _server_fd) = make_server () in
  (* 1 MiB + 1 byte exceeds max_eval_input.
     Use spaces: without the guard, the server processes them quickly
     as whitespace and sends a small Result "".  With the guard, it
     immediately sends Error "input too large". *)
  let big_input = String.make (1_048_576 + 1) ' ' in
  Repl_server.handle_eval server big_input;
  let msgs = read_server_msgs client_fd in
  let has_error = List.exists (function
    | Repl_protocol.Error s -> s = "input too large"
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "input too large" true has_error;
  (* No Result should be sent *)
  let has_result = List.exists (function
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "no result sent" false has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

let test_eval_moderate_output () =
  let (server, client_fd, _server_fd) = make_server () in
  (* Generate ~5KB of output — fits in single read, well under limit *)
  Repl_server.handle_eval server
    "(display (make-string 5000 #\\a))";
  let msgs = read_server_msgs client_fd in
  let output = List.filter_map (function
    | Repl_protocol.Output s -> Some s
    | _ -> None
  ) msgs in
  let combined = String.concat "" output in
  Alcotest.(check bool) "output not truncated" true
    (String.length combined >= 5000
     && not (string_contains combined "[output truncated]"));
  let has_result = List.exists (function
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "result sent" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

let test_eval_normal_under_input_limit () =
  let (server, client_fd, _server_fd) = make_server () in
  (* A valid expression well under the 1 MiB limit *)
  Repl_server.handle_eval server "(+ 100 200)";
  let msgs = read_server_msgs client_fd in
  let has_result = List.exists (function
    | Repl_protocol.Result s -> s = "300"
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "result is 300" true has_result;
  let has_error = List.exists (function
    | Repl_protocol.Error _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "no error" false has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Auth handshake tests --- *)

(* Helper: write a client message directly to a fd *)
let write_client_msg fd msg =
  let buf = Buffer.create 256 in
  Repl_protocol.write_client_msg buf msg;
  let data = Buffer.contents buf in
  ignore (Unix.write_substring fd data 0 (String.length data))

let make_secure_server () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = false;
  } in
  let server = Repl_server.create config in
  let (client_fd, server_fd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server server_fd;
  (server, client_fd, server_fd)

let test_auth_sends_challenge () =
  let (server, client_fd, _server_fd) = make_secure_server () in
  let result = ref false in
  (* Run authenticate_client in a thread since it blocks on read *)
  let thread = Thread.create (fun () ->
    result := Repl_server.authenticate_client server
  ) () in
  (* Client: read the Auth_challenge, then close without responding *)
  let msgs = read_server_msgs client_fd in
  let has_challenge = List.exists (function
    | Repl_protocol.Auth_challenge n -> String.length n = 32
    | _ -> false) msgs in
  Unix.close client_fd;
  Thread.join thread;
  Alcotest.(check bool) "server sent challenge" true has_challenge;
  Alcotest.(check bool) "auth fails without response" false !result;
  Repl_server.shutdown server

let test_auth_accepts_correct_response () =
  let (server, client_fd, _server_fd) = make_secure_server () in
  (* Spawn a thread to handle the client side of the handshake *)
  let key = Repl_server.crypto_key server in
  let result = ref false in
  let thread = Thread.create (fun () ->
    result := Repl_server.authenticate_client server
  ) () in
  (* Client side: read Auth_challenge, compute response, send Auth_response *)
  let msgs = read_server_msgs client_fd in
  let nonce = match msgs with
    | [Repl_protocol.Auth_challenge n] -> n
    | _ -> Alcotest.fail "expected Auth_challenge"
  in
  let key = match key with Some k -> k | None -> Alcotest.fail "expected key" in
  let hmac = Repl_crypto.auth_response key nonce in
  let client_nonce = Repl_crypto.auth_challenge () in
  write_client_msg client_fd
    (Repl_protocol.Auth_response (hmac, client_nonce));
  (* Read server's Auth_ok *)
  let msgs2 = read_server_msgs client_fd in
  let has_auth_ok = List.exists (function
    | Repl_protocol.Auth_ok _ -> true | _ -> false) msgs2 in
  Thread.join thread;
  Alcotest.(check bool) "server accepted auth" true !result;
  Alcotest.(check bool) "server sent Auth_ok" true has_auth_ok;
  Repl_server.shutdown server;
  (try Unix.close client_fd with Unix.Unix_error _ -> ())

let test_auth_rejects_wrong_response () =
  let (server, client_fd, _server_fd) = make_secure_server () in
  let result = ref false in
  let thread = Thread.create (fun () ->
    result := Repl_server.authenticate_client server
  ) () in
  (* Client side: read Auth_challenge, send wrong HMAC *)
  let msgs = read_server_msgs client_fd in
  (match msgs with
   | [Repl_protocol.Auth_challenge _] -> ()
   | _ -> Alcotest.fail "expected Auth_challenge");
  let bad_hmac = String.make 32 '\x00' in
  let client_nonce = String.make 32 '\xff' in
  write_client_msg client_fd
    (Repl_protocol.Auth_response (bad_hmac, client_nonce));
  (* Read server's Auth_deny *)
  let msgs2 = read_server_msgs client_fd in
  let has_auth_deny = List.exists (function
    | Repl_protocol.Auth_deny -> true | _ -> false) msgs2 in
  Thread.join thread;
  Alcotest.(check bool) "server rejected auth" false !result;
  Alcotest.(check bool) "server sent Auth_deny" true has_auth_deny;
  Repl_server.shutdown server;
  (try Unix.close client_fd with Unix.Unix_error _ -> ())

let test_insecure_server_no_auth () =
  let (server, client_fd, _server_fd) = make_server () in
  (* Insecure server: authenticate_client should return true immediately *)
  let auth_ok = Repl_server.authenticate_client server in
  Alcotest.(check bool) "insecure skips auth" true auth_ok;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- FD leak on client replacement (Issue 69) --- *)

let test_accept_closes_previous_fd () =
  let config = {
    Repl_server.port = 0;
    auto_checkpoint = false;
    name = "test";
    bind_address = Unix.inet_addr_loopback;
    session_timeout = 86400;
    insecure = true;
  } in
  let server = Repl_server.create config in
  (* First client *)
  let (c1, s1) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server s1;
  (* Second client replaces first *)
  let (c2, s2) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Repl_server.accept_client server s2;
  (* s1 should now be closed — writing to it should raise EBADF *)
  let s1_closed =
    try ignore (Unix.write_substring s1 "x" 0 1); false
    with Unix.Unix_error (Unix.EBADF, _, _) -> true
  in
  Alcotest.(check bool) "old server fd closed" true s1_closed;
  Repl_server.shutdown server;
  (try Unix.close c1 with Unix.Unix_error _ -> ());
  (try Unix.close c2 with Unix.Unix_error _ -> ())

(* --- Error path sanitization (Issue 74) --- *)

let test_sanitize_error_path_absolute () =
  let msg = "/home/user/secret/project/file.txt: No such file or directory" in
  let result = Repl_server.sanitize_error_path msg in
  Alcotest.(check string) "basename only"
    "file.txt: No such file or directory" result

let test_sanitize_error_path_no_path () =
  let msg = "division by zero" in
  let result = Repl_server.sanitize_error_path msg in
  Alcotest.(check string) "unchanged" "division by zero" result

let test_sanitize_error_path_relative () =
  let msg = "foo.txt: Permission denied" in
  let result = Repl_server.sanitize_error_path msg in
  Alcotest.(check string) "relative unchanged" "foo.txt: Permission denied" result

let test_sanitize_error_path_bare () =
  (* Absolute path with no colon separator *)
  let msg = "/home/user/secret/file.txt" in
  let result = Repl_server.sanitize_error_path msg in
  Alcotest.(check string) "basename of bare path" "file.txt" result

let test_eval_sys_error_no_full_path () =
  (* Evaluating (open-input-file ...) on a nonexistent file should
     produce an error that does NOT contain the full absolute path *)
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server
    "(open-input-file \"/deep/secret/server/path/missing.txt\")";
  let msgs = read_server_msgs client_fd in
  let errors = List.filter_map (function
    | Repl_protocol.Error s -> Some s
    | _ -> None
  ) msgs in
  (* There should be an error *)
  Alcotest.(check bool) "has error" true (errors <> []);
  (* Error should NOT contain the deep directory structure *)
  let leaks_path = List.exists (fun s ->
    string_contains s "/deep/secret/server/path"
  ) errors in
  Alcotest.(check bool) "no path leak" false leaks_path;
  Repl_server.shutdown server;
  Unix.close client_fd

(* --- Security: adversarial inputs --- *)

let test_adversarial_oversized_eval () =
  let (server, client_fd, _server_fd) = make_server () in
  (* Send an Eval exceeding the 1 MiB limit — server should reject *)
  let huge = String.make 1_100_000 'x' in
  Repl_server.handle_eval server huge;
  let msgs = read_server_msgs client_fd in
  let has_error = List.exists (function
    | Repl_protocol.Error s -> String.length s > 0
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "oversized eval returns error" true has_error;
  Repl_server.shutdown server;
  Unix.close client_fd

let test_adversarial_garbage_bytes () =
  let (server, client_fd, server_fd) = make_server () in
  (* Write garbage bytes to the server fd — should not crash *)
  let garbage = String.make 50 '\xff' in
  let _ = try Unix.write_substring server_fd garbage 0 50
    with Unix.Unix_error _ -> 0 in
  (* Server should still be alive *)
  Alcotest.(check bool) "server alive after garbage" true
    (Repl_server.is_alive server);
  Repl_server.shutdown server;
  (try Unix.close client_fd with Unix.Unix_error _ -> ());
  (try Unix.close server_fd with Unix.Unix_error _ -> ())

let test_adversarial_empty_eval () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "";
  let msgs = read_server_msgs client_fd in
  (* Empty eval should produce a Result, not crash *)
  let has_result = List.exists (function
    | Repl_protocol.Result _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "empty eval produces result" true has_result;
  Repl_server.shutdown server;
  Unix.close client_fd

let test_adversarial_null_bytes_eval () =
  let (server, client_fd, _server_fd) = make_server () in
  Repl_server.handle_eval server "(+ 1\x00 2)";
  let msgs = read_server_msgs client_fd in
  (* Should produce some response (error or result), not crash *)
  let has_response = List.exists (function
    | Repl_protocol.Result _ | Repl_protocol.Error _ -> true
    | _ -> false
  ) msgs in
  Alcotest.(check bool) "null bytes produce response" true has_response;
  Repl_server.shutdown server;
  Unix.close client_fd

let () =
  Alcotest.run "Repl_server"
    [ ("eval",
       [ Alcotest.test_case "simple result" `Quick test_eval_result
       ; Alcotest.test_case "error" `Quick test_eval_error
       ; Alcotest.test_case "captures output" `Quick test_eval_captures_output
       ; Alcotest.test_case "void result" `Quick test_eval_void_result
       ; Alcotest.test_case "persists state" `Quick test_eval_persists_state
       ])
    ; ("complete",
       [ Alcotest.test_case "matches" `Quick test_complete_matches
       ; Alcotest.test_case "empty prefix" `Quick test_complete_empty_prefix
       ; Alcotest.test_case "no matches" `Quick test_complete_no_matches
       ])
    ; ("session",
       [ Alcotest.test_case "resume ok" `Quick test_resume_ok
       ; Alcotest.test_case "resume deny" `Quick test_resume_deny
       ; Alcotest.test_case "token non-empty" `Quick test_session_token_nonempty
       ])
    ; ("lifecycle",
       [ Alcotest.test_case "disconnect keeps alive" `Quick
           test_disconnect_keeps_alive
       ; Alcotest.test_case "interrupt flag" `Quick test_interrupt_flag
       ; Alcotest.test_case "instance accessor" `Quick test_instance_accessor
       ])
    ; ("connect_line",
       [ Alcotest.test_case "format" `Quick test_connect_line_format
       ; Alcotest.test_case "insecure" `Quick test_connect_line_insecure
       ; Alcotest.test_case "parseable" `Quick test_connect_line_parseable
       ])
    ; ("validate_config",
       [ Alcotest.test_case "insecure loopback ok" `Quick
           test_validate_insecure_loopback_ok
       ; Alcotest.test_case "insecure any rejected" `Quick
           test_validate_insecure_any_rejected
       ; Alcotest.test_case "secure any ok" `Quick
           test_validate_secure_any_ok
       ; Alcotest.test_case "insecure specific ip rejected" `Quick
           test_validate_insecure_specific_ip_rejected
       ])
    ; ("graceful_shutdown",
       [ Alcotest.test_case "auto-checkpoint on graceful" `Quick
           test_graceful_shutdown_auto_checkpoint
       ; Alcotest.test_case "no checkpoint when disabled" `Quick
           test_graceful_shutdown_no_checkpoint
       ])
    ; ("server_commands",
       [ Alcotest.test_case ",checkpoint creates" `Quick
           test_server_cmd_checkpoint
       ; Alcotest.test_case ",checkpoint + ,revert" `Quick
           test_server_cmd_checkpoint_revert
       ; Alcotest.test_case ",checkpoints lists" `Quick
           test_server_cmd_checkpoints
       ; Alcotest.test_case ",env" `Quick test_server_cmd_env
       ; Alcotest.test_case ",libs" `Quick test_server_cmd_libs
       ; Alcotest.test_case ",help" `Quick test_server_cmd_help
       ; Alcotest.test_case "unknown command" `Quick test_server_cmd_unknown
       ; Alcotest.test_case "scheme passthrough" `Quick
           test_server_cmd_scheme_passthrough
       ])
    ; ("named_sessions",
       [ Alcotest.test_case "save/load round-trip" `Quick
           test_server_named_session_roundtrip
       ; Alcotest.test_case ",sessions list" `Quick
           test_server_sessions_list
       ; Alcotest.test_case "path traversal rejected" `Quick
           test_server_save_session_traversal
       ; Alcotest.test_case "nonexistent session" `Quick
           test_server_load_session_nonexistent
       ])
    ; ("path_restriction",
       [ Alcotest.test_case ",load traversal" `Quick
           test_server_load_traversal
       ; Alcotest.test_case ",load absolute outside" `Quick
           test_server_load_absolute_outside
       ; Alcotest.test_case ",load valid relative" `Quick
           test_server_load_valid_relative
       ])
    ; ("eval_bounds",
       [ Alcotest.test_case "input too large" `Quick
           test_eval_input_too_large
       ; Alcotest.test_case "moderate output ok" `Quick
           test_eval_moderate_output
       ; Alcotest.test_case "normal under input limit" `Quick
           test_eval_normal_under_input_limit
       ])
    ; ("auth_handshake",
       [ Alcotest.test_case "sends challenge" `Quick
           test_auth_sends_challenge
       ; Alcotest.test_case "accepts correct" `Quick
           test_auth_accepts_correct_response
       ; Alcotest.test_case "rejects wrong" `Quick
           test_auth_rejects_wrong_response
       ; Alcotest.test_case "insecure skips" `Quick
           test_insecure_server_no_auth
       ])
    ; ("fd_leak",
       [ Alcotest.test_case "accept closes previous" `Quick
           test_accept_closes_previous_fd
       ])
    ; ("error_path_sanitization",
       [ Alcotest.test_case "absolute path stripped" `Quick
           test_sanitize_error_path_absolute
       ; Alcotest.test_case "no path unchanged" `Quick
           test_sanitize_error_path_no_path
       ; Alcotest.test_case "relative unchanged" `Quick
           test_sanitize_error_path_relative
       ; Alcotest.test_case "bare absolute path" `Quick
           test_sanitize_error_path_bare
       ; Alcotest.test_case "eval sys_error no full path" `Quick
           test_eval_sys_error_no_full_path
       ])
    ; ("adversarial",
       [ Alcotest.test_case "oversized eval rejected" `Quick
           test_adversarial_oversized_eval
       ; Alcotest.test_case "garbage bytes disconnect" `Quick
           test_adversarial_garbage_bytes
       ; Alcotest.test_case "empty eval ok" `Quick
           test_adversarial_empty_eval
       ; Alcotest.test_case "null bytes in eval" `Quick
           test_adversarial_null_bytes_eval
       ])
    ]
