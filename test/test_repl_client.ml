open Bilk

(* Helper: write server messages to a fd *)
let write_server_msg fd msg =
  let buf = Buffer.create 256 in
  Repl_protocol.write_server_msg buf msg;
  let data = Buffer.contents buf in
  ignore (Unix.write_substring fd data 0 (String.length data))

(* Helper: read a client message from a fd *)
let read_client_msg_from_fd fd =
  let tmp = Bytes.create 4096 in
  let n = Unix.read fd tmp 0 4096 in
  let data = Bytes.sub_string tmp 0 n in
  let (msg, _) = Repl_protocol.read_client_msg data 0 in
  msg

(* Helper: create a socketpair and return (client_conn, server_fd) *)
let make_pair () =
  let (cfd, sfd) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (Repl_client.connection_of_fd cfd, sfd)

(* --- send_msg / recv_msg round-trip --- *)

let test_send_recv_roundtrip () =
  let (conn, server_fd) = make_pair () in
  Repl_client.send_msg conn (Repl_protocol.Eval "(+ 1 2)");
  let msg = read_client_msg_from_fd server_fd in
  Alcotest.(check bool) "eval message" true
    (match msg with Repl_protocol.Eval s -> s = "(+ 1 2)" | _ -> false);
  write_server_msg server_fd (Repl_protocol.Result "3");
  let resp = Repl_client.recv_msg conn in
  Alcotest.(check bool) "result message" true
    (match resp with Repl_protocol.Result s -> s = "3" | _ -> false);
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: simple result --- *)

let test_eval_result () =
  let (conn, server_fd) = make_pair () in
  (* Pre-write the server response before calling eval_remote *)
  write_server_msg server_fd (Repl_protocol.Result "42");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "(+ 40 2)" in
  Alcotest.(check bool) "result is 42" true
    (result = `Result "42");
  (* Verify the client sent Eval *)
  let msg = read_client_msg_from_fd server_fd in
  Alcotest.(check bool) "sent eval" true
    (match msg with Repl_protocol.Eval s -> s = "(+ 40 2)" | _ -> false);
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: output then result --- *)

let test_eval_output_then_result () =
  let (conn, server_fd) = make_pair () in
  let output_buf = Buffer.create 64 in
  write_server_msg server_fd (Repl_protocol.Output "hello\n");
  write_server_msg server_fd (Repl_protocol.Result "42");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun s -> Buffer.add_string output_buf s)
    ~on_read:(fun _ -> "")
    "(begin (display \"hello\") (newline) 42)" in
  Alcotest.(check string) "output captured" "hello\n"
    (Buffer.contents output_buf);
  Alcotest.(check bool) "result is 42" true
    (result = `Result "42");
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: error --- *)

let test_eval_error () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Error "unbound variable: x");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "x" in
  Alcotest.(check bool) "error returned" true
    (result = `Error "unbound variable: x");
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: read request --- *)

let test_eval_read_request () =
  let (conn, server_fd) = make_pair () in
  (* Pre-write Read_request. After eval_remote sends Input, we need
     the Result to be readable. Use a helper process approach:
     write Read_request, then after reading Input back, write Result.
     Since we can't do that without threads, test the simpler case:
     pre-write Read_request + Result, and verify Input was sent. *)
  write_server_msg server_fd (Repl_protocol.Read_request "Enter: ");
  (* We also pre-write the Result that follows.  eval_remote will:
     1. send Eval, 2. read Read_request, 3. call on_read, 4. send Input,
     5. read Result.  Since the socket buffer has both messages, this works. *)
  write_server_msg server_fd (Repl_protocol.Result "done");
  let read_called = ref false in
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun prompt ->
      Alcotest.(check string) "prompt" "[remote] Enter: " prompt;
      read_called := true;
      "user-input")
    "(read)" in
  Alcotest.(check bool) "on_read called" true !read_called;
  Alcotest.(check bool) "result is done" true
    (result = `Result "done");
  (* Verify that Eval and Input were sent *)
  let tmp = Bytes.create 4096 in
  let n = Unix.read server_fd tmp 0 4096 in
  let data = Bytes.sub_string tmp 0 n in
  let (msg1, off1) = Repl_protocol.read_client_msg data 0 in
  Alcotest.(check bool) "sent eval" true
    (match msg1 with Repl_protocol.Eval _ -> true | _ -> false);
  let (msg2, _) = Repl_protocol.read_client_msg data off1 in
  Alcotest.(check bool) "sent input" true
    (match msg2 with Repl_protocol.Input s -> s = "user-input" | _ -> false);
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- eval_remote: disconnect --- *)

let test_eval_disconnect () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (conn, server_fd) = make_pair () in
  Unix.close server_fd;
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "(+ 1 2)" in
  Alcotest.(check bool) "disconnected" true
    (result = `Disconnected);
  Repl_client.close_connection conn

(* --- eval_remote: skips Status messages --- *)

let test_eval_skips_status () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Status Repl_protocol.Busy);
  write_server_msg server_fd (Repl_protocol.Status Repl_protocol.Ready);
  write_server_msg server_fd (Repl_protocol.Result "ok");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "expr" in
  Alcotest.(check bool) "result is ok" true
    (result = `Result "ok");
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- request_completions: normal --- *)

let test_completions () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd
    (Repl_protocol.Completions ["car"; "cdr"; "cons"]);
  let result = Repl_client.request_completions conn "c" in
  Alcotest.(check (list string)) "completions" ["car"; "cdr"; "cons"] result;
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- request_completions: empty --- *)

let test_completions_empty () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Completions []);
  let result = Repl_client.request_completions conn "zzz" in
  Alcotest.(check (list string)) "empty completions" [] result;
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- request_completions: skips Status before Completions --- *)

let test_completions_skips_status () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Status Repl_protocol.Busy);
  write_server_msg server_fd
    (Repl_protocol.Completions ["define"; "display"]);
  let result = Repl_client.request_completions conn "d" in
  Alcotest.(check (list string)) "completions" ["define"; "display"] result;
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- request_completions: disconnect returns [] --- *)

let test_completions_disconnect () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (conn, server_fd) = make_pair () in
  Unix.close server_fd;
  let result = Repl_client.request_completions conn "c" in
  Alcotest.(check (list string)) "empty on disconnect" [] result;
  Repl_client.close_connection conn

(* --- config: all fields --- *)

let test_config_fields () =
  let config : Repl_client.config = {
    host = "example.com";
    port = 7890;
    theme = Some "dark";
    history_file = Some "/tmp/history";
    paredit = true;
    key = Some "secret-key";
  } in
  Alcotest.(check string) "host" "example.com" config.host;
  Alcotest.(check int) "port" 7890 config.port;
  Alcotest.(check (option string)) "theme" (Some "dark") config.theme;
  Alcotest.(check (option string)) "history_file"
    (Some "/tmp/history") config.history_file;
  Alcotest.(check bool) "paredit" true config.paredit;
  Alcotest.(check (option string)) "key" (Some "secret-key") config.key

(* --- Command classification integration --- *)

let test_classify_quit_integration () =
  Alcotest.(check bool) ",quit is Client_local Quit" true
    (Repl_command.classify ",quit" = Client_local Quit)

let test_classify_checkpoint_integration () =
  Alcotest.(check bool) ",checkpoint is Server_side" true
    (Repl_command.classify ",checkpoint x" = Server_side)

let test_classify_scheme_integration () =
  Alcotest.(check bool) "(+ 1 2) is Scheme_input" true
    (Repl_command.classify "(+ 1 2)" = Scheme_input)

(* --- recv_buf bounded (Issue 67) --- *)

let test_recv_buf_rejects_oversized () =
  (* Verify that a malicious server sending a header claiming payload
     larger than max_frame_size triggers Protocol_error — the
     frame_available guard (primary defense from Issue 64) prevents
     unbounded buffer growth.  The recv_buf overflow check added here
     provides defense-in-depth for the same threat. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (conn, server_fd) = make_pair () in
  let too_large = Repl_protocol.max_frame_size + 1 in
  let header = Bytes.create 4 in
  Bytes.set header 0 (Char.chr ((too_large lsr 24) land 0xff));
  Bytes.set header 1 (Char.chr ((too_large lsr 16) land 0xff));
  Bytes.set header 2 (Char.chr ((too_large lsr 8) land 0xff));
  Bytes.set header 3 (Char.chr (too_large land 0xff));
  ignore (Unix.write server_fd header 0 4);
  let filler = Bytes.make 100 '\x00' in
  ignore (Unix.write server_fd filler 0 100);
  Unix.close server_fd;
  let raised_protocol_error = ref false in
  (try ignore (Repl_client.recv_msg conn)
   with Repl_protocol.Protocol_error _ -> raised_protocol_error := true);
  Alcotest.(check bool) "protocol error raised" true !raised_protocol_error;
  Repl_client.close_connection conn

(* --- Normal large frame decodes without error --- *)

let test_large_frame_ok () =
  let (conn, server_fd) = make_pair () in
  let payload = String.make 1000 'a' in
  write_server_msg server_fd (Repl_protocol.Output payload);
  let msg = Repl_client.recv_msg conn in
  Alcotest.(check bool) "got large output" true
    (match msg with Repl_protocol.Output s -> s = payload | _ -> false);
  Repl_client.close_connection conn;
  Unix.close server_fd

let () = Mirage_crypto_rng_unix.use_default ()

(* --- Auth handshake tests --- *)

let test_auth_success () =
  let (conn, server_fd) = make_pair () in
  let key = Repl_crypto.generate_key () in
  (* Server side: send Auth_challenge *)
  let server_nonce = Repl_crypto.auth_challenge () in
  write_server_msg server_fd (Repl_protocol.Auth_challenge server_nonce);
  (* Run authenticate in a thread since it blocks on recv *)
  let result = ref None in
  let thread = Thread.create (fun () ->
    (try
       Repl_client.authenticate conn key;
       result := Some true
     with _ ->
       result := Some false)
  ) () in
  (* Read client's Auth_response *)
  let msg = read_client_msg_from_fd server_fd in
  let (client_hmac, client_nonce) = match msg with
    | Repl_protocol.Auth_response (h, n) -> (h, n)
    | _ -> Alcotest.fail "expected Auth_response"
  in
  (* Verify client's HMAC *)
  Alcotest.(check bool) "client HMAC correct" true
    (Repl_crypto.verify_auth key server_nonce client_hmac);
  (* Send Auth_ok with server's proof *)
  let server_hmac = Repl_crypto.auth_response key client_nonce in
  write_server_msg server_fd (Repl_protocol.Auth_ok server_hmac);
  Thread.join thread;
  Alcotest.(check bool) "auth succeeded" true
    (!result = Some true);
  Repl_client.close_connection conn;
  Unix.close server_fd

let test_auth_deny () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (conn, server_fd) = make_pair () in
  let key = Repl_crypto.generate_key () in
  let server_nonce = Repl_crypto.auth_challenge () in
  write_server_msg server_fd (Repl_protocol.Auth_challenge server_nonce);
  let raised = ref false in
  let thread = Thread.create (fun () ->
    (try Repl_client.authenticate conn key
     with Repl_protocol.Protocol_error _ -> raised := true)
  ) () in
  (* Read client's response (ignore it) *)
  ignore (read_client_msg_from_fd server_fd);
  (* Send Auth_deny *)
  write_server_msg server_fd Repl_protocol.Auth_deny;
  Thread.join thread;
  Alcotest.(check bool) "raised Protocol_error" true !raised;
  Repl_client.close_connection conn;
  (try Unix.close server_fd with Unix.Unix_error _ -> ())

let test_auth_fake_server () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (conn, server_fd) = make_pair () in
  let key = Repl_crypto.generate_key () in
  let server_nonce = Repl_crypto.auth_challenge () in
  write_server_msg server_fd (Repl_protocol.Auth_challenge server_nonce);
  let raised = ref false in
  let thread = Thread.create (fun () ->
    (try Repl_client.authenticate conn key
     with Repl_protocol.Protocol_error _ -> raised := true)
  ) () in
  (* Read client's response *)
  let msg = read_client_msg_from_fd server_fd in
  let (_hmac, _client_nonce) = match msg with
    | Repl_protocol.Auth_response (h, n) -> (h, n)
    | _ -> Alcotest.fail "expected Auth_response"
  in
  (* Send Auth_ok with wrong HMAC (fake server doesn't know the key) *)
  let bad_hmac = String.make 32 '\x00' in
  write_server_msg server_fd (Repl_protocol.Auth_ok bad_hmac);
  Thread.join thread;
  Alcotest.(check bool) "detected fake server" true !raised;
  Repl_client.close_connection conn;
  (try Unix.close server_fd with Unix.Unix_error _ -> ())

(* --- Security: adversarial server responses --- *)

let test_adversarial_unexpected_msg () =
  (* Server sends Session_ok during eval — client should skip it *)
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd Repl_protocol.Session_ok;
  write_server_msg server_fd (Repl_protocol.Result "ok");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "(+ 1 2)" in
  Alcotest.(check bool) "skips unexpected, gets result" true
    (result = `Result "ok");
  Repl_client.close_connection conn;
  Unix.close server_fd

let test_adversarial_rapid_disconnect () =
  (* Server closes connection immediately after client sends Eval *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (conn, server_fd) = make_pair () in
  Unix.close server_fd;
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "(loop)" in
  Alcotest.(check bool) "returns disconnected" true
    (result = `Disconnected);
  Repl_client.close_connection conn

let test_adversarial_empty_result () =
  (* Server sends empty Result string *)
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Result "");
  let result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun _ -> "")
    "(void)" in
  Alcotest.(check bool) "empty result ok" true
    (result = `Result "");
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- sanitize_read_prompt tests (Issue 76) --- *)

let test_sanitize_prefix () =
  Alcotest.(check string) "prefix added"
    "[remote] Enter: " (Repl_client.sanitize_read_prompt "Enter: ")

let test_sanitize_empty () =
  Alcotest.(check string) "empty prompt"
    "[remote] " (Repl_client.sanitize_read_prompt "")

let test_sanitize_strips_control () =
  (* Bell, backspace, escape sequences should be stripped *)
  Alcotest.(check string) "control chars stripped"
    "[remote] hello" (Repl_client.sanitize_read_prompt "\x07\x08hello")

let test_sanitize_strips_ansi () =
  Alcotest.(check string) "ANSI escape stripped"
    "[remote] prompt" (Repl_client.sanitize_read_prompt "\x1b[31mprompt")

let test_sanitize_preserves_printable () =
  Alcotest.(check string) "printable preserved"
    "[remote] Password: " (Repl_client.sanitize_read_prompt "Password: ")

let test_sanitize_truncates_long () =
  let long = String.make 200 'x' in
  let result = Repl_client.sanitize_read_prompt long in
  (* "[remote] " = 9 chars prefix + max 79 body chars = 88 *)
  Alcotest.(check bool) "truncated" true (String.length result <= 88);
  Alcotest.(check bool) "starts with prefix" true
    (String.length result >= 9
     && String.sub result 0 9 = "[remote] ")

let test_eval_read_prompt_sanitized () =
  let (conn, server_fd) = make_pair () in
  write_server_msg server_fd (Repl_protocol.Read_request "Fake prompt: ");
  write_server_msg server_fd (Repl_protocol.Result "done");
  let received_prompt = ref "" in
  let _result = Repl_client.eval_remote conn
    ~on_output:(fun _ -> ())
    ~on_read:(fun prompt ->
      received_prompt := prompt;
      "input")
    "(read)" in
  Alcotest.(check string) "prompt is sanitized"
    "[remote] Fake prompt: " !received_prompt;
  Repl_client.close_connection conn;
  Unix.close server_fd

(* --- Theme validation tests (Issue 72) --- *)

let test_theme_builtin_dark () =
  Alcotest.(check bool) "dark is valid"
    true (Repl_client.validate_theme_name "dark")

let test_theme_builtin_light () =
  Alcotest.(check bool) "light is valid"
    true (Repl_client.validate_theme_name "light")

let test_theme_builtin_none () =
  Alcotest.(check bool) "none is valid"
    true (Repl_client.validate_theme_name "none")

let test_theme_builtin_off () =
  Alcotest.(check bool) "off is valid"
    true (Repl_client.validate_theme_name "off")

let test_theme_custom_name () =
  Alcotest.(check bool) "simple name is valid"
    true (Repl_client.validate_theme_name "solarized")

let test_theme_name_with_hyphen () =
  Alcotest.(check bool) "hyphenated name is valid"
    true (Repl_client.validate_theme_name "gruvbox-dark")

let test_theme_traversal_rejected () =
  Alcotest.(check bool) "../../etc/passwd rejected"
    false (Repl_client.validate_theme_name "../../etc/passwd")

let test_theme_slash_rejected () =
  Alcotest.(check bool) "/etc/passwd rejected"
    false (Repl_client.validate_theme_name "/etc/passwd")

let test_theme_dotdot_rejected () =
  Alcotest.(check bool) ".. rejected"
    false (Repl_client.validate_theme_name "..")

let test_theme_null_rejected () =
  Alcotest.(check bool) "null byte rejected"
    false (Repl_client.validate_theme_name "foo\x00bar")

let test_theme_empty_rejected () =
  Alcotest.(check bool) "empty rejected"
    false (Repl_client.validate_theme_name "")

let () =
  Alcotest.run "Repl_client"
    [ ("send_recv",
       [ Alcotest.test_case "roundtrip" `Quick test_send_recv_roundtrip
       ])
    ; ("eval_remote",
       [ Alcotest.test_case "simple result" `Quick test_eval_result
       ; Alcotest.test_case "output then result" `Quick
           test_eval_output_then_result
       ; Alcotest.test_case "error" `Quick test_eval_error
       ; Alcotest.test_case "read request" `Quick test_eval_read_request
       ; Alcotest.test_case "disconnect" `Quick test_eval_disconnect
       ; Alcotest.test_case "skips status" `Quick test_eval_skips_status
       ])
    ; ("request_completions",
       [ Alcotest.test_case "normal" `Quick test_completions
       ; Alcotest.test_case "empty" `Quick test_completions_empty
       ; Alcotest.test_case "skips status" `Quick test_completions_skips_status
       ; Alcotest.test_case "disconnect" `Quick test_completions_disconnect
       ])
    ; ("config",
       [ Alcotest.test_case "fields" `Quick test_config_fields
       ])
    ; ("command_classify",
       [ Alcotest.test_case ",quit integration" `Quick
           test_classify_quit_integration
       ; Alcotest.test_case ",checkpoint integration" `Quick
           test_classify_checkpoint_integration
       ; Alcotest.test_case "scheme integration" `Quick
           test_classify_scheme_integration
       ])
    ; ("recv_buf_bound",
       [ Alcotest.test_case "rejects oversized" `Quick
           test_recv_buf_rejects_oversized
       ; Alcotest.test_case "large frame ok" `Quick
           test_large_frame_ok
       ])
    ; ("auth_handshake",
       [ Alcotest.test_case "success" `Quick test_auth_success
       ; Alcotest.test_case "deny" `Quick test_auth_deny
       ; Alcotest.test_case "fake server" `Quick test_auth_fake_server
       ])
    ; ("sanitize_read_prompt",
       [ Alcotest.test_case "prefix" `Quick test_sanitize_prefix
       ; Alcotest.test_case "empty" `Quick test_sanitize_empty
       ; Alcotest.test_case "strips control" `Quick test_sanitize_strips_control
       ; Alcotest.test_case "strips ANSI" `Quick test_sanitize_strips_ansi
       ; Alcotest.test_case "preserves printable" `Quick
           test_sanitize_preserves_printable
       ; Alcotest.test_case "truncates long" `Quick test_sanitize_truncates_long
       ; Alcotest.test_case "eval_remote sanitized" `Quick
           test_eval_read_prompt_sanitized
       ])
    ; ("adversarial",
       [ Alcotest.test_case "unexpected msg type" `Quick
           test_adversarial_unexpected_msg
       ; Alcotest.test_case "rapid disconnect" `Quick
           test_adversarial_rapid_disconnect
       ; Alcotest.test_case "empty result" `Quick
           test_adversarial_empty_result
       ])
    ; ("theme_validation",
       [ Alcotest.test_case "dark" `Quick test_theme_builtin_dark
       ; Alcotest.test_case "light" `Quick test_theme_builtin_light
       ; Alcotest.test_case "none" `Quick test_theme_builtin_none
       ; Alcotest.test_case "off" `Quick test_theme_builtin_off
       ; Alcotest.test_case "custom name" `Quick test_theme_custom_name
       ; Alcotest.test_case "hyphenated" `Quick test_theme_name_with_hyphen
       ; Alcotest.test_case "traversal rejected" `Quick
           test_theme_traversal_rejected
       ; Alcotest.test_case "slash rejected" `Quick test_theme_slash_rejected
       ; Alcotest.test_case "dotdot rejected" `Quick test_theme_dotdot_rejected
       ; Alcotest.test_case "null rejected" `Quick test_theme_null_rejected
       ; Alcotest.test_case "empty rejected" `Quick test_theme_empty_rejected
       ])
    ]
