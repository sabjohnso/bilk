type config = {
  host : string;
  port : int;
}

let send_msg fd msg =
  let buf = Buffer.create 256 in
  Repl_protocol.write_client_msg buf msg;
  let data = Buffer.contents buf in
  let _ = Unix.write_substring fd data 0 (String.length data) in
  ()

let handle_server_msg msg =
  match msg with
  | Repl_protocol.Output s ->
    let _ = Unix.write_substring Unix.stdout s 0 (String.length s) in
    true
  | Repl_protocol.Scrollback s ->
    let _ = Unix.write_substring Unix.stdout s 0 (String.length s) in
    true
  | Repl_protocol.Pong -> true
  | Repl_protocol.Server_exit ->
    false

let connect config =
  let addr = Unix.inet_addr_of_string
      (try config.host
       with _ ->
         let entry = Unix.gethostbyname config.host in
         Unix.string_of_inet_addr entry.Unix.h_addr_list.(0))
  in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (try
     Unix.connect sock (Unix.ADDR_INET (addr, config.port))
   with e ->
     Unix.close sock;
     raise e);
  let term = Terminal.enter_raw Unix.stdin in
  (* Send initial terminal size *)
  let (rows, cols) = Terminal.get_terminal_size term in
  send_msg sock (Repl_protocol.Resize (rows, cols));
  let running = ref true in
  let recv_buf = Buffer.create 4096 in
  while !running do
    let (ready, _, _) = Unix.select [Unix.stdin; sock] [] [] 1.0 in
    List.iter (fun fd ->
      if fd == Unix.stdin then begin
        (* Read local keystrokes and forward *)
        let tmp = Bytes.create 256 in
        let n = try Unix.read fd tmp 0 256 with Unix.Unix_error _ -> 0 in
        if n > 0 then begin
          let s = Bytes.sub_string tmp 0 n in
          (try send_msg sock (Repl_protocol.Key_input s)
           with Unix.Unix_error _ -> running := false)
        end else
          running := false
      end else if fd == sock then begin
        (* Read from server *)
        let tmp = Bytes.create 4096 in
        let n = try Unix.read fd tmp 0 4096 with Unix.Unix_error _ -> 0 in
        if n = 0 then
          running := false
        else begin
          Buffer.add_subbytes recv_buf tmp 0 n;
          let data = Buffer.contents recv_buf in
          let off = ref 0 in
          while Repl_protocol.frame_available data !off do
            let (msg, next) = Repl_protocol.read_server_msg data !off in
            if not (handle_server_msg msg) then
              running := false;
            off := next
          done;
          (* Keep unconsumed bytes *)
          let remaining = String.length data - !off in
          Buffer.clear recv_buf;
          if remaining > 0 then
            Buffer.add_string recv_buf (String.sub data !off remaining)
        end
      end
    ) ready
  done;
  Terminal.leave_raw term;
  Unix.close sock
