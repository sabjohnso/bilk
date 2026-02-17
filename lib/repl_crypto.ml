(* Authenticated encryption for remote REPL communication.

   Uses ChaCha20-Poly1305 AEAD with direction-tagged nonces and
   sequence-number-based replay protection. Session tokens use
   HMAC-SHA256 with constant-time verification. *)

(* --- Key management --- *)

type key = string  (* 32 bytes *)

let key_len = 32

let rng_initialized = ref false

let ensure_rng () =
  if not !rng_initialized then begin
    Mirage_crypto_rng_unix.use_default ();
    rng_initialized := true
  end

let generate_key () =
  ensure_rng ();
  Mirage_crypto_rng.generate key_len

(* --- Base64 encoding (URL-safe, no padding) --- *)

let key_to_base64 k =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet k

let key_of_base64 s =
  match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s with
  | Ok raw when String.length raw = key_len -> Some raw
  | _ -> None

(* --- Cipher state --- *)

type direction = Client_to_server | Server_to_client

let direction_byte = function
  | Client_to_server -> '\x00'
  | Server_to_client -> '\x01'

type cipher_state = {
  aead_key : Mirage_crypto.Chacha20.key;
  direction : direction;
  mutable seq : int64;
  mutable highest_seen : int64;
}

let nonce_len = 12

let create_cipher key dir =
  { aead_key = Mirage_crypto.Chacha20.of_secret key;
    direction = dir;
    seq = 0L;
    highest_seen = (-1L); }

(* Build 12-byte nonce: 4 zero bytes + 1 direction byte + 7 bytes big-endian seq *)
let make_nonce dir seq =
  let buf = Bytes.make nonce_len '\x00' in
  Bytes.set buf 4 (direction_byte dir);
  (* 7 bytes of seq in big-endian, occupying bytes 5..11 *)
  for i = 0 to 6 do
    let shift = (6 - i) * 8 in
    Bytes.set buf (5 + i) (Char.chr (Int64.to_int (Int64.shift_right_logical seq shift) land 0xff))
  done;
  Bytes.unsafe_to_string buf

let encrypt cs plaintext =
  let nonce = make_nonce cs.direction cs.seq in
  cs.seq <- Int64.add cs.seq 1L;
  let ciphertext = Mirage_crypto.Chacha20.authenticate_encrypt
      ~key:cs.aead_key ~nonce plaintext in
  nonce ^ ciphertext

let decrypt cs packet =
  if String.length packet < nonce_len then None
  else begin
    let nonce = String.sub packet 0 nonce_len in
    let ciphertext = String.sub packet nonce_len
        (String.length packet - nonce_len) in
    (* Verify direction byte *)
    let expected_dir = direction_byte cs.direction in
    if nonce.[4] <> expected_dir then None
    else begin
      (* Extract sequence number from nonce bytes 5..11 *)
      let seq = ref 0L in
      for i = 0 to 6 do
        seq := Int64.logor
            (Int64.shift_left !seq 8)
            (Int64.of_int (Char.code nonce.[5 + i]))
      done;
      (* Replay protection: reject if seq < highest_seen *)
      if !seq <= cs.highest_seen then None
      else begin
        match Mirage_crypto.Chacha20.authenticate_decrypt
                ~key:cs.aead_key ~nonce ciphertext with
        | None -> None
        | Some plaintext ->
          cs.highest_seen <- !seq;
          Some plaintext
      end
    end
  end

(* --- Session tokens --- *)

let session_token key session_id =
  let data = "bilk-session:" ^ session_id in
  Digestif.SHA256.(to_raw_string (hmac_string ~key data))

let verify_token key session_id token =
  let expected = session_token key session_id in
  Eqaf.equal expected token

(* --- Authentication --- *)

let auth_challenge () =
  ensure_rng ();
  Mirage_crypto_rng.generate 32

let auth_response key nonce =
  let data = "bilk-auth:" ^ nonce in
  Digestif.SHA256.(to_raw_string (hmac_string ~key data))

let verify_auth key nonce response =
  let expected = auth_response key nonce in
  Eqaf.equal expected response

let key_fingerprint key =
  let hash = Digestif.SHA256.(to_raw_string (digest_string key)) in
  let buf = Buffer.create 95 in
  String.iteri (fun i c ->
    if i > 0 then Buffer.add_char buf ':';
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))
  ) hash;
  Buffer.contents buf
