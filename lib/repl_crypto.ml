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

let base64_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

let base64_encode s =
  let n = String.length s in
  let buf = Buffer.create (((n + 2) / 3) * 4) in
  let i = ref 0 in
  while !i + 2 < n do
    let b0 = Char.code s.[!i] in
    let b1 = Char.code s.[!i + 1] in
    let b2 = Char.code s.[!i + 2] in
    Buffer.add_char buf base64_alphabet.[(b0 lsr 2) land 0x3f];
    Buffer.add_char buf base64_alphabet.[((b0 lsl 4) lor (b1 lsr 4)) land 0x3f];
    Buffer.add_char buf base64_alphabet.[((b1 lsl 2) lor (b2 lsr 6)) land 0x3f];
    Buffer.add_char buf base64_alphabet.[b2 land 0x3f];
    i := !i + 3
  done;
  let rem = n - !i in
  if rem = 1 then begin
    let b0 = Char.code s.[!i] in
    Buffer.add_char buf base64_alphabet.[(b0 lsr 2) land 0x3f];
    Buffer.add_char buf base64_alphabet.[(b0 lsl 4) land 0x3f]
  end else if rem = 2 then begin
    let b0 = Char.code s.[!i] in
    let b1 = Char.code s.[!i + 1] in
    Buffer.add_char buf base64_alphabet.[(b0 lsr 2) land 0x3f];
    Buffer.add_char buf base64_alphabet.[((b0 lsl 4) lor (b1 lsr 4)) land 0x3f];
    Buffer.add_char buf base64_alphabet.[(b1 lsl 2) land 0x3f]
  end;
  Buffer.contents buf

let base64_decode_char c =
  match c with
  | 'A' .. 'Z' -> Some (Char.code c - Char.code 'A')
  | 'a' .. 'z' -> Some (Char.code c - Char.code 'a' + 26)
  | '0' .. '9' -> Some (Char.code c - Char.code '0' + 52)
  | '-' -> Some 62
  | '_' -> Some 63
  | _ -> None

let base64_decode s =
  let n = String.length s in
  let buf = Buffer.create ((n * 3) / 4) in
  let i = ref 0 in
  let ok = ref true in
  while !i < n && !ok do
    let remaining = n - !i in
    if remaining >= 4 then begin
      match base64_decode_char s.[!i],
            base64_decode_char s.[!i + 1],
            base64_decode_char s.[!i + 2],
            base64_decode_char s.[!i + 3] with
      | Some a, Some b, Some c, Some d ->
        Buffer.add_char buf (Char.chr (((a lsl 2) lor (b lsr 4)) land 0xff));
        Buffer.add_char buf (Char.chr (((b lsl 4) lor (c lsr 2)) land 0xff));
        Buffer.add_char buf (Char.chr (((c lsl 6) lor d) land 0xff));
        i := !i + 4
      | _ -> ok := false
    end else if remaining = 3 then begin
      match base64_decode_char s.[!i],
            base64_decode_char s.[!i + 1],
            base64_decode_char s.[!i + 2] with
      | Some a, Some b, Some c ->
        Buffer.add_char buf (Char.chr (((a lsl 2) lor (b lsr 4)) land 0xff));
        Buffer.add_char buf (Char.chr (((b lsl 4) lor (c lsr 2)) land 0xff));
        i := !i + 3
      | _ -> ok := false
    end else if remaining = 2 then begin
      match base64_decode_char s.[!i],
            base64_decode_char s.[!i + 1] with
      | Some a, Some b ->
        Buffer.add_char buf (Char.chr (((a lsl 2) lor (b lsr 4)) land 0xff));
        i := !i + 2
      | _ -> ok := false
    end else
      ok := false
  done;
  if !ok then Some (Buffer.contents buf) else None

let key_to_base64 k = base64_encode k

let key_of_base64 s =
  match base64_decode s with
  | Some raw when String.length raw = key_len -> Some raw
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
