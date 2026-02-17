open Bilk

let () = Mirage_crypto_rng_unix.use_default ()

(* --- Helpers --- *)

let nonce_len = 12
let tag_len = 16

(* --- Alcotest example tests --- *)

let test_encrypt_decrypt_roundtrip () =
  let key = Repl_crypto.generate_key () in
  let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let dec = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let plaintext = "hello, encrypted world!" in
  let packet = Repl_crypto.encrypt enc plaintext in
  let result = Repl_crypto.decrypt dec packet in
  Alcotest.(check (option string)) "roundtrip"
    (Some plaintext) result

let test_wrong_key_fails () =
  let k1 = Repl_crypto.generate_key () in
  let k2 = Repl_crypto.generate_key () in
  let enc = Repl_crypto.create_cipher k1 Repl_crypto.Client_to_server in
  let dec = Repl_crypto.create_cipher k2 Repl_crypto.Client_to_server in
  let packet = Repl_crypto.encrypt enc "secret" in
  Alcotest.(check (option string)) "wrong key"
    None (Repl_crypto.decrypt dec packet)

let test_replay_rejected () =
  let key = Repl_crypto.generate_key () in
  let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let dec = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let p1 = Repl_crypto.encrypt enc "first" in
  let p2 = Repl_crypto.encrypt enc "second" in
  (* Decrypt second first — advances highest_seen *)
  let _ = Repl_crypto.decrypt dec p2 in
  (* Now first should be rejected (sequence < highest_seen) *)
  Alcotest.(check (option string)) "replay rejected"
    None (Repl_crypto.decrypt dec p1)

let test_direction_isolation () =
  let key = Repl_crypto.generate_key () in
  let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let dec = Repl_crypto.create_cipher key Repl_crypto.Server_to_client in
  let packet = Repl_crypto.encrypt enc "hello" in
  Alcotest.(check (option string)) "wrong direction"
    None (Repl_crypto.decrypt dec packet)

let test_base64_roundtrip () =
  let key = Repl_crypto.generate_key () in
  let encoded = Repl_crypto.key_to_base64 key in
  let decoded = Repl_crypto.key_of_base64 encoded in
  (* We can't compare keys directly, so encrypt with original,
     decrypt with decoded *)
  match decoded with
  | None -> Alcotest.fail "base64 roundtrip returned None"
  | Some key2 ->
    let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
    let dec = Repl_crypto.create_cipher key2 Repl_crypto.Client_to_server in
    let packet = Repl_crypto.encrypt enc "test" in
    Alcotest.(check (option string)) "base64 roundtrip decrypt"
      (Some "test") (Repl_crypto.decrypt dec packet)

let test_session_token_verify () =
  let key = Repl_crypto.generate_key () in
  let token = Repl_crypto.session_token key "session-42" in
  Alcotest.(check bool) "correct key verifies"
    true (Repl_crypto.verify_token key "session-42" token)

let test_session_token_wrong_key () =
  let k1 = Repl_crypto.generate_key () in
  let k2 = Repl_crypto.generate_key () in
  let token = Repl_crypto.session_token k1 "session-42" in
  Alcotest.(check bool) "wrong key fails"
    false (Repl_crypto.verify_token k2 "session-42" token)

let test_empty_plaintext () =
  let key = Repl_crypto.generate_key () in
  let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let dec = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let packet = Repl_crypto.encrypt enc "" in
  Alcotest.(check (option string)) "empty plaintext"
    (Some "") (Repl_crypto.decrypt dec packet)

let test_large_plaintext () =
  let key = Repl_crypto.generate_key () in
  let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let dec = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
  let plaintext = String.make 65536 'x' in
  let packet = Repl_crypto.encrypt enc plaintext in
  Alcotest.(check (option string)) "64KB roundtrip"
    (Some plaintext) (Repl_crypto.decrypt dec packet)

(* --- QCheck2 property tests --- *)

let prop_encrypt_decrypt_roundtrip =
  QCheck2.Test.make ~count:200
    ~name:"arbitrary plaintext survives roundtrip"
    QCheck2.Gen.(string_size (int_range 0 1000))
    (fun plaintext ->
       let key = Repl_crypto.generate_key () in
       let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
       let dec = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
       let packet = Repl_crypto.encrypt enc plaintext in
       Repl_crypto.decrypt dec packet = Some plaintext)

let prop_base64_roundtrip =
  QCheck2.Test.make ~count:200
    ~name:"any key survives base64 roundtrip"
    QCheck2.Gen.unit
    (fun () ->
       let key = Repl_crypto.generate_key () in
       let encoded = Repl_crypto.key_to_base64 key in
       match Repl_crypto.key_of_base64 encoded with
       | None -> false
       | Some key2 ->
         (* Verify by encrypting with one and decrypting with the other *)
         let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
         let dec = Repl_crypto.create_cipher key2 Repl_crypto.Client_to_server in
         let packet = Repl_crypto.encrypt enc "test" in
         Repl_crypto.decrypt dec packet = Some "test")

let prop_ciphertext_has_overhead =
  QCheck2.Test.make ~count:200
    ~name:"ciphertext has fixed overhead (nonce + tag)"
    QCheck2.Gen.(string_size (int_range 0 500))
    (fun plaintext ->
       let key = Repl_crypto.generate_key () in
       let enc = Repl_crypto.create_cipher key Repl_crypto.Client_to_server in
       let packet = Repl_crypto.encrypt enc plaintext in
       String.length packet = String.length plaintext + nonce_len + tag_len)

(* --- Auth challenge/response tests --- *)

let test_auth_challenge_length () =
  let nonce = Repl_crypto.auth_challenge () in
  Alcotest.(check int) "nonce is 32 bytes" 32 (String.length nonce)

let test_auth_response_deterministic () =
  let key = Repl_crypto.generate_key () in
  let nonce = Repl_crypto.auth_challenge () in
  let r1 = Repl_crypto.auth_response key nonce in
  let r2 = Repl_crypto.auth_response key nonce in
  Alcotest.(check string) "same key+nonce => same response" r1 r2

let test_verify_auth_correct () =
  let key = Repl_crypto.generate_key () in
  let nonce = Repl_crypto.auth_challenge () in
  let response = Repl_crypto.auth_response key nonce in
  Alcotest.(check bool) "verify correct"
    true (Repl_crypto.verify_auth key nonce response)

let test_verify_auth_wrong_response () =
  let key = Repl_crypto.generate_key () in
  let nonce = Repl_crypto.auth_challenge () in
  let bad_response = String.make 32 '\x00' in
  Alcotest.(check bool) "reject wrong response"
    false (Repl_crypto.verify_auth key nonce bad_response)

let test_verify_auth_wrong_key () =
  let k1 = Repl_crypto.generate_key () in
  let k2 = Repl_crypto.generate_key () in
  let nonce = Repl_crypto.auth_challenge () in
  let response = Repl_crypto.auth_response k1 nonce in
  Alcotest.(check bool) "reject wrong key"
    false (Repl_crypto.verify_auth k2 nonce response)

let test_key_fingerprint_format () =
  let key = Repl_crypto.generate_key () in
  let fp = Repl_crypto.key_fingerprint key in
  (* SHA-256 = 32 bytes = 32 hex pairs with 31 colons = 95 chars *)
  Alcotest.(check int) "fingerprint length" 95 (String.length fp);
  (* All colons in right positions *)
  let parts = String.split_on_char ':' fp in
  Alcotest.(check int) "32 hex pairs" 32 (List.length parts);
  List.iter (fun part ->
    Alcotest.(check int) "each part is 2 chars" 2 (String.length part)
  ) parts

let prop_verify_auth_roundtrip =
  QCheck2.Test.make ~count:200
    ~name:"verify_auth(k, n, auth_response(k, n)) = true"
    QCheck2.Gen.unit
    (fun () ->
       let key = Repl_crypto.generate_key () in
       let nonce = Repl_crypto.auth_challenge () in
       let response = Repl_crypto.auth_response key nonce in
       Repl_crypto.verify_auth key nonce response)

let prop_different_keys_different_responses =
  QCheck2.Test.make ~count:200
    ~name:"different keys produce different responses"
    QCheck2.Gen.unit
    (fun () ->
       let k1 = Repl_crypto.generate_key () in
       let k2 = Repl_crypto.generate_key () in
       let nonce = Repl_crypto.auth_challenge () in
       let r1 = Repl_crypto.auth_response k1 nonce in
       let r2 = Repl_crypto.auth_response k2 nonce in
       r1 <> r2)

let () =
  Alcotest.run "Repl_crypto"
    [ ("encrypt_decrypt",
       [ Alcotest.test_case "roundtrip" `Quick test_encrypt_decrypt_roundtrip
       ; Alcotest.test_case "wrong key" `Quick test_wrong_key_fails
       ; Alcotest.test_case "replay rejected" `Quick test_replay_rejected
       ; Alcotest.test_case "direction isolation" `Quick test_direction_isolation
       ; Alcotest.test_case "empty plaintext" `Quick test_empty_plaintext
       ; Alcotest.test_case "large plaintext" `Quick test_large_plaintext
       ])
    ; ("base64",
       [ Alcotest.test_case "roundtrip" `Quick test_base64_roundtrip
       ])
    ; ("session_token",
       [ Alcotest.test_case "verify" `Quick test_session_token_verify
       ; Alcotest.test_case "wrong key" `Quick test_session_token_wrong_key
       ])
    ; ("auth",
       [ Alcotest.test_case "challenge length" `Quick test_auth_challenge_length
       ; Alcotest.test_case "response deterministic" `Quick
           test_auth_response_deterministic
       ; Alcotest.test_case "verify correct" `Quick test_verify_auth_correct
       ; Alcotest.test_case "reject wrong response" `Quick
           test_verify_auth_wrong_response
       ; Alcotest.test_case "reject wrong key" `Quick test_verify_auth_wrong_key
       ; Alcotest.test_case "fingerprint format" `Quick
           test_key_fingerprint_format
       ])
    ; ("properties",
       List.map QCheck_alcotest.to_alcotest
         [ prop_encrypt_decrypt_roundtrip
         ; prop_base64_roundtrip
         ; prop_ciphertext_has_overhead
         ; prop_verify_auth_roundtrip
         ; prop_different_keys_different_responses
         ])
    ]
