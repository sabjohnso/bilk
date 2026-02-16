(** Authenticated encryption for remote REPL communication.

    Provides ChaCha20-Poly1305 AEAD encryption with replay protection.
    Each direction (client-to-server, server-to-client) maintains an
    independent sequence counter encoded in the nonce, preventing
    cross-direction and replay attacks.

    Session tokens use HMAC-SHA256 for reconnection authentication. *)

(** {1 Keys} *)

type key
(** An opaque 256-bit symmetric key. *)

val generate_key : unit -> key
(** [generate_key ()] returns a fresh random key.
    Initializes the system RNG on first call. *)

val key_to_base64 : key -> string
(** [key_to_base64 k] encodes [k] as a URL-safe base64 string
    (no padding). *)

val key_of_base64 : string -> key option
(** [key_of_base64 s] decodes a base64-encoded key.
    Returns [None] if [s] is malformed or not 32 bytes. *)

(** {1 Cipher state} *)

type direction = Client_to_server | Server_to_client
(** Traffic direction, encoded in the nonce to prevent cross-direction
    attacks. *)

type cipher_state
(** Mutable state for one direction of an encrypted channel.
    Tracks sequence numbers for replay protection. *)

val create_cipher : key -> direction -> cipher_state
(** [create_cipher k dir] creates a fresh cipher state for the given
    direction, with sequence counter starting at zero. *)

val encrypt : cipher_state -> string -> string
(** [encrypt cs plaintext] encrypts [plaintext] and advances the
    sequence counter. Returns [nonce (12 bytes) ++ ciphertext_with_tag].
    The ciphertext is [String.length plaintext + 28] bytes. *)

val decrypt : cipher_state -> string -> string option
(** [decrypt cs packet] decrypts a packet produced by [encrypt].
    Returns [None] if authentication fails, the direction byte is
    wrong, or the sequence number is less than the highest previously
    seen (replay protection). On success, updates the highest-seen
    counter. *)

(** {1 Session tokens} *)

val session_token : key -> string -> string
(** [session_token k session_id] computes an HMAC-SHA256 authentication
    token for [session_id] using [k]. *)

val verify_token : key -> string -> string -> bool
(** [verify_token k session_id token] returns [true] if [token] is a
    valid authentication token for [session_id] under [k].
    Uses constant-time comparison. *)
