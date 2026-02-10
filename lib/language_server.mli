(** Language server implementing the Language Server Protocol (LSP).

    Provides diagnostics, hover, completion, go-to-definition, document
    symbols, and semantic tokens for Scheme source files.  The server
    communicates over stdin/stdout or a TCP socket using JSON-RPC 2.0
    with Content-Length framing. *)

(** {1 Exceptions} *)

exception Lsp_server_error of string
(** Raised on language server errors. *)

(** {1 Types} *)

(** The language server state. *)
type t

(** {1 Core API} *)

val create : Instance.t -> t
(** [create inst] returns a new language server backed by [inst]. *)

val run_session : t -> in_channel -> out_channel -> unit
(** [run_session t ic oc] runs the LSP session loop, reading JSON-RPC
    messages from [ic] and writing responses/notifications to [oc].
    Returns when the client sends "exit". *)
