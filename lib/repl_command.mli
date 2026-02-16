(** REPL command classification.

    Classifies user input into client-local commands (handled entirely
    by the REPL client), server-side commands (forwarded to the server
    for execution), and ordinary Scheme input. *)

(** Client-local commands that never leave the client. *)
type client_cmd =
  | Quit
  | Help
  | Paredit
  | Theme of string
  | Clear

(** Classification of a line of user input. *)
type classification =
  | Client_local of client_cmd
  | Server_side
  | Scheme_input

val classify : string -> classification
(** [classify input] determines how a line of user input should be
    handled. Lines starting with [,] are commands; the client-local
    set ([,quit], [,q], [,help], [,h], [,paredit], [,theme], [,clear])
    is handled locally; all other comma commands are forwarded to the
    server. Non-comma lines are ordinary Scheme input. *)
