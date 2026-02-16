(** Named session store — maps session names to files under
    [{home}/sessions/{name}.bses].

    Session names are validated to reject path separators, [.], [..],
    and null bytes.  All functions take an explicit [~home] so tests
    can use temporary directories. *)

exception Store_error of string

val validate_name : string -> (string, string) result
(** [validate_name s] returns [Ok s] for safe session names,
    [Error msg] for names containing path separators, [.], [..],
    null bytes, or the empty string. *)

val session_path : home:string -> string -> string
(** [session_path ~home name] returns [{home}/sessions/{name}.bses].
    Does {b not} validate the name. *)

val save : home:string -> Session.t -> string -> unit
(** [save ~home session name] validates [name], creates the sessions
    directory if needed, and delegates to {!Session.save}.
    @raise Store_error if the name is invalid or I/O fails. *)

val load : home:string -> string -> Session.t
(** [load ~home name] validates [name] and delegates to
    {!Session.load}.
    @raise Store_error if the name is invalid or the file is missing. *)

val list : home:string -> unit -> string list
(** [list ~home ()] returns sorted names of [*.bses] files in
    [{home}/sessions/].  Returns [[]] if the directory does not exist. *)

val delete : home:string -> string -> unit
(** [delete ~home name] removes the named session file.
    @raise Store_error if the name is invalid. *)
