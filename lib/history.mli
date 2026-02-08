(** History — Line history with navigation and file persistence.

    Manages a bounded list of history entries with forward/backward navigation,
    consecutive deduplication, and file-based save/load. *)

(** {1 Types} *)

(** Opaque history state. *)
type t

(** {1 Creation} *)

(** [create ~max_length] creates an empty history that holds at most
    [max_length] entries.
    @param max_length maximum number of entries (default 1000) *)
val create : ?max_length:int -> unit -> t

(** {1 Mutation} *)

(** [add t entry] appends [entry] to the history. Empty strings and entries
    identical to the most recent entry are silently ignored. If the history
    exceeds [max_length], the oldest entry is dropped. *)
val add : t -> string -> unit

(** {1 Navigation} *)

(** [prev t] moves the navigation cursor backward (toward older entries) and
    returns the entry, or [None] if already at the oldest entry. The first
    call after {!reset_nav} returns the most recent entry. *)
val prev : t -> string option

(** [next t] moves the navigation cursor forward (toward newer entries) and
    returns the entry, or [None] if already past the newest entry. *)
val next : t -> string option

(** [reset_nav t] resets the navigation cursor to the "after newest" position.
    Should be called when a new prompt starts. *)
val reset_nav : t -> unit

(** {1 Queries} *)

(** [length t] returns the current number of entries. *)
val length : t -> int

(** [to_list t] returns all entries from oldest to newest. *)
val to_list : t -> string list

(** {1 File I/O} *)

(** [save_to_file t path] writes all entries to [path], one per line.
    Creates the file if it does not exist, overwrites if it does. *)
val save_to_file : t -> string -> unit

(** [load_from_file t path] loads entries from [path] (one per line) and
    appends them to the history, respecting [max_length]. Silently does
    nothing if the file does not exist. *)
val load_from_file : t -> string -> unit
