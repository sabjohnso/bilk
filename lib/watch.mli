(** Watch — File system watcher with inotify and polling backends.

    Watches directories for file changes, filtering by extension.
    Uses inotify on Linux for efficiency, falling back to mtime-based
    polling on other platforms or when inotify limits are exceeded.
    Includes built-in debouncing. *)

(** {1 Types} *)

(** A file system event. *)
type event = {
  path : string;
  (** Absolute path to the changed file. *)
  kind : [ `Modified | `Created | `Deleted ];
  (** The kind of change detected. *)
}

(** Which backend is in use. *)
type backend = Inotify | Polling

(** Opaque watcher state. *)
type t

(** {1 Lifecycle} *)

val create : ?poll_interval_ms:int -> ?extensions:string list -> unit -> t
(** [create ?poll_interval_ms ?extensions ()] creates a new watcher.
    @param poll_interval_ms Polling interval in milliseconds (default 200).
    @param extensions File extensions to watch (default [[".sld"; ".scm"]]).
      Only files matching these extensions trigger events. *)

val destroy : t -> unit
(** [destroy t] releases all resources (inotify fd, watch descriptors). *)

(** {1 Directories} *)

val add_directory : t -> string -> unit
(** [add_directory t dir] adds [dir] (and its subdirectories, recursively)
    to the watch set.  On inotify, registers watches; on polling, records
    the directory for scanning. *)

val add_directories : t -> string list -> unit
(** [add_directories t dirs] adds each directory in [dirs]. *)

(** {1 Events} *)

val wait : t -> event list
(** [wait t] blocks until at least one matching event is available.
    Uses inotify blocking (with select) or polling interval sleep.
    Returns a debounced batch of events. *)

val poll : t -> event list
(** [poll t] returns any pending events without blocking.
    Returns [[]] if no changes detected since last poll/wait. *)

(** {1 Introspection} *)

val fd : t -> Unix.file_descr option
(** [fd t] returns the inotify file descriptor, or [None] for polling. *)

val backend : t -> backend
(** [backend t] returns which backend is in use. *)
