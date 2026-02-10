(** Runtime profiler using VM instrumentation hooks.

    Collects per-procedure call counts, wall-clock timing (total and self),
    trace events for Chrome DevTools, and collapsed flame stacks for flame
    graph generation.  Uses {!Instance.on_call} / {!Instance.on_return} hooks
    installed by {!install}.

    @since M24 *)

(** {1 Exceptions} *)

exception Profiler_error of string
(** Raised on profiler errors (e.g. misuse of the API). *)

(** {1 Types} *)

(** Identifies a profiled procedure by name and definition site. *)
type proc_key = {
  pk_name : string;    (** Procedure name (e.g. ["fibonacci"], ["+"]) *)
  pk_loc : Loc.t;      (** Definition site, or {!Loc.none} for primitives *)
}

(** Accumulated statistics for one procedure. *)
type proc_stats = {
  mutable call_count : int;   (** Number of times called *)
  mutable total_time : float; (** Wall-clock seconds including children *)
  mutable self_time : float;  (** Wall-clock seconds excluding children *)
}

(** A single trace event for Chrome Trace Event format. *)
type trace_event = {
  te_ph : char;      (** Phase: ['B'] (begin) or ['E'] (end) *)
  te_name : string;  (** Procedure name *)
  te_cat : string;   (** Category (always ["scheme"]) *)
  te_ts : float;     (** Timestamp in microseconds since profiler start *)
}

(** The profiler state (abstract). *)
type t

(** {1 Lifecycle} *)

val create : unit -> t
(** [create ()] returns a fresh profiler with no collected data. *)

val install : t -> Instance.t -> unit
(** [install prof inst] installs the profiler's hooks into the instance.
    Sets [inst.on_call], [inst.on_return], and [inst.debug_state].
    @raise Profiler_error if hooks are already installed on [inst]. *)

val uninstall : Instance.t -> unit
(** [uninstall inst] removes profiler hooks from the instance, restoring
    [on_call], [on_return], and [debug_state] to [None]. *)

val finalize : t -> unit
(** [finalize prof] flushes any remaining stack entries, recording their
    timing data.  Must be called after execution completes and before
    reading results. *)

(** {1 Accessors} *)

val entries : t -> (proc_key * proc_stats) list
(** [entries prof] returns all collected procedure statistics as an
    association list. *)

val trace_events : t -> trace_event list
(** [trace_events prof] returns all trace events in chronological order. *)

val flame_stacks : t -> (string * float) list
(** [flame_stacks prof] returns collapsed flame stacks as
    [(path, self_time_us)] pairs, where [path] is a semicolon-separated
    stack of procedure names and [self_time_us] is self time in
    microseconds. *)
