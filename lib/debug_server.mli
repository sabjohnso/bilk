(** Debug server implementing the Debug Adapter Protocol (DAP).

    Provides breakpoint management, stepping, stack inspection, and
    variable evaluation for interactive debugging of Scheme programs.
    The core engine is a state machine with no I/O; the DAP session
    loop handles communication. *)

(** {1 Exceptions} *)

exception Debug_error of string
(** Raised on debugger errors. *)

(** {1 Types} *)

(** A breakpoint. *)
type breakpoint = {
  bp_id : int;
  (** Unique breakpoint identifier. *)
  bp_file : string;
  (** Source file path. *)
  bp_line : int;
  (** Source line number. *)
  bp_verified : bool;
  (** Whether the breakpoint was verified (always [true] for now). *)
}

(** Why the debugger stopped. *)
type stop_reason =
  | Breakpoint of int
  | Step
  | Pause_request
  | Entry

(** Stepping mode. *)
type step_mode =
  | Run
  | Step_in
  | Step_over
  | Step_out
  | Paused

(** A stack frame for DAP. *)
type stack_frame = {
  sf_id : int;
  (** Frame identifier. *)
  sf_name : string;
  (** Procedure name. *)
  sf_file : string;
  (** Source file path. *)
  sf_line : int;
  (** Source line. *)
  sf_col : int;
  (** Source column. *)
}

(** A variable scope. *)
type scope = {
  sc_name : string;
  (** Scope name (e.g. "Locals", "Globals"). *)
  sc_ref : int;
  (** Variables reference handle. *)
}

(** A variable. *)
type variable = {
  var_name : string;
  (** Variable name. *)
  var_value : string;
  (** Display value. *)
  var_type : string;
  (** Type name. *)
  var_ref : int;
  (** Variables reference (0 = leaf, >0 = expandable). *)
}

(** The debug server state. *)
type t

(** {1 Core API} *)

val create : Instance.t -> t
(** [create inst] returns a new debug server for [inst]. *)

val set_breakpoints : t -> string -> int list -> breakpoint list
(** [set_breakpoints t file lines] sets breakpoints in [file] at the
    given [lines], replacing any previous breakpoints for that file.
    Returns the created breakpoints. *)

val clear_breakpoints : t -> string -> unit
(** [clear_breakpoints t file] removes all breakpoints for [file]. *)

val on_call : t -> Loc.t -> Datum.t -> Datum.t list -> stop_reason option
(** [on_call t loc proc args] is called by the VM hook on each
    procedure call.  Returns [Some reason] if the debugger should
    stop, [None] to continue. *)

val on_return : t -> Loc.t -> Datum.t -> unit
(** [on_return t loc value] is called by the VM hook on each return. *)

val continue : t -> unit
(** [continue t] resumes execution in run mode. *)

val step_in : t -> unit
(** [step_in t] resumes execution stopping at the next call. *)

val step_over : t -> unit
(** [step_over t] resumes execution stopping at the next call
    at the same or lower depth. *)

val step_out : t -> unit
(** [step_out t] resumes execution stopping when returning to
    the caller. *)

val request_pause : t -> unit
(** [request_pause t] sets a flag to pause at the next call. *)

(** {1 Inspection API} *)

val stack_trace : t -> stack_frame list
(** [stack_trace t] returns the current call stack. *)

val scopes : t -> int -> scope list
(** [scopes t frame_id] returns the scopes for the given stack frame. *)

val variables : t -> int -> variable list
(** [variables t var_ref] returns variables for the given reference handle. *)

val evaluate : t -> string -> int option -> string
(** [evaluate t expr frame_id] evaluates [expr] in the context of the
    given stack frame (or global env if [None]).  Returns the result
    as a display string. *)

val reset_handles : t -> unit
(** [reset_handles t] clears all variable reference handles.
    Called when resuming execution. *)

(** {1 DAP session} *)

val run_session :
  t -> in_channel -> out_channel -> string -> string list -> unit
(** [run_session t dap_in dap_out program_path args] runs a full
    DAP debug session.  Reads DAP messages from [dap_in], writes
    responses and events to [dap_out], executes [program_path] with
    the debugger hooks active. *)
