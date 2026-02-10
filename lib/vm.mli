(** Bytecode virtual machine.

    Executes {!Datum.code} objects in a given environment.  The VM is an
    accumulator machine with a value stack and a call stack.

    Per R7RS §6.3, only [#f] is false — everything else (including [0],
    [""], [()]) is truthy. *)

(** {1 Exceptions} *)

exception Runtime_error of string
(** Raised when the VM encounters an error at runtime (e.g. unbound
    variable, wrong number of arguments, type error in a primitive). *)

(** {1 Debug state} *)

(** Mutable state snapshot populated by the VM when debugging is active.
    Provides access to the current environment, call frames, code, and
    program counter at each procedure call site. *)
type debug_state = {
  mutable dbg_env : Datum.env;
  (** The environment at the current call site. *)
  mutable dbg_frames : Datum.call_frame list;
  (** The call stack (Standard frames only) at the current call site. *)
  mutable dbg_code : Datum.code;
  (** The code object being executed at the current call site. *)
  mutable dbg_pc : int;
  (** The program counter at the current call site. *)
}

val make_debug_state : unit -> debug_state
(** [make_debug_state ()] returns a fresh debug state with empty defaults. *)

(** {1 Execution} *)

val execute :
  ?winds:Datum.wind list ref ->
  ?on_call:(Loc.t -> Datum.t -> Datum.t list -> unit) ->
  ?on_return:(Loc.t -> Datum.t -> unit) ->
  ?debug_state:debug_state ->
  Datum.env -> Datum.code -> Datum.t
(** [execute ?winds ?on_call ?on_return ?debug_state env code] runs
    bytecode [code] in environment [env] and returns the final value
    (the accumulator when [Halt] is reached).
    @param winds  Shared mutable wind stack for [dynamic-wind] and
                  continuation invocation.  If omitted, a fresh empty
                  wind stack is created.
    @param on_call  Optional callback fired before each procedure call.
                    Receives the source location, procedure, and arguments.
    @param on_return  Optional callback fired after each procedure return.
                      Receives the source location and return value.
    @param debug_state  When provided, the VM populates this state before
                        firing [on_call], giving debuggers access to the
                        current environment and call stack.
    @raise Runtime_error on runtime errors. *)
