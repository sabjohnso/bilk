(** Bytecode virtual machine.

    Executes {!Datum.code} objects in a given environment.  The VM is an
    accumulator machine with a value stack and a call stack.

    Per R7RS §6.3, only [#f] is false — everything else (including [0],
    [""], [()]) is truthy. *)

(** {1 Exceptions} *)

exception Runtime_error of string
(** Raised when the VM encounters an error at runtime (e.g. unbound
    variable, wrong number of arguments, type error in a primitive). *)

(** {1 Execution} *)

val execute :
  ?winds:Datum.wind list ref ->
  ?on_call:(Loc.t -> Datum.t -> Datum.t list -> unit) ->
  ?on_return:(Loc.t -> Datum.t -> unit) ->
  Datum.env -> Datum.code -> Datum.t
(** [execute ?winds ?on_call ?on_return env code] runs bytecode [code] in
    environment [env] and returns the final value (the accumulator when
    [Halt] is reached).
    @param winds  Shared mutable wind stack for [dynamic-wind] and
                  continuation invocation.  If omitted, a fresh empty
                  wind stack is created.
    @param on_call  Optional callback fired before each procedure call.
                    Receives the source location, procedure, and arguments.
    @param on_return  Optional callback fired after each procedure return.
                      Receives the source location and return value.
    @raise Runtime_error on runtime errors. *)
