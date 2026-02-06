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

val execute : Datum.env -> Datum.code -> Datum.t
(** [execute env code] runs bytecode [code] in environment [env] and
    returns the final value (the accumulator when [Halt] is reached).
    @raise Runtime_error on runtime errors. *)
