(** Bytecode instructions.

    The VM is an accumulator machine: most instructions operate on the
    accumulator register ([acc]).  {!Push} saves [acc] to the value stack.
    {!Call} expects the procedure in [acc] and arguments on the stack.

    Integer arguments refer to indices into the associated {!Datum.code}
    tables (constants, symbols, children). *)

(** {1 Types} *)

(** A single bytecode instruction. *)
type t =
  | Halt                  (** Stop VM; acc holds final result *)
  | Const of int          (** acc = constants\[i\] *)
  | Lookup of int         (** acc = env\[symbols\[i\]\] *)
  | Define of int         (** define symbols\[i\] = acc in env *)
  | SetBang of int        (** set! symbols\[i\] = acc in env *)
  | Push                  (** Push acc onto value stack *)
  | Jump of int           (** Unconditional jump to absolute PC *)
  | JumpFalse of int      (** Jump if acc is [Bool false] *)
  | Call of int            (** Call proc in acc with n args from stack *)
  | TailCall of int        (** Tail call (reuse current frame) *)
  | Return                (** Return acc to caller *)
  | MakeClosure of int    (** acc = Closure(children\[i\], current env) *)

(** {1 Display} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt op] pretty-prints the instruction in a human-readable form. *)

val to_string : t -> string
(** [to_string op] is [Format.asprintf "%a" pp op]. *)
