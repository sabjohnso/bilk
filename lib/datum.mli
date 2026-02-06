(** Runtime Scheme values.

    {!t} is the core value representation used by the VM and the embedding
    API.  It carries no source‐location information — that is the domain of
    {!Syntax.t}.  Every Scheme object that can exist at run time is a [Datum.t].

    The type is mutually recursive with {!code}, {!env}, and {!frame} to
    support closures that capture environments containing values. *)

(** {1 Types} *)

(** Identifies built-in procedures that require special VM handling. *)
type intrinsic_id =
  | Intrinsic_call_cc           (** [call/cc] / [call-with-current-continuation] *)
  | Intrinsic_apply             (** [apply] *)
  | Intrinsic_call_with_values  (** [call-with-values] *)
  | Intrinsic_dynamic_wind      (** [dynamic-wind] *)

(** The Scheme value type.  Exposed so callers can pattern‐match on values. *)
type t =
  | Bool of bool               (** [#t] / [#f] *)
  | Fixnum of int              (** Exact integer (machine word) *)
  | Flonum of float            (** Inexact real *)
  | Char of Uchar.t            (** Unicode character *)
  | Str of string              (** Immutable string *)
  | Symbol of string           (** Symbol (uninterned at this level) *)
  | Pair of t * t              (** Cons cell *)
  | Vector of t array          (** Mutable vector *)
  | Bytevector of bytes        (** Mutable bytevector *)
  | Nil                        (** Empty list [()] *)
  | Eof                        (** End‐of‐file object *)
  | Void                       (** Result of [define], [set!], [display], etc. *)
  | Primitive of primitive     (** Built-in OCaml function *)
  | Closure of closure         (** User-defined procedure *)
  | Continuation of continuation (** First-class continuation *)
  | Values of t list           (** Multiple return values *)

(** A built-in primitive function. *)
and primitive = {
  prim_name : string;          (** Name for error messages / display *)
  prim_fn : t list -> t;       (** Implementation *)
  prim_intrinsic : intrinsic_id option;
  (** [Some id] for intrinsics that require special VM dispatch;
      [None] for regular primitives. *)
}

(** A user-defined closure (compiled procedure + captured environment). *)
and closure = {
  clos_name : string;          (** Name for display (or ["<lambda>"]) *)
  clos_code : code;            (** Compiled body *)
  clos_env : env;              (** Captured lexical environment *)
}

(** A compiled code object. *)
and code = {
  instructions : Opcode.t array;  (** Bytecode instructions *)
  constants : t array;            (** Literal values referenced by {!Opcode.Const} *)
  symbols : Symbol.t array;       (** Interned symbols referenced by lookup/define/set! *)
  children : code array;          (** Sub-code objects for nested lambdas *)
  params : Symbol.t array;        (** Parameter names (for binding on call) *)
  variadic : bool;                (** Last param is rest arg *)
  name : string;                  (** Name for debugging *)
}

(** A lexical environment: a chain of frames, searched inner-to-outer. *)
and env = frame list

(** A single environment frame mapping symbol ids to mutable value slots. *)
and frame = (int, t ref) Hashtbl.t

(** A saved call frame for the VM call stack. *)
and call_frame = {
  saved_code : code;    (** Code object of the caller *)
  saved_pc : int;       (** Program counter to resume at *)
  saved_env : env;      (** Environment of the caller *)
  saved_sp : int;       (** Stack pointer of the caller *)
}

(** A dynamic-wind entry pairing before and after thunks. *)
and wind = {
  wind_before : t;      (** Thunk called on entry *)
  wind_after : t;       (** Thunk called on exit *)
}

(** A captured first-class continuation. *)
and continuation = {
  cont_stack : t array;          (** Snapshot of the value stack *)
  cont_sp : int;                 (** Stack pointer at capture time *)
  cont_frames : call_frame list; (** Saved call frames *)
  cont_code : code;              (** Code at capture site *)
  cont_pc : int;                 (** Program counter at capture site *)
  cont_env : env;                (** Environment at capture site *)
  cont_winds : wind list;        (** Dynamic-wind stack at capture time *)
}

(** {1 Equality} *)

val equal : t -> t -> bool
(** [equal a b] is recursive structural equality.  [Flonum] uses
    [Float.equal] (so [nan = nan] is [true]).  Vectors and bytevectors are
    compared element‐wise.  [Void = Void] is [true].  [Primitive] compares
    by name.  [Closure] and [Continuation] always return [false]
    (identity semantics).  [Values] compares element-wise. *)

(** {1 Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt d] pretty‐prints [d] in Scheme external representation.
    Uses dotted‐pair notation for improper lists and the standard [#t],
    [#f], [+inf.0], [-inf.0], [+nan.0] forms.

    Suitable for use with [Format.asprintf] and Alcotest's [testable]. *)

val to_string : t -> string
(** [to_string d] is [Format.asprintf "%a" pp d]. *)

val pp_display : Format.formatter -> t -> unit
(** [pp_display fmt d] prints [d] in display representation per R7RS §6.13.3.
    Like {!pp} except strings are printed without quotes and characters are
    printed as their raw character (not #\\ notation). *)

val to_display_string : t -> string
(** [to_display_string d] is [Format.asprintf "%a" pp_display d]. *)
