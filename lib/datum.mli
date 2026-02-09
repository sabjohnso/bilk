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

(** Tag distinguishing error types. *)
type error_tag = General_error | Read_error | File_error

(** An R7RS error object. *)
type error_obj = {
  err_message : string;        (** The error message *)
  err_irritants : t list;      (** The irritant values *)
  err_tag : error_tag;         (** The error type tag *)
}

(** The Scheme value type.  Exposed so callers can pattern‐match on values. *)
and t =
  | Bool of bool               (** [#t] / [#f] *)
  | Fixnum of int              (** Exact integer (machine word) *)
  | Flonum of float            (** Inexact real *)
  | Char of Uchar.t            (** Unicode character *)
  | Str of bytes               (** Mutable string (bytes) *)
  | Symbol of string           (** Symbol (uninterned at this level) *)
  | Pair of { mutable car : t; mutable cdr : t }  (** Mutable cons cell *)
  | Vector of t array          (** Mutable vector *)
  | Bytevector of bytes        (** Mutable bytevector *)
  | Nil                        (** Empty list [()] *)
  | Eof                        (** End‐of‐file object *)
  | Void                       (** Result of [define], [set!], [display], etc. *)
  | Primitive of primitive     (** Built-in OCaml function *)
  | Closure of closure         (** User-defined procedure *)
  | Continuation of continuation (** First-class continuation *)
  | Values of t list           (** Multiple return values *)
  | Error_object of error_obj  (** R7RS error object *)
  | Port of Port.t             (** First-class port value *)
  | Promise of promise         (** Lazy promise (delay/force) *)
  | Hash_table of hash_table   (** Mutable hash table (SRFI 69/125) *)
  | Char_set of char_set       (** Character set (SRFI 14) *)
  | Regexp of regexp           (** Compiled regular expression (SRFI 115) *)

(** A lazy promise created by [delay] or [make-promise]. *)
and promise = {
  mutable promise_done : bool;   (** Whether the promise has been forced *)
  mutable promise_value : t;     (** The thunk (unforced) or result (forced) *)
}

(** A mutable hash table with configurable equality and hash functions. *)
and hash_table = {
  mutable ht_data : (t * t) list array;  (** Bucket array *)
  mutable ht_size : int;                  (** Number of entries *)
  ht_equal : t;       (** Equality proc, or [Bool true] for structural equality *)
  ht_hash : t;        (** Hash proc, or [Bool true] for built-in hash *)
  ht_mutable : bool;  (** Whether mutation is allowed *)
}

(** A 256-bit character set covering code points 0-255. *)
and char_set = {
  cs_bits : bytes;   (** 32 bytes = 256 bits, one bit per code point *)
}

(** A compiled regular expression with its original SRE source. *)
and regexp = {
  rx_compiled : Obj.t;  (** Compiled regex engine object *)
  rx_source : t;        (** Original SRE datum for display *)
}

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
  source_map : Loc.t array;       (** Source locations parallel to instructions *)
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

(** A saved call frame in a continuation, preserving its frame type.
    Standard frames are normal call/return frames.  The other variants
    track multi-step intrinsic operations (call-with-values, dynamic-wind). *)
and cont_frame =
  | CF_standard of call_frame
  | CF_cwv_pending of call_frame * t    (** call-with-values: pending consumer *)
  | CF_dw_before of call_frame * t * t * t  (** dynamic-wind: before, thunk, after *)
  | CF_dw_thunk of call_frame * t * t   (** dynamic-wind: before, after *)
  | CF_dw_after of call_frame * t       (** dynamic-wind: saved result *)

(** A captured first-class continuation. *)
and continuation = {
  cont_stack : t array;          (** Snapshot of the value stack *)
  cont_sp : int;                 (** Stack pointer at capture time *)
  cont_frames : cont_frame list; (** Saved call frames with frame types *)
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

(** {1 Helpers} *)

val is_true : t -> bool
(** [is_true d] is R7RS truthiness: only [Bool false] is false.
    All other values, including [Nil], [Void], [Fixnum 0], and the empty
    string, are true. *)

val list_of : t list -> t
(** [list_of xs] builds a proper Scheme list from an OCaml list. *)

val to_list : t -> t list option
(** [to_list d] extracts a proper Scheme list as an OCaml list.
    Returns [Some xs] for proper lists (including [Nil] → [Some \[\]]),
    [None] for improper lists or non-list values. *)
