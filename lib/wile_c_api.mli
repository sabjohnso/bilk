(** C embedding API bridge.

    This module provides the OCaml side of the C embedding API.  It manages
    handle-based references to OCaml values so that C code can safely
    interact with the Scheme runtime through integer handles.

    Handles are 32-bit integers indexing into per-instance hash tables.
    Handle [0] ([WILE_NULL]) is reserved as the error/not-found sentinel.

    All API functions catch Wile exceptions and store error messages in the
    per-instance [last_error] field.  C callers query errors via
    {!error_message}.

    This module is an internal bridge — C programs use the [wile.h] header,
    not this module directly.  OCaml programs should use {!Instance} and
    {!Datum} instead. *)

(** {1 Instance lifecycle} *)

val create_instance : unit -> int
(** [create_instance ()] creates a new Scheme instance and returns its
    handle.  Returns [0] on error (should not normally fail). *)

val destroy_instance : int -> unit
(** [destroy_instance ih] destroys the instance with handle [ih], releasing
    all associated value handles.  Does nothing if [ih] is invalid. *)

(** {1 Error handling} *)

val error_message : int -> string
(** [error_message ih] returns the last error message for instance [ih],
    or the empty string if no error is pending.  Also checks the global
    error for invalid instance handles. *)

(** {1 Evaluation} *)

val eval_string : int -> string -> int
(** [eval_string ih src] evaluates one Scheme expression from [src] in the
    instance [ih].  Returns a value handle, or [0] on error. *)

val do_load_file : int -> string -> int
(** [do_load_file ih path] loads and executes all expressions from the file
    at [path].  Returns [1] on success, [0] on error. *)

val do_load_fasl : int -> string -> int
(** [do_load_fasl ih path] loads and executes a pre-compiled FASL file.
    Returns [1] on success, [0] on error. *)

(** {1 Lookup and call} *)

val do_lookup : int -> string -> int
(** [do_lookup ih name] looks up a global binding by name.  Returns a value
    handle, or [0] if not found. *)

val do_call : int -> int -> int array -> int
(** [do_call ih proc_h arg_hs] calls the Scheme procedure at handle [proc_h]
    with arguments at handles [arg_hs].  Returns a result handle, or [0]
    on error. *)

(** {1 Primitive registration} *)

val do_define_primitive : int -> string -> int -> unit
(** [do_define_primitive ih name prim_id] registers a C function (identified
    by [prim_id] in the C dispatch table) as a Scheme primitive named
    [name] in instance [ih]. *)

(** {1 Value constructors} *)

val make_nil : int -> int
(** [make_nil ih] returns a handle to the empty list. *)

val make_void : int -> int
(** [make_void ih] returns a handle to the void value. *)

val make_bool : int -> int -> int
(** [make_bool ih b] returns a handle to [#t] (if [b <> 0]) or [#f]. *)

val make_fixnum : int -> int -> int
(** [make_fixnum ih n] returns a handle to the fixnum [n]. *)

val make_flonum : int -> float -> int
(** [make_flonum ih d] returns a handle to the flonum [d]. *)

val make_string : int -> string -> int
(** [make_string ih s] returns a handle to the Scheme string [s]. *)

val make_symbol : int -> string -> int
(** [make_symbol ih name] returns a handle to an interned symbol. *)

val make_cons : int -> int -> int -> int
(** [make_cons ih car_h cdr_h] returns a handle to a new pair. *)

val make_vector : int -> int array -> int
(** [make_vector ih elts] returns a handle to a new vector. *)

val make_list : int -> int array -> int
(** [make_list ih elts] returns a handle to a new proper list. *)

(** {1 Type predicates} *)

val is_nil : int -> int -> int
(** [is_nil ih vh] returns [1] if the value is the empty list, [0] otherwise. *)

val is_bool : int -> int -> int
(** [is_bool ih vh] returns [1] if the value is a boolean, [0] otherwise. *)

val is_fixnum : int -> int -> int
(** [is_fixnum ih vh] returns [1] if the value is a fixnum, [0] otherwise. *)

val is_integer : int -> int -> int
(** [is_integer ih vh] returns [1] if the value is an exact integer
    (fixnum or bignum), [0] otherwise. *)

val is_flonum : int -> int -> int
(** [is_flonum ih vh] returns [1] if the value is a flonum, [0] otherwise. *)

val is_string : int -> int -> int
(** [is_string ih vh] returns [1] if the value is a string, [0] otherwise. *)

val is_symbol : int -> int -> int
(** [is_symbol ih vh] returns [1] if the value is a symbol, [0] otherwise. *)

val is_pair : int -> int -> int
(** [is_pair ih vh] returns [1] if the value is a pair, [0] otherwise. *)

val is_vector : int -> int -> int
(** [is_vector ih vh] returns [1] if the value is a vector, [0] otherwise. *)

val is_true : int -> int -> int
(** [is_true ih vh] returns [1] if the value is true (R7RS truthiness),
    [0] if it is [#f]. *)

(** {1 Value extractors} *)

val get_bool : int -> int -> int
(** [get_bool ih vh] returns [1] for [#t], [0] for [#f].
    Returns [0] and sets error if not a boolean. *)

val get_fixnum : int -> int -> int
(** [get_fixnum ih vh] returns the fixnum value.  For bignums that fit
    in a native int, the value is returned.  Returns [0] and sets error
    if not an integer or if the bignum does not fit. *)

val get_integer_string : int -> int -> string
(** [get_integer_string ih vh] returns the decimal string representation
    of an exact integer (fixnum or bignum).  Returns [""] and sets error
    if the value is not an integer. *)

val get_flonum : int -> int -> float
(** [get_flonum ih vh] returns the flonum value.
    Returns [0.0] and sets error if not a flonum. *)

val get_string : int -> int -> string
(** [get_string ih vh] returns the string value.
    Returns [""] and sets error if not a string. *)

val get_symbol_name : int -> int -> string
(** [get_symbol_name ih vh] returns the symbol name.
    Returns [""] and sets error if not a symbol. *)

val do_car : int -> int -> int
(** [do_car ih vh] returns a handle to the car of the pair.
    Returns [0] and sets error if not a pair. *)

val do_cdr : int -> int -> int
(** [do_cdr ih vh] returns a handle to the cdr of the pair.
    Returns [0] and sets error if not a pair. *)

val do_vector_length : int -> int -> int
(** [do_vector_length ih vh] returns the length of the vector.
    Returns [0] and sets error if not a vector. *)

val do_vector_ref : int -> int -> int -> int
(** [do_vector_ref ih vh i] returns a handle to element [i] of the vector.
    Returns [0] and sets error if not a vector or index out of range. *)

(** {1 Display and write} *)

val display_to_string : int -> int -> string
(** [display_to_string ih vh] returns the display representation of the value. *)

val write_to_string : int -> int -> string
(** [write_to_string ih vh] returns the write representation of the value. *)

(** {1 Memory management} *)

val release : int -> int -> unit
(** [release ih vh] releases the value handle [vh] from instance [ih].
    The handle becomes invalid after this call. *)

(** {1 Temporary handles for C extensions} *)

val temporary_handle : Instance.t -> int
(** [temporary_handle inst] registers an existing {!Instance.t} in the
    handle table and returns a fresh handle.  Used to give C extension
    init functions a valid [wile_inst_t] handle.  The handle should be
    released with {!release_handle} when no longer needed. *)

val release_handle : int -> unit
(** [release_handle ih] removes the handle [ih] from the instance table
    without destroying the underlying instance.  Used after C extension
    init completes. *)

(** {1 Handle resolution (for testing)} *)

val resolve : int -> int -> Datum.t option
(** [resolve ih vh] returns the datum at handle [vh] in instance [ih],
    or [None] if the handle or instance is invalid. *)
