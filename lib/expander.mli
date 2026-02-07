(** Hygienic macro expander.

    Walks a {!Syntax.t} tree top-down, expanding macro uses on encounter.
    Recognises core forms and passes them through; resolves
    [define-syntax], [let-syntax], and [letrec-syntax] bindings;
    desugars [quasiquote], [guard], and [define-record-type].

    After expansion only core forms remain, ready for {!Compiler.compile}. *)

(** {1 Syntactic environment} *)

(** An opaque syntactic environment mapping identifiers to bindings. *)
type syn_env

val core_env : unit -> syn_env
(** [core_env ()] returns a fresh syntactic environment pre-populated with
    bindings for all core forms ([if], [lambda], [define], [set!],
    [begin], [quote], [let], [let*], [letrec], [letrec*], [cond],
    [case], [do], [and], [or], [when], [unless], [define-syntax],
    [let-syntax], [letrec-syntax], [quasiquote], [guard],
    [define-record-type], [syntax-error], [cond-expand], [include],
    [include-ci]). *)

(** {1 Expansion} *)

val expand : syn_env:syn_env -> gensym:(unit -> string) ->
             ?features:(string list) ->
             ?has_library:(string list -> bool) ->
             ?read_include:(fold_case:bool -> string -> Syntax.t list) ->
             Syntax.t -> Syntax.t
(** [expand ~syn_env ~gensym ?features ?has_library ?read_include expr]
    macro-expands [expr] using the syntactic environment [syn_env].
    [gensym] is called to generate fresh identifiers for hygienic
    renaming.

    Optional callbacks:
    - [features]: list of feature identifiers for [cond-expand]
      (default [[]])
    - [has_library]: predicate for [(library ...)] tests in [cond-expand]
      (default [fun _ -> false])
    - [read_include]: reads a file and returns its forms as syntax objects;
      used by [include] and [include-ci] (default raises error)

    @raise Compiler.Compile_error on malformed syntax. *)

(** {1 Binding API} *)

(** An opaque syntactic binding.  May be a variable, core form, or macro. *)
type binding

val lookup_binding : syn_env -> string -> binding option
(** [lookup_binding env name] searches the syntactic environment for a
    binding of [name].  Returns [Some b] if found, [None] otherwise. *)

val define_binding : syn_env -> string -> binding -> unit
(** [define_binding env name b] adds or replaces the binding for [name]
    in the top (innermost) frame of [env]. *)

val var_binding : binding
(** A variable binding, suitable for registering user-defined primitives
    so the expander does not reject them as unbound. *)

val binding_names : syn_env -> string list
(** [binding_names env] returns the list of all bound names visible in
    [env], with no duplicates.  Inner frames shadow outer frames. *)
