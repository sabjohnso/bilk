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
    [define-record-type], [syntax-error]). *)

(** {1 Expansion} *)

val expand : syn_env:syn_env -> gensym:(unit -> string) -> Syntax.t -> Syntax.t
(** [expand ~syn_env ~gensym expr] macro-expands [expr] using the
    syntactic environment [syn_env].  [gensym] is called to generate
    fresh identifiers for hygienic renaming.
    @raise Compiler.Compile_error on malformed syntax. *)
