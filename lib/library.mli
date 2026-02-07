(** R7RS library system.

    Provides library types, a per-instance registry, parsing of library
    names / export specs / import sets from syntax trees, and import-set
    resolution.  Used by {!Instance} to implement [define-library] and
    top-level [import]. *)

(** {1 Types} *)

(** A library name such as [(scheme base)] represented as a string list. *)
type library_name = string list

(** An export specification from a [define-library] export declaration. *)
type export_spec =
  | Export_id of string
      (** Export a binding under its own name. *)
  | Export_rename of string * string
      (** [Export_rename (internal, external)] exports [internal] as
          [external]. *)

(** An import set, possibly with modifiers. *)
type import_set =
  | Import_lib of library_name
      (** Import all exports of a library. *)
  | Import_only of import_set * string list
      (** Import only the listed names from the inner set. *)
  | Import_except of import_set * string list
      (** Import everything except the listed names. *)
  | Import_prefix of import_set * string
      (** Prefix all imported names with the given string. *)
  | Import_rename of import_set * (string * string) list
      (** Rename specific imported names. *)

(** A registered library. *)
type t = {
  name : library_name;
  (** The library's name. *)
  env : Env.t;
  (** The library's internal environment. *)
  exports : (string, int * Datum.t ref) Hashtbl.t;
  (** Runtime exports: external name -> (symbol id, shared slot). *)
  syntax_exports : (string, Expander.binding) Hashtbl.t;
  (** Syntax exports: external name -> syntactic binding. *)
}

(** An opaque library registry. *)
type registry

(** {1 Registry operations} *)

val create_registry : unit -> registry
(** [create_registry ()] returns a fresh, empty library registry. *)

val register : registry -> t -> unit
(** [register reg lib] adds [lib] to the registry.  Replaces any
    previously registered library with the same name. *)

val lookup : registry -> library_name -> t option
(** [lookup reg name] returns the library registered under [name],
    or [None] if not found. *)

(** {1 Display} *)

val name_to_string : library_name -> string
(** [name_to_string name] formats a library name for display.
    E.g. [name_to_string ["scheme"; "base"]] is ["(scheme base)"]. *)

(** {1 Parsing} *)

val parse_library_name : Syntax.t -> library_name
(** [parse_library_name s] parses a library name from a syntax object.
    Components may be identifiers or integers.
    @raise Compiler.Compile_error on malformed input. *)

val parse_export_spec : Syntax.t -> export_spec
(** [parse_export_spec s] parses an export specification.
    @raise Compiler.Compile_error on malformed input. *)

val parse_import_set : Syntax.t -> import_set
(** [parse_import_set s] parses an import set with optional modifiers.
    @raise Compiler.Compile_error on malformed input. *)

(** {1 Resolution} *)

val resolve_import :
  (library_name -> t option) -> import_set ->
  (string * int * Datum.t ref) list * (string * Expander.binding) list
(** [resolve_import lookup_fn iset] resolves an import set.  [lookup_fn]
    is called to find libraries by name.  Returns
    [(runtime_bindings, syntax_bindings)] where each runtime binding is
    [(external_name, sym_id, slot)] and each syntax binding is
    [(external_name, binding)].
    Raises [Failure] if a library is not found or a modifier references a
    name not in the export set. *)
