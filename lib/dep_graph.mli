(** Static dependency graph for R7RS libraries.

    Analyses [.sld] and [.scm] files without evaluating them, building a
    dependency DAG that the build system uses for incremental compilation.

    All functions in this module are pure (no side-effects on the global
    environment).  They parse files, extract import clauses, resolve
    library names to file paths, and compute topological orderings. *)

(** {1 Types} *)

(** A node in the dependency graph. *)
type node = {
  name : Library.library_name;
  (** The library name, e.g. [["scheme"; "base"]]. *)
  sld_path : string;
  (** Absolute or relative path to the [.sld] source file. *)
  imports : Library.library_name list;
  (** Library names imported by this library. *)
}

(** Information extracted from a [.sld] file by static analysis. *)
type sld_info = {
  declared_name : Library.library_name;
  (** The library name declared in the [define-library] form. *)
  imports : Library.library_name list;
  (** Library names imported by this library. *)
}

(** {1 Exceptions} *)

exception Cycle_error of Library.library_name list list
(** Raised when a circular dependency is detected.  The payload is the
    list of cycles found, where each cycle is the sequence of library
    names forming the loop (the first element is repeated at the end). *)

exception Resolve_error of Library.library_name * string
(** [Resolve_error (name, msg)] is raised when a library name cannot be
    resolved to a [.sld] file and is not in the builtins list. *)

(** {1 Import extraction} *)

val base_library_name : Library.import_set -> Library.library_name
(** [base_library_name iset] recursively unwraps import-set modifiers
    ([only], [except], [prefix], [rename]) and returns the innermost
    library name. *)

val parse_sld : Readtable.t -> string -> sld_info
(** [parse_sld rt path] parses the [.sld] file at [path] and returns
    both the declared library name and the imported library names.
    Does not evaluate any code.

    Handles [(cond-expand ...)] declarations conservatively by extracting
    imports from all branches.

    Raises [Sys_error] if the file cannot be opened.
    Raises [Failure] if the file is not a [define-library] form. *)

val imports_of_sld : Readtable.t -> string -> Library.library_name list
(** [imports_of_sld rt path] is [(parse_sld rt path).imports].
    Convenience wrapper when only the imports are needed. *)

val imports_of_scm : Readtable.t -> string -> Library.library_name list
(** [imports_of_scm rt path] parses the [.scm] file at [path] and returns
    the library names from its top-level [(import ...)] forms.
    Raises [Sys_error] if the file cannot be opened. *)

(** {1 Path resolution} *)

val resolve_sld :
  search_paths:string list -> Library.library_name -> string option
(** [resolve_sld ~search_paths name] finds the [.sld] file for [name]
    by searching each directory in [search_paths].  The convention is
    that library [(foo bar)] maps to [<dir>/foo/bar.sld].  Returns the
    first match, or [None] if no file is found. *)

(** {1 Graph construction} *)

val build_graph :
  ?builtins:Library.library_name list ->
  search_paths:string list -> Readtable.t ->
  Library.library_name list -> node list
(** [build_graph ?builtins ~search_paths rt roots] transitively resolves
    all imports starting from [roots] and returns the reachable libraries
    in topological order (dependencies before dependents).

    Libraries listed in [builtins] are silently omitted from the graph —
    they are assumed to be always available at runtime.  If [builtins] is
    not provided, {b all} unresolvable libraries are silently skipped
    (legacy behaviour; prefer providing an explicit list).

    When [builtins] is provided, any library that is neither found on the
    search path nor listed in [builtins] raises {!Resolve_error}.

    @raise Cycle_error if circular dependencies exist.
    @raise Resolve_error if a library cannot be found and is not built-in
    (only when [~builtins] is provided). *)

(** {1 Graphviz export} *)

val to_dot : node list -> string
(** [to_dot nodes] returns a Graphviz DOT digraph.  Each node is
    labelled with the library name; edges point from a library to its
    imports.  If the graph contains cycles, cycle edges are coloured red. *)
