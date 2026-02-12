(** Incremental FASL compilation.

    Uses the dependency graph from {!Dep_graph} to determine which libraries
    need recompilation, compiles them in topological order, and caches the
    results as [.fasl] files alongside the source [.sld] files.

    Separation of concerns: {!Dep_graph} is pure graph computation;
    this module adds I/O-heavy staleness checking and build orchestration. *)

(** {1 Types} *)

(** The reason a library needs recompilation. *)
type stale_reason =
  | No_fasl
      (** No [.fasl] cache file exists. *)
  | Source_newer
      (** The [.sld] source file is newer than the [.fasl] cache. *)
  | Dep_newer of Library.library_name
      (** A dependency's [.fasl] is newer than this library's [.fasl]. *)

(** A single compilation action in a build plan. *)
type build_action = {
  node : Dep_graph.node;
  (** The dependency graph node to compile. *)
  reason : stale_reason;
  (** Why this library needs recompilation. *)
}

(** The result of compiling a single library. *)
type build_result = {
  name : Library.library_name;
  (** The library that was compiled. *)
  elapsed : float;
  (** Wall-clock seconds spent compiling this library. *)
}

(** {1 Exceptions} *)

exception Build_error of Library.library_name * string
(** [Build_error (name, msg)] is raised when compilation of a library fails. *)

(** {1 Staleness checking} *)

val is_stale : Dep_graph.node list -> Dep_graph.node -> stale_reason option
(** [is_stale nodes node] checks whether [node] needs recompilation.
    The [nodes] list provides context for checking dependency mtimes.
    Returns [None] if the library is fresh, [Some reason] if stale. *)

(** {1 Build planning} *)

val plan : Dep_graph.node list -> build_action list
(** [plan nodes] walks the topological order and returns the subset of
    nodes that need recompilation, with staleness propagation: if a
    dependency is stale, all downstream dependents are also stale. *)

(** {1 Build execution} *)

val execute :
  ?verbose:bool ->
  ?on_compile:(Library.library_name -> unit) ->
  search_paths:string list ->
  build_action list -> build_result list
(** [execute ?verbose ?on_compile ~search_paths actions] compiles each
    library in the plan.  Deletes stale [.fasl] files before loading to
    force recompilation from source.

    @param verbose Print progress to stderr (default [false]).
    @param on_compile Callback invoked before each library is compiled.
    @param search_paths Directories to search for [.sld] files.
    @raise Build_error if any library fails to compile. *)

(** {1 Cleaning} *)

val clean : Dep_graph.node list -> int
(** [clean nodes] removes [.fasl] files for all nodes.
    Returns the number of files actually deleted. *)

(** {1 Utilities} *)

val builtin_library_names : Instance.t -> Library.library_name list
(** [builtin_library_names inst] returns the names of all libraries
    registered in [inst] at creation time (the built-in libraries). *)
