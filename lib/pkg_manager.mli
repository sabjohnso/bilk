(** Local package manager.

    Manages a local registry of packages stored under a root directory.
    Supports installation, removal, dependency resolution, and search
    path generation.

    {b Registry layout:}
    {v
    <registry_root>/
      <package-name>/
        <version>/
          package.scm
          src/
            ...
    v} *)

(** {1 Exceptions} *)

exception Pkg_error of string
(** Raised on package manager errors (missing packages, version conflicts,
    circular dependencies, etc.). *)

(** {1 Registry root} *)

val default_registry_root : unit -> string
(** [default_registry_root ()] returns [~/.wile/packages/]. *)

(** {1 Registry operations} *)

val install : registry_root:string -> src_dir:string -> unit
(** [install ~registry_root ~src_dir] reads [package.scm] from [src_dir],
    creates [registry_root/<name>/<version>/], and copies the package
    files (package.scm and src/ tree).
    @raise Pkg_error if [package.scm] is missing or the version is already
    installed.
    @raise Package.Package_error if the package file is malformed. *)

val remove : registry_root:string -> name:string -> version:string -> unit
(** [remove ~registry_root ~name ~version] removes the specified package
    version from the registry.
    @raise Pkg_error if the package version is not installed. *)

val list_packages : registry_root:string -> (string * Semver.t list) list
(** [list_packages ~registry_root] returns all installed packages with
    their versions, sorted by name.  Versions are sorted ascending. *)

val package_info :
  registry_root:string -> name:string -> version:string -> Package.t
(** [package_info ~registry_root ~name ~version] reads and returns the
    package descriptor for the specified installed version.
    @raise Pkg_error if the package version is not installed. *)

(** {1 Dependency resolution} *)

val resolve :
  registry_root:string -> Package.dependency list ->
  (string * Semver.t) list
(** [resolve ~registry_root deps] resolves a dependency list to concrete
    package versions.  Uses a greedy algorithm: for each dependency, picks
    the latest installed version satisfying the constraints, then
    transitively resolves its dependencies.

    Returns a list of [(name, version)] pairs for all resolved packages.
    @raise Pkg_error on version conflicts, missing packages, or circular
    dependencies. *)

(** {1 Search paths} *)

val search_paths_for :
  registry_root:string -> (string * Semver.t) list -> string list
(** [search_paths_for ~registry_root resolved] maps each resolved
    [(name, version)] pair to its [src/] directory in the registry. *)
