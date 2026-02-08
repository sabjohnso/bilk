(** Package metadata and parsing.

    A package is a named, versioned collection of R7RS libraries
    with dependency information.  Package metadata is stored in a
    [package.scm] file at the package root.

    {b Format:}
    {v
    (define-package
      (name my-package)
      (version "1.0.0")
      (description "A useful library")
      (license "MIT")
      (depends
        (other-package (>= "1.0.0") (< "2.0.0"))
        (yet-another))
      (libraries
        (my-package foo)
        (my-package bar)))
    v} *)

(** {1 Types} *)

(** A dependency on another package with optional version constraints. *)
type dependency = {
  dep_name : string;
  (** The package name. *)
  dep_constraints : Semver.constraint_set;
  (** Version constraints (conjunction).  Empty means any version. *)
}

(** A package descriptor. *)
type t = {
  name : string;
  (** The package name. *)
  version : Semver.t;
  (** The package version. *)
  description : string;
  (** A short description. *)
  license : string;
  (** License identifier. *)
  depends : dependency list;
  (** Dependencies on other packages. *)
  libraries : Library.library_name list;
  (** R7RS libraries provided by this package. *)
}

(** {1 Exceptions} *)

exception Package_error of string
(** Raised when a [package.scm] file is malformed or cannot be read. *)

(** {1 Parsing} *)

val parse : Readtable.t -> string -> t
(** [parse readtable path] reads and parses a [package.scm] file.
    Raises [Sys_error] if the file cannot be opened.
    @raise Package_error if the file is malformed. *)

(** {1 Discovery} *)

val find_package_file : string -> string option
(** [find_package_file dir] searches [dir] and its parent directories
    for a [package.scm] file.  Returns [Some path] if found, [None]
    if the filesystem root is reached without finding one. *)
