(** Lockfile for reproducible installs.

    Records exact versions and content hashes for all resolved
    dependencies.  Written to [bilk.lock] next to [package.scm]
    by [bilk pkg lock], consumed by [bilk pkg install].

    {b Format:}
    {v
    (lock
      (created "2026-02-12T12:00:00Z")
      (packages
        (json "0.3.1"
          (sha256 "abc123..."))
        (srfi-extra "1.1.0"
          (sha256 "def456..."))))
    v} *)

(** {1 Exceptions} *)

exception Lockfile_error of string
(** Raised on lockfile parse/write/verify errors. *)

(** {1 Types} *)

type locked_package = {
  name : string;
  version : Semver.t;
  sha256 : string;
}

type t = {
  created : string;
  packages : locked_package list;
}

type mismatch =
  | Hash_mismatch of { name : string; expected : string; actual : string }
  | Not_installed of { name : string; version : string }

(** {1 Path} *)

val lockfile_path : string -> string
(** [lockfile_path project_dir] returns [project_dir/bilk.lock]. *)

(** {1 Parsing and writing} *)

val parse : Readtable.t -> string -> t
(** [parse readtable path] parses a lockfile from a file at [path].
    @raise Lockfile_error if the file is malformed. *)

val write : string -> t -> unit
(** [write path lock] writes the lockfile to [path]. *)

(** {1 Content hashing} *)

val hash_directory : string -> string
(** [hash_directory path] computes a deterministic SHA-256 hash of
    all files under [path], using sha256sum.
    @raise Lockfile_error if sha256sum is not available. *)

(** {1 Lock creation} *)

val create : registry_root:string -> Package.dependency list -> t
(** [create ~registry_root deps] resolves deps via {!Pkg_manager.resolve},
    hashes each resolved package, returns a lockfile. *)

(** {1 Verification} *)

val verify : registry_root:string -> t -> mismatch list
(** [verify ~registry_root lock] checks all locked packages.
    Returns [[]] if everything matches. *)
