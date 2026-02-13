(** Library search path resolution.

    Assembles the ordered list of directories to search for [.sld]
    library files.  Follows a Python-inspired resolution order:

    {ol
      {- Script directory or CWD (caller-provided [base_dirs])}
      {- Package dependencies (prepended separately by [setup_package_paths])}
      {- Virtual environment [lib/] (from [BILK_VENV])}
      {- [BILK_PATH] entries (colon-separated)}
      {- Bundled standard library ([stdlib/] from source tree or install prefix)}
      {- Site library ([~/.bilk/lib/])}
    }

    Bundled SRFIs are found as [.sld] files in the [stdlib/] directory,
    discovered via compile-time source path, binary-relative install path,
    or the [BILK_STDLIB] environment variable. *)

(** {1 Directory queries} *)

val bilk_home : unit -> string
(** [bilk_home ()] returns the Bilk home directory.  Uses [BILK_HOME]
    if set, otherwise defaults to [$HOME/.bilk]. *)

val site_lib : unit -> string
(** [site_lib ()] returns the site library directory:
    [{bilk_home}/lib]. *)

val env_paths : unit -> string list
(** [env_paths ()] parses [BILK_PATH] into a list of directory paths.
    Returns [[]] if the variable is unset or empty.  Empty segments
    from consecutive colons are filtered out. *)

val venv_lib_path : unit -> string option
(** [venv_lib_path ()] returns [Some path] if [BILK_VENV] is set and
    points to a valid virtual environment directory (containing
    [bilk-venv.cfg]), where [path] is the [lib/] subdirectory.
    Returns [None] otherwise. *)

val stdlib_dirs : unit -> string list
(** [stdlib_dirs ()] returns the directories containing bundled standard
    library [.sld] files.  Checks [BILK_STDLIB] env var first, then the
    compile-time source tree path, then discovers relative to the binary
    for installed deployments. *)

(** {1 Resolution} *)

val resolve : base_dirs:string list -> string list
(** [resolve ~base_dirs] assembles the full search path list:
    [base_dirs ++ venv_lib ++ BILK_PATH ++ stdlib_dirs ++ site_lib].
    Directories that do not exist on the filesystem are filtered out.
    Package dependency paths are {b not} included — they are prepended
    separately by [Instance.setup_package_paths]. *)
