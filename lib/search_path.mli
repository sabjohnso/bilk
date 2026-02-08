(** Library search path resolution.

    Assembles the ordered list of directories to search for [.sld]
    library files.  Follows a Python-inspired resolution order:

    {ol
      {- Script directory or CWD (caller-provided [base_dirs])}
      {- Package dependencies (prepended separately by [setup_package_paths])}
      {- Virtual environment [lib/] (from [WILE_VENV])}
      {- [WILE_PATH] entries (colon-separated)}
      {- Site library ([~/.wile/lib/])}
    }

    Built-in libraries and bundled SRFIs are resolved as a fallback by
    the library loader and are not part of the filesystem search path. *)

(** {1 Directory queries} *)

val wile_home : unit -> string
(** [wile_home ()] returns the Wile home directory.  Uses [WILE_HOME]
    if set, otherwise defaults to [$HOME/.wile]. *)

val site_lib : unit -> string
(** [site_lib ()] returns the site library directory:
    [{wile_home}/lib]. *)

val env_paths : unit -> string list
(** [env_paths ()] parses [WILE_PATH] into a list of directory paths.
    Returns [[]] if the variable is unset or empty.  Empty segments
    from consecutive colons are filtered out. *)

val venv_lib_path : unit -> string option
(** [venv_lib_path ()] returns [Some path] if [WILE_VENV] is set and
    points to a valid virtual environment directory (containing
    [wile-venv.cfg]), where [path] is the [lib/] subdirectory.
    Returns [None] otherwise. *)

(** {1 Resolution} *)

val resolve : base_dirs:string list -> string list
(** [resolve ~base_dirs] assembles the full search path list:
    [base_dirs ++ venv_lib ++ WILE_PATH ++ site_lib].  Directories
    that do not exist on the filesystem are filtered out.  Package
    dependency paths are {b not} included — they are prepended
    separately by [Instance.setup_package_paths]. *)
