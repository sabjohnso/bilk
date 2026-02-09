(** Native extension loading for Wile.

    Extensions are shared libraries (.cmxs for OCaml, .so for C) that
    register Scheme primitives when loaded.  They integrate with the
    library system via the [(include-shared "name")] declaration in
    [define-library].

    Two loading strategies are supported:

    - {b Static linking}: The extension is compiled into the executable.
      Its init code calls {!register_static} at module-load time.  When
      [include-shared] is encountered, the static registry is checked first.

    - {b Dynamic loading}: The extension is a separate [.cmxs] or [.so]
      file found on the search path.  [.cmxs] files are loaded via
      [Dynlink]; [.so] files are loaded via [dlopen] (C extensions). *)

(** {1 Exceptions} *)

exception Extension_error of string
(** Raised when an extension cannot be found or fails to load. *)

(** {1 Static registry} *)

val register_static : string -> (Instance.t -> unit) -> unit
(** [register_static name init_fn] registers an extension init function
    under [name] in the process-global static registry.  This is
    typically called at OCaml module-init time (top-level [let () = ...]).
    If [name] is already registered, the previous entry is replaced. *)

val init_static : Instance.t -> string -> bool
(** [init_static inst name] looks up [name] in the static registry and,
    if found, calls the init function with [inst].  Returns [true] if
    the name was found (and init was called), [false] otherwise. *)

(** {1 Dynamic loading} *)

val load_native : Instance.t -> search_dirs:string list
  -> sld_dir:string option -> string -> unit
(** [load_native inst ~search_dirs ~sld_dir name] loads a native extension
    named [name].

    Search order:
    + Static registry (already linked)
    + [wile_<name>.cmxs] in [sld_dir] (if given), then [search_dirs]
    + [wile_<name>.so] in [sld_dir] (if given), then [search_dirs]

    For [.cmxs] files, the module is loaded via [Dynlink].  Its init code
    is expected to call {!register_static}, after which {!init_static} is
    called.

    For [.so] files, the shared library is loaded via [dlopen] and its
    [wile_ext_init] entry point is called with a temporary instance handle.

    @raise Extension_error if the extension is not found or fails to load. *)

val load_c : Instance.t -> string -> unit
(** [load_c inst path] loads a C extension [.so] file at [path].

    Opens the shared library with [dlopen], finds the [wile_ext_init]
    symbol, and calls it with a temporary instance handle.  The C init
    function registers primitives via the C API ([wile_define_primitive]).

    @raise Extension_error if the file cannot be loaded or the entry
    point is missing. *)

(** {1 C handle bridge}

    Forward references that {!Wile_c_api} fills in at module-init time
    to break the dependency cycle
    [Extension -> Instance -> Extension (OK)] vs
    [Extension -> Wile_c_api -> Instance -> Extension (cycle)]. *)

val c_temporary_handle_ref : (Instance.t -> int) ref
(** Set by {!Wile_c_api} to {!Wile_c_api.temporary_handle}. *)

val c_release_handle_ref : (int -> unit) ref
(** Set by {!Wile_c_api} to {!Wile_c_api.release_handle}. *)
