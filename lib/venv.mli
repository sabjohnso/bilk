(** Virtual environment support.

    A virtual environment is a directory containing a marker file
    ([wile-venv.cfg]) and a library directory ([lib/]) for
    user-installed [.sld] files.  Inspired by Python's venv model. *)

(** {1 Types} *)

(** Virtual environment configuration stored in [wile-venv.cfg]. *)
type config = {
  wile_version : string;  (** Wile version that created the venv. *)
  created : string;        (** ISO 8601 creation timestamp. *)
}

exception Venv_error of string
(** Raised on virtual environment errors (e.g. directory already
    exists as a venv, missing config). *)

(** {1 Operations} *)

val create : wile_version:string -> string -> unit
(** [create ~wile_version dir] creates a new virtual environment at
    [dir].  Creates the directory (and parents) if needed, writes
    [wile-venv.cfg], and creates the [lib/] subdirectory.
    @raise Venv_error if [dir] already contains a [wile-venv.cfg]. *)

val is_venv : string -> bool
(** [is_venv dir] returns [true] if [dir] contains a [wile-venv.cfg]
    marker file. *)

val read_config : string -> config
(** [read_config dir] reads and parses [wile-venv.cfg] from [dir].
    @raise Venv_error if the file is missing or malformed. *)

val lib_path : string -> string
(** [lib_path dir] returns [dir/lib], the library search directory
    for the virtual environment. *)
