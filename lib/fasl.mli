(** FASL (Fast-load) binary serialization for compiled code.

    Provides round-trip serialization of [Datum.code] objects to a compact
    binary format.  The primary use case is caching compiled library code
    so that subsequent loads skip the Reader → Expander → Compiler pipeline
    and go straight to VM execution.

    The format is versioned (major.minor) with a 16-byte header containing
    magic bytes ["WFAS"], version numbers, and a format type tag. *)

(** {1 Exceptions} *)

exception Fasl_error of string
(** Raised when serialization or deserialization fails due to format errors,
    version mismatches, unsupported value types, or I/O problems. *)

(** {1 Version} *)

val version_major : int
(** Major version of the FASL format.  Incremented on breaking changes. *)

val version_minor : int
(** Minor version of the FASL format.  Incremented on compatible extensions. *)

(** {1 Types} *)

(** A library declaration in a library FASL. *)
type lib_declaration =
  | Lib_import of Library.import_set
      (** An import declaration to be replayed. *)
  | Lib_code of Datum.code
      (** A compiled code object to be executed. *)

(** A serialized library. *)
type lib_fasl = {
  lib_name : Library.library_name;
  (** The library's name. *)
  has_syntax_exports : bool;
  (** Whether the library has syntax exports.  If [true], the cached FASL
      should be bypassed and the library recompiled from source. *)
  exports : Library.export_spec list;
  (** The library's export specifications. *)
  declarations : lib_declaration list;
  (** The library's declarations (imports and code) in order. *)
}

(** {1 Code serialization} *)

val write_code : Datum.code -> bytes
(** [write_code code] serializes a code object to bytes with a FASL header.
    @raise Fasl_error if the code contains unserializable constants
    (e.g. closures, continuations, primitives). *)

val read_code : Symbol.table -> bytes -> Datum.code
(** [read_code symbols data] deserializes a code object from bytes.
    Symbols are re-interned into [symbols].
    @raise Fasl_error on format errors or version mismatches. *)

(** {1 File I/O} *)

val write_code_to_file : string -> Datum.code -> unit
(** [write_code_to_file path code] writes a serialized code object to a file.
    @raise Fasl_error if the code contains unserializable constants. *)

val read_code_from_file : Symbol.table -> string -> Datum.code
(** [read_code_from_file symbols path] reads a serialized code object from a file.
    @raise Fasl_error on format or I/O errors. *)

(** {1 Library FASL} *)

val write_lib_fasl : string -> lib_fasl -> unit
(** [write_lib_fasl path fasl] writes a library FASL to a file.
    @raise Fasl_error if any code object contains unserializable constants. *)

val read_lib_fasl : Symbol.table -> string -> lib_fasl
(** [read_lib_fasl symbols path] reads a library FASL from a file.
    @raise Fasl_error on format or I/O errors. *)

(** {1 Cache helpers} *)

val fasl_path_for : string -> string
(** [fasl_path_for sld_path] returns the FASL cache path for a [.sld] file.
    Replaces the [.sld] extension with [.fasl]. *)

val is_cache_valid : sld_path:string -> fasl_path:string -> bool
(** [is_cache_valid ~sld_path ~fasl_path] returns [true] if [fasl_path]
    exists and its modification time is at least as recent as [sld_path]. *)
