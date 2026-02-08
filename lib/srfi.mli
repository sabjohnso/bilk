(** Bundled SRFI library sources.

    Provides embedded [define-library] source strings for bundled SRFIs.
    When [(import (srfi N))] is first encountered and no on-disk [.sld] file
    is found, {!Instance} falls back to {!lookup} to check for a bundled
    implementation.

    Currently bundled: SRFI 1, 2, 8, 11, 13, 14, 16, 26, 28, 31, 41, 111,
    113, 115, 125, 128, 132, 133.
    SRFI 14, 69, 115, and 151 are registered as built-in libraries with
    OCaml primitives.  SRFI 13 is a mixed OCaml/Scheme library. *)

val lookup : string list -> string option
(** [lookup name] returns the embedded source for a bundled SRFI library,
    or [None] if no bundled source exists for [name].
    [name] is a library name as a list of strings, e.g. [["srfi"; "1"]]. *)

val bundled_features : string list
(** Feature identifiers for all bundled SRFIs
    (e.g. [["srfi-1"; "srfi-8"; "srfi-151"]]). *)
