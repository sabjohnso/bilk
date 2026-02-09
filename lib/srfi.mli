(** Bundled SRFI metadata.

    Provides the list of feature identifiers for all bundled SRFIs,
    used by [cond-expand] in the expander. *)

val bundled_features : string list
(** Feature identifiers for all bundled SRFIs
    (e.g. [["srfi-1"; "srfi-8"; "srfi-151"]]). *)
