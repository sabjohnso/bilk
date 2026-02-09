(** Compile-time configuration.

    Paths baked in at build time by dune rules. *)

val stdlib_source_dir : string
(** Absolute path to the [stdlib/] directory in the source tree.
    Valid during development; may not exist after installation. *)
