(** Profile report generation.

    Produces three output formats from collected profiler data:
    a flat text table, an interactive flame graph SVG, and a
    Chrome Trace Event JSON file.

    @since M24 *)

val text_report : Profiler.t -> string
(** [text_report prof] returns a flat text table sorted by self time
    descending, showing self%, total time, self time, call count,
    procedure name, and source location. *)

val flamegraph_svg : Profiler.t -> string
(** [flamegraph_svg prof] returns a self-contained SVG string representing
    a flame graph.  Width is 1200px, frames are 16px tall.  Includes
    embedded JavaScript for click-to-zoom and reset.  Color is hashed
    from procedure name using a warm palette. *)

val trace_json : Profiler.t -> string
(** [trace_json prof] returns a Chrome Trace Event JSON string.
    Each event has phase (B/E), name, category, pid, tid, and timestamp
    in microseconds.  Load in [chrome://tracing] or Perfetto. *)
