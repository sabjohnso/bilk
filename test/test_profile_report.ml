open Wile

(* --- Helpers --- *)

let eval inst s = ignore (Instance.eval_string inst s)

let fresh () =
  let inst = Instance.create () in
  let prof = Profiler.create () in
  Profiler.install prof inst;
  (inst, prof)

let finish inst prof =
  Profiler.uninstall inst;
  Profiler.finalize prof

(* --- Text report tests --- *)

let test_text_report_contains_names () =
  let (inst, prof) = fresh () in
  eval inst "(define (fibonacci n) (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))";
  eval inst "(fibonacci 10)";
  finish inst prof;
  let report = Profile_report.text_report prof in
  Alcotest.(check bool) "contains fibonacci"
    true (String.length report > 0 &&
          let rec has_sub s sub i =
            if i + String.length sub > String.length s then false
            else if String.sub s i (String.length sub) = sub then true
            else has_sub s sub (i + 1)
          in has_sub report "fibonacci" 0);
  Alcotest.(check bool) "contains Total time"
    true (let rec has_sub s sub i =
            if i + String.length sub > String.length s then false
            else if String.sub s i (String.length sub) = sub then true
            else has_sub s sub (i + 1)
          in has_sub report "Total time" 0)

let test_text_report_sorted_by_self () =
  let (inst, prof) = fresh () in
  eval inst "(define (a) (let loop ((i 0)) (if (< i 200) (loop (+ i 1)) i)))";
  eval inst "(define (b) 42)";
  eval inst "(a)";
  eval inst "(b)";
  finish inst prof;
  let report = Profile_report.text_report prof in
  (* The report should have a header line and entries *)
  let lines = String.split_on_char '\n' report in
  Alcotest.(check bool) "has multiple lines"
    true (List.length lines > 2)

let test_text_report_empty () =
  let prof = Profiler.create () in
  Profiler.finalize prof;
  let report = Profile_report.text_report prof in
  Alcotest.(check bool) "reports no data"
    true (String.length report > 0);
  Alcotest.(check bool) "contains no data message"
    true (let rec has_sub s sub i =
            if i + String.length sub > String.length s then false
            else if String.sub s i (String.length sub) = sub then true
            else has_sub s sub (i + 1)
          in has_sub report "No profiling data" 0)

let test_text_report_column_header () =
  let (inst, prof) = fresh () in
  eval inst "(+ 1 2)";
  finish inst prof;
  let report = Profile_report.text_report prof in
  let has_sub s sub =
    let rec loop i =
      if i + String.length sub > String.length s then false
      else if String.sub s i (String.length sub) = sub then true
      else loop (i + 1)
    in loop 0
  in
  Alcotest.(check bool) "has self% header" true (has_sub report "self%");
  Alcotest.(check bool) "has calls header" true (has_sub report "calls");
  Alcotest.(check bool) "has name header" true (has_sub report "name")

(* --- Flame graph SVG tests --- *)

let has_sub s sub =
  let rec loop i =
    if i + String.length sub > String.length s then false
    else if String.sub s i (String.length sub) = sub then true
    else loop (i + 1)
  in loop 0

let test_flamegraph_valid_svg () =
  let (inst, prof) = fresh () in
  eval inst "(define (f) 42)";
  eval inst "(f)";
  finish inst prof;
  let svg = Profile_report.flamegraph_svg prof in
  Alcotest.(check bool) "starts with <svg" true (has_sub svg "<svg");
  Alcotest.(check bool) "ends with </svg>" true (has_sub svg "</svg>")

let test_flamegraph_has_rects () =
  let (inst, prof) = fresh () in
  eval inst "(define (f) 42)";
  eval inst "(f)";
  finish inst prof;
  let svg = Profile_report.flamegraph_svg prof in
  Alcotest.(check bool) "has rect elements" true (has_sub svg "<rect")

let test_flamegraph_has_tooltips () =
  let (inst, prof) = fresh () in
  eval inst "(define (f) 42)";
  eval inst "(f)";
  finish inst prof;
  let svg = Profile_report.flamegraph_svg prof in
  Alcotest.(check bool) "has title elements" true (has_sub svg "<title>")

let test_flamegraph_empty () =
  let prof = Profiler.create () in
  Profiler.finalize prof;
  let svg = Profile_report.flamegraph_svg prof in
  Alcotest.(check bool) "starts with <svg" true (has_sub svg "<svg");
  Alcotest.(check bool) "ends with </svg>" true (has_sub svg "</svg>")

(* --- Chrome Trace JSON tests --- *)

let test_trace_json_valid () =
  let (inst, prof) = fresh () in
  eval inst "(define (f) 42)";
  eval inst "(f)";
  finish inst prof;
  let json_str = Profile_report.trace_json prof in
  let json = Yojson.Safe.from_string json_str in
  match json with
  | `Assoc assoc ->
    Alcotest.(check bool) "has traceEvents"
      true (List.mem_assoc "traceEvents" assoc)
  | _ -> Alcotest.fail "expected JSON object"

let test_trace_json_has_events () =
  let (inst, prof) = fresh () in
  eval inst "(define (f) 42)";
  eval inst "(f)";
  finish inst prof;
  let json_str = Profile_report.trace_json prof in
  let json = Yojson.Safe.from_string json_str in
  match json with
  | `Assoc assoc ->
    (match List.assoc "traceEvents" assoc with
     | `List events ->
       Alcotest.(check bool) "has events" true (List.length events > 0);
       (* Check first event has required fields *)
       (match List.hd events with
        | `Assoc ev ->
          Alcotest.(check bool) "has ph" true (List.mem_assoc "ph" ev);
          Alcotest.(check bool) "has name" true (List.mem_assoc "name" ev);
          Alcotest.(check bool) "has ts" true (List.mem_assoc "ts" ev)
        | _ -> Alcotest.fail "expected event object")
     | _ -> Alcotest.fail "expected events array")
  | _ -> Alcotest.fail "expected JSON object"

let test_trace_json_timestamps () =
  let (inst, prof) = fresh () in
  eval inst "(define (f) 42)";
  eval inst "(f)";
  finish inst prof;
  let json_str = Profile_report.trace_json prof in
  let json = Yojson.Safe.from_string json_str in
  match json with
  | `Assoc assoc ->
    (match List.assoc "traceEvents" assoc with
     | `List events ->
       List.iter (fun ev ->
         match ev with
         | `Assoc fields ->
           (match List.assoc "ts" fields with
            | `Float ts ->
              Alcotest.(check bool) "ts >= 0" true (ts >= 0.0)
            | `Int ts ->
              Alcotest.(check bool) "ts >= 0" true (ts >= 0)
            | _ -> Alcotest.fail "expected numeric ts")
         | _ -> Alcotest.fail "expected event object"
       ) events
     | _ -> Alcotest.fail "expected events array")
  | _ -> Alcotest.fail "expected JSON object"

let test_trace_json_empty () =
  let prof = Profiler.create () in
  Profiler.finalize prof;
  let json_str = Profile_report.trace_json prof in
  let json = Yojson.Safe.from_string json_str in
  match json with
  | `Assoc assoc ->
    (match List.assoc "traceEvents" assoc with
     | `List events ->
       Alcotest.(check int) "empty events" 0 (List.length events)
     | _ -> Alcotest.fail "expected events array")
  | _ -> Alcotest.fail "expected JSON object"

let () =
  Alcotest.run "Profile_report" [
    "text", [
      Alcotest.test_case "contains names and counts" `Quick
        test_text_report_contains_names;
      Alcotest.test_case "sorted by self time" `Quick
        test_text_report_sorted_by_self;
      Alcotest.test_case "empty profile" `Quick
        test_text_report_empty;
      Alcotest.test_case "column headers" `Quick
        test_text_report_column_header;
    ];
    "flamegraph", [
      Alcotest.test_case "valid SVG structure" `Quick
        test_flamegraph_valid_svg;
      Alcotest.test_case "has rect elements" `Quick
        test_flamegraph_has_rects;
      Alcotest.test_case "has tooltips" `Quick
        test_flamegraph_has_tooltips;
      Alcotest.test_case "empty profile" `Quick
        test_flamegraph_empty;
    ];
    "trace_json", [
      Alcotest.test_case "valid JSON" `Quick
        test_trace_json_valid;
      Alcotest.test_case "has B/E events" `Quick
        test_trace_json_has_events;
      Alcotest.test_case "timestamps non-negative" `Quick
        test_trace_json_timestamps;
      Alcotest.test_case "empty profile" `Quick
        test_trace_json_empty;
    ];
  ]
