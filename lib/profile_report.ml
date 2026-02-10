(* --- Text report --- *)

let text_report prof =
  let entries = Profiler.entries prof in
  if entries = [] then
    "No profiling data collected.\n"
  else begin
    (* Sort by self time descending *)
    let sorted = List.sort (fun (_, a) (_, b) ->
      compare (b : Profiler.proc_stats).self_time a.self_time) entries in
    let total_time = List.fold_left (fun acc (_, s) ->
      acc +. s.Profiler.self_time) 0.0 sorted in
    let buf = Buffer.create 256 in
    Buffer.add_string buf
      (Printf.sprintf "  %5s   %8s  %8s  %8s  %-28s %s\n"
         "self%" "total" "self" "calls" "name" "location");
    List.iter (fun ((pk : Profiler.proc_key), (s : Profiler.proc_stats)) ->
      let pct = if total_time > 0.0 then s.self_time /. total_time *. 100.0
                else 0.0 in
      let loc_str =
        if Loc.equal pk.pk_loc Loc.none then "<primitive>"
        else Loc.to_string pk.pk_loc
      in
      Buffer.add_string buf
        (Printf.sprintf "  %5.1f   %8.3f  %8.3f  %8d  %-28s %s\n"
           pct s.total_time s.self_time s.call_count pk.pk_name loc_str)
    ) sorted;
    Buffer.add_string buf
      (Printf.sprintf "Total time: %.3fs\n" total_time);
    Buffer.contents buf
  end

(* --- Flame graph SVG --- *)

type flame_node = {
  fn_name : string;
  mutable fn_self : float;    (* self time in μs *)
  mutable fn_total : float;   (* total time = self + children *)
  fn_children : (string, flame_node) Hashtbl.t;
}

let make_node name = {
  fn_name = name;
  fn_self = 0.0;
  fn_total = 0.0;
  fn_children = Hashtbl.create 4;
}

(* Build prefix tree from collapsed flame stacks *)
let build_tree stacks =
  let root = make_node "root" in
  List.iter (fun (path, self_us) ->
    let parts = String.split_on_char ';' path in
    let rec walk node = function
      | [] -> ()
      | [leaf] ->
        let child = match Hashtbl.find_opt node.fn_children leaf with
          | Some c -> c
          | None ->
            let c = make_node leaf in
            Hashtbl.replace node.fn_children leaf c; c
        in
        child.fn_self <- child.fn_self +. self_us
      | hd :: tl ->
        let child = match Hashtbl.find_opt node.fn_children hd with
          | Some c -> c
          | None ->
            let c = make_node hd in
            Hashtbl.replace node.fn_children hd c; c
        in
        walk child tl
    in
    walk root parts
  ) stacks;
  (* Compute total_time bottom-up *)
  let rec compute node =
    let child_total = Hashtbl.fold (fun _ child acc ->
      compute child;
      acc +. child.fn_total
    ) node.fn_children 0.0 in
    node.fn_total <- node.fn_self +. child_total
  in
  compute root;
  root

(* Hash a string to a warm color *)
let warm_color name =
  let h = Hashtbl.hash name in
  let r = 200 + (h mod 55) in
  let g = 80 + ((h / 55) mod 120) in
  let b = 30 + ((h / (55 * 120)) mod 50) in
  Printf.sprintf "rgb(%d,%d,%d)" r g b

(* Escape XML special characters *)
let xml_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&apos;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let flamegraph_svg prof =
  let stacks = Profiler.flame_stacks prof in
  let root = build_tree stacks in
  let svg_width = 1200.0 in
  let frame_height = 16.0 in
  (* Calculate max depth for SVG height *)
  let rec max_depth node =
    Hashtbl.fold (fun _ child acc ->
      max acc (max_depth child)
    ) node.fn_children 0 + 1
  in
  let depth = max_depth root in
  let svg_height = float_of_int (depth + 1) *. frame_height +. 20.0 in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    (Printf.sprintf
       "<svg xmlns=\"http://www.w3.org/2000/svg\" \
        width=\"%.0f\" height=\"%.0f\" \
        viewBox=\"0 0 %.0f %.0f\">\n"
       svg_width svg_height svg_width svg_height);
  Buffer.add_string buf
    "<style>text { font-family: monospace; font-size: 11px; fill: #333; }</style>\n";
  (* Render frames bottom-up: root at bottom *)
  let total = root.fn_total in
  if total > 0.0 then begin
    let rec render node x_offset width level =
      let y = svg_height -. (float_of_int (level + 1)) *. frame_height -. 10.0 in
      if width >= 1.0 then begin
        let color = warm_color node.fn_name in
        Buffer.add_string buf
          (Printf.sprintf
             "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.0f\" \
              fill=\"%s\" stroke=\"white\" stroke-width=\"0.5\">\
              <title>%s (%.1f &#181;s, %.1f%%)</title></rect>\n"
             x_offset y width (frame_height -. 1.0) color
             (xml_escape node.fn_name) node.fn_total
             (node.fn_total /. total *. 100.0));
        if width > 50.0 then begin
          let max_chars = int_of_float (width /. 7.0) in
          let label =
            if String.length node.fn_name > max_chars then
              String.sub node.fn_name 0 (max max_chars 1)
            else node.fn_name
          in
          Buffer.add_string buf
            (Printf.sprintf
               "<text x=\"%.1f\" y=\"%.1f\" \
                clip-path=\"url(#clip)\">%s</text>\n"
               (x_offset +. 2.0) (y +. frame_height -. 4.0)
               (xml_escape label))
        end;
        (* Render children left to right *)
        let child_x = ref x_offset in
        let children = Hashtbl.fold (fun _ c acc -> c :: acc)
          node.fn_children [] in
        let sorted = List.sort (fun a b ->
          compare b.fn_total a.fn_total) children in
        List.iter (fun child ->
          let child_width = child.fn_total /. total *. svg_width in
          render child !child_x child_width (level + 1);
          child_x := !child_x +. child_width
        ) sorted
      end
    in
    render root 0.0 svg_width 0
  end;
  (* Embed zoom script *)
  Buffer.add_string buf
    "<script type=\"text/ecmascript\"><![CDATA[\n\
     var svg = document.querySelector('svg');\n\
     svg.addEventListener('click', function(e) {\n\
       var t = e.target;\n\
       if (t.tagName === 'rect') {\n\
         var x = parseFloat(t.getAttribute('x'));\n\
         var w = parseFloat(t.getAttribute('width'));\n\
         var vb = svg.viewBox.baseVal;\n\
         vb.x = x; vb.width = w;\n\
       } else { svg.viewBox.baseVal.x = 0; \
         svg.viewBox.baseVal.width = 1200; }\n\
     });\n\
     ]]></script>\n";
  Buffer.add_string buf "</svg>\n";
  Buffer.contents buf

(* --- Chrome Trace Event JSON --- *)

let trace_json prof =
  let events = Profiler.trace_events prof in
  let json_events = List.map (fun (ev : Profiler.trace_event) ->
    `Assoc [
      ("ph", `String (String.make 1 ev.te_ph));
      ("name", `String ev.te_name);
      ("cat", `String ev.te_cat);
      ("pid", `Int 1);
      ("tid", `Int 1);
      ("ts", `Float ev.te_ts);
    ]
  ) events in
  let json = `Assoc [("traceEvents", `List json_events)] in
  Yojson.Safe.to_string json
