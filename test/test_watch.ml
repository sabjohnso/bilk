open Bilk

(* --- Helpers --- *)

let event_testable =
  let pp fmt (e : Watch.event) =
    let kind_str = match e.kind with
      | `Modified -> "Modified"
      | `Created -> "Created"
      | `Deleted -> "Deleted"
    in
    Format.fprintf fmt "{path=%s; kind=%s}" e.path kind_str
  in
  Alcotest.testable pp (fun a b ->
    a.Watch.path = b.Watch.path && a.kind = b.kind)

let with_tmp_dir f =
  let dir = Filename.temp_dir "bilk_watch_test" "" in
  Fun.protect ~finally:(fun () ->
    ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* Force polling backend by passing a watcher that was created as polling *)
let make_polling_watcher ?(extensions = [".sld"; ".scm"]) () =
  (* Create with short poll interval for tests *)
  Watch.create ~poll_interval_ms:50 ~extensions ()

(* --- Polling tests --- *)

let test_poll_detects_modification () =
  with_tmp_dir (fun dir ->
    let path = Filename.concat dir "test.sld" in
    write_file path "initial";
    let w = make_polling_watcher () in
    Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
      Watch.add_directory w dir;
      (* Initial poll should be empty (baseline snapshot taken) *)
      let events = Watch.poll w in
      Alcotest.(check (list event_testable)) "no initial events" [] events;
      (* Modify the file — need mtime to change *)
      Unix.sleepf 0.05;
      write_file path "modified";
      let events = Watch.poll w in
      Alcotest.(check bool) "has modification event"
        true (List.exists (fun (e : Watch.event) ->
          Filename.basename e.path = "test.sld" && e.kind = `Modified) events)))

let test_poll_detects_creation () =
  with_tmp_dir (fun dir ->
    let w = make_polling_watcher () in
    Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
      Watch.add_directory w dir;
      (* Baseline poll *)
      ignore (Watch.poll w);
      (* Create a new file *)
      let path = Filename.concat dir "new.sld" in
      write_file path "created";
      let events = Watch.poll w in
      Alcotest.(check bool) "has creation event"
        true (List.exists (fun (e : Watch.event) ->
          Filename.basename e.path = "new.sld" && e.kind = `Created) events)))

let test_poll_detects_deletion () =
  with_tmp_dir (fun dir ->
    let path = Filename.concat dir "delete_me.sld" in
    write_file path "to be deleted";
    let w = make_polling_watcher () in
    Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
      Watch.add_directory w dir;
      (* Baseline poll *)
      ignore (Watch.poll w);
      (* Delete the file *)
      Sys.remove path;
      let events = Watch.poll w in
      Alcotest.(check bool) "has deletion event"
        true (List.exists (fun (e : Watch.event) ->
          Filename.basename e.path = "delete_me.sld" && e.kind = `Deleted) events)))

let test_poll_ignores_non_matching_extension () =
  with_tmp_dir (fun dir ->
    let w = make_polling_watcher () in
    Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
      Watch.add_directory w dir;
      ignore (Watch.poll w);
      (* Create a .txt file — should be ignored *)
      write_file (Filename.concat dir "notes.txt") "hello";
      let events = Watch.poll w in
      Alcotest.(check (list event_testable)) "no events for .txt" [] events))

let test_poll_empty_when_no_changes () =
  with_tmp_dir (fun dir ->
    let path = Filename.concat dir "stable.sld" in
    write_file path "content";
    let w = make_polling_watcher () in
    Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
      Watch.add_directory w dir;
      (* Baseline poll *)
      ignore (Watch.poll w);
      (* Immediate second poll — no changes *)
      let events = Watch.poll w in
      Alcotest.(check (list event_testable)) "no events" [] events))

let test_poll_recursive () =
  with_tmp_dir (fun dir ->
    let sub = Filename.concat dir "sub" in
    Unix.mkdir sub 0o755;
    let path = Filename.concat sub "nested.sld" in
    write_file path "initial";
    let w = make_polling_watcher () in
    Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
      Watch.add_directory w dir;
      (* Baseline *)
      ignore (Watch.poll w);
      (* Modify nested file *)
      Unix.sleepf 0.05;
      write_file path "modified";
      let events = Watch.poll w in
      Alcotest.(check bool) "has nested modification event"
        true (List.exists (fun (e : Watch.event) ->
          Filename.basename e.path = "nested.sld" && e.kind = `Modified) events)))

(* --- Inotify tests --- *)

let test_inotify_backend_selected () =
  (* On Linux, inotify should be selected; elsewhere, polling *)
  let w = Watch.create () in
  Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
    let is_linux = Sys.file_exists "/proc/version" in
    if is_linux then
      Alcotest.(check bool) "inotify backend on Linux"
        true (Watch.backend w = Watch.Inotify)
    else
      Alcotest.(check bool) "polling backend on non-Linux"
        true (Watch.backend w = Watch.Polling))

let test_inotify_fd_is_some () =
  let w = Watch.create () in
  Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
    if Watch.backend w = Watch.Inotify then
      Alcotest.(check bool) "fd is Some on inotify"
        true (Watch.fd w <> None)
    else
      Alcotest.(check bool) "fd is None on polling"
        true (Watch.fd w = None))

let test_inotify_detects_modification () =
  with_tmp_dir (fun dir ->
    let w = Watch.create () in
    Fun.protect ~finally:(fun () -> Watch.destroy w) (fun () ->
      if Watch.backend w <> Watch.Inotify then
        (* Skip on non-inotify *)
        Alcotest.(check pass) "skipped (no inotify)" () ()
      else begin
        let path = Filename.concat dir "test.sld" in
        write_file path "initial";
        Watch.add_directory w dir;
        (* Small delay then modify *)
        Unix.sleepf 0.05;
        write_file path "modified";
        (* Wait briefly for inotify events *)
        Unix.sleepf 0.1;
        let events = Watch.poll w in
        Alcotest.(check bool) "has inotify modification event"
          true (List.exists (fun (e : Watch.event) ->
            Filename.basename e.path = "test.sld"
            && (e.kind = `Modified || e.kind = `Created)) events)
      end))

(* --- Test suite --- *)

let () =
  Alcotest.run "Watch" [
    "polling", [
      Alcotest.test_case "detects modification" `Quick
        test_poll_detects_modification;
      Alcotest.test_case "detects creation" `Quick
        test_poll_detects_creation;
      Alcotest.test_case "detects deletion" `Quick
        test_poll_detects_deletion;
      Alcotest.test_case "ignores non-matching extension" `Quick
        test_poll_ignores_non_matching_extension;
      Alcotest.test_case "empty when no changes" `Quick
        test_poll_empty_when_no_changes;
      Alcotest.test_case "recursive" `Quick
        test_poll_recursive;
    ];
    "inotify", [
      Alcotest.test_case "backend selected" `Quick
        test_inotify_backend_selected;
      Alcotest.test_case "fd is some" `Quick
        test_inotify_fd_is_some;
      Alcotest.test_case "detects modification" `Quick
        test_inotify_detects_modification;
    ];
  ]
