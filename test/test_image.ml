(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Store_irmin
module I = Octra_core.Ledger_image
module W = Test_workspace

let run = Lwt_main.run
let expect message value = if not value then failwith message
let ok = function Ok value -> value | Error reason -> failwith reason

let with_store path action =
  let store = run (S.open_store path) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () -> action store)

let seed store pairs =
  let tree = run (S.begin_bulk store) in
  let tree = List.fold_left (fun tree (path, value) ->
    run (S.bulk_add tree path value)) tree pairs in
  run (S.commit_bulk store tree "image inputs");
  Option.get (run (S.get_commit_hash store)), Option.get (run (S.get_head_hash store))

let test_roundtrip () =
  W.with_dir "image" (fun dir ->
    let source = Filename.concat dir "ledger.dat" in
    let target = Filename.concat dir "restored" in
    with_store (Filename.concat dir "origin") (fun store ->
      let keys = ["short"; String.make 4096 'b'; String.make 4097 'c';
        String.init 8214 (fun index -> Char.chr (index land 255));
        String.init 8229 (fun index -> Char.chr ((index * 17) land 255));
        "../key/with/slashes"; "";
        String.make 131072 'd'; String.make 1048576 'e' ^ "\000\255";
        String.make 4194305 'f'] in
      let pairs = List.map (fun key -> ["contracts"; "program"; "storage"; key], "kept") keys in
      let commit, root = seed store pairs in
      let written = run (I.write store ~commit ~path:source) |> ok in
      expect "export root differs" (written.root = root);
      expect "export byte count differs"
        (written.bytes = (Unix.LargeFile.stat source).Unix.LargeFile.st_size);
      expect "existing image was overwritten" (Result.is_error (run (I.write store ~commit ~path:source)));
      expect "source commit changed" (run (S.get_commit_hash store) = Some commit);
      let restored = run (I.restore ~source ~target ~expected_root:root) |> ok in
      expect "restored root differs" (restored.root = root);
      expect "record count differs" (restored.records = written.records);
      with_store target (fun restored ->
        List.iter (fun (path, value) ->
          expect "key or value changed" (run (S.read restored path) = Some value)) pairs);
      let again = run (I.restore ~source ~target ~expected_root:root) |> ok in
      expect "repeated restore changed commit" (again.commit = restored.commit);
      let rejected = Filename.concat dir "rejected" in
      expect "wrong root accepted after batch commit"
        (Result.is_error (run (I.restore ~source ~target:rejected ~expected_root:(String.make 128 '0'))));
      expect "wrong root published" (not (Sys.file_exists rejected));
      expect "wrong root retained staged state" (not (Sys.file_exists (rejected ^ ".next")));
      expect "staging remains" (not (Sys.file_exists (target ^ ".next")))));
  print_endline "status = pass test = image_roundtrip"

let test_large_value () =
  W.with_dir "image-value" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let value = String.make 50_000_000 'x' in
      let path = ["circles"; "program"; "stable"; "by_hash"; "entry"] in
      let commit, root = seed store [path, value] in
      let source = Filename.concat dir "ledger.dat" in
      let written = run (I.write store ~commit ~path:source) |> ok in
      let target = Filename.concat dir "restored" in
      let restored = run (I.restore ~source ~target ~expected_root:root) |> ok in
      expect "large value root differs" (written.root = root && restored.root = root);
      expect "large value changed source" (run (S.get_commit_hash store) = Some commit);
      with_store target (fun store ->
        expect "large value differs" (run (S.read store path) = Some value))));
  print_endline "status = pass test = image_value"

let test_circle_value () =
  W.with_dir "image-circle" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let module C = Octra_core.Circles in
      let entry : C.stable_entry = {
        raw_key = String.make 8_000_000 '\000';
        key_hash = String.make 64 'a'; value = C.Inline "one";
      } in
      let values = Hashtbl.create 1 in
      Hashtbl.add values entry.raw_key "one";
      ignore (C.validate_stable_storage C.default_limits values |> ok);
      run (S.save_circle_stable_entry store "program" entry);
      let commit = Option.get (run (S.get_commit_hash store)) in
      let root = Option.get (run (S.get_head_hash store)) in
      let source = Filename.concat dir "ledger.dat" in
      let written = run (I.write store ~commit ~path:source) |> ok in
      expect "circle encoding did not exceed old limit" (written.bytes > 44_739_244L);
      let target = Filename.concat dir "restored" in
      let restored = run (I.restore ~source ~target ~expected_root:root) |> ok in
      expect "circle value root differs" (restored.root = root);
      with_store target (fun store ->
        expect "circle value differs"
          (run (S.get_circle_stable_entry store "program" entry.key_hash) = Some entry))));
  print_endline "status = pass test = image_circle"

let write path bytes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel bytes)

let integer width value =
  String.init width (fun index ->
    Char.chr (Int64.to_int (Int64.logand 255L (Int64.shift_right_logical value (index * 8)))))

let u32 value = integer 4 (Int64.of_int value)

let record ?(full = false) ~wide ~kind path value =
  u32 (List.length path) ^ String.make 1 (Char.chr kind)
  ^ String.concat "" (List.map (fun part ->
      integer (if wide then 8 else 4) (Int64.of_int (String.length part)) ^ part) path)
  ^ (if kind = 2 then integer (if full then 8 else 4)
      (Int64.of_int (String.length value)) ^ value else "")

let image ?(full = false) ~wide records =
  (if full then "octra-ledger-image-3\n"
   else if wide then "octra-ledger-image-2\n" else "octra-ledger-image\n")
  ^ String.concat "" records ^ u32 0

let contains value term =
  let rec loop index =
    index + String.length term <= String.length value
    && (String.sub value index (String.length term) = term || loop (index + 1)) in
  loop 0

let refuse ~source ~target ~root ~reason =
  begin match run (I.restore ~source ~target ~expected_root:root) with
  | Ok _ -> failwith "invalid image accepted"
  | Error actual -> expect ("unexpected refusal: " ^ actual) (contains actual reason)
  end;
  expect "refused target became visible" (not (Sys.file_exists target));
  expect "refused staging remains" (not (Sys.file_exists (target ^ ".next")))

let test_formats () =
  W.with_dir "image-formats" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let _, root = seed store [["a"], "one"; ["b"], "two"] in
      List.iter (fun (wide, full, name) ->
        let source = Filename.concat dir name in
        let target = source ^ ".store" in
        let records = List.map (fun (path, value) -> record ~full ~wide ~kind:2 path value)
            [["a"], "one"; ["b"], "two"] in
        write source (image ~full ~wide records);
        let result = run (I.restore ~source ~target ~expected_root:root) |> ok in
        expect "compatible root differs" (result.root = root);
        let commit = result.commit in
        begin match run (I.restore ~source ~target ~expected_root:(String.make 128 '0')) with
        | Error _ -> ()
        | Ok _ -> failwith "existing root mismatch accepted"
        end;
        with_store target (fun saved ->
          expect "existing target changed" (run (S.get_commit_hash saved) = Some commit))
      ) [false, false, "v1.dat"; true, false, "v2.dat"; true, true, "v3.dat"]));
  print_endline "status = pass test = image_formats"

let test_invalid () =
  W.with_dir "image-invalid" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let _, root = seed store [["a"], "one"] in
      let header = "octra-ledger-image-2\n" in
      let valid = record ~wide:true ~kind:2 ["a"] "one" in
      let cases = [
        "header", "not an image\n", "header is invalid";
        "header_size", String.make 256 'x', "header is invalid";
        "width", header ^ u32 1025 ^ "\001", "path width exceeds limit";
        "length", header ^ u32 1 ^ "\002" ^ integer 8 4_294_967_296L ^ "x", "remaining file bytes";
        "length_sign", header ^ u32 1 ^ "\002" ^ integer 8 Int64.min_int, "exceeds limit";
        "part_limit", image ~wide:false [record ~wide:false ~kind:2 [String.make 4097 'x'] "one"], "part = 0 bytes = 4097";
        "part_short", header ^ u32 1 ^ "\002" ^ integer 8 100L ^ "x", "remaining file bytes";
        "value_short", header ^ u32 1 ^ "\002" ^ integer 8 1L ^ "a" ^ u32 100 ^ "x", "remaining file bytes";
        "value_limit", header ^ u32 1 ^ "\002" ^ integer 8 1L ^ "a" ^ u32 50_000_000, "value exceeds limit";
        "kind", image ~wide:true [record ~wide:true ~kind:3 ["a"] ""], "record kind is invalid";
        "duplicate", image ~wide:true [valid; valid], "not strictly ordered";
        "order", image ~wide:true [record ~wide:true ~kind:2 ["b"] "two"; valid], "not strictly ordered";
        "extra", image ~wide:true [valid] ^ "x", "trailing bytes";
        "root", image ~wide:true [record ~wide:true ~kind:2 ["a"] "different"], "root differs";
        "end", header ^ valid, "End_of_file";
      ] in
      List.iter (fun (name, bytes, reason) ->
        let source = Filename.concat dir (name ^ ".dat") in
        write source bytes;
        try refuse ~source ~target:(source ^ ".store") ~root ~reason
        with Failure message -> failwith (name ^ ": " ^ message)
      ) cases));
  print_endline "status = pass test = image_invalid"

let test_export_failure () =
  W.with_dir "image-export" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let commit, root = seed store [List.init 1025 (fun _ -> "x"), "one"] in
      let source = Filename.concat dir "ledger.dat" in
      begin match run (I.write store ~commit ~path:source) with
      | Error reason -> expect "export refusal differs" (contains reason "parts = 1025")
      | Ok _ -> failwith "invalid path exported"
      end;
      expect "partial export remains" (not (Sys.file_exists source));
      expect "failed export changed root" (run (S.get_head_hash store) = Some root);
      expect "failed export changed commit" (run (S.get_commit_hash store) = Some commit)));
  print_endline "status = pass test = image_export"

let test_full_lengths () =
  W.with_dir "image-lengths" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let _, root = seed store [["a"], "one"] in
      let start = "octra-ledger-image-3\n" ^ u32 1 ^ "\002" in
      let value = start ^ integer 8 1L ^ "a" in
      List.iter (fun (name, prefix) ->
        List.iter (fun (length, reason) ->
          let source = Filename.concat dir (name ^ Int64.to_string length ^ ".dat") in
          write source (prefix ^ integer 8 length ^ "x");
          refuse ~source ~target:(source ^ ".store") ~root ~reason)
          [50_000_000L, "remaining file bytes";
           4_294_967_296L, "remaining file bytes";
           Int64.min_int, "exceeds limit";
           Int64.max_int, "exceeds limit"])
        ["part", start; "value", value];
      let valid = image ~full:true ~wide:true [record ~full:true ~wide:true ~kind:2 ["a"] "one"] in
      List.iter (fun length ->
        let source = Filename.concat dir ("short" ^ string_of_int length ^ ".dat") in
        write source (String.sub valid 0 length);
        refuse ~source ~target:(source ^ ".store") ~root ~reason:"End_of_file")
        [19; 23; 24; 25; 31; 32; 33; 39; String.length valid - 1]));
  print_endline "status = pass test = image_lengths"

let test_empty () =
  W.with_dir "image-empty" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let commit, root = seed store [] in
      let source = Filename.concat dir "ledger.dat" in
      let written = run (I.write store ~commit ~path:source) |> ok in
      let target = Filename.concat dir "restored" in
      let restored = run (I.restore ~source ~target ~expected_root:root) |> ok in
      expect "empty image has records" (written.records = 0L && restored.records = 0L);
      expect "empty root differs" (restored.root = root)));
  print_endline "status = pass test = image_empty"

let test_live source commit dir =
  Unix.mkdir dir 0o750;
  let store = run (S.open_store ~readonly:true source) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let head = run (S.get_commit_hash store) in
    let source = Filename.concat dir "ledger.dat" in
    let written = run (I.write store ~commit ~path:source) |> ok in
    Printf.printf "event = image_export records = %Ld bytes = %Ld commit = %s root = %s\n%!"
      written.records written.bytes written.commit written.root;
    let target = Filename.concat dir "restored" in
    let restored = run (I.restore ~source ~target ~expected_root:written.root) |> ok in
    expect "live record count differs" (restored.records = written.records);
    expect "live image root differs" (restored.root = written.root);
    expect "live source changed" (run (S.get_commit_hash store) = head);
    Printf.printf "event = image_restore status = pass records = %Ld bytes = %Ld root = %s\n%!"
      restored.records restored.bytes restored.root)

let () =
  match Array.to_list Sys.argv with
  | [_] ->
      test_large_value ();
      test_circle_value ();
      test_roundtrip ();
      test_formats ();
      test_invalid ();
      test_full_lengths ();
      test_export_failure ();
      test_empty ()
  | [_; "--store"; source; commit; dir] -> test_live source commit dir
  | _ -> failwith "image test arguments are invalid"