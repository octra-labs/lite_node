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

let test_roundtrip ?(format = I.Flat) () =
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
      let written = run (I.write ~format store ~commit ~path:source) |> ok in
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
      ) [false, false, "prior.dat"; true, false, "path.dat"; true, true, "full.dat"]));
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

let test_foreign_path () =
  W.with_dir "image-path" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let commit, root = seed store [["a"], "one"] in
      let path = Filename.concat dir "ledger.dat" in
      let destination = Filename.concat dir "absent" in
      Unix.symlink destination path;
      expect "image replaced an existing path"
        (Result.is_error (run (I.write store ~commit ~path)));
      let preserved = try (Unix.lstat path).Unix.st_kind = Unix.S_LNK
        with Unix.Unix_error (Unix.ENOENT, _, _) -> false in
      expect "image removed a path it did not create" preserved;
      expect "image changed existing link" (Unix.readlink path = destination);
      expect "image wrote through existing link" (not (Sys.file_exists destination));
      expect "image refusal changed source" (run (S.get_head_hash store) = Some root)));
  print_endline "status = pass test = image_path"

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let same_stream left right =
  In_channel.with_open_bin left (fun left ->
    In_channel.with_open_bin right (fun right ->
      let a = Bytes.create 65536 in
      let b = Bytes.create 65536 in
      let rec loop () =
        let n = input left a 0 (Bytes.length a) in
        let m = input right b 0 (Bytes.length b) in
        n = m && Bytes.sub_string a 0 n = Bytes.sub_string b 0 m
        && (n = 0 || loop ()) in
      loop ()))

let test_shared flat =
  W.with_dir "image-shared" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let payload = String.init 131089 (fun n -> Char.chr ((n * 17) land 255)) in
      let pairs = List.init 65 (fun n ->
        let path = ["compute"; "roots"; Printf.sprintf "%02d" n] in
        [path @ ["data"; "payload"], payload;
         path @ ["state"], if n = 64 then "changed" else "same"])
        |> List.concat in
      let commit, root = seed store pairs in
      let source = Filename.concat dir "shared.dat" in
      let format = if flat then I.Flat else I.Shared in
      let written = run (I.write ~format store ~commit ~path:source) |> ok in
      expect "shared image expanded repeated roots" (written.bytes < 200_000L);
      expect "shared byte count differs"
        (written.bytes = (Unix.LargeFile.stat source).Unix.LargeFile.st_size);
      let target = Filename.concat dir "restored" in
      let restored = run (I.restore ~source ~target ~expected_root:root) |> ok in
      expect "shared root differs" (restored.root = root);
      expect "shared record count differs" (restored.records = written.records);
      let flat_bytes = with_store target (fun saved ->
        List.iter (fun (path, value) ->
          expect "shared subtree differs" (run (S.read saved path) = Some value)) pairs;
        let commit = Option.get (run (S.get_commit_hash saved)) in
        let copy = Filename.concat dir "copy.dat" in
        ignore (run (I.write ~format:I.Shared saved ~commit ~path:copy) |> ok);
        expect "shared stream changed after restore" (same_stream source copy);
        let a = Filename.concat dir "flat.dat" in
        let b = Filename.concat dir "flat-copy.dat" in
        let report = run (I.write saved ~commit ~path:a) |> ok in
        ignore (run (I.write ~format:I.Flat saved ~commit ~path:b) |> ok);
        expect "default format changed" (same_stream a b);
        expect "flat size control did not expand" (report.bytes > 8_000_000L);
        let start = In_channel.with_open_bin a (fun input ->
          really_input_string input (String.length "octra-ledger-image-3\n")) in
        expect "default header changed" (start = "octra-ledger-image-3\n");
        report.bytes) in
      expect "shared export changed source" (run (S.get_commit_hash store) = Some commit);
      Printf.printf "status = pass test = image_shared bytes = %Ld flat_bytes = %Ld records = %Ld\n%!"
        written.bytes flat_bytes written.records))

let test_shared_pvac () =
  W.with_dir "image-shared-pvac" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let hash = String.make 64 'a' in
      let pairs = [["a"; "key"], hash; ["pvac_hashes"; "key"], hash] in
      let commit, root = seed store pairs in
      let source = Filename.concat dir "shared.dat" in
      let report = run (I.write ~format:I.Shared store ~commit ~path:source) |> ok in
      expect "shared PVAC hash was omitted" (report.pvac_hashes = [hash]);
      let target = Filename.concat dir "restored" in
      ignore (run (I.restore ~source ~target ~expected_root:root) |> ok);
      let commit, _ = seed store [["a"; "key"], "invalid"; ["pvac_hashes"; "key"], "invalid"] in
      let source = Filename.concat dir "invalid.dat" in
      expect "shared PVAC hash validation was skipped"
        (Result.is_error (run (I.write ~format:I.Shared store ~commit ~path:source)));
      expect "invalid shared image remains" (not (Sys.file_exists source))));
  print_endline "status = pass test = image_shared_pvac"

let test_shared_invalid () =
  W.with_dir "image-shared-invalid" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let _, root = seed store [["a"; "x"], "one"; ["b"; "x"], "one"] in
      let tree = run (S.Store.Tree.add (S.Store.Tree.empty ()) ["x"] "one") in
      let hash = Irmin.Type.to_string S.Store.Hash.t (S.Store.Tree.hash tree) in
      let value = run (S.Store.Tree.add (S.Store.Tree.empty ()) [] "one") in
      let leaf = Irmin.Type.to_string S.Store.Hash.t (S.Store.Tree.hash value) in
      let other = run (S.Store.Tree.add (S.Store.Tree.empty ()) ["x"] "two") in
      let wrong = Irmin.Type.to_string S.Store.Hash.t (S.Store.Tree.hash other) in
      let row kind path = record ~full:true ~wide:true ~kind path "" in
      let text hash = integer 8 (Int64.of_int (String.length hash)) ^ hash in
      let finish path hash = row 5 path ^ text hash in
      let reference kind path id hash = row kind path ^ integer 8 id ^ text hash in
      let node = row 1 ["a"] in
      let data = record ~full:true ~wide:true ~kind:2 ["a"; "x"] "one" in
      let closed = [node; data; finish ["a"] hash] in
      let image rows = "octra-ledger-image-refs\n" ^ String.concat "" rows ^ u32 0 in
      let cases = [
        "zero", closed @ [reference 3 ["b"] 0L hash], "not backward";
        "negative", closed @ [reference 3 ["b"] (-1L) hash], "not backward";
        "self", closed @ [reference 3 ["b"] 4L hash], "not backward";
        "forward", closed @ [reference 3 ["b"] 5L hash], "not backward";
        "unfinished", [node; reference 3 ["a"; "x"] 1L hash], "not finished";
        "cycle", [node; row 1 ["a"; "x"]; reference 3 ["a"; "x"; "y"] 1L hash], "not finished";
        "finish_id", closed @ [reference 3 ["b"] 3L hash], "not finished";
        "node_kind", closed @ [reference 4 ["b"] 1L hash], "kind mismatch";
        "value_kind", closed @ [reference 3 ["b"] 2L leaf], "kind mismatch";
        "ref_hash", closed @ [reference 3 ["b"] 1L wrong], "reference hash mismatch";
        "finish_hash", [node; data; finish ["a"] wrong], "subtree hash mismatch";
        "hash_text", closed @ [reference 3 ["b"] 1L "invalid"], "hash is invalid";
        "hash_case", closed @ [reference 3 ["b"] 1L (String.uppercase_ascii hash)], "hash is invalid";
        "hash_size", closed @ [row 3 ["b"] ^ integer 8 1L ^ integer 8 257L], "exceeds limit";
        "hash_short", closed @ [row 3 ["b"] ^ integer 8 1L ^ integer 8 128L], "remaining file bytes";
        "id_short", closed @ [row 3 ["b"] ^ "\001"], "End_of_file";
        "kind", closed @ [row 6 ["b"]], "record kind is invalid";
        "finish_order", [node; row 1 ["a"; "x"]; finish ["a"] hash], "finish order";
        "finish_twice", closed @ [finish ["a"] hash], "finish order";
        "open", [node; data], "unfinished";
        "parent", [data], "subtree order";
        "sibling_open", [node; row 1 ["b"]], "subtree order";
        "ref_child", closed @ [reference 3 ["b"] 1L hash; row 1 ["b"; "x"]], "subtree order";
        "value_child", [record ~full:true ~wide:true ~kind:2 ["a"] "one"; row 1 ["a"; "x"]], "subtree order";
        "duplicate", closed @ [reference 3 ["a"] 1L hash], "not strictly ordered";
        "order", closed @ [reference 3 [""] 1L hash], "not strictly ordered";
      ] in
      List.iter (fun (name, rows, reason) ->
        let source = Filename.concat dir (name ^ ".dat") in
        write source (image rows);
        refuse ~source ~target:(source ^ ".store") ~root ~reason) cases;
      List.iter (fun (name, rows, root) ->
        let source = Filename.concat dir (name ^ ".dat") in
        write source (image rows);
        let report = run (I.restore ~source ~target:(source ^ ".store") ~expected_root:root) |> ok in
        expect "valid reference root differs" (report.root = root))
        ["node", closed @ [reference 3 ["b"] 1L hash], root;
         "value", [node; data; finish ["a"] hash; row 1 ["b"];
            reference 4 ["b"; "x"] 2L leaf; finish ["b"] hash], root];
      List.iter (fun header ->
        let source = Filename.concat dir (string_of_int (String.length header) ^ "-old.dat") in
        write source (header ^ row 3 ["a"] ^ integer 8 1L ^ text hash ^ u32 0);
        refuse ~source ~target:(source ^ ".store") ~root ~reason:"record kind is invalid")
        ["octra-ledger-image\n"; "octra-ledger-image-2\n"; "octra-ledger-image-3\n"]));
  print_endline "status = pass test = image_shared_invalid"

let test_write_yield () =
  W.with_dir "image-write" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let pairs = List.init 1024 (fun index -> ["values"; string_of_int index], "one") in
      let commit, root = seed store pairs in
      let source = Filename.concat dir "ledger.dat" in
      let completed = ref false in
      let changed = ref false in
      let open Lwt.Syntax in
      let control =
        let* () = Lwt.pause () in
        changed := not !completed;
        let* tree = S.begin_bulk store in
        let* tree = S.bulk_add tree ["values"; "0"] "two" in
        S.commit_bulk store tree "image concurrent write" in
      let export =
        let* written = I.write store ~commit ~path:source in
        completed := true;
        Lwt.return written in
      let written, () = run (Lwt.both export control) in
      let written = ok written in
      expect "image export did not allow another operation" !changed;
      expect "image export changed selected root" (written.root = root);
      expect "image export lost concurrent write"
        (run (S.read store ["values"; "0"]) = Some "two");
      expect "image export restored old head" (run (S.get_commit_hash store) <> Some commit);
      let records = record ~full:true ~wide:true ~kind:1 ["values"] ""
        :: List.map (fun (path, value) -> record ~full:true ~wide:true ~kind:2 path value)
          (List.sort compare pairs) in
      let expected = image ~full:true ~wide:true records in
      expect "image export changed encoded bytes" (read source = expected);
      expect "image export changed byte count" (written.bytes = Int64.of_int (String.length expected));
      expect "image export changed record count" (written.records = 1025L);
      let target = Filename.concat dir "restored" in
      let restored = run (I.restore ~source ~target ~expected_root:root) |> ok in
      expect "image export restored another root" (restored.root = root);
      with_store target (fun store ->
        List.iter (fun (path, value) ->
          expect "image export included concurrent write" (run (S.read store path) = Some value)) pairs)));
  print_endline "status = pass test = image_write_yield"

let test_replaced_path () =
  W.with_dir "image-replace" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let pairs = List.init 1024 (fun index -> ["a"; string_of_int index], "one") in
      let commit, root = seed store (("z" :: List.init 1024 (fun _ -> "x"), "one") :: pairs) in
      let source = Filename.concat dir "ledger.dat" in
      let held = Filename.concat dir "held.dat" in
      let open Lwt.Syntax in
      let control =
        let* () = Lwt.pause () in
        expect "image replacement did not find an open export" (Sys.file_exists source);
        Unix.rename source held;
        write source "preserved";
        Lwt.return_unit in
      let result, () = run (Lwt.both (I.write store ~commit ~path:source) control) in
      begin match result with
      | Error reason -> expect "replacement export refusal differs" (contains reason "parts = 1025")
      | Ok _ -> failwith "replacement export accepted invalid path"
      end;
      expect "image removed replacement path" (Sys.file_exists source);
      expect "image changed replacement contents" (read source = "preserved");
      expect "image removed moved export" (Sys.file_exists held);
      expect "image replacement changed source" (run (S.get_head_hash store) = Some root)));
  print_endline "status = pass test = image_replaced_path"

let test_empty ?(format = I.Flat) () =
  W.with_dir "image-empty" (fun dir ->
    with_store (Filename.concat dir "origin") (fun store ->
      let commit, root = seed store [] in
      let source = Filename.concat dir "ledger.dat" in
      let written = run (I.write ~format store ~commit ~path:source) |> ok in
      let target = Filename.concat dir "restored" in
      let restored = run (I.restore ~source ~target ~expected_root:root) |> ok in
      expect "empty image has records" (written.records = 0L && restored.records = 0L);
      expect "empty root differs" (restored.root = root)));
  print_endline "status = pass test = image_empty"

let test_restore_path () =
  W.with_dir "image-restore-path" (fun dir ->
    let target = Filename.concat dir "restored" in
    let commit, root = with_store target (fun store ->
      seed store [["value"], "kept"]) in
    let stage = target ^ ".next" in
    Unix.mkdir stage 0o750;
    let held = Filename.concat stage "held" in
    write held "other import";
    let source = Filename.concat dir "missing.dat" in
    expect "missing source accepted for existing store"
      (Result.is_error (run (I.restore ~source ~target ~expected_root:root)));
    expect "existing restore removed unrelated stage" (Sys.file_exists held);
    expect "existing restore changed unrelated stage" (read held = "other import");
    with_store target (fun store ->
      expect "existing restore changed commit" (run (S.get_commit_hash store) = Some commit));
    write source "unused";
    let restored = run (I.restore ~source ~target ~expected_root:root) |> ok in
    expect "existing restore returned another commit" (restored.commit = commit);
    expect "existing restore removed retained stage" (read held = "other import");
    expect "existing restore accepted another root"
      (Result.is_error (run (I.restore ~source ~target ~expected_root:(String.make 128 '0'))));
    expect "root refusal changed retained stage" (read held = "other import"));
  print_endline "status = pass test = image_restore_path"

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
      test_shared false;
      test_shared_pvac ();
      test_shared_invalid ();
      test_restore_path ();
      test_large_value ();
      test_circle_value ();
      test_roundtrip ();
      test_roundtrip ~format:I.Shared ();
      test_formats ();
      test_invalid ();
      test_full_lengths ();
      test_export_failure ();
      test_foreign_path ();
      test_write_yield ();
      test_replaced_path ();
      test_empty ();
      test_empty ~format:I.Shared ()
  | [_; "--store"; source; commit; dir] -> test_live source commit dir
  | [_; "--flat"] -> test_shared true
  | _ -> failwith "image test arguments are invalid"