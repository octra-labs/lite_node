(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Store_irmin
module T = Octra_core.Ledger_types
module W = Test_workspace

let expect label value = if not value then failwith label
let run = Lwt_main.run
let hash value = Digestif.SHA256.(digest_string value |> to_hex)

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let rec files root =
  Sys.readdir root |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
    let path = Filename.concat root name in
    let stat = Unix.lstat path in
    if stat.Unix.st_kind = Unix.S_DIR then files path
    else [path, hash (read path), stat.st_perm, stat.st_mtime])

let contains text part =
  let rec find index =
    index + String.length part <= String.length text
    && (String.sub text index (String.length part) = part || find (index + 1))
  in
  find 0

let invoke binary root =
  let path = Filename.concat root "audit.log" in
  let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o600 in
  let pid = Fun.protect ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.create_process binary [|binary; root|] Unix.stdin fd fd) in
  let status = W.wait pid in
  status, read path

let account cipher = T.{
  balance = Z.of_int 31;
  nonce = 9;
  public_key = Some "do-not-print-key";
  encrypted_balance = cipher;
  decrypt_allowance = Z.of_int 7;
}

let cases = [
  "absent", None;
  "empty", Some "";
  "zero", Some "0";
  "foreign", Some "do-not-print-cipher\000\255";
  "prefix", Some "hfhe_v1x";
  "envelope", Some "hfhe_v1|AA==";
  "truncated", Some "hfhe_v1|";
]

let seed root ~broken =
  let store = run (S.open_store ~fresh:true (Filename.concat root "irmin_store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    List.iter (fun (layout, mode) ->
      run (S.begin_epoch_batch ~mode store);
      List.iter (fun (name, cipher) ->
        run (S.set_account store (layout ^ "-" ^ name) (account cipher))
      ) cases;
      run (S.commit_epoch_batch store "audit inputs")
    ) ["old", Octra_core.Rule_graph.Prior; "parts", Octra_core.Rule_graph.Active];
    if broken then begin
      run (S.write store ["accounts"; "broken-json"; "data"] "do-not-print-invalid");
      run (S.write store ["accounts"; "missing-data"; "other"] "do-not-print-missing");
      let raw = Octra_core.Account_pack.old (account None) |> Yojson.Safe.from_string in
      let raw = match raw with
        | `Assoc fields -> `Assoc (("encrypted_balance", `Int 13)
            :: List.remove_assoc "encrypted_balance" fields)
        | _ -> assert false
      in
      run (S.write store ["accounts"; "wrong-type"; "data"] (Yojson.Safe.to_string raw));
      run (S.write store ["accounts"; "broken-parts"; "data"]
        (Octra_core.Account_pack.image (account (Some "unretained"))).data);
      let damaged = account (Some (String.make 70_000 'x')) in
      let image = Octra_core.Account_pack.image damaged in
      run (S.begin_epoch_batch ~mode:Octra_core.Rule_graph.Active store);
      run (S.set_account store "changed-part" damaged);
      run (S.commit_epoch_batch store "part input");
      let part = List.hd image.parts in
      run (S.write store (S.account_part_path "changed-part" part.id) "do-not-print-changed")
    end;
    run (S.write store ["meta"; "last_epoch"] "42");
    let snapshot = match run (S.capture_read_snapshot store) with
      | Ok value -> value
      | Error reason -> failwith reason
    in
    snapshot.state_root, snapshot.commit_hash)

let test_scan binary broken =
  W.with_dir "fhe-audit" (fun root ->
    let state_root, commit = seed root ~broken in
    let path = Filename.concat root "irmin_store" in
    let before = files path in
    let status, text = invoke binary root in
    expect "audit reports review without mutating" (status = Unix.WEXITED 2);
    expect "audit preserves all store files" (files path = before);
    expect "audit names exact root" (contains text ("state_root = " ^ state_root));
    expect "audit names exact commit" (contains text ("commit = " ^ commit));
    expect "audit completes inventory" (contains text "status = complete");
    expect "audit counts all accounts"
      (contains text (if broken then "accounts = 19 rejected" else "accounts = 14 rejected"));
    expect "audit counts explicit zeros" (contains text "loader_clears = 2");
    expect "audit counts unsupported ciphers" (contains text "loader_refuses = 2");
    List.iter (fun (format, count) ->
      expect "audit class count"
        (contains text (Printf.sprintf "format = %s accounts = %d" format count))
    ) ["absent", 2; "empty_text", 2; "zero_text", 2; "hfhe_envelope", 2; "other_text", 6];
    List.iter (fun (_, cipher) ->
      Option.iter (fun cipher ->
        expect "audit records cipher digest" (contains text ("cipher_hash = " ^ hash cipher))
      ) cipher
    ) cases;
    expect "audit counts rejected records"
      (contains text (if broken then "rejected = 5" else "rejected = 0"));
    List.iter (fun secret ->
      expect "audit does not print account contents" (not (contains text secret))
    ) ["do-not-print"; "unretained"];
    let again, second = invoke binary root in
    expect "audit restart keeps result" (again = status && second = text);
    expect "audit restart preserves store" (files path = before))

let test_absent binary =
  W.with_dir "fhe-audit-absent" (fun root ->
    let status, _ = invoke binary root in
    expect "missing store is refused" (status = Unix.WEXITED 1);
    expect "missing store is not created"
      (not (Sys.file_exists (Filename.concat root "irmin_store"))))

let test_readable binary =
  W.with_dir "fhe-audit-readable" (fun root ->
    let path = Filename.concat root "irmin_store" in
    let store = run (S.open_store ~fresh:true path) in
    run (S.set_account store "present" (account None));
    run (S.close store);
    let before = files path in
    let status, text = invoke binary root in
    expect "readable storage returns zero" (status = Unix.WEXITED 0);
    expect "readable storage is not proof acceptance"
      (contains text "classification = storage" && contains text "result = storage_readable");
    expect "readable store is unchanged" (files path = before))

let test_root binary =
  W.with_dir "fhe-audit-root" (fun root ->
    let path = Filename.concat root "irmin_store" in
    let store = run (S.open_store ~fresh:true path) in
    run (S.write store ["accounts"] "do-not-print-root");
    run (S.close store);
    let before = files path in
    let status, text = invoke binary root in
    expect "bad account root is not an empty inventory" (status = Unix.WEXITED 1);
    expect "bad account root has no completion" (not (contains text "status = complete"));
    expect "bad account root is unchanged" (files path = before))

let test_wait () =
  match Unix.fork () with
  | 0 -> Unix.sleepf 0.15; Unix._exit 23
  | pid ->
      let signals = ref 0 in
      let handler = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> incr signals)) in
      let timer = Unix.setitimer Unix.ITIMER_REAL
        {Unix.it_interval = 0.005; it_value = 0.005} in
      let status = Fun.protect
        ~finally:(fun () ->
          ignore (Unix.setitimer Unix.ITIMER_REAL timer);
          Sys.set_signal Sys.sigalrm handler)
        (fun () -> W.wait pid) in
      expect "wait received signal" (!signals > 0);
      expect "wait preserves child status" (status = Unix.WEXITED 23)

let write path value =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel value)

let invoke_args binary root args =
  let path = Filename.concat root "command.log" in
  let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o600 in
  let pid = Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.create_process binary (Array.of_list (binary :: args)) Unix.stdin fd fd) in
  let status = W.wait pid in
  status, read path

let package_source root =
  let source = Filename.concat root "source" in
  Unix.mkdir source 0o700;
  ignore (seed source ~broken:true);
  let store = run (S.open_store (Filename.concat source "irmin_store")) in
  let key = "public-pvac-bytes\000\255" in
  List.iter (fun (path, value) -> run (S.write store path value)) [
    ["stealth"; "4"], "{\"claimed\":1,\"enc_amount\":\"opaque\"}";
    ["index"; "stealth_counter"], "5";
    ["key_images"; "spent"], "\000\255spent";
    ["circles"; "circle"; "assets"; "by_hash"; "asset"; "ciphertext_b64"], "opaque-asset";
    ["contracts"; "program"; "unknown"], "\000program";
    ["unknown"; "bytes"], "\255\000unknown";
    ["pvac_hashes"; "old-envelope"], hash key;
    ["pvac_hashes"; "parts-envelope"], hash "missing-key"];
  let commit = run (S.get_commit_hash store) |> Option.get in
  let state_root = run (S.get_head_hash store) |> Option.get in
  run (S.close store);
  let head = Octra_core.Head_manifest.{schema_version = 3; generation = 42; epoch_id = 42;
    state_root; ledger_state_root = None; irmin_commit = Some commit;
    txid_hi = 10L; txlog_seg = Some 0; txlog_off = Some 16; epochlog_off = Some 16;
    commit_id = "audit-state"; ts = 100.; quorum_cert_hash = None;
    epoch_index_hash = None; epoch_index_root = None} in
  let raw = Octra_core.Head_manifest.to_json head in
  write (Octra_core.Head_manifest.path source) raw;
  let pvac = Filename.concat source "pvac" in
  Unix.mkdir pvac 0o700;
  Unix.mkdir (Filename.concat pvac "blobs") 0o700;
  write (Filename.concat pvac ("blobs/" ^ hash key ^ ".pk")) key;
  write (Filename.concat pvac "old-foreign.pk") "legacy-public-key";
  write (Filename.concat pvac "old-foreign.kat") (Octra_core.Pvac_registry.expected_kat () ^ "\n");
  write (Filename.concat pvac "parts-foreign.kat") "wrong-tag";
  write (Filename.concat source Octra_core.Pvac_migration_check.state_relative_path) "opaque-migration-state\000";
  write (Filename.concat source "wallet.json") "test-wallet-not-for-export";
  source, raw

let tree_values root =
  let store = run (S.open_store ~readonly:true (Filename.concat root "irmin_store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let tree = run (S.Store.tree store.store) in
    run (S.Store.Tree.fold ~force:`True ~cache:false ~uniq:`False
      ~contents:(fun path value rows -> Lwt.return ((path, value) :: rows)) tree [])
    |> List.sort compare)

let test_package binary =
  W.with_dir "fhe-package" (fun root ->
    let source, head = package_source root in
    let target = Filename.concat root "copy" in
    let before = files source in
    let status, output = invoke_args binary root ["--export"; source; target; hash head] in
    expect "state export failed" (status = Unix.WEXITED 0);
    expect "export scope differs" (contains output "scope = current_state_and_pvac" && contains output "crypto_validity = not_checked");
    expect "export changed source" (files source = before);
    expect "wallet was exported" (not (Sys.file_exists (Filename.concat target "wallet.json")));
    expect "export left incomplete marker" (not (Sys.file_exists (Filename.concat target "INCOMPLETE")));
    expect "HEAD bytes changed" (read (Octra_core.Head_manifest.path target) = head);
    expect "raw state changed" (tree_values source = tree_values target);
    List.iter (fun (path, _, _, _) ->
      let relative = String.sub path (String.length source + 1) (String.length path - String.length source - 1) in
      expect "external PVAC bytes changed" (read path = read (Filename.concat target relative)))
      (files (Filename.concat source "pvac"));
    expect "migration state omitted" (read (Filename.concat target Octra_core.Pvac_migration_check.state_relative_path) = "opaque-migration-state\000");
    let manifest = Filename.concat target "manifest.json" in
    let raw = read manifest in
    let value = Yojson.Safe.from_string raw in
    let open Yojson.Safe.Util in
    let report = value |> member "findings" |> member "counts" in
    expect "missing reference was not reported" (report |> member "hashed_key_missing" |> to_int = 1);
    expect "matched reference was not reported" (report |> member "hashed_key_match" |> to_int = 1);
    expect "legacy KAT absence was not reported" (report |> member "kat_absent" |> to_int > 0);
    expect "legacy key was not reported" (report |> member "legacy_key_present" |> to_int = 1);
    expect "bad KAT was not reported" (report |> member "kat_tag_mismatch" |> to_int = 1);
    expect "malformed accounts were not reported" (report |> member "account_unreadable" |> to_int = 5);
    let verify pin = invoke_args binary root ["--verify"; target; pin] in
    let saved = files target in
    expect "state verification failed" (fst (verify (hash raw)) = Unix.WEXITED 0);
    expect "verification changed package" (files target = saved);
    let refused label = expect (label ^ " accepted") (fst (verify (hash (read manifest))) = Unix.WEXITED 1) in
    expect "wrong manifest digest accepted" (fst (verify (String.make 64 '0')) = Unix.WEXITED 1);
    let alter field replacement = match value with
      | `Assoc fields -> `Assoc (List.map (fun (name, old) -> name, if name = field then replacement else old) fields)
      | _ -> assert false in
    let rows = value |> member "files" |> to_list in
    List.iter (fun (label, changed) ->
      write manifest (Yojson.Safe.to_string changed);
      refused label;
      write manifest raw) [
      "duplicate manifest path", alter "files" (`List (List.hd rows :: rows));
      "missing manifest path", alter "files" (`List (List.tl rows));
      "tree count", alter "leaves" (`Int 0);
      "account count", alter "accounts" (`Int 0);
      "logical size", alter "bytes" (`String "0");
      "findings", alter "findings" (`Assoc []);
      "epoch", alter "epoch" (`Int 43);
      "commit", alter "commit" (`String "wrong");
      "root", alter "root" (`String "wrong");
      "scope", alter "scope" (`String "whole_node");
      "duplicate field", (match value with `Assoc fields -> `Assoc (List.hd fields :: fields) | _ -> assert false)];
    let key = Filename.concat target "pvac/old-foreign.pk" in
    let key_raw = read key in
    write key (String.make (String.length key_raw) 'x');
    refused "changed key";
    write key key_raw;
    Unix.unlink key;
    refused "missing key";
    write key key_raw;
    let kat = Filename.concat target "pvac/old-foreign.kat" in
    let kat_raw = read kat in
    Unix.unlink kat;
    refused "missing KAT";
    write kat kat_raw;
    let migration = Filename.concat target Octra_core.Pvac_migration_check.state_relative_path in
    let migration_raw = read migration in
    Unix.unlink migration;
    refused "missing migration state";
    write migration migration_raw;
    let marker = Filename.concat target "INCOMPLETE" in
    write marker "interrupted";
    refused "incomplete package";
    Unix.unlink marker;
    let extra = Filename.concat target "pvac/extra.pk" in
    write extra "extra";
    refused "unlisted key";
    Unix.unlink extra;
    Unix.unlink key;
    Unix.symlink (Filename.concat source "pvac/old-foreign.pk") key;
    refused "linked key";
    Unix.unlink key;
    Unix.link (Filename.concat source "pvac/old-foreign.pk") key;
    refused "shared key";
    Unix.unlink key;
    write key key_raw;
    write (Octra_core.Head_manifest.path target) (head ^ "\n");
    refused "changed HEAD bytes";
    write (Octra_core.Head_manifest.path target) head;
    expect "restored package refused" (fst (verify (hash raw)) = Unix.WEXITED 0);
    let status, output = invoke_args binary root [target] in
    expect "typed findings were changed" (status = Unix.WEXITED 2 && contains output "needs_review");
    expect "audit modified copied raw state" (tree_values source = tree_values target))

let test_export_refusal binary =
  W.with_dir "fhe-export-refusal" (fun root ->
    let source, head = package_source root in
    let target = Filename.concat root "copy" in
    let reject label src dst pin =
      let before = files source in
      expect (label ^ " accepted") (fst (invoke_args binary root ["--export"; src; dst; pin]) = Unix.WEXITED 1);
      expect (label ^ " changed source") (files source = before) in
    reject "wrong HEAD digest" source target (String.make 64 '0');
    expect "bad digest created output" (not (Sys.file_exists target));
    reject "source as output" source source (hash head);
    reject "output inside source" source (Filename.concat source "copy") (hash head);
    Unix.mkdir target 0o700;
    write (Filename.concat target "INCOMPLETE") "preserve failed output";
    reject "existing output" source target (hash head);
    expect "existing output removed" (read (Filename.concat target "INCOMPLETE") = "preserve failed output");
    let link = Filename.concat root "linked" in
    Unix.symlink source link;
    reject "linked source" link (Filename.concat root "new") (hash head);
    let unknown = Filename.concat source "pvac/private.json" in
    write unknown "test-not-public";
    reject "unsupported PVAC file" source (Filename.concat root "unknown") (hash head);
    Unix.unlink unknown;
    let parsed = Octra_core.Head_manifest.of_json head in
    let changed = Octra_core.Head_manifest.to_json {parsed with epoch_id = 43} in
    write (Octra_core.Head_manifest.path source) changed;
    reject "wrong state epoch" source (Filename.concat root "epoch") (hash changed);
    write (Octra_core.Head_manifest.path source) head;
    W.remove (Filename.concat source "pvac");
    let absent = Filename.concat root "no-pvac" in
    expect "raw state with absent keys refused" (fst (invoke_args binary root ["--export"; source; absent; hash head]) = Unix.WEXITED 0);
    let open Yojson.Safe.Util in
    let report = Yojson.Safe.from_file (Filename.concat absent "manifest.json") |> member "findings" in
    expect "absent keys hidden" (report |> member "counts" |> member "hashed_key_missing" |> to_int = 2);
    expect "key validity claimed" (report |> member "crypto_validity" |> to_string = "not_checked"))

let test_head_change binary =
  W.with_dir "fhe-head-change" (fun root ->
    let source, head = package_source root in
    let channel = open_out_bin (Filename.concat source "pvac/large.pk") in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      let block = String.make (1024 * 1024) 'p' in
      for _index = 1 to 64 do output_string channel block done);
    let target = Filename.concat root "copy" in
    let output = Filename.concat root "race.log" in
    let fd = Unix.openfile output [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o600 in
    let pid = Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      Unix.create_process binary [|binary; "--export"; source; target; hash head|] Unix.stdin fd fd) in
    let finished = ref false in
    Fun.protect ~finally:(fun () -> if not !finished then begin
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
      ignore (W.wait pid)
    end) (fun () ->
      let deadline = Unix.gettimeofday () +. 30. in
      let rec await () =
        if Sys.file_exists (Filename.concat target "INCOMPLETE") then ()
        else begin
          expect "export did not reach copy phase" (Unix.gettimeofday () < deadline);
          Unix.sleepf 0.0005;
          await ()
        end in
      await ();
      Unix.kill pid Sys.sigstop;
      write (Octra_core.Head_manifest.path source) (head ^ "\n");
      Unix.kill pid Sys.sigcont;
      let status = W.wait pid in
      finished := true;
      expect "changed source HEAD accepted" (status = Unix.WEXITED 1);
      expect "failed export was removed or completed" (Sys.file_exists (Filename.concat target "INCOMPLETE"));
      let text = read output in
      expect "HEAD refusal came from another failure" (contains text "source HEAD changed" || contains text "HEAD digest differs");
      expect "changed HEAD was overwritten" (read (Octra_core.Head_manifest.path source) = head ^ "\n")))

let () =
  test_wait ();
  let binary = W.absolute Sys.argv.(1) in
  test_scan binary false;
  test_scan binary true;
  test_absent binary;
  test_readable binary;
  test_root binary;
  test_package binary;
  test_export_refusal binary;
  test_head_change binary;
  print_endline "status = pass test = fhe_audit"