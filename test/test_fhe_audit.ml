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
  let _, status = Unix.waitpid [] pid in
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

let () =
  let binary = W.absolute Sys.argv.(1) in
  test_scan binary false;
  test_scan binary true;
  test_absent binary;
  test_readable binary;
  test_root binary;
  print_endline "status = pass test = fhe_audit"