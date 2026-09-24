(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Store_irmin
module L = Octra_core.Ledger
module T = Octra_core.Ledger_types
module P = Octra_core.Account_pack
module W = Test_workspace

let run = Lwt_main.run
let expect label value = if not value then failwith label

let account cipher = T.{
  balance = Z.of_int 19;
  nonce = 7;
  public_key = Some "retained-key";
  encrypted_balance = cipher;
  decrypt_allowance = Z.of_int 3;
}

let snapshot store =
  match run (S.capture_read_snapshot store) with
  | Ok value -> value.state_root, value.commit_hash
  | Error reason -> failwith reason

let with_disk root ?(readonly = false) f =
  let store = run (S.open_store ~readonly (Filename.concat root "irmin_store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () -> f store)

let with_store name f =
  W.with_dir name (fun root -> with_disk root f)

let seed store layout values =
  let mode = if layout = "parts" then Octra_core.Rule_graph.Active
    else Octra_core.Rule_graph.Prior in
  run (S.begin_epoch_batch ~mode store);
  List.iter (fun (address, value) ->
    if layout = "early" then
      let raw = P.old value |> Yojson.Safe.from_string in
      let raw = match raw with
        | `Assoc fields -> `Assoc (List.remove_assoc "decrypt_allowance" fields)
        | _ -> assert false
      in
      run (S.write store (S.account_data_path address) (Yojson.Safe.to_string raw))
    else run (S.set_account store address value)
  ) values;
  run (S.write store ["meta"; "last_epoch"] "42");
  run (S.commit_epoch_batch store "cipher inputs")

let kept = [
  None; Some ""; Some "hfhe_v1"; Some "hfhe_v1|";
  Some "hfhe_v10|x"; Some "hfhe_v1|!!!";
  Some ("hfhe_v1|" ^ Base64.encode_exn (String.make 140_000 'x'));
]

let unknown = [
  "v2|legacy"; String.make 140_000 'z'; " hfhe_v1|payload";
  "\000\255cipher"; "0 "; "00"; "HFHE_v1|payload";
]

let test_formats () =
  List.iter (fun cipher ->
    expect "prior accepted format loads" (L.can_load_cipher cipher)
  ) (Some "0" :: kept);
  List.iter (fun cipher ->
    expect "unknown format refuses" (not (L.can_load_cipher (Some cipher)))
  ) unknown

let test_kept layout create =
  with_store "cipher-kept" (fun store ->
    let values = List.mapi (fun index cipher ->
      string_of_int index, account cipher) kept in
    seed store layout values;
    let before = snapshot store in
    let accounts = run (S.load_all_accounts store) in
    let ledger = create store in
    expect "all accounts loaded" (L.length ledger = List.length values);
    expect "public supply unchanged"
      (L.get_total_supply ledger = Z.of_int (19 * List.length values));
    expect "active count unchanged" (L.active_count ledger = List.length values);
    List.iter (fun (address, value) ->
      expect "all account fields retained" (L.find ledger address = value)
    ) accounts;
    expect "accepted load is clean" (Result.is_ok (L.freeze ledger));
    run (L.flush_dirty_lwt ledger);
    expect "accepted flush keeps root and commit" (snapshot store = before);
    let again = create store in
    List.iter (fun (address, value) ->
      expect "repeated load retains bytes" (L.find again address = value)
    ) accounts;
    run (L.flush_dirty_lwt again);
    expect "repeated flush keeps root and commit" (snapshot store = before))

let test_zero layout create =
  with_store "cipher-zero" (fun store ->
    seed store layout ["zero", account (Some "0"); "other", account (Some "")];
    let prior = Option.get (run (S.get_account store "zero")) in
    let other = run (S.read_tree store ["accounts"; "other"]) in
    let ledger = create store in
    let expected = { prior with T.encrypted_balance = None } in
    expect "explicit zero keeps prior normalization" (L.find ledger "zero" = expected);
    expect "zero normalization requires flush" (Result.is_error (L.freeze ledger));
    run (L.flush_dirty_lwt ledger);
    expect "zero writes prior encoding"
      (run (S.read store (S.account_data_path "zero")) = Some (P.old expected));
    expect "zero removes only its cipher tree"
      (run (S.read_tree store (S.account_cipher_path "zero")) = None);
    expect "zero does not rewrite another account"
      (Option.map S.Store.Tree.hash (run (S.read_tree store ["accounts"; "other"]))
       = Option.map S.Store.Tree.hash other);
    let after = snapshot store in
    let again = create store in
    expect "zero restart is clean" (Result.is_ok (L.freeze again));
    run (L.flush_dirty_lwt again);
    expect "zero restart keeps root and commit" (snapshot store = after))

let test_refused layout create reverse =
  with_store "cipher-refused" (fun store ->
    let values = [
      "a", account None;
      "b", account (Some "0");
    ] @ List.mapi (fun index cipher ->
      "unknown-" ^ string_of_int index, account (Some cipher)
    ) unknown in
    seed store layout (if reverse then List.rev values else values);
    let before = snapshot store in
    let accounts = run (S.load_all_accounts store) in
    let refused () =
      try
        let ledger = create store in
        run (L.flush_dirty_lwt ledger);
        false
      with Failure reason ->
        reason = "ledger cipher format unsupported; run fhe_audit on a data copy"
    in
    expect "unknown cipher stops loading" (refused ());
    expect "refusal preserves root and commit" (snapshot store = before);
    expect "refusal preserves all account fields" (run (S.load_all_accounts store) = accounts);
    expect "repeated load refuses" (refused ());
    expect "repeated refusal preserves store" (snapshot store = before))

let test_runtime () =
  with_store "cipher-runtime" (fun store ->
    seed store "parts" ["account", account None];
    let ledger = L.create store in
    expect "runtime setter unchanged"
      (L.update_enc_balance ledger "account" "v2|legacy" = Ok ());
    run (L.flush_dirty_lwt ledger);
    let frozen = match L.freeze ledger with
      | Ok value -> value
      | Error reason -> failwith reason
    in
    let restored = L.thaw store frozen in
    expect "cache restore is not cipher normalization"
      ((L.find restored "account").encrypted_balance = Some "v2|legacy");
    let before = snapshot store in
    run (L.flush_dirty_lwt restored);
    expect "cache restore does not erase" (snapshot store = before))

let test_disk_refused layout create cipher =
  W.with_dir "cipher-disk-refused" (fun root ->
    let before, accounts = with_disk root (fun store ->
      seed store layout [
        "unknown", account (Some cipher);
        "zero", account (Some "0");
        "plain", account None;
      ];
      snapshot store, run (S.load_all_accounts store)) in
    List.iter (fun readonly ->
      with_disk root ~readonly (fun store ->
        let refused = try ignore (create store); false with
          | Failure reason ->
            reason = "ledger cipher format unsupported; run fhe_audit on a data copy"
        in
        expect "reopened unknown format refuses" refused;
        expect "reopened refusal retains accounts"
          (run (S.load_all_accounts store) = accounts);
        expect "reopened refusal retains root and commit" (snapshot store = before))
    ) [false; true; false])

let test_disk_kept layout create =
  W.with_dir "cipher-disk-kept" (fun root ->
    let before, accounts = with_disk root (fun store ->
      seed store layout (List.mapi (fun index cipher ->
        string_of_int index, account cipher) (Some "0" :: kept));
      let ledger = create store in
      run (L.flush_dirty_lwt ledger);
      snapshot store, run (S.load_all_accounts store)) in
    List.iter (fun readonly ->
      with_disk root ~readonly (fun store ->
        let ledger = create store in
        expect "reopened known formats are clean" (Result.is_ok (L.freeze ledger));
        List.iter (fun (address, value) ->
          expect "reopened known formats retain fields" (L.find ledger address = value)
        ) accounts;
        run (L.flush_dirty_lwt ledger);
        expect "reopened load retains root and commit" (snapshot store = before))
    ) [false; true; false])

let () =
  test_formats ();
  List.iter (fun layout ->
    List.iter (fun create ->
      test_refused layout create false;
      test_refused layout create true;
      test_kept layout create;
      test_zero layout create;
      List.iter (test_disk_refused layout create) unknown;
      test_disk_kept layout create
    ) [L.create; L.create_from_store]
  ) ["early"; "old"; "parts"];
  test_runtime ();
  print_endline "status = pass test = cipher_load"