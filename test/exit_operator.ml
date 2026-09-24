(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Exit_case

module Service = Exit_service
module Staging = Octra_core.Tx_staging

let file_hash path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    let buffer = Bytes.create 65_536 in
    let rec read hash =
      match input channel buffer 0 (Bytes.length buffer) with
      | 0 -> Digestif.SHA256.(to_hex (get hash))
      | length -> read (Digestif.SHA256.feed_bytes hash ~off:0 ~len:length buffer)
    in
    read Digestif.SHA256.empty)

let write_json path value =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
  let output = Unix.out_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output (Yojson.Safe.to_string value))

let tools () =
  let exported = Test_workspace.source "controls/lib" in
  if Sys.file_exists (Filename.concat exported "validator_enroll.py") then exported
  else Test_workspace.source "docs/release/validator_tools"

let invoke ?(renew = false) ?pointer config operation expected =
  let open Lwt.Syntax in
  let args = [|"python3"; Test_workspace.source "test/exit_client.py";
    tools (); config; operation; expected|] in
  let args = if renew then Array.append args [|"renew"|] else args in
  let args = match pointer with None -> args | Some path -> Array.append args [|path|] in
  Lwt_process.with_process_full ~timeout:20. ("", args) (fun process ->
    let* output, errors = Lwt.both
      (Lwt_io.read process#stdout) (Lwt_io.read process#stderr) in
    let* status = process#status in
    expect ("operator process: " ^ output ^ errors) (status = Unix.WEXITED 0);
    Lwt.return_unit)

let configure data_dir port =
  let binary = Test_workspace.absolute
    (Filename.concat (Filename.dirname Sys.executable_name) "../bin/bft_control_tx.exe") in
  let config = Filename.concat data_dir "node.env" in
  let values = [
    "OCTRA_CHAIN_ID", chain_id;
    "OCTRA_API_PORT", string_of_int port;
    "OCTRA_OPERATOR_RPC_URL", Printf.sprintf "http://127.0.0.1:%d/rpc" port;
    "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH", "1266000";
    "OCTRA_DATA_DIR", data_dir;
    "OCTRA_OPERATOR_CONTROL_BINARY", binary;
    "OCTRA_OPERATOR_CONTROL_BINARY_HASH", file_hash binary;
  ] in
  let fd = Unix.openfile config [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
  let output = Unix.out_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () ->
    List.iter (fun (key, value) ->
      Printf.fprintf output "%s=%s\n" key (Filename.quote value)) values);
  config

let resume ?(renew = false) config operation clear_queue (node : Service.t) =
  let open Lwt.Syntax in
  let tx = List.hd node.submitted in
  let pending = Staging.find_by_hash (T.hash tx) in
  expect "lost response queue matches delivery"
    (Option.is_some pending = (node.loss = Service.Queued));
  let* () = Service.stop_duty node in
  let prior = Filename.concat node.data_dir ("saved-" ^ string_of_int node.head) in
  Unix.mkdir prior 0o700;
  let control = Filename.concat node.data_dir "validator-control" in
  let source = Filename.concat prior "validator-control" in
  Unix.rename control source;
  let* () = invoke config "restore" prior in
  Sys.readdir source |> Array.iter (fun name ->
    if Filename.check_suffix name ".json" then
      expect "recovery preserves signed control bytes"
        (file_hash (Filename.concat source name) = file_hash (Filename.concat control name)));
  let pointer = Filename.concat control "last.json" in
  let saved = file_hash pointer in
  Unix.unlink pointer;
  let* () = invoke ~pointer:(Filename.concat source "last.json") config "repair-pointer" "pass" in
  expect "pointer repair preserves control identity" (file_hash pointer = saved);
  Sys.readdir source |> Array.iter (fun name ->
    if Filename.check_suffix name ".json" then
      expect "pointer repair preserves signed bytes"
        (file_hash (Filename.concat source name) = file_hash (Filename.concat control name)));
  if clear_queue then Staging.clear ();
  let* () = Service.restart node in
  let submissions = List.length node.submitted in
  let* () = invoke ~renew config operation "pass" in
  let* () = match Staging.find_by_hash (T.hash tx) with
    | None -> Lwt.return_unit
    | Some queued ->
      expect "pending receipt prevents another POST"
        (List.length node.submitted = submissions);
      Service.accept node queued
  in
  Lwt.return_unit

let check_exit config ordinary clear_queue (node : Service.t) =
  let open Lwt.Syntax in
  Service.wake node;
  let* () = invoke config "validator_exit" "account has pending transactions" in
  expect "ordinary queue kept" (Staging.find_by_hash (T.hash ordinary) <> None);
  expect "pending transfer prevents preparation" (node.submitted = []);
  expect "exit intent acknowledged"
    (Result.get_ok (Service.Control.status node.control node.identity) <> None);
  let* () = Service.apply node (node.head + 1) [ordinary] in
  let* () = invoke config "validator_exit" "exit submission unresolved" in
  let* () = resume config "validator_exit" clear_queue node in
  expect "committed exit exists" ((Option.get (Service.bond_entry node.view)).exit_epoch <> None);
  Lwt.return_unit

let check_withdraw config maturity clear_queue (node : Service.t) =
  let open Lwt.Syntax in
  let* () = invoke config "validator_withdraw" "unbonding period incomplete" in
  let entry = Option.get (Service.bond_entry node.view) in
  let before = List.length node.submitted in
  let outbox = Filename.concat node.data_dir
    (Printf.sprintf "validator-control/validator_withdraw-%Ld.json" entry.bonded_epoch) in
  let locked epoch =
    let pending = P.withdraw_epoch ~chain_id ~epoch entry
      |> get "operator deadline" |> Int64.to_int in
    let remaining = pending - epoch in
    expect "withdrawal still locked" (remaining > 0);
    let* () = Service.apply node epoch [] in
    let* () = invoke config "validator_withdraw"
      (Printf.sprintf "remaining_epochs = %d" remaining) in
    expect "early withdrawal never posted" (List.length node.submitted = before);
    expect "early withdrawal has no outbox" (not (Sys.file_exists outbox));
    Lwt.return_unit
  in
  let gate = (Option.get (G.exit_activation rules)).activation_epoch in
  let epochs = [gate - 1; gate; maturity - 1]
    |> List.filter (fun epoch -> node.head < epoch && epoch < maturity)
    |> List.sort_uniq Int.compare in
  let* () = Lwt_list.iter_s locked epochs in
  let* () = Service.restart node in
  let* () = Service.apply node maturity [] in
  let* () = invoke config "validator_withdraw" "exit submission unresolved" in
  let* () = resume config "validator_withdraw" clear_queue node in
  expect "operator bond removed" (Service.bond_entry node.view = None);
  expect "operator escrow empty" (Z.equal node.view.escrow Z.zero);
  expect "duty paused through operator restarts" (node.sent = 0);
  expect "one exit and one withdrawal committed" (Hashtbl.length node.confirmed = 2);
  let fees = Hashtbl.fold (fun _ (tx, _) total -> Z.add total tx.T.ou) node.confirmed
    (Z.mul fee (Z.of_int 2)) in
  expect "operator balance conserves bond" Z.(equal node.view.account.balance (sub balance fees));
  expect "operator nonce counted once" (node.view.account.nonce = 4);
  let paid = node.view in
  let pointer = Filename.concat node.data_dir "validator-control/last.json" in
  let backup = Filename.concat node.data_dir "last.backup" in
  write_json backup (Yojson.Safe.from_file pointer);
  Unix.unlink pointer;
  let* () = invoke config "validator_withdraw" "repair-pointer" in
  let* () = invoke ~pointer:backup config "repair-pointer" "pass" in
  let* () = invoke config "validator_withdraw" "pass" in
  expect "repeated operator withdraw cannot pay" (node.view = paid);
  let expected = match node.loss, clear_queue with
    | Service.Committed, _ | Service.Queued, false -> 2
    | Service.Before, _ | Service.Queued, true -> 4
  in
  expect "only unresolved uncommitted bytes resubmitted" (List.length node.submitted = expected);
  Lwt.return_unit

let check_case ?(epoch = first_epoch) ?release ?(snapshot = false) after loss clear_queue =
  Test_workspace.with_dir "exit_operator" (fun directory ->
  let data_dir = Filename.concat directory ".keys" in
  Unix.mkdir data_dir 0o700;
  write_json (Filename.concat data_dir "wallet.json") (`Assoc [
    "address", `String owner.address;
    "pub", `String (Base64.encode_exn owner.public);
    "priv", `String (Base64.encode_exn owner.secret);
  ]);
  let database = Filename.concat directory "irmin" in
  seed database;
  ignore (run database [step epoch [bond ~epoch ()]]);
  Staging.clear ();
  let ordinary = transaction ~epoch:(epoch + 1) ~nonce:2 ~amount:Z.one T.Standard in
  ignore (Staging.add_smart ~lookup:(fun _ -> Some (balance, 1)) ordinary
    |> get "ordinary staging");
  Lwt_main.run (let open Lwt.Syntax in
    let* node = Service.create ~data_dir ~database ~loss ~epoch in
    Lwt.finalize (fun () -> Service.serve node (fun port ->
      let config = configure data_dir port in
      let* () = check_exit config ordinary clear_queue node in
      let* () = if snapshot then Exit_snapshot.restore node else Lwt.return_unit in
      let maturity = match release with
        | Some epoch -> epoch
        | None -> P.withdraw_epoch ~chain_id ~epoch:node.head
          (Option.get (Service.bond_entry node.view))
          |> get "operator maturity" |> Int64.to_int
      in
      let* () = check_withdraw config maturity clear_queue node in
      let* () = after config node in
      let label = match loss with
        | Service.Before -> "before"
        | Service.Queued -> "queued"
        | Service.Committed -> "committed"
      in
      Printf.printf "status = pass test = exit_operator loss = %s queue_cleared = %b epoch = %d release = %d\n%!"
        label clear_queue epoch maturity;
      Lwt.return_unit))
      (fun () -> Service.close node)))

let check_rebond config (node : Service.t) =
  let open Lwt.Syntax in
  let epoch = node.head + 1 in
  let* () = Service.apply node epoch [bond ~epoch ~nonce:5 ()] in
  let vote proposal =
    let value = C.{
      chain_id; epoch_id = Int64.of_int epoch; round = 0;
      vote_type = Precommit; proposal_id = String.make 32 proposal;
      validator = owner.address; signature = String.make 64 '\000';
    } in
    { value with signature =
      H.sign_ed25519 ~priv_raw:owner.secret ~msg:(H.vote_sign_bytes value) }
  in
  let proof = Octra_consensus.C_evidence.vote_conflict (vote 'a') (vote 'b') |> Option.get in
  let tx = transaction ~epoch:(epoch + 1) ~nonce:6
    ~message:(Octra_core.Validator_evidence.message proof) T.ValidatorEvidence in
  let* () = Service.apply node (epoch + 1) [tx] in
  expect "new bond removed by slash" (Service.bond_entry node.view = None);
  let paid = node.view in
  let* () = Service.restart node in
  let* () = invoke config "validator_withdraw" "account nonce advanced" in
  expect "old receipt cannot pay new bond" (node.view = paid);
  Lwt.return_unit

let check_bond ?(renew = false) clear_queue config (node : Service.t) =
  let open Lwt.Syntax in
  let before = node.view in
  let path = Filename.concat node.data_dir "enrollment.json" in
  let fields = Yojson.Safe.from_file path |> Yojson.Safe.Util.to_assoc in
  let records = List.assoc "transactions" fields |> Yojson.Safe.Util.to_assoc in
  let entry = `Assoc ["tx_hash", `String (String.make 64 'a')] in
  Unix.rename path (path ^ ".prior");
  write_json path (`Assoc (("transactions", `Assoc (("bond", entry) :: records))
    :: List.remove_assoc "transactions" fields));
  Service.History.close node.history;
  let history = node.database ^ ".history" in
  Unix.rename history (history ^ ".prior");
  node.history <- Service.History.open_chaindata history;
  List.iter (fun tx -> expect "old receipts absent after history reset"
    (Service.History.get_tx_by_hash node.history (T.hash tx) = None)) node.submitted;
  let* () = invoke config "validator_bond" "bond submission unresolved" in
  let* () = if not renew then Lwt.return_unit else begin
    expect "rejected test retains an unspent bond" (node.view = before);
    let tx = List.hd node.submitted in
    Service.History.begin_batch node.history;
    Service.History.save_rejected node.history ~hash:(T.hash tx)
      ~from_addr:tx.from ~to_addr:tx.to_ ~amount:(Z.to_string tx.amount)
      ~nonce:tx.nonce ~error_type:"timestamp" ~reason:"expired timestamp"
      ~epoch_id:node.head ~ts:(Unix.gettimeofday ());
    Service.History.commit_batch node.history;
    let* () = Service.apply node (node.head + 1) [] in
    invoke config "validator_bond" "--renew"
  end in
  let* () = resume ~renew config "validator_bond" clear_queue node in
  if renew then begin
    let attempts = List.filter (fun tx -> tx.T.op_type = T.ValidatorBond) node.submitted in
    expect "renewed bond submitted exactly twice" (List.length attempts = 2);
    let fresh = List.hd attempts and old = List.nth attempts 1 in
    expect "renewal changes signature" (T.hash fresh <> T.hash old);
    expect "renewal retains full payment"
      ({ fresh with T.timestamp = old.timestamp; signature = old.signature } = old)
  end;
  let entry = Option.get (Service.bond_entry node.view) in
  expect "rebond amount applied once" Z.(equal entry.bond P.min_bond);
  expect "rebond consumes one nonce" (node.view.account.nonce = before.account.nonce + 1);
  let tx = List.find (fun tx -> tx.T.op_type = T.ValidatorBond) node.submitted in
  expect "rebond debit applied once"
    Z.(equal node.view.account.balance (sub before.account.balance (add P.min_bond tx.ou)));
  let submissions = List.length node.submitted in
  let* () = invoke config "validator_bond" "pass" in
  expect "confirmed rebond sends nothing" (List.length node.submitted = submissions);
  Lwt.return_unit

let check_cleanup () =
  let check fail =
    let created = ref "" in
    let result = try
      Test_workspace.with_dir "exit_cleanup" (fun directory ->
        created := directory;
        let child = Filename.concat directory ".keys" in
        Unix.mkdir child 0o700;
        write_json (Filename.concat child "wallet.json") (`Assoc []);
        Unix.symlink child (Filename.concat directory "link");
        if fail then raise Exit);
      true
    with Exit -> false in
    expect "cleanup preserves result" (result = not fail);
    expect "test directory removed" (not (Sys.file_exists !created))
  in
  check false;
  check true

let check () =
  check_cleanup ();
  check_case (check_bond ~renew:true true) Service.Before true;
  let gate = (Option.get (G.exit_activation rules)).activation_epoch in
  List.iter (fun (loss, clear_queue) ->
    check_case ~epoch:(gate - 9_001) ~release:gate ~snapshot:true
      (check_bond clear_queue) loss clear_queue;
    check_case (check_bond clear_queue) loss clear_queue;
    check_case check_rebond loss clear_queue;
    check_case ~epoch:(gate - 9_001) ~release:gate check_rebond loss clear_queue;
    check_case ~epoch:(gate - 3) ~release:(gate + 8_191) check_rebond loss clear_queue
  ) [
    Service.Before, true;
    Service.Queued, false;
    Service.Queued, true;
    Service.Committed, true;
  ]