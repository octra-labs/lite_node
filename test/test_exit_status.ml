(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Admission = Octra_core.Validator_admission
module Policy = Octra_core.Validator_policy
module Status = Octra_node_runtime.Status_rpc

let expect label value =
  if not value then failwith ("test_exit_status: " ^ label)

let candidate = Admission.{
  address = "octA";
  pubkey = String.make 32 'k';
  bond = Z.of_int 1_000_000;
  bonded_epoch = 1L;
  ready_epoch = Some 2L;
  exit_epoch = Some 10L;
}

let read ?(head = 10) ?(chain_id = "octra-devnet-9871-cluster") value =
  Status.validator_enrollment ~chain_id ~head_epoch:head ~address:candidate.address
    ~pubkey:(Base64.encode_exn candidate.pubkey) value

let field name = function
  | Ok (`Assoc fields) -> List.assoc name fields
  | _ -> failwith "enrollment response refused"

let check_membership () =
  let module Members = Octra_node_runtime.Enroll_members in
  let module Update = Octra_core.Validator_set_update in
  let member byte =
    let pubkey = String.make 32 byte in
    let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
    Admission.{ address; pubkey; weight = Z.one }
  in
  let first = member 'a' in
  let second = member 'b' in
  let update epoch members =
    Update.make_weighted ~source_epoch:1L ~activate_epoch:epoch members
    |> Result.get_ok |> Update.to_string |> Option.some
  in
  let active = update 2L [first] in
  let pending = update 10L [second] in
  let read ?(pubkey = Base64.encode_exn first.pubkey) head sets =
    Members.of_values ~head_epoch:head ~address:first.address ~pubkey sets
  in
  let before = read 9L (active, pending) |> Result.get_ok |> Option.get in
  expect "membership uses committed epoch"
    (before.epoch = 9L && before.active && not before.scheduled
     && before.next_set_epoch = Some 10L && before.activate_epoch = None);
  let after = read 10L (active, pending) |> Result.get_ok |> Option.get in
  expect "activation follows the snapshot head"
    (not after.active && not after.scheduled && after.next_set_epoch = None);
  let queued = read 9L (update 2L [second], update 10L [first])
    |> Result.get_ok |> Option.get in
  expect "future admission is not active membership"
    (not queued.active && queued.scheduled && queued.activate_epoch = Some 10L);
  expect "missing committed set stays unknown" (read 9L (None, pending) = Ok None);
  expect "invalid set is refused" (Result.is_error (read 9L (Some "invalid", pending)));
  expect "future active set is refused" (Result.is_error (read 1L (active, None)));
  expect "changed consensus key is refused"
    (Result.is_error (read ~pubkey:(Base64.encode_exn second.pubkey) 9L (active, None)));
  expect "conflicting slots are refused"
    (Result.is_error (read 10L (active, update 2L [second])));
  expect "older pending slot cannot replace active"
    ((read 11L (update 10L [first], update 2L [second])
      |> Result.get_ok |> Option.get).active)

let check_snapshot () =
  let module S = Octra_core.Store_irmin in
  let module R = Octra_node_runtime.Status_read_rpc in
  let module Registry = Octra_core.Validator_registry in
  let module Update = Octra_core.Validator_set_update in
  let module Head = Octra_core.Head_manifest in
  let pubkey = Base64.encode_exn candidate.pubkey in
  let address = Octra_core.Crypto.Address.address_from_pubkey pubkey in
  let candidate = { candidate with address; exit_epoch = None } in
  let registry = Registry.of_yojson (`Assoc [
    "standard", `String Policy.standard_name;
    "candidates", `List [`Assoc [
      "address", `String address;
      "consensus_pubkey", `String pubkey;
      "bond", `String (Z.to_string candidate.bond);
      "bonded_epoch", `String "1";
      "ready_epoch", `String "2";
      "exit_epoch", `Null;
    ]];
  ]) |> Result.get_ok in
  let update = Update.make_weighted ~source_epoch:1L ~activate_epoch:2L
    [Admission.{ address; pubkey = candidate.pubkey; weight = Z.one }]
    |> Result.get_ok |> Update.to_string in
  (try Unix.mkdir "runtime_data" 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path = Filename.concat "runtime_data" ("exit_view_" ^ string_of_int (Unix.getpid ())) in
  Lwt_main.run (let open Lwt.Syntax in
    let* store = S.open_store path in
    Lwt.finalize (fun () ->
      let* () = S.begin_epoch_batch store in
      let* () = S.set_meta store "last_epoch" "9" in
      let* () = S.set_meta store Registry.meta_key (Registry.to_string registry) in
      let* () = S.set_meta store Update.active_meta_key update in
      let* () = S.commit_epoch_batch store "enrollment sample" in
      let* () = S.tag_epoch store 9 in
      let* binding = S.epoch_binding store 9 in
      let binding = Result.get_ok binding in
      let head = Head.{
        schema_version; generation = 9; epoch_id = 9;
        state_root = String.make 64 'a'; ledger_state_root = Some binding.root;
        irmin_commit = Some binding.commit; txid_hi = 0L;
        txlog_seg = None; txlog_off = None; epochlog_off = None;
        commit_id = "sample"; ts = 0.; quorum_cert_hash = None;
        epoch_index_hash = None; epoch_index_root = None;
      } in
      let* () = S.set_meta store Registry.meta_key (Registry.to_string Registry.empty) in
      let* () = S.set_meta store Update.active_meta_key "invalid live set" in
      let read head = R.load_validator_enrollment ~store ~head:(Some head)
        ~validator_address:address ~chain_id:"octra-test" ~config_hash:"config"
        ~automatic:false in
      let* snapshot = read head in
      let snapshot = Result.get_ok snapshot in
      expect "registry comes from pinned epoch" (snapshot.candidate = Some candidate);
      expect "membership comes from the same epoch" (snapshot.sets = (Some update, None));
      let* response = R.validator_enrollment ~snapshot:(Ok snapshot)
        ~validator_address:address ~validator_pubkey:pubkey in
      let member = field "membership" response in
      expect "snapshot reports committed membership"
        (Yojson.Safe.Util.member "active" member = `Bool true
         && Yojson.Safe.Util.member "epoch" member = `String "9");
      let* missing = read { head with epoch_id = 10 } in
      expect "missing epoch is refused" (Result.is_error missing);
      let* wrong = read { head with ledger_state_root = Some "wrong" } in
      expect "different root is refused" (Result.is_error wrong);
      Lwt.return_unit
    ) (fun () -> S.close store))

let check_transition () =
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Option.get (Octra_core.Rule_graph.exit_activation_for_chain chain_id) in
  let epoch = plan.activation_epoch in
  let exited = Int64.sub (Int64.of_int epoch) 9_000L in
  let candidate = { candidate with exit_epoch = Some exited } in
  expect "prior withdrawal epoch retained"
    (field "withdraw_epoch" (read ~head:(epoch - 1) (Some candidate))
     = `String (Int64.to_string (Int64.add exited Policy.unbonding_epochs)));
  List.iter (fun head ->
    expect "existing exit matures at activation"
      (field "withdraw_epoch" (read ~head (Some candidate)) = `String (string_of_int epoch));
    expect "existing exit has no remaining wait"
      (field "withdraw_remaining_epochs" (read ~head (Some candidate)) = `String "0"))
    [epoch; epoch + 1];
  let candidate = { candidate with exit_epoch = Some (Int64.of_int epoch) } in
  expect "new exit retains complete shorter wait"
    (field "withdraw_epoch" (read ~head:epoch (Some candidate))
     = `String (Int64.to_string (Int64.add (Int64.of_int epoch) Policy.exit_wait)));
  List.iter (fun chain_id ->
    expect "other chain keeps original wait"
      (field "withdraw_epoch" (read ~chain_id ~head:epoch (Some candidate))
       = `String (Int64.to_string (Int64.add (Int64.of_int epoch) Policy.unbonding_epochs))))
    ["octra-mainnet"; "other"]

let () =
  check_transition ();
  check_snapshot ();
  check_membership ();
  let epoch = Admission.withdraw_epoch Policy.parameters candidate |> Result.get_ok in
  expect "RPC uses protocol withdrawal epoch"
    (field "withdraw_epoch" (read (Some candidate)) = `String (Int64.to_string epoch));
  List.iter (fun (head, remaining) ->
    expect "remaining epochs use the same committed head"
      (field "withdraw_remaining_epochs" (read ~head (Some candidate))
       = `String (string_of_int remaining)))
    [Int64.to_int epoch - 1, 1; Int64.to_int epoch, 0; Int64.to_int epoch + 1, 0];
  expect "negative head is refused" (Result.is_error (read ~head:(-1) (Some candidate)));
  expect "absent enrollment has no withdrawal epoch"
    (field "withdraw_epoch" (read None) = `Null);
  expect "ready enrollment needs exit first"
    (field "withdraw_epoch" (read (Some { candidate with exit_epoch = None })) = `Null);
  expect "overflow cannot advertise withdrawal"
    (Result.is_error (read (Some { candidate with exit_epoch = Some Int64.max_int })));
  Printf.printf "status = pass test = exit_status\n%!"