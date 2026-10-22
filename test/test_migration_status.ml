(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module A = Octra_core.Pvac_migration_check
module M = Octra_core.Pvac_migration
module R = Octra_core.Pvac_legacy_public_replay
module V = Octra_node_runtime.Rpc_view

let check name value =
  if not value then failwith ("migration_status: " ^ name)

let unwrap = function
  | Ok value -> value
  | Error reason -> failwith reason

let field name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> failwith "migration_status: object required"

let address =
  let key = String.make 32 '\042' |> Base64.encode_exn in
  Octra_core.Crypto.Address.address_from_pubkey key

let chain_id = "migration-status-test"
let cipher = "encrypted history"
let clean = R.{
  audit_class = Public_clean;
  can_public_migrate = true;
  public_net = Some (Z.of_int 17);
  commitment_net = None;
  blockers = [];
  effects = [];
  reason = "legacy history is public";
}

let entry verdict = A.{
  address;
  source_cipher_hash = source_cipher_hash cipher;
  total = 3;
  decision = verdict;
}

let records entries =
  A.create ~chain_id ~snapshot_epoch:10 ~state_root:(String.make 64 'a')
    ~activation_epoch:20 entries |> unwrap

let view ?(epoch = 20) ?(cipher = cipher) ?(mode = Octra_core.Rule_graph.Active)
    ?(math = true)
    ?(cipher_mode = Octra_core.Rule_graph.Prior)
    ?(proof_mode = Octra_core.Rule_graph.Active)
    ?(status = M.status_of_classes M.V3 M.Historical) checks =
  V.pvac_migration_status ~keyed:false ~addr:address ~cipher ~epoch
    ~math ~owner_migration_mode:mode ~cipher_mode ~proof_mode status checks

let check_missing name reason response =
  let replay = field "legacy_public_replay" response in
  check "missing history is not poisoned"
    (field "audit_class" replay = `String "unavailable");
  check "missing admission status" (field "admission_status" replay = `String name);
  check "missing reason" (field "reason" replay = `String reason);
  check "incomplete history" (field "complete" replay = `Bool false);
  check "no public migration" (field "can_public_migrate" replay = `Bool false);
  check "no owner migration" (field "can_owner_proof_migrate" response = `Bool false);
  check "no public amount" (field "public_net" replay = `Null);
  check "no commitment" (field "commitment_net" replay = `Null)

let check_errors () =
  let enabled = records [entry clean] in
  check "wide arithmetic visible" (field "proof_math" (view enabled) = `String "wide");
  check "prior arithmetic visible" (field "proof_math" (view ~math:false enabled) = `String "prior");
  let cases = [
    "unavailable", "migration records unavailable", A.disabled ~chain_id, 20, cipher,
      "migration entitlement artifact unavailable";
    "not_active", "migration records not active", enabled, 19, cipher,
      "migration entitlement is not active";
    "not_found", "migration record not found", records [], 20, cipher,
      "migration entitlement not found";
    "cipher_mismatch", "migration record ciphertext mismatch", enabled, 20, cipher ^ "changed",
      "migration entitlement source ciphertext mismatch";
  ] in
  List.iter (fun (name, note, checks, epoch, cipher, reason) ->
    let response = view ~epoch ~cipher checks in
    check_missing name note response;
    let record = field "migration_record" response in
    check "record status" (field "status" record = `String name);
    check "record count" (field "count" record = `Int (A.entry_count checks));
    check "record root compatibility" (field "root" record = field "entitlement_root" response);
    check "find compatibility"
      (A.find checks ~epoch ~address ~cipher = Error reason);
    let denied = A.decision checks ~epoch ~address ~cipher in
    check "execution remains refused"
      (denied.audit_class = R.Poisoned && not denied.can_public_migrate
       && denied.commitment_net = None && denied.reason = reason)
  ) cases

let check_entries () =
  let poisoned = {clean with
    audit_class = R.Poisoned;
    can_public_migrate = false;
    public_net = None;
    blockers = ["history is incomplete"];
    reason = "history is incomplete";
  } in
  List.iter (fun decision ->
    let response = view (records [entry decision]) in
    let replay = field "legacy_public_replay" response in
    check "admission found" (field "admission_status" replay = `String "ready");
    check "complete decision" (field "complete" replay = `Bool true);
    check "recorded class preserved"
      (field "audit_class" replay = `String (R.string_of_audit_class decision.audit_class));
    check "public permission preserved"
      (field "can_public_migrate" replay = `Bool decision.can_public_migrate);
    check "no owner proof without commitment"
      (field "can_owner_proof_migrate" response = `Bool false)
  ) [clean; poisoned];
  let status = M.status_of_classes M.Empty M.Current in
  check "empty balance needs no history"
    (view ~status (A.disabled ~chain_id) |> field "legacy_public_replay" = `Null)

let check_owner () =
  let commitment = Pvac_ffi.pedersen_identity () |> Bytes.to_string |> Base64.encode_exn in
  let hidden = {clean with
    audit_class = R.Hidden_witness;
    can_public_migrate = false;
    public_net = None;
    commitment_net = Some commitment;
    reason = "legacy history needs hidden witness migration";
  } in
  List.iter (fun mode ->
    List.iter (fun verdict ->
      let response = view ~mode (records [entry verdict]) in
      let expected = mode = Octra_core.Rule_graph.Active
        && verdict.R.audit_class <> R.Poisoned in
      check "owner admission rule preserved"
        (field "can_owner_proof_migrate" response = `Bool expected);
      let replay = field "legacy_public_replay" response in
      check "public migration remains forbidden"
        (field "can_public_migrate" replay = `Bool false);
      check "recorded commitment preserved"
        (field "commitment_net" replay = `String commitment)
    ) [hidden; {hidden with audit_class = R.Poisoned}]
  ) [Octra_core.Rule_graph.Prior; Octra_core.Rule_graph.Active]

let check_cipher () =
  let records = A.disabled ~chain_id in
  let open Octra_core.Rule_graph in
  List.iter (fun (cipher_mode, proof_mode) ->
    List.iter (fun (cipher_class, key_class) ->
      let status = M.status_of_classes cipher_class key_class in
      let response = view ~cipher_mode ~proof_mode ~status records in
      let expected = proof_mode = Active && cipher_mode = Active && cipher_class = M.V3
        && key_class = M.Historical in
      check "cipher proof requires no record"
        (field "can_cipher_proof_migrate" response = `Bool expected);
      check "history authority remains separate"
        (field "can_owner_proof_migrate" response = `Bool false)
    ) [M.V3, M.Historical; M.V3, M.Current; M.V3, M.Missing;
       M.Legacy_hfhe, M.Historical; M.Empty, M.Historical]
  ) [Prior, Prior; Prior, Active; Active, Prior; Active, Active]

let check_recovery () =
  let open Octra_core.Rule_graph in
  let records = records [entry clean] in
  List.iter (fun mode ->
    let status = M.status_of_classes M.V3 M.Missing in
    let result = view ~cipher_mode:mode ~status records in
    check "semantic format" (field "cipher_format" result = `String "proven");
    check "missing key history gate" (field "history_recovery" result = `Bool (mode = Active));
    check "missing key record gate" (if mode = Active then
        field "status" (field "migration_record" result) = `String "ready"
      else field "legacy_public_replay" result = `Null);
    check "missing key cannot use cipher proof"
      (field "can_cipher_proof_migrate" result = `Bool false)
  ) [Prior; Active];
  List.iter (fun cipher_class ->
    let status = M.status_of_classes cipher_class M.Missing in
    let result = view ~cipher_mode:Active ~status records in
    check "invalid source cannot recover" (field "history_recovery" result = `Bool false)
  ) [M.Empty; M.Foreign; M.Malformed "invalid"]

let check_key_policy () =
  let module G = Octra_core.Rule_graph in
  let module Rpc = Octra_node_runtime.Node_rpc_server in
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = G.proof_key_activation_for_chain chain_id |> Option.get in
  let read bad epoch =
    if epoch = plan.anchor_epoch then bad
    else match G.root_after_floor ~chain_id ~floor_epoch:max_int ~epoch with
      | Some root -> G.Root root
      | None -> G.Missing in
  List.iter (fun epoch ->
    let current = epoch >= plan.activation_epoch in
    let good = G.create ~chain_id ~root_at:(read (G.Root plan.anchor_state_root)) in
    check "RPC key policy differs" (match Rpc.migration_policy good ~epoch with
      | Ok (_, _, _, _, key) -> key = if current then G.Active else G.Prior
      | Error _ -> false);
    List.iter (fun bad ->
      let graph = G.create ~chain_id ~root_at:(read bad) in
      check "RPC ignored key anchor"
        (Result.is_error (Rpc.migration_policy graph ~epoch) = current))
      [G.Missing; G.Unreadable "test"; G.Root (String.make 64 '0')])
    [plan.activation_epoch - 1; plan.activation_epoch; plan.activation_epoch + 1]

let () =
  check_key_policy ();
  check_errors ();
  check_entries ();
  check_owner ();
  check_cipher ();
  check_recovery ();
  Printf.printf "status = pass test = migration_status\n%!"