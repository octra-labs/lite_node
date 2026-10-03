(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module A = Octra_core.Pvac_migration_admission
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

let admission entries =
  A.create ~chain_id ~snapshot_epoch:10 ~state_root:(String.make 64 'a')
    ~activation_epoch:20 entries |> unwrap

let view ?(epoch = 20) ?(cipher = cipher) ?(mode = Octra_core.Rule_graph.Active)
    ?(status = M.status_of_classes M.V3 M.Historical) admissions =
  V.pvac_migration_status ~addr:address ~cipher ~epoch
    ~owner_migration_mode:mode status admissions

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
  let enabled = admission [entry clean] in
  let cases = [
    "unavailable", A.disabled ~chain_id, 20, cipher,
      "migration entitlement artifact unavailable";
    "not_active", enabled, 19, cipher,
      "migration entitlement is not active";
    "not_found", admission [], 20, cipher,
      "migration entitlement not found";
    "cipher_mismatch", enabled, 20, cipher ^ "changed",
      "migration entitlement source ciphertext mismatch";
  ] in
  List.iter (fun (name, admissions, epoch, cipher, reason) ->
    check_missing name reason (view ~epoch ~cipher admissions);
    check "find compatibility"
      (A.find admissions ~epoch ~address ~cipher = Error reason);
    let denied = A.decision admissions ~epoch ~address ~cipher in
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
    let response = view (admission [entry decision]) in
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
      let response = view ~mode (admission [entry verdict]) in
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

let () =
  check_errors ();
  check_entries ();
  check_owner ();
  Printf.printf "status = pass test = migration_status\n%!"