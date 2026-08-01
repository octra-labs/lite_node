(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Preverify_receipt
module Q = Preverify_queue
module T = Transaction
module A = Preverify_availability

type ready = {
  tx : T.t;
  receipt : R.t option;
}

type skip = {
  tx : T.t;
  reason : string;
  kind : skip_kind;
}

and skip_kind =
  | Deferred
  | Invalid

type batch = {
  ready : ready list;
  skipped : skip list;
}

type verdict =
  | Ready of R.t
  | Defer of string
  | Skip of string

type checked =
  | Checked_ready of ready
  | Checked_skip of skip

let hex_hash tag payload =
  Digestif.SHA256.digest_string (tag ^ "\000" ^ payload)
  |> Digestif.SHA256.to_hex

let tx_json tx =
  T.to_yojson tx |> Yojson.Safe.to_string

let input_hash tx =
  hex_hash "octra:preverify_input:v1" (tx_json tx)

let output_hash tx status =
  hex_hash "octra:preverify_output:v1" (T.hash tx ^ "\000" ^ status)

let state_hash root =
  hex_hash "octra:preverify_state:v2" root

let bound_payload tx state =
  let fields = [
    tx_json tx;
    state.R.pre_state_hash;
    state.source_cipher_hash;
    state.pvac_key_hash;
  ] in
  let fields =
    match state.transition_hash with
    | None -> fields
    | Some transition_hash -> fields @ [transition_hash]
  in
  String.concat "\000" fields

let bound_input_hash tx state =
  hex_hash "octra:preverify_input:v2" (bound_payload tx state)

let bound_output_hash tx state status =
  hex_hash "octra:preverify_output:v2"
    (T.hash tx ^ "\000" ^ bound_payload tx state ^ "\000" ^ status)

let circle_payload tx (circle : R.circle_state) =
  String.concat "\000" [
    tx_json tx;
    circle.snapshot_hash;
    circle.circle_id;
    circle.code_hash;
    circle.stable_root;
    circle.public_reads_hash;
    circle.context_hash;
    Circle_hfhe_transcript.canonical circle.transcript;
  ]

let circle_input_hash tx circle =
  hex_hash "octra:circle_preverify_input:v1" (circle_payload tx circle)

let circle_output_hash tx circle status =
  hex_hash
    "octra:circle_preverify_output:v1"
    (T.hash tx ^ "\000" ^ circle_payload tx circle ^ "\000" ^ status)

let receipt tx state prepared =
  let state = {
    state with
    R.transition_hash =
      Some (Private_ledger.hash_prepared prepared);
  } in
  match R.for_tx_bound
    ~input_hash:(bound_input_hash tx state)
    ~output_hash:(bound_output_hash tx state "ok")
    ~state
    ~ok:true
    ~reason:""
    tx with
  | Ok r -> Ready r
  | Error e -> Skip e

let transition_receipt tx state transition_hash =
  let state = {
    state with
    R.transition_hash = Some transition_hash;
  } in
  match R.for_tx_bound
    ~input_hash:(bound_input_hash tx state)
    ~output_hash:(bound_output_hash tx state "ok")
    ~state
    ~ok:true
    ~reason:""
    tx with
  | Ok receipt -> Ready receipt
  | Error error -> Skip error

let circle_receipt tx circle =
  match R.make_circle
    ~tx_hash:(T.hash tx)
    ~input_hash:(circle_input_hash tx circle)
    ~output_hash:(circle_output_hash tx circle "ok")
    ~circle
    ~ok:true
    ~reason:""
    ~cost:(Resource_lanes.cost tx) with
  | Ok receipt -> Ready receipt
  | Error e -> Skip e

let json_of_receipts receipts =
  List.map (fun r -> R.canonical r) receipts

let has_hash hashes hash =
  List.exists (String.equal hash) hashes

let receipts_for_hashes (ready : ready list) hashes =
  ready
  |> List.filter_map (fun item ->
    match item.receipt with
    | Some r when has_hash hashes r.R.tx_hash -> Some r
    | Some _ | None -> None)

let receipt_json_for_hashes ready hashes =
  receipts_for_hashes ready hashes
  |> json_of_receipts

let txs (batch : batch) =
  List.map (fun (item : ready) -> item.tx) batch.ready

let sender_enc ledger addr =
  match Ledger.find_opt ledger addr with
  | Some acc -> Ok (Option.value ~default:"0" acc.Ledger.encrypted_balance)
  | None -> Error "sender_missing"

let source_cipher_hash ledger tx =
  match tx.T.op_type with
  | T.CircleBalanceCellPut
  | T.CircleRegisterCellPut ->
    Circle_cell_transition.source_cipher_hash tx
  | _ ->
    begin
      match sender_enc ledger tx.T.from with
      | Error error -> Error error
      | Ok cipher ->
        Ok
          (Digestif.SHA256.digest_string cipher
           |> Digestif.SHA256.to_hex)
    end

let source_binding ledger pre_state_hash tx =
  let open Lwt.Syntax in
  match source_cipher_hash ledger tx with
  | Error e -> Lwt.return_error e
  | Ok source_cipher_hash ->
    let* key_hash = Ledger.get_pvac_key_hash ledger tx.T.from in
    let key_hash =
      match tx.T.op_type, key_hash with
      | T.KeySwitch, None -> Some R.zero_hash
      | _, value -> value
    in
    match key_hash with
    | None -> Lwt.return_error "pvac_key_not_consensus_bound"
    | Some pvac_key_hash ->
      Lwt.return_ok R.{
        pre_state_hash;
        source_cipher_hash;
        pvac_key_hash;
        transition_hash = None;
      }

let plan_result make task =
  let open Lwt.Syntax in
  let* result = task in
  match result with
  | Ok plan -> Lwt.return_ok (make plan)
  | Error failure -> Lwt.return_error failure.Private_ledger.reason

let verify_encrypt result_policy ledger tx =
  plan_result
    (fun plan -> Private_ledger.Prepared_encrypt plan)
    (Private_ledger.encrypt_plan ~result_policy ledger tx)

let verify_decrypt result_policy ledger tx =
  plan_result
    (fun plan -> Private_ledger.Prepared_decrypt plan)
    (Private_ledger.decrypt_plan ~result_policy ledger tx)

let verify_stealth result_policy ledger tx =
  let open Lwt.Syntax in
  let* plan = Private_ledger.stealth_plan ~result_policy ledger tx in
  match plan with
  | Error failure -> Lwt.return_error failure.Private_ledger.reason
  | Ok plan ->
    let* range = Private_ledger.stealth_inline_range ledger tx plan in
    begin
      match range with
      | Error failure -> Lwt.return_error failure.Private_ledger.reason
      | Ok range ->
        begin
          match Private_ledger.stealth_accept_range range with
          | Error failure -> Lwt.return_error failure.Private_ledger.reason
          | Ok () ->
            let* binding = Private_ledger.stealth_binding ledger tx plan in
            begin
              match binding with
              | Error failure ->
                Lwt.return_error failure.Private_ledger.reason
              | Ok () ->
                Lwt.return_ok (Private_ledger.Prepared_stealth plan)
            end
        end
    end

let verify_claim result_policy ledger tx =
  let open Lwt.Syntax in
  let* plan = Private_ledger.claim_plan ledger tx in
  match plan with
  | Error failure -> Lwt.return_error failure.Private_ledger.tag
  | Ok plan ->
    let* balance =
      Private_ledger.claim_balance_plan ~result_policy ledger tx plan
    in
    begin
      match balance with
      | Error failure -> Lwt.return_error failure.Private_ledger.reason
      | Ok balance ->
        Lwt.return_ok (Private_ledger.Prepared_claim (plan, balance))
    end

let verify_key_switch ?legacy_replay ledger tx =
  let open Lwt.Syntax in
  let replay =
    if Private_ledger.key_switch_requests_legacy_audit tx then
      match legacy_replay, sender_enc ledger tx.T.from with
      | Some lookup, Ok cipher ->
        Some (lookup ~address:tx.T.from ~cipher)
      | Some _, Error _
      | None, _ -> None
    else
      None
  in
  let* plan =
    Private_ledger.key_switch_plan
      ?legacy_public_replay:replay
      ledger
      tx
  in
  match plan with
  | Ok plan ->
    Lwt.return_ok (Private_ledger.Prepared_key_switch plan)
  | Error failure -> Lwt.return_error failure.Private_ledger.reason

let prepared_key_switch prepared ledger tx =
  let open Lwt.Syntax in
  match prepared with
  | None ->
    let* result = verify_key_switch ledger tx in
    Lwt.return (Result.fold ~ok:(fun value -> A.Ready value)
      ~error:(fun reason -> A.Invalid reason) result)
  | Some lookup ->
    let* available = lookup tx in
    begin
      match available with
      | A.Unmanaged ->
        let* result = verify_key_switch ledger tx in
        Lwt.return (Result.fold ~ok:(fun value -> A.Ready value)
          ~error:(fun reason -> A.Invalid reason) result)
      | A.Pending -> Lwt.return A.Pending
      | A.Invalid reason -> Lwt.return (A.Invalid reason)
      | A.Ready (Private_ledger.Prepared_key_switch _ as value) ->
        Lwt.return (A.Ready value)
      | A.Ready _ ->
        Lwt.return (A.Invalid "key switch prepared operation mismatch")
    end

let run_heavy
    ?ledger
    ?legacy_replay
    ?prepared
    ?(result_policy = Private_result_policy.Recoverable)
    tx =
  let open Lwt.Syntax in
  match tx.T.op_type, ledger with
  | T.KeySwitch, Some ledger ->
    if Private_ledger.key_switch_requests_legacy_audit tx then
      let* result = verify_key_switch ?legacy_replay ledger tx in
      Lwt.return
        (Result.fold
           ~ok:(fun value -> A.Ready value)
           ~error:(fun reason -> A.Invalid reason)
           result)
    else
      prepared_key_switch prepared ledger tx
  | T.EncryptOp, Some ledger ->
    let* result = verify_encrypt result_policy ledger tx in
    Lwt.return
      (Result.fold ~ok:(fun value -> A.Ready value)
         ~error:(fun reason -> A.Invalid reason) result)
  | T.DecryptOp, Some ledger ->
    let* result = verify_decrypt result_policy ledger tx in
    Lwt.return
      (Result.fold ~ok:(fun value -> A.Ready value)
         ~error:(fun reason -> A.Invalid reason) result)
  | T.StealthOp, Some ledger ->
    let* result = verify_stealth result_policy ledger tx in
    Lwt.return
      (Result.fold ~ok:(fun value -> A.Ready value)
         ~error:(fun reason -> A.Invalid reason) result)
  | T.ClaimOp, Some ledger ->
    let* result = verify_claim result_policy ledger tx in
    Lwt.return
      (Result.fold ~ok:(fun value -> A.Ready value)
         ~error:(fun reason -> A.Invalid reason) result)
  | T.PrivateOp, _ -> Lwt.return (A.Invalid "private_disabled")
  | T.RecryptOp, _ -> Lwt.return (A.Invalid "recrypt_disabled")
  | T.CircleBalanceCellPut, _ ->
    Lwt.return (A.Invalid "circle_balance_preverify_not_ready")
  | T.CircleRegisterCellPut, _ ->
    Lwt.return (A.Invalid "circle_register_preverify_not_ready")
  | _, None -> Lwt.return (A.Invalid "ledger_required")
  | _ -> Lwt.return (A.Invalid "lane_not_heavy")

let run
    ?ledger
    ?circle_preverify
    ?circle_cell_preverify
    ?legacy_replay
    ?prepared
    ?pre_state_hash
    ?pre_state_root
    ?(result_policy = Private_result_policy.Recoverable)
    tx =
  if not (Q.needs_preverify tx) then Lwt.return (Skip "lane_not_heavy")
  else
    let open Lwt.Syntax in
    let* pre_state_hash =
      match ledger, pre_state_hash with
      | Some ledger, None ->
        let* hash = Ledger.hash ledger in
        Lwt.return_some (state_hash hash)
      | _, value -> Lwt.return value
    in
    if tx.T.op_type = T.CircleCall then
      begin
        match circle_preverify, pre_state_hash, pre_state_root with
        | Some verify, Some pre_state_hash, Some pre_state_root ->
          let* circle =
            verify ~pre_state_hash ~pre_state_root tx
          in
          begin
            match circle with
            | Ok circle -> Lwt.return (circle_receipt tx circle)
            | Error e -> Lwt.return (Skip e)
          end
        | None, _, _ -> Lwt.return (Skip "circle_preverify_required")
        | _, None, _
        | _, _, None -> Lwt.return (Skip "state_binding_required")
      end
    else if
      tx.T.op_type = T.CircleBalanceCellPut
      || tx.T.op_type = T.CircleRegisterCellPut
    then
      begin
        match
          ledger,
          circle_cell_preverify,
          pre_state_hash,
          pre_state_root
        with
        | Some ledger, Some verify, Some pre_state_hash, Some pre_state_root ->
          let* state = source_binding ledger pre_state_hash tx in
          begin
            match state with
            | Error error -> Lwt.return (Skip error)
            | Ok state ->
              let* transition_hash =
                verify ~pre_state_hash ~pre_state_root tx
              in
              begin
                match transition_hash with
                | Ok transition_hash ->
                  Lwt.return (transition_receipt tx state transition_hash)
                | Error error -> Lwt.return (Skip error)
              end
          end
        | None, _, _, _ -> Lwt.return (Skip "ledger_required")
        | _, None, _, _ ->
          Lwt.return (Skip "circle_cell_preverify_required")
        | _, _, None, _
        | _, _, _, None -> Lwt.return (Skip "state_binding_required")
      end
    else
      let* v = run_heavy ?ledger ?legacy_replay ?prepared ~result_policy tx in
      match v with
      | A.Unmanaged -> Lwt.return (Skip "preverify operation is unmanaged")
      | A.Pending -> Lwt.return (Defer "key_switch_preverify_pending")
      | A.Invalid e -> Lwt.return (Skip e)
      | A.Ready prepared ->
        begin
          match ledger, pre_state_hash with
          | Some ledger, Some pre_state_hash ->
            let* state = source_binding ledger pre_state_hash tx in
            begin
              match state with
              | Ok state -> Lwt.return (receipt tx state prepared)
              | Error e -> Lwt.return (Skip e)
            end
          | _ -> Lwt.return (Skip "state_binding_required")
        end

let snapshot_transition tx =
  match tx.T.op_type with
  | T.CircleCall
  | T.CircleBalanceCellPut
  | T.CircleRegisterCellPut -> true
  | _ -> false

let first_snapshot_transition txs =
  List.find_opt
    snapshot_transition
    txs

let snapshot_isolation_reason tx =
  if tx.T.op_type = T.CircleCall then "circle_receipt_snapshot_isolation"
  else "circle_cell_receipt_snapshot_isolation"

let isolate_snapshot selected txs =
  let selected_hash = T.hash selected in
  List.fold_right
    (fun tx (ready, skipped) ->
      if String.equal (T.hash tx) selected_hash then
        tx :: ready, skipped
      else
        ready,
        {
          tx;
          reason = snapshot_isolation_reason selected;
          kind = Deferred;
        } :: skipped)
    txs
    ([], [])

let without_snapshot_transitions txs =
  List.filter
    (fun tx -> not (snapshot_transition tx))
    txs

let deferred_snapshot_transitions selected_skip txs =
  List.filter_map
    (fun tx ->
      if not (snapshot_transition tx) then None
      else if String.equal (T.hash tx) (T.hash selected_skip.tx) then
        Some selected_skip
      else
        Some {
          tx;
          reason = "circle_preverify_deferred";
          kind = Deferred;
        })
    txs

let private_state_transition tx =
  match tx.T.op_type with
  | T.EncryptOp
  | T.DecryptOp
  | T.StealthOp
  | T.ClaimOp
  | T.KeySwitch -> true
  | _ -> false

let tx_position tx =
  tx.T.nonce, T.hash tx

let compare_position (left_nonce, left_hash) (right_nonce, right_hash) =
  let nonce_order = compare left_nonce right_nonce in
  if nonce_order <> 0 then nonce_order
  else String.compare left_hash right_hash

let private_transition_cutoffs txs =
  let by_sender = Hashtbl.create 32 in
  List.iter
    (fun tx ->
      if private_state_transition tx then
        let current =
          match Hashtbl.find_opt by_sender tx.T.from with
          | Some values -> values
          | None -> []
        in
        Hashtbl.replace by_sender tx.T.from (tx_position tx :: current))
    txs;
  let cutoffs = Hashtbl.create (Hashtbl.length by_sender) in
  Hashtbl.iter
    (fun sender positions ->
      match List.sort compare_position positions with
      | _ :: second :: _ -> Hashtbl.add cutoffs sender second
      | _ -> ())
    by_sender;
  cutoffs

let isolate_private_transitions txs =
  let cutoffs = private_transition_cutoffs txs in
  List.fold_right
    (fun tx (ready, skipped) ->
      match Hashtbl.find_opt cutoffs tx.T.from with
      | Some cutoff
        when compare_position (tx_position tx) cutoff >= 0 ->
        ready,
        {
          tx;
          reason = "private_transition_dependency_deferred";
          kind = Deferred;
        } :: skipped
      | _ -> tx :: ready, skipped)
    txs
    ([], [])

let batch_of_checked checked isolated =
  let ready, skipped =
    List.fold_right
      (fun verdict (ready, skipped) ->
        match verdict with
        | Checked_ready item -> item :: ready, skipped
        | Checked_skip item -> ready, item :: skipped)
      checked
      ([], isolated)
  in
  { ready; skipped }

let verify_checked_batch verify txs isolated =
  let open Lwt.Syntax in
  let* checked = Lwt_list.map_p verify txs in
  Lwt.return (batch_of_checked checked isolated)

let run_checked verify txs =
  let open Lwt.Syntax in
  let txs, private_isolated = isolate_private_transitions txs in
  match first_snapshot_transition txs with
  | None ->
    verify_checked_batch verify txs private_isolated
  | Some selected ->
    let* selected_verdict = verify selected in
    begin
      match selected_verdict with
      | Checked_ready item ->
        let _, isolated = isolate_snapshot selected txs in
        Lwt.return {
          ready = [item];
          skipped = private_isolated @ isolated;
        }
      | Checked_skip selected_skip ->
        verify_checked_batch
          verify
          (without_snapshot_transitions txs)
          (private_isolated
           @ deferred_snapshot_transitions selected_skip txs)
    end

let checked_of_single_batch tx batch =
  let same_hash candidate =
    String.equal (T.hash tx) (T.hash candidate)
  in
  match batch.ready, batch.skipped with
  | [item], [] when same_hash item.tx ->
    Ok (Checked_ready item)
  | [], [item] when same_hash item.tx ->
    Ok (Checked_skip item)
  | _ ->
    Error "invalid_single_preverify_batch"

let run_many
    ?ledger
    ?circle_preverify
    ?circle_cell_preverify
    ?legacy_replay
    ?prepared
    ?(result_policy = Private_result_policy.Recoverable)
    txs =
  let open Lwt.Syntax in
  let* pre_state_root =
    match ledger with
    | Some ledger ->
      Ledger.hash ledger
    | None -> Lwt.return R.zero_hash
  in
  let pre_state_hash = state_hash pre_state_root in
  let verify tx =
    if not (Q.needs_preverify tx) then
      Lwt.return (Checked_ready { tx; receipt = None })
    else
      let* verdict =
        run
          ?ledger
          ?circle_preverify
          ?circle_cell_preverify
          ?legacy_replay
          ?prepared
          ~pre_state_hash
          ~pre_state_root
          ~result_policy
          tx
      in
      match verdict with
      | Ready receipt ->
        Lwt.return (Checked_ready { tx; receipt = Some receipt })
      | Defer reason ->
        Lwt.return (Checked_skip { tx; reason; kind = Deferred })
      | Skip reason ->
        Lwt.return (Checked_skip { tx; reason; kind = Invalid })
  in
  run_checked verify txs

let checked_cacheable = function
  | Checked_ready _ -> true
  | Checked_skip item -> item.kind = Invalid