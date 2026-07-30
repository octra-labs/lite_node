(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type peer_head = {
  responder_addr : string;
  responder_head_epoch : int64;
}

type lag_decision =
  | In_sync
  | Do_catchup of int
  | Snapshot_required of int
  | Ahead_of_target of int

type lag_mode =
  | Soft
  | Quarantine

type probe_sample =
  | Fresh
  | Stale of { start_head : int; live_head : int }

type apply_point = {
  epoch : int64;
  root : string;
  eic : string option;
  txid : int64;
}

type fork_repair_decision =
  | No_fork_repair
  | Rollback_fork_head of int64
  | Fork_snapshot_required of string

type base_gate_recovery =
  | Base_gate_quarantine
  | Base_gate_retry
  | Base_gate_already_applied

type apply_recovery =
  | Apply_quarantine
  | Apply_retry
  | Apply_already_applied

type record_apply_decision =
  | Apply_record
  | Skip_applied_record
  | Retry_moved_head

let pick_target_epoch ~(responses : peer_head list) ~(f : int) ~(fallback : int64) : int64 =
  let heads = List.map (fun r -> r.responder_head_epoch) responses in
  let sorted = List.sort (fun a b -> Int64.compare b a) heads in
  match List.nth_opt sorted f with
  | Some e -> e
  | None -> fallback

let bounded_distance high low =
  let distance = Z.sub (Z.of_int64 high) (Z.of_int64 low) in
  if Z.gt distance (Z.of_int max_int) then max_int
  else Z.to_int distance

let classify_lag ~(target : int64) ~(our_head : int64) ~(max_lag : int) : lag_decision =
  match Int64.compare target our_head with
  | 0 -> In_sync
  | value when value < 0 ->
    Ahead_of_target (bounded_distance our_head target)
  | _ ->
    let lag = bounded_distance target our_head in
    if lag > max_lag then Snapshot_required lag
    else Do_catchup lag

let lag_mode ~(lag : int) ~(soft_lag : int) : lag_mode =
  if lag <= max 0 soft_lag then Soft else Quarantine

let base_gate_recovery
    ~target
    ~start_head
    ~current_head =
  if Int64.compare current_head target >= 0 then Base_gate_already_applied
  else if Int64.compare current_head start_head > 0 then Base_gate_retry
  else Base_gate_quarantine

let apply_recovery
    ~gap_active:_
    ~target
    ~start_head
    ~current_head =
  match base_gate_recovery ~target ~start_head ~current_head with
  | Base_gate_quarantine -> Apply_quarantine
  | Base_gate_retry -> Apply_retry
  | Base_gate_already_applied -> Apply_already_applied

let record_apply_decision
    ~head
    ~record_epoch =
  if head < record_epoch then Apply_record
  else if head = record_epoch then Skip_applied_record
  else Retry_moved_head

let classify_probe_sample ~start_head ~live_head =
  if start_head = live_head then Fresh
  else Stale { start_head; live_head }

let genesis_state_attested
    ~head
    ~target
    ~has_committed_root
    ~responses
    ~required =
  head < 0
  && Int64.compare target (Int64.of_int head) = 0
  && not has_committed_root
  && responses >= required

let decide_fork_repair
    ~our_head
    ~target
    ~current_root_quorum
    ~target_root_quorum
    ~target_local_matches
    ~empty_fork =
  if Int64.compare target our_head >= 0 then No_fork_repair
  else if current_root_quorum then No_fork_repair
  else if not target_root_quorum then No_fork_repair
  else if not target_local_matches then Fork_snapshot_required "target_root_mismatch"
  else if not empty_fork then Fork_snapshot_required "non_empty_fork"
  else Rollback_fork_head target

let is_valid_chunk_first_epoch ~(status : string) ~(records : C_codec.catchup_epoch_record list) ~(from_epoch : int64) : bool =
  if status <> "ok" then false
  else match records with
  | [] -> false
  | first :: _ -> Int64.compare first.C_codec.epoch_id from_epoch = 0

let verify_record_hashes
    ~(record : C_codec.catchup_epoch_record)
    ~(parsed_tx_hashes : string list) : (unit, string) result =
  let hash32_to_hex h =
    if String.length h = 64 then String.lowercase_ascii h
    else String.concat "" (List.init (String.length h) (fun i ->
      Printf.sprintf "%02x" (Char.code h.[i])))
  in
  let record_tx_hashes_hex = List.map hash32_to_hex record.tx_hashes in
  let parsed_tx_hashes_hex = List.map hash32_to_hex parsed_tx_hashes in
  let computed_tlh =
    Octra_net.Hash_domain.hash "octra:tx_list:v1"
      (String.concat "" record_tx_hashes_hex) in
  if computed_tlh <> record.tx_list_hash then
    Error (Printf.sprintf "tx_list_hash mismatch at epoch = %Ld" record.epoch_id)
  else if List.length record_tx_hashes_hex <> List.length parsed_tx_hashes_hex then
    Error (Printf.sprintf "tx_hash count mismatch at epoch = %Ld: expected = %d got = %d"
             record.epoch_id (List.length record_tx_hashes_hex) (List.length parsed_tx_hashes_hex))
  else
    let mismatch =
      List.fold_left2 (fun acc expected got ->
        match acc with
        | Some _ -> acc
        | None -> if expected = got then None else Some (expected, got)
      ) None record_tx_hashes_hex parsed_tx_hashes_hex
    in
    match mismatch with
    | None -> Ok ()
    | Some (e, g) ->
      Error (Printf.sprintf "tx hash mismatch at epoch = %Ld: expected = %s got = %s"
               record.epoch_id (String.sub e 0 (min 16 (String.length e)))
               (String.sub g 0 (min 16 (String.length g))))

let verify_record_receipts
    ~(record : C_codec.catchup_epoch_record) : (unit, string) result =
  let expected = C_hash.receipt_root record.receipts_json in
  if expected = record.receipt_root then Ok ()
  else Error (Printf.sprintf "receipt_root mismatch at epoch = %Ld" record.epoch_id)

let verify_record_finality
    ~chain_id
    ~expected_validator_set_hash
    ~expected_txid
    ~(record : C_codec.catchup_epoch_record) =
  match record.finality with
  | None ->
    Error
      (Printf.sprintf
         "finality is missing at epoch = %Ld"
         record.epoch_id)
  | Some finality ->
    let finalize = finality.finalize in
    let header = finalize.C_types.header in
    let validator_set_hash =
      C_config.validator_set_hash finality.validator_set
    in
    let verify_vote (vote : C_types.vote) =
      match
        C_types.pubkey_of_addr
          finality.validator_set
          vote.C_types.validator
      with
      | Some pubkey -> C_hash.verify_vote ~pubkey_raw:pubkey vote
      | None -> false
    in
    if validator_set_hash <> expected_validator_set_hash then
      Error
        (Printf.sprintf
           "finality validator set mismatch at epoch = %Ld"
           record.epoch_id)
    else
    match
      C_qc.validate_persisted_finalize
        ~chain_id
        ~validator_set:finality.validator_set
        ~verify_vote
        finalize
    with
    | C_qc.Invalid reason ->
      Error
        (Printf.sprintf
           "finality qc is invalid at epoch = %Ld reason = %s"
           record.epoch_id
           reason)
    | C_qc.Valid ->
      let expected_txid_hi = Int64.pred expected_txid in
      let same_timestamp =
        Int64.bits_of_float header.ts
        = Int64.bits_of_float record.epoch_ts
      in
      if finalize.epoch_id <> record.epoch_id then
        Error "finality epoch mismatch"
      else if header.prev_state_root <> record.prev_state_root then
        Error "finality previous root mismatch"
      else if header.proposed_state_root <> record.state_root then
        Error "finality state root mismatch"
      else if header.tx_list_hash <> record.tx_list_hash then
        Error "finality transaction root mismatch"
      else if header.receipt_root <> record.receipt_root then
        Error "finality receipt root mismatch"
      else if header.txid_hi <> expected_txid_hi then
        Error "finality transaction high-water mismatch"
      else if not same_timestamp then
        Error "finality timestamp mismatch"
      else if header.creator_addr <> record.creator_addr then
        Error "finality proposer mismatch"
      else if finalize.commit_round <> record.commit_round then
        Error "finality round mismatch"
      else
        Ok finality

let verify_chain_continuity
    ~(records : C_codec.catchup_epoch_record list)
    ~(from_epoch : int64)
    ~(prev_root : string) : (unit, string) result =
  let initial_prev =
    match records with
    | first :: _ when Int64.compare from_epoch 0L = 0 ->
      first.C_codec.prev_state_root
    | _ -> prev_root
  in
  let rec walk expected_epoch expected_prev = function
    | [] -> Ok ()
    | r :: rest ->
      if Int64.compare r.C_codec.epoch_id expected_epoch <> 0 then
        Error (Printf.sprintf "epoch_id chain break at %Ld: expected = %Ld got = %Ld"
                 r.epoch_id expected_epoch r.epoch_id)
      else if r.prev_state_root <> expected_prev then
        Error (Printf.sprintf "prev_state_root chain break at epoch = %Ld" r.epoch_id)
      else
        walk (Int64.add r.epoch_id 1L) r.state_root rest
  in
  walk from_epoch initial_prev records

let verify_apply_base ~(from_epoch : int64) ~(head : apply_point) : (unit, string) result =
  let expected_epoch = Int64.sub from_epoch 1L in
  if Int64.compare head.epoch expected_epoch <> 0 then
    Error (Printf.sprintf "catchup base epoch moved: expected = %Ld got = %Ld"
      expected_epoch head.epoch)
  else if String.length head.root = 0 then
    Error "catchup base root missing"
  else
    Ok ()

let verify_apply_final
    ~(last_epoch : int64)
    ~(expected_root : string)
    ~(expected_eic : string)
    ~(expected_txid : int64)
    ~(head : apply_point) : (unit, string) result =
  if Int64.compare head.epoch last_epoch <> 0 then
    Error (Printf.sprintf "catchup final epoch mismatch: expected = %Ld got = %Ld"
      last_epoch head.epoch)
  else if head.root <> expected_root then
    Error (Printf.sprintf "catchup final root mismatch at epoch = %Ld" last_epoch)
  else if head.eic <> Some expected_eic then
    Error (Printf.sprintf "catchup final EIC mismatch at epoch = %Ld" last_epoch)
  else if Int64.compare head.txid expected_txid <> 0 then
    Error (Printf.sprintf "catchup final txid mismatch at epoch = %Ld expected = %Ld got = %Ld"
      last_epoch expected_txid head.txid)
  else
    Ok ()