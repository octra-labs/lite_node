(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction

type limits = {
  max_txs : int;
  max_bytes : int;
  max_ou : Z.t;
}

type totals = {
  count : int;
  bytes : int;
  ou : Z.t;
}

type capped = {
  txs : Transaction.t list;
  skipped : int;
  totals : totals;
}

type verified_bundle = {
  txs : Transaction.t list;
  candidates : Transaction.t list;
  receipts_json : string list;
  rejections : Octra_core.Tx_outcome.rejection list;
  preverify : Octra_core.Preverify_commit.t;
}

type local_preverify_bundle = {
  batch : Octra_core.Preverify_worker.batch;
  ready_txs : Transaction.t list;
  receipts_json : string list;
  skipped_count : int;
  skipped_sample : string;
}

type build_preverify = {
  batch : Octra_core.Preverify_worker.batch;
  heavy_count : int;
  capped : capped;
  txs : Transaction.t list;
  tx_hashes : string list;
  receipts : Octra_core.Preverify_receipt.t list;
  skipped_count : int;
  skipped_sample : string;
}

type proposal_admission_result =
  | Proposal_admit
  | Proposal_defer

type layera_diag_context = {
  validator_addrs : string list;
  validators_sha : string;
  meta_current : string;
  meta_last : string;
  meta_supply : string;
  meta_emission : string;
}

type preview_status =
  | Preview_ok of {
      post_state_root : string;
    }
  | Preview_error of string

type preview_decision =
  | Preview_accept of {
      computed_root : string;
      preview_eic_root : string;
    }
  | Preview_root_mismatch of {
      expected_root : string;
      computed_root : string option;
      preview_eic_root : string;
    }
  | Preview_error_reject of string

type prev_root_decision =
  | Prev_root_match
  | Prev_root_mismatch of {
      waited_steps : int;
      streak_after : int;
      quarantine_reason : string option;
    }

type prev_root_wait_deps = {
  read_root : unit -> string Lwt.t;
  replay_stashed : unit -> unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

type prev_root_sample = {
  initial_root : string;
  current_root : string;
  tries_left : int;
}

type build_preview_status =
  | Build_preview_ok of {
      post_state_root : string;
      confirmed : Transaction.t list;
      rejected : Transaction.t list;
    }
  | Build_preview_error of string

type build_preview_plan = {
  final_txs : Transaction.t list;
  final_hashes : string list;
  rejected_hashes : string list;
  proposed_state_root : string;
  preview_consensus_root : string option;
  preview_error : string option;
}

type build_preview_output = {
  plan : build_preview_plan;
  receipts_json : string list;
  rejected_count : int;
}

type proposal_roots = {
  prev_ledger_root : string;
  prev_state_root : string;
}

type tx_hash_admission =
  | Tx_hash_ok of string list
  | Tx_hash_mismatch

type proposal_envelope = {
  header : Octra_consensus.C_types.epoch_header;
  parent_commit : Octra_consensus.C_types.parent_commit option;
  proposal_id : string;
  txid_hi : int64;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  frozen_bundle : Consensus_bundle_cache.frozen;
}

type frozen_proposal = {
  header : Octra_consensus.C_types.epoch_header;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  proposal_id : string;
  proposal_id_short : string;
}

type build_preview_request = {
  epoch_id : int64;
  epoch_ts : float;
  proposal_id : string;
  expected_prev_root : string;
  prev_state_root : string;
  parent_commit : Octra_consensus.C_types.parent_commit option;
  proposer : string;
  validator_pubkeys : (string * string) list;
  preverify : Octra_core.Preverify_commit.t;
  txs : Transaction.t list;
}

type make_proposal_deps = {
  start_height : int64 -> unit Lwt.t;
  current_epoch : unit -> int;
  state_attested : unit -> bool;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  read_prev_ledger_root : unit -> string option Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  current_round : unit -> int;
  parent_commit :
    epoch_id:int64 ->
    (Octra_consensus.C_types.parent_commit option, string) result;
  frozen_bundle : string -> Consensus_bundle_cache.frozen option;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  staging_txs : unit -> Transaction.t list;
  admits_tx : Transaction.t -> bool;
  build_preverify_once :
    state_root:string ->
    tx_hashes:string list ->
    Transaction.t list ->
    Octra_core.Preverify_worker.batch Lwt.t;
  staging_total : unit -> int;
  proposer : unit -> string;
  validator_pubkeys : int64 -> (string * string) list;
  preview :
    build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  prev_eic_root : unit -> string;
  next_txid : unit -> int64;
  set_proposal : Transaction.t list -> string list -> unit;
  head_txid_hi : unit -> int64 option;
  freeze : string -> Consensus_bundle_cache.frozen -> unit;
  now : unit -> float;
  previous_epoch_ts : int64 -> float option;
}

type verify_proposal_deps = {
  now : unit -> float;
  previous_epoch_ts : int64 -> float option;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  mark_quarantine : string -> unit;
  prev_root_streak : unit -> int;
  set_prev_root_streak : int -> unit;
  state_root_streak : unit -> int;
  set_state_root_streak : int -> unit;
  limits : limits;
  layera_diag_live : unit -> bool;
  layera_env_diag_context : unit -> layera_diag_context;
  layera_diag_context : validator_addrs:string list -> layera_diag_context;
  wait_prev_root : prev_root_wait_deps;
  max_prev_root_wait_tries : int;
  prev_root_wait_delay_seconds : float;
  quarantine_mismatch_threshold : int;
  staging_txs : unit -> Transaction.t list;
  cached_bundle : proposal_id:string -> Consensus_bundle_fetch.proposal_bundle option;
  validate_preverify_once :
    state_root:string ->
    tx_hashes:string list ->
    Transaction.t list ->
    Octra_core.Preverify_worker.batch Lwt.t;
  driver_available : unit -> bool;
  validate_bundle :
    header:Octra_consensus.C_types.epoch_header ->
    expected_hashes:string list ->
    Octra_consensus.C_driver.bundle_response_record ->
    Consensus_bundle_validation.accepted option;
  query_bundle :
    epoch_id:int64 ->
    proposal_id:string ->
    validate:(Octra_consensus.C_driver.bundle_response_record -> bool) ->
    Octra_consensus.C_driver.bundle_response_record option Lwt.t;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  public_key_for_tx : Transaction.t -> string option;
  verify_address_pubkey : addr:string -> pubkey:string -> bool;
  verify_tx_signature : Transaction.t -> pubkey:string -> bool;
  validator_pubkeys : int64 -> (string * string) list;
  read_local_ledger_root : unit -> string Lwt.t;
  preview :
    build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  prev_eic_root : unit -> string;
  next_txid : unit -> int64;
  root_to_raw32 : string -> string;
  set_proposal : Transaction.t list -> string list -> unit;
  verify_parent_commit :
    epoch_id:int64 ->
    Octra_consensus.C_types.parent_commit option ->
    (unit, string) result;
}

type before_precommit_deps = {
  chain_id : string;
  validator_set : unit -> Octra_consensus.C_types.validator_set;
  current_tx_hashes : unit -> string list;
  cached_bundle : string -> Consensus_bundle_cache.encoded option;
  sync_bundle :
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  mark_unsynced : unit -> unit;
  write_pending : Octra_core.Wal.pending_commit -> unit;
  now : unit -> float;
}

type precommit_sync_plan =
  | Precommit_sync_current
  | Precommit_sync_missing of {
      pid_short : string;
    }
  | Precommit_sync_decoded of {
      pid_short : string;
      tx_hashes : string list;
      txs : Transaction.t list;
      receipts_json : string list;
    }
  | Precommit_sync_decode_failed of {
      pid_short : string;
      error : string;
    }

type reject_reason =
  | Missing_txs of {
      have : int;
      need : int;
    }
  | Disabled_operation of {
      hash : string;
      op_type : string;
    }
  | Underpriced_transaction of {
      hash : string;
      op_type : string;
      provided : Z.t;
      required : Z.t;
    }
  | Receipt_root_mismatch
  | Receipt_decode_failed of string
  | Preverify_gate_failed of string
  | Bundle_limit of {
      totals : totals;
      limits : limits;
    }
  | Invalid_tx_signature of {
      hash : string;
      from_addr : string;
    }

type verification_deps = {
  public_key_for_tx : Transaction.t -> string option;
  verify_address_pubkey : addr:string -> pubkey:string -> bool;
  verify_tx_signature : Transaction.t -> pubkey:string -> bool;
}

type validator_pubkeys = (string * string) list

type admission_plan =
  | Proceed
  | Defer_state_not_attested
  | Defer_quarantine of {
      reason : string;
    }
  | Realign_stale_height of {
      target_epoch : int64;
    }
  | Defer_apply_gap

let limits ~max_txs ~max_bytes ~max_ou =
  {
    max_txs = max 1 max_txs;
    max_bytes = max 1024 max_bytes;
    max_ou;
  }

let wire_size tx =
  String.length (Yojson.Safe.to_string (Transaction.to_yojson tx))

let totals txs =
  let bytes, ou =
    List.fold_left
      (fun (bytes, ou) tx ->
         bytes + wire_size tx, Z.add ou (Transaction.ou_cost tx))
      (0, Z.zero)
      txs
  in
  {
    count = List.length txs;
    bytes;
    ou;
  }

let within_limits ~limits txs =
  let t = totals txs in
  t.count <= limits.max_txs
  && t.bytes <= limits.max_bytes
  && Z.leq t.ou limits.max_ou

let cap ~limits txs =
  let rec loop count bytes ou acc skipped = function
    | [] ->
      {
        txs = List.rev acc;
        skipped;
        totals = { count; bytes; ou };
      }
    | tx :: rest ->
      let next_count = count + 1 in
      let next_bytes = bytes + wire_size tx in
      let next_ou = Z.add ou (Transaction.ou_cost tx) in
      if next_count > limits.max_txs
         || next_bytes > limits.max_bytes
         || Z.gt next_ou limits.max_ou then
        {
          txs = List.rev acc;
          skipped = skipped + 1 + List.length rest;
          totals = { count; bytes; ou };
        }
      else
        loop next_count next_bytes next_ou (tx :: acc) skipped rest
  in
  loop 0 0 Z.zero [] 0 txs

let first_three render items =
  items
  |> List.fold_left
       (fun (n, acc) item ->
          if n >= 3 then (n, acc)
          else (n + 1, render item :: acc))
       (0, [])
  |> snd
  |> List.rev
  |> String.concat ","

let first_disabled_bft_tx txs =
  List.find_opt
    (fun tx ->
       not
         (Transaction.bft_consensus_admits_op
            tx.Transaction.op_type))
    txs

let first_underpriced_tx txs =
  List.find_opt
    (fun tx ->
       Z.lt tx.Transaction.ou (Transaction.ou_cost tx))
    txs

let local_preverify_bundle ~run_many ~tx_hashes txs =
  match first_disabled_bft_tx txs with
  | Some tx ->
    Lwt.return {
      batch = { Octra_core.Preverify_worker.ready = []; skipped = [] };
      ready_txs = [];
      receipts_json = [];
      skipped_count = 1;
      skipped_sample = Transaction.bft_reject_reason tx.Transaction.op_type;
    }
  | None ->
    let open Lwt.Syntax in
    let* batch = run_many txs in
    let ready_txs = Octra_core.Preverify_worker.txs batch in
    let receipts_json =
      Octra_core.Preverify_worker.receipt_json_for_hashes batch.ready tx_hashes
    in
    let skipped_sample =
      first_three
        (fun (item : Octra_core.Preverify_worker.skip) -> item.reason)
        batch.skipped
    in
    Lwt.return {
      batch;
      ready_txs;
      receipts_json;
      skipped_count = List.length batch.skipped;
      skipped_sample;
    }

let check_local_bundle ~expected_hashes
    (received : Consensus_bundle_fetch.proposal_bundle)
    (local : local_preverify_bundle) =
  let received_hashes = List.map Transaction.hash received.txs in
  let local_hashes = List.map Transaction.hash local.ready_txs in
  if received_hashes <> expected_hashes then
    Error "received_tx_hash_mismatch"
  else if local_hashes <> expected_hashes then
    Error "local_preverify_tx_mismatch"
  else
    match Octra_core.Tx_outcome.split received.receipts_json with
    | Error error -> Error ("received_outcome_invalid:" ^ error)
    | Ok artifacts ->
      if local.receipts_json <> artifacts.preverify then
        Error "local_preverify_receipt_mismatch"
      else
        Ok received

let build_preverify ~run_many ~limits txs =
  let open Lwt.Syntax in
  let heavy_count =
    List.fold_left
      (fun n tx ->
         if Octra_core.Preverify_queue.needs_preverify tx then n + 1 else n)
      0
      txs
  in
  let* batch = run_many txs in
  let ready_txs = Octra_core.Preverify_worker.txs batch in
  let capped = cap ~limits ready_txs in
  let txs = capped.txs in
  let tx_hashes = List.map Transaction.hash txs in
  let receipts =
    Octra_core.Preverify_worker.receipts_for_hashes batch.ready tx_hashes
  in
  let skipped_sample =
    first_three
      (fun (item : Octra_core.Preverify_worker.skip) ->
         let hash = Transaction.hash item.tx in
         let short = String.sub hash 0 (min 12 (String.length hash)) in
         short ^ ":" ^ item.reason)
      batch.skipped
  in
  Lwt.return {
    batch;
    heavy_count;
    capped;
    txs;
    tx_hashes;
    receipts;
    skipped_count = List.length batch.skipped;
    skipped_sample;
  }

let receipt_root_matches ~header receipts_json =
  Octra_consensus.C_hash.receipt_root receipts_json =
  header.Octra_consensus.C_types.receipt_root

let short n s =
  String.sub s 0 (min n (String.length s))

let invalid_signature deps tx =
  match deps.public_key_for_tx tx with
  | None ->
    Some (Invalid_tx_signature {
      hash = short 12 (Transaction.hash tx);
      from_addr = short 14 tx.from;
    })
  | Some pubkey ->
    if deps.verify_address_pubkey ~addr:tx.from ~pubkey
       && deps.verify_tx_signature tx ~pubkey then
      None
    else
      Some (Invalid_tx_signature {
        hash = short 12 (Transaction.hash tx);
        from_addr = short 14 tx.from;
      })

let verify_signatures deps txs =
  List.find_map (invalid_signature deps) txs

let verify_bundle deps ~limits ~header ~expected_tx_count txs receipts_json =
  let have = List.length txs in
  if have <> expected_tx_count then
    Error (Missing_txs { have; need = expected_tx_count })
  else if not (receipt_root_matches ~header receipts_json) then
    Error Receipt_root_mismatch
  else
    match Octra_core.Tx_outcome.split receipts_json with
    | Error error -> Error (Receipt_decode_failed error)
    | Ok artifacts ->
      match
        Octra_core.Tx_outcome.merge
          ~confirmed:txs
          ~rejections:artifacts.rejections
      with
      | Error error -> Error (Receipt_decode_failed error)
      | Ok candidates ->
    match first_disabled_bft_tx candidates with
    | Some tx ->
      Error
        (Disabled_operation {
           hash = short 12 (Transaction.hash tx);
           op_type = Transaction.op_type_to_string tx.Transaction.op_type;
         })
    | None ->
      match first_underpriced_tx candidates with
      | Some tx ->
        Error
          (Underpriced_transaction {
             hash = short 12 (Transaction.hash tx);
             op_type =
               Transaction.op_type_to_string tx.Transaction.op_type;
             provided = tx.Transaction.ou;
             required = Transaction.ou_cost tx;
           })
      | None ->
        match
          Octra_core.Preverify_commit.receipts_of_strings artifacts.preverify
        with
        | Error e -> Error (Receipt_decode_failed e)
        | Ok receipts ->
          let preverify = Octra_core.Preverify_commit.create receipts in
          match Octra_core.Preverify_commit.check preverify txs with
          | Error e -> Error (Preverify_gate_failed e)
          | Ok () ->
            if not (within_limits ~limits candidates) then
              Error (Bundle_limit { totals = totals candidates; limits })
            else
              match verify_signatures deps candidates with
              | Some bad -> Error bad
              | None ->
                Ok {
                  txs;
                  candidates;
                  receipts_json;
                  rejections = artifacts.rejections;
                  preverify;
                }

let validator_pubkeys_from_driver driver =
  driver.Octra_consensus.C_driver.engine.Octra_consensus.C_engine.vs.validators
  |> List.map (fun v ->
       v.Octra_consensus.C_types.address,
       Base64.encode_exn v.Octra_consensus.C_types.pubkey)

let validator_pubkeys ~driver ~fallback =
  match driver with
  | Some driver -> validator_pubkeys_from_driver driver
  | None -> fallback ()

let epoch_exec_env ~chain_id ~epoch_id ~epoch_ts ~proposer ~validator_pubkeys
    ~prev_state_root ~ready_state_root_at ~ready_max_lag =
  Octra_core.Epoch_exec.{
    chain_id;
    epoch_id = Int64.to_int epoch_id;
    proposer_addr = proposer;
    validator_addrs = List.map fst validator_pubkeys;
    validator_pubkeys;
    prev_state_root;
    epoch_ts;
    ready_state_root_at = Some ready_state_root_at;
    ready_max_lag;
  }

let wait_for_prev_root deps ~target_root ~max_tries ~delay_seconds =
  let open Lwt.Syntax in
  let* initial_root = deps.read_root () in
  let* current_root =
    if initial_root = target_root then
      Lwt.return initial_root
    else
      let* () = deps.replay_stashed () in
      deps.read_root ()
  in
  let rec loop tries_left current_root =
    if current_root = target_root || tries_left <= 0 then
      Lwt.return { initial_root; current_root; tries_left }
    else
      let* () = deps.sleep delay_seconds in
      let* next_root = deps.read_root () in
      loop (tries_left - 1) next_root
  in
  loop max_tries current_root

let log_reject ~epoch_id = function
  | Missing_txs { have; need } ->
    Octra_log.warn "consensus"
      "reject proposal reason = missing_txs have = %d need = %d"
      have need
  | Disabled_operation { hash; op_type } ->
    Octra_log.warn "consensus"
      "reject proposal reason = disabled_operation hash = %s op_type = %s"
      hash op_type
  | Underpriced_transaction { hash; op_type; provided; required } ->
    Octra_log.warn "consensus"
      "reject proposal reason = underpriced_transaction hash = %s op_type = %s provided = %s required = %s"
      hash
      op_type
      (Z.to_string provided)
      (Z.to_string required)
  | Receipt_root_mismatch ->
    Octra_log.warn "consensus"
      "reject proposal reason = receipt_root_mismatch"
  | Receipt_decode_failed e ->
    Octra_log.warn "consensus"
      "reject proposal reason = receipt_decode_failed error = %s"
      e
  | Preverify_gate_failed e ->
    Octra_log.warn "consensus"
      "reject proposal reason = preverify_gate_failed error = %s"
      e
  | Bundle_limit { totals; limits } ->
    Octra_log.warn "consensus"
      "reject proposal reason = bundle_limit epoch = %Ld txs = %d/%d bytes = %d/%d ou = %s/%s"
      epoch_id
      totals.count limits.max_txs
      totals.bytes limits.max_bytes
      (Z.to_string totals.ou)
      (Z.to_string limits.max_ou)
  | Invalid_tx_signature { hash; from_addr } ->
    Octra_log.warn "consensus"
      "reject proposal reason = invalid_tx_signature hash = %s from = %s"
      hash from_addr

let preview_eic_root ~epoch_id ~tx_hashes ~start_txid ~prev_eic_root =
  let _, root =
    Octra_core.Epoch_index_commitment.next_root_from_hashes
      ~prev:prev_eic_root
      ~epoch_id:(Int64.to_int epoch_id)
      ~start_txid
      tx_hashes
  in
  root

let preview_ledger_root ~tx_count ~local_ledger_root ~post_state_root =
  if String.length post_state_root > 0 then
    Some post_state_root
  else if tx_count = 0 then
    Some local_ledger_root
  else
    None

let preview_decision ~root_to_raw32 ~epoch_id ~tx_hashes ~tx_count
    ~start_txid ~prev_eic_root ~local_ledger_root ~proposed_state_root
    ~preview =
  match preview with
  | Preview_error e ->
    Preview_error_reject e
  | Preview_ok { post_state_root } ->
    let preview_eic_root =
      preview_eic_root ~epoch_id ~tx_hashes ~start_txid ~prev_eic_root
    in
    let computed_root =
      match
        preview_ledger_root
          ~tx_count
          ~local_ledger_root
          ~post_state_root
      with
      | Some ledger_root ->
        let consensus_root =
          Octra_core.Epoch_index_commitment.folded_state_root
            ~ledger_state_root:ledger_root
            ~epoch_index_root:preview_eic_root
        in
        Some (root_to_raw32 consensus_root)
      | None ->
        None
    in
    match computed_root with
    | Some root when root = proposed_state_root ->
      Preview_accept {
        computed_root = root;
        preview_eic_root;
      }
    | _ ->
      Preview_root_mismatch {
        expected_root = proposed_state_root;
        computed_root;
        preview_eic_root;
      }

let preview_status_of_result = function
  | Stdlib.Ok res ->
    Preview_ok {
      post_state_root = res.Octra_core.Epoch_exec.post_state_root;
    }
  | Stdlib.Error e ->
    Preview_error e

let prev_eic_root_from_head = function
  | Some head ->
    (match head.Octra_core.Head_manifest.ledger_state_root,
           head.Octra_core.Head_manifest.epoch_index_root with
     | Some _, Some root -> root
     | _ -> Octra_core.Epoch_index_commitment.genesis_root)
  | None -> Octra_core.Epoch_index_commitment.genesis_root

let proposal_roots ~root_to_raw32 ~ledger_head ~cached_head =
  let zero = String.make 32 '\x00' in
  let prev_ledger_root =
    match ledger_head with
    | Some h when String.length h > 0 -> root_to_raw32 h
    | _ -> zero
  in
  let prev_state_root =
    match cached_head with
    | Some h ->
      let root = h.Octra_core.Head_manifest.state_root in
      if String.length root > 0 then root_to_raw32 root else prev_ledger_root
    | None -> prev_ledger_root
  in
  {
    prev_ledger_root;
    prev_state_root;
  }

let prev_root_decision ~epoch_id ~target_root ~current_root ~max_wait_tries
    ~tries_left ~current_streak ~quarantine_threshold =
  if target_root = current_root then
    Prev_root_match
  else
    let streak_after = current_streak + 1 in
    let quarantine_reason =
      if streak_after >= quarantine_threshold then
        Some (
          Printf.sprintf
            "prev_state_root_mismatch_streak = %d epoch = %Ld"
            streak_after
            epoch_id
        )
      else
        None
    in
    Prev_root_mismatch {
      waited_steps = max 0 (max_wait_tries - tries_left);
      streak_after;
      quarantine_reason;
    }

let build_preview_plan ~root_to_raw32 ~epoch_id ~start_txid ~prev_eic_root
    ~prev_ledger_root ~fallback_ledger_root ~input_txs:_ ~preview =
  match preview with
  | Build_preview_error e ->
    {
      final_txs = [];
      final_hashes = [];
      rejected_hashes = [];
      proposed_state_root = fallback_ledger_root;
      preview_consensus_root = None;
      preview_error = Some e;
    }
  | Build_preview_ok { confirmed; rejected = _ :: _ as rejected; _ } ->
    {
      final_txs = confirmed;
      final_hashes = List.map Transaction.hash confirmed;
      rejected_hashes = List.map Transaction.hash rejected;
      proposed_state_root = fallback_ledger_root;
      preview_consensus_root = None;
      preview_error = Some "preview_rejected_transactions";
    }
  | Build_preview_ok { post_state_root; confirmed; rejected } ->
    let final_hashes = List.map Transaction.hash confirmed in
    let rejected_hashes = List.map Transaction.hash rejected in
    let preview_eic_root =
      preview_eic_root
        ~epoch_id
        ~tx_hashes:final_hashes
        ~start_txid
        ~prev_eic_root
    in
    let preview_ledger_root =
      if String.length post_state_root > 0 then
        post_state_root
      else if confirmed = [] then
        prev_ledger_root
      else
        fallback_ledger_root
    in
    let consensus_root =
      Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:preview_ledger_root
        ~epoch_index_root:preview_eic_root
    in
    {
      final_txs = confirmed;
      final_hashes;
      rejected_hashes;
      proposed_state_root = root_to_raw32 consensus_root;
      preview_consensus_root = Some consensus_root;
      preview_error = None;
    }

let build_preview_status_of_result = function
  | Stdlib.Ok res ->
    let artifacts = res.Octra_core.Epoch_exec.artifacts in
    Build_preview_ok {
      post_state_root = res.Octra_core.Epoch_exec.post_state_root;
      confirmed = List.map fst artifacts.confirmed;
      rejected =
        List.map
          (fun (r : Octra_core.Epoch_exec.tx_reject) -> r.tx)
          artifacts.rejected;
    }
  | Stdlib.Error e ->
    Build_preview_error e

let build_preview_output ~root_to_raw32 ~epoch_id ~start_txid ~prev_eic_root
    ~prev_ledger_root ~fallback_ledger_root ~input_txs ~batch
    ~rejections ~preview_result =
  let plan =
    build_preview_plan
      ~root_to_raw32
      ~epoch_id
      ~start_txid
      ~prev_eic_root
      ~prev_ledger_root
      ~fallback_ledger_root
      ~input_txs
      ~preview:(build_preview_status_of_result preview_result)
  in
  {
    plan;
    receipts_json =
      Octra_core.Tx_outcome.encode
        (Octra_core.Preverify_worker.receipt_json_for_hashes
           batch.Octra_core.Preverify_worker.ready
           plan.final_hashes)
        rejections;
    rejected_count = List.length rejections;
  }

type preview_partition =
  | Preview_partition_stable of Transaction.t list
  | Preview_partition_retry of {
      confirmed : Transaction.t list;
      rejections : Octra_core.Epoch_exec.tx_reject list;
    }
  | Preview_partition_invalid

let sorted_hashes txs =
  List.map Transaction.hash txs
  |> List.sort String.compare

let distinct hashes =
  List.length hashes = List.length (List.sort_uniq String.compare hashes)

let preview_partition ~input_txs ~confirmed ~rejected =
  let input_hashes = sorted_hashes input_txs in
  let confirmed_hashes = sorted_hashes confirmed in
  let rejected_txs =
    List.map (fun (item : Octra_core.Epoch_exec.tx_reject) -> item.tx) rejected
  in
  let rejected_hashes = sorted_hashes rejected_txs in
  let output_hashes =
    List.sort String.compare (confirmed_hashes @ rejected_hashes)
  in
  if
    not (distinct input_hashes)
    || not (distinct output_hashes)
    || input_hashes <> output_hashes
  then
    Preview_partition_invalid
  else
    match rejected with
    | [] -> Preview_partition_stable confirmed
    | _ ->
      Preview_partition_retry {
        confirmed;
        rejections = rejected;
      }

let rejection_tuples rejections =
  List.map
    (fun (item : Octra_core.Epoch_exec.tx_reject) ->
       item.tx, item.error_type, item.reason)
    rejections

let verify_preview_partition ~candidates ~confirmed ~rejections = function
  | Stdlib.Error error -> Error ("preview_failed:" ^ error)
  | Stdlib.Ok result ->
    let artifacts = result.Octra_core.Epoch_exec.artifacts in
    let actual_confirmed = List.map fst artifacts.confirmed in
    if
      List.map Transaction.hash actual_confirmed
      <> List.map Transaction.hash confirmed
    then
      Error "preview_confirmed_mismatch"
    else
      match
        Octra_core.Tx_outcome.build
          ~candidates
          (rejection_tuples artifacts.rejected)
      with
      | Error error -> Error ("preview_outcome_invalid:" ^ error)
      | Ok actual ->
        if Octra_core.Tx_outcome.equal actual rejections then Ok ()
        else Error "preview_rejection_mismatch"

let raw32_to_hex r =
  Text.hash32_hex r

let raw_hex_prefix n s =
  String.concat ""
    (List.init (min n (String.length s))
       (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let raw_hex8 s =
  raw_hex_prefix 8 s

let short_or_unknown n s =
  if String.length s >= n then
    String.sub s 0 n
  else
    "?"

let layera_validator_addrs ~env ~fallback =
  match env with
  | Some s ->
    String.split_on_char ',' s
    |> List.filter_map (fun e ->
      match String.split_on_char ':' e with
      | addr :: _ when String.length addr > 3 -> Some addr
      | _ -> None)
    |> List.sort String.compare
  | None ->
    [fallback]

let layera_diag_context ~validator_addrs ~hash_validators ~meta =
  {
    validator_addrs;
    validators_sha = hash_validators (String.concat "," validator_addrs);
    meta_current = meta "current_epoch";
    meta_last = meta "last_epoch";
    meta_supply = meta "total_supply";
    meta_emission = meta "emission_remaining";
  }

let layera_env_diag_context ~env ~fallback ~hash_validators ~meta =
  layera_diag_context
    ~validator_addrs:(layera_validator_addrs ~env ~fallback)
    ~hash_validators
    ~meta

let log_layera_prev_mismatch ~epoch_id ~round ~leader ~target_root ~our_root
    ~initial_root ~waited_steps ctx =
  Octra_log.error "consensus"
    "LAYERA_PREV_MISMATCH epoch = %Ld round = %d leader = %s validators_n = %d validators_sha = %s target = %s our = %s initial = %s waited = %dx0.05s meta[current = %s,last = %s,supply = %s,emission = %s]"
    epoch_id
    round
    (short_or_unknown 14 leader)
    (List.length ctx.validator_addrs)
    (raw_hex8 ctx.validators_sha)
    (raw_hex8 target_root)
    (raw_hex8 our_root)
    (raw_hex8 initial_root)
    waited_steps
    ctx.meta_current
    ctx.meta_last
    ctx.meta_supply
    ctx.meta_emission

let log_layera_preview_mismatch ~epoch_id ~round ~proposer ~tx_count
    ~expected_root ~computed_root ~tx_list_hash ~tx_hashes ctx =
  let sample_hashes = first_three (short 12) tx_hashes in
  Octra_log.error "consensus"
    "LAYERA_PREVIEW_MISMATCH epoch = %Ld round = %d proposer = %s tx_count = %d validators_n = %d validators_sha = %s expected = %s computed = %s tx_list_hash = %s sample_txs = %s meta[current = %s,last = %s,supply = %s,emission = %s]"
    epoch_id
    round
    (short_or_unknown 14 proposer)
    tx_count
    (List.length ctx.validator_addrs)
    (raw_hex8 ctx.validators_sha)
    (raw_hex8 expected_root)
    (match computed_root with Some root -> raw_hex8 root | None -> "-")
    (raw_hex8 tx_list_hash)
    sample_hashes
    ctx.meta_current
    ctx.meta_last
    ctx.meta_supply
    ctx.meta_emission

let log_layera_preview_error ~epoch_id ~round ~proposer ~error ctx =
  Octra_log.error "consensus"
    "LAYERA_PREVIEW_ERROR epoch = %Ld round = %d proposer = %s validators_n = %d validators_sha = %s err = %s"
    epoch_id
    round
    (short_or_unknown 14 proposer)
    (List.length ctx.validator_addrs)
    (raw_hex8 ctx.validators_sha)
    error

let log_build_preverify ~epoch_id ~round ~limits ~staging_total ~raw_count
    ~admitted_count ~prev_root shaped =
  let capped = shaped.capped in
  Octra_log.info "consensus"
    "make_proposal epoch = %Ld round = %d staging_total = %d epoch_txs_raw = %d after_consensus_filter = %d heavy = %d preverify_ready = %d preverify_skipped = %d final_txs = %d receipts = %d skipped_by_limit = %d bytes = %d/%d ou = %s/%s prev_root = %s"
    epoch_id
    round
    staging_total
    raw_count
    admitted_count
    shaped.heavy_count
    (List.length shaped.receipts)
    shaped.skipped_count
    (List.length shaped.txs)
    (List.length shaped.receipts)
    capped.skipped
    capped.totals.bytes
    limits.max_bytes
    (Z.to_string capped.totals.ou)
    (Z.to_string limits.max_ou)
    (raw_hex8 prev_root)

let log_build_skipped ~epoch_id shaped =
  if shaped.skipped_count > 0 then
    Octra_log.warn "consensus"
      "make_proposal skipped_heavy_preverify_txs = %d epoch = %Ld sample = %s"
      shaped.skipped_count
      epoch_id
      shaped.skipped_sample

let log_preview_result ~epoch_id final_tx_count = function
  | { preview_consensus_root = Some root; preview_error = None; _ } ->
    Octra_log.info "consensus"
      "preview epoch = %Ld consensus_root = %s txs = %d"
      epoch_id
      (short 16 root)
      final_tx_count
  | { preview_error = Some e; _ } ->
    Octra_log.warn "consensus" "preview error = %s action = use_prev_root" e
  | _ -> ()

let tx_list_hash tx_hashes =
  Octra_net.Hash_domain.hash "octra:tx_list:v1" (String.concat "" tx_hashes)

let tx_hash_admission ~expected_tx_list_hash ~tx_hashes =
  if expected_tx_list_hash = tx_list_hash tx_hashes then
    Tx_hash_ok tx_hashes
  else
    Tx_hash_mismatch

let proposal_id_short proposal_id =
  short 16 (Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string proposal_id))

let sha256_raw_hex raw =
  Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string raw)

let frozen_proposal (frozen : Consensus_bundle_cache.frozen) =
  let header = frozen.header in
  let proposal_id = Octra_consensus.C_hash.proposal_id header in
  {
    header;
    tx_hashes = frozen.tx_hashes;
    txs = frozen.txs;
    receipts_json = frozen.receipts_json;
    proposal_id;
    proposal_id_short = proposal_id_short proposal_id;
  }

let log_frozen_proposal ~epoch_id ~round (frozen : frozen_proposal) =
  Octra_log.info "consensus"
    "make_proposal epoch = %Ld round = %d using frozen bundle txs = %d pid = %s"
    epoch_id
    round
    (List.length frozen.txs)
    frozen.proposal_id_short

let proposal_txid_hi ~final_count ~next_txid ~head_txid_hi =
  if final_count = 0 then
    match head_txid_hi with
    | Some txid -> txid
    | None -> Int64.sub next_txid 1L
  else
    Int64.add next_txid (Int64.of_int (final_count - 1))

let build_header ~chain_id ~epoch_id ~prev_state_root ~final_hashes
    ~receipts_json ~proposed_state_root ~creator_addr ~next_txid
    ~head_txid_hi ~parent_commit_hash ~ts =
  let proto_version =
    Octra_consensus.C_protocol.version_for_epoch epoch_id
  in
  let parent_commit_hash =
    if
      proto_version
      = Octra_consensus.C_types.proto_version_parent_legacy
    then
      if Octra_net.Hash_domain.is_nil parent_commit_hash then
        parent_commit_hash
      else
        invalid_arg "legacy proposal has parent commit"
    else
      parent_commit_hash
  in
  Octra_consensus.C_types.{
    proto_version;
    chain_id;
    epoch_id;
    prev_state_root;
    tx_list_hash = tx_list_hash final_hashes;
    receipt_root = Octra_consensus.C_hash.receipt_root receipts_json;
    proposed_state_root;
    parent_commit_hash;
    creator_addr;
    txid_hi =
      proposal_txid_hi
        ~final_count:(List.length final_hashes)
        ~next_txid
        ~head_txid_hi;
    ts;
  }

let build_proposal_envelope ~chain_id ~epoch_id ~prev_state_root
    ~final_hashes ~final_txs ~receipts_json ~proposed_state_root
    ~creator_addr ~next_txid ~head_txid_hi ~parent_commit ~ts =
  let proto_version =
    Octra_consensus.C_protocol.version_for_epoch epoch_id
  in
  let parent_commit =
    if
      proto_version
      = Octra_consensus.C_types.proto_version_parent_legacy
    then
      match parent_commit with
      | None -> None
      | Some _ -> invalid_arg "legacy proposal has parent commit"
    else
      parent_commit
  in
  let parent_commit_hash =
    Octra_consensus.C_hash.parent_commit_hash_opt parent_commit
  in
  let header =
    build_header
      ~chain_id
      ~epoch_id
      ~prev_state_root
      ~final_hashes
      ~receipts_json
      ~proposed_state_root
      ~creator_addr
      ~next_txid
      ~head_txid_hi
      ~parent_commit_hash
      ~ts
  in
  {
    header;
    parent_commit;
    proposal_id = Octra_consensus.C_hash.proposal_id header;
    txid_hi = header.Octra_consensus.C_types.txid_hi;
    tx_hashes = final_hashes;
    txs = final_txs;
    receipts_json;
    frozen_bundle =
      {
        Consensus_bundle_cache.header = header;
        tx_hashes = final_hashes;
        txs = final_txs;
        receipts_json;
      };
  }

let precommit_sync_plan ~proposal_id ~current_tx_hashes ~cached_bundle =
  let pid_short = proposal_id_short proposal_id in
  match cached_bundle with
  | None ->
    Precommit_sync_missing { pid_short }
  | Some raw ->
    match Consensus_bundle_cache.decode raw with
    | Ok bundle ->
      if current_tx_hashes = bundle.tx_hashes then
        Precommit_sync_current
      else
        Precommit_sync_decoded {
          pid_short;
          tx_hashes = bundle.tx_hashes;
          txs = bundle.txs;
          receipts_json = bundle.receipts_json;
        }
    | Error error ->
      Precommit_sync_decode_failed {
        pid_short;
        error;
      }

let pending_commit ~epoch_id ~round ~proposal_id ~proposed_state_root ~txid_hi
    ~ts ~validator_addr ~proposal_wire ~vote_wire ~tx_hashes ~txs_json
    ~receipts_json =
  Octra_core.Wal.{
    epoch_id = Int64.to_int epoch_id;
    round;
    proposal_id = sha256_raw_hex proposal_id;
    proposed_state_root = sha256_raw_hex proposed_state_root;
    txid_hi;
    ts;
    validator_addr;
    proposal_b64 = Some (Base64.encode_exn proposal_wire);
    vote_b64 = Some (Base64.encode_exn vote_wire);
    tx_hashes;
    txs_json;
    receipts_json;
  }

let precommit_record_matches ~chain_id ~validator_set ~epoch_id ~round ~proposal_id
    ~proposed_state_root ~txid_hi ~validator_addr ~proposal_wire ~vote_wire
    (tx_hashes, _txs_json, receipts_json) =
  try
    let proposal = Octra_consensus.C_codec.decode_propose proposal_wire in
    let vote = Octra_consensus.C_codec.decode_vote vote_wire in
    let proposal_hash =
      Octra_consensus.C_hash.proposal_id proposal.header
    in
    let proposal_hashes = List.map Text.hash32_hex proposal.tx_hashes in
    if proposal.epoch_id <> epoch_id then Error "proposal epoch mismatch"
    else if proposal.round <> round then Error "proposal round mismatch"
    else if not
      (Octra_consensus.C_types.proposal_is_well_formed
         ~chain_id
         ~validator_set
         proposal)
    then
      Error "proposal envelope mismatch"
    else if proposal_hash <> proposal_id then Error "proposal id mismatch"
    else if proposal.header.proposed_state_root <> proposed_state_root then
      Error "proposal state root mismatch"
    else if proposal.header.txid_hi <> txid_hi then Error "proposal txid mismatch"
    else if proposal_hashes <> tx_hashes then Error "proposal bundle mismatch"
    else if
      Octra_consensus.C_engine.tx_list_hash_for_header tx_hashes
      <> proposal.header.tx_list_hash
    then
      Error "proposal tx list root mismatch"
    else if
      Octra_consensus.C_hash.receipt_root receipts_json
      <> proposal.header.receipt_root
    then
      Error "proposal receipt root mismatch"
    else if
      Octra_consensus.C_hash.parent_commit_hash_opt proposal.parent_commit
      <> proposal.header.parent_commit_hash
    then
      Error "proposal parent commit mismatch"
    else if vote.chain_id <> proposal.chain_id then Error "vote chain mismatch"
    else if vote.epoch_id <> epoch_id then Error "vote epoch mismatch"
    else if vote.round <> round then Error "vote round mismatch"
    else if vote.vote_type <> Octra_consensus.C_types.Precommit then
      Error "vote type mismatch"
    else if vote.proposal_id <> proposal_id then Error "vote proposal mismatch"
    else if vote.validator <> validator_addr then Error "vote validator mismatch"
    else
      Ok ()
  with exn ->
    Error (Printexc.to_string exn)

let handle_before_precommit deps ~epoch_id ~round ~proposal_id
    ~proposed_state_root ~txid_hi ~validator_addr ~proposal_wire ~vote_wire =
  let cached_bundle = deps.cached_bundle proposal_id in
  let plan =
    precommit_sync_plan
      ~proposal_id
      ~current_tx_hashes:(deps.current_tx_hashes ())
      ~cached_bundle
  in
  match plan, cached_bundle with
  | (Precommit_sync_current
    | Precommit_sync_decoded _),
    Some ((tx_hashes, txs_json, receipts_json) as bundle) ->
    (match
       precommit_record_matches
         ~chain_id:deps.chain_id
         ~validator_set:(deps.validator_set ())
         ~epoch_id
         ~round
         ~proposal_id
         ~proposed_state_root
         ~txid_hi
         ~validator_addr
         ~proposal_wire
         ~vote_wire
         bundle
     with
     | Error error ->
       deps.mark_unsynced ();
       Octra_log.error "consensus"
         "event = refuse_precommit reason = record_mismatch error = %s"
         error;
       false
     | Ok () ->
       (match plan with
        | Precommit_sync_decoded { pid_short; tx_hashes; txs; receipts_json } ->
          deps.sync_bundle ~tx_hashes ~txs ~receipts_json;
          Octra_log.info "consensus"
            "event = before_precommit bundle = synced pid = %s n = %d"
            pid_short
            (List.length tx_hashes)
        | _ -> ());
       let entry =
         pending_commit
           ~epoch_id
           ~round
           ~proposal_id
           ~proposed_state_root
           ~txid_hi
           ~ts:(deps.now ())
           ~validator_addr
           ~proposal_wire
           ~vote_wire
           ~tx_hashes
           ~txs_json
           ~receipts_json
       in
       (try
          deps.write_pending entry;
          Octra_log.info "consensus"
            "event = pending_commit durable = true epoch = %Ld round = %d pid = %s txid_hi = %Ld"
            epoch_id
            round
            (short 16 entry.proposal_id)
            txid_hi;
          true
        with exn ->
          deps.mark_unsynced ();
          Octra_log.error "consensus"
            "event = refuse_precommit reason = pending_commit_write error = %s"
            (Printexc.to_string exn);
          false))
  | Precommit_sync_decode_failed { pid_short; error }, _ ->
    deps.mark_unsynced ();
    Octra_log.error "consensus"
      "event = refuse_precommit reason = bundle_decode pid = %s error = %s"
      pid_short
      error;
    false
  | Precommit_sync_missing { pid_short }, _ ->
    deps.mark_unsynced ();
    Octra_log.warn "consensus"
      "event = refuse_precommit reason = bundle_missing pid = %s"
      pid_short;
    false
  | _ ->
    deps.mark_unsynced ();
    Octra_log.error "consensus"
      "event = refuse_precommit reason = bundle_state";
    false

let admission_plan ~epoch_id ~current_epoch ~state_attested ~quarantine_active
    ~quarantine_reason =
  if not state_attested then
    Defer_state_not_attested
  else if quarantine_active then
    Defer_quarantine { reason = quarantine_reason }
  else
    let target_local_epoch = Int64.to_int epoch_id in
    if target_local_epoch < current_epoch then
      Realign_stale_height { target_epoch = Int64.of_int current_epoch }
    else if target_local_epoch > current_epoch then
      Defer_apply_gap
    else
      Proceed

let handle_proposal_admission ~start_height ~epoch_id ~current_epoch
    ~state_attested ~quarantine_active ~quarantine_reason =
  let open Lwt.Syntax in
  match
    admission_plan
      ~epoch_id
      ~current_epoch
      ~state_attested
      ~quarantine_active
      ~quarantine_reason
  with
  | Defer_state_not_attested ->
    Octra_log.warn "consensus"
      "defer proposal epoch = %Ld local_current_epoch = %d reason = state_not_attested"
      epoch_id current_epoch;
    Lwt.return Proposal_defer
  | Defer_quarantine { reason } ->
    Octra_log.warn "consensus"
      "defer proposal epoch = %Ld local_current_epoch = %d reason = self_quarantine quarantine_reason = %s"
      epoch_id current_epoch reason;
    Lwt.return Proposal_defer
  | Realign_stale_height { target_epoch } ->
    Octra_log.warn "consensus"
      "defer stale proposal epoch = %Ld local_current_epoch = %d action = realign_height target_epoch = %Ld"
      epoch_id current_epoch target_epoch;
    let* () = start_height target_epoch in
    Lwt.return Proposal_defer
  | Defer_apply_gap ->
    Octra_log.warn "consensus"
      "defer proposal epoch = %Ld local_current_epoch = %d reason = apply_not_caught_up"
      epoch_id current_epoch;
    Lwt.return Proposal_defer
  | Proceed ->
    Lwt.return Proposal_admit

let verify_proposal deps ~chain_id:_ (propose : Octra_consensus.C_types.propose) =
  let open Lwt.Syntax in
  let open Octra_consensus.C_types in
  match
    deps.verify_parent_commit
      ~epoch_id:propose.epoch_id
      propose.parent_commit
  with
  | Error reason ->
    Octra_log.warn "consensus"
      "reject proposal reason = parent_commit detail = %s"
      reason;
    Lwt.return_false
  | Ok () ->
  let previous =
    if propose.epoch_id <= 0L then Ok None
    else
      match deps.previous_epoch_ts (Int64.pred propose.epoch_id) with
      | None -> Error "previous epoch time unavailable"
      | Some value ->
        begin
          match Octra_consensus.Epoch_time.of_seconds value with
          | Ok time -> Ok (Some time)
          | Error reason -> Error reason
        end
  in
  match previous with
  | Error reason ->
    Octra_log.warn "consensus"
      "reject proposal reason = invalid_epoch_time detail = %s"
      reason;
    Lwt.return_false
  | Ok previous ->
    let epoch_time_check =
      if not
        (valid_round_is_well_formed
           ~round:propose.round
           propose.valid_round)
      then
        Error "proposal valid round is invalid"
      else
        match propose.valid_round with
        | None ->
          Octra_consensus.Epoch_time.check
            ~now:(deps.now ())
            ~previous
            ~candidate:propose.header.ts
        | Some _ ->
          Octra_consensus.Epoch_time.check_reproposal
            ~previous
            ~candidate:propose.header.ts
    in
    match epoch_time_check with
  | Error reason ->
    Octra_log.warn "consensus"
      "reject proposal reason = invalid_epoch_time detail = %s"
      reason;
    Lwt.return_false
  | Ok _ when deps.quarantine_active () -> begin
      Octra_log.warn "consensus"
        "reject proposal reason = self_quarantine epoch = %Ld detail = %s"
        propose.epoch_id
        (deps.quarantine_reason ());
      Lwt.return_false
    end
  | Ok _ ->
    let target = propose.header.prev_state_root in
    let* root_sample =
      wait_for_prev_root
        deps.wait_prev_root
        ~target_root:target
        ~max_tries:deps.max_prev_root_wait_tries
        ~delay_seconds:deps.prev_root_wait_delay_seconds
    in
    let initial_our = root_sample.initial_root in
    let our_root = root_sample.current_root in
    match
      prev_root_decision
        ~epoch_id:propose.epoch_id
        ~target_root:target
        ~current_root:our_root
        ~max_wait_tries:deps.max_prev_root_wait_tries
        ~tries_left:root_sample.tries_left
        ~current_streak:(deps.prev_root_streak ())
        ~quarantine_threshold:deps.quarantine_mismatch_threshold
    with
    | Prev_root_mismatch { waited_steps; streak_after; quarantine_reason } ->
      deps.set_prev_root_streak streak_after;
      Option.iter deps.mark_quarantine quarantine_reason;
      if deps.layera_diag_live () then
        log_layera_prev_mismatch
          ~epoch_id:propose.epoch_id
          ~round:propose.round
          ~leader:propose.header.creator_addr
          ~target_root:target
          ~our_root
          ~initial_root:initial_our
          ~waited_steps
          (deps.layera_env_diag_context ());
      Octra_log.warn "consensus"
        "reject proposal reason = prev_state_root_mismatch epoch = %Ld leader = %s target = %s our = %s initial = %s wait_steps = %d wait_delay_sec = %.2f"
        propose.epoch_id
        (short_or_unknown 14 propose.header.creator_addr)
        (raw_hex8 target)
        (raw_hex8 our_root)
        (raw_hex8 initial_our)
        waited_steps
        deps.prev_root_wait_delay_seconds;
      Lwt.return_false
    | Prev_root_match ->
      let tx_hashes_hex = List.map raw32_to_hex propose.tx_hashes in
      match
        tx_hash_admission
          ~expected_tx_list_hash:propose.header.tx_list_hash
          ~tx_hashes:tx_hashes_hex
      with
      | Tx_hash_mismatch ->
        Octra_log.warn "consensus"
          "reject proposal reason = tx_list_hash_mismatch";
        Lwt.return_false
      | Tx_hash_ok tx_hashes_hex ->
        let pid = Octra_consensus.C_hash.proposal_id propose.header in
        let staging_snapshot = deps.staging_txs () in
        let tx_list_local =
          List.filter_map
            (fun hash ->
              List.find_opt
                (fun tx -> Transaction.hash tx = hash)
                staging_snapshot)
            tx_hashes_hex
        in
        let missing_count =
          List.length tx_hashes_hex - List.length tx_list_local
        in
        let local_preverify txs =
          let run_many txs =
            deps.validate_preverify_once
              ~state_root:propose.header.prev_state_root
              ~tx_hashes:tx_hashes_hex
              txs
          in
          let* shaped =
            local_preverify_bundle
              ~run_many
              ~tx_hashes:tx_hashes_hex
              txs
          in
          if shaped.skipped_count > 0 then
            Octra_log.warn "consensus"
              "verify_proposal local_preverify_skipped = %d sample = %s"
              shaped.skipped_count
              shaped.skipped_sample;
          Lwt.return Consensus_bundle_fetch.{
            txs = shaped.ready_txs;
            receipts_json = shaped.receipts_json;
            rejections = [];
          }
        in
        let proposal_id_short = proposal_id_short pid in
        let* proposal_bundle =
          Consensus_bundle_fetch.ensure_proposal
            {
              cached_proposal_bundle = (fun () ->
                deps.cached_bundle ~proposal_id:pid);
              local_preverify_bundle = (fun () ->
                local_preverify tx_list_local);
              local_bundle_valid = (fun bundle ->
                receipt_root_matches
                  ~header:propose.header
                  bundle.Consensus_bundle_fetch.receipts_json);
              driver_available = deps.driver_available;
              validate_bundle = (fun response ->
                deps.validate_bundle
                  ~header:propose.header
                  ~expected_hashes:tx_hashes_hex
                  response);
              query_bundle = (fun ~validate ->
                deps.query_bundle
                  ~epoch_id:propose.epoch_id
                  ~proposal_id:pid
                  ~validate);
              store_proposal_bundle = (fun accepted ->
                let bundle = accepted.Consensus_bundle_fetch.bundle in
                deps.store_bundle
                  ~proposal_id:pid
                  ~tx_hashes:tx_hashes_hex
                  ~txs:bundle.txs
                  ~receipts_json:bundle.receipts_json);
            }
            ~epoch_id:propose.epoch_id
            ~proposal_id_short
            ~expected_hash_count:(List.length tx_hashes_hex)
            ~local_tx_count:(List.length tx_list_local)
            ~missing_count
            ~hashes_empty:
              (Consensus_bundle_cache.header_has_empty_bundle propose.header)
        in
        let tx_list = proposal_bundle.Consensus_bundle_fetch.txs in
        let receipts_json = proposal_bundle.receipts_json in
        match
            verify_bundle
              {
                public_key_for_tx = deps.public_key_for_tx;
                verify_address_pubkey = deps.verify_address_pubkey;
                verify_tx_signature = deps.verify_tx_signature;
              }
              ~limits:deps.limits
              ~header:propose.header
              ~expected_tx_count:(List.length propose.tx_hashes)
              tx_list
              receipts_json
        with
        | Error reason ->
          log_reject ~epoch_id:propose.epoch_id reason;
          Lwt.return_false
        | Ok verified ->
          let candidate_hashes = List.map Transaction.hash verified.candidates in
          let* local_candidates =
            local_preverify_bundle
              ~run_many:(fun txs ->
                deps.validate_preverify_once
                  ~state_root:propose.header.prev_state_root
                  ~tx_hashes:candidate_hashes
                  txs)
              ~tx_hashes:candidate_hashes
              verified.candidates
          in
          if local_candidates.skipped_count > 0 then
            Octra_log.warn "consensus"
              "verify_proposal candidate_preverify_skipped = %d sample = %s"
              local_candidates.skipped_count
              local_candidates.skipped_sample;
          let local_candidate_hashes =
            List.map Transaction.hash local_candidates.ready_txs
          in
          if local_candidate_hashes <> candidate_hashes then begin
            Octra_log.warn "consensus"
              "reject proposal reason = candidate_preverify_mismatch epoch = %Ld"
              propose.epoch_id;
            Lwt.return_false
          end else
            let local = {
              local_candidates with
              ready_txs = verified.txs;
              receipts_json =
                Octra_core.Preverify_worker.receipt_json_for_hashes
                  local_candidates.batch.ready
                  tx_hashes_hex;
            } in
            match
              check_local_bundle
                ~expected_hashes:tx_hashes_hex
                proposal_bundle
                local
            with
            | Error reason ->
              Octra_log.warn "consensus"
                "reject proposal reason = local_preverify_mismatch epoch = %Ld detail = %s"
                propose.epoch_id
                reason;
              Lwt.return_false
            | Ok _ ->
            let tx_list = verified.txs in
            let receipts_json = verified.receipts_json in
            let preverify = verified.preverify in
            deps.store_bundle
              ~proposal_id:pid
              ~tx_hashes:tx_hashes_hex
              ~txs:tx_list
              ~receipts_json;
            let proposer = propose.header.creator_addr in
            let validator_pubkeys = deps.validator_pubkeys propose.epoch_id in
            let validator_addrs = List.map fst validator_pubkeys in
            let* local_ledger_root_for_preview = deps.read_local_ledger_root () in
            let candidate_preverify =
              Octra_core.Preverify_commit.create
                (Octra_core.Preverify_worker.receipts_for_hashes
                   local_candidates.batch.ready
                   candidate_hashes)
            in
            let* rejection_partition =
              match verified.rejections with
              | [] -> Lwt.return_ok ()
              | rejections ->
                let* result =
                  deps.preview {
                    epoch_id = propose.epoch_id;
                    epoch_ts = propose.header.ts;
                    proposal_id =
                      Printf.sprintf "verify-outcomes-%Ld" propose.epoch_id;
                    expected_prev_root = local_ledger_root_for_preview;
                    prev_state_root = our_root;
                    parent_commit = propose.parent_commit;
                    proposer;
                    validator_pubkeys;
                    preverify = candidate_preverify;
                    txs = verified.candidates;
                  }
                in
                Lwt.return
                  (verify_preview_partition
                     ~candidates:verified.candidates
                     ~confirmed:tx_list
                     ~rejections
                     result)
            in
            match rejection_partition with
            | Error reason ->
              Octra_log.warn "consensus"
                "reject proposal reason = rejection_partition_mismatch epoch = %Ld detail = %s"
                propose.epoch_id
                reason;
              Lwt.return_false
            | Ok () ->
            let* preview_result =
              deps.preview {
                epoch_id = propose.epoch_id;
                epoch_ts = propose.header.ts;
                proposal_id = Printf.sprintf "verify-%Ld" propose.epoch_id;
                expected_prev_root = local_ledger_root_for_preview;
                prev_state_root = our_root;
                parent_commit = propose.parent_commit;
                proposer;
                validator_pubkeys;
                preverify;
                txs = tx_list;
              }
            in
            match
              verify_preview_partition
                ~candidates:tx_list
                ~confirmed:tx_list
                ~rejections:[]
                preview_result
            with
            | Error reason ->
              Octra_log.warn "consensus"
                "reject proposal reason = accepted_partition_mismatch epoch = %Ld detail = %s"
                propose.epoch_id
                reason;
              Lwt.return_false
            | Ok () ->
            let decision =
            preview_decision
              ~root_to_raw32:deps.root_to_raw32
              ~epoch_id:propose.epoch_id
              ~tx_hashes:tx_hashes_hex
              ~tx_count:(List.length tx_list)
              ~start_txid:(deps.next_txid ())
              ~prev_eic_root:(deps.prev_eic_root ())
              ~local_ledger_root:local_ledger_root_for_preview
              ~proposed_state_root:propose.header.proposed_state_root
              ~preview:(preview_status_of_result preview_result)
          in
            let root_matches =
            match decision with
            | Preview_accept _ ->
              Octra_log.info "consensus"
                "accept proposal epoch = %Ld root = verified txs = %d"
                propose.epoch_id
                (List.length tx_list);
              true
            | Preview_root_mismatch { computed_root; _ } ->
              if deps.layera_diag_live () then
                log_layera_preview_mismatch
                  ~epoch_id:propose.epoch_id
                  ~round:propose.round
                  ~proposer
                  ~tx_count:(List.length tx_list)
                  ~expected_root:propose.header.proposed_state_root
                  ~computed_root
                  ~tx_list_hash:propose.header.tx_list_hash
                  ~tx_hashes:tx_hashes_hex
                  (deps.layera_diag_context ~validator_addrs);
              Octra_log.warn "consensus"
                "reject proposal reason = state_root_mismatch epoch = %Ld"
                propose.epoch_id;
              false
            | Preview_error_reject error ->
              if deps.layera_diag_live () then
                log_layera_preview_error
                  ~epoch_id:propose.epoch_id
                  ~round:propose.round
                  ~proposer:propose.header.creator_addr
                  ~error
                  (deps.layera_diag_context ~validator_addrs);
              Octra_log.warn "consensus"
                "reject proposal reason = preview_failed error = %s"
                error;
              false
          in
            if root_matches then begin
              deps.set_prev_root_streak 0;
              deps.set_state_root_streak 0;
              deps.set_proposal tx_list tx_hashes_hex;
              deps.store_bundle
                ~proposal_id:pid
                ~tx_hashes:tx_hashes_hex
                ~txs:tx_list
                ~receipts_json;
              Lwt.return_true
            end else begin
              let next_streak = deps.state_root_streak () + 1 in
              deps.set_state_root_streak next_streak;
              if next_streak >= deps.quarantine_mismatch_threshold then
                deps.mark_quarantine
                  (Printf.sprintf
                     "state_root_mismatch_streak = %d epoch = %Ld"
                     next_streak
                     propose.epoch_id);
              Lwt.return_false
            end

let make_proposal deps ~chain_id ~root_to_raw32 ~limits ~epoch_id =
  let open Lwt.Syntax in
  let* proposal_admission =
    handle_proposal_admission
      ~start_height:deps.start_height
      ~epoch_id
      ~current_epoch:(deps.current_epoch ())
      ~state_attested:(deps.state_attested ())
      ~quarantine_active:(deps.quarantine_active ())
      ~quarantine_reason:(deps.quarantine_reason ())
  in
  match proposal_admission with
  | Proposal_defer ->
    Lwt.return_none
  | Proposal_admit ->
    let now = deps.now () in
    let delay_ms =
      if epoch_id <= 0L then
        0L
      else
        match deps.previous_epoch_ts (Int64.pred epoch_id) with
        | Some previous ->
          Octra_consensus.Epoch_time.next_delay_ms ~now ~previous
        | None ->
          Octra_consensus.Epoch_time.interval_ms
    in
    if delay_ms > 0L then begin
      Octra_log.info "consensus"
        "defer proposal reason = epoch_time epoch = %Ld delay_ms = %Ld"
        epoch_id
        delay_ms;
      Lwt.return_none
    end else
    begin
      match deps.parent_commit ~epoch_id with
      | Error reason ->
        Octra_log.warn "consensus"
          "defer proposal reason = parent_commit epoch = %Ld detail = %s"
          epoch_id
          reason;
        Lwt.return_none
      | Ok parent_commit ->
    let* prev_ledger_root_opt = deps.read_prev_ledger_root () in
    let roots =
      proposal_roots
        ~root_to_raw32
        ~ledger_head:prev_ledger_root_opt
        ~cached_head:(deps.cached_head ())
    in
    let prev_ledger_root = roots.prev_ledger_root in
    let prev_root = roots.prev_state_root in
    let proposal_round = deps.current_round () in
    let freeze_key = Consensus_bundle_cache.freeze_key ~epoch_id ~round:proposal_round in
    match deps.frozen_bundle freeze_key with
    | Some frozen ->
      let frozen = frozen_proposal frozen in
      if
        frozen.header.parent_commit_hash
        <> Octra_consensus.C_hash.parent_commit_hash_opt parent_commit
      then begin
        Octra_log.warn "consensus"
          "defer proposal reason = frozen_parent_commit_mismatch epoch = %Ld"
          epoch_id;
        Lwt.return_none
      end else begin
        deps.store_bundle
          ~proposal_id:frozen.proposal_id
          ~tx_hashes:frozen.tx_hashes
          ~txs:frozen.txs
          ~receipts_json:frozen.receipts_json;
        log_frozen_proposal ~epoch_id ~round:proposal_round frozen;
        Lwt.return_some Octra_consensus.C_driver.{
          header = frozen.header;
          tx_hashes = frozen.tx_hashes;
          parent_commit;
        }
      end
    | None ->
      let tx_list_raw = deps.staging_txs () in
      let tx_list_admitted = List.filter deps.admits_tx tx_list_raw in
      let admitted_hashes = List.map Transaction.hash tx_list_admitted in
      let* pre_shape =
        build_preverify
          ~run_many:(fun txs ->
            deps.build_preverify_once
              ~state_root:prev_root
              ~tx_hashes:admitted_hashes
              txs)
          ~limits
          tx_list_admitted
      in
      log_build_skipped ~epoch_id pre_shape;
      let tx_list = pre_shape.txs in
      log_build_preverify
        ~epoch_id
        ~round:proposal_round
        ~limits
        ~staging_total:(deps.staging_total ())
        ~raw_count:(List.length tx_list_raw)
        ~admitted_count:(List.length tx_list_admitted)
        ~prev_root
        pre_shape;
      let proposer = deps.proposer () in
      let validator_pubkeys = deps.validator_pubkeys epoch_id in
      let epoch_ts = deps.now () in
      let rec preview_until_stable attempt remaining accumulated =
        if attempt > List.length tx_list + 1 then
          Lwt.return_error "preview_retry_limit"
        else
          let remaining_hashes = List.map Transaction.hash remaining in
          let receipts =
            Octra_core.Preverify_worker.receipts_for_hashes
              pre_shape.batch.ready
              remaining_hashes
          in
          let preverify = Octra_core.Preverify_commit.create receipts in
          let* preview_result =
            deps.preview {
              epoch_id;
              epoch_ts;
              proposal_id = Printf.sprintf "propose-%Ld" epoch_id;
              expected_prev_root = prev_ledger_root;
              prev_state_root = prev_root;
              parent_commit;
              proposer;
              validator_pubkeys;
              preverify;
              txs = remaining;
            }
          in
          match preview_result with
          | Stdlib.Error error ->
            Lwt.return_error error
          | Stdlib.Ok result ->
            let artifacts = result.Octra_core.Epoch_exec.artifacts in
            let confirmed = List.map fst artifacts.confirmed in
            let current_rejections = artifacts.rejected in
            begin
              match
                preview_partition
                  ~input_txs:remaining
                  ~confirmed
                  ~rejected:current_rejections
              with
              | Preview_partition_invalid ->
                Lwt.return_error "preview_partition_mismatch"
              | Preview_partition_stable stable_txs ->
                begin
                  match
                    Octra_core.Tx_outcome.build
                      ~candidates:tx_list
                      (rejection_tuples accumulated)
                  with
                  | Error error ->
                    Lwt.return_error ("preview_outcome_invalid:" ^ error)
                  | Ok rejections ->
                    Lwt.return_ok
                      (build_preview_output
                         ~root_to_raw32
                         ~epoch_id
                         ~start_txid:(deps.next_txid ())
                         ~prev_eic_root:(deps.prev_eic_root ())
                         ~prev_ledger_root
                         ~fallback_ledger_root:prev_root
                         ~input_txs:stable_txs
                         ~batch:pre_shape.batch
                         ~rejections
                         ~preview_result:(Stdlib.Ok result))
                end
              | Preview_partition_retry { confirmed; rejections = current } ->
                Octra_log.warn "consensus"
                  "event = proposal_preview_retry epoch = %Ld attempt = %d rejected = %d remaining = %d"
                  epoch_id
                  attempt
                  (List.length current)
                  (List.length confirmed);
                preview_until_stable
                  (attempt + 1)
                  confirmed
                  (List.rev_append current accumulated)
            end
      in
      let* stable_preview = preview_until_stable 1 tx_list [] in
      match stable_preview with
      | Stdlib.Error error ->
        Octra_log.warn "consensus"
          "defer proposal reason = preview_failed epoch = %Ld detail = %s"
          epoch_id
          error;
        Lwt.return_none
      | Stdlib.Ok build_output ->
        let build_plan = build_output.plan in
        let final_tx_list = build_plan.final_txs in
        let final_tx_hashes = build_plan.final_hashes in
        log_preview_result ~epoch_id (List.length final_tx_list) build_plan;
        deps.set_proposal final_tx_list final_tx_hashes;
        let proposal_envelope =
          build_proposal_envelope
            ~chain_id
            ~epoch_id
            ~prev_state_root:prev_root
            ~final_hashes:final_tx_hashes
            ~final_txs:final_tx_list
            ~receipts_json:build_output.receipts_json
            ~proposed_state_root:build_plan.proposed_state_root
            ~creator_addr:proposer
            ~next_txid:(deps.next_txid ())
            ~head_txid_hi:(deps.head_txid_hi ())
            ~parent_commit
            ~ts:epoch_ts
        in
        Octra_log.info "consensus"
          "proposing epoch = %Ld txs = %d txid_hi = %Ld"
          epoch_id
          (List.length final_tx_list)
          proposal_envelope.txid_hi;
        deps.store_bundle
          ~proposal_id:proposal_envelope.proposal_id
          ~tx_hashes:proposal_envelope.tx_hashes
          ~txs:proposal_envelope.txs
          ~receipts_json:proposal_envelope.receipts_json;
        deps.freeze freeze_key proposal_envelope.frozen_bundle;
        Lwt.return_some Octra_consensus.C_driver.{
          header = proposal_envelope.header;
          tx_hashes = final_tx_hashes;
          parent_commit = proposal_envelope.parent_commit;
        }
    end