(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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
  receipts_json : string list;
  preverify : Octra_core.Preverify_commit.t;
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

type tx_hash_admission =
  | Tx_hash_ok of string list
  | Tx_hash_mismatch

type proposal_envelope = {
  header : Octra_consensus.C_types.epoch_header;
  proposal_id : string;
  txid_hi : int64;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  frozen_bundle : Consensus_bundle_cache.frozen;
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
  if expected_tx_count <> 0 && have <> expected_tx_count then
    Error (Missing_txs { have; need = expected_tx_count })
  else if not (receipt_root_matches ~header receipts_json) then
    Error Receipt_root_mismatch
  else
    match Octra_core.Preverify_commit.receipts_of_strings receipts_json with
    | Error e -> Error (Receipt_decode_failed e)
    | Ok receipts ->
      let preverify = Octra_core.Preverify_commit.create receipts in
      match Octra_core.Preverify_commit.check preverify txs with
      | Error e -> Error (Preverify_gate_failed e)
      | Ok () ->
        if not (within_limits ~limits txs) then
          Error (Bundle_limit { totals = totals txs; limits })
        else
          match verify_signatures deps txs with
          | Some bad -> Error bad
          | None ->
            Ok { txs; receipts_json; preverify }

let log_reject ~epoch_id = function
  | Missing_txs { have; need } ->
    Octra_log.warn "consensus"
      "reject proposal reason = missing_txs have = %d need = %d"
      have need
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
    ~prev_ledger_root ~fallback_ledger_root ~input_txs ~preview =
  match preview with
  | Build_preview_error e ->
    let final_hashes = List.map Transaction.hash input_txs in
    {
      final_txs = input_txs;
      final_hashes;
      rejected_hashes = [];
      proposed_state_root = fallback_ledger_root;
      preview_consensus_root = None;
      preview_error = Some e;
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

let raw32_to_hex r =
  if String.length r = 64 then
    r
  else
    String.concat ""
      (List.init (String.length r)
         (fun i -> Printf.sprintf "%02x" (Char.code r.[i])))

let tx_list_hash tx_hashes =
  Octra_net.Hash_domain.hash "octra:tx_list:v1" (String.concat "" tx_hashes)

let tx_hash_admission ~expected_tx_list_hash ~tx_hashes =
  if expected_tx_list_hash = tx_list_hash tx_hashes then
    Tx_hash_ok tx_hashes
  else
    Tx_hash_mismatch

let proposal_id_short proposal_id =
  short 16 (Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string proposal_id))

let proposal_txid_hi ~final_count ~next_txid ~head_txid_hi =
  if final_count = 0 then
    match head_txid_hi with
    | Some txid -> txid
    | None -> Int64.sub next_txid 1L
  else
    Int64.add next_txid (Int64.of_int (final_count - 1))

let build_header ~chain_id ~epoch_id ~prev_state_root ~final_hashes
    ~receipts_json ~proposed_state_root ~creator_addr ~next_txid
    ~head_txid_hi ~ts =
  Octra_consensus.C_types.{
    proto_version = proto_version_current;
    chain_id;
    epoch_id;
    prev_state_root;
    tx_list_hash = tx_list_hash final_hashes;
    receipt_root = Octra_consensus.C_hash.receipt_root receipts_json;
    proposed_state_root;
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
    ~creator_addr ~next_txid ~head_txid_hi ~ts =
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
      ~ts
  in
  {
    header;
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
  | Some (tx_hashes, _txs_json, receipts_json as raw) ->
    if current_tx_hashes = tx_hashes then
      Precommit_sync_current
    else
      match Consensus_bundle_cache.parse_txs raw with
      | Ok txs ->
        Precommit_sync_decoded {
          pid_short;
          tx_hashes;
          txs;
          receipts_json;
        }
      | Error error ->
        Precommit_sync_decode_failed {
          pid_short;
          error;
        }

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