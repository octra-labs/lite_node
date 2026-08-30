(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_driver = Octra_consensus.C_driver
module C_codec = Octra_consensus.C_codec
module C_hash = Octra_consensus.C_hash
module C_types = Octra_consensus.C_types
module Head_manifest = Octra_core.Head_manifest
module Transaction = Octra_core.Transaction
module Wal = Octra_core.Wal

type decision =
  | Confirmed_elsewhere of {
      agreed : int;
      quorum : int;
    }
  | All_peers_missing of {
      responses : int;
    }
  | Inconclusive of {
      agreed : int;
      peers : int;
      responses : int;
      quorum : int;
    }

type recovered = {
  proposal : C_types.propose;
  vote : C_types.vote;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
}

type restore_result =
  | Restored
  | Legacy
  | Invalid of string

let raw32_of_hex value =
  let hex_digit = function
    | '0' .. '9' as digit -> Some (Char.code digit - Char.code '0')
    | 'a' .. 'f' as digit ->
      Some (10 + Char.code digit - Char.code 'a')
    | _ -> None
  in
  if String.length value <> 64 then None
  else
    let bytes = Bytes.create 32 in
    let rec loop index =
      if index = 32 then Some (Bytes.unsafe_to_string bytes)
      else
        match hex_digit value.[index * 2], hex_digit value.[(index * 2) + 1] with
        | Some high, Some low ->
          Bytes.set bytes index (Char.chr ((high lsl 4) lor low));
          loop (index + 1)
        | _ ->
          None
    in
    loop 0

let root_matches promised_root_raw (r : C_driver.epoch_root_response_record) =
  match r.state_root, promised_root_raw with
  | Some local, Some promised -> local = promised
  | _ -> false

let all_missing responses =
  List.for_all
    (fun (r : C_driver.epoch_root_response_record) -> r.state_root = None)
    responses

let decide ~validator_count ~peer_quorum ~promised_root_raw responses =
  let agreed =
    List.filter (root_matches promised_root_raw) responses
    |> List.length
  in
  let response_count = List.length responses in
  let peers = validator_count - 1 in
  if agreed >= peer_quorum then
    Confirmed_elsewhere { agreed; quorum = peer_quorum }
  else if response_count >= peers && all_missing responses then
    All_peers_missing { responses = response_count }
  else
    Inconclusive {
      agreed;
      peers;
      responses = response_count;
      quorum = peer_quorum;
    }

let recover_record ~chain_id ~validator_set pending =
  match pending.Wal.proposal_b64, pending.vote_b64 with
  | None, _
  | _, None ->
    Error "legacy pending record"
  | Some proposal_b64, Some vote_b64 ->
    try
      let proposal =
        proposal_b64
        |> Base64.decode_exn
        |> C_codec.decode_propose
      in
      let vote =
        vote_b64
        |> Base64.decode_exn
        |> C_codec.decode_vote
      in
      let proposal_id = C_hash.proposal_id proposal.header in
      let proposal_id_hex = Text.hash32_hex proposal_id in
      let proposed_root_hex =
        Text.hash32_hex proposal.header.proposed_state_root
      in
      let proposal_hashes =
        List.map Text.hash32_hex proposal.tx_hashes
      in
      let proposer_signature_valid =
        match C_types.pubkey_of_addr validator_set proposal.proposer with
        | Some pubkey -> C_hash.verify_propose ~pubkey_raw:pubkey proposal
        | None -> false
      in
      let vote_signature_valid =
        match C_types.pubkey_of_addr validator_set vote.validator with
        | Some pubkey -> C_hash.verify_vote ~pubkey_raw:pubkey vote
        | None -> false
      in
      let response : C_driver.bundle_response_record = {
        responder_addr = "pending_commit";
        tx_hashes = pending.tx_hashes;
        txs_json = pending.txs_json;
        receipts_json = pending.receipts_json;
      } in
      if proposal.epoch_id <> Int64.of_int pending.epoch_id then
        Error "pending proposal epoch mismatch"
      else if proposal.round <> pending.round then
        Error "pending proposal round mismatch"
      else if not
        (C_types.proposal_is_well_formed
           ~chain_id
           ~validator_set
           proposal)
      then
        Error "pending proposal envelope invalid"
      else if proposal_id_hex <> pending.proposal_id then
        Error "pending proposal id mismatch"
      else if proposed_root_hex <> pending.proposed_state_root then
        Error "pending state root mismatch"
      else if proposal.header.txid_hi <> pending.txid_hi then
        Error "pending txid mismatch"
      else if not proposer_signature_valid then
        Error "pending proposal signature invalid"
      else if vote.chain_id <> chain_id then
        Error "pending vote chain mismatch"
      else if vote.epoch_id <> proposal.epoch_id then
        Error "pending vote epoch mismatch"
      else if vote.round <> proposal.round then
        Error "pending vote round mismatch"
      else if vote.vote_type <> C_types.Precommit then
        Error "pending vote type mismatch"
      else if vote.proposal_id <> proposal_id then
        Error "pending vote proposal mismatch"
      else if vote.validator <> pending.validator_addr then
        Error "pending vote validator mismatch"
      else if not vote_signature_valid then
        Error "pending vote signature invalid"
      else if proposal_hashes <> pending.tx_hashes then
        Error "pending proposal bundle mismatch"
      else
        match
          Consensus_bundle_validation.finalized
            ~header:proposal.header
            response
        with
        | Error error -> Error ("pending bundle " ^ error)
        | Ok accepted ->
          Ok {
            proposal;
            vote;
            tx_hashes = accepted.tx_hashes;
            txs = accepted.txs;
            receipts_json = accepted.receipts_json;
          }
    with exn ->
      Error (Printexc.to_string exn)

type deps = {
  read_pending_commits : unit -> Wal.pending_commit list;
  head_epoch : unit -> int;
  query_epoch_root :
    epoch_id:int64 ->
    C_driver.epoch_root_response_record list Lwt.t;
  run_catchup_to_target : target_epoch:int64 -> reason:string -> unit Lwt.t;
  delete_pending_commit : epoch_id:int -> round:int -> unit;
  restore_pending : Wal.pending_commit -> restore_result;
}

type 'driver driver_runtime = {
  read_pending_commits : unit -> Wal.pending_commit list;
  head : unit -> Head_manifest.t option;
  query_epoch_root :
    'driver ->
    epoch_id:int64 ->
    C_driver.epoch_root_response_record list Lwt.t;
  run_catchup_to_target :
    'driver ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  delete_pending_commit : epoch_id:int -> round:int -> unit;
  restore_pending : 'driver -> Wal.pending_commit -> restore_result;
  validator_count : int;
  peer_quorum : int;
}

type node_driver_runtime = {
  data_dir : string;
  query_timeout : float;
  run_catchup_to_target :
    C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  chain_id : string;
  validator_set : C_types.validator_set;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  validator_count : int;
  quorum : int;
}

let driver_runtime
    ~read_pending_commits
    ~head
    ~query_epoch_root
    ~run_catchup_to_target
    ~delete_pending_commit
    ~restore_pending
    ~validator_count
    ~quorum =
  {
    read_pending_commits;
    head;
    query_epoch_root;
    run_catchup_to_target;
    delete_pending_commit;
    restore_pending;
    validator_count;
    peer_quorum = max 0 (quorum - 1);
  }

let node_driver_runtime runtime =
  driver_runtime
    ~read_pending_commits:(fun () ->
      Wal.read_pending_commits runtime.data_dir)
    ~head:Head_manifest.get_cached
    ~query_epoch_root:(fun driver ~epoch_id ->
      C_driver.query_epoch_root
        driver
        ~epoch_id
        ~timeout_seconds:runtime.query_timeout)
    ~run_catchup_to_target:runtime.run_catchup_to_target
    ~delete_pending_commit:(fun ~epoch_id ~round ->
      Wal.delete_pending_commit runtime.data_dir epoch_id round)
    ~restore_pending:(fun driver pending ->
      match pending.Wal.proposal_b64, pending.vote_b64 with
      | None, _
      | _, None ->
        Legacy
      | Some _, Some _ ->
        match
          recover_record
            ~chain_id:runtime.chain_id
            ~validator_set:runtime.validator_set
            pending
        with
        | Error error ->
          Invalid error
        | Ok recovered ->
          try
            runtime.store_bundle
              ~proposal_id:(C_hash.proposal_id recovered.proposal.header)
              ~tx_hashes:recovered.tx_hashes
              ~txs:recovered.txs
              ~receipts_json:recovered.receipts_json;
            match
              C_driver.restore_precommit_lock driver recovered.proposal
            with
            | Ok () -> Restored
            | Error error -> Invalid error
          with exn ->
            Invalid (Printexc.to_string exn))
    ~validator_count:runtime.validator_count
    ~quorum:runtime.quorum

let head_epoch_of_manifest = function
  | Some head -> head.Head_manifest.epoch_id
  | None -> -1

let deps_of_driver_runtime (runtime : 'driver driver_runtime) driver =
  {
    read_pending_commits = runtime.read_pending_commits;
    head_epoch = (fun () -> head_epoch_of_manifest (runtime.head ()));
    query_epoch_root = (fun ~epoch_id ->
      runtime.query_epoch_root driver ~epoch_id);
    run_catchup_to_target = (fun ~target_epoch ~reason ->
      runtime.run_catchup_to_target driver ~target_epoch ~reason);
    delete_pending_commit = runtime.delete_pending_commit;
    restore_pending = (fun pending ->
      runtime.restore_pending driver pending);
  }

let unresolved (deps : deps) =
  try
    let head = deps.head_epoch () in
    let pending =
      deps.read_pending_commits ()
      |> List.filter (fun p -> p.Wal.epoch_id > head)
    in
    Ok (head, pending)
  with exn ->
    Error (Printexc.to_string exn)

let hold_unreadable error =
  Octra_log.error "consensus"
    "event = pending_commit_recovery action = hold reason = store_unreadable error = %s"
    error;
  Lwt.return_false

let delete (deps : deps) p =
  deps.delete_pending_commit ~epoch_id:p.Wal.epoch_id ~round:p.round

let run_confirmed (deps : deps) p ~agreed ~quorum =
  let open Lwt.Syntax in
  let epoch = Int64.of_int p.Wal.epoch_id in
  Octra_log.warn "consensus"
    "event = pending_commit action = catchup_recovery epoch = %d peer_root_match = %d quorum = %d"
    p.epoch_id agreed quorum;
  let* () = deps.run_catchup_to_target
    ~target_epoch:epoch
    ~reason:"pending_commit_recovery" in
  let head_after = deps.head_epoch () in
  if head_after >= p.epoch_id then begin
    delete deps p;
    Octra_log.info "consensus"
      "event = pending_commit action = recovery_applied epoch = %d head = %d"
      p.epoch_id head_after;
    Lwt.return_unit
  end else begin
    Octra_log.warn "consensus"
      "event = pending_commit action = keep_breadcrumb epoch = %d head = %d reason = catchup_no_advance"
      p.epoch_id head_after;
    Lwt.return_unit
  end

let run_all_missing p ~responses =
  Octra_log.warn "consensus"
    "event = pending_commit action = keep_breadcrumb epoch = %d responses = %d reason = peers_missing_epoch"
    p.Wal.epoch_id responses;
  Lwt.return_unit

let run_inconclusive p ~agreed ~peers ~responses ~quorum =
  Octra_log.warn "consensus"
    "event = pending_commit action = keep_breadcrumb epoch = %d agreed = %d peers = %d responses = %d quorum = %d reason = inconclusive"
    p.Wal.epoch_id agreed peers responses quorum;
  Lwt.return_unit

let run_pending (deps : deps) ~validator_count ~peer_quorum p =
  let open Lwt.Syntax in
  let epoch = Int64.of_int p.Wal.epoch_id in
  let* responses = deps.query_epoch_root ~epoch_id:epoch in
  let promised_root_raw = raw32_of_hex p.Wal.proposed_state_root in
  match decide ~validator_count ~peer_quorum ~promised_root_raw responses with
  | Confirmed_elsewhere { agreed; quorum } ->
    run_confirmed deps p ~agreed ~quorum
  | All_peers_missing { responses } ->
    run_all_missing p ~responses
  | Inconclusive { agreed; peers; responses; quorum } ->
    run_inconclusive p ~agreed ~peers ~responses ~quorum

let run_once (deps : deps) ~validator_count ~peer_quorum =
  match unresolved deps with
  | Error error -> hold_unreadable error
  | Ok (_, pending) when pending = [] -> Lwt.return_true
  | Ok (head, pending) ->
    let expected_epoch = head + 1 in
    let future =
      List.filter
        (fun candidate -> candidate.Wal.epoch_id <> expected_epoch)
        pending
    in
    if future <> [] then begin
      let first = List.hd future in
      Octra_log.error "consensus"
        "event = pending_commit_recovery action = hold reason = non_contiguous_record epoch = %d expected_epoch = %d head = %d"
        first.epoch_id
        expected_epoch
        head;
      Lwt.return_false
    end else
      let latest =
        List.fold_left
          (fun selected candidate ->
            if candidate.Wal.round > selected.Wal.round then candidate
            else selected)
          (List.hd pending)
          (List.tl pending)
      in
      match deps.restore_pending latest with
      | Restored ->
        Octra_log.warn "consensus"
          "event = pending_commit_recovery action = restore_lock epoch = %d round = %d head = %d"
          latest.epoch_id
          latest.round
          head;
        Lwt.return_true
      | Invalid error ->
        Octra_log.error "consensus"
          "event = pending_commit_recovery action = hold reason = invalid_record epoch = %d round = %d error = %s"
          latest.epoch_id
          latest.round
          error;
        Lwt.return_false
      | Legacy ->
        let open Lwt.Syntax in
        Octra_log.warn "consensus"
          "event = pending_commit_recovery action = query_peers records = %d head = %d reason = legacy_record"
          (List.length pending)
          head;
        let* () =
          Lwt_list.iter_s
            (run_pending deps ~validator_count ~peer_quorum)
            pending
        in
        (match unresolved deps with
         | Error error -> hold_unreadable error
         | Ok (_, remaining) -> Lwt.return (remaining = []))

let run_with_driver runtime driver =
  run_once
    (deps_of_driver_runtime runtime driver)
    ~validator_count:runtime.validator_count
    ~peer_quorum:runtime.peer_quorum