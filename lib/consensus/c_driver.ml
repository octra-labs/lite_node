(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Frame = Octra_net.P2p_frame

type resource_committee_config = {
  activation_delay : int;
  committee_size : int;
  minimum_weight : int64;
  fraud_window : int64;
  future_window : int64;
  pubkey_of_node : string -> string option;
  source_seed_for_epoch : int64 -> Resource_attestation_flow.epoch_seed option;
  on_committee_selected : Resource_attestation_flow.committee_snapshot -> unit Lwt.t;
}

type scheduled_validator_set_config = {
  activate_epoch : int64;
  validator_set : C_types.validator_set;
  fingerprint : string;
}

type proposal_plan = {
  header : C_types.epoch_header;
  tx_hashes : string list;
  parent_commit : C_types.parent_commit option;
}

type proposal_verdict =
  | Proposal_accept
  | Proposal_wait
  | Proposal_reject

type config = {
  chain_id : string;
  my_addr : string;
  sign_fn : string -> string;
  verify_fn : string -> string -> string -> bool;
  role_can_vote : unit -> bool;
  can_vote : unit -> bool;
  execute_fn : C_types.propose -> bool;
  verify_proposal : C_types.propose -> proposal_verdict Lwt.t;
  verify_parent_commit :
    epoch_id:int64 ->
    C_types.parent_commit option ->
    (unit, string) result;
  on_finalized :
    validator_set:C_types.validator_set ->
    C_types.finalize ->
    unit Lwt.t;
  make_proposal : int64 -> proposal_plan option Lwt.t;
  before_precommit_broadcast :
    epoch_id:int64 -> round:int -> proposal_id:string ->
    proposed_state_root:string -> txid_hi:int64 ->
    proposal_wire:string -> vote_wire:string -> bool Lwt.t;
  lookup_epoch_root : int64 -> string option;
  local_head_epoch : unit -> int64;
  lookup_bundle : string -> (string list * string list * string list) option;
  lookup_catchup_range :
    from_epoch:int64 -> max_epochs:int ->
    [ `Ok of C_codec.catchup_epoch_record list * int64 option
    | `NotFound
    | `Internal of string ];
  on_resource_attestation : Resource_attestation_flow.gossip -> unit Lwt.t;
  scheduled_validator_set_config : scheduled_validator_set_config option;
  load_scheduled_validator_set_config :
    unit -> scheduled_validator_set_config option Lwt.t;
  resource_committee_config : resource_committee_config option;
}

type epoch_root_response_record = {
  responder_addr : string;
  responder_head_epoch : int64;
  state_root : string option;
}

type epoch_root_wait =
  | Source_agreement
  | Consensus_quorum

type bundle_response_record = {
  responder_addr : string;
  tx_hashes : string list;
  txs_json : string list;
  receipts_json : string list;
}

type catchup_range_response_record = {
  responder_addr : string;
  request_id : string;
  status : string;
  records : C_codec.catchup_epoch_record list;
  next_epoch : int64 option;
}

type catchup_query_window = {
  from_epoch : int64;
  max_epochs : int;
}

type peer_state_record = {
  responder_addr : string;
  mutable head_epoch : int64;
  mutable checked_epoch : int64;
  mutable state_root : string option;
  mutable last_seen : float;
  mutable source : string;
}

type round_state = {
  epoch_id : int64;
  round : int;
  step : C_types.round_step;
}

type round_tally = {
  proposal_id : string;
  voters : int;
  weight : Z.t;
}

type round_votes = {
  prevotes : round_tally list;
  precommits : round_tally list;
  quorum : int;
  quorum_weight : Z.t;
}

type round_witness = {
  wire : string;
  seen_at : float;
}

type round_peer_record = {
  validator_addr : string;
  mutable epoch_id : int64;
  mutable round : int;
  mutable step : C_types.round_step;
  mutable last_seen : float;
}

type proposal_build = {
  gen : int;
  height : int64;
  round : int;
  step : C_types.round_step;
  started_at : int64;
}

type pending_finalize = {
  finalize : C_types.finalize;
  validator_set_hash : string;
}

type proposal_height_status =
  | Proposal_current
  | Proposal_stale
  | Proposal_future

type proposal_frame_error =
  | Proposal_unknown_validator
  | Proposal_bad_signature
  | Proposal_envelope
  | Proposal_tx_list_hash
  | Proposal_parent_commit_hash

type proposal_fault_source =
  | Proposal_fault_unresolved
  | Proposal_fault_sender
  | Proposal_fault_signer

type vote_evidence_validation =
  | Evidence_valid
  | Evidence_unknown_validator
  | Evidence_invalid

type verified_proposal_route =
  | Publish_verified_proposal
  | Relay_verified_proposal of {
      source_peer : string;
      payload : string;
    }

type proposal_wait = {
  gen : int;
  pid : string;
  proposal : C_types.propose;
  route : verified_proposal_route;
  attempt : int;
  retry_at : int64;
}

type proposal_fetch = {
  height : int64;
  round : int;
  proposal_id : string;
  generation : int;
}

type proposal_fetch_decision =
  | Stop_proposal_fetch
  | Send_proposal_fetch

type round_sync_reply = {
  epoch_id : int64;
  round : int;
  step : C_types.round_step;
  sent_at : int64;
}

type round_fetch_reply = {
  sent_at : int64;
}

type past_round = {
  epoch_id : int64;
  target : int;
  sent_at : int64;
  tries : int;
}

type vote_fault = {
  epoch_id : int64;
  round : int;
  reason : string;
}

type t = {
  mutable n_validators : int;
  config : config;
  engine : C_engine.t;
  swarm : Octra_net.P2p_swarm.t;
  seen : C_seen.t;
  historical_replays : C_seen.t;
  mutable running : bool;
  mutable epoch_start_mono : int64;
  epoch_root_responses : (int64, epoch_root_response_record list) Hashtbl.t;
  bundle_responses : (string, bundle_response_record list) Hashtbl.t;
  catchup_responses : (string, catchup_range_response_record list) Hashtbl.t;
  peer_states : (string, peer_state_record) Hashtbl.t;
  round_peers : (string, round_peer_record) Hashtbl.t;
  resource_attestations : (string, Resource_attestations.attestation) Hashtbl.t;
  resource_admission : Resource_attestation_admission.pool;
  vote_evidence : (string, C_evidence.vote_conflict) Hashtbl.t;
  activated_validator_set_fingerprints : (string, bool) Hashtbl.t;
  mutable plan_seen : bool;
  mutable plan_mark : (int64 * string) option;
  pending_votes : (string, C_types.vote) Hashtbl.t;
  future_votes : (string, C_types.vote) Hashtbl.t;
  vote_log : C_vote_log.t;
  sync_log : C_sync_log.t;
  mutable vote_log_issue : string option;
  mutable vote_fault : vote_fault option;
  durable_votes : (string, C_types.vote) Hashtbl.t;
  pending_finalizes : (int64, pending_finalize) Hashtbl.t;
  pending_proposals : (string, C_types.propose) Hashtbl.t;
  deferred_proposals : (string, C_types.propose) Hashtbl.t;
  round_sync_replies : (string, round_sync_reply) Hashtbl.t;
  round_fetch_replies : (string, round_fetch_reply) Hashtbl.t;
  mutable past_round : past_round option;
  round_pool : C_round_pool.t;
  finality_query_requests : (string, int64) Hashtbl.t;
  finality_proof_requests : (string, int64) Hashtbl.t;
  finality_proof_needed : bool ref;
  mutable check_finality_proof :
    C_types.validator_set -> C_types.finalize -> (unit, string) result;
  mutable on_finality_proof :
    C_types.validator_set -> C_types.finalize -> bool Lwt.t;
  mutable finality_query : C_finality_query.t;
  proposal_fetches : (string, unit) Hashtbl.t;
  catchup_query_windows : (string, catchup_query_window) Hashtbl.t;
  mutable proposal_build : proposal_build option;
  mutable proposal_retry : proposal_build option;
  mutable proposal_verify : proposal_build option;
  mutable proposal_wait : proposal_wait option;
  mutable round_spread_warned_at : float;
  proposal_work_gate : C_proposal_work_gate.t;
  mutable on_validator_set_activated :
    C_types.validator_set -> string -> unit Lwt.t;
  mutable on_fold :
    next_epoch:int64 ->
    (C_types.vote * C_types.parent_commit) option ->
    unit;
  mutable output_actor : C_output_actor.t;
}

let raw_to_hex s =
  String.concat "" (List.init (String.length s)
    (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let log_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.info "consensus" "node = %s module = driver %s" addr msg)
    fmt

let trace_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.trace "consensus" "node = %s module = driver %s" addr msg)
    fmt

let warn_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.warn "consensus" "node = %s module = driver %s" addr msg)
    fmt

let error_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.error "consensus" "node = %s module = driver %s" addr msg)
    fmt

let log fmt =
  Printf.ksprintf
    (fun msg -> Octra_log.info "consensus" "module = driver %s" msg)
    fmt

let catchup_record_complete (record : C_codec.catchup_epoch_record) =
  Option.is_some record.reward_source
  && Option.is_some record.finality

let catchup_records_complete = function
  | [] -> false
  | records -> List.for_all catchup_record_complete records

let normalize_legacy_range_response
    (response : C_codec.catchup_range_response) =
  match List.rev response.records with
  | [] -> None
  | last :: _ when catchup_records_complete response.records ->
    Some {
      response with
      status = "ok";
      error_code = None;
      next_epoch = Some (Int64.succ last.epoch_id);
    }
  | _ -> None

let response_source_matches
    validator_set
    ~peer_id
    ~responder_addr =
  match C_types.pubkey_of_addr validator_set responder_addr with
  | None -> false
  | Some public_key ->
    String.equal
      peer_id
      (Octra_net.P2p_handshake.node_id_of_pubkey public_key)

let catchup_response_key (rec_ : catchup_range_response_record) =
  if List.for_all catchup_record_complete rec_.records then
    let records_root = C_hash.catchup_records_root rec_.records in
    let next_epoch_s =
      match rec_.next_epoch with
      | Some e -> Int64.to_string e
      | None -> "-" in
    Some (rec_.status ^ "|" ^ next_epoch_s ^ "|" ^ records_root)
  else
    None

let catchup_source_validator_set = C_types.resilient_validator_set

let catchup_agreement_validator_set t ~epoch_id:_ =
  catchup_source_validator_set t.engine.vs

let catchup_agreement_weight t ~epoch_id =
  catchup_agreement_validator_set t ~epoch_id
  |> C_types.round_skip_weight

let catchup_source_agreement_reached validator_set
    ~signer_count ~signed_weight =
  signer_count >= validator_set.C_types.f + 1
  && Z.geq signed_weight (C_types.round_skip_weight validator_set)

let catchup_agreement_reached t ~epoch_id ~count ~weight =
  let validator_set = catchup_agreement_validator_set t ~epoch_id in
  catchup_source_agreement_reached
    validator_set
    ~signer_count:count
    ~signed_weight:weight

let catchup_responder_weight t ~epoch_id responder_addr =
  catchup_agreement_validator_set t ~epoch_id
  |> fun validator_set ->
     match C_types.weight_of_addr validator_set responder_addr with
     | Some weight -> weight
     | None -> Z.zero

let catchup_agreement_epoch ~from_epoch records =
  match List.rev records with
  | last :: _ -> last.C_codec.epoch_id
  | [] -> from_epoch

let catchup_epoch_in_window window epoch =
  window.max_epochs > 0
  && Int64.compare epoch window.from_epoch >= 0
  && Int64.compare
       (Int64.sub epoch window.from_epoch)
       (Int64.of_int window.max_epochs)
     < 0

let catchup_response_in_window window records =
  window.max_epochs >= 0
  && List.length records <= window.max_epochs
  && List.for_all
       (fun (record : C_codec.catchup_epoch_record) ->
         catchup_epoch_in_window window record.epoch_id)
       records

let create ~config ~validator_set ~swarm ~start_height ~sync_log ~vote_log =
  let finality_proof_needed = ref false in
  let can_vote () =
    config.can_vote () && not !finality_proof_needed
  in
  let validator_set =
    C_types.validator_set_for_epoch
      ~chain_id:config.chain_id
      ~epoch_id:start_height
      validator_set
  in
  let engine = C_engine.create
    ~chain_id:config.chain_id
    ~my_addr:config.my_addr
    ~validator_set
    ~start_height
    ~can_vote in
  { n_validators = validator_set.C_types.n; config; engine; swarm;
    seen = C_seen.create ~capacity:10_000; running = false;
    historical_replays = C_seen.create ~capacity:4_096;
    epoch_start_mono = Mtime_clock.elapsed_ns ();
    epoch_root_responses = Hashtbl.create 16;
    bundle_responses = Hashtbl.create 16;
    catchup_responses = Hashtbl.create 8;
    peer_states = Hashtbl.create 16;
    round_peers = Hashtbl.create 32;
    resource_attestations = Hashtbl.create 256;
    resource_admission = Resource_attestation_admission.create_pool ();
    vote_evidence = Hashtbl.create 16;
    activated_validator_set_fingerprints = Hashtbl.create 8;
    plan_seen = false;
    plan_mark = None;
    pending_votes = Hashtbl.create 16;
    future_votes = Hashtbl.create 32;
    vote_log;
    sync_log;
    vote_log_issue = None;
    vote_fault = None;
    durable_votes = Hashtbl.create 16;
    pending_finalizes = Hashtbl.create 16;
    pending_proposals = Hashtbl.create 8;
    deferred_proposals = Hashtbl.create 16;
    round_sync_replies = Hashtbl.create 16;
    round_fetch_replies = Hashtbl.create 16;
    past_round = None;
    round_pool = C_round_pool.create ();
    finality_query_requests = Hashtbl.create 4;
    finality_proof_requests = Hashtbl.create 4;
    finality_proof_needed;
    check_finality_proof =
      (fun _ _ -> Error "finality proof check unavailable");
    on_finality_proof = (fun _ _ -> Lwt.return_false);
    finality_query = C_finality_query.idle;
    proposal_fetches = Hashtbl.create 4;
    catchup_query_windows = Hashtbl.create 8;
    proposal_build = None;
    proposal_retry = None;
    proposal_verify = None;
    proposal_wait = None;
    round_spread_warned_at = 0.0;
    proposal_work_gate = C_proposal_work_gate.create ();
    on_validator_set_activated =
      (fun _ _ -> Lwt.return_unit);
    on_fold = (fun ~next_epoch:_ _ -> ());
    output_actor = C_output_actor.idle }

let set_validator_set_activation_handler t handler =
  t.on_validator_set_activated <- handler

let set_fold_handler t handler =
  t.on_fold <- handler

let set_finality_proof_handler t ~needed ~check handler =
  t.finality_proof_needed := needed;
  t.check_finality_proof <- check;
  t.on_finality_proof <- handler

let grace_ms name ~default ~limit =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
    try
      let value = int_of_string raw in
      if value < 0 || value > limit then default else value
    with _ -> default

let proposal_build_grace_ms () =
  grace_ms "OCTRA_BFT_PROPOSAL_BUILD_GRACE_MS"
    ~default:180_000
    ~limit:300_000

let proposal_retry_ms = 500

let proposal_verify_grace_ms () =
  grace_ms "OCTRA_BFT_PROPOSAL_VERIFY_GRACE_MS"
    ~default:180_000
    ~limit:300_000

let within_grace started_at grace_ms =
  let elapsed = Int64.sub (Mtime_clock.elapsed_ns ()) started_at in
  let limit = Int64.mul (Int64.of_int grace_ms) 1_000_000L in
  Int64.compare elapsed 0L >= 0 && Int64.compare elapsed limit < 0

let bounded_timeout value =
  match classify_float value with
  | FP_nan | FP_infinite -> 0.
  | FP_normal | FP_subnormal | FP_zero -> min 300. (max 0. value)

let deadline_after seconds =
  let ns = Int64.of_float (bounded_timeout seconds *. 1e9) in
  Int64.add (Mtime_clock.elapsed_ns ()) ns

let deadline_reached deadline =
  Int64.compare (Mtime_clock.elapsed_ns ()) deadline >= 0

let same_proposal_build (b : proposal_build) ~gen ~height ~round ~step =
  b.gen = gen
  && b.height = height
  && b.round = round
  && b.step = step

let local_validator t =
  C_types.is_validator t.engine.vs t.config.my_addr

let vote_allowed t =
  t.config.can_vote () && not !(t.finality_proof_needed)

let clear_finality_proof_requests t =
  Hashtbl.iter
    (fun request_id _ ->
      Hashtbl.remove t.catchup_query_windows request_id)
    t.finality_proof_requests;
  Hashtbl.clear t.finality_proof_requests

let finality_request_epoch t request_id =
  match Hashtbl.find_opt t.finality_query_requests request_id with
  | Some epoch -> Some epoch
  | None -> Hashtbl.find_opt t.finality_proof_requests request_id

let repeatable_query_frame (frame : Frame.frame) =
  (frame.msg_type = Frame.msg_cons_round_sync
   &&
   try (C_codec.decode_round_sync frame.payload).request
   with _ -> false)
  || frame.msg_type = Frame.msg_cons_round_fetch
  || frame.msg_type = Frame.msg_query_epoch_root
  || frame.msg_type = Frame.msg_epoch_root_response
  || frame.msg_type = Frame.msg_query_bundle
  || frame.msg_type = Frame.msg_bundle_response
  || frame.msg_type = Frame.msg_query_catchup_range
  || frame.msg_type = Frame.msg_catchup_range_response
  || frame.msg_type = Frame.msg_query_catchup_range_v2
  || frame.msg_type = Frame.msg_catchup_range_response_v2

let engine_output_frame msg_type =
  msg_type = Frame.msg_cons_propose
  || msg_type = Frame.msg_cons_vote
  || msg_type = Frame.msg_cons_finalize
  || msg_type = Frame.msg_cons_round_sync

let frame_allowed ~running msg_type =
  running || not (engine_output_frame msg_type)

let clear_proposal_build t ~gen ~height ~round ~step =
  match t.proposal_build with
  | Some b when same_proposal_build b ~gen ~height ~round ~step ->
      t.proposal_build <- None
  | _ -> ()

let proposal_build_active t ~generation ~round ~step =
  match t.proposal_build with
  | Some b ->
      b.gen = generation
      && b.height = t.engine.state.height
      && b.round = round
      && b.step = step
  | None -> false

let proposal_build_grace_left t ~generation ~round ~step =
  match t.proposal_build with
  | Some b when
      b.gen = generation
      && b.height = t.engine.state.height
      && b.round = round
      && b.step = step ->
      within_grace b.started_at (proposal_build_grace_ms ())
  | _ -> false

let proposal_retry_pending t ~gen ~height ~round ~step =
  match t.proposal_retry with
  | Some retry
    when same_proposal_build retry ~gen ~height ~round ~step ->
      within_grace retry.started_at proposal_retry_ms
  | _ -> false

let mark_proposal_verify t ~gen ~height ~round ~step =
  t.proposal_verify <- Some {
    gen;
    height;
    round;
    step;
    started_at = Mtime_clock.elapsed_ns ();
  }

let clear_proposal_verify t ~gen ~height ~round ~step =
  match t.proposal_verify with
  | Some b when same_proposal_build b ~gen ~height ~round ~step ->
      t.proposal_verify <- None
  | _ -> ()

let proposal_verify_grace_left t ~generation ~round ~step =
  match t.proposal_verify with
  | Some b when
      b.gen = generation
      && b.height = t.engine.state.height
      && b.round = round
      && b.step = step ->
      within_grace b.started_at (proposal_verify_grace_ms ())
  | _ -> false

let proposal_work_current t work =
  same_proposal_build work
    ~gen:t.engine.generation
    ~height:t.engine.state.height
    ~round:t.engine.state.round
    ~step:t.engine.state.step

let proposal_work_active t =
  let active grace = function
    | Some work ->
      proposal_work_current t work
      && within_grace work.started_at grace
    | None -> false
  in
  active (proposal_build_grace_ms ()) t.proposal_build
  || active (proposal_verify_grace_ms ()) t.proposal_verify

let vote_step_label = function
  | C_types.Prevote -> "PREVOTE"
  | C_types.Precommit -> "PRECOMMIT"

let round_step_label = function
  | C_types.ProposeStep -> "propose"
  | C_types.PrevoteStep -> "prevote"
  | C_types.PrecommitStep -> "precommit"

let proposal_height_status ~current ~proposal =
  match Int64.compare proposal current with
  | 0 -> Proposal_current
  | n when n < 0 -> Proposal_stale
  | _ -> Proposal_future

let proposal_local_status ~engine_head ~local_head ~proposal =
  if Int64.compare proposal local_head <= 0 then Proposal_stale
  else proposal_height_status ~current:engine_head ~proposal

let vote_relay_relevant ~current_height ~current_round (vote : C_types.vote) =
  vote.epoch_id = current_height
  && vote.round >= current_round
  && vote.round <= current_round + C_engine.max_round_ahead

let next_height_relay_relevant ~current_height ~epoch_id ~round =
  current_height <> Int64.max_int
  && epoch_id = Int64.succ current_height
  && round >= 0
  && round <= C_engine.max_round_ahead

let future_vote_relay_relevant ~current_height (vote : C_types.vote) =
  next_height_relay_relevant
    ~current_height
    ~epoch_id:vote.epoch_id
    ~round:vote.round

let round_sync_relay_relevant
    ~current_height
    ~current_round
    (sync : C_codec.round_sync) =
  not sync.request
  && sync.epoch_id = current_height
  && sync.round >= current_round
  && sync.round <= current_round + C_engine.max_round_ahead

let round_sync_allowed ~current_round (sync : C_codec.round_sync) =
  sync.round <= current_round + C_engine.max_sync_ahead

let proposal_frame_error_label = function
  | Proposal_unknown_validator -> "unknown_validator"
  | Proposal_bad_signature -> "bad_signature"
  | Proposal_envelope -> "envelope"
  | Proposal_tx_list_hash -> "tx_list_hash"
  | Proposal_parent_commit_hash -> "parent_commit_hash"

let proposal_frame_peer_reason = function
  | Proposal_unknown_validator -> "unknown_validator_propose"
  | Proposal_bad_signature -> "bad_signature_propose"
  | Proposal_envelope -> "invalid_frame_propose_envelope"
  | Proposal_tx_list_hash -> "invalid_frame_propose_tx_list_hash"
  | Proposal_parent_commit_hash -> "invalid_frame_propose_parent_commit"

let proposal_fault_source = function
  | Proposal_unknown_validator -> Proposal_fault_unresolved
  | Proposal_bad_signature
  | Proposal_tx_list_hash
  | Proposal_parent_commit_hash -> Proposal_fault_sender
  | Proposal_envelope -> Proposal_fault_signer

let proposal_tx_list_is_bound (proposal : C_types.propose) =
  C_engine.tx_list_hash_for_header proposal.tx_hashes
  = proposal.header.tx_list_hash

let validate_proposal_frame ~chain_id ~validator_set (p : C_types.propose) =
  match C_types.pubkey_of_addr validator_set p.proposer with
  | None -> Error Proposal_unknown_validator
  | Some pubkey when not (C_hash.verify_propose ~pubkey_raw:pubkey p) ->
    Error Proposal_bad_signature
  | Some _
    when not
      (C_types.proposal_is_well_formed
         ~chain_id
         ~validator_set
         p) ->
    Error Proposal_envelope
  | Some _ when not (proposal_tx_list_is_bound p) ->
    Error Proposal_tx_list_hash
  | Some _
    when C_hash.parent_commit_hash_opt p.parent_commit
         <> p.header.parent_commit_hash ->
    Error Proposal_parent_commit_hash
  | Some _ -> Ok ()

let proposal_verify_relevant ~current_round ~current_step ~proposal_round =
  proposal_round >= current_round
  && proposal_round - current_round <= C_engine.max_round_ahead
  && (proposal_round > current_round
      || current_step = C_types.ProposeStep)

let proposal_verify_current t (p : C_types.propose) =
  p.epoch_id = t.engine.state.height
  && proposal_verify_relevant
       ~current_round:t.engine.state.round
       ~current_step:t.engine.state.step
       ~proposal_round:p.round

let vote_key (v : C_types.vote) =
  Printf.sprintf "%s|%Ld|%d|%s|%s"
    (vote_step_label v.vote_type)
    v.epoch_id
    v.round
    v.validator
    (raw_to_hex v.proposal_id)

let future_vote_key (v : C_types.vote) =
  Printf.sprintf "%s|%Ld|%d|%s"
    (vote_step_label v.vote_type)
    v.epoch_id
    v.round
    v.validator

type future_vote_result =
  | Future_vote_not_applicable
  | Future_vote_deferred
  | Future_vote_same
  | Future_vote_conflict of C_types.vote

let pending_vote_count t = Hashtbl.length t.pending_votes

let pending_finalize_count t = Hashtbl.length t.pending_finalizes

let max_pending_finalizes = 128

let pending_finalize_mem t epoch =
  Hashtbl.mem t.pending_finalizes epoch

let farthest_pending_finalize t =
  Hashtbl.fold
    (fun epoch _ farthest ->
      match farthest with
      | None -> Some epoch
      | Some old -> Some (Int64.max epoch old))
    t.pending_finalizes None

let queue_vote t (v : C_types.vote) =
  Hashtbl.replace t.pending_votes (vote_key v) v

let defer_future_vote t (v : C_types.vote) =
  let height = t.engine.state.height in
  if height = Int64.max_int
     || v.epoch_id <> Int64.succ height
     || v.round < 0
     || v.round > C_engine.max_round_ahead
  then
    Future_vote_not_applicable
  else
    let key = future_vote_key v in
    match Hashtbl.find_opt t.future_votes key with
    | Some prior when prior.proposal_id = v.proposal_id ->
      Future_vote_same
    | Some prior ->
      Future_vote_conflict prior
    | None ->
      Hashtbl.add t.future_votes key v;
      Future_vote_deferred

let queue_future_finalize t (f : C_types.finalize) =
  if Int64.compare f.epoch_id t.engine.state.height <= 0 then ()
  else begin
    let pending = {
      finalize = f;
      validator_set_hash = C_config.validator_set_hash t.engine.vs;
    } in
    match Hashtbl.find_opt t.pending_finalizes f.epoch_id with
    | Some old when f.commit_round < old.finalize.commit_round ->
      Hashtbl.replace t.pending_finalizes f.epoch_id pending
    | Some _ -> ()
    | None when Hashtbl.length t.pending_finalizes < max_pending_finalizes ->
      Hashtbl.add t.pending_finalizes f.epoch_id pending
    | None ->
      match farthest_pending_finalize t with
      | Some epoch when Int64.compare f.epoch_id epoch < 0 ->
        Hashtbl.remove t.pending_finalizes epoch;
        Hashtbl.add t.pending_finalizes f.epoch_id pending
      | _ -> ()
  end

let vote_still_relevant t (v : C_types.vote) =
  Int64.compare v.epoch_id t.engine.state.height = 0
  && Int64.compare v.epoch_id t.engine.finalized_height > 0

let hold_vote_fault t (v : C_types.vote) reason =
  t.vote_fault <- Some { epoch_id = v.epoch_id; round = v.round; reason };
  error_node t.config.my_addr
    "event = refuse_precommit_broadcast reason = %s epoch = %Ld round = %d"
    reason
    v.epoch_id
    v.round

let persist_precommit t (v : C_types.vote) =
  let nil_hash = String.make 32 '\x00' in
  if v.vote_type = C_types.Precommit && v.proposal_id <> nil_hash then
    let key = vote_key v in
    if Hashtbl.mem t.durable_votes key then begin
      t.vote_fault <- None;
      Lwt.return_true
    end
    else
      match
        C_engine.find_proposal_message
          t.engine
          ~proposal_id:v.proposal_id
          ~round:v.round
      with
      | Some proposal ->
        let open Lwt.Syntax in
        let* durable =
          t.config.before_precommit_broadcast
            ~epoch_id:v.epoch_id ~round:v.round
            ~proposal_id:v.proposal_id
            ~proposed_state_root:proposal.header.proposed_state_root
            ~txid_hi:proposal.header.txid_hi
            ~proposal_wire:(C_codec.encode_propose proposal)
            ~vote_wire:(C_codec.encode_vote v)
        in
        if durable then begin
          Hashtbl.replace t.durable_votes key v;
          t.vote_fault <- None
        end else
          hold_vote_fault t v "precommit_not_durable";
        Lwt.return durable
      | None ->
        hold_vote_fault t v "precommit_proposal_missing";
        Lwt.return_false
  else
    Lwt.return_true

let hold_vote_log t reason =
  t.vote_log_issue <- Some reason;
  error_node t.config.my_addr
    "event = refuse_local_vote reason = vote_log_error detail = %s"
    reason

let vote_log_reason reason =
  if String.equal reason "local vote conflicts with stored vote" then
    "vote_log_conflict"
  else if String.equal reason "vote log reached round limit" then
    "vote_log_limit"
  else if String.equal reason "vote log bootstrap required" then
    "vote_log_bootstrap"
  else if String.starts_with ~prefix:"vote log record" reason
       || String.starts_with ~prefix:"vote log wire" reason
       || String.starts_with ~prefix:"vote log epoch" reason
       || String.starts_with ~prefix:"vote log path" reason
       || String.starts_with ~prefix:"vote log floor" reason then
    "vote_log_corrupt"
  else
    "vote_log_storage"

let vote_state t =
  match t.vote_log_issue with
  | Some reason -> false, Some (vote_log_reason reason)
  | None ->
    (match t.vote_fault with
     | Some fault
       when Int64.equal fault.epoch_id t.engine.state.height
            && fault.round >= t.engine.state.round ->
       false, Some fault.reason
     | _ when not (t.config.role_can_vote ()) -> false, Some "role"
     | _ when !(t.finality_proof_needed) -> false, Some "finality_proof"
     | _ when not (vote_allowed t) -> false, Some "not_ready"
     | _ -> true, None)

let resume_local_vote t =
  match
    C_vote_log.max_round t.vote_log
      ~chain_id:t.config.chain_id
      ~validator:t.config.my_addr
      ~epoch_id:t.engine.state.height
  with
  | Ok None -> ()
  | Ok (Some round) when round < C_codec.max_round ->
    let target = round + 1 in
    if target > t.engine.state.round then begin
      C_engine.realign_round t.engine target;
      log_node t.config.my_addr
        "event = resume_local_vote epoch = %Ld prior_round = %d next_round = %d"
        t.engine.state.height
        round
        target
    end
  | Ok (Some _) ->
    hold_vote_log t "vote log reached round limit"
  | Error reason ->
    hold_vote_log t reason

let restored_vote t wire =
  try
    let vote = C_codec.decode_vote wire in
    if not (String.equal (C_codec.encode_vote vote) wire) then
      Error "vote log wire is not exact"
    else if not (String.equal vote.chain_id t.config.chain_id) then
      Error "vote log record chain does not match"
    else if not (String.equal vote.validator t.config.my_addr) then
      Error "vote log record validator does not match"
    else if not (Int64.equal vote.epoch_id t.engine.state.height) then
      Error "vote log record epoch does not match"
    else
      match C_types.pubkey_of_addr t.engine.vs vote.validator with
      | Some pubkey when C_hash.verify_vote ~pubkey_raw:pubkey vote ->
        (match C_vote_log.keep t.vote_log vote with
         | Ok _ -> Ok ()
         | Error reason -> Error reason)
      | Some _ ->
        Error "vote log record signature is invalid"
      | None ->
        Error "vote log record validator is unavailable"
  with exn ->
    Error ("vote log wire decode failed: " ^ Printexc.to_string exn)

let restore_votes t wires =
  match t.vote_log_issue, wires with
  | Some _, _ -> ()
  | None, Error reason ->
    hold_vote_log t reason
  | None, Ok wires ->
    let restored =
      List.fold_left
        (fun result wire ->
          match result with
          | Error _ -> result
          | Ok count ->
            (match restored_vote t wire with
             | Ok () -> Ok (count + 1)
             | Error reason -> Error reason))
        (Ok 0)
        wires
    in
    (match restored with
     | Error reason ->
       hold_vote_log t reason
     | Ok count ->
       resume_local_vote t;
       if count > 0 then
         log_node t.config.my_addr
           "event = vote_log_restore count = %d epoch = %Ld"
           count
           t.engine.state.height)

let require_vote_floor t =
  match t.vote_log_issue with
  | Some _ ->
    ()
  | None ->
    (match C_vote_log.floor t.vote_log with
     | Error reason ->
       hold_vote_log t reason
     | Ok (Some floor)
       when Int64.compare t.engine.state.height floor > 0 ->
       ()
     | Ok (Some _) ->
       hold_vote_log t "vote log floor is not behind current height"
     | Ok None when Int64.compare t.engine.state.height 1L <= 0 ->
       (match
          C_vote_log.set_floor
            t.vote_log
            ~through_epoch:(Int64.pred t.engine.state.height)
        with
        | Ok () -> ()
        | Error reason -> hold_vote_log t reason)
     | Ok None ->
       hold_vote_log t "vote log bootstrap required")

let prepare_vote_log t wires =
  if t.running then
    Error "consensus driver is already running"
  else begin
    t.vote_log_issue <- None;
    restore_votes t wires;
    require_vote_floor t;
    match t.vote_log_issue with
    | None -> Ok ()
    | Some reason -> Error reason
  end

let saved_vote t (v : C_types.vote) =
  if not (String.equal v.validator t.config.my_addr) then
    Lwt.return_some v
  else if !(t.finality_proof_needed) then
    Lwt.return_none
  else
    match t.vote_log_issue with
    | Some reason ->
      error_node t.config.my_addr
        "event = refuse_local_vote reason = vote_log_blocked detail = %s"
        reason;
      Lwt.return_none
    | None ->
      match C_vote_log.check t.vote_log v with
      | Error reason ->
        hold_vote_log t reason;
        Lwt.return_none
      | Ok () ->
        let open Lwt.Syntax in
        let* durable = persist_precommit t v in
        if not durable then Lwt.return_none
        else
          match C_vote_log.keep t.vote_log v with
          | Ok stored -> Lwt.return_some stored
          | Error reason ->
            hold_vote_log t reason;
            Lwt.return_none

let vote_durable t v =
  let open Lwt.Syntax in
  let* vote = saved_vote t v in
  Lwt.return (Option.is_some vote)

let broadcast_vote t (v : C_types.vote) =
  let open Lwt.Syntax in
  let* stored = saved_vote t v in
  match stored with
  | None ->
    error_node t.config.my_addr
      "event = refuse_vote_broadcast type = %s epoch = %Ld round = %d"
      (vote_step_label v.vote_type)
      v.epoch_id
      v.round;
    Lwt.return_false
  | Some vote ->
    trace_node t.config.my_addr
      "event = send_vote type = %s epoch = %Ld round = %d pid = %s"
      (vote_step_label vote.vote_type)
      vote.epoch_id
      vote.round
      (let h =
         Digestif.SHA256.to_hex
           (Digestif.SHA256.of_raw_string vote.proposal_id)
       in
       if String.length h >= 8 then String.sub h 0 8 else h);
    let payload = C_codec.encode_vote vote in
    let* () =
      Octra_net.P2p_swarm.broadcast t.swarm
        { msg_type = Frame.msg_cons_vote; payload }
    in
    Lwt.return_true

let tx_hash_batches hashes =
  let rec take count acc rest =
    if count = 0 then List.rev acc, rest
    else
      match rest with
      | [] -> List.rev acc, []
      | hash :: tail -> take (count - 1) (hash :: acc) tail
  in
  let rec loop acc rest =
    match rest with
    | [] -> List.rev acc
    | _ ->
      let batch, tail = take Octra_net.P2p_tx_gossip.max_hashes [] rest in
      loop (batch :: acc) tail
  in
  loop [] hashes

let relay_txs t hashes =
  Lwt_list.iter_s
    (fun batch ->
       let payload =
         Octra_net.P2p_tx_gossip.encode
           (Octra_net.P2p_tx_gossip.Inv batch)
       in
       Octra_net.P2p_swarm.broadcast t.swarm
         { msg_type = Frame.msg_tx_gossip; payload })
    (tx_hash_batches hashes)

let make_round_sync_at t ~round ~step ~request =
  let unsigned =
    C_codec.{
      chain_id = t.config.chain_id;
      epoch_id = t.engine.state.height;
      round;
      step;
      request;
      validator = t.config.my_addr;
      signature = String.make 64 '\x00';
    }
  in
  {
    unsigned with
    signature = t.config.sign_fn (C_hash.round_sync_sign_bytes unsigned);
  }

let make_round_sync t ~request =
  make_round_sync_at
    t
    ~round:t.engine.state.round
    ~step:t.engine.state.step
    ~request

let round_sync_frame sync =
  {
    Frame.msg_type = Frame.msg_cons_round_sync;
    payload = C_codec.encode_round_sync sync;
  }

let round_fetch_frame fetch =
  {
    Frame.msg_type = Frame.msg_cons_round_fetch;
    payload = C_codec.encode_round_fetch fetch;
  }

let local_round_sync_valid t sync =
  match C_types.pubkey_of_addr t.engine.vs t.config.my_addr with
  | Some pubkey -> C_hash.verify_round_sync ~pubkey_raw:pubkey sync
  | None -> false

let save_round_sync t sync =
  if sync.C_codec.request then begin
    C_round_pool.add_local t.round_pool sync;
    Some sync
  end else
    match
      C_sync_log.keep
        t.sync_log
        ~verify:(local_round_sync_valid t)
        sync
    with
    | Ok saved ->
      C_round_pool.add_local t.round_pool saved;
      Some saved
    | Error reason ->
      warn_node t.config.my_addr
        "event = round_sync_store_failed epoch = %Ld round = %d reason = %s"
        sync.epoch_id
        sync.round
        reason;
      None

let load_round_sync t =
  if not (local_validator t) then ()
  else
    match
      C_sync_log.load
        t.sync_log
        ~chain_id:t.config.chain_id
        ~validator:t.config.my_addr
        ~epoch_id:t.engine.state.height
        ~verify:(local_round_sync_valid t)
    with
    | Ok syncs ->
      List.iter (C_round_pool.add_local t.round_pool) syncs;
      if syncs <> [] then
        log_node t.config.my_addr
          "event = round_sync_load epoch = %Ld count = %d"
          t.engine.state.height
          (List.length syncs)
    | Error reason ->
      warn_node t.config.my_addr
        "event = round_sync_load_failed epoch = %Ld reason = %s"
        t.engine.state.height
        reason

let broadcast_round_sync t ~request =
  if not (local_validator t)
     || (not request && not (vote_allowed t)) then
    Lwt.return_unit
  else begin
    match save_round_sync t (make_round_sync t ~request) with
    | Some sync ->
      Octra_net.P2p_swarm.broadcast
        t.swarm
        (round_sync_frame sync)
    | None -> Lwt.return_unit
  end

let round_sync_request_for_step = function
  | C_types.ProposeStep -> false
  | C_types.PrevoteStep
  | C_types.PrecommitStep -> true

let broadcast_round_sync_at t ~round =
  if not (local_validator t) || not (vote_allowed t) then
    Lwt.return_unit
  else
    Octra_net.P2p_swarm.broadcast
      t.swarm
      (round_sync_frame
         (make_round_sync_at
            t
            ~round
            ~step:C_types.PrevoteStep
            ~request:true))

let broadcast_round_fetch t fetch =
  if not (local_validator t) then Lwt.return_unit
  else Octra_net.P2p_swarm.broadcast t.swarm (round_fetch_frame fetch)

let round_peer_gap = 2
let round_peer_age = 30.0
let round_spread_limit = 100
let round_spread_warn_interval = 60.0

let next_past_round ~height ~round ~limit ~(prior : past_round option) ~now =
  match prior with
  | Some prior
    when Int64.equal prior.epoch_id height
      && Int64.compare (Int64.sub now prior.sent_at) 5_000_000_000L < 0 ->
    None
  | Some prior
    when Int64.equal prior.epoch_id height
      && prior.tries = 0
      && round < prior.target ->
    Some (prior.target, 1)
  | Some prior when Int64.equal prior.epoch_id height ->
    let base = max round prior.target in
    Some (min limit (base + C_engine.round_history_limit), 0)
  | Some _
  | None ->
    Some (min limit (round + C_engine.round_history_limit), 0)

let past_round_target t ~limit now =
  next_past_round
    ~height:t.engine.state.height
    ~round:t.engine.state.round
    ~limit
    ~prior:t.past_round
    ~now

let ask_past t =
  let state = t.engine.state in
  let gap = C_engine.round_history_limit / 2 in
  let now = Unix.gettimeofday () in
  let ahead =
    Hashtbl.fold
      (fun _ (peer : round_peer_record) found ->
        if Int64.equal peer.epoch_id state.height
           && peer.round >= state.round + gap
           && now -. peer.last_seen >= 0.0
           && now -. peer.last_seen <= round_peer_age
        then Some (max peer.round (Option.value ~default:0 found))
        else found)
      t.round_peers
      None
  in
  match ahead with
  | None -> Lwt.return_unit
  | Some peer ->
    let sent_at = Mtime_clock.elapsed_ns () in
    let limit = min peer (state.round + C_engine.max_sync_ahead) in
    (match past_round_target t ~limit sent_at with
     | None -> Lwt.return_unit
     | Some (target, tries) ->
       let after = max state.round (target - C_engine.round_history_limit) in
       if after >= target then Lwt.return_unit
       else begin
         t.past_round <- Some { epoch_id = state.height; target; sent_at; tries };
         log_node t.config.my_addr
           "event = round_fetch epoch = %Ld after = %d through = %d"
           state.height
           after
           target;
         broadcast_round_fetch t C_codec.{
           chain_id = t.config.chain_id;
           epoch_id = state.height;
           after_round = after;
           through_round = target;
         }
       end)

let proposal_fetch_attempts = 4
let proposal_fetch_interval = 1.05

let proposal_fetch_key fetch =
  Printf.sprintf
    "%d:%Ld:%d:%s"
    fetch.generation
    fetch.height
    fetch.round
    (raw_to_hex fetch.proposal_id)

let decide_proposal_fetch
    fetch
    ~running
    ~height
    ~round
    ~generation
    ~proposal_known
    ~attempts_left =
  if not running
     || attempts_left <= 0
     || height <> fetch.height
     || round <> fetch.round
     || generation <> fetch.generation
     || proposal_known
  then
    Stop_proposal_fetch
  else
    Send_proposal_fetch

let start_proposal_fetch t ~round ~proposal_id =
  if not (local_validator t) || not (vote_allowed t) then
    Lwt.return_unit
  else
    let fetch = {
      height = t.engine.state.height;
      round;
      proposal_id;
      generation = t.engine.generation;
    } in
    let key = proposal_fetch_key fetch in
    if Hashtbl.mem t.proposal_fetches key then
      Lwt.return_unit
    else begin
      Hashtbl.add t.proposal_fetches key ();
      let open Lwt.Syntax in
      let rec run attempts_left =
        let proposal_known =
          Option.is_some
            (C_engine.find_proposal_message
               t.engine
               ~proposal_id:fetch.proposal_id
               ~round:fetch.round)
        in
        match
          decide_proposal_fetch
            fetch
            ~running:t.running
            ~height:t.engine.state.height
            ~round:t.engine.state.round
            ~generation:t.engine.generation
            ~proposal_known
            ~attempts_left
        with
        | Stop_proposal_fetch ->
          Lwt.return_unit
        | Send_proposal_fetch ->
          log_node t.config.my_addr
            "event = request_proposal epoch = %Ld round = %d attempt = %d"
            fetch.height
            fetch.round
            (proposal_fetch_attempts - attempts_left + 1);
          let* () = broadcast_round_sync_at t ~round:fetch.round in
          if attempts_left = 1 then
            Lwt.return_unit
          else
            let* () = Lwt_unix.sleep proposal_fetch_interval in
            run (attempts_left - 1)
      in
      Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            Lwt.finalize
              (fun () -> run proposal_fetch_attempts)
              (fun () ->
                Hashtbl.remove t.proposal_fetches key;
                Lwt.return_unit))
          (fun exn ->
            warn_node t.config.my_addr
              "event = request_proposal_failed epoch = %Ld round = %d error = %s"
              fetch.height
              fetch.round
              (Printexc.to_string exn);
            Lwt.return_unit));
      Lwt.return_unit
    end

let round_vote_rank (vote : C_types.vote) =
  match vote.vote_type with
  | C_types.Prevote -> 0
  | C_types.Precommit -> 1

let compare_round_vote (left : C_types.vote) (right : C_types.vote) =
  let by_type = compare (round_vote_rank left) (round_vote_rank right) in
  if by_type <> 0 then by_type
  else String.compare left.validator right.validator

let current_round_votes t =
  let values set =
    Hashtbl.fold
      (fun _ (vote : C_types.vote) acc ->
        if vote.epoch_id = t.engine.state.height
           && vote.round = t.engine.state.round
        then vote :: acc
        else acc)
      set.C_engine.votes
      []
  in
  values t.engine.prevotes @ values t.engine.precommits
  |> List.sort compare_round_vote

let local_round_proposal t =
  match t.engine.current_proposal with
  | Some proposal
    when proposal.epoch_id = t.engine.state.height
      && proposal.round = t.engine.state.round ->
    Some proposal
  | _ -> None

let send_vote_to t conn vote =
  let open Lwt.Syntax in
  let* stored = saved_vote t vote in
  match stored with
  | None ->
    error_node t.config.my_addr
      "event = refuse_round_sync_vote type = %s epoch = %Ld round = %d"
      (vote_step_label vote.C_types.vote_type)
      vote.epoch_id
      vote.round;
    Lwt.return_unit
  | Some vote ->
    Octra_net.P2p_conn.send
      conn
      {
        Frame.msg_type = Frame.msg_cons_vote;
        payload = C_codec.encode_vote vote;
      }

let send_verified_vote_to conn vote =
  Octra_net.P2p_conn.send
    conn
    {
      Frame.msg_type = Frame.msg_cons_vote;
      payload = C_codec.encode_vote vote;
    }

let send_round_vote_to t conn (vote : C_types.vote) =
  if String.equal vote.validator t.config.my_addr then
    send_vote_to t conn vote
  else
    send_verified_vote_to conn vote

let send_round_sync_response t conn ~requested_round ~with_witness =
  let open Lwt.Syntax in
  let current_sync =
    if local_validator t && vote_allowed t then
      save_round_sync t (make_round_sync t ~request:false)
    else
      None
  in
  let compat_sync =
    C_round_pool.sent_reply
      t.round_pool
      ~epoch_id:t.engine.state.height
      ~after_round:requested_round
      ~through_round:(requested_round + C_engine.max_round_ahead)
  in
  let* () =
    match current_sync with
    | Some sync ->
      Octra_net.P2p_conn.send
        conn
        (round_sync_frame sync)
    | None -> Lwt.return_unit
  in
  let* () =
    match current_sync, compat_sync with
    | Some current, Some compat when current = compat -> Lwt.return_unit
    | _, Some sync ->
      Octra_net.P2p_conn.send
        conn
        (round_sync_frame sync)
    | _, None -> Lwt.return_unit
  in
  let witness =
    if with_witness then
      C_round_pool.witness
        t.round_pool
        ~chain_id:t.config.chain_id
        ~epoch_id:t.engine.state.height
        ~after_round:requested_round
        ~through_round:(requested_round + C_engine.max_sync_ahead)
        ~validator_set:t.engine.vs
      |> List.filter (fun sync -> sync.C_codec.validator <> t.config.my_addr)
    else
      []
  in
  let* () =
    Lwt_list.iter_s
      (fun sync ->
        Octra_net.P2p_conn.send conn (round_sync_frame sync))
      witness
  in
  let* () =
    match local_round_proposal t with
    | None -> Lwt.return_unit
    | Some proposal ->
      Octra_net.P2p_conn.send
        conn
        {
          Frame.msg_type = Frame.msg_cons_propose;
          payload = C_codec.encode_propose proposal;
        }
  in
  let* () =
    Lwt_list.iter_s (send_round_vote_to t conn) (current_round_votes t)
  in
  if requested_round < t.engine.state.round then
    Lwt_list.iter_s
      (send_round_vote_to t conn)
      (C_engine.polc_votes_for_round t.engine requested_round)
  else
    Lwt.return_unit

let round_step_rank = function
  | C_types.ProposeStep -> 1
  | C_types.PrevoteStep -> 2
  | C_types.PrecommitStep -> 3

let round_spread = function
  | []
  | [ _ ] -> 0
  | rounds ->
    let low = List.fold_left min max_int rounds in
    let high = List.fold_left max min_int rounds in
    high - low

let round_spread_warning
    ~now
    ~last_warned_at
    ~epoch_id
    ~local_round
    observations =
  if now -. last_warned_at < round_spread_warn_interval then
    None
  else
    let rounds =
      List.fold_left
        (fun acc (peer_epoch, peer_round, last_seen) ->
          if peer_epoch = epoch_id && now -. last_seen <= round_peer_age then
            peer_round :: acc
          else
            acc)
        [ local_round ]
        observations
    in
    let spread = round_spread rounds in
    if spread > round_spread_limit then
      Some (spread, List.length rounds - 1)
    else
      None

let warn_round_spread t ~now =
  let state = t.engine.state in
  let observations =
    Hashtbl.fold
      (fun _ (peer : round_peer_record) acc ->
        (peer.epoch_id, peer.round, peer.last_seen) :: acc)
      t.round_peers
      []
  in
  match round_spread_warning
          ~now
          ~last_warned_at:t.round_spread_warned_at
          ~epoch_id:state.height
          ~local_round:state.round
          observations with
  | Some (spread, peers) ->
    t.round_spread_warned_at <- now;
    warn_node t.config.my_addr
      "event = consensus_round_spread spread = %d peers = %d local_round = %d"
      spread
      peers
      state.round
  | None -> ()

let round_peer_later (peer : round_peer_record) (sync : C_codec.round_sync) =
  Int64.compare sync.C_codec.epoch_id peer.epoch_id > 0
  || (sync.epoch_id = peer.epoch_id
      && (sync.round > peer.round
          || (sync.round = peer.round
              && round_step_rank sync.step > round_step_rank peer.step)))

let remember_round_peer t (sync : C_codec.round_sync) =
  let now = Unix.gettimeofday () in
  begin
    match Hashtbl.find_opt t.round_peers sync.C_codec.validator with
    | Some peer when round_peer_later peer sync ->
      peer.epoch_id <- sync.epoch_id;
      peer.round <- sync.round;
      peer.step <- sync.step;
      peer.last_seen <- now
    | Some _ ->
      ()
    | None ->
      Hashtbl.replace t.round_peers sync.validator {
        validator_addr = sync.validator;
        epoch_id = sync.epoch_id;
        round = sync.round;
        step = sync.step;
        last_seen = now;
      }
  end;
  warn_round_spread t ~now

let round_state t = {
  epoch_id = t.engine.state.height;
  round = t.engine.state.round;
  step = t.engine.state.step;
}

let vote_tallies validator_set votes =
  let table = Hashtbl.create validator_set.C_types.n in
  Hashtbl.iter
    (fun _ (vote : C_types.vote) ->
      let voters, weight =
        match Hashtbl.find_opt table vote.proposal_id with
        | Some value -> value
        | None -> 0, Z.zero
      in
      let weight =
        match C_types.weight_of_addr validator_set vote.validator with
        | Some value -> Z.add weight value
        | None -> weight
      in
      Hashtbl.replace table vote.proposal_id (voters + 1, weight))
    votes.C_engine.votes;
  Hashtbl.to_seq table
  |> List.of_seq
  |> List.map (fun (proposal_id, (voters, weight)) -> {
       proposal_id;
       voters;
       weight;
     })
  |> List.sort (fun left right ->
       let by_weight = Z.compare right.weight left.weight in
       if by_weight <> 0 then by_weight
       else
         let by_voters = Int.compare right.voters left.voters in
         if by_voters <> 0 then by_voters
         else String.compare left.proposal_id right.proposal_id)

let round_vote_snapshot t = {
  prevotes = vote_tallies t.engine.vs t.engine.prevotes;
  precommits = vote_tallies t.engine.vs t.engine.precommits;
  quorum = t.engine.vs.quorum;
  quorum_weight = t.engine.vs.quorum_weight;
}

let round_peer_snapshot t =
  Hashtbl.fold (fun _ peer acc -> peer :: acc) t.round_peers []

let round_witness t ~validator =
  if not (C_types.is_validator t.engine.vs validator) then None
  else
    match
      C_round_pool.reply
        t.round_pool
        ~epoch_id:t.engine.state.height
        ~validator
    with
    | None -> None
    | Some (sync, _) when sync.C_codec.request -> None
    | Some (sync, seen_at) ->
      Some {
        wire = C_codec.encode_round_sync sync;
        seen_at;
      }

let round_agreed t ~now =
  let state = round_state t in
  let signers =
    Hashtbl.fold
      (fun _ peer acc ->
        let age = now -. peer.last_seen in
        if peer.epoch_id = state.epoch_id
           && abs (peer.round - state.round) <= round_peer_gap
           && age >= 0.0
           && age <= round_peer_age
        then peer.validator_addr :: acc
        else acc)
      t.round_peers
      (if local_validator t && vote_allowed t then [t.config.my_addr]
       else [])
  in
  C_types.has_quorum_at
    ~chain_id:t.config.chain_id
    ~epoch_id:state.epoch_id
    t.engine.vs
    signers

let round_sync_reply_needed t (sync : C_codec.round_sync) =
  sync.request
  || sync.round < t.engine.state.round
  || (sync.round = t.engine.state.round
      && round_step_rank sync.step < round_step_rank t.engine.state.step)

let round_sync_source_matches t conn (sync : C_codec.round_sync) =
  match C_types.pubkey_of_addr t.engine.vs sync.validator with
  | None -> false
  | Some pubkey ->
    String.equal
      conn.Octra_net.P2p_conn.peer_id
      (Octra_net.P2p_handshake.node_id_of_pubkey pubkey)

let round_fetch_source_matches t conn =
  List.exists
    (fun (validator : C_types.validator_info) ->
      String.equal
        conn.Octra_net.P2p_conn.peer_id
        (Octra_net.P2p_handshake.node_id_of_pubkey validator.pubkey))
    t.engine.vs.validators

let round_sync_response_due ~last ~now =
  match last with
  | None -> true
  | Some prior ->
    Int64.compare now prior >= 0
    && Int64.compare (Int64.sub now prior) 1_000_000_000L >= 0

let round_sync_reply_progresses (prior : round_sync_reply) (sync : C_codec.round_sync) =
  Int64.compare sync.epoch_id prior.epoch_id > 0
  || (Int64.equal sync.epoch_id prior.epoch_id
      && sync.round = prior.round
      && round_step_rank sync.step > round_step_rank prior.step)

let round_sync_response_allowed_at t (sync : C_codec.round_sync) now =
  let prior = Hashtbl.find_opt t.round_sync_replies sync.validator in
  let due =
    round_sync_response_due
      ~last:(Option.map (fun (reply : round_sync_reply) -> reply.sent_at) prior)
      ~now
  in
  let progresses =
    Option.fold ~none:false ~some:(fun reply ->
      round_sync_reply_progresses reply sync) prior
  in
  if due || progresses then begin
    let reply =
      match prior with
      | Some reply when not progresses ->
        { reply with sent_at = now }
      | None
      | Some _ ->
        {
          epoch_id = sync.epoch_id;
          round = sync.round;
          step = sync.step;
          sent_at = now;
        }
    in
    Hashtbl.replace
      t.round_sync_replies
      sync.validator
      reply;
    true
  end else
    false

let round_sync_response_allowed t sync =
  round_sync_response_allowed_at t sync (Mtime_clock.elapsed_ns ())

let round_fetch_valid t (fetch : C_codec.round_fetch) =
  let state = t.engine.state in
  String.equal fetch.chain_id t.config.chain_id
  && Int64.equal fetch.epoch_id state.height
  && fetch.after_round >= max 0 (state.round - C_engine.sync_history_limit)
  && fetch.after_round < fetch.through_round
  && fetch.through_round <= state.round
  && fetch.through_round - fetch.after_round <= C_engine.round_history_limit

let round_fetch_response_allowed t conn =
  let now = Mtime_clock.elapsed_ns () in
  match Hashtbl.find_opt t.round_fetch_replies conn.Octra_net.P2p_conn.peer_id with
  | Some prior
    when Int64.compare (Int64.sub now prior.sent_at) 5_000_000_000L < 0 -> false
  | None
  | Some _ ->
    Hashtbl.replace
      t.round_fetch_replies
      conn.Octra_net.P2p_conn.peer_id
      { sent_at = now };
    true

let send_round_fetch_response t conn (fetch : C_codec.round_fetch) =
  C_round_pool.sent_range
    t.round_pool
    ~epoch_id:fetch.C_codec.epoch_id
    ~after_round:fetch.after_round
    ~through_round:fetch.through_round
  |> Lwt_list.iter_s (fun sync ->
    Octra_net.P2p_conn.send conn (round_sync_frame sync))

let flush_pending_votes t =
  if not (vote_allowed t) then Lwt.return_unit
  else begin
    let queued =
      Hashtbl.fold (fun key vote acc -> (key, vote) :: acc) t.pending_votes [] in
    List.iter (fun (key, _) -> Hashtbl.remove t.pending_votes key) queued;
    let votes =
      queued
      |> List.filter_map (fun (_, vote) ->
        if vote_still_relevant t vote then Some vote else None)
      |> List.sort (fun (a : C_types.vote) b ->
        let c = Int64.compare a.epoch_id b.epoch_id in
        if c <> 0 then c else
        let c = compare a.round b.round in
        if c <> 0 then c else
        compare (vote_step_label a.vote_type) (vote_step_label b.vote_type))
    in
    match votes with
    | [] -> Lwt.return_unit
    | _ ->
      trace_node t.config.my_addr "event = flush_pending_votes count = %d"
        (List.length votes);
      Lwt_list.iter_s
        (fun vote ->
          let open Lwt.Syntax in
          let* _ = broadcast_vote t vote in
          Lwt.return_unit)
        votes
  end

let verify_set_signature validator_set addr msg signature =
  match C_types.pubkey_of_addr validator_set addr with
  | Some pk -> C_hash.verify_ed25519 ~pubkey_raw:pk ~msg ~signature
  | None -> false

let verify_engine_signature t addr msg signature =
  verify_set_signature t.engine.vs addr msg signature

let proposal_round_key epoch round =
  Printf.sprintf "%Ld|%d" epoch round

let defer_verified_proposal t (p : C_types.propose) =
  let leader =
    C_engine.leader_of t.engine.vs ~epoch_id:p.epoch_id ~round:p.round
  in
  if leader.address <> p.proposer then ()
  else
    let key = proposal_round_key p.epoch_id p.round in
    if not (Hashtbl.mem t.deferred_proposals key) then
      Hashtbl.add t.deferred_proposals key p

let defer_pending_proposal t (p : C_types.propose) =
  let height = t.engine.state.height in
  if height = Int64.max_int
     || p.epoch_id <> Int64.succ height
     || p.round < 0
     || p.round > C_engine.max_round_ahead
  then
    false
  else
    let key = proposal_round_key p.epoch_id p.round in
    if Hashtbl.mem t.pending_proposals key then
      false
    else begin
      Hashtbl.add t.pending_proposals key p;
      true
    end

let replay_deferred_proposal t =
  let height = t.engine.state.height in
  let round = t.engine.state.round in
  let key = proposal_round_key height round in
  match Hashtbl.find_opt t.deferred_proposals key with
  | None -> ()
  | Some p ->
    Hashtbl.remove t.deferred_proposals key;
    log_node t.config.my_addr
      "event = replay_deferred_proposal epoch = %Ld round = %d"
      height round;
    C_engine.on_propose
      t.engine
      p
      ~verify_fn:(verify_engine_signature t)
      ~execute_fn:(fun _ -> true)
      ~sign_fn:t.config.sign_fn

let remember_peer_state t ~source ~responder_addr ~head_epoch ~checked_epoch ~state_root =
  match Hashtbl.find_opt t.peer_states responder_addr with
  | Some rec_ ->
      rec_.head_epoch <- head_epoch;
      rec_.checked_epoch <- checked_epoch;
      rec_.state_root <- state_root;
      rec_.last_seen <- Unix.gettimeofday ();
      rec_.source <- source
  | None ->
      Hashtbl.replace t.peer_states responder_addr {
        responder_addr;
        head_epoch;
        checked_epoch;
        state_root;
        last_seen = Unix.gettimeofday ();
        source;
      }

let peer_state_snapshot t =
  Hashtbl.fold (fun _ rec_ acc -> rec_ :: acc) t.peer_states []

let msg_id msg_type payload =
  Octra_net.Hash_domain.hash
    "octra:seen:consensus:v1"
    (String.make 1 (Char.chr msg_type) ^ payload)

let is_seen t msg_type payload =
  let id = msg_id msg_type payload in
  not (C_seen.remember t.seen id)

let frame_known t msg_type payload =
  C_seen.known t.seen (msg_id msg_type payload)

let remember_frame t msg_type payload =
  ignore (C_seen.remember t.seen (msg_id msg_type payload))

let historical_vote_replay_needed t payload =
  try
    let vote = C_codec.decode_vote payload in
    vote.chain_id = t.config.chain_id
    && vote.epoch_id = t.engine.state.height
    && vote.vote_type = C_types.Prevote
    && vote.round < t.engine.state.round
    && C_engine.polc_request_pending t.engine vote.round
    && not
         (C_engine.historical_prevote_known
            t.engine
            ~round:vote.round
            ~validator:vote.validator)
    && C_seen.remember
         t.historical_replays
         (msg_id Frame.msg_cons_vote payload)
  with _ -> false

let finalize_replay_needed t payload =
  try
    let finalize = C_codec.decode_finalize payload in
    finalize.chain_id = t.config.chain_id
    && finalize.epoch_id = t.engine.state.height
    && Int64.compare finalize.epoch_id t.engine.finalized_height > 0
    && C_seen.remember
         t.historical_replays
         (msg_id Frame.msg_cons_finalize payload)
  with _ -> false

let historical_replay_needed t msg_type payload =
  if msg_type = Frame.msg_cons_vote then
    historical_vote_replay_needed t payload
  else if msg_type = Frame.msg_cons_finalize then
    finalize_replay_needed t payload
  else
    false

let resource_attestation_pool t =
  Hashtbl.fold (fun _ attestation acc -> attestation :: acc) t.resource_attestations []

let vote_evidence_window = 128L
let max_vote_evidence = 4_096

let prune_vote_evidence t epoch =
  let oldest = Int64.sub epoch vote_evidence_window in
  Hashtbl.filter_map_inplace
    (fun _ evidence ->
      if evidence.C_evidence.second.epoch_id < oldest then None
      else Some evidence)
    t.vote_evidence

let remember_vote_evidence t evidence =
  prune_vote_evidence t evidence.C_evidence.second.epoch_id;
  let slot = C_evidence.vote_slot_id evidence.C_evidence.second in
  if Hashtbl.mem t.vote_evidence slot then false
  else if Hashtbl.length t.vote_evidence >= max_vote_evidence then false
  else begin
    Hashtbl.replace t.vote_evidence slot evidence;
    true
  end

let vote_evidence t =
  Hashtbl.fold (fun _ evidence acc -> evidence :: acc) t.vote_evidence []
  |> List.sort (fun left right ->
    String.compare
      (C_evidence.vote_conflict_id left)
      (C_evidence.vote_conflict_id right))

let vote_evidence_root t =
  C_evidence.vote_conflict_root (vote_evidence t)

let vote_evidence_in_window ~current evidence =
  let epoch = evidence.C_evidence.second.epoch_id in
  let newest =
    if current = Int64.max_int then current else Int64.succ current
  in
  epoch <= newest && epoch >= Int64.sub current vote_evidence_window

let validate_vote_evidence ~chain_id ~validator_set ~current evidence =
  let vote = evidence.C_evidence.second in
  if
    vote.chain_id <> chain_id
    || not (vote_evidence_in_window ~current evidence)
  then Evidence_invalid
  else
    match C_types.pubkey_of_addr validator_set vote.validator with
    | None -> Evidence_unknown_validator
    | Some pubkey when
        C_evidence.verify_vote_conflict ~pubkey_raw:pubkey evidence ->
      Evidence_valid
    | Some _ -> Evidence_invalid

let vote_evidence_frame evidence =
  {
    Frame.msg_type = Frame.msg_vote_evidence;
    payload = C_evidence.encode_vote_conflict evidence;
  }

let report_validator_identity t validator_set validator ~reason =
  match C_types.pubkey_of_addr validator_set validator with
  | None -> ()
  | Some pubkey ->
    Octra_net.P2p_swarm.report_bad_identity
      t.swarm
      ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey pubkey)
      ~reason

let report_proposal_error t conn validator_set proposer error =
  match proposal_fault_source error with
  | Proposal_fault_unresolved -> ()
  | Proposal_fault_sender ->
    Octra_net.P2p_swarm.report_bad_peer
      t.swarm
      conn
      ~reason:(proposal_frame_peer_reason error)
  | Proposal_fault_signer ->
    report_validator_identity
      t
      validator_set
      proposer
      ~reason:(proposal_frame_peer_reason error)

let record_vote_conflict ~validator_set t prior vote =
  match C_evidence.vote_conflict prior vote with
  | None -> None
  | Some evidence ->
    let evidence_id = C_evidence.vote_conflict_id evidence in
    let remembered = remember_vote_evidence t evidence in
    error_node t.config.my_addr
      "event = vote_equivocation epoch = %Ld round = %d validator = %s evidence = %s stored = %b"
      vote.epoch_id
      vote.round
      (String.sub vote.validator 0 (min 12 (String.length vote.validator)))
      (Digestif.SHA256.to_hex
        (Digestif.SHA256.of_raw_string evidence_id)
       |> fun hex -> String.sub hex 0 16)
      remembered;
    report_validator_identity
      t
      validator_set
      vote.validator
      ~reason:"vote_equivocation";
    if remembered then Some evidence else None

let accept_dynamic_plan t cfg =
  match t.plan_mark with
  | None ->
    t.plan_mark <- Some (cfg.activate_epoch, cfg.fingerprint);
    true
  | Some (prior_epoch, prior_fingerprint) ->
    let order = Int64.compare cfg.activate_epoch prior_epoch in
    if order > 0 then begin
      t.plan_mark <- Some (cfg.activate_epoch, cfg.fingerprint);
      true
    end
    else if order = 0 && String.equal cfg.fingerprint prior_fingerprint then
      true
    else begin
      error_node t.config.my_addr
        "event = validator_set_plan_refused reason = regression activate_epoch = %Ld prior_epoch = %Ld"
        cfg.activate_epoch
        prior_epoch;
      false
    end

let static_plan t =
  match t.config.scheduled_validator_set_config with
  | Some cfg when Int64.compare cfg.activate_epoch t.engine.state.height < 0 ->
    t.plan_seen <- true;
    error_node t.config.my_addr
      "event = validator_set_plan_refused reason = stale_static activate_epoch = %Ld head = %Ld"
      cfg.activate_epoch
      t.engine.state.height;
    None
  | plan -> plan

let load_validator_set_plan t =
  let open Lwt.Syntax in
  let* dynamic_cfg = t.config.load_scheduled_validator_set_config () in
  match dynamic_cfg with
  | Some cfg when accept_dynamic_plan t cfg ->
    t.plan_seen <- true;
    Lwt.return_some cfg
  | Some _ ->
    t.plan_seen <- true;
    Lwt.return_none
  | None when t.plan_seen -> Lwt.return_none
  | None -> Lwt.return (static_plan t)

let validator_set_at ~chain_id ~current ~epoch plan =
  let source =
    match plan with
    | Some cfg when epoch >= cfg.activate_epoch -> cfg.validator_set
    | Some _
    | None -> current
  in
  C_types.validator_set_for_epoch ~chain_id ~epoch_id:epoch source

let validator_set_for_frame t epoch =
  let open Lwt.Syntax in
  if epoch <= t.engine.state.height then
    Lwt.return t.engine.vs
  else
    let* plan = load_validator_set_plan t in
    Lwt.return
      (validator_set_at
         ~chain_id:t.config.chain_id
         ~current:t.engine.vs
         ~epoch
         plan)

let maybe_activate_scheduled_validator_set_raw t ~target_epoch =
  let open Lwt.Syntax in
  let* cfg_opt = load_validator_set_plan t in
  match cfg_opt with
  | None -> Lwt.return_unit
  | Some cfg ->
    if Int64.compare target_epoch cfg.activate_epoch < 0 then
      Lwt.return_unit
    else
      let validator_set =
        C_types.validator_set_for_epoch
          ~chain_id:t.config.chain_id
          ~epoch_id:target_epoch
          cfg.validator_set
      in
      let current_hash = C_config.validator_set_hash t.engine.vs in
      let target_hash = C_config.validator_set_hash validator_set in
      if String.equal current_hash target_hash then
        Lwt.return_unit
      else if Int64.compare cfg.activate_epoch t.engine.state.height < 0 then begin
        error_node t.config.my_addr
          "event = validator_set_activation_refused reason = stale_plan activate_epoch = %Ld head = %Ld"
          cfg.activate_epoch
          t.engine.state.height;
        Lwt.return_unit
      end
      else begin
      let* () =
        t.on_validator_set_activated
          validator_set
          cfg.fingerprint
      in
      C_engine.replace_validator_set t.engine validator_set;
      Hashtbl.clear t.round_peers;
      t.n_validators <- validator_set.C_types.n;
      Hashtbl.replace t.activated_validator_set_fingerprints cfg.fingerprint true;
      log_node t.config.my_addr
        "event = validator_set_activated target_epoch = %Ld n = %d quorum = %d fingerprint = %s"
        target_epoch validator_set.n validator_set.quorum cfg.fingerprint;
      if C_types.is_validator validator_set t.config.my_addr then
        Lwt.return_unit
      else begin
        log_node t.config.my_addr
          "event = validator_set_observer target_epoch = %Ld"
          target_epoch;
        Lwt.return_unit
      end
      end

let maybe_activate_quorum_policy t ~target_epoch =
  let open Lwt.Syntax in
  let current = t.engine.vs in
  let effective =
    C_types.validator_set_for_epoch
      ~chain_id:t.config.chain_id
      ~epoch_id:target_epoch
      current
  in
  if String.equal
       (C_config.validator_set_hash current)
       (C_config.validator_set_hash effective)
  then
    Lwt.return_unit
  else
    let fingerprint =
      C_config.validator_set_hash effective
      |> raw_to_hex
    in
    let* () = t.on_validator_set_activated effective fingerprint in
    C_engine.replace_validator_set t.engine effective;
    Hashtbl.clear t.round_peers;
    t.n_validators <- effective.n;
    log_node t.config.my_addr
      "event = validator_quorum_policy_activated target_epoch = %Ld n = %d quorum = %d total_weight = %s quorum_weight = %s fingerprint = %s"
      target_epoch
      effective.n
      effective.quorum
      (Z.to_string effective.total_weight)
      (Z.to_string effective.quorum_weight)
      fingerprint;
    Lwt.return_unit

let maybe_activate_scheduled_validator_set t ~target_epoch =
  let open Lwt.Syntax in
  let* () = maybe_activate_scheduled_validator_set_raw t ~target_epoch in
  maybe_activate_quorum_policy t ~target_epoch

let resource_committee_snapshot t ~activation_delay ~committee_size ~target_epoch ~source_seed =
  Resource_attestation_flow.select_snapshot
    ~activation_delay
    ~committee_size
    ~target_epoch
    ~source_seed
    (resource_attestation_pool t)

let maybe_activate_resource_committee t ~target_epoch =
  match t.config.resource_committee_config with
  | None -> Lwt.return_unit
  | Some cfg ->
      match Resource_attestation_flow.activated_source_epoch
        ~activation_delay:cfg.activation_delay ~target_epoch with
      | None -> Lwt.return_unit
      | Some source_epoch ->
          match cfg.source_seed_for_epoch source_epoch with
          | None -> Lwt.return_unit
          | Some source_seed ->
              match resource_committee_snapshot t
                ~activation_delay:cfg.activation_delay
                ~committee_size:cfg.committee_size
                ~target_epoch
                ~source_seed with
              | None -> Lwt.return_unit
              | Some snapshot
                when not (Resource_attestation_flow.ready_for_voting
                  ~minimum_weight:cfg.minimum_weight snapshot) ->
                  log_node t.config.my_addr
                    "event = resource_committee_pending target_epoch = %Ld source_epoch = %Ld weight = %Ld minimum = %Ld"
                    target_epoch snapshot.source_epoch snapshot.total_weight cfg.minimum_weight;
                  Lwt.return_unit
              | Some snapshot ->
                  match Resource_attestation_flow.validator_set_of_committee
                    ~pubkey_of_node:cfg.pubkey_of_node snapshot.committee with
                  | None ->
                      log_node t.config.my_addr
                        "event = resource_committee_rejected target_epoch = %Ld reason = missing_pubkey"
                        target_epoch;
                      Lwt.return_unit
                  | Some validator_set ->
                      C_engine.replace_validator_set t.engine validator_set;
                      Hashtbl.clear t.round_peers;
                      t.n_validators <- validator_set.C_types.n;
                      let root_hex = raw_to_hex snapshot.committee_root in
                      log_node t.config.my_addr
                        "event = resource_committee_activated target_epoch = %Ld source_epoch = %Ld n = %d quorum = %d weight = %Ld root = %s"
                        target_epoch snapshot.source_epoch
                        validator_set.n validator_set.quorum snapshot.total_weight
                        (String.sub root_hex 0 (min 16 (String.length root_hex)));
                      cfg.on_committee_selected snapshot

let try_current_leader_proposal t =
  let open Lwt.Syntax in
  if t.running
     && C_engine.am_i_leader t.engine
     && t.engine.state.step = C_types.ProposeStep then begin
    let gen = t.engine.generation in
    let height = t.engine.state.height in
    let round = t.engine.state.round in
    let step = t.engine.state.step in
    if proposal_retry_pending t ~gen ~height ~round ~step then
      Lwt.return_false
    else begin
      t.proposal_retry <- None;
      match t.proposal_build with
      | Some _ ->
        Lwt.return_false
      | None ->
        let work = {
          gen;
          height;
          round;
          step;
          started_at = Mtime_clock.elapsed_ns ();
        } in
        t.proposal_build <- Some work;
        let* proposal_opt =
          Lwt.finalize
            (fun () ->
              C_proposal_work_gate.run
                t.proposal_work_gate
                ~relevant:(fun () ->
                  t.running
                  && C_engine.am_i_leader t.engine
                  && proposal_work_current t work)
                (fun () -> t.config.make_proposal height))
            (fun () ->
              clear_proposal_build t ~gen ~height ~round ~step;
              Lwt.return_unit)
        in
        if t.engine.generation = gen
           && t.engine.state.height = height
           && t.engine.state.round = round
           && t.engine.state.step = step then
          match proposal_opt with
          | Some plan ->
            C_engine.do_propose
              ?parent_commit:plan.parent_commit
              t.engine
              plan.header
              plan.tx_hashes
              ~sign_fn:t.config.sign_fn;
            Lwt.return (t.engine.state.step <> step)
          | None ->
            t.proposal_retry <- Some {
              work with
              started_at = Mtime_clock.elapsed_ns ();
            };
            log_node t.config.my_addr
              "event = make_proposal_none height = %Ld round = %d"
              height
              round;
            Lwt.return_false
        else begin
          log_node t.config.my_addr
            "event = make_proposal_stale old_generation = %d new_generation = %d old_height = %Ld new_height = %Ld old_round = %d new_round = %d old_step = %s new_step = %s"
            gen
            t.engine.generation
            height
            t.engine.state.height
            round
            t.engine.state.round
            (round_step_label step)
            (round_step_label t.engine.state.step);
          Lwt.return_false
        end
    end
  end else
    Lwt.return_false

let admit_resource_attestation t attestation =
  match t.config.resource_committee_config with
  | None ->
      Resource_attestation_admission.Reject
        Resource_attestation_admission.Disabled
  | Some cfg ->
      match cfg.source_seed_for_epoch attestation.Resource_attestations.epoch_id with
      | None -> Resource_attestation_admission.Quarantine Resource_attestation_admission.MissingChallenge
      | Some source_seed ->
          let challenge = Resource_attestation_flow.challenge source_seed in
          let window = Resource_attestation_admission.{
            current_epoch = t.config.local_head_epoch ();
            fraud_window = cfg.fraud_window;
            future_window = cfg.future_window;
          } in
          Resource_attestation_admission.ingest
            ~chain_id:t.config.chain_id
            ~challenge
            ~window
            ~pubkey_of_node:cfg.pubkey_of_node
            t.resource_admission
            attestation

let send_verified_proposal t route (p : C_types.propose) =
  match route with
  | Publish_verified_proposal ->
    Octra_net.P2p_swarm.broadcast
      t.swarm
      {
        msg_type = Frame.msg_cons_propose;
        payload = C_codec.encode_propose p;
      }
  | Relay_verified_proposal { source_peer; payload } ->
    Octra_net.P2p_swarm.broadcast_except
      t.swarm
      ~except:source_peer
      { msg_type = Frame.msg_cons_propose; payload }

let proposal_wait_delay attempt =
  let ms =
    match attempt with
    | 1 -> 0
    | 2 -> 500
    | 3 -> 1_000
    | 4 -> 2_000
    | _ -> 4_000
  in
  Int64.mul (Int64.of_int ms) 1_000_000L

let proposal_wait_id (p : C_types.propose) =
  C_hash.proposal_id p.header

let clear_proposal_wait t (p : C_types.propose) =
  let pid = proposal_wait_id p in
  match t.proposal_wait with
  | Some wait when wait.pid = pid -> t.proposal_wait <- None
  | Some _
  | None -> ()

let retain_proposal_wait t ~route (p : C_types.propose) =
  let gen = t.engine.generation in
  let pid = proposal_wait_id p in
  let prior = t.proposal_wait in
  let keep_prior =
    match prior with
    | Some wait when wait.gen = gen
                     && wait.proposal.epoch_id = p.epoch_id
                     && wait.pid <> pid ->
      wait.proposal.round <= p.round
    | Some _
    | None -> false
  in
  if keep_prior then ()
  else
    let attempt =
      match prior with
      | Some wait when wait.gen = gen && wait.pid = pid ->
        min 16 (wait.attempt + 1)
      | Some _
      | None -> 1
    in
    let retry_at =
      Int64.add
        (Mtime_clock.elapsed_ns ())
        (proposal_wait_delay attempt)
    in
    t.proposal_wait <- Some {
      gen;
      pid;
      proposal = p;
      route;
      attempt;
      retry_at;
    };
    log_node t.config.my_addr
      "event = proposal_wait epoch = %Ld round = %d attempt = %d"
      p.epoch_id
      p.round
      attempt

let admit_current_proposal t ~route (p : C_types.propose) =
  let open Lwt.Syntax in
  let signature_valid =
    match C_types.pubkey_of_addr t.engine.vs p.proposer with
    | None -> false
    | Some pubkey -> C_hash.verify_propose ~pubkey_raw:pubkey p
  in
  let envelope_valid =
    C_types.proposal_is_well_formed
      ~chain_id:t.config.chain_id
      ~validator_set:t.engine.vs
      p
    && proposal_tx_list_is_bound p
    && C_hash.parent_commit_hash_opt p.parent_commit
       = p.header.parent_commit_hash
  in
  if not signature_valid
     || not envelope_valid
     || not
          (proposal_verify_relevant
             ~current_round:t.engine.state.round
             ~current_step:t.engine.state.step
             ~proposal_round:p.round)
  then
    begin
      clear_proposal_wait t p;
      Lwt.return_unit
    end
  else
    let* preview =
      C_proposal_work_gate.run
        t.proposal_work_gate
        ~relevant:(fun () -> proposal_verify_current t p)
        (fun () ->
          let gen = t.engine.generation in
          let height = t.engine.state.height in
          let round = t.engine.state.round in
          let step = t.engine.state.step in
          mark_proposal_verify t ~gen ~height ~round ~step;
          let* verdict =
            Lwt.finalize
              (fun () ->
                Lwt.catch
                  (fun () -> t.config.verify_proposal p)
                  (fun exn ->
                    warn_node t.config.my_addr
                      "event = proposal_verify_wait epoch = %Ld round = %d reason = %s"
                      p.epoch_id
                      p.round
                      (Printexc.to_string exn);
                    Lwt.return Proposal_wait))
              (fun () ->
                clear_proposal_verify t ~gen ~height ~round ~step;
                Lwt.return_unit)
          in
          Lwt.return_some verdict)
    in
    match preview with
    | None ->
      Lwt.return_unit
    | Some _ when not (proposal_verify_current t p) ->
      clear_proposal_wait t p;
      Lwt.return_unit
    | Some Proposal_wait ->
      retain_proposal_wait t ~route p;
      Lwt.return_unit
    | Some verdict ->
      clear_proposal_wait t p;
      let accepted = verdict = Proposal_accept in
      let* () =
        if accepted then
          send_verified_proposal t route p
        else
          Lwt.return_unit
      in
      if accepted && p.round > t.engine.state.round then
        defer_verified_proposal t p;
      ignore
        (C_engine.on_propose
           t.engine
           p
           ~verify_fn:(verify_engine_signature t)
           ~execute_fn:(fun _ -> accepted)
           ~sign_fn:t.config.sign_fn);
      Lwt.return_unit

let replay_waiting_proposal t =
  match t.proposal_wait with
  | Some wait when not (proposal_verify_current t wait.proposal) ->
    t.proposal_wait <- None;
    Lwt.return_unit
  | Some wait when Int64.compare
                     (Mtime_clock.elapsed_ns ())
                     wait.retry_at >= 0 ->
    log_node t.config.my_addr
      "event = proposal_retry epoch = %Ld round = %d attempt = %d"
      wait.proposal.epoch_id
      wait.proposal.round
      wait.attempt;
    admit_current_proposal t ~route:wait.route wait.proposal
  | Some _
  | None -> Lwt.return_unit

let replay_pending_proposal t =
  let height = t.engine.state.height in
  let round = t.engine.state.round in
  Hashtbl.filter_map_inplace
    (fun _ (p : C_types.propose) ->
      if p.epoch_id < height then None else Some p)
    t.pending_proposals;
  let key = proposal_round_key height round in
  match Hashtbl.find_opt t.pending_proposals key with
  | None -> Lwt.return_unit
  | Some p ->
    Hashtbl.remove t.pending_proposals key;
    log_node t.config.my_addr
      "event = replay_pending_proposal epoch = %Ld round = %d"
      height
      round;
    admit_current_proposal t ~route:Publish_verified_proposal p

let vote_type_rank = function
  | C_types.Prevote -> 0
  | C_types.Precommit -> 1

let compare_future_vote (left : C_types.vote) (right : C_types.vote) =
  match
    Int.compare
      (vote_type_rank left.vote_type)
      (vote_type_rank right.vote_type)
  with
  | 0 ->
    (match Int.compare left.round right.round with
     | 0 -> String.compare left.validator right.validator
     | value -> value)
  | value -> value

let replay_future_votes t =
  let height = t.engine.state.height in
  let round = t.engine.state.round in
  let ready = ref [] in
  Hashtbl.filter_map_inplace
    (fun _ (vote : C_types.vote) ->
      if vote.epoch_id < height then
        None
      else if vote.epoch_id = height && vote.round = round then begin
        ready := vote :: !ready;
        None
      end else
        Some vote)
    t.future_votes;
  let evidence =
    List.sort compare_future_vote !ready
    |> List.filter_map (fun (vote : C_types.vote) ->
    match C_types.pubkey_of_addr t.engine.vs vote.validator with
    | Some pubkey when C_hash.verify_vote ~pubkey_raw:pubkey vote ->
      (match C_engine.conflicting_vote t.engine vote with
       | Some prior ->
         record_vote_conflict ~validator_set:t.engine.vs t prior vote
       | None ->
         C_engine.on_vote t.engine vote ~sign_fn:t.config.sign_fn;
         None)
    | _ -> None)
  in
  (match !ready with
   | [] -> ()
   | _ ->
     log_node t.config.my_addr
       "event = replay_future_votes epoch = %Ld round = %d count = %d"
       height
       round
       (List.length !ready));
  evidence

let fold_event t (finalize : C_types.finalize) =
  let signed =
    List.exists
      (fun (vote : C_types.vote) ->
        String.equal vote.validator t.config.my_addr)
      finalize.precommits
  in
  if signed then None
  else
    let votes =
      Hashtbl.to_seq_values t.durable_votes
      |> List.of_seq
      |> List.filter (fun (vote : C_types.vote) ->
        vote.vote_type = C_types.Precommit
        && String.equal vote.validator t.config.my_addr
        && Int64.equal vote.epoch_id finalize.epoch_id
        && vote.round = finalize.commit_round
        && String.equal vote.proposal_id finalize.proposal_id)
      |> List.sort (fun left right ->
        String.compare (vote_key left) (vote_key right))
    in
    match votes with
    | [] -> None
    | vote :: _ ->
      Some
        (vote,
         C_types.{
           certificate = C_types.certificate_of_finalize finalize;
           validator_set = t.engine.vs;
         })

let notify_fold t ~next_epoch event =
  try t.on_fold ~next_epoch event
  with exn ->
    warn_node t.config.my_addr
      "event = set_fold_notice_failed epoch = %Ld reason = %s"
      next_epoch
      (Printexc.to_string exn)

let rec process_outputs_once t =
  let open Lwt.Syntax in
  C_engine.on_ready t.engine ~sign_fn:t.config.sign_fn;
  let* () = replay_waiting_proposal t in
  let* () = replay_pending_proposal t in
  let replayed_evidence = replay_future_votes t in
  let* () =
    Lwt_list.iter_s
      (fun evidence ->
        Octra_net.P2p_swarm.broadcast t.swarm
          (vote_evidence_frame evidence))
      replayed_evidence
  in
  replay_deferred_proposal t;
  let current_height = t.engine.state.height in
  let validator_set_hash = C_config.validator_set_hash t.engine.vs in
  Hashtbl.filter_map_inplace (fun epoch pending ->
    if Int64.compare epoch t.engine.C_engine.finalized_height <= 0 then None
    else if pending.validator_set_hash <> validator_set_hash then begin
      warn_node t.config.my_addr
        "event = drop_pending_finalize epoch = %Ld reason = validator_set_changed"
        epoch;
      None
    end
    else if Int64.compare epoch current_height = 0 then begin
      let accepted =
        C_engine.accept_finalize_batch t.engine pending.finalize
      in
      log_node t.config.my_addr
        "event = pending_finalize_current epoch = %Ld accepted = %b"
        epoch accepted;
      None
    end else
      Some pending
  ) t.pending_finalizes;
  let outputs = C_engine.drain_outputs t.engine in
  let has_finalized =
    List.exists (function
      | C_engine.Finalized _ -> true
      | _ -> false
    ) outputs
  in
  List.iter (fun o ->
    let name = match o with
      | C_engine.SendPropose _ -> "SendPropose"
      | C_engine.SendVote _ -> "SendVote"
      | C_engine.SendFinalize _ -> "SendFinalize"
      | C_engine.RequestProposal _ -> "RequestProposal"
      | C_engine.RequestRoundEvidence _ -> "RequestRoundEvidence"
      | C_engine.ScheduleTimeout _ -> "ScheduleTimeout"
      | C_engine.Finalized _ -> "Finalized" in
    trace_node t.config.my_addr "event = engine_output output = %s" name
  ) outputs;
  let* () = flush_pending_votes t in
  let* () =
    Lwt_list.iter_s
      (fun output ->
        match output with
            | C_engine.SendPropose p ->
              trace_node t.config.my_addr
                "event = send_propose epoch = %Ld round = %d"
                p.epoch_id
                p.round;
              let payload = C_codec.encode_propose p in
              Octra_net.P2p_swarm.broadcast t.swarm
                { msg_type = Frame.msg_cons_propose; payload }
            | C_engine.SendFinalize f ->
              trace_node t.config.my_addr
                "event = send_finalize epoch = %Ld round = %d pid = %s creator = %s precommits = %d"
                f.epoch_id
                f.commit_round
                (let h =
                   Digestif.SHA256.to_hex
                     (Digestif.SHA256.of_raw_string f.proposal_id)
                 in
                 if String.length h >= 16 then String.sub h 0 16 else h)
                (String.sub
                  f.header.creator_addr
                  0
                  (min 14 (String.length f.header.creator_addr)))
                (List.length f.precommits);
              let payload = C_codec.encode_finalize f in
              Octra_net.P2p_swarm.broadcast t.swarm
                { msg_type = Frame.msg_cons_finalize; payload }
            | C_engine.RequestProposal { round; proposal_id } ->
              start_proposal_fetch t ~round ~proposal_id
            | C_engine.RequestRoundEvidence round ->
              broadcast_round_sync_at t ~round
            | C_engine.ScheduleTimeout { step; round; delay_ms; generation } ->
              Lwt.async (fun () ->
                let* () =
                  Lwt_unix.sleep (float_of_int delay_ms /. 1000.0)
                in
                let rec fire () =
                  if not t.running then Lwt.return_unit
                  else if
                    step = C_types.ProposeStep
                    && proposal_build_active t ~generation ~round ~step
                    && proposal_build_grace_left t ~generation ~round ~step
                  then begin
                    log_node t.config.my_addr
                      "event = defer_propose_timeout reason = proposal_build_active height = %Ld round = %d"
                      t.engine.state.height
                      round;
                    let* () = Lwt_unix.sleep 0.5 in
                    fire ()
                  end else if
                    step = C_types.ProposeStep
                    && proposal_verify_grace_left t ~generation ~round ~step
                  then begin
                    log_node t.config.my_addr
                      "event = defer_propose_timeout reason = proposal_verify_active height = %Ld round = %d"
                      t.engine.state.height
                      round;
                    let* () = Lwt_unix.sleep 0.5 in
                    fire ()
                  end else begin
                    C_engine.on_timeout
                      t.engine
                      ~step
                      ~round
                      ~generation
                      ~sign_fn:t.config.sign_fn;
                    let* _ = try_current_leader_proposal t in
                    process_outputs t
                  end
                in
                fire ());
              let* () =
                broadcast_round_sync
                  t
                  ~request:(round_sync_request_for_step step)
              in
              if step = C_types.ProposeStep then ask_past t else Lwt.return_unit
            | C_engine.SendVote v ->
              if not (vote_still_relevant t v) then begin
                log_node t.config.my_addr
                  "event = drop_stale_vote_output type = %s epoch = %Ld round = %d height = %Ld"
                  (vote_step_label v.vote_type)
                  v.epoch_id
                  v.round
                  t.engine.state.height;
                Lwt.return_unit
              end else if not (vote_allowed t) then begin
                let* stored = saved_vote t v in
                match stored with
                | None ->
                  error_node t.config.my_addr
                    "event = refuse_vote_queue type = %s epoch = %Ld round = %d"
                    (vote_step_label v.vote_type)
                    v.epoch_id
                    v.round;
                  Lwt.return_unit
                | Some vote ->
                  queue_vote t vote;
                  log_node t.config.my_addr
                    "event = queue_vote_not_ready type = %s epoch = %Ld round = %d pending = %d"
                    (vote_step_label vote.vote_type)
                    vote.epoch_id
                    vote.round
                    (pending_vote_count t);
                  Lwt.return_unit
              end else
                let* _ = broadcast_vote t v in
                Lwt.return_unit
            | C_engine.Finalized { epoch_id; finalize } ->
              let header = finalize.header in
              let round = finalize.commit_round in
              let fold = fold_event t finalize in
              log_node t.config.my_addr
                "event = finalized epoch = %Ld round = %d creator = %s root = %s"
                epoch_id
                round
                (String.sub
                  header.creator_addr
                  0
                  (min 14 (String.length header.creator_addr)))
                (let h =
                   Digestif.SHA256.to_hex
                     (Digestif.SHA256.of_raw_string
                       header.proposed_state_root)
                 in
                 if String.length h >= 16 then String.sub h 0 16 else h);
              let* () =
                t.config.on_finalized
                  ~validator_set:t.engine.vs
                  finalize
              in
              let vote_log_ready =
                match C_vote_log.set_floor t.vote_log ~through_epoch:epoch_id with
                | Error reason ->
                  hold_vote_log t reason;
                  false
                | Ok () ->
                  (match
                     C_vote_log.prune t.vote_log ~through_epoch:epoch_id
                   with
                   | Ok () -> true
                   | Error reason ->
                     hold_vote_log t reason;
                     false)
              in
              (match C_sync_log.prune t.sync_log ~through_epoch:epoch_id with
               | Ok () -> ()
               | Error reason ->
                 warn_node t.config.my_addr
                   "event = round_sync_prune_failed epoch = %Ld reason = %s"
                   epoch_id
                   reason);
              let* () =
                maybe_activate_scheduled_validator_set
                  t
                  ~target_epoch:(Int64.add epoch_id 1L)
              in
              let* () =
                maybe_activate_resource_committee
                  t
                  ~target_epoch:(Int64.add epoch_id 1L)
              in
              let next = Int64.add epoch_id 1L in
              notify_fold t ~next_epoch:next fold;
              Hashtbl.clear t.durable_votes;
              Hashtbl.clear t.round_peers;
              t.vote_fault <- None;
              if vote_log_ready then t.vote_log_issue <- None;
              C_engine.start_height t.engine next;
              resume_local_vote t;
              let now_ns = Mtime_clock.elapsed_ns () in
              let slot_ns =
                Int64.of_float (Epoch_time.interval_seconds *. 1e9)
              in
              let target_ns = Int64.add t.epoch_start_mono slot_ns in
              let monotonic_remaining_ns = Int64.sub target_ns now_ns in
              let epoch_time_remaining_ns =
                Int64.mul
                  (Epoch_time.next_delay_ms
                     ~now:(Unix.gettimeofday ())
                     ~previous:header.ts)
                  1_000_000L
              in
              let remaining_ns =
                if epoch_time_remaining_ns > monotonic_remaining_ns then
                  epoch_time_remaining_ns
                else
                  monotonic_remaining_ns
              in
              let carry_ms =
                if now_ns > target_ns then
                  Int64.div
                    (Int64.sub now_ns target_ns)
                    1_000_000L
                else
                  0L
              in
              let wait_ms =
                if remaining_ns > 0L then
                  Int64.div remaining_ns 1_000_000L
                else
                  0L
              in
              log_node t.config.my_addr
                "event = epoch_pacer height = %Ld carry_ms = %Ld wait_ms = %Ld"
                next
                carry_ms
                wait_ms;
              let* () =
                if remaining_ns > 100_000_000L then
                  Lwt_unix.sleep (Int64.to_float remaining_ns /. 1e9)
                else Lwt.return_unit
              in
              t.epoch_start_mono <- Mtime_clock.elapsed_ns ();
              let* _ = try_current_leader_proposal t in
              let* () = process_outputs t in
              Lwt.return_unit)
      outputs
  in
  if has_finalized then Lwt.return_unit
  else
    let* proposed = try_current_leader_proposal t in
    if proposed then process_outputs t else Lwt.return_unit
and process_outputs t =
  let open Lwt.Syntax in
  let actor, request = C_output_actor.request t.output_actor in
  t.output_actor <- actor;
  match request with
  | C_output_actor.Join -> Lwt.return_unit
  | C_output_actor.Start ->
    let step () =
      let* () = process_outputs_once t in
      let actor, completion = C_output_actor.complete t.output_actor in
      t.output_actor <- actor;
      Lwt.return completion
    in
    let fail _ =
      t.output_actor <- C_output_actor.fail t.output_actor
    in
    C_output_actor.drain ~step ~fail

let finality_proof_target t (finalize : C_types.finalize) =
  !(t.finality_proof_needed)
  && Int64.equal finalize.epoch_id (t.config.local_head_epoch ())
  && Int64.equal t.engine.state.height (Int64.succ finalize.epoch_id)

let accept_finality_proof t finalize =
  if not (finality_proof_target t finalize) then
    Lwt.return_unit
  else
    Lwt.catch
      (fun () ->
        match
          t.check_finality_proof t.engine.vs finalize
        with
        | Error reason ->
          warn_node t.config.my_addr
            "event = finality_proof status = refused reason = local_binding detail = %s"
            reason;
          Lwt.return_unit
        | Ok () ->
          let verify_vote (vote : C_types.vote) =
            match C_types.pubkey_of_addr t.engine.vs vote.validator with
            | Some pubkey -> C_hash.verify_vote ~pubkey_raw:pubkey vote
            | None -> false
          in
          begin
            match
              C_qc.validate_finalize
                ~chain_id:t.config.chain_id
                ~validator_set:t.engine.vs
                ~verify_vote
                finalize
            with
            | C_qc.Invalid reason ->
              warn_node t.config.my_addr
                "event = finality_proof status = refused reason = qc_%s"
                reason;
              Lwt.return_unit
            | C_qc.Valid ->
              let open Lwt.Syntax in
              let* repaired = t.on_finality_proof t.engine.vs finalize in
              if not repaired then
                Lwt.return_unit
              else begin
                t.finality_proof_needed := false;
                clear_finality_proof_requests t;
                log_node t.config.my_addr
                  "event = finality_proof status = repaired epoch = %Ld"
                  finalize.epoch_id;
                let* () = broadcast_round_sync t ~request:true in
                let* _ = try_current_leader_proposal t in
                process_outputs t
              end
          end)
      (fun exn ->
        warn_node t.config.my_addr
          "event = finality_proof status = refused reason = local_error detail = %s"
          (Printexc.to_string exn);
        Lwt.return_unit)

let rec on_p2p_message t _conn (frame : Frame.frame) =
  let open Lwt.Syntax in
  let skip_dedup = repeatable_query_frame frame in
  let allowed = frame_allowed ~running:t.running frame.msg_type in
  let verify_before_dedup =
    engine_output_frame frame.msg_type
    || frame.msg_type = Frame.msg_vote_evidence
  in
  let repeated =
    allowed
    && not skip_dedup
    && if verify_before_dedup then
         frame_known t frame.msg_type frame.payload
       else
         is_seen t frame.msg_type frame.payload
  in
  if not allowed then begin
    trace_node t.config.my_addr
      "event = defer_consensus_frame reason = driver_not_started type = %d"
      frame.msg_type;
    Lwt.return_unit
  end
  else if repeated
          && not
               (historical_replay_needed
                  t
                  frame.msg_type
                  frame.payload) then
    Lwt.return_unit
  else begin
    (match frame.msg_type with
    | t' when t' = Frame.msg_cons_round_fetch ->
      Lwt.catch
        (fun () ->
          let fetch = C_codec.decode_round_fetch frame.payload in
          if not t.running
             || not (local_validator t)
             || not (round_fetch_source_matches t _conn)
             || not (round_fetch_valid t fetch)
             || not (round_fetch_response_allowed t _conn)
          then Lwt.return_unit
          else send_round_fetch_response t _conn fetch)
        (fun exn ->
          warn_node t.config.my_addr
            "event = reject_round_fetch reason = invalid_frame detail = %s"
            (Printexc.to_string exn);
          Octra_net.P2p_swarm.report_bad_peer
            t.swarm
            _conn
            ~reason:"invalid_frame_round_fetch";
          Lwt.return_unit)
    | t' when t' = Frame.msg_cons_round_sync ->
      Lwt.catch
        (fun () ->
          let sync = C_codec.decode_round_sync frame.payload in
          if sync.chain_id <> t.config.chain_id then begin
            remember_frame t frame.msg_type frame.payload;
            Lwt.return_unit
          end else
            match
              proposal_height_status
                ~current:t.engine.state.height
                ~proposal:sync.epoch_id
            with
            | Proposal_stale ->
              remember_frame t frame.msg_type frame.payload;
              Lwt.return_unit
            | Proposal_future ->
              Lwt.return_unit
            | Proposal_current ->
              match C_types.pubkey_of_addr t.engine.vs sync.validator with
              | None ->
                remember_frame t frame.msg_type frame.payload;
                warn_node t.config.my_addr
                  "event = reject_round_sync reason = unknown_validator from = %s"
                  (String.sub sync.validator 0
                    (min 12 (String.length sync.validator)));
                Lwt.return_unit
              | Some pubkey
                when not (C_hash.verify_round_sync ~pubkey_raw:pubkey sync) ->
                remember_frame t frame.msg_type frame.payload;
                warn_node t.config.my_addr
                  "event = reject_round_sync reason = bad_signature from = %s"
                  (String.sub sync.validator 0
                    (min 12 (String.length sync.validator)));
                Octra_net.P2p_swarm.report_bad_peer
                  t.swarm
                  _conn
                  ~reason:"bad_signature_round_sync";
                Lwt.return_unit
              | Some _ ->
                remember_frame t frame.msg_type frame.payload;
                let current_round = t.engine.state.round in
                remember_round_peer t sync;
                let relay_candidate =
                  round_sync_relay_relevant
                    ~current_height:t.engine.state.height
                    ~current_round
                    sync
                in
                if not (round_sync_allowed ~current_round sync) then
                  Lwt.return_unit
                else begin
                  C_round_pool.add_at
                    t.round_pool
                    ~current:current_round
                    sync;
                  let* () =
                    if relay_candidate then
                      Octra_net.P2p_swarm.broadcast_except
                        t.swarm
                        ~except:_conn.Octra_net.P2p_conn.peer_id
                        { msg_type = Frame.msg_cons_round_sync;
                          payload = frame.payload }
                    else
                      Lwt.return_unit
                  in
                  C_engine.on_round_sync
                    t.engine
                    ~round:sync.round
                    ~validator:sync.validator;
                  let* () = process_outputs t in
                  if round_sync_reply_needed t sync
                     && round_sync_response_allowed t sync then
                    send_round_sync_response
                      t
                      _conn
                      ~requested_round:sync.round
                      ~with_witness:(round_sync_source_matches t _conn sync)
                  else
                    Lwt.return_unit
                end)
        (fun exn ->
          remember_frame t frame.msg_type frame.payload;
          warn_node t.config.my_addr
            "event = bad_round_sync error = %s"
            (Printexc.to_string exn);
          Octra_net.P2p_swarm.report_bad_peer
            t.swarm
            _conn
            ~reason:"invalid_frame_round_sync";
          Lwt.return_unit)
    | t' when t' = Frame.msg_cons_propose ->
      Lwt.catch (fun () ->
        let p = C_codec.decode_propose frame.payload in
        if p.chain_id <> t.config.chain_id then begin
          remember_frame t frame.msg_type frame.payload;
          warn_node t.config.my_addr
            "event = reject_propose reason = chain_id_mismatch got = %s ours = %s"
            p.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        match proposal_local_status
                ~engine_head:t.engine.state.height
                ~local_head:(t.config.local_head_epoch ())
                ~proposal:p.epoch_id with
        | Proposal_stale ->
          remember_frame t frame.msg_type frame.payload;
          Lwt.return_unit
        | Proposal_future ->
          let* validator_set = validator_set_for_frame t p.epoch_id in
          (match
             validate_proposal_frame
               ~chain_id:t.config.chain_id
               ~validator_set
               p
           with
           | Error error ->
             log_node t.config.my_addr
               "event = ignore_future_propose reason = unresolved_validator_set predicate = %s epoch = %Ld local_height = %Ld"
               (proposal_frame_error_label error)
               p.epoch_id
               t.engine.state.height;
             (match error with
              | Proposal_unknown_validator -> ()
              | _ ->
                remember_frame t frame.msg_type frame.payload;
                report_proposal_error
                  t
                  _conn
                  validator_set
                  p.proposer
                  error);
             Lwt.return_unit
           | Ok () ->
             let deferred = defer_pending_proposal t p in
             if deferred then begin
               remember_frame t frame.msg_type frame.payload;
               log_node t.config.my_addr
                 "event = defer_pending_proposal epoch = %Ld round = %d local_height = %Ld"
                 p.epoch_id
                 p.round
                 t.engine.state.height
             end;
             if deferred then
               Octra_net.P2p_swarm.broadcast_except
                 t.swarm
                 ~except:_conn.Octra_net.P2p_conn.peer_id
                 { msg_type = Frame.msg_cons_propose; payload = frame.payload }
             else
               Lwt.return_unit)
        | Proposal_current ->
          let* validator_set = validator_set_for_frame t p.epoch_id in
          (match
             validate_proposal_frame
               ~chain_id:t.config.chain_id
               ~validator_set
               p
           with
           | Error error ->
             warn_node t.config.my_addr
               "event = reject_propose reason = %s epoch = %Ld round = %d from = %s"
               (proposal_frame_error_label error)
               p.epoch_id
               p.round
               (String.sub p.proposer 0
                  (min 12 (String.length p.proposer)));
             (match error with
              | Proposal_unknown_validator ->
                remember_frame t frame.msg_type frame.payload
              | _ ->
                remember_frame t frame.msg_type frame.payload;
                report_proposal_error
                  t
                  _conn
                  validator_set
                  p.proposer
                  error);
             Lwt.return_unit
           | Ok () ->
             remember_frame t frame.msg_type frame.payload;
             let route =
               Relay_verified_proposal {
                 source_peer = _conn.Octra_net.P2p_conn.peer_id;
                 payload = frame.payload;
               }
             in
             let* () = admit_current_proposal t ~route p in
             process_outputs t)
        )
      (fun exn ->
        remember_frame t frame.msg_type frame.payload;
        warn_node t.config.my_addr "event = bad_propose error = %s"
          (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"invalid_frame_propose";
        Lwt.return_unit)
    | t' when t' = Frame.msg_cons_vote ->
      Lwt.catch (fun () ->
        let v = C_codec.decode_vote frame.payload in
        if v.chain_id <> t.config.chain_id then begin
          remember_frame t frame.msg_type frame.payload;
          warn_node t.config.my_addr
            "event = reject_vote reason = chain_id_mismatch got = %s ours = %s"
            v.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        match proposal_local_status
                ~engine_head:t.engine.state.height
                ~local_head:(t.config.local_head_epoch ())
                ~proposal:v.epoch_id with
        | Proposal_stale ->
          remember_frame t frame.msg_type frame.payload;
          Lwt.return_unit
        | status ->
          let* validator_set = validator_set_for_frame t v.epoch_id in
          let validation =
            match C_types.pubkey_of_addr validator_set v.validator with
            | None -> `Unknown_validator
            | Some pubkey when C_hash.verify_vote ~pubkey_raw:pubkey v ->
              `Valid
            | Some _ -> `Bad_signature
          in
          if validation = `Unknown_validator then begin
            (match status with
             | Proposal_current ->
               remember_frame t frame.msg_type frame.payload
             | Proposal_future
             | Proposal_stale -> ());
            log_node t.config.my_addr
              "event = ignore_vote reason = unresolved_validator_set epoch = %Ld local_height = %Ld validator = %s"
              v.epoch_id
              t.engine.state.height
              v.validator;
            Lwt.return_unit
          end else if validation = `Bad_signature then begin
            remember_frame t frame.msg_type frame.payload;
            warn_node t.config.my_addr
              "event = reject_vote reason = bad_signature epoch = %Ld validator = %s"
              v.epoch_id
              v.validator;
            Octra_net.P2p_swarm.report_bad_peer
              t.swarm
              _conn
              ~reason:"bad_signature_vote";
            Lwt.return_unit
          end else begin
            remember_frame t frame.msg_type frame.payload;
            let future =
              match status with
              | Proposal_future -> defer_future_vote t v
              | Proposal_current
              | Proposal_stale -> Future_vote_not_applicable
            in
            let relay_candidate =
              match status, future with
              | Proposal_current, _ ->
                vote_relay_relevant
                  ~current_height:t.engine.state.height
                  ~current_round:t.engine.state.round
                  v
              | Proposal_future, Future_vote_deferred ->
                future_vote_relay_relevant
                  ~current_height:t.engine.state.height
                  v
              | Proposal_future, _
              | Proposal_stale, _ -> false
            in
            (match future with
             | Future_vote_deferred ->
               log_node t.config.my_addr
                 "event = defer_future_vote type = %s epoch = %Ld round = %d validator = %s local_height = %Ld"
                 (vote_step_label v.vote_type)
                 v.epoch_id
                 v.round
                 v.validator
                 t.engine.state.height
             | _ -> ());
            let evidence =
              match status, future with
              | Proposal_future, Future_vote_conflict prior ->
                record_vote_conflict ~validator_set t prior v
              | Proposal_future, _ ->
                None
              | Proposal_current, _ ->
                (match C_engine.conflicting_vote t.engine v with
                 | Some prior ->
                   record_vote_conflict ~validator_set t prior v
                 | None ->
                   C_engine.on_vote t.engine v ~sign_fn:t.config.sign_fn;
                   None)
              | Proposal_stale, _ ->
                None
            in
            let* () =
              match evidence with
              | None -> Lwt.return_unit
              | Some value ->
                Octra_net.P2p_swarm.broadcast
                  t.swarm
                  (vote_evidence_frame value)
            in
            let* () =
              if relay_candidate && Option.is_none evidence then
                Octra_net.P2p_swarm.broadcast_except
                  t.swarm
                  ~except:_conn.Octra_net.P2p_conn.peer_id
                  { msg_type = Frame.msg_cons_vote; payload = frame.payload }
              else
                Lwt.return_unit
            in
            Lwt.return_unit
          end)
      (fun exn ->
        remember_frame t frame.msg_type frame.payload;
        warn_node t.config.my_addr "event = bad_vote error = %s"
          (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"invalid_frame_vote";
        Lwt.return_unit)
    | t' when t' = Frame.msg_cons_finalize ->
      Lwt.catch (fun () ->
        let f = C_codec.decode_finalize frame.payload in
        if f.chain_id <> t.config.chain_id then begin
          remember_frame t frame.msg_type frame.payload;
          warn_node t.config.my_addr
            "event = reject_finalize reason = chain_id_mismatch got = %s ours = %s"
            f.chain_id t.config.chain_id;
          Lwt.return_unit
        end else
        match proposal_local_status
                ~engine_head:t.engine.state.height
                ~local_head:(t.config.local_head_epoch ())
                ~proposal:f.epoch_id with
        | Proposal_stale ->
          remember_frame t frame.msg_type frame.payload;
          accept_finality_proof t f
        | Proposal_future ->
          log_node t.config.my_addr
            "event = ignore_future_finalize reason = unresolved_validator_set epoch = %Ld local_height = %Ld"
            f.epoch_id
            t.engine.state.height;
          Lwt.return_unit
        | Proposal_current ->
          match
            t.config.verify_parent_commit
              ~epoch_id:f.epoch_id
              f.parent_commit
          with
          | Error reason ->
            remember_frame t frame.msg_type frame.payload;
            warn_node t.config.my_addr
              "event = reject_finalize reason = parent_commit detail = %s"
              reason;
            Lwt.return_unit
          | Ok () ->
          let verify_vote (vote : C_types.vote) =
            match C_types.pubkey_of_addr t.engine.vs vote.C_types.validator with
            | Some pk -> C_hash.verify_vote ~pubkey_raw:pk vote
            | None -> false
          in
          match C_qc.validate_finalize
            ~chain_id:t.config.chain_id
            ~validator_set:t.engine.vs
            ~verify_vote
            f
          with
          | C_qc.Invalid reason ->
            remember_frame t frame.msg_type frame.payload;
            warn_node t.config.my_addr
              "event = reject_finalize reason = qc_%s" reason;
            let peer_reason =
              if reason = "signature" then "bad_signature_finalize"
              else "invalid_frame_finalize_qc_" ^ reason
            in
            Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:peer_reason;
            Lwt.return_unit
          | C_qc.Valid ->
            remember_frame t frame.msg_type frame.payload;
            let expected_pid = C_hash.proposal_id f.header in
          trace_node t.config.my_addr
            "event = recv_finalize epoch = %Ld commit_round = %d pid = %s creator = %s precommits = %d"
            f.epoch_id f.commit_round
            (let h = Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string f.proposal_id) in
             if String.length h >= 16 then String.sub h 0 16 else h)
            (String.sub f.header.creator_addr 0 (min 14 (String.length f.header.creator_addr)))
            (List.length f.precommits);
              let accepted = C_engine.accept_finalize_batch t.engine f in
              if accepted then begin
                trace_node t.config.my_addr
                  "event = finalize_batch epoch = %Ld round = %d valid_precommits = %d total_precommits = %d"
                  f.epoch_id f.commit_round
                  (List.length f.precommits)
                  (List.length f.precommits);
                let* () = Octra_net.P2p_swarm.broadcast t.swarm
                  { msg_type = Frame.msg_cons_finalize; payload = frame.payload } in
                process_outputs t
              end else begin
                warn_node t.config.my_addr
                  "event = reject_finalize reason = engine_state epoch = %Ld round = %d pid = %s"
                  f.epoch_id f.commit_round
                  (String.sub expected_pid 0 (min 16 (String.length expected_pid)));
                Lwt.return_unit
              end
        )
      (fun exn ->
        remember_frame t frame.msg_type frame.payload;
        warn_node t.config.my_addr "event = bad_finalize error = %s"
          (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"invalid_frame_finalize";
        Lwt.return_unit)
    | t' when t' = Frame.msg_query_epoch_root ->
      Lwt.catch (fun () ->
        let q = C_codec.decode_epoch_root_query frame.payload in
        if q.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (local_validator t) then Lwt.return_unit
        else begin
          let state_root = t.config.lookup_epoch_root q.epoch_id in
          let responder_addr = t.config.my_addr in
          let responder_head_epoch = t.config.local_head_epoch () in
          let sign_bytes = C_hash.epoch_root_response_sign_bytes
            ~chain_id:t.config.chain_id ~epoch_id:q.epoch_id
            ~state_root ~responder_addr ~responder_head_epoch in
          let signature = t.config.sign_fn sign_bytes in
          let response = C_codec.{
            chain_id = t.config.chain_id;
            epoch_id = q.epoch_id;
            state_root;
            responder_addr;
            responder_head_epoch;
            signature;
          } in
          let payload = C_codec.encode_epoch_root_response response in
          Octra_net.P2p_conn.send _conn
            { msg_type = Frame.msg_epoch_root_response; payload }
        end)
        (fun exn ->
          log_node t.config.my_addr "event = bad_epoch_root_query error = %s"
            (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_epoch_root_response ->
      Lwt.catch (fun () ->
        let r = C_codec.decode_epoch_root_response frame.payload in
        if r.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (Hashtbl.mem t.epoch_root_responses r.epoch_id) then
          Lwt.return_unit
        else if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
          log_node t.config.my_addr
            "event = ignore_epoch_root_response reason = non_validator from = %s"
            (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else begin
          let sign_bytes = C_hash.epoch_root_response_sign_bytes
            ~chain_id:r.chain_id ~epoch_id:r.epoch_id
            ~state_root:r.state_root ~responder_addr:r.responder_addr
            ~responder_head_epoch:r.responder_head_epoch in
          if not (verify_engine_signature t r.responder_addr sign_bytes r.signature) then begin
            log_node t.config.my_addr
              "event = ignore_epoch_root_response reason = bad_signature from = %s"
              (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
            Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"bad_signature_epoch_root";
            Lwt.return_unit
          end else begin
            remember_peer_state t
              ~source:"epoch_root"
              ~responder_addr:r.responder_addr
              ~head_epoch:r.responder_head_epoch
              ~checked_epoch:r.epoch_id
              ~state_root:r.state_root;
            let record = {
              responder_addr = r.responder_addr;
              responder_head_epoch = r.responder_head_epoch;
              state_root = r.state_root;
            } in
            let prior = try Hashtbl.find t.epoch_root_responses r.epoch_id with Not_found -> [] in
            let already_seen = List.exists (fun (rec_ : epoch_root_response_record) ->
              rec_.responder_addr = r.responder_addr) prior in
            if not already_seen then
              Hashtbl.replace t.epoch_root_responses r.epoch_id (record :: prior);
            Lwt.return_unit
          end
        end)
        (fun exn ->
          log_node t.config.my_addr "event = bad_epoch_root_response error = %s"
            (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_query_bundle ->
      Lwt.catch (fun () ->
        let q = C_codec.decode_bundle_query frame.payload in
        if q.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (local_validator t) then Lwt.return_unit
        else begin
          match t.config.lookup_bundle q.proposal_id with
          | None -> Lwt.return_unit
          | Some (tx_hashes, txs_json, receipts_json) ->
            let r = C_codec.{
              chain_id = t.config.chain_id;
              epoch_id = q.epoch_id;
              proposal_id = q.proposal_id;
              responder_addr = t.config.my_addr;
              tx_hashes;
              txs_json;
              receipts_json;
              signature = String.make 64 '\x00';
            } in
            let r = {
              r with
              signature = t.config.sign_fn (C_hash.bundle_response_sign_bytes r);
            } in
            let payload = C_codec.encode_bundle_response r in
            Octra_net.P2p_conn.send _conn
              { msg_type = Frame.msg_bundle_response; payload }
        end)
        (fun exn ->
          log_node t.config.my_addr "event = bad_bundle_query error = %s"
            (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_bundle_response ->
      Lwt.catch (fun () ->
        let r = C_codec.decode_bundle_response frame.payload in
        if r.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (Hashtbl.mem t.bundle_responses r.proposal_id) then
          Lwt.return_unit
        else if not (C_types.is_validator t.engine.vs r.responder_addr) then begin
          log_node t.config.my_addr
            "event = ignore_bundle_response reason = non_validator from = %s"
            (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else if not (verify_engine_signature t r.responder_addr
          (C_hash.bundle_response_sign_bytes r) r.signature) then begin
          log_node t.config.my_addr
            "event = ignore_bundle_response reason = bad_signature from = %s"
            (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
            ~reason:"bad_signature_bundle";
          Lwt.return_unit
        end else if List.length r.tx_hashes <> List.length r.txs_json then begin
          log_node t.config.my_addr
            "event = ignore_bundle_response reason = mismatched_lengths from = %s"
            (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
          Lwt.return_unit
        end else begin
          let record = {
            responder_addr = r.responder_addr;
            tx_hashes = r.tx_hashes;
            txs_json = r.txs_json;
            receipts_json = r.receipts_json;
          } in
          let prior = try Hashtbl.find t.bundle_responses r.proposal_id with Not_found -> [] in
          let already_seen = List.exists (fun (rec_ : bundle_response_record) ->
            rec_.responder_addr = r.responder_addr) prior in
          if not already_seen then
            Hashtbl.replace t.bundle_responses r.proposal_id (record :: prior);
          Lwt.return_unit
        end)
        (fun exn ->
          log_node t.config.my_addr "event = bad_bundle_response error = %s"
            (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_query_catchup_range
              || t' = Frame.msg_query_catchup_range_v2 ->
      let is_v2 = (t' = Frame.msg_query_catchup_range_v2) in
      Lwt.catch (fun () ->
        let q = C_codec.decode_catchup_query_range frame.payload in
        if q.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (local_validator t) then Lwt.return_unit
        else begin
          let outcome = t.config.lookup_catchup_range
            ~from_epoch:q.from_epoch ~max_epochs:q.max_epochs in
          let make_response status error_code records next_epoch =
            let unsigned = C_codec.{
              chain_id = t.config.chain_id;
              request_id = q.request_id;
              status;
              error_code;
              records;
              next_epoch;
              responder_addr = t.config.my_addr;
              signature = "";
            }
            in
            let sign_bytes =
              if is_v2
                 && C_hash.catchup_request_is_complete q.request_id then
                C_hash.catchup_range_response_complete_sign_bytes
                  ~from_epoch:q.from_epoch
                  unsigned
              else
                let records_root =
                  if is_v2 then C_hash.catchup_records_root records
                  else C_hash.catchup_records_root_v1_wire records
                in
                C_hash.catchup_range_response_sign_bytes
                  ~chain_id:t.config.chain_id
                  ~request_id:q.request_id
                  ~responder_addr:t.config.my_addr
                  ~from_epoch:q.from_epoch
                  ~records_root
            in
            { unsigned with signature = t.config.sign_fn sign_bytes }
          in
          let response = match outcome with
            | `Ok (records, next_epoch) -> make_response "ok" None records next_epoch
            | `NotFound -> make_response "not_found" (Some "no_data") [] None
            | `Internal err -> make_response "internal_error" (Some err) [] None
          in
          let (payload, response_msg_type) =
            if is_v2 then
              (C_codec.encode_catchup_range_response_v2 response,
               Frame.msg_catchup_range_response_v2)
            else
              (C_codec.encode_catchup_range_response_v1 response,
               Frame.msg_catchup_range_response)
          in
          Octra_net.P2p_conn.send _conn
            { msg_type = response_msg_type; payload }
        end)
        (fun exn ->
          log_node t.config.my_addr
            "event = bad_catchup_query_range v2 = %b error = %s"
            is_v2 (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_catchup_range_response
              || t' = Frame.msg_catchup_range_response_v2 ->
      let is_v2 = (t' = Frame.msg_catchup_range_response_v2) in
      Lwt.catch (fun () ->
        let r =
          if is_v2 then C_codec.decode_catchup_range_response_v2 frame.payload
          else C_codec.decode_catchup_range_response_v1 frame.payload
        in
        if r.chain_id <> t.config.chain_id then Lwt.return_unit
        else if not (Hashtbl.mem t.catchup_query_windows r.request_id) then
          Lwt.return_unit
        else begin
          let window_opt =
            Hashtbl.find_opt t.catchup_query_windows r.request_id
          in
          match window_opt with
          | None ->
            log_node t.config.my_addr
              "event = ignore_catchup_range_response v2 = %b reason = unknown_request_empty_records"
              is_v2;
            Lwt.return_unit
          | Some window when not (catchup_response_in_window window r.records) ->
            log_node t.config.my_addr
              "event = ignore_catchup_range_response v2 = %b reason = epoch_outside_request from = %s"
              is_v2
              (String.sub
                 r.responder_addr
                 0
                 (min 12 (String.length r.responder_addr)));
            Lwt.return_unit
          | Some window ->
            let from_epoch = window.from_epoch in
            let response_epoch =
              match List.rev r.records with
              | last :: _ -> last.C_codec.epoch_id
              | [] -> from_epoch
            in
            let* validator_set = validator_set_for_frame t response_epoch in
            if not (C_types.is_validator validator_set r.responder_addr) then begin
              log_node t.config.my_addr
                "event = ignore_catchup_range_response v2 = %b reason = non_validator from = %s"
                is_v2
                (String.sub
                   r.responder_addr
                   0
                   (min 12 (String.length r.responder_addr)));
              Lwt.return_unit
            end else
            let records_root =
              if is_v2 then C_hash.catchup_records_root r.records
              else C_hash.catchup_records_root_v1_wire r.records in
            let legacy_sign_bytes = C_hash.catchup_range_response_sign_bytes
              ~chain_id:r.chain_id
              ~request_id:r.request_id
              ~responder_addr:r.responder_addr
              ~from_epoch
              ~records_root in
            let complete_request =
              C_hash.catchup_request_is_complete r.request_id
            in
            let complete_valid =
              is_v2
              && complete_request
              && verify_set_signature
                   validator_set
                   r.responder_addr
                   (C_hash.catchup_range_response_complete_sign_bytes
                      ~from_epoch
                      r)
                   r.signature
            in
            let legacy_valid =
              verify_set_signature
                validator_set
                r.responder_addr
                legacy_sign_bytes
                r.signature
            in
            let finality_request =
              Option.is_some (finality_request_epoch t r.request_id)
            in
            let request_scope =
              C_finality_query.request_scope
                ~complete_request
                ~finality_request
            in
            let response_proof =
              C_finality_query.response_proof
                ~complete_valid
                ~legacy_valid
                ~legacy_complete:
                  (is_v2 && catchup_records_complete r.records)
            in
            let response_route =
              C_finality_query.response_route
                request_scope
                response_proof
            in
            if response_route = C_finality_query.Ignore_legacy_response then begin
              log_node t.config.my_addr
                "event = ignore_catchup_range_response v2 = %b reason = legacy_signature from = %s"
                is_v2
                (String.sub
                   r.responder_addr
                   0
                   (min 12 (String.length r.responder_addr)));
              Lwt.return_unit
            end else if response_route = C_finality_query.Reject_response then begin
              log_node t.config.my_addr
                "event = ignore_catchup_range_response v2 = %b reason = bad_signature from = %s"
                is_v2 (String.sub r.responder_addr 0 (min 12 (String.length r.responder_addr)));
              Octra_net.P2p_swarm.report_bad_peer t.swarm _conn ~reason:"bad_signature_catchup_range";
              Lwt.return_unit
            end else if
              response_route = C_finality_query.Legacy_range_response
              && not
                   (response_source_matches
                      validator_set
                      ~peer_id:_conn.Octra_net.P2p_conn.peer_id
                      ~responder_addr:r.responder_addr)
            then begin
              log_node t.config.my_addr
                "event = ignore_catchup_range_response v2 = %b reason = relayed_legacy_response from = %s"
                is_v2
                (String.sub
                   r.responder_addr
                   0
                   (min 12 (String.length r.responder_addr)));
              Octra_net.P2p_swarm.report_bad_peer
                t.swarm
                _conn
                ~reason:"relayed_legacy_catchup_range";
              Lwt.return_unit
            end else
            let routed_response : C_codec.catchup_range_response option =
              match response_route with
              | C_finality_query.Legacy_range_response ->
                normalize_legacy_range_response r
              | C_finality_query.Complete_response
              | C_finality_query.Finality_response -> Some r
              | C_finality_query.Ignore_legacy_response
              | C_finality_query.Reject_response -> None
            in
            match routed_response with
            | None ->
              log_node t.config.my_addr
                "event = ignore_catchup_range_response v2 = %b reason = incomplete_legacy from = %s"
                is_v2
                (String.sub
                   r.responder_addr
                   0
                   (min 12 (String.length r.responder_addr)));
              Lwt.return_unit
            | Some (r : C_codec.catchup_range_response)
              when response_route = C_finality_query.Finality_response ->
              begin match finality_request_epoch t r.request_id with
              | Some expected_epoch ->
                let finalize =
                  match r.records with
                  | [ record ] when record.C_codec.epoch_id = expected_epoch ->
                    Option.map
                      (fun finality -> finality.C_codec.finalize)
                      record.finality
                  | _ -> None
                in
                (match finalize with
                 | Some value when value.C_types.epoch_id = expected_epoch ->
                   on_p2p_message
                     t
                     _conn
                     {
                       msg_type = Frame.msg_cons_finalize;
                       payload = C_codec.encode_finalize value;
                     }
                 | Some _
                 | None -> Lwt.return_unit)
              | None -> Lwt.return_unit
              end
            | Some (r : C_codec.catchup_range_response) ->
              if response_route = C_finality_query.Legacy_range_response then begin
                log_node t.config.my_addr
                  "event = accept_catchup_range_response proof = legacy_complete from = %s records = %d"
                  (String.sub
                     r.responder_addr
                     0
                     (min 12 (String.length r.responder_addr)))
                  (List.length r.records)
              end;
              let checked_epoch, state_root =
                match List.rev r.records with
                | last :: _ -> last.C_codec.epoch_id, Some last.C_codec.state_root
                | [] -> from_epoch, None in
              let head_epoch =
                match r.next_epoch with
                | Some e -> Int64.sub e 1L
                | None -> checked_epoch in
              remember_peer_state t
                ~source:"catchup_range"
                ~responder_addr:r.responder_addr
                ~head_epoch
                ~checked_epoch
                ~state_root;
              match finality_request_epoch t r.request_id with
              | Some expected_epoch ->
                let finalize =
                  match r.records with
                  | [ record ] when record.C_codec.epoch_id = expected_epoch ->
                    Option.map
                      (fun finality -> finality.C_codec.finalize)
                      record.finality
                  | _ -> None
                in
                (match finalize with
                 | Some value when value.C_types.epoch_id = expected_epoch ->
                   on_p2p_message
                     t
                     _conn
                     {
                       msg_type = Frame.msg_cons_finalize;
                       payload = C_codec.encode_finalize value;
                     }
                 | Some _
                 | None -> Lwt.return_unit)
              | None ->
                let record = {
                  responder_addr = r.responder_addr;
                  request_id = r.request_id;
                  status = r.status;
                  records = r.records;
                  next_epoch = r.next_epoch;
                } in
                let prior =
                  try Hashtbl.find t.catchup_responses r.request_id
                  with Not_found -> []
                in
                let already_seen =
                  List.exists
                    (fun (rec_ : catchup_range_response_record) ->
                      rec_.responder_addr = r.responder_addr)
                    prior
                in
                if not already_seen then
                  Hashtbl.replace
                    t.catchup_responses
                    r.request_id
                    (record :: prior);
                Lwt.return_unit
        end)
        (fun exn ->
          log_node t.config.my_addr
            "event = bad_catchup_range_response v2 = %b error = %s"
            is_v2 (Printexc.to_string exn);
          Lwt.return_unit)
    | t' when t' = Frame.msg_vote_evidence ->
      Lwt.catch (fun () ->
        let evidence = C_evidence.decode_vote_conflict frame.payload in
        let epoch = evidence.C_evidence.second.epoch_id in
        let* validator_set = validator_set_for_frame t epoch in
        match
          validate_vote_evidence
            ~chain_id:t.config.chain_id
            ~validator_set
            ~current:t.engine.state.height
            evidence
        with
        | Evidence_unknown_validator ->
          if epoch <= t.engine.state.height then
            remember_frame t frame.msg_type frame.payload;
          log_node t.config.my_addr
            "event = ignore_vote_evidence reason = unresolved_validator_set epoch = %Ld"
            epoch;
          Lwt.return_unit
        | Evidence_invalid ->
          remember_frame t frame.msg_type frame.payload;
          Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
            ~reason:"invalid_vote_evidence";
          Lwt.return_unit
        | Evidence_valid ->
          remember_frame t frame.msg_type frame.payload;
          report_validator_identity
            t
            validator_set
            evidence.C_evidence.second.validator
            ~reason:"vote_equivocation";
          if not (remember_vote_evidence t evidence) then
            Lwt.return_unit
          else
            Octra_net.P2p_swarm.broadcast_except t.swarm
              ~except:_conn.Octra_net.P2p_conn.peer_id
              (vote_evidence_frame evidence)
        )
      (fun exn ->
        remember_frame t frame.msg_type frame.payload;
        log_node t.config.my_addr "event = bad_vote_evidence error = %s"
          (Printexc.to_string exn);
        Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
          ~reason:"invalid_frame_vote_evidence";
        Lwt.return_unit)
    | t' when t' = Frame.msg_resource_attestation ->
      Lwt.catch (fun () ->
        let gossip = Resource_attestation_flow.decode_gossip frame.payload in
        if gossip.chain_id <> t.config.chain_id then begin
          Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
            ~reason:"resource_attestation_chain_mismatch";
          Lwt.return_unit
        end
        else begin
          let decision = admit_resource_attestation t gossip.attestation in
          log_node t.config.my_addr
            "event = resource_attestation decision = %s seen_epoch = %Ld resource_node = %s"
            (Resource_attestation_admission.decision_to_string decision)
            gossip.seen_epoch
            (String.sub gossip.attestation.Resource_attestations.node_id 0
              (min 14 (String.length gossip.attestation.Resource_attestations.node_id)));
          match decision with
          | Resource_attestation_admission.Accept ->
              Hashtbl.replace
                t.resource_attestations
                (Resource_attestations.attestation_id gossip.attestation)
                gossip.attestation;
              t.config.on_resource_attestation gossip
          | Resource_attestation_admission.Reject _ ->
              Octra_net.P2p_swarm.report_bad_peer t.swarm _conn
                ~reason:"invalid_resource_attestation";
              Lwt.return_unit
          | Resource_attestation_admission.Quarantine _ ->
              Lwt.return_unit
        end)
        (fun exn ->
          log_node t.config.my_addr "event = bad_resource_attestation error = %s"
            (Printexc.to_string exn);
          Lwt.return_unit)
    | _ -> Lwt.return_unit)
  end |> fun p ->
  if engine_output_frame frame.msg_type then
    Lwt.bind p (fun () -> process_outputs t)
  else
    p

let broadcast_resource_attestation t attestation =
  let gossip = Resource_attestation_flow.{
    chain_id = t.config.chain_id;
    seen_epoch = t.config.local_head_epoch ();
    attestation;
  } in
  match admit_resource_attestation t attestation with
  | Resource_attestation_admission.Accept ->
      Hashtbl.replace
        t.resource_attestations
        (Resource_attestations.attestation_id attestation)
        attestation;
      let payload = Resource_attestation_flow.encode_gossip gossip in
      Octra_net.P2p_swarm.broadcast t.swarm { msg_type = Frame.msg_resource_attestation; payload }
  | Resource_attestation_admission.Reject _
  | Resource_attestation_admission.Quarantine _ ->
      Lwt.return_unit

let split_prefix n values =
  let rec take selected remaining count =
    if count <= 0 then List.rev selected, remaining
    else
      match remaining with
      | [] -> List.rev selected, []
      | value :: rest -> take (value :: selected) rest (count - 1)
  in
  take [] values n

let rotate_values offset values =
  let before, after = split_prefix offset values in
  after @ before

let bundle_query_offset proposal_id count =
  if count <= 0 then 0
  else
    let width = min 16 (String.length proposal_id) in
    let rec fold index value =
      if index >= width then value
      else
        fold
          (index + 1)
          (((value * 257) + Char.code proposal_id.[index]) mod count)
    in
    fold 0 0

let ordered_query_peer_ids t seed =
  let active_ids = Hashtbl.create t.engine.vs.C_types.n in
  List.iter
    (fun (validator : C_types.validator_info) ->
      Hashtbl.replace
        active_ids
        (Octra_net.P2p_handshake.node_id_of_pubkey validator.pubkey)
        ())
    t.engine.vs.validators;
  let connected =
    Octra_net.P2p_swarm.connected_peers t.swarm
    |> List.map (fun conn -> conn.Octra_net.P2p_conn.peer_id)
    |> List.sort_uniq String.compare
  in
  let validators, others =
    List.partition (fun peer_id -> Hashtbl.mem active_ids peer_id) connected
  in
  let rotate values =
    rotate_values (bundle_query_offset seed (List.length values)) values
  in
  rotate validators @ rotate others

let bundle_query_peer_ids t proposal_id =
  ordered_query_peer_ids t proposal_id

let bundle_query_waves peer_ids =
  let first, rest = split_prefix 2 peer_ids in
  let second, rest = split_prefix 4 rest in
  [first; second; rest] |> List.filter (fun wave -> wave <> [])

let catchup_query_waves validator_set peer_ids =
  let width = max 1 (validator_set.C_types.f + 2) in
  let first, rest = split_prefix width peer_ids in
  let second, rest = split_prefix width rest in
  [first; second; rest] |> List.filter (fun wave -> wave <> [])

let catchup_wave_delay ~budget ~wave_count =
  if wave_count <= 1 then budget
  else min 0.5 (budget /. float_of_int wave_count)

let catchup_query_limit ~from_epoch ~max_epochs = function
  | Some cfg when from_epoch < cfg.activate_epoch ->
    let until_activation = Int64.sub cfg.activate_epoch from_epoch in
    if max_epochs <= 0
       || Int64.compare until_activation (Int64.of_int max_epochs) >= 0 then
      max_epochs
    else
      Int64.to_int until_activation
  | Some _
  | None -> max_epochs

let query_bundle t ~epoch_id ~proposal_id ~timeout_seconds
    ~(validate : bundle_response_record -> bool) =
  let open Lwt.Syntax in
  Hashtbl.replace t.bundle_responses proposal_id [];
  let q = C_codec.{
    chain_id = t.config.chain_id;
    epoch_id;
    proposal_id;
  } in
  let payload = C_codec.encode_bundle_query q in
  let query_frame = { Frame.msg_type = Frame.msg_query_bundle; payload } in
  let rejected : (string, unit) Hashtbl.t = Hashtbl.create 4 in
  let pick_valid responses =
    List.find_opt (fun (rec_ : bundle_response_record) ->
      if Hashtbl.mem rejected rec_.responder_addr then false
      else if validate rec_ then true
      else begin
        Hashtbl.replace rejected rec_.responder_addr ();
        log_node t.config.my_addr
          "event = reject_bundle_response reason = validate from = %s"
          (String.sub rec_.responder_addr 0 (min 14 (String.length rec_.responder_addr)));
        false
      end
    ) responses
  in
  let rec wait_valid deadline =
    if deadline_reached deadline then Lwt.return_none
    else
      let responses =
        try Hashtbl.find t.bundle_responses proposal_id with Not_found -> []
      in
      match pick_valid responses with
      | Some r -> Lwt.return_some r
      | None ->
        let* () = Lwt_unix.sleep 0.1 in
        wait_valid deadline
  in
  let deadline = deadline_after timeout_seconds in
  let send_wave peer_ids =
    Lwt_list.iter_p
      (fun peer_id ->
        Octra_net.P2p_swarm.send_to t.swarm ~peer_id query_frame)
      peer_ids
  in
  let rec query_waves = function
    | [] -> wait_valid deadline
    | _ when deadline_reached deadline -> Lwt.return_none
    | wave :: rest ->
      let* () = send_wave wave in
      let wave_deadline =
        if rest = [] then deadline
        else min deadline (deadline_after 0.25)
      in
      let* result = wait_valid wave_deadline in
      (match result with
       | Some _ -> Lwt.return result
       | None -> query_waves rest)
  in
  let peer_ids = bundle_query_peer_ids t proposal_id in
  let* result =
    match bundle_query_waves peer_ids with
    | [] ->
      let* () = Octra_net.P2p_swarm.broadcast t.swarm query_frame in
      wait_valid deadline
    | waves -> query_waves waves
  in
  Hashtbl.remove t.bundle_responses proposal_id;
  Lwt.return result

let query_catchup_range t ~from_epoch ~max_epochs ~timeout_seconds
    ~(validate : catchup_range_response_record -> bool) =
  let open Lwt.Syntax in
  let* validator_set_plan = load_validator_set_plan t in
  let max_epochs =
    catchup_query_limit
      ~from_epoch
      ~max_epochs
      validator_set_plan
  in
  let agreement_validator_set epoch_id =
    validator_set_at
      ~chain_id:t.config.chain_id
      ~current:t.engine.vs
      ~epoch:epoch_id
      validator_set_plan
    |> catchup_source_validator_set
  in
  let agreement_weight epoch_id =
    agreement_validator_set epoch_id
    |> C_types.round_skip_weight
  in
  let weight_of_responder epoch_id responder_addr =
    match
      C_types.weight_of_addr
        (agreement_validator_set epoch_id)
        responder_addr
    with
    | Some weight -> weight
    | None -> Z.zero
  in
  let agreement_reached epoch_id count weight =
    catchup_source_agreement_reached
      (agreement_validator_set epoch_id)
      ~signer_count:count
      ~signed_weight:weight
  in
  let take_prefix n xs =
    let rec loop acc i = function
      | _ when i <= 0 -> List.rev acc
      | [] -> List.rev acc
      | x :: tl -> loop (x :: acc) (i - 1) tl
    in
    loop [] n xs
  in
  let prefix_next_epoch (records : C_codec.catchup_epoch_record list) =
    match List.rev records with
    | last :: _ -> Some (Int64.add last.C_codec.epoch_id 1L)
    | [] -> None
  in
  let request_nonce = Unix.gettimeofday () in
  let request_id phase =
    Octra_net.Hash_domain.hash "octra:catchup_range_request_id:v1"
      (Printf.sprintf "%s:%Ld:%d:%f:%s"
        t.config.my_addr from_epoch max_epochs request_nonce phase)
    |> C_hash.mark_complete_catchup_request
  in
  let pick_valid_with rejected responses =
    let groups
      : (string, Z.t * int * int64 * catchup_range_response_record) Hashtbl.t =
      Hashtbl.create 8 in
    let prefix_groups
      : (string, Z.t * int * int * int64 * catchup_range_response_record) Hashtbl.t =
      Hashtbl.create 16
    in
    List.iter (fun (rec_ : catchup_range_response_record) ->
      if not (Hashtbl.mem rejected rec_.responder_addr) then begin
        if validate rec_ then begin
          match catchup_response_key rec_ with
          | None ->
            Hashtbl.replace rejected rec_.responder_addr ();
            log_node t.config.my_addr
              "event = reject_catchup_range_response reason = incomplete_record from = %s"
              (String.sub rec_.responder_addr 0
                (min 14 (String.length rec_.responder_addr)))
          | Some key ->
          let agreement_epoch =
            catchup_agreement_epoch ~from_epoch rec_.records
          in
          let responder_weight =
            weight_of_responder agreement_epoch rec_.responder_addr
          in
          let weight, count, agreement_epoch, repr =
            match Hashtbl.find_opt groups key with
            | Some (weight, count, agreement_epoch, repr) ->
              Z.add weight responder_weight, count + 1, agreement_epoch, repr
            | None -> responder_weight, 1, agreement_epoch, rec_
          in
          Hashtbl.replace groups key
            (weight, count, agreement_epoch, repr);
          if rec_.status = "ok" then begin
            let len = List.length rec_.records in
            for prefix_len = 1 to len do
              let prefix_records = take_prefix prefix_len rec_.records in
              let prefix_root = C_hash.catchup_records_root prefix_records in
              let prefix_key =
                "ok|prefix:" ^ string_of_int prefix_len ^ "|" ^ prefix_root
              in
              let prefix_repr = {
                rec_ with
                records = prefix_records;
                next_epoch = prefix_next_epoch prefix_records;
              } in
              let prefix_epoch =
                catchup_agreement_epoch ~from_epoch prefix_records
              in
              let prefix_responder_weight =
                weight_of_responder prefix_epoch rec_.responder_addr
              in
              let
                prefix_weight,
                prefix_count,
                _prefix_best_len,
                prefix_epoch,
                prefix_best_repr
              =
                match Hashtbl.find_opt prefix_groups prefix_key with
                | Some (weight, count, best_len, prefix_epoch, best_repr) ->
                  Z.add weight prefix_responder_weight,
                  count + 1,
                  best_len,
                  prefix_epoch,
                  best_repr
                | None ->
                  prefix_responder_weight,
                  1,
                  prefix_len,
                  prefix_epoch,
                  prefix_repr
              in
              Hashtbl.replace prefix_groups prefix_key
                (prefix_weight, prefix_count, prefix_len, prefix_epoch,
                 prefix_best_repr)
            done
          end
        end else begin
          Hashtbl.replace rejected rec_.responder_addr ();
          log_node t.config.my_addr
            "event = reject_catchup_range_response reason = validate from = %s"
            (String.sub rec_.responder_addr 0 (min 14 (String.length rec_.responder_addr)));
        end
      end
    ) responses;
    let best_full =
      Hashtbl.fold (fun _ (weight, count, epoch_id, repr) acc ->
        match acc with
        | None -> Some (weight, count, epoch_id, repr)
        | Some (best_weight, _, _, _) when Z.gt weight best_weight ->
          Some (weight, count, epoch_id, repr)
        | _ -> acc
      ) groups None
    in
    let best_prefix =
      Hashtbl.fold (fun _ (weight, count, prefix_len, epoch_id, repr) acc ->
        match acc with
        | None -> Some (weight, count, prefix_len, epoch_id, repr)
        | Some (best_weight, _, best_prefix_len, _, _)
          when prefix_len > best_prefix_len
               || (prefix_len = best_prefix_len
                   && Z.gt weight best_weight) ->
          Some (weight, count, prefix_len, epoch_id, repr)
        | _ -> acc
      ) prefix_groups None
    in
    match best_full, best_prefix with
    | Some (weight, count, epoch_id, repr), _
      when agreement_reached epoch_id count weight ->
      let required_weight = agreement_weight epoch_id in
      let records_root_hex =
        raw_to_hex (C_hash.catchup_records_root repr.records) in
      log_node t.config.my_addr
        "event = catchup_range_agreed responders = %d weight = %s required_weight = %s status = %s root = %s records = %d"
        count
        (Z.to_string weight)
        (Z.to_string required_weight)
        repr.status
        (String.sub records_root_hex 0 (min 16 (String.length records_root_hex)))
        (List.length repr.records);
      Some repr
    | _, Some (weight, count, prefix_len, epoch_id, repr)
      when agreement_reached epoch_id count weight ->
      let required_weight = agreement_weight epoch_id in
      let records_root_hex =
        raw_to_hex (C_hash.catchup_records_root repr.records) in
      log_node t.config.my_addr
        "event = catchup_range_prefix_agreed responders = %d weight = %s required_weight = %s prefix_records = %d root = %s"
        count
        (Z.to_string weight)
        (Z.to_string required_weight)
        prefix_len
        (String.sub records_root_hex 0 (min 16 (String.length records_root_hex)));
      Some repr
    | _ -> None
  in
  let rec wait_valid_with request_id rejected deadline =
    if deadline_reached deadline then Lwt.return_none
    else
      let responses =
        try Hashtbl.find t.catchup_responses request_id with Not_found -> []
      in
      match pick_valid_with rejected responses with
      | Some r -> Lwt.return_some r
      | None ->
        let* () = Lwt_unix.sleep 0.1 in
        wait_valid_with request_id rejected deadline
  in
  let cleanup request_id =
    Hashtbl.remove t.catchup_responses request_id;
    Hashtbl.remove t.catchup_query_windows request_id
  in
  let run_query ~msg_type ~budget =
    let request_id = request_id "range" in
    let query = C_codec.{
      chain_id = t.config.chain_id;
      request_id;
      from_epoch;
      max_epochs;
    } in
    let payload = C_codec.encode_catchup_query_range query in
    Hashtbl.remove t.catchup_responses request_id;
    Hashtbl.replace
      t.catchup_query_windows
      request_id
      { from_epoch; max_epochs };
    let rejected : (string, unit) Hashtbl.t = Hashtbl.create 4 in
    let frame = { Frame.msg_type; payload } in
    let peer_ids = ordered_query_peer_ids t request_id in
    let waves =
      catchup_query_waves
        (agreement_validator_set from_epoch)
        peer_ids
    in
    let deadline = deadline_after budget in
    let wave_delay =
      catchup_wave_delay ~budget ~wave_count:(List.length waves)
    in
    let rec send = function
      | [] -> wait_valid_with request_id rejected deadline
      | _ when deadline_reached deadline -> Lwt.return_none
      | wave :: rest ->
        let* () =
          Lwt_list.iter_p
            (fun peer_id ->
              Octra_net.P2p_swarm.send_to t.swarm ~peer_id frame)
            wave
        in
        let wave_deadline =
          if rest = [] then deadline
          else min deadline (deadline_after wave_delay)
        in
        let* result = wait_valid_with request_id rejected wave_deadline in
        (match result with
         | Some _ -> Lwt.return result
         | None -> send rest)
    in
    Lwt.finalize
      (fun () -> send waves)
      (fun () ->
        cleanup request_id;
        Lwt.return_unit)
  in
  let total_budget = bounded_timeout timeout_seconds in
  run_query
    ~msg_type:Frame.msg_query_catchup_range_v2
    ~budget:total_budget

let epoch_root_consensus_quorum t ~epoch_id ~root records =
  let validators =
    records
    |> List.filter_map (fun (record : epoch_root_response_record) ->
      match record.state_root with
      | Some candidate when String.equal candidate root ->
        Some record.responder_addr
      | Some _
      | None -> None)
    |> List.sort_uniq String.compare
  in
  C_types.has_quorum_at
    ~chain_id:t.config.chain_id
    ~epoch_id
    t.engine.vs
    validators

let epoch_root_wait_reached t ~epoch_id ~wait_for records =
  match wait_for with
  | Source_agreement ->
    let counts = Hashtbl.create 8 in
    List.exists (fun (r : epoch_root_response_record) ->
      match r.state_root with
      | None -> false
      | Some root ->
        let responder_weight =
          catchup_responder_weight t ~epoch_id r.responder_addr
        in
        let weight, count =
          match Hashtbl.find_opt counts root with
          | Some (weight, count) -> Z.add weight responder_weight, count + 1
          | None -> responder_weight, 1
        in
        Hashtbl.replace counts root (weight, count);
        catchup_agreement_reached t ~epoch_id ~count ~weight
    ) records
  | Consensus_quorum ->
    records
    |> List.filter_map (fun (record : epoch_root_response_record) ->
      record.state_root)
    |> List.sort_uniq String.compare
    |> List.exists (fun root ->
      epoch_root_consensus_quorum t ~epoch_id ~root records)

let finality_query_peer_ids validator_set ~epoch_id records =
  let limit = max 1 (validator_set.C_types.f + 2) in
  records
  |> List.filter (fun (record : epoch_root_response_record) ->
    Int64.compare record.responder_head_epoch epoch_id >= 0)
  |> List.filter_map (fun (record : epoch_root_response_record) ->
    match C_types.pubkey_of_addr validator_set record.responder_addr with
    | Some pubkey ->
      Some (Octra_net.P2p_handshake.node_id_of_pubkey pubkey)
    | None -> None)
  |> List.sort_uniq String.compare
  |> List.filteri (fun index _ -> index < limit)

let finality_proof_peer_ids validator_set ~epoch_id records =
  records
  |> List.filter (fun (record : epoch_root_response_record) ->
    Int64.compare record.responder_head_epoch epoch_id >= 0)
  |> List.filter_map (fun (record : epoch_root_response_record) ->
    match C_types.pubkey_of_addr validator_set record.responder_addr with
    | Some pubkey ->
      Some (Octra_net.P2p_handshake.node_id_of_pubkey pubkey)
    | None -> None)
  |> List.sort_uniq String.compare

let close_finality_query_windows t =
  Hashtbl.iter
    (fun request_id _ ->
      Hashtbl.remove t.catchup_query_windows request_id)
    t.finality_query_requests;
  Hashtbl.clear t.finality_query_requests

let request_missing_finalize t ~epoch_id records =
  let open Lwt.Syntax in
  let* validator_set = validator_set_for_frame t epoch_id in
  let peer_ids = finality_query_peer_ids validator_set ~epoch_id records in
  match peer_ids with
  | [] -> Lwt.return_unit
  | _ ->
    let now = Mtime_clock.elapsed_ns () in
    match C_finality_query.plan ~now ~epoch:epoch_id t.finality_query with
    | C_finality_query.Wait
    | C_finality_query.Rest -> Lwt.return_unit
    | C_finality_query.Send next ->
      t.finality_query <- next;
      let attempts =
        match next with
        | C_finality_query.Idle -> 0
        | C_finality_query.Sent sent -> sent.attempts
      in
      close_finality_query_windows t;
      let request_id =
        Octra_net.Hash_domain.hash
          "octra:finality_query_request"
          (Printf.sprintf
             "%s:%Ld:%d:%Ld"
             t.config.my_addr
             epoch_id
             attempts
             now)
        |> C_hash.mark_complete_catchup_request
      in
      let query = C_codec.{
        chain_id = t.config.chain_id;
        request_id;
        from_epoch = epoch_id;
        max_epochs = 1;
      } in
      let frame = {
        Frame.msg_type = Frame.msg_query_catchup_range_v2;
        payload = C_codec.encode_catchup_query_range query;
      } in
      Hashtbl.replace
        t.catchup_query_windows
        request_id
        { from_epoch = epoch_id; max_epochs = 1 };
      Hashtbl.replace t.finality_query_requests request_id epoch_id;
      log_node t.config.my_addr
        "event = request_missing_finalize epoch = %Ld peers = %d attempt = %d"
        epoch_id
        (List.length peer_ids)
        attempts;
      Lwt_list.iter_p
        (fun peer_id ->
          Octra_net.P2p_swarm.send_to t.swarm ~peer_id frame)
        peer_ids

let request_finality_proof t ~epoch_id records =
  let open Lwt.Syntax in
  let* validator_set = validator_set_for_frame t epoch_id in
  let peer_ids = finality_proof_peer_ids validator_set ~epoch_id records in
  match peer_ids with
  | [] ->
    warn_node t.config.my_addr
      "event = finality_proof status = unavailable epoch = %Ld reason = no_peer"
      epoch_id;
    Lwt.return_unit
  | _ ->
    let now = Mtime_clock.elapsed_ns () in
    let request_id =
      Octra_net.Hash_domain.hash
        "octra:finality_proof_request"
        (Printf.sprintf "%s:%Ld:%Ld" t.config.my_addr epoch_id now)
      |> C_hash.mark_complete_catchup_request
    in
    let query = C_codec.{
      chain_id = t.config.chain_id;
      request_id;
      from_epoch = epoch_id;
      max_epochs = 1;
    } in
    let frame = {
      Frame.msg_type = Frame.msg_query_catchup_range_v2;
      payload = C_codec.encode_catchup_query_range query;
    } in
    clear_finality_proof_requests t;
    Hashtbl.replace
      t.catchup_query_windows
      request_id
      { from_epoch = epoch_id; max_epochs = 1 };
    Hashtbl.replace t.finality_proof_requests request_id epoch_id;
    log_node t.config.my_addr
      "event = finality_proof status = request epoch = %Ld peers = %d"
      epoch_id
      (List.length peer_ids);
    Lwt_list.iter_p
      (fun peer_id ->
        Octra_net.P2p_swarm.send_to t.swarm ~peer_id frame)
      peer_ids

let query_epoch_root
    ?(wait_for = Source_agreement)
    ?(request_next = true)
    t
    ~epoch_id
    ~timeout_seconds =
  let open Lwt.Syntax in
  Hashtbl.replace t.epoch_root_responses epoch_id [];
  let has_root_quorum records =
    epoch_root_wait_reached t ~epoch_id ~wait_for records
  in
  let q = C_codec.{
    chain_id = t.config.chain_id;
    epoch_id;
  } in
  let payload = C_codec.encode_epoch_root_query q in
  let* () = Octra_net.P2p_swarm.broadcast t.swarm
    { msg_type = Frame.msg_query_epoch_root; payload } in
  let deadline = deadline_after timeout_seconds in
  let rec wait () =
    let collected = match Hashtbl.find_opt t.epoch_root_responses epoch_id with
      | Some records -> records
      | None -> []
    in
    if deadline_reached deadline || has_root_quorum collected then
      Lwt.return collected
    else
      let* () = Lwt_unix.sleep 0.1 in
      wait ()
  in
  let* collected = wait () in
  let local_head = t.config.local_head_epoch () in
  let* () =
    if request_next
       && epoch_id = local_head
       && Int64.sub t.engine.state.height local_head = 1L then
      request_missing_finalize
        t
        ~epoch_id:t.engine.state.height
        collected
    else
      Lwt.return_unit
  in
  Hashtbl.remove t.epoch_root_responses epoch_id;
  Lwt.return collected

let proof_wait tries =
  match tries with
  | 0 -> 5.0
  | 1 -> 10.0
  | _ -> 30.0

let recover_finality_proof t =
  let open Lwt.Syntax in
  let retry tries =
    let* () = Lwt_unix.sleep (proof_wait tries) in
    Lwt.return (C_loop.Next (min 2 (tries + 1)))
  in
  let turn tries =
    if not t.running || not !(t.finality_proof_needed) then
      Lwt.return C_loop.Stop
    else
      Lwt.catch
        (fun () ->
          let local_head = t.config.local_head_epoch () in
          if Int64.compare local_head 0L < 0
             || not (Int64.equal t.engine.state.height (Int64.succ local_head))
          then
            retry tries
          else begin
            let* records =
              query_epoch_root
                ~request_next:false
                t
                ~epoch_id:local_head
                ~timeout_seconds:3.0
            in
            let* () = request_finality_proof t ~epoch_id:local_head records in
            let* () = Lwt_unix.sleep (proof_wait tries) in
            clear_finality_proof_requests t;
            Lwt.return (C_loop.Next (min 2 (tries + 1)))
          end)
        (fun exn ->
          warn_node t.config.my_addr
            "event = finality_proof status = retry reason = local_error detail = %s"
            (Printexc.to_string exn);
          retry tries)
  in
  C_loop.run turn 0

let try_propose ?parent_commit t ~header ~tx_hashes =
  C_engine.do_propose
    ?parent_commit
    t.engine
    header
    tx_hashes
    ~sign_fn:t.config.sign_fn;
  process_outputs t

let wake_ready t =
  let open Lwt.Syntax in
  let* () = broadcast_round_sync t ~request:true in
  let* _ = try_current_leader_proposal t in
  process_outputs t

let clear_local_transients t =
  t.proposal_build <- None;
  t.proposal_verify <- None;
  Hashtbl.clear t.pending_votes;
  t.epoch_start_mono <- Mtime_clock.elapsed_ns ()

let clear_height_local_state t =
  clear_local_transients t;
  t.proposal_wait <- None;
  t.past_round <- None;
  Hashtbl.clear t.round_fetch_replies;
  Hashtbl.clear t.durable_votes;
  Hashtbl.clear t.deferred_proposals

let clear_round_local_state t ~height ~round =
  clear_local_transients t;
  (match t.proposal_wait with
   | Some wait when wait.proposal.epoch_id = height
                    && wait.proposal.round >= round -> ()
   | Some _
   | None -> t.proposal_wait <- None);
  Hashtbl.filter_map_inplace
    (fun _ (proposal : C_types.propose) ->
      if proposal.epoch_id = height && proposal.round >= round then
        Some proposal
      else
        None)
    t.deferred_proposals

let reset_height t height =
  Hashtbl.clear t.round_peers;
  C_engine.start_height t.engine height;
  resume_local_vote t

let start_height t height =
  let open Lwt.Syntax in
  let* () = maybe_activate_scheduled_validator_set t ~target_epoch:height in
  clear_height_local_state t;
  reset_height t height;
  load_round_sync t;
  process_outputs t

let restore_precommit_lock t proposal =
  if t.running then Error "consensus driver is already running"
  else C_engine.restore_precommit_lock t.engine proposal

let realign_progress t ~height ~round =
  let open Lwt.Syntax in
  let current_height = t.engine.state.height in
  if height < current_height then
    Lwt.return_unit
  else
    let* () =
      if height > current_height then begin
        log_node t.config.my_addr
          "event = realign_progress old_height = %Ld new_height = %Ld"
          current_height height;
        start_height t height
      end else begin
        let current_round = t.engine.state.round in
        let target_round = max (current_round + 1) (round + 1) in
        log_node t.config.my_addr
          "event = realign_progress height = %Ld old_round = %d new_round = %d"
          height current_round target_round;
        clear_round_local_state t ~height ~round:target_round;
        C_engine.realign_round t.engine target_round;
        process_outputs t
      end
    in
    wake_ready t

let start t =
  C_engine.set_round_skip_ready
    t.engine
    (fun () -> not (proposal_work_active t));
  resume_local_vote t;
  t.running <- true;
  let open Lwt.Syntax in
  log "event = driver_start node = %s height = %Ld n = %d quorum = %d"
    t.config.my_addr t.engine.state.height t.engine.vs.n t.engine.vs.quorum;
  let min_peers =
    if t.n_validators <= 1 then 0
    else max 1 (t.engine.vs.quorum - 1) in
  let rec wait_peers tries =
    let pc = Octra_net.P2p_swarm.connected_count t.swarm in
    if pc >= min_peers || tries <= 0 then Lwt.return_unit
    else begin
      log_node t.config.my_addr
        "event = wait_peers connected = %d validators = %d min = %d"
        pc t.n_validators min_peers;
      let* () = Lwt_unix.sleep 1.0 in
      wait_peers (tries - 1)
    end
  in
  let* () = wait_peers 60 in
  log_node t.config.my_addr
    "event = start_consensus connected = %d validators = %d min = %d"
    (Octra_net.P2p_swarm.connected_count t.swarm) t.n_validators min_peers;
  let* () = Lwt_unix.sleep 3.0 in
  let pristine = C_engine.is_pristine t.engine in
  let* () =
    if pristine then
      start_height t t.engine.state.height
    else
      maybe_activate_scheduled_validator_set
        t
        ~target_epoch:t.engine.state.height
  in
  if not pristine then load_round_sync t;
  if !(t.finality_proof_needed) then
    Lwt.async (fun () ->
      recover_finality_proof t);
  let* () = broadcast_round_sync t ~request:true in
  let* _ = try_current_leader_proposal t in
  process_outputs t

let stop t =
  t.running <- false;
  Lwt.return_unit

let current_height t = t.engine.state.height
let current_round t = t.engine.state.round
let current_step t = t.engine.state.step
let active_validator_set t = t.engine.vs
let am_i_leader t = C_engine.am_i_leader t.engine