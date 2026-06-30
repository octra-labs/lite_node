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


type finalized_state =
  | No_trigger
  | Missing_header
  | Cached_bundle
  | Empty_bundle
  | Missing_bundle of {
      target_epoch : int64;
    }

type action =
  | Wait
  | Apply
  | Clear_stale_trigger
  | Store_empty_bundle_and_apply
  | Queue_missing_bundle of {
      target_epoch : int64;
      reason : string;
    }

type t = {
  action : action;
  log_tick : bool;
  next_sleep : float;
}

type 'header prepared = {
  state : finalized_state;
  empty_bundle_header : 'header option;
}

let missing_bundle_reason = "finalized_bundle_missing_tick"

let prepared state empty_bundle_header =
  { state; empty_bundle_header }

let finalized_state ~consensus_mode ~consensus_finalized ~current_epoch
    ~find_finalized ~cached_bundle_for_pid ~header_has_empty_bundle =
  if consensus_mode && consensus_finalized then
    match find_finalized current_epoch with
    | Some finalize ->
      let header = finalize.Octra_consensus.C_types.header in
      let pid = Octra_consensus.C_hash.proposal_id header in
      if cached_bundle_for_pid pid then
        prepared Cached_bundle None
      else if header_has_empty_bundle header then
        prepared Empty_bundle (Some header)
      else
        prepared
          (Missing_bundle {
            target_epoch = header.Octra_consensus.C_types.epoch_id;
          })
          None
    | None ->
      prepared Missing_header None
  else
    prepared No_trigger None

let consensus_action = function
  | No_trigger -> Wait
  | Missing_header -> Clear_stale_trigger
  | Cached_bundle -> Apply
  | Empty_bundle -> Store_empty_bundle_and_apply
  | Missing_bundle { target_epoch } ->
    Queue_missing_bundle {
      target_epoch;
      reason = missing_bundle_reason;
    }

let should_apply = function
  | Apply | Store_empty_bundle_and_apply -> true
  | Wait | Clear_stale_trigger | Queue_missing_bundle _ -> false

let apply_action ~current_epoch ~clear_trigger ~store_empty_bundle
    ~queue_missing_bundle ~warn ~clear_finalized = function
  | Store_empty_bundle_and_apply ->
    store_empty_bundle ();
    if clear_trigger then clear_finalized ()
  | Queue_missing_bundle queue ->
    queue_missing_bundle
      ~target_epoch:queue.target_epoch
      ~reason:queue.reason;
    warn
      (Printf.sprintf
         "BFT finalize trigger epoch = %d deferred = canonical_bundle_missing"
         current_epoch);
    if clear_trigger then clear_finalized ()
  | Clear_stale_trigger ->
    warn
      (Printf.sprintf
         "BFT finalize trigger epoch = %d pending_finalized_header = missing action = clear_stale_trigger"
         current_epoch);
    if clear_trigger then clear_finalized ()
  | Wait | Apply ->
    if clear_trigger then clear_finalized ()

let plan ~consensus_mode ~epoch_duration ~elapsed ~finalized_state =
  if consensus_mode then
    let action = consensus_action finalized_state in
    {
      action;
      log_tick = should_apply action;
      next_sleep = 0.1;
    }
  else
    let action =
      if elapsed >= epoch_duration then Apply else Wait
    in
    {
      action;
      log_tick = true;
      next_sleep = epoch_duration;
    }