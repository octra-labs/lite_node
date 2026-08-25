(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_catchup = Octra_consensus.C_catchup
module C_driver = Octra_consensus.C_driver
module H = Consensus_health_probe
module Log = Octra_log

type config = {
  active_f : int;
  validator_count : int;
  state_attest_configured : int;
  snapshot_policy_threshold : int;
  soft_catchup_max_lag : int;
  quarantine_ahead_streak_threshold : int;
  quarantine_ahead_grace_epochs : int;
  quarantine_ahead_drift_tolerance : int;
}

type deps = {
  normalize_next_epoch_for_head : source:string -> unit;
  committed_head_epoch : unit -> int;
  current_epoch : unit -> int;
  catchup_next_target : unit -> int64 option;
  attested_head : int -> bool;
  clear_state_attested : unit -> unit;
  set_catchup_in_progress : bool -> unit;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  query_epoch_root :
    epoch_id:int64 ->
    timeout_seconds:float ->
    C_driver.epoch_root_response_record list Lwt.t;
  root_consensus_quorum :
    epoch_id:int64 ->
    root:string ->
    C_driver.epoch_root_response_record list ->
    bool;
  read_local_root_raw : unit -> string Lwt.t;
  committed_epoch_root_raw : int -> string option;
  peer_snapshot : unit -> string;
  drain_pending_finalized : unit -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
  require_sync : Sync_need.t -> unit;
  run_catchup_to_target :
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  ahead_streak : unit -> int;
  incr_ahead_streak : unit -> unit;
}

type fork_repair_deps = {
  committed_head_epoch : unit -> int;
  rewind_allowed : target:int -> head:int -> bool;
  target_matches : target:int -> root:string -> bool;
  empty_after : target:int -> head:int -> bool;
  finality_target_ready : target:int -> root:string -> (unit, string) result;
  stage_empty : target:int -> root:string -> Octra_core.Fork_head_repair.result Lwt.t;
  mark_quarantine : string -> unit;
  stop : string -> unit;
}

let short_hex8 s =
  String.concat ""
    (List.init
       (min 8 (String.length s))
       (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let repair_snapshot (deps : fork_repair_deps) reason =
  deps.mark_quarantine ("fork_snapshot_required:" ^ reason);
  Lwt.return false

let await_rule_activation_quorum (deps : fork_repair_deps) =
  deps.mark_quarantine "ahead_of_target_by_rule_activation_boundary";
  Lwt.return false

let repair_empty_fork (deps : fork_repair_deps) ~target_epoch ~target_root ~required
    ~current_root_quorum =
  let open Lwt.Syntax in
  let target = Int64.to_int target_epoch in
  let our_head = deps.committed_head_epoch () in
  if target < 0 then repair_snapshot deps "negative_target"
  else if not (deps.rewind_allowed ~target ~head:our_head) then
    await_rule_activation_quorum deps
  else
    let target_local_matches =
      deps.target_matches ~target ~root:target_root
    in
    let empty_fork =
      deps.empty_after ~target ~head:our_head
    in
    match C_catchup.decide_fork_repair
            ~our_head:(Int64.of_int our_head)
            ~target:target_epoch
            ~current_root_quorum
            ~target_root_quorum:true
            ~target_local_matches
            ~empty_fork with
    | C_catchup.No_fork_repair ->
      Lwt.return false
    | C_catchup.Fork_snapshot_required reason ->
      repair_snapshot deps reason
    | C_catchup.Rollback_fork_head _ ->
      begin
        match deps.finality_target_ready ~target ~root:target_root with
        | Error reason ->
          repair_snapshot deps ("finality_target:" ^ reason)
        | Ok () ->
          let* result = deps.stage_empty ~target ~root:target_root in
          match result with
          | Octra_core.Fork_head_repair.Snapshot_required reason ->
            repair_snapshot deps reason
          | Octra_core.Fork_head_repair.Staged staged ->
            deps.mark_quarantine "fork_repair_staged";
            Log.fatal "catchup"
              "event = fork_repair status = staged target = %d old_head = %d required = %d root = %s action = restart"
              staged.target staged.old_head required (short_hex8 staged.root);
            deps.stop "fork_repair_staged";
            Lwt.return true
      end

let prove_target_root deps ~source_head ~target_epoch ~required =
  let open Lwt.Syntax in
  if Int64.compare target_epoch (Int64.of_int source_head) >= 0 then
    Lwt.return_none
  else
    let* responses =
      deps.query_epoch_root ~epoch_id:target_epoch ~timeout_seconds:5.0
    in
    match H.peer_root_with_quorum ~required responses with
    | Some majority
      when deps.root_consensus_quorum
             ~epoch_id:target_epoch
             ~root:majority.H.root
             responses ->
      Lwt.return_some majority
    | Some majority ->
      Log.warn "catchup"
        "event = fork_target_root_unproved target = %Ld count = %d required = %d"
        target_epoch majority.H.count required;
      Lwt.return_none
    | None ->
      Log.warn "catchup"
        "event = fork_target_root_missing target = %Ld required = %d"
        target_epoch required;
      Lwt.return_none

let peer_target_epoch (deps : deps) ~active_f ~our_head responses =
  let peer_heads =
    List.map
      (fun (r : C_driver.epoch_root_response_record) ->
        {
          C_catchup.responder_addr = r.responder_addr;
          responder_head_epoch = r.responder_head_epoch;
        })
      responses
  in
  let peer_target =
    C_catchup.pick_target_epoch
      ~responses:peer_heads
      ~f:active_f
      ~fallback:our_head
  in
  match deps.catchup_next_target () with
  | Some queued when Int64.compare queued peer_target > 0 -> queued
  | _ -> peer_target

let clear_unattested_current_head (deps : deps) head =
  if not (deps.attested_head head) then deps.clear_state_attested ()

let quarantine_peer_root_mismatch (deps : deps) ~head count =
  deps.mark_quarantine
    (Printf.sprintf "peer_root_mismatch_at_head count = %d epoch = %d" count head)

let accept_current_root
    (deps : deps)
    ~head
    ~root
    ~matching
    ~consensus_proved
    reason =
  let open Lwt.Syntax in
  let quarantine_active = deps.quarantine_active () in
  let quarantine_reason = deps.quarantine_reason () in
  let clear_evidence =
    if not quarantine_active then
      Some reason
    else
      Consensus_runtime_state.root_recovery_evidence
        ~reason:quarantine_reason
        ~evidence:reason
        ~consensus_proved
  in
  match clear_evidence with
  | None ->
    deps.clear_state_attested ();
    deps.set_catchup_in_progress false;
    Log.warn "catchup"
      "event = quarantine_preserved reason = %s evidence = %s head = %d matching = %d consensus_proved = %b"
      quarantine_reason reason head matching consensus_proved;
    Lwt.return_unit
  | Some clear_evidence ->
    deps.set_state_attested ~head ~root;
    deps.set_catchup_in_progress false;
    deps.clear_quarantine clear_evidence;
    let* () = deps.drain_pending_finalized () in
    deps.wake_ready ()

let wake_if_ready (deps : deps) wake_ready =
  let open Lwt.Syntax in
  if wake_ready then
    let* () = deps.drain_pending_finalized () in
    deps.wake_ready ()
  else
    Lwt.return_unit

let range_recovery_reason reason =
  String.starts_with ~prefix:"catchup_failed:" reason
  || String.starts_with ~prefix:"catchup_apply_failed:" reason
  || String.starts_with ~prefix:"lag_" reason
  || String.starts_with ~prefix:"bundle_wait_timeout_epoch_" reason

let run ?(stale_retries = 1) ?repair
    ?(http_target = fun () -> Lwt.return_none) cfg deps =
  let open Lwt.Syntax in
  let state_attest_required =
    H.required_attesters
      ~active_f:cfg.active_f
      ~validator_count:cfg.validator_count
      ~configured:cfg.state_attest_configured
  in
  let rec attempt stale_retries =
    deps.normalize_next_epoch_for_head ~source:"probe_consensus_health";
    let our_head_int = deps.committed_head_epoch () in
    let our_head = Int64.of_int our_head_int in
    let already_attested = deps.attested_head our_head_int in
    let query_log = if already_attested then Log.trace else Log.info in
    query_log "catchup" "event = query_peers head = %d" our_head_int;
    let* responses =
      deps.query_epoch_root ~epoch_id:our_head ~timeout_seconds:5.0
    in
    let head_after_query = deps.committed_head_epoch () in
    if head_after_query <> our_head_int then begin
      Log.trace "catchup"
        "event = moving_head phase = query start_head = %d head_after_query = %d retries_left = %d"
        our_head_int head_after_query stale_retries;
      match H.moving_head_plan ~stale_retries with
      | H.Retry_probe next_retries -> attempt next_retries
      | H.Stop_probe -> Lwt.return_unit
    end else
      let n_resp = List.length responses in
      if n_resp < state_attest_required then begin
        let has_head = deps.attested_head our_head_int in
        let log = if has_head then Log.trace else Log.warn in
        log "catchup"
          "event = low_peer_responses responses = %d required = %d head = %d attested = %b"
          n_resp state_attest_required our_head_int has_head;
        deps.set_catchup_in_progress false;
        if not has_head then deps.clear_state_attested ();
        let range_allowed =
          if deps.quarantine_active () then
            range_recovery_reason (deps.quarantine_reason ())
          else
            true
        in
        if range_allowed then begin
          let* target = http_target () in
          match target with
          | Some target when Int64.compare target our_head > 0 ->
            Log.warn "catchup"
              "event = attestation_http_secondary head = %d target = %Ld responses = %d required = %d"
              our_head_int target n_resp state_attest_required;
            begin
              match
                C_catchup.classify_lag
                  ~target
                  ~our_head
                  ~max_lag:cfg.snapshot_policy_threshold
              with
              | C_catchup.Snapshot_required lag ->
                Log.warn "catchup"
                  "event = signed_snapshot_required head = %d target = %Ld lag = %d source = http_attestation"
                  our_head_int target lag;
                deps.set_catchup_in_progress false;
                if our_head_int < 0 || our_head_int = max_int then begin
                  deps.mark_quarantine "snapshot_plan_invalid";
                  Lwt.return_unit
                end else begin
                  deps.require_sync
                    (Sync_need.root
                       ~epoch:(our_head_int + 1)
                       ~head:our_head_int);
                  Lwt.return_unit
                end
              | C_catchup.Do_catchup _ ->
                deps.run_catchup_to_target
                  ~target_epoch:target
                  ~reason:"attestation_secondary"
              | C_catchup.In_sync
              | C_catchup.Ahead_of_target _ -> Lwt.return_unit
            end
          | _ -> Lwt.return_unit
        end else
          Lwt.return_unit
      end else
        let target_epoch =
          peer_target_epoch deps ~active_f:cfg.active_f ~our_head responses
        in
        let live_epoch_before_root = deps.current_epoch () - 1 in
        match C_catchup.classify_probe_sample
                ~start_head:our_head_int
                ~live_head:live_epoch_before_root with
        | C_catchup.Stale _ ->
          Log.trace "catchup"
            "event = stale_sample phase = query start_head = %d live_head = %d responses = %d retries_left = %d"
            our_head_int live_epoch_before_root n_resp stale_retries;
          begin
            match H.stale_sample_plan
                    ~live_head_attested:(deps.attested_head live_epoch_before_root)
                    ~stale_retries with
            | H.Retry_probe next_retries -> attempt next_retries
            | H.Stop_probe -> Lwt.return_unit
          end
        | C_catchup.Fresh ->
          let* live_root = deps.read_local_root_raw () in
          let head_after_root = deps.committed_head_epoch () in
          if head_after_root <> our_head_int then begin
            Log.trace "catchup"
              "event = moving_head phase = root_read start_head = %d head_after_root = %d retries_left = %d"
              our_head_int head_after_root stale_retries;
            match H.moving_head_plan ~stale_retries with
            | H.Retry_probe next_retries -> attempt next_retries
            | H.Stop_probe -> Lwt.return_unit
          end else
            let live_epoch_after_root = deps.current_epoch () - 1 in
            match C_catchup.classify_probe_sample
                    ~start_head:our_head_int
                    ~live_head:live_epoch_after_root with
            | C_catchup.Stale _ ->
              Log.trace "catchup"
                "event = stale_sample phase = root_read start_head = %d live_head = %d responses = %d retries_left = %d"
                our_head_int live_epoch_after_root n_resp stale_retries;
              begin
                match H.stale_sample_plan
                        ~live_head_attested:(deps.attested_head live_epoch_after_root)
                        ~stale_retries with
                | H.Retry_probe next_retries -> attempt next_retries
                | H.Stop_probe -> Lwt.return_unit
              end
            | C_catchup.Fresh ->
              let committed_root_opt = deps.committed_epoch_root_raw our_head_int in
              let peer_snapshot = deps.peer_snapshot () in
              match committed_root_opt with
              | Some committed_root when committed_root <> live_root ->
                Log.error "catchup"
                  "event = local_root_mismatch head = %d live = %s committed = %s peers = %s"
                  our_head_int (short_hex8 live_root) (short_hex8 committed_root)
                  peer_snapshot;
                deps.mark_quarantine
                  (Printf.sprintf "local_state_root_mismatch_at_head_%d" our_head_int);
                deps.clear_state_attested ();
                deps.set_catchup_in_progress false;
                Lwt.return_unit
              | _ ->
                let peer_root_quorum =
                  H.peer_root_quorum
                    ~required:state_attest_required
                    ~local_root:live_root
                    responses
                in
                let peer_root_quorum_count =
                  H.root_quorum_count peer_root_quorum
                in
                let root_consensus_proved =
                  deps.root_consensus_quorum
                    ~epoch_id:our_head
                    ~root:live_root
                    responses
                in
                let genesis_attested =
                  C_catchup.genesis_state_attested
                    ~head:our_head_int
                    ~target:target_epoch
                    ~has_committed_root:(committed_root_opt <> None)
                    ~responses:n_resp
                    ~required:state_attest_required
                in
                let majority_peer_root = H.peer_root_majority responses in
                let attestation_log =
                  if already_attested then Log.trace else Log.info
                in
                attestation_log "catchup"
                  "event = state_attestation head = %d live = %s committed = %s responses = %d required = %d target = %Ld majority = %s peers = %s"
                  our_head_int
                  (short_hex8 live_root)
                  (match committed_root_opt with Some r -> short_hex8 r | None -> "-")
                  n_resp
                  state_attest_required
                  target_epoch
                  (match majority_peer_root with
                   | Some { H.root; count } ->
                     Printf.sprintf "%s:%d" (short_hex8 root) count
                   | None -> "-")
                  peer_snapshot;
                match C_catchup.classify_lag
                        ~target:target_epoch
                        ~our_head
                        ~max_lag:cfg.snapshot_policy_threshold with
                | C_catchup.In_sync ->
                  begin
                    match H.in_sync_plan
                            ~required:state_attest_required
                            ~has_committed_root:(committed_root_opt <> None)
                            ~genesis_attested
                            peer_root_quorum with
                    | H.Peer_root_mismatch { count; _ } ->
                      quarantine_peer_root_mismatch deps ~head:our_head_int count;
                      Lwt.return_unit
                    | H.Accept_genesis ->
                      accept_current_root deps ~head:our_head_int ~root:live_root
                        ~matching:peer_root_quorum_count
                        ~consensus_proved:root_consensus_proved
                        "genesis_in_sync"
                    | H.Missing_peer_root_quorum count ->
                      Log.warn "catchup"
                        "event = missing_peer_root_quorum head = %d count = %d required = %d"
                        our_head_int count state_attest_required;
                      clear_unattested_current_head deps our_head_int;
                      deps.set_catchup_in_progress false;
                      Lwt.return_unit
                    | H.Accept_current_root ->
                      accept_current_root deps ~head:our_head_int ~root:live_root
                        ~matching:peer_root_quorum_count
                        ~consensus_proved:root_consensus_proved
                        "in_sync"
                  end
                | C_catchup.Ahead_of_target n ->
                  deps.incr_ahead_streak ();
                  deps.set_catchup_in_progress false;
                  if not (H.root_quorum_has_quorum peer_root_quorum) then begin
                    Log.warn "catchup"
                      "event = ahead_no_quorum ahead = %d count = %d required = %d"
                      n peer_root_quorum_count state_attest_required;
                    match H.ahead_no_quorum_plan
                            ~head:our_head_int
                            ~ahead_by:n
                            ~grace_epochs:cfg.quarantine_ahead_grace_epochs
                            ~drift_tolerance:cfg.quarantine_ahead_drift_tolerance
                            ~quarantine_active:(deps.quarantine_active ()) with
                    | H.Wait_current_head ->
                      Log.warn "catchup"
                        "event = ahead_wait_current_head ahead = %d head = %d current_quorum = false grace = %d tolerance = %d"
                        n our_head_int cfg.quarantine_ahead_grace_epochs
                        cfg.quarantine_ahead_drift_tolerance;
                      accept_current_root deps ~head:our_head_int ~root:live_root
                        ~matching:peer_root_quorum_count
                        ~consensus_proved:root_consensus_proved
                        "ahead_wait_current_head"
                    | H.Probe_current_head ->
                      let* head_responses =
                        deps.query_epoch_root
                          ~epoch_id:our_head
                          ~timeout_seconds:2.0
                      in
                      let head_root_quorum =
                        H.peer_root_quorum
                          ~required:state_attest_required
                          ~local_root:live_root
                          head_responses
                      in
                      let head_root_consensus_proved =
                        deps.root_consensus_quorum
                          ~epoch_id:our_head
                          ~root:live_root
                          head_responses
                      in
                      match H.ahead_current_root_plan head_root_quorum with
                      | H.Current_root_matches ->
                        accept_current_root deps ~head:our_head_int ~root:live_root
                          ~matching:(H.root_quorum_count head_root_quorum)
                          ~consensus_proved:head_root_consensus_proved
                          "ahead_current_root_quorum"
                      | H.Current_root_mismatch { root; count } ->
                        quarantine_peer_root_mismatch deps ~head:our_head_int count;
                        Log.error "catchup"
                          "event = fresh_root_mismatch head = %d live = %s peer = %s count = %d"
                          our_head_int (short_hex8 live_root) (short_hex8 root) count;
                        Lwt.return_unit
                      | H.Wait_current_root_quorum ->
                        begin
                          match repair with
                          | None ->
                            Log.warn "catchup"
                              "event = ahead_wait_current_root_quorum head = %d target = %Ld"
                              our_head_int
                              target_epoch;
                            deps.set_catchup_in_progress false;
                            Lwt.return_unit
                          | Some repair ->
                            let* target_root =
                              prove_target_root
                                deps
                                ~source_head:our_head_int
                                ~target_epoch
                                ~required:state_attest_required
                            in
                            begin
                              match target_root with
                              | Some majority
                                when deps.committed_head_epoch () = our_head_int ->
                                let* repaired =
                                  repair
                                    ~target_epoch
                                    ~target_root:majority.H.root
                                    ~required:state_attest_required
                                    ~current_root_quorum:false
                                in
                                if repaired then Lwt.return_unit
                                else begin
                                  Log.warn "catchup"
                                    "event = ahead_wait_current_root_quorum head = %d target = %Ld"
                                    our_head_int
                                    target_epoch;
                                  deps.set_catchup_in_progress false;
                                  Lwt.return_unit
                                end
                              | Some _ ->
                                Log.trace "catchup"
                                  "event = moving_head phase = fork_proof start_head = %d head_after_query = %d"
                                  our_head_int
                                  (deps.committed_head_epoch ());
                                Lwt.return_unit
                              | None ->
                                Log.warn "catchup"
                                  "event = ahead_wait_current_root_quorum head = %d target = %Ld"
                                  our_head_int
                                  target_epoch;
                                deps.set_catchup_in_progress false;
                                Lwt.return_unit
                            end
                        end
                  end else begin
                    deps.set_state_attested ~head:our_head_int ~root:live_root;
                    match H.ahead_quorum_plan
                            ~head:our_head_int
                            ~ahead_by:n
                            ~streak:(deps.ahead_streak ())
                            ~grace_epochs:cfg.quarantine_ahead_grace_epochs
                            ~drift_tolerance:cfg.quarantine_ahead_drift_tolerance
                            ~streak_threshold:cfg.quarantine_ahead_streak_threshold
                            ~quarantine_active:(deps.quarantine_active ()) with
                    | H.Stay_active { wake_ready } ->
                      Log.warn "catchup"
                        "event = ahead_stay_active ahead = %d head = %d grace = %d tolerance = %d streak = %d"
                        n our_head_int cfg.quarantine_ahead_grace_epochs
                        cfg.quarantine_ahead_drift_tolerance
                        (deps.ahead_streak ());
                      wake_if_ready deps wake_ready
                    | H.Delay_quarantine { wake_ready } ->
                      Log.warn "catchup"
                        "event = ahead_delay_quarantine ahead = %d head = %d streak = %d threshold = %d"
                        n our_head_int (deps.ahead_streak ())
                        cfg.quarantine_ahead_streak_threshold;
                      wake_if_ready deps wake_ready
                    | H.Quarantine_ahead ->
                      deps.mark_quarantine
                        (Printf.sprintf "ahead_of_target_by_%d_streak_%d"
                           n (deps.ahead_streak ()));
                      Lwt.return_unit
                  end
                | C_catchup.Snapshot_required lag ->
                  Log.warn "catchup"
                    "event = signed_snapshot_required head = %d target = %Ld lag = %d"
                    our_head_int
                    target_epoch
                    lag;
                  deps.set_catchup_in_progress false;
                  if our_head_int < 0 || our_head_int = max_int then begin
                    deps.mark_quarantine "snapshot_plan_invalid";
                    Lwt.return_unit
                  end else begin
                    deps.require_sync
                      (Sync_need.root
                         ~epoch:(our_head_int + 1)
                         ~head:our_head_int);
                    Lwt.return_unit
                  end
                | C_catchup.Do_catchup lag ->
                  begin
                    match H.catchup_lag_plan
                            ~lag
                            ~soft_lag:cfg.soft_catchup_max_lag with
                    | H.Lagging_soft lag ->
                      let* () = deps.drain_pending_finalized () in
                      let refreshed_head = deps.committed_head_epoch () in
                      if refreshed_head > our_head_int then begin
                        Log.info "catchup"
                          "event = soft_catchup_local_finalize head_before = %d head_after = %d target = %Ld"
                          our_head_int refreshed_head target_epoch;
                        attempt stale_retries
                      end else begin
                        match peer_root_quorum with
                        | H.Matching_quorum _ when lag = 1 && not already_attested ->
                          Log.info "catchup"
                            "event = soft_lag_current_root lag = %d target = %Ld head = %Ld"
                            lag target_epoch our_head;
                          accept_current_root deps ~head:our_head_int ~root:live_root
                            ~matching:peer_root_quorum_count
                            ~consensus_proved:root_consensus_proved
                            "soft_lag_current_root"
                        | _ ->
                          Log.info "catchup"
                            "event = soft_catchup lag = %d target = %Ld head = %Ld"
                            lag target_epoch our_head;
                          deps.run_catchup_to_target
                            ~target_epoch
                            ~reason:"lagging"
                      end
                    | H.Lagging_quarantine lag ->
                      deps.mark_quarantine (Printf.sprintf "lag_%d" lag);
                      deps.run_catchup_to_target
                        ~target_epoch
                        ~reason:"lagging"
                    | _ -> Lwt.return_unit
                  end
  in
  attempt stale_retries