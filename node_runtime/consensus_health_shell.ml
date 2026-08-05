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
  read_local_root_raw : unit -> string Lwt.t;
  committed_epoch_root_raw : int -> string option;
  peer_snapshot : unit -> string;
  drain_pending_finalized : unit -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
  repair_empty_fork :
    target_epoch:int64 ->
    target_root:string ->
    required:int ->
    current_root_quorum:bool ->
    bool Lwt.t;
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
  finality_target_ready : int -> (unit, string) result;
  run_empty : target:int -> root:string -> Octra_core.Fork_head_repair.result Lwt.t;
  rewind_finality : int -> (unit, string) result;
  drop_finality_after : int -> int;
  prune_after_epoch : int -> unit;
  set_current_epoch : int -> unit;
  set_state_attested : head:int -> root:string -> unit;
  set_catchup_in_progress : bool -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  start_height : int64 -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
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
        match deps.finality_target_ready target with
        | Error reason ->
          repair_snapshot deps ("finality_target:" ^ reason)
        | Ok () ->
          let* result = deps.run_empty ~target ~root:target_root in
          match result with
          | Octra_core.Fork_head_repair.Snapshot_required reason ->
            repair_snapshot deps reason
          | Octra_core.Fork_head_repair.Repaired r ->
            begin
              match deps.rewind_finality target with
              | Error reason ->
                repair_snapshot deps ("finality_rewind:" ^ reason)
              | Ok () ->
                let dropped = deps.drop_finality_after target in
                deps.prune_after_epoch target;
                deps.set_current_epoch (target + 1);
                deps.set_state_attested ~head:target ~root:target_root;
                deps.set_catchup_in_progress false;
                deps.clear_quarantine "fork_empty_rollback";
                Log.warn "catchup"
                  "event = fork_empty_rollback target = %d old_head = %d required = %d finality_dropped = %d root = %s"
                  r.target r.old_head required dropped (short_hex8 r.root);
                let* () = deps.start_height (Int64.succ target_epoch) in
                let* () = deps.wake_ready () in
                Lwt.return true
            end
      end

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

let accept_current_root (deps : deps) ~head ~root reason =
  let open Lwt.Syntax in
  let quarantine_active = deps.quarantine_active () in
  let quarantine_reason = deps.quarantine_reason () in
  if
    quarantine_active
    && not (Consensus_runtime_state.root_attestation_recovers quarantine_reason)
  then begin
    deps.clear_state_attested ();
    deps.set_catchup_in_progress false;
    Log.warn "catchup"
      "event = quarantine_preserved reason = %s evidence = %s head = %d"
      quarantine_reason reason head;
    Lwt.return_unit
  end else begin
    deps.set_state_attested ~head ~root;
    deps.set_catchup_in_progress false;
    deps.clear_quarantine reason;
    let* () = deps.drain_pending_finalized () in
    deps.wake_ready ()
  end

let wake_if_ready (deps : deps) wake_ready =
  let open Lwt.Syntax in
  if wake_ready then
    let* () = deps.drain_pending_finalized () in
    deps.wake_ready ()
  else
    Lwt.return_unit

let run ?(stale_retries = 1) cfg deps =
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
                    match H.in_sync_plan ~genesis_attested peer_root_quorum with
                    | H.Peer_root_mismatch { count; _ } ->
                      quarantine_peer_root_mismatch deps ~head:our_head_int count;
                      Lwt.return_unit
                    | H.Accept_genesis ->
                      accept_current_root deps ~head:our_head_int ~root:live_root
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
                        "ahead_wait_current_head"
                    | H.Probe_current_head ->
                      let* head_responses =
                        deps.query_epoch_root
                          ~epoch_id:our_head
                          ~timeout_seconds:2.0
                      in
                      match
                        H.peer_root_quorum
                          ~required:state_attest_required
                          ~local_root:live_root
                          head_responses
                        |> H.ahead_current_root_plan
                      with
                      | H.Current_root_matches ->
                        accept_current_root deps ~head:our_head_int ~root:live_root
                          "ahead_current_root_quorum"
                      | H.Current_root_mismatch { root; count } ->
                        quarantine_peer_root_mismatch deps ~head:our_head_int count;
                        Log.error "catchup"
                          "event = fresh_root_mismatch head = %d live = %s peer = %s count = %d"
                          our_head_int (short_hex8 live_root) (short_hex8 root) count;
                        Lwt.return_unit
                      | H.Probe_target_root ->
                        let* target_responses =
                          deps.query_epoch_root
                            ~epoch_id:target_epoch
                            ~timeout_seconds:5.0
                        in
                        match
                          H.peer_root_with_quorum
                            ~required:state_attest_required
                            target_responses
                        with
                        | Some { H.root = target_root; count = _ } ->
                          let* repaired =
                            deps.repair_empty_fork
                              ~target_epoch
                              ~target_root
                              ~required:state_attest_required
                              ~current_root_quorum:false
                          in
                          if repaired then Lwt.return_unit
                          else begin
                            clear_unattested_current_head deps our_head_int;
                            Lwt.return_unit
                          end
                        | _ ->
                          clear_unattested_current_head deps our_head_int;
                          Lwt.return_unit
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
                  begin
                    match H.snapshot_plan ~lag with
                    | H.Snapshot_fallback lag ->
                      deps.mark_quarantine
                        (Printf.sprintf "snapshot_preferred_lag_%d" lag);
                      deps.run_catchup_to_target
                        ~target_epoch
                        ~reason:"snapshot_fallback"
                    | _ -> Lwt.return_unit
                  end
                | C_catchup.Do_catchup lag ->
                  begin
                    match H.catchup_lag_plan
                            ~lag
                            ~soft_lag:cfg.soft_catchup_max_lag with
                    | H.Lagging_soft lag ->
                      begin
                        match peer_root_quorum with
                        | H.Matching_quorum _ when lag = 1 && not already_attested ->
                          Log.info "catchup"
                            "event = soft_lag_current_root lag = %d target = %Ld head = %Ld"
                            lag target_epoch our_head;
                          accept_current_root deps ~head:our_head_int ~root:live_root
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