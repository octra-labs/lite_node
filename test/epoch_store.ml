(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Epoch_replay
module X = Octra_core.Epoch_exec
module S = Octra_core.Store_irmin
module D = Octra_core.Store_chaindata
module G = Octra_core.Rule_graph
module N = Octra_node_runtime

type context = {
  store : S.t;
  ledger : Octra_core.Ledger.t;
  chaindata : D.t;
  rules : G.t;
  trust : Octra_vm.Program_trust.t;
  chain_id : string;
  ready_root : int -> string option Lwt.t;
  legacy_replay :
    epoch:int -> address:string -> cipher:string ->
    Octra_core.Pvac_legacy_public_replay.decision;
  result_policy : int -> Octra_core.Private_result_policy.t;
}

let network ~getenv ~chain_id ~config_hash ~configured_hash =
  let ( let* ) = Result.bind in
  let require reason value = if value then Ok () else Error reason in
  let* () = N.Consensus_profile.validate getenv in
  let* () = require "replay chain configuration differs"
    (getenv "OCTRA_CHAIN_ID" = Some chain_id) in
  let* () = require "replay configured identity differs from checkpoint"
    (configured_hash = config_hash) in
  let module Trust = Octra_vm.Program_trust in
  let* trust = Trust.of_env getenv |> Result.map_error Trust.error_message in
  let actual = Octra_consensus.C_config.network_hash ~chain_id
    ?program_trust_hash:(Trust.config_hash trust)
    ~runtime_profile_hash:(N.Consensus_profile.compat_hash getenv) ()
    |> Octra_bootstrap.State_sync_checkpoint.raw_to_hex
  in
  let* () = require "replay network identity differs from checkpoint"
    (actual = config_hash) in
  Ok trust

let persist_index context (cursor : R.J.cursor) (prepared : R.J.prepared)
    (result : X.exec_result) =
  R.artifacts prepared.txs result;
  let hashes = R.hashes (List.map fst result.artifacts.confirmed) in
  let epoch_hash, root =
    R.E.next_root_from_hashes_i64
      ~prev:cursor.eic
      ~epoch_id:prepared.record.epoch_id
      ~start_txid:cursor.txid
      hashes
  in
  R.require "replay applied consensus root differs"
    (R.E.folded_state_root
       ~ledger_state_root:result.post_state_root
       ~epoch_index_root:root = prepared.record.state_root);
  R.require "replay chaindata batch is active"
    (Option.is_none context.chaindata.index.batch);
  D.begin_batch context.chaindata;
  begin
    try
      D.set_epoch_index_commitment context.chaindata
        ~epoch_id:prepared.epoch_int ~epoch_hash ~root;
      D.commit_batch context.chaindata
    with error ->
      D.abort_batch context.chaindata;
      raise error
  end;
  match D.get_epoch_index_commitment context.chaindata prepared.epoch_int with
  | Some stored_hash, Some index_root
    when stored_hash = epoch_hash && index_root = root ->
    R.{ result; index_root }
  | _ -> failwith "replay committed epoch index differs"

let deps (context : context) ~(cursor : R.J.cursor)
    ~(prepared : R.J.prepared) : R.deps =
  let open Lwt.Syntax in
  let record = prepared.record in
  let epoch = prepared.epoch_int in
  R.require "replay epoch differs"
    (Int64.of_int epoch = record.epoch_id && cursor.epoch = record.epoch_id);
  R.require "replay chain differs"
    (record.finality.finalize.chain_id = context.chain_id);
  let parent_commit = record.finality.finalize.parent_commit in
  let fold =
    R.get (N.Set_rule.bind context.rules ~chain_id:context.chain_id
      ~parent:parent_commit ~epoch)
  in
  let proof_mode = (R.get (fold epoch)).X.standard_mode in
  let rule select =
    select context.rules ~epoch |> Result.map_error G.fault_message |> R.get
  in
  let circle_mode = rule G.circle in
  let wasm_compute_mode = rule G.wasm_compute in
  let object_cost = rule G.object_cost = G.Active in
  let owner_migration_mode = rule G.owner_migration in
  let field_policy =
    Octra_core.Private_ledger.field_policy_of_mode (rule G.private_payload)
  in
  let key_pool =
    N.Consensus_key_switch_preverify.create
      ~field_policy:(fun () -> field_policy)
      ~strict:(fun () -> proof_mode = G.Active)
      context.ledger
  in
  let private_pool =
    N.Consensus_private_preverify.create
      ~field_policy:(fun () -> field_policy)
      ~strict:(fun () -> proof_mode = G.Active)
      ~result_policy:(fun () -> context.result_policy epoch)
      context.ledger
  in
  let limits =
    N.Startup_runtime_limits.private_limits {
      int_value = N.Env.int_value;
      opt = Sys.getenv_opt;
    }
  in
  let max_fhe = limits.max_fhe_per_epoch in
  let max_stealth = limits.max_stealth_per_epoch in
  let validator_pubkeys =
    List.map (fun (validator : Octra_consensus.C_types.validator_info) ->
      validator.address, Base64.encode_exn validator.pubkey)
      record.finality.validator_set.validators
  in
  let env =
    N.Consensus_proposal.epoch_exec_env
      ~chain_id:context.chain_id
      ~epoch_id:record.epoch_id
      ~epoch_ts:record.epoch_ts
      ~proposer:record.creator_addr
      ~validator_pubkeys
      ~prev_state_root:record.prev_state_root
      ~ready_state_root_at:context.ready_root
      ~ready_max_lag:
        (max 0 (N.Env.int_value "OCTRA_VALIDATOR_READY_MAX_LAG_EPOCHS" 64))
  in
  let head () =
    let* root = S.get_head_hash context.store in
    match root with
    | Some root -> Lwt.return root
    | None -> Lwt.fail_with "replay store head is missing"
  in
  let preview =
    N.Consensus_proposal_preview_shell.node_backend
      ~private_artifacts:(N.Consensus_private_preverify.artifacts private_pool)
      ~key_artifacts:(N.Consensus_key_switch_preverify.artifacts key_pool)
      ~program_trust:context.trust
      ~rules:context.rules
      ~legacy_replay:context.legacy_replay
      ~private_result_policy:context.result_policy
      ~max_fhe
      ~max_stealth
      context.store context.ledger
  in
  {
    head;
    preverify = (fun txs ->
      let* root = head () in
      let* batch =
        Octra_core.State_preview.with_state
          ~base_store:context.store
          ~base_ledger:context.ledger
          ~epoch_id:epoch
          ~proposal_id:"replay-preverify"
          ~expected_prev_root:root
          (fun store ledger ->
            let preverify_env ~pre_state_root =
              { env with X.epoch_ts = 0.; prev_state_root = pre_state_root }
            in
            let circle = N.Consensus_circle_preverify.{
              store;
              ledger;
              program_trust = context.trust;
              rules = context.rules;
              env = preverify_env;
            } in
            let cell = N.Consensus_circle_cell_preverify.{
              store;
              ledger;
              rules = context.rules;
              env = preverify_env;
            } in
            let* batch =
              R.W.run_many
                ~prepared:(fun tx ->
                  if tx.R.T.op_type = R.T.KeySwitch then
                    N.Consensus_key_switch_preverify.await key_pool tx
                  else N.Consensus_private_preverify.await private_pool tx)
                ~field_policy
                ~strict:(proof_mode = G.Active)
                ~ledger
                ~result_policy:(context.result_policy epoch)
                ~circle_preverify:(N.Consensus_circle_preverify.run circle)
                ~circle_cell_preverify:(N.Consensus_circle_cell_preverify.run cell)
                ~legacy_replay:(context.legacy_replay ~epoch)
                txs
            in
            Lwt.return_ok batch)
      in
      Lwt.return (R.get batch));
    preview = (fun preverify txs ->
      let* root = head () in
      preview.run
        ~epoch_id:epoch
        ~proposal_id:"replay-preview"
        ~expected_prev_root:(Some root)
        ~preverify
        ~parent_commit
        ~reward:prepared.reward
        ~env
        ~txs);
    apply = (fun preverify current ->
      let preverify =
        Octra_core.Preverify_commit.with_artifacts
          (N.Consensus_private_preverify.artifacts private_pool current.R.J.txs)
          preverify
        |> Octra_core.Preverify_commit.with_keys
             (N.Consensus_key_switch_preverify.artifacts key_pool current.R.J.txs)
      in
      R.require "replay apply preparation differs" (current = prepared);
      R.require "replay chaindata is read-only"
        (not context.chaindata.index.readonly);
      R.require "replay chaindata batch is active"
        (Option.is_none context.chaindata.index.batch);
      R.require "replay epoch index already exists"
        (D.get_epoch_index_commitment context.chaindata epoch = (None, None));
      List.iter (fun (tx : R.T.t) ->
        if not (R.T.bft_consensus_admits_op tx.op_type) then
          failwith (R.T.bft_reject_reason tx.op_type)) prepared.txs;
      let backend =
        X.make_live_backend ~proof_mode ~fold context.store context.ledger
      in
      let private_transition =
        Octra_core.Private_transition.create
          ~preverify:(Some preverify)
          ~legacy_replay:context.legacy_replay
          ~ledger:context.ledger
          ~epoch_id:epoch
          ~owner_migration_mode
          ~proof_mode
          ~field_policy
          ~result_policy:(context.result_policy epoch)
          ~limits:{ max_fhe; max_stealth }
      in
      let process_tx ~backend ~env (tx : R.T.t) =
        if R.T.bft_crypto_active () && R.T.bft_crypto_op tx.op_type then
          let* result =
            Octra_core.Private_transition.process private_transition
              ~backend ~env tx
          in
          Lwt.return (Result.map (fun fee -> X.Confirmed fee) result)
        else
          N.Consensus_vm_transition.process_tx
            ~preverify
            ~backend
            ~env
            ~circle_mode
            ~wasm_compute_mode
            ~program_trust:context.trust
            ~object_cost
            tx
      in
      let* result =
        X.run_transition_rewarded
          ~reward:prepared.reward
          ~preverify
          ~backend
          ~env
          ~txs:prepared.txs
          ~process_tx
      in
      Lwt.return (persist_index context cursor prepared (R.get result)));
  }