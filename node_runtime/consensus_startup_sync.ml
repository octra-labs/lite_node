(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  env : string -> string option;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply_replay : Consensus_replay.plan -> unit Lwt.t;
  apply_join : Consensus_join_rpc.apply;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  require_sync : Sync_need.t -> unit;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

type apply_finalized =
  ?override_ordered_txs:Octra_core.Transaction.t list ->
  ?override_receipts_json:string list ->
  ?override_proposer_info:Octra_core.Epochlog.proposer_info ->
  ?override_reward:Consensus_reward_attribution.t ->
  ?override_epoch_ts:float ->
  ?override_parent_commit:Octra_consensus.C_types.parent_commit ->
  now:float ->
  elapsed:float ->
  unit ->
  unit Lwt.t

type apply_callbacks = {
  replay : Consensus_replay.plan -> unit Lwt.t;
  join :
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    proposer_info:Octra_core.Epochlog.proposer_info option ->
    reward:Consensus_reward_attribution.t ->
    epoch_ts:float ->
    parent_commit:Octra_consensus.C_types.parent_commit option ->
    unit Lwt.t;
}

type node_deps = {
  env : string -> string option;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply_finalized : apply_finalized;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  require_sync : Sync_need.t -> unit;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

let apply_callbacks ~now (apply : apply_finalized) =
  {
    replay = (fun replay ->
      apply
        ~override_ordered_txs:replay.Consensus_replay.txs
        ?override_proposer_info:replay.proposer_info
        ?override_parent_commit:replay.finalize.parent_commit
        ~now:(now ())
        ~elapsed:0.0
        ());
    join = (fun ~txs ~receipts_json ~proposer_info ~reward ~epoch_ts
        ~parent_commit ->
      apply
        ~override_ordered_txs:txs
        ~override_receipts_json:receipts_json
        ?override_proposer_info:proposer_info
        ~override_reward:reward
        ~override_epoch_ts:epoch_ts
        ?override_parent_commit:parent_commit
        ~now:(now ())
        ~elapsed:0.0
        ());
  }

let node_deps runtime =
  let apply = apply_callbacks ~now:runtime.now runtime.apply_finalized in
  {
    env = runtime.env;
    expected_validator_set_hash = runtime.expected_validator_set_hash;
    fetch_json = runtime.fetch_json;
    current_epoch = runtime.current_epoch;
    head = runtime.head;
    next_txid = runtime.next_txid;
    finality = runtime.finality;
    set_proposal = runtime.set_proposal;
    write_entry = runtime.write_entry;
    apply_replay = apply.replay;
    apply_join = apply.join;
    sleep = runtime.sleep;
    now = runtime.now;
    data_dir = runtime.data_dir;
    consensus_role = runtime.consensus_role;
    chain_id = runtime.chain_id;
    validator = runtime.validator;
    validator_pubkey = runtime.validator_pubkey;
    priv_b64 = runtime.priv_b64;
    require_sync = runtime.require_sync;
    exit_error = runtime.exit_error;
    exit_success = runtime.exit_success;
  }

let replay_deps (deps : deps) =
  Consensus_replay.node_one_shot_deps {
    current_epoch = deps.current_epoch;
    finality = deps.finality;
    set_proposal = deps.set_proposal;
    write_entry = deps.write_entry;
    apply = deps.apply_replay;
    exit_error = deps.exit_error;
    exit_success = deps.exit_success;
  }

let join_wiring (deps : deps) =
  Consensus_join_rpc.{
    env = deps.env;
    expected_validator_set_hash = deps.expected_validator_set_hash;
    fetch_json = deps.fetch_json;
    current_epoch = deps.current_epoch;
    head = deps.head;
    next_txid = deps.next_txid;
    finality = deps.finality;
    write_entry = deps.write_entry;
    apply = deps.apply_join;
    sleep = deps.sleep;
    now = deps.now;
    data_dir = deps.data_dir;
    consensus_role = deps.consensus_role;
    chain_id = deps.chain_id;
    validator = deps.validator;
    validator_pubkey = deps.validator_pubkey;
    priv_b64 = deps.priv_b64;
    require_sync = deps.require_sync;
  }

let run_replay (deps : deps) =
  Consensus_replay.run_one_shot
    ~env:deps.env
    ~default_chain_id:deps.chain_id
    (replay_deps deps)

let run_join (deps : deps) =
  Consensus_join_rpc.run_configured_node_wiring
    (join_wiring deps)

let run (deps : deps) =
  let open Lwt.Syntax in
  let* () = run_replay deps in
  run_join deps

let run_node runtime =
  run (node_deps runtime)