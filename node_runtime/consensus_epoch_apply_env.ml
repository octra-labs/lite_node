(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec
module Head_manifest = Octra_core.Head_manifest

type pre_state_effects = {
  ledger_hash : unit -> string Lwt.t;
  cached_head : unit -> Head_manifest.t option;
}

type pre_state = {
  ledger_root : string;
  consensus_root : string;
}

type node_env = {
  chain_id : string;
  current_epoch : unit -> int;
  epoch_ts : int -> float option;
  driver : unit -> Octra_consensus.C_driver.t option;
  validator_fallback : int -> (string * string) list;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
}

let read_pre_state effects =
  let open Lwt.Syntax in
  let* ledger_root = effects.ledger_hash () in
  let consensus_root =
    match effects.cached_head () with
    | Some h -> h.Head_manifest.state_root
    | None -> ledger_root
  in
  Lwt.return { ledger_root; consensus_root }

let validator_pubkeys ~driver ~fallback =
  match driver with
  | Some driver ->
    driver.Octra_consensus.C_driver.engine.Octra_consensus.C_engine.vs.validators
    |> List.map (fun v ->
      v.Octra_consensus.C_types.address,
      Base64.encode_exn v.pubkey)
  | None ->
    fallback ()

let current_validator_pubkeys env =
  validator_pubkeys
    ~driver:(env.driver ())
    ~fallback:(fun () -> env.validator_fallback (env.current_epoch ()))

let standard
    ~chain_id
    ~epoch_id
    ~proposer_addr
    ~validator_pubkeys
    ~prev_state_root
    ~ready_state_root_at
    ~ready_max_lag =
  Epoch_exec.{
    chain_id;
    epoch_id;
    proposer_addr;
    validator_addrs = List.map fst validator_pubkeys;
    validator_pubkeys;
    prev_state_root;
    epoch_ts = float_of_int (epoch_id * 10);
    ready_state_root_at = Some ready_state_root_at;
    ready_max_lag;
  }

let standard_for_proposer env ~proposer_addr ~prev_state_root =
  let epoch_id = env.current_epoch () in
  standard
    ~chain_id:env.chain_id
    ~epoch_id
    ~proposer_addr
    ~validator_pubkeys:(current_validator_pubkeys env)
    ~prev_state_root
    ~ready_state_root_at:env.ready_state_root_at
    ~ready_max_lag:env.ready_max_lag
  |> fun result ->
  match env.epoch_ts epoch_id with
  | Some epoch_ts -> { result with Epoch_exec.epoch_ts = epoch_ts }
  | None -> result