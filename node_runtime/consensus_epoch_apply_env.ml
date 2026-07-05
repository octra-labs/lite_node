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


module Epoch_exec = Octra_core.Epoch_exec

let validator_pubkeys ~driver ~fallback =
  match driver with
  | Some driver ->
    driver.Octra_consensus.C_driver.engine.Octra_consensus.C_engine.vs.validators
    |> List.map (fun v ->
      v.Octra_consensus.C_types.address,
      Base64.encode_exn v.pubkey)
  | None ->
    fallback ()

let standard
    ~epoch_id
    ~proposer_addr
    ~validator_pubkeys
    ~prev_state_root
    ~ready_state_root_at
    ~ready_max_lag =
  Epoch_exec.{
    chain_id = "";
    epoch_id;
    proposer_addr;
    validator_addrs = List.map fst validator_pubkeys;
    validator_pubkeys;
    prev_state_root;
    epoch_ts = float_of_int (epoch_id * 10);
    ready_state_root_at = Some ready_state_root_at;
    ready_max_lag;
  }