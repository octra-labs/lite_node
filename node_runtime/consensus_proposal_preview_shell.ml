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


type backend = {
  run :
    epoch_id:int ->
    proposal_id:string ->
    expected_prev_root:string option ->
    preverify:Octra_core.Preverify_commit.t ->
    env:Octra_core.Epoch_exec.env ->
    txs:Octra_core.Transaction.t list ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
}

type deps = {
  chain_id : string;
  backend : backend;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  warn : string -> unit;
}

let node_backend store =
  {
    run = (fun ~epoch_id ~proposal_id ~expected_prev_root ~preverify ~env ~txs ->
      Octra_core.State_preview.with_preview
        ~base_store:store
        ~epoch_id
        ~proposal_id
        ?expected_prev_root
        (fun backend ->
          Octra_core.Epoch_exec.run_checked
            ~preverify
            ~backend
            ~env
            ~txs
            ~process_tx:Octra_core.Epoch_exec.process_standard_tx));
  }

let run (deps : deps) =
  Consensus_driver_wiring.node_proposal_preview
    Consensus_driver_wiring.{
      chain_id = deps.chain_id;
      ready_state_root_at = deps.ready_state_root_at;
      ready_max_lag = deps.ready_max_lag;
      warn = deps.warn;
      run_preview = (fun request ~env ->
        deps.backend.run
          ~epoch_id:(Int64.to_int request.Consensus_proposal.epoch_id)
          ~proposal_id:request.proposal_id
          ~expected_prev_root:(Some request.expected_prev_root)
          ~preverify:request.preverify
          ~env
          ~txs:request.txs);
    }