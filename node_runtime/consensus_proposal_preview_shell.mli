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

val node_backend : Octra_core.Store_irmin.t -> backend

val run :
  deps ->
  ?catch_exn:bool ->
  Consensus_proposal.build_preview_request ->
  (Octra_core.Epoch_exec.exec_result, string) result Lwt.t