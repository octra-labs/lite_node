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


module Private_gate = Consensus_epoch_apply_private_gate
module Private_ledger = Octra_core.Private_ledger
module Transaction = Octra_core.Transaction

type deps = {
  gate : unit -> Private_gate.reject option;
  apply : unit -> Private_ledger.key_switch_outcome Lwt.t;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  reject_key_switch :
    event:string ->
    Private_ledger.key_switch_rejection ->
    unit Lwt.t;
  log_applied : Private_ledger.key_switch_apply -> unit;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

type tx_deps = {
  gate : unit -> Private_gate.reject option;
  needs_legacy_audit : Transaction.t -> bool;
  legacy_replay : string -> Octra_core.Pvac_legacy_public_replay.decision;
  apply_tx :
    ?legacy_public_replay:Octra_core.Pvac_legacy_public_replay.decision ->
    Transaction.t ->
    Private_ledger.key_switch_outcome Lwt.t;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  reject_key_switch :
    event:string ->
    Private_ledger.key_switch_rejection ->
    unit Lwt.t;
  log_applied : Private_ledger.key_switch_apply -> unit;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

type live_tx_args = {
  gate : unit -> Private_gate.reject option;
  needs_legacy_audit : Transaction.t -> bool;
  legacy_replay : string -> Octra_core.Pvac_legacy_public_replay.decision;
  apply_tx :
    ?legacy_public_replay:Octra_core.Pvac_legacy_public_replay.decision ->
    Transaction.t ->
    Private_ledger.key_switch_outcome Lwt.t;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  record_rejected : Transaction.t -> string -> string -> unit;
  continue_after_reject : consume_nonce:bool -> unit Lwt.t;
  short_addr : string -> string;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

type live_ledger_tx_args = {
  ledger : Octra_core.Ledger.t;
  chaindata : Octra_core.Store_chaindata.t;
  gate : unit -> Private_gate.reject option;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  record_rejected : Transaction.t -> string -> string -> unit;
  continue_after_reject : consume_nonce:bool -> unit Lwt.t;
  short_addr : string -> string;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

val run : deps -> unit Lwt.t

val run_tx : tx_deps -> Transaction.t -> unit Lwt.t

val live_tx_deps : live_tx_args -> Transaction.t -> tx_deps

val run_live_tx : live_tx_args -> Transaction.t -> unit Lwt.t

val run_live_ledger_tx :
  live_ledger_tx_args ->
  Transaction.t ->
  unit Lwt.t