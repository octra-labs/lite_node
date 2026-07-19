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


module Transaction = Octra_core.Transaction

type deps = {
  ledger : Octra_core.Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  program_trust : Octra_vm.Program_trust.t;
  wallet_addr : string;
  pre_state_hash : string;
  standard_env : unit -> Octra_core.Epoch_exec.env;
  current_epoch : unit -> int;
  max_fhe_per_epoch : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  stealth_inline_verify_allowed : bool;
  fhe_in_epoch_counter : int ref;
  stealth_in_epoch_counter : int ref;
  stealth_defer_count : (string, int) Hashtbl.t;
  deferred_stealth_txs : Transaction.t list ref;
  confirmed_fees : Z.t ref;
  short_addr : string -> string;
  log_rejected : Transaction.t -> string -> string -> unit;
  confirm_tx : Transaction.t -> unit;
  notify_new_account : string -> unit;
  notify_confirmed : Transaction.t -> int -> unit;
  notify_rejected : Transaction.t -> string -> unit;
}

val trace_enc_balance :
  short:(string -> string) ->
  string ->
  string ->
  string ->
  string ->
  unit

val run :
  deps ->
  Transaction.t list ->
  unit Lwt.t