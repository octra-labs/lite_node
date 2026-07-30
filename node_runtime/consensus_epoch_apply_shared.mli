(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module Epoch_exec = Octra_core.Epoch_exec

type process =
  Transaction.t ->
  (Epoch_exec.tx_effect, string * string) result Lwt.t

type deps = {
  process : process;
  confirm : Transaction.t -> unit;
  reject : Transaction.t -> error_type:string -> reason:string -> unit;
  reject_after_fee :
    Transaction.t ->
    fee:Z.t ->
    error_type:string ->
    reason:string ->
    unit;
}

type standard_or_sender_deps = {
  log_shared : tx_count:int -> unit;
  shared : deps;
  sender : Consensus_epoch_apply_sender.deps;
}

type runtime = {
  consensus_mode : bool;
  current_epoch : unit -> int;
  log_shared : tx_count:int -> unit;
  short : string -> string;
  fatal : string -> unit;
  exit : unit -> unit;
  backend : unit -> Epoch_exec.backend;
  env : unit -> Epoch_exec.env;
  max_fhe_per_epoch : int;
  max_stealth_per_epoch : int;
  process_sender : Transaction.t list -> unit Lwt.t;
  confirm_tx : Transaction.t -> unit;
  reject_tx : Transaction.t -> string -> string -> unit;
  notify_confirmed : Transaction.t -> int -> unit;
  notify_rejected : Transaction.t -> string -> unit;
  program_trust : Octra_vm.Program_trust.t;
  legacy_replay :
    epoch:int ->
    address:string ->
    cipher:string ->
    Octra_core.Pvac_legacy_public_replay.decision;
  private_result_policy :
    int ->
    Octra_core.Private_result_policy.t;
  add_rejected_fee : Z.t -> unit;
}

type node_runtime = {
  ledger : Octra_core.Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  program_trust : Octra_vm.Program_trust.t;
  wallet_addr : string;
  pre_state_hash : string;
  standard_env : unit -> Epoch_exec.env;
  current_epoch : unit -> int;
  consensus_mode : bool;
  max_fhe_per_epoch : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  stealth_inline_verify_allowed : bool;
  fhe_in_epoch_counter : int ref;
  stealth_in_epoch_counter : int ref;
  stealth_defer_count : (string, int) Hashtbl.t;
  pending_tx_saves : (Transaction.t * int) list ref;
  total_tx_count : int ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
  short : string -> string;
  log_shared : tx_count:int -> unit;
  fatal : string -> unit;
  exit : unit -> unit;
  notify_new_account : string -> unit;
  notify_confirmed : Transaction.t -> int -> unit;
  notify_rejected : Transaction.t -> string -> unit;
  legacy_replay :
    epoch:int ->
    address:string ->
    cipher:string ->
    Octra_core.Pvac_legacy_public_replay.decision;
  private_result_policy :
    int ->
    Octra_core.Private_result_policy.t;
}

type node_result = {
  deferred_stealth_txs : Transaction.t list ref;
}

val is_shared_bft_tx : Transaction.t -> bool

val all_shared_bft : Transaction.t list -> bool

val canonical_order : Transaction.t list -> Transaction.t list

val process_standard :
  backend:Epoch_exec.backend ->
  env:Epoch_exec.env ->
  Transaction.t ->
  (unit, string * string) result Lwt.t

val run : deps -> Transaction.t list -> unit Lwt.t

val run_standard_or_sender :
  consensus_mode:bool ->
  standard_or_sender_deps ->
  Transaction.t list ->
  unit Lwt.t

val runtime_deps :
  ?preverify:Octra_core.Preverify_commit.t ->
  ?save_receipt_raw:(tx_hash:string -> json:string -> unit) ->
  runtime ->
  standard_or_sender_deps

val run_runtime :
  ?preverify:Octra_core.Preverify_commit.t ->
  ?save_receipt_raw:(tx_hash:string -> json:string -> unit) ->
  runtime ->
  Transaction.t list ->
  unit Lwt.t

val run_node :
  ?preverify:Octra_core.Preverify_commit.t ->
  node_runtime ->
  Transaction.t list ->
  node_result Lwt.t