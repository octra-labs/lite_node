(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction

type deps = {
  ledger : Octra_core.Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  program_trust : Octra_vm.Program_trust.t;
  wallet_addr : string;
  pre_state_hash : string;
  fold : int -> (Octra_core.Epoch_exec.fold_ctx, string) result;
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
  legacy_replay :
    epoch:int ->
    address:string ->
    cipher:string ->
    Octra_core.Pvac_legacy_public_replay.decision;
  private_field_policy : Octra_core.Private_ledger.field_policy;
  private_result_policy :
    int ->
    Octra_core.Private_result_policy.t;
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