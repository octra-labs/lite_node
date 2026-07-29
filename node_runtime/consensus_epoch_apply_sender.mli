(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction

type batch = {
  sender : string;
  txs : Transaction.t list;
}

type deps = {
  process : Transaction.t list -> unit Lwt.t;
  fatal : sender:string -> exn -> unit;
}

type tx_context = {
  tx : Transaction.t;
  confirm : unit -> unit Lwt.t;
  reject :
    ?consume_nonce:bool ->
    ?notify_reason:string ->
    ?persist_state:bool ->
    string ->
    string ->
    unit Lwt.t;
  continue_after_reject : consume_nonce:bool -> unit Lwt.t;
}

type rejected_record = {
  hash : string;
  from_addr : string;
  to_addr : string;
  amount : string;
  nonce : int;
  error_type : string;
  reason : string;
  epoch_id : int;
  ts : float;
}

type confirmed_record = {
  hash : string;
  tx_json : string;
  from_addr : string;
  to_addr : string;
  op_type : string;
  encrypted_data : string;
  message : string;
  fee : Z.t;
}

type tx_sink_refs = {
  pending_tx_saves : (Transaction.t * int) list ref;
  total_tx_count : int ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
}

type tx_sink_effects = {
  now : unit -> float;
  epoch : unit -> int;
  save_rejected : rejected_record -> unit;
  save_tx : confirmed_record -> epoch_id:int -> unit;
  record_tx : Transaction.t -> unit;
  warn_rejected : string -> unit;
}

type tx_sink = {
  reject : Transaction.t -> string -> string -> unit;
  confirm : Transaction.t -> unit;
}

type live_tx_sink_deps = {
  chaindata : Octra_core.Store_chaindata.t;
  current_epoch : unit -> int;
  pending_tx_saves : (Transaction.t * int) list ref;
  total_tx_count : int ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
}

type ('backend, 'env) epoch_exec_deps = {
  backend : unit -> 'backend;
  standard_env : unit -> 'env;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type public_deps = {
  apply : Transaction.t -> Octra_core.Ledger_apply.outcome;
  notify_created : string -> unit;
  reject : notify_reason:string -> string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
  log_burn : Transaction.t -> unit;
}

val live_epoch_exec_deps :
  backend:(unit -> 'backend) ->
  standard_env:(unit -> 'env) ->
  reject:(string -> string -> unit Lwt.t) ->
  confirm:(unit -> unit Lwt.t) ->
  ('backend, 'env) epoch_exec_deps

val live_public_deps :
  apply:(Transaction.t -> Octra_core.Ledger_apply.outcome) ->
  notify_created:(string -> unit) ->
  reject:(notify_reason:string -> string -> string -> unit Lwt.t) ->
  confirm:(unit -> unit Lwt.t) ->
  short:(string -> string) ->
  public_deps

val nonce_order : Transaction.t list -> Transaction.t list

val group : Transaction.t list -> batch list

val short_hash : string -> string

val rejected_line :
  hash:string ->
  error_type:string ->
  reason:string ->
  string

val rejected_record :
  epoch_id:int ->
  ts:float ->
  error_type:string ->
  reason:string ->
  Transaction.t ->
  rejected_record

val confirmed_record :
  Transaction.t ->
  confirmed_record

val make_tx_sink :
  tx_sink_refs ->
  tx_sink_effects ->
  tx_sink

val live_tx_sink :
  live_tx_sink_deps ->
  tx_sink

val nonce_mismatch_reason : expected:int -> got:int -> string

val next_nonce : expected:int -> consume:bool -> int

val savepoint_result :
  accepted:bool ->
  persist_state:bool ->
  (unit, unit) result

val initial_nonce :
  account_nonce:(string -> int option) ->
  Transaction.t list ->
  int option

val run_nonce_loop :
  account_nonce:(string -> int option) ->
  nonce_mismatch:(Transaction.t -> expected:int -> got:int -> unit Lwt.t) ->
  confirm:(Transaction.t -> unit Lwt.t) ->
  reject:(
    Transaction.t ->
    notify_reason:string option ->
    error_type:string ->
    reason:string ->
    unit Lwt.t) ->
  handle:(tx_context -> unit Lwt.t) ->
  Transaction.t list ->
  unit Lwt.t

val handle_public_result :
  notify_created:(string -> unit) ->
  reject:(notify_reason:string -> string -> string -> unit Lwt.t) ->
  confirm:(unit -> unit Lwt.t) ->
  log_burn:(unit -> unit) ->
  Octra_core.Ledger_apply.outcome ->
  unit Lwt.t

val log_op01_burn :
  short:(string -> string) ->
  Transaction.t ->
  unit

val run_public_tx :
  public_deps ->
  Transaction.t ->
  unit Lwt.t

val handle_epoch_exec_result :
  reject:(string -> string -> unit Lwt.t) ->
  confirm:(unit -> unit Lwt.t) ->
  ('a, string * string) result ->
  unit Lwt.t

val run_epoch_exec :
  process:(unit -> ('a, string * string) result Lwt.t) ->
  reject:(string -> string -> unit Lwt.t) ->
  confirm:(unit -> unit Lwt.t) ->
  unit Lwt.t

val run_circle_epoch_exec :
  ('backend, 'env) epoch_exec_deps ->
  process:(
    backend:'backend ->
    current_epoch:int ->
    Transaction.t ->
    ('a, string * string) result Lwt.t) ->
  current_epoch:int ->
  Transaction.t ->
  unit Lwt.t

val run_standard_epoch_exec :
  ('backend, 'env) epoch_exec_deps ->
  process:(
    backend:'backend ->
    env:'env ->
    Transaction.t ->
    ('a, string * string) result Lwt.t) ->
  Transaction.t ->
  unit Lwt.t

val reject_contract_upgrade :
  reject:(notify_reason:string -> string -> string -> unit Lwt.t) ->
  unit Lwt.t

val fatal_lines :
  short:(string -> string) ->
  epoch_id:int ->
  sender:string ->
  exn ->
  string list

val run : deps -> Transaction.t list -> unit Lwt.t