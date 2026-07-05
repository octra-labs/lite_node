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

type batch = {
  sender : string;
  txs : Transaction.t list;
}

type deps = {
  process : Transaction.t list -> unit Lwt.t;
  fatal : sender:string -> exn -> unit;
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

val nonce_mismatch_reason : expected:int -> got:int -> string

val next_nonce : expected:int -> consume:bool -> int

val initial_nonce :
  account_nonce:(string -> int option) ->
  Transaction.t list ->
  int option

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

val fatal_lines :
  short:(string -> string) ->
  epoch_id:int ->
  sender:string ->
  exn ->
  string list

val run : deps -> Transaction.t list -> unit Lwt.t