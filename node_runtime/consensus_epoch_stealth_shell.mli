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
  fee : Z.t;
  nonce : int;
  stealth_count : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  inline_verify_allowed : bool;
  gate_reject : unit -> Private_gate.reject option;
  tx_hash : unit -> string;
  short_hash : string -> string;
  defer_count : string -> int;
  set_defer_count : string -> int -> unit;
  clear_defer_count : string -> unit;
  preverify_state : string -> Preverify_cache.state;
  preverify_remove : string -> unit;
  preverify_ready :
    string ->
    sender_enc_snapshot:string ->
    Preverify_cache.result option;
  log_cap_defer : count:int -> max:int -> tx:string -> unit;
  log_defer : count:int -> max:int -> status:string -> tx:string -> unit;
  log_cache_hit : unit -> unit;
  log_cache_miss_verify : unit -> unit;
  log_cache_miss_disabled : tx:string -> unit;
  defer : unit -> unit Lwt.t;
  reject : string -> string -> unit Lwt.t;
  plan : unit -> (Private_ledger.stealth_plan, Private_ledger.failure) result Lwt.t;
  trace : Private_ledger.stealth_plan -> unit;
  inline_range :
    Private_ledger.stealth_plan ->
    (Private_ledger.stealth_range, Private_ledger.failure) result Lwt.t;
  accept_range : Private_ledger.stealth_range -> (unit, Private_ledger.failure) result;
  binding :
    Private_ledger.stealth_plan ->
    (unit, Private_ledger.failure) result Lwt.t;
  debit : Z.t -> int -> (unit, string) result;
  update : string -> (unit, string) result;
  create_output :
    tx_hash:string ->
    Private_ledger.stealth_plan ->
    (int64, string) result Lwt.t;
  accept : int64 -> Private_ledger.stealth_plan -> unit Lwt.t;
}

type tx_deps = {
  stealth_count : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  inline_verify_allowed : bool;
  gate_reject : Transaction.t -> Private_gate.reject option;
  short_hash : string -> string;
  defer_count : string -> int;
  set_defer_count : string -> int -> unit;
  clear_defer_count : string -> unit;
  preverify_state : string -> Preverify_cache.state;
  preverify_remove : string -> unit;
  preverify_ready :
    string ->
    sender_enc_snapshot:string ->
    Preverify_cache.result option;
  log_cap_defer : count:int -> max:int -> tx:string -> unit;
  log_defer : count:int -> max:int -> status:string -> tx:string -> unit;
  log_cache_hit : unit -> unit;
  log_cache_miss_verify : unit -> unit;
  log_cache_miss_disabled : tx:string -> unit;
  defer_tx : Transaction.t -> unit Lwt.t;
  reject : string -> string -> unit Lwt.t;
  plan :
    Transaction.t ->
    (Private_ledger.stealth_plan, Private_ledger.failure) result Lwt.t;
  trace : Transaction.t -> Private_ledger.stealth_plan -> unit;
  inline_range :
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (Private_ledger.stealth_range, Private_ledger.failure) result Lwt.t;
  accept_range : Private_ledger.stealth_range -> (unit, Private_ledger.failure) result;
  binding :
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (unit, Private_ledger.failure) result Lwt.t;
  debit : Transaction.t -> Z.t -> int -> (unit, string) result;
  update : Transaction.t -> string -> (unit, string) result;
  create_output :
    tx_hash:string ->
    Transaction.t ->
    Private_ledger.stealth_plan ->
    (int64, string) result Lwt.t;
  accept :
    Transaction.t ->
    int64 ->
    Private_ledger.stealth_plan ->
    unit Lwt.t;
}

type gate_deps = {
  expected_kat : unit -> string;
  stored_kat : string -> string option;
  set_kat : string -> string -> unit;
  log_backfill : string -> unit;
  debit_gate : unit -> Private_gate.reject option;
  fhe_gate : unit -> Private_gate.reject option;
}

val run : deps -> unit Lwt.t
val run_tx : tx_deps -> Transaction.t -> unit Lwt.t
val gate_reject : gate_deps -> Transaction.t -> Private_gate.reject option
val log_cap_defer : count:int -> max:int -> tx:string -> unit
val log_defer : count:int -> max:int -> status:string -> tx:string -> unit
val log_cache_hit : unit -> unit
val log_cache_miss_verify : unit -> unit
val log_cache_miss_disabled : tx:string -> unit