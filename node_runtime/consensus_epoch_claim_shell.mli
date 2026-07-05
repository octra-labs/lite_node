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


module Private_ledger = Octra_core.Private_ledger
module Transaction = Octra_core.Transaction

type deps = {
  fee : Z.t;
  nonce : int;
  claim_plan : unit -> (Private_ledger.claim_plan, Private_ledger.failure) result Lwt.t;
  debit : Z.t -> int -> (unit, string) result;
  balance_plan :
    Private_ledger.claim_plan ->
    (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  trace : Private_ledger.balance_plan -> unit;
  update : string -> (unit, string) result;
  tx_hash : unit -> string;
  mark : int -> string -> (unit, string) result Lwt.t;
  log_accept : int -> unit;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type tx_deps = {
  claim_plan :
    Transaction.t ->
    (Private_ledger.claim_plan, Private_ledger.failure) result Lwt.t;
  debit : Transaction.t -> Z.t -> int -> (unit, string) result;
  balance_plan :
    Transaction.t ->
    Private_ledger.claim_plan ->
    (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  trace : Transaction.t -> Private_ledger.balance_plan -> unit;
  update : Transaction.t -> string -> (unit, string) result;
  mark : int -> string -> (unit, string) result Lwt.t;
  log_accept : Transaction.t -> int -> unit;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

val run : deps -> unit Lwt.t
val run_tx : tx_deps -> Transaction.t -> unit Lwt.t
val log_accept : string -> int -> unit