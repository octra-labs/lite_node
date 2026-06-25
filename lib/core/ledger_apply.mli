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


type accepted_kind =
  | Applied_standard
  | Applied_op01_burn

type accepted = {
  created_account : string option;
  kind : accepted_kind;
}

type rejection = {
  created_account : string option;
  tag : string;
  reason : string;
  notify_reason : string;
}

type outcome =
  | Accepted of accepted
  | Rejected of rejection

val apply_epoch_public :
  Ledger.t ->
  Transaction.t ->
  outcome