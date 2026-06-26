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


val account_of_params :
  Yojson.Safe.t ->
  (string -> Octra_core.Ledger.account option) ->
  (string * Octra_core.Ledger.account, Octra_core.Rpc.rpc_error) result

val balance :
  Yojson.Safe.t ->
  find_account:(string -> Octra_core.Ledger.account option) ->
  pending_nonce:(string -> int -> int) ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val nonce :
  Yojson.Safe.t ->
  find_account:(string -> Octra_core.Ledger.account option) ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val public_key :
  Yojson.Safe.t ->
  find_account:(string -> Octra_core.Ledger.account option) ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val validate_address :
  Yojson.Safe.t ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val supply :
  true_total:Z.t ->
  encrypted:Z.t ->
  max_supply:Z.t ->
  Yojson.Safe.t

val total_transactions :
  confirmed:int ->
  staging:int ->
  Yojson.Safe.t