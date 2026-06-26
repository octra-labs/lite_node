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


type verdict =
  | Accept
  | Invalid_address
  | Timestamp_drift of float
  | Invalid_signature

let to_valid tx =
  match tx.Octra_core.Transaction.op_type with
  | Octra_core.Transaction.StealthOp ->
    String.equal tx.Octra_core.Transaction.to_ "stealth"
  | Octra_core.Transaction.MultiExec ->
    String.equal tx.Octra_core.Transaction.to_ "multi_exec"
  | _ -> Octra_core.Crypto.Address.is_valid_address tx.Octra_core.Transaction.to_

let addr_valid tx =
  Octra_core.Crypto.Address.is_valid_address tx.Octra_core.Transaction.from
  && to_valid tx

let sig_valid tx = function
  | None -> false
  | Some pk ->
    Octra_core.Crypto.Address.verify_address_pubkey tx.Octra_core.Transaction.from pk
    && Octra_core.Transaction.verify tx pk

let admit ~now ~max_drift ~sender_pk tx =
  if not (addr_valid tx) then Invalid_address
  else
    let drift = Float.abs (tx.Octra_core.Transaction.timestamp -. now) in
    if drift > max_drift then Timestamp_drift drift
    else if not (sig_valid tx sender_pk) then Invalid_signature
    else Accept