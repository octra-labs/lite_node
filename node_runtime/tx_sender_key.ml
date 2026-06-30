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


let resolve ~find_account tx =
  match find_account tx.Octra_core.Transaction.from with
  | Some account ->
    begin
      match account.Octra_core.Ledger.public_key with
      | Some public_key -> Some public_key
      | None -> tx.Octra_core.Transaction.public_key
    end
  | None -> tx.Octra_core.Transaction.public_key