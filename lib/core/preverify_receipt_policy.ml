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


let activation_epoch () =
  match Sys.getenv_opt "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH" with
  | Some raw ->
    (try int_of_string raw with _ -> max_int)
  | None -> max_int

let active ~epoch_id =
  epoch_id >= activation_epoch ()

let should_check ~epoch_id ~receipts =
  receipts <> [] || active ~epoch_id

let check ~epoch_id ~receipts txs =
  if should_check ~epoch_id ~receipts then
    Preverify_commit.check_strings receipts txs
  else
    Ok ()