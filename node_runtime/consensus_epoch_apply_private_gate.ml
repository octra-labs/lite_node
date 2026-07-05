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


type reject = {
  tag : string;
  reason : string;
}

type notify_reject = {
  tag : string;
  reason : string;
  notify_reason : string;
}

type kat_action =
  | Kat_apply
  | Kat_backfill_then_apply
  | Kat_reject of notify_reject

let fhe_cap ~current ~max =
  if current >= max then
    Some {
      tag = "fhe_epoch_cap";
      reason = "FHE operation cap reached for this epoch";
    }
  else
    None

let encrypted_debit_limit ~had_debit =
  if had_debit then
    Some {
      tag = "encrypted_op_limit";
      reason = "only one encrypted debit operation per sender per epoch";
    }
  else
    None

let stealth_target ~target =
  if String.equal target "stealth" then
    None
  else
    Some {
      tag = "invalid_stealth_target";
      reason = "stealth transfer must have to_=\"stealth\"";
    }

let aes_kat_mismatch ~op =
  {
    tag = "aes_kat_mismatch";
    reason =
      Printf.sprintf
        "%s: pubkey registered with incompatible AES - re-register with compatible client"
        op;
  }

let aes_kat_notify ~op =
  Printf.sprintf "%s: AES KAT mismatch - re-register pvac pubkey" op

let aes_kat_notify_reject ~op =
  let reject = aes_kat_mismatch ~op in
  {
    tag = reject.tag;
    reason = reject.reason;
    notify_reason = aes_kat_notify ~op;
  }

let kat_action ~op = function
  | Octra_core.Private_ledger.Kat_missing ->
    Kat_backfill_then_apply
  | Octra_core.Private_ledger.Kat_current ->
    Kat_apply
  | Octra_core.Private_ledger.Kat_mismatch ->
    Kat_reject (aes_kat_notify_reject ~op)

let first_reject =
  List.find_map Fun.id