(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Address = Octra_core.Crypto.Address
module Denomination = Octra_core.Denomination
module Ledger = Octra_core.Ledger
module Transaction = Octra_core.Transaction
module Crypto = Octra_core.Crypto
module Pvac_registry = Octra_core.Pvac_registry
module Rpc = Octra_core.Rpc
module Program_package = Octra_vm.Program_package

let anon = "-"

type signed_rpc_auth_error =
  | Rpc_malformed of string
  | Rpc_auth_error of string

let display_from tx =
  match tx.Transaction.op_type with
  | Transaction.StealthOp
  | Transaction.ClaimOp -> anon
  | _ -> tx.Transaction.from

let display_to tx =
  match tx.Transaction.op_type with
  | Transaction.ClaimOp -> anon
  | _ -> tx.Transaction.to_

let mask_stealth_row row =
  match row with
  | `Assoc fields ->
    let op =
      match List.assoc_opt "op_type" fields with
      | Some (`String s) -> s
      | _ -> ""
    in
    if op = "stealth" || op = "claim" then
      let mask_to = op = "claim" in
      `Assoc (
        List.map
          (fun (k, v) ->
            match k with
            | "from" -> k, `String anon
            | "to" when mask_to -> k, `String anon
            | _ -> k, v)
          fields)
    else row
  | other -> other

let mask_stealth_rows rows =
  List.map mask_stealth_row rows

let tx_fields ~decode_message tx =
  [
    "from", `String (display_from tx);
    "to", `String (display_to tx);
    "amount", `String (Denomination.format_balance tx.Transaction.amount);
    "amount_raw", `String (Z.to_string tx.Transaction.amount);
    "nonce", `Int tx.Transaction.nonce;
    "ou", `String (Z.to_string tx.Transaction.ou);
    "timestamp", `Float tx.Transaction.timestamp;
    "op_type", `String (Transaction.op_type_to_string tx.Transaction.op_type);
    "message",
      (match tx.Transaction.message with
       | Some m -> `String (decode_message m)
      | None -> `Null);
  ]

let standard_fields ~decode_message ~from_addr ~to_addr ~amount_raw ~nonce ~ou ~timestamp ~message =
  let amount = Z.of_string amount_raw in
  [
    "op_type", `String "standard";
    "from", `String from_addr;
    "to", `String to_addr;
    "amount", `String (Denomination.format_balance amount);
    "amount_raw", `String (Z.to_string amount);
    "nonce", `Int nonce;
    "ou", `String ou;
    "timestamp", `Float timestamp;
    "message",
      (match message with
       | Some m -> `String (decode_message m)
       | None -> `Null);
  ]

let rpc_pending ~hash ~fields =
  `Assoc ([
    "status", `String "pending";
    "tx_hash", `String hash;
    "epoch", `Null;
  ] @ fields)

let rpc_confirmed ~hash ~epoch ~fields =
  `Assoc ([
    "status", `String "confirmed";
    "tx_hash", `String hash;
    "epoch", `Int epoch;
  ] @ fields)

let rpc_rejected ~hash ~from_addr ~to_addr ~amount ~nonce ~err_type ~reason ~epoch ~rejected_at =
  `Assoc [
    "status", `String "rejected";
    "tx_hash", `String hash;
    "epoch", `Int epoch;
    "error", `Assoc [
      "type", `String err_type;
      "reason", `String reason;
    ];
    "from", `String from_addr;
    "to", `String to_addr;
    "amount", `String amount;
    "nonce", `Int nonce;
    "rejected_at", `Float rejected_at;
    "source", `String "rejected_txs";
  ]

let rpc_dropped ~hash ~reason ~detail ~dropped_at ~from_addr ~to_addr ~nonce ~ou ~op_type =
  `Assoc [
    "status", `String "dropped";
    "tx_hash", `String hash;
    "reason", `String reason;
    "detail", `String detail;
    "dropped_at", `Float dropped_at;
    "from", `String from_addr;
    "to_", `String to_addr;
    "nonce", `Int nonce;
    "ou", `String (Z.to_string ou);
    "op_type", `String (Transaction.op_type_to_string op_type);
  ]

type rejected_lookup = {
  rejected_from : string;
  rejected_to : string;
  rejected_amount : string;
  rejected_nonce : int;
  rejected_type : string;
  rejected_reason : string;
  rejected_epoch : int;
  rejected_at : float;
}

type dropped_lookup = {
  dropped_reason : string;
  dropped_detail : string;
  dropped_at : float;
  dropped_from : string;
  dropped_to : string;
  dropped_nonce : int;
  dropped_ou : Z.t;
  dropped_op : Transaction.op_type;
}

type transaction_lookup =
  | Lookup_pending of Transaction.t
  | Lookup_confirmed of int * string
  | Lookup_rejected of rejected_lookup
  | Lookup_dropped of dropped_lookup
  | Lookup_missing

let rejected_lookup (fa, ta, amt, nn, etype, reason, eid, ts) =
  Lookup_rejected {
    rejected_from = fa;
    rejected_to = ta;
    rejected_amount = amt;
    rejected_nonce = nn;
    rejected_type = etype;
    rejected_reason = reason;
    rejected_epoch = eid;
    rejected_at = ts;
  }

let dropped_lookup (reason, detail, ts, fa, ta, nn, ou, op) =
  Lookup_dropped {
    dropped_reason = reason;
    dropped_detail = detail;
    dropped_at = ts;
    dropped_from = fa;
    dropped_to = ta;
    dropped_nonce = nn;
    dropped_ou = ou;
    dropped_op = op;
  }

let transaction_lookup ~pending ~confirmed ~rejected ~dropped =
  match pending with
  | Some tx -> Lookup_pending tx
  | None ->
    match confirmed with
    | Some (epoch, tx_json) -> Lookup_confirmed (epoch, tx_json)
    | None ->
      match rejected with
      | Some row -> rejected_lookup row
      | None ->
        match dropped with
        | Some row -> dropped_lookup row
        | None -> Lookup_missing

let transaction_lookup_response ~decode_message ~hash = function
  | Lookup_pending tx ->
    Ok (rpc_pending ~hash ~fields:(tx_fields ~decode_message tx))
  | Lookup_confirmed (epoch, tx_json) ->
    let fields =
      match Transaction.parse_raw_json tx_json with
      | Some tx -> tx_fields ~decode_message tx
      | None -> []
    in
    Ok (rpc_confirmed ~hash ~epoch ~fields)
  | Lookup_rejected row ->
    Ok (rpc_rejected
      ~hash
      ~from_addr:row.rejected_from
      ~to_addr:row.rejected_to
      ~amount:row.rejected_amount
      ~nonce:row.rejected_nonce
      ~err_type:row.rejected_type
      ~reason:row.rejected_reason
      ~epoch:row.rejected_epoch
      ~rejected_at:row.rejected_at)
  | Lookup_dropped row ->
    Ok (rpc_dropped
      ~hash
      ~reason:row.dropped_reason
      ~detail:row.dropped_detail
      ~dropped_at:row.dropped_at
      ~from_addr:row.dropped_from
      ~to_addr:row.dropped_to
      ~nonce:row.dropped_nonce
      ~ou:row.dropped_ou
      ~op_type:row.dropped_op)
  | Lookup_missing ->
    Error (Rpc.not_found "transaction not found")

let rest_pending ~hash ~fields =
  let top = [
    "status", `String "accepted";
    "finality", `String "pending";
    "phase", `String "staging";
    "tx_hash", `String hash;
    "epoch", `Null;
    "source", `String "staging";
  ] @ fields in
  `Assoc (("parsed_tx", `Assoc fields) :: top)

let rest_confirmed ~hash ~epoch ~tx_json ~fields =
  let top = [
    "status", `String "confirmed";
    "finality", `String "confirmed";
    "epoch", `Int epoch;
    "tx_hash", `String hash;
    "data", `String tx_json;
    "source", `String "database";
  ] in
  `Assoc (("parsed_tx", `Assoc fields) :: (fields @ top))

let rest_rejected ~hash ~from_addr ~to_addr ~amount ~nonce ~err_type ~reason ~epoch ~rejected_at =
  `Assoc [
    "status", `String "rejected";
    "finality", `String "rejected";
    "tx_hash", `String hash;
    "epoch", `Int epoch;
    "error", `Assoc [
      "type", `String err_type;
      "reason", `String reason;
    ];
    "from", `String from_addr;
    "to", `String to_addr;
    "amount", `String amount;
    "nonce", `Int nonce;
    "rejected_at", `Float rejected_at;
    "source", `String "rejected_txs";
  ]

let priority_label = function
  | Transaction.Low -> "low"
  | Transaction.Normal -> "normal"
  | Transaction.High -> "high"
  | Transaction.Express -> "express"

let has_encrypted_data_marker tx =
  match tx.Transaction.op_type with
  | Transaction.PrivateOp
  | Transaction.EncryptOp
  | Transaction.DecryptOp
  | Transaction.ContractDeploy -> Option.is_some tx.Transaction.encrypted_data
  | Transaction.ProgramDeploy -> Option.is_some tx.Transaction.encrypted_data
  | _ -> false

let staging_row ~decode_message tx =
  `Assoc [
    "hash", `String (Transaction.hash tx);
    "from", `String (display_from tx);
    "to", `String (display_to tx);
    "amount",
      (match tx.Transaction.op_type with
       | Transaction.PrivateOp -> `String "no data"
       | _ -> `String (Denomination.format_balance tx.Transaction.amount));
    "nonce", `Int tx.Transaction.nonce;
    "ou", `String (Z.to_string tx.Transaction.ou);
    "timestamp", `Float tx.Transaction.timestamp;
    "op_type", `String (Transaction.op_type_to_string tx.Transaction.op_type);
    "stage_status", `String "awaiting_epoch";
    "has_public_key", `Bool (Option.is_some tx.Transaction.public_key);
    "has_encrypted_data", `Bool (has_encrypted_data_marker tx);
    "message",
      (match tx.Transaction.message with
       | Some m -> `String (decode_message m)
       | None -> `Null);
    "priority", `String (priority_label (Transaction.priority_of tx));
  ]

let staging_response ~decode_message txs =
  `Assoc [
    "count", `Int (List.length txs);
    "staged_transactions", `List (List.map (staging_row ~decode_message) txs);
    "message", `String "Transactions staged for next epoch";
  ]

let requires_heavy_preverify tx =
  match tx.Transaction.op_type with
  | Transaction.StealthOp
  | Transaction.EncryptOp
  | Transaction.DecryptOp
  | Transaction.RecryptOp
  | Transaction.KeySwitch -> true
  | _ -> false

let decode_submit_tx tx_json =
  try Transaction.of_yojson tx_json with _ -> Error "invalid tx JSON"

let decode_submit_batch_tx tx_json =
  try Transaction.of_yojson tx_json with _ -> Error "invalid tx"

let submit_params params =
  match Rpc.param_json params 0 with
  | None ->
    Error (Rpc.invalid_params "missing transaction object")
  | Some tx_json ->
    match decode_submit_tx tx_json with
    | Ok tx ->
      Ok tx
    | Error e ->
      Error (Rpc.malformed_tx e)

let submit_batch_params params =
  match Rpc.param_json params 0 with
  | None ->
    Error (Rpc.invalid_params "missing transactions array")
  | Some (`List txs_json) when List.length txs_json > Rpc.max_batch_size ->
    Error (Rpc.invalid_params (Printf.sprintf "batch exceeds %d limit" Rpc.max_batch_size))
  | Some (`List txs_json) ->
    Ok txs_json
  | Some _ ->
    Error (Rpc.invalid_params "expected array of transactions")

let submit_rpc_error etype reason =
  match etype with
  | "sender_not_found" -> Rpc.sender_not_found
  | "invalid_signature" -> Rpc.invalid_signature
  | "invalid_nonce" -> Rpc.invalid_nonce
  | "nonce_too_far" -> Rpc.nonce_too_far
  | "insufficient_balance" -> Rpc.insufficient_balance
  | "duplicate_transaction" -> Rpc.duplicate_tx
  | "self_transfer" -> Rpc.self_transfer
  | "invalid_address" -> Rpc.invalid_address reason
  | "staging_full" -> Rpc.staging_full
  | "fee_too_low" -> Rpc.fee_too_low reason
  | "tx_too_large" -> Rpc.tx_too_large reason
  | "self_only_operation" -> Rpc.self_only_op reason
  | "unsupported_operation" ->
    Rpc.err 116 "unsupported operation" (Some (`String reason))
  | "read_only_observer" ->
    Rpc.err 117 "read-only observer" (Some (`String reason))
  | _ ->
    Rpc.malformed_tx reason

let submit_accepted ~tx_hash tx =
  Rpc_view.submit_accepted
    ~tx_hash
    ~nonce:tx.Transaction.nonce
    ~ou_cost:(Transaction.ou_cost tx)

let submit_batch_row tx result =
  match result with
  | Error (_, reason) ->
    Rpc_view.submit_batch_rejected ~reason ~nonce:tx.Transaction.nonce
  | Ok tx_hash ->
    Rpc_view.submit_batch_accepted ~tx_hash ~nonce:tx.Transaction.nonce

let submit_batch_decode_error reason =
  Rpc_view.submit_batch_decode_error ~reason

let admission_fee_check ~min_ou tx =
  if requires_heavy_preverify tx && Z.lt tx.Transaction.ou min_ou then
    Error (Printf.sprintf "fee too low: stealth requires ou >= %s, got %s"
             (Z.to_string min_ou) (Z.to_string tx.Transaction.ou))
  else Ok ()

let low_fee_heavy_hashes ~min_ou txs =
  List.filter_map
    (fun tx ->
      if requires_heavy_preverify tx && Z.lt tx.Transaction.ou min_ou then
        Some (Transaction.hash tx)
      else None)
    txs

let semantic_check tx =
  if Z.sign tx.Transaction.amount < 0 then Error "amount must not be negative"
  else if Z.sign tx.Transaction.ou < 0 then Error "ou must not be negative"
  else match tx.Transaction.op_type with
  | Transaction.Standard ->
    if Z.leq tx.Transaction.amount Z.zero then Error "amount must be positive" else Ok ()
  | Transaction.PrivateOp
  | Transaction.ContractDeploy
  | Transaction.ProgramDeploy
  | Transaction.CircleDeploy
  | Transaction.CircleProgramUpdate
  | Transaction.CircleAssetPut
  | Transaction.CircleAssetPutEncrypted
  | Transaction.CircleSealedSlotPut
  | Transaction.CircleSlotPolicyPut
  | Transaction.CircleStateDescriptorPut
  | Transaction.CircleBalanceCellPut
  | Transaction.CircleRegisterCellPut
  | Transaction.CircleTransportPolicyPut
  | Transaction.CircleHfhePolicyPut
  | Transaction.CircleKeyPolicyPut
  | Transaction.CircleKeyGrant
  | Transaction.CircleKeyExtend
  | Transaction.CircleKeyRevoke
  | Transaction.CircleKeyErase
  | Transaction.CircleOutboxOpen
  | Transaction.CircleRelayClaim
  | Transaction.CircleRelayCancel
  | Transaction.CircleIngressCommit
  | Transaction.ValidatorSetUpdate
  | Transaction.ValidatorReady
  | Transaction.ValidatorExit
  | Transaction.ValidatorWithdraw
  | Transaction.ValidatorEvidence ->
    if not (Z.equal tx.Transaction.amount Z.zero) then Error "amount must be zero for this operation" else Ok ()
  | Transaction.ValidatorBond ->
    if Z.leq tx.Transaction.amount Z.zero then Error "validator bond must be positive" else Ok ()
  | Transaction.ContractCall
  | Transaction.ProgramExec
  | Transaction.CircleCall ->
    Ok ()
  | Transaction.MultiExec ->
    if not (Z.equal tx.Transaction.amount Z.zero) then Error "amount must be zero for multi_exec" else Ok ()
  | Transaction.EncryptOp
  | Transaction.DecryptOp ->
    if Z.leq tx.Transaction.amount Z.zero then Error "amount must be positive" else Ok ()
  | Transaction.ContractUpgrade ->
    if not (Z.equal tx.Transaction.amount Z.zero) then Error "amount must be zero for this operation" else Ok ()
  | Transaction.StealthOp ->
    if not (Z.equal tx.Transaction.amount Z.zero) then Error "amount must be zero for stealth transfer" else Ok ()
  | Transaction.ClaimOp ->
    if not (Z.equal tx.Transaction.amount Z.zero) then Error "amount must be zero for claim" else Ok ()
  | Transaction.RecryptOp ->
    if not (Z.equal tx.Transaction.amount Z.zero) then Error "amount must be zero for recrypt" else Ok ()
  | Transaction.KeySwitch ->
    if not (Z.equal tx.Transaction.amount Z.zero) then Error "amount must be zero for key_switch" else Ok ()
  | Transaction.Op01Burn ->
    if Z.leq tx.Transaction.amount Z.zero then Error "amount must be positive for op01_burn" else Ok ()

let staging_submit_admission ~min_ou ~min_relay_fee tx =
  let required = Z.max min_relay_fee (Transaction.ou_cost tx) in
  match admission_fee_check ~min_ou tx with
  | Error e -> Error e
  | Ok () ->
    match semantic_check tx with
    | Error e -> Error e
    | Ok () ->
      if Z.lt tx.Transaction.ou required then
        Error (Printf.sprintf "fee too low (min: %s)" (Z.to_string required))
      else Ok ()

type staging_submit_effects = {
  total_txs : int;
  total_ou : Z.t;
  max_ou : Z.t;
  relay_payload : string option;
}

let staging_submit_effects ~relay ~peer_count ~total_txs ~total_ou ~max_ou ~tx_hash =
  let relay_payload =
    if relay && peer_count > 0 then
      Some (Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Inv [tx_hash]))
    else None in
  { total_txs; total_ou; max_ou; relay_payload }

let address_route_admission tx =
  let to_valid =
    match tx.Transaction.op_type with
    | Transaction.StealthOp -> String.equal tx.Transaction.to_ "stealth"
    | Transaction.MultiExec -> String.equal tx.Transaction.to_ "multi_exec"
    | _ -> Address.is_valid_address tx.Transaction.to_
  in
  if not (Address.is_valid_address tx.Transaction.from) || not to_valid then
    Error ("invalid_address", "malformed sender or recipient address")
  else Ok ()

let self_route_admission tx =
  let self = String.equal tx.Transaction.from tx.Transaction.to_ in
  match tx.Transaction.op_type, self with
  | Transaction.Standard, true ->
    Error ("self_transfer", "sender and recipient are the same")
  | Transaction.EncryptOp, false ->
    Error ("self_only_operation", "encrypt operation must target sender")
  | Transaction.DecryptOp, false ->
    Error ("self_only_operation", "decrypt operation must target sender")
  | Transaction.PrivateOp, true ->
    Error ("self_transfer", "private transfer must target different address")
  | Transaction.ClaimOp, false ->
    Error ("self_only_operation", "claim operation must target sender")
  | Transaction.RecryptOp, false ->
    Error ("self_only_operation", "recrypt operation must target sender")
  | Transaction.KeySwitch, false ->
    Error ("self_only_operation", "key_switch operation must target sender")
  | Transaction.Op01Burn, false ->
    Error ("self_only_operation", "op01_burn must target sender (self-tx)")
  | _ -> Ok ()

let address_validation_json ledger addr =
  let is_valid = Address.is_valid_address addr in
  let exists = Ledger.mem ledger addr in
  let pubkey = if exists then Ledger.get_public_key ledger addr else None in
  `Assoc [
    "address", `String addr;
    "valid_format", `Bool is_valid;
    "exists", `Bool exists;
    "has_public_key", `Bool (Option.is_some pubkey);
    "format_rules", `Assoc [
      "prefix", `String "oct";
      "base58_length", `Int 44;
      "total_length", `Int 47;
      "allowed_chars", `String "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
    ];
    "error",
      (if not is_valid then
         `String (
           if not (String.starts_with ~prefix:"oct" addr) then
             "Address must start with 'oct'"
           else if String.length addr <> 47 then
             Printf.sprintf "Invalid address length: expected 47, got %d" (String.length addr)
           else
             "Address contains invalid characters")
       else `Null);
  ]

let staging_remove_message tx_hash =
  "staging_remove|" ^ tx_hash

type staging_remove_request = {
  staging_remove_hash : string;
  staging_remove_signature_b64 : string;
  staging_remove_pubkey_b64 : string;
}

let staging_remove_params params =
  match Rpc.require_hash params 0 "tx_hash" with
  | Error e -> Error e
  | Ok staging_remove_hash ->
    match Rpc.require_string params 1 "signature" with
    | Error e -> Error e
    | Ok staging_remove_signature_b64 ->
      match Rpc.require_string params 2 "pubkey" with
      | Error e -> Error e
      | Ok staging_remove_pubkey_b64 ->
        Ok {
          staging_remove_hash;
          staging_remove_signature_b64;
          staging_remove_pubkey_b64;
        }

let verify_address_signature ~addr ~pubkey_b64 ~signature_b64 ~msg ~mismatch_error =
  if not (Address.verify_address_pubkey addr pubkey_b64) then
    Error mismatch_error
  else
    match Base64.decode pubkey_b64 with
    | Error _ -> Error "invalid pubkey b64"
    | Ok pub_raw ->
      if not (Octra_ed25519.safe pub_raw) then Error "invalid pubkey"
      else
        match Base64.decode signature_b64 with
        | Error _ -> Error "invalid signature b64"
        | Ok sig_raw ->
          if Octra_ed25519.verify ~pub:pub_raw ~msg sig_raw then Ok ()
          else Error "signature verification failed"

let staging_remove_auth ~sender ~tx_hash ~signature_b64 ~pubkey_b64 =
  verify_address_signature
    ~addr:sender
    ~pubkey_b64
    ~signature_b64
    ~msg:(staging_remove_message tx_hash)
    ~mismatch_error:"pubkey does not match tx sender"

let staging_remove_request_auth ~sender req =
  staging_remove_auth
    ~sender
    ~tx_hash:req.staging_remove_hash
    ~signature_b64:req.staging_remove_signature_b64
    ~pubkey_b64:req.staging_remove_pubkey_b64

let public_key_registration_message addr =
  "register_pubkey:" ^ addr

let public_key_registration_auth ~addr ~pubkey_b64 ~signature_b64 =
  let pubkey_len =
    try String.length (Base64.decode_exn pubkey_b64) with _ -> 0 in
  let signature_len =
    try String.length (Base64.decode_exn signature_b64) with _ -> 0 in
  if pubkey_len <> 32 || signature_len <> 64 then
    Error "invalid key or signature length"
  else
    verify_address_signature
      ~addr
      ~pubkey_b64
      ~signature_b64
      ~msg:(public_key_registration_message addr)
      ~mismatch_error:"public key does not match address"

let encrypted_balance_message addr =
  "octra_encryptedBalance|" ^ addr

let encrypted_balance_auth params ~addr =
  let sig_b64 =
    match params with
    | `List values ->
      begin
        match List.nth_opt values 1 with
        | Some (`String value) -> Some value
        | _ -> None
      end
    | _ -> None in
  let pubkey_b64 =
    match params with
    | `List values ->
      begin
        match List.nth_opt values 2 with
        | Some (`String value) -> Some value
        | _ -> None
      end
    | _ -> None in
  match sig_b64, pubkey_b64 with
  | None, _
  | _, None ->
    Error (Rpc_malformed "encryptedBalance requires [address, signature, pubkey]")
  | Some signature_b64, Some pubkey_b64 ->
    match verify_address_signature
            ~addr
            ~pubkey_b64
            ~signature_b64
            ~msg:(encrypted_balance_message addr)
            ~mismatch_error:"pubkey does not match address" with
    | Ok () -> Ok ()
    | Error e -> Error (Rpc_auth_error e)

let staging_error msg =
  let m = String.lowercase_ascii msg in
  if m = "duplicate transaction" || String.length m > 14 && String.sub m 0 14 = "duplicate nonce" then
    "duplicate_transaction", "tx already in staging"
  else if m = "nonce too low (already used)" then
    "invalid_nonce", "nonce already used"
  else if m = "nonce too far ahead" then
    "nonce_too_far", "nonce too far ahead"
  else if String.length m > 20 && String.sub m 0 21 = "insufficient balance " then
    "insufficient_balance", msg
  else if String.length m > 20 && String.sub m 0 20 = "amount must be posit" then
    "malformed_transaction", "amount must be greater than zero"
  else if String.length m > 20 && String.sub m 0 20 = "amount must be zero " then
    "malformed_transaction", msg
  else if String.length m > 8 && String.sub m 0 8 = "fee too " then
    "fee_too_low", msg
  else if String.length m > 12 && String.sub m 0 12 = "staging full" then
    "staging_full", msg
  else if String.length m > 11 && String.sub m 0 11 = "transaction" then
    "tx_too_large", msg
  else
    "internal_error", msg

let semantic_error_reason tx e =
  match e with
  | "Amount must be positive" -> "amount must be greater than zero"
  | "Amount must be zero for this operation" ->
    Printf.sprintf
      "amount must be zero for %s"
      (Transaction.op_type_to_string tx.Transaction.op_type)
  | other -> other

let has_html value =
  let len = String.length value in
  let rec scan i =
    if i >= len - 1 then false
    else if value.[i] = '<' then
      let c = value.[i + 1] in
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '/' || c = '!'
      || scan (i + 1)
    else scan (i + 1)
  in
  scan 0

let call_params_message_ok msg =
  try
    let json = Yojson.Safe.from_string msg in
    let value_ok = function
      | `String value -> not (has_html value)
      | _ -> true
    in
    match json with
    | `List values -> List.for_all value_ok values
    | _ -> true
  with _ -> true

let call_params_ok tx =
  match tx.Transaction.op_type with
  | Transaction.ContractCall
  | Transaction.ProgramExec
  | Transaction.MultiExec
  | Transaction.ContractDeploy
  | Transaction.ProgramDeploy
  | Transaction.CircleCall ->
    begin
      match tx.Transaction.message with
      | Some msg -> call_params_message_ok msg
      | None -> true
    end
  | _ -> true

let base64_decodes_to_len expected_len encoded =
  try
    String.length (Base64.decode_exn encoded) = expected_len
  with _ -> false

let hex_len n s =
  String.length s = n
  && String.for_all
       (function
         | '0'..'9'
         | 'a'..'f'
         | 'A'..'F' -> true
         | _ -> false)
       s

let range_proof_payload_ok encoded_proof =
  let prefix = "rp_v1|" in
  let prefix_len = String.length prefix in
  String.length encoded_proof > prefix_len
  && String.sub encoded_proof 0 prefix_len = prefix
  &&
  try
    let raw =
      String.sub encoded_proof prefix_len (String.length encoded_proof - prefix_len)
      |> Base64.decode_exn in
    String.length raw > 0
  with _ -> false

let cipher_payload_ok encoded_cipher =
  match Crypto.FheBalance.decode_cipher encoded_cipher with
  | Ok _ -> true
  | Error _ -> false

let zero_proof_payload_ok encoded_proof =
  match Crypto.FheBalance.decode_zero_proof encoded_proof with
  | Ok _ -> true
  | Error _ -> false

let encrypt_decrypt_payload_ok s =
  try
    let j = Yojson.Safe.from_string s in
    let str_k k = Yojson.Safe.Util.(member k j |> to_string_option) in
    match str_k "cipher", str_k "amount_commitment", str_k "zero_proof", str_k "blinding" with
    | Some c, Some ac, Some zp, Some bl ->
      cipher_payload_ok c
      && base64_decodes_to_len 32 ac
      && zero_proof_payload_ok zp
      && base64_decodes_to_len 32 bl
    | _ -> false
  with _ -> false

let stealth_payload_ok json =
  try
    let j = Yojson.Safe.from_string json in
    match Yojson.Safe.Util.(member "version" j |> to_int_option) with
    | Some 5 ->
      begin
        match Crypto.PrivateTransferV4.of_json j with
        | Ok ptd ->
          hex_len 64 ptd.claim_pub
          && hex_len 32 ptd.stealth_tag
          && String.length ptd.eph_pub > 0
          && cipher_payload_ok ptd.delta_cipher
          && range_proof_payload_ok ptd.range_proof_delta
          && range_proof_payload_ok ptd.range_proof_balance
          && base64_decodes_to_len 32 ptd.commitment
          && base64_decodes_to_len 32 ptd.amount_commitment
          && (String.length ptd.send_zero_proof = 0
              || zero_proof_payload_ok ptd.send_zero_proof)
        | Error _ -> false
      end
    | _ -> false
  with _ -> false

let preverify_stealth_payload tx =
  match tx.Transaction.op_type, tx.Transaction.encrypted_data with
  | Transaction.StealthOp, Some payload ->
    begin
      try
        match Crypto.PrivateTransferV4.of_json (Yojson.Safe.from_string payload) with
        | Ok ptd -> Some ptd
        | Error _ -> None
      with _ -> None
    end
  | _ -> None

let preverify_stealth_ranges ~pubkey_blob ~sender_enc ptd =
  let open Lwt.Syntax in
  let* new_enc =
    Octra_core.Proof_pool.run (fun () ->
      match Crypto.FheBalance.load_pubkey_result pubkey_blob with
      | Error e -> Error e
      | Ok pk ->
        Crypto.FheBalance.ct_sub_encoded
          pk
          sender_enc
          ptd.Crypto.PrivateTransferV4.delta_cipher)
  in
  match new_enc with
  | Error e -> Lwt.return_error e
  | Ok new_enc ->
    let* delta =
      Octra_core.Pvac_verify_worker.verify_range
        ~pubkey:pubkey_blob
        ~cipher:ptd.Crypto.PrivateTransferV4.delta_cipher
        ~proof:ptd.Crypto.PrivateTransferV4.range_proof_delta
    in
    let* balance =
      Octra_core.Pvac_verify_worker.verify_range
        ~pubkey:pubkey_blob
        ~cipher:new_enc
        ~proof:ptd.Crypto.PrivateTransferV4.range_proof_balance
    in
    Lwt.return_ok (Result.is_ok delta, Result.is_ok balance)

let claim_payload_ok json =
  try
    let j = Yojson.Safe.from_string json in
    match Yojson.Safe.Util.(member "version" j |> to_int_option) with
    | Some 5 ->
      begin
        match Crypto.StealthClaimV5.of_json j with
        | Ok claim ->
          hex_len 64 claim.claim_secret
          && claim.output_id >= 0
          && cipher_payload_ok claim.claim_cipher
          && zero_proof_payload_ok claim.zero_proof
          && base64_decodes_to_len 32 claim.commitment
        | Error _ -> false
      end
    | _ -> false
  with _ -> false

let key_switch_payload_ok json =
  try
    let j = Yojson.Safe.from_string json in
    let str_k k = Yojson.Safe.Util.(member k j |> to_string_option) in
    match str_k "new_pubkey", str_k "aes_kat" with
    | Some new_pk_b64, Some kat_hex ->
      kat_hex = Pvac_registry.expected_kat ()
      &&
      begin
        try
          match Pvac_registry.decode_b64 new_pk_b64 with
          | Error _ -> false
          | Ok pk_blob ->
            begin
              match Pvac_registry.load_pubkey pk_blob with
              | Ok _ -> true
              | Error _ -> false
            end
        with _ -> false
      end
    | _ -> false
  with _ -> false

let encrypted_payload_ok tx =
  match tx.Transaction.op_type, tx.Transaction.encrypted_data with
  | (Transaction.EncryptOp | Transaction.DecryptOp), Some s ->
    encrypt_decrypt_payload_ok s
  | Transaction.PrivateOp, _ -> true
  | Transaction.StealthOp, Some json ->
    stealth_payload_ok json
  | Transaction.ClaimOp, Some json ->
    claim_payload_ok json
  | Transaction.RecryptOp, _ -> true
  | Transaction.KeySwitch, Some json ->
    key_switch_payload_ok json
  | Transaction.KeySwitch, None -> false
  | Transaction.ProgramDeploy, Some package ->
    Result.is_ok (Program_package.validate_base64 package)
  | Transaction.ProgramDeploy, None -> false
  | (Transaction.EncryptOp
    | Transaction.DecryptOp
    | Transaction.StealthOp
    | Transaction.ClaimOp), None -> false
  | _ -> true

type payload_limits = {
  encrypted_data_len : int;
  message_len : int;
  message_zkp_len : int;
  message_validator_len : int;
  message_program_len : int;
}

let payload_limits = {
  encrypted_data_len = 50_000_000;
  message_len = 256;
  message_zkp_len = 50_000;
  message_validator_len = 16_384;
  message_program_len = 10_000_000;
}

let payload_message_limit limits tx =
  match tx.Transaction.op_type with
  | Transaction.RecryptOp
  | Transaction.ClaimOp -> limits.message_zkp_len
  | Transaction.CircleAssetPut
  | Transaction.CircleAssetPutEncrypted
  | Transaction.CircleSealedSlotPut -> Transaction.circle_asset_message_len
  | Transaction.ContractCall
  | Transaction.ProgramExec
  | Transaction.MultiExec
  | Transaction.CircleCall
  | Transaction.CircleSlotPolicyPut
  | Transaction.CircleStateDescriptorPut
  | Transaction.CircleBalanceCellPut
  | Transaction.CircleRegisterCellPut
  | Transaction.CircleTransportPolicyPut
  | Transaction.CircleHfhePolicyPut
  | Transaction.CircleKeyPolicyPut
  | Transaction.CircleKeyGrant
  | Transaction.CircleKeyExtend
  | Transaction.CircleKeyRevoke
  | Transaction.CircleKeyErase
  | Transaction.CircleOutboxOpen
  | Transaction.CircleRelayClaim
  | Transaction.CircleRelayCancel
  | Transaction.CircleIngressCommit
  | Transaction.ContractDeploy
  | Transaction.ProgramDeploy
  | Transaction.CircleDeploy
  | Transaction.CircleProgramUpdate -> limits.message_program_len
  | Transaction.ValidatorSetUpdate
  | Transaction.ValidatorReady
  | Transaction.ValidatorBond
  | Transaction.ValidatorEvidence -> limits.message_validator_len
  | _ -> limits.message_len

let payload_encrypted_data_limit limits tx =
  match tx.Transaction.op_type with
  | Transaction.CircleAssetPut
  | Transaction.CircleAssetPutEncrypted
  | Transaction.CircleSealedSlotPut
  | Transaction.CircleBalanceCellPut
  | Transaction.CircleRegisterCellPut ->
    Transaction.circle_asset_max_encrypted_data_len
  | _ -> limits.encrypted_data_len

let circle_asset_body_too_large tx =
  match tx.Transaction.op_type, tx.Transaction.encrypted_data with
  | (Transaction.CircleAssetPut
    | Transaction.CircleAssetPutEncrypted
    | Transaction.CircleSealedSlotPut
    | Transaction.CircleBalanceCellPut
    | Transaction.CircleRegisterCellPut), Some value ->
    String.length value > Transaction.circle_asset_max_encrypted_data_len
  | _ -> false

let payload_size_ok ~limits tx =
  let enc_limit = payload_encrypted_data_limit limits tx in
  let msg_limit = payload_message_limit limits tx in
  (match tx.Transaction.encrypted_data with
   | Some value -> String.length value <= enc_limit
   | None -> true)
  && (match tx.Transaction.message with
      | Some value -> String.length value <= msg_limit
      | None -> true)

let payload_admission ~limits tx =
  if circle_asset_body_too_large tx then
    Error ("malformed_transaction", "circle asset body exceeds max encoded size")
  else if not (payload_size_ok ~limits tx) then
    Error ("malformed_transaction", "encrypted_data or message exceeds size limit")
  else
    match Octra_core.Bft_control_admission.validate_message tx.Transaction.op_type tx.Transaction.message with
    | Error e -> Error ("malformed_transaction", e)
    | Ok () ->
      if not (encrypted_payload_ok tx) then
        Error ("malformed_transaction", "malformed or oversized encrypted_data")
      else if not (call_params_ok tx) then
        Error ("malformed_transaction", "contract call params contain invalid characters")
      else Ok ()

let bft_op_admission ~bft_mode tx =
  if
    bft_mode
    && not
         (Transaction.bft_consensus_admits_op
            tx.Transaction.op_type)
  then
    Error ("unsupported_operation", Transaction.bft_reject_reason tx.Transaction.op_type)
  else
    Ok ()

let pre_route_admission
    ~now
    ~max_timestamp_drift
    ~observer_rpc_mode
    ~bft_mode
    tx =
  let timestamp_drift = Float.abs (tx.Transaction.timestamp -. now) in
  if timestamp_drift > max_timestamp_drift then
    Error ("malformed_transaction",
      Printf.sprintf "timestamp drift %.0fs exceeds %ds limit"
        timestamp_drift (int_of_float max_timestamp_drift))
  else if observer_rpc_mode then
    Error ("read_only_observer", "observer node is read-only")
  else
    bft_op_admission ~bft_mode tx

let submission_pubkey ~account_public_key tx =
  match account_public_key, tx.Transaction.public_key with
  | Some pk, _ -> Some pk
  | None, pk -> pk

let signature_admission ~account_public_key tx =
  match submission_pubkey ~account_public_key tx with
  | None -> Error ("invalid_signature", "no public key available")
  | Some pk ->
    if not (Address.verify_address_pubkey tx.Transaction.from pk) then
      Error ("invalid_address", "address does not match public key")
    else if not (Transaction.verify tx pk) then
      Error ("invalid_signature", "signature verification failed")
    else Ok ()

let sender_admission ~sender_exists tx =
  if Z.leq tx.Transaction.ou Z.zero then
    Error ("malformed_transaction", "OU must be greater than zero")
  else if not sender_exists then
    Error ("sender_not_found", "sender account does not exist")
  else Ok ()

let submit_pre_signature_admission
    ~now
    ~max_timestamp_drift
    ~observer_rpc_mode
    ~bft_mode
    ~limits
    ~sender_exists
    tx =
  match pre_route_admission
          ~now
          ~max_timestamp_drift
          ~observer_rpc_mode
          ~bft_mode
          tx with
  | Error e -> Error e
  | Ok () ->
    match address_route_admission tx with
    | Error e -> Error e
    | Ok () ->
      match payload_admission ~limits tx with
      | Error e -> Error e
      | Ok () ->
        match self_route_admission tx with
        | Error e -> Error e
        | Ok () ->
          match semantic_check tx with
          | Error e -> Error ("malformed_transaction", semantic_error_reason tx e)
          | Ok () -> sender_admission ~sender_exists tx

let encrypted_data_missing_or_empty tx =
  match tx.Transaction.encrypted_data with
  | None -> true
  | Some value -> String.equal value ""

let post_signature_admission
    ~preverify_has_capacity
    ~preverify_pending
    ~preverify_pending_max
    ~bft_mode
    ~confirmed_nonce
    ~bft_window
    tx =
  if tx.Transaction.op_type = Transaction.StealthOp
     && encrypted_data_missing_or_empty tx then
    Error ("malformed_transaction", "stealth tx requires encrypted_data with FHE delta + range proofs")
  else if (tx.Transaction.op_type = Transaction.EncryptOp
           || tx.Transaction.op_type = Transaction.DecryptOp
           || tx.Transaction.op_type = Transaction.RecryptOp
           || tx.Transaction.op_type = Transaction.KeySwitch)
          && encrypted_data_missing_or_empty tx then
    Error ("malformed_transaction", "FHE op requires encrypted_data with cipher + proofs")
  else if tx.Transaction.op_type = Transaction.StealthOp && not preverify_has_capacity then
    Error ("pre_verify_busy",
      Printf.sprintf "stealth pre-verify busy (%d/%d), retry shortly"
        preverify_pending preverify_pending_max)
  else if bft_mode && tx.Transaction.nonce > confirmed_nonce + bft_window then
    Error ("nonce_too_far",
      Printf.sprintf
        "nonce too far ahead for BFT admission window (confirmed=%d window=%d)"
        confirmed_nonce bft_window)
  else Ok ()

let submit_signature_admission
    ~account_public_key
    ~preverify_has_capacity
    ~preverify_pending
    ~preverify_pending_max
    ~bft_mode
    ~confirmed_nonce
    ~bft_window
    tx =
  match signature_admission ~account_public_key tx with
  | Error e -> Error e
  | Ok () ->
    post_signature_admission
      ~preverify_has_capacity
      ~preverify_pending
      ~preverify_pending_max
      ~bft_mode
      ~confirmed_nonce
      ~bft_window
      tx