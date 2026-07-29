(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type priority = Low | Normal | High | Express

type op_type =
  | Standard | EncryptOp | DecryptOp | PrivateOp
  | ContractDeploy | ProgramDeploy | ContractCall | ProgramExec | MultiExec | ContractUpgrade
  | CircleDeploy | CircleProgramUpdate | CircleAssetPut | CircleAssetPutEncrypted
  | CircleSealedSlotPut | CircleSlotPolicyPut
  | CircleStateDescriptorPut
  | CircleBalanceCellPut | CircleRegisterCellPut
  | CircleTransportPolicyPut | CircleHfhePolicyPut | CircleKeyPolicyPut
  | CircleKeyGrant | CircleKeyExtend | CircleKeyRevoke | CircleKeyErase
  | CircleOutboxOpen | CircleRelayClaim | CircleRelayCancel | CircleIngressCommit
  | CircleCall
  | ValidatorSetUpdate | ValidatorReady
  | ValidatorBond | ValidatorExit | ValidatorWithdraw | ValidatorEvidence
  | StealthOp | ClaimOp | RecryptOp | KeySwitch
  | Op01Burn

let op_type_to_string = function
  | Standard -> "standard" | EncryptOp -> "encrypt"
  | DecryptOp -> "decrypt" | PrivateOp -> "private"
  | ContractDeploy -> "deploy" | ContractCall -> "call"
  | ProgramDeploy -> "program_deploy"
  | ProgramExec -> "program_exec"
  | MultiExec -> "multi_exec"
  | ContractUpgrade -> "upgrade"
  | CircleDeploy -> "deploy_circle"
  | CircleProgramUpdate -> "circle_program_update"
  | CircleAssetPut -> "circle_asset_put"
  | CircleAssetPutEncrypted -> "circle_asset_put_encrypted"
  | CircleSealedSlotPut -> "circle_sealed_slot_put"
  | CircleSlotPolicyPut -> "circle_slot_policy_put"
  | CircleStateDescriptorPut -> "circle_state_descriptor_put"
  | CircleBalanceCellPut -> "circle_balance_cell_put"
  | CircleRegisterCellPut -> "circle_register_cell_put"
  | CircleTransportPolicyPut -> "circle_transport_policy_put"
  | CircleHfhePolicyPut -> "circle_hfhe_policy_put"
  | CircleKeyPolicyPut -> "circle_key_policy_put"
  | CircleKeyGrant -> "circle_key_grant"
  | CircleKeyExtend -> "circle_key_extend"
  | CircleKeyRevoke -> "circle_key_revoke"
  | CircleKeyErase -> "circle_key_erase"
  | CircleOutboxOpen -> "circle_outbox_open"
  | CircleRelayClaim -> "circle_relay_claim"
  | CircleRelayCancel -> "circle_relay_cancel"
  | CircleIngressCommit -> "circle_ingress_commit"
  | CircleCall -> "circle_call"
  | ValidatorSetUpdate -> "validator_set_update"
  | ValidatorReady -> "validator_ready"
  | ValidatorBond -> "validator_bond"
  | ValidatorExit -> "validator_exit"
  | ValidatorWithdraw -> "validator_withdraw"
  | ValidatorEvidence -> "validator_evidence"
  | StealthOp -> "stealth" | ClaimOp -> "claim"
  | RecryptOp -> "recrypt"
  | KeySwitch -> "key_switch"
  | Op01Burn -> "op01_burn"

let is_circle_noncall = function
  | CircleDeploy
  | CircleProgramUpdate
  | CircleAssetPut
  | CircleAssetPutEncrypted
  | CircleSealedSlotPut
  | CircleSlotPolicyPut
  | CircleStateDescriptorPut
  | CircleBalanceCellPut
  | CircleRegisterCellPut
  | CircleTransportPolicyPut
  | CircleHfhePolicyPut
  | CircleKeyPolicyPut
  | CircleKeyGrant
  | CircleKeyExtend
  | CircleKeyRevoke
  | CircleKeyErase
  | CircleOutboxOpen
  | CircleRelayClaim
  | CircleRelayCancel
  | CircleIngressCommit -> true
  | _ -> false

let bft_circle_noncall op =
  is_circle_noncall op

type bft_crypto_profile =
  | Crypto_off
  | Private_v1

type bft_release_profile =
  | Release_off
  | Devnet_private_v1
  | Devnet_full_v1

let bft_release_profile_of getenv =
  match getenv "OCTRA_BFT_RELEASE_PROFILE" with
  | Some "devnet_private_v1" -> Devnet_private_v1
  | Some "devnet_full_v1" -> Devnet_full_v1
  | Some _
  | None -> Release_off

let bft_release_profile () =
  bft_release_profile_of Sys.getenv_opt

let bft_release_profile_name = function
  | Release_off -> "off"
  | Devnet_private_v1 -> "devnet_private_v1"
  | Devnet_full_v1 -> "devnet_full_v1"

let bft_crypto_profile_of getenv =
  match bft_release_profile_of getenv with
  | Devnet_private_v1
  | Devnet_full_v1 -> Private_v1
  | Release_off ->
    match getenv "OCTRA_BFT_CRYPTO_PROFILE" with
    | Some "private_v1" -> Private_v1
    | Some _
    | None -> Crypto_off

let bft_crypto_profile () =
  bft_crypto_profile_of Sys.getenv_opt

let bft_crypto_profile_name = function
  | Crypto_off -> "off"
  | Private_v1 -> "private_v1"

let bft_crypto_op = function
  | EncryptOp
  | DecryptOp
  | StealthOp
  | ClaimOp
  | KeySwitch -> true
  | _ -> false

let bft_crypto_active () =
  bft_crypto_profile () = Private_v1

let bft_experimental_can_admit = function
  | op when bft_circle_noncall op -> true
  | op when bft_crypto_active () && bft_crypto_op op -> true
  | CircleCall -> true
  | _ -> false

let bft_allow_token_matches token op =
  let token = String.trim token in
  token <> "" && (
    String.equal token (op_type_to_string op)
    || (String.equal token "circle_noncall" && bft_circle_noncall op)
  )

let bft_lab_op_allowed op =
  match bft_release_profile () with
  | Devnet_private_v1 -> bft_crypto_op op
  | Devnet_full_v1 ->
    bft_crypto_op op
    || bft_circle_noncall op
    || begin
      match op with
      | ContractDeploy
      | ProgramDeploy
      | ContractCall
      | ProgramExec
      | MultiExec
      | ContractUpgrade
      | CircleCall -> true
      | _ -> false
    end
  | Release_off ->
    if bft_crypto_active () && bft_crypto_op op then
      true
    else match Sys.getenv_opt "OCTRA_BFT_ADMIT_OPS" with
    | None | Some "" -> false
    | Some raw ->
      bft_experimental_can_admit op
      && (raw
          |> String.split_on_char ','
          |> List.exists (fun token -> bft_allow_token_matches token op))

let bft_admits_op = function
  | Standard -> true
  | op -> bft_lab_op_allowed op

let bft_consensus_admits_op = function
  | ValidatorSetUpdate
  | ValidatorReady
  | ValidatorBond
  | ValidatorExit
  | ValidatorWithdraw -> true
  | ValidatorEvidence -> true
  | op -> bft_admits_op op

let bft_reject_reason op =
  Printf.sprintf
    "BFT operation disabled by the active release profile; op_type = %s"
    (op_type_to_string op)

let op_type_of_string = function
  | "standard" -> Ok Standard
  | "encrypt" -> Ok EncryptOp | "decrypt" -> Ok DecryptOp
  | "private" -> Ok PrivateOp | "deploy" -> Ok ContractDeploy
  | "program_deploy" -> Ok ProgramDeploy
  | "call" -> Ok ContractCall
  | "program_exec" -> Ok ProgramExec
  | "multi_exec" -> Ok MultiExec
  | "upgrade" -> Ok ContractUpgrade
  | "deploy_circle" -> Ok CircleDeploy
  | "circle_program_update" -> Ok CircleProgramUpdate
  | "circle_asset_put" -> Ok CircleAssetPut
  | "circle_asset_put_encrypted" -> Ok CircleAssetPutEncrypted
  | "circle_sealed_slot_put" -> Ok CircleSealedSlotPut
  | "circle_slot_policy_put" -> Ok CircleSlotPolicyPut
  | "circle_state_descriptor_put" -> Ok CircleStateDescriptorPut
  | "circle_balance_cell_put" -> Ok CircleBalanceCellPut
  | "circle_register_cell_put" -> Ok CircleRegisterCellPut
  | "circle_transport_policy_put" -> Ok CircleTransportPolicyPut
  | "circle_hfhe_policy_put" -> Ok CircleHfhePolicyPut
  | "circle_key_policy_put" -> Ok CircleKeyPolicyPut
  | "circle_key_grant" -> Ok CircleKeyGrant
  | "circle_key_extend" -> Ok CircleKeyExtend
  | "circle_key_revoke" -> Ok CircleKeyRevoke
  | "circle_key_erase" -> Ok CircleKeyErase
  | "circle_outbox_open" -> Ok CircleOutboxOpen
  | "circle_relay_claim" -> Ok CircleRelayClaim
  | "circle_relay_cancel" -> Ok CircleRelayCancel
  | "circle_ingress_commit" -> Ok CircleIngressCommit
  | "circle_call" -> Ok CircleCall
  | "validator_set_update" -> Ok ValidatorSetUpdate
  | "validator_ready" -> Ok ValidatorReady
  | "validator_bond" -> Ok ValidatorBond
  | "validator_exit" -> Ok ValidatorExit
  | "validator_withdraw" -> Ok ValidatorWithdraw
  | "validator_evidence" -> Ok ValidatorEvidence
  | "stealth" -> Ok StealthOp | "claim" -> Ok ClaimOp
  | "recrypt" -> Ok RecryptOp
  | "key_switch" -> Ok KeySwitch
  | "op01_burn" -> Ok Op01Burn
  | value -> Error ("unsupported op_type: " ^ value)

type t = {
  from : string;
  to_ : string;
  amount : Z.t;
  nonce : int;
  ou : Z.t;
  timestamp : float;
  signature : string;
  public_key : string option;
  message : string option;
  op_type : op_type;
  encrypted_data : string option;
}

let compare_by_ou a b = Z.compare b.ou a.ou

let opt_field k = function None -> [] | Some v -> [k, `String v]

let to_yojson tx =
  `Assoc ([
    "from", `String tx.from;
    "to_", `String tx.to_;
    "amount", `String (Z.to_string tx.amount);
    "nonce", `Int tx.nonce;
    "ou", `String (Z.to_string tx.ou);
    "timestamp", `Float tx.timestamp;
    "signature", `String tx.signature;
    "op_type", `String (op_type_to_string tx.op_type);
  ] @ opt_field "public_key" tx.public_key
    @ opt_field "message" tx.message
    @ opt_field "encrypted_data" tx.encrypted_data)

let of_yojson = function
  | `Assoc f ->
    let str k = match List.assoc_opt k f with
      | Some (`String s) -> Ok s | _ -> Error k in
    let int k = match List.assoc_opt k f with
      | Some (`Int n) -> Ok n | _ -> Error k in
    let z k = match List.assoc_opt k f with
      | Some (`String s) ->
        (try
          let v = Z.of_string s in
          if Z.sign v < 0 then Error (k ^ ":negative")
          else Ok v
         with _ -> Error (k ^ ":parse"))
      | Some (`Int n) ->
        if n < 0 then Error (k ^ ":negative")
        else Ok (Z.of_int n)
      | _ -> Error k in
    let fl k = match List.assoc_opt k f with
      | Some (`Float x) -> Ok x
      | Some (`Int n) -> Ok (float_of_int n) | _ -> Error k in
    let opt k = match List.assoc_opt k f with
      | Some (`String s) -> Some s | _ -> None in
    let op = match List.assoc_opt "op_type" f with
      | Some (`String s) -> op_type_of_string s | _ -> Ok Standard in
    (match str "from", str "to_", z "amount", int "nonce",
           z "ou", fl "timestamp", str "signature", op with
    | Ok from, Ok to_, Ok amount, Ok nonce,
      Ok ou, Ok timestamp, Ok signature, Ok op_type ->
      Ok { from; to_; amount; nonce; ou; timestamp; signature;
           public_key = opt "public_key";
           message = opt "message";
           op_type;
           encrypted_data = opt "encrypted_data" }
    | _ -> Error "Malformed JSON: missing or invalid fields")
  | _ -> Error "Expected JSON object"

let serialize_for_signing tx =
  let base = [
    "from", `String tx.from; "to_", `String tx.to_;
    "amount", `String (Z.to_string tx.amount);
    "nonce", `Int tx.nonce;
    "ou", `String (Z.to_string tx.ou);
    "timestamp", `Float tx.timestamp;
    "op_type", `String (op_type_to_string tx.op_type);
  ] in
  let fields = match tx.encrypted_data with
    | Some ed -> base @ ["encrypted_data", `String ed]
    | None -> base
  in
  let fields = match tx.message with
    | Some m -> fields @ ["message", `String m]
    | None -> fields
  in
  `Assoc fields |> Yojson.Safe.to_string

let sign_with_privkey tx priv_b64 =
  match Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn priv_b64) with
  | Error _ -> tx
  | Ok sk ->
    let raw = Mirage_crypto_ec.Ed25519.sign ~key:sk (serialize_for_signing tx) in
    { tx with signature = Base64.encode_exn raw }

let verify tx pub_b64 =
  try
    Octra_ed25519.verify
      ~pub:(Base64.decode_exn pub_b64)
      ~msg:(serialize_for_signing tx)
      (Base64.decode_exn tx.signature)
  with _ -> false

let circle_asset_max_raw_bytes = 33_554_432

let circle_asset_max_encrypted_data_len =
  ((circle_asset_max_raw_bytes + 2) / 3) * 4

let circle_asset_message_len = 8_192

let circle_asset_decoded_size_upper_bound wire_len =
  ((wire_len + 3) / 4) * 3

let circle_asset_fee_bucket raw_size =
  if raw_size <= 4_096 then 5_000
  else if raw_size <= 16_384 then 10_000
  else if raw_size <= 32_768 then 20_000
  else if raw_size <= 131_072 then 40_000
  else if raw_size <= 524_288 then 80_000
  else if raw_size <= 2_097_152 then 160_000
  else if raw_size <= 8_388_608 then 320_000
  else 640_000

let circle_asset_min_ou_from_wire_len wire_len =
  Z.of_int (circle_asset_fee_bucket (circle_asset_decoded_size_upper_bound wire_len))

let circle_asset_min_ou = function
  | None -> Z.of_int 5_000
  | Some body_b64 -> circle_asset_min_ou_from_wire_len (String.length body_b64)

let stealth_min_ou_default = Z.of_int 5_000
let stealth_min_ou_max = Z.of_int64 10_000_000_000L

let stealth_min_ou_of getenv =
  match getenv "OCTRA_STEALTH_MIN_OU" with
  | Some raw ->
    (try
       let value = Z.of_string raw in
       if Z.sign value <= 0 || Z.gt value stealth_min_ou_max then
         stealth_min_ou_default
       else value
     with _ -> stealth_min_ou_default)
  | None -> stealth_min_ou_default

let program_deploy_min_ou_from_wire_len encoded_size =
  Z.of_int
    (200_000 + ((((max 0 encoded_size) + 1_023) / 1_024) * 1_000))

let ou_cost tx = match tx.op_type with
  | EncryptOp | DecryptOp -> Z.of_int 3_000
  | PrivateOp | StealthOp ->
    stealth_min_ou_of Sys.getenv_opt
  | ClaimOp -> Z.of_int 3_000
  | RecryptOp -> Z.of_int 5_000
  | ContractDeploy -> Z.of_int 200_000
  | ProgramDeploy ->
    let encoded_size =
      match tx.encrypted_data with
      | Some value -> String.length value
      | None -> 0
    in
    program_deploy_min_ou_from_wire_len encoded_size
  | ContractCall | ProgramExec -> Z.of_int 1_000
  | MultiExec ->
    let base = 1_000 in
    let per_call = 1_000 in
    let count =
      match tx.message with
      | Some msg ->
        (try
           match Yojson.Safe.from_string msg with
           | `List calls -> List.length calls
           | `Assoc fields ->
             (match List.assoc_opt "calls" fields with
              | Some (`List calls) -> List.length calls
              | _ -> 1)
           | _ -> 1
         with _ -> 1)
      | None -> 1
    in
    Z.of_int (base + (per_call * max 1 count))
  | ContractUpgrade -> Z.of_int 200_000
  | CircleDeploy -> Z.of_int 200_000
  | CircleProgramUpdate -> Z.of_int 200_000
  | CircleAssetPut -> circle_asset_min_ou tx.encrypted_data
  | CircleAssetPutEncrypted -> circle_asset_min_ou tx.encrypted_data
  | CircleSealedSlotPut -> circle_asset_min_ou tx.encrypted_data
  | CircleSlotPolicyPut -> Z.of_int 1_000
  | CircleStateDescriptorPut -> Z.of_int 1_000
  | CircleBalanceCellPut -> circle_asset_min_ou tx.encrypted_data
  | CircleRegisterCellPut -> circle_asset_min_ou tx.encrypted_data
  | CircleTransportPolicyPut -> Z.of_int 1_000
  | CircleHfhePolicyPut -> Z.of_int 1_000
  | CircleKeyPolicyPut -> Z.of_int 1_000
  | CircleKeyGrant -> Z.of_int 1_000
  | CircleKeyExtend -> Z.of_int 1_000
  | CircleKeyRevoke -> Z.of_int 1_000
  | CircleKeyErase -> Z.of_int 1_000
  | CircleOutboxOpen -> Z.of_int 3_000
  | CircleRelayClaim -> Z.of_int 3_000
  | CircleRelayCancel -> Z.of_int 3_000
  | CircleIngressCommit -> Z.of_int 3_000
  | CircleCall -> Z.of_int 1_000
  | ValidatorSetUpdate | ValidatorReady
  | ValidatorBond | ValidatorExit | ValidatorWithdraw -> Z.of_int 1_000
  | ValidatorEvidence -> Z.of_int 5_000
  | KeySwitch -> Z.of_int 3_000
  | Standard -> Z.of_int 1_000
  | Op01Burn -> Z.of_int 1_000

let better_fee_rate a b =
  let ca = ou_cost a in
  let cb = ou_cost b in
  Z.gt (Z.mul a.ou cb) (Z.mul b.ou ca)

let cmp_fee_rate_desc a b =
  let ca = ou_cost a in
  let cb = ou_cost b in
  Z.compare (Z.mul b.ou ca) (Z.mul a.ou cb)

let priority_of tx =
  let min_ou = ou_cost tx in
  if Z.leq tx.ou min_ou then Low
  else if Z.leq tx.ou (Z.mul min_ou (Z.of_int 2)) then Normal
  else if Z.leq tx.ou (Z.mul min_ou (Z.of_int 5)) then High
  else Express

let priority_rank = function
  | Express -> 0
  | High -> 1
  | Normal -> 2
  | Low -> 3

let compare_by_priority a b =
  let rank_a = priority_rank (priority_of a) in
  let rank_b = priority_rank (priority_of b) in
  let cmp = compare rank_a rank_b in
  if cmp <> 0 then cmp
  else cmp_fee_rate_desc a b

let opt_json = function None -> `Null | Some v -> `String v

let hash tx =
  `Assoc [
    "from", `String tx.from;
    "to_", `String tx.to_;
    "amount", `String (Z.to_string tx.amount);
    "nonce", `Int tx.nonce;
    "ou", `String (Z.to_string tx.ou);
    "timestamp", `Float tx.timestamp;
    "signature", `String tx.signature;
    "op_type", `String (op_type_to_string tx.op_type);
    "public_key", opt_json tx.public_key;
    "message", opt_json tx.message;
    "encrypted_data", opt_json tx.encrypted_data;
  ] |> Yojson.Safe.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let parse_raw_json s =
  match Yojson.Safe.from_string s |> of_yojson with
  | Ok t -> Some t | _ -> None