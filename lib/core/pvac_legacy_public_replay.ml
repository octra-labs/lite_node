(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effect =
  | Neutral
  | Public_encrypt of Z.t
  | Public_decrypt of Z.t
  | Hidden_commitment of string * int * string
  | Hidden_flow of string
  | Poisoned_flow of string

type audit_class =
  | Public_clean
  | Hidden_witness
  | Poisoned

type decision = {
  audit_class : audit_class;
  can_public_migrate : bool;
  public_net : Z.t option;
  commitment_net : string option;
  blockers : string list;
  effects : effect list;
  reason : string;
}

let string_of_audit_class = function
  | Public_clean -> "public_clean"
  | Hidden_witness -> "hidden_witness"
  | Poisoned -> "poisoned"

let string_field fields name =
  match List.assoc_opt name fields with
  | Some (`String s) -> Some s
  | _ -> None

let z_field fields name =
  match string_field fields name with
  | Some s ->
    (try Some (Z.of_string s) with _ -> None)
  | None ->
    match List.assoc_opt name fields with
    | Some (`Int n) when n >= 0 -> Some (Z.of_int n)
    | _ -> None

let amount_in_supply_envelope amount =
  Z.sign amount >= 0 && Z.leq amount Denomination.max_supply

let op_field fields =
  match string_field fields "op_type" with
  | Some s -> s
  | None -> "standard"

let mentions_addr addr fields =
  string_field fields "from" = Some addr || string_field fields "to_" = Some addr

let encrypted_fields fields =
  match List.assoc_opt "encrypted_data" fields with
  | Some (`String s) ->
    (try
       match Yojson.Safe.from_string s with
       | `Assoc xs -> xs
       | _ -> []
     with _ -> [])
  | Some (`Assoc xs) -> xs
  | _ -> []

let has_string fields name =
  match string_field fields name with
  | Some s -> String.length s > 0
  | None -> false

let bytes_of_b64 value =
  try
    let bytes = Bytes.of_string (Base64.decode_exn value) in
    if Bytes.length bytes = 32 then Some bytes else None
  with _ -> None

let canonical_commitment value =
  match bytes_of_b64 value with
  | None ->
    None
  | Some point ->
    begin
      try
        ignore
          (Pvac_ffi.pedersen_add
             (Pvac_ffi.pedersen_identity ())
             point);
        Some point
      with _ ->
        None
    end

let stealth_effect ~addr fields =
  let ed = encrypted_fields fields in
  if not (has_string ed "send_zero_proof") then
    Poisoned_flow "stealth missing send_zero_proof"
  else if not (has_string ed "amount_commitment") then
    Poisoned_flow "stealth missing amount_commitment"
  else if not (has_string ed "delta_cipher") then
    Poisoned_flow "stealth missing delta_cipher"
  else if string_field fields "from" = Some addr then
    begin
      match string_field ed "amount_commitment" with
      | Some commitment when Option.is_some (canonical_commitment commitment) ->
        Hidden_commitment ("stealth", -1, commitment)
      | Some _ ->
        Poisoned_flow "stealth invalid amount_commitment"
      | None ->
        Poisoned_flow "stealth missing amount_commitment"
    end
  else
    Neutral

let claim_effect ~addr fields =
  let ed = encrypted_fields fields in
  if not (has_string ed "zero_proof") then
    Poisoned_flow "claim missing zero_proof"
  else if not (has_string ed "claim_cipher") then
    Poisoned_flow "claim missing claim_cipher"
  else if not (List.mem_assoc "output_id" ed) then
    Poisoned_flow "claim missing output_id"
  else if string_field fields "from" = Some addr then
    Hidden_flow "claim"
  else
    Neutral

let hidden_effect ~addr op fields =
  match op with
  | "stealth" -> stealth_effect ~addr fields
  | "claim" -> claim_effect ~addr fields
  | "private" -> Poisoned_flow "private legacy flow is disabled"
  | "key_switch" -> Hidden_flow "key_switch"
  | "recrypt" -> Hidden_flow "recrypt"
  | _ -> Neutral

let classify_tx : addr:string -> [> Yojson.Safe.t ] -> effect =
  fun ~addr json -> match json with
  | `Assoc fields when not (mentions_addr addr fields) -> Neutral
  | `Assoc fields ->
    let op = op_field fields in
    (match op, string_field fields "from", z_field fields "amount" with
    | ("encrypt" | "decrypt"), _, Some amount
        when not (amount_in_supply_envelope amount) ->
      Poisoned_flow (op ^ " amount is outside the supply envelope")
    | "encrypt", Some from_addr, Some amount when from_addr = addr ->
      Public_encrypt amount
    | "decrypt", Some from_addr, Some amount when from_addr = addr ->
      Public_decrypt amount
    | "encrypt", _, None
    | "decrypt", _, None ->
      Poisoned_flow (op ^ " missing amount")
    | "stealth", _, _
    | "claim", _, _
    | "private", _, _
    | "key_switch", _, _
    | "recrypt", _, _ ->
      hidden_effect ~addr op fields
    | _ -> Neutral)
  | `Bool _
  | `Float _
  | `Int _
  | `Intlit _
  | `List _
  | `Null
  | `String _
  | _ -> Poisoned_flow "malformed json"

let effect_blocker = function
  | Hidden_commitment (op, _, _) -> Some ("hidden witness required: " ^ op)
  | Hidden_flow op -> Some ("hidden witness required: " ^ op)
  | Poisoned_flow reason -> Some ("poisoned legacy history: " ^ reason)
  | _ -> None

let effect_poison = function
  | Poisoned_flow _ -> true
  | _ -> false

let effect_hidden = function
  | Hidden_commitment _ -> true
  | Hidden_flow _ -> true
  | _ -> false

let add_effect acc = function
  | Public_encrypt amount -> Z.add acc amount
  | Public_decrypt amount -> Z.sub acc amount
  | _ -> acc

let zero_blinding = Bytes.make 32 '\000'

let b64_of_bytes b =
  Base64.encode_exn (Bytes.to_string b)

let public_amount_commitment amount =
  if Z.sign amount < 0 || Z.compare amount (Z.of_int64 Int64.max_int) > 0 then
    None
  else
    Some (Pvac_ffi.pedersen_commit_amount (Z.to_int64 amount) zero_blinding)

let point_op op left right =
  try Some (op left right) with _ -> None

let apply_commitment acc = function
  | Neutral
  | Hidden_flow _
  | Poisoned_flow _ -> Some acc
  | Public_encrypt amount ->
    begin
      match public_amount_commitment amount with
      | None -> None
      | Some point -> point_op Pvac_ffi.pedersen_add acc point
    end
  | Public_decrypt amount ->
    begin
      match public_amount_commitment amount with
      | None -> None
      | Some point -> point_op Pvac_ffi.pedersen_sub acc point
    end
  | Hidden_commitment (_, sign, commitment) ->
    match canonical_commitment commitment with
    | None -> None
    | Some point ->
      if sign >= 0 then point_op Pvac_ffi.pedersen_add acc point
      else point_op Pvac_ffi.pedersen_sub acc point

let replay_commitment effects =
  let start = Pvac_ffi.pedersen_identity () in
  let rec loop acc = function
    | [] -> Some (b64_of_bytes acc)
    | effect :: rest ->
      match apply_commitment acc effect with
      | None -> None
      | Some next -> loop next rest
  in
  loop start effects

let replay_history ~addr txs =
  let effects = List.map (classify_tx ~addr) txs in
  let blockers = List.filter_map effect_blocker effects in
  let public_net = List.fold_left add_effect Z.zero effects in
  let commitment_net = replay_commitment effects in
  let net_violation =
    if Z.sign public_net < 0 then
      Some
        ( "public net is negative",
          "legacy public flow is not a valid non-negative balance" )
    else if Z.gt public_net Denomination.max_supply then
      Some
        ( "public net exceeds the supply envelope",
          "legacy public flow exceeds the supply envelope" )
    else
      None
  in
  match net_violation with
  | Some (blocker, reason) ->
    {
      audit_class = Poisoned;
      can_public_migrate = false;
      public_net = None;
      commitment_net = None;
      blockers = blockers @ [blocker];
      effects;
      reason;
    }
  | None when blockers <> [] ->
    let audit_class =
      if List.exists effect_poison effects then Poisoned else Hidden_witness
    in
    let public_net, commitment_net =
      if audit_class = Poisoned then None, None
      else Some public_net, commitment_net
    in
    {
      audit_class;
      can_public_migrate = false;
      public_net;
      commitment_net;
      blockers;
      effects;
      reason =
        if audit_class = Poisoned then
          "legacy history is poisoned"
        else
          "legacy history needs hidden witness migration";
    }
  | None ->
    {
      audit_class =
        if List.exists effect_hidden effects then Hidden_witness else Public_clean;
      can_public_migrate = true;
      public_net = Some public_net;
      commitment_net;
      blockers = [];
      effects;
      reason = "legacy public flow is reconstructable";
    }