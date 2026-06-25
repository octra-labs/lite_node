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

let stealth_effect ~addr fields =
  let ed = encrypted_fields fields in
  if not (has_string ed "send_zero_proof") then
    Poisoned_flow "stealth missing send_zero_proof"
  else if not (has_string ed "amount_commitment") then
    Poisoned_flow "stealth missing amount_commitment"
  else if not (has_string ed "delta_cipher") then
    Poisoned_flow "stealth missing delta_cipher"
  else if string_field fields "from" = Some addr then
    Hidden_commitment ("stealth", -1, Option.get (string_field ed "amount_commitment"))
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
    match string_field ed "amount_commitment" with
    | Some commitment -> Hidden_commitment ("claim", 1, commitment)
    | None -> Hidden_flow "claim"
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

let classify_tx : addr:string -> Yojson.Safe.t -> effect =
  fun ~addr json -> match json with
  | `Assoc fields when not (mentions_addr addr fields) -> Neutral
  | `Assoc fields ->
    let op = op_field fields in
    (match op, string_field fields "from", z_field fields "amount" with
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
  | `String _ -> Poisoned_flow "malformed json"

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

let bytes_of_b64 s =
  try
    let b = Bytes.of_string (Base64.decode_exn s) in
    if Bytes.length b = 32 then Some b else None
  with _ -> None

let public_amount_commitment amount =
  if Z.sign amount < 0 || Z.compare amount (Z.of_int64 Int64.max_int) > 0 then
    None
  else
    Some (Pvac_ffi.pedersen_commit_amount (Z.to_int64 amount) zero_blinding)

let apply_commitment acc = function
  | Neutral
  | Hidden_flow _
  | Poisoned_flow _ -> Some acc
  | Public_encrypt amount ->
    Option.map (Pvac_ffi.pedersen_add acc) (public_amount_commitment amount)
  | Public_decrypt amount ->
    Option.map (Pvac_ffi.pedersen_sub acc) (public_amount_commitment amount)
  | Hidden_commitment (_, sign, commitment) ->
    match bytes_of_b64 commitment with
    | None -> None
    | Some point ->
      if sign >= 0 then Some (Pvac_ffi.pedersen_add acc point)
      else Some (Pvac_ffi.pedersen_sub acc point)

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
  if blockers <> [] then
    let audit_class =
      if List.exists effect_poison effects then Poisoned else Hidden_witness
    in
    {
      audit_class;
      can_public_migrate = false;
      public_net = Some public_net;
      commitment_net;
      blockers;
      effects;
      reason =
        if audit_class = Poisoned then
          "legacy history is poisoned"
        else
          "legacy history needs hidden witness migration";
    }
  else
    if Z.sign public_net < 0 then
      {
        audit_class = Poisoned;
        can_public_migrate = false;
        public_net = Some public_net;
        commitment_net;
        blockers = ["public net is negative"];
        effects;
        reason = "legacy public flow is not a valid non-negative balance";
      }
    else
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