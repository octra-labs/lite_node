(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'a parsed = ('a, string) result

let bind r f =
  match r with
  | Ok v -> f v
  | Error e -> Error e

let ( let* ) = bind

let field name = function
  | `Assoc xs ->
    begin match List.assoc_opt name xs with
    | Some v -> Ok v
    | None -> Error ("missing field: " ^ name)
    end
  | _ -> Error "object expected"

let str name json =
  let* v = field name json in
  match v with
  | `String s -> Ok s
  | _ -> Error ("string expected: " ^ name)

let intv name json =
  let* v = field name json in
  match v with
  | `Int n -> Ok n
  | `Intlit s ->
    begin match int_of_string_opt s with
    | Some n -> Ok n
    | None -> Error ("int expected: " ^ name)
    end
  | _ -> Error ("int expected: " ^ name)

let i64 name json =
  let* v = field name json in
  match v with
  | `String s ->
    begin match Int64.of_string_opt s with
    | Some n -> Ok n
    | None -> Error ("int64 expected: " ^ name)
    end
  | `Int n -> Ok (Int64.of_int n)
  | `Intlit s ->
    begin match Int64.of_string_opt s with
    | Some n -> Ok n
    | None -> Error ("int64 expected: " ^ name)
    end
  | _ -> Error ("int64 expected: " ^ name)

let z name json =
  let* v = field name json in
  match v with
  | `String s
  | `Intlit s ->
    begin
      try Ok (Z.of_string s)
      with _ -> Error ("integer expected: " ^ name)
    end
  | `Int n -> Ok (Z.of_int n)
  | _ -> Error ("integer expected: " ^ name)

let boolv name json =
  let* v = field name json in
  match v with
  | `Bool b -> Ok b
  | _ -> Error ("bool expected: " ^ name)

let opt_str name json =
  let* v = field name json in
  match v with
  | `Null -> Ok None
  | `String s -> Ok (Some s)
  | _ -> Error ("optional string expected: " ^ name)

let list name item json =
  let* v = field name json in
  match v with
  | `List xs ->
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | x :: rest ->
        let* v = item x in
        go (v :: acc) rest in
    go [] xs
  | _ -> Error ("list expected: " ^ name)

let hex_char = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let raw32 name hex =
  if String.length hex <> 64 || not (String.for_all hex_char hex) then
    Error ("hash32 expected: " ^ name)
  else
    Ok (String.init 32 (fun i ->
      let hi = hex_value hex.[i * 2] in
      let lo = hex_value hex.[i * 2 + 1] in
      Char.chr ((hi lsl 4) lor lo)))

let opt_raw32 name = function
  | None -> Ok None
  | Some s ->
    let* raw = raw32 name s in
    Ok (Some raw)

let b64 name s =
  match Base64.decode s with
  | Ok raw -> Ok raw
  | Error _ -> Error ("base64 expected: " ^ name)

let version expected json =
  let* got = str "version" json in
  if got = expected then Ok () else Error ("version mismatch: " ^ got)

let rpc_result json =
  match json with
  | `Assoc xs ->
    begin match List.assoc_opt "error" xs with
    | Some (`Null) | None ->
      begin match List.assoc_opt "result" xs with
      | Some v -> Ok v
      | None -> Error "missing rpc result"
      end
    | Some err -> Error ("rpc error: " ^ Yojson.Safe.to_string err)
    end
  | _ -> Error "rpc response object expected"

let signed_root json =
  let* () = version "octra-signed-root-v1" json in
  let* chain_id = str "chain_id" json in
  let* validator_addr = str "validator_addr" json in
  let* head_epoch = i64 "head_epoch" json in
  let* state_root_hex = str "state_root" json in
  let* state_root = raw32 "state_root" state_root_hex in
  let* ledger_state_root = opt_str "ledger_state_root" json in
  let* epoch_index_root_hex = opt_str "epoch_index_root" json in
  let* epoch_index_root = opt_raw32 "epoch_index_root" epoch_index_root_hex in
  let* txid_hi = i64 "txid_hi" json in
  let* config_hash_hex = str "config_hash" json in
  let* config_hash = raw32 "config_hash" config_hash_hex in
  let* signature_b64 = str "signature" json in
  let* signature = b64 "signature" signature_b64 in
  Ok C_light_root.{
    chain_id;
    validator_addr;
    head_epoch;
    state_root;
    ledger_state_root;
    epoch_index_root;
    txid_hi;
    config_hash;
    signature;
  }

let validator ~weighted json =
  let* address = str "address" json in
  let* pubkey_b64 = str "pubkey" json in
  let* pubkey = b64 "pubkey" pubkey_b64 in
  let* weight =
    if weighted then z "weight" json
    else Ok Z.one
  in
  Ok C_light_validator_set.{ address; pubkey; weight }

let scheduled ~weighted_proof json =
  match json with
  | `Null -> Ok None
  | _ ->
    let* activate_epoch = i64 "activate_epoch" json in
    let* weighted =
      if weighted_proof then boolv "weighted" json
      else Ok false
    in
    let* validators = list "validators" (validator ~weighted) json in
    Ok (Some C_light_validator_set.{ activate_epoch; validators; weighted })

let validator_set json =
  let* proof_version = str "version" json in
  let* program_trust_hash, runtime_profile_hash, weighted_proof =
    match proof_version with
    | "octra-validator-set-proof-v1" -> Ok (None, None, false)
    | "octra-validator-set-proof-v2" ->
      let* hash_hex = str "program_trust_hash" json in
      let* hash = raw32 "program_trust_hash" hash_hex in
      Ok (Some hash, None, false)
    | "octra-validator-set-proof-v3" ->
      let* trust_hex = opt_str "program_trust_hash" json in
      let* program_trust_hash = opt_raw32 "program_trust_hash" trust_hex in
      let* profile_hex = str "runtime_profile_hash" json in
      let* runtime_profile_hash = raw32 "runtime_profile_hash" profile_hex in
      Ok (program_trust_hash, Some runtime_profile_hash, false)
    | "octra-validator-set-proof-v4" ->
      let* trust_hex = opt_str "program_trust_hash" json in
      let* program_trust_hash = opt_raw32 "program_trust_hash" trust_hex in
      let* profile_hex = opt_str "runtime_profile_hash" json in
      let* runtime_profile_hash = opt_raw32 "runtime_profile_hash" profile_hex in
      Ok (program_trust_hash, runtime_profile_hash, true)
    | other -> Error ("version mismatch: " ^ other)
  in
  let* weighted =
    if weighted_proof then boolv "weighted" json
    else Ok false
  in
  let* chain_id = str "chain_id" json in
  let* config_hash_hex = str "config_hash" json in
  let* config_hash = raw32 "config_hash" config_hash_hex in
  let* validator_set_hash_hex = str "validator_set_hash" json in
  let* validator_set_hash = raw32 "validator_set_hash" validator_set_hash_hex in
  let* n = intv "n" json in
  let* f = intv "f" json in
  let* quorum = intv "quorum" json in
  let* total_weight =
    if weighted then z "total_weight" json
    else Ok (Z.of_int n)
  in
  let* quorum_weight =
    if weighted then z "quorum_weight" json
    else Ok (Z.of_int quorum)
  in
  let* validators = list "validators" (validator ~weighted) json in
  let* scheduled_json = field "scheduled" json in
  let* scheduled = scheduled ~weighted_proof scheduled_json in
  Ok C_light_validator_set.{
    chain_id;
    config_hash;
    validator_set_hash;
    n;
    f;
    quorum;
    total_weight;
    quorum_weight;
    weighted;
    validators;
    scheduled;
    program_trust_hash;
    runtime_profile_hash;
  }

let epoch json =
  let* chain_id = str "chain_id" json in
  let* epoch_id = i64 "epoch_id" json in
  let* tx_hashes = list "tx_hashes" (function
    | `String s -> Ok (String.lowercase_ascii s)
    | _ -> Error "tx hash string expected") json in
  let* start_txid = i64 "start_txid" json in
  let* tx_count = intv "tx_count" json in
  let* prev_epoch_index_root =
    str "prev_epoch_index_root" json in
  let* epoch_index_root_text =
    str "epoch_index_root" json in
  Ok C_light_epoch.{
    chain_id;
    epoch_id;
    tx_hashes;
    start_txid;
    tx_count;
    prev_epoch_index_root = String.lowercase_ascii prev_epoch_index_root;
    epoch_index_root = String.lowercase_ascii epoch_index_root_text;
  }

let epoch_proof json =
  let* proof_kind = str "proof_kind" json in
  if proof_kind <> "epoch_inclusion" then
    Error ("proof kind mismatch: " ^ proof_kind)
  else
    let* inner = field "epoch" json in
    epoch inner

let account json =
  let* proof_kind = str "proof_kind" json in
  if proof_kind <> "account_witness_v1" then Error ("proof kind mismatch: " ^ proof_kind)
  else
    let* _inclusion = boolv "inclusion" json in
    let* chain_id = str "chain_id" json in
    let* address = str "address" json in
    let* head_epoch = i64 "head_epoch" json in
    let* state_root_hex = str "state_root" json in
    let* state_root = raw32 "state_root" state_root_hex in
    let* balance = str "balance" json in
    let* nonce = intv "nonce" json in
    let* public_key = opt_str "public_key" json in
    let* encrypted_balance = opt_str "encrypted_balance" json in
    let* decrypt_allowance = str "decrypt_allowance" json in
    let* account_hash_hex = str "account_hash" json in
    let* account_hash = raw32 "account_hash" account_hash_hex in
    Ok C_light_account.{
      chain_id;
      address;
      head_epoch;
      state_root;
      balance;
      nonce;
      public_key;
      encrypted_balance;
      decrypt_allowance;
      account_hash;
    }

type account_irmin_proof = {
  proof_kind : string;
  path : string list;
  ledger_state_root : string;
  exists : bool;
  proof : string;
}

type account_proof = {
  root : C_light_root.t;
  exists : bool;
  account : C_light_account.t option;
  irmin_proof : account_irmin_proof;
}

let account_irmin_proof json =
  let* proof_kind = str "proof_kind" json in
  if proof_kind <> "irmin_account_path_v1" && proof_kind <> "irmin_account_tree" then
    Error ("proof kind mismatch: " ^ proof_kind)
  else
    let* path = list "path" (function
      | `String s -> Ok s
      | _ -> Error "path string expected") json in
    let path_ok =
      match proof_kind, path with
      | "irmin_account_path_v1", ["accounts"; address; "data"] ->
        address <> ""
      | "irmin_account_tree", ["accounts"; address] -> address <> ""
      | _ -> false
    in
    if not path_ok then Error "account proof path mismatch"
    else
      let* ledger_state_root = str "ledger_state_root" json in
      let* exists = boolv "exists" json in
      let* proof = str "proof" json in
      Ok { proof_kind; path; ledger_state_root; exists; proof }

let account_proof json =
  let* version = str "version" json in
  if version <> "octra-account-proof-v2" && version <> "octra-account-proof-v1" then
    Error ("version mismatch: " ^ version)
  else
    let* root_json = field "root" json in
    let* root = signed_root root_json in
    let* exists =
      match version with
      | "octra-account-proof-v2" -> boolv "exists" json
      | _ -> Ok true in
    let* account_json = field "account" json in
    let* account =
      match account_json with
      | `Null -> Ok None
      | _ ->
        let* account = account account_json in
        Ok (Some account) in
    let* irmin_json = field "irmin_proof" json in
    let* irmin_proof = account_irmin_proof irmin_json in
    Ok { root; exists; account; irmin_proof }