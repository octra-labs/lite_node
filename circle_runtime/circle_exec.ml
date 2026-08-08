(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Contract = Octra_vm.Contract
module ContractVM = Octra_vm.Contract_vm

exception Execution_unavailable of string

type call_result = {
  receipt : Contract.exec_result;
  storage_tbl : (string, string) Hashtbl.t;
  baseline_storage_tbl : (string, string) Hashtbl.t;
  spawns : Octra_core.Circle_wasm_host.spawn list;
  assets : Octra_core.Circle_wasm_host.asset_put list;
  encrypted_assets : Octra_core.Circle_wasm_host.encrypted_asset_put list;
  caller : string;
  tx_hash : string;
  hfhe_binding : hfhe_binding;
}

and hfhe_binding = {
  circle_id : string;
  code_hash : string;
  stable_root : string;
  public_reads_hash : string;
  context_hash : string;
  transcript : Octra_core.Circle_hfhe_transcript.entry list;
}

type runtime_hfhe_details = {
  exec_ctx : ContractVM.exec_ctx;
  policy : Octra_core.Circle_hfhe_policy.t;
  owner : string;
  active_relay : string option;
}

type checked_spawn = {
  src : Octra_core.Circle_deploy.source;
  payload : Octra_core.Circles.deploy_payload;
  prepared : Octra_core.Circle_deploy.prepared;
}

type checked_asset = {
  circle_id : string;
  meta : Octra_core.Circles.asset_meta;
  body_b64 : string;
  usage_bytes : int64;
}

type checked_encrypted_asset = {
  circle_id : string;
  meta : Octra_core.Circles.asset_meta;
  ciphertext_b64 : string;
  usage_bytes : int64;
}

type slot_policy = {
  delivery_key_id : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  tombstone : bool;
  revoked : bool;
}

type wasm_view_storage_cache_entry = {
  storage_tbl : (string, string) Hashtbl.t;
  weight : int;
  mutable touched_at : float;
}

let wasm_view_storage_cache : (string, wasm_view_storage_cache_entry) Hashtbl.t =
  Hashtbl.create 8

let wasm_view_storage_cache_key circle_id stable_root =
  circle_id ^ ":" ^ stable_root

let wasm_view_storage_cache_ttl_secs = 300.0
let wasm_view_storage_cache_limit = 16
let wasm_view_storage_cache_byte_limit = 48 * 1024 * 1024
let wasm_view_storage_cache_bytes = ref 0

let wasm_view_storage_weight storage_tbl =
  Hashtbl.fold
    (fun key value total ->
      total + String.length key + String.length value + 64)
    storage_tbl
    0

let remove_wasm_view_storage_cache key =
  match Hashtbl.find_opt wasm_view_storage_cache key with
  | None ->
    ()
  | Some entry ->
    Hashtbl.remove wasm_view_storage_cache key;
    wasm_view_storage_cache_bytes :=
      max 0 (!wasm_view_storage_cache_bytes - entry.weight);
    Octra_core.Circle_wasm_host.remove_storage_cache key

let oldest_wasm_view_storage_cache_key () =
  Hashtbl.fold
    (fun key entry oldest ->
      match oldest with
      | None -> Some (key, entry.touched_at)
      | Some (_, touched_at) when entry.touched_at < touched_at ->
        Some (key, entry.touched_at)
      | Some _ ->
        oldest)
    wasm_view_storage_cache
    None
  |> Option.map fst

let prune_wasm_view_storage_cache incoming_count incoming_weight =
  let now = Unix.gettimeofday () in
  let stale =
    Hashtbl.fold
      (fun key entry keys ->
        if now -. entry.touched_at > wasm_view_storage_cache_ttl_secs then
          key :: keys
        else
          keys)
      wasm_view_storage_cache
      [] in
  List.iter remove_wasm_view_storage_cache stale;
  while
    Hashtbl.length wasm_view_storage_cache + incoming_count
    > wasm_view_storage_cache_limit
    || !wasm_view_storage_cache_bytes + incoming_weight
       > wasm_view_storage_cache_byte_limit
  do
    match oldest_wasm_view_storage_cache_key () with
    | None ->
      wasm_view_storage_cache_bytes := 0
    | Some key ->
      remove_wasm_view_storage_cache key
  done

let drop_previous_wasm_view_storage circle_id keep_key =
  let prefix = circle_id ^ ":" in
  let prefix_len = String.length prefix in
  let stale =
    Hashtbl.fold
      (fun key _ keys ->
        if
          not (String.equal key keep_key)
          && String.length key >= prefix_len
          && String.sub key 0 prefix_len = prefix
        then
          key :: keys
        else
          keys)
      wasm_view_storage_cache
      [] in
  List.iter remove_wasm_view_storage_cache stale

let cache_wasm_view_storage circle_id stable_root storage_tbl =
  let cache_key = wasm_view_storage_cache_key circle_id stable_root in
  let weight = wasm_view_storage_weight storage_tbl in
  drop_previous_wasm_view_storage circle_id cache_key;
  remove_wasm_view_storage_cache cache_key;
  if weight <= wasm_view_storage_cache_byte_limit then begin
    prune_wasm_view_storage_cache 1 weight;
    Hashtbl.replace
      wasm_view_storage_cache
      cache_key
      {
        storage_tbl;
        weight;
        touched_at = Unix.gettimeofday ();
      };
    wasm_view_storage_cache_bytes := !wasm_view_storage_cache_bytes + weight
  end;
  cache_key

let wasm_view_storage_cache_stats () =
  Hashtbl.length wasm_view_storage_cache, !wasm_view_storage_cache_bytes

let wasm_view_storage_cache_mem circle_id stable_root =
  wasm_view_storage_cache_key circle_id stable_root
  |> Hashtbl.mem wasm_view_storage_cache

let clear_wasm_view_storage_cache () =
  let keys = Hashtbl.fold (fun key _ keys -> key :: keys) wasm_view_storage_cache [] in
  List.iter remove_wasm_view_storage_cache keys;
  wasm_view_storage_cache_bytes := 0

type preview_session_entry = {
  result_csv : string;
  result_tokens : int list;
  updated_at : float;
}

let preview_session_cache : (string, preview_session_entry) Hashtbl.t =
  Hashtbl.create 128

let preview_session_inflight : (string, unit) Hashtbl.t =
  Hashtbl.create 64

let preview_session_ttl_secs = 300.0
let preview_session_cache_limit = 512
let preview_max_context_tokens = 64
let preview_prefetch_tokens = 8
let spawn_cap = 4
let asset_effect_cap = 4
let asset_effect_raw_cap = 1_048_576
let asset_effect_body_b64_cap = ((asset_effect_raw_cap + 2) / 3) * 4

let preview_prune_cache () =
  let now = Unix.gettimeofday () in
  let stale = ref [] in
  Hashtbl.iter
    (fun key entry ->
      if now -. entry.updated_at > preview_session_ttl_secs then
        stale := key :: !stale)
    preview_session_cache;
  List.iter (fun key -> Hashtbl.remove preview_session_cache key) !stale;
  if Hashtbl.length preview_session_cache > preview_session_cache_limit then begin
    Hashtbl.reset preview_session_cache;
    Hashtbl.reset preview_session_inflight
  end

let int_of_string_opt value =
  try Some (int_of_string value) with _ -> None

let int_of_yojson = function
  | `Int value -> Some value
  | `Intlit value
  | `String value ->
    int_of_string_opt value
  | _ ->
    None

let parse_csv_tokens csv =
  let rec loop acc = function
    | [] ->
      begin
        match List.rev acc with
        | [] -> None
        | values -> Some values
      end
    | raw :: rest ->
      let part = String.trim raw in
      if String.equal part "" then
        loop acc rest
      else
        begin
          match int_of_string_opt part with
          | Some value when value >= 0 ->
            loop (value :: acc) rest
          | _ ->
            None
        end in
  loop [] (String.split_on_char ',' csv)

let csv_of_tokens tokens =
  String.concat "," (List.map string_of_int tokens)

let rec take_tokens n tokens =
  if n <= 0 then
    []
  else
    match tokens with
    | [] -> []
    | value :: rest ->
      value :: take_tokens (n - 1) rest

let rec drop_tokens n tokens =
  if n <= 0 then
    tokens
  else
    match tokens with
    | [] -> []
    | _ :: rest ->
      drop_tokens (n - 1) rest

let append_csv left right =
  if String.equal left "" then
    right
  else if String.equal right "" then
    left
  else
    left ^ "," ^ right

let preview_request_of_call method_name params =
  match method_name, params with
  | "complete_preview", [`String prompt_csv; n_json] ->
    begin
      match parse_csv_tokens prompt_csv, int_of_yojson n_json with
      | Some prompt_tokens, Some n_tokens when n_tokens >= 1 && n_tokens <= 16 ->
        Some (csv_of_tokens prompt_tokens, prompt_tokens, n_tokens)
      | _ ->
        None
    end
  | _ ->
    None

let preview_cache_key circle_id caller prompt_csv =
  circle_id ^ "|" ^ caller ^ "|" ^ prompt_csv

let preview_cache_store key result_csv =
  match parse_csv_tokens result_csv with
  | None ->
    ()
  | Some result_tokens ->
    Hashtbl.replace
      preview_session_cache
      key
      {
        result_csv = csv_of_tokens result_tokens;
        result_tokens;
        updated_at = Unix.gettimeofday ();
      }

let preview_cache_lookup key n_tokens =
  preview_prune_cache ();
  match Hashtbl.find_opt preview_session_cache key with
  | Some entry when List.length entry.result_tokens >= n_tokens ->
    Some (csv_of_tokens (take_tokens n_tokens entry.result_tokens))
  | _ ->
    None

let preview_receipt result_csv = {
  Contract.success = true;
  return_value = Some (ContractVM.VString result_csv);
  effort_used = 0;
  events = [];
  error = None;
  storage_writes = 0;
}

let preview_result_csv (receipt : Contract.exec_result) =
  match receipt.success, receipt.return_value with
  | true, Some (ContractVM.VString value) ->
    Some value
  | _ ->
    None

let load_wasm_view_storage_cached store circle_id stable_root =
  let cache_key = wasm_view_storage_cache_key circle_id stable_root in
  prune_wasm_view_storage_cache 0 0;
  match Hashtbl.find_opt wasm_view_storage_cache cache_key with
  | Some entry ->
    entry.touched_at <- Unix.gettimeofday ();
    Lwt.return (Ok (cache_key, entry.storage_tbl))
  | None ->
    let* storage_result = Octra_core.Store_irmin.load_circle_stable_storage store circle_id in
    begin
      match storage_result with
      | Error _ as e ->
        Lwt.return e
      | Ok storage_tbl ->
        ignore (cache_wasm_view_storage circle_id stable_root storage_tbl);
        Lwt.return (Ok (cache_key, storage_tbl))
    end

let failed_receipt error = {
  Contract.success = false;
  return_value = None;
  effort_used = 0;
  events = [];
  error = Some error;
  storage_writes = 0;
}

let failed_call_result error = {
  receipt = failed_receipt error;
  storage_tbl = Hashtbl.create 0;
  baseline_storage_tbl = Hashtbl.create 0;
  spawns = [];
  assets = [];
  encrypted_assets = [];
  caller = "";
  tx_hash = "";
  hfhe_binding = {
    circle_id = "";
    code_hash = String.make 64 '0';
    stable_root = String.make 64 '0';
    public_reads_hash = String.make 64 '0';
    context_hash = String.make 64 '0';
    transcript = [];
  };
}

let hash_json domain value =
  Digestif.SHA256.digest_string
    (domain ^ "\000" ^ Yojson.Safe.to_string value)
  |> Digestif.SHA256.to_hex

let public_reads_hash snapshots =
  snapshots
  |> List.map Octra_core.Circle_wasm_public_read.yojson_of_snapshot
  |> fun values -> hash_json "octra:circle_public_reads:v1" (`List values)

let hfhe_context_hash caps pubkeys active_key =
  let caps =
    caps
    |> List.sort_uniq String.compare
    |> List.map (fun value -> `String value)
  in
  let pubkeys =
    pubkeys
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> List.map (fun (addr, pubkey_b64) ->
      `Assoc [
        "addr", `String addr;
        "pubkey_b64", `String pubkey_b64;
      ])
  in
  let active_key =
    match active_key with
    | Some (key_id, pubkey_b64, _) ->
      `Assoc [
        "key_id", `String key_id;
        "pubkey_b64", `String pubkey_b64;
      ]
    | None -> `Null
  in
  hash_json
    "octra:circle_hfhe_context:v1"
    (`Assoc [
      "caps", `List caps;
      "pubkeys", `List pubkeys;
      "active_key", active_key;
    ])

let hfhe_binding loaded circle_id public_reads_hash context_hash transcript = {
  circle_id;
  code_hash = loaded.Circle_program.info.code_hash;
  stable_root = loaded.info.stable_root;
  public_reads_hash;
  context_hash;
  transcript;
}

let hfhe_binding_equal (left : hfhe_binding) (right : hfhe_binding) =
  String.equal left.circle_id right.circle_id
  && String.equal left.code_hash right.code_hash
  && String.equal left.stable_root right.stable_root
  && String.equal left.public_reads_hash right.public_reads_hash
  && String.equal left.context_hash right.context_hash
  && left.transcript = right.transcript

let receipt_mode = function
  | Octra_core.Circle_hfhe_transcript.Direct -> false
  | Octra_core.Circle_hfhe_transcript.Capture
  | Octra_core.Circle_hfhe_transcript.Consume _ -> true

let without_hfhe_verifiers (ctx : ContractVM.exec_ctx) =
  let allow = ctx.allow_fhe_capability in
  {
    ctx with
    allow_fhe_capability = function
      | ContractVM.Fhe_verify_zero_cap
      | Fhe_verify_range_cap
      | Fhe_verify_bound_cap -> false
      | capability -> allow capability;
  }

let string_has_prefix prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal (String.sub value 0 prefix_len) prefix

let wasm_method_needs_hfhe method_name =
  string_has_prefix "fhe_" method_name

let wasm_view_method_timing_enabled method_name =
  match method_name with
  | "version"
  | "prepare_preview"
  | "complete_preview" ->
    true
  | _ ->
    false

let vm_value_of_wasm_response = function
  | Octra_core.Circle_wasm_codec.Resp_null ->
    ContractVM.VString ""
  | Octra_core.Circle_wasm_codec.Resp_bool b ->
    ContractVM.VBool b
  | Octra_core.Circle_wasm_codec.Resp_int value ->
    ContractVM.VInt (Z.of_string value)
  | Octra_core.Circle_wasm_codec.Resp_string value ->
    ContractVM.VString value

let load_slot_policy store circle_id path_key =
  let* delivery_key_id =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.slot_policy_delivery_key_key path_key;
    ] in
  let* activate_after_epoch =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.slot_policy_activate_after_key path_key;
      Octra_core.Circles.mailbox_activate_after_key path_key;
    ] in
  let* expire_after_epoch =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.slot_policy_expire_after_key path_key;
      Octra_core.Circles.mailbox_expire_after_key path_key;
    ] in
  let* tombstone =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.slot_policy_tombstone_key path_key;
      Octra_core.Circles.mailbox_tombstone_key path_key;
    ] in
  let* revoked =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.slot_policy_revoked_key path_key;
      Octra_core.Circles.mailbox_revoked_key path_key;
    ] in
  Lwt.return {
    delivery_key_id = Octra_core.Circle_policy_value.string_value delivery_key_id;
    activate_after_epoch = Octra_core.Circle_policy_value.int64_value activate_after_epoch;
    expire_after_epoch = Octra_core.Circle_policy_value.int64_value expire_after_epoch;
    tombstone = Octra_core.Circle_policy_value.bool_value tombstone;
    revoked = Octra_core.Circle_policy_value.bool_value revoked;
  }

let load_state_policy store circle_id path_key =
  let* delivery_key_id =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.state_policy_delivery_key_key path_key;
    ] in
  let* activate_after_epoch =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.state_policy_activate_after_key path_key;
    ] in
  let* expire_after_epoch =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.state_policy_expire_after_key path_key;
    ] in
  let* tombstone =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.state_policy_tombstone_key path_key;
    ] in
  let* revoked =
    Octra_core.Circle_policy_store.read_first_inline_value store circle_id [
      Octra_core.Circles.state_policy_revoked_key path_key;
    ] in
  Lwt.return {
    delivery_key_id = Octra_core.Circle_policy_value.string_value delivery_key_id;
    activate_after_epoch = Octra_core.Circle_policy_value.int64_value activate_after_epoch;
    expire_after_epoch = Octra_core.Circle_policy_value.int64_value expire_after_epoch;
    tombstone = Octra_core.Circle_policy_value.bool_value tombstone;
    revoked = Octra_core.Circle_policy_value.bool_value revoked;
  }

let load_key_policy store circle_id key_id =
  Octra_core.Circle_policy_store.load_key_policy store circle_id key_id

let asset_visible_now store circle_id current_epoch (meta : Octra_core.Circles.asset_meta) =
  let* policy =
    match meta.Octra_core.Circles.locator_mode with
    | Octra_core.Circles.State_locator ->
      load_state_policy store circle_id meta.path_key
    | Path_locator
    | Slot_locator ->
      load_slot_policy store circle_id meta.path_key in
  let activate_after_epoch =
    match policy.activate_after_epoch with
    | Some epoch -> Some epoch
    | None -> meta.Octra_core.Circles.activate_after_epoch
  in
  let expire_after_epoch =
    match policy.expire_after_epoch with
    | Some epoch -> Some epoch
    | None -> meta.Octra_core.Circles.expire_after_epoch
  in
  let tombstoned = policy.tombstone || policy.revoked in
  let activate_ok =
    match activate_after_epoch with
    | Some activate_after_epoch -> Int64.compare current_epoch activate_after_epoch >= 0
    | None -> true
  in
  let expire_ok =
    match expire_after_epoch with
    | Some expire_after_epoch -> Int64.compare current_epoch expire_after_epoch < 0
    | None -> true
  in
  let* key_live =
    match policy.delivery_key_id, meta.Octra_core.Circles.key_id with
    | Some key_id, _ ->
      let* key_policy = load_key_policy store circle_id key_id in
      Lwt.return (Octra_core.Circle_key_policy.live key_policy current_epoch)
    | None, Some key_id ->
      let* key_policy = load_key_policy store circle_id key_id in
      Lwt.return (Octra_core.Circle_key_policy.live key_policy current_epoch)
    | None, None ->
      Lwt.return true
  in
  Lwt.return (not tombstoned && activate_ok && expire_ok && key_live)

let resolve_runtime_active_relay_id store circle_id current_epoch transport_policy intent_id_opt relay_id_opt =
  match intent_id_opt, relay_id_opt with
  | Some intent_id, Some relay_id ->
    let* active_claims =
      Octra_core.Circle_transport_state.active_outbox_claims
        store
        circle_id
        intent_id
        current_epoch in
    begin
      match Octra_core.Circle_transport_claim_set.find_claim_by_relay active_claims relay_id with
      | Some _ when Octra_core.Circle_transport_quorum.relay_allowed transport_policy active_claims relay_id ->
        Lwt.return (Some relay_id)
      | _ ->
        Lwt.return_none
    end
  | _ ->
    Lwt.return_none

let runtime_key_policy_live store circle_id current_epoch key_id_opt =
  match key_id_opt with
  | Some key_id ->
    let* key_policy =
      Octra_core.Circle_policy_store.load_key_policy store circle_id key_id in
    Lwt.return
      (Octra_core.Circle_key_policy.live
         key_policy
         (Int64.of_int current_epoch))
  | None ->
    Lwt.return false

let restrict_runtime_exec_ctx (ctx : ContractVM.exec_ctx) =
  {
    ctx with
    do_transfer = (fun _ _ _ -> false);
    call_contract = (fun _ _ _ _ _ -> Error "circle runtime xcall disabled");
    deploy_contract = (fun _ _ _ _ _ -> Error "circle runtime spawn disabled");
  }

let with_circle_spawn ctx circle_id caller spawns =
  let deploy_contract parent payload_json _nonce _depth params =
    if not (String.equal parent circle_id) then
      Error "circle spawn parent mismatch"
    else if String.equal ctx.ContractVM.tx_hash "" then
      Error "circle spawn requires transaction hash"
    else if List.length !spawns >= spawn_cap then
      Error "circle spawn cap exceeded"
    else
      match params with
      | [ContractVM.VString owner_raw] ->
        begin
          match Octra_core.Circles.spawn_owner_of_string owner_raw with
          | Error error -> Error error
          | Ok owner_mode ->
            begin
              match Octra_core.Circle_deploy.decode_spawn_payload_json payload_json with
              | Error (_, error) -> Error error
              | Ok payload ->
                let spawn_nonce = List.length !spawns in
                let source =
                  Octra_core.Circle_deploy.Spawn
                    {
                      parent = circle_id;
                      caller;
                      tx_hash = ctx.tx_hash;
                      spawn_nonce;
                      owner_mode;
                      payload_json;
                    }
                in
                begin
                  match Octra_core.Circle_deploy.prepare source payload with
                  | Error (_, error) -> Error error
                  | Ok prepared ->
                    let spawn =
                      {
                        Octra_core.Circle_wasm_host.circle_id = prepared.circle_id;
                        owner_mode;
                        spawn_nonce;
                        payload_json;
                      }
                    in
                    spawns := !spawns @ [spawn];
                    Ok
                      {
                        ContractVM.spawned_addr = prepared.circle_id;
                        effort_used = 0;
                        events = [];
                      }
                end
            end
        end
      | _ -> Error "circle spawn owner mode missing"
  in
  { ctx with ContractVM.deploy_contract }

let empty_runtime_hfhe_details (ctx : ContractVM.exec_ctx) owner = {
  exec_ctx =
    {
      (restrict_runtime_exec_ctx ctx) with
      get_fhe_pubkey = (fun _ -> None);
      get_fhe_keypair = (fun _ -> None);
      allow_fhe_capability = (fun _ -> false);
    };
  policy = Octra_core.Circle_hfhe_policy.default;
  owner;
  active_relay = None;
}

let load_runtime_hfhe_ctx ?(receipt_bound=false)
    (ctx : ContractVM.exec_ctx) store circle_id caller =
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None ->
    Lwt.return (Error "circle not found")
  | Some info ->
    let runtime_ctx = restrict_runtime_exec_ctx ctx in
    let* policy = Octra_core.Circle_policy_store.load_hfhe_policy store circle_id in
    let* transport_policy = Octra_core.Circle_policy_store.load_transport_policy store circle_id in
    let owner = info.owner in
    let* active_relay =
      resolve_runtime_active_relay_id
        store
        circle_id
        runtime_ctx.current_epoch
        transport_policy
        runtime_ctx.circle_hfhe_intent_id
        runtime_ctx.circle_hfhe_active_relay_id in
    let* key_policy_live =
      runtime_key_policy_live store circle_id runtime_ctx.current_epoch runtime_ctx.circle_hfhe_key_id in
    let key_policy_satisfied =
      not policy.require_live_key_policy || key_policy_live in
    let proof_binding_satisfied =
      receipt_bound || not policy.require_receipt_transport_binding in
    let get_fhe_pubkey requested_addr =
      if
        key_policy_satisfied
        && Octra_core.Circle_hfhe_policy.load_pk_allowed_with_transport
             policy
             ~owner
             ~caller
             ~requested_addr
             ~active_relay
      then
        ctx.get_fhe_pubkey requested_addr
      else
        None
    in
    let get_fhe_keypair key_id =
      match runtime_ctx.circle_hfhe_key_id with
      | Some scoped_key_id when String.equal scoped_key_id key_id && key_policy_satisfied ->
        ctx.get_fhe_keypair key_id
      | _ ->
        None
    in
    let allow_fhe_capability = function
      | ContractVM.Fhe_load_pk_cap ->
        key_policy_satisfied
      | Fhe_encrypt_cap ->
        key_policy_satisfied
        && Octra_core.Circle_hfhe_policy.encrypt_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_decrypt_cap ->
        key_policy_satisfied
        && Octra_core.Circle_hfhe_policy.decrypt_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_cipher_arithmetic_cap ->
        key_policy_satisfied
        && Octra_core.Circle_hfhe_policy.cipher_arithmetic_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_verify_zero_cap ->
        key_policy_satisfied
        && proof_binding_satisfied
        && Octra_core.Circle_hfhe_policy.verify_zero_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_verify_range_cap ->
        key_policy_satisfied
        && proof_binding_satisfied
        && Octra_core.Circle_hfhe_policy.verify_range_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_verify_bound_cap ->
        key_policy_satisfied
        && proof_binding_satisfied
        && Octra_core.Circle_hfhe_policy.verify_bound_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_commit_cap ->
        key_policy_satisfied
        && Octra_core.Circle_hfhe_policy.commit_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_pedersen_cap ->
        key_policy_satisfied
        && Octra_core.Circle_hfhe_policy.pedersen_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_cipher_serde_cap ->
        key_policy_satisfied
        && Octra_core.Circle_hfhe_policy.cipher_serde_allowed
          policy
          ~owner
          ~caller
          ~active_relay
      | Fhe_pubkey_serde_cap ->
        key_policy_satisfied
        && Octra_core.Circle_hfhe_policy.pubkey_serde_allowed
          policy
          ~owner
          ~caller
          ~active_relay
    in
    Lwt.return
      (Ok {
         exec_ctx = { runtime_ctx with get_fhe_pubkey; get_fhe_keypair; allow_fhe_capability };
         policy;
         owner;
         active_relay;
       })

let wasm_hfhe_caps_of_runtime_ctx ?(has_active_keypair=false) (ctx : ContractVM.exec_ctx) =
  let caps = ref [] in
  let add flag name =
    if flag then
      caps := name :: !caps in
  add
    (ctx.allow_fhe_capability ContractVM.Fhe_load_pk_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_pubkey_serde_cap)
    "fhe_load_pk";
  add
    (has_active_keypair
     && ctx.allow_fhe_capability ContractVM.Fhe_encrypt_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_cipher_serde_cap)
    "fhe_encrypt";
  add
    (has_active_keypair
     && ctx.allow_fhe_capability ContractVM.Fhe_decrypt_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_cipher_serde_cap)
    "fhe_decrypt";
  add
    (ctx.allow_fhe_capability ContractVM.Fhe_cipher_arithmetic_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_pubkey_serde_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_cipher_serde_cap)
    "fhe_cipher_arithmetic";
  add
    (ctx.allow_fhe_capability ContractVM.Fhe_commit_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_pubkey_serde_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_cipher_serde_cap)
    "fhe_commit";
  add (ctx.allow_fhe_capability ContractVM.Fhe_pedersen_cap) "fhe_pedersen";
  add
    (ctx.allow_fhe_capability ContractVM.Fhe_verify_zero_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_pubkey_serde_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_cipher_serde_cap)
    "fhe_verify_zero";
  add
    (ctx.allow_fhe_capability ContractVM.Fhe_verify_range_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_pubkey_serde_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_cipher_serde_cap)
    "fhe_verify_range";
  add
    (ctx.allow_fhe_capability ContractVM.Fhe_verify_bound_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_pubkey_serde_cap
     && ctx.allow_fhe_capability ContractVM.Fhe_cipher_serde_cap)
    "fhe_verify_bound";
  List.rev !caps

let wasm_hfhe_pubkeys_of_runtime_ctx
    (ctx : ContractVM.exec_ctx)
    (policy : Octra_core.Circle_hfhe_policy.t)
    ~owner
    ~caller
    ~active_relay =
  let load_pk_string_allowed =
    ctx.allow_fhe_capability ContractVM.Fhe_load_pk_cap
    && ctx.allow_fhe_capability ContractVM.Fhe_pubkey_serde_cap in
  if not load_pk_string_allowed then
    []
  else
    let candidates =
      let from_allowlist =
        match policy.pk_allowlist with
        | Some values -> values
        | None -> [] in
      let seeded =
        owner :: caller :: from_allowlist in
      match active_relay with
      | Some relay_id -> relay_id :: seeded
      | None -> seeded in
    let unique =
      List.fold_left
        (fun acc addr ->
          if List.mem addr acc then acc else addr :: acc)
        []
        candidates
      |> List.rev in
    List.filter_map
      (fun requested_addr ->
        if
          Octra_core.Circle_hfhe_policy.load_pk_allowed_with_transport
            policy
            ~owner
            ~caller
            ~requested_addr
            ~active_relay
        then
          match ctx.get_fhe_pubkey requested_addr with
          | Some pk ->
            let pubkey_b64 =
              Base64.encode_exn
                (Bytes.to_string (Pvac_ffi.serialize_pubkey pk)) in
            Some (requested_addr, pubkey_b64)
          | None ->
            None
        else
          None)
      unique

let wasm_hfhe_active_key_of_runtime_ctx
    (ctx : ContractVM.exec_ctx) =
  let key_ops_allowed =
    ctx.allow_fhe_capability ContractVM.Fhe_encrypt_cap
    || ctx.allow_fhe_capability ContractVM.Fhe_decrypt_cap in
  match ctx.circle_hfhe_key_id with
  | None ->
    None
  | Some _ when not key_ops_allowed ->
    None
  | Some key_id ->
    begin
      match ctx.get_fhe_keypair key_id with
      | Some (pk, sk) ->
        Some
          ( key_id,
            Base64.encode_exn (Bytes.to_string (Pvac_ffi.serialize_pubkey pk)),
            Base64.encode_exn (Bytes.to_string (Pvac_ffi.serialize_seckey sk)) )
      | None ->
        None
    end

let prepare_octb_call ~ctx ~depth ~limit ~caller ~address ~value ~method_name ~params
    ~bytecode ~profile ~storage_tbl =
  let fixed = Contract.fix_jumps bytecode in
  match Contract.extract_method_target fixed method_name with
  | None -> Error "method not found"
  | Some target ->
    match Contract.runtime_params profile target params with
    | Error error -> Error (Octra_vm.Program_input.error_message error)
    | Ok values ->
      Ok (fixed, Contract.setup_call_state_values
        ~ctx ~depth ~limit ~strict_values:(Contract.strict_values profile)
        ~caller ~address ~value ~storage_tbl
        ~method_name ~params:values ())

let wasm_fuel_limit limit =
  max 0 (min limit 20_000_000)

let wasm_compute_fuel_limit limit =
  max 0 (min limit 2_000_000_000)

let execute_wasm_view
    execution
    ~code_b64
    ~export_name
    ~request_bytes
    ~storage_tbl
    ~storage_cache_key
    ~caller
    ~address
    ~tx_hash
    ~current_epoch
    ~hfhe_caps
    ~hfhe_pubkeys
    ~hfhe_active_key
    ~hfhe_mode
    ~public_reads
    ~fuel_limit =
  match execution with
  | Circle_program.Standard ->
    Octra_core.Circle_wasm_host.execute
      ~code_b64
      ~export_name
      ~request_bytes
      ~storage_tbl
      ~storage_cache_key
      ~caller
      ~address
      ~tx_hash
      ~current_epoch
      ~hfhe_caps
      ~hfhe_pubkeys
      ~hfhe_active_key
      ~hfhe_mode
      ~public_reads
      ~fuel_limit:(wasm_fuel_limit fuel_limit)
      ~is_view:true
      ~update_policy:false
  | Circle_program.Compute ->
    Octra_core.Circle_wasm_host.execute_compute
      ~code_b64
      ~export_name
      ~request_bytes
      ~storage_tbl
      ~storage_cache_key
      ~caller
      ~address
      ~tx_hash
      ~current_epoch
      ~hfhe_caps
      ~hfhe_pubkeys
      ~hfhe_active_key
      ~hfhe_mode
      ~public_reads
      ~fuel_limit:(wasm_compute_fuel_limit fuel_limit)

let rec execute_view_call_with_execution execution ?(trusted=[]) ?(ctx=ContractVM.default_ctx) ?(depth=0) ?(limit=2_000_000_000)
    store circle_id method_name params caller =
  let timing_enabled = wasm_view_method_timing_enabled method_name in
  let timing_started_at = if timing_enabled then Some (Unix.gettimeofday ()) else None in
  let timing_last_at = ref timing_started_at in
  let timing_phases = ref [] in
  let timing_mark label =
    match !timing_last_at with
    | None ->
      ()
    | Some last_at ->
      let now = Unix.gettimeofday () in
      timing_phases := (label, now -. last_at) :: !timing_phases;
      timing_last_at := Some now in
  let finish receipt =
    if timing_enabled then begin
      let total =
        match timing_started_at with
        | None -> 0.0
        | Some started_at -> Unix.gettimeofday () -. started_at in
      let outcome =
        if receipt.Contract.success then
          "ok"
        else
          Option.value ~default:"execution reverted" receipt.Contract.error in
      let phases =
        List.rev !timing_phases
        |> List.map (fun (label, secs) -> Printf.sprintf "%s:%.3fs" label secs)
        |> String.concat "," in
      Octra_log.trace "circle"
        "event = view_timing circle = %s method = %s caller = %s total_s = %.3f outcome = %s phases = %s"
        circle_id
        method_name
        caller
        total
        outcome
        phases
    end;
    Lwt.return receipt in
  let* loaded_result = Circle_program.load ~trusted store circle_id in
  timing_mark "load_program";
  match loaded_result with
  | Error e ->
    finish
      (failed_receipt (Octra_core.Circle_wasm_host.error_message e))
  | Ok loaded ->
    let declared_execution =
      match loaded.code with
      | Circle_program.Octb _ -> Circle_program.Standard
      | Circle_program.Wasm_v1 wasm ->
        Circle_program.execution_for_method wasm.methods method_name
    in
    begin
      match execution, declared_execution with
      | Circle_program.Standard, Circle_program.Compute ->
        finish (failed_receipt "circle compute method requires compute executor")
      | Circle_program.Compute, Circle_program.Standard ->
        finish (failed_receipt "circle method is not declared for compute execution")
      | _ ->
    begin
      match loaded.code with
      | Circle_program.Octb { bytecode; profile } ->
        let* storage_result = Octra_core.Store_irmin.load_circle_stable_storage store circle_id in
        timing_mark "load_storage";
        begin
          match storage_result with
          | Error e -> finish (failed_receipt e)
          | Ok storage_tbl ->
            let* runtime_ctx_result = load_runtime_hfhe_ctx ctx store circle_id caller in
            timing_mark "load_hfhe";
            begin
              match runtime_ctx_result with
              | Error e ->
                finish (failed_receipt e)
              | Ok runtime_hfhe ->
                let storage_copy = Hashtbl.copy storage_tbl in
                begin
                  match prepare_octb_call
                      ~ctx:runtime_hfhe.exec_ctx
                      ~depth
                      ~limit
                      ~caller
                      ~address:circle_id
                      ~value:Z.zero
                      ~method_name
                      ~params
                      ~bytecode
                      ~profile
                      ~storage_tbl:storage_copy with
                  | Error error -> finish (failed_receipt error)
                  | Ok (fixed, state) ->
                    state.ContractVM.is_view <- true;
                    let* receipt =
                      Lwt_preemptive.detach
                        (fun () ->
                          Contract.run_fixed_from_dispatcher state fixed)
                        ()
                    in
                    timing_mark "run_dispatcher";
                    finish receipt
                end
            end
        end
      | Circle_program.Wasm_v1 wasm ->
        let* storage_result =
          load_wasm_view_storage_cached store circle_id loaded.info.stable_root in
        timing_mark "load_storage";
        begin
          match storage_result with
          | Error e -> finish (failed_receipt e)
          | Ok (storage_cache_key, storage_tbl) ->
            let* runtime_ctx_result =
              if wasm_method_needs_hfhe method_name then
                load_runtime_hfhe_ctx ctx store circle_id caller
              else
                Lwt.return (Ok (empty_runtime_hfhe_details ctx loaded.info.owner))
            in
            timing_mark "load_hfhe";
            begin
              match runtime_ctx_result with
              | Error e ->
                finish (failed_receipt e)
              | Ok runtime_hfhe ->
                let hfhe_active_key =
                  wasm_hfhe_active_key_of_runtime_ctx runtime_hfhe.exec_ctx in
                let hfhe_caps =
                  wasm_hfhe_caps_of_runtime_ctx
                    ~has_active_keypair:(Option.is_some hfhe_active_key)
                    runtime_hfhe.exec_ctx in
                let hfhe_pubkeys =
                  wasm_hfhe_pubkeys_of_runtime_ctx
                    runtime_hfhe.exec_ctx
                    runtime_hfhe.policy
                    ~owner:runtime_hfhe.owner
                    ~caller
                    ~active_relay:runtime_hfhe.active_relay in
                match Octra_core.Circle_wasm_codec.encode_request ~method_name params with
                | Error e ->
                  finish (failed_receipt e)
                | Ok request_bytes ->
                  timing_mark "encode_request";
                  begin
                    let declarations =
                      Octra_core.Circle_wasm_public_read.for_method
                        wasm.public_reads
                        method_name in
                    let* public_reads_result =
                      Octra_core.Circle_wasm_public_read.load
                        store
                        (Int64.of_int runtime_hfhe.exec_ctx.current_epoch)
                        declarations in
                    match public_reads_result with
                    | Error e ->
                      finish (failed_receipt e)
                    | Ok public_reads ->
                      let wasm_limit = limit - public_reads.effort_used in
                      if wasm_limit <= 0 then
                        finish (failed_receipt "wasm public read effort exceeds limit")
                      else
                        let* wasm_result =
                          execute_wasm_view
                            execution
                            ~code_b64:wasm.code_b64
                            ~export_name:"octra_query"
                            ~request_bytes
                            ~storage_tbl
                            ~storage_cache_key:(Some storage_cache_key)
                            ~caller
                            ~address:circle_id
                            ~tx_hash:runtime_hfhe.exec_ctx.tx_hash
                            ~current_epoch:runtime_hfhe.exec_ctx.current_epoch
                            ~hfhe_caps
                            ~hfhe_pubkeys
                            ~hfhe_active_key
                            ~hfhe_mode:Octra_core.Circle_hfhe_transcript.Direct
                            ~public_reads:public_reads.snapshots
                            ~fuel_limit:wasm_limit in
                        timing_mark "native_execute";
                        begin
                          match wasm_result with
                          | Error (Octra_core.Circle_wasm_host.Rejected e)
                          | Error (Octra_core.Circle_wasm_host.Unavailable e) ->
                            finish (failed_receipt e)
                          | Ok result ->
                            let events =
                              List.map
                                (fun event ->
                                  {
                                    ContractVM.contract = circle_id;
                                    depth;
                                    event = event.Octra_core.Circle_wasm_host.topic;
                                    values = [ContractVM.VString event.data];
                                  })
                                result.events in
                            finish {
                              Contract.success = result.success;
                              return_value = Option.map vm_value_of_wasm_response result.response_value;
                              effort_used = public_reads.effort_used + result.effort_used;
                              events;
                              error =
                                if result.success then None
                                else Some (Option.value ~default:"execution reverted" result.error);
                              storage_writes = 0;
                            }
                        end
                  end
            end
        end
    end
    end

and maybe_prefetch_preview ?(ctx=ContractVM.default_ctx) ?(depth=0) ?(limit=2_000_000_000)
    store circle_id caller prompt_csv prompt_tokens delivered_csv =
  match parse_csv_tokens delivered_csv with
  | None ->
    ()
  | Some delivered_tokens ->
    let next_prompt_tokens = prompt_tokens @ delivered_tokens in
    let remaining = preview_max_context_tokens - List.length next_prompt_tokens in
    if remaining <= 0 then
      ()
    else
      let next_prompt_csv = append_csv prompt_csv delivered_csv in
      let next_key = preview_cache_key circle_id caller next_prompt_csv in
      let prefetch_n = min preview_prefetch_tokens remaining in
      if prefetch_n <= 0 then
        ()
      else if Hashtbl.mem preview_session_cache next_key || Hashtbl.mem preview_session_inflight next_key then
        ()
      else begin
        Hashtbl.replace preview_session_inflight next_key ();
        Lwt.async (fun () ->
          let params = [`String next_prompt_csv; `Int prefetch_n] in
          let* receipt =
            execute_view_call_with_execution
              Circle_program.Standard
              ~ctx
              ~depth
              ~limit
              store
              circle_id
              "complete_preview"
              params
              caller in
          begin
            match preview_result_csv receipt with
            | Some result_csv ->
              preview_cache_store next_key result_csv
            | None ->
              ()
          end;
          Hashtbl.remove preview_session_inflight next_key;
          Lwt.return_unit)
      end

and execute_view_call ?(trusted=[]) ?(ctx=ContractVM.default_ctx) ?(depth=0) ?(limit=2_000_000_000)
    store circle_id method_name params caller =
  match preview_request_of_call method_name params with
  | None ->
    execute_view_call_with_execution
      Circle_program.Standard
      ~trusted
      ~ctx
      ~depth
      ~limit
      store
      circle_id
      method_name
      params
      caller
  | Some (prompt_csv, prompt_tokens, n_tokens) ->
    let cache_key = preview_cache_key circle_id caller prompt_csv in
    begin
      match preview_cache_lookup cache_key n_tokens with
      | Some result_csv ->
        maybe_prefetch_preview
          ~ctx
          ~depth
          ~limit
          store
          circle_id
          caller
          prompt_csv
          prompt_tokens
          result_csv;
        Lwt.return (preview_receipt result_csv)
      | None ->
        let* receipt =
          execute_view_call_with_execution
            Circle_program.Standard
            ~ctx
            ~depth
            ~limit
            store
            circle_id
            method_name
            params
            caller in
        begin
          match preview_result_csv receipt with
          | Some result_csv ->
            preview_cache_store cache_key result_csv;
            maybe_prefetch_preview
              ~ctx
              ~depth
              ~limit
              store
              circle_id
              caller
              prompt_csv
              prompt_tokens
              result_csv
          | None ->
            ()
        end;
        Lwt.return receipt
    end

let execute_view_call_direct ?(trusted=[]) ?(ctx=ContractVM.default_ctx) ?(depth=0) ?(limit=2_000_000_000)
    store circle_id method_name params caller =
  execute_view_call_with_execution
    Circle_program.Standard
    ~trusted
    ~ctx
    ~depth
    ~limit
    store
    circle_id
    method_name
    params
    caller

let execute_compute_view_call ?(trusted=[]) ?(ctx=ContractVM.default_ctx) ?(depth=0) ?(limit=2_000_000_000)
    store circle_id method_name params caller =
  execute_view_call_with_execution
    Circle_program.Compute
    ~trusted
    ~ctx
    ~depth
    ~limit
    store
    circle_id
    method_name
    params
    caller

let execute_call ?(trusted=[]) ?(ctx=ContractVM.default_ctx) ?(depth=0)
    ?(limit=1_000_000)
    ?(hfhe_mode=Octra_core.Circle_hfhe_transcript.Direct)
    ?(update_policy=false)
    store circle_id method_name params caller value =
  let* loaded_result = Circle_program.load ~trusted store circle_id in
  match loaded_result with
  | Error (Octra_core.Circle_wasm_host.Rejected e) ->
    Lwt.return (failed_call_result e)
  | Error (Octra_core.Circle_wasm_host.Unavailable e) ->
    Lwt.fail (Execution_unavailable e)
  | Ok loaded ->
    let declared_execution =
      match loaded.code with
      | Circle_program.Octb _ -> Circle_program.Standard
      | Circle_program.Wasm_v1 wasm ->
        Circle_program.execution_for_method wasm.methods method_name
    in
    if declared_execution = Circle_program.Compute then
      Lwt.return (failed_call_result "circle compute method cannot execute as update")
    else
    let* storage_result = Octra_core.Store_irmin.load_circle_stable_storage store circle_id in
    begin
      match storage_result with
      | Error e -> Lwt.return (failed_call_result e)
      | Ok storage_tbl ->
        let baseline_storage_tbl = Hashtbl.copy storage_tbl in
        begin
          match loaded.code with
          | Circle_program.Octb { bytecode; profile } ->
            let* runtime_ctx_result = load_runtime_hfhe_ctx ctx store circle_id caller in
            begin
              match runtime_ctx_result with
              | Error e ->
                Lwt.return (failed_call_result e)
              | Ok runtime_hfhe ->
                let spawns = ref [] in
                let runtime_ctx =
                  if receipt_mode hfhe_mode then
                    without_hfhe_verifiers runtime_hfhe.exec_ctx
                  else
                    runtime_hfhe.exec_ctx
                in
                let exec_ctx =
                  with_circle_spawn
                    runtime_ctx
                    circle_id
                    caller
                    spawns
                in
                begin
                  match prepare_octb_call
                      ~ctx:exec_ctx
                      ~depth
                      ~limit
                      ~caller
                      ~address:circle_id
                      ~value
                      ~method_name
                      ~params
                      ~bytecode
                      ~profile
                      ~storage_tbl with
                  | Error error -> Lwt.return (failed_call_result error)
                  | Ok (fixed, state) ->
                    let* receipt =
                      Lwt_preemptive.detach
                        (fun () ->
                          Contract.run_fixed_from_dispatcher state fixed)
                        ()
                    in
                    Lwt.return {
                      receipt;
                      storage_tbl;
                      baseline_storage_tbl;
                      spawns = !spawns;
                      assets = [];
                      encrypted_assets = [];
                      caller;
                      tx_hash = exec_ctx.tx_hash;
                      hfhe_binding =
                        hfhe_binding
                          loaded
                          circle_id
                          (public_reads_hash [])
                          (hfhe_context_hash [] [] None)
                          [];
                    }
                end
            end
          | Circle_program.Wasm_v1 wasm ->
            begin
              let* runtime_ctx_result =
                if wasm_method_needs_hfhe method_name then
                  load_runtime_hfhe_ctx
                    ~receipt_bound:(receipt_mode hfhe_mode)
                    ctx
                    store
                    circle_id
                    caller
                else
                  Lwt.return (Ok (empty_runtime_hfhe_details ctx loaded.info.owner))
              in
              begin
                match runtime_ctx_result with
                | Error e ->
                  Lwt.return (failed_call_result e)
                | Ok runtime_hfhe ->
                  let hfhe_active_key =
                    wasm_hfhe_active_key_of_runtime_ctx runtime_hfhe.exec_ctx in
                  let hfhe_caps =
                    wasm_hfhe_caps_of_runtime_ctx
                      ~has_active_keypair:(Option.is_some hfhe_active_key)
                      runtime_hfhe.exec_ctx in
                  let hfhe_pubkeys =
                    wasm_hfhe_pubkeys_of_runtime_ctx
                      runtime_hfhe.exec_ctx
                      runtime_hfhe.policy
                      ~owner:runtime_hfhe.owner
                      ~caller
                      ~active_relay:runtime_hfhe.active_relay in
                  match Octra_core.Circle_wasm_codec.encode_request ~method_name params with
                  | Error e ->
                    Lwt.return (failed_call_result e)
                  | Ok request_bytes ->
                    begin
                      let declarations =
                        Octra_core.Circle_wasm_public_read.for_method
                          wasm.public_reads
                          method_name in
                      let* public_reads_result =
                        Octra_core.Circle_wasm_public_read.load
                          store
                          (Int64.of_int runtime_hfhe.exec_ctx.current_epoch)
                          declarations in
                      match public_reads_result with
                      | Error e ->
                        Lwt.return (failed_call_result e)
                      | Ok public_reads ->
                        let wasm_limit = limit - public_reads.effort_used in
                        if wasm_limit <= 0 then
                          Lwt.return
                            (failed_call_result "wasm public read effort exceeds limit")
                        else
                          let* wasm_result =
                            Octra_core.Circle_wasm_host.execute
                              ~code_b64:wasm.code_b64
                              ~export_name:"octra_update"
                              ~request_bytes
                              ~storage_tbl
                              ~storage_cache_key:None
                              ~caller
                              ~address:circle_id
                              ~tx_hash:runtime_hfhe.exec_ctx.tx_hash
                              ~current_epoch:runtime_hfhe.exec_ctx.current_epoch
                              ~hfhe_caps
                              ~hfhe_pubkeys
                              ~hfhe_active_key
                              ~hfhe_mode
                              ~public_reads:public_reads.snapshots
                              ~fuel_limit:(wasm_fuel_limit wasm_limit)
                              ~is_view:false
                              ~update_policy in
                          begin
                            match wasm_result with
                            | Error (Octra_core.Circle_wasm_host.Rejected e) ->
                              Lwt.return (failed_call_result e)
                            | Error (Octra_core.Circle_wasm_host.Unavailable e) ->
                              Lwt.fail (Execution_unavailable e)
                            | Ok result ->
                              let events =
                                List.map
                                  (fun event ->
                                    {
                                      ContractVM.contract = circle_id;
                                      depth;
                                      event = event.Octra_core.Circle_wasm_host.topic;
                                      values = [ContractVM.VString event.data];
                                    })
                                  result.events in
                              let receipt = {
                                Contract.success = result.success;
                                return_value = Option.map vm_value_of_wasm_response result.response_value;
                                effort_used = public_reads.effort_used + result.effort_used;
                                events;
                                error =
                                  if result.success then None
                                  else Some (Option.value ~default:"execution reverted" result.error);
                                storage_writes = 0;
                              } in
                              Lwt.return {
                                receipt;
                                storage_tbl = result.storage_tbl;
                                baseline_storage_tbl;
                                spawns = result.spawns;
                                assets = result.assets;
                                encrypted_assets = result.encrypted_assets;
                                caller;
                                tx_hash = runtime_hfhe.exec_ctx.tx_hash;
                                hfhe_binding =
                                  hfhe_binding
                                    loaded
                                    circle_id
                                    (public_reads_hash public_reads.snapshots)
                                    (hfhe_context_hash
                                       hfhe_caps
                                       hfhe_pubkeys
                                       hfhe_active_key)
                                    result.hfhe_transcript;
                              }
                          end
                    end
              end
            end
        end
    end

let read_storage_key store circle_id key =
  Octra_core.Store_irmin.read_circle_stable_inline_value store circle_id key

let read_slot_policy store circle_id path_key =
  load_slot_policy store circle_id path_key

let read_state_policy store circle_id path_key =
  load_state_policy store circle_id path_key

let read_state_descriptor store circle_id path_key =
  Octra_core.Circle_state_descriptor_store.load_descriptor store circle_id path_key

let read_balance_binding store circle_id subject_addr =
  Octra_core.Circle_balance_binding_store.load store circle_id subject_addr

let read_register_binding store circle_id register_ref =
  Octra_core.Circle_register_binding_store.load store circle_id register_ref

let read_balance_cell store circle_id path_key =
  Octra_core.Circle_balance_cell_store.load store circle_id path_key

let read_balance_workflow store circle_id workflow_ref =
  Octra_core.Circle_balance_workflow_store.load store circle_id workflow_ref

let read_register_cell store circle_id path_key =
  Octra_core.Circle_register_cell_store.load store circle_id path_key

let read_register_workflow store circle_id workflow_ref =
  Octra_core.Circle_register_workflow_store.load store circle_id workflow_ref

let list_storage_pairs store circle_id =
  let* storage_result = Octra_core.Store_irmin.load_circle_stable_storage store circle_id in
  match storage_result with
  | Error e -> Lwt.return (Error e)
  | Ok storage_tbl ->
    let pairs = Hashtbl.fold (fun key value acc -> (key, value) :: acc) storage_tbl [] in
    Lwt.return (Ok pairs)

let list_storage_page store circle_id ~limit =
  Octra_core.Store_irmin.load_circle_stable_storage_page store circle_id ~limit

let commit_call_result store circle_id t =
  let* info_opt = Octra_core.Store_irmin.get_circle_info store circle_id in
  match info_opt with
  | None -> Lwt.return (Error "circle not found")
  | Some info ->
    if not t.receipt.Contract.success then
      Lwt.return (Error "circle call failed")
    else begin
      match
        Circle_runtime_storage.validate_runtime_storage_delta
          t.baseline_storage_tbl
          t.storage_tbl
      with
      | Error (_code, raw_key, prefix) ->
        Lwt.return
          (Error
             ("circle runtime attempted to write reserved stable key "
              ^ raw_key
              ^ " under prefix "
              ^ prefix))
      | Ok () ->
        let rec duplicate_id seen = function
          | [] -> None
          | (spawn : Octra_core.Circle_wasm_host.spawn) :: rest ->
            if List.exists (String.equal spawn.circle_id) seen then
              Some spawn.circle_id
            else
              duplicate_id (spawn.circle_id :: seen) rest
        in
        let check_spawn idx spawn =
          if spawn.Octra_core.Circle_wasm_host.spawn_nonce <> idx then
            Lwt.return (Error ("circle spawn nonce mismatch"))
          else
            match Octra_core.Circle_deploy.decode_spawn_payload_json spawn.payload_json with
            | Error (_code, reason) ->
              Lwt.return (Error reason)
            | Ok payload ->
              let src =
                Octra_core.Circle_deploy.Spawn {
                  parent = circle_id;
                  caller = t.caller;
                  tx_hash = t.tx_hash;
                  spawn_nonce = spawn.spawn_nonce;
                  owner_mode = spawn.owner_mode;
                  payload_json = spawn.payload_json;
                } in
              let expected =
                Octra_core.Circle_deploy.circle_id_of_source src payload in
              if not (String.equal expected spawn.circle_id) then
                Lwt.return (Error "circle spawn id mismatch")
              else
                let* checked =
                  Octra_core.Circle_deploy.check_available store src payload in
                begin
                  match checked with
                  | Error (_code, reason) ->
                    Lwt.return (Error reason)
                  | Ok prepared ->
                    Lwt.return (Ok { src; payload; prepared })
                end
        in
        if List.length t.spawns > spawn_cap then
          Lwt.return (Error "circle spawn cap exceeded")
        else
          match duplicate_id [] t.spawns with
          | Some dup ->
            Lwt.return (Error ("duplicate circle spawn id " ^ dup))
          | None ->
            let* checked =
              Lwt_list.mapi_s check_spawn t.spawns in
            match List.find_opt (function Error _ -> true | Ok _ -> false) checked with
            | Some (Error e) ->
              Lwt.return (Error e)
            | _ ->
              let checked =
                List.filter_map
                  (function
                    | Ok value -> Some value
                    | Error _ -> None)
                  checked in
              let spawn_info target =
                List.find_opt
                  (fun spawn -> String.equal spawn.prepared.Octra_core.Circle_deploy.circle_id target)
                  checked
                |> Option.map
                     (fun spawn -> spawn.prepared.Octra_core.Circle_deploy.info)
              in
              let usage_tbl = Hashtbl.create 8 in
              let usage_for target =
                match Hashtbl.find_opt usage_tbl target with
                | Some usage -> Lwt.return usage
                | None ->
                  if String.equal target circle_id then
                    let* usage =
                      Octra_core.Store_irmin.get_circle_asset_usage_bytes store target in
                    Hashtbl.replace usage_tbl target usage;
                    Lwt.return usage
                  else begin
                    Hashtbl.replace usage_tbl target 0L;
                    Lwt.return 0L
                  end
              in
              let target_info target =
                if String.equal target circle_id then
                  Some info
                else
                  spawn_info target
              in
              let target_owner_ok target target_info =
                if String.equal target circle_id then
                  String.equal target_info.Octra_core.Circles.owner t.caller
                else
                  String.equal target_info.Octra_core.Circles.owner t.caller
                  || String.equal target_info.Octra_core.Circles.owner circle_id
              in
              let is_hex64 value =
                String.length value = 64
                && not
                     (String.exists
                        (fun c -> not (Octra_core.Circles.is_hex_char c))
                        value)
              in
              let resolve_encrypted_locator target asset =
                match
                  asset.Octra_core.Circle_wasm_host.path,
                  asset.Octra_core.Circle_wasm_host.slot_ref,
                  asset.Octra_core.Circle_wasm_host.state_ref
                with
                | Some _, Some _, _
                | Some _, _, Some _
                | None, Some _, Some _ ->
                  Error "provide exactly one of path, slot_ref, or state_ref"
                | None, None, None ->
                  Error "path, slot_ref, or state_ref is required"
                | Some raw_path, None, None ->
                  begin
                    match Octra_core.Circles.path_key_of_raw_path raw_path with
                    | Ok (canonical_path, path_key) ->
                      Ok
                        ( canonical_path,
                          path_key,
                          Octra_core.Circles.resource_key_of_path
                            ~circle_id:target
                            ~canonical_path,
                          Octra_core.Circles.Path_locator,
                          None )
                    | Error e -> Error e
                  end
                | None, Some raw_slot_ref, None ->
                  begin
                    match Octra_core.Circles.path_key_of_slot_ref raw_slot_ref with
                    | Ok (slot_ref, canonical_path, path_key) ->
                      Ok
                        ( canonical_path,
                          path_key,
                          Octra_core.Circles.resource_key_of_slot_ref
                            ~circle_id:target
                            ~slot_ref,
                          Octra_core.Circles.Slot_locator,
                          Some slot_ref )
                    | Error e -> Error e
                  end
                | None, None, Some raw_state_ref ->
                  begin
                    match Octra_core.Circles.path_key_of_state_ref raw_state_ref with
                    | Ok (_state_ref, canonical_path, path_key) ->
                      Ok
                        ( canonical_path,
                          path_key,
                          Octra_core.Circles.resource_key_of_state_ref
                            ~circle_id:target
                            ~state_ref:raw_state_ref,
                          Octra_core.Circles.State_locator,
                          None )
                    | Error e -> Error e
                  end
              in
              let check_asset seen (asset : Octra_core.Circle_wasm_host.asset_put) =
                let target = asset.Octra_core.Circle_wasm_host.circle_id in
                let target_info = target_info target in
                match target_info with
                | None ->
                  Lwt.return (Error "circle asset target not in call scope")
                | Some target_info ->
                  if not (target_owner_ok target target_info) then
                    Lwt.return (Error "circle asset owner authorization failed")
                  else if target_info.Octra_core.Circles.resource_mode = Octra_core.Circles.Sealed_read then
                    Lwt.return (Error "sealed_read circles require encrypted asset effects")
                  else if String.length asset.body_b64 > asset_effect_body_b64_cap then
                    Lwt.return (Error "circle asset effect body exceeds max encoded size")
                  else if String.length asset.content_type = 0 then
                    Lwt.return (Error "circle asset content_type must not be empty")
                  else
                    match Octra_core.Circles.path_key_of_raw_path asset.path with
                    | Error e ->
                      Lwt.return (Error e)
                    | Ok (canonical_path, path_key) ->
                      let effect_key = target ^ ":" ^ path_key in
                      if List.exists (String.equal effect_key) seen then
                        Lwt.return (Error "duplicate circle asset effect")
                      else
                        let raw_body_result =
                          try Ok (Base64.decode_exn asset.body_b64)
                          with e -> Error (Printexc.to_string e)
                        in
                        match raw_body_result with
                        | Error e ->
                          Lwt.return (Error ("invalid circle asset body: " ^ e))
                        | Ok raw_body ->
                          let size_bytes = Int64.of_int (String.length raw_body) in
                          if Int64.compare size_bytes (Int64.of_int asset_effect_raw_cap) > 0 then
                            Lwt.return (Error "circle asset effect body exceeds max raw size")
                          else if Int64.compare size_bytes target_info.Octra_core.Circles.limits.max_assets_bytes > 0 then
                            Lwt.return (Error "asset exceeds circle asset limit")
                          else
                            let* old_size =
                              if String.equal target circle_id then
                                let* existing_meta =
                                  Octra_core.Store_irmin.get_circle_asset_meta
                                    store
                                    target
                                    path_key in
                                Lwt.return
                                  (match existing_meta with
                                   | Some meta -> meta.Octra_core.Circles.size_bytes
                                   | None -> 0L)
                              else
                                Lwt.return 0L in
                            let* usage = usage_for target in
                            let next_usage =
                              Int64.add (Int64.sub usage old_size) size_bytes in
                            if Int64.compare next_usage target_info.Octra_core.Circles.limits.max_assets_bytes > 0 then
                              Lwt.return (Error "asset update exceeds circle asset budget")
                            else begin
                              Hashtbl.replace usage_tbl target next_usage;
                              let meta = {
                                Octra_core.Circles.path_key;
                                canonical_path;
                                content_type = asset.content_type;
                                encoding = Option.value ~default:"identity" asset.encoding;
                                size_bytes;
                                blob_hash = Octra_core.Circles.asset_blob_hash raw_body;
                                body_mode = Octra_core.Circles.Public_resources;
                                plaintext_hash = Some (Octra_core.Circles.sha256_hex raw_body);
                                key_id = None;
                                padding_class = None;
                                resource_key =
                                  Octra_core.Circles.resource_key_of_path
                                    ~circle_id:target
                                    ~canonical_path;
                                locator_mode = Octra_core.Circles.Path_locator;
                                slot_ref = None;
                                activate_after_epoch = None;
                                expire_after_epoch = None;
                                metadata_mode = Octra_core.Circles.Metadata_reveal;
                              } in
                              Lwt.return (Ok (effect_key, {
                                circle_id = target;
                                meta;
                                body_b64 = asset.body_b64;
                                usage_bytes = next_usage;
                              }))
                            end
              in
              let check_encrypted_asset seen (asset : Octra_core.Circle_wasm_host.encrypted_asset_put) =
                let target = asset.Octra_core.Circle_wasm_host.circle_id in
                let target_info = target_info target in
                match target_info with
                | None ->
                  Lwt.return (Error "circle encrypted asset target not in call scope")
                | Some target_info ->
                  if not (target_owner_ok target target_info) then
                    Lwt.return (Error "circle encrypted asset owner authorization failed")
                  else if target_info.Octra_core.Circles.resource_mode <> Octra_core.Circles.Sealed_read then
                    Lwt.return (Error "encrypted asset effects require sealed_read circles")
                  else if String.length asset.ciphertext_b64 > asset_effect_body_b64_cap then
                    Lwt.return (Error "circle encrypted asset effect body exceeds max encoded size")
                  else if String.length asset.content_type = 0 then
                    Lwt.return (Error "circle encrypted asset content_type must not be empty")
                  else if String.length asset.key_id = 0 then
                    Lwt.return (Error "circle encrypted asset key_id must not be empty")
                  else if not (is_hex64 asset.plaintext_hash) then
                    Lwt.return (Error "circle encrypted asset plaintext_hash must be a 64-char hex string")
                  else if
                    match asset.activate_after_epoch, asset.expire_after_epoch with
                    | Some activate_after_epoch, Some expire_after_epoch ->
                      Int64.compare expire_after_epoch activate_after_epoch <= 0
                    | _ ->
                      false
                  then
                    Lwt.return (Error "circle encrypted asset expire_after_epoch must be greater than activate_after_epoch")
                  else
                    match resolve_encrypted_locator target asset with
                    | Error e ->
                      Lwt.return (Error e)
                    | Ok (canonical_path, path_key, resource_key, locator_mode, slot_ref) ->
                      let effect_key = target ^ ":" ^ path_key in
                      if List.exists (String.equal effect_key) seen then
                        Lwt.return (Error "duplicate circle asset effect")
                      else
                        let raw_body_result =
                          try Ok (Base64.decode_exn asset.ciphertext_b64)
                          with e -> Error (Printexc.to_string e)
                        in
                        match raw_body_result with
                        | Error e ->
                          Lwt.return (Error ("invalid circle encrypted asset body: " ^ e))
                        | Ok raw_body ->
                          let size_bytes = Int64.of_int (String.length raw_body) in
                          if Int64.compare size_bytes (Int64.of_int asset_effect_raw_cap) > 0 then
                            Lwt.return (Error "circle encrypted asset effect body exceeds max raw size")
                          else if Int64.compare size_bytes target_info.Octra_core.Circles.limits.max_assets_bytes > 0 then
                            Lwt.return (Error "encrypted asset exceeds circle asset limit")
                          else
                            let* old_size =
                              if String.equal target circle_id then
                                let* existing_meta =
                                  Octra_core.Store_irmin.get_circle_asset_meta
                                    store
                                    target
                                    path_key in
                                Lwt.return
                                  (match existing_meta with
                                   | Some meta -> meta.Octra_core.Circles.size_bytes
                                   | None -> 0L)
                              else
                                Lwt.return 0L in
                            let* usage = usage_for target in
                            let next_usage =
                              Int64.add (Int64.sub usage old_size) size_bytes in
                            if Int64.compare next_usage target_info.Octra_core.Circles.limits.max_assets_bytes > 0 then
                              Lwt.return (Error "encrypted asset update exceeds circle asset budget")
                            else begin
                              Hashtbl.replace usage_tbl target next_usage;
                              let meta = {
                                Octra_core.Circles.path_key;
                                canonical_path;
                                content_type = asset.content_type;
                                encoding = Option.value ~default:"identity" asset.encoding;
                                size_bytes;
                                blob_hash = Octra_core.Circles.asset_blob_hash raw_body;
                                body_mode = Octra_core.Circles.Sealed_read;
                                plaintext_hash = Some asset.plaintext_hash;
                                key_id = Some asset.key_id;
                                padding_class = asset.padding_class;
                                resource_key;
                                locator_mode;
                                slot_ref;
                                activate_after_epoch = asset.activate_after_epoch;
                                expire_after_epoch = asset.expire_after_epoch;
                                metadata_mode =
                                  Option.value
                                    ~default:Octra_core.Circles.Metadata_reveal
                                    asset.metadata_mode;
                              } in
                              Lwt.return (Ok (effect_key, {
                                circle_id = target;
                                meta;
                                ciphertext_b64 = asset.ciphertext_b64;
                                usage_bytes = next_usage;
                              }))
                            end
              in
              let rec check_assets seen acc = function
                | [] -> Lwt.return (Ok (seen, List.rev acc))
                | asset :: rest ->
                  let* checked_asset = check_asset seen asset in
                  match checked_asset with
                  | Error e -> Lwt.return (Error e)
                  | Ok (effect_key, value) ->
                    check_assets (effect_key :: seen) (value :: acc) rest
              in
              let rec check_encrypted_assets seen acc = function
                | [] -> Lwt.return (Ok (List.rev acc))
                | asset :: rest ->
                  let* checked_asset = check_encrypted_asset seen asset in
                  match checked_asset with
                  | Error e -> Lwt.return (Error e)
                  | Ok (effect_key, value) ->
                    check_encrypted_assets (effect_key :: seen) (value :: acc) rest
              in
              let* checked_assets =
                if List.length t.assets + List.length t.encrypted_assets > asset_effect_cap then
                  Lwt.return (Error "circle asset effect cap exceeded")
                else
                  check_assets [] [] t.assets in
              match checked_assets with
              | Error e ->
                Lwt.return (Error e)
              | Ok (seen_assets, checked_assets) ->
              let* checked_encrypted_assets =
                check_encrypted_assets seen_assets [] t.encrypted_assets in
              match checked_encrypted_assets with
              | Error e ->
                Lwt.return (Error e)
              | Ok checked_encrypted_assets ->
              let* saved =
                Octra_core.Store_irmin.save_circle_stable_storage_checked
                  store
                  circle_id
                  info.Octra_core.Circles.limits
                  t.storage_tbl in
              begin
                match saved with
                | Error e ->
                  Lwt.return (Error e)
                | Ok _ ->
                  let* written =
                    Lwt_list.map_s
                      (fun spawn ->
                        Octra_core.Circle_deploy.write_prepared
                          store
                          spawn.src
                          spawn.prepared
                          spawn.payload)
                      checked in
                  match List.find_opt (function Error _ -> true | Ok _ -> false) written with
                  | Some (Error (_code, reason)) ->
                    Lwt.return (Error reason)
                  | _ ->
                    let write_asset (asset : checked_asset) =
                      let* () =
                        Octra_core.Store_irmin.save_circle_asset_meta
                          store
                          asset.circle_id
                          asset.meta in
                      let* () =
                        Octra_core.Store_irmin.save_circle_asset_body_b64
                          store
                          asset.circle_id
                          asset.meta.Octra_core.Circles.path_key
                          asset.body_b64 in
                      let* () =
                        Octra_core.Store_irmin.save_circle_asset_resource_index
                          store
                          asset.circle_id
                          asset.meta.Octra_core.Circles.resource_key
                          asset.meta.Octra_core.Circles.path_key in
                      let* () =
                        Octra_core.Store_irmin.set_circle_asset_usage_bytes
                          store
                          asset.circle_id
                          asset.usage_bytes in
                      let* assets_root_opt =
                        Octra_core.Store_irmin.get_tree_hash_at_path
                          store
                          ["circles"; asset.circle_id; "assets"; "by_hash"] in
                      let assets_root =
                        Option.value
                          ~default:Octra_core.Circles.zero_hash_hex
                          assets_root_opt in
                      Octra_core.Store_irmin.set_circle_assets_root
                        store
                        asset.circle_id
                        assets_root
                    in
                    let write_encrypted_asset (asset : checked_encrypted_asset) =
                      let* () =
                        Octra_core.Store_irmin.save_circle_asset_meta
                          store
                          asset.circle_id
                          asset.meta in
                      let* () =
                        Octra_core.Store_irmin.save_circle_asset_ciphertext_b64
                          store
                          asset.circle_id
                          asset.meta.Octra_core.Circles.path_key
                          asset.ciphertext_b64 in
                      let* () =
                        Octra_core.Store_irmin.save_circle_asset_resource_index
                          store
                          asset.circle_id
                          asset.meta.Octra_core.Circles.resource_key
                          asset.meta.Octra_core.Circles.path_key in
                      let* () =
                        Octra_core.Store_irmin.set_circle_asset_usage_bytes
                          store
                          asset.circle_id
                          asset.usage_bytes in
                      let* assets_root_opt =
                        Octra_core.Store_irmin.get_tree_hash_at_path
                          store
                          ["circles"; asset.circle_id; "assets"; "by_hash"] in
                      let assets_root =
                        Option.value
                          ~default:Octra_core.Circles.zero_hash_hex
                          assets_root_opt in
                      Octra_core.Store_irmin.set_circle_assets_root
                        store
                        asset.circle_id
                        assets_root
                    in
                    let* () = Lwt_list.iter_s write_asset checked_assets in
                    let* () = Lwt_list.iter_s write_encrypted_asset checked_encrypted_assets in
                    Lwt.return (Ok ())
              end
    end