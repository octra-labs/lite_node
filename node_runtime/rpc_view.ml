(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let raw_to_hex = Text.raw_to_hex

module Ledger = Octra_core.Ledger
module Pvac_registry = Octra_core.Pvac_registry
module Pvac_migration = Octra_core.Pvac_migration
module Pvac_legacy_public_replay = Octra_core.Pvac_legacy_public_replay
module FheBalance = Octra_core.Crypto.FheBalance
module Rule_graph = Octra_core.Rule_graph

let opt_hex = function
  | None -> `Null
  | Some s -> `String s

let opt_int = function
  | Some value -> `Int value
  | None -> `Null

let opt_string = function
  | Some value -> `String value
  | None -> `Null

let json_short limit json =
  let value = Yojson.Safe.to_string json in
  if String.length value <= limit then value
  else String.sub value 0 limit ^ "..."

let first_forwarded_ip value =
  match String.split_on_char ',' value with
  | first :: _ -> String.trim first
  | [] -> ""

let peer_of_headers ~cf_connecting_ip ~x_real_ip ~x_forwarded_for =
  let cf = String.trim cf_connecting_ip in
  if cf <> "" then cf
  else
    let xr = String.trim x_real_ip in
    if xr <> "" then xr
    else
      let xf = first_forwarded_ip x_forwarded_for in
      if xf <> "" then xf else "unknown"

let user_agent value =
  let ua = String.trim value in
  if ua = "" then "unknown"
  else if String.length ua > 96 then String.sub ua 0 96 ^ "..."
  else ua

let signed_root ~validator_pubkey h signed =
  `Assoc [
    "version", `String "octra-signed-root-v1";
    "chain_id", `String signed.Octra_consensus.C_light_root.chain_id;
    "validator_addr", `String signed.validator_addr;
    "validator_pubkey", `String validator_pubkey;
    "head_epoch", `String (Int64.to_string signed.head_epoch);
    "state_root", `String h.Octra_core.Head_manifest.state_root;
    "ledger_state_root", opt_hex h.ledger_state_root;
    "epoch_index_root", opt_hex h.epoch_index_root;
    "txid_hi", `String (Int64.to_string signed.txid_hi);
    "config_hash", `String (raw_to_hex signed.config_hash);
    "signature", `String (Base64.encode_exn signed.signature);
  ]

let account_witness ~inclusion (p : Octra_consensus.C_light_account.t) =
  let opt = function
    | None -> `Null
    | Some s -> `String s
  in
  `Assoc [
    "proof_kind", `String "account_witness_v1";
    "inclusion", `Bool inclusion;
    "chain_id", `String p.chain_id;
    "address", `String p.address;
    "head_epoch", `String (Int64.to_string p.head_epoch);
    "state_root", `String (raw_to_hex p.state_root);
    "balance", `String p.balance;
    "nonce", `Int p.nonce;
    "public_key", opt p.public_key;
    "encrypted_balance", opt p.encrypted_balance;
    "decrypt_allowance", `String p.decrypt_allowance;
    "account_hash", `String (raw_to_hex p.account_hash);
  ]

let account_irmin_proof ~addr (p : Octra_core.Store_irmin.account_merkle_proof) ~exists =
  `Assoc [
    "proof_kind", `String "irmin_account_path_v1";
    "path", `List [`String "accounts"; `String addr; `String "data"];
    "ledger_state_root", `String p.ledger_state_root;
    "exists", `Bool exists;
    "proof", `String p.proof;
  ]

let account_proof ~root ~exists ~account ~irmin_proof =
  `Assoc [
    "version", `String "octra-account-proof-v2";
    "root", root;
    "exists", `Bool exists;
    "account", account;
    "irmin_proof", irmin_proof;
  ]

let validator ~weighted
    (v : Octra_consensus.C_light_validator_set.validator) =
  `Assoc ([
    "address", `String v.address;
    "pubkey", `String (Base64.encode_exn v.pubkey);
  ] @
    if weighted then ["weight", `String (Z.to_string v.weight)]
    else [])

let scheduled_validator = function
  | None -> `Null
  | Some s ->
    `Assoc [
      "activate_epoch", `String (Int64.to_string s.Octra_consensus.C_light_validator_set.activate_epoch);
      "weighted", `Bool s.weighted;
      "validators", `List (List.map (validator ~weighted:s.weighted) s.validators);
    ]

let validator_set_proof proof =
  let weighted_proof =
    proof.Octra_consensus.C_light_validator_set.weighted
    || Option.fold
         ~none:false
         ~some:(fun
           (scheduled : Octra_consensus.C_light_validator_set.scheduled) ->
           scheduled.weighted)
         proof.scheduled
  in
  let version, profile =
    if weighted_proof then
      let trust =
        match proof.program_trust_hash with
        | None -> `Null
        | Some hash -> `String (raw_to_hex hash)
      in
      let runtime =
        match proof.runtime_profile_hash with
        | None -> `Null
        | Some hash -> `String (raw_to_hex hash)
      in
      "octra-validator-set-proof-v4", [
        "program_trust_hash", trust;
        "runtime_profile_hash", runtime;
        "weighted", `Bool proof.weighted;
        "total_weight", `String (Z.to_string proof.total_weight);
        "quorum_weight", `String (Z.to_string proof.quorum_weight);
      ]
    else
    match proof.runtime_profile_hash with
    | Some value ->
      let trust =
        match proof.program_trust_hash with
        | None -> `Null
        | Some hash -> `String (raw_to_hex hash)
      in
      "octra-validator-set-proof-v3", [
        "program_trust_hash", trust;
        "runtime_profile_hash", `String (raw_to_hex value);
      ]
    | None ->
      match proof.program_trust_hash with
      | None -> "octra-validator-set-proof-v1", []
      | Some value ->
        "octra-validator-set-proof-v2",
        ["program_trust_hash", `String (raw_to_hex value)]
  in
  `Assoc ([
    "version", `String version;
    "chain_id", `String proof.Octra_consensus.C_light_validator_set.chain_id;
    "config_hash", `String (raw_to_hex proof.config_hash);
    "validator_set_hash", `String (raw_to_hex proof.validator_set_hash);
    "n", `Int proof.n;
    "f", `Int proof.f;
    "quorum", `Int proof.quorum;
    "validators",
      `List
        (List.map
           (validator ~weighted:proof.weighted)
           proof.validators);
    "scheduled", scheduled_validator proof.scheduled;
  ] @ profile)

let light_epoch (p : Octra_consensus.C_light_epoch.t) =
  `Assoc [
    "chain_id", `String p.chain_id;
    "epoch_id", `String (Int64.to_string p.epoch_id);
    "tx_hashes", `List (List.map (fun h -> `String h) p.tx_hashes);
    "start_txid", `String (Int64.to_string p.start_txid);
    "tx_count", `Int p.tx_count;
    "prev_epoch_index_root", `String p.prev_epoch_index_root;
    "epoch_index_root", `String p.epoch_index_root;
  ]

let epoch_proof proof =
  `Assoc [
    "proof_kind", `String "epoch_inclusion";
    "epoch", light_epoch proof;
  ]

let tx_json_value raw =
  try Yojson.Safe.from_string raw with _ -> `String raw

let tx_inclusion_proof proof ~tx_json =
  `Assoc [
    "proof_kind", `String "tx_inclusion";
    "tx_hash", `String proof.Octra_consensus.C_light_epoch.tx_hash;
    "index", `Int proof.index;
    "epoch", light_epoch proof.epoch;
    "tx", tx_json_value tx_json;
  ]

let head_fields = function
  | None ->
    [
      "head_epoch", `Null;
      "state_root", `Null;
      "txid_hi", `Null;
      "irmin_commit", `Null;
    ]
  | Some h ->
    [
      "head_epoch", `Int h.Octra_core.Head_manifest.epoch_id;
      "state_root", `String h.Octra_core.Head_manifest.state_root;
      "txid_hi", `String (Int64.to_string h.Octra_core.Head_manifest.txid_hi);
      "irmin_commit", opt_hex h.Octra_core.Head_manifest.irmin_commit;
    ]

let node_status ~epoch ~validator ~roots ~timestamp ~head =
  `Assoc ([
    "epoch", `Int epoch;
    "current_epoch", `Int epoch;
    "validator", `String validator;
    "roots", `Int roots;
    "timestamp", `Float timestamp;
    "network_version", `String "v3.0.0-irmin";
  ] @ head_fields head)

let format_balance = Octra_core.Denomination.format_balance

let node_stats ~current_epoch ~total_accounts ~active_accounts ~display_supply
    ~encrypted_supply ~max_supply ~total_confirmed ~staging ~recent_tx_count
    ~latest_epochs ~head =
  `Assoc ([
    "current_epoch", `Int current_epoch;
    "total_accounts", `Int total_accounts;
    "active_accounts", `Int active_accounts;
    "total_supply", `String (format_balance display_supply);
    "total_supply_raw", `String (Z.to_string display_supply);
    "circulating_supply", `String (format_balance display_supply);
    "circulating_supply_raw", `String (Z.to_string display_supply);
    "encrypted_supply", `String (format_balance encrypted_supply);
    "encrypted_supply_raw", `String (Z.to_string encrypted_supply);
    "max_supply", `String (format_balance max_supply);
    "max_supply_raw", `String (Z.to_string max_supply);
    "supply_remaining", `String (format_balance (Z.sub max_supply display_supply));
    "supply_pct", `Float (Z.to_float display_supply /. Z.to_float max_supply *. 100.0);
    "circulating_pct", `Float (Z.to_float display_supply /. Z.to_float max_supply *. 100.0);
    "total_transactions", `Int (total_confirmed + staging);
    "recent_tx_count", `Int recent_tx_count;
    "staging_size", `Int staging;
    "latest_epochs", `List (List.map (fun e -> `Int e) latest_epochs);
  ] @ head_fields head)

let node_version () =
  `Assoc [
    "node", `String "octra";
    "version", `String "3.0.0";
    "protocol", `String "json-rpc-2.0";
  ]

let runtime_version ~source_commit ~binary_hash ~consensus_profile
    ~consensus_rules_id ~runtime_profile_hash ~config_hash ~chain_id
    ~validator =
  let runtime_profile_hash =
    match runtime_profile_hash with
    | None -> `Null
    | Some value -> `String (raw_to_hex value)
  in
  `Assoc [
    "schema", `String "octra-runtime-release-v1";
    "schema_version", `Int 1;
    "node", `String "octra";
    "node_version", `String "3.0.0";
    "source_commit", opt_string source_commit;
    "binary_hash", opt_string binary_hash;
    "consensus_profile", `Int consensus_profile;
    "consensus_rules_id", `String consensus_rules_id;
    "runtime_profile_hash", runtime_profile_hash;
    "config_hash", `String (raw_to_hex config_hash);
    "chain_id", `String chain_id;
    "validator", `String validator;
    "self_reported", `Bool true;
  ]

let validator_view_pubkey ~validator_view_pub ~validator_address =
  `Assoc [
    "validator_view_pubkey", `String (Base64.encode_exn validator_view_pub);
    "validator_address", `String validator_address;
  ]

let epoch_tags ~tags ~keep_epochs =
  let min_tag =
    match tags with
    | first :: _ -> first
    | [] -> 0
  in
  let max_tag =
    match List.rev tags with
    | last :: _ -> last
    | [] -> 0
  in
  `Assoc [
    "count", `Int (List.length tags);
    "min_epoch", `Int min_tag;
    "max_epoch", `Int max_tag;
    "keep_epochs", `Int keep_epochs;
  ]

let balance ~addr ~account ~pending_nonce =
  `Assoc [
    "address", `String addr;
    "balance", `String (format_balance account.Ledger.balance);
    "balance_raw", `String (Z.to_string account.Ledger.balance);
    "nonce", `Int account.Ledger.nonce;
    "pending_nonce", `Int pending_nonce;
    "has_public_key", `Bool (Option.is_some account.Ledger.public_key);
  ]

let account_public_balance_or_zero = function
  | Some account -> Z.to_string account.Ledger.balance
  | None -> "0"

let account_encrypted_balance_or_zero = function
  | Some account -> Option.value ~default:"0" account.Ledger.encrypted_balance
  | None -> "0"

let nonce ~addr ~account =
  `Assoc [
    "address", `String addr;
    "nonce", `Int account.Ledger.nonce;
  ]

let public_key ~addr ~public_key =
  `Assoc [
    "address", `String addr;
    "public_key", `String public_key;
  ]

let validate_address ~raw ~is_valid =
  let error_msg =
    if is_valid then `Null
    else if not (String.starts_with ~prefix:"oct" raw) then `String "must start with 'oct'"
    else if String.length raw <> 47 then `String (Printf.sprintf "expected length 47, got %d" (String.length raw))
    else `String "invalid base58 characters"
  in
  `Assoc [
    "address", `String raw;
    "valid", `Bool is_valid;
    "error", error_msg;
  ]

let supply ~display_supply ~encrypted_supply ~max_supply ~emission_remaining
    ~retired_supply =
  let remaining = Z.sub max_supply display_supply in
  `Assoc [
    "total_supply", `String (format_balance display_supply);
    "total_supply_raw", `String (Z.to_string display_supply);
    "circulating_supply", `String (format_balance display_supply);
    "circulating_supply_raw", `String (Z.to_string display_supply);
    "encrypted_supply", `String (format_balance encrypted_supply);
    "encrypted_supply_raw", `String (Z.to_string encrypted_supply);
    "max_supply", `String (format_balance max_supply);
    "max_supply_raw", `String (Z.to_string max_supply);
    "supply_remaining", `String (format_balance remaining);
    "supply_remaining_raw", `String (Z.to_string remaining);
    "emission_remaining", `String (format_balance emission_remaining);
    "emission_remaining_raw", `String (Z.to_string emission_remaining);
    "retired_supply", `String (format_balance retired_supply);
    "retired_supply_raw", `String (Z.to_string retired_supply);
  ]

let storage_visible ?limit value =
  match limit with
  | Some limit when String.length value > limit -> String.sub value 0 limit
  | _ -> value

let storage_assoc ?limit pairs =
  `Assoc (List.map (fun (key, value) ->
    (key, `String (storage_visible ?limit value))
  ) pairs)

let storage_sizes pairs =
  `Assoc (List.map (fun (key, value) -> (key, `Int (String.length value))) pairs)

let storage_value ~key ~value ~limit =
  let size = String.length value in
  `Assoc [
    "key", `String key;
    "value", `String (storage_visible ~limit value);
    "size", `Int size;
    "truncated", `Bool (size > limit);
    "limit", `Int limit;
  ]

let storage_missing ~key =
  `Assoc [
    "key", `String key;
    "value", `Null;
    "size", `Int 0;
    "truncated", `Bool false;
  ]

let program_storage_dump ~address storage_pairs =
  `Assoc [
    "address", `String address;
    "storage", storage_assoc storage_pairs;
    "storage_sizes", storage_sizes storage_pairs;
    "count", `Int (List.length storage_pairs);
  ]

let program_bytecode ~address ~bytecode_b64 ~code_hash =
  `Assoc [
    "address", `String address;
    "bytecode", `String bytecode_b64;
    "code_hash", `String code_hash;
    "size", `Int (String.length bytecode_b64);
  ]

let program_info ~address ~version ~code_hash ~balance ~owner =
  `Assoc [
    "address", `String address;
    "version", `String version;
    "code_hash", `String code_hash;
    "balance", `String balance;
    "owner", `String owner;
  ]

let program_abi_saved =
  `Assoc ["saved", `Bool true]

let pvac_pubkey ~addr = function
  | None ->
    `Assoc [
      "address", `String addr;
      "pvac_pubkey", `Null;
    ]
  | Some pk_blob ->
    `Assoc [
      "address", `String addr;
      "pvac_pubkey", `String (Base64.encode_exn pk_blob);
      "pubkey_size", `Int (String.length pk_blob);
    ]

let pvac_already_registered ~addr =
  `Assoc [
    "address", `String addr;
    "status", `String "already_registered";
    "registration", `String "canonical_key_switch";
  ]

let pvac_status ~addr (status : Pvac_registry.status) =
  `Assoc [
    "address", `String addr;
    "has_pvac_pubkey", `Bool status.has_pvac_pubkey;
    "pubkey_size", opt_int status.pubkey_size;
    "deserializable", `Null;
    "canonical_binding", `Bool status.canonical_binding;
    "pubkey_format", opt_string status.pubkey_format;
  ]

let pvac_migration_status ~addr ~cipher ~epoch ~owner_migration_mode
    (status : Pvac_migration.status) entitlements =
  let entitlement =
    if Pvac_migration.needs_history_migration status then
      Some
        (Octra_core.Pvac_migration_entitlement.find
           entitlements
           ~epoch
           ~address:addr
           ~cipher)
    else
      None
  in
  let can_owner_proof_migrate =
    match owner_migration_mode, status.cipher_class, status.key_class, entitlement with
    | Rule_graph.Active, Pvac_migration.V3, Pvac_migration.Historical,
      Some (Ok entry)
      when
        entry.Octra_core.Pvac_migration_entitlement.decision.audit_class
        <> Pvac_legacy_public_replay.Poisoned
        && Option.is_some entry.decision.commitment_net ->
      true
    | _ -> false
  in
  let replay_json =
    match entitlement with
    | None -> `Null
    | Some result ->
      match result with
      | Error reason ->
        `Assoc [
          "total", `Int 0;
          "scanned", `Int 0;
          "complete", `Bool false;
          "audit_class", `String "poisoned";
          "can_public_migrate", `Bool false;
          "public_net", `Null;
          "commitment_net", `Null;
          "blockers", `List [`String reason];
          "reason", `String reason;
        ]
      | Ok entry ->
        let decision = entry.Octra_core.Pvac_migration_entitlement.decision in
        `Assoc [
          "total", `Int entry.total;
          "scanned", `Int entry.total;
          "complete", `Bool true;
          "audit_class",
            `String
              (Pvac_legacy_public_replay.string_of_audit_class
                 decision.audit_class);
          "can_public_migrate", `Bool decision.can_public_migrate;
          "public_net",
            (match decision.public_net with
             | None -> `Null
             | Some amount -> `String (Z.to_string amount));
          "commitment_net",
            (match decision.commitment_net with
             | None -> `Null
             | Some commitment -> `String commitment);
          "blockers",
            `List (List.map (fun value -> `String value) decision.blockers);
          "reason", `String decision.reason;
        ]
  in
  `Assoc [
    "address", `String addr;
    "cipher_class", `String (Pvac_migration.cipher_class_to_string status.cipher_class);
    "key_class", `String (Pvac_migration.key_class_to_string status.key_class);
    "target_key_class", `String "current";
    "migration_route", `String (Pvac_migration.route_to_string status.route);
    "can_key_switch", `Bool (Pvac_migration.can_key_switch status);
    "can_v3_migrate", `Bool (Pvac_migration.can_bound_migrate status);
    "can_owner_proof_migrate", `Bool can_owner_proof_migrate;
    "owner_migration_authority",
      `String
        (if can_owner_proof_migrate then
           "finalized_history_commitment"
         else
           "unavailable");
    "owner_proof_rule",
      `String
        (match owner_migration_mode with
         | Rule_graph.Active -> "active"
         | Rule_graph.Prior -> "prior");
    "needs_legacy_public_replay", `Bool (Pvac_migration.needs_history_migration status);
    "reason", `String status.reason;
    "entitlement_root",
      (match Octra_core.Pvac_migration_entitlement.root entitlements with
       | None -> `Null
       | Some value -> `String value);
    "entitlement_activation_epoch",
      (match Octra_core.Pvac_migration_entitlement.activation_epoch entitlements with
       | None -> `Null
       | Some value -> `Int value);
    "entitlement_snapshot_epoch",
      (match Octra_core.Pvac_migration_entitlement.snapshot_epoch entitlements with
       | None -> `Null
       | Some value -> `Int value);
    "entitlement_state_root",
      (match Octra_core.Pvac_migration_entitlement.state_root entitlements with
       | None -> `Null
       | Some value -> `String value);
    "legacy_public_replay", replay_json;
  ]

let encrypted_cipher ~addr ~cipher =
  let cipher_type =
    if FheBalance.is_fhe_cipher cipher then "pvac_fhe"
    else if not (String.equal cipher "0") && not (String.equal cipher "") then "legacy_aes_gcm"
    else "none"
  in
  `Assoc [
    "address", `String addr;
    "cipher", `String cipher;
    "cipher_type", `String cipher_type;
  ]

let encrypted_balance ~addr ~cipher ~has_pvac_pubkey =
  `Assoc [
    "address", `String addr;
    "cipher", `String cipher;
    "has_pvac_pubkey", `Bool has_pvac_pubkey;
  ]

let public_key_registration_ignored_bft ~addr =
  `Assoc [
    "ok", `Bool true;
    "address", `String addr;
    "status", `String "ignored_in_bft";
    "note", `String "public key registration is a no-op in BFT mode; tx.public_key must be carried in canonical transactions";
  ]

let public_key_registered ~addr =
  `Assoc [
    "ok", `Bool true;
    "address", `String addr;
  ]

let account_view_pubkey ~addr ~view_pubkey ~reason =
  let reason_field =
    match reason with
    | Some value -> ["reason", `String value]
    | None -> []
  in
  `Assoc ([
    "address", `String addr;
    "view_pubkey", opt_string view_pubkey;
  ] @ reason_field)

let stealth_outputs ~from_epoch ~outputs =
  `Assoc [
    "from_epoch", `Int from_epoch;
    "count", `Int (List.length outputs);
    "outputs", `List outputs;
  ]

let stealth_outputs_page
    ~from_epoch
    ~before_id
    ~outputs
    ~next_before_id
    ~has_more
    ~scanned =
  `Assoc [
    "from_epoch", `Int from_epoch;
    "before_id", (match before_id with Some id -> `Intlit (Int64.to_string id) | None -> `Null);
    "count", `Int (List.length outputs);
    "scanned", `Int scanned;
    "has_more", `Bool has_more;
    "next_before_id",
      (match next_before_id with Some id -> `Intlit (Int64.to_string id) | None -> `Null);
    "outputs", `List outputs;
  ]

let stealth_outputs_by_id ~requested ~outputs =
  `Assoc [
    "requested", `Int requested;
    "count", `Int (List.length outputs);
    "complete", `Bool (List.length outputs = requested);
    "outputs", `List outputs;
  ]

let optional_json_field name raw =
  if raw = "" then
    []
  else
    try [name, Yojson.Safe.from_string raw]
    with _ -> []

let compile_assembly ~bytecode_b64 ~bytecode_size ~instructions =
  `Assoc [
    "bytecode", `String bytecode_b64;
    "size", `Int bytecode_size;
    "instructions", `Int instructions;
  ]

let compile_aml
    ~bytecode_b64
    ~bytecode_size
    ~instructions
    ~abi_json
    ~version
    ~disasm
    ~verification_json
    ~certificate_json =
  `Assoc
    ([
      "bytecode", `String bytecode_b64;
      "size", `Int bytecode_size;
      "instructions", `Int instructions;
      "abi", `String abi_json;
      "version", `String version;
      "disasm", `String disasm;
    ]
    @ optional_json_field "verification" verification_json
    @ optional_json_field "certificate" certificate_json)

let program_address ~address ~deployer ~nonce =
  `Assoc [
    "address", `String address;
    "deployer", `String deployer;
    "nonce", `Int nonce;
  ]

let total_transactions ~confirmed ~staging =
  `Assoc [
    "confirmed", `Int confirmed;
    "staging", `Int staging;
    "total", `Int (confirmed + staging);
  ]

let submit_accepted ~tx_hash ~nonce ~ou_cost =
  `Assoc [
    "tx_hash", `String tx_hash;
    "status", `String "accepted";
    "nonce", `Int nonce;
    "ou_cost", `String (Z.to_string ou_cost);
  ]

let private_transfer_pending ~tx_hash =
  `Assoc [
    "tx_hash", `String tx_hash;
    "status", `String "pending";
  ]

let submit_batch_decode_error ~reason =
  `Assoc [
    "status", `String "error";
    "reason", `String reason;
  ]

let submit_batch_rejected ~reason ~nonce =
  `Assoc [
    "status", `String "rejected";
    "reason", `String reason;
    "nonce", `Int nonce;
  ]

let submit_batch_accepted ~tx_hash ~nonce =
  `Assoc [
    "status", `String "accepted";
    "tx_hash", `String tx_hash;
    "nonce", `Int nonce;
  ]

let accepted_batch_row = function
  | `Assoc fields ->
    (match List.assoc_opt "status" fields with
     | Some (`String "accepted") -> true
     | _ -> false)
  | _ -> false

let submit_batch results =
  let total = List.length results in
  let accepted =
    List.fold_left
      (fun count row -> if accepted_batch_row row then count + 1 else count)
      0
      results
  in
  `Assoc [
    "total", `Int total;
    "accepted", `Int accepted;
    "rejected", `Int (total - accepted);
    "results", `List results;
  ]

let peer_source (row : Octra_net.P2p_peer_diag.source) =
  `Assoc [
    "bucket", `String row.bucket;
    "records", `Int row.records;
    "connected", `Int row.connected;
  ]

let peer_score ~now (row : Octra_net.P2p_peer_guard.score_record) =
  let remaining = max 0.0 (row.ban_until -. now) in
  `Assoc [
    "key", `String row.key;
    "bad_count", `Int row.bad_count;
    "conn_count", `Int row.conn_count;
    "ban_until", `Float row.ban_until;
    "ban_remaining_sec", `Float remaining;
    "last_reason", `String row.last_reason;
    "invalid_frame_count", `Int row.invalid_frame_count;
    "bad_signature_count", `Int row.bad_signature_count;
    "stale_root_count", `Int row.stale_root_count;
  ]

let peer_record_rejection (row : Octra_net.P2p_peer_diag.rejection) =
  `Assoc [
    "reason", `String row.reason;
    "count", `Int row.count;
  ]

let relayed_record_diag (stats : Octra_net.P2p_peer_diag.relayed_records) =
  `Assoc [
    "accepted", `Int stats.accepted;
    "unchanged", `Int stats.unchanged;
    "rejected", `Int stats.rejected;
    "rejections", `List (List.map peer_record_rejection stats.rejections);
  ]

let peer_diag = function
  | None -> fun ~now:_ -> `Null, []
  | Some diag ->
    fun ~now ->
      let json =
        `Assoc [
          "connected", `Int diag.Octra_net.P2p_peer_diag.connected;
          "known", `Int diag.known;
          "public_sources", `Int diag.public_sources;
          "sources", `List (List.map peer_source diag.sources);
          "relayed_records", relayed_record_diag diag.relayed_records;
          "risks", `List (List.map (fun s -> `String s) diag.risks);
        ]
      in
      json, List.map (peer_score ~now) diag.scores

let consensus_peer ~now (row : Octra_consensus.C_driver.peer_state_record) =
  `Assoc [
    "responder_addr", `String row.responder_addr;
    "head_epoch", `String (Int64.to_string row.head_epoch);
    "checked_epoch", `String (Int64.to_string row.checked_epoch);
    "state_root",
      (match row.state_root with
       | Some root -> `String (raw_to_hex root)
       | None -> `Null);
    "source", `String row.source;
    "age_sec", `Float (max 0.0 (now -. row.last_seen));
  ]

let consensus_peer_states ~enabled ~peers ~scores ~diag =
  `Assoc [
    "enabled", `Bool enabled;
    "peers", `List peers;
    "scores", `List scores;
    "p2p_diagnostics", diag;
  ]