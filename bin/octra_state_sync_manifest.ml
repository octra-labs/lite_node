(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Manifest = Octra_bootstrap.State_sync_manifest
module Journal = Octra_bootstrap.State_sync_journal
module State_sync = Octra_bootstrap.State_sync
module Verify = Octra_bootstrap.State_sync_verify

let command = ref ""
let source_dir = ref ""
let chain_id = ref ""
let config_hash = ref ""
let chunk_size = ref (16 * 1024 * 1024)
let validity_seconds = ref 604_800L
let output_path = ref ""
let draft_path = ref ""
let certificate_path = ref ""
let wallet_path = ref ""
let checkpoint_signature_paths = ref []
let exporter_signature_path = ref ""
let validator_entries = ref []
let exporter_entries = ref []
let historical = ref false

let usage =
  "octra_state_sync_manifest --command seal|build|sign-checkpoint|sign-manifest|assemble|verify"

let options = [
  "--command", Arg.Set_string command, "operation";
  "--source", Arg.Set_string source_dir, "local state or immutable snapshot";
  "--chain-id", Arg.Set_string chain_id, "chain identifier";
  "--config-hash", Arg.Set_string config_hash, "consensus config hash";
  "--chunk-size", Arg.Set_int chunk_size, "chunk size";
  "--validity", Arg.String (fun value -> validity_seconds := Int64.of_string value),
    "checkpoint validity seconds";
  "--output", Arg.Set_string output_path, "output path";
  "--draft", Arg.Set_string draft_path, "checkpoint manifest draft";
  "--certificate", Arg.Set_string certificate_path, "certificate path";
  "--wallet", Arg.Set_string wallet_path, "signer wallet path";
  "--checkpoint-signature",
    Arg.String (fun value -> checkpoint_signature_paths := value :: !checkpoint_signature_paths),
    "checkpoint signature path";
  "--exporter-signature", Arg.Set_string exporter_signature_path,
    "exporter signature path";
  "--validator", Arg.String (fun value -> validator_entries := value :: !validator_entries),
    "trusted validator address:pubkey";
  "--exporter", Arg.String (fun value -> exporter_entries := value :: !exporter_entries),
    "trusted exporter address:pubkey";
  "--historical", Arg.Set historical, "verify checkpoint against live history";
]

let fail message =
  Printf.eprintf "error = %s\n%!" message;
  exit 1

let require name value =
  if String.trim value = "" then fail (name ^ " is required") else String.trim value

let read_file_limited path limit =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
      let size = in_channel_length input in
      if size > limit then fail "input exceeds size limit";
      really_input_string input size)

let load_draft () =
  match Manifest.load_draft (require "draft" !draft_path) with
  | Ok draft -> draft
  | Error reason -> fail reason

let load_certificate () =
  match Manifest.load_certificate (require "certificate" !certificate_path) with
  | Ok certificate -> certificate
  | Error reason -> fail reason

let parse_signature path =
  let raw = read_file_limited path 16_384 in
  let json =
    try Yojson.Safe.from_string raw
    with exn -> fail (Printexc.to_string exn)
  in
  match Checkpoint.parse_signature json with
  | Ok signature -> signature
  | Error reason -> fail reason

let signer_set name entries =
  match Manifest.validator_set_of_entries (List.rev entries) with
  | Ok signer_set -> signer_set
  | Error reason -> fail (name ^ ": " ^ reason)

let validator_set () =
  signer_set "validator set" !validator_entries

let exporter_set () =
  signer_set "exporter set" !exporter_entries

let load_wallet () =
  let path = require "wallet" !wallet_path in
  if not (Sys.file_exists path) then fail "wallet file is missing";
  Octra_core.Crypto.Wallet.ensure path

let exact_fields expected fields =
  let actual = List.map fst fields |> List.sort String.compare in
  let expected = List.sort String.compare expected in
  if actual <> expected then fail "snapshot ready marker has unexpected fields"

let ready_string fields name =
  match List.assoc_opt name fields with
  | Some (`String value) -> value
  | _ -> fail ("snapshot ready marker has invalid " ^ name)

let ready_int fields name =
  match List.assoc_opt name fields with
  | Some (`Int value) -> value
  | _ -> fail ("snapshot ready marker has invalid " ^ name)

let verify_ready checkpoint source =
  let path = State_sync.snapshot_ready_path source in
  if not (Sys.file_exists path) then fail "source is not an immutable ready snapshot";
  let raw = read_file_limited path 16_384 in
  let fields =
    match Yojson.Safe.from_string raw with
    | `Assoc fields -> fields
    | _ -> fail "snapshot ready marker must be an object"
  in
  exact_fields
    ["version"; "head_epoch"; "state_root"; "commit_id"; "created_at"]
    fields;
  if ready_string fields "version" <> "octra-state-sync-snapshot-ready" then
    fail "snapshot ready marker version mismatch";
  if Int64.of_int (ready_int fields "head_epoch") <> checkpoint.Checkpoint.epoch then
    fail "snapshot ready marker epoch mismatch";
  if ready_string fields "state_root" <> checkpoint.state_root then
    fail "snapshot ready marker state root mismatch"

let load_matching_head checkpoint source =
  match Octra_core.Head_manifest.load_result source with
  | Octra_core.Head_manifest.Missing -> fail "snapshot HEAD.json is missing"
  | Octra_core.Head_manifest.Corrupt reason -> fail ("snapshot HEAD.json is corrupt: " ^ reason)
  | Octra_core.Head_manifest.Present head ->
      if not (Checkpoint.matches_head checkpoint head) then
        fail "local HEAD.json does not match checkpoint";
      head

let verify_snapshot (draft : Manifest.draft) source =
  verify_ready draft.Manifest.checkpoint source;
  let head = load_matching_head draft.checkpoint source in
  let ready =
    Yojson.Safe.from_file (State_sync.snapshot_ready_path source)
  in
  let ready_commit =
    match ready with
    | `Assoc fields -> ready_string fields "commit_id"
    | _ -> fail "snapshot ready marker must be an object"
  in
  if head.Octra_core.Head_manifest.commit_id <> ready_commit then
    fail "snapshot ready marker commit mismatch";
  let actual = State_sync.list_files source in
  let expected =
    List.rev
      (List.rev_map (fun file -> file.Manifest.path) draft.manifest.files)
  in
  if actual <> expected then fail "snapshot file set does not match manifest";
  List.iter (fun file ->
    let path = Filename.concat source file.Manifest.path in
    match Journal.hash_file path with
    | Error reason -> fail reason
    | Ok hash when hash <> file.sha256 -> fail ("snapshot file hash mismatch: " ^ file.path)
    | Ok _ -> ()
  ) draft.manifest.files;
  match Lwt_main.run (Verify.verify draft.checkpoint source) with
  | Ok () -> ()
  | Error reason -> fail reason

let seal () =
  let source = require "source" !source_dir in
  let chain = require "chain-id" !chain_id in
  let config = require "config-hash" !config_hash in
  if not (Checkpoint.lower_hex 64 config) then fail "config hash must be lowercase hex64";
  let validators = validator_set () in
  if not (Sys.file_exists source && (Unix.stat source).Unix.st_kind = Unix.S_DIR) then
    fail "snapshot source must be a directory";
  List.iter (fun name ->
    if not (Sys.file_exists (Filename.concat source name)) then
      fail ("snapshot source is missing " ^ name)
  ) ["HEAD.json"; "irmin_store"; "chaindata"];
  let head =
    match Octra_core.Head_manifest.load_result source with
    | Octra_core.Head_manifest.Missing -> fail "snapshot HEAD.json is missing"
    | Octra_core.Head_manifest.Corrupt reason -> fail ("snapshot HEAD.json is corrupt: " ^ reason)
    | Octra_core.Head_manifest.Present head -> head
  in
  if not (State_sync.stable_head source head) then
    fail "snapshot source is not stable";
  let created_at = Int64.of_float (Unix.gettimeofday ()) in
  let checkpoint =
    Checkpoint.of_head
      ~chain_id:chain
      ~config_hash:config
      ~validator_set_hash:(Manifest.set_hash validators)
      ~created_at
      ~valid_until:(Int64.add created_at !validity_seconds)
      head
  in
  begin
    match Lwt_main.run (Verify.verify checkpoint source) with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  State_sync.write_ready source head;
  Printf.printf
    "event = snapshot_sealed epoch = %d state_root = %s source = %s\n%!"
    head.epoch_id
    head.state_root
    source

let require_wallet_member signer_set wallet =
  match Octra_consensus.C_types.pubkey_of_addr signer_set wallet.Octra_core.Crypto.Wallet.address with
  | Some pubkey when pubkey = wallet.pub -> ()
  | _ -> fail "wallet is not in the configured signer set"

let build () =
  let source = require "source" !source_dir in
  let chain = require "chain-id" !chain_id in
  let config = require "config-hash" !config_hash in
  let output = require "output" !output_path in
  if not (Checkpoint.lower_hex 64 config) then fail "config hash must be lowercase hex64";
  if Int64.compare !validity_seconds 1L < 0
     || Int64.compare !validity_seconds 2_678_400L > 0 then
    fail "validity must be in 1..2678400 seconds";
  let validators = validator_set () in
  let head =
    match Octra_core.Head_manifest.load_result source with
    | Octra_core.Head_manifest.Missing -> fail "snapshot HEAD.json is missing"
    | Octra_core.Head_manifest.Corrupt reason -> fail ("snapshot HEAD.json is corrupt: " ^ reason)
    | Octra_core.Head_manifest.Present head -> head
  in
  let created_at = Int64.of_float (Unix.gettimeofday ()) in
  let checkpoint =
    Checkpoint.of_head
      ~chain_id:chain
      ~config_hash:config
      ~validator_set_hash:(Manifest.set_hash validators)
      ~created_at
      ~valid_until:(Int64.add created_at !validity_seconds)
      head
  in
  match Manifest.build
    ~checkpoint
    ~source_dir:source
    ~chunk_size:!chunk_size with
  | Error reason -> fail reason
  | Ok draft ->
      verify_snapshot draft source;
      Manifest.write_json output (Manifest.draft_json draft);
      Printf.printf
        "event = manifest_built checkpoint = %s manifest = %s epoch = %Ld files = %d chunks = %d bytes = %Ld\n%!"
        draft.checkpoint_hash
        draft.manifest_hash
        draft.checkpoint.epoch
        draft.manifest.file_count
        draft.manifest.chunk_count
        draft.manifest.total_size

let sign_checkpoint () =
  let draft = load_draft () in
  begin
    match Manifest.verify_draft draft with
    | Ok _ -> ()
    | Error reason -> fail reason
  end;
  let validators = validator_set () in
  if draft.checkpoint.validator_set_hash <> Manifest.set_hash validators then
    fail "checkpoint validator set hash mismatch";
  if draft.checkpoint.chain_id <> require "chain-id" !chain_id then
    fail "checkpoint chain id mismatch";
  if draft.checkpoint.config_hash <> require "config-hash" !config_hash then
    fail "checkpoint config hash mismatch";
  let now = Int64.of_float (Unix.gettimeofday ()) in
  if Int64.compare (Int64.add now 300L) draft.checkpoint.created_at < 0
     || Int64.compare now draft.checkpoint.valid_until > 0 then
    fail "checkpoint is outside its validity window";
  let source = require "source" !source_dir in
  begin
    if !historical then
      match Verify.verify_historical draft.checkpoint source with
      | Ok () -> ()
      | Error reason -> fail reason
    else
      let head = load_matching_head draft.checkpoint source in
      if not (State_sync.stable_head source head) then
        fail "local checkpoint is not stable"
  end;
  let wallet = load_wallet () in
  require_wallet_member validators wallet;
  match Manifest.make_checkpoint_signature ~wallet draft.checkpoint with
  | Error reason -> fail reason
  | Ok signature ->
      Manifest.write_json
        (require "output" !output_path)
        (Checkpoint.signature_json signature);
      Printf.printf
        "event = checkpoint_signed hash = %s signer = %s\n%!"
        draft.checkpoint_hash
        signature.signer

let sign_manifest () =
  let draft = load_draft () in
  begin
    match Manifest.verify_draft draft with
    | Ok _ -> ()
    | Error reason -> fail reason
  end;
  verify_snapshot draft (require "source" !source_dir);
  let exporters = exporter_set () in
  let wallet = load_wallet () in
  require_wallet_member exporters wallet;
  match Manifest.make_exporter_signature ~wallet draft.manifest with
  | Error reason -> fail reason
  | Ok signature ->
      Manifest.write_json
        (require "output" !output_path)
        (Checkpoint.signature_json signature);
      Printf.printf
        "event = manifest_signed hash = %s exporter = %s\n%!"
        draft.manifest_hash
        signature.signer

let assemble () =
  let draft = load_draft () in
  if !source_dir <> "" then verify_snapshot draft !source_dir;
  let checkpoint_signatures =
    List.rev !checkpoint_signature_paths
    |> List.map parse_signature
    |> List.sort (fun left right -> String.compare left.Checkpoint.signer right.signer)
  in
  let exporter_signature =
    parse_signature (require "exporter-signature" !exporter_signature_path)
  in
  let certificate = Manifest.{
    checkpoint = draft.checkpoint;
    checkpoint_hash = draft.checkpoint_hash;
    checkpoint_signatures;
    manifest = draft.manifest;
    manifest_hash = draft.manifest_hash;
    exporter_signature;
  } in
  let validators = validator_set () in
  let exporters = exporter_set () in
  match Manifest.verify_certificate ~validator_set:validators ~exporter_set:exporters certificate with
  | Error reason -> fail reason
  | Ok verified ->
      Manifest.write_json
        (require "output" !output_path)
        (Manifest.certificate_json verified);
      Printf.printf
        "event = certificate_assembled checkpoint = %s manifest = %s signatures = %d quorum = %d exporter = %s\n%!"
        verified.checkpoint_hash
        verified.manifest_hash
        (List.length verified.checkpoint_signatures)
        validators.Octra_consensus.C_types.quorum
        verified.exporter_signature.signer

let verify () =
  let certificate = load_certificate () in
  let validators = validator_set () in
  let exporters = exporter_set () in
  match Manifest.verify_certificate ~validator_set:validators ~exporter_set:exporters certificate with
  | Error reason -> fail reason
  | Ok verified ->
      let now = Int64.of_float (Unix.gettimeofday ()) in
      if not (Manifest.fresh ~now verified) then fail "certificate is outside its validity window";
      if !source_dir <> "" then
        verify_snapshot Manifest.{
          checkpoint = verified.checkpoint;
          checkpoint_hash = verified.checkpoint_hash;
          manifest = verified.manifest;
          manifest_hash = verified.manifest_hash;
        } !source_dir;
      Printf.printf
        "event = certificate_verified checkpoint = %s manifest = %s epoch = %Ld signatures = %d files = %d chunks = %d\n%!"
        verified.checkpoint_hash
        verified.manifest_hash
        verified.checkpoint.epoch
        (List.length verified.checkpoint_signatures)
        verified.manifest.file_count
        verified.manifest.chunk_count

let run () =
  Arg.parse options (fun value -> fail ("unexpected argument: " ^ value)) usage;
  match !command with
  | "seal" -> seal ()
  | "build" -> build ()
  | "sign-checkpoint" -> sign_checkpoint ()
  | "sign-manifest" -> sign_manifest ()
  | "assemble" -> assemble ()
  | "verify" -> verify ()
  | _ ->
      fail
        "command must be seal, build, sign-checkpoint, sign-manifest, assemble or verify"

let () =
  run ()