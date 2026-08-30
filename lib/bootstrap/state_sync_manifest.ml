(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Checkpoint = State_sync_checkpoint

type chunk = {
  index : int;
  offset : int64;
  size : int;
  sha256 : string;
}

type file = {
  path : string;
  size : int64;
  sha256 : string;
  chunks : chunk list;
}

type body = {
  checkpoint_hash : string;
  snapshot_id : string;
  irmin_commit : string option;
  chunk_size : int;
  total_size : int64;
  file_count : int;
  chunk_count : int;
  chunks_root : string;
  files : file list;
}

type draft = {
  checkpoint : Checkpoint.body;
  checkpoint_hash : string;
  manifest : body;
  manifest_hash : string;
}

type authority =
  | Checkpoint_quorum of Checkpoint.signature list
  | Finalized of string

type certificate = {
  checkpoint : Checkpoint.body;
  checkpoint_hash : string;
  authority : authority;
  manifest : body;
  manifest_hash : string;
  exporter_signatures : Checkpoint.signature list;
}

let format = "octra-state-sync"
let manifest_limit = 32 * 1024 * 1024
let file_limit = 200_000
let chunk_limit = 262_144
let path_limit = 1024
let chunk_size_min = State_sync_limits.chunk_min
let chunk_size_max = State_sync_limits.chunk_max

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let ( >>= ) value f =
  let* item = value in
  f item

let protect f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let raw_to_hex = Checkpoint.raw_to_hex

let is_lower_hex_64 value =
  String.length value = 64
  && String.for_all (function
    | '0'..'9' | 'a'..'f' -> true
    | _ -> false
  ) value

let normalize_path value =
  if value = ""
     || String.length value > path_limit
     || not (Filename.is_relative value)
     || String.contains value '\000'
     || String.contains value '\\' then
    None
  else
    let parts = String.split_on_char '/' value in
    if List.exists (fun part -> part = "" || part = "." || part = "..") parts then
      None
    else
      Some (String.concat "/" parts)

let valid_id value =
  value <> ""
  && String.length value <= 128
  && String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true
    | _ -> false
  ) value

let exact_fields expected fields =
  let actual = List.map fst fields |> List.sort String.compare in
  let expected = List.sort String.compare expected in
  if actual = expected then Ok () else Error "unexpected manifest fields"

let assoc fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let string_value = function
  | `String value -> Ok value
  | _ -> Error "expected string"

let int_value = function
  | `Int value -> Ok value
  | `Intlit value -> protect (fun () -> int_of_string value)
  | _ -> Error "expected integer"

let int64_value = function
  | `String value | `Intlit value -> protect (fun () -> Int64.of_string value)
  | `Int value -> Ok (Int64.of_int value)
  | _ -> Error "expected int64"

let list_value = function
  | `List values -> Ok values
  | _ -> Error "expected list"

let option_string_value = function
  | `Null -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error "expected optional string"

let encode_chunk buffer path chunk =
  Octra_net.Oce1.put_string buffer path;
  Octra_net.Oce1.put_u32_int buffer chunk.index;
  Octra_net.Oce1.put_u64 buffer chunk.offset;
  Octra_net.Oce1.put_u32_int buffer chunk.size;
  Octra_net.Oce1.put_hash32 buffer chunk.sha256

let chunks_root files =
  Octra_net.Hash_domain.hash_encoded_hex "octra:state_sync_chunks" (fun buffer ->
    let count =
      List.fold_left
        (fun total file -> total + List.length file.chunks)
        0
        files
    in
    Octra_net.Oce1.put_u32_int buffer count;
    List.iter (fun file ->
      List.iter (encode_chunk buffer file.path) file.chunks
    ) files)

let manifest_hash_raw (body : body) =
  Octra_net.Hash_domain.hash_encoded "octra:state_sync_manifest" (fun buffer ->
    Octra_net.Oce1.put_string buffer format;
    Octra_net.Oce1.put_hash32 buffer body.checkpoint_hash;
    Octra_net.Oce1.put_string buffer body.snapshot_id;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_string buffer body.irmin_commit;
    Octra_net.Oce1.put_u32_int buffer body.chunk_size;
    Octra_net.Oce1.put_u64 buffer body.total_size;
    Octra_net.Oce1.put_u32_int buffer body.file_count;
    Octra_net.Oce1.put_u32_int buffer body.chunk_count;
    Octra_net.Oce1.put_hash32 buffer body.chunks_root;
    Octra_net.Oce1.put_list (fun output file ->
      Octra_net.Oce1.put_string output file.path;
      Octra_net.Oce1.put_u64 output file.size;
      Octra_net.Oce1.put_hash32 output file.sha256;
      Octra_net.Oce1.put_u32_int output (List.length file.chunks);
      List.iter (encode_chunk output file.path) file.chunks
    ) buffer body.files)

let chunk_json chunk =
  `Assoc [
    "index", `Int chunk.index;
    "offset", `String (Int64.to_string chunk.offset);
    "size", `Int chunk.size;
    "sha256", `String chunk.sha256;
  ]

let file_json file =
  `Assoc [
    "path", `String file.path;
    "size", `String (Int64.to_string file.size);
    "sha256", `String file.sha256;
    "chunks", `List (List.rev (List.rev_map chunk_json file.chunks));
  ]

let body_json (body : body) =
  `Assoc [
    "version", `String format;
    "checkpoint_hash", `String body.checkpoint_hash;
    "snapshot_id", `String body.snapshot_id;
    "irmin_commit",
      (match body.irmin_commit with Some value -> `String value | None -> `Null);
    "chunk_size", `Int body.chunk_size;
    "total_size", `String (Int64.to_string body.total_size);
    "file_count", `Int body.file_count;
    "chunk_count", `Int body.chunk_count;
    "chunks_root", `String body.chunks_root;
    "files", `List (List.rev (List.rev_map file_json body.files));
  ]

let draft_json (draft : draft) =
  `Assoc [
    "version", `String format;
    "checkpoint", Checkpoint.body_json draft.checkpoint;
    "checkpoint_hash", `String draft.checkpoint_hash;
    "manifest", body_json draft.manifest;
    "manifest_hash", `String draft.manifest_hash;
  ]

let certificate_json certificate =
  let common = [
    "checkpoint", Checkpoint.body_json certificate.checkpoint;
    "checkpoint_hash", `String certificate.checkpoint_hash;
    "manifest", body_json certificate.manifest;
    "manifest_hash", `String certificate.manifest_hash;
  ] in
  match certificate.authority, certificate.exporter_signatures with
  | Checkpoint_quorum signatures, [exporter_signature] ->
      `Assoc (
        ("version", `String format)
        :: ("checkpoint_signatures",
          `List (List.map Checkpoint.signature_json signatures))
        :: ("exporter_signature", Checkpoint.signature_json exporter_signature)
        :: common)
  | Finalized finality, exporter_signatures ->
      `Assoc (
        ("version", `String format)
        :: ("finality", `String finality)
        :: ("exporter_signatures",
          `List (List.map Checkpoint.signature_json exporter_signatures))
        :: common)
  | Checkpoint_quorum _, _ ->
      invalid_arg "checkpoint certificate requires one exporter signature"

let parse_chunk = function
  | `Assoc fields ->
      let* () = exact_fields ["index"; "offset"; "size"; "sha256"] fields in
      let* index = assoc fields "index" >>= int_value in
      let* offset = assoc fields "offset" >>= int64_value in
      let* size = assoc fields "size" >>= int_value in
      let* sha256 = assoc fields "sha256" >>= string_value in
      Ok { index; offset; size; sha256 }
  | _ -> Error "chunk must be an object"

let parse_file = function
  | `Assoc fields ->
      let* () = exact_fields ["path"; "size"; "sha256"; "chunks"] fields in
      let* path = assoc fields "path" >>= string_value in
      let* size = assoc fields "size" >>= int64_value in
      let* sha256 = assoc fields "sha256" >>= string_value in
      let* chunks_json = assoc fields "chunks" >>= list_value in
      if List.length chunks_json > chunk_limit then Error "file chunk count exceeds limit"
      else
        let* chunks =
          List.fold_left (fun state item ->
            let* items = state in
            let* chunk = parse_chunk item in
            Ok (chunk :: items)
          ) (Ok []) chunks_json
          |> Result.map List.rev
        in
        Ok { path; size; sha256; chunks }
  | _ -> Error "file must be an object"

let parse_body = function
  | `Assoc fields ->
      let names = [
        "version"; "checkpoint_hash"; "snapshot_id"; "irmin_commit";
        "chunk_size"; "total_size"; "file_count"; "chunk_count";
        "chunks_root"; "files";
      ] in
      let* () = exact_fields names fields in
      let* parsed_version = assoc fields "version" >>= string_value in
      if parsed_version <> format then Error "unsupported manifest format"
      else
        let* checkpoint_hash = assoc fields "checkpoint_hash" >>= string_value in
        let* snapshot_id = assoc fields "snapshot_id" >>= string_value in
        let* irmin_commit = assoc fields "irmin_commit" >>= option_string_value in
        let* chunk_size = assoc fields "chunk_size" >>= int_value in
        let* total_size = assoc fields "total_size" >>= int64_value in
        let* file_count = assoc fields "file_count" >>= int_value in
        let* chunk_count = assoc fields "chunk_count" >>= int_value in
        let* chunks_root = assoc fields "chunks_root" >>= string_value in
        let* files_json = assoc fields "files" >>= list_value in
        if List.length files_json > file_limit then Error "file count exceeds limit"
        else
          let* files =
            List.fold_left (fun state item ->
              let* items = state in
              let* file = parse_file item in
              Ok (file :: items)
            ) (Ok []) files_json
            |> Result.map List.rev
          in
          Ok {
            checkpoint_hash;
            snapshot_id;
            irmin_commit;
            chunk_size;
            total_size;
            file_count;
            chunk_count;
            chunks_root;
            files;
          }
  | _ -> Error "manifest must be an object"

let parse_draft_json = function
  | `Assoc fields ->
      let* () =
        exact_fields
          ["version"; "checkpoint"; "checkpoint_hash"; "manifest"; "manifest_hash"]
          fields
      in
      let* parsed_version = assoc fields "version" >>= string_value in
      if parsed_version <> format then Error "unsupported draft format"
      else
        let* checkpoint_json = assoc fields "checkpoint" in
        let* checkpoint = Checkpoint.parse_body checkpoint_json in
        let* checkpoint_hash = assoc fields "checkpoint_hash" >>= string_value in
        let* manifest_json = assoc fields "manifest" in
        let* manifest = parse_body manifest_json in
        let* manifest_hash = assoc fields "manifest_hash" >>= string_value in
        Ok { checkpoint; checkpoint_hash; manifest; manifest_hash }
  | _ -> Error "draft must be an object"

let parse_signatures limit values =
  if List.length values > limit then Error "signature count exceeds limit"
  else
    List.fold_left (fun state item ->
      let* items = state in
      let* signature = Checkpoint.parse_signature item in
      Ok (signature :: items)
    ) (Ok []) values
    |> Result.map List.rev

let parse_checkpoint_certificate fields =
  let names = [
    "version"; "checkpoint"; "checkpoint_hash"; "checkpoint_signatures";
    "manifest"; "manifest_hash"; "exporter_signature";
  ] in
  let* () = exact_fields names fields in
  let* checkpoint_json = assoc fields "checkpoint" in
  let* checkpoint = Checkpoint.parse_body checkpoint_json in
  let* checkpoint_hash = assoc fields "checkpoint_hash" >>= string_value in
  let* signatures_json = assoc fields "checkpoint_signatures" >>= list_value in
  let* checkpoint_signatures = parse_signatures 1_024 signatures_json in
  let* manifest_json = assoc fields "manifest" in
  let* manifest = parse_body manifest_json in
  let* manifest_hash = assoc fields "manifest_hash" >>= string_value in
  let* exporter_json = assoc fields "exporter_signature" in
  let* exporter_signature = Checkpoint.parse_signature exporter_json in
  Ok {
    checkpoint;
    checkpoint_hash;
    authority = Checkpoint_quorum checkpoint_signatures;
    manifest;
    manifest_hash;
    exporter_signatures = [exporter_signature];
  }

let parse_finalized_certificate fields =
  let names = [
    "version"; "checkpoint"; "checkpoint_hash"; "finality";
    "manifest"; "manifest_hash"; "exporter_signatures";
  ] in
  let* () = exact_fields names fields in
  let* checkpoint_json = assoc fields "checkpoint" in
  let* checkpoint = Checkpoint.parse_body checkpoint_json in
  let* checkpoint_hash = assoc fields "checkpoint_hash" >>= string_value in
  let* finality = assoc fields "finality" >>= string_value in
  let* manifest_json = assoc fields "manifest" in
  let* manifest = parse_body manifest_json in
  let* manifest_hash = assoc fields "manifest_hash" >>= string_value in
  let* signatures_json = assoc fields "exporter_signatures" >>= list_value in
  let* exporter_signatures = parse_signatures 1_024 signatures_json in
  Ok {
    checkpoint;
    checkpoint_hash;
    authority = Finalized finality;
    manifest;
    manifest_hash;
    exporter_signatures;
  }

let parse_certificate_json = function
  | `Assoc fields ->
      let* parsed_version = assoc fields "version" >>= string_value in
      if parsed_version <> format then Error "unsupported certificate format"
      else if List.mem_assoc "finality" fields then
        parse_finalized_certificate fields
      else
        parse_checkpoint_certificate fields
  | _ -> Error "certificate must be an object"

let parse_limited parse raw =
  if String.length raw > manifest_limit then Error "manifest exceeds size limit"
  else
    let* json = protect (fun () -> Yojson.Safe.from_string raw) in
    parse json

let parse_draft_string raw =
  parse_limited parse_draft_json raw

let parse_certificate_string raw =
  parse_limited parse_certificate_json raw

let validate_file chunk_size file =
  let* normalized =
    match normalize_path file.path with
    | Some path when path = file.path -> Ok path
    | _ -> Error "invalid file path"
  in
  let _ = normalized in
  if Int64.compare file.size 0L < 0 then Error "negative file size"
  else if not (is_lower_hex_64 file.sha256) then Error "invalid file hash"
  else
    let rec loop expected_index expected_offset = function
      | [] ->
          if expected_offset = file.size then Ok ()
          else Error "file chunks do not cover file"
      | chunk :: rest ->
          if chunk.index <> expected_index then Error "chunk index gap"
          else if chunk.offset <> expected_offset then Error "chunk offset gap"
          else if chunk.size <= 0 || chunk.size > chunk_size then Error "invalid chunk size"
          else if not (is_lower_hex_64 chunk.sha256) then Error "invalid chunk hash"
          else
            let size = Int64.of_int chunk.size in
            if Int64.compare expected_offset (Int64.sub Int64.max_int size) > 0 then
              Error "chunk offset overflow"
            else
              let next = Int64.add expected_offset size in
              if Int64.compare next file.size > 0 then Error "chunk exceeds file"
              else loop (expected_index + 1) next rest
    in
    if file.size = 0L && file.chunks <> [] then Error "empty file has chunks"
    else if file.size <> 0L && file.chunks = [] then Error "non-empty file has no chunks"
    else loop 0 0L file.chunks

let validate_body (body : body) =
  if not (is_lower_hex_64 body.checkpoint_hash) then Error "invalid checkpoint hash"
  else if not (valid_id body.snapshot_id) then Error "invalid snapshot id"
  else if body.snapshot_id <> body.checkpoint_hash then
    Error "snapshot id must equal checkpoint hash"
  else if (match body.irmin_commit with
    | Some value -> String.length value = 0 || String.length value > 256
    | None -> false) then Error "invalid irmin commit"
  else if body.chunk_size < chunk_size_min || body.chunk_size > chunk_size_max then
    Error "invalid manifest chunk size"
  else if body.file_count <> List.length body.files then Error "file count mismatch"
  else if body.file_count < 1 || body.file_count > file_limit then Error "file count exceeds limit"
  else
    let paths =
      List.rev (List.rev_map (fun file -> file.path) body.files)
    in
    if paths <> List.sort_uniq String.compare paths then Error "file paths are not canonical"
    else
      let chunk_count =
        List.fold_left (fun total file -> total + List.length file.chunks) 0 body.files
      in
      if chunk_count <> body.chunk_count then Error "chunk count mismatch"
      else if chunk_count < 1 || chunk_count > chunk_limit then Error "chunk count exceeds limit"
      else
        let* total_size =
          List.fold_left (fun state file ->
            let* total = state in
            if Int64.compare file.size 0L < 0 then Error "negative file size"
            else if Int64.compare total (Int64.sub Int64.max_int file.size) > 0 then
              Error "total size overflow"
            else
              Ok (Int64.add total file.size)
          ) (Ok 0L) body.files
        in
        if total_size <> body.total_size then Error "total size mismatch"
        else
          let* () =
            List.fold_left (fun state file ->
              let* () = state in
              validate_file body.chunk_size file
            ) (Ok ()) body.files
          in
          let root = chunks_root body.files in
          if root <> body.chunks_root then Error "chunk root mismatch" else Ok ()

let pvac_file path =
  let prefix = "pvac/blobs/" in
  let suffix = ".pk" in
  let first = String.length prefix in
  let length = String.length path - first - String.length suffix in
  String.starts_with ~prefix path
  && String.ends_with ~suffix path
  && length = 64
  && is_lower_hex_64 (String.sub path first length)

let reference_file path =
  path = "HEAD.json"
  || path = "state_root"
  || path = "ledger.dat"
  || path = Octra_core.Pvac_migration_entitlement.state_relative_path
  || pvac_file path

let validate_reference_body body =
  let* () = validate_body body in
  let paths = List.map (fun file -> file.path) body.files in
  match List.find_opt (fun path -> not (reference_file path)) paths with
    | Some path -> Error ("reference snapshot file is unsupported: " ^ path)
    | None when not (List.mem "HEAD.json" paths) ->
        Error "reference snapshot has no HEAD"
    | None when not (List.mem "state_root" paths) ->
        Error "reference snapshot has no state root"
    | None when not (List.mem "ledger.dat" paths) ->
        Error "reference snapshot has no ledger image"
    | None -> Ok ()

let manifest_hash (body : body) =
  let* () = validate_body body in
  Ok (manifest_hash_raw body |> raw_to_hex)

let validate_validator_set validator_set =
  if validator_set.Octra_consensus.C_types.n < 1 then Error "empty signer set"
  else
    let addresses =
      List.map (fun item -> item.Octra_consensus.C_types.address)
        validator_set.validators
    in
    if addresses <> List.sort_uniq String.compare addresses then
      Error "duplicate signer address"
    else
      List.fold_left (fun state item ->
        let* () = state in
        let* _ = Checkpoint.raw_pubkey item.Octra_consensus.C_types.pubkey in
        if not (Octra_core.Crypto.Address.verify_address_pubkey
          item.address item.pubkey) then
          Error "signer address does not match public key"
        else
          Ok ()
      ) (Ok ()) validator_set.validators

let validator_set_of_entries entries =
  let* validators =
    List.fold_left (fun state entry ->
      let* items = state in
      match String.split_on_char ':' (String.trim entry) with
      | [address; pubkey] when address <> "" && pubkey <> "" ->
          Ok (Octra_consensus.C_types.{ address; pubkey } :: items)
      | _ -> Error "invalid signer entry"
    ) (Ok []) entries
    |> Result.map List.rev
  in
  let validator_set = Octra_consensus.C_types.make_validator_set validators in
  let* () = validate_validator_set validator_set in
  Ok validator_set

let verify_draft (draft : draft) =
  let* () = Checkpoint.validate draft.checkpoint in
  let* checkpoint_hash = Checkpoint.hash draft.checkpoint in
  if checkpoint_hash <> draft.checkpoint_hash then Error "checkpoint hash mismatch"
  else if draft.manifest.checkpoint_hash <> checkpoint_hash then
    Error "manifest checkpoint binding mismatch"
  else
    let* manifest_hash = manifest_hash draft.manifest in
    if manifest_hash <> draft.manifest_hash then Error "manifest hash mismatch"
    else Ok draft

let set_hash validator_set =
  Octra_consensus.C_config.validator_set_hash validator_set |> raw_to_hex

let verify_exporters ~exporter_set ~required ~message signatures =
  let signers = List.map (fun item -> item.Checkpoint.signer) signatures in
  if signers <> List.sort_uniq String.compare signers then
    Error "exporter signatures are not ordered"
  else if List.length signatures < required then
    Error "exporter signature threshold missing"
  else
    List.fold_left (fun state signature ->
      let* () = state in
      Checkpoint.verify_signature
        ~signer_set:exporter_set
        ~message
        signature
    ) (Ok ()) signatures

let verify_certificate ~validator_set ~exporter_set certificate =
  let* () = validate_validator_set validator_set in
  let* () = validate_validator_set exporter_set in
  let draft = {
    checkpoint = certificate.checkpoint;
    checkpoint_hash = certificate.checkpoint_hash;
    manifest = certificate.manifest;
    manifest_hash = certificate.manifest_hash;
  } in
  let* _ = verify_draft draft in
  let* message =
    let* () = validate_body certificate.manifest in
    Ok (manifest_hash_raw certificate.manifest)
  in
  match certificate.authority with
  | Checkpoint_quorum signatures ->
      if certificate.checkpoint.validator_set_hash <> set_hash validator_set then
        Error "checkpoint validator set hash mismatch"
      else
        let* () =
          Checkpoint.verify_quorum
            ~validator_set
            certificate.checkpoint
            signatures
        in
        let* () =
          verify_exporters
            ~exporter_set
            ~required:1
            ~message
            certificate.exporter_signatures
        in
        Ok certificate
  | Finalized finality ->
      let* () = validate_reference_body certificate.manifest in
      let* _ =
        Sync_anchor.verify
          ~validator_set
          certificate.checkpoint
          finality
      in
      let* () =
        verify_exporters
          ~exporter_set
          ~required:1
          ~message
          certificate.exporter_signatures
      in
      Ok certificate

let verify_reference_certificate ~validator_set ~exporter_set certificate =
  match certificate.authority with
  | Checkpoint_quorum _ -> Error "reference checkpoint finality is required"
  | Finalized _ -> verify_certificate ~validator_set ~exporter_set certificate

let is_reference certificate =
  match certificate.authority with
  | Finalized _ -> true
  | Checkpoint_quorum _ -> false

let checkpoint_signatures certificate =
  match certificate.authority with
  | Checkpoint_quorum signatures -> signatures
  | Finalized _ -> []

let finality certificate =
  match certificate.authority with
  | Finalized value -> Some value
  | Checkpoint_quorum _ -> None

let fresh ~now certificate =
  Int64.compare (Int64.add now 300L) certificate.checkpoint.created_at >= 0
  && Int64.compare now certificate.checkpoint.valid_until <= 0

let make_checkpoint_signature ~wallet checkpoint =
  Checkpoint.make_signature ~wallet checkpoint

let make_exporter_signature ~wallet manifest =
  let* () = validate_body manifest in
  if not (Octra_core.Crypto.Address.verify_address_pubkey
    wallet.Octra_core.Crypto.Wallet.address wallet.pub) then
    Error "wallet address does not match public key"
  else
    let* private_raw = protect (fun () -> Base64.decode_exn wallet.priv) in
    if String.length private_raw < 32 then Error "invalid wallet private key"
    else
      let signature =
        Octra_consensus.C_hash.sign_ed25519
          ~priv_raw:(String.sub private_raw 0 32)
          ~msg:(manifest_hash_raw manifest)
        |> Base64.encode_exn
      in
      Ok Checkpoint.{
        signer = wallet.address;
        signature;
      }

let read_chunk channel buffer =
  let capacity = Bytes.length buffer in
  let rec fill offset =
    if offset = capacity then offset
    else
      let read = input channel buffer offset (capacity - offset) in
      if read = 0 then offset else fill (offset + read)
  in
  fill 0

let hash_file_chunks ~chunk_size path =
  let channel = open_in_bin path in
  let buffer = Bytes.create chunk_size in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let rec loop index offset file_hash chunks =
        let read = read_chunk channel buffer in
        if read = 0 then
          Digestif.SHA256.get file_hash |> Digestif.SHA256.to_hex,
          List.rev chunks
        else
          let chunk_hash =
            Digestif.SHA256.digest_bytes ~off:0 ~len:read buffer
            |> Digestif.SHA256.to_hex
          in
          let file_hash =
            Digestif.SHA256.feed_bytes file_hash ~off:0 ~len:read buffer
          in
          let chunk = { index; offset; size = read; sha256 = chunk_hash } in
          loop
            (index + 1)
            (Int64.add offset (Int64.of_int read))
            file_hash
            (chunk :: chunks)
      in
      loop 0 0L (Digestif.SHA256.init ()) [])

let build ~checkpoint ~source_dir ~chunk_size =
  if chunk_size < chunk_size_min || chunk_size > chunk_size_max then
    Error "invalid chunk size"
  else
    let* checkpoint_hash = Checkpoint.hash checkpoint in
    let* head =
      match Octra_core.Head_manifest.load_result source_dir with
      | Octra_core.Head_manifest.Missing -> Error "snapshot HEAD.json is missing"
      | Octra_core.Head_manifest.Corrupt reason ->
          Error ("snapshot HEAD.json is corrupt: " ^ reason)
      | Octra_core.Head_manifest.Present head
        when Checkpoint.matches_head checkpoint head ->
          Ok head
      | Octra_core.Head_manifest.Present _ ->
          Error "snapshot HEAD.json does not match checkpoint"
    in
    let* files =
      protect (fun () ->
        State_sync.list_files source_dir
        |> List.rev_map (fun path ->
          let absolute = Filename.concat source_dir path in
          let stat = Unix.stat absolute in
          let sha256, chunks = hash_file_chunks ~chunk_size absolute in
          {
            path;
            size = Int64.of_int stat.Unix.st_size;
            sha256;
            chunks;
          })
        |> List.rev)
    in
    let* total_size =
      List.fold_left (fun state file ->
        let* total = state in
        if Int64.compare total (Int64.sub Int64.max_int file.size) > 0 then
          Error "snapshot total size overflow"
        else
          Ok (Int64.add total file.size)
      ) (Ok 0L) files
    in
    let chunk_count =
      List.fold_left (fun total file -> total + List.length file.chunks) 0 files
    in
    let manifest = {
      checkpoint_hash;
      snapshot_id = checkpoint_hash;
      irmin_commit = head.irmin_commit;
      chunk_size;
      total_size;
      file_count = List.length files;
      chunk_count;
      chunks_root = chunks_root files;
      files;
    } in
    let* manifest_hash = manifest_hash manifest in
    Ok {
      checkpoint;
      checkpoint_hash;
      manifest;
      manifest_hash;
    }

let find_chunk manifest ~path ~index ~sha256 =
  match List.find_opt (fun file -> file.path = path) manifest.files with
  | None -> Error "manifest file not found"
  | Some file ->
      match List.find_opt (fun chunk -> chunk.index = index) file.chunks with
      | None -> Error "manifest chunk not found"
      | Some chunk when chunk.sha256 <> sha256 -> Error "manifest chunk hash mismatch"
      | Some chunk -> Ok chunk

let load_limited parse path =
  let* raw =
    protect (fun () ->
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          let size = in_channel_length channel in
          if size > manifest_limit then failwith "manifest exceeds size limit";
          really_input_string channel size))
  in
  parse raw

let load_draft path =
  load_limited parse_draft_string path

let load_certificate path =
  load_limited parse_certificate_string path

let write_json path json =
  let parent = Filename.dirname path in
  let rec mkdir current =
    if current = "" || current = "." || Sys.file_exists current then ()
    else begin
      mkdir (Filename.dirname current);
      Unix.mkdir current 0o755
    end
  in
  mkdir parent;
  let stamp = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) in
  let staged =
    Printf.sprintf "%s.next.%d.%Ld" path (Unix.getpid ()) stamp
  in
  let output =
    open_out_gen
      [Open_wronly; Open_creat; Open_excl; Open_binary]
      0o644
      staged
  in
  begin
    try
      Fun.protect
        ~finally:(fun () -> close_out_noerr output)
        (fun () ->
          Yojson.Safe.to_channel output json;
          output_char output '\n';
          flush output;
          Unix.fsync (Unix.descr_of_out_channel output))
    with exn ->
      (try Unix.unlink staged with _ -> ());
      raise exn
  end;
  Unix.rename staged path;
  let directory = Unix.openfile parent [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close directory)
    (fun () -> Unix.fsync directory)