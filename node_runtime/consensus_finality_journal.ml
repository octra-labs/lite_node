(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types
module C_codec = Octra_consensus.C_codec
module C_hash = Octra_consensus.C_hash
module C_qc = Octra_consensus.C_qc
module C_config = Octra_consensus.C_config
module Finality_log = Octra_consensus.Finality_log
module Bundle_validation = Consensus_bundle_validation

type bundle = {
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
}

type record = {
  finalize : C_types.finalize;
  validator_set : C_types.validator_set;
  bundle : bundle option;
}

type read_result =
  | Missing
  | Valid of record
  | Invalid of string

let schema = "octra_finality_journal"
let max_bytes = 128 * 1024 * 1024
let history_limit = 4096L
let zero_root = String.make 32 '\x00'

let dir base =
  Filename.concat base "finality"

let path base =
  Filename.concat (dir base) "pending_finalized.json"

let committed_path base =
  Filename.concat (dir base) "committed_finalized.json"

let history_dir base =
  Filename.concat (dir base) "committed"

let history_path base epoch =
  Filename.concat
    (history_dir base)
    (Int64.to_string epoch ^ ".json")

let fsync_directory target =
  let fd = Unix.openfile target [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.fsync fd)

let ensure_dir base =
  let target = dir base in
  if Sys.file_exists target then begin
    if not (Sys.is_directory target) then
      failwith "finality journal parent is not a directory"
  end else begin
    Unix.mkdir target 0o750;
    fsync_directory base
  end

let ensure_history_dir base =
  ensure_dir base;
  let target = history_dir base in
  if Sys.file_exists target then begin
    if not (Sys.is_directory target) then
      failwith "finality history is not a directory"
  end else begin
    Unix.mkdir target 0o750;
    fsync_directory (dir base)
  end

let rec write_all fd bytes offset =
  if offset < String.length bytes then begin
    let written =
      Unix.write_substring fd bytes offset (String.length bytes - offset)
    in
    if written <= 0 then failwith "finality journal write returned zero";
    write_all fd bytes (offset + written)
  end

let fsync_dir base =
  fsync_directory (dir base)

let encode_finalize finalize =
  C_codec.encode_finalize finalize
  |> Base64.encode_exn

let decode_finalize encoded =
  encoded
  |> Base64.decode_exn
  |> C_codec.decode_finalize

let bundle_to_json bundle =
  `Assoc [
    "tx_hashes", `List (List.map (fun value -> `String value) bundle.tx_hashes);
    "txs", `List (List.map Transaction.to_yojson bundle.txs);
    "receipts_json",
      `List (List.map (fun value -> `String value) bundle.receipts_json);
  ]

let record_to_json record =
  `Assoc [
    "schema", `String schema;
    "finalize", `String (encode_finalize record.finalize);
    "validator_set",
      `String
        (record.validator_set
         |> C_codec.encode_validator_set
         |> Base64.encode_exn);
    "bundle",
      (match record.bundle with
       | Some bundle -> bundle_to_json bundle
       | None -> `Null);
  ]

let parse_transaction json =
  match Transaction.of_yojson json with
  | Ok tx -> tx
  | Error error -> failwith ("finality journal transaction: " ^ error)

let bundle_of_json json =
  let open Yojson.Safe.Util in
  {
    tx_hashes =
      json
      |> member "tx_hashes"
      |> to_list
      |> List.map to_string;
    txs =
      json
      |> member "txs"
      |> to_list
      |> List.map parse_transaction;
    receipts_json =
      json
      |> member "receipts_json"
      |> to_list
      |> List.map to_string;
  }

let record_of_json json =
  let open Yojson.Safe.Util in
  let got_schema = json |> member "schema" |> to_string in
  if got_schema <> schema then failwith "finality journal schema mismatch";
  {
    finalize = json |> member "finalize" |> to_string |> decode_finalize;
    validator_set =
      json
      |> member "validator_set"
      |> to_string
      |> Base64.decode_exn
      |> C_codec.decode_validator_set;
    bundle =
      match member "bundle" json with
      | `Null -> None
      | value -> Some (bundle_of_json value);
  }

let bytes record =
  let encoded = Yojson.Safe.to_string (record_to_json record) ^ "\n" in
  if String.length encoded > max_bytes then
    failwith "finality journal exceeds size limit";
  encoded

let read_bytes target =
  if not (Sys.file_exists target) then None
  else
    let stat = Unix.lstat target in
    if stat.Unix.st_kind <> Unix.S_REG then
      failwith "finality journal is not a regular file";
    if stat.Unix.st_size <= 0 || stat.Unix.st_size > max_bytes then
      failwith "finality journal size is invalid";
    let input = open_in_bin target in
    Fun.protect
      ~finally:(fun () -> close_in input)
      (fun () -> Some (really_input_string input stat.Unix.st_size))

let read_record target =
  match read_bytes target with
  | None -> None
  | Some encoded ->
    Some (record_of_json (Yojson.Safe.from_string encoded))

let same_finalize left right =
  C_codec.encode_finalize left = C_codec.encode_finalize right

let same_bundle left right =
  left.tx_hashes = right.tx_hashes
  && List.map Transaction.to_yojson left.txs
     = List.map Transaction.to_yojson right.txs
  && left.receipts_json = right.receipts_json

let temporary_counter = ref 0

let rec open_temporary target attempts =
  if attempts <= 0 then
    failwith "unable to allocate finality journal temporary file";
  incr temporary_counter;
  let temporary =
    Printf.sprintf
      "%s.next.%d.%d"
      target
      (Unix.getpid ())
      !temporary_counter
  in
  try
    let fd =
      Unix.openfile
        temporary
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL]
        0o640
    in
    temporary, fd
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) ->
    open_temporary target (attempts - 1)

let remove_temporary path =
  try Unix.unlink path with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let write_encoded target encoded =
  let temporary, fd = open_temporary target 1024 in
  let renamed = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !renamed then remove_temporary temporary)
    (fun () ->
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          write_all fd encoded 0;
          Unix.fsync fd);
      Unix.rename temporary target;
      renamed := true;
      fsync_directory (Filename.dirname target))

let write_record base record =
  ensure_dir base;
  write_encoded (path base) (bytes record)

let archive_record base record =
  ensure_history_dir base;
  let compact = { record with bundle = None } in
  let epoch = compact.finalize.C_types.epoch_id in
  let target = history_path base epoch in
  if Sys.file_exists target then begin
    match read_record target with
    | Some prior
      when Finality_log.same_commitment
             (Finality_log.of_finalize prior.finalize)
             (Finality_log.of_finalize compact.finalize)
           && C_config.validator_set_hash prior.validator_set
              = C_config.validator_set_hash compact.validator_set ->
      ()
    | Some _
    | None ->
      failwith "conflicting finality history"
  end else
    write_encoded target (bytes compact);
  let expired = Int64.sub epoch history_limit in
  if Int64.compare expired 0L >= 0 then begin
    let stale = history_path base expired in
    if Sys.file_exists stale then begin
      Unix.unlink stale;
      fsync_directory (history_dir base)
    end
  end

let persist_certificate base ~validator_set finalize =
  match read_record (path base) with
  | None ->
    write_record base { finalize; validator_set; bundle = None }
  | Some prior
    when same_finalize prior.finalize finalize
         && C_config.validator_set_hash prior.validator_set
            = C_config.validator_set_hash validator_set ->
    ()
  | Some _ ->
    failwith "conflicting finality journal certificate"

let persist_bundle base finalize bundle =
  match read_record (path base) with
  | None ->
    failwith "finality journal bundle requires certificate"
  | Some prior when not (same_finalize prior.finalize finalize) ->
    failwith "conflicting finality journal certificate"
  | Some { bundle = Some existing; _ } ->
    if not (same_bundle existing bundle) then
      failwith "conflicting finality journal bundle"
  | Some prior ->
    write_record base { prior with bundle = Some bundle }

let validate_qc ~chain_id ~validator_set finalize =
  let verify_vote (vote : C_types.vote) =
    match C_types.pubkey_of_addr validator_set vote.C_types.validator with
    | Some pubkey -> C_hash.verify_vote ~pubkey_raw:pubkey vote
    | None -> false
  in
  let verdict =
    C_qc.validate_persisted_finalize
      ~chain_id
      ~validator_set
      ~verify_vote
      finalize
  in
  match verdict with
  | C_qc.Valid -> Ok ()
  | C_qc.Invalid reason -> Error ("finality qc " ^ reason)

let validate_bundle finalize bundle =
  let response : Octra_consensus.C_driver.bundle_response_record = {
    responder_addr = "finality_journal";
    tx_hashes = bundle.tx_hashes;
    txs_json =
      List.map
        (fun tx -> Yojson.Safe.to_string (Transaction.to_yojson tx))
        bundle.txs;
    receipts_json = bundle.receipts_json;
  } in
  match Bundle_validation.finalized ~header:finalize.C_types.header response with
  | Ok _ -> Ok ()
  | Error reason -> Error reason

let validate_record ~chain_id record =
  match validate_qc ~chain_id ~validator_set:record.validator_set record.finalize with
  | Error _ as error -> error
  | Ok () ->
    match record.bundle with
    | None -> Ok ()
    | Some bundle -> validate_bundle record.finalize bundle

let validate ~chain_id ~validator_set record =
  if
    C_config.validator_set_hash validator_set
    <> C_config.validator_set_hash record.validator_set
  then
    Error "finality journal validator set mismatch"
  else
    validate_record ~chain_id record

let read_validated_path ~chain_id ~validator_set target =
  try
    match read_record target with
    | None -> Missing
    | Some record ->
      match validate ~chain_id ~validator_set record with
      | Ok () -> Valid record
      | Error reason -> Invalid reason
  with exn ->
    Invalid (Printexc.to_string exn)

let read_validated ~chain_id ~validator_set base =
  read_validated_path
    ~chain_id
    ~validator_set
    (path base)

let read_pending_epoch base =
  try
    Ok
      (Option.map
         (fun record -> record.finalize.C_types.epoch_id)
         (read_record (path base)))
  with exn ->
    Error (Printexc.to_string exn)

let committed_matches entry record =
  Finality_log.same_commitment
    entry
    (Finality_log.of_finalize record.finalize)

let committed_record_path base epoch =
  let current = committed_path base in
  let historical = history_path base epoch in
  match read_record current with
  | Some record
    when Int64.equal record.finalize.C_types.epoch_id epoch ->
    Some current
  | Some _
  | None ->
    if Sys.file_exists historical then Some historical else None

let read_committed_epoch ~chain_id ~epoch base =
  try
    match committed_record_path base epoch with
    | None -> Missing
    | Some target ->
      begin
        match read_record target with
        | None -> Missing
        | Some record
          when not (Int64.equal record.finalize.C_types.epoch_id epoch) ->
          Invalid "committed finality journal height mismatch"
        | Some record ->
          begin
            match validate_record ~chain_id record with
            | Error reason -> Invalid reason
            | Ok () -> Valid record
          end
      end
  with exn ->
    Invalid (Printexc.to_string exn)

let read_committed_epoch_validated ~chain_id ~validator_set ~epoch base =
  match read_committed_epoch ~chain_id ~epoch base with
  | Missing -> Missing
  | Invalid _ as invalid -> invalid
  | Valid record ->
    begin
      match validate ~chain_id ~validator_set record with
      | Ok () -> Valid record
      | Error reason -> Invalid reason
    end

let read_history_epoch_validated ~chain_id ~validator_set ~epoch base =
  read_validated_path
    ~chain_id
    ~validator_set
    (history_path base epoch)

let empty_bundle = {
  tx_hashes = [];
  txs = [];
  receipts_json = [];
}

let header_has_empty_bundle header =
  header.C_types.tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header []
  && header.C_types.receipt_root = C_hash.receipt_root []

let replayable record =
  match record.bundle with
  | Some _ -> Ok record
  | None when header_has_empty_bundle record.finalize.C_types.header ->
    Ok { record with bundle = Some empty_bundle }
  | None ->
    Error "committed finality bundle is missing"

let history_epoch name =
  let suffix = ".json" in
  let suffix_len = String.length suffix in
  let name_len = String.length name in
  if name_len <= suffix_len
     || String.sub name (name_len - suffix_len) suffix_len <> suffix then
    None
  else
    try
      Some
        (String.sub name 0 (name_len - suffix_len)
         |> Int64.of_string)
    with Failure _ ->
      None

let committed_epochs base =
  let historical =
    let target = history_dir base in
    if Sys.file_exists target && Sys.is_directory target then
      Sys.readdir target
      |> Array.to_list
      |> List.filter_map history_epoch
    else
      []
  in
  let current =
    match read_record (committed_path base) with
    | Some record -> [record.finalize.C_types.epoch_id]
    | None -> []
  in
  List.sort_uniq Int64.compare (historical @ current)

let read_replay_backlog ~chain_id ~validator_set ~head_epoch ~head_root base =
  try
    let epochs =
      committed_epochs base
      |> List.filter (fun epoch -> Int64.compare epoch head_epoch > 0)
    in
    let rec loop expected_epoch expected_root records = function
      | [] -> Ok (List.rev records)
      | epoch :: _ when not (Int64.equal epoch expected_epoch) ->
        Error "committed finality history height gap"
      | epoch :: rest ->
        begin
          match
            read_committed_epoch_validated
              ~chain_id
              ~validator_set
              ~epoch
              base
          with
          | Missing ->
            Error "committed finality history is missing"
          | Invalid reason ->
            Error reason
          | Valid record ->
            let header = record.finalize.C_types.header in
            if header.C_types.prev_state_root <> expected_root then
              Error "committed finality history root discontinuity"
            else
              match replayable record with
              | Error _ as error -> error
              | Ok replay ->
                loop
                  (Int64.succ epoch)
                  header.C_types.proposed_state_root
                  (replay :: records)
                  rest
        end
    in
    loop (Int64.succ head_epoch) head_root [] epochs
  with exn ->
    Error (Printexc.to_string exn)

let read_committed_validated ~chain_id ~entry base =
  match
    read_committed_epoch
      ~chain_id
      ~epoch:(Int64.of_int entry.Finality_log.height)
      base
  with
  | Missing -> Missing
  | Invalid _ as invalid -> invalid
  | Valid record when committed_matches entry record -> Valid record
  | Valid _ -> Invalid "committed finality journal commitment mismatch"

let remove base =
  let target = path base in
  if Sys.file_exists target then begin
    Unix.unlink target;
    fsync_dir base
  end

let drop_invalid_unapplied base ~head =
  try
    match read_record (path base) with
    | None -> Error "pending finality journal is missing"
    | Some record ->
      let expected = Finality_log.of_finalize record.finalize in
      if expected.Finality_log.height <> head + 1 then
        Error "pending finality journal is not next after durable head"
      else
        begin
          match
            Finality_log.drop_matching_uncommitted
              base
              ~head
              expected
          with
          | Error _ as error -> error
          | Ok dropped ->
            remove base;
            Ok dropped
        end
  with exn ->
    Error (Printexc.to_string exn)

let pending base =
  Sys.file_exists (path base)

let same_record left right =
  same_finalize left.finalize right.finalize
  && C_config.validator_set_hash left.validator_set
     = C_config.validator_set_hash right.validator_set
  &&
  match left.bundle, right.bundle with
  | None, None -> true
  | Some left, Some right -> same_bundle left right
  | _ -> false

let committed_for ~chain_id ~entry base =
  match read_committed_validated ~chain_id ~entry base with
  | Valid _ -> Ok ()
  | Missing -> Error "committed finality journal is missing"
  | Invalid reason -> Error reason

let rewind_committed ~chain_id ~entry base =
  match read_committed_validated ~chain_id ~entry base with
  | Missing -> Error "committed finality journal is missing"
  | Invalid reason -> Error reason
  | Valid _ ->
    begin
      match
        committed_record_path
          base
          (Int64.of_int entry.Finality_log.height)
      with
      | None -> Error "finality rewind source is missing"
      | Some source ->
        let target = committed_path base in
        if source = target then Ok ()
        else if pending base then
          Error "pending finality journal blocks rewind"
        else
          try
            let encoded =
              match read_bytes source with
              | Some value -> value
              | None -> failwith "finality rewind source is missing"
            in
            write_encoded target encoded;
            Ok ()
          with exn ->
            Error (Printexc.to_string exn)
    end

let restore_committed_from_history ~chain_id ~validator_set ~epoch base =
  match
    read_history_epoch_validated
      ~chain_id
      ~validator_set
      ~epoch
      base
  with
  | Missing ->
    Error "finality history source is missing"
  | Invalid reason ->
    Error reason
  | Valid record ->
    if pending base then
      Error "pending finality journal blocks restore"
    else
      try
        write_encoded (committed_path base) (bytes record);
        Ok record
      with exn ->
        Error (Printexc.to_string exn)

let promote_record base ~allow_gap pending_record =
  let target = committed_path base in
  begin
    match read_record target with
    | None -> ()
    | Some committed_record ->
      let pending_epoch =
        pending_record.finalize.C_types.epoch_id
      in
      let committed_epoch =
        committed_record.finalize.C_types.epoch_id
      in
      if Int64.compare pending_epoch committed_epoch < 0 then
        failwith "finality journal promotion height regression"
      else if Int64.equal pending_epoch committed_epoch
              && not (same_record pending_record committed_record) then
        failwith "conflicting committed finality journal"
      else if Int64.compare pending_epoch committed_epoch > 0
              && not allow_gap
              && not
                   (Int64.equal
                      pending_epoch
                      (Int64.add committed_epoch 1L)) then
        failwith "finality journal promotion height gap"
      else
        archive_record base committed_record
  end;
  Unix.rename (path base) target;
  fsync_dir base;
  archive_record base pending_record

let pending_with_bundle base =
  let pending_path = path base in
  match read_record pending_path with
  | None ->
    failwith "finality journal promotion requires pending record"
  | Some { bundle = None; _ } ->
    failwith "finality journal promotion requires canonical bundle"
  | Some pending_record ->
    pending_record

let promote base =
  promote_record base ~allow_gap:false (pending_with_bundle base)

let promote_applied base ~epoch ~state_root =
  let pending_record = pending_with_bundle base in
  let header = pending_record.finalize.C_types.header in
  if not (Int64.equal pending_record.finalize.C_types.epoch_id epoch) then
    failwith "finality journal applied epoch mismatch";
  if header.C_types.proposed_state_root = zero_root then
    promote_record base ~allow_gap:false pending_record
  else begin
    if header.C_types.proposed_state_root <> state_root then
      failwith "finality journal applied root mismatch";
    promote_record base ~allow_gap:true pending_record
  end

let committed base =
  Sys.file_exists (committed_path base)