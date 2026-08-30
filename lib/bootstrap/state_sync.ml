(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type progress_report = {
  peer : string;
  phase : string;
  path : string;
  file_done : int;
  file_size : int;
  total_done : int;
  total_size : int;
  rate_bps : float;
  eta_seconds : float option;
}

let state_sync_version = "octra-state-sync"

let normalize_path raw =
  let raw =
    String.split_on_char '/' raw
    |> List.filter (fun part -> part <> "" && part <> ".")
    |> String.concat "/"
  in
  if raw = "" || String.contains raw '\000' then None
  else
    let parts = String.split_on_char '/' raw in
    if List.exists (( = ) "..") parts then None else Some raw

let path_allowed rel =
  let components = String.split_on_char '/' rel in
  let compact_control = rel = "irmin_store/store.control" in
  let component_allowed name =
    name <> "lock"
    && name <> "lock.mdb"
    && name <> ".-lock"
    && name <> "store.control"
    && not (String.starts_with ~prefix:"index.bak" name)
  in
  (compact_control || List.for_all component_allowed components)
  && rel <> "commit_journal.log"
  && (rel = "HEAD.json"
      || rel = "state_root"
      || rel = Root_win.name
      || rel = "ledger.dat"
      || String.starts_with ~prefix:"irmin_store/" rel
      || String.starts_with ~prefix:"chaindata/" rel
      || String.starts_with ~prefix:"pvac/" rel
      || String.starts_with ~prefix:"preverify_receipts/" rel)

let regular_file path =
  try (Unix.stat path).Unix.st_kind = Unix.S_REG with _ -> false

let fsync_parent path =
  try
    let fd = Unix.openfile (Filename.dirname path) [Unix.O_RDONLY] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> Unix.fsync fd)
  with _ -> ()

let rec walk_dir ~base ~rel acc =
  let dir = if rel = "" then base else Filename.concat base rel in
  let dh = Unix.opendir dir in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dh)
    (fun () ->
      let rec loop acc =
        match Unix.readdir dh with
        | exception End_of_file -> acc
        | "." | ".." -> loop acc
        | name ->
            let child_rel = if rel = "" then name else Filename.concat rel name in
            let child_abs = Filename.concat base child_rel in
            let acc =
              try
                match (Unix.lstat child_abs).Unix.st_kind with
                | Unix.S_DIR -> walk_dir ~base ~rel:child_rel acc
                | Unix.S_REG when path_allowed child_rel -> child_rel :: acc
                | _ -> acc
              with _ -> acc
            in
            loop acc
      in
      loop acc)

let roots = [
  "HEAD.json";
  "state_root";
  Root_win.name;
  "ledger.dat";
  "irmin_store";
  "chaindata";
  "pvac";
  "preverify_receipts";
]

let list_files base =
  let control_ready =
    Sys.file_exists (Filename.concat base ".compact-ready.json")
    || Sys.file_exists (Filename.concat base ".ready.json")
  in
  List.fold_left (fun acc root ->
    let abs = Filename.concat base root in
    if Sys.file_exists abs then
      try
        match (Unix.lstat abs).Unix.st_kind with
        | Unix.S_DIR -> walk_dir ~base ~rel:root acc
        | Unix.S_REG when path_allowed root -> root :: acc
        | _ -> acc
      with _ -> acc
    else acc
  ) [] roots
  |> List.sort_uniq String.compare
  |> List.filter (fun rel ->
    rel <> "irmin_store/store.control" || control_ready)
  |> List.filter (fun rel -> regular_file (Filename.concat base rel))

let head_equal a b =
  a.Octra_core.Head_manifest.epoch_id = b.Octra_core.Head_manifest.epoch_id
  && a.generation = b.generation
  && a.state_root = b.state_root
  && a.txid_hi = b.txid_hi
  && a.commit_id = b.commit_id
  && a.irmin_commit = b.irmin_commit

let snapshot_root data_dir =
  match Sys.getenv_opt "OCTRA_STATE_SYNC_SNAPSHOT_DIR" with
  | Some dir when String.trim dir <> "" -> dir
  | _ -> Filename.concat data_dir "state_sync_snapshots"

let valid_snapshot_id id =
  id <> ""
  && not (String.contains id '/')
  && not (String.contains id '\000')
  && not (String.contains id ':')
  && not (String.contains id '\\')
  && not (String.contains id '.')

let snapshot_dir data_dir id =
  Filename.concat (snapshot_root data_dir) id

let anchor_path data_dir =
  Filename.concat data_dir "state_sync/anchor.json"

let snapshot_ready_path dir =
  Filename.concat dir ".ready.json"

let snapshot_certificate_path dir =
  Filename.concat dir ".certificate.json"

let write_ready dir head =
  let body =
    `Assoc [
      "version", `String "octra-state-sync-snapshot-ready";
      "head_epoch", `Int head.Octra_core.Head_manifest.epoch_id;
      "state_root", `String head.state_root;
      "commit_id", `String head.commit_id;
      "created_at", `Float (Unix.gettimeofday ());
    ]
    |> Yojson.Safe.pretty_to_string
  in
  let path = snapshot_ready_path dir in
  let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o644 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc body;
      output_char oc '\n';
      flush oc;
      (try Unix.fsync (Unix.descr_of_out_channel oc) with _ -> ()));
  fsync_parent path

let no_commit_in_progress data_dir =
  Octra_core.Epoch_commit_marker.read_marker data_dir = None
  && Octra_core.Wal.read_pending data_dir = []

let stable_head data_dir expected =
  no_commit_in_progress data_dir
  &&
  match Octra_core.Head_manifest.load data_dir with
  | Some h -> head_equal h expected
  | None -> false

let head_json ~current_epoch ~snapshot_epoch =
  let head_fields =
    match Octra_core.Head_manifest.get_cached () with
    | None -> [
        "head_epoch", `Null;
        "state_root", `Null;
        "txid_hi", `Null;
      ]
    | Some h -> [
        "head_epoch", `Int h.Octra_core.Head_manifest.epoch_id;
        "state_root", `String h.Octra_core.Head_manifest.state_root;
        "txid_hi", `String (Int64.to_string h.Octra_core.Head_manifest.txid_hi);
      ]
  in
  `Assoc ([
    "version", `String state_sync_version;
    "mode", `String "live_head";
    "current_epoch", `Int !current_epoch;
    "snapshot_epoch",
      Option.fold
        ~none:`Null
        ~some:(fun epoch -> `Intlit (Int64.to_string epoch))
        snapshot_epoch;
  ] @ head_fields)

let progress_accepted_json =
  `Assoc ["ok", `Bool true]

let readiness_not_ready_json =
  `Assoc [
    "version", `String "octra-observer-ready";
    "status", `String "not_ready";
  ]

let observer_ready_marker_json ~consensus_role ~leader_rpc ~chain_id ~validator
    ~validator_pubkey ~ready_epoch ~state_root ~records_verified ~sign_payload
    ~signature ~generated_at =
  `Assoc [
    "version", `String "octra-observer-ready";
    "status", `String "observer_ready";
    "consensus_role", `String consensus_role;
    "leader_rpc", `String leader_rpc;
    "chain_id", `String chain_id;
    "validator", `String validator;
    "validator_pubkey", `String validator_pubkey;
    "ready_epoch", `String (Int64.to_string ready_epoch);
    "state_root", `String state_root;
    "catchup_records_verified", `Int records_verified;
    "sign_payload", `String sign_payload;
    "signature", `String signature;
    "generated_at", `Float generated_at;
  ]

let raw_to_hex s =
  let b = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c))) s;
  Buffer.contents b

let hex_to_raw32 hh =
  let len = String.length hh / 2 in
  let raw = String.init len (fun bi ->
    Char.chr (int_of_string ("0x" ^ String.sub hh (bi * 2) 2))) in
  if String.length raw = 32 then raw
  else if String.length raw > 32 then String.sub raw 0 32
  else raw ^ String.make (32 - String.length raw) '\x00'

let check_outcome ~epoch_id receipts txs =
  match Octra_core.Tx_outcome.decode_final ~confirmed:txs receipts with
  | Error error -> Error ("outcome:" ^ error)
  | Ok partition ->
    Octra_core.Preverify_receipt_policy.check
      ~epoch_id
      ~receipts:partition.preverify
      txs

let decode_range_txs txs_json =
  List.fold_left
    (fun acc tx_json ->
      match acc with
      | Error _ -> acc
      | Ok txs ->
        try
          match
            Yojson.Safe.from_string tx_json
            |> Octra_core.Tx_payload.decode_final
          with
          | Ok tx -> Ok (tx :: txs)
          | Error error -> Error error
        with exn -> Error (Printexc.to_string exn))
    (Ok [])
    txs_json
  |> Result.map List.rev

let build_range ~on_stop ~chain_id ~data_dir ~chaindata ~reward_source ~read_finality
    ~from_epoch ~max_epochs =
  try
    let max_chunk = min (max 1 max_epochs) 16 in
    let max_bytes = 4_000_000 in
    let records = ref [] in
    let total_bytes = ref 0 in
    let next_epoch = ref None in
    let stop = ref false in
    let i = ref 0 in
    while not !stop && !i < max_chunk do
      let target_epoch = Int64.add from_epoch (Int64.of_int !i) in
      let target_int = Int64.to_int target_epoch in
      let halt reason =
        on_stop ~epoch:target_epoch ~reason;
        stop := true
      in
      match Octra_core.Chaindata_index.get_epoch
        (Octra_core.Store_chaindata.index chaindata) target_int with
      | None ->
          if !records = [] then halt "epoch_missing"
          else begin
            next_epoch := Some target_epoch;
            halt "epoch_missing"
          end
      | Some json_str ->
          match Octra_core.Epochlog.epoch_of_json json_str with
          | None -> halt "epoch_invalid"
          | Some elog ->
              let start_txid = elog.Octra_core.Epochlog.start_txid in
              let tx_count = elog.tx_count in
              let tx_hashes = ref [] in
              let txs_json_list = ref [] in
              let txs_ok = ref true in
              for k = 0 to tx_count - 1 do
                let txid = Int64.add start_txid (Int64.of_int k) in
                match Octra_core.Store_chaindata.get_tx_by_txid chaindata txid with
                | None -> txs_ok := false
                | Some (h, j) ->
                    tx_hashes := h :: !tx_hashes;
                    txs_json_list := j :: !txs_json_list
              done;
              if not !txs_ok then halt "transaction_missing"
              else begin
                let hashes_in_order = List.rev !tx_hashes in
                let txs_in_order = List.rev !txs_json_list in
                let parsed_txs = decode_range_txs txs_in_order in
                match parsed_txs with
                | Stdlib.Error error ->
                  halt ("transaction_invalid:" ^ error)
                | Stdlib.Ok parsed_txs ->
                  let receipts_json =
                    match data_dir with
                    | Some base ->
                      (match Octra_core.Preverify_receipt_store.read base ~epoch_id:target_int with
                       | Some receipts -> receipts
                       | None -> [])
                    | None -> []
                  in
                  match check_outcome ~epoch_id:target_int receipts_json parsed_txs with
                  | Stdlib.Error error ->
                    halt ("receipt_invalid:" ^ error)
                  | Stdlib.Ok () ->
                    match read_finality target_int with
                    | None -> halt "finality_missing"
                    | Some finality ->
                    match reward_source target_int elog with
                    | Stdlib.Error error ->
                      halt ("reward_source_invalid:" ^ error)
                    | Stdlib.Ok reward_source ->
                    let tx_list_hash_raw = Octra_net.Hash_domain.hash
                      "octra:tx_list:v1" (String.concat "" hashes_in_order) in
                    let catchup_creator_addr =
                      elog.proposer.Octra_core.Epochlog.creator_addr in
                    let record = Octra_consensus.C_codec.{
                      epoch_id = target_epoch;
                      prev_state_root = hex_to_raw32 elog.prev_state_root;
                      state_root = hex_to_raw32 elog.state_root;
                      tx_list_hash = tx_list_hash_raw;
                      tx_hashes = hashes_in_order;
                      txs_json = txs_in_order;
                      receipt_root = Octra_consensus.C_hash.receipt_root receipts_json;
                      receipts_json;
                      epoch_ts = elog.finalized_at;
                      creator_addr = catchup_creator_addr;
                      commit_round = elog.proposer.commit_round;
                      reward_source = Some reward_source;
                      finality = Some finality;
                    } in
                    let expected_txid =
                      Int64.add
                        start_txid
                        (Int64.of_int (List.length hashes_in_order))
                    in
                    match
                      Octra_consensus.C_catchup.verify_record_finality
                        ~chain_id
                        ~expected_validator_set_hash:
                          (Octra_consensus.C_config.validator_set_hash
                             finality.validator_set)
                        ~expected_txid
                        ~record
                    with
                    | Stdlib.Error error ->
                      halt ("finality_invalid:" ^ error)
                    | Stdlib.Ok _ ->
                    let tx_bytes =
                      List.fold_left (fun acc s -> acc + 32 + String.length s) 0 txs_in_order in
                    let receipt_bytes =
                      List.fold_left (fun acc s -> acc + 32 + String.length s) 0 receipts_json in
                    let reward_bytes =
                      Octra_net.Oce1.encode
                        (fun buf ->
                          Octra_consensus.C_reward_source.encode_into
                            buf
                            reward_source)
                      |> String.length
                    in
                    let finality_bytes =
                      Octra_consensus.C_codec.encode_finalize
                        finality.finalize
                      |> String.length
                    in
                    let validator_bytes =
                      Octra_consensus.C_codec.encode_validator_set
                        finality.validator_set
                      |> String.length
                    in
                    let approx_size =
                      136
                      + tx_bytes
                      + receipt_bytes
                      + reward_bytes
                      + finality_bytes
                      + validator_bytes
                    in
                    if !total_bytes + approx_size > max_bytes && !records <> [] then begin
                      next_epoch := Some target_epoch;
                      stop := true
                    end else begin
                      records := record :: !records;
                      total_bytes := !total_bytes + approx_size;
                      incr i;
                      if !i >= max_chunk then
                        next_epoch := Some (Int64.add from_epoch (Int64.of_int !i))
                    end
              end
    done;
    match List.rev !records with
    | [] -> `NotFound
    | records -> `Ok (records, !next_epoch)
  with exn -> `Internal (Printexc.to_string exn)

let record_json (r : Octra_consensus.C_codec.catchup_epoch_record) =
  `Assoc [
    "epoch_id", `String (Int64.to_string r.epoch_id);
    "prev_state_root", `String (raw_to_hex r.prev_state_root);
    "state_root", `String (raw_to_hex r.state_root);
    "tx_list_hash", `String (raw_to_hex r.tx_list_hash);
    "tx_hashes", `List (List.map (fun h -> `String h) r.tx_hashes);
    "txs_json", `List (List.map (fun j -> `String j) r.txs_json);
    "receipt_root", `String (raw_to_hex r.receipt_root);
    "receipts_json", `List (List.map (fun j -> `String j) r.receipts_json);
    "epoch_ts", `Float r.epoch_ts;
    "creator_addr", `String r.creator_addr;
    "commit_round", `Int r.commit_round;
    "reward_source",
      (match r.reward_source with
       | Some source -> Octra_consensus.C_reward_source.to_yojson source
       | None -> `Null);
    "finality",
      (match r.finality with
       | None -> `Null
       | Some finality ->
         `Assoc [
           "finalize",
             `String
               (finality.finalize
                |> Octra_consensus.C_codec.encode_finalize
                |> Base64.encode_exn);
           "validator_set",
             `String
               (finality.validator_set
                |> Octra_consensus.C_codec.encode_validator_set
                |> Base64.encode_exn);
         ]);
  ]

let range_json ~on_stop ~chain_id ~data_dir ~chaindata ~reward_source
    ~read_finality ~from_epoch ~max_epochs =
  let head_fields =
    match Octra_core.Head_manifest.get_cached () with
    | None -> [ "head_epoch", `Null; "head_state_root", `Null ]
    | Some h -> [
        "head_epoch", `Int h.Octra_core.Head_manifest.epoch_id;
        "head_state_root", `String h.Octra_core.Head_manifest.state_root;
      ]
  in
  match
    build_range
      ~chain_id
      ~data_dir
      ~chaindata
      ~on_stop
      ~reward_source
      ~read_finality
      ~from_epoch
      ~max_epochs
  with
  | `Ok (records, next_epoch) ->
      `Assoc ([
        "version", `String state_sync_version;
        "mode", `String "catchup_range";
        "status", `String "ok";
        "from_epoch", `String (Int64.to_string from_epoch);
        "max_epochs", `Int max_epochs;
        "next_epoch",
          (match next_epoch with Some e -> `String (Int64.to_string e) | None -> `Null);
        "records", `List (List.map record_json records);
      ] @ head_fields)
  | `NotFound ->
      `Assoc ([
        "version", `String state_sync_version;
        "mode", `String "catchup_range";
        "status", `String "not_found";
        "from_epoch", `String (Int64.to_string from_epoch);
        "max_epochs", `Int max_epochs;
        "next_epoch", `Null;
        "records", `List [];
      ] @ head_fields)
  | `Internal msg ->
      `Assoc ([
        "version", `String state_sync_version;
        "mode", `String "catchup_range";
        "status", `String "error";
        "error", `String msg;
        "from_epoch", `String (Int64.to_string from_epoch);
        "max_epochs", `Int max_epochs;
        "next_epoch", `Null;
        "records", `List [];
      ] @ head_fields)

let read_chunk ~data_dir ~snapshot_id ~rel ~offset ~len =
  match normalize_path rel with
  | None -> Error "invalid path"
  | Some rel ->
      if not (path_allowed rel) then Error "path not allowed"
      else
        let base_result =
          match snapshot_id with
          | Some id when id <> "" && not (valid_snapshot_id id) ->
              Error "invalid snapshot id"
          | Some id when id <> "" ->
              let dir = snapshot_dir data_dir id in
              if Sys.file_exists (snapshot_ready_path dir) then Ok dir
              else Error "snapshot not found"
          | _ -> Ok data_dir
        in
        match base_result with
        | Error msg -> Error msg
        | Ok base ->
        let abs = Filename.concat base rel in
        if not (regular_file abs) then Error "file not found"
        else
          let st = Unix.stat abs in
          if offset < 0 || len <= 0 then Error "invalid offset/len"
          else if offset >= st.Unix.st_size then Ok ("", st.Unix.st_size)
          else
            let max_len = State_sync_limits.chunk_max in
            let len = min len max_len in
            let len = min len (st.Unix.st_size - offset) in
            let ic = open_in_bin abs in
            Fun.protect
              ~finally:(fun () -> close_in_noerr ic)
              (fun () ->
                seek_in ic offset;
                let buf = really_input_string ic len in
                Ok (buf, st.Unix.st_size))

let manifest_stats = function
  | `Assoc fields ->
      (match List.assoc_opt "files" fields with
       | Some (`List files) ->
           List.fold_left (fun (n, bytes) file ->
             match file with
             | `Assoc item_fields ->
                 let size =
                   match List.assoc_opt "size" item_fields with
                   | Some (`Int i) -> i
                   | Some (`Intlit s) -> (try int_of_string s with _ -> 0)
                   | _ -> 0
                 in
                 (n + 1, bytes + size)
             | _ -> (n + 1, bytes)
           ) (0, 0) files
       | _ -> (0, 0))
  | _ -> (0, 0)

let json_string fields name default =
  match List.assoc_opt name fields with
  | Some (`String s) -> s
  | _ -> default

let json_int fields name default =
  match List.assoc_opt name fields with
  | Some (`Int i) -> i
  | Some (`Intlit s) -> (try int_of_string s with _ -> default)
  | _ -> default

let json_float fields name default =
  match List.assoc_opt name fields with
  | Some (`Float f) -> f
  | Some (`Int i) -> float_of_int i
  | Some (`Intlit s) -> (try float_of_string s with _ -> default)
  | _ -> default

let json_float_opt fields name =
  match List.assoc_opt name fields with
  | Some (`Float f) -> Some f
  | Some (`Int i) -> Some (float_of_int i)
  | Some (`Intlit s) -> (try Some (float_of_string s) with _ -> None)
  | Some `Null | None -> None
  | _ -> None

let progress_to_yojson p =
  `Assoc [
    "peer", `String p.peer;
    "phase", `String p.phase;
    "path", `String p.path;
    "file_done", `Int p.file_done;
    "file_size", `Int p.file_size;
    "total_done", `Int p.total_done;
    "total_size", `Int p.total_size;
    "rate_bps", `Float p.rate_bps;
    "eta_seconds", (match p.eta_seconds with Some f -> `Float f | None -> `Null);
  ]

let progress_of_yojson = function
  | `Assoc fields ->
      Ok {
        peer = json_string fields "peer" "unknown";
        phase = json_string fields "phase" "unknown";
        path = json_string fields "path" "-";
        file_done = json_int fields "file_done" 0;
        file_size = json_int fields "file_size" 0;
        total_done = json_int fields "total_done" 0;
        total_size = json_int fields "total_size" 0;
        rate_bps = json_float fields "rate_bps" 0.0;
        eta_seconds = json_float_opt fields "eta_seconds";
      }
  | _ -> Error "progress report must be a JSON object"

let format_bytes bytes =
  let b = float_of_int (max 0 bytes) in
  let kib = 1024.0 in
  let mib = kib *. 1024.0 in
  let gib = mib *. 1024.0 in
  if b >= gib then Printf.sprintf "%.2fGB" (b /. gib)
  else if b >= mib then Printf.sprintf "%.2fMB" (b /. mib)
  else if b >= kib then Printf.sprintf "%.2fKB" (b /. kib)
  else Printf.sprintf "%dB" bytes

let format_rate bps =
  Printf.sprintf "%s/s" (format_bytes (int_of_float (max 0.0 bps)))

let format_eta = function
  | None -> "unknown"
  | Some seconds ->
      let seconds = int_of_float (max 0.0 seconds) in
      let h = seconds / 3600 in
      let m = (seconds mod 3600) / 60 in
      let s = seconds mod 60 in
      if h > 0 then Printf.sprintf "%dh%02dm%02ds" h m s
      else if m > 0 then Printf.sprintf "%dm%02ds" m s
      else Printf.sprintf "%ds" s

let pct done_ total =
  if total <= 0 then 0.0
  else (float_of_int done_ /. float_of_int total) *. 100.0

let progress_log_line p =
  Printf.sprintf
    "peer = %s phase = %s path = %s file_done = %s file_size = %s file_pct = %.2f%% total_done = %s total_size = %s total_pct = %.2f%% rate = %s eta = %s"
    p.peer
    p.phase
    p.path
    (format_bytes p.file_done)
    (format_bytes p.file_size)
    (pct p.file_done p.file_size)
    (format_bytes p.total_done)
    (format_bytes p.total_size)
    (pct p.total_done p.total_size)
    (format_rate p.rate_bps)
    (format_eta p.eta_seconds)