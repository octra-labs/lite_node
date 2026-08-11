(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Head = Octra_core.Head_manifest
module Irmin = Octra_core.Store_irmin
module Chaindata = Octra_core.Store_chaindata
module Floor = Octra_core.History_floor
module Verify = State_sync_verify
module Checkpoint = State_sync_checkpoint

type report = {
  epoch : int;
  state_root : string;
  ledger_state_root : string;
  irmin_commit : string;
  accounts : int;
  supply : Z.t;
  pvac_keys : int;
  files : int;
  bytes : int64;
}

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let fail message =
  Error message

let overlay_marker = "octra-checkpoint-overlay\n"

let isolated_source source_dir =
  let path = Filename.concat source_dir ".checkpoint-overlay" in
  try
    let input = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () ->
        let length = in_channel_length input in
        really_input_string input length = overlay_marker)
  with _ -> false

let protect label f =
  try Ok (f ()) with exn ->
    Error (label ^ ": " ^ Printexc.to_string exn)

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let rec strip_trailing_separator path =
  let length = String.length path in
  if length > 1 && path.[length - 1] = Filename.dir_sep.[0] then
    strip_trailing_separator (String.sub path 0 (length - 1))
  else
    path

let absolute_path path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  strip_trailing_separator path

let rec remove_tree path =
  if Sys.file_exists path then
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name ->
        if name <> "." && name <> ".." then
          remove_tree (Filename.concat path name));
      Unix.rmdir path
    | _ -> Unix.unlink path

let fsync_dir path =
  let descriptor = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close descriptor)
    (fun () -> Unix.fsync descriptor)

let all_zero bytes length =
  let rec loop index =
    index = length
    || Bytes.get bytes index = '\000' && loop (index + 1)
  in
  loop 0

let rec write_all descriptor bytes offset length =
  if length = 0 then ()
  else
    let written = Unix.write descriptor bytes offset length in
    if written = 0 then failwith "short checkpoint write"
    else write_all descriptor bytes (offset + written) (length - written)

let copy_sparse_file source target =
  mkdir_p (Filename.dirname target);
  let input = Unix.openfile source [Unix.O_RDONLY] 0 in
  let output =
    Unix.openfile target [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600
  in
  let digest = ref (Digestif.SHA256.init ()) in
  let buffer = Bytes.create (1024 * 1024) in
  Fun.protect
    ~finally:(fun () ->
      Unix.close input;
      Unix.close output)
    (fun () ->
      let rec loop size =
        match Unix.read input buffer 0 (Bytes.length buffer) with
        | 0 ->
          Unix.LargeFile.ftruncate output size;
          Unix.fsync output;
          Digestif.SHA256.get !digest |> Digestif.SHA256.to_hex, size
        | count ->
          digest := Digestif.SHA256.feed_bytes !digest ~off:0 ~len:count buffer;
          if all_zero buffer count then
            ignore (Unix.LargeFile.lseek output (Int64.of_int count) Unix.SEEK_CUR)
          else
            write_all output buffer 0 count;
          loop (Int64.add size (Int64.of_int count))
      in
      loop 0L)

let hash_file path =
  let channel = open_in_bin path in
  let buffer = Bytes.create (1024 * 1024) in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let rec loop digest =
        match input channel buffer 0 (Bytes.length buffer) with
        | 0 -> Digestif.SHA256.get digest |> Digestif.SHA256.to_hex
        | count ->
          loop (Digestif.SHA256.feed_bytes digest ~off:0 ~len:count buffer)
      in
      loop (Digestif.SHA256.init ()))

let copy_file source_dir target_dir path =
  let source = Filename.concat source_dir path in
  let target = Filename.concat target_dir path in
  let source_hash, size = copy_sparse_file source target in
  let target_hash = hash_file target in
  if source_hash <> target_hash then
    fail ("checkpoint file hash mismatch: " ^ path)
  else
    Ok size

let snapshot_shape data_dir =
  State_sync.list_files data_dir
  |> List.fold_left (fun (count, total) path ->
    let size =
      (Unix.LargeFile.stat (Filename.concat data_dir path)).Unix.LargeFile.st_size
    in
    count + 1, Int64.add total size
  ) (0, 0L)

let checkpoint_of_head head floor =
  Checkpoint.of_head
    ~chain_id:(Floor.chain_id floor)
    ~config_hash:(Floor.config_hash floor)
    ~validator_set_hash:(Floor.validator_set_hash floor)
    ~created_at:1L
    ~valid_until:2L
    head

let source_floor data_dir =
  try
    let store =
      Chaindata.open_chaindata
        ~readonly:true
        (Filename.concat data_dir "chaindata")
    in
    Fun.protect
      ~finally:(fun () -> Chaindata.close store)
      (fun () ->
        match Chaindata.history_floor store with
        | Error reason -> Error ("source history floor is invalid: " ^ reason)
        | Ok None -> Error "source history floor is missing"
        | Ok (Some floor) -> Ok floor)
  with exn ->
    Error ("source history floor read failed: " ^ Printexc.to_string exn)

let verify_data head data_dir =
  match source_floor data_dir with
  | Error _ as error -> error
  | Ok floor ->
      Lwt_main.run (Verify.verify (checkpoint_of_head head floor) data_dir)

let inspect_store data_dir expected_commit expected_root =
  let path = Filename.concat data_dir "irmin_store" in
  let store = Lwt_main.run (Irmin.open_store ~readonly:true path) in
  Fun.protect
    ~finally:(fun () -> Lwt_main.run (Irmin.close store))
    (fun () ->
      let commit = Lwt_main.run (Irmin.get_commit_hash store) in
      let root = Lwt_main.run (Irmin.get_head_hash store) in
      if commit <> Some expected_commit then
        fail "Irmin commit does not match checkpoint"
      else if root <> Some expected_root then
        fail "Irmin tree root does not match checkpoint"
      else
        let accounts = Lwt_main.run (Irmin.account_count store) in
        let supply = Lwt_main.run (Irmin.sum_balances store) in
        let pvac = Lwt_main.run (Irmin.inspect_pvac_blobs store) in
        if pvac.missing <> [] then fail "PVAC blob is missing"
        else if pvac.corrupt <> [] then fail "PVAC blob is corrupt"
        else Ok (accounts, supply, pvac.bound))

let write_ready path report =
  let body =
    `Assoc [
      "version", `String "octra-compact-checkpoint";
      "epoch", `Int report.epoch;
      "state_root", `String report.state_root;
      "ledger_state_root", `String report.ledger_state_root;
      "irmin_commit", `String report.irmin_commit;
      "accounts", `Int report.accounts;
      "supply", `String (Z.to_string report.supply);
      "pvac_keys", `Int report.pvac_keys;
      "files", `Int report.files;
      "bytes", `String (Int64.to_string report.bytes);
    ]
    |> Yojson.Safe.pretty_to_string
  in
  let output = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () ->
      output_string output body;
      output_char output '\n';
      flush output;
      Unix.fsync (Unix.descr_of_out_channel output))

let build ~source_dir ~target_dir =
  let source_dir = absolute_path source_dir in
  let target_dir = absolute_path target_dir in
  if source_dir = target_dir then fail "source and target directories are equal"
  else if not (isolated_source source_dir) then
    fail "source directory is not an isolated checkpoint overlay"
  else if Sys.file_exists target_dir then fail "checkpoint target already exists"
  else
    match Head.load_result source_dir with
    | Head.Missing -> fail "source HEAD.json is missing"
    | Head.Corrupt reason -> fail ("source HEAD.json is corrupt: " ^ reason)
    | Head.Present head ->
      let expected_commit =
        match head.irmin_commit with
        | Some value when value <> "" -> Ok value
        | _ -> fail "source HEAD.json has no Irmin commit"
      in
      let* expected_commit = expected_commit in
      let expected_root = Head.ledger_state_root head in
      let* () =
        if not (State_sync.no_commit_in_progress source_dir) then
          fail "source checkpoint has an active commit marker or WAL"
        else
          Ok ()
      in
      let* () = verify_data head source_dir in
      let* source_stats =
        inspect_store source_dir expected_commit expected_root
      in
      let staging =
        Printf.sprintf "%s.building.%d" target_dir (Unix.getpid ())
      in
      if Sys.file_exists staging then fail "checkpoint staging directory already exists"
      else
        protect "checkpoint compaction failed" (fun () ->
          mkdir_p (Filename.dirname target_dir);
          Unix.mkdir staging 0o700;
          let source_store =
            Lwt_main.run
              (Irmin.open_store ~readonly:true (Filename.concat source_dir "irmin_store"))
          in
          Fun.protect
            ~finally:(fun () -> Lwt_main.run (Irmin.close source_store))
            (fun () ->
              match Lwt_main.run
                (Irmin.create_compact_store
                  source_store
                  ~expected_commit
                  ~target:(Filename.concat staging "irmin_store")) with
              | Error reason -> failwith reason
              | Ok result ->
                if result.tree_hash <> expected_root then
                  failwith "compact Irmin root does not match checkpoint");
          let files =
            State_sync.list_files source_dir
            |> List.filter (fun path ->
              not (String.starts_with ~prefix:"irmin_store/" path))
          in
          let copied =
            List.fold_left (fun state path ->
              let* () = state in
              let* _ = copy_file source_dir staging path in
              Ok ()
            ) (Ok ()) files
          in
          begin
            match copied with
            | Ok () -> ()
            | Error reason -> failwith reason
          end;
          if not (State_sync.stable_head source_dir head) then
            failwith "source checkpoint changed during compaction";
          begin
            match verify_data head staging with
            | Ok () -> ()
            | Error reason -> failwith reason
          end;
          let target_stats =
            match inspect_store staging expected_commit expected_root with
            | Ok value -> value
            | Error reason -> failwith reason
          in
          if source_stats <> target_stats then
            failwith "compact checkpoint state counters mismatch";
          let accounts, supply, pvac_keys = target_stats in
          let files, bytes = snapshot_shape staging in
          let report = {
            epoch = head.epoch_id;
            state_root = head.state_root;
            ledger_state_root = expected_root;
            irmin_commit = expected_commit;
            accounts;
            supply;
            pvac_keys;
            files;
            bytes;
          } in
          write_ready (Filename.concat staging ".compact-ready.json") report;
          fsync_dir staging;
          Unix.rename staging target_dir;
          fsync_dir (Filename.dirname target_dir);
          report)
        |> Result.map_error (fun reason ->
          (try remove_tree staging with _ -> ());
          reason)