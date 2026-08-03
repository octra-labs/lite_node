(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Head = Octra_core.Head_manifest
module Irmin = Octra_core.Store_irmin
module Image = Octra_core.Ledger_image
module Txlog = Octra_core.Txlog
module Epochlog = Octra_core.Epochlog

open Lwt.Infix

type source = {
  data_dir : string;
  head : Head.t;
  store : Irmin.t;
}

type report = {
  epoch : int;
  state_root : string;
  ledger_root : string;
  irmin_commit : string;
  files : int;
  bytes : int64;
}

type bounds = {
  commit : string;
  tx_seg : int;
  tx_off : int;
  epoch_off : int;
}

let bounds head =
  match head.Head.irmin_commit, head.txlog_seg, head.txlog_off, head.epochlog_off with
  | Some commit, Some tx_seg, Some tx_off, Some epoch_off
    when commit <> ""
         && tx_seg >= 0
         && tx_off >= Txlog.header_size
         && epoch_off >= Epochlog.header_size ->
      Ok { commit; tx_seg; tx_off; epoch_off }
  | _ -> Error "state sync head boundaries are incomplete"

let reference_head head =
  Head.{ head with irmin_commit = None }

let rec mkdir_p path =
  if path = "" || path = "." || Sys.file_exists path then ()
  else begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o750
  end

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

let rec write_all descriptor bytes offset length =
  if length > 0 then
    let count = Unix.write descriptor bytes offset length in
    if count = 0 then failwith "state sync write returned zero"
    else write_all descriptor bytes (offset + count) (length - count)

let write_file path body =
  mkdir_p (Filename.dirname path);
  let descriptor =
    Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o640
  in
  Fun.protect
    ~finally:(fun () -> Unix.close descriptor)
    (fun () ->
      let bytes = Bytes.of_string body in
      write_all descriptor bytes 0 (Bytes.length bytes);
      Unix.fsync descriptor)

let copy_exact source target size =
  if Int64.compare size 0L < 0 then failwith "state sync copy size is negative";
  let before = Unix.LargeFile.stat source in
  if before.Unix.LargeFile.st_kind <> Unix.S_REG then
    failwith "state sync source is not a regular file";
  if Int64.compare before.st_size size < 0 then
    failwith "state sync source is shorter than checkpoint boundary";
  mkdir_p (Filename.dirname target);
  let input = Unix.openfile source [Unix.O_RDONLY] 0 in
  let output =
    Unix.openfile target [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o640
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close input;
      Unix.close output)
    (fun () ->
      let buffer = Bytes.create (1024 * 1024) in
      let rec loop remaining =
        if Int64.compare remaining 0L > 0 then begin
          let count =
            min
              (Bytes.length buffer)
              (Int64.to_int (Int64.min remaining (Int64.of_int (Bytes.length buffer))))
          in
          let read = Unix.read input buffer 0 count in
          if read = 0 then failwith "state sync source ended before checkpoint boundary";
          write_all output buffer 0 read;
          loop (Int64.sub remaining (Int64.of_int read))
        end
      in
      loop size;
      Unix.LargeFile.ftruncate output size;
      Unix.fsync output)

let copy_whole source target =
  let before = Unix.LargeFile.stat source in
  copy_exact source target before.Unix.LargeFile.st_size;
  let after = Unix.LargeFile.stat source in
  if before.st_size <> after.st_size then
    failwith "state sync source changed during copy"

let copy_txlog source target bounds =
  let source_dir = Filename.concat source "chaindata/txlog" in
  let target_dir = Filename.concat target "chaindata/txlog" in
  let rec loop segment =
    if segment > bounds.tx_seg then ()
    else begin
      let source_path = Txlog.seg_path source_dir segment in
      let target_path = Txlog.seg_path target_dir segment in
      if segment = bounds.tx_seg then
        copy_exact source_path target_path (Int64.of_int bounds.tx_off)
      else
        copy_whole source_path target_path;
      loop (segment + 1)
    end
  in
  loop 0

let copy_epochlog source target bounds =
  copy_exact
    (Filename.concat source "chaindata/epochlog/epochs.dat")
    (Filename.concat target "chaindata/epochlog/epochs.dat")
    (Int64.of_int bounds.epoch_off)

let file_hash path =
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

let copy_pvac source target hashes =
  List.iter (fun hash ->
    let source_path = Irmin.pvac_blob_path source.store hash in
    let target_path = Filename.concat target ("pvac/blobs/" ^ hash ^ ".pk") in
    copy_whole source_path target_path;
    if file_hash target_path <> hash then
      failwith "state sync PVAC blob hash mismatch") hashes

let snapshot_shape root =
  State_sync.list_files root
  |> List.fold_left (fun (files, bytes) path ->
    let size =
      (Unix.LargeFile.stat (Filename.concat root path)).Unix.LargeFile.st_size
    in
    files + 1, Int64.add bytes size) (0, 0L)

let build source ~target =
  match bounds source.head with
  | Error _ as error -> Lwt.return error
  | Ok bounds ->
      let reference = reference_head source.head in
      if Sys.file_exists target then
        Lwt.return_error "state sync target already exists"
      else
        let stage = target ^ ".next" in
        if Sys.file_exists stage then
          Lwt.return_error "state sync staged target already exists"
        else
          Lwt.catch
            (fun () ->
              mkdir_p (Filename.dirname target);
              Unix.mkdir stage 0o750;
              Image.write
                source.store
                ~commit:bounds.commit
                ~path:(Filename.concat stage "ledger.dat")
              >>= function
              | Error reason -> Lwt.fail_with reason
              | Ok image when image.root <> Head.ledger_state_root source.head ->
                  Lwt.fail_with "state sync ledger image root mismatch"
              | Ok image ->
                  Lwt_preemptive.detach (fun () ->
                    copy_txlog source.data_dir stage bounds;
                    copy_epochlog source.data_dir stage bounds;
                    copy_pvac source stage image.pvac_hashes;
                    write_file
                      (Filename.concat stage "HEAD.json")
                      (Head.to_json reference);
                    write_file
                      (Filename.concat stage "state_root")
                      reference.state_root;
                    State_sync.write_ready stage reference;
                    let files, bytes = snapshot_shape stage in
                    fsync_dir stage;
                    Unix.rename stage target;
                    fsync_dir (Filename.dirname target);
                    {
                      epoch = source.head.epoch_id;
                      state_root = source.head.state_root;
                      ledger_root = Head.ledger_state_root source.head;
                      irmin_commit = bounds.commit;
                      files;
                      bytes;
                    }) () >>= fun report ->
                  Lwt.return_ok report)
            (fun exn ->
              let reason = Printexc.to_string exn in
              Lwt_preemptive.detach
                (fun () -> try remove_tree stage with _ -> ())
                () >>= fun () ->
              Lwt.return_error reason)