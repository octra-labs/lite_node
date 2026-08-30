(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Irmin_store = Store_irmin
module Store = Store_irmin.Store

type write_report = {
  commit : string;
  root : string;
  records : int;
  bytes : int64;
  pvac_hashes : string list;
}

type restore_report = {
  commit : string;
  root : string;
  records : int;
  bytes : int64;
}

type record =
  | Node of string list
  | Value of string list * string

type sink = {
  channel : out_channel;
  buffer : Buffer.t;
  mutable records : int;
  mutable bytes : int64;
  mutable prior : string list option;
  mutable pvac_hashes : string list;
}

let magic = "octra-ledger-image\n"
let max_records = 16_777_216
let max_path_parts = 1_024
let max_part_bytes = 4_096
let max_value_bytes = Transaction.circle_asset_max_encrypted_data_len
let drain_bytes = 4 * 1_024 * 1_024
let import_batch = 4_096

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

let lower_hex_64 value =
  String.length value = 64
  && String.for_all
       (function
         | '0'..'9' | 'a'..'f' -> true
         | _ -> false)
       value

let put_u8 buffer value =
  Buffer.add_char buffer (Char.chr value)

let put_u32 buffer value =
  if value < 0 then invalid_arg "negative ledger image length";
  Buffer.add_char buffer (Char.chr (value land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 16) land 0xff));
  Buffer.add_char buffer (Char.chr ((value lsr 24) land 0xff))

let put_string buffer value =
  put_u32 buffer (String.length value);
  Buffer.add_string buffer value

let path_of_record = function
  | Node path
  | Value (path, _) -> path

let validate_path path =
  path <> []
  && List.length path <= max_path_parts
  && List.for_all
       (fun part -> String.length part <= max_part_bytes)
       path

let validate_order prior path =
  match prior with
  | None -> true
  | Some prior -> compare prior path < 0

let encoded_record record =
  let path = path_of_record record in
  if not (validate_path path) then
    Error "ledger image path is invalid"
  else
    let buffer = Buffer.create 256 in
    put_u32 buffer (List.length path);
    begin
      match record with
      | Node _ -> put_u8 buffer 1
      | Value (_, value) ->
          if String.length value > max_value_bytes then
            raise (Invalid_argument "ledger image value exceeds limit");
          put_u8 buffer 2
    end;
    List.iter (put_string buffer) path;
    begin
      match record with
      | Node _ -> ()
      | Value (_, value) -> put_string buffer value
    end;
    Ok (Buffer.contents buffer)

let drain sink =
  if Buffer.length sink.buffer = 0 then Lwt.return_unit
  else
    let payload = Buffer.contents sink.buffer in
    Buffer.clear sink.buffer;
    Lwt_preemptive.detach
      (fun () ->
        output_string sink.channel payload;
        flush sink.channel)
      ()

let add_pvac sink path value =
  match path with
  | ["pvac_hashes"; _] when value <> "none" ->
      if lower_hex_64 value then
        sink.pvac_hashes <- value :: sink.pvac_hashes
      else
        failwith "ledger image PVAC hash is invalid"
  | _ -> ()

let emit sink record =
  let path = path_of_record record in
  if sink.records >= max_records then
    Lwt.fail_with "ledger image record count exceeds limit"
  else if not (validate_order sink.prior path) then
    Lwt.fail_with "ledger image paths are not strictly ordered"
  else
    match encoded_record record with
    | Error reason -> Lwt.fail_with reason
    | Ok encoded ->
        sink.records <- sink.records + 1;
        sink.bytes <- Int64.add sink.bytes (Int64.of_int (String.length encoded));
        sink.prior <- Some path;
        begin
          match record with
          | Node _ -> ()
          | Value (path, value) -> add_pvac sink path value
        end;
        Buffer.add_string sink.buffer encoded;
        if Buffer.length sink.buffer >= drain_bytes then drain sink
        else Lwt.return_unit

let sorted_entries tree =
  let* entries = Store.Tree.list tree [] in
  Lwt.return
    (List.sort
       (fun (left, _) (right, _) -> String.compare left right)
       entries)

let rec write_tree sink tree prefix =
  let* entries = sorted_entries tree in
  Lwt_list.iter_s
    (fun (name, child) ->
      let path = prefix @ [name] in
      let* kind = Store.Tree.kind child [] in
      match kind with
      | Some `Contents ->
          let* value = Store.Tree.find child [] in
          begin
            match value with
            | None -> Lwt.fail_with "ledger image content disappeared"
            | Some value -> emit sink (Value (path, value))
          end
      | Some `Node ->
          let* () = emit sink (Node path) in
          write_tree sink child path
      | None -> Lwt.fail_with "ledger image entry disappeared")
    entries

let write store ~commit ~path =
  if Sys.file_exists path then
    Lwt.return_error "ledger image target already exists"
  else
    match Irmin.Type.of_string Store.Hash.t commit with
    | Error _ -> Lwt.return_error "ledger image commit hash is invalid"
    | Ok hash when Irmin.Type.to_string Store.Hash.t hash <> commit ->
        Lwt.return_error "ledger image commit hash is not exact"
    | Ok hash ->
        let* selected = Store.Commit.of_hash store.Irmin_store.repo hash in
        begin
          match selected with
          | None -> Lwt.return_error "ledger image commit is unavailable"
          | Some selected ->
              Lwt.catch
                (fun () ->
                  let tree = Store.Commit.tree selected in
                  let root =
                    Irmin.Type.to_string Store.Hash.t (Store.Tree.hash tree)
                  in
                  let channel =
                    open_out_gen
                      [Open_wronly; Open_creat; Open_excl; Open_binary]
                      0o640
                      path
                  in
                  let sink = {
                    channel;
                    buffer = Buffer.create drain_bytes;
                    records = 0;
                    bytes = Int64.of_int (String.length magic);
                    prior = None;
                    pvac_hashes = [];
                  } in
                  output_string channel magic;
                  Lwt.catch
                    (fun () ->
                      let* () = write_tree sink tree [] in
                      put_u32 sink.buffer 0;
                      sink.bytes <- Int64.add sink.bytes 4L;
                      let* () = drain sink in
                      let* () =
                        Lwt_preemptive.detach
                          (fun () ->
                            Unix.fsync (Unix.descr_of_out_channel channel);
                            close_out channel)
                          ()
                      in
                      Lwt.return_ok {
                        commit;
                        root;
                        records = sink.records;
                        bytes = sink.bytes;
                        pvac_hashes =
                          List.sort_uniq String.compare sink.pvac_hashes;
                      })
                    (fun exn ->
                      close_out_noerr channel;
                      (try Unix.unlink path with _ -> ());
                      Lwt.return_error (Printexc.to_string exn)))
                (fun exn ->
                  (try Unix.unlink path with _ -> ());
                  Lwt.return_error (Printexc.to_string exn))
        end

type reader = {
  channel : in_channel;
  mutable records : int;
  mutable prior : string list option;
}

let read_u8 reader =
  input_byte reader.channel

let read_u32 reader =
  let b0 = read_u8 reader in
  let b1 = read_u8 reader in
  let b2 = read_u8 reader in
  let b3 = read_u8 reader in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let read_string reader ~max name =
  let length = read_u32 reader in
  if length < 0 || length > max then
    failwith (name ^ " exceeds limit")
  else
    really_input_string reader.channel length

let read_path reader count =
  if count < 1 || count > max_path_parts then
    failwith "ledger image path width exceeds limit";
  let rec loop remaining path =
    if remaining = 0 then List.rev path
    else
      let part = read_string reader ~max:max_part_bytes "ledger image path part" in
      loop (remaining - 1) (part :: path)
  in
  loop count []

let read_record reader =
  let count = read_u32 reader in
  if count = 0 then None
  else begin
    if reader.records >= max_records then
      failwith "ledger image record count exceeds limit";
    let kind = read_u8 reader in
    let path = read_path reader count in
    if not (validate_order reader.prior path) then
      failwith "ledger image paths are not strictly ordered";
    reader.records <- reader.records + 1;
    reader.prior <- Some path;
    match kind with
    | 1 -> Some (Node path)
    | 2 ->
        let value = read_string reader ~max:max_value_bytes "ledger image value" in
        Some (Value (path, value))
    | _ -> failwith "ledger image record kind is invalid"
  end

let exact_end reader =
  match input_char reader.channel with
  | _ -> failwith "ledger image has trailing bytes"
  | exception End_of_file -> ()

let restore_records store source =
  let channel = open_in_bin source in
  Lwt.finalize
    (fun () ->
      try
        let got_magic = really_input_string channel (String.length magic) in
        if got_magic <> magic then failwith "ledger image header is invalid";
        let reader = { channel; records = 0; prior = None } in
        let commit tree =
          let* () = Irmin_store.commit_bulk store tree "state sync" in
          Store.flush store.Irmin_store.repo;
          Irmin_store.begin_bulk store
        in
        let rec loop tree pending committed =
          match read_record reader with
          | None ->
              exact_end reader;
              if pending > 0 || not committed then
                let* _ = commit tree in
                Lwt.return reader.records
              else
                Lwt.return reader.records
          | Some (Node path) ->
              let* tree = Store.Tree.add_tree tree path (Store.Tree.empty ()) in
              next tree pending committed
          | Some (Value (path, value)) ->
              let* tree = Store.Tree.add tree path value in
              next tree pending committed
        and next tree pending committed =
          let pending = pending + 1 in
          if pending < import_batch then loop tree pending committed
          else
            let* tree = commit tree in
            loop tree 0 true
        in
        let* tree = Irmin_store.begin_bulk store in
        loop tree 0 false
      with exn -> Lwt.fail exn)
    (fun () ->
      close_in_noerr channel;
      Lwt.return_unit)

let verify_existing target expected_root =
  Lwt.catch
    (fun () ->
      let* store = Irmin_store.open_store ~readonly:true target in
      Lwt.finalize
        (fun () ->
          let* root = Irmin_store.get_head_hash store in
          let* commit = Irmin_store.get_commit_hash store in
          match root, commit with
          | Some root, Some commit when root = expected_root ->
              Lwt.return_ok (commit, root)
          | _ -> Lwt.return_error "restored ledger root differs")
        (fun () -> Irmin_store.close store))
    (fun exn -> Lwt.return_error (Printexc.to_string exn))

let build source stage expected_root =
  let source_stat = Unix.LargeFile.lstat source in
  if source_stat.Unix.LargeFile.st_kind <> Unix.S_REG then
    Lwt.return_error "ledger image source is not a regular file"
  else
  let size = source_stat.Unix.LargeFile.st_size in
  let* store = Irmin_store.open_store ~fresh:true stage in
  Lwt.finalize
    (fun () ->
      let* records = restore_records store source in
      let* committed_root = Irmin_store.get_head_hash store in
      let* commit = Irmin_store.get_commit_hash store in
      match committed_root, commit with
      | Some committed_root, Some commit when committed_root = expected_root ->
          Lwt.return_ok { commit; root = committed_root; records; bytes = size }
      | Some _, Some _ ->
          Lwt.return_error "ledger image root differs from checkpoint"
      | _ -> Lwt.return_error "restored ledger commit is missing")
    (fun () -> Irmin_store.close store)

let restore ~source ~target ~expected_root =
  let stage = target ^ ".next" in
  Lwt.catch
    (fun () ->
      if Sys.file_exists target then
        let* existing = verify_existing target expected_root in
        begin
          match existing with
          | Error _ as error -> Lwt.return error
          | Ok (commit, root) ->
              let size = (Unix.LargeFile.stat source).Unix.LargeFile.st_size in
              Lwt.return_ok { commit; root; records = 0; bytes = size }
        end
      else begin
        remove_tree stage;
        let* result = build source stage expected_root in
        match result with
        | Error _ as error ->
            remove_tree stage;
            Lwt.return error
        | Ok report ->
            Unix.rename stage target;
            fsync_dir (Filename.dirname target);
            Lwt.return_ok report
      end)
    (fun exn ->
      (try remove_tree stage with _ -> ());
      Lwt.return_error (Printexc.to_string exn))