(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Irmin_store = Store_irmin
module Store = Store_irmin.Store

type write_report = {
  commit : string;
  root : string;
  records : int64;
  bytes : int64;
  pvac_hashes : string list;
}

type restore_report = {
  commit : string;
  root : string;
  records : int64;
  bytes : int64;
}

type record =
  | Node of string list
  | Value of string list * string

type sink = {
  channel : out_channel;
  buffer : Buffer.t;
  mutable records : int64;
  mutable bytes : int64;
  mutable prior : string list option;
  mutable pvac_hashes : string list;
}

let prior_magic = "octra-ledger-image\n"
let path_magic = "octra-ledger-image-2\n"
let magic = "octra-ledger-image-3\n"
type format = Prior | Path64 | Full64
let prior_records = 16_777_216L
let max_path_parts = 1_024
let prior_part_bytes = 4_096
let prior_value_bytes = Transaction.circle_asset_max_encrypted_data_len
let drain_bytes = 4 * 1_024 * 1_024
let import_batch = 4_096
let import_reserve = Int64.mul 1_024L 1_024L |> Int64.mul 1_024L

external disk_free : string -> int64 = "octra_disk_free"

let space_need bytes =
  if Int64.compare bytes (Int64.div (Int64.sub Int64.max_int import_reserve) 2L) > 0
  then Int64.max_int
  else Int64.add (Int64.mul bytes 2L) import_reserve

let close_store store =
  Lwt.catch
    (fun () ->
      let* () = Irmin_store.close store in
      Lwt.return_none)
    (fun exn -> Lwt.return_some (Printexc.to_string exn))

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

let sync_path sync path =
  let descriptor = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close descriptor)
    (fun () -> sync descriptor)

let rec sync_tree sync path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> sync_tree sync (Filename.concat path name));
      sync_path sync path
  | Unix.S_REG -> sync_path sync path
  | _ -> failwith "restored ledger entry is not a regular file or directory"

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

let put_u64 buffer value =
  let value = Int64.of_int value in
  for index = 0 to 7 do
    Buffer.add_char buffer
      (Char.chr (Int64.to_int (Int64.logand 255L (Int64.shift_right_logical value (index * 8)))))
  done

let path_of_record = function
  | Node path
  | Value (path, _) -> path

let validate_path path =
  let count = List.length path in
  if count = 0 || count > max_path_parts then
    Error (Printf.sprintf "ledger image path width exceeds limit: parts = %d limit = %d"
      count max_path_parts)
  else Ok ()

let validate_order prior path =
  match prior with
  | None -> true
  | Some prior -> compare prior path < 0

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

let append sink value =
  let rec loop offset =
    if offset = String.length value then Lwt.return_unit
    else if Buffer.length sink.buffer >= drain_bytes then
      let* () = drain sink in
      loop offset
    else
      let count = min (String.length value - offset)
          (drain_bytes - Buffer.length sink.buffer) in
      Buffer.add_substring sink.buffer value offset count;
      sink.bytes <- Int64.add sink.bytes (Int64.of_int count);
      loop (offset + count)
  in
  loop 0

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
  if not (validate_order sink.prior path) then
    Lwt.fail_with "ledger image paths are not strictly ordered"
  else
    match validate_path path with
    | Error reason -> Lwt.fail_with reason
    | Ok () ->
        sink.records <- Int64.succ sink.records;
        sink.prior <- Some path;
        begin
          match record with
          | Node _ -> ()
          | Value (path, value) -> add_pvac sink path value
        end;
        put_u32 sink.buffer (List.length path);
        put_u8 sink.buffer (match record with Node _ -> 1 | Value _ -> 2);
        sink.bytes <- Int64.add sink.bytes 5L;
        let* () = Lwt_list.iter_s (fun part ->
          put_u64 sink.buffer (String.length part);
          sink.bytes <- Int64.add sink.bytes 8L;
          append sink part) path in
        let* () = match record with
          | Node _ -> Lwt.return_unit
          | Value (_, value) ->
              put_u64 sink.buffer (String.length value);
              sink.bytes <- Int64.add sink.bytes 8L;
              append sink value
        in
        if Buffer.length sink.buffer >= drain_bytes then drain sink
        else if Int64.rem sink.records 256L = 0L then Lwt.pause ()
        else Lwt.return_unit

let sorted_entries tree =
  let* sequence = Store.Tree.seq tree [] in
  let rec collect count entries sequence =
    match sequence () with
    | Seq.Nil -> Lwt.return (count, entries)
    | Seq.Cons (entry, rest) ->
        let count = count + 1 in
        let* () = if count mod 256 = 0 then Lwt.pause () else Lwt.return_unit in
        collect count (entry :: entries) rest in
  let* count, entries = collect 0 [] sequence in
  let sort entries =
    List.sort (fun (left, _) (right, _) -> String.compare left right) entries in
  if count < 256 then Lwt.return (sort entries)
  else Lwt_preemptive.detach sort entries

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

let discard channel path =
  let owned = try Some (Unix.fstat (Unix.descr_of_out_channel channel))
    with _ -> None in
  close_out_noerr channel;
  match owned with
  | None -> ()
  | Some owned ->
      try
        let current = Unix.lstat path in
        if current.st_kind = Unix.S_REG && current.st_dev = owned.st_dev
           && current.st_ino = owned.st_ino then Unix.unlink path
      with _ -> ()

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
                  Lwt.catch
                    (fun () ->
                      let sink = {
                        channel;
                        buffer = Buffer.create drain_bytes;
                        records = 0L;
                        bytes = Int64.of_int (String.length magic);
                        prior = None;
                        pvac_hashes = [];
                      } in
                      output_string channel magic;
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
                      discard channel path;
                      Lwt.return_error (Printexc.to_string exn)))
                (fun exn ->
                  Lwt.return_error (Printexc.to_string exn))
        end

type reader = {
  channel : in_channel;
  size : int64;
  format : format;
  mutable records : int64;
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

let read_u64 reader =
  let rec loop index value =
    if index = 8 then value
    else
      let byte = Int64.of_int (read_u8 reader) in
      loop (index + 1) (Int64.logor value (Int64.shift_left byte (index * 8)))
  in
  loop 0 0L

let read_string reader ~length ~max name =
  if length < 0L || length > Int64.of_int max then
    failwith (name ^ " exceeds limit")
  else if length > Int64.sub reader.size (LargeFile.pos_in reader.channel) then
    failwith (name ^ " exceeds remaining file bytes")
  else
    really_input_string reader.channel (Int64.to_int length)

let read_path reader count =
  if count < 1 || count > max_path_parts then
    failwith "ledger image path width exceeds limit";
  let rec loop remaining path =
    if remaining = 0 then List.rev path
    else
      let index = count - remaining in
      let length = match reader.format with
        | Prior -> Int64.of_int (read_u32 reader)
        | Path64 | Full64 -> read_u64 reader in
      let max = match reader.format with
        | Prior -> prior_part_bytes
        | Path64 | Full64 -> Sys.max_string_length in
      let name = Printf.sprintf "ledger image path part = %d bytes = %Ld" index length in
      let part = read_string reader ~length ~max name in
      loop (remaining - 1) (part :: path)
  in
  loop count []

let read_record reader =
  let count = read_u32 reader in
  if count = 0 then None
  else begin
    if reader.format <> Full64 && reader.records >= prior_records then
      failwith "ledger image record count exceeds limit";
    let kind = read_u8 reader in
    if kind <> 1 && kind <> 2 then
      failwith "ledger image record kind is invalid";
    let path = read_path reader count in
    if not (validate_order reader.prior path) then
      failwith "ledger image paths are not strictly ordered";
    reader.records <- Int64.succ reader.records;
    reader.prior <- Some path;
    match kind with
    | 1 -> Some (Node path)
    | 2 ->
        let length, max = match reader.format with
          | Prior | Path64 -> Int64.of_int (read_u32 reader), prior_value_bytes
          | Full64 -> read_u64 reader, Sys.max_string_length in
        let value = read_string reader ~length ~max "ledger image value" in
        Some (Value (path, value))
    | _ -> failwith "ledger image record kind is invalid"
  end

let exact_end reader =
  match input_char reader.channel with
  | _ -> failwith "ledger image has trailing bytes"
  | exception End_of_file -> ()

let read_header channel =
  let buffer = Buffer.create (String.length magic) in
  let rec loop () =
    if Buffer.length buffer >= String.length magic then
      failwith "ledger image header is invalid";
    let byte = input_char channel in
    Buffer.add_char buffer byte;
    if byte <> '\n' then loop ()
    else match Buffer.contents buffer with
      | value when value = prior_magic -> Prior
      | value when value = path_magic -> Path64
      | value when value = magic -> Full64
      | _ -> failwith "ledger image header is invalid"
  in
  loop ()

let restore_records store source =
  let channel = open_in_bin source in
  Lwt.finalize
    (fun () ->
      try
        let format = read_header channel in
        let size = LargeFile.in_channel_length channel in
        let reader = { channel; size; format; records = 0L; prior = None } in
        let commit tree =
          let* () = Irmin_store.commit_bulk store tree "state sync" in
          Store.flush store.Irmin_store.repo;
          Irmin_store.begin_bulk store
        in
        let rec loop tree pending bytes committed =
          let offset = LargeFile.pos_in channel in
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
              next tree pending bytes offset committed
          | Some (Value (path, value)) ->
              let* tree = Store.Tree.add tree path value in
              next tree pending bytes offset committed
        and next tree pending bytes offset committed =
          let pending = pending + 1 in
          let bytes = Int64.add bytes (Int64.sub (LargeFile.pos_in channel) offset) in
          if pending < import_batch && bytes < Int64.of_int drain_bytes then
            loop tree pending bytes committed
          else
            let* tree = commit tree in
            loop tree 0 0L true
        in
        let* tree = Irmin_store.begin_bulk store in
        loop tree 0 0L false
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

let build ~free source stage expected_root =
  let source_stat = Unix.LargeFile.lstat source in
  if source_stat.Unix.LargeFile.st_kind <> Unix.S_REG then
    Lwt.return_error "ledger image source is not a regular file"
  else
  let size = source_stat.Unix.LargeFile.st_size in
  let needed = space_need size in
  let available = free (Filename.dirname stage) in
  if Int64.compare available needed < 0 then
    Lwt.return_error
      (Printf.sprintf
         "ledger restore space is insufficient: need = %Ld available = %Ld"
         needed
         available)
  else
    let* store = Irmin_store.open_store ~fresh:true stage in
    let run () =
      let* records = restore_records store source in
      let* committed_root = Irmin_store.get_head_hash store in
      let* commit = Irmin_store.get_commit_hash store in
      match committed_root, commit with
      | Some committed_root, Some commit when committed_root = expected_root ->
          Lwt.return_ok { commit; root = committed_root; records; bytes = size }
      | Some _, Some _ ->
          Lwt.return_error "ledger image root differs from checkpoint"
      | _ -> Lwt.return_error "restored ledger commit is missing"
    in
    Lwt.try_bind
      run
      (fun result ->
        let* close_error = close_store store in
        match result, close_error with
        | Error _, _ | Ok _, None -> Lwt.return result
        | Ok _, Some reason ->
            Lwt.return_error ("restored ledger close failed: " ^ reason))
      (fun exn ->
        let* _ = close_store store in
        Lwt.fail exn)

let restore_new ~sync ~free ~source ~target ~expected_root =
  let stage = target ^ ".next" in
  Lwt.catch
    (fun () ->
      remove_tree stage;
      let* result = build ~free source stage expected_root in
      match result with
      | Error _ as error ->
          remove_tree stage;
          Lwt.return error
      | Ok report ->
          sync_tree sync stage;
          Unix.rename stage target;
          sync_path sync (Filename.dirname target);
          Lwt.return_ok report)
    (fun exn ->
      (try remove_tree stage with _ -> ());
      Lwt.fail exn)

let restore_run ~sync ~free ~source ~target ~expected_root =
  Lwt.catch
    (fun () ->
      if Sys.file_exists target then
        let* existing = verify_existing target expected_root in
        begin
          match existing with
          | Error _ as error -> Lwt.return error
          | Ok (commit, root) ->
              let size = (Unix.LargeFile.stat source).Unix.LargeFile.st_size in
              sync_tree sync target;
              sync_path sync (Filename.dirname target);
              Lwt.return_ok { commit; root; records = 0L; bytes = size }
        end
      else restore_new ~sync ~free ~source ~target ~expected_root)
    (fun exn ->
      Lwt.return_error (Printexc.to_string exn))

let restore_with ~free ~source ~target ~expected_root =
  restore_run ~sync:Unix.fsync ~free ~source ~target ~expected_root

let restore ~source ~target ~expected_root =
  restore_with ~free:disk_free ~source ~target ~expected_root