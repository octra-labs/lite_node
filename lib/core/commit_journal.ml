(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

include Commit_record

module Scope = Store_scope

exception Read_error of string * int64 * string

let max_record_bytes = 1024 * 1024
let path data_dir = Filename.concat data_dir "commit_journal.log"

let mk_commit_id epoch_id =
  Printf.sprintf "ep%d-%d-%d" epoch_id
    (int_of_float (Unix.gettimeofday () *. 1000.))
    (Random.bits ())

let file_state path =
  match Unix.lstat path with
  | stat when stat.Unix.st_kind = Unix.S_REG -> Some stat
  | _ -> failwith ("commit journal is not a regular file: " ^ path)
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None

let with_file path flags expected action =
  let scope = Scope.create ~release:(fun () -> ()) in
  Scope.guard scope (fun () ->
    let fd = Scope.acquire scope (fun () -> Unix.openfile path flags 0o644) Unix.close in
    let stat = Unix.fstat fd in
    if stat.Unix.st_kind <> Unix.S_REG then failwith "commit journal file kind changed";
    (match expected with
    | Some prior when prior.Unix.st_dev <> stat.st_dev || prior.st_ino <> stat.st_ino ->
      failwith "commit journal file identity changed"
    | _ -> ());
    let value = action fd in
    Scope.close scope;
    value)

let rec read_bytes fd bytes offset count =
  try Unix.read fd bytes offset count with
  | Unix.Unix_error (Unix.EINTR, _, _) -> read_bytes fd bytes offset count

let check_end fd =
  let length = Unix.LargeFile.lseek fd 0L Unix.SEEK_END in
  if length > 0L then begin
    ignore (Unix.LargeFile.lseek fd (-1L) Unix.SEEK_END);
    let byte = Bytes.create 1 in
    if read_bytes fd byte 0 1 <> 1 || Bytes.get byte 0 <> '\n' then
      failwith "commit journal ends in an unfinished record"
  end

let rec write_bytes write fd line offset count =
  try write fd line offset count with
  | Unix.Unix_error (Unix.EINTR, _, _) -> write_bytes write fd line offset count

let rec write_all write fd line offset =
  if offset < String.length line then
    let count = String.length line - offset in
    let written = write_bytes write fd line offset count in
    if written <= 0 || written > count then failwith "commit journal write made invalid progress"
    else write_all write fd line (offset + written)

let sync_dir sync data_dir =
  let scope = Scope.create ~release:(fun () -> ()) in
  Scope.guard scope (fun () ->
    let fd = Scope.acquire scope (fun () ->
      Unix.openfile data_dir [Unix.O_RDONLY; Unix.O_CLOEXEC] 0) Unix.close in
    if (Unix.fstat fd).Unix.st_kind <> Unix.S_DIR then failwith "commit journal parent is not a directory";
    sync fd;
    Scope.close scope)

let append ?(sync = Unix.fsync) ?(write = Unix.write_substring) data_dir row =
  let json = record_to_json row in
  (match decode json with Ok _ -> () | Error reason -> failwith reason);
  let line = Yojson.Safe.to_string json ^ "\n" in
  if String.length line - 1 > max_record_bytes then failwith "commit journal record exceeds size limit";
  let target = path data_dir in
  let expected = file_state target in
  let flags = [Unix.O_RDWR; Unix.O_APPEND; Unix.O_CLOEXEC; Unix.O_NONBLOCK] @
    (match expected with None -> [Unix.O_CREAT; Unix.O_EXCL] | Some _ -> []) in
  with_file target flags expected (fun fd ->
    check_end fd;
    write_all write fd line 0;
    sync fd);
  sync_dir sync data_dir

let fold_records target fd initial step =
  let bytes = Bytes.create 65536 in
  let line = Buffer.create 256 in
  let offset = ref 0L in
  let state = ref initial in
  let fail reason = raise (Read_error (target, !offset, reason)) in
  let finish () =
    if Buffer.length line = 0 then fail "empty record";
    let json = try Yojson.Safe.from_string (Buffer.contents line) with
      | Yojson.Json_error reason -> fail reason in
    let row = match decode json with Ok row -> row | Error reason -> fail reason in
    state := step !state row;
    offset := Int64.add !offset (Int64.of_int (Buffer.length line + 1));
    Buffer.clear line in
  let rec chunk size start =
    if start < size then begin
      let stop = match Bytes.index_from_opt bytes start '\n' with
        | Some stop when stop < size -> stop
        | _ -> size in
      let count = stop - start in
      if count > max_record_bytes - Buffer.length line then fail "record exceeds size limit";
      Buffer.add_subbytes line bytes start count;
      if stop < size then begin finish (); chunk size (stop + 1) end
    end in
  let rec loop () =
    let size = try read_bytes fd bytes 0 (Bytes.length bytes) with
      | Unix.Unix_error _ as error -> fail (Printexc.to_string error) in
    match size with
    | 0 ->
      if Buffer.length line <> 0 then fail "unfinished record";
      !state
    | size -> chunk size 0; loop () in
  loop ()

let fold data_dir ~init ~f =
  let target = path data_dir in
  try
    match file_state target with
    | None -> init
    | Some _ as expected ->
      with_file target [Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK] expected
        (fun fd -> fold_records target fd init f)
  with
  | (Unix.Unix_error _ | Failure _ | Scope.Close_failed _) as error ->
    raise (Read_error (target, 0L, Printexc.to_string error))

let read_all data_dir =
  fold data_dir ~init:[] ~f:(fun rows row -> row :: rows) |> List.rev

let check data_dir = fold data_dir ~init:() ~f:(fun () _ -> ())

let last_commit_id_for_epoch journal epoch_id =
  let last = ref None in
  List.iter (function
    | Prepare p when p.epoch_id = epoch_id -> last := Some p.commit_id
    | Commit c when (try Scanf.sscanf c.commit_id "ep%d-%_d-%_d"
                            (fun e -> e = epoch_id) with _ -> false) ->
      last := Some c.commit_id
    | _ -> ()
  ) journal;
  !last

let pending_prepares ?head_commit_id journal =
  let committed = Hashtbl.create 16 in
  List.iter (function
    | Commit c -> Hashtbl.replace committed c.commit_id ()
    | _ -> ()
  ) journal;
  (match head_commit_id with
   | Some cid -> Hashtbl.replace committed cid ()
   | None -> ());
  List.filter_map (function
    | Prepare p when not (Hashtbl.mem committed p.commit_id) ->
      Some (p.commit_id, p.epoch_id)
    | _ -> None
  ) journal