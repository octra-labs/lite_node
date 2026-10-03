(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  root : string;
  mutable lock : Octra_core.Store_lock.t option;
}

let rec mkdir path =
  if path <> "" && path <> "." && not (Sys.file_exists path) then begin
    mkdir (Filename.dirname path);
    try Unix.mkdir path 0o750 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let acquire root =
  try
    mkdir root;
    if (Unix.lstat root).Unix.st_kind <> Unix.S_DIR then
      Error "state sync archive is not a directory"
    else
      let root = Unix.realpath root in
      let lock = Octra_core.Store_lock.acquire root in
      Ok { root; lock = Some lock }
  with
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) ->
      Error "state sync archive is in use"
  | exn -> Error (Printexc.to_string exn)

let release owner =
  match owner.lock with
  | None -> ()
  | Some lock ->
      owner.lock <- None;
      Octra_core.Store_lock.release lock

let path owner id =
  if owner.lock = None then invalid_arg "state sync archive is closed";
  if not (State_sync.valid_snapshot_id id) then
    invalid_arg "state sync snapshot id is invalid";
  Filename.concat owner.root id

let owns owner target =
  let expected = path owner (Filename.basename target) in
  let actual =
    Filename.concat
      (Unix.realpath (Filename.dirname target))
      (Filename.basename target)
  in
  String.equal expected actual

let stage_mark = "octra-sync-archive-1\n"

let marker owner id =
  Filename.concat (path owner id ^ ".next") ".writer"

let marked root =
  try
    let file = Filename.concat root ".writer" in
    (Unix.lstat root).Unix.st_kind = Unix.S_DIR
    && (Unix.lstat file).Unix.st_kind = Unix.S_REG
    && In_channel.with_open_bin file (fun channel ->
      let body = really_input_string channel (String.length stage_mark) in
      String.equal body stage_mark
      && (try ignore (input_char channel); false with End_of_file -> true))
  with
  | End_of_file -> false
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false

let marked_stage owner id = marked (path owner id ^ ".next")

let mark_stage owner id =
  if (Unix.lstat (path owner id ^ ".next")).Unix.st_kind <> Unix.S_DIR then
    invalid_arg "state sync stage is not a directory";
  let output = open_out_gen
    [Open_wronly; Open_creat; Open_excl; Open_binary] 0o640 (marker owner id) in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () ->
    output_string output stage_mark;
    flush output;
    Unix.fsync (Unix.descr_of_out_channel output))

let sync_path sync path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> sync fd)

let rec sync_tree sync path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.iter (fun name -> sync_tree sync (Filename.concat path name));
      sync_path sync path
  | Unix.S_REG -> sync_path sync path
  | _ -> invalid_arg "state sync archive entry is not a regular file or directory"

let complete_move sync owner id =
  let target = path owner id in
  let file = Filename.concat target ".writer" in
  let remove = match Unix.lstat file with
    | _ ->
        if not (marked target) then
          invalid_arg "state sync image ownership is unknown";
        true
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false in
  sync_path sync owner.root;
  if remove then Unix.unlink file;
  sync_path sync target

let finish_publish ?(sync = Unix.fsync) owner id =
  sync_tree sync (path owner id);
  complete_move sync owner id

let publish_stage ?(sync = Unix.fsync) owner id =
  if not (marked_stage owner id) then
    invalid_arg "state sync stage ownership is unknown";
  let target = path owner id in
  let stage = target ^ ".next" in
  let exists = match Unix.lstat target with
    | _ -> true
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false in
  if exists then invalid_arg "state sync target already exists";
  sync_tree sync stage;
  Unix.rename stage target;
  complete_move sync owner id

let run root action =
  match acquire root with
  | Error _ as error -> error
  | Ok owner ->
      try
        Fun.protect ~finally:(fun () -> release owner)
          (fun () -> Ok (action owner))
      with exn -> Error (Printexc.to_string exn)

let run_lwt root action =
  match acquire root with
  | Error reason -> Lwt.return_error reason
  | Ok owner ->
      let task =
        Lwt.catch
          (fun () -> action owner)
          (fun exn -> Lwt.return_error (Printexc.to_string exn))
      in
      Lwt.finalize
        (fun () -> Lwt.protected task)
        (fun () ->
          Lwt.try_bind
            (fun () -> Lwt.no_cancel task)
            (fun _ -> release owner; Lwt.return_unit)
            (fun _ -> release owner; Lwt.return_unit))