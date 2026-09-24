(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = { directory : string; mutable busy : bool }

let create ~data_dir = {
  directory = Filename.concat data_dir "validator-control";
  busy = false;
}

let protected mode stat =
  stat.Unix.st_kind = mode && stat.st_uid = Unix.getuid ()
  && stat.st_perm land 0o077 = 0

let inspect path =
  try Ok (Some (Unix.lstat path)) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
  | exn -> Error ("validator control read failed: " ^ Printexc.to_string exn)

let directory t =
  match inspect t.directory with
  | Error _ as error -> error
  | Ok None -> Ok false
  | Ok (Some stat) when protected Unix.S_DIR stat -> Ok true
  | Ok _ -> Error "validator control directory is not private"

let lock_status t =
  match inspect (Filename.concat t.directory "lock") with
  | Error _ as error -> error
  | Ok None -> Ok ()
  | Ok (Some stat) when protected Unix.S_REG stat
      && stat.st_perm land 0o600 = 0o600 -> Ok ()
  | Ok _ -> Error "validator control lock must be private and writable"

let sync_dir path =
  let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd)

let prepare t =
  match directory t with
  | Error _ as error -> error
  | Ok true -> Ok ()
  | Ok false ->
    begin
      try
        Unix.mkdir t.directory 0o700;
        sync_dir (Filename.dirname t.directory);
        Ok ()
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
        begin match directory t with
        | Ok true -> Ok ()
        | Ok false -> Error "validator control directory disappeared"
        | Error _ as error -> error
        end
      | exn -> Error ("validator control create failed: " ^ Printexc.to_string exn)
    end

let open_private path =
  let ( let* ) = Result.bind in
  let* prior = inspect path in
  match prior with
  | Some stat when not (protected Unix.S_REG stat) ->
    Error "validator control file is not private"
  | _ ->
    try
      let flags = match prior with
        | None -> [Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC]
        | Some _ -> [Unix.O_RDWR; Unix.O_CLOEXEC]
      in
      let fd = Unix.openfile path flags 0o600 in
      try
      let current = Unix.fstat fd in
      let linked = Unix.lstat path in
      if not (protected Unix.S_REG current)
         || linked.st_ino <> current.st_ino || linked.st_dev <> current.st_dev
         || not (protected Unix.S_REG linked) then begin
        Unix.close fd;
        Error "validator control file changed"
      end else Ok fd
      with exn -> Unix.close fd; raise exn
    with exn -> Error ("validator control open failed: " ^ Printexc.to_string exn)

let locked t action =
  let ( let* ) = Result.bind in
  if t.busy then Error "validator control is busy"
  else
    let* () = prepare t in
    let* fd = open_private (Filename.concat t.directory "lock") in
    t.busy <- true;
    Fun.protect
      ~finally:(fun () -> t.busy <- false; Unix.close fd)
      (fun () ->
        try
          Unix.lockf fd Unix.F_TLOCK 0;
          action ()
        with
        | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
          Error "validator control is busy"
        | exn -> Error ("validator control failed: " ^ Printexc.to_string exn))

let load t =
  let ( let* ) = Result.bind in
  let* exists = directory t in
  if not exists then Ok None
  else
    let* () = lock_status t in
    let path = Filename.concat t.directory "exit.json" in
    let* prior = inspect path in
    match prior with
    | None -> Ok None
    | Some stat when not (protected Unix.S_REG stat) || stat.st_size > 4_096 ->
      Error "invalid validator exit intent file"
    | Some stat ->
      try
        let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
        let channel = Unix.in_channel_of_descr fd in
        Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
          let current = Unix.fstat fd in
          if not (protected Unix.S_REG current) || current.st_ino <> stat.st_ino
             || current.st_dev <> stat.st_dev || current.st_size > 4_096 then
            Error "validator exit intent file changed"
          else
            really_input_string channel current.st_size
            |> Validator_intent.decode |> Result.map Option.some)
      with exn -> Error ("validator exit intent read failed: " ^ Printexc.to_string exn)

let status t identity =
  let ( let* ) = Result.bind in
  let* value = load t in
  match value with
  | None -> Ok None
  | Some value ->
    let* matches = Validator_intent.applies identity value in
    Ok (if matches then Some (Validator_intent.id value) else None)

let write t value =
  let ( let* ) = Result.bind in
  let path = Filename.concat t.directory "exit.json" in
  let next = Filename.concat t.directory "exit.next" in
  let* prior = inspect next in
  match prior with
  | Some stat when not (protected Unix.S_REG stat) ->
    Error "invalid validator exit staging file"
  | _ ->
    try
      Option.iter (fun _ -> Unix.unlink next) prior;
      let fd = Unix.openfile next
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600 in
      let channel = Unix.out_channel_of_descr fd in
      Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
        output_string channel (Validator_intent.encode value);
        flush channel;
        Unix.fsync fd);
      Unix.rename next path;
      sync_dir t.directory;
      Ok ()
    with exn -> Error ("validator exit intent write failed: " ^ Printexc.to_string exn)

let request t identity ~privkey =
  let ( let* ) = Result.bind in
  let* value = Validator_intent.create identity ~privkey in
  locked t (fun () ->
    let* prior = status t identity in
    match prior with
    | Some id -> sync_dir t.directory; Ok id
    | None ->
      let* () = write t value in
      Ok (Validator_intent.id value))

let cancel t identity ~privkey =
  let ( let* ) = Result.bind in
  let* _ = Validator_intent.create identity ~privkey in
  locked t (fun () ->
    let* value = load t in
    match value with
    | None -> Ok ()
    | Some value ->
      let* _ = Validator_intent.applies identity value in
      Unix.unlink (Filename.concat t.directory "exit.json");
      sync_dir t.directory;
      Ok ())

let guard t identity action =
  locked t (fun () ->
    match status t identity with
    | Error _ as error -> error
    | Ok (Some _) -> Error "validator exit intent pauses duty"
    | Ok None -> action ())