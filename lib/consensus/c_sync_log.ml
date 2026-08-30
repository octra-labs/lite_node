(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Memory of (string, C_codec.round_sync) Hashtbl.t
  | Disk of string

let ( let* ) result f =
  Result.bind result f

let max_wire_bytes = 512
let max_records = 3 * (C_engine.sync_history_limit + 1)
let max_before_trim = max_records + 3

let disk ~data_dir =
  Disk (Filename.concat data_dir "round_sync_log")

let memory () =
  Memory (Hashtbl.create max_records)

let error exn =
  Printexc.to_string exn

let step = function
  | C_types.ProposeStep -> "propose"
  | C_types.PrevoteStep -> "prevote"
  | C_types.PrecommitStep -> "precommit"

let step_rank = function
  | C_types.ProposeStep -> 1
  | C_types.PrevoteStep -> 2
  | C_types.PrecommitStep -> 3

let key (sync : C_codec.round_sync) =
  String.concat "\x00"
    [ "octra:round_sync_log";
      sync.chain_id;
      sync.validator;
      Int64.to_string sync.epoch_id;
      string_of_int sync.round;
      step sync.step ]

let same (left : C_codec.round_sync) (right : C_codec.round_sync) =
  String.equal left.chain_id right.chain_id
  && Int64.equal left.epoch_id right.epoch_id
  && left.round = right.round
  && left.step = right.step
  && Bool.equal left.request right.request
  && String.equal left.validator right.validator

let digest (sync : C_codec.round_sync) =
  key sync
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let name (sync : C_codec.round_sync) =
  Printf.sprintf "%020Ld_%08d_%s_%s.sync"
    sync.C_codec.epoch_id
    sync.round
    (step sync.step)
    (digest sync)

let epoch_name epoch_id =
  Printf.sprintf "%020Ld" epoch_id

let epoch_path root epoch_id =
  Filename.concat root (epoch_name epoch_id)

let valid (sync : C_codec.round_sync) =
  not sync.request
  && Int64.compare sync.epoch_id 0L >= 0
  && sync.round >= 0
  && sync.round <= C_codec.max_round
  && String.length sync.signature = 64

let rec write_all fd wire offset =
  if offset < String.length wire then begin
    let written =
      Unix.write_substring fd wire offset (String.length wire - offset)
    in
    if written <= 0 then failwith "round sync log write returned zero";
    write_all fd wire (offset + written)
  end

let sync_dir path =
  try
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> Unix.fsync fd);
    Ok ()
  with exn ->
    Error (error exn)

let rec make_dir path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR -> Ok ()
    | _ -> Error "round sync log path is not a directory"
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    (try
       Unix.mkdir path 0o700;
       let* () = sync_dir (Filename.dirname path) in
       sync_dir path
     with
     | Unix.Unix_error (Unix.EEXIST, _, _) -> make_dir path
     | exn -> Error (error exn))
  | exn -> Error (error exn)

let existing_dir path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR -> Ok true
    | _ -> Error "round sync log path is not a directory"
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | exn -> Error (error exn)

let read_all path size =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel size)

let read path =
  try
    let stat = Unix.lstat path in
    if stat.Unix.st_kind <> Unix.S_REG then
      Error "round sync log record is not regular"
    else if stat.Unix.st_size <= 0 || stat.Unix.st_size > max_wire_bytes then
      Error "round sync log record size is invalid"
    else
      let wire = read_all path stat.Unix.st_size in
      let sync = C_codec.decode_round_sync wire in
      if String.equal (C_codec.encode_round_sync sync) wire then Ok sync
      else Error "round sync log wire is not exact"
  with exn ->
    Error (error exn)

let write path wire =
  let fd =
    Unix.openfile path
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL]
      0o600
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      write_all fd wire 0;
      Unix.fsync fd)

let compare_sync (left : C_codec.round_sync) (right : C_codec.round_sync) =
  let by_round = Int.compare left.round right.round in
  if by_round <> 0 then by_round
  else Int.compare (step_rank left.step) (step_rank right.step)

let records path =
  Sys.readdir path
  |> Array.to_list
  |> List.filter (fun entry -> Filename.check_suffix entry ".sync")

let check ~chain_id ~validator ~epoch_id ~verify sync =
  if not (valid sync) then Error "round sync log record is invalid"
  else if not (String.equal sync.chain_id chain_id) then
    Error "round sync log chain does not match"
  else if not (String.equal sync.validator validator) then
    Error "round sync log validator does not match"
  else if not (Int64.equal sync.epoch_id epoch_id) then
    Error "round sync log epoch does not match"
  else if not (verify sync) then
    Error "round sync log signature is invalid"
  else Ok sync

let trim path epoch_id round =
  let floor = max 0 (round - C_engine.sync_history_limit) in
  try
    let entries = records path in
    if List.length entries > max_before_trim then
      Error "round sync log record limit exceeded"
    else
      let* removed =
        List.fold_left
          (fun result entry ->
            let* removed = result in
            let record = Filename.concat path entry in
            let* sync = read record in
            if not (Int64.equal sync.epoch_id epoch_id) then
              Error "round sync log epoch does not match"
            else if sync.round < floor then begin
              Unix.unlink record;
              Ok true
            end else
              Ok removed)
          (Ok false)
          entries
      in
      if removed then sync_dir path else Ok ()
  with exn ->
    Error (error exn)

let keep_disk root ~verify sync =
  if not (valid sync) || not (verify sync) then
    Error "round sync log record is invalid"
  else
    let* () = make_dir root in
    let path = epoch_path root sync.C_codec.epoch_id in
    let* () = make_dir path in
    let record = Filename.concat path (name sync) in
    let wire = C_codec.encode_round_sync sync in
    let* stored, wrote =
      try
        write record wire;
        Ok (sync, true)
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
        (match read record with
         | Ok stored when same stored sync && verify stored -> Ok (stored, false)
         | Ok _ -> Error "round sync log record conflicts"
         | Error reason -> Error reason)
      | exn -> Error (error exn)
    in
    let* () =
      if wrote then begin
        let* () = sync_dir path in
        trim path sync.epoch_id sync.round
      end else
        Ok ()
    in
    Ok stored

let keep store ~verify sync =
  match store with
  | Memory records ->
    if not (valid sync) || not (verify sync) then
      Error "round sync log record is invalid"
    else
      let id = key sync in
      (match Hashtbl.find_opt records id with
       | None ->
         Hashtbl.add records id sync;
         let floor = max 0 (sync.round - C_engine.sync_history_limit) in
         Hashtbl.filter_map_inplace
           (fun _ (saved : C_codec.round_sync) ->
             if Int64.equal saved.epoch_id sync.epoch_id && saved.round < floor
             then None
             else Some saved)
           records;
         let count =
           Hashtbl.fold
             (fun _ (saved : C_codec.round_sync) count ->
               if Int64.equal saved.epoch_id sync.epoch_id then count + 1
               else count)
             records
             0
         in
         if count <= max_records then Ok sync
         else begin
           Hashtbl.remove records id;
           Error "round sync log record limit exceeded"
         end
       | Some stored when same stored sync && verify stored -> Ok stored
       | Some _ -> Error "round sync log record conflicts")
  | Disk root -> keep_disk root ~verify sync

let load_disk root ~chain_id ~validator ~epoch_id ~verify =
  let* root_exists = existing_dir root in
  if not root_exists then Ok []
  else
    let path = epoch_path root epoch_id in
    let* path_exists = existing_dir path in
    if not path_exists then Ok []
    else
      try
        let entries = records path in
        if List.length entries > max_records then
          Error "round sync log record limit exceeded"
        else
          List.fold_left
            (fun result entry ->
              let* values = result in
              let* sync = read (Filename.concat path entry) in
              if not (String.equal entry (name sync)) then
                Error "round sync log name does not match"
              else
                let* sync =
                  check ~chain_id ~validator ~epoch_id ~verify sync
                in
                Ok (sync :: values))
            (Ok [])
            entries
          |> Result.map (List.sort compare_sync)
      with exn ->
        Error (error exn)

let load store ~chain_id ~validator ~epoch_id ~verify =
  match store with
  | Memory records ->
    Hashtbl.to_seq_values records
    |> List.of_seq
    |> List.filter (fun (sync : C_codec.round_sync) ->
      Int64.equal sync.epoch_id epoch_id)
    |> List.fold_left
      (fun result sync ->
        let* values = result in
        let* sync = check ~chain_id ~validator ~epoch_id ~verify sync in
        Ok (sync :: values))
      (Ok [])
    |> Result.map (List.sort compare_sync)
  | Disk root -> load_disk root ~chain_id ~validator ~epoch_id ~verify

let epoch_of_name name =
  try
    let epoch_id = Int64.of_string name in
    if String.equal name (epoch_name epoch_id) then Some epoch_id else None
  with _ -> None

let rec remove path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_REG -> Unix.unlink path
  | Unix.S_DIR ->
    Sys.readdir path
    |> Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path
  | _ -> failwith "round sync log entry is not removable"

let prune_disk root through_epoch =
  let* exists = existing_dir root in
  if not exists then Ok ()
  else
    try
      let removed = ref false in
      Sys.readdir root
      |> Array.iter (fun name ->
        match epoch_of_name name with
        | Some epoch_id when Int64.compare epoch_id through_epoch <= 0 ->
          remove (Filename.concat root name);
          removed := true
        | _ -> ());
      if !removed then sync_dir root else Ok ()
    with exn ->
      Error (error exn)

let prune store ~through_epoch =
  match store with
  | Memory records ->
    Hashtbl.filter_map_inplace
      (fun _ (sync : C_codec.round_sync) ->
        if Int64.compare sync.epoch_id through_epoch <= 0 then None
        else Some sync)
      records;
    Ok ()
  | Disk root -> prune_disk root through_epoch