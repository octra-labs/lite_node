(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types

let ( let* ) result f =
  Result.bind result f

type t =
  | Memory of (string, vote) Hashtbl.t
  | Disk of string

let max_wire_bytes = 512
let max_epoch_files = 4_096
let max_floor_bytes = 32
let max_round_mark_bytes = 64
let staged_serial = ref 0

let disk ~data_dir =
  Disk (Filename.concat data_dir "vote_log")

let memory () =
  Memory (Hashtbl.create 16)

let vote_type = function
  | Prevote -> "prevote"
  | Precommit -> "precommit"

let key (vote : vote) =
  String.concat "\x00"
    [ "octra:local_vote:v1";
      vote.chain_id;
      vote.validator;
      Int64.to_string vote.epoch_id;
      string_of_int vote.round;
      vote_type vote.vote_type ]

let same_statement (left : vote) (right : vote) =
  String.equal left.chain_id right.chain_id
  && Int64.equal left.epoch_id right.epoch_id
  && left.round = right.round
  && left.vote_type = right.vote_type
  && String.equal left.proposal_id right.proposal_id
  && String.equal left.validator right.validator

let digest (vote : vote) =
  key vote
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let name (vote : vote) =
  Printf.sprintf "%020Ld_%08d_%s.vote"
    vote.epoch_id
    vote.round
    (digest vote)

type record_position = {
  epoch_id : int64;
  round : int;
}

let lowercase_hex value =
  String.length value = 64
  && String.for_all
       (function
         | '0' .. '9'
         | 'a' .. 'f' -> true
         | _ -> false)
       value

let record_position entry =
  try
    if not (Filename.check_suffix entry ".vote") then None
    else
      let body = Filename.remove_extension entry in
      match String.split_on_char '_' body with
      | [raw_epoch; raw_round; raw_digest] ->
        let epoch_id = Int64.of_string raw_epoch in
        let round = int_of_string raw_round in
        if Int64.compare epoch_id 0L >= 0
           && Int64.compare epoch_id Int64.max_int < 0
           && round >= 0
           && round <= C_codec.max_round
           && String.equal raw_epoch (Printf.sprintf "%020Ld" epoch_id)
           && String.equal raw_round (Printf.sprintf "%08d" round)
           && lowercase_hex raw_digest
        then Some { epoch_id; round }
        else None
      | _ -> None
  with _ ->
    None

let floor_path path =
  Filename.concat path "floor"

let round_mark_path path =
  Filename.concat path "round_mark"

let error exn =
  Printexc.to_string exn

let sync_directory path =
  try
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> Unix.fsync fd);
    Ok ()
  with exn ->
    Error (error exn)

let rec directory path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR -> Ok ()
    | _ -> Error "vote log path is not a directory"
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    (try
       Unix.mkdir path 0o700;
       let* () = sync_directory (Filename.dirname path) in
       sync_directory path
     with
     | Unix.Unix_error (Unix.EEXIST, _, _) -> directory path
     | exn -> Error (error exn))
  | exn -> Error (error exn)

let read_all path size =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel size)

let read_floor path =
  try
    let stat = Unix.lstat path in
    if stat.Unix.st_kind <> Unix.S_REG then
      Error "vote log floor is not regular"
    else if stat.Unix.st_size <= 0 || stat.Unix.st_size > max_floor_bytes then
      Error "vote log floor size is invalid"
    else
      let raw = read_all path stat.Unix.st_size in
      if raw.[String.length raw - 1] <> '\n' then
        Error "vote log floor is invalid"
      else
        let body = String.sub raw 0 (String.length raw - 1) in
        let value = Int64.of_string body in
        if String.equal raw (Int64.to_string value ^ "\n") then Ok value
        else Error "vote log floor is invalid"
  with exn ->
    Error (error exn)

let round_mark_wire epoch_id round =
  Int64.to_string epoch_id ^ " " ^ string_of_int round ^ "\n"

let valid_round_mark epoch_id round =
  Int64.compare epoch_id 0L >= 0
  && Int64.compare epoch_id Int64.max_int < 0
  && round >= 0
  && round <= C_codec.max_round

let read_round_mark path =
  try
    let stat = Unix.lstat path in
    if stat.Unix.st_kind <> Unix.S_REG then
      Error "vote log round mark is not regular"
    else if stat.Unix.st_size <= 0 || stat.Unix.st_size > max_round_mark_bytes then
      Error "vote log round mark size is invalid"
    else
      let raw = read_all path stat.Unix.st_size in
      if raw.[String.length raw - 1] <> '\n' then
        Error "vote log round mark is invalid"
      else
        let body = String.sub raw 0 (String.length raw - 1) in
        match String.split_on_char ' ' body with
        | [raw_epoch; raw_round] ->
          let epoch_id = Int64.of_string raw_epoch in
          let round = int_of_string raw_round in
          if valid_round_mark epoch_id round
             && String.equal raw (round_mark_wire epoch_id round)
          then Ok (epoch_id, round)
          else Error "vote log round mark is invalid"
        | _ -> Error "vote log round mark is invalid"
  with exn ->
    Error (error exn)

let floor = function
  | Memory _ ->
    Ok (Some Int64.min_int)
  | Disk path ->
    if not (Sys.file_exists path) then Ok None
    else
      match directory path with
      | Error reason -> Error reason
      | Ok () ->
        let record = floor_path path in
        if Sys.file_exists record then
          match read_floor record with
          | Ok value -> Ok (Some value)
          | Error reason -> Error reason
        else
          Ok None

let round_mark = function
  | Memory _ ->
    Ok None
  | Disk path ->
    if not (Sys.file_exists path) then Ok None
    else
      match directory path with
      | Error reason -> Error reason
      | Ok () ->
        let record = round_mark_path path in
        if Sys.file_exists record then
          read_round_mark record |> Result.map Option.some
        else
          Ok None

let vote_of_wire wire =
  try
    let vote = C_codec.decode_vote wire in
    if String.equal (C_codec.encode_vote vote) wire then Ok vote
    else Error "vote log wire is not exact"
  with exn ->
    Error (error exn)

let read path =
  try
    let stat = Unix.lstat path in
    if stat.Unix.st_kind <> Unix.S_REG then
      Error "vote log record is not regular"
    else if stat.Unix.st_size <= 0 || stat.Unix.st_size > max_wire_bytes then
      Error "vote log record size is invalid"
    else
      read_all path stat.Unix.st_size
      |> vote_of_wire
  with exn ->
    Error (error exn)

let rec write_all fd wire offset =
  if offset < String.length wire then begin
    let written =
      Unix.write_substring fd wire offset (String.length wire - offset)
    in
    if written <= 0 then failwith "vote log write returned zero";
    write_all fd wire (offset + written)
  end

let rec open_staged record attempt =
  if attempt >= 1024 then
    Error "vote log staged record limit exceeded"
  else
    let serial = !staged_serial in
    incr staged_serial;
    let staged =
      record ^ "." ^ string_of_int (Unix.getpid ()) ^ "."
      ^ string_of_int serial ^ ".staged"
    in
    try
      Ok (staged,
          Unix.openfile staged
            [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL]
            0o600)
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) ->
      open_staged record (attempt + 1)
    | exn -> Error (error exn)

let publish_record path record wire =
  let* staged, fd = open_staged record 0 in
  let renamed = ref false in
  try
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        write_all fd wire 0;
        Unix.fsync fd);
    Unix.rename staged record;
    renamed := true;
    sync_directory path
  with exn ->
    if not !renamed then (try Unix.unlink staged with _ -> ());
    Error (error exn)

let publish_new_record path record wire =
  let* staged, fd = open_staged record 0 in
  try
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        write_all fd wire 0;
        Unix.fsync fd);
    let result =
      try
        Unix.link staged record;
        Ok `Published
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok `Existing
      | exn -> Error (error exn)
    in
    (try Unix.unlink staged with _ -> ());
    let* result = result in
    let* () = sync_directory path in
    Ok result
  with exn ->
    (try Unix.unlink staged with _ -> ());
    Error (error exn)

let write_floor path value =
  publish_record path (floor_path path) (Int64.to_string value ^ "\n")

let write_round_mark path epoch_id round =
  publish_record path (round_mark_path path) (round_mark_wire epoch_id round)

let set_floor store ~through_epoch =
  match store with
  | Memory _ ->
    Ok ()
  | Disk path ->
    let* prior = floor store in
    match prior with
    | Some value when Int64.compare value through_epoch > 0 ->
      Error "vote log floor exceeds finalized epoch"
    | Some value when Int64.equal value through_epoch ->
      Ok ()
    | Some _ ->
      write_floor path through_epoch
    | None ->
      let* () = directory path in
      write_floor path through_epoch

let set_round_mark store ~epoch_id ~round =
  if not (valid_round_mark epoch_id round) then
    Error "vote log round mark is invalid"
  else
    match store with
    | Memory _ ->
      Ok ()
    | Disk path ->
      let* () = directory path in
      let* prior = round_mark store in
      match prior with
      | Some (prior_epoch, _) when Int64.compare prior_epoch epoch_id > 0 ->
        Error "vote log round mark exceeds current epoch"
      | Some (prior_epoch, prior_round)
        when Int64.equal prior_epoch epoch_id && prior_round > round ->
        Error "vote log round mark exceeds current round"
      | Some (prior_epoch, prior_round)
        when Int64.equal prior_epoch epoch_id && prior_round = round ->
        Ok ()
      | _ ->
        write_round_mark path epoch_id round

let check_existing path (vote : vote) =
  match read path with
  | Error reason -> Error reason
  | Ok stored when same_statement stored vote -> Ok stored
  | Ok _ -> Error "local vote conflicts with stored vote"

let check_disk path (vote : vote) =
  let* () = directory path in
  let record = Filename.concat path (name vote) in
  if Sys.file_exists record then
    check_existing record vote |> Result.map (fun _ -> ())
  else
    Ok ()

let check store (vote : vote) =
  match store with
  | Memory records ->
    let id = key vote in
    (match Hashtbl.find_opt records id with
     | None -> Ok ()
     | Some stored when same_statement stored vote -> Ok ()
     | Some _ -> Error "local vote conflicts with stored vote")
  | Disk path -> check_disk path vote

let keep_disk path (vote : vote) =
  match directory path with
  | Error reason -> Error reason
  | Ok () ->
    let record = Filename.concat path (name vote) in
    let wire = C_codec.encode_vote vote in
    if Sys.file_exists record then
      check_existing record vote
    else
      match publish_new_record path record wire with
      | Error reason -> Error reason
      | Ok `Published -> Ok vote
      | Ok `Existing -> check_existing record vote

let keep store (vote : vote) =
  match store with
  | Memory records ->
    let id = key vote in
    (match Hashtbl.find_opt records id with
     | None ->
       Hashtbl.add records id vote;
       Ok vote
     | Some stored when same_statement stored vote -> Ok stored
     | Some _ -> Error "local vote conflicts with stored vote")
  | Disk path -> keep_disk path vote

let belongs ~chain_id ~validator ~epoch_id (vote : vote) =
  String.equal vote.chain_id chain_id
  && String.equal vote.validator validator
  && Int64.equal vote.epoch_id epoch_id

let max_value (values : int list) =
  List.fold_left
    (fun current value ->
      match current with
      | None -> Some value
      | Some prior -> Some (max prior value))
    None
    values

let with_round_mark epoch_id mark rounds =
  match mark with
  | Some (marked_epoch, round) when Int64.equal marked_epoch epoch_id ->
    round :: rounds
  | _ -> rounds

let max_round store ~chain_id ~validator ~epoch_id =
  match store with
  | Memory records ->
    Hashtbl.to_seq_values records
    |> List.of_seq
    |> List.filter (belongs ~chain_id ~validator ~epoch_id)
    |> List.map (fun (vote : vote) -> vote.round)
    |> with_round_mark epoch_id None
    |> max_value
    |> fun value -> Ok value
  | Disk path ->
    if not (Sys.file_exists path) then Ok None
    else
      match directory path with
      | Error reason -> Error reason
      | Ok () ->
        (match sync_directory path with
         | Error reason -> Error reason
         | Ok () ->
           try
             let entries =
               Sys.readdir path
               |> Array.to_list
               |> List.filter_map (fun entry ->
                 match record_position entry with
                 | Some position when Int64.equal position.epoch_id epoch_id ->
                   Some (entry, position)
                 | _ -> None)
             in
             if List.length entries > max_epoch_files then
               Error "vote log epoch record limit exceeded"
             else
               let votes =
                 List.fold_left
                   (fun result (entry, position) ->
                     let* values = result in
                     match read (Filename.concat path entry) with
                     | Ok vote when Int64.equal vote.epoch_id epoch_id ->
                       if belongs ~chain_id ~validator ~epoch_id vote then
                         Ok (vote.round :: values)
                       else
                         Ok values
                     | Ok _ ->
                       Error "vote log epoch does not match file"
                     | Error _ ->
                       Ok (position.round :: values))
                   (Ok [])
                   entries
               in
               let* rounds = votes in
               let* mark = round_mark store in
               rounds
               |> with_round_mark epoch_id mark
               |> max_value
               |> fun value -> Ok value
           with exn ->
             Error (error exn))

let prune_disk path through_epoch =
  if not (Sys.file_exists path) then Ok ()
  else
    match directory path with
    | Error reason -> Error reason
    | Ok () ->
      try
        let* mark = round_mark (Disk path) in
        let entries =
          Sys.readdir path
          |> Array.to_list
          |> List.filter (fun entry -> Filename.check_suffix entry ".vote")
        in
        let* removed =
          List.fold_left
            (fun result entry ->
              let* removed = result in
              let record = Filename.concat path entry in
              let epoch_id =
                match read record, record_position entry with
                | Ok vote, _ -> Some vote.epoch_id
                | Error _, Some position -> Some position.epoch_id
                | Error _, None -> None
              in
              match epoch_id with
              | Some value when Int64.compare value through_epoch <= 0 ->
                Unix.unlink record;
                Ok true
              | Some _
              | None -> Ok removed)
            (Ok false)
            entries
        in
        let* marked =
          match mark with
          | Some (epoch_id, _)
            when Int64.compare epoch_id through_epoch <= 0 ->
            Unix.unlink (round_mark_path path);
            Ok true
          | _ -> Ok false
        in
        if removed || marked then sync_directory path else Ok ()
      with exn ->
        Error (error exn)

let prune store ~through_epoch =
  match store with
  | Memory records ->
    Hashtbl.filter_map_inplace
      (fun _ (vote : vote) ->
        if Int64.compare vote.epoch_id through_epoch <= 0 then None
        else Some vote)
      records;
    Ok ()
  | Disk path -> prune_disk path through_epoch