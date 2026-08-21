(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type memory = {
  marks : (int64, C_relief.mark) Hashtbl.t;
}

type t =
  | Memory of memory
  | Disk of string

let max_wire_bytes = 262_144
let max_marks = 64

let disk ~data_dir =
  Disk (Filename.concat data_dir "relief_log")

let memory () =
  Memory { marks = Hashtbl.create 4 }

let error exn =
  Printexc.to_string exn

let hex raw =
  String.concat ""
    (List.init (String.length raw) (fun index ->
       Printf.sprintf "%02x" (Char.code raw.[index])))

let raw value =
  if String.length value <> 64 then Error "relief log hash length is invalid"
  else
    try
      Ok
        (String.init 32 (fun index ->
           Char.chr
             (int_of_string ("0x" ^ String.sub value (index * 2) 2))))
    with _ -> Error "relief log hash encoding is invalid"

let sync_json sync =
  `String (Base64.encode_exn (C_codec.encode_round_sync sync))

let encode mark =
  `Assoc [
    "standard", `String "octra-consensus-relief";
    "height", `String (Int64.to_string mark.C_relief.height);
    "round", `Int mark.round;
    "activate_epoch", `String (Int64.to_string mark.activate_epoch);
    "source_hash", `String (hex mark.source_hash);
    "target_hash", `String (hex mark.target_hash);
    "fingerprint", `String mark.fingerprint;
    "proof", `List (List.map sync_json mark.proof);
  ]
  |> Yojson.Safe.to_string

let field name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("relief log field is missing: " ^ name)

let text name = function
  | `String value -> Ok value
  | _ -> Error ("relief log field must be text: " ^ name)

let int64 name json =
  match text name json with
  | Error _ as result -> result
  | Ok value ->
    try Ok (Int64.of_string value)
    with _ -> Error ("relief log integer is invalid: " ^ name)

let int name = function
  | `Int value -> Ok value
  | _ -> Error ("relief log field must be integer: " ^ name)

let proof_of_json = function
  | `List values ->
    List.fold_left
      (fun result value ->
        match result, text "proof" value with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok proof, Ok wire ->
          try
            let wire = Base64.decode_exn wire in
            let sync = C_codec.decode_round_sync wire in
            if String.equal (C_codec.encode_round_sync sync) wire then
              Ok (sync :: proof)
            else
              Error "relief log proof wire is not exact"
          with exn -> Error (error exn))
      (Ok [])
      values
    |> Result.map List.rev
  | _ -> Error "relief log proof must be a list"

let decode wire =
  try
    match Yojson.Safe.from_string wire with
    | `Assoc fields ->
      let ( let* ) result next = Result.bind result next in
      let* standard = field "standard" fields in
      let* standard = text "standard" standard in
      if not (String.equal standard "octra-consensus-relief") then
        Error "relief log standard is invalid"
      else
        let* height = field "height" fields in
        let* height = int64 "height" height in
        let* round = field "round" fields in
        let* round = int "round" round in
        let* activate_epoch = field "activate_epoch" fields in
        let* activate_epoch = int64 "activate_epoch" activate_epoch in
        let* source_hash = field "source_hash" fields in
        let* source_hash = text "source_hash" source_hash in
        let* source_hash = raw source_hash in
        let* target_hash = field "target_hash" fields in
        let* target_hash = text "target_hash" target_hash in
        let* target_hash = raw target_hash in
        let* fingerprint = field "fingerprint" fields in
        let* fingerprint = text "fingerprint" fingerprint in
        let* proof = field "proof" fields in
        let* proof = proof_of_json proof in
        let mark = C_relief.{
          height;
          round;
          activate_epoch;
          source_hash;
          target_hash;
          fingerprint;
          proof;
        } in
        if String.equal (encode mark) wire then Ok mark
        else Error "relief log wire is not exact"
    | _ -> Error "relief log record must be an object"
  with exn -> Error (error exn)

let rec write_all fd wire offset =
  if offset < String.length wire then begin
    let count = Unix.write_substring fd wire offset (String.length wire - offset) in
    if count <= 0 then failwith "relief log write returned zero";
    write_all fd wire (offset + count)
  end

let sync_dir path =
  try
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> Unix.fsync fd);
    Ok ()
  with exn -> Error (error exn)

let rec make_dir path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR -> Ok ()
    | _ -> Error "relief log path is not a directory"
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    begin
      try
        Unix.mkdir path 0o700;
        match sync_dir (Filename.dirname path) with
        | Error _ as result -> result
        | Ok () -> sync_dir path
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> make_dir path
      | exn -> Error (error exn)
    end
  | exn -> Error (error exn)

let dir_exists path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR -> Ok true
    | _ -> Error "relief log path is not a directory"
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
      Error "relief log record is not regular"
    else if stat.Unix.st_size <= 0 || stat.Unix.st_size > max_wire_bytes then
      Error "relief log record size is invalid"
    else
      read_all path stat.Unix.st_size |> decode
  with exn -> Error (error exn)

let read_opt path =
  try
    ignore (Unix.lstat path);
    Result.map Option.some (read path)
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
  | exn -> Error (error exn)

let name height =
  Printf.sprintf "%020Ld.mark" height

let height_of_name value =
  if not (Filename.check_suffix value ".mark") then None
  else
    let raw = Filename.chop_suffix value ".mark" in
    try
      let height = Int64.of_string raw in
      if String.equal value (name height) then Some height else None
    with _ -> None

let write path wire =
  let fd =
    Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600
  in
  try
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        write_all fd wire 0;
        Unix.fsync fd)
  with exn ->
    begin
      try Unix.unlink path
      with _ -> ()
    end;
    raise exn

let stage record wire =
  let rec create attempt =
    if attempt >= max_marks then
      Error "relief log stage limit exceeded"
    else
      let staged =
        Printf.sprintf "%s.staged.%d.%d" record (Unix.getpid ()) attempt
      in
      try
        write staged wire;
        Ok staged
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> create (attempt + 1)
      | exn -> Error (error exn)
  in
  create 0

let remove_stage staged =
  try
    Unix.unlink staged;
    Ok ()
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | exn -> Error (error exn)

let finish root record staged stored =
  let linked =
    try
      Unix.link staged record;
      Ok stored
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) ->
      begin
        match read record with
        | Ok saved when C_relief.same_plan saved stored -> Ok saved
        | Ok _ -> Error "relief log plan conflicts"
        | Error _ as result -> result
      end
    | exn -> Error (error exn)
  in
  let removed = remove_stage staged in
  let synced = sync_dir root in
  match linked, removed, synced with
  | (Error _ as result), _, _ -> result
  | _, (Error _ as result), _ -> result
  | _, _, (Error _ as result) -> result
  | Ok saved, Ok (), Ok () -> Ok saved

let keep_disk root mark =
  match make_dir root with
  | Error _ as result -> result
  | Ok () ->
    let record = Filename.concat root (name mark.C_relief.height) in
    begin
      match read_opt record with
      | Error _ as result -> result
      | Ok (Some saved) ->
        if C_relief.same_plan saved mark then Ok saved
        else Error "relief log plan conflicts"
      | Ok None ->
        let wire = encode mark in
        match stage record wire with
        | Error _ as result -> result
        | Ok staged -> finish root record staged mark
    end

let keep store mark =
  match store with
  | Memory memory ->
    begin
      match Hashtbl.find_opt memory.marks mark.C_relief.height with
      | Some saved when C_relief.same_plan saved mark -> Ok saved
      | Some _ -> Error "relief log plan conflicts"
      | None ->
        if Hashtbl.length memory.marks >= max_marks then
          Error "relief log record limit exceeded"
        else begin
          Hashtbl.add memory.marks mark.height mark;
          Ok mark
        end
    end
  | Disk root -> keep_disk root mark

let latest_marks marks through_height =
  List.fold_left
    (fun best mark ->
      if Int64.compare mark.C_relief.height through_height > 0 then best
      else
        match best with
        | None -> Some mark
        | Some prior when Int64.compare mark.height prior.C_relief.height > 0 ->
          Some mark
        | Some _ -> best)
    None
    marks

let disk_marks root =
  match dir_exists root with
  | Error _ as result -> result
  | Ok false -> Ok []
  | Ok true ->
    try
      let names =
        Sys.readdir root
        |> Array.to_list
        |> List.filter_map (fun entry ->
          Option.map (fun height -> entry, height) (height_of_name entry))
      in
      if List.length names > max_marks then
        Error "relief log record limit exceeded"
      else
        List.fold_left
          (fun result (entry, height) ->
            match result with
            | Error _ as result -> result
            | Ok marks ->
              begin
                match read (Filename.concat root entry) with
                | Error _ as result -> result
                | Ok mark when Int64.equal mark.C_relief.height height ->
                  Ok (mark :: marks)
                | Ok _ -> Error "relief log name differs from record"
              end)
          (Ok [])
          names
    with exn -> Error (error exn)

let latest store ~through_height =
  match store with
  | Memory memory ->
    Hashtbl.to_seq_values memory.marks
    |> List.of_seq
    |> fun marks -> Ok (latest_marks marks through_height)
  | Disk root ->
    begin
      match disk_marks root with
      | Error _ as result -> result
      | Ok marks -> Ok (latest_marks marks through_height)
    end

let expired through_epoch mark =
  Int64.compare through_epoch (Int64.pred mark.C_relief.activate_epoch) >= 0

let prune store ~through_epoch =
  match store with
  | Memory memory ->
    Hashtbl.filter_map_inplace
      (fun _ mark -> if expired through_epoch mark then None else Some mark)
      memory.marks;
    Ok ()
  | Disk root ->
    begin
      match disk_marks root with
      | Error _ as result -> result
      | Ok marks ->
        try
          let removed = ref false in
          List.iter
            (fun mark ->
              if expired through_epoch mark then begin
                Unix.unlink (Filename.concat root (name mark.C_relief.height));
                removed := true
              end)
            marks;
          if !removed then sync_dir root else Ok ()
        with exn -> Error (error exn)
    end