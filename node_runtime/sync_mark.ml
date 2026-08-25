(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type state =
  | Missing
  | Ready of Sync_need.t
  | Invalid of string

type write =
  | Stored
  | Present of Sync_need.t

let schema = "octra_sync_need_v1"
let max_bytes = 4096
let staged_serial = ref 0
let fields = ["cause"; "chain_id"; "epoch"; "head"; "schema"; "target"]

let dir base =
  Filename.concat base "recovery"

let path base =
  Filename.concat (dir base) "sync_need.json"

let fsync_dir target =
  let fd = Unix.openfile target [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.fsync fd)

let ensure_dir base =
  let target = dir base in
  match
    try Ok (Some (Unix.lstat target)) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
    | exn -> Error (Printexc.to_string exn)
  with
  | Error _ as error -> error
  | Ok (Some stat) ->
      if stat.Unix.st_kind = Unix.S_DIR then Ok ()
      else Error "recovery path is not a directory"
  | Ok None ->
      try
        Unix.mkdir target 0o750;
        fsync_dir base;
        Ok ()
      with exn ->
        Error (Printexc.to_string exn)

let json chain need =
  `Assoc [
    "schema", `String schema;
    "chain_id", `String chain;
    "cause", `String (Sync_need.label need.Sync_need.cause);
    "epoch", `Int need.epoch;
    "head", `Int need.head;
    "target",
      (match need.target with
       | Some target -> `Intlit (Int64.to_string target)
       | None -> `Null);
  ]

let decode chain raw =
  try
    let open Yojson.Safe.Util in
    let value = Yojson.Safe.from_string raw in
    let names =
      value
      |> to_assoc
      |> List.map fst
      |> List.sort_uniq String.compare
    in
    if names <> fields then
      Error "recovery marker fields differ"
    else if value |> member "schema" |> to_string <> schema then
      Error "recovery marker schema differs"
    else if value |> member "chain_id" |> to_string <> chain then
      Error "recovery marker chain differs"
    else
      match value |> member "cause" |> to_string |> Sync_need.cause with
      | None -> Error "recovery marker cause is invalid"
      | Some parsed ->
          let epoch = value |> member "epoch" |> to_int in
          let head = value |> member "head" |> to_int in
          let target =
            match value |> member "target" with
            | `Null -> None
            | `Int target -> Some (Int64.of_int target)
            | `Intlit target -> Some (Int64.of_string target)
            | _ -> failwith "recovery marker target is invalid"
          in
          let need = Sync_need.{ cause = parsed; epoch; head; target } in
          if Sync_need.valid need then Ok need
          else Error "recovery marker plan is invalid"
  with exn ->
    Error ("recovery marker is invalid: " ^ Printexc.to_string exn)

let rec read_all fd bytes offset =
  if offset < Bytes.length bytes then
    let count = Unix.read fd bytes offset (Bytes.length bytes - offset) in
    if count <= 0 then failwith "recovery marker ended early"
    else read_all fd bytes (offset + count)

let read ~data_dir ~chain =
  let target = path data_dir in
  match
    try Ok (Some (Unix.lstat target)) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
    | exn -> Error (Printexc.to_string exn)
  with
  | Ok None -> Missing
  | Error reason -> Invalid reason
  | Ok (Some stat) ->
      try
        if stat.Unix.st_kind <> Unix.S_REG then
          Invalid "recovery marker is not a regular file"
        else
          let fd = Unix.openfile target [Unix.O_RDONLY] 0 in
          Fun.protect
            ~finally:(fun () -> Unix.close fd)
            (fun () ->
              let held = Unix.fstat fd in
              let current = Unix.lstat target in
              if current.Unix.st_kind <> Unix.S_REG
                 || current.Unix.st_dev <> held.Unix.st_dev
                 || current.Unix.st_ino <> held.Unix.st_ino then
                Invalid "recovery marker changed during read"
              else if held.Unix.st_size <= 0 || held.Unix.st_size > max_bytes then
                Invalid "recovery marker size is invalid"
              else
                let bytes = Bytes.create held.Unix.st_size in
                read_all fd bytes 0;
                match decode chain (Bytes.unsafe_to_string bytes) with
                | Ok need -> Ready need
                | Error reason -> Invalid reason)
      with exn ->
        Invalid (Printexc.to_string exn)

let need = function
  | Missing -> Ok None
  | Ready value -> Ok (Some value)
  | Invalid reason -> Error reason

let rec write_all fd raw offset =
  if offset < String.length raw then
    let count = Unix.write_substring fd raw offset (String.length raw - offset) in
    if count <= 0 then failwith "recovery marker write returned zero"
    else write_all fd raw (offset + count)

let rec open_staged target attempt =
  if attempt >= 1024 then
    Error "recovery marker staged record limit exceeded"
  else
    let serial = !staged_serial in
    incr staged_serial;
    let staged =
      target ^ "." ^ string_of_int (Unix.getpid ()) ^ "."
      ^ string_of_int serial ^ ".staged"
    in
    try
      Ok (staged,
          Unix.openfile staged
            [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL]
            0o640)
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> open_staged target (attempt + 1)
    | exn -> Error (Printexc.to_string exn)

let write_new target raw =
  match open_staged target 0 with
  | Error _ as error -> error
  | Ok (staged, fd) ->
      try
        Fun.protect
          ~finally:(fun () -> Unix.close fd)
          (fun () ->
            write_all fd raw 0;
            Unix.fsync fd);
        let published =
          try
            Unix.link staged target;
            Ok true
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok false
          | exn -> Error (Printexc.to_string exn)
        in
        (try Unix.unlink staged with _ -> ());
        begin
          match published with
          | Ok value ->
              fsync_dir (Filename.dirname target);
              Ok value
          | Error _ as error -> error
        end
      with exn ->
        (try Unix.unlink staged with _ -> ());
        Error (Printexc.to_string exn)

let write ~data_dir ~chain need =
  if not (Sync_need.valid need) then
    Error "recovery plan is invalid"
  else match read ~data_dir ~chain with
  | Ready prior -> Ok (Present prior)
  | Invalid reason -> Error reason
  | Missing ->
      begin
        match ensure_dir data_dir with
        | Error _ as error -> error
        | Ok () ->
            try
              let raw = Yojson.Safe.to_string (json chain need) ^ "\n" in
              begin
                match write_new (path data_dir) raw with
                | Error _ as error -> error
                | Ok true -> Ok Stored
                | Ok false ->
                    match read ~data_dir ~chain with
                    | Ready prior -> Ok (Present prior)
                    | Missing -> Error "recovery marker publication disappeared"
                    | Invalid reason -> Error reason
              end
            with exn ->
              Error (Printexc.to_string exn)
      end

let rec link_consumed target attempt =
  if attempt >= 1024 then
    Error "recovery marker consumed record limit exceeded"
  else
    let serial = !staged_serial in
    incr staged_serial;
    let consumed =
      target ^ "." ^ string_of_int (Unix.getpid ()) ^ "."
      ^ string_of_int serial ^ ".consumed"
    in
    try
      Unix.link target consumed;
      Ok consumed
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> link_consumed target (attempt + 1)
    | exn -> Error (Printexc.to_string exn)

let consume ~data_dir ~chain expected =
  match read ~data_dir ~chain with
  | Missing -> Ok ()
  | Invalid reason -> Error reason
  | Ready current when not (Sync_need.equal current expected) ->
    Error "recovery marker changed before consumption"
  | Ready _ ->
    let target = path data_dir in
    begin
      match link_consumed target 0 with
      | Error _ as error -> error
      | Ok consumed ->
        try
          let linked = Unix.lstat consumed in
          let current = Unix.lstat target in
          if linked.Unix.st_kind <> Unix.S_REG
             || current.Unix.st_kind <> Unix.S_REG
             || linked.Unix.st_dev <> current.Unix.st_dev
             || linked.Unix.st_ino <> current.Unix.st_ino then
            failwith "recovery marker changed during consumption";
          let parent = Filename.dirname target in
          fsync_dir parent;
          Unix.unlink target;
          fsync_dir parent;
          (try
             Unix.unlink consumed;
             fsync_dir parent
           with _ -> ());
          Ok ()
        with exn ->
          (try Unix.unlink consumed with _ -> ());
          if Sys.file_exists target then Error (Printexc.to_string exn)
          else Ok ()
    end

let consume_journal ~data_dir ~chain ~verified_head expected =
  if expected.Sync_need.cause <> Sync_need.Journal then
    Error "only a journal recovery marker can be consumed"
  else if verified_head < 0 || verified_head = max_int then
    Error "verified journal head is invalid"
  else if expected.head <> verified_head || expected.epoch <> verified_head + 1 then
    Error "verified journal head differs from recovery marker"
  else
    consume ~data_dir ~chain expected

let consume_root ~data_dir ~chain ~verified_head expected =
  if expected.Sync_need.cause <> Sync_need.Root then
    Error "only a root recovery marker can be consumed"
  else if verified_head < expected.epoch || verified_head = max_int then
    Error "verified root head is below recovery epoch"
  else
    consume ~data_dir ~chain expected