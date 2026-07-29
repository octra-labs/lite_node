(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Manifest = State_sync_manifest

type t = {
  path : string;
  manifest_hash : string;
  completed : (string, bool) Hashtbl.t;
}

let version = "octra-state-sync-journal"

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let protect f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let mkdir_p path =
  let rec loop current =
    if current = "" || current = "." || Sys.file_exists current then ()
    else begin
      loop (Filename.dirname current);
      Unix.mkdir current 0o755
    end
  in
  loop path

let fsync_parent path =
  let descriptor = Unix.openfile (Filename.dirname path) [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close descriptor)
    (fun () -> Unix.fsync descriptor)

let safe_child root relative =
  match Manifest.normalize_path relative with
  | Some normalized when normalized = relative -> Ok (Filename.concat root normalized)
  | _ -> Error "invalid journal file path"

let part_path path =
  path ^ ".octra-part"

let chunk_key path chunk =
  Printf.sprintf "%s|%d|%Ld|%d|%s"
    path chunk.Manifest.index chunk.offset chunk.size chunk.sha256

let exact_fields expected fields =
  let expected = List.sort String.compare expected in
  let actual = List.map fst fields |> List.sort String.compare in
  if expected = actual then Ok () else Error "unexpected journal fields"

let header_json manifest_hash =
  `Assoc [
    "version", `String version;
    "kind", `String "header";
    "manifest_hash", `String manifest_hash;
  ]

let chunk_json key =
  `Assoc [
    "version", `String version;
    "kind", `String "chunk";
    "key", `String key;
  ]

let drop_json key =
  `Assoc [
    "version", `String version;
    "kind", `String "drop";
    "key", `String key;
  ]

let append_line path json =
  mkdir_p (Filename.dirname path);
  let output =
    open_out_gen [Open_wronly; Open_creat; Open_append; Open_binary] 0o600 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () ->
      output_string output (Yojson.Safe.to_string json);
      output_char output '\n';
      flush output;
      Unix.fsync (Unix.descr_of_out_channel output));
  fsync_parent path

let parse_line line =
  if String.length line > 16_384 then Error "journal entry exceeds size limit"
  else
    let* json = protect (fun () -> Yojson.Safe.from_string line) in
    match json with
  | `Assoc fields ->
      let kind =
        match List.assoc_opt "kind" fields with
        | Some (`String value) -> Ok value
        | _ -> Error "journal kind missing"
      in
      let* kind = kind in
      if kind = "header" then
        let* () = exact_fields ["version"; "kind"; "manifest_hash"] fields in
        begin
          match List.assoc_opt "version" fields, List.assoc_opt "manifest_hash" fields with
          | Some (`String parsed_version), Some (`String manifest_hash)
            when parsed_version = version ->
              Ok (`Header manifest_hash)
          | _ -> Error "invalid journal header"
        end
      else if kind = "chunk" then
        let* () = exact_fields ["version"; "kind"; "key"] fields in
        begin
          match List.assoc_opt "version" fields, List.assoc_opt "key" fields with
          | Some (`String parsed_version), Some (`String key)
            when parsed_version = version && key <> "" ->
              Ok (`Chunk key)
          | _ -> Error "invalid journal chunk"
        end
      else if kind = "drop" then
        let* () = exact_fields ["version"; "kind"; "key"] fields in
        begin
          match List.assoc_opt "version" fields, List.assoc_opt "key" fields with
          | Some (`String parsed_version), Some (`String key)
            when parsed_version = version && key <> "" ->
              Ok (`Drop key)
          | _ -> Error "invalid journal drop"
        end
      else
        Error "invalid journal entry kind"
    | _ -> Error "journal entry must be an object"

let open_journal ~path ~manifest_hash =
  if not (Sys.file_exists path) then begin
    append_line path (header_json manifest_hash);
    Ok { path; manifest_hash; completed = Hashtbl.create 1024 }
  end else
    let* lines =
      protect (fun () ->
        let input = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in_noerr input)
          (fun () ->
            let size = in_channel_length input in
            let trailing_newline =
              if size = 0 then false
              else begin
                seek_in input (size - 1);
                input_char input = '\n'
              end
            in
            seek_in input 0;
            let rec loop entries =
              match input_line input with
              | line -> loop (line :: entries)
              | exception End_of_file -> List.rev entries, trailing_newline
            in
            loop []))
    in
    match lines with
    | [], _ -> Error "empty state sync journal"
    | header :: entries, trailing_newline ->
        let* parsed_header = parse_line header in
        begin
          match parsed_header with
          | `Header parsed_hash when parsed_hash = manifest_hash ->
              let completed = Hashtbl.create (List.length entries + 16) in
              let entry_count = List.length entries in
              let* () =
                List.fold_left (fun state (index, line) ->
                  let* () = state in
                  match parse_line line with
                  | Error _ when index = entry_count - 1 && not trailing_newline -> Ok ()
                  | Error _ as error -> error
                  | Ok (`Chunk key) ->
                      Hashtbl.replace completed key true;
                      Ok ()
                  | Ok (`Drop key) ->
                      Hashtbl.remove completed key;
                      Ok ()
                  | Ok (`Header _) -> Error "duplicate journal header"
                ) (Ok ()) (List.mapi (fun index line -> index, line) entries)
              in
              Ok { path; manifest_hash; completed }
          | `Header _ -> Error "journal manifest mismatch"
          | `Chunk _ | `Drop _ -> Error "journal header missing"
        end

let is_completed journal path chunk =
  Hashtbl.mem journal.completed (chunk_key path chunk)

let record_completed journal path chunk =
  let key = chunk_key path chunk in
  if not (Hashtbl.mem journal.completed key) then begin
    append_line journal.path (chunk_json key);
    Hashtbl.replace journal.completed key true
  end

let record_invalid journal path chunk =
  let key = chunk_key path chunk in
  if Hashtbl.mem journal.completed key then begin
    append_line journal.path (drop_json key);
    Hashtbl.remove journal.completed key
  end

let prepare_file ~stage file =
  let* destination = safe_child stage file.Manifest.path in
  mkdir_p (Filename.dirname destination);
  let partial = part_path destination in
  if Sys.file_exists destination then Ok (destination, partial, `Final)
  else
    let* () =
      protect (fun () ->
        let descriptor =
          Unix.openfile partial [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
        Fun.protect
          ~finally:(fun () -> Unix.close descriptor)
          (fun () ->
            Unix.LargeFile.ftruncate descriptor file.size;
            Unix.fsync descriptor))
    in
    Ok (destination, partial, `Partial)

let write_all descriptor bytes =
  let rec loop offset =
    if offset < Bytes.length bytes then
      let written = Unix.write descriptor bytes offset (Bytes.length bytes - offset) in
      if written <= 0 then failwith "state sync chunk write failed"
      else loop (offset + written)
  in
  loop 0

let write_chunk ~partial (chunk : Manifest.chunk) body =
  if String.length body <> chunk.Manifest.size then
    Error "chunk body length mismatch"
  else
    let actual = Digestif.SHA256.(digest_string body |> to_hex) in
    if actual <> chunk.sha256 then Error "chunk body hash mismatch"
    else
      protect (fun () ->
        let descriptor = Unix.openfile partial [Unix.O_RDWR] 0o600 in
        Fun.protect
          ~finally:(fun () -> Unix.close descriptor)
          (fun () ->
            ignore (Unix.LargeFile.lseek descriptor chunk.offset Unix.SEEK_SET);
            write_all descriptor (Bytes.unsafe_of_string body);
            Unix.fsync descriptor))

let hash_region path offset size =
  protect (fun () ->
    let input = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () ->
        LargeFile.seek_in input offset;
        let buffer = Bytes.create size in
        really_input input buffer 0 size;
        Digestif.SHA256.digest_bytes buffer |> Digestif.SHA256.to_hex))

let hash_file path =
  protect (fun () ->
    let channel = open_in_bin path in
    let buffer = Bytes.create (1024 * 1024) in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let rec loop hash =
          let read = input channel buffer 0 (Bytes.length buffer) in
          if read = 0 then Digestif.SHA256.get hash |> Digestif.SHA256.to_hex
          else loop (Digestif.SHA256.feed_bytes hash ~off:0 ~len:read buffer)
        in
        loop (Digestif.SHA256.init ())))

let verify_completed_chunk ~partial (chunk : Manifest.chunk) =
  match hash_region partial chunk.Manifest.offset chunk.size with
  | Ok hash -> Ok (hash = chunk.sha256)
  | Error _ as error -> error

let finalize_file ~destination ~partial (file : Manifest.file) =
  let candidate =
    if Sys.file_exists destination then destination else partial in
  let* hash = hash_file candidate in
  if hash <> file.Manifest.sha256 then Error "file hash mismatch"
  else if candidate = destination then Ok ()
  else
    protect (fun () ->
      Unix.rename partial destination;
      Unix.chmod destination 0o600;
      fsync_parent destination)