(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


let schema = "octra_preverify_receipt_store_v1"

let dir base =
  Filename.concat base "preverify_receipts"

let ensure_dir base =
  let d = dir base in
  if Sys.file_exists d then begin
    if not (Sys.is_directory d) then
      failwith ("preverify receipt path is not a directory: " ^ d)
  end else
    Unix.mkdir d 0o750

let path base ~epoch_id =
  Filename.concat (dir base) (Printf.sprintf "epoch_%d.json" epoch_id)

let rec write_all fd s pos =
  if pos < String.length s then begin
    let n = Unix.write_substring fd s pos (String.length s - pos) in
    if n <= 0 then failwith "preverify receipt write returned zero";
    write_all fd s (pos + n)
  end

let fsync_dir base =
  let fd = Unix.openfile (dir base) [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.fsync fd)

let to_json ~epoch_id ~receipts =
  `Assoc [
    "schema", `String schema;
    "epoch_id", `Int epoch_id;
    "receipts", `List (List.map (fun r -> `String r) receipts);
  ]

let write base ~epoch_id ~receipts =
  ensure_dir base;
  let p = path base ~epoch_id in
  let tmp = Printf.sprintf "%s.tmp.%d" p (Unix.getpid ()) in
  let bytes = Yojson.Safe.to_string (to_json ~epoch_id ~receipts) ^ "\n" in
  let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      write_all fd bytes 0;
      Unix.fsync fd);
  Unix.rename tmp p;
  fsync_dir base

let read base ~epoch_id =
  let p = path base ~epoch_id in
  if not (Sys.file_exists p) then None
  else
    let json = Yojson.Safe.from_file p in
    let open Yojson.Safe.Util in
    let got_schema = json |> member "schema" |> to_string in
    let got_epoch = json |> member "epoch_id" |> to_int in
    if got_schema <> schema then
      failwith "preverify receipt store schema mismatch";
    if got_epoch <> epoch_id then
      failwith "preverify receipt store epoch mismatch";
    Some (json |> member "receipts" |> to_list |> List.map to_string)