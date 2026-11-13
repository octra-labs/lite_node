(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module S = Octra_core.Store_irmin
module A = Octra_core.Account_pack
module Head = Octra_core.Head_manifest
module Counts = Map.Make(String)

exception Refused of string

type totals = {
  accounts : int;
  rejected : int;
  clears : int;
  unsupported : int;
  review : int;
  bytes : int64;
  kinds : int Counts.t;
}

let log fmt = Printf.printf (fmt ^^ "\n%!")
let hash value = Digestif.SHA256.(digest_string value |> to_hex)
let tree_hash tree = Irmin.Type.to_string S.Store.Hash.t (S.Store.Tree.hash tree)

let kind = function
  | None -> "absent"
  | Some "" -> "empty_text"
  | Some "0" -> "zero_text"
  | Some cipher when Octra_core.Crypto.FheBalance.is_fhe_cipher cipher -> "hfhe_envelope"
  | Some _ -> "other_text"

let inspect tree addr =
  let find = S.Store.Tree.find tree in
  let* raw = find (S.account_data_path addr) in
  match raw with
  | None -> Lwt.return_error "account data is missing"
  | Some raw ->
    match A.data raw with
    | Error reason -> Lwt.return_error reason
    | Ok data ->
      let* result = S.account_from find (S.Store.Tree.find_tree tree) addr data in
      let layout = match data with A.Old _ -> "old" | A.Parts _ -> "parts" in
      Lwt.return (Result.map (fun account -> layout, hash raw, account) result)

let add tree totals (addr, subtree) =
  let id = hash addr in
  let root = tree_hash subtree in
  let* result = inspect tree addr in
  match result with
  | Error reason ->
    log "event = account account = %s record_root = %s status = rejected reason = %S"
      id root reason;
    Lwt.return { totals with
      accounts = totals.accounts + 1;
      rejected = totals.rejected + 1;
    }
  | Ok (layout, data_hash, account) ->
    let cipher = account.Octra_core.Ledger_types.encrypted_balance in
    let format = kind cipher in
    let size, digest = match cipher with
      | None -> 0, "none"
      | Some value -> String.length value, hash value
    in
    let clears = cipher = Some "0" in
    let unsupported = not (Octra_core.Ledger.can_load_cipher cipher) in
    log
      "event = account account = %s record_root = %s data_hash = %s layout = %s format = %s cipher_bytes = %d cipher_hash = %s loader_clears = %b loader_refuses = %b"
      id root data_hash layout format size digest clears unsupported;
    let count = Option.value ~default:0 (Counts.find_opt format totals.kinds) in
    Lwt.return {
      accounts = totals.accounts + 1;
      rejected = totals.rejected;
      clears = totals.clears + (if clears then 1 else 0);
      unsupported = totals.unsupported + (if unsupported then 1 else 0);
      review = totals.review + (if format = "other_text" then 1 else 0);
      bytes = Int64.add totals.bytes (Int64.of_int size);
      kinds = Counts.add format (count + 1) totals.kinds;
    }

let scan store =
  let* head = S.Store.Head.find store.S.store in
  match head with
  | None -> Lwt.fail (Refused "store head is missing")
  | Some commit ->
    let tree = S.Store.Commit.tree commit in
    log "event = audit state_root = %s commit = %s access = readonly classification = storage"
      (tree_hash tree)
      (Irmin.Type.to_string S.Store.Hash.t (S.Store.Commit.hash commit));
    let* root_kind = S.Store.Tree.kind tree ["accounts"] in
    let* () = match root_kind with
      | Some `Contents -> Lwt.fail (Refused "account root is not a directory")
      | Some `Node | None -> Lwt.return_unit
    in
    let* entries = S.Store.Tree.list tree ["accounts"] in
    let* totals = Lwt_list.fold_left_s (add tree) {
      accounts = 0; rejected = 0; clears = 0; unsupported = 0; review = 0;
      bytes = 0L; kinds = Counts.empty;
    } (List.sort (fun (a, _) (b, _) -> String.compare a b) entries) in
    Counts.iter (fun format count ->
      log "event = cipher_format format = %s accounts = %d" format count
    ) totals.kinds;
    let review = totals.rejected > 0 || totals.clears > 0
      || totals.unsupported > 0 || totals.review > 0 in
    log
      "event = audit status = complete accounts = %d rejected = %d loader_clears = %d loader_refuses = %d cipher_bytes = %Ld result = %s"
      totals.accounts totals.rejected totals.clears totals.unsupported totals.bytes
      (if review then "needs_review" else "storage_readable");
    Lwt.return (if review then 2 else 0)

let audit () =
  if Array.length Sys.argv <> 2 then
    Lwt.fail (Refused "usage: fhe_audit <data-directory>")
  else
    let path = Filename.concat Sys.argv.(1) "irmin_store" in
    if not (Sys.file_exists path && Sys.is_directory path) then
      Lwt.fail (Refused "store directory is missing")
    else
      let* store = S.open_store ~readonly:true path in
      Lwt.finalize (fun () -> scan store) (fun () -> S.close store)

type counts = { leaves : int; bytes : int64; accounts : int }
type entry = { path : string; size : int64; sha256 : string option }

let require value reason = if not value then raise (Refused reason)
let exists path =
  try ignore (Unix.lstat path); true with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false

let same_file (a : Unix.LargeFile.stats) (b : Unix.LargeFile.stats) =
  a.st_dev = b.st_dev && a.st_ino = b.st_ino && a.st_kind = b.st_kind
  && a.st_nlink = b.st_nlink && a.st_size = b.st_size
  && a.st_perm = b.st_perm && a.st_mtime = b.st_mtime && a.st_ctime = b.st_ctime

let directory path =
  let path = if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path in
  List.fold_left (fun parent name ->
    if name = "" || name = "." then parent else begin
      require (name <> "..") "parent path is not allowed";
      let next = Filename.concat parent name in
      require ((Unix.lstat next).Unix.st_kind = Unix.S_DIR) "directory is missing or linked";
      next
    end) "/" (String.split_on_char '/' path)

let regular path =
  ignore (directory (Filename.dirname path));
  let stat = Unix.LargeFile.lstat path in
  require (stat.st_kind = Unix.S_REG && stat.st_nlink = 1) "file is not independent and regular";
  stat

let with_input path f =
  let before = regular path in
  let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK] 0 in
  let channel = Unix.in_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    require (same_file before (Unix.LargeFile.fstat fd)) "opened file identity differs";
    let value = f channel before.st_size in
    require (same_file before (Unix.LargeFile.fstat fd)
      && same_file before (regular path)) "file changed during read";
    value)

let stream channel size consume =
  let buffer = Bytes.create 131072 in
  let rec loop left digest =
    if left = 0L then begin
      require (input channel buffer 0 1 = 0) "file grew during read";
      Digestif.SHA256.(get digest |> to_hex)
    end else begin
      let count = Int64.to_int (Int64.min left (Int64.of_int (Bytes.length buffer))) in
      let got = input channel buffer 0 count in
      require (got > 0) "file shortened during read";
      consume buffer got;
      let digest = Digestif.SHA256.feed_string digest (Bytes.sub_string buffer 0 got) in
      loop (Int64.sub left (Int64.of_int got)) digest
    end
  in
  loop size (Digestif.SHA256.init ())

let file_hash path = with_input path (fun channel size -> stream channel size (fun _ _ -> ()))

let read_record path limit =
  with_input path (fun channel size ->
    require (size <= Int64.of_int limit) "record size exceeds limit";
    let raw = really_input_string channel (Int64.to_int size) in
    (match input_char channel with
     | _ -> raise (Refused "record grew during read")
     | exception End_of_file -> ());
    raw)

let sync_dir path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd)

let write_new path f =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600 in
  let channel = Unix.out_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    let value = f channel in
    flush channel;
    Unix.fsync fd;
    value)

let digest_ok value =
  String.length value = 64 && String.for_all
    (function '0'..'9' | 'a'..'f' -> true | _ -> false) value

let head_read root pin =
  require (digest_ok pin) "HEAD digest is invalid";
  let raw = read_record (Head.path root) Head.max_record_bytes in
  require (hash raw = pin) "HEAD digest differs";
  let head = Head.of_json raw in
  require (Option.is_some head.irmin_commit) "HEAD commit is missing";
  require (match head.epoch_index_root, head.ledger_state_root with
    | Some index, Some ledger -> Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:ledger ~epoch_index_root:index = head.state_root
    | None, Some ledger -> ledger = head.state_root
    | None, None -> true
    | _ -> false) "HEAD roots differ";
  raw, head

let walk root name =
  let count = ref 0 in
  let rec visit depth name =
    incr count;
    require (!count <= 1_000_000 && depth <= 32 && String.length name <= 4096) "file inventory exceeds limit";
    let path = Filename.concat root name in
    let stat = Unix.LargeFile.lstat path in
    match stat.st_kind with
    | Unix.S_DIR ->
      ignore (directory path);
      let children = Sys.readdir path |> Array.to_list |> List.sort String.compare in
      (name, stat) :: List.concat_map (fun child -> visit (depth + 1) (Filename.concat name child)) children
    | Unix.S_REG -> ignore (regular path); [name, stat]
    | _ -> raise (Refused "inventory contains a linked or special file")
  in
  visit 0 name

let pvac_files root =
  let path = Filename.concat root "pvac" in
  if not (exists path) then [] else
    let rows = walk root "pvac" in
    List.iter (fun (name, (stat : Unix.LargeFile.stats)) ->
      let valid = match String.split_on_char '/' name, stat.st_kind with
        | ["pvac"], Unix.S_DIR | ["pvac"; "blobs"], Unix.S_DIR -> true
        | ["pvac"; file], Unix.S_REG -> name = Octra_core.Pvac_migration_check.state_relative_path
            || Filename.check_suffix file ".pk" || Filename.check_suffix file ".kat"
        | ["pvac"; "blobs"; file], Unix.S_REG -> Filename.check_suffix file ".pk"
        | _ -> false in
      require valid "PVAC directory contains an unsupported path") rows;
    rows

let unchanged before after =
  require (List.length before = List.length after
    && List.for_all2 (fun (a, x) (b, y) -> a = b && same_file x y) before after)
    "source file inventory changed"

let inventory root =
  let names = Sys.readdir root |> Array.to_list |> List.sort String.compare in
  require (List.for_all (fun name -> List.mem name
    ["HEAD.json"; "irmin_store"; "pvac"; "manifest.json"; "INCOMPLETE"]) names)
    "package contains an unexpected path";
  let rows = ("HEAD.json", regular (Head.path root)) :: walk root "irmin_store" @ pvac_files root in
  List.sort (fun (a, _) (b, _) -> String.compare a b) rows
  |> List.map (fun (path, (stat : Unix.LargeFile.stats)) ->
    if stat.st_kind = Unix.S_DIR then {path; size = 0L; sha256 = None}
    else {path; size = stat.st_size; sha256 = Some (file_hash (Filename.concat root path))})

let state_counts store head =
  let* found = S.Store.Head.find store.S.store in
  let commit = match found with Some value -> value | None -> raise (Refused "store head is missing") in
  require (Some (Irmin.Type.to_string S.Store.Hash.t (S.Store.Commit.hash commit)) = head.Head.irmin_commit)
    "state commit differs from HEAD";
  let tree = S.Store.Commit.tree commit in
  require (tree_hash tree = Head.ledger_state_root head) "state root differs from HEAD";
  let* epoch = S.Store.Tree.find tree ["meta"; "last_epoch"] in
  require (Option.bind epoch int_of_string_opt = Some head.epoch_id) "state epoch differs from HEAD";
  let* leaves, bytes = S.Store.Tree.fold ~force:`True ~cache:false ~uniq:`False
    ~contents:(fun _ raw (leaves, bytes) ->
      require (leaves < max_int && bytes <= Int64.sub Int64.max_int (Int64.of_int (String.length raw)))
        "tree count exceeds limit";
      Lwt.return (leaves + 1, Int64.add bytes (Int64.of_int (String.length raw)))) tree (0, 0L) in
  let* kind = S.Store.Tree.kind tree ["accounts"] in
  let* accounts = match kind with
    | Some `Node -> let* rows = S.Store.Tree.list tree ["accounts"] in Lwt.return (List.length rows)
    | _ -> Lwt.return 0 in
  Lwt.return {leaves; bytes; accounts}

let with_store root f =
  let* store = S.open_store ~readonly:true (Filename.concat root "irmin_store") in
  Lwt.finalize (fun () -> f store) (fun () -> S.close store)

let entry_json row = `Assoc [
  "path", `String row.path; "bytes", `String (Int64.to_string row.size);
  "sha256", (match row.sha256 with None -> `Null | Some value -> `String value)]

let findings root store entries =
  let files = List.fold_left (fun map row -> Counts.add row.path row map) Counts.empty entries in
  let report = ref Counts.empty in
  let add name = report := Counts.add name (1 + Option.value ~default:0 (Counts.find_opt name !report)) !report in
  let present name = match Counts.find_opt name files with Some {sha256 = Some _; _} -> true | _ -> false in
  let* tree = S.Store.tree store.S.store in
  let* refs = S.Store.Tree.find_tree tree ["pvac_hashes"] in
  let* () = match refs with
    | None -> Lwt.return_unit
    | Some refs -> S.Store.Tree.fold ~force:`True ~cache:false ~uniq:`False
        ~contents:(fun path value () ->
          (match path with
           | [_] when value = "none" -> add "explicit_no_key"
           | [_] when digest_ok value ->
             add "hashed_references";
             (match Counts.find_opt ("pvac/blobs/" ^ value ^ ".pk") files with
              | Some {sha256 = Some got; _} when got = value -> add "hashed_key_match"
              | Some {sha256 = Some _; _} -> add "hashed_key_mismatch"
              | _ -> add "hashed_key_missing")
           | _ -> add "invalid_key_reference");
          Lwt.return_unit) refs () in
  let* kind = S.Store.Tree.kind tree ["accounts"] in
  let* accounts = match kind with Some `Node -> S.Store.Tree.list tree ["accounts"] | _ -> Lwt.return [] in
  let* () = Lwt_list.iter_s (fun (addr, _) ->
    let* result = inspect tree addr in
    let* reference = S.Store.Tree.find tree ["pvac_hashes"; addr] in
    let needs_key = match result with
      | Error _ -> add "account_unreadable"; true
      | Ok (_, _, account) -> Option.is_some account.Octra_core.Ledger_types.encrypted_balance in
    if needs_key || Option.is_some reference then begin
      let safe = addr <> "" && addr <> "." && addr <> ".." && not (String.contains addr '/') && not (String.contains addr '\000') in
      if not safe then add "account_path_invalid" else begin
        (match reference with
         | None -> add (if present ("pvac/" ^ addr ^ ".pk") then "legacy_key_present" else "legacy_key_absent")
         | Some "none" when needs_key -> add "cipher_without_key_reference"
         | Some _ -> ());
        let name = "pvac/" ^ addr ^ ".kat" in
        if not (present name) then add "kat_absent" else begin
          let tag = try Some (read_record (Filename.concat root name) 4096 |> String.split_on_char '\n' |> List.hd)
            with Refused _ -> None in
          add (match tag with
            | None -> "kat_unreadable"
            | Some tag when tag = Octra_core.Pvac_registry.expected_kat () -> "kat_tag_match"
            | Some _ -> "kat_tag_mismatch")
        end
      end
    end;
    Lwt.return_unit) accounts in
  Lwt.return (`Assoc ["crypto_validity", `String "not_checked";
    "counts", `Assoc (Counts.bindings !report |> List.map (fun (name, count) -> name, `Int count))])

let manifest head_pin head counts report entries = `Assoc [
  "scope", `String "current_state_and_pvac";
  "head_sha256", `String head_pin;
  "epoch", `Int head.Head.epoch_id;
  "commit", `String (Option.get head.irmin_commit);
  "root", `String (Head.ledger_state_root head);
  "leaves", `Int counts.leaves;
  "bytes", `String (Int64.to_string counts.bytes);
  "accounts", `Int counts.accounts;
  "findings", report;
  "files", `List (List.map entry_json entries)]

let fields expected = function
  | `Assoc values when List.sort String.compare (List.map fst values) = List.sort String.compare expected -> values
  | _ -> raise (Refused "manifest fields differ or repeat")

let check_manifest value =
  let open Yojson.Safe.Util in
  let values = fields ["scope"; "head_sha256"; "epoch"; "commit"; "root"; "leaves"; "bytes"; "accounts"; "findings"; "files"] value in
  let rows = List.assoc "files" values |> to_list in
  require (List.length rows <= 1_000_000) "manifest file count exceeds limit";
  ignore (List.fold_left (fun prior row ->
    let fields = fields ["path"; "bytes"; "sha256"] row in
    let name = List.assoc "path" fields |> to_string in
    require (String.length name <= 4096 && String.compare prior name < 0) "manifest paths repeat or are unordered";
    name) "" rows);
  List.assoc "head_sha256" values |> to_string

let verify root pin =
  require (digest_ok pin) "manifest digest is invalid";
  let root = directory root in
  require (not (exists (Filename.concat root "INCOMPLETE"))) "state package is incomplete";
  let path = Filename.concat root "manifest.json" in
  let raw = read_record path (128 * 1024 * 1024) in
  require (hash raw = pin) "manifest digest differs";
  let value = Yojson.Safe.from_string raw in
  let head_pin = check_manifest value in
  let head_raw, head = head_read root head_pin in
  let before = walk root "irmin_store" @ pvac_files root in
  let* counts = with_store root (fun store -> state_counts store head) in
  let entries = inventory root in
  let* report = with_store root (fun store -> findings root store entries) in
  require (value = manifest head_pin head counts report entries) "state package manifest differs";
  unchanged before (walk root "irmin_store" @ pvac_files root);
  require (fst (head_read root head_pin) = head_raw && file_hash path = pin) "package changed during verification";
  require (not (exists (Filename.concat root "INCOMPLETE"))) "state package became incomplete";
  log "event = state_verify status = verified scope = current_state_and_pvac crypto_validity = not_checked manifest = %s leaves = %d bytes = %Ld accounts = %d"
    pin counts.leaves counts.bytes counts.accounts;
  Lwt.return 0

let export source target pin =
  let source = directory source in
  let parent = directory (Filename.dirname target) in
  let name = Filename.basename target in
  require (name <> "." && name <> ".." && name <> "") "output name is invalid";
  let target = Filename.concat parent name in
  require (not (exists target)) "output already exists";
  require (target <> source && not (String.starts_with ~prefix:(source ^ "/") target)
    && not (String.starts_with ~prefix:(target ^ "/") source)) "source and output overlap";
  let raw, head = head_read source pin in
  let head_stat = regular (Head.path source) in
  let before = walk source "irmin_store" in
  let keys = pvac_files source in
  let parent_stat = Unix.LargeFile.lstat parent in
  require (not (List.exists (fun (_, (stat : Unix.LargeFile.stats)) ->
    stat.st_kind = Unix.S_DIR && stat.st_dev = parent_stat.st_dev && stat.st_ino = parent_stat.st_ino)
    (before @ keys))) "output parent aliases source";
  let* counts = with_store source (fun store ->
    let* counts = state_counts store head in
    Unix.mkdir target 0o700;
    write_new (Filename.concat target "INCOMPLETE") (fun channel -> output_string channel "state export in progress\n");
    sync_dir target;
    sync_dir parent;
    write_new (Head.path target) (fun channel -> output_string channel raw);
    let* copied = S.create_compact_store store ~expected_commit:(Option.get head.irmin_commit)
      ~target:(Filename.concat target "irmin_store") in
    (match copied with Ok _ -> () | Error _ -> raise (Refused "raw state copy failed"));
    let copied_keys = List.map (fun (path, (stat : Unix.LargeFile.stats)) ->
      let output = Filename.concat target path in
      if stat.st_kind = Unix.S_DIR then begin
        Unix.mkdir output 0o700;
        {path; size = 0L; sha256 = None}
      end else
        let digest = with_input (Filename.concat source path) (fun channel size ->
          require (size = stat.st_size) "PVAC source size differs";
          write_new output (fun output -> stream channel size (fun buffer length -> Stdlib.output output buffer 0 length))) in
        {path; size = stat.st_size; sha256 = Some digest}) keys in
    List.rev keys |> List.iter (fun (path, (stat : Unix.LargeFile.stats)) ->
      if stat.st_kind = Unix.S_DIR then sync_dir (Filename.concat target path));
    let* restored = with_store target (fun store -> state_counts store head) in
    require (restored = counts) "restored tree counts differ";
    let entries = inventory target in
    let* report = with_store target (fun store -> findings target store entries) in
    let present = List.filter (fun row -> row.path = "pvac" || String.starts_with ~prefix:"pvac/" row.path) entries in
    require (present = List.sort (fun a b -> String.compare a.path b.path) copied_keys) "PVAC copy differs";
    let* final = state_counts store head in
    require (final = counts) "source tree counts changed";
    unchanged before (walk source "irmin_store");
    unchanged keys (pvac_files source);
    require (same_file head_stat (regular (Head.path source)) && fst (head_read source pin) = raw) "source HEAD changed";
    let manifest_path = Filename.concat target "manifest.json" in
    write_new manifest_path (fun channel -> Yojson.Safe.to_channel channel (manifest pin head counts report entries));
    require ((regular manifest_path).st_size <= Int64.of_int (128 * 1024 * 1024)) "manifest size exceeds limit";
    Lwt.return counts) in
  unchanged before (walk source "irmin_store");
  unchanged keys (pvac_files source);
  require (same_file head_stat (regular (Head.path source)) && fst (head_read source pin) = raw) "source HEAD changed";
  let manifest_pin = file_hash (Filename.concat target "manifest.json") in
  Unix.unlink (Filename.concat target "INCOMPLETE");
  sync_dir target;
  sync_dir parent;
  log "event = state_export status = verified scope = current_state_and_pvac crypto_validity = not_checked manifest = %s leaves = %d bytes = %Ld accounts = %d"
    manifest_pin counts.leaves counts.bytes counts.accounts;
  Lwt.return 0

let main () =
  match Array.to_list Sys.argv with
  | [_; "--export"; source; target; pin] -> export source target pin
  | [_; "--verify"; target; pin] -> verify target pin
  | [_; _] -> audit ()
  | _ -> Lwt.fail (Refused "usage: fhe_audit <data-directory> | --export <offline-data> <new-output> <head-sha256> | --verify <state-package> <manifest-sha256>")

let () =
  let code =
    try Lwt_main.run (main ()) with
    | Refused reason -> log "status = refused reason = %S" reason; 1
    | _ -> log "status = refused reason = store_read_failed"; 1
  in
  exit code