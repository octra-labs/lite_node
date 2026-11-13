(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module S = Octra_core.Store_irmin

let need name value = if not value then failwith name
let epoch at = 2_000_000 + at
let json_int value = `Intlit (Int64.to_string value)
let save path value =
  let channel = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
    output_string channel (Yojson.Safe.pretty_to_string value ^ "\n"))

let rec files root path =
  Sys.readdir path |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
    let path = Filename.concat path name in
    let stat = Unix.lstat path in
    match stat.Unix.st_kind with
    | Unix.S_DIR -> files root path
    | Unix.S_REG -> [String.sub path (String.length root + 1)
        (String.length path - String.length root - 1), stat.Unix.st_size]
    | _ -> failwith "unexpected store file kind")

let allocated path =
  let channel = Unix.open_process_args_in "/usr/bin/du" [|"du"; "-sk"; path|] in
  let line = input_line channel in
  need "disk measurement failed" (Unix.close_process_in channel = Unix.WEXITED 0);
  Scanf.sscanf line "%Ld" Fun.id

let storage_path owner key = ["contracts"; owner; "storage"; key]
let key name slot =
  let slot = string_of_int slot in
  "@aml/map/" ^ name ^ "/" ^ string_of_int (String.length slot) ^ "#" ^ slot

let rows size shared generation owner =
  List.init 16 (fun slot ->
    let index = if shared then generation else generation + owner * 16 + slot in
    let data = String.make size (Char.chr (index mod 256)) in
    [key "chunks" slot, data;
     key "chunk_ids" slot, Digestif.SHA256.(to_hex (digest_string data));
     key "chunk_until" slot, "2000010"])
  |> List.concat
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let state store owners =
  Lwt_list.map_s (fun owner ->
    let* table = S.load_contract_storage store owner in
    Lwt.return (owner, S.sorted_storage_pairs table)) owners

let snapshot output name store owners =
  S.Store.flush store.S.repo;
  S.sync_branches store.store_path;
  let* values = state store owners in
  let* tree = S.Store.tree store.store in
  let* measure = S.measure_tree tree in
  let rows = List.concat_map snd values in
  let entries = files store.store_path store.store_path in
  let record = `Assoc [
    "fields", `Int (List.length rows);
    "key_bytes", `Int (List.fold_left (fun n (k, _) -> n + String.length k) 0 rows);
    "value_bytes", `Int (List.fold_left (fun n (_, v) -> n + String.length v) 0 rows);
    "tree_bytes", json_int measure.bytes; "tree_records", json_int measure.records;
    "file_bytes", `Int (List.fold_left (fun n (_, size) -> n + size) 0 entries);
    "allocated_kib", json_int (allocated store.store_path);
    "files", `Assoc (List.map (fun (name, size) -> name, `Int size) entries)] in
  save (Filename.concat output (name ^ ".json")) record;
  Lwt.return (values, record)

let commit store at write =
  let at = epoch at in
  let* () = S.begin_epoch_batch store in
  let* () = write () in
  let* () = S.write store ["measurement_epoch"] (string_of_int at) in
  let* () = S.commit_epoch_batch store "buffer storage measurement" in
  S.tag_epoch store at

let collect output store at =
  let at = epoch at in
  let started = Unix.gettimeofday () in
  let* before = S.Store.Head.get store.S.store in
  let* status = S.collect_pack_at store ~keep:9 at in
  let floor = match status with
    | S.Gc_started {floor; _} -> floor
    | _ -> failwith "pack collection did not start" in
  let* result = S.wait_pack_gc store in
  need "pack collection failed" (result = Ok ());
  let* after = S.Store.Head.get store.store in
  need "collection changed head" (S.Store.Commit.hash before = S.Store.Commit.hash after);
  save (Filename.concat output ("collect_" ^ string_of_int at ^ ".json"))
    (`Assoc ["floor", `Int floor; "seconds", `Float (Unix.gettimeofday () -. started)]);
  Lwt.return_unit

let check_epoch store at values =
  let at = epoch at in
  Lwt_list.iter_s (fun (owner, rows) ->
    Lwt_list.iter_s (fun (key, value) ->
      let* found = S.read_at_epoch store at (storage_path owner key) in
      need "retained value differs" (found = Some value);
      Lwt.return_unit) rows) values

let run output size count shared =
  need "invalid sample size" (size >= 0 && size <= 3_145_728);
  need "invalid owner count" (count > 0 && count <= 16);
  let owners = List.init count (fun index -> Printf.sprintf "owner_%02d" index) in
  let values generation = List.mapi (fun index owner ->
    owner, if size = 0 then [] else rows size shared generation index) owners in
  let write store values () = Lwt_list.iter_s (fun (owner, rows) ->
    S.save_contract_storage store owner (Hashtbl.of_seq (List.to_seq rows))) values in
  let path = Filename.concat output "store" in
  need "store already exists" (not (Sys.file_exists path));
  let* store = S.open_store ~fresh:true path in
  let* expected, final = Lwt.finalize (fun () ->
    let* () = commit store 1 (fun () -> Lwt.return_unit) in
    let* _ = snapshot output "empty" store owners in
    let first = values 0 in
    let* () = commit store 2 (write store first) in
    let* filled, record = snapshot output "filled" store owners in
    need "written values differ" (filled = first);
    let unique = List.concat_map snd first |> List.map snd |> List.sort_uniq String.compare in
    let minimum = List.fold_left (fun n value -> n + String.length value) 0 unique in
    let measured = Yojson.Safe.Util.member "tree_bytes" record in
    let measured = match measured with `Intlit value -> Int64.of_string value | _ -> assert false in
    need "unique contents not measured" (measured >= Int64.of_int minimum);
    let* split = S.collect_pack_at store ~keep:9 (epoch 2) in
    need "initial split missing" (split = S.Gc_split (epoch 2));
    let* () = commit store 3 (write store first) in
    let* repeated, _ = snapshot output "repeated" store owners in
    need "repeat changed current fields" (repeated = first);
    let* () = commit store 11 (fun () -> Lwt.return_unit) in
    let* expired, _ = snapshot output "expired" store owners in
    need "expiry removed current fields" (expired = first);
    let* () = collect output store 11 in
    let* retained, _ = snapshot output "expired_gc" store owners in
    need "collection removed current fields" (retained = first);
    let* () = check_epoch store 2 first in
    let second = values 37 in
    let* () = commit store 12 (write store second) in
    let* replaced, _ = snapshot output "replaced" store owners in
    need "replacement differs" (replaced = second);
    let* () = check_epoch store 11 first in
    let* () = commit store 20 (fun () -> Lwt.return_unit) in
    let* () = collect output store 20 in
    let* () = check_epoch store 11 first in
    let* _ = snapshot output "retained_gc" store owners in
    let cleared = List.map (fun owner -> owner, []) owners in
    let* () = commit store 21 (write store cleared) in
    let* current, _ = snapshot output "cleared" store owners in
    need "removed fields remain at head" (current = cleared);
    let* () = commit store 29 (fun () -> Lwt.return_unit) in
    let* () = collect output store 29 in
    let* () = check_epoch store 20 second in
    let* _ = snapshot output "cleared_retained_gc" store owners in
    let* () = commit store 38 (fun () -> Lwt.return_unit) in
    let* () = collect output store 38 in
    let* current, final = snapshot output "released_gc" store owners in
    need "released fields remain at head" (current = cleared);
    let* expired = S.read_at_epoch store (epoch 20) (storage_path (List.hd owners) (key "chunks" 0)) in
    need "expired epoch still readable" (expired = None);
    let* head = S.Store.Head.get store.store in
    Lwt.return (S.Store.Commit.hash head, final)
  ) (fun () -> S.close store) in
  let* store = S.open_store path in
  let* () = Lwt.finalize (fun () ->
    let* head = S.Store.Head.get store.store in
    need "reopen changed head" (S.Store.Commit.hash head = expected);
    let* values = state store owners in
    need "reopen restored removed fields" (List.for_all (fun (_, rows) -> rows = []) values);
    let* prior = S.read_at_epoch store (epoch 29) ["measurement_epoch"] in
    need "reopen lost retained epoch" (prior = Some (string_of_int (epoch 29)));
    let* _ = snapshot output "reopened" store owners in
    Lwt.return_unit) (fun () -> S.close store) in
  save (Filename.concat output "result.json") (`Assoc ["status", `String "passed";
    "payload_bytes", `Int size; "owners", `Int count; "shared_contents", `Bool shared;
    "slots_per_owner", `Int 16; "keep_test_epochs", `Int 9;
    "production_keep_epochs", `Int S.gc_keep_epochs; "final", final;
    "storage_only", `Bool true; "pool_execution", `Bool false;
    "proofs_generated", `Int 0; "production_fix", `Bool false]);
  Lwt.return_unit

let inspect output owner =
  let* store = S.open_store (Filename.concat output "store") in
  Lwt.finalize (fun () ->
    let* before = S.Store.Head.get store.store in
    let* values, _ = snapshot output "actual" store [owner] in
    let rows = List.concat_map snd values in
    let group key =
      let belongs names = List.exists (fun name ->
        String.starts_with ~prefix:("@aml/map/" ^ name ^ "/") key) names in
      if belongs ["chunks"; "chunk_ids"; "chunk_until"] then "chunks"
      else if belongs ["facts"; "fact_until"] then "certificates"
      else if belongs ["ready_leaf"; "ready_cipher"; "ready_message"; "ready_key";
        "ready_record"; "ready_point"; "ready_until"] then "prepared"
      else "other" in
    let groups = List.map (fun name ->
      let fields = List.filter (fun (key, _) -> group key = name) rows in
      name, `Assoc ["fields", `Int (List.length fields);
        "key_bytes", `Int (List.fold_left (fun n (key, _) -> n + String.length key) 0 fields);
        "value_bytes", `Int (List.fold_left (fun n (_, value) -> n + String.length value) 0 fields)])
      ["chunks"; "certificates"; "prepared"; "other"] in
    let expired = List.filter_map (fun (key, value) ->
      if String.starts_with ~prefix:"@aml/map/ready_until/" key then Some (key, `String value)
      else None) rows in
    let* after = S.Store.Head.get store.store in
    need "inspection changed head" (S.Store.Commit.hash before = S.Store.Commit.hash after);
    save (Filename.concat output "result.json") (`Assoc ["status", `String "passed";
      "groups", `Assoc groups; "ready_until", `Assoc expired; "copied_accepted_store", `Bool true;
      "proofs_generated", `Int 0; "production_fix", `Bool false]);
    Lwt.return_unit) (fun () -> S.close store)

let () =
  if Array.length Sys.argv = 4 && Sys.argv.(1) = "inspect" then
    Lwt_main.run (inspect Sys.argv.(2) Sys.argv.(3))
  else begin
    need "output size owners sharing required" (Array.length Sys.argv = 5);
    Lwt_main.run (run Sys.argv.(1) (int_of_string Sys.argv.(2))
      (int_of_string Sys.argv.(3)) (bool_of_string Sys.argv.(4)))
  end;
  print_endline "event = buffer_store status = passed"