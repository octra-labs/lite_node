(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module S = Octra_core.Store_irmin
module A = Octra_core.Account_pack
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

let main () =
  if Array.length Sys.argv <> 2 then
    Lwt.fail (Refused "usage: fhe_audit <data-directory>")
  else
    let path = Filename.concat Sys.argv.(1) "irmin_store" in
    if not (Sys.file_exists path && Sys.is_directory path) then
      Lwt.fail (Refused "store directory is missing")
    else
      let* store = S.open_store ~readonly:true path in
      Lwt.finalize (fun () -> scan store) (fun () -> S.close store)

let () =
  let code =
    try Lwt_main.run (main ()) with
    | Refused reason -> log "status = refused reason = %S" reason; 1
    | _ -> log "status = refused reason = store_read_failed"; 1
  in
  exit code