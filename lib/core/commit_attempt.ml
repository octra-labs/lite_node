(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Journal = Commit_journal
module Ids = Set.Make (String)
module Rows = Map.Make (String)

type prepared = {
  commit_id : string;
  planned_txid_hi : int64;
  planned_state_root : string;
}

type phase = Waiting of prepared | Retired | Published
type t = phase Rows.t

let ( let* ) = Result.bind
let require reason condition = if condition then Ok () else Error reason

let read ~epoch ~generation journal =
  let ids = List.fold_left (fun ids -> function
    | Journal.Prepare row when row.epoch_id = epoch -> Ids.add row.commit_id ids
    | Journal.Commit row when row.generation = epoch -> Ids.add row.commit_id ids
    | _ -> ids) Ids.empty journal in
  List.fold_left (fun result record ->
    let* rows = result in
    match record with
    | Journal.Prepare row when Ids.mem row.commit_id ids ->
      let* () = require "commit attempt epoch or generation differs"
        (row.epoch_id = epoch && row.prev_generation = generation) in
      let* () = require "commit attempt identity is empty" (row.commit_id <> "") in
      let* () = require "commit attempt identity was reused" (not (Rows.mem row.commit_id rows)) in
      let* () = require "commit attempt overlaps an unfinished or published attempt"
        (Rows.for_all (fun _ -> function Retired -> true | _ -> false) rows) in
      Ok (Rows.add row.commit_id (Waiting {
        commit_id = row.commit_id; planned_txid_hi = row.planned_txid_hi;
        planned_state_root = row.planned_state_root}) rows)
    | Journal.Abort row when Ids.mem row.commit_id ids ->
      (match Rows.find_opt row.commit_id rows with
      | Some (Waiting _) | Some Retired -> Ok (Rows.add row.commit_id Retired rows)
      | Some Published -> Error "published commit attempt was aborted"
      | None -> Error "commit abort precedes prepare")
    | Journal.Commit row when Ids.mem row.commit_id ids ->
      let* () = require "commit completion generation differs" (row.generation = epoch) in
      (match Rows.find_opt row.commit_id rows with
      | Some (Waiting _) | Some Published -> Ok (Rows.add row.commit_id Published rows)
      | Some Retired -> Error "aborted commit attempt was published"
      | None -> Error "commit completion precedes prepare")
    | _ -> Ok rows) (Ok Rows.empty) journal

let active rows =
  Rows.fold (fun _ phase result ->
    let* found = result in
    match phase with
    | Retired -> Ok found
    | Published -> Error "successor has a commit record ahead of HEAD"
    | Waiting row ->
      match found with
      | None -> Ok (Some row)
      | Some _ -> Error "multiple commit attempts remain unfinished") rows (Ok None)

let retire rows =
  let* _ = active rows in
  Ok (List.map fst (Rows.bindings rows))

let initial journal =
  let* _ = List.fold_left (fun result record ->
    let* ids = result in
    match record with
    | Journal.Prepare row when row.epoch_id = 0 && row.prev_generation = -1 ->
      Ok (Ids.add row.commit_id ids)
    | Journal.Prepare _ -> Error "initial attempt epoch or generation differs"
    | Journal.Commit _ -> Error "published attempt requires HEAD"
    | Journal.Abort row when Ids.mem row.commit_id ids -> Ok ids
    | Journal.Abort _ -> Error "initial abort precedes prepare") (Ok Ids.empty) journal in
  let* attempts = read ~epoch:0 ~generation:(-1) journal in
  retire attempts