(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  read_plan : unit -> (Octra_core.Fork_repair_log.plan option, string) result;
  head : unit -> Octra_core.Head_manifest.t option;
  finality_at : int -> Octra_consensus.Finality_log.entry option;
  journal_committed : unit -> bool;
  committed_for :
    Octra_consensus.Finality_log.entry ->
    (unit, string) result;
  rewind_journal : Octra_consensus.Finality_log.entry -> (unit, string) result;
  drop_after : int -> int;
  clear : unit -> unit;
}

type outcome =
  | Idle
  | Resumed of {
      target : int;
      head : int;
      dropped : int;
    }

let head_matches head entry =
  String.equal
    head.Octra_core.Head_manifest.state_root
    entry.Octra_consensus.Finality_log.state_root
  && Int64.equal head.txid_hi entry.txid_hi

let target_entry deps plan target =
  match deps.finality_at target with
  | None -> Error "target finality entry is missing"
  | Some entry when not (String.equal entry.state_root plan.Octra_core.Fork_repair_log.target_root) ->
    Error "target finality root mismatch"
  | Some entry when entry.txid_hi <> plan.head.txid_hi ->
    Error "target finality txid mismatch"
  | Some entry -> Ok entry

let finish_target deps current target entry =
  if not (head_matches current entry) then
    Error "HEAD differs from target finality"
  else
    let rewound =
      if deps.journal_committed () then deps.rewind_journal entry
      else Ok ()
    in
    match rewound with
    | Error reason -> Error reason
    | Ok () ->
      let dropped = deps.drop_after target in
      deps.clear ();
      Ok (Resumed { target; head = target; dropped })

let finish_advanced deps current target =
  let head = current.Octra_core.Head_manifest.epoch_id in
  match deps.finality_at head with
  | None -> Error "current finality entry is missing"
  | Some entry when not (head_matches current entry) ->
    Error "HEAD differs from current finality"
  | Some entry ->
    begin
      match deps.committed_for entry with
      | Error reason -> Error reason
      | Ok () ->
        deps.clear ();
        Ok (Resumed { target; head; dropped = 0 })
    end

let run deps =
  match deps.read_plan () with
  | Error reason -> Error reason
  | Ok None -> Ok Idle
  | Ok (Some plan) ->
    let target = plan.head.Octra_core.Head_manifest.epoch_id in
    match deps.head () with
    | None -> Error "HEAD is missing"
    | Some current when current.epoch_id < target ->
      Error "HEAD is behind fork repair target"
    | Some current ->
      begin
        match target_entry deps plan target with
        | Error reason -> Error reason
        | Ok entry when current.epoch_id = target ->
          finish_target deps current target entry
        | Ok _ -> finish_advanced deps current target
      end