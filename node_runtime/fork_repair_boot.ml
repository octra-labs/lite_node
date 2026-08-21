(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  read_plan : unit -> (Octra_core.Fork_repair_log.plan option, string) result;
  head_epoch : unit -> int;
  finality_at : int -> Octra_consensus.Finality_log.entry option;
  journal_committed : unit -> bool;
  rewind_journal : Octra_consensus.Finality_log.entry -> (unit, string) result;
  drop_after : int -> int;
  clear : unit -> unit;
}

type outcome =
  | Idle
  | Resumed of {
      target : int;
      dropped : int;
    }

let run deps =
  match deps.read_plan () with
  | Error reason -> Error reason
  | Ok None -> Ok Idle
  | Ok (Some plan) ->
    let target = plan.head.Octra_core.Head_manifest.epoch_id in
    if deps.head_epoch () <> target then
      Error "HEAD is not at fork repair target"
    else
      match deps.finality_at target with
      | None -> Error "target finality entry is missing"
      | Some entry when not (String.equal entry.state_root plan.target_root) ->
        Error "target finality root mismatch"
      | Some entry when entry.txid_hi <> plan.head.txid_hi ->
        Error "target finality txid boundary mismatch"
      | Some entry ->
        let rewound =
          if deps.journal_committed () then deps.rewind_journal entry
          else Ok ()
        in
        begin
          match rewound with
          | Error reason -> Error reason
          | Ok () ->
            let dropped = deps.drop_after target in
            deps.clear ();
            Ok (Resumed { target; dropped })
        end