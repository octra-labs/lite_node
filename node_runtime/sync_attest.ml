(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let create ~data_dir ~chain ~getenv ~set ~exit ~exit_error need =
  let stopped = ref false in
  fun ~head ~root ->
    if not !stopped then begin
      set ~head ~root;
      let result = match need with
        | None -> Ok false
        | Some need ->
          match need.Sync_need.cause with
          | Sync_need.Conflict -> Ok false
          | Sync_need.Journal -> Sync_mark.finish_journal ~data_dir ~chain ~verified_head:head need
          | Sync_need.Root ->
            let limit = Consensus_health_wiring.snapshot_policy_threshold ~getenv in
            if not (Sync_need.snapshot_ready ~head ~limit need) then Ok false else
            Result.map (fun () -> true)
              (Sync_mark.consume_root ~data_dir ~chain ~verified_head:head ~limit need)
          | Sync_need.Range ->
            let limit = Consensus_health_wiring.snapshot_policy_threshold ~getenv in
            if not (Sync_need.range_ready ~head ~limit need) then Ok false else
            Result.map (fun () -> true)
              (Sync_mark.consume_range ~data_dir ~chain ~verified_head:head ~limit need) in
      let cause = Option.fold ~none:"none" ~some:(fun need -> Sync_need.label need.Sync_need.cause) need in
      match result with
      | Ok false -> ()
      | Ok true ->
        stopped := true;
        Log.warn "init"
          "event = sync_recovery status = consumed cause = %s head = %d action = restart"
          cause head;
        exit Sync_mark.restart_code
      | Error reason ->
        stopped := true;
        Log.fatal "init"
          "event = sync_recovery status = rejected cause = %s reason = %s" cause reason;
        exit_error ()
    end