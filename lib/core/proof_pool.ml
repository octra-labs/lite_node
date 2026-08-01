(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Scheduler = Compute_pool

type priority = Scheduler.priority =
  | Required
  | Speculative

let capacity_of_getenv getenv =
  match getenv "OCTRA_PROOF_POOL_WORKERS" with
  | None -> 2
  | Some raw ->
    begin
      try
        let value = int_of_string raw in
        if value < 1 || value > 8 then 2 else value
      with _ -> 2
    end

let capacity = capacity_of_getenv Sys.getenv_opt

let scheduler =
  Scheduler.create
    ~capacity
    ~required_limit:Resource_lanes.preverify_required_queue_limit
    ~speculative_limit:Resource_lanes.preverify_speculative_queue_limit
    ~required_burst:Resource_lanes.preverify_required_burst
    ()

let try_run ?(priority = Required) task =
  Scheduler.run_threaded scheduler priority (fun () -> task ()) ()

let run ?(priority = Required) task =
  let open Lwt.Syntax in
  let* result = try_run ~priority task in
  match result with
  | Some value -> Lwt.return value
  | None -> Lwt.fail_with "proof_pool_queue_full"