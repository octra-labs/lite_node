(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax
module Range = Octra_node_runtime.Sync_range
module Http = Octra_node_runtime.State_sync_http
module Visibility = Octra_node_runtime.Epoch_visibility

let require value reason = if not value then failwith reason

let query epoch = Range.{
  from_epoch = epoch;
  max_epochs = 1;
  part = None;
  hash = None;
  head = None;
  pubkeys = [];
  activation = None;
}

let reply = Ok Range.{ body = "{}"; status = "ok"; records = 0; encoded = None }

let test_delta () =
  let state, effects = Range.delta Range.empty (Ask (1L, query 1L, 0.)) in
  require (match effects with [Range.Run job] -> job.id = 1L | _ -> false)
    "first range did not start";
  let same, repeated = Range.delta state (Ask (1L, query 1L, 0.)) in
  require (same = state && repeated = []) "duplicate id changed state";
  let state, duplicate = Range.delta state (Ask (2L, query 1L, 0.)) in
  require (duplicate = [Range.Reply (2L, Error Busy)]) "duplicate work admitted";
  let state = List.fold_left (fun state id ->
    let state, effects = Range.delta state (Ask (id, query id, 0.)) in
    require (effects = []) "queued range started early";
    state) state [3L; 4L; 5L] in
  require (Range.pending state = Range.capacity) "range capacity differs";
  let state, effects = Range.delta state (Ask (6L, query 6L, 0.)) in
  require (effects = [Range.Reply (6L, Error Busy)]) "range overload admitted";
  let same, effects = Range.delta state (Completed (1L, 1L, 0., reply)) in
  require (same = state && effects = []) "wrong generation completed";
  let same, effects = Range.delta state (Completed (3L, 0L, 0., reply)) in
  require (same = state && effects = []) "queued id completed";
  let state, _ = Range.delta state (Cancel 1L) in
  require (Range.pending state = Range.capacity) "cancel released running slot";
  let state, effects = Range.delta state (Completed (1L, 0L, 0., reply)) in
  require (match effects with [Range.Run job] -> job.id = 3L | _ -> false)
    "cancelled result was delivered";
  let state, effects = Range.delta state (Tick Range.lifetime) in
  require (Range.pending state = 1) "expiry released running slot";
  require (List.mem (Range.Interrupt 3L) effects
    && List.mem (Range.Reply (3L, Error Expired)) effects
    && List.mem (Range.Reply (4L, Error Expired)) effects
    && List.mem (Range.Reply (5L, Error Expired)) effects) "expiry control incomplete";
  let state, effects = Range.delta state (Completed (3L, 0L, Range.lifetime, reply)) in
  require (Range.pending state = 0 && effects = []) "expired result delivered";
  let state, _ = Range.delta state (Ask (7L, query 7L, 20.)) in
  let state, effects = Range.delta state Stop in
  require (List.mem (Range.Reply (7L, Error Stopped)) effects) "stop omitted reply";
  let same, effects = Range.delta state (Completed (7L, 0L, 20., reply)) in
  require (same = state && effects = []) "late result changed stopped actor";
  let _, effects = Range.delta state (Ask (8L, query 8L, 20.)) in
  require (effects = [Range.Reply (8L, Error Stopped)]) "stopped actor admitted work"

let test_runtime () =
  let first, finish = Lwt.wait () in
  let calls = ref [] in
  let interrupted = ref (fun () -> false) in
  let actor = Range.create {
    now = (fun () -> 0.);
    read = (fun ~cancelled query ->
      calls := query.Range.from_epoch :: !calls;
      if query.from_epoch = 1L then begin
        interrupted := cancelled;
        first
      end else Lwt.return reply);
  } in
  let active = Range.load actor (query 1L) in
  let queued = Range.load actor (query 2L) in
  let* duplicate = Range.load actor (query 1L) in
  require (duplicate = Error Busy) "runtime duplicate accepted";
  Lwt.cancel active;
  require ((!interrupted) ()) "runtime cancellation not delivered";
  require (!calls = [1L] && Lwt.is_sleeping queued) "cancel started concurrent reader";
  Lwt.wakeup_later finish reply;
  let* result = queued in
  require (result = reply && !calls = [2L; 1L]) "runtime queue order differs";
  let* () = Range.shutdown actor in
  let* result = Range.load actor (query 3L) in
  require (result = Error Stopped) "runtime stop accepted read";
  Lwt.return_unit

let test_cache () =
  let module Parts = Octra_bootstrap.Range_part in
  let encoded = match Parts.encode (`String (String.make Parts.body_max 'x')) with
    | Ok value -> value
    | Error reason -> failwith reason in
  let value = Ok Range.{ body = ""; status = "ok"; records = 1; encoded = Some encoded } in
  let begin_read () = fst (Range.delta Range.empty (Ask (1L, query 1L, 0.))) in
  let complete state = fst (Range.delta state (Completed (1L, 0L, 1., value))) in
  let request = { (query 1L) with part = Some 1; hash = Parts.digest encoded } in
  let cached state request now =
    match snd (Range.delta state (Ask (2L, request, now))) with
    | [Range.Run job] -> job.cached <> None
    | _ -> failwith "part did not enter actor" in
  let state = complete (begin_read ()) in
  require (cached state request 2.) "completed range was not retained";
  require (not (cached state request (1. +. Range.retention))) "range expiry ignored";
  require (not (cached state { request with hash = Some (String.make 64 '0') } 2.))
    "range hash was not checked";
  require (not (cached state { request with from_epoch = 2L } 2.))
    "range epoch was not checked";
  let cancelled = fst (Range.delta (begin_read ()) (Cancel 1L)) |> complete in
  require (not (cached cancelled request 2.)) "cancelled range was retained";
  let expired = fst (Range.delta (begin_read ()) (Tick Range.lifetime)) |> complete in
  require (not (cached expired request 2.)) "expired read was retained"

let test_stop () =
  let pending, finish = Lwt.wait () in
  let stopped = ref (fun () -> false) in
  let actor = Range.create {
    now = (fun () -> 0.);
    read = (fun ~cancelled _ -> stopped := cancelled; pending);
  } in
  let active = Range.load actor (query 1L) in
  let queued = Range.load actor (query 2L) in
  let closing = Range.shutdown actor in
  require ((!stopped) ()) "shutdown did not interrupt reader";
  let* result = active in
  let* next = queued in
  require (result = Error Stopped && next = Error Stopped) "shutdown omitted clients";
  require (Lwt.is_sleeping closing) "shutdown closed before reader drained";
  Lwt.wakeup_later finish reply;
  closing

let test_load () =
  let pending, finish = Lwt.wait () in
  let calls = ref [] in
  let actor = Range.create {
    now = (fun () -> 0.);
    read = (fun ~cancelled:_ query ->
      calls := query.Range.from_epoch :: !calls;
      if query.from_epoch = 1L then pending else Lwt.return reply);
  } in
  let requests = List.init 1024 (fun index ->
    Range.load actor (query (Int64.of_int (index + 1)))) in
  let queued, rejected = List.partition Lwt.is_sleeping requests in
  require (List.length queued = Range.capacity) "load exceeded admitted capacity";
  let* () = Lwt_list.iter_s (fun request ->
    let* result = request in
    require (result = Error Range.Busy) "load rejection differs";
    Lwt.return_unit) rejected in
  require (!calls = [1L]) "load started concurrent readers";
  Lwt.cancel (List.hd queued);
  Lwt.cancel (List.nth queued 1);
  let next = Range.load actor (query 1025L) in
  require (Lwt.is_sleeping next && !calls = [1L]) "cancel released active reader";
  Lwt.wakeup_later finish reply;
  let* results = Lwt.all [List.nth queued 2; List.nth queued 3; next] in
  require (results = [reply; reply; reply]) "admitted load lost replies";
  require (!calls = [1025L; 4L; 3L; 1L]) "load reordered readers";
  Range.shutdown actor

let test_http () =
  Unix.putenv "OCTRA_STATE_SYNC_ENABLE" "1";
  let pending, finish = Lwt.wait () in
  let calls = ref 0 in
  let actor = Range.create {
    now = (fun () -> 0.);
    read = (fun ~cancelled:_ _ -> incr calls; pending);
  } in
  let validators = Octra_consensus.C_types.make_validator_set [] in
  let request epoch = Http.handle_range ~ranges:actor ~validator_set:validators
    ["from_epoch", [epoch]; "max_epochs", ["1"]] in
  let first = request "12" in
  let* response, _ = request "12" in
  require (Cohttp.Response.status response = `Service_unavailable) "http overload status";
  require (Cohttp.Header.get (Cohttp.Response.headers response) "retry-after" = Some "2")
    "http overload retry omitted";
  let* invalid, _ = request (Int64.to_string Int64.max_int) in
  require (Cohttp.Response.status invalid = `Bad_request && !calls = 1)
    "http epoch overflow reached reader";
  let* () = Lwt_list.iter_s (fun fields ->
    let* response, _ = Http.handle_range ~ranges:actor ~validator_set:validators
      (["from_epoch", ["12"]; "max_epochs", ["1"]] @ fields) in
    require (Cohttp.Response.status response = `Bad_request && !calls = 1)
      "http invalid range hash reached reader";
    Lwt.return_unit) [
      ["sha256", [String.make 64 'a']];
      ["part", ["1"]; "sha256", [String.make 63 'a']];
      ["part", ["1"]; "sha256", [String.make 64 'z']];
    ] in
  Lwt.wakeup_later finish reply;
  let* response, body = first in
  let* body = Cohttp_lwt.Body.to_string body in
  require (Cohttp.Response.status response = `OK && body = "{}") "http encoded body changed";
  let* response, _ = Http.handle_range ~ranges:actor ~validator_set:validators [
    "from_epoch", ["12"]; "max_epochs", ["1"];
    "part", ["1"]; "sha256", [String.make 64 'a'];
  ] in
  require (Cohttp.Response.status response = `Service_unavailable && !calls = 1)
    "http unknown range hash reached reader";
  Range.shutdown actor

let test_visibility () =
  let visibility = Visibility.create () in
  let pending, finish = Lwt.wait () in
  let calls = ref 0 in
  let read () =
    incr calls;
    if !calls = 1 then pending else Lwt.return "new" in
  let request = Visibility.read visibility read in
  require (Visibility.begin_apply visibility = Ok ()) "visibility apply failed";
  require (Visibility.finish_apply visibility = Ok ()) "visibility publish failed";
  Lwt.wakeup_later finish "old";
  let* result = request in
  require (result = Some "new" && !calls = 2) "previous generation was served";
  Lwt.return_unit

let () =
  test_delta ();
  test_cache ();
  Lwt_main.run (let* () = test_runtime () in
    let* () = test_stop () in
    let* () = test_load () in
    let* () = test_http () in
    test_visibility ());
  print_endline "status = pass test = range_actor"