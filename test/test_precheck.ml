(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Octra_node_runtime.Consensus_proposal
module T = Octra_core.Transaction
module W = Octra_core.Preverify_worker

let need name value = if not value then failwith name

let tx ?(size = 0) ?(sender = "sender") ?(fee = 1000) nonce = T.{
  from = sender; to_ = "receiver"; amount = Z.zero; nonce;
  ou = Z.of_int fee; timestamp = 1.; signature = "test"; public_key = None;
  message = Some (String.make size 'a'); op_type = ProgramExec; encrypted_data = None;
}

let ready tx = W.{tx; receipt = None}

let run ?(ordered = true) inputs batch =
  let seen = ref [] in
  let result = Lwt_main.run (P.precheck ~ordered ~run_many:(fun items ->
    seen := items;
    Lwt.return batch) inputs) in
  need "precheck worker inputs differ"
    (!seen = if ordered then List.filter (fun tx -> tx.T.op_type <> T.CircleCall) inputs else inputs);
  result

let cases () =
  let a = tx 1 and b = tx 2 and c = tx ~sender:"another" 1 in
  let first = ready a in
  let duplicate = ready {a with signature = a.signature} in
  let batch = W.{ready = [ready b; first; duplicate; ready c]; skipped = []} in
  let result = run [a; b] batch in
  need "precheck input order differs" (List.map (fun (row : W.ready) -> row.tx) result.ready = [a; b]);
  need "precheck first match differs" (List.hd result.ready == first);
  need "precheck unexpected refusal" (result.skipped = []);
  need "precheck unordered batch differs" (run ~ordered:false [a; b] batch == batch);
  need "precheck empty result differs" (run [] W.{ready = []; skipped = []} = W.{ready = []; skipped = []});
  let result = run [a; b] W.{ready = [ready c; ready b]; skipped = []} in
  need "precheck missing entry accepted" (List.map (fun (row : W.ready) -> row.tx) result.ready = [b]);
  let denied = W.{tx = a; reason = "test unavailable"; kind = Unavailable} in
  let result = run [a; b; c] W.{ready = [ready b; ready c]; skipped = [denied]} in
  need "precheck predecessor admitted" (List.map (fun (row : W.ready) -> row.tx) result.ready = [c]);
  need "precheck refusal kind differs" (result.skipped = [denied;
    W.{tx = b; reason = "predecessor deferred"; kind = Unavailable}]);
  let expensive = tx ~fee:10_000_000 1 in
  let later = tx ~sender:"later" 1 in
  let result = run [expensive; later] W.{ready = List.map ready [later; expensive]; skipped = []} in
  need "precheck lane limit removed" (List.map (fun (row : W.ready) -> row.tx) result.ready = [expensive]);
  need "precheck lane refusal differs" (match result.skipped with
    | [{tx; reason = "lane_budget:program:ou"; kind = Deferred}] -> tx = later
    | _ -> false);
  let same_nonce = {a with message = Some "different"} in
  let result = run [a; same_nonce] W.{ready = List.map ready [same_nonce; a]; skipped = []} in
  need "precheck transaction identity differs" (List.map (fun (row : W.ready) -> row.tx) result.ready = [a; same_nonce]);
  let circle = {a with op_type = T.CircleCall; from = "circle"; message = None} in
  let result = run [circle; c] W.{ready = [ready c]; skipped = []} in
  need "precheck circle order differs" (List.map (fun (row : W.ready) -> row.tx) result.ready = [circle; c]);
  let second = run [a] W.{ready = []; skipped = []} in
  need "precheck retained old verdict" (second.ready = []);
  let delayed, wake = Lwt.wait () in
  let work = P.precheck ~ordered:true ~run_many:(fun _ -> delayed) [a] in
  need "precheck did not wait" (Lwt.is_sleeping work);
  Lwt.wakeup wake W.{ready = [first]; skipped = []};
  need "precheck delayed result differs" ((Lwt_main.run work).ready = [first]);
  let delayed, _ = Lwt.task () in
  let work = P.precheck ~ordered:true ~run_many:(fun _ -> delayed) [a] in
  Lwt.cancel work;
  need "precheck cancellation differs" (match Lwt.state work with Lwt.Fail Lwt.Canceled -> true | _ -> false)

let measure count =
  let inputs = List.init count (fun index ->
    let value = tx ~size:65_536 ~sender:(string_of_int index) (index + 1) in
    {value with op_type = T.Standard}) in
  let batch = W.{ready = List.map ready inputs; skipped = []} in
  Gc.full_major ();
  let bytes = Gc.allocated_bytes () in
  let started = Mtime_clock.counter () in
  let result = run inputs batch in
  let elapsed = Mtime.Span.to_float_ns (Mtime_clock.count started) /. 1e6 in
  let allocated = Gc.allocated_bytes () -. bytes in
  need "precheck measured result differs" (result.ready = batch.ready && result.skipped = []);
  Printf.printf "event = precheck count = %d payload_bytes = %d allocated_bytes = %.0f elapsed_ms = %.3f\n%!"
    count (count * 65_536) allocated elapsed;
  allocated

let () =
  cases ();
  let small = measure 8 in
  let large = measure 32 in
  need "precheck allocation grows quadratically" (large < small *. 6.);
  need "precheck repeats complete payload hashing" (large < 10. *. 32. *. 65_536.);
  Printf.printf "event = precheck status = passed\n%!"