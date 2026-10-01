(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Q = Octra_consensus.C_root_query

let expect label value = if not value then failwith label
let joined epoch state = match Q.join ~epoch state with
  | Ok value -> value | Error reason -> failwith reason
let added epoch value state = Q.add ~epoch ~same:Int.equal value state

let () =
  let first, a = joined 40L Q.empty in
  let filled = added 40L 1 first in
  let second, b = joined 40L filled in
  expect "join erased first reader replies" (Q.read a second = [1]);
  expect "join did not share prior reply" (Q.read b second = [1]);
  let second = added 40L 1 second |> added 40L 2 in
  expect "duplicate reply was counted" (Q.read a second = [2; 1]);
  let other, c = joined 41L second in
  let other = added 41L 3 other in
  expect "epochs share replies" (Q.read a other = [2; 1] && Q.read c other = [3]);
  let released = Q.leave a other in
  expect "repeat release changed query state" (Q.leave a released = released);
  expect "release retained access" (Q.read a released = []);
  expect "release removed another reader" (Q.read b released = [2; 1]);
  expect "release removed another epoch" (Q.read c released = [3]);
  let finished = Q.leave b released in
  expect "last reader retained slot" (not (Q.listening ~epoch:40L finished));
  let reopened, d = joined 40L finished in
  let reopened = added 40L 4 reopened |> Q.leave a |> Q.leave b in
  expect "old release removed new query" (Q.read d reopened = [4]);
  expect "old lease read new replies" (Q.read a reopened = [] && Q.read b reopened = []);
  let finished = Q.leave d reopened |> Q.leave c in
  expect "closed epoch accepted a reply" (not (Q.listening ~epoch:40L (added 40L 5 finished)));
  Printf.printf "event = query_pool status = pass cases = 12\n%!"