(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module F = Octra_core.Set_fold

let expect name value = if not value then failwith name
let get = function Ok value -> value | Error reason -> failwith reason
let num value = `String (Int64.to_string value)
let live since = `Assoc ["kind", `String "live"; "since", num since]
let shadow pulse = `Assoc (["kind", `String "shadow"] @
  match pulse with None -> [] | Some (first, last) ->
    ["pulse", `Assoc ["first", num first; "last", num last; "count", `Int 2]])

let state ~safe_after phase marks =
  let member = `Assoc ["address", `String "owner"; "phase", phase;
    "marks", `List (List.map num marks)] in
  match F.to_yojson F.empty with
  | `Assoc fields ->
    `Assoc (List.map (function
      | "members", _ -> "members", `List [member]
      | "safe_after", _ -> "safe_after", num safe_after
      | field -> field) fields)
    |> F.of_yojson |> get
  | _ -> failwith "state shape"

let marks size = List.init size (fun index -> Int64.of_int (53 + index))
let check ?(start = 0L) ?(source = 100L) state =
  F.exclusion F.participating ~start ~source ~address:"owner" state

let test_reasons () =
  expect "missing member" (check F.empty = Some F.Member_missing);
  expect "missing pulse" (check (state ~safe_after:0L (shadow None) []) = Some F.Pulse_missing);
  List.iter (fun signed ->
    expect "participation reason"
      (check (state ~safe_after:0L (live 0L) (marks signed))
       = Some (F.Marks_short { low = 53L; high = 84L; signed; required = 16 }))) [4; 13; 15];
  expect "enough marks" (check (state ~safe_after:0L (live 0L) (marks 16)) = None);
  expect "new member" (check (state ~safe_after:0L (live 80L) []) = None);
  expect "warm interval" (check ~start:95L (state ~safe_after:0L (live 0L) []) = None);
  expect "safe interval" (check (state ~safe_after:101L (live 0L) []) = None);
  expect "old pulse" (check (state ~safe_after:0L (shadow (Some (20L, 84L))) [])
    = Some (F.Pulse_old 84L));
  expect "future pulse" (check (state ~safe_after:0L (shadow (Some (20L, 101L))) [])
    = Some (F.Pulse_future 101L));
  expect "short pulse" (check (state ~safe_after:0L (shadow (Some (40L, 96L))) [])
    = Some (F.Pulse_short { first = 40L; last = 96L }));
  expect "ready pulse" (check (state ~safe_after:0L (shadow (Some (20L, 96L))) []) = None)

let test_decision () =
  let phases = [live 0L; live 80L; shadow None;
    shadow (Some (20L, 84L)); shadow (Some (40L, 96L))] in
  List.iter (fun cfg -> List.iter (fun phase -> List.iter (fun count ->
    List.iter (fun safe_after ->
      let value = state ~safe_after phase (marks count) in
      let before = F.to_string value in
      for source = 0 to 160 do
        List.iter (fun start ->
          let source = Int64.of_int source in
          let reason = F.exclusion cfg ~start ~source ~address:"owner" value in
          expect "diagnosis differs from policy"
            (Option.is_none reason = F.allows cfg ~start ~source ~address:"owner" value);
          expect "diagnosis changed state" (F.to_string value = before)) [0L; 95L]
      done) [0L; 101L]) [0; 4; 13; 15; 16; 32]) phases)
    [F.standard; F.participating]

let () =
  test_reasons ();
  test_decision ();
  print_endline "event = test name = set_report status = passed"