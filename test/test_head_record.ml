(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module H = Octra_core.Head_record
module Runtime = Octra_core.Head_manifest

let expect reason valid = if not valid then failwith reason
let refused bytes = try ignore (H.of_json bytes); false with _ -> true
let current = {|{"schema_version":3,"generation":7,"epoch_id":7,"state_root":"root","ledger_state_root":"ledger","irmin_commit":"commit","txid_hi":"9","txlog_seg":0,"txlog_off":42,"epochlog_off":63,"commit_id":"first","ts":0.0,"quorum_cert_hash":null,"epoch_index_hash":"index","epoch_index_root":"epochs"}|}
let legacy = [
  {|{"generation":1,"epoch_id":1,"state_root":"deadbeef","txid_hi":"42","txlog_seg":null,"txlog_off":null,"epochlog_off":null,"commit_id":"ep1-x-y","ts":1.0}|};
  {|{"schema_version":1,"generation":1,"epoch_id":1,"state_root":"deadbeef","txid_hi":"42","txlog_seg":null,"txlog_off":null,"epochlog_off":null,"commit_id":"ep1-x-y","ts":1.0}|};
  {|{"schema_version":2,"generation":99,"epoch_id":99,"state_root":"abcd","irmin_commit":"icommit","txid_hi":"100","txlog_seg":null,"txlog_off":null,"epochlog_off":null,"commit_id":"ep99-x-y","ts":99.0}|};
  {|{"schema_version":3,"generation":99,"epoch_id":99,"state_root":"abcd","irmin_commit":"icommit","txid_hi":"100","txlog_seg":null,"txlog_off":null,"epochlog_off":null,"commit_id":"ep99-x-y","ts":99.0,"quorum_cert_hash":null}|};
]

let rows = match Yojson.Safe.from_string current with `Assoc rows -> rows | _ -> assert false
let bytes rows = Yojson.Safe.to_string (`Assoc rows)
let replace key value = List.map (fun (name, prior) -> name, if name = key then value else prior) rows |> bytes

let () =
  let head = H.of_json current in
  expect "current HEAD writer bytes changed" (H.to_json head = current);
  expect "published HEAD writer bytes differ" (Runtime.to_json (Runtime.of_json current) = current);
  List.iter (fun source ->
    let parsed = H.of_json source in
    let emitted = H.to_json parsed in
    expect "legacy HEAD conversion changed writer bytes" (emitted = Runtime.to_json (Runtime.of_json source));
    expect "legacy HEAD conversion cannot be reopened" (H.of_json emitted = parsed)) legacy;
  List.iter (fun (key, value) ->
    expect ("missing field accepted: " ^ key) (refused (bytes (List.remove_assoc key rows)));
    expect ("duplicate field accepted: " ^ key) (refused (bytes ((key, value) :: rows)));
    expect ("wrong field type accepted: " ^ key) (refused (replace key (`Bool false)))) rows;
  expect "unknown field accepted" (refused (bytes (("unknown", `Null) :: rows)));
  List.iter (fun (key, value) ->
    expect ("invalid value accepted: " ^ key) (refused (replace key value)))
    ["schema_version", `Int 0; "schema_version", `Int 999;
     "generation", `Int (-1); "epoch_id", `Int (-1); "txid_hi", `String "-2";
     "ts", `Int (-1); "txlog_seg", `Int (-1); "txlog_off", `Int (-1);
     "epochlog_off", `Int (-1); "txlog_seg", `Null; "epoch_index_hash", `Null];
  List.iter (fun key -> expect ("empty string accepted: " ^ key) (refused (replace key (`String ""))))
    ["state_root"; "ledger_state_root"; "irmin_commit"; "commit_id";
     "quorum_cert_hash"; "epoch_index_hash"; "epoch_index_root"];
  let overflow = Str.global_replace (Str.regexp_string "\"ts\":0.0") "\"ts\":1e999" current in
  expect "nonfinite timestamp accepted" (refused overflow);
  let empty = {head with H.txid_hi = -1L; txlog_off = Some 0; epochlog_off = Some 0} in
  expect "empty-store positions rejected" (H.of_json (H.to_json empty) = empty);
  Printf.printf "event = passed test = head_record legacy = %d fields = %d\n%!" (List.length legacy) (List.length rows)