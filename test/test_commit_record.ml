(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Octra_core.Commit_record

let expect reason valid = if not valid then failwith reason
let ok = function Ok row -> row | Error reason -> failwith reason

let vectors = [
  {|{"type":"PREPARE","commit_id":"genesis","prev_generation":-1,"epoch_id":0,"planned_txid_hi":"-1","planned_state_root":"root","ts":0.0}|};
  {|{"type":"PREPARE","commit_id":"next","prev_generation":7,"epoch_id":8,"planned_txid_hi":"9223372036854775807","planned_state_root":"root","ts":1.25}|};
  {|{"type":"COMMIT","commit_id":"next","generation":8,"ts":1.25}|};
  {|{"type":"ABORT","commit_id":"next","reason":"recovery_to_head","ts":0.0}|}
]

let change name value = function
  | `Assoc rows -> `Assoc (List.map (fun (key, prior) -> key, if key = name then value else prior) rows)
  | _ -> assert false

let run () =
  List.iter (fun text ->
    let json = Yojson.Safe.from_string text in
    let row = ok (R.decode json) in
    expect "record output bytes changed" (Yojson.Safe.to_string (R.record_to_json row) = text);
    let fields = match json with `Assoc rows -> rows | _ -> assert false in
    List.iter (fun (name, value) ->
      let missing = `Assoc (List.remove_assoc name fields) in
      let duplicate = `Assoc ((name, value) :: fields) in
      expect ("missing field accepted: " ^ name) (Result.is_error (R.decode missing));
      expect ("duplicate field accepted: " ^ name) (Result.is_error (R.decode duplicate))) fields;
    expect "unknown field accepted" (Result.is_error (R.decode (`Assoc (("extra", `Null) :: fields))));
    List.iter (fun (name, value) ->
      expect ("invalid field accepted: " ^ name)
        (Result.is_error (R.decode (change name value json))))
      ["type", `String "OTHER"; "commit_id", `String ""; "ts", `Float nan;
       "ts", `Float infinity; "ts", `Float neg_infinity; "ts", `Float (-1.);
       "ts", `String "0"; "commit_id", `Int 1]) vectors;
  let prepare = Yojson.Safe.from_string (List.hd vectors) in
  List.iter (fun (name, value) ->
    expect ("invalid prepare accepted: " ^ name)
      (Result.is_error (R.decode (change name value prepare))))
    ["epoch_id", `Int (-1); "prev_generation", `Int (-2);
     "planned_txid_hi", `String "-2"; "planned_txid_hi", `String "9223372036854775808";
     "planned_txid_hi", `Int 1; "planned_state_root", `String ""];
  expect "nonobject accepted" (Result.is_error (R.decode `Null));
  Printf.printf "event = passed scope = commit_record vectors = %d\n%!" (List.length vectors)

let () = run ()