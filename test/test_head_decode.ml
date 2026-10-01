(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Head = Octra_core.Head_manifest

let write path text =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel text)

let run_case root name edit =
  let dir = Filename.concat root name in
  Unix.mkdir dir 0o700;
  let head, _ = Case.prepare dir in
  let path = Head.path dir in
  let json = Yojson.Safe.from_string (Head.to_json head) in
  let json = match json, edit with
    | _, None -> json
    | `Assoc rows, Some (field, value) ->
      `Assoc (List.map (fun (key, prior) -> key, if key = field then value else prior) rows)
    | _ -> assert false in
  let bytes = Yojson.Safe.to_string json in
  write path bytes;
  let before = Case.evidence dir in
  let result = Case.recover dir in
  let refused = result = Unix.WEXITED 2 in
  let preserved = Case.evidence dir = before in
  Printf.printf "event = head_decode case = %s refused = %b evidence_preserved = %b\n%!"
    name refused preserved;
  match edit with
  | None ->
    Case.expect "valid HEAD recovery failed" (result = Unix.WEXITED 0);
    Case.expect "valid HEAD changed" (Head.load dir = Some head)
  | Some _ ->
    Case.expect "invalid HEAD accepted by recovery" refused;
    Case.expect "invalid HEAD recovery changed evidence" preserved;
    Case.expect "invalid HEAD did not retain guard" (Case.Marker.recovery_required dir)

let run root =
  let failed = ref false in
  List.iter (fun (name, edit) ->
    try run_case root name edit with error ->
      failed := true;
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string error))
    ["valid", None;
     "commit_type", Some ("irmin_commit", `Int 7);
     "schema_type", Some ("schema_version", `String "broken");
     "schema_future", Some ("schema_version", `Int 999);
     "generation_negative", Some ("generation", `Int (-1))];
  if !failed then exit 1

let () = Test_workspace.with_dir "head_decode" run