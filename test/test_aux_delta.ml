(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Delta = Octra_core.Aux_delta

let need = function Ok value -> value | Error reason -> failwith reason
let check name result =
  if not result then failwith name;
  Printf.printf "event = aux_delta test = %s status = pass\n%!" name

let () =
  let previous = Delta.{epoch = 4; root = String.make 64 'a'; commit_id = "four"} in
  let target = Delta.{epoch = 5; root = String.make 64 'b'; commit_id = "five"} in
  let old = Delta.Value (Some "old\000\255") in
  let fresh = Delta.Value (Some "new") in
  let before = Delta.[
    Receipt "r", Value None; Rejected "x", old;
    Address ("alice", "old"), Member true; Address ("alice", "new"), Member false;
    Epoch (5, "x"), Member false; Metadata "next_txid", Value (Some "1")
  ] in
  let after = Delta.[
    Receipt "r", fresh; Rejected "x", fresh;
    Address ("alice", "old"), Member false; Address ("alice", "new"), Member true;
    Epoch (5, "x"), Member true; Metadata "next_txid", Value (Some "2")
  ] in
  let journal = need (Delta.seal ~previous:(Some previous) ~target ~before ~after) in
  check "codec_bytes" (Delta.decode (Delta.encode journal) = Ok journal);
  check "restore_prior" (Delta.restore ~head:(Some previous) ~current:after journal = Ok (List.sort compare before));
  check "retain_commit" (Delta.decide ~head:(Some target) journal = Ok Delta.Retire);
  check "no_committed_reverse" (Result.is_error (Delta.restore ~head:(Some target) ~current:after journal));
  check "head_identity" (Result.is_error (Delta.decide ~head:(Some {previous with commit_id = "other"}) journal));
  check "head_root" (Result.is_error (Delta.decide ~head:(Some {previous with root = String.make 64 'c'}) journal));
  check "empty_head" (Result.is_error (Delta.decide ~head:None journal));
  check "changed_value" (Result.is_error (Delta.restore ~head:(Some previous) ~current:before journal));
  check "missing_key" (Result.is_error (Delta.restore ~head:(Some previous) ~current:(List.tl after) journal));
  check "duplicate_key" (Result.is_error (Delta.seal ~previous:(Some previous) ~target ~before:(List.hd before :: before) ~after));
  check "wrong_cell" (Result.is_error (Delta.seal ~previous:(Some previous) ~target
    ~before:[Delta.Receipt "r", Delta.Member false] ~after:[Delta.Receipt "r", Delta.Member true]));
  check "negative_epoch_key" (Result.is_error (Delta.seal ~previous:(Some previous) ~target
    ~before:[Delta.Epoch (-1, "r"), Delta.Member false] ~after:[Delta.Epoch (-1, "r"), Delta.Member true]));
  check "empty_key" (Result.is_error (Delta.seal ~previous:(Some previous) ~target
    ~before:[Delta.Receipt "", Delta.Value None] ~after:[Delta.Receipt "", fresh]));
  check "invalid_anchor" (Result.is_error (Delta.seal ~previous:(Some previous)
    ~target:{target with root = ""} ~before ~after));
  check "legacy_root_size" (Result.is_ok (Delta.seal
    ~previous:(Some {previous with root = String.make 128 'a'})
    ~target:{target with root = String.make 128 'b'} ~before ~after));
  check "epoch_gap" (Result.is_error (Delta.seal ~previous:(Some previous) ~target:{target with epoch = 6} ~before ~after));
  check "malformed_json" (Result.is_error (Delta.decode "{"));
  let encoded = Yojson.Safe.from_string (Delta.encode journal) in
  let replace_rows action = match encoded with
    | `List [`List [schema; prior; next; `List rows]; _] ->
      let payload = `List [schema; prior; next; `List (action rows)] in
      let digest = Digestif.SHA256.(digest_string
        ("octra_aux_delta\000" ^ Yojson.Safe.to_string payload) |> to_hex) in
      Yojson.Safe.to_string (`List [payload; `String digest])
    | _ -> failwith "test journal shape differs" in
  check "repeated_rows" (Result.is_error (Delta.decode (replace_rows (fun rows -> List.hd rows :: rows))));
  check "unordered_rows" (Result.is_error (Delta.decode (replace_rows List.rev)));
  check "unchanged_rows" (Result.is_error (Delta.decode (replace_rows (List.map (function
    | `List [key; prior; _] -> `List [key; prior; prior]
    | _ -> failwith "test row shape differs")))));
  check "initial_epoch" (Result.is_ok (Delta.seal ~previous:None ~target:{target with epoch = 0} ~before ~after));
  check "initial_gap" (Result.is_error (Delta.seal ~previous:None ~target ~before ~after));
  check "unchanged_omitted" ((need (Delta.seal ~previous:(Some previous) ~target ~before ~after:before)).rows = []);
  for seed = 0 to 255 do
    let before = List.init (1 + seed mod 31) (fun key ->
      Delta.Receipt (string_of_int key), Delta.Value (if (seed + key) mod 3 = 0
        then None else Some (string_of_int (seed - key)))) in
    let after = List.map (fun (key, _) -> key, Delta.Value (Some (string_of_int seed))) before in
    let journal = need (Delta.seal ~previous:(Some previous) ~target ~before ~after) in
    let current = List.map (fun row -> row.Delta.key, row.next) journal.rows in
    let restored = need (Delta.restore ~head:(Some previous) ~current journal) in
    let result = List.map (fun (key, value) -> key,
      match List.assoc_opt key restored with None -> value | Some value -> value) after in
    if result <> before then failwith "generated restore changed prior values"
  done;
  check "generated_restore_256" true