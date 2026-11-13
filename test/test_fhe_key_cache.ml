(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Pvac_ffi
module C = Octra_core
module Calc = C.Fhe_calc
module Image = C.Fhe_image

open Yojson.Safe.Util

let checks = ref 0
let need reason value =
  if not value then failwith reason;
  incr checks
let read path = In_channel.with_open_bin path In_channel.input_all
let hash value = Digestif.SHA256.(to_hex (digest_string value))

let input path row field =
  let raw = read (Filename.concat (Filename.dirname path) (member field row |> to_string)) in
  need "input hash differs" (hash raw = (member (field ^ "_sha256") row |> to_string));
  raw

let image = function Ok (Calc.Key value) -> value | _ -> failwith "key image missing"

let () =
  if Array.length Sys.argv <> 3 then failwith "input and output required";
  let path = Sys.argv.(1) in
  let rows = Yojson.Safe.from_file path |> member "samples" |> to_list
    |> List.filter (fun row -> member "accepted" row |> to_bool) in
  let first = List.hd rows in
  let other = List.find (fun row -> member "key" row <> member "key" first) rows in
  let raw = input path first "key" and alternate = input path other "key" in
  let cipher = input path first "cipher" in
  let expected = Calc.eval (Calc.Read_key (false, raw)) |> image in
  let parsed = ref 0 and encoded = ref 0 in
  let decode raw = incr parsed; P.deserialize_pubkey (Bytes.of_string raw) in
  let encode key = incr encoded; Image.of_key key in
  let execute = Calc.session_eval ~decode ~encode () in
  let cold = execute (Calc.Read_key (false, raw)) |> image in
  need "cold image differs" (cold = expected);
  let warm = execute (Calc.Read_key (true, raw)) |> image in
  need "repeat read did not reuse image" (cold == warm);
  need "repeat read parsed key" (!parsed = 1);
  need "repeat read encoded key" (!encoded = 1);
  let commit = Calc.Commit (raw, cipher) in
  let commitment = Calc.eval commit in
  need "cached arithmetic differs" (execute commit = commitment);
  need "cached reference differs" (execute (Calc.map_key Calc.key_ref commit) = commitment);
  need "arithmetic parsed key again" (!parsed = 1);
  need "key equality check skipped"
    (execute (Calc.Read_key (false, String.make (String.length raw) '\000')) = Error Calc.Invalid);
  need "read after different bytes differs" (image (execute (Calc.Read_key (false, raw))) = expected);
  List.iter (fun registry ->
    need "reference accepted as key" (execute (Calc.Read_key (registry, Calc.key_ref raw)) = Error Calc.Invalid))
    [false; true];
  need "invalid key accepted" (execute (Calc.Read_key (false, "invalid")) = Error Calc.Invalid);
  need "read after invalid differs" (image (execute (Calc.Read_key (false, raw))) = expected);
  let next = execute (Calc.Read_key (false, alternate)) |> image in
  need "different key reused previous image" (next <> expected);
  need "different key bytes differ" (next.data = alternate);
  let count = !parsed in
  need "second key repeat differs" (image (execute (Calc.Read_key (true, alternate))) == next);
  need "second key repeat parsed" (!parsed = count);
  need "evicted reference accepted" (execute (Calc.map_key Calc.key_ref commit) = Error Calc.Invalid);
  let count = !parsed in
  need "first key reload differs" (image (execute (Calc.Read_key (false, raw))) = expected);
  need "evicted key not parsed" (!parsed = count + 1);
  let fresh = Calc.session_eval () in
  need "new session accepted reference" (fresh (Calc.map_key Calc.key_ref commit) = Error Calc.Invalid);
  let native = P.deserialize_pubkey (Bytes.of_string raw) in
  let legacy = P.serialize_pubkey_legacy_v2 native |> Bytes.to_string in
  let historical = Calc.eval (Calc.Read_key (false, legacy)) in
  need "historical read refused" (Result.is_ok historical);
  need "historical session differs" (execute (Calc.Read_key (false, legacy)) = historical);
  need "historical registry read differs"
    (execute (Calc.Read_key (true, legacy)) = Calc.eval (Calc.Read_key (true, legacy)));
  let oversized = String.make (C.Pvac_registry.max_pubkey_bytes + 1) '\236' in
  let registry = Calc.session_eval ~decode:(fun bytes ->
    if bytes = oversized then native else failwith "unexpected size input") () in
  need "size control did not warm key" (image (registry (Calc.Read_key (false, oversized))) = expected);
  need "registry check skipped on cached key"
    (registry (Calc.Read_key (true, oversized)) = Error Calc.Invalid);
  need "registry refusal changed non-registry read"
    (image (registry (Calc.Read_key (false, oversized))) = expected);
  let no_cache = Calc.session_eval ~decode ~encode ~capacity:0 () in
  let before = !parsed, !encoded in
  for _ = 1 to 2 do
    need "uncached key image differs" (image (no_cache (Calc.Read_key (false, raw))) = expected)
  done;
  need "zero capacity retained key" (!parsed = fst before + 2);
  need "zero capacity retained image" (!encoded = snd before + 2);
  need "zero capacity accepted reference" (no_cache (Calc.map_key Calc.key_ref commit) = Error Calc.Invalid);
  need "uncached arithmetic differs" (no_cache commit = commitment);
  need "zero capacity retained arithmetic key"
    (no_cache (Calc.map_key Calc.key_ref commit) = Error Calc.Invalid);
  let size = String.length raw + P.pubkey_image_size native in
  let key_only = Calc.session_eval ~decode ~encode ~capacity:size () in
  let before = !parsed, !encoded in
  for _ = 1 to 2 do
    need "key-only image differs" (image (key_only (Calc.Read_key (false, raw))) = expected)
  done;
  need "key-only cache parsed twice" (!parsed = fst before + 1);
  need "image exceeded capacity" (!encoded = snd before + 2);
  let native_only = Calc.session_eval ~decode ~encode ~capacity:(P.pubkey_image_size native) () in
  let before = !parsed in
  for _ = 1 to 2 do
    need "large read image differs" (image (native_only (Calc.Read_key (false, raw))) = expected)
  done;
  need "read kept oversized entry" (!parsed = before + 2);
  let before = !parsed in
  need "limited arithmetic differs" (native_only commit = commitment);
  need "arithmetic cache differs from parent"
    (native_only (Calc.map_key Calc.key_ref commit) = commitment);
  need "limited arithmetic parsed twice" (!parsed = before + 1);
  let error_cases = [Out_of_memory, Calc.Memory; Stack_overflow, Calc.Stack;
    Failure "invalid key", Calc.Invalid] in
  List.iter (fun (error, expected) ->
    let run = Calc.session_eval ~decode:(fun _ -> raise error) () in
    need "decode error changed" (run (Calc.Read_key (false, raw)) = Error expected);
    let run = Calc.session_eval ~encode:(fun _ -> raise error) () in
    need "encode error changed" (run (Calc.Read_key (false, raw)) = Error expected)) error_cases;
  let result = `Assoc ["status", `String "passed"; "checks", `Int !checks;
    "proofs_generated", `Int 0; "activation_changed", `Bool false] in
  Out_channel.with_open_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 Sys.argv.(2)
    (fun channel -> output_string channel (Yojson.Safe.pretty_to_string result ^ "\n"));
  Printf.printf "event = fhe_key_cache checks = %d status = passed\n%!" !checks