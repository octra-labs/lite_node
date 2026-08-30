(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let ( let* ) result next =
  Result.bind result next

let fail reason =
  Printf.eprintf "status = refused reason = %s\n%!" reason;
  exit 1

let ready status head_epoch round =
  Printf.printf "status = %s head_epoch = %Ld round = %d\n%!"
    status
    head_epoch
    round

let args () =
  let allowed = [
    "--data-dir";
    "--chain-id";
    "--validator";
    "--validator-pub";
    "--head-epoch";
    "--round";
    "--round-min";
    "--round-sync";
  ] in
  let rec pairs fields = function
    | [] -> Ok fields
    | "--check" :: rest ->
      if List.mem_assoc "--check" fields then Error "duplicate --check"
      else pairs (("--check", "true") :: fields) rest
    | "--staged" :: rest ->
      if List.mem_assoc "--staged" fields then Error "duplicate --staged"
      else pairs (("--staged", "true") :: fields) rest
    | "--prepared" :: rest ->
      if List.mem_assoc "--prepared" fields then Error "duplicate --prepared"
      else pairs (("--prepared", "true") :: fields) rest
    | key :: value :: rest when List.mem key allowed ->
      if List.mem_assoc key fields then Error ("duplicate " ^ key)
      else pairs ((key, value) :: fields) rest
    | key :: _ when List.mem key allowed -> Error ("missing value for " ^ key)
    | key :: _ -> Error ("unsupported argument " ^ key)
  in
  let require name fields =
    match List.assoc_opt name fields with
    | Some value when String.length value > 0 -> Ok value
    | Some _ -> Error ("empty " ^ name)
    | None -> Error ("missing " ^ name)
  in
  let* fields = pairs [] (Array.to_list Sys.argv |> List.tl) in
  let* data_dir = require "--data-dir" fields in
  let* chain_id = require "--chain-id" fields in
  let* validator = require "--validator" fields in
  let* validator_pub = require "--validator-pub" fields in
  let* raw_epoch = require "--head-epoch" fields in
  let round = List.assoc_opt "--round" fields in
  let sync = List.assoc_opt "--round-sync" fields in
  let raw_min = List.assoc_opt "--round-min" fields in
  let* round_min =
    match raw_min with
    | None -> Ok 0
    | Some raw ->
      begin
        try
          let value = int_of_string raw in
          if value < 0 || value > Octra_consensus.C_codec.max_round then
            Error "round minimum is invalid"
          else Ok value
        with Failure _ -> Error "round minimum is invalid"
      end
  in
  let* source =
    match round, sync with
    | Some raw_round, None when String.length raw_round > 0 -> Ok (`Round raw_round)
    | None, Some encoded when String.length encoded > 0 -> Ok (`Sync encoded)
    | Some _, Some _ -> Error "round source is ambiguous"
    | _ -> Error "missing round source"
  in
  try
    let head_epoch = Int64.of_string raw_epoch in
    let pubkey = Base64.decode_exn validator_pub in
    let staged = List.mem_assoc "--staged" fields in
    let prepared = List.mem_assoc "--prepared" fields in
    if staged && prepared then Error "staged and prepared checks are exclusive"
    else
      Ok (data_dir, chain_id, validator, pubkey, head_epoch, source, round_min,
          List.mem_assoc "--check" fields, staged, prepared)
  with Failure _ ->
    Error "vote floor arguments are invalid"

let () =
  match args () with
  | Error reason -> fail reason
  | Ok (data_dir, chain_id, validator, pubkey, head_epoch, source, round_min,
        checking, staged, prepared) ->
    let result =
      match checking, staged, prepared, source with
      | _, true, true, _ -> Error "staged and prepared checks are exclusive"
      | true, true, false, `Round raw_round ->
        begin
          try
            Octra_node_runtime.Vote_floor.staged
              ~data_dir
              ~chain_id
              ~validator
              ~pubkey
              ~through_epoch:head_epoch
              ~round:(int_of_string raw_round)
          with Failure _ -> Error "vote floor arguments are invalid"
        end
      | true, false, true, `Round raw_round ->
        begin
          try
            Octra_node_runtime.Vote_floor.prepared
              ~data_dir
              ~chain_id
              ~validator
              ~pubkey
              ~through_epoch:head_epoch
              ~round:(int_of_string raw_round)
          with Failure _ -> Error "vote floor arguments are invalid"
        end
      | true, true, false, `Sync _ -> Error "staged check requires round"
      | true, false, true, `Sync _ -> Error "prepared check requires round"
      | false, true, false, _ -> Error "staged check requires --check"
      | false, false, true, _ -> Error "prepared check requires --check"
      | true, false, false, `Round raw_round ->
        begin
          try
            Octra_node_runtime.Vote_floor.check
              ~data_dir
              ~chain_id
              ~validator
              ~pubkey
              ~through_epoch:head_epoch
              ~round:(int_of_string raw_round)
          with Failure _ -> Error "vote floor arguments are invalid"
        end
      | false, false, false, `Round raw_round ->
        begin
          try
            Octra_node_runtime.Vote_floor.set
              ~data_dir
              ~chain_id
              ~validator
              ~pubkey
              ~through_epoch:head_epoch
              ~round:(int_of_string raw_round)
          with Failure _ -> Error "vote floor arguments are invalid"
        end
      | true, false, false, `Sync sync ->
        Octra_node_runtime.Vote_floor.check_sync
          ~data_dir
          ~chain_id
          ~validator
          ~pubkey
          ~through_epoch:head_epoch
          ~round_min
          ~sync
      | false, false, false, `Sync sync ->
        Octra_node_runtime.Vote_floor.set_sync
          ~data_dir
          ~chain_id
          ~validator
          ~pubkey
          ~through_epoch:head_epoch
          ~round_min
          ~sync
    in
    (match result with
     | Ok marked -> ready (if checking then "vote_floor_checked" else "vote_floor_ready") head_epoch marked
     | Error reason -> fail reason)