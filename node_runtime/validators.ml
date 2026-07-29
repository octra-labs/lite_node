(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let entries_of_env_name name =
  match Sys.getenv_opt name with
  | Some raw when String.length raw > 3 ->
    raw
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.filter (fun value -> String.length value > 3)
  | _ -> []

let pubkeys_of_entries entries =
  List.filter_map (fun entry ->
    match String.split_on_char ':' (String.trim entry) with
    | [addr; pub_b64] when String.length addr > 3 -> Some (addr, pub_b64)
    | _ -> None)
    entries

let pubkeys_of_env_name_in_order name =
  entries_of_env_name name
  |> pubkeys_of_entries

let raw32_of_base64 encoded =
  try
    let raw = Base64.decode_exn encoded in
    if String.length raw = 32 then Some raw else None
  with _ -> None

let parsed_entry entry =
  match String.split_on_char ':' (String.trim entry) with
  | [addr; pub_b64] when String.length addr > 3 ->
    Option.map (fun pubkey -> addr, pubkey) (raw32_of_base64 pub_b64)
  | _ -> None

let entry_errors ~label entries =
  let rec loop seen_addr seen_key errors = function
    | [] -> List.rev errors
    | entry :: rest ->
      match parsed_entry entry with
      | None ->
        loop seen_addr seen_key ((label ^ ":invalid_entry") :: errors) rest
      | Some (addr, _pubkey) when List.mem addr seen_addr ->
        loop seen_addr seen_key ((label ^ ":duplicate_address:" ^ addr) :: errors) rest
      | Some (addr, pubkey) when List.mem pubkey seen_key ->
        loop seen_addr seen_key ((label ^ ":duplicate_pubkey:" ^ addr) :: errors) rest
      | Some (addr, pubkey) ->
        loop (addr :: seen_addr) (pubkey :: seen_key) errors rest
  in
  loop [] [] [] entries

let raw_pubkey_of_entry entry =
  Option.map snd (parsed_entry entry)

let raw_pubkeys_of_entries entries =
  List.filter_map raw_pubkey_of_entry entries

let pubkeys_of_env_name name =
  pubkeys_of_env_name_in_order name
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let pubkeys_of_env ~wallet_addr ~wallet_pub =
  match pubkeys_of_env_name "OCTRA_VALIDATORS" with
  | [] -> [wallet_addr, wallet_pub]
  | parsed -> parsed

let addrs_of_env ~wallet_addr ~wallet_pub =
  pubkeys_of_env ~wallet_addr ~wallet_pub
  |> List.map fst

let activation_epoch_of_env () =
  match Sys.getenv_opt "OCTRA_VALIDATORS_ACTIVATE_EPOCH" with
  | Some raw -> (try Some (int_of_string raw) with _ -> None)
  | None ->
    match Sys.getenv_opt "OCTRA_VALIDATORS_NEXT_EPOCH" with
    | Some raw -> (try Some (int_of_string raw) with _ -> None)
    | None -> None

let activation_epoch_int64 () =
  match Sys.getenv_opt "OCTRA_VALIDATORS_ACTIVATE_EPOCH" with
  | Some raw -> (try Some (Int64.of_string raw) with _ -> None)
  | None ->
    match Sys.getenv_opt "OCTRA_VALIDATORS_NEXT_EPOCH" with
    | Some raw -> (try Some (Int64.of_string raw) with _ -> None)
    | None -> None

let pubkeys_for_epoch ~wallet_addr ~wallet_pub ~epoch =
  let current = pubkeys_of_env ~wallet_addr ~wallet_pub in
  let next = pubkeys_of_env_name "OCTRA_VALIDATORS_NEXT" in
  match next, activation_epoch_of_env () with
  | [], _ -> current
  | _, None -> current
  | parsed, Some activation when epoch >= activation -> parsed
  | _ -> current

let addrs_for_epoch ~wallet_addr ~wallet_pub ~epoch =
  pubkeys_for_epoch ~wallet_addr ~wallet_pub ~epoch
  |> List.map fst