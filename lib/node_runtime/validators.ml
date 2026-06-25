(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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
    Some (if String.length raw >= 32 then String.sub raw 0 32 else raw)
  with _ -> None

let raw_pubkey_of_entry entry =
  match String.split_on_char ':' entry with
  | [_addr; pub_b64] -> raw32_of_base64 pub_b64
  | _ -> None

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