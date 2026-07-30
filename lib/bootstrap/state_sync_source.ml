(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  url : string;
  mutable ewma_ms : float;
  mutable failures : int;
  mutable successes : int;
  mutable cooldown_until : float;
}

let normalize_url raw =
  let value = String.trim raw in
  let value =
    if String.length value > 0 && value.[String.length value - 1] = '/' then
      String.sub value 0 (String.length value - 1)
    else
      value
  in
  if value = "" then Error "empty state sync source"
  else
    try
      let uri = Uri.of_string value in
      match
        Uri.scheme uri,
        Uri.host uri,
        Uri.userinfo uri,
        Uri.fragment uri,
        Uri.query uri,
        Uri.path uri
      with
      | Some scheme, Some host, None, None, [], path
        when
          (scheme = "https" || scheme = "http")
          && host <> ""
          && (path = "" || path.[0] = '/') ->
          Ok (value, scheme, String.lowercase_ascii host)
      | _, _, Some _, _, _, _ -> Error "state sync source must not contain credentials"
      | _, _, _, Some _, _, _ -> Error "state sync source must not contain a fragment"
      | _, _, _, _, _ :: _, _ -> Error "state sync source must not contain a query"
      | _ -> Error "state sync source must be an absolute HTTP URL"
    with _ -> Error "invalid state sync source URL"

let decimal_octet raw =
  let length = String.length raw in
  if length = 0
     || length > 3
     || (length > 1 && raw.[0] = '0')
     || not
          (String.for_all
             (function
               | '0' .. '9' -> true
               | _ -> false)
             raw) then
    None
  else
    match int_of_string_opt raw with
    | Some value when value <= 255 -> Some value
    | _ -> None

let ipv4_octets host =
  match String.split_on_char '.' host with
  | [a; b; c; d] ->
    begin
      match List.map decimal_octet [a; b; c; d] with
      | [Some a; Some b; Some c; Some d] -> Some [a; b; c; d]
      | _ -> None
    end
  | _ -> None

let loopback_host host =
  host = "localhost"
  || host = "::1"
  || (match ipv4_octets host with
    | Some (127 :: _) -> true
    | _ -> false)

let private_ipv4 host =
  match ipv4_octets host with
  | Some values ->
      begin
        match values with
        | 10 :: _ -> true
        | 172 :: second :: _ when second >= 16 && second <= 31 -> true
        | 192 :: 168 :: _ -> true
        | 169 :: 254 :: _ -> true
        | _ -> false
      end
  | _ -> false

let ipv6_first_hextet host =
  if not (String.contains host ':') then
    None
  else
    try
      let normalized =
        Unix.inet_addr_of_string host
        |> Unix.string_of_inet_addr
        |> String.lowercase_ascii
      in
      match String.index_opt normalized ':' with
      | Some 0 -> Some 0
      | Some index ->
        String.sub normalized 0 index
        |> fun value -> int_of_string_opt ("0x" ^ value)
      | None -> None
    with _ ->
      None

let private_ipv6 host =
  match ipv6_first_hextet host with
  | Some value ->
    value land 0xfe00 = 0xfc00
    || value land 0xffc0 = 0xfe80
  | None -> false

let private_host host =
  loopback_host host || private_ipv4 host || private_ipv6 host

let create ~allow_private_http raw =
  match normalize_url raw with
  | Error _ as error -> error
  | Ok (url, "https", _) ->
      Ok {
        url;
        ewma_ms = 0.0;
        failures = 0;
        successes = 0;
        cooldown_until = 0.0;
      }
  | Ok (url, "http", host)
    when loopback_host host || (allow_private_http && private_host host) ->
      Ok {
        url;
        ewma_ms = 0.0;
        failures = 0;
        successes = 0;
        cooldown_until = 0.0;
      }
  | Ok _ -> Error "plain HTTP state sync requires loopback or explicit private transport approval"

let rendezvous ~salt ~chunk_key source =
  let raw =
    Octra_net.Hash_domain.hash
      "octra:state_sync_source"
      (salt ^ "\x00" ^ chunk_key ^ "\x00" ^ source.url)
  in
  let value = ref 0L in
  for i = 0 to 6 do
    value := Int64.logor
      (Int64.shift_left !value 8)
      (Int64.of_int (Char.code raw.[i]))
  done;
  Int64.to_float !value

let health source =
  let reliability =
    float_of_int (source.successes + 2)
    /. float_of_int (source.successes + source.failures + 2)
  in
  let latency =
    if source.ewma_ms <= 0.0 then 1.0
    else 1.0 /. (1.0 +. (source.ewma_ms /. 5_000.0))
  in
  reliability *. latency

let rank ~salt ~chunk_key ~now ~attempted sources =
  sources
  |> List.filter (fun source ->
    source.cooldown_until <= now && not (List.mem source.url attempted))
  |> List.map (fun source ->
    source, rendezvous ~salt ~chunk_key source *. health source)
  |> List.sort (fun (_, left) (_, right) -> compare right left)
  |> List.map fst

let earliest_cooldown ~attempted sources =
  sources
  |> List.filter (fun source -> not (List.mem source.url attempted))
  |> List.map (fun source -> source.cooldown_until)
  |> List.sort compare
  |> function
  | first :: _ -> Some first
  | [] -> None

let record_success source elapsed_ms =
  source.successes <- source.successes + 1;
  source.failures <- max 0 (source.failures - 1);
  source.ewma_ms <-
    if source.ewma_ms <= 0.0 then elapsed_ms
    else (source.ewma_ms *. 0.8) +. (elapsed_ms *. 0.2);
  source.cooldown_until <- 0.0

let record_failure source now =
  source.failures <- min 16 (source.failures + 1);
  let exponent = min 6 source.failures in
  let delay = min 60.0 (0.5 *. (2. ** float_of_int exponent)) in
  source.cooldown_until <- now +. delay

let snapshot source =
  source.url, source.successes, source.failures, source.ewma_ms, source.cooldown_until