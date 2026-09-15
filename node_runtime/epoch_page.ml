(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc

type anchor = { chain : string; epoch : int; root : string }

type request = { start : int; limit : int; anchor : anchor option; previous : string }

type row = {
  epoch : int;
  root : string;
  previous : string;
  tx_start : int64;
  tx_count : int;
  time : float;
}

type stop = Complete | More | Gap

type page = { anchor : anchor; rows : row list; next : int; stop : stop }

let max_epoch = Int64.to_int (Int64.of_int32 Int32.max_int)
let max_count = 64
let max_record = 262_144
let max_bytes = 1_048_576

let hex value =
  (String.length value = 64 || String.length value = 128)
  && String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) value

let valid_anchor (value : anchor) =
  value.epoch >= 0 && value.epoch <= max_epoch && hex value.root
  && value.chain <> "" && String.length value.chain <= 128
  && String.for_all (fun c -> c >= '!' && c <= '~') value.chain

let validate (request : request) =
  if request.start < 0 || request.start > max_epoch then
    Error (Rpc.invalid_params "start exceeds epoch range")
  else if request.limit < 1 || request.limit > max_count then
    Error (Rpc.invalid_params "limit must be between 1 and 64")
  else if request.previous <> "" && not (hex request.previous) then
    Error (Rpc.invalid_params "previous root is invalid")
  else if Option.fold ~none:false ~some:(fun a -> not (valid_anchor a)) request.anchor then
    Error (Rpc.invalid_params "anchor is invalid")
  else Ok request

let integer = function
  | `Int value -> value
  | `String value | `Intlit value when value <> ""
      && String.for_all (function '0'..'9' -> true | _ -> false) value -> int_of_string value
  | _ -> invalid_arg "integer required"

let string = function `String value -> value | _ -> invalid_arg "string required"

let fields names = function
  | `Assoc fields when
      List.length fields = List.length (List.sort_uniq String.compare (List.map fst fields))
      && List.for_all (fun (name, _) -> List.mem name names) fields -> fields
  | _ -> invalid_arg "fields are invalid"

let required name fields = List.assoc name fields
let optional name default fields = Option.value ~default (List.assoc_opt name fields)

let anchor_of_json value =
  let fields = fields ["chain_id"; "epoch"; "state_root"] value in
  { chain = string (required "chain_id" fields);
    epoch = integer (required "epoch" fields);
    root = string (required "state_root" fields) }

let anchor_json (value : anchor) =
  `Assoc ["chain_id", `String value.chain; "epoch", `Int value.epoch;
          "state_root", `String value.root]

let request_json (value : request) =
  `Assoc ["start", `Int value.start; "limit", `Int value.limit;
          "anchor", Option.fold ~none:`Null ~some:anchor_json value.anchor;
          "previous_root", `String value.previous]

let parse value =
  try
    let fields = fields ["start"; "limit"; "anchor"; "previous_root"] value in
    validate {
      start = integer (optional "start" (`Int 0) fields);
      limit = integer (optional "limit" (`Int 32) fields);
      anchor = (match optional "anchor" `Null fields with
        | `Null -> None | value -> Some (anchor_of_json value));
      previous = string (optional "previous_root" (`String "") fields);
    }
  with Not_found | Invalid_argument _ | Failure _ ->
    Error (Rpc.invalid_params "epoch page parameters are invalid")

let row_of_json value =
  let open Yojson.Safe.Util in
  let time = match member "finalized_at" value with
    | `Float time -> time | `Int time -> float_of_int time
    | _ -> invalid_arg "epoch time is invalid"
  in
  let row = {
    epoch = integer (member "id" value);
    root = string (member "state_root" value);
    previous = string (member "prev_state_root" value);
    tx_start = Int64.of_string (string (member "start_txid" value));
    tx_count = integer (member "tx_count" value);
    time;
  } in
  if row.epoch < 0 || row.epoch > max_epoch || not (hex row.root)
     || (not (hex row.previous) && not (row.epoch = 0 && row.previous = ""))
     || row.tx_start < 0L || row.tx_count < 0
     || Int64.of_int row.tx_count > Int64.sub Int64.max_int row.tx_start
     || not (Float.is_finite row.time) || row.time < 0.
  then invalid_arg "epoch row is invalid";
  row

let row_json (row : row) =
  `Assoc ["id", `Int row.epoch; "state_root", `String row.root;
          "prev_state_root", `String row.previous;
          "start_txid", `String (Int64.to_string row.tx_start);
          "tx_count", `Int row.tx_count; "finalized_at", `Float row.time]

let stop_text = function Complete -> "complete" | More -> "more" | Gap -> "gap"

let json (page : page) =
  `Assoc ["anchor", anchor_json page.anchor; "epochs", `List (List.map row_json page.rows);
          "next_epoch", `Int page.next; "stop", `String (stop_text page.stop)]

let continuous rows next =
  let rec loop = function
    | [] -> true
    | [row] -> row.epoch + 1 = next
    | row :: ((following :: _) as rest) ->
      row.epoch + 1 = following.epoch && row.root = following.previous
      && Int64.add row.tx_start (Int64.of_int row.tx_count) = following.tx_start
      && loop rest
  in loop rows

let of_json value =
  try
    let open Yojson.Safe.Util in
    let anchor = anchor_of_json (member "anchor" value) in
    let rows = member "epochs" value |> to_list |> List.map row_of_json in
    let next = integer (member "next_epoch" value) in
    let stop = match member "stop" value with
      | `String "complete" -> Complete | `String "more" -> More
      | `String "gap" -> Gap | _ -> invalid_arg "page stop is invalid"
    in
    if not (valid_anchor anchor) || List.length rows > max_count
       || next < 0 || next > anchor.epoch + 1 || not (continuous rows next)
       || (match stop with
         | Complete -> next <> anchor.epoch + 1
             || (match List.rev rows with row :: _ -> row.root <> anchor.root | [] -> false)
         | More -> next > anchor.epoch || rows = []
         | Gap -> next > anchor.epoch)
    then invalid_arg "page is invalid";
    Ok { anchor; rows; next; stop }
  with Not_found | Invalid_argument _ | Failure _ | Yojson.Safe.Util.Type_error _ ->
    Error "epoch page response is invalid"

let unavailable message = Rpc.err (-32012) message None

let admit lock run =
  if Lwt_mutex.is_locked lock then
    Lwt.return (Error (Rpc.err 107 "epoch page reader is busy; retry" None))
  else Lwt_mutex.with_lock lock run

let read ~head ~load (request : request) =
  let open Lwt.Syntax in
  let finish value = Lwt.return value in
  let fetch budget epoch =
    let* () = Lwt.pause () in
    match load ~max_bytes:(min max_record budget) epoch with
    | Error error -> finish (Error error)
    | Ok None -> finish (Ok (None, budget))
    | Ok (Some raw) when String.length raw > min max_record budget ->
      finish (Error (Rpc.err 107 "epoch record exceeds read limit" None))
    | Ok (Some raw) ->
      let row = try
        let row = row_of_json (Yojson.Safe.from_string raw) in
        if row.epoch <> epoch then invalid_arg "epoch id differs";
        Ok (Some row, budget - String.length raw)
      with Invalid_argument _ | Failure _ | Yojson.Json_error _
         | Yojson.Safe.Util.Type_error _ -> Error (unavailable "epoch record is inconsistent")
      in finish row
  in
  match validate request, head with
  | Error error, _ -> finish (Error error)
  | _, None -> finish (Error (unavailable "committed head is unavailable"))
  | Ok _, Some head when not (valid_anchor head) ->
    finish (Error (unavailable "committed head is invalid"))
  | Ok request, Some head ->
    let anchor = Option.value ~default:head request.anchor in
    if anchor.chain <> head.chain || anchor.epoch > head.epoch
       || request.start > anchor.epoch then
      finish (Error (Rpc.invalid_params "anchor or start differs from committed history"))
    else
      let* checked =
        if anchor.epoch = head.epoch then
          finish (if anchor.root = head.root then Ok max_bytes
            else Error (unavailable "anchor root differs"))
        else
          let* found = fetch max_bytes anchor.epoch in
          finish (match found with
            | Ok (Some row, budget) when row.root = anchor.root -> Ok budget
            | Error error -> Error error
            | _ -> Error (unavailable "anchor is not available in local history"))
      in
      match checked with
      | Error error -> finish (Error error)
      | Ok budget ->
        let rec loop epoch remaining budget previous tx_start rows =
          let done_ stop = finish (Ok { anchor; rows = List.rev rows; next = epoch; stop }) in
          if epoch > anchor.epoch then done_ Complete
          else if remaining = 0 then done_ More
          else
            let* found = fetch budget epoch in
            match found with
            | Error error -> finish (Error error)
            | Ok (None, _) -> done_ Gap
            | Ok (Some row, budget) ->
              if (previous <> "" && row.previous <> previous)
                 || Option.fold ~none:false ~some:((<>) row.tx_start) tx_start
                 || (epoch = anchor.epoch && row.root <> anchor.root) then
                finish (Error (unavailable "epoch chain is inconsistent"))
              else loop (epoch + 1) (remaining - 1) budget row.root
                (Some (Int64.add row.tx_start (Int64.of_int row.tx_count))) (row :: rows)
        in
        loop request.start request.limit budget request.previous None []