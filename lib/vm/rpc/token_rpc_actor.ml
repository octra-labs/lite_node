(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Policy = Token_rpc_policy
module Store = Octra_core.Store_irmin
module String_map = Map.Make (String)

type error =
  | Busy
  | Stopped
  | Read_failed

type stats = {
  cache_entries : int;
  cache_bytes : int;
  queued : int;
  generation : int64;
}

type meta_cache = {
  expires_at : float;
  programs : Policy.meta list;
  limited : bool;
}

type cache_entry = {
  expires_at : float;
  touched_at : float;
  bytes : int;
  payload : Yojson.Safe.t;
}

type state = {
  meta : meta_cache option;
  pages : cache_entry String_map.t;
  page_bytes : int;
}

type query = {
  holder : string;
  offset : int;
  limit : int;
}

type message =
  | Query of query
  | Read_stats
  | Stop

type reply =
  | Query_reply of (Yojson.Safe.t, error) result
  | Stats_reply of stats
  | Stop_reply

type command = {
  generation : int64;
  correlation : string;
  accepted_at : float;
  message : message;
  promise : reply Lwt.t;
  reply : reply Lwt.u;
}

type t = {
  store : Store.t;
  mutable state : state;
  data_channel : command Queue.t;
  control_channel : command Queue.t;
  channel_ready : unit Lwt_condition.t;
  mutable channel_open : bool;
  mutable generation : int64;
}

let metadata_ttl = 60.0
let page_ttl = 10.0
let request_ttl = 5.0
let cache_entry_limit = 128
let cache_byte_limit = 4 * 1_024 * 1_024
let data_channel_limit = 16
let control_channel_limit = 2

let data_admitted ~queued =
  queued >= 0 && queued < data_channel_limit

let empty_state = {
  meta = None;
  pages = String_map.empty;
  page_bytes = 0;
}

let cache_key query =
  Printf.sprintf "%s:%d:%d" query.holder query.offset query.limit

let command_key generation = function
  | Query query ->
    Printf.sprintf "%Ld:%s" generation (cache_key query)
  | Read_stats ->
    Printf.sprintf "%Ld:stats" generation
  | Stop ->
    Printf.sprintf "%Ld:stop" generation

let prune_pages now state =
  let pages, page_bytes =
    String_map.fold
      (fun key entry (pages, bytes) ->
        if entry.expires_at > now then
          String_map.add key entry pages, bytes + entry.bytes
        else
          pages, bytes)
      state.pages
      (String_map.empty, 0)
  in
  { state with pages; page_bytes }

let oldest_key pages =
  String_map.fold
    (fun key entry oldest ->
      match oldest with
      | None -> Some (key, entry.touched_at)
      | Some (_, touched_at) when entry.touched_at < touched_at ->
        Some (key, entry.touched_at)
      | Some _ -> oldest)
    pages
    None
  |> Option.map fst

let remove_page key state =
  match String_map.find_opt key state.pages with
  | None -> state
  | Some entry ->
    {
      state with
      pages = String_map.remove key state.pages;
      page_bytes = max 0 (state.page_bytes - entry.bytes);
    }

let rec reserve_page incoming state =
  if
    String_map.cardinal state.pages < cache_entry_limit
    && state.page_bytes + incoming <= cache_byte_limit
  then
    state
  else
    match oldest_key state.pages with
    | None -> { state with page_bytes = 0 }
    | Some key -> reserve_page incoming (remove_page key state)

let insert_page now key payload state =
  let bytes = Policy.page_bytes payload in
  let state = remove_page key (prune_pages now state) in
  if bytes > Policy.max_page_bytes || bytes > cache_byte_limit then state
  else
    let state = reserve_page bytes state in
    let entry = {
      expires_at = now +. page_ttl;
      touched_at = now;
      bytes;
      payload;
    } in
    {
      state with
      pages = String_map.add key entry state.pages;
      page_bytes = state.page_bytes + bytes;
    }

let find_page now key state =
  let state = prune_pages now state in
  match String_map.find_opt key state.pages with
  | None -> None, state
  | Some entry ->
    let touched = { entry with touched_at = now } in
    Some entry.payload, {
      state with pages = String_map.add key touched state.pages;
    }

let take count values =
  let rec loop left acc = function
    | _ when left <= 0 -> List.rev acc
    | [] -> List.rev acc
    | value :: rest -> loop (left - 1) (value :: acc) rest
  in
  loop count [] values

let read_meta store address =
  let open Lwt.Syntax in
  let* symbol = Store.read_contract_storage_key store address "symbol" in
  match symbol with
  | None -> Lwt.return_none
  | Some symbol ->
    let symbol =
      if String.length symbol <= 32 then symbol else String.sub symbol 0 32
    in
    let* name = Store.read_contract_storage_key store address "name" in
    let name =
      Option.map
        (fun value ->
          if String.length value <= 64 then value else String.sub value 0 64)
        name
    in
    let* decimals = Store.read_contract_storage_key store address "decimals" in
    begin
      match decimals with
      | Some value when String.length value > 2 -> Lwt.return_none
      | _ ->
        let* total_supply =
          Store.read_contract_storage_key store address "total_supply"
        in
        begin
          match total_supply with
          | Some value when String.length value > 39 -> Lwt.return_none
          | _ ->
            let* info = Store.get_contract_info store address in
            let owner =
              match info with
              | Some (_, _, _, owner) -> owner
              | None -> ""
            in
            Lwt.return
              (Policy.meta
                 ~address
                 ~owner
                 ~symbol
                 ~name
                 ~decimals
                 ~total_supply)
        end
    end

let refresh_meta t now =
  let open Lwt.Syntax in
  let* addresses = Store.list_contracts t.store in
  let addresses = List.sort_uniq String.compare addresses in
  let limited = List.length addresses > Policy.max_scan_programs in
  let addresses = take Policy.max_scan_programs addresses in
  let* programs = Lwt_list.filter_map_s (read_meta t.store) addresses in
  let meta = {
    expires_at = now +. metadata_ttl;
    programs;
    limited;
  } in
  t.state <- { t.state with meta = Some meta };
  Lwt.return meta

let load_meta t now =
  match t.state.meta with
  | Some meta when meta.expires_at > now -> Lwt.return meta
  | _ -> refresh_meta t now

let rec collect_rows t holder page programs =
  let open Lwt.Syntax in
  match programs with
  | [] -> Lwt.return (page, false)
  | token :: rest ->
    let* balance =
      Store.read_contract_storage_key
        t.store
        (Policy.address token)
        ("balances:" ^ holder)
    in
    match Option.bind balance (fun value -> Policy.row token ~balance:value) with
    | None -> collect_rows t holder page rest
    | Some row ->
      begin
        match Policy.add page row with
        | Policy.Continue next -> collect_rows t holder next rest
        | Policy.Complete next -> Lwt.return (next, true)
      end

let build_page t query meta =
  let open Lwt.Syntax in
  let page =
    Policy.empty_page
      ~address:query.holder
      ~offset:query.offset
      ~limit:query.limit
  in
  let* page, full = collect_rows t query.holder page meta.programs in
  Lwt.return (Policy.finish ~limited:(meta.limited || full) page)

let query_direct t query =
  let now = Unix.gettimeofday () in
  let key = cache_key query in
  let cached, state = find_page now key t.state in
  t.state <- state;
  match cached with
  | Some payload -> Lwt.return (Ok payload)
  | None ->
    let open Lwt.Syntax in
    let* meta = load_meta t now in
    let* payload = build_page t query meta in
    t.state <- insert_page (Unix.gettimeofday ()) key payload t.state;
    Lwt.return (Ok payload)

let actor_stats t = {
  cache_entries = String_map.cardinal t.state.pages;
  cache_bytes = t.state.page_bytes;
  queued = Queue.length t.data_channel;
  generation = t.generation;
}

let error_reply = function
  | Query _ -> Query_reply (Error Stopped)
  | Read_stats -> Stats_reply {
      cache_entries = 0;
      cache_bytes = 0;
      queued = 0;
      generation = 0L;
    }
  | Stop -> Stop_reply

let resolve command value =
  if Lwt.is_sleeping command.promise then
    Lwt.wakeup_later command.reply value

let drain queue =
  let rec loop () =
    match Queue.take_opt queue with
    | None -> ()
    | Some command ->
      resolve command (error_reply command.message);
      loop ()
  in
  loop ()

let stop_direct t =
  t.channel_open <- false;
  t.generation <- Int64.succ t.generation;
  t.state <- empty_state;
  drain t.control_channel;
  drain t.data_channel;
  Lwt_condition.broadcast t.channel_ready ()

let handle t = function
  | Query query ->
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* result = query_direct t query in
        Lwt.return (Query_reply result))
      (fun _ -> Lwt.return (Query_reply (Error Read_failed)))
  | Read_stats -> Lwt.return (Stats_reply (actor_stats t))
  | Stop -> Lwt.return Stop_reply

let take_command t =
  match Queue.take_opt t.control_channel with
  | Some _ as command -> command
  | None -> Queue.take_opt t.data_channel

let rec actor_loop t =
  match take_command t with
  | None when not t.channel_open -> Lwt.return_unit
  | None ->
    let open Lwt.Syntax in
    let* () = Lwt_condition.wait t.channel_ready in
    actor_loop t
  | Some command ->
    if
      command.generation <> t.generation
      || not
           (String.equal
              command.correlation
              (command_key command.generation command.message))
    then begin
      resolve command (error_reply command.message);
      actor_loop t
    end else if
      match command.message with
      | Query _ -> Unix.gettimeofday () -. command.accepted_at > request_ttl
      | Read_stats | Stop -> false
    then begin
      resolve command (Query_reply (Error Busy));
      actor_loop t
    end else
      match command.message with
      | Stop ->
        stop_direct t;
        resolve command Stop_reply;
        Lwt.return_unit
      | Query _ | Read_stats ->
        let open Lwt.Syntax in
        let* value = handle t command.message in
        resolve command value;
        actor_loop t

let enqueue t ~control message =
  let queue, limit =
    if control then t.control_channel, control_channel_limit
    else t.data_channel, data_channel_limit
  in
  let admitted =
    if control then Queue.length queue < limit
    else data_admitted ~queued:(Queue.length queue)
  in
  if not t.channel_open || not admitted then None
  else
    let promise, reply = Lwt.wait () in
    let command = {
      generation = t.generation;
      correlation = command_key t.generation message;
      accepted_at = Unix.gettimeofday ();
      message;
      promise;
      reply;
    } in
    Queue.push command queue;
    Lwt_condition.signal t.channel_ready ();
    Some promise

let create ~store () =
  let t = {
    store;
    state = empty_state;
    data_channel = Queue.create ();
    control_channel = Queue.create ();
    channel_ready = Lwt_condition.create ();
    channel_open = true;
    generation = 0L;
  } in
  Lwt.async (fun () -> actor_loop t);
  t

let query t ~holder ~offset ~limit =
  if
    not (Octra_core.Crypto.is_octra_address holder)
    || offset < 0
    || offset > Policy.max_scan_programs
    || limit <= 0
    || limit > Policy.max_page_rows
  then
    Lwt.return (Error Read_failed)
  else match enqueue t ~control:false (Query { holder; offset; limit }) with
  | None -> Lwt.return (Error Busy)
  | Some response ->
    let open Lwt.Syntax in
    let* response = response in
    match response with
    | Query_reply result -> Lwt.return result
    | Stats_reply _ | Stop_reply -> Lwt.return (Error Read_failed)

let stats t =
  match enqueue t ~control:false Read_stats with
  | None -> Lwt.return {
      cache_entries = 0;
      cache_bytes = 0;
      queued = data_channel_limit;
      generation = 0L;
    }
  | Some response ->
    let open Lwt.Syntax in
    let* response = response in
    match response with
    | Stats_reply value -> Lwt.return value
    | Query_reply _ | Stop_reply -> Lwt.return {
        cache_entries = 0;
        cache_bytes = 0;
        queued = 0;
        generation = 0L;
      }

let shutdown t =
  if not t.channel_open then Lwt.return_unit
  else
    match enqueue t ~control:true Stop with
    | None -> Lwt.return_unit
    | Some response ->
      let open Lwt.Syntax in
      let* _ = response in
      Lwt.return_unit