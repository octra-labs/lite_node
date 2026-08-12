(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Migration = Octra_core.Pvac_migration
module String_map = Map.Make (String)
module Int64_set = Set.Make (Int64)

type error =
  | Busy
  | Stopped
  | Read_failed

type stats = {
  cache_entries : int;
  queued : int;
  generation : int64;
}

type deps = {
  load_pubkey : addr:string -> string option Lwt.t;
  hash_pubkey : string -> string;
  classify_cipher : string -> Migration.cipher_class;
  classify_key : string option -> Migration.key_class;
}

type cache_entry = {
  key_class : Migration.key_class;
  touched : int64;
}

type state = {
  keys : cache_entry String_map.t;
  clock : int64;
}

type query = {
  addr : string;
  key_hash : string option;
  cipher : string;
}

type message =
  | Query of query
  | Read_stats
  | Stop

type reply =
  | Query_reply of (Migration.status, error) result
  | Stats_reply of stats
  | Stop_reply

type command = {
  generation : int64;
  correlation : int64;
  accepted_at : float;
  message : message;
  promise : reply Lwt.t;
  resolver : reply Lwt.u;
}

type t = {
  deps : deps;
  mutable state : state;
  request_channel : command Queue.t;
  control_channel : command Queue.t;
  channel_ready : unit Lwt_condition.t;
  mutable channel_open : bool;
  mutable generation : int64;
  mutable next_correlation : int64;
  mutable pending : Int64_set.t;
}

type key_decision =
  | Use_key of Migration.key_class
  | Load_key

type cache_space =
  | By_hash
  | By_addr

type loaded_key = {
  key_hash : string option;
  cipher_class : Migration.cipher_class;
  key_class : Migration.key_class;
}

let request_capacity = 4
let control_capacity = 2
let cache_capacity = 4_096
let missing_capacity = 1_024
let request_lifetime = 5.0

let empty_state = {
  keys = String_map.empty;
  clock = 0L;
}

let request_admitted queued =
  queued >= 0 && queued < request_capacity

let key_space key =
  if String.length key >= 2 && String.sub key 0 2 = "h:" then By_hash
  else By_addr

let same_space space key = key_space key = space

let space_capacity = function
  | By_hash -> cache_capacity
  | By_addr -> missing_capacity

let oldest_key space keys =
  String_map.fold
    (fun key entry current ->
      match same_space space key, current with
      | false, _ -> current
      | true, None -> Some (key, entry.touched)
      | true, Some (_, touched) when entry.touched < touched ->
        Some (key, entry.touched)
      | true, Some _ -> current)
    keys
    None
  |> Option.map fst

let space_size space keys =
  String_map.fold
    (fun key _ count ->
      if same_space space key then count + 1 else count)
    keys
    0

let reserve_cache key keys =
  let space = key_space key in
  if space_size space keys < space_capacity space then keys
  else
    match oldest_key space keys with
    | None -> keys
    | Some oldest -> String_map.remove oldest keys

let find_key key state =
  let clock = Int64.succ state.clock in
  match String_map.find_opt key state.keys with
  | None -> None, { state with clock }
  | Some entry ->
    let entry = { entry with touched = clock } in
    Some entry.key_class, {
      keys = String_map.add key entry state.keys;
      clock;
    }

let insert_key key key_class state =
  let clock = Int64.succ state.clock in
  let keys = String_map.remove key state.keys |> reserve_cache key in
  {
    keys = String_map.add key { key_class; touched = clock } keys;
    clock;
  }

let hash_key hash = "h:" ^ hash
let addr_key addr = "a:" ^ addr

let query_key (query : query) =
  match query.key_hash with
  | Some hash -> hash_key hash
  | None -> addr_key query.addr

let decide_key (query : query) state =
  let cached, state = find_key (query_key query) state in
  match cached with
  | Some key_class -> Use_key key_class, state
  | None -> Load_key, state

let classify_loaded deps (query : query) pubkey =
  let key_hash = Option.map deps.hash_pubkey pubkey in
  match query.key_hash, key_hash with
  | Some expected, Some actual when not (String.equal expected actual) ->
    Error Read_failed
  | Some _, None -> Error Read_failed
  | _ ->
    Ok {
      key_hash;
      cipher_class = deps.classify_cipher query.cipher;
      key_class = deps.classify_key pubkey;
    }

let classify_cipher deps cipher =
  deps.classify_cipher cipher

let resolve_cached deps (query : query) key_class =
  let open Lwt.Syntax in
  let* cipher_class =
    Lwt_preemptive.detach (classify_cipher deps) query.cipher
  in
  Lwt.return
    (Ok (Migration.status_of_classes cipher_class key_class))

let resolve_loaded t (query : query) =
  let open Lwt.Syntax in
  let* pubkey = t.deps.load_pubkey ~addr:query.addr in
  let* classified =
    Lwt_preemptive.detach
      (fun () -> classify_loaded t.deps query pubkey)
      ()
  in
  match classified with
  | Error error -> Lwt.return (Error error)
  | Ok loaded ->
    let query_key = query_key query in
    let state =
      match loaded.key_hash with
      | None -> t.state
      | Some hash ->
        let state = insert_key query_key loaded.key_class t.state in
        if String.equal query_key (hash_key hash) then state
        else insert_key (hash_key hash) loaded.key_class state
    in
    t.state <- state;
    Lwt.return
      (Ok
         (Migration.status_of_classes
            loaded.cipher_class
            loaded.key_class))

let resolve_query t (query : query) =
  let decision, state =
    decide_key query t.state
  in
  t.state <- state;
  match decision with
  | Use_key key_class -> resolve_cached t.deps query key_class
  | Load_key -> resolve_loaded t query

let actor_stats t = {
  cache_entries = String_map.cardinal t.state.keys;
  queued = Queue.length t.request_channel;
  generation = t.generation;
}

let stopped_reply = function
  | Query _ -> Query_reply (Error Stopped)
  | Read_stats -> Stats_reply {
      cache_entries = 0;
      queued = 0;
      generation = 0L;
    }
  | Stop -> Stop_reply

let resolve command reply =
  if Lwt.is_sleeping command.promise then
    Lwt.wakeup_later command.resolver reply

let rec drain queue =
  match Queue.take_opt queue with
  | None -> ()
  | Some command ->
    resolve command (stopped_reply command.message);
    drain queue

let stop_direct t =
  t.channel_open <- false;
  t.generation <- Int64.succ t.generation;
  t.state <- empty_state;
  t.pending <- Int64_set.empty;
  drain t.control_channel;
  drain t.request_channel;
  Lwt_condition.broadcast t.channel_ready ()

let handle t = function
  | Query query ->
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* result = resolve_query t query in
        Lwt.return (Query_reply result))
      (fun _ -> Lwt.return (Query_reply (Error Read_failed)))
  | Read_stats -> Lwt.return (Stats_reply (actor_stats t))
  | Stop -> Lwt.return Stop_reply

let take_command t =
  match Queue.take_opt t.control_channel with
  | Some _ as command -> command
  | None -> Queue.take_opt t.request_channel

let expired command =
  match command.message with
  | Query _ -> Unix.gettimeofday () -. command.accepted_at > request_lifetime
  | Read_stats
  | Stop -> false

let rec actor_loop t =
  match take_command t with
  | None when not t.channel_open -> Lwt.return_unit
  | None ->
    let open Lwt.Syntax in
    let* () = Lwt_condition.wait t.channel_ready in
    actor_loop t
  | Some command when
      command.generation <> t.generation
      || not (Int64_set.mem command.correlation t.pending) ->
    resolve command (stopped_reply command.message);
    actor_loop t
  | Some command ->
    t.pending <- Int64_set.remove command.correlation t.pending;
    if expired command then begin
      resolve command (Query_reply (Error Busy));
      actor_loop t
    end else
    begin
      match command.message with
      | Stop ->
        stop_direct t;
        resolve command Stop_reply;
        Lwt.return_unit
      | Query _
      | Read_stats ->
        let open Lwt.Syntax in
        let* reply = handle t command.message in
        resolve command reply;
        actor_loop t
    end

let enqueue t ~control message =
  let queue, capacity =
    if control then t.control_channel, control_capacity
    else t.request_channel, request_capacity
  in
  let admitted =
    if control then Queue.length queue < capacity
    else request_admitted (Queue.length queue)
  in
  if not t.channel_open || not admitted then None
  else
    let correlation = Int64.succ t.next_correlation in
    t.next_correlation <- correlation;
    let promise, resolver = Lwt.wait () in
    let command = {
      generation = t.generation;
      correlation;
      accepted_at = Unix.gettimeofday ();
      message;
      promise;
      resolver;
    } in
    t.pending <- Int64_set.add correlation t.pending;
    Queue.push command queue;
    Lwt_condition.signal t.channel_ready ();
    Some promise

let create deps =
  let t = {
    deps;
    state = empty_state;
    request_channel = Queue.create ();
    control_channel = Queue.create ();
    channel_ready = Lwt_condition.create ();
    channel_open = true;
    generation = 0L;
    next_correlation = 0L;
    pending = Int64_set.empty;
  } in
  Lwt.async (fun () -> actor_loop t);
  t

let query t ~addr ~key_hash ~cipher =
  let message = Query { addr; key_hash; cipher } in
  match enqueue t ~control:false message with
  | None -> Lwt.return (Error Busy)
  | Some response ->
    let open Lwt.Syntax in
    let* response = response in
    begin
      match response with
      | Query_reply result -> Lwt.return result
      | Stats_reply _
      | Stop_reply -> Lwt.return (Error Read_failed)
    end

let stats t =
  match enqueue t ~control:true Read_stats with
  | None -> Lwt.return {
      cache_entries = 0;
      queued = request_capacity;
      generation = 0L;
    }
  | Some response ->
    let open Lwt.Syntax in
    let* response = response in
    begin
      match response with
      | Stats_reply stats -> Lwt.return stats
      | Query_reply _
      | Stop_reply -> Lwt.return {
          cache_entries = 0;
          queued = 0;
          generation = 0L;
        }
    end

let shutdown t =
  if not t.channel_open then Lwt.return_unit
  else
    match enqueue t ~control:true Stop with
    | None -> Lwt.return_unit
    | Some response ->
      let open Lwt.Syntax in
      let* _ = response in
      Lwt.return_unit