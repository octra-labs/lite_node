(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Peers = Map.Make (String)

let network = function
  | Unix.ADDR_UNIX path -> "local:" ^ path
  | Unix.ADDR_INET (host, _) ->
    let ip = Ipaddr.of_string_exn (Unix.string_of_inet_addr host) in
    match Ipaddr.to_v4 ip with
    | Some ip ->
      Ipaddr.V4.Prefix.(network (make 24 ip)) |> Ipaddr.V4.to_string
    | None ->
      Ipaddr.V6.Prefix.(network (make 64 (Ipaddr.to_v6 ip))) |> Ipaddr.V6.to_string

type bucket = { at : int64; credit : int64 }
type peer = { bucket : bucket; seen : int64; validator : bool }
type phase = Building | Writing of int
type job = { id : int; peer : string; network : string; validator : bool; cost : int; phase : phase }
type waiting = { who : string; group : string; until : int64; lane : bool; cost : int }
type t = {
  public : bucket;
  validators : bucket;
  peers : peer Peers.t;
  networks : peer Peers.t;
  active : job list;
  waiting : waiting list;
  next : int;
}
type message =
  | Start of { now : int64; peer : string; network : string; validator : bool; cost : int }
  | Finish of int
  | Prepared of { id : int; bytes : int; cost : int }

let reply_limit = 8 * 1024 * 1024
let lane_limit = 4 * reply_limit
let bytes job = match job.phase with Building -> reply_limit | Writing bytes -> bytes
let lane_bytes state validator = List.fold_left (fun total job ->
  if job.validator = validator then total + bytes job else total) 0 state.active

let unit_credit = 1_000_000_000L
let wait_span = 16_000_000_000L
let credit units = Int64.mul (Int64.of_int units) unit_credit
let initial now units = {at = now; credit = credit units}
let create now = {public = initial now 16; validators = initial now 64;
  peers = Peers.empty; networks = Peers.empty; active = []; waiting = []; next = 0}

let refill ~now ~capacity ~rate bucket =
  let elapsed = if Int64.compare now bucket.at <= 0 then 0L
    else Int64.sub now bucket.at in
  let cap = credit capacity in
  let earned = Int64.mul (Int64.min cap elapsed) (Int64.of_int rate) in
  {at = Int64.max now bucket.at; credit = Int64.min cap (Int64.add bucket.credit earned)}

let step state = function
  | Finish id -> {state with active = List.filter (fun job -> job.id <> id) state.active}, None
  | Prepared {id; bytes; cost} ->
    (match List.find_opt (fun job -> job.id = id) state.active with
     | Some ({phase = Building; _} as job)
       when bytes >= 0 && bytes <= reply_limit && cost >= 1 && cost <= job.cost ->
       let refund = credit (job.cost - cost) in
       let restore capacity bucket = {bucket with
         credit = Int64.min (credit capacity) (Int64.add bucket.credit refund)} in
       let peers = Peers.update job.peer (Option.map (fun saved ->
         {saved with bucket = restore (if job.validator then 32 else 16) saved.bucket})) state.peers in
       let networks = Peers.update job.network (Option.map (fun saved ->
         {saved with bucket = restore (if job.validator then 32 else 16) saved.bucket})) state.networks in
       {state with peers; networks;
         public = (if job.validator then state.public else restore 16 state.public);
         validators = (if job.validator then restore 64 state.validators else state.validators);
         active = List.map (fun found -> if found.id = id then
           {found with phase = Writing bytes; cost} else found) state.active}, Some id
     | _ -> state, None)
  | Start {now; peer; network; validator; cost} ->
    let network = if validator then "validator:" ^ peer else "public:" ^ network in
    let busy = List.exists (fun job -> String.equal job.peer peer) state.active in
    let count = List.fold_left (fun count job ->
      if job.validator = validator && job.phase = Building then count + 1 else count) 0 state.active in
    if busy || count >= 2
       || lane_bytes state validator > lane_limit - reply_limit
       || cost < 1 || cost > 16 || state.next = max_int then
      state, None
    else
      let peers = Peers.filter (fun name saved ->
        Int64.compare now saved.seen < 0 || Int64.sub now saved.seen < 60_000_000_000L
        || List.exists (fun job -> String.equal job.peer name) state.active) state.peers in
      let networks = Peers.filter (fun name saved ->
        Int64.compare now saved.seen < 0 || Int64.sub now saved.seen < 60_000_000_000L
        || List.exists (fun job -> String.equal job.network name) state.active) state.networks in
      let tracked = Peers.fold (fun _ (saved : peer) count ->
        if saved.validator = validator then count + 1 else count) peers 0 in
      let same_lane = match Peers.find_opt peer peers with
        | Some saved -> saved.validator = validator | None -> false in
      let groups = Peers.fold (fun _ (saved : peer) count ->
        if saved.validator = validator then count + 1 else count) networks 0 in
      if (not same_lane && tracked >= 256)
         || (not (Peers.mem network networks) && groups >= 256) then state, None
      else
        let capacity, rate = if validator then 32, 4 else 16, 1 in
        let saved = match Peers.find_opt peer peers with
          | None -> initial now capacity
          | Some saved -> refill ~now ~capacity ~rate saved.bucket in
        let group = match Peers.find_opt network networks with
          | None -> initial now capacity
          | Some saved -> refill ~now ~capacity ~rate saved.bucket in
        let global = if validator then refill ~now ~capacity:64 ~rate:16 state.validators
          else refill ~now ~capacity:16 ~rate:4 state.public in
        let price = credit cost in
        let waiting = List.filter (fun item -> item.until > now) state.waiting
          |> List.map (fun item -> if item.who = peer && item.group = network then
            {item with until = Int64.add now wait_span; cost} else item) in
        let first = List.find_opt (fun item -> String.equal item.group network) waiting in
        let available map key = match Peers.find_opt key map with
          | None -> credit capacity
          | Some saved -> (refill ~now ~capacity ~rate saved.bucket).credit in
        let head = List.find_opt (fun item -> item.lane = validator
          && available peers item.who >= credit item.cost
          && available networks item.group >= credit item.cost
          && match List.find_opt (fun before -> before.group = item.group) waiting with
             | Some before -> before.who = item.who | None -> false) waiting in
        let room funds head = match head with
          | Some item when item.who <> peer -> funds >= Int64.add price (credit item.cost)
          | _ -> funds >= price in
        if Int64.compare saved.credit price < 0 || Int64.compare group.credit price < 0
           || not (room global.credit head) || not (room group.credit first) then
          let waiting =
            if saved.credit < price
               || List.exists (fun item -> item.who = peer && item.group = network) waiting
               || List.fold_left (fun total item -> total + if item.group = network then 1 else 0) 0 waiting >= 2
               || List.fold_left (fun total item -> total + if item.lane = validator then 1 else 0) 0 waiting >= 256
            then waiting
            else waiting @ [{who = peer; group = network; lane = validator; cost;
              until = Int64.add now wait_span}] in
          {state with peers; networks; waiting}, None
        else
          let saved = {saved with credit = Int64.sub saved.credit price} in
          let global = {global with credit = Int64.sub global.credit price} in
          let group = {group with credit = Int64.sub group.credit price} in
          let job = {id = state.next; peer; network; validator; cost; phase = Building} in
          {peers = Peers.add peer {bucket = saved; seen = now; validator} peers;
            networks = Peers.add network {bucket = group; seen = now; validator} networks;
            public = (if validator then state.public else global);
            validators = (if validator then global else state.validators);
            active = job :: state.active;
            waiting = List.filter (fun item -> item.who <> peer || item.group <> network) waiting;
            next = state.next + 1}, Some job.id