(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cfg = {
  window_s : float;
  max_msgs : int;
  max_bytes : int;
  max_payload_bytes : int;
  max_inv_request : int;
  max_get_reply : int;
}

type bucket = {
  mutable start : float;
  mutable msgs : int;
  mutable bytes : int;
  mutable last_seen : float;
}

type t = (string, bucket) Hashtbl.t

type verdict =
  | Accept
  | Drop of string

let default = {
  window_s = 60.0;
  max_msgs = 4096;
  max_bytes = 64 * 1024 * 1024;
  max_payload_bytes = P2p_tx_gossip.max_tx_json + 4096;
  max_inv_request = 256;
  max_get_reply = 64;
}

let bucket_ttl_s = 300.0

let max_buckets = 4096

let create () =
  Hashtbl.create 64

let reset b now =
  b.start <- now;
  b.msgs <- 0;
  b.bytes <- 0;
  b.last_seen <- now

let rec take n xs =
  if n <= 0 then []
  else
    match xs with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

let prune ?(ttl = bucket_ttl_s) ?(max_entries = max_buckets) t ~now =
  Hashtbl.fold
    (fun peer b acc ->
      if now -. b.last_seen > ttl then peer :: acc else acc)
    t
    []
  |> List.iter (Hashtbl.remove t);
  let overflow = Hashtbl.length t - max_entries in
  if overflow > 0 then
    Hashtbl.fold
      (fun peer b acc -> (b.last_seen, peer) :: acc)
      t
      []
    |> List.sort (fun (ta, pa) (tb, pb) ->
      let by_time = compare ta tb in
      if by_time <> 0 then by_time else String.compare pa pb)
    |> take overflow
    |> List.iter (fun (_, peer) -> Hashtbl.remove t peer)

let size t =
  Hashtbl.length t

let get_bucket t peer now =
  prune t ~now;
  match Hashtbl.find_opt t peer with
  | Some b ->
    b.last_seen <- now;
    b
  | None ->
    let b = { start = now; msgs = 0; bytes = 0; last_seen = now } in
    Hashtbl.replace t peer b;
    b

let admit ?(cfg=default) t ~now ~peer ~bytes =
  if bytes > cfg.max_payload_bytes then
    Drop "payload_too_large"
  else
    let b = get_bucket t peer now in
    if now -. b.start >= cfg.window_s then
      reset b now;
    let next_msgs = b.msgs + 1 in
    let next_bytes = b.bytes + bytes in
    if next_msgs > cfg.max_msgs then
      Drop "message_budget"
    else if next_bytes > cfg.max_bytes then
      Drop "byte_budget"
    else begin
      b.msgs <- next_msgs;
      b.bytes <- next_bytes;
      Accept
    end

let uniq xs =
  let seen = Hashtbl.create (List.length xs) in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then
        false
      else begin
        Hashtbl.replace seen x ();
        true
      end)
    xs

let plan_inv ?(cfg=default) ~has hashes =
  hashes
  |> uniq
  |> List.filter (fun h -> not (has h))
  |> take cfg.max_inv_request

let plan_get ?(cfg=default) ~has hashes =
  hashes
  |> uniq
  |> List.filter has
  |> take cfg.max_get_reply