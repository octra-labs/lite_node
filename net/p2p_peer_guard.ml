(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cfg = {
  bad_window_s : float;
  max_bad : int;
  ban_s : float;
  conn_window_s : float;
  max_conn : int;
}

type entry = {
  mutable bad_start : float;
  mutable bad_count : int;
  mutable conn_start : float;
  mutable conn_count : int;
  mutable ban_until : float;
  mutable last_seen : float;
  mutable last_reason : string;
  mutable invalid_frame_count : int;
  mutable bad_signature_count : int;
  mutable stale_root_count : int;
}

type t = (string, entry) Hashtbl.t

type verdict =
  | Accept
  | Drop of string

type bad_result =
  | Noted of int
  | Banned of float

type score_record = {
  key : string;
  bad_count : int;
  conn_count : int;
  ban_until : float;
  last_reason : string;
  invalid_frame_count : int;
  bad_signature_count : int;
  stale_root_count : int;
}

let default = {
  bad_window_s = 60.0;
  max_bad = 3;
  ban_s = 120.0;
  conn_window_s = 10.0;
  max_conn = 64;
}

let entry_ttl_s = 600.0

let max_entries = 4096

let create () =
  Hashtbl.create 64

let make_entry now = {
  bad_start = now;
  bad_count = 0;
  conn_start = now;
  conn_count = 0;
  ban_until = 0.0;
  last_seen = now;
  last_reason = "";
  invalid_frame_count = 0;
  bad_signature_count = 0;
  stale_root_count = 0;
}

let contains s part =
  let n = String.length s in
  let m = String.length part in
  let rec loop i =
    if m = 0 then true
    else if i + m > n then false
    else if String.sub s i m = part then true
    else loop (i + 1)
  in
  loop 0

let count_reason (e : entry) reason =
  if contains reason "invalid_frame"
     || contains reason "malformed"
     || contains reason "decode"
     || contains reason "exception" then
    e.invalid_frame_count <- e.invalid_frame_count + 1;
  if contains reason "bad_signature"
     || contains reason "signature" then
    e.bad_signature_count <- e.bad_signature_count + 1;
  if contains reason "stale_root"
     || contains reason "wrong_height"
     || contains reason "stale" then
    e.stale_root_count <- e.stale_root_count + 1

let banned (e : entry) now =
  e.ban_until > now

let entry_until now (e : entry) =
  if banned e now then max e.last_seen e.ban_until else e.last_seen

let take n xs =
  let rec loop left acc = function
    | _ when left <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: rest -> loop (left - 1) (x :: acc) rest
  in
  loop n [] xs

let prune ?(ttl = entry_ttl_s) ?(max_entries = max_entries) t ~now =
  Hashtbl.fold
    (fun key e acc ->
      if not (banned e now) && now -. e.last_seen > ttl then key :: acc
      else acc)
    t
    []
  |> List.iter (Hashtbl.remove t);
  let overflow = Hashtbl.length t - max_entries in
  if overflow > 0 then
    Hashtbl.fold
      (fun key e acc -> (entry_until now e, key) :: acc)
      t
      []
    |> List.sort (fun (ta, ka) (tb, kb) ->
      let by_time = compare ta tb in
      if by_time <> 0 then by_time else String.compare ka kb)
    |> take overflow
    |> List.iter (fun (_, key) -> Hashtbl.remove t key)

let entry t key now =
  prune t ~now;
  match Hashtbl.find_opt t key with
  | Some e ->
    e.last_seen <- now;
    e
  | None ->
    let e = make_entry now in
    Hashtbl.replace t key e;
    e

let host_of_addr addr =
  match String.rindex_opt addr ':' with
  | None -> addr
  | Some i -> String.sub addr 0 i

let unix_key = function
  | Unix.ADDR_INET (ip, _) -> Unix.string_of_inet_addr ip
  | Unix.ADDR_UNIX s -> s

let identity_key node_id =
  "node:" ^ node_id

let handshake_penalty reason =
  if contains reason "invalid Ed25519 signature" then
    Some "bad_signature_handshake"
  else if contains reason "node_id does not match pubkey" then
    Some "bad_signature_handshake"
  else if contains reason "expected HELLO" then
    Some "invalid_frame_handshake"
  else
    None

let admit_conn ?(cfg=default) t ~now ~key =
  let e = entry t key now in
  if banned e now then
    Drop "banned"
  else begin
    if now -. e.conn_start >= cfg.conn_window_s then begin
      e.conn_start <- now;
      e.conn_count <- 0
    end;
    let next = e.conn_count + 1 in
    if next > cfg.max_conn then begin
      e.conn_count <- next;
      e.last_reason <- "connection_budget";
      e.ban_until <- now +. cfg.ban_s;
      Drop "connection_budget"
    end else begin
      e.conn_count <- next;
      Accept
    end
  end

let report_bad ?(cfg=default) t ~now ~key ~reason =
  let e = entry t key now in
  if now -. e.bad_start >= cfg.bad_window_s then begin
    e.bad_start <- now;
    e.bad_count <- 0
  end;
  e.bad_count <- e.bad_count + 1;
  e.last_reason <- reason;
  count_reason e reason;
  if e.bad_count >= cfg.max_bad then begin
    e.ban_until <- now +. cfg.ban_s;
    Banned e.ban_until
  end else
    Noted e.bad_count

let is_banned (t : t) ~now ~key =
  prune t ~now;
  match Hashtbl.find_opt t key with
  | Some e -> banned e now
  | None -> false

let score (t : t) key =
  match Hashtbl.find_opt t key with
  | None -> None
  | Some e -> Some (e.bad_count, e.conn_count, e.ban_until, e.last_reason)

let snapshot (t : t) =
  Hashtbl.fold
    (fun key (e : entry) acc -> {
      key;
      bad_count = e.bad_count;
      conn_count = e.conn_count;
      ban_until = e.ban_until;
      last_reason = e.last_reason;
      invalid_frame_count = e.invalid_frame_count;
      bad_signature_count = e.bad_signature_count;
      stale_root_count = e.stale_root_count;
    } :: acc)
    t
    []
  |> List.sort (fun a b -> String.compare a.key b.key)