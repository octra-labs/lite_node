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

let create () =
  Hashtbl.create 64

let make_entry now = {
  bad_start = now;
  bad_count = 0;
  conn_start = now;
  conn_count = 0;
  ban_until = 0.0;
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

let entry t key now =
  match Hashtbl.find_opt t key with
  | Some e -> e
  | None ->
    let e = make_entry now in
    Hashtbl.replace t key e;
    e

let banned (e : entry) now =
  e.ban_until > now

let host_of_addr addr =
  match String.rindex_opt addr ':' with
  | None -> addr
  | Some i -> String.sub addr 0 i

let unix_key = function
  | Unix.ADDR_INET (ip, _) -> Unix.string_of_inet_addr ip
  | Unix.ADDR_UNIX s -> s

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

let is_banned t ~now ~key =
  match Hashtbl.find_opt t key with
  | Some e -> banned e now
  | None -> false

let score t key =
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