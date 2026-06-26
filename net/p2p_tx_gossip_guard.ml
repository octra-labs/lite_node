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

let create () =
  Hashtbl.create 64

let reset b now =
  b.start <- now;
  b.msgs <- 0;
  b.bytes <- 0

let get_bucket t peer now =
  match Hashtbl.find_opt t peer with
  | Some b -> b
  | None ->
    let b = { start = now; msgs = 0; bytes = 0 } in
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

let rec take n xs =
  if n <= 0 then []
  else
    match xs with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

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