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


type t = {
  host : string;
  port : int;
}

let trim s =
  String.trim s

let of_string raw =
  match String.split_on_char ':' (trim raw) with
  | [host; port_s] ->
    let host = trim host in
    let port_s = trim port_s in
    if host = "" then None
    else
      (try
        let port = int_of_string port_s in
        if port > 0 && port <= 65535 then Some { host; port } else None
      with _ -> None)
  | _ -> None

let to_string t =
  Printf.sprintf "%s:%d" t.host t.port

let dedupe xs =
  let seen = Hashtbl.create 16 in
  xs
  |> List.filter (fun x ->
    let key = to_string x in
    if Hashtbl.mem seen key then false
    else begin
      Hashtbl.add seen key ();
      true
    end)

let list_of_csv raw =
  raw
  |> String.split_on_char ','
  |> List.filter_map of_string
  |> dedupe

let env_list name =
  match Sys.getenv_opt name with
  | Some raw -> list_of_csv raw
  | None -> []

let bootstrap configured =
  let base = List.filter_map of_string configured in
  dedupe (base @ env_list "OCTRA_BOOTSTRAP_PEERS" @ env_list "OCTRA_BOOTSTRAP_PEERS_EXTRA")

let advertised ~default_port =
  match Sys.getenv_opt "OCTRA_ADVERTISE_ENDPOINT" with
  | Some raw ->
    (match of_string raw with
     | Some endpoint -> endpoint
     | None -> { host = "127.0.0.1"; port = default_port })
  | None ->
    let host =
      match Sys.getenv_opt "OCTRA_ADVERTISE_HOST" with
      | Some host when String.trim host <> "" -> String.trim host
      | _ -> "127.0.0.1"
    in
    let port =
      match Sys.getenv_opt "OCTRA_ADVERTISE_PORT" with
      | Some raw ->
        (try
          let port = int_of_string (String.trim raw) in
          if port > 0 && port <= 65535 then port else default_port
        with _ -> default_port)
      | None -> default_port
    in
    { host; port }