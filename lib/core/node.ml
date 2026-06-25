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


type metrics = {
  ths : float;
  npt : float;
  svb : float;
  sp : float;
  cps : float;
}

let metrics_to_yojson m =
  `Assoc [
    "ths", `Float m.ths; "npt", `Float m.npt; "svb", `Float m.svb;
    "sp", `Float m.sp; "cps", `Float m.cps;
  ]

let metrics_of_yojson = function
  | `Assoc fields ->
    let f k = match List.assoc k fields with `Float f -> Ok f | _ -> Error ("Expected float for " ^ k) in
    (match f "ths", f "npt", f "svb", f "sp", f "cps" with
     | Ok ths, Ok npt, Ok svb, Ok sp, Ok cps -> Ok { ths; npt; svb; sp; cps }
     | _ -> Error "Invalid metrics JSON")
  | _ -> Error "Expected JSON object for metrics"

type t = {
  id : string;
  parent : string option;
  children : string list;
  state_hash : string;
  txs : string list;
  tx_hashes : (string * string) list;
  validator : string;
  metrics : metrics;
  timestamp : float;
  signature : string;
}

let tx_hash_to_yojson (hash, data) =
  `Assoc ["hash", `String hash; "data", `String data]

let to_yojson n =
  `Assoc [
    "id", `String n.id;
    "parent", (match n.parent with Some p -> `String p | None -> `Null);
    "children", `List (List.map (fun s -> `String s) n.children);
    "state_hash", `String n.state_hash;
    "txs", `List (List.map (fun s -> `String s) n.txs);
    "txs_with_hashes", `List (List.map tx_hash_to_yojson n.tx_hashes);
    "validator", `String n.validator;
    "metrics", metrics_to_yojson n.metrics;
    "timestamp", `Float n.timestamp;
    "signature", `String n.signature;
  ]

let of_yojson = function
  | `Assoc fields ->
    let str k = match List.assoc k fields with `String s -> Ok s | _ -> Error ("Expected string for " ^ k) in
    let str_opt k = match List.assoc k fields with `String s -> Ok (Some s) | `Null -> Ok None | _ -> Error ("Expected string or null for " ^ k) in
    let str_list k = match List.assoc k fields with
      | `List lst ->
        let rec go acc = function [] -> Ok (List.rev acc) | `String s :: t -> go (s :: acc) t | _ -> Error ("Expected string list for " ^ k) in
        go [] lst
      | _ -> Error ("Expected list for " ^ k)
    in
    let flt k = match List.assoc k fields with `Float f -> Ok f | _ -> Error ("Expected float for " ^ k) in
    let tx_hashes k = match List.assoc_opt k fields with
      | Some (`List lst) ->
        let rec go acc = function
          | [] -> Ok (List.rev acc)
          | `Assoc f :: t ->
            (match List.assoc_opt "hash" f, List.assoc_opt "data" f with
             | Some (`String h), Some (`String d) -> go ((h, d) :: acc) t
             | _ -> Error "Invalid tx_with_hash structure")
          | _ -> Error "Expected object in txs_with_hashes list"
        in
        go [] lst
      | Some _ -> Error ("Expected list for " ^ k)
      | None -> Ok []
    in
    (match str "id", str_opt "parent", str_list "children", str "state_hash",
           str_list "txs", str "validator", metrics_of_yojson (List.assoc "metrics" fields),
           flt "timestamp", str "signature", tx_hashes "txs_with_hashes" with
     | Ok id, Ok parent, Ok children, Ok state_hash, Ok txs, Ok validator,
       Ok metrics, Ok timestamp, Ok signature, Ok tx_hashes ->
       Ok { id; parent; children; state_hash; txs; validator; metrics; timestamp; signature; tx_hashes }
     | Error e, _, _, _, _, _, _, _, _, _
     | _, Error e, _, _, _, _, _, _, _, _
     | _, _, Error e, _, _, _, _, _, _, _
     | _, _, _, Error e, _, _, _, _, _, _
     | _, _, _, _, Error e, _, _, _, _, _
     | _, _, _, _, _, Error e, _, _, _, _
     | _, _, _, _, _, _, Error e, _, _, _
     | _, _, _, _, _, _, _, Error e, _, _
     | _, _, _, _, _, _, _, _, Error e, _
     | _, _, _, _, _, _, _, _, _, Error e -> Error e)
  | _ -> Error "Expected JSON object for node"

let score m =
  0.3 *. m.ths +. 0.2 *. m.npt +. 0.2 *. m.svb +. 0.15 *. m.sp +. 0.15 *. m.cps

let hash n =
  Digestif.SHA256.digest_string (n.id ^ n.state_hash ^ n.validator ^ string_of_float n.timestamp)
  |> Digestif.SHA256.to_hex

let sign node privkey_b64 =
  let msg =
    `Assoc [
      "id", `String node.id;
      "parent", (match node.parent with Some p -> `String p | None -> `Null);
      "children", `List (List.map (fun s -> `String s) node.children);
      "state_hash", `String node.state_hash;
      "txs", `List (List.map (fun s -> `String s) node.txs);
      "txs_with_hashes", `List (List.map tx_hash_to_yojson node.tx_hashes);
      "validator", `String node.validator;
      "metrics", metrics_to_yojson node.metrics;
      "timestamp", `Float node.timestamp;
    ] |> Yojson.Safe.to_string
  in
  match Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn privkey_b64) with
  | Ok sk -> Base64.encode_exn (Mirage_crypto_ec.Ed25519.sign ~key:sk msg)
  | Error _ -> failwith "Invalid private key"