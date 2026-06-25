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
  tx_hash : string;
  lane : Resource_lanes.lane;
  engine : string;
  input_hash : string;
  output_hash : string;
  ok : bool;
  reason : string;
  cost : Resource_lanes.used;
}

let schema = "octra_preverify_receipt_v1"

let zero_hash = String.make 64 '0'

let is_hex c =
  ('0' <= c && c <= '9')
  || ('a' <= c && c <= 'f')

let hex64 s =
  String.length s = 64 && String.for_all is_hex s

let lane_of_string = function
  | "pvac" -> Ok Resource_lanes.Pvac
  | "fhe" -> Ok Resource_lanes.Fhe
  | _ -> Error "invalid_lane"

let engine_of_lane = function
  | Resource_lanes.Pvac -> "pvac_preverify_v1"
  | Resource_lanes.Fhe -> "fhe_preverify_v1"
  | Resource_lanes.Standard
  | Resource_lanes.Program
  | Resource_lanes.Circle_metadata
  | Resource_lanes.Circle_assets -> "unsupported_preverify_lane"

let valid_lane = function
  | Resource_lanes.Pvac
  | Resource_lanes.Fhe -> true
  | Resource_lanes.Standard
  | Resource_lanes.Program
  | Resource_lanes.Circle_metadata
  | Resource_lanes.Circle_assets -> false

let z_json z =
  `String (Z.to_string z)

let cost_json cost =
  `Assoc [
    "txs", `Int cost.Resource_lanes.txs;
    "bytes", `Int cost.Resource_lanes.bytes;
    "ou", z_json cost.Resource_lanes.ou;
    "proof", `Int cost.Resource_lanes.proof;
  ]

let to_yojson r =
  `Assoc [
    "schema", `String schema;
    "tx_hash", `String r.tx_hash;
    "lane", `String (Resource_lanes.to_string r.lane);
    "engine", `String r.engine;
    "input_hash", `String r.input_hash;
    "output_hash", `String r.output_hash;
    "ok", `Bool r.ok;
    "reason", `String r.reason;
    "cost", cost_json r.cost;
  ]

let canonical r =
  Yojson.Safe.to_string (to_yojson r)

let hash r =
  Digestif.SHA256.digest_string (schema ^ "\000" ^ canonical r)
  |> Digestif.SHA256.to_hex

let get name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (name ^ "_missing")

let string_field name fields =
  match get name fields with
  | Ok (`String value) -> Ok value
  | Ok _ -> Error (name ^ "_invalid")
  | Error e -> Error e

let bool_field name fields =
  match get name fields with
  | Ok (`Bool value) -> Ok value
  | Ok _ -> Error (name ^ "_invalid")
  | Error e -> Error e

let int_field name fields =
  match get name fields with
  | Ok (`Int value) -> Ok value
  | Ok _ -> Error (name ^ "_invalid")
  | Error e -> Error e

let z_field name fields =
  match get name fields with
  | Ok (`String value) ->
    begin
      try Ok (Z.of_string value)
      with _ -> Error (name ^ "_invalid")
    end
  | Ok _ -> Error (name ^ "_invalid")
  | Error e -> Error e

let bind result f =
  match result with
  | Ok value -> f value
  | Error e -> Error e

let parse_cost = function
  | `Assoc fields ->
    bind (int_field "txs" fields) (fun txs ->
    bind (int_field "bytes" fields) (fun bytes ->
    bind (z_field "ou" fields) (fun ou ->
    bind (int_field "proof" fields) (fun proof ->
      Ok Resource_lanes.{ txs; bytes; ou; proof }))))
  | _ -> Error "cost_invalid"

let of_yojson = function
  | `Assoc fields ->
    bind (string_field "schema" fields) (fun parsed_schema ->
    bind (string_field "tx_hash" fields) (fun tx_hash ->
    bind (string_field "lane" fields) (fun lane_raw ->
    bind (lane_of_string lane_raw) (fun lane ->
    bind (string_field "engine" fields) (fun engine ->
    bind (string_field "input_hash" fields) (fun input_hash ->
    bind (string_field "output_hash" fields) (fun output_hash ->
    bind (bool_field "ok" fields) (fun ok ->
    bind (string_field "reason" fields) (fun reason ->
    bind (get "cost" fields) (fun cost_json ->
    bind (parse_cost cost_json) (fun cost ->
      if parsed_schema <> schema then Error "invalid_schema"
      else Ok { tx_hash; lane; engine; input_hash; output_hash; ok; reason; cost })))))))))))
  | _ -> Error "receipt_invalid"

let of_string raw =
  try Yojson.Safe.from_string raw |> of_yojson
  with _ -> Error "receipt_json_invalid"

let valid_cost cost =
  cost.Resource_lanes.txs = 1
  && cost.bytes >= 0
  && Z.geq cost.ou Z.zero
  && cost.proof >= 1

let validate r =
  if not (hex64 r.tx_hash) then Error "invalid_tx_hash"
  else if not (valid_lane r.lane) then Error "invalid_lane"
  else if r.engine <> engine_of_lane r.lane then Error "invalid_engine"
  else if not (hex64 r.input_hash) then Error "invalid_input_hash"
  else if not (hex64 r.output_hash) then Error "invalid_output_hash"
  else if r.ok && r.reason <> "" then Error "ok_reason_not_empty"
  else if (not r.ok) && r.reason = "" then Error "fail_reason_empty"
  else if String.length r.reason > 128 then Error "reason_too_long"
  else if not (valid_cost r.cost) then Error "invalid_cost"
  else Ok ()

let make ~tx_hash ~lane ~input_hash ~output_hash ~ok ~reason ~cost =
  let r = {
    tx_hash;
    lane;
    engine = engine_of_lane lane;
    input_hash;
    output_hash;
    ok;
    reason;
    cost;
  } in
  match validate r with
  | Ok () -> Ok r
  | Error e -> Error e

let for_tx ~input_hash ~output_hash ~ok ~reason tx =
  make
    ~tx_hash:(Transaction.hash tx)
    ~lane:(Resource_lanes.of_op tx.Transaction.op_type)
    ~input_hash
    ~output_hash
    ~ok
    ~reason
    ~cost:(Resource_lanes.cost tx)