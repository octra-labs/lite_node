(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  tx_hash : string;
  lane : Resource_lanes.lane;
  engine : string;
  input_hash : string;
  output_hash : string;
  state : state option;
  circle : circle_state option;
  ok : bool;
  reason : string;
  cost : Resource_lanes.used;
}

and state = {
  pre_state_hash : string;
  source_cipher_hash : string;
  pvac_key_hash : string;
  transition_hash : string option;
}

and circle_state = {
  snapshot_hash : string;
  circle_id : string;
  code_hash : string;
  stable_root : string;
  public_reads_hash : string;
  context_hash : string;
  transcript : Circle_hfhe_transcript.entry list;
}

let schema = "octra_preverify_receipt_v1"
let bound_schema = "octra_preverify_receipt_v2"
let circle_schema = "octra_preverify_receipt_v3"
let transition_schema = "octra_private_transition_receipt"

let zero_hash = String.make 64 '0'

let is_hex c =
  ('0' <= c && c <= '9')
  || ('a' <= c && c <= 'f')

let hex_len length value =
  String.length value = length && String.for_all is_hex value

let hex64 value =
  hex_len 64 value

let circle_stable_root value =
  hex64 value || hex_len 128 value

let lane_of_string = function
  | "program" -> Ok Resource_lanes.Program
  | "circle_compute" -> Ok Resource_lanes.Circle_compute
  | "pvac" -> Ok Resource_lanes.Pvac
  | "fhe" -> Ok Resource_lanes.Fhe
  | _ -> Error "invalid_lane"

let engine_of_lane = function
  | Resource_lanes.Pvac -> "pvac_preverify_v1"
  | Resource_lanes.Fhe -> "fhe_preverify_v1"
  | Resource_lanes.Standard
  | Resource_lanes.Program_deploy
  | Resource_lanes.Program
  | Resource_lanes.Circle_compute
  | Resource_lanes.Circle_metadata
  | Resource_lanes.Circle_assets -> "unsupported_preverify_lane"

let bound_engine_of_lane = function
  | Resource_lanes.Pvac -> "pvac_preverify_v2"
  | Resource_lanes.Fhe -> "fhe_preverify_v2"
  | Resource_lanes.Standard
  | Resource_lanes.Program_deploy
  | Resource_lanes.Program
  | Resource_lanes.Circle_compute
  | Resource_lanes.Circle_metadata
  | Resource_lanes.Circle_assets -> "unsupported_preverify_lane"

let transition_engine_of_lane = function
  | Resource_lanes.Pvac -> "pvac_transition_preverify"
  | Resource_lanes.Fhe -> "fhe_transition_preverify"
  | Resource_lanes.Standard
  | Resource_lanes.Program_deploy
  | Resource_lanes.Program
  | Resource_lanes.Circle_compute
  | Resource_lanes.Circle_metadata
  | Resource_lanes.Circle_assets -> "unsupported_preverify_lane"

let circle_engine = "circle_hfhe_preverify_v1"

let valid_lane r =
  match r.state, r.circle, r.lane with
  | None, None, (Resource_lanes.Pvac | Resource_lanes.Fhe)
  | Some _, None, (Resource_lanes.Pvac | Resource_lanes.Fhe)
  | None, Some _, Resource_lanes.Circle_compute -> true
  | _ -> false

let expected_engine r =
  match r.state, r.circle with
  | None, None -> engine_of_lane r.lane
  | Some { transition_hash = None; _ }, None -> bound_engine_of_lane r.lane
  | Some { transition_hash = Some _; _ }, None ->
    transition_engine_of_lane r.lane
  | None, Some _ -> circle_engine
  | Some _, Some _ -> "invalid"

let z_json z =
  `String (Z.to_string z)

let cost_json cost =
  `Assoc [
    "txs", `Int cost.Resource_lanes.txs;
    "bytes", `Int cost.Resource_lanes.bytes;
    "ou", z_json cost.Resource_lanes.ou;
    "proof", `Int cost.Resource_lanes.proof;
  ]

let base_fields r parsed_schema =
  [
    "schema", `String parsed_schema;
    "tx_hash", `String r.tx_hash;
    "lane", `String (Resource_lanes.to_string r.lane);
    "engine", `String r.engine;
    "input_hash", `String r.input_hash;
    "output_hash", `String r.output_hash;
    "ok", `Bool r.ok;
    "reason", `String r.reason;
    "cost", cost_json r.cost;
  ]

let to_yojson r =
  match r.state, r.circle with
  | None, None -> `Assoc (base_fields r schema)
  | Some ({ transition_hash = None; _ } as state), None ->
    `Assoc
      (base_fields r bound_schema @ [
        "pre_state_hash", `String state.pre_state_hash;
        "source_cipher_hash", `String state.source_cipher_hash;
        "pvac_key_hash", `String state.pvac_key_hash;
      ])
  | Some ({ transition_hash = Some transition_hash; _ } as state), None ->
    `Assoc
      (base_fields r transition_schema @ [
        "pre_state_hash", `String state.pre_state_hash;
        "source_cipher_hash", `String state.source_cipher_hash;
        "pvac_key_hash", `String state.pvac_key_hash;
        "transition_hash", `String transition_hash;
      ])
  | None, Some circle ->
    `Assoc
      (base_fields r circle_schema @ [
        "pre_state_hash", `String circle.snapshot_hash;
        "circle_id", `String circle.circle_id;
        "code_hash", `String circle.code_hash;
        "stable_root", `String circle.stable_root;
        "public_reads_hash", `String circle.public_reads_hash;
        "context_hash", `String circle.context_hash;
        "transcript",
        Circle_hfhe_transcript.entries_json circle.transcript;
      ])
  | Some _, Some _ ->
    `Assoc []

let canonical r =
  Yojson.Safe.to_string (to_yojson r)

let schema_of r =
  match r.state, r.circle with
  | None, None -> schema
  | Some { transition_hash = None; _ }, None -> bound_schema
  | Some { transition_hash = Some _; _ }, None -> transition_schema
  | None, Some _ -> circle_schema
  | Some _, Some _ -> "invalid"

let hash r =
  let parsed_schema = schema_of r in
  Digestif.SHA256.digest_string (parsed_schema ^ "\000" ^ canonical r)
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

let parse_state parsed_schema fields =
  if parsed_schema = schema then
    Ok None
  else if parsed_schema = bound_schema then
    bind (string_field "pre_state_hash" fields) (fun pre_state_hash ->
    bind (string_field "source_cipher_hash" fields) (fun source_cipher_hash ->
    bind (string_field "pvac_key_hash" fields) (fun pvac_key_hash ->
      Ok
        (Some {
           pre_state_hash;
           source_cipher_hash;
           pvac_key_hash;
           transition_hash = None;
         }))))
  else if parsed_schema = transition_schema then
    bind (string_field "pre_state_hash" fields) (fun pre_state_hash ->
    bind (string_field "source_cipher_hash" fields) (fun source_cipher_hash ->
    bind (string_field "pvac_key_hash" fields) (fun pvac_key_hash ->
    bind (string_field "transition_hash" fields) (fun transition_hash ->
      Ok
        (Some {
           pre_state_hash;
           source_cipher_hash;
           pvac_key_hash;
           transition_hash = Some transition_hash;
         })))))
  else if parsed_schema = circle_schema then
    Ok None
  else
    Error "invalid_schema"

let parse_circle parsed_schema fields =
  if
    parsed_schema = schema
    || parsed_schema = bound_schema
    || parsed_schema = transition_schema
  then
    Ok None
  else if parsed_schema = circle_schema then
    bind (string_field "pre_state_hash" fields) (fun pre_state_hash ->
    bind (string_field "circle_id" fields) (fun circle_id ->
    bind (string_field "code_hash" fields) (fun code_hash ->
    bind (string_field "stable_root" fields) (fun stable_root ->
    bind (string_field "public_reads_hash" fields) (fun public_reads_hash ->
    bind (string_field "context_hash" fields) (fun context_hash ->
    bind (get "transcript" fields) (fun transcript_json ->
    bind
      (Circle_hfhe_transcript.entries_of_json transcript_json)
      (fun transcript ->
        Ok
          (Some {
             snapshot_hash = pre_state_hash;
             circle_id;
             code_hash;
             stable_root;
             public_reads_hash;
             context_hash;
             transcript;
           })))))))))
  else
    Error "invalid_schema"

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
    bind (parse_state parsed_schema fields) (fun state ->
    bind (parse_circle parsed_schema fields) (fun circle ->
      Ok {
        tx_hash;
        lane;
        engine;
        input_hash;
        output_hash;
        state;
        circle;
        ok;
        reason;
        cost;
      })))))))))))))
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
  else if not (valid_lane r) then Error "invalid_lane"
  else if r.engine <> expected_engine r
  then Error "invalid_engine"
  else if not (hex64 r.input_hash) then Error "invalid_input_hash"
  else if not (hex64 r.output_hash) then Error "invalid_output_hash"
  else if
    match r.state with
    | None -> false
    | Some state ->
      not (hex64 state.pre_state_hash)
      || not (hex64 state.source_cipher_hash)
      || not (hex64 state.pvac_key_hash)
      || Option.fold
           ~none:false
           ~some:(fun transition_hash -> not (hex64 transition_hash))
           state.transition_hash
  then Error "invalid_state_binding"
  else if
    match r.circle with
    | None -> false
    | Some circle ->
      not (hex64 circle.snapshot_hash)
      || not (Crypto.Address.is_valid_address circle.circle_id)
      || not (hex64 circle.code_hash)
      || not (circle_stable_root circle.stable_root)
      || not (hex64 circle.public_reads_hash)
      || not (hex64 circle.context_hash)
      || Result.is_error
           (Circle_hfhe_transcript.validate circle.transcript)
  then Error "invalid_circle_binding"
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
    state = None;
    circle = None;
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

let make_bound ~tx_hash ~lane ~input_hash ~output_hash ~state ~ok ~reason
    ~cost =
  let r = {
    tx_hash;
    lane;
    engine =
      begin
        match state.transition_hash with
        | None -> bound_engine_of_lane lane
        | Some _ -> transition_engine_of_lane lane
      end;
    input_hash;
    output_hash;
    state = Some state;
    circle = None;
    ok;
    reason;
    cost;
  } in
  match validate r with
  | Ok () -> Ok r
  | Error e -> Error e

let for_tx_bound ~input_hash ~output_hash ~state ~ok ~reason tx =
  make_bound
    ~tx_hash:(Transaction.hash tx)
    ~lane:(Resource_lanes.of_op tx.Transaction.op_type)
    ~input_hash
    ~output_hash
    ~state
    ~ok
    ~reason
    ~cost:(Resource_lanes.cost tx)

let make_circle ~tx_hash ~input_hash ~output_hash ~circle ~ok ~reason ~cost =
  let r = {
    tx_hash;
    lane = Resource_lanes.Circle_compute;
    engine = circle_engine;
    input_hash;
    output_hash;
    state = None;
    circle = Some circle;
    ok;
    reason;
    cost;
  } in
  match validate r with
  | Ok () -> Ok r
  | Error e -> Error e