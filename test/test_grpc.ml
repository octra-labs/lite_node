(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Config = Octra_node_runtime.Grpc_config
module Deadline = Octra_node_runtime.Grpc_deadline
module Dispatch = Octra_node_runtime.Rpc_dispatch
module Frame = Octra_node_runtime.Grpc_frame
module Http = Octra_node_runtime.Rpc_http
module Proto = Octra_node_runtime.Grpc_proto
module Service = Octra_node_runtime.Grpc_service
module Status = Octra_node_runtime.Grpc_status
module Rpc = Octra_core.Rpc

let fail reason =
  failwith ("test_grpc: " ^ reason)

let expect condition reason =
  if not condition then fail reason

let env values name =
  List.assoc_opt name values

let field_string value =
  let encoder = Pbrt.Encoder.create () in
  Pbrt.Encoder.string value encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder;
  Pbrt.Encoder.to_bytes encoder

let field_epoch value =
  let encoder = Pbrt.Encoder.create () in
  Pbrt.Encoder.uint64_as_varint (`unsigned value) encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder;
  Pbrt.Encoder.to_bytes encoder

let oversized_string =
  Bytes.of_string
    "\010\255\255\255\255\255\255\255\255\255\001"

let decode_json_reply body =
  let decoder = Pbrt.Decoder.of_string body in
  let rec loop value =
    match Pbrt.Decoder.key decoder with
    | None ->
      begin
        match value with
        | Some json -> Yojson.Safe.from_string json
        | None -> fail "json reply has no value"
      end
    | Some (1, Pbrt.Bytes) ->
      loop (Some (Pbrt.Decoder.bytes decoder |> Bytes.to_string))
    | Some (_, kind) ->
      Pbrt.Decoder.skip decoder kind;
      loop value
  in
  loop None

let test_config () =
  List.iter (fun values ->
    match Config.of_env (env values) with
    | Ok Config.Disabled -> ()
    | _ -> fail "disabled config differs")
    [[]; ["OCTRA_GRPC_SUBMIT_ENABLE", "true"];
     ["OCTRA_GRPC_ENABLE", "false"; "OCTRA_GRPC_SUBMIT_ENABLE", "true"]];
  let config =
    match Config.of_env (env ["OCTRA_GRPC_ENABLE", "1"]) with
    | Ok (Config.Enabled value) -> value
    | Ok Config.Disabled -> fail "enabled config is disabled"
    | Error reason -> fail reason
  in
  expect (config.host = "127.0.0.1") "default host differs";
  expect (config.port = 8081) "default port differs";
  begin
    match
      Config.of_env
        (env [
           "OCTRA_GRPC_ENABLE", "1";
           "OCTRA_GRPC_HOST", "0.0.0.0";
         ])
    with
    | Error "OCTRA_GRPC_HOST must be a loopback address" -> ()
    | _ -> fail "non-loopback address was admitted"
  end;
  begin
    match
      Config.of_env
        (env [
           "OCTRA_GRPC_ENABLE", "1";
           "OCTRA_GRPC_PORT", "70000";
         ])
    with
    | Error _ -> ()
    | Ok _ -> fail "invalid port was admitted"
  end

let test_deadline () =
  expect
    (Deadline.seconds ~default:5.0 ~limit:10.0 None = Ok 5.0)
    "default deadline differs";
  expect
    (Deadline.seconds ~default:5.0 ~limit:10.0 (Some "25m") = Ok 0.025)
    "millisecond deadline differs";
  expect
    (Deadline.seconds ~default:5.0 ~limit:10.0 (Some "99S") = Ok 10.0)
    "deadline cap differs";
  begin
    match Deadline.seconds ~default:5.0 ~limit:10.0 (Some "123456789S") with
    | Error _ -> ()
    | Ok _ -> fail "long deadline was admitted"
  end

let test_submit_config () =
  let get extra = Config.of_env (env (("OCTRA_GRPC_ENABLE", "1") :: extra)) in
  let size extra = match get extra with
    | Ok (Config.Enabled value) -> value.submit_bytes
    | _ -> fail "submission config was refused"
  in
  let limit = Octra_net.P2p_tx_gossip.max_tx_json + 5 in
  expect (size [] = Some limit) "submission default cap differs";
  List.iter (fun value ->
    expect (size ["OCTRA_GRPC_SUBMIT_ENABLE", value] = Some limit)
      "submission enable differs") ["1"; "true"; "YES"];
  List.iter (fun value ->
    expect (size ["OCTRA_GRPC_SUBMIT_ENABLE", value] = None)
      "submission disable differs") ["0"; "false"; "NO"];
  List.iter (fun value ->
    expect (size ["OCTRA_GRPC_MAX_SUBMIT_BYTES", string_of_int value] = Some value)
      "submission configured cap differs") [1; 65_536; limit];
  List.iter (fun value ->
    expect (Result.is_error (get ["OCTRA_GRPC_MAX_SUBMIT_BYTES", value]))
      "invalid submission cap admitted") ["0"; "-1"; string_of_int (limit + 1); "invalid"];
  expect (Result.is_error (get ["OCTRA_GRPC_SUBMIT_ENABLE", "invalid"]))
    "invalid submission flag admitted"

let test_submit_proto () =
  let json = `Assoc ["nonce", `Int 7; "signature", `String "signed"] in
  let encoded = field_string (Yojson.Safe.to_string json) in
  expect (Proto.decode_submit encoded = Ok json) "submission object differs";
  expect (Proto.decode_submit (Bytes.cat (field_string "{}") encoded) = Ok json)
    "protobuf final field differs";
  List.iter (fun value ->
    expect (Result.is_error (Proto.decode_submit value)) "invalid submission admitted")
    [Bytes.empty; field_epoch 7L; Bytes.cat encoded (field_epoch 7L);
     oversized_string; field_string ""; field_string "[{}]"; field_string "null";
     field_string "{"; field_string "{} {}"];
  let limit = Octra_net.P2p_tx_gossip.max_tx_json in
  let sized size = field_string ("{\"p\":\"" ^ String.make (size - 8) 'a' ^ "\"}") in
  expect (Result.is_ok (Proto.decode_submit (sized limit))) "maximum submission refused";
  expect (Result.is_error (Proto.decode_submit (sized (limit + 1)))) "large submission admitted"

let test_frame () =
  let payload = "abc" in
  let wire = Frame.encode payload in
  begin
    match Frame.decode ~max_message:3 wire with
    | Ok bytes when Bytes.to_string bytes = payload -> ()
    | _ -> fail "frame round trip differs"
  end;
  begin
    match Frame.decode ~max_message:2 wire with
    | Error Frame.Too_large -> ()
    | _ -> fail "large frame was admitted"
  end;
  begin
    match Frame.decode ~max_message:3 (wire ^ "x") with
    | Error Frame.Extra -> ()
    | _ -> fail "second unary payload was admitted"
  end;
  let compressed = Bytes.of_string wire in
  Bytes.set compressed 0 '\001';
  begin
    match Frame.decode ~max_message:3 (Bytes.to_string compressed) with
    | Error Frame.Compressed -> ()
    | _ -> fail "compressed frame was admitted"
  end

let test_proto () =
  expect
    (Proto.decode_address (field_string "oct-address") = Ok "oct-address")
    "address decode differs";
  expect
    (Proto.decode_hash (field_string "hash") = Ok "hash")
    "hash decode differs";
  expect
    (Proto.decode_epoch (field_epoch 42L) = Ok 42)
    "epoch decode differs";
  expect
    (Proto.decode_epoch (Bytes.create 0) = Ok 0)
    "zero epoch decode differs";
  begin
    match Proto.decode_address (Bytes.create 0) with
    | Error _ -> ()
    | Ok _ -> fail "missing address was admitted"
  end;
  begin
    match Proto.decode_address oversized_string with
    | Error _ -> ()
    | Ok _ -> fail "oversized protobuf string was admitted"
  end

let meta = Http.{
  rpc_peer = "local";
  rpc_user_agent = "grpc-test";
  rpc_body_bytes = 0;
}

let result_json params =
  `Assoc ["params", params; "source", `String "shared"]

let route params () =
  Lwt.return (Ok (result_json params))

let routes = [
  "node_status", route;
  "octra_account", route;
  "octra_transaction", route;
  "epoch_get", route;
]

let call meta request =
  Dispatch.handle_request meta request () routes

let direct method_ params =
  let request = Rpc.{ jsonrpc = "2.0"; method_; params; id = `Null } in
  match Lwt_main.run (call meta request) with
  | Rpc.Result (json, _) -> json
  | Rpc.Error_ _ -> fail "direct dispatch failed"

let grpc path payload =
  Lwt_main.run (Service.invoke ~call ~meta ~path payload)

let grpc_json path payload =
  let reply = grpc path payload in
  expect (reply.Service.status.code = Status.Ok) "gRPC call failed";
  match reply.body with
  | Some body -> decode_json_reply body
  | None -> fail "gRPC reply has no body"

let test_malformed_proto () =
  let calls = ref 0 in
  let counted _ _ =
    incr calls;
    Lwt.return (Rpc.Result (`Null, `Null))
  in
  let reply =
    Lwt_main.run
      (Service.invoke
         ~call:counted
         ~meta
         ~path:"/octra.node.v1.Node/Account"
         oversized_string)
  in
  expect
    (reply.Service.status.code = Status.Invalid_argument)
    "malformed protobuf status differs";
  expect (!calls = 0) "malformed protobuf reached RPC dispatch"

let test_parity () =
  let cases = [
    "/octra.node.v1.Node/Status", Bytes.create 0, "node_status", `List [];
    "/octra.node.v1.Node/Account", field_string "oct-address",
      "octra_account", `List [`String "oct-address"];
    "/octra.node.v1.Node/Transaction", field_string "hash",
      "octra_transaction", `List [`String "hash"];
    "/octra.node.v1.Node/Epoch", field_epoch 42L,
      "epoch_get", `List [`Int 42];
  ] in
  List.iter
    (fun (path, payload, method_, params) ->
      expect
        (grpc_json path payload = direct method_ params)
        ("dispatch parity differs for " ^ method_))
    cases

let test_read_only () =
  let calls = ref 0 in
  let counted _ _ =
    incr calls;
    Lwt.return (Rpc.Result (`Null, `Null))
  in
  let reply =
    Lwt_main.run
      (Service.invoke
         ~call:counted
         ~meta
         ~path:"/octra.node.v1.Node/Submit"
         (Bytes.create 0))
  in
  expect (reply.Service.status.code = Status.Unimplemented) "unknown path status differs";
  expect (!calls = 0) "unknown path reached RPC dispatch"

let test_health () =
  let reply =
    grpc
      "/grpc.health.v1.Health/Check"
      (field_string "octra.node.v1.Node")
  in
  expect (reply.Service.status.code = Status.Ok) "health check failed";
  expect (reply.body = Some "\008\001") "health response differs"

let test_submit_parity () =
  let module Tx = Octra_core.Transaction in
  let module Submit = Octra_node_runtime.Submit_rpc in
  let tx = Tx.{
    from = "octCixRsEcmuMHP1SHc4MMVJUSJZbUeQq9kBpNFNj1WKqeB";
    to_ = "octB86WduDWXVPq3KxifVvczzc6NonLzSPHCG4cdkghCErq";
    amount = Z.one; nonce = 7; ou = Z.of_int 1000; timestamp = 1.0;
    signature = "sig"; public_key = None; message = None;
    op_type = Standard; encrypted_data = None;
  } in
  let read _ _ = fail "submission reached read capability" in
  let seen = ref [] in
  let validate tx = seen := tx :: !seen; Ok "accepted-7" in
  let dispatch validate =
    let handler params () = Submit.submit ~validate params in
    fun meta request -> Dispatch.handle_request meta request () ["octra_submit", handler]
  in
  let submit = dispatch validate in
  List.iter (fun op_type ->
    let tx = { tx with op_type } in
    let json = Tx.to_yojson tx in
    let params = `List [json] in
    let request = Rpc.{ jsonrpc = "2.0"; method_ = "octra_submit"; params; id = `Null } in
    let expected = Lwt_main.run (submit meta request) in
    seen := [];
    let reply = Lwt_main.run (Service.invoke ~call:read ~submit ~meta
      ~path:Service.submit_path (field_string (Yojson.Safe.to_string json))) in
    expect (!seen = [tx]) "transaction changed or admitted more than once";
    match expected, reply.body with
    | Rpc.Result (json, _), Some body ->
      expect (reply.status.code = Status.Ok && decode_json_reply body = json) "accepted reply differs"
    | _ -> fail "submission parity failed")
    Tx.[Standard; EncryptOp; DecryptOp; StealthOp; ClaimOp; KeySwitch];
  List.iter (fun (code, message) ->
    let submit = dispatch (fun _ -> Error (code, message)) in
    let params = `List [Tx.to_yojson tx] in
    let request = Rpc.{ jsonrpc = "2.0"; method_ = "octra_submit"; params; id = `Null } in
    let expected = Lwt_main.run (submit meta request) in
    let reply = Lwt_main.run (Service.invoke ~call:read ~submit ~meta ~path:Service.submit_path
      (field_string (Yojson.Safe.to_string (Tx.to_yojson tx)))) in
    match expected with
    | Rpc.Error_ (error, _) ->
      expect (reply.status = Status.of_rpc error && reply.body = None) "submission refusal differs"
    | _ -> fail "rejected submission succeeded")
    ["invalid_signature", "signature differs"; "invalid_nonce", "nonce differs";
     "duplicate", "already admitted"; "staging_full", "full"; "readonly_observer", "read only"];
  seen := [];
  let reply = Lwt_main.run (Service.invoke ~call:read ~submit ~meta ~path:Service.submit_path
    (field_string "{}")) in
  expect (reply.status.code = Status.Invalid_argument && !seen = [])
    "invalid transaction reached validation"

let test_error_map () =
  let failing method_ error meta request =
    let route _ () = Lwt.return (Error error) in
    Dispatch.handle_request meta request () [method_, route]
  in
  let reply =
    Lwt_main.run
      (Service.invoke
         ~call:(failing "octra_transaction" (Rpc.not_found "missing"))
         ~meta
         ~path:"/octra.node.v1.Node/Transaction"
         (field_string "hash"))
  in
  expect (reply.Service.status.code = Status.Not_found) "not-found map differs";
  let trailers = Status.trailers reply.status in
  expect
    (List.assoc_opt "grpc-status" trailers = Some "5")
    "not-found status trailer differs";
  expect
    (List.assoc_opt "octra-rpc-code" trailers = Some "112")
    "RPC code trailer differs";
  let reply =
    Lwt_main.run
      (Service.invoke
         ~call:(failing "octra_account" Rpc.sender_not_found)
         ~meta
         ~path:"/octra.node.v1.Node/Account"
         (field_string "oct-address"))
  in
  expect
    (reply.Service.status.code = Status.Not_found)
    "missing-account map differs"

let test_submit_depth () =
  let rec nest wrap count value =
    if count = 0 then value else wrap (nest wrap (count - 1) value)
  in
  List.iter (fun wrap ->
    List.iter (fun depth ->
      let json = `Assoc ["payload", nest wrap depth (`String "value")] in
      let params = `List [json] in
      let body = `Assoc [
        "jsonrpc", `String "2.0"; "method", `String "octra_submit";
        "params", params; "id", `Null;
      ] in
      let expected = Rpc.parse_body (Yojson.Safe.to_string body) in
      let seen = ref [] in
      let submit _ request =
        seen := request :: !seen;
        Lwt.return (Rpc.Result (request.Rpc.params, request.id))
      in
      let payload = field_string (Yojson.Safe.to_string json) in
      let reply = Lwt_main.run
        (Service.invoke ~call ~submit ~meta ~path:Service.submit_path payload)
      in
      match expected with
      | Error error ->
        expect (error = Rpc.invalid_params "params too deep or too large")
          "depth refusal differs";
        expect (reply.status = Status.of_rpc error && reply.body = None)
          "shared validation refusal differs";
        expect (!seen = []) "deep submission reached backend"
      | Ok (`Single request) ->
        expect (depth <= Rpc.max_param_depth - 2) "deep HTTP submission admitted";
        expect (!seen = [request]) "valid submission changed";
        expect (reply.status = Status.ok) "valid submission refused";
        expect (Option.map decode_json_reply reply.body = Some params)
          "valid submission reply differs"
      | _ -> fail "single submission parsed as batch")
      [0; Rpc.max_param_depth - 2; Rpc.max_param_depth - 1; 32])
    [(fun value -> `List [value]); (fun value -> `Assoc ["value", value])]

let () =
  test_config ();
  test_submit_config ();
  test_submit_proto ();
  test_submit_depth ();
  test_submit_parity ();
  test_deadline ();
  test_frame ();
  test_proto ();
  test_malformed_proto ();
  test_parity ();
  test_read_only ();
  test_health ();
  test_error_map ();
  print_endline "test_grpc: ok"