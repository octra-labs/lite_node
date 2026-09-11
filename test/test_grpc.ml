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
  begin
    match Config.of_env (env []) with
    | Ok Config.Disabled -> ()
    | _ -> fail "disabled config differs"
  end;
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

let () =
  test_config ();
  test_deadline ();
  test_frame ();
  test_proto ();
  test_malformed_proto ();
  test_parity ();
  test_read_only ();
  test_health ();
  test_error_map ();
  print_endline "test_grpc: ok"