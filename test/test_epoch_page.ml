(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Page = Octra_node_runtime.Epoch_page
module Proto = Octra_node_runtime.Grpc_proto
module Service = Octra_node_runtime.Grpc_service
module Dispatch = Octra_node_runtime.Rpc_dispatch
module Status = Octra_node_runtime.Grpc_status
module Rpc = Octra_core.Rpc
module Journal = Octra_core.Epochlog

let expect label condition = if not condition then failwith label
let root epoch = Printf.sprintf "%064x" (epoch + 1)
let anchor epoch = Page.{ chain = "octra-test"; epoch; root = root epoch }
let row epoch = Page.{
  epoch; root = root epoch; previous = if epoch = 0 then "" else root (epoch - 1);
  tx_start = Int64.add 9_007_199_254_740_993L (Int64.of_int epoch);
  tx_count = 1; time = float_of_int epoch +. 0.25;
}
let raw epoch = Yojson.Safe.to_string (Page.row_json (row epoch))
let request ?anchor ?(previous = "") ?(limit = 32) start =
  Page.{ start; limit; anchor; previous }
let load ~max_bytes epoch =
  let value = raw epoch in
  if String.length value > max_bytes then Error (Rpc.err 107 "read limit" None) else Ok (Some value)
let run ?(load = load) ?(head = Some (anchor 9)) request =
  Lwt_main.run (Page.read ~head ~load request)
let page = function Ok value -> value | Error error -> failwith error.Rpc.message
let rejected = function Error _ -> true | Ok _ -> false

let test_pages () =
  let first = run (request ~limit:3 0) |> page in
  expect "first page" (first.stop = Page.More && first.next = 3
    && List.map (fun row -> row.Page.epoch) first.rows = [0; 1; 2]);
  let second = run ~head:(Some (anchor 12))
    (request ~anchor:first.anchor ~previous:(root 2) ~limit:3 first.next) |> page in
  expect "fixed head" (second.anchor = first.anchor && second.next = 6);
  let last = run (request ~anchor:first.anchor ~previous:(root 5) 6) |> page in
  expect "complete page" (last.stop = Page.Complete && last.next = 10);
  expect "response round trip" (Page.of_json (Page.json last) = Ok last);
  expect "large txid exact" ((List.hd first.rows).tx_start = 9_007_199_254_740_993L);
  expect "root mismatch" (rejected (run
    (request ~anchor:{ first.anchor with root = root 11 } 0)));
  expect "wrong chain" (rejected (run
    (request ~anchor:{ first.anchor with chain = "another-chain" } 0)));
  expect "root link" (rejected (run (request ~previous:(root 8) 3)));
  expect "no head" (rejected (run ~head:None (request 0)))

let test_gaps () =
  let missing ~max_bytes epoch = if epoch = 2 then Ok None else load ~max_bytes epoch in
  let result = run ~load:missing (request 0) |> page in
  expect "stop at first gap" (result.stop = Page.Gap && result.next = 2
    && List.length result.rows = 2);
  let empty = run ~load:missing (request 2) |> page in
  expect "empty gap differs from complete" (empty.rows = [] && empty.stop = Page.Gap);
  expect "missing anchor" (rejected (run ~load:missing
    (request ~anchor:(anchor 2) 0)));
  let changed field value ~max_bytes epoch =
    if epoch <> 1 then load ~max_bytes epoch else
      let fields = match Page.row_json (row epoch) with `Assoc fields -> fields | _ -> assert false in
      Ok (Some (Yojson.Safe.to_string (`Assoc ((field, value) :: List.remove_assoc field fields))))
  in
  List.iter (fun (field, value) ->
    expect field (rejected (run ~load:(changed field value) (request 0)))) [
    "id", `Int 2; "prev_state_root", `String (root 8);
    "start_txid", `String "1"; "tx_count", `Int (-1);
    "state_root", `String "not-a-root";
  ];
  expect "invalid index bytes" (rejected
    (run ~load:(fun ~max_bytes:_ _ -> Ok (Some "{")) (request 0)))

let test_roots () =
  List.iter (fun width ->
    let root epoch = Printf.sprintf "%0*x" (width epoch) (epoch + 1) in
    let row epoch = { (row epoch) with
      root = root epoch; previous = if epoch = 0 then "" else root (epoch - 1) } in
    let head epoch = { (anchor epoch) with root = root epoch } in
    let load ~max_bytes epoch =
      let raw = Yojson.Safe.to_string (Page.row_json (row epoch)) in
      if String.length raw > max_bytes then Error (Rpc.err 107 "read limit" None)
      else Ok (Some raw)
    in
    let first = run ~load ~head:(Some (head 9)) (request ~limit:3 2) |> page in
    let next = run ~load ~head:(Some (head 12))
      (request ~anchor:first.anchor ~previous:(root 4) first.next) |> page in
    expect "root bytes survive pages" (first.rows @ next.rows = List.init 8 (fun i -> row (i + 2)));
    expect "root bytes survive JSON" (Page.of_json (Page.json next) = Ok next);
    let decoder = Pbrt.Decoder.of_string (Proto.encode_page next) in
    expect "anchor field" (Pbrt.Decoder.key decoder = Some (1, Pbrt.Bytes));
    let nested = Pbrt.Decoder.nested decoder in
    let rec find_root () = match Pbrt.Decoder.key nested with
      | None -> failwith "anchor root missing"
      | Some (3, Pbrt.Bytes) -> Pbrt.Decoder.string nested
      | Some (_, kind) -> Pbrt.Decoder.skip nested kind; find_root ()
    in
    expect "root bytes survive protobuf" (find_root () = (head 9).root);
    expect "full root compared" (rejected (run ~load ~head:(Some (head 9))
      (request ~previous:(String.make (width 4) 'f') 5)))) [
    (fun _ -> 64); (fun _ -> 128); (fun epoch -> if epoch < 5 then 128 else 64);
  ];
  List.iter (fun root ->
    expect "root width rejected" (rejected (Page.validate
      (request ~anchor:{ (anchor 9) with root } 0))))
    (List.map (fun width -> String.make width 'a') [0; 63; 65; 127; 129]
      @ [String.make 128 'g'; String.make 128 'A']);
  let missing ~max_bytes:_ epoch =
    Ok (Some (Yojson.Safe.to_string (Page.row_json { (row epoch) with previous = "" }))) in
  expect "missing historical link rejected" (rejected (run ~load:missing (request 1)))

let test_partition () =
  List.iter (fun head ->
    List.iter (fun limit ->
      let rec collect start previous rows =
        let result = run ~head:(Some (anchor (head + 2)))
          (request ~anchor:(anchor head) ~limit ~previous start) |> page in
        let rows = rows @ result.rows in
        match result.stop with
        | Page.More -> collect result.next (List.hd (List.rev result.rows)).root rows
        | Page.Complete -> rows
        | Page.Gap -> failwith "unexpected gap"
      in
      let rows = collect 0 "" [] in
      expect "partition has no loss or repeats"
        (rows = List.init (head + 1) row)) [1; 2; 3; 7; 32; 64]) (List.init 21 Fun.id)

let test_limits () =
  let calls = ref 0 in
  let counted ~max_bytes epoch = incr calls; load ~max_bytes epoch in
  List.iter (fun request -> expect "invalid input" (rejected (run ~load:counted request))) [
    request (-1); request (Page.max_epoch + 1); request ~limit:0 0;
    request ~limit:65 0; request ~anchor:(anchor 10) 0;
  ];
  expect "reject before storage" (!calls = 0);
  let end_ = Page.max_epoch in
  let last = run ~head:(Some (anchor end_)) (request end_) |> page in
  expect "last index" (last.stop = Page.Complete && last.next = end_ + 1);
  let limits = ref [] in
  let large ~max_bytes epoch =
    limits := max_bytes :: !limits;
    let value = raw epoch in
    let size = Page.max_record in
    if max_bytes < size then Error (Rpc.err 107 "read limit" None)
    else Ok (Some (value ^ String.make (size - String.length value) ' '))
  in
  expect "aggregate bytes" (rejected (run ~load:large (request 0)));
  expect "aggregate cap" (List.length !limits = 5 && List.hd !limits = 0);
  expect "oversized record" (rejected (run
    ~load:(fun ~max_bytes _ -> Ok (Some (String.make (max_bytes + 1) ' '))) (request 0)));
  List.iter (fun params -> expect "invalid JSON" (rejected (Page.parse params))) [
    `Assoc ["limit", `Int 1; "limit", `Int 2];
    `Assoc ["start", `String "0x10"];
    `Assoc ["start", `Int 0; "extra", `Null];
  ]

let test_cancel () =
  let calls = ref 0 in
  let load ~max_bytes epoch = incr calls; load ~max_bytes epoch in
  let pending = Page.read ~head:(Some (anchor 9)) ~load (request 0) in
  Lwt.cancel pending;
  expect "cancel before read" (!calls = 0);
  expect "cancelled promise" (match Lwt.state pending with Lwt.Fail Lwt.Canceled -> true | _ -> false);
  let pending = Page.read ~head:(Some (anchor 9)) ~load (request 0) in
  Lwt_main.run (let open Lwt.Syntax in
    let* () = Lwt.pause () in
    Lwt.cancel pending;
    Lwt.return_unit);
  expect "cancel between rows" (!calls = 1)

let test_admission () =
  let lock = Lwt_mutex.create () in
  let work, _ = Lwt.task () in
  let first = Page.admit lock (fun () -> work) in
  expect "reader owned" (Lwt_mutex.is_locked lock);
  let entered = ref false in
  let next = Page.admit lock (fun () -> entered := true; Lwt.return (Ok ())) in
  expect "busy without queue" (rejected (Lwt_main.run next) && not !entered);
  Lwt.cancel first;
  expect "cancel releases reader" (not (Lwt_mutex.is_locked lock));
  expect "reader reusable" (Lwt_main.run (Page.admit lock (fun () -> Lwt.return (Ok ()))) = Ok ());
  let failed = Page.admit lock (fun () -> Lwt.fail Exit) in
  expect "exception releases reader" (match Lwt.state failed with
    | Lwt.Fail Exit -> not (Lwt_mutex.is_locked lock) | _ -> false)

let field field value encoder =
  Pbrt.Encoder.uint64_as_varint (`unsigned value) encoder;
  Pbrt.Encoder.key field Pbrt.Varint encoder

let payload ?limit start =
  let encoder = Pbrt.Encoder.create () in
  Option.iter (fun value -> field 2 value encoder) limit;
  field 1 start encoder;
  Pbrt.Encoder.to_bytes encoder

let meta = Octra_node_runtime.Rpc_http.{
  rpc_peer = "local"; rpc_user_agent = "epoch-page-test"; rpc_body_bytes = 0;
}

let test_proto () =
  expect "protobuf defaults" (Proto.decode_page (Bytes.empty) = Ok (request 0));
  expect "protobuf request" (Proto.decode_page (payload ~limit:3L 2L) = Ok (request ~limit:3 2));
  expect "uint64 range" (rejected (Proto.decode_page (payload Int64.min_int)));
  expect "zero limit" (rejected (Proto.decode_page (payload ~limit:0L 0L)));
  expect "field type" (rejected (Proto.decode_page (Bytes.of_string "\010\000")));
  let encoder = Pbrt.Encoder.create () in
  let text tag value encoder =
    Pbrt.Encoder.string value encoder; Pbrt.Encoder.key tag Pbrt.Bytes encoder
  in
  Pbrt.Encoder.nested (fun () encoder -> text 3 (root 9) encoder) () encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder;
  Pbrt.Encoder.nested (fun () encoder -> field 2 9L encoder; text 1 "octra-test" encoder) () encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder;
  expect "message fields merge" (Proto.decode_page (Pbrt.Encoder.to_bytes encoder)
    = Ok (request ~anchor:(anchor 9) 0));
  let fields = ref [] in
  let handler params () =
    fields := params :: !fields;
    match Page.parse params with
    | Error error -> Lwt.return (Error error)
    | Ok request -> Lwt.map (Result.map Page.json)
        (Page.read ~head:(Some (anchor 9)) ~load request)
  in
  let call meta request = Dispatch.handle_request meta request () ["octra_epochPage", handler] in
  let reply = Lwt_main.run (Service.invoke ~call ~meta
    ~path:"/octra.node.v1.Node/Epochs" (payload ~limit:3L 2L)) in
  expect "shared route" (!fields = [Page.request_json (request ~limit:3 2)]);
  expect "typed reply" (reply.status.code = Status.Ok);
  let bytes = Option.get reply.body |> Pbrt.Decoder.of_string in
  let rec decode epochs next stop =
    match Pbrt.Decoder.key bytes with
    | None -> List.rev epochs, next, stop
    | Some (2, Pbrt.Bytes) ->
      let row = Pbrt.Decoder.nested bytes in
      expect "row first field" (Pbrt.Decoder.key row = Some (1, Pbrt.Varint));
      let `unsigned epoch = Pbrt.Decoder.uint64_as_varint row in
      let rec rest () = match Pbrt.Decoder.key row with
        | None -> ()
        | Some (4, Pbrt.Varint) ->
          let `unsigned txid = Pbrt.Decoder.uint64_as_varint row in
          expect "protobuf txid precision" (txid = Int64.add 9_007_199_254_740_993L epoch); rest ()
        | Some (6, Pbrt.Bits64) ->
          expect "protobuf time" (Pbrt.Decoder.float_as_bits64 row = Int64.to_float epoch +. 0.25);
          rest ()
        | Some (_, kind) -> Pbrt.Decoder.skip row kind; rest ()
      in rest (); decode (epoch :: epochs) next stop
    | Some (3, Pbrt.Varint) ->
      let `unsigned next = Pbrt.Decoder.uint64_as_varint bytes in decode epochs next stop
    | Some (4, Pbrt.Varint) ->
      let `unsigned stop = Pbrt.Decoder.uint64_as_varint bytes in decode epochs next stop
    | Some (_, kind) -> Pbrt.Decoder.skip bytes kind; decode epochs next stop
  in
  expect "wire fields and order" (decode [] 0L 0L = ([2L; 3L; 4L], 5L, 2L));
  let calls = List.length !fields in
  let invalid = Lwt_main.run (Service.invoke ~call ~meta
    ~path:"/octra.node.v1.Node/Epochs" (payload ~limit:65L 0L)) in
  expect "invalid proto before RPC" (invalid.status.code = Status.Invalid_argument
    && List.length !fields = calls);
  let invalid_reply = Lwt_main.run (Service.invoke
    ~call:(fun _ _ -> Lwt.return (Rpc.Result (`Null, `Null))) ~meta
    ~path:"/octra.node.v1.Node/Epochs" Bytes.empty) in
  expect "invalid reply is not success" (invalid_reply.status.code = Status.Internal);
  let good = run (request ~limit:3 2) |> page in
  let broken = { good with rows = List.rev good.rows } in
  expect "reply order validated" (rejected (Page.of_json (Page.json broken)));
  let routes = Octra_node_runtime.Status_read_rpc.core_dispatch { status_read = Fun.id } in
  expect "node route registered" (List.mem_assoc "octra_epochPage" routes)

let test_journal () =
  if not (Sys.file_exists "runtime_data") then Unix.mkdir "runtime_data" 0o700;
  let dir = Printf.sprintf "runtime_data/epoch-page-%d-%Ld" (Unix.getpid ()) (Mtime_clock.elapsed_ns ()) in
  Unix.mkdir dir 0o700;
  let path = Filename.concat dir "epochs.dat" in
  let journal = Journal.open_log path in
  Fun.protect ~finally:(fun () -> Journal.close journal) (fun () ->
    let first = row 0 in
    let header = Journal.{ empty_epoch_header with
      state_root = first.root; start_txid = first.tx_start;
      tx_count = first.tx_count; finalized_at = first.time;
    } in
    ignore (Journal.append journal header);
    let raw = Journal.epoch_to_json header in
    expect "journal read" (Journal.read_entry journal ~max_bytes:Page.max_record 0 = Ok (Some raw));
    expect "journal size" (Journal.read_entry journal ~max_bytes:1 0 = Error `Limit);
    expect "journal missing" (Journal.read_entry journal ~max_bytes:Page.max_record 1 = Ok None);
    let load ~max_bytes epoch = Journal.read_entry journal ~max_bytes epoch
      |> Result.map_error (fun _ -> Rpc.err (-32012) "journal error" None)
    in
    let result = run ~head:(Some (anchor 0)) ~load (request 0) |> page in
    expect "journal page" (result.stop = Page.Complete && result.rows = [first]);
    let offset = (Unix.fstat journal.fd).Unix.st_size - 1 in
    ignore (Unix.lseek journal.fd offset Unix.SEEK_SET);
    expect "checksum byte read" (Unix.read journal.fd (Bytes.create 1) 0 1 = 1);
    ignore (Unix.lseek journal.fd offset Unix.SEEK_SET);
    let last = (Journal.checksum raw).[3] in
    expect "checksum byte changed"
      (Unix.write journal.fd (Bytes.make 1 (Char.chr (Char.code last lxor 1))) 0 1 = 1);
    expect "checksum mismatch" (Journal.read_entry journal ~max_bytes:Page.max_record 0 = Error `Invalid))

let listen port =
  let module Config = Octra_node_runtime.Grpc_config in
  let module Http2 = Octra_node_runtime.Grpc_http2 in
  let env = function
    | "OCTRA_GRPC_ENABLE" -> Some "true"
    | "OCTRA_GRPC_PORT" -> Some port
    | _ -> None
  in
  let config = match Config.of_env env with
    | Ok (Config.Enabled config) -> config
    | Ok Config.Disabled -> failwith "test listener is disabled"
    | Error reason -> failwith reason
  in
  let lock = Lwt_mutex.create () in
  let call _ request =
    let open Lwt.Syntax in
    let* value =
      if request.Rpc.method_ = "octra_epochPage" then
        match Page.parse request.params with
        | Error error -> Lwt.return (Error error)
        | Ok request ->
          let* result = Page.admit lock (fun () ->
            Page.read ~head:(Some (anchor 9)) ~load request) in
          Lwt.return (Result.map Page.json result)
      else
        let* () = Lwt_unix.sleep 0.05 in
        Lwt.return (Ok (`Assoc ["method", `String request.method_; "params", request.params]))
    in
    Lwt.return (match value with
      | Ok value -> Rpc.Result (value, request.id)
      | Error error -> Rpc.Error_ (error, request.id))
  in
  let submit meta request =
    let validate tx =
      if tx.Octra_core.Transaction.nonce = 7 then Ok (String.make 64 'a')
      else Error ("invalid_nonce", "test nonce differs")
    in
    let route params () = Octra_node_runtime.Submit_rpc.submit ~validate params in
    Octra_node_runtime.Rpc_dispatch.handle_request meta request () ["octra_submit", route]
  in
  Lwt_main.run (Lwt.pick [Http2.start config ~call ~submit; Lwt_unix.sleep 60.0])

let tests () =
  test_pages ();
  test_gaps ();
  test_roots ();
  test_partition ();
  test_limits ();
  test_cancel ();
  test_admission ();
  test_proto ();
  test_journal ();
  print_endline "status = pass test = epoch_page"

let () = match Array.to_list Sys.argv with
  | [_] -> tests ()
  | [_; "--listen"; port] -> listen port
  | _ -> failwith "usage: test_epoch_page [--listen port]"