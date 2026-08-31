open Octra_core

module View = Octra_node_runtime.Tx_view
module Dispatch = Octra_node_runtime.Rpc_dispatch
module Http = Octra_node_runtime.Rpc_http

let cap = Rpc.max_admits

let check name value =
  if not value then failwith name

let sender index =
  Printf.sprintf "oct%044d" index

let tx ?(fee = 1_000) ?message ?(op_type = Transaction.Standard) from nonce =
  Transaction.{
    from;
    to_ = "oct99999999999999999999999999999999999999999999";
    amount = Z.zero;
    nonce;
    ou = Z.of_int fee;
    timestamp = 0.;
    signature = from ^ string_of_int nonce;
    public_key = None;
    message;
    op_type;
    encrypted_data = None;
  }

let lookup _ =
  Some (Z.of_int 1_000_000_000, 0)

let add ?(ou_limit = Tx_staging.max_ou) ~limit value =
  Tx_staging.add_smart ~ou_limit ~tx_limit:limit ~lookup value

let fill limit =
  for index = 1 to limit do
    add ~limit (tx (sender index) 1) |> Result.get_ok |> ignore
  done

let rpc method_ params id =
  `Assoc [
    "jsonrpc", `String "2.0";
    "method", `String method_;
    "params", params;
    "id", `Int id;
  ]

let batch_params count =
  `List [`List (List.init count (fun _ -> `Null))]

let parse json =
  Rpc.parse_body (Yojson.Safe.to_string json)

let check_denied count result =
  let expected =
    Printf.sprintf
      "admission attempts = %d limit = %d"
      count
      cap
  in
  match result with
  | Error error ->
    check "admission error code" (error.Rpc.code = -32602);
    check "admission error count" (error.data = Some (`String expected))
  | Ok _ ->
    failwith "admission limit accepted"

let check_rpc_limit () =
  let left = cap / 2 in
  let right = cap - left in
  let allowed =
    `List [
      rpc "octra_submitBatch" (batch_params left) 1;
      rpc "octra_submitBatch" (batch_params right) 2;
    ]
  in
  check "one hundred admissions pass" (Result.is_ok (parse allowed));
  let over = cap + 1 in
  let denied =
    `List [
      rpc "octra_submitBatch" (batch_params left) 1;
      rpc "octra_submitBatch" (batch_params (right + 1)) 2;
    ]
  in
  check_denied over (parse denied);
  let multiplied =
    `List
      (List.init Rpc.max_batch_size (fun index ->
         rpc "octra_submitBatch" (batch_params cap) index))
  in
  check_denied (Rpc.max_batch_size * cap) (parse multiplied);
  let singles =
    `List
      (List.init cap (fun index ->
         rpc "octra_submit" (`List [`Null]) index))
  in
  check "one hundred singles pass" (Result.is_ok (parse singles));
  let private_calls =
    List.init (cap - 1) (fun index ->
      rpc "octra_privateTransfer" (`List [`Null]) index)
  in
  let mixed =
    `List
      (private_calls
       @ [rpc "octra_submitBatch" (batch_params 2) cap])
  in
  check_denied (cap + 1) (parse mixed);
  check
    "single submit batch passes"
    (Result.is_ok (parse (rpc "octra_submitBatch" (batch_params cap) 1)));
  check_denied
    over
    (parse (rpc "octra_submitBatch" (batch_params over) 1))

let check_inner_limit () =
  check "inner batch limit passes"
    (Result.is_ok (View.submit_batch_params (batch_params cap)));
  let over = cap + 1 in
  match View.submit_batch_params (batch_params over) with
  | Error error ->
    check "inner batch error code" (error.Rpc.code = -32602);
    let expected =
      Printf.sprintf
        "batch entries = %d limit = %d"
        over
        cap
    in
    check "inner batch error count"
      (error.data = Some (`String expected))
  | Ok _ ->
    failwith "inner batch limit accepted"

let check_dispatch_limit () =
  let calls = ref 0 in
  let handler _ () =
    incr calls;
    Lwt.return (Ok `Null)
  in
  let routes = ["octra_submitBatch", handler] in
  let meta : Http.meta = {
    rpc_peer = "test";
    rpc_user_agent = "test";
    rpc_body_bytes = 0;
  } in
  let multiplied =
    `List
      (List.init Rpc.max_batch_size (fun index ->
         rpc "octra_submitBatch" (batch_params cap) index))
    |> Yojson.Safe.to_string
  in
  Dispatch.process_body meta multiplied () routes |> Lwt_main.run |> ignore;
  check "denied batch skips handler" (!calls = 0);
  let allowed =
    rpc "octra_submitBatch" (batch_params cap) 1
    |> Yojson.Safe.to_string
  in
  Dispatch.process_body meta allowed () routes |> Lwt_main.run |> ignore;
  check "allowed batch reaches handler" (!calls = 1)

let check_full_reject () =
  let limit = 100_000 in
  Tx_staging.clear ();
  fill limit;
  let payload = String.make 32_768 'x' in
  let message =
    Yojson.Safe.to_string
      (`Assoc ["calls", `List []; "pad", `String payload])
  in
  let value =
    tx
      ~fee:2_000
      ~message
      ~op_type:Transaction.MultiExec
      (sender (limit + 1))
      1
  in
  let started = Unix.gettimeofday () in
  let result = add ~limit value in
  let elapsed_ms = (Unix.gettimeofday () -. started) *. 1_000. in
  check
    "full pool rejects equal fee rate"
    (result = Error "staging full (insufficient evictable capacity)");
  check "full rejection deadline" (elapsed_ms < 100.);
  let repeated_at = Unix.gettimeofday () in
  for _ = 1 to cap do
    check "replayed full rejection"
      (add ~limit value
       = Error "staging full (insufficient evictable capacity)")
  done;
  let repeated_ms = (Unix.gettimeofday () -. repeated_at) *. 1_000. in
  check "request rejection deadline" (repeated_ms < 500.);
  let better = tx ~fee:4_000 (sender (limit + 2)) 1 in
  check "better fee rate enters full pool" (Result.is_ok (add ~limit better));
  check "full pool size stays fixed" (Tx_staging.staging_size () = limit);
  Printf.printf
    "status = pass entries = %d reject_ms = %.3f request_ms = %.3f\n"
    limit
    elapsed_ms
    repeated_ms

let check_rbf_cap () =
  let limit = 10 in
  let ou_limit = Z.of_int 200_000 in
  Tx_staging.clear ();
  add ~ou_limit ~limit (tx (sender 7_000) 1) |> Result.get_ok |> ignore;
  add ~ou_limit ~limit (tx (sender 7_001) 1) |> Result.get_ok |> ignore;
  let replacement =
    tx
      ~fee:1_000_000
      ~op_type:Transaction.ProgramDeploy
      (sender 7_000)
      1
  in
  check
    "replacement respects ou cap"
    (add ~ou_limit ~limit replacement
     = Error "staging full (insufficient evictable capacity)");
  check "replacement leaves pool unchanged" (Tx_staging.staging_size () = 2);
  check "replacement leaves ou unchanged"
    (Z.equal (Tx_staging.staging_total_ou ()) (Z.of_int 2_000))

let check_evict_cap () =
  let limit = 10 in
  let ou_limit = Z.of_int 3_500 in
  Tx_staging.clear ();
  for index = 1 to 3 do
    add ~ou_limit ~limit (tx (sender (8_000 + index)) 1)
    |> Result.get_ok
    |> ignore
  done;
  let value =
    tx
      ~fee:6_000
      ~message:"[]"
      ~op_type:Transaction.MultiExec
      (sender 8_100)
      1
  in
  check "one admission cannot evict two entries"
    (add ~ou_limit ~limit value
     = Error "staging full (insufficient evictable capacity)");
  check "eviction cap leaves pool unchanged" (Tx_staging.staging_size () = 3);
  check "eviction cap leaves ou unchanged"
    (Z.equal (Tx_staging.staging_total_ou ()) (Z.of_int 3_000))

let () =
  check_full_reject ();
  check_rbf_cap ();
  check_evict_cap ();
  check_rpc_limit ();
  check_inner_limit ();
  check_dispatch_limit ()