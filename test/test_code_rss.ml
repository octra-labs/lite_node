(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module S = C.Store_irmin
module E = C.Epoch_exec
module R = C.Rule_graph
module T = C.Transaction
module Read = Octra_node_runtime.Circle_read_rpc
module V = Octra_vm

external rss : unit -> int64 = "octra_test_rss"

let run = Lwt_main.run
let need reason value = if not value then failwith reason
let owner = "oct" ^ String.make 44 '1'
let chain_id = "octra-devnet-9871-cluster"
let epoch = (Option.get (R.proof_key_activation_for_chain chain_id)).activation_epoch
let field key value = Yojson.Safe.Util.member key value
let ok = function Ok value -> value | Error reason -> failwith reason

let codes = Array.init 8 (fun index -> Base64.encode_exn (Code_rss_input.make index))

let payload code = C.Circles.{
  runtime = Wasm_v1; privacy_class = Public; browser_mode = Native_sealed;
  resource_mode = Public_resources; code_b64 = Some code;
  policy_hash = None; members_root = None; export_policy = None; limits = default_limits;
}

let fold epoch = E.prior_fold epoch |> Result.map (fun state ->
  {state with E.proof_key = R.Active; proof_exec = R.Active; vm_bytes = R.Active;
    wasm_float = R.Active})

let profile ~epoch = Ok V.Contract_rpc.{
  epoch; point_ops = false; math = false; object_cost = false;
  int_work = V.Int_work.Prior; fhe_work = R.Prior;
  proof_exec = R.Active; proof_key = R.Active; wasm_float = R.Active;
}

let transact backend nonce target op_type message =
  let tx = T.{from = owner; to_ = target; amount = Z.zero; nonce; ou = Z.of_int 100_000;
    timestamp = 1.; signature = ""; public_key = None; op_type;
    encrypted_data = if op_type = T.CircleCall then Some "update" else None;
    message = Some (Yojson.Safe.to_string message)} in
  let env = E.{chain_id; epoch_id = epoch; proposer_addr = owner;
    validator_addrs = []; validator_pubkeys = []; prev_state_root = String.make 32 '\000';
    epoch_ts = 1.; ready_state_root_at = None; ready_max_lag = -1} in
  run (backend.E.begin_batch R.Prior);
  begin match run (Octra_node_runtime.Consensus_vm_transition.process_tx
    ~backend ~env ~circle_mode:R.Prior ~wasm_compute_mode:R.Active
    ~program_trust:V.Program_trust.empty ~object_cost:false tx) with
  | Ok (E.Confirmed _) -> ()
  | Ok (E.Rejected_after_fee error) -> failwith (error.error_type ^ ": " ^ error.reason)
  | Error (kind, reason) -> failwith (kind ^ ": " ^ reason)
  end;
  run (backend.commit_batch ());
  need "apply did not consume nonce" ((Option.get (backend.ops.find_opt owner)).nonce = nonce)

let replay code =
  let step = `Assoc [
    "action", `String "execute"; "code_b64", `String code;
    "execution_profile", `String "compute"; "is_view", `Bool true;
    "caller", `String owner; "address", `String "rss";
    "compute_session_scope", `String (String.make 64 'a');
    "export_name", `String "octra_query"; "request_b64", `String "";
    "fuel_limit", `Int 100_000; "hfhe_strict", `Bool true;
    "table_limit", `Bool true; "stable_errors", `Bool true; "code_limit", `Bool true;
  ] |> Yojson.Safe.to_string in
  let input = `Assoc ["action", `String "compute_replay"; "is_view", `Bool true;
    "execution_profile", `String "compute";
    "replay_steps", `List [`String step; `String step]] |> Yojson.Safe.to_string in
  let raw = C.Circle_wasm_native.run_json input |> ok in
  let replies = Yojson.Safe.from_string raw |> field "replies" |> Yojson.Safe.Util.to_list in
  need "replay reply count differs" (List.length replies = 2);
  List.iter (fun reply ->
    need "replay failed" (field "compute_exit" reply = `Int 0);
    need "replay response differs"
      (field "response_b64" reply = `String (Base64.encode_exn "OCWS1\003\000\000\000\0017"))) replies

let measure route call =
  let cycles = match Sys.getenv_opt "OCTRA_RSS_CYCLES" with
    | None -> 10
    | Some raw -> int_of_string raw in
  need "RSS cycles must be from 10 to 1000" (cycles >= 10 && cycles <= 1000);
  let warm = ref 0L in
  let low = ref Int64.max_int in
  let high = ref 0L in
  for cycle = 1 to cycles do
    for index = 0 to Array.length codes - 1 do call index done;
    let raw = rss () in
    Gc.compact ();
    let bytes = rss () in
    if cycle <= 2 then warm := Int64.max !warm bytes
    else begin low := Int64.min !low bytes; high := Int64.max !high bytes end;
    Printf.printf "event = code_rss route = %s cycle = %d calls = %d raw = %Ld rss = %Ld\n%!"
      route cycle (cycle * Array.length codes) raw bytes;
    if cycle > 2 then need "compiled code RSS keeps growing"
      (Int64.sub bytes !warm <= 67_108_864L)
  done;
  Printf.printf "event = code_rss route = %s status = pass warm = %Ld low = %Ld high = %Ld growth = %Ld\n%!"
    route !warm !low !high (Int64.sub !high !warm)

let stored route path =
  let store = run (S.open_store (Filename.concat path "store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let ledger = C.Ledger.create store in
    C.Ledger.add_account ledger owner (Z.of_int 1_000_000_000) |> ok;
    let backend = E.make_live_backend ~fold store ledger in
    let targets = Array.mapi (fun index code ->
      let data = payload code in
      let nonce = index + 1 in
      let target = C.Circles.circle_id_of_deploy ~deployer:owner ~nonce data in
      transact backend nonce target T.CircleDeploy (C.Circles.yojson_of_deploy_payload data);
      target) codes in
    run (S.set_meta store "last_epoch" (string_of_int (epoch - 1)));
    run (C.Ledger.flush_dirty_lwt ledger);
    run (S.commit_epoch_batch store "RSS setup");
    let unused _ _ _ = failwith "unexpected RPC route" in
    let routes = Read.dispatch Read.{
      store_read = unused; program_read = unused; epoch_read = unused;
      public_asset_read = unused;
      circle_view = (fun params store -> Read.view_rpc ~authenticated:false
        ~trusted:[] ~profile ~store ~ledger params);
      circle_view_auth = (fun _ _ -> failwith "unexpected private RPC");
    } in
    let nonce = ref (Array.length codes) in
    measure route (fun index ->
      if route = "view" then begin
        let params = `List [`String targets.(index); `String "query"; `List []] in
        let result = run ((List.assoc "octra_circleView" routes) params store) in
        let reply = match result with
          | Ok value -> value
          | Error error -> failwith error.C.Rpc.message in
        need "public view result differs" (field "result" reply = `String "7")
      end else begin
        incr nonce;
        transact backend !nonce targets.(index) T.CircleCall (`List [])
      end))

let () =
  need "expected view, apply or replay" (Array.length Sys.argv = 2);
  let route = Sys.argv.(1) in
  need "unknown RSS route" (List.mem route ["view"; "apply"; "replay"]);
  Printf.printf "event = code_rss route = %s modules = %d module_bytes = %d\n%!"
    route (Array.length codes) (String.length (Base64.decode_exn codes.(0)));
  if route = "replay" then measure route (fun index -> replay codes.(index))
  else Test_workspace.with_dir ("code_rss_" ^ route) (stored route)