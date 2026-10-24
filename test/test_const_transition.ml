(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module G = C.Rule_graph
module E = C.Epoch_exec
module P = Octra_vm.Program_package
module S = C.Store_irmin
module T = C.Transaction

let run = Lwt_main.run
let check label value = if not value then failwith label
let ok = function Ok value -> value | Error reason -> failwith reason
let owner = "oct" ^ String.make 44 '1'
let chain_id = "octra-devnet-9871-cluster"
let plan = Option.get (G.program_records_activation_for_chain chain_id)
let source constants result = "program Constants { " ^ constants
  ^ " public fn get(): int { return " ^ result ^ " } }"

let compile body =
  P.compile_at ~loops:true ~compiler:P.Preview ~point_ops:true ~main:"main.aml"
    ~sources:[P.{path = "main.aml"; body}]
  |> Result.map_error P.error_message |> ok

let package = compile (source "" "7")

let replace body =
  let bytes = Bytes.of_string package.package in
  let count_at = 10 + Bytes.get_uint16_le bytes 8 in
  check "source count differs" (Bytes.get_uint16_le bytes count_at = 1);
  let length_at = count_at + 4 + Bytes.get_uint16_le bytes (count_at + 2) in
  let body_at = length_at + 4 in
  let after = body_at + Int32.to_int (Bytes.get_int32_le bytes length_at) in
  let prefix = Bytes.sub bytes 0 body_at in
  Bytes.set_int32_le prefix length_at (Int32.of_int (String.length body));
  Bytes.to_string prefix ^ body ^ String.sub package.package after
    (String.length package.package - after)

let apply ~preview epoch (payload, envelope) =
  Test_workspace.with_dir "const_transition" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let graph = G.create ~chain_id ~root_at:(fun epoch ->
        check "constant anchor differs" (epoch = plan.anchor_epoch);
        G.Root plan.anchor_state_root) in
      let fold epoch =
        match G.program_records graph ~epoch with
        | Error error -> Error (G.fault_message error)
        | Ok program_records -> E.prior_fold epoch |> Result.map (fun value ->
          {value with E.program_records; program_mode = G.Active;
            proof_exec = G.Active; fhe_work = G.Active; vm_bytes = G.Active}) in
      let ledger = C.Ledger.create store in
      C.Ledger.add_account ledger owner (Z.of_int 10_000_000) |> ok;
      run (C.Ledger.flush_dirty_lwt ledger);
      let backend = if preview then
          E.make_overlay_backend ~fold store ledger (C.Ledger.Overlay.create ledger)
        else E.make_live_backend ~fold store ledger in
      run (backend.begin_batch G.Prior);
      let tx = T.{from = owner;
        to_ = Octra_vm.Contract.addr_from_code envelope owner 1;
        amount = Z.zero; nonce = 1; ou = Z.of_int 1_000_000; timestamp = 1.;
        signature = ""; public_key = None; message = Some "[]";
        op_type = ProgramDeploy; encrypted_data = Some (Base64.encode_exn payload)} in
      let env = E.{chain_id; epoch_id = epoch; proposer_addr = owner;
        validator_addrs = []; validator_pubkeys = []; prev_state_root = String.make 64 'a';
        epoch_ts = 1.; ready_state_root_at = None; ready_max_lag = -1} in
      let result = run (Octra_node_runtime.Consensus_vm_transition.process_tx
        ~backend ~env ~circle_mode:G.Prior ~wasm_compute_mode:G.Prior
        ~program_trust:Octra_vm.Program_trust.empty ~object_cost:false tx) in
      let account = Option.get (backend.ops.find_opt owner) in
      let stored = run (S.get_contract_info store tx.to_) in
      result, account.balance, account.nonce, Option.is_some stored))

let outcome = function
  | Ok (E.Confirmed fee) -> "confirmed", Z.to_string fee
  | Ok (E.Rejected_after_fee error) -> error.error_type, error.reason
  | Error (kind, reason) -> kind, reason

let () =
  List.iter (fun depth ->
    let locals = List.init depth (fun i -> Printf.sprintf "let a%d = %s" i
      (if i = 0 then "1" else Printf.sprintf "(a%d, a%d)" (i - 1) (i - 1)))
      |> String.concat " " in
    let body = "program Types { public fn get(): int { " ^ locals ^ " return 1 } }" in
    let payload = replace body, package.envelope in
    List.iter (fun epoch ->
      let live, balance, nonce, stored = apply ~preview:false epoch payload in
      let preview, other_balance, other_nonce, _ = apply ~preview:true epoch payload in
      check "shared type preview differs" (outcome live = outcome preview);
      check "shared type account differs" (balance = other_balance && nonce = other_nonce);
      check "shared type changed envelope accepted" (not stored);
      check ("shared type compiler result differs: " ^ snd (outcome live)) (snd (outcome live) =
        "Program source and envelope mismatch"))
      [plan.activation_epoch - 1; plan.activation_epoch]) [8; 45];
  let definitions = List.init 28 (fun i -> Printf.sprintf "const C%d = %s" i
    (if i = 0 then "1" else Printf.sprintf "C%d + C%d" (i - 1) (i - 1)))
    |> String.concat " " in
  let doubling = replace (source definitions "C27"), package.envelope in
  List.iter (fun epoch -> List.iter (fun preview ->
    let result, _, _, stored = apply ~preview epoch doubling in
    check "constant DAG reached deployment" (not stored);
    check "constant DAG did not reach finite compiler refusal"
      (snd (outcome result) = "Program compile failed: source = main.aml line 1 column 1: register capacity exceeded maximum = 64")
  ) [false; true]) [plan.activation_epoch - 1; plan.activation_epoch];
  let valid = compile (source "const A: int = 7" "A") in
  let unused = compile (source "const A: int = A" "7") in
  let used = replace (source "const A: int = 1 + A" "A"), package.envelope in
  List.iter (fun epoch ->
    List.iter (fun (body, accepted) ->
      let live, balance, nonce, stored = apply ~preview:false epoch body in
      let preview, next_balance, next_nonce, _ = apply ~preview:true epoch body in
      check "preview outcome differs" (outcome live = outcome preview);
      check "preview account differs" (balance = next_balance && nonce = next_nonce);
      check ("program presence differs epoch = " ^ string_of_int epoch
        ^ " reason = " ^ snd (outcome live)) (stored = accepted);
      if accepted then check "valid deploy refused" (fst (outcome live) = "confirmed")
      else begin
        let expected = if epoch < plan.activation_epoch then
            "Program compile failed: source = main.aml line 1 column 1: register capacity exceeded maximum = 64"
          else "Program compile failed: constant dependency is cyclic" in
        check ("constant refusal differs: " ^ snd (outcome live))
          (outcome live = ("program_deploy_rejected", expected))
      end;
      check "deploy fee differs" (balance = Z.of_int 9_000_000);
      check "deploy nonce differs" (nonce = 1))
      [(valid.package, valid.envelope), true;
       (unused.package, unused.envelope), epoch < plan.activation_epoch;
       used, false])
    [plan.activation_epoch - 1; plan.activation_epoch; plan.activation_epoch + 1];
  Printf.printf "status = pass test = const_transition\n%!"