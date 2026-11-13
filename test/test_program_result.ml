(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Source_case

let source = {|
program PaidCall {
  state { counter: int }
  constructor() { self.counter = 0 }
  fn init() { self.counter = 0 }
  fn exhaust(): int {
    while true { self.counter += 1 }
    return self.counter
  }
  payable fn pay(to: address, refuse: bool) {
    self.counter += 1
    require(transfer(to, 1), "payment refused")
    require(!refuse, "caller refused")
  }
}
|}

let charge_cases () =
  List.iter (fun program -> List.iter (fun compute ->
    List.iter (fun operation -> List.iter (fun error_type ->
      let expected = match operation with
        | T.ComputeFund | ComputeReturn | ComputeSettle -> compute = R.Active
        | T.ProgramExec -> program = R.Active && error_type = "program_exec_failed"
        | T.CircleCall -> program = R.Active && error_type = "circle_call_failed"
        | _ -> false in
      need "charged operation differs" (C.Tx_envelope.charged_operation ~compute ~program
        ~error_type operation = expected);
      let tx = transaction ~timestamp:0. ~public:"key" ~sender:"sender" ~target:"program"
        ~nonce:1 ~fee:1000 ~operation ~payload:"pay" ~params:(`List []) in
      let receipt = C.Tx_outcome.{position = 0; tx; error_type; reason = "refused"; charged = true} in
      need "charged receipt accepted outside rule" (Result.is_ok
        (C.Tx_envelope.check_charged ~program compute [receipt]) = expected))
      ["program_exec_failed"; "program_exec_exception"; "circle_call_failed";
       "circle_call_exception"; "circle_call_commit_failed"; "malformed_transaction";
       "nonce_error"; "host_unavailable"; "memory"; "time"])
      [T.ComputeFund; ComputeReturn; ComputeSettle; ProgramExec; ProgramDeploy;
       ContractCall; CircleCall; MultiExec; Standard]) [R.Prior; R.Active]) [R.Prior; R.Active]

let policy enabled =
  let mode = if enabled then R.Active else R.Prior in
  need "result schedule differs" (R.program_result_at ~chain_id:chain ~epoch = mode);
  need "result graph differs" (R.program_result (rules ()) ~epoch = Ok mode);
  List.iter (fun chain_id ->
    need "result enabled on other chain" (R.program_result_activation_for_chain chain_id = None))
    ["octra-mainnet"; "octra-test"; ""; chain ^ " "];
  let plan = R.{anchor_epoch = 17; anchor_state_root = String.make 64 'a'; activation_epoch = 23} in
  let order result plans = match plans with
    | [program; envelope; bytes] -> R.program_result_order ~result ~program ~envelope ~bytes
    | _ -> failwith "result dependency count differs" in
  let names = ["program_source"; "tx_envelope"; "vm_bytes"] in
  need "disabled result requires dependencies" (order None [None; None; None] = Ok ());
  need "equal result dependencies refused" (order (Some plan) [Some plan; Some plan; Some plan] = Ok ());
  List.iteri (fun index name -> List.iter (fun (value, prefix) ->
    let plans = List.mapi (fun at _ -> if at = index then value else Some plan) names in
    need "result dependency accepted" (order (Some plan) plans = Error (prefix ^ name)))
    [None, "program_result requires "; Some {plan with activation_epoch = 24}, "program_result precedes "]) names;
  charge_cases ();
  if enabled then begin
    need "result epoch missing" (List.mem epoch (R.profile_epochs ~chain_id:chain));
    need "result active early" (R.program_result_at ~chain_id:chain ~epoch:(epoch - 1) = R.Prior);
    List.iter (fun value ->
      let bad = R.create ~chain_id:chain ~root_at:(fun at -> if at = 1_950_000 then value else roots at) in
      need "result anchor accepted" (Result.is_error (Set_rule.policy bad epoch)))
      [R.Missing; R.Root (String.make 64 'b'); R.Unreadable "read failed"];
    let before = Consensus_profile.hash ~chain_id:chain ~epoch:(epoch - 1) (fun _ -> None) in
    let after = Consensus_profile.hash ~chain_id:chain ~epoch (fun _ -> None) in
    need "result profile did not change" (before <> after);
    need "result switch missing" (Consensus_profile.switch_after ~chain_id:chain ~applied_epoch:(epoch - 1))
  end

let flow ?(circle = false) output enabled =
  let epoch = if not circle then epoch else
    let activation = (Option.get (R.program_result_activation_for_chain chain)).activation_epoch in
    if enabled then activation else activation - 32 in
  let key, public, owner = identity 180 in
  let _, payee_public, payee = identity 181 in
  let _, leader_public, leader = identity 182 in
  let package = compile source in
  let compiler_key, compiler_public, _ = identity 183 in
  let program_trust = V.Program_trust.of_env (fun name ->
    if name = "OCTRA_PROGRAM_RELEASE_KEYS" then Some ("result-test = " ^ compiler_public)
    else None) |> unwrap in
  let envelope = V.Program_envelope.decode package.envelope |> unwrap in
  let cert = V.Program_attestation.attach ~key_id:"result-test"
    ~private_key:(Base64.decode_exn compiler_key) envelope.cert |> unwrap in
  let circle_code = V.Program_envelope.encode ~code:envelope.code ~cert |> unwrap in
  let payload = C.Circles.{runtime = Octb; privacy_class = Public;
    browser_mode = Native_sealed; resource_mode = Public_resources;
    code_b64 = Some (Base64.encode_exn circle_code); policy_hash = None;
    members_root = None; export_policy = None; limits = default_limits} in
  let address = if circle then C.Circles.circle_id_of_deploy ~deployer:owner ~nonce:1 payload
    else V.Contract.addr_from_code package.envelope owner 1 in
  let now = Unix.gettimeofday () in
  let env = { (environment now leader) with E.epoch_id = epoch } in
  let state store = if circle then run (S.load_circle_stable_storage store address) |> unwrap
      |> S.sorted_storage_pairs
    else fields store address in
  let path = Filename.concat output "store" in
  let store = run (S.open_store ~fresh:true path) in
  let saved = Fun.protect ~finally:(fun () -> C.Tx_staging.clear (); run (S.close store)) (fun () ->
    let ledger = C.Ledger.create store in
    List.iter (fun (address, public) -> C.Ledger.add_account_with_pubkey ledger address initial public |> unwrap)
      [owner, public; payee, payee_public; leader, leader_public];
    run (C.Ledger.flush_dirty_lwt ledger);
    run (S.set_meta store "last_epoch" (string_of_int (epoch - 1)));
    run (S.set_meta store "total_supply" (Z.to_string (C.Ledger.get_total_supply ledger)));
    run (S.set_meta store "emission_remaining" "0");
    Source_proposal.initialize ~store ~ledger ~env;
    let account target = Option.get (C.Ledger.find_opt ledger target) in
    let funds target = Option.fold ~none:Z.zero ~some:(fun value -> value.C.Ledger_types.balance)
      (C.Ledger.find_opt ledger target) in
    let state () = state store in
    let block offset ?(paid = []) ?(refused = []) ?(deferred = []) ?effort expected =
      let env = {env with E.epoch_id = epoch + offset; epoch_ts = now +. float_of_int (offset * 10)} in
      let txs = expected @ List.map fst paid @ List.map fst refused @ deferred in
      C.Tx_staging.clear ();
      let queue = ref (C.Preverify_queue.create ()) in
      let preverify_admit tx = if not (C.Preverify_queue.needs_preverify tx) then Ok ()
        else match C.Preverify_queue.admit !queue tx with
        | C.Preverify_queue.Queued next -> queue := next; Ok ()
        | Duplicate -> Ok ()
        | Rejected reason -> Error reason in
      List.iter (fun tx -> match ingress ~preverify_admit ~at:env.epoch_id ledger tx with
        | Ok _ -> () | Error error -> failwith ("result RPC refused: " ^ error.C.Rpc.message)) txs;
      let request, execution, seconds = Source_proposal.check_many ~program_trust ~paid ~refused ~deferred ~progress:false
        ~store ~ledger ~env ~expected txs in
      let receipts = ref [] in
      let save_receipt_raw ~tx_hash ~json = receipts := (tx_hash, Yojson.Safe.from_string json) :: !receipts in
      need "result application differs"
        (Source_proposal.apply ~program_trust ~save_receipt_raw ~store ~ledger request = execution);
      Option.iter (fun (low, high) ->
        let tx = fst (List.hd paid) in
        let receipt = List.assoc (T.hash tx) !receipts in
        let actual = Yojson.Safe.Util.(receipt |> member "effort" |> to_int) in
        need "work refusal effort differs" (actual >= low && actual <= high)) effort;
      save (Filename.concat output ("block_" ^ string_of_int offset ^ ".json"))
        (`Assoc ["epoch", `Int env.epoch_id; "seconds", `Float seconds;
          "fees", `String (Z.to_string execution.artifacts.confirmed_fees);
          "paid", `Int (List.length paid); "refused", `Int (List.length refused);
          "deferred", `Int (List.length deferred)]);
      C.Tx_staging.clear ();
      execution in
    let make nonce fee payload params = transaction ~timestamp:now ~public ~sender:owner ~target:address
      ~nonce ~fee ~operation:(if circle then T.CircleCall else T.ProgramExec) ~payload ~params in
    let sign = sign ~at:epoch key in
    let deploy = transaction ~timestamp:now ~public ~sender:owner ~target:address ~nonce:1 ~fee:400_000
      ~operation:T.ProgramDeploy ~payload:(Base64.encode_exn package.package) ~params:(`List []) in
    let deploy = if circle then {deploy with T.op_type = T.CircleDeploy; encrypted_data = None;
      message = Some (C.Json_tree.write (C.Circles.yojson_of_deploy_payload payload))} else deploy in
    let deploy = sign deploy in
    if circle then begin match run (Consensus_circle_code_check.admit
      ~scalar_work:enabled ~resource_errors:true ~store ~program_trust
      ~point_ops:true deploy) with
      | Ok () -> ()
      | Error (code, reason) -> failwith (code ^ ": " ^ reason)
    end;
    ignore (block 0 [deploy]);
    if circle then ignore (block 1 [sign (make 2 1000 "init" (`List []))]);
    let block offset = block (offset + if circle then 1 else 0) in
    let first = if circle then 2 else 1 in
    let before = account owner and recipient = account payee and storage = state () in
    let supply = C.Ledger.get_total_supply ledger in
    let reward_before = (account leader).balance in
    let voters = List.init 4 (fun index ->
      let _, _, address = identity (210 + index) in address, (account address).balance) in
    let failed = make (first + 1) 1000 "pay" (`List [`String payee; `Bool true]) in
    let failed = sign {failed with T.amount = Z.of_int 10} in
    let interrupted = make (first + 1) 1000 "pay" (`List [`String payee; `Bool false]) in
    let interrupted = sign {interrupted with T.amount = Z.of_int 10} in
    List.iteri (fun index (phase, kind) ->
      let names = ["OCTRA_CHAOS_FAIL_AT"; "OCTRA_CHAOS_FAIL_KIND"] in
      let saved = List.map (fun name -> name, Sys.getenv_opt name) names in
      let execution = Fun.protect ~finally:(fun () -> List.iter (fun (name, value) ->
        Unix.putenv name (Option.value ~default:"" value)) saved) (fun () ->
        Unix.putenv "OCTRA_CHAOS_FAIL_AT" phase;
        Unix.putenv "OCTRA_CHAOS_FAIL_KIND" kind;
        block (index + 1) ~deferred:[interrupted] []) in
      need "local resource failure charged" (execution.artifacts.confirmed_fees = Z.zero);
      need "local resource failure kept account changes" (account owner = before && account payee = recipient);
      need "local resource failure kept program changes" (state () = storage && funds address = Z.zero))
      ["vm_commit_after_program", "host"; "vm_commit_after_value", "memory"];
    let reason = "execution reverted [caller refused]" in
    let execution = if enabled then block 3 ~paid:[failed, reason] []
      else block 3 ~refused:[failed, reason] [] in
    let paid = if enabled then failed.ou else Z.zero in
    need "result fee differs" ((account owner).balance = Z.sub before.balance paid);
    need "result nonce differs" ((account owner).nonce = first + if enabled then 1 else 0);
    need "result fee total differs" (execution.artifacts.confirmed_fees = paid);
    let leader_fee = Z.div (Z.mul paid (Z.of_int 7)) (Z.of_int 10) in
    let member_fee = Z.div (Z.sub paid leader_fee) (Z.of_int 4) in
    need "current proposer received parent reward" ((account leader).balance = reward_before);
    List.iteri (fun index (address, balance) ->
      let amount = if index = 0 then Z.add leader_fee member_fee else member_fee in
      need "result parent reward differs" ((account address).balance = Z.add balance amount)) voters;
    need "result public supply differs" (C.Ledger.get_total_supply ledger = supply);
    need "result emission reserve differs" (run (S.get_meta store "emission_remaining") = Some "0");
    need "refused transfer retained" (account payee = recipient);
    need "refused storage retained" (state () = storage);
    need "refused value retained" (funds address = Z.zero);
    if enabled then need "paid nonce replay accepted" (Result.is_error (ingress ~at:(epoch + 4) ledger failed));
    C.Tx_staging.clear ();
    let nonce = first + if enabled then 2 else 1 in
    let success = make nonce 1000 "pay" (`List [`String payee; `Bool false]) in
    let success = sign {success with T.amount = Z.of_int 10} in
    let changed = {success with T.ou = Z.succ success.ou} in
    need "changed fee signature accepted" (Result.is_error (ingress ~at:(epoch + 4) ledger changed));
    need "invalid fee request retained" (C.Tx_staging.staging_size () = 0);
    ignore (block 4 [success]);
    need "next nonce did not execute" ((account owner).nonce = nonce);
    need "success fee or amount differs"
      ((account owner).balance = Z.sub before.balance (Z.add paid (Z.of_int 1010)));
    need "successful transfer differs" ((account payee).balance = Z.succ recipient.balance);
    need "successful value differs" ((account address).balance = Z.of_int 9);
    need "successful storage differs" (List.assoc "counter" (state ()) = "1");
    if circle && enabled then begin
      List.iteri (fun index (method_name, params, reason) ->
        let before = account owner and storage = state () and balance = funds address in
        let refused = sign (make (before.nonce + 1) 1000 method_name params) in
        let execution = block (index + 5) ~paid:[refused, reason] ~effort:(990_000, 1_000_000) [] in
        need "work refusal fee differs" (execution.artifacts.confirmed_fees = refused.ou);
        need "work refusal debit differs" ((account owner).balance = Z.sub before.balance refused.ou);
        need "work refusal nonce differs" ((account owner).nonce = before.nonce + 1);
        need "work refusal state changed" (state () = storage && funds address = balance);
        need "work refusal nonce reused" (Result.is_error
          (ingress ~at:(epoch + index + 7) ledger refused));
        C.Tx_staging.clear ())
        ["exhaust", `List [], "execution reverted";
         "exhaust", `List [`Intlit (String.make 40_000 '1')], "circle input effort exceeds limit"];
      let before = account owner and storage = state () and balance = funds address in
      let created = C.Circles.circle_id_of_deploy ~deployer:owner ~nonce:(before.nonce + 1) payload in
      let success = sign {deploy with T.signature = ""; nonce = before.nonce + 1; to_ = created} in
      let refused = sign {(make (before.nonce + 2) 1000 "exhaust"
        (`List [`Intlit (String.make 40_000 '1')])) with T.to_ = created} in
      let execution = block 7 ~paid:[refused, "circle input effort exceeds limit"] [success] in
      let fees = Z.add success.ou refused.ou in
      need "dependent refusal fee differs" (execution.artifacts.confirmed_fees = fees);
      need "dependent refusal debit differs" ((account owner).balance = Z.sub before.balance fees);
      need "dependent refusal nonce differs" ((account owner).nonce = before.nonce + 2);
      need "dependent refusal lost creation" (Option.is_some (run (S.get_circle_info store created)));
      need "dependent refusal changed prior state" (state () = storage && funds address = balance)
    end;
    let result = [owner, account owner; payee, account payee; address, account address; leader, account leader] in
    result, state (), run (C.Ledger.hash ledger)) in
  let reopened = run (S.open_store path) in
  Fun.protect ~finally:(fun () -> run (S.close reopened)) (fun () ->
    let ledger = C.Ledger.create reopened in
    let accounts, storage, root = saved in
    List.iter (fun (address, account) -> need "reopened account differs"
      (C.Ledger.find_opt ledger address = Some account)) accounts;
    need "reopened storage differs" (state reopened = storage);
    need "reopened root differs" (run (C.Ledger.hash ledger) = root));
  save (Filename.concat output "result.json") (`Assoc ["status", `String "passed";
    "enabled", `Bool enabled; "circle", `Bool circle;
    "signed_blocks", `Int (if circle && enabled then 9 else if circle then 6 else 5);
    "resource_failures", `Int 2;
    "paid_failures", `Int (if circle && enabled then 4 else if enabled then 1 else 0);
    "reopened", `Bool true; "proofs_generated", `Int 0; "deployment_ready", `Bool false])

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "circle" then begin
    charge_cases ();
    List.iter (fun enabled -> Test_workspace.with_dir "circle_result" (fun output ->
      flow ~circle:true output enabled)) [false; true];
    print_endline "event = circle_result status = passed";
    exit 0
  end;
  need "mode and output required" (Array.length Sys.argv = 3);
  let enabled = match Sys.argv.(1) with
    | "active" -> true | "disabled" -> false | _ -> failwith "result mode invalid" in
  policy enabled;
  flow Sys.argv.(2) enabled;
  print_endline "event = program_result status = passed"