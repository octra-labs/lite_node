(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Funding = Octra_core.Compute_funding
module Protocol = Octra_consensus.Resource_compute_protocol
module Certificate = Octra_consensus.Resource_compute_certificate
module Store = Octra_core.Store_irmin
module Ledger = Octra_core.Ledger
module Chaos = Octra_core.Chaos
module Epoch = Octra_core.Epoch_exec
module History = Octra_core.Compute_history
module Ed = Mirage_crypto_ec.Ed25519
module Bill = Octra_core.Compute_bill

let run work =
  let result = Lwt_main.run (Lwt.try_bind (fun () -> work)
    (fun value -> Lwt.return_ok value) (fun error -> Lwt.return_error error)) in
  match result with Ok value -> value | Error error -> raise error
let get = function Ok value -> value | Error error -> failwith error
let expect name value = if not value then failwith ("compute deposit: " ^ name)
let key seed = match Ed.priv_of_octets (String.make 32 seed) with
  | Ok key -> key | Error _ -> failwith "test signing key"
let public key = Ed.pub_to_octets (Ed.pub_of_priv key)
let address key = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn (public key))
let payer = key 'p'
let coordinator = key 'c'
let chain_id = "compute-deposit-test"
let hash byte = String.make 32 byte

let signed call =
  let call = {call with Protocol.caller_signature = Ed.sign ~key:payer (Protocol.intent_sign_bytes call)} in
  {call with Protocol.signature = Ed.sign ~key:coordinator (Protocol.call_sign_bytes call)}

let call sequence =
  let request = Certificate.{unit_price = Some 2L; fee_cap = Some 10_000L;
    memory_bytes = Some 131_072; guest_bytes = Some 65_536; fuel_limit = Some 5000;
    chain_id; epoch_id = 42L; expires_epoch = 50L;
    validator_set_root = hash 'v'; selection_root = hash 's'; circle_id = address coordinator;
    state_root = hash 'r'; model_epoch = 40L; model_state_root = hash 'm';
    graph_root = hash 'g'; model_root = hash 'w'; program_root = hash 'p'; executor_root = hash 'e';
    caller = address payer; session_id = hash 'i'; sequence;
    input_hash = Protocol.input_hash ~method_name:"predict" ~params_json:"[]";
    input_bytes = 2; max_output_bytes = 256} in
  let call = Protocol.{request; valid_until = Some 50L; method_name = "predict";
    params_json = "[]"; caller_public_key = public payer; caller_signature = String.make 64 '\000';
    coordinator = address coordinator; signature = String.make 64 '\000'} in
  signed call

let permission ?(target = Funding.escrow) nonce call =
  let terms = Funding.prepare ~call ~target ~nonce |> get in
  Funding.authorize terms ~signature:(Ed.sign ~key:payer (Funding.sign_bytes terms)) |> get

let balance ledger address = match Ledger.find_opt ledger address with
  | None -> Z.zero | Some account -> account.Ledger.balance

let state ledger =
  let account = Ledger.find ledger (address payer) in
  (account, balance ledger Funding.escrow, Ledger.get_total_supply ledger,
   run (Store.get_batch_tree_hash ledger.Ledger.store))

let with_ledger action =
  Test_workspace.with_dir "compute-deposit" (fun root ->
    let path = Filename.concat root "irmin" in
    let store = run (Store.open_store path) in
    Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
      let ledger = Ledger.create_from_store store in
      Ledger.add_account ledger (address payer) (Z.of_int 50_000) |> get;
      run (Ledger.flush_dirty_lwt ledger);
      action path store ledger))

let hold ?(epoch = 42L) ?(chain_id = chain_id) ?(fee = Z.zero) ledger call value =
  run (Funding.hold ~ledger ~chain_id ~epoch ~fee ~call value) |> Result.map fst

let refund ?(epoch = 51L) ?(chain_id = chain_id) ledger id =
  run (Funding.refund ~ledger ~chain_id ~epoch ~id)

let begin_epoch store ledger =
  run (Store.begin_epoch_batch store);
  Ledger.begin_journal ledger |> get

let commit_epoch store ledger message =
  run (Store.commit_epoch_batch store message);
  Ledger.commit_journal ledger |> get

let test_context () = with_ledger (fun _ store ledger ->
  let call = call 1L in
  let value = permission 1L call in
  expect "hold outside batch accepted" (Result.is_error (hold ledger call value));
  run (Store.begin_epoch_batch store);
  expect "hold without outer journal accepted" (Result.is_error (hold ledger call value));
  Ledger.begin_journal ledger |> get;
  let before = state ledger in
  List.iter (fun attempt ->
    expect "invalid hold accepted" (Result.is_error (attempt ()));
    expect "invalid hold changed state" (state ledger = before)) [
    (fun () -> hold ~epoch:41L ledger call value);
    (fun () -> hold ~epoch:51L ledger call value);
    (fun () -> hold ~chain_id:"other" ledger call value);
    (fun () -> hold ledger call (permission ~target:(address coordinator) 1L call));
    (fun () -> hold ledger call (permission 2L call));
    (fun () -> hold ledger call (permission Int64.max_int call))];
  expect "invalid id accepted" (Result.is_error (refund ledger "bad"));
  expect "missing id accepted" (Result.is_error (refund ledger (hash 'x')));
  let id = hold ledger call value |> get in
  let funded = state ledger in
  expect "permit replay accepted" (Result.is_error (hold ledger call value));
  expect "job funded twice" (Result.is_error (hold ledger call (permission 2L call)));
  expect "funding replay changed state" (state ledger = funded);
  expect "expiry returned early" (Result.is_error (refund ~epoch:50L ledger id));
  expect "wrong-chain refund accepted" (Result.is_error (refund ~chain_id:"other" ledger id));
  expect "refusal changed state" (state ledger = funded);
  expect "inner journal left open" (List.length ledger.Ledger.epoch_journals = 1))

let test_conservation () = with_ledger (fun _ store ledger ->
  begin_epoch store ledger;
  let first = call 1L in
  let second = call 2L in
  let id = hold ledger first (permission 1L first) |> get in
  let other = hold ledger second (permission 2L second) |> get in
  expect "funded balances differ"
    (balance ledger (address payer) = Z.of_int 30_000
     && balance ledger Funding.escrow = Z.of_int 20_000);
  expect "hold changed supply" (Ledger.get_total_supply ledger = Z.of_int 50_000);
  expect "refund amount differs" (refund ledger id = Ok (Z.of_int 10_000));
  let returned = state ledger in
  expect "repeat refund spent another deposit" (Result.is_error (refund ledger id));
  expect "repeat refund changed state" (state ledger = returned);
  expect "closed job reopened" (Result.is_error (hold ledger first (permission 3L first)));
  expect "other deposit missing" (Result.is_ok (run (Funding.read store ~id:other)));
  expect "second refund failed" (refund ledger other = Ok (Z.of_int 10_000));
  expect "refund changed supply" (Ledger.get_total_supply ledger = Z.of_int 50_000);
  expect "funds lost"
    (balance ledger (address payer) = Z.of_int 50_000 && balance ledger Funding.escrow = Z.zero);
  expect "refund changed nonce" ((Ledger.find ledger (address payer)).nonce = 2))

let test_amounts () =
  List.iter (fun amount -> with_ledger (fun _ store ledger ->
    begin_epoch store ledger;
    let call = call 1L in
    let call = signed {call with request = {call.request with
      fee_cap = Some amount; unit_price = Some 1L; fuel_limit = Some 1}} in
    let before = state ledger in
    let result = hold ledger call (permission 1L call) in
    if amount > 50_000L then begin
      expect "unfunded amount accepted" (Result.is_error result);
      expect "unfunded amount changed state" (state ledger = before)
    end else begin
      let id = get result in
      expect "exact cap not held" (balance ledger Funding.escrow = Z.of_int64 amount);
      expect "amount changed supply" (Ledger.get_total_supply ledger = Z.of_int 50_000);
      expect "exact refund differs" (refund ledger id = Ok (Z.of_int64 amount))
    end)) [1L; 9999L; 50_000L; 50_001L; Int64.max_int]

let test_fees () = with_ledger (fun _ store ledger ->
  begin_epoch store ledger;
  let total = Ledger.get_total_supply ledger in
  let first = call 1L in
  let before = state ledger in
  List.iter (fun fee ->
    expect "invalid funding fee accepted"
      (Result.is_error (hold ~fee ledger first (permission 1L first)));
    expect "invalid funding fee changed state" (state ledger = before))
    [Z.minus_one; Z.of_int 40_001; Z.shift_left Z.one 256];
  let fund sequence fee =
    let call = call sequence in
    let expected = Z.of_int fee in
    let id, charged = run (Funding.hold ~ledger ~chain_id ~epoch:42L ~fee:expected
      ~call (permission sequence call)) |> get in
    expect "charged fee differs" (Z.equal charged expected);
    id, charged in
  let first, fee = fund 1L 250 in
  expect "funding charged nonce twice" ((Ledger.find ledger (address payer)).nonce = 1);
  expect "funding fee not debited" (balance ledger (address payer) = Z.of_int 39_750);
  expect "funding fee entered escrow" (balance ledger Funding.escrow = Z.of_int 10_000);
  let second, more = fund 2L 750 in
  let fees = Z.add fee more in
  expect "funding fee accounting differs"
    (Z.add fees (Z.add (balance ledger (address payer)) (balance ledger Funding.escrow))
      = total && Z.add fees (Ledger.get_total_supply ledger) = total);
  let paid = state ledger in
  expect "funding fee replay accepted"
    (Result.is_error (hold ~fee ledger (call 1L) (permission 1L (call 1L))));
  expect "funding fee replay changed state" (state ledger = paid);
  expect "first fee refund differs" (refund ledger first = Ok (Z.of_int 10_000));
  expect "second fee refund differs" (refund ledger second = Ok (Z.of_int 10_000));
  expect "refund returned funding fees" (balance ledger (address payer) = Z.of_int 49_000);
  expect "fee funding nonce differs" ((Ledger.find ledger (address payer)).nonce = 2);
  Ledger.credit ledger (address coordinator) fees |> get;
  expect "distributed fees changed supply"
    (Z.add (balance ledger (address payer)) (balance ledger (address coordinator))
      = total && Ledger.get_total_supply ledger = total))

let test_epoch_fee () =
  List.iter (fun reject -> with_ledger (fun _ store ledger ->
    let call = call 1L in
    let permission = permission 1L call in
    let tx = Octra_core.Transaction.{from = address payer; to_ = Funding.escrow;
      amount = Z.of_int 10_000; nonce = 1; ou = Z.of_int 250; timestamp = 420.;
      signature = "test"; public_key = None; message = None; encrypted_data = None;
      op_type = Standard} in
    let env = Epoch.{chain_id; epoch_id = 42; epoch_ts = 420.;
      proposer_addr = address coordinator; validator_addrs = []; validator_pubkeys = [];
      prev_state_root = ""; ready_state_root_at = None; ready_max_lag = 0} in
    let backend = Epoch.make_live_backend ~emission_policy:Octra_core.Emission_policy.Guard
      ~emission_schedule:Octra_core.Emission_schedule.Inactive
      ~validator_policy:Octra_core.Validator_policy.Inactive ~legacy_total_supply:"50000"
      store ledger in
    let result = run (Epoch.run ~backend ~env ~txs:[tx]
      ~process_tx:(fun ~backend:_ ~env:_ current ->
        let open Lwt.Syntax in
        let* result = Funding.hold ~ledger ~chain_id ~epoch:42L ~fee:current.ou ~call permission in
        Lwt.return (match result with
          | Error reason -> Error ("compute_funding", reason)
          | Ok _ when reject -> Error ("compute_funding", "reject after funding")
          | Ok (_, fee) -> Ok fee))) |> get in
    let id = Certificate.request_id call.request in
    expect "epoch fee changed supply" (Ledger.get_total_supply ledger = Z.of_int 50_000);
    if reject then begin
      expect "failed funding paid fee" (result.artifacts.confirmed_fees = Z.zero);
      expect "failed funding lost rejection" (List.length result.artifacts.rejected = 1);
      expect "failed funding kept deposit" (run (Funding.read store ~id) = Ok None);
      expect "failed funding kept nonce" ((Ledger.find ledger (address payer)).nonce = 0);
      expect "failed funding kept debit" (balance ledger (address payer) = Z.of_int 50_000);
      expect "failed funding rewarded fee" (balance ledger (address coordinator) = Z.zero)
    end else begin
      expect "epoch funding fee differs" (result.artifacts.confirmed_fees = tx.ou);
      expect "epoch funding not confirmed" (List.length result.artifacts.confirmed = 1);
      expect "epoch funding fee not rewarded" (balance ledger (address coordinator) = tx.ou);
      expect "epoch funding debit differs" (balance ledger (address payer) = Z.of_int 39_750);
      expect "epoch funding reserve differs" (balance ledger Funding.escrow = tx.amount);
      expect "epoch funding nonce differs" ((Ledger.find ledger (address payer)).nonce = 1);
      expect "epoch funding record missing" (Result.is_ok (run (Funding.read store ~id)))
    end)) [false; true]

let test_private_input () = with_ledger (fun _ store ledger ->
  begin_epoch store ledger;
  let call = call 1L in
  let params_json = "\"" ^ String.make 32_768 'p' ^ "\"" in
  let call = signed {call with params_json; request = {call.request with
    input_bytes = String.length params_json;
    input_hash = Protocol.input_hash ~method_name:call.method_name ~params_json}} in
  let header_raw = Protocol.header call |> get |> Protocol.encode_header in
  expect "header persisted input body" (String.length header_raw < 2048);
  let header = Protocol.decode_header header_raw |> get in
  let terms = Funding.prepare_header ~header ~target:Funding.escrow ~nonce:1L |> get in
  let permit = Funding.authorize terms ~signature:(Ed.sign ~key:payer (Funding.sign_bytes terms)) |> get in
  let id, fee = run (Funding.hold_header ~ledger ~chain_id ~epoch:42L ~fee:Z.zero ~header permit) |> get in
  expect "zero funding fee changed" (Z.equal fee Z.zero);
  let path = ["compute"; "funds"; Digestif.SHA256.(to_hex (of_raw_string id))] in
  let raw = run (Store.read store path) |> Option.get in
  expect "deposit persisted input body" (String.length raw < 2048);
  let deposit = run (Funding.read store ~id) |> get |> Option.get in
  expect "private input request changed" (deposit.request_id = Certificate.request_id call.request))

let with_fault phase kind action =
  let vars = ["OCTRA_CHAOS_FAIL_AT", phase; "OCTRA_CHAOS_FAIL_KIND", kind] in
  let old = List.map (fun (key, _) -> key, Sys.getenv_opt key) vars in
  List.iter (fun (key, value) -> Unix.putenv key value) vars;
  Fun.protect ~finally:(fun () ->
    List.iter (fun (key, value) -> Unix.putenv key (Option.value value ~default:"")) old) action

let test_faults () = with_ledger (fun _ store ledger ->
  begin_epoch store ledger;
  let call = call 1L in
  let value = permission 1L call in
  let check attempt =
    List.iter (fun phase -> List.iter (fun kind ->
      let before = state ledger in
      let raised = with_fault phase kind (fun () ->
        try ignore (attempt ()); false with
        | Chaos.Chaos_injected_failure _ | Out_of_memory | Stack_overflow | Lwt.Canceled
        | Octra_core.Exec_resource.Unavailable _ -> true) in
      expect "fault converted to decision" raised;
      expect "fault changed state" (state ledger = before);
      expect "fault left journal open" (List.length ledger.Ledger.epoch_journals = 1))
      ["fail"; "memory"; "stack"; "host"; "cancel"])
      ["compute_fund_after_debit"; "compute_fund_after_record"; "compute_fund_after_accounts"] in
  check (fun () -> hold ~fee:(Z.of_int 250) ledger call value);
  let id = hold ~fee:(Z.of_int 250) ledger call value |> get in
  check (fun () -> refund ledger id);
  expect "refund after faults failed" (Result.is_ok (refund ledger id)))

let test_outer_abort () = with_ledger (fun _ store ledger ->
  run (Store.begin_epoch_batch store);
  Ledger.begin_journal ledger |> get;
  let call = call 1L in
  let id = hold ~fee:(Z.of_int 250) ledger call (permission 1L call) |> get in
  expect "inner commit closed outer journal" (Ledger.journal_active ledger);
  Ledger.abort_journal ledger |> get;
  Store.abort_epoch_batch store;
  expect "outer abort lost funds" (balance ledger (address payer) = Z.of_int 50_000);
  expect "outer abort retained escrow" (balance ledger Funding.escrow = Z.zero);
  expect "outer abort retained record" (run (Funding.read store ~id) = Ok None);
  expect "outer abort retained nonce" ((Ledger.find ledger (address payer)).nonce = 0))

let test_reopen () =
  Test_workspace.with_dir "compute-deposit-reopen" (fun root ->
    let path = Filename.concat root "irmin" in
    let store = run (Store.open_store path) in
    let ledger = Ledger.create_from_store store in
    Ledger.add_account ledger (address payer) (Z.of_int 50_000) |> get;
    run (Ledger.flush_dirty_lwt ledger);
    run (Store.set_meta store "last_epoch" "41");
    let snapshot = run (Store.capture_read_snapshot store) |> get in
    begin_epoch store ledger;
    let call = call 1L in
    let id, fee = run (Funding.hold ~ledger ~chain_id ~epoch:42L ~fee:(Z.of_int 250)
      ~call (permission 1L call)) |> get in
    Ledger.credit ledger (address coordinator) fee |> get;
    run (Ledger.flush_dirty_lwt ledger);
    expect "uncommitted deposit leaked" (run (Funding.read ~snapshot store ~id) = Ok None);
    commit_epoch store ledger "compute hold";
    run (Store.close store);
    let store = run (Store.open_store path) in
    let ledger = Ledger.create_from_store store in
    expect "restart lost escrow" (balance ledger Funding.escrow = Z.of_int 10_000);
    expect "restart lost fee" (balance ledger (address coordinator) = Z.of_int 250);
    expect "restart lost fee debit" (balance ledger (address payer) = Z.of_int 39_750);
    expect "restart changed supply" (Ledger.get_total_supply ledger = Z.of_int 50_000);
    let saved = run (Funding.read store ~id) |> get |> Option.get in
    expect "restart lost funding status" (saved.opened_epoch = 42L && saved.returned_epoch = None);
    begin_epoch store ledger;
    refund ledger id |> get |> ignore;
    commit_epoch store ledger "compute refund";
    run (Store.close store);
    let store = run (Store.open_store path) in
    Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
      let ledger = Ledger.create_from_store store in
      begin_epoch store ledger;
      expect "restart allowed repeat refund" (Result.is_error (refund ledger id));
      expect "restart lost return" (balance ledger (address payer) = Z.of_int 49_750);
      expect "restart lost closed status"
        ((run (Funding.read store ~id) |> get |> Option.get).returned_epoch = Some 51L)))

let test_bad_record () = with_ledger (fun _ store ledger ->
  begin_epoch store ledger;
  let call = call 1L in
  let id = hold ledger call (permission 1L call) |> get in
  let path = ["compute"; "funds"; Digestif.SHA256.(to_hex (of_raw_string id))] in
  run (Store.write store path "broken");
  let before = state ledger in
  expect "bad record treated as absent" (Result.is_error (run (Funding.read store ~id)));
  expect "bad record paid" (Result.is_error (refund ledger id));
  expect "bad record overwritten" (Result.is_error (hold ledger call (permission 2L call)));
  expect "bad record changed funds" (state ledger = before))

let test_funding_wait () = with_ledger (fun path store ledger ->
  let request = call 1L in
  let header = Protocol.header request |> get in
  let caller = request.request.caller and session = request.request.session_id in
  let id = Protocol.header_id header in
  expect "header intent id changed" (id = Protocol.intent_id request);
  let prior_id = Octra_net.Hash_domain.hash_encoded "octra:resource_compute_intent_id"
    (fun buffer ->
      Octra_net.Oce1.put_hash32 buffer (Protocol.intent_sign_bytes request);
      Octra_net.Oce1.put_sig64 buffer request.caller_signature) in
  expect "stored intent id changed" (id = prior_id);
  let scope = History.{sequence = 1L; request = Certificate.request_id request.request; body = "scope"} in
  let db_path = Filename.dirname path in
  let open_db chain_id = History.open_db ~chain_id ~node_id:"funding-test" db_path in
  let db = open_db chain_id in
  expect "waiting session" (History.ensure db ~caller ~session = Ok true);
  expect "waiting plan" (History.plan_job db ~caller ~session ~id ~body:"plan" = Ok true);
  expect "waiting write" (History.wait_job ~scope ~room:4096 db ~caller ~session ~id
    ~before:"plan" ~body:"call" = Ok true);
  expect "waiting fetch started" (History.claim_fetch db ~caller ~session ~id ~limit:8 = Ok None);
  expect "waiting result saved" (History.save_job db ~caller ~session ~id ~body:"result" = Ok false);
  run (Store.set_meta store "last_epoch" "42");
  run (Store.tag_epoch store 42);
  let absent_root = (run (Store.epoch_binding store 42) |> get).root in
  let observe ?index_root epoch state_root =
    run (Funding.observe ?index_root ~chain_id ~epoch ~state_root ~header store) in
  expect "absent reserve approved" (observe 42L absent_root = Ok None);
  begin_epoch store ledger;
  let deposit_id = hold ledger request (permission 1L request) |> get in
  expect "uncommitted reserve approved" (observe 42L absent_root = Ok None);
  run (Store.set_meta store "last_epoch" "43");
  commit_epoch store ledger "funded wait";
  run (Store.tag_epoch store 43);
  let root = (run (Store.epoch_binding store 43) |> get).root in
  let funds = observe 43L root |> get |> Option.get in
  let check_scope name chain altered cancel =
    Test_workspace.with_dir name (fun folder ->
      let db = History.open_db ~chain_id:chain ~node_id:"funding-test" folder in
      Fun.protect ~finally:(fun () -> History.close db) (fun () ->
        expect "changed scope session" (History.ensure db ~caller ~session = Ok true);
        expect "changed scope plan" (History.plan_job db ~caller ~session ~id ~body:"plan" = Ok true);
        expect "changed scope wait" (History.wait_job ~scope:altered ~room:4096 db
          ~caller ~session ~id ~before:"plan" ~body:"call" = Ok true);
        if cancel then ignore (History.cancel db ~caller ~session);
        let result = History.fund_job db ~epoch:43L ~state_root:root ~id ~body:"call" funds in
        expect "changed funding context released work" (not (result = Ok true));
        expect "changed context altered wait" (History.work db ~caller ~id = Ok (Some (Funding "call"))))) in
  check_scope "funding_chain" "another-chain" scope false;
  check_scope "funding_request" chain_id {scope with request = hash 'x'} false;
  check_scope "funding_sequence" chain_id {scope with sequence = 2L} false;
  check_scope "funding_cancel" chain_id scope true;
  expect "observed request differs" (funds.deposit.request_id = scope.request);
  expect "wrong root observed" (Result.is_error (observe 43L (String.make 64 '0')));
  expect "missing epoch observed" (Result.is_error (observe 44L root));
  let index_root = String.make 64 '1' in
  let folded = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:root ~epoch_index_root:index_root in
  expect "folded root refused" (Result.is_ok (observe ~index_root 43L folded));
  expect "wrong folded index observed"
    (Result.is_error (observe ~index_root:(String.make 64 '2') 43L folded));
  let release ?(epoch = 43L) ?(state_root = root) ?(body = "call") db =
    History.fund_job db ~epoch ~state_root ~id ~body funds in
  let other_id = hash 'x' in
  expect "other waiting plan" (History.plan_job db ~caller ~session ~id:other_id ~body:"plan" = Ok true);
  expect "other waiting call" (History.wait_job ~scope ~room:4096 db ~caller ~session
    ~id:other_id ~before:"plan" ~body:"call" = Ok true);
  expect "another job used reserve" (Result.is_error
    (History.fund_job db ~epoch:43L ~state_root:root ~id:other_id ~body:"call" funds));
  expect "other wait changed" (History.work db ~caller ~id:other_id = Ok (Some (Funding "call")));
  List.iter (fun check -> expect "changed funding released work" (Result.is_error (check ()))) [
    (fun () -> release ~epoch:44L db);
    (fun () -> release ~state_root:absent_root db)];
  expect "changed body released work" (release ~body:"other" db = Ok false);
  expect "pending state changed" (History.work db ~caller ~id = Ok (Some (Funding "call")));
  History.close db;
  let db = open_db chain_id in
  expect "waiting lost after restart" (History.work db ~caller ~id = Ok (Some (Funding "call")));
  expect "verified reserve did not release" (release db = Ok true);
  expect "verified repeat changed result" (release db = Ok true);
  expect "work not issued" (History.work db ~caller ~id = Ok (Some (Issued "call")));
  expect "issued fetch unavailable" (History.claim_fetch db ~caller ~session ~id ~limit:8 = Ok (Some 0));
  expect "issued completion failed" (History.save_job db ~caller ~session ~id ~body:"result" = Ok true);
  expect "completed work reopened" (release db = Ok false);
  History.close db;
  begin_epoch store ledger;
  refund ledger deposit_id |> get |> ignore;
  run (Store.set_meta store "last_epoch" "51");
  commit_epoch store ledger "returned wait";
  run (Store.tag_epoch store 51);
  let returned_root = (run (Store.epoch_binding store 51) |> get).root in
  expect "returned reserve observed" (Result.is_error (observe 51L returned_root)))

let crash_child path phase mode =
  let store = run (Store.open_store path) in
  let ledger = Ledger.create_from_store store in
  begin_epoch store ledger;
  Unix.putenv "OCTRA_CHAOS_KILL_AT" phase;
  let call = call 1L in
  if mode = "hold" then ignore (hold ledger call (permission 1L call))
  else ignore (refund ledger (Certificate.request_id call.request));
  failwith "compute deposit crash was not reached"

let test_crash () =
  List.iter (fun mode -> List.iter (fun phase ->
    Test_workspace.with_dir "compute-deposit-crash" (fun root ->
      let path = Filename.concat root "irmin" in
      let store = run (Store.open_store path) in
      let ledger = Ledger.create_from_store store in
      Ledger.add_account ledger (address payer) (Z.of_int 50_000) |> get;
      run (Ledger.flush_dirty_lwt ledger);
      let call = call 1L in
      let id = Certificate.request_id call.request in
      if mode = "refund" then begin
        begin_epoch store ledger;
        hold ledger call (permission 1L call) |> get |> ignore;
        commit_epoch store ledger "compute hold"
      end;
      let head = run (Store.get_head_hash store) in
      run (Store.close store);
      let executable = if Filename.is_relative Sys.executable_name then
          Filename.concat (Sys.getcwd ()) Sys.executable_name else Sys.executable_name in
      let child = Unix.create_process executable
        [|executable; "--crash"; path; phase; mode|] Unix.stdin Unix.stdout Unix.stderr in
      expect "crash did not exit at write stage" (Test_workspace.wait child = Unix.WEXITED 137);
      let store = run (Store.open_store path) in
      Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
        let ledger = Ledger.create_from_store store in
        expect "crash changed committed head" (run (Store.get_head_hash store) = head);
        expect "crash changed supply" (Ledger.get_total_supply ledger = Z.of_int 50_000);
        if mode = "hold" then begin
          expect "crash debited payer" (balance ledger (address payer) = Z.of_int 50_000);
          expect "crash created record" (run (Funding.read store ~id) = Ok None)
        end else begin
          expect "crash returned funds" (balance ledger (address payer) = Z.of_int 40_000);
          expect "crash consumed escrow" (balance ledger Funding.escrow = Z.of_int 10_000);
          expect "crash closed record"
            ((run (Funding.read store ~id) |> get |> Option.get).returned_epoch = None)
        end)))
    ["compute_fund_after_debit"; "compute_fund_after_record"; "compute_fund_after_accounts"])
    ["hold"; "refund"]

let bill_inputs ?(sequence = 1L) outcome steps =
  let keys = [key 'a'; key 'b'; coordinator] in
  let validators = Octra_consensus.C_types.make_validator_set
    (List.map (fun key -> Octra_consensus.C_types.{address = address key; pubkey = public key}) keys) in
  let first = call sequence in
  let call = signed {first with request = {first.request with
    validator_set_root = Octra_consensus.C_config.validator_set_hash validators}} in
  let output = match outcome with Bill.Complete -> "42" | Bill.Stopped code ->
    Printf.sprintf "{\"compute_stop\":%d}" code
    | Bill.Exhausted -> "{\"compute_stop\":\"fuel\"}" in
  let measured = Bill.measure ~request:call.request ~output ~trace_root:(hash 't')
    ~steps ~operations:7L ~outcome |> get in
  let votes = List.map (fun key ->
    let vote = Certificate.{request_id = measured.request_id; node_id = address key;
      output_hash = measured.output_hash; trace_root = measured.trace_root;
      output_bytes = measured.output_bytes; steps; operations = measured.operations;
      signature = String.make 64 '\000'} in
    {vote with signature = Ed.sign ~key (Certificate.vote_sign_bytes vote)}) keys in
  let header = Protocol.header call |> get in
  let verify votes measured = Bill.verify ~epoch:42L ~header ~validators
    ~selected:(List.map address keys) ~votes ~measured in
  call, measured, votes, verify

let test_bill_checks () =
  let call, measured, votes, verify = bill_inputs (Bill.Stopped 7) 101L in
  let bill = verify votes measured |> get in
  expect "bill charge differs" (bill.charged = Z.of_int 202 && bill.refund = Z.of_int 9798);
  expect "bill split lost units"
    (List.map snd bill.payments = [Z.of_int 68; Z.of_int 67; Z.of_int 67]);
  expect "bill vote order matters" (verify (List.rev votes) measured = Ok bill);
  expect "bill missing quorum accepted" (Result.is_error (verify [List.hd votes] measured));
  expect "bill duplicate quorum accepted"
    (Result.is_error (verify [List.hd votes; List.hd votes; List.hd votes] measured));
  let unsigned = List.map (fun vote -> {vote with Certificate.signature = String.make 64 '\000'}) votes in
  expect "bill unsigned work accepted" (Result.is_error (verify unsigned measured));
  let other = Bill.measure ~request:call.request ~output:"42" ~trace_root:(hash 'u')
    ~steps:102L ~operations:7L ~outcome:Bill.Complete |> get in
  expect "bill different execution accepted" (Result.is_error (verify votes other));
  expect "bill work exceeds fuel" (Result.is_error (Bill.measure ~request:call.request
    ~output:"42" ~trace_root:(hash 't') ~steps:5001L ~operations:7L ~outcome:Bill.Complete));
  expect "bill host fault accepted" (Result.is_error (Bill.measure ~request:call.request
    ~output:"error" ~trace_root:(hash 't') ~steps:10L ~operations:7L ~outcome:(Bill.Stopped (-1))))

let test_settlement () =
  List.iter (fun outcome -> with_ledger (fun _ store ledger ->
    let call, measured, votes, verify = bill_inputs outcome 101L in
    let bill = verify votes measured |> get in
    begin_epoch store ledger;
    let id = hold ledger call (permission 1L call) |> get in
    let funded = state ledger in
    List.iter (fun (chain, epoch) ->
      expect "invalid settlement accepted"
        (Result.is_error (run (Funding.settle ~ledger ~chain_id:chain ~epoch bill)));
      expect "invalid settlement changed state" (state ledger = funded))
      ["other", 42L; chain_id, 41L; chain_id, 51L];
    expect "settlement refused"
      (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill) = Ok (Z.of_int 202, Z.of_int 9798));
    expect "payer refund differs" (balance ledger (address payer) = Z.of_int 49798);
    expect "escrow retained funds" (balance ledger Funding.escrow = Z.zero);
    List.iter (fun (node, amount) -> expect "payee credit differs" (balance ledger node = amount)) bill.payments;
    expect "settlement changed supply" (Ledger.get_total_supply ledger = Z.of_int 50000);
    expect "settlement changed nonce" ((Ledger.find ledger (address payer)).nonce = 1);
    let paid = state ledger in
    expect "settlement repeated" (Result.is_error (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill)));
    expect "settled funds refunded" (Result.is_error (refund ledger id));
    expect "repeat changed state" (state ledger = paid);
    commit_epoch store ledger "compute paid"))
    [Bill.Complete; Bill.Stopped 7; Bill.Exhausted]

let test_unfunded_bill () = with_ledger (fun _ store ledger ->
  let call, measured, votes, verify = bill_inputs (Bill.Stopped 7) 101L in
  let bill = verify votes measured |> get in
  begin_epoch store ledger;
  let before = state ledger in
  expect "unfunded work paid"
    (Result.is_error (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill)));
  expect "unfunded bill changed state" (state ledger = before);
  let id = hold ledger call (permission 1L call) |> get in
  ignore (refund ledger id |> get);
  let returned = state ledger in
  expect "returned funds paid"
    (Result.is_error (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill)));
  expect "returned bill changed state" (state ledger = returned))

let test_settle_reopen () =
  Test_workspace.with_dir "compute-paid" (fun root ->
    let path = Filename.concat root "irmin" in
    let call, measured, votes, verify = bill_inputs (Bill.Stopped 7) 101L in
    let bill = verify votes measured |> get in
    let store = run (Store.open_store path) in
    Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
      let ledger = Ledger.create_from_store store in
      Ledger.add_account ledger (address payer) (Z.of_int 50_000) |> get;
      run (Ledger.flush_dirty_lwt ledger);
      begin_epoch store ledger;
      ignore (hold ledger call (permission 1L call) |> get);
      ignore (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill) |> get);
      commit_epoch store ledger "compute paid");
    let store = run (Store.open_store path) in
    Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
      let ledger = Ledger.create_from_store store in
      begin_epoch store ledger;
      let id = measured.request_id in
      expect "paid status lost" ((run (Funding.read store ~id) |> get |> Option.get).paid
        = Some (42L, bill.root));
      expect "reopened settlement repeated"
        (Result.is_error (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill)));
      expect "reopened paid funds refunded" (Result.is_error (refund ledger id));
      let raw = run (Store.read store (Funding.deposit_path id)) |> Option.get in
      expect "paid record accepted as funding" (Result.is_error (Funding.verify_deposit
        ~chain_id ~epoch:42L ~header:bill.header raw));
      expect "reopened payment lost" (balance ledger (address payer) = Z.of_int 49798)))

let test_settle_faults () =
  List.iter (fun point -> with_ledger (fun _ store ledger ->
    let call, measured, votes, verify = bill_inputs (Bill.Stopped 7) 101L in
    let bill = verify votes measured |> get in
    begin_epoch store ledger;
    ignore (hold ledger call (permission 1L call) |> get);
    let before = state ledger in
    Unix.putenv "OCTRA_CHAOS_FAIL_AT" point;
    let result = Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_CHAOS_FAIL_AT" "")
      (fun () -> try ignore (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill)); false
        with _ -> true) in
    expect "payment fault not reached" result;
    expect "payment fault changed state" (state ledger = before);
    List.iter (fun (node, _) -> expect "payment fault credited payee" (balance ledger node = Z.zero)) bill.payments;
    expect "payment fault blocked retry"
      (Result.is_ok (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill)))))
    ["compute_pay_after_debit"; "compute_pay_after_credit"; "compute_pay_after_record"]

let pay_child path point =
  let store = run (Store.open_store path) in
  let ledger = Ledger.create_from_store store in
  let _, measured, votes, verify = bill_inputs (Bill.Stopped 7) 101L in
  begin_epoch store ledger;
  Unix.putenv "OCTRA_CHAOS_KILL_AT" point;
  ignore (run (Funding.settle ~ledger ~chain_id ~epoch:42L (verify votes measured |> get)));
  failwith "compute payment crash was not reached"

let test_pay_crash () =
  List.iter (fun point -> Test_workspace.with_dir "compute-pay-crash" (fun root ->
    let path = Filename.concat root "irmin" in
    let call, measured, votes, verify = bill_inputs (Bill.Stopped 7) 101L in
    let bill = verify votes measured |> get in
    let store = run (Store.open_store path) in
    let head = Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
      let ledger = Ledger.create_from_store store in
      Ledger.add_account ledger (address payer) (Z.of_int 50_000) |> get;
      run (Ledger.flush_dirty_lwt ledger);
      begin_epoch store ledger;
      ignore (hold ledger call (permission 1L call) |> get);
      commit_epoch store ledger "compute reserved";
      run (Store.get_head_hash store)) in
    let executable = if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name else Sys.executable_name in
    let child = Unix.create_process executable [|executable; "--pay-crash"; path; point|]
      Unix.stdin Unix.stdout Unix.stderr in
    expect "payment crash point missed" (Test_workspace.wait child = Unix.WEXITED 137);
    let store = run (Store.open_store path) in
    Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
      let ledger = Ledger.create_from_store store in
      expect "payment crash changed head" (run (Store.get_head_hash store) = head);
      expect "payment crash consumed reserve" (balance ledger Funding.escrow = Z.of_int 10_000);
      List.iter (fun (node, _) -> expect "payment crash credited worker" (balance ledger node = Z.zero)) bill.payments;
      begin_epoch store ledger;
      expect "payment crash blocked retry"
        (Result.is_ok (run (Funding.settle ~ledger ~chain_id ~epoch:42L bill))))))
    ["compute_pay_after_debit"; "compute_pay_after_credit"; "compute_pay_after_record";
     "compute_fund_after_accounts"]

let test_transactions () =
  let module Tx = Octra_core.Transaction in
  List.iter (fun overlay -> with_ledger (fun _ store ledger ->
    let call = call 1L in
    let header = Protocol.header call |> get in
    let auth = permission 1L call in
    let message = Funding.encode_request ~header auth in
    expect "funding request round trip" (Funding.decode_request message = Ok (header, auth));
    List.iter (fun raw -> expect "bad funding bytes accepted"
      (Result.is_error (Funding.decode_request raw))) [""; "!"; message ^ "A"; String.make 8193 'A'];
    let tx = Tx.{from = address payer; to_ = Funding.escrow; amount = Z.of_int 10_000;
      nonce = 1; ou = Z.of_int 10_000; timestamp = 420.; signature = "test";
      public_key = Some (Base64.encode_exn (public payer)); message = Some message;
      encrypted_data = None; op_type = ComputeFund} in
    let env = Epoch.{chain_id; epoch_id = 42; epoch_ts = 420.; proposer_addr = address coordinator;
      validator_addrs = []; validator_pubkeys = []; prev_state_root = "";
      ready_state_root_at = None; ready_max_lag = 0} in
    let mode = ref Octra_core.Rule_graph.Active in
    let fold epoch = Result.map (fun context -> Epoch.{context with vm_bytes = !mode})
      (Epoch.prior_fold epoch) in
    let backend = if overlay then Epoch.make_overlay_backend ~fold store ledger (Ledger.Overlay.create ledger)
      else Epoch.make_live_backend ~fold store ledger in
    let balance node = match backend.ops.find_opt node with
      | None -> Z.zero | Some account -> account.balance in
    let current () = balance tx.from, balance tx.to_, backend.ops.find_opt tx.from,
      run (Store.get_batch_tree_hash store) in
    let apply ?(env = env) tx = run (Epoch.apply_compute_tx ~backend ~env tx) in
    begin_epoch store ledger;
    let before = current () in
    let reject ?(env = env) tx =
      expect "invalid compute transaction accepted" (Result.is_error (apply ~env tx));
      expect "refused compute changed state" (current () = before) in
    mode := Octra_core.Rule_graph.Prior;
    expect "compute prior epoch accepted" (Result.is_error
      (run (Epoch.process_compute_effect ~backend ~env tx)));
    mode := Octra_core.Rule_graph.Active;
    List.iter reject [ {tx with amount = Z.one}; {tx with nonce = 2};
      {tx with from = address coordinator}; {tx with to_ = address coordinator};
      {tx with public_key = Some (Base64.encode_exn (public coordinator))};
      {tx with encrypted_data = Some "x"}; {tx with ou = Z.one}; {tx with message = None} ];
    reject ~env:{env with chain_id = "other"} tx;
    reject ~env:{env with epoch_id = 51} tx;
    expect "valid compute fund refused" (apply tx = Ok tx.ou);
    expect "fund transaction did not reserve" (balance tx.from = Z.of_int 30_000
      && balance tx.to_ = tx.amount);
    let funded = current () in
    let repeated = {tx with nonce = 2;
      message = Some (Funding.encode_request ~header (permission 2L call))} in
    expect "fund transaction repeated"
      (apply repeated = Error ("compute_request_refused", "compute deposit exists"));
    expect "repeat fund changed state" (current () = funded);
    let id = Certificate.request_id call.request in
    let refund = Tx.{tx with op_type = ComputeReturn; amount = Z.zero; nonce = 2;
      ou = Z.of_int 1000; message = Some (Digestif.SHA256.(to_hex (of_raw_string id)))} in
    expect "return before expiry" (Result.is_error (apply refund));
    expect "early return changed state" (current () = funded);
    let env = {env with epoch_id = 51} in
    expect "other payer returned deposit" (Result.is_error (apply ~env {refund with from = address coordinator}));
    expect "return refused" (apply ~env refund = Ok refund.ou);
    expect "return balance differs" (balance tx.from = Z.of_int 39_000 && balance tx.to_ = Z.zero);
    let returned = current () in
    expect "return repeated" (Result.is_error (apply ~env {refund with nonce = 3}));
    expect "repeat return changed state" (current () = returned);
    let path = Funding.deposit_path id in
    let record = run (Store.read store path) |> Option.get in
    run (Store.write store path "broken");
    let broken = current () in
    List.iter (fun (env, tx) ->
      let stopped = try ignore (apply ~env tx); false with
        | Failure reason -> String.starts_with ~prefix:"compute deposit unreadable:" reason in
      expect "broken deposit became transaction refusal" stopped;
      expect "broken deposit changed accounts" (current () = broken))
      [env, refund; {env with epoch_id = 42}, tx];
    run (Store.write store path record);
    if overlay then expect "preview changed live payer"
      ((Ledger.find ledger tx.from).balance = Z.of_int 50_000
        && (Ledger.find ledger tx.from).nonce = 0))) [false; true]

let test_epoch_transfer () =
  let module Tx = Octra_core.Transaction in
  let module Rule = Octra_core.Rule_graph in
  with_ledger (fun _ store ledger ->
    let chain_id = "octra-devnet-9871-cluster" in
    let epoch = (Option.get (Rule.vm_bytes_activation_for_chain chain_id)).activation_epoch in
    let first = call 1L in
    let call = signed {first with valid_until = Some (Int64.of_int (epoch + 5));
      request = {first.request with chain_id; epoch_id = Int64.of_int epoch;
        expires_epoch = Int64.of_int (epoch + 5)}} in
    let header = Protocol.header call |> get in
    let tx = Tx.{from = address payer; to_ = Funding.escrow; amount = Z.of_int 10_000;
      nonce = 1; ou = Z.of_int 10_000; timestamp = 420.; signature = "";
      public_key = Some (Base64.encode_exn (public payer)); encrypted_data = None;
      message = Some (Funding.encode_request ~header (permission 1L call)); op_type = ComputeFund} in
    let sign tx = Tx.sign_with_privkey tx (Base64.encode_exn (Ed.priv_to_octets payer)) in
    let tx = sign tx |> Tx.to_yojson |> Octra_core.Tx_payload.decode_admit |> get in
    expect "compute reserve fee changed" (Tx.ou_cost tx = Z.of_int 10_000);
    expect "compute return fee changed" (Tx.ou_cost {tx with op_type = ComputeReturn} = Z.of_int 1000);
    expect "compute reserve size changed"
      (Octra_core.Tx_payload.message_limit Octra_core.Tx_payload.standard tx = 8192);
    expect "compute return size changed"
      (Octra_core.Tx_payload.message_limit Octra_core.Tx_payload.standard {tx with op_type = ComputeReturn} = 64);
    expect "compute transaction signature" (Tx.verify tx (Option.get tx.public_key));
    expect "compute accepted before activation"
      (Octra_core.Tx_envelope.check_epoch ~chain_id ~epoch:(Int64.of_int (epoch - 1)) [tx]
        = Error "compute transfers not active");
    expect "compute accepted on other chain"
      (Octra_core.Tx_envelope.check_epoch ~chain_id:"other" ~epoch:Int64.max_int [tx]
        = Error "compute transfers not active");
    expect "compute refused at activation"
      (Octra_core.Tx_envelope.check_epoch ~chain_id ~epoch:(Int64.of_int epoch) [tx] = Ok ());
    let fold height = Result.map (fun context -> Epoch.{context with
      vm_bytes = Rule.vm_bytes_at ~chain_id ~epoch:height}) (Epoch.prior_fold height) in
    let backend = Epoch.make_live_backend ~fold ~emission_policy:Octra_core.Emission_policy.Guard
      ~emission_schedule:Octra_core.Emission_schedule.Inactive
      ~validator_policy:Octra_core.Validator_policy.Inactive ~legacy_total_supply:"50000" store ledger in
    let env = Epoch.{chain_id; epoch_id = epoch; epoch_ts = 420.; proposer_addr = address coordinator;
      validator_addrs = []; validator_pubkeys = []; prev_state_root = "";
      ready_state_root_at = None; ready_max_lag = 0} in
    let apply env tx = run (Epoch.run ~backend ~env ~txs:[tx] ~process_tx:Epoch.process_standard_tx) in
    let id = Certificate.request_id call.request in
    let crash env tx = List.iter (fun point ->
      let before = Ledger.find ledger (address payer), balance ledger Funding.escrow,
        run (Store.get_head_hash store) in
      let stopped = with_fault point "fail" (fun () ->
        try Result.is_error (apply env tx) with Chaos.Chaos_injected_failure _ -> true) in
      expect "compute transaction fault not reached" stopped;
      expect "compute transaction fault changed state"
        (before = (Ledger.find ledger (address payer), balance ledger Funding.escrow,
          run (Store.get_head_hash store)))) ["compute_tx_after_debit"; "compute_tx_after_record"] in
    let lane = Octra_core.Resource_lanes.of_op tx.op_type in
    expect "compute funding lane differs" (lane = Octra_core.Resource_lanes.Program);
    let costly = {tx with ou = Z.of_int 8_000_000} in
    expect "compute funding work not counted"
      ((Octra_core.Resource_lanes.work costly).ou = costly.ou);
    let refused = apply env tx |> get in
    expect "unpinned funding confirmed" (refused.artifacts.confirmed = []);
    let row = match refused.artifacts.rejected with
      | [row] when row.charged -> row
      | _ -> failwith "paid funding outcome missing" in
    let receipts = Octra_core.Tx_outcome.build ~charged:[tx] ~inputs:[tx]
      [tx, row.error_type, row.reason] |> get |> Octra_core.Tx_outcome.encode [] in
    expect "paid outcome accepted before rule"
      (Result.is_error (Octra_core.Tx_envelope.check_outcome ~chain_id
        ~epoch:(Int64.of_int (epoch - 1)) ~receipts []));
    expect "paid outcome refused at rule"
      (Octra_core.Tx_envelope.check_outcome ~chain_id ~epoch:(Int64.of_int epoch)
        ~receipts [] = Ok ());
    let decoded = Octra_core.Tx_outcome.decode ~confirmed:[] receipts |> get in
    expect "paid outcome excluded from apply"
      (Octra_core.Tx_outcome.executed ~confirmed:[] decoded.rejections = [tx]);
    expect "unpaid outcome executed"
      (Octra_core.Tx_outcome.executed ~confirmed:[]
        (List.map (fun (row : Octra_core.Tx_outcome.rejection) ->
          {row with charged = false}) decoded.rejections) = []);
    expect "unpinned funding not charged" (balance ledger (address payer) = Z.of_int 40_000);
    expect "unpinned funding created reserve" (balance ledger Funding.escrow = Z.zero);
    begin_epoch store ledger;
    ignore (hold ~epoch:(Int64.of_int epoch) ~chain_id ledger call (permission 2L call) |> get);
    run (Ledger.flush_dirty_lwt ledger);
    commit_epoch store ledger "compute preexisting reserve";
    expect "compute funding epoch supply differs" (Ledger.get_total_supply ledger = Z.of_int 50_000);
    let returned = sign {tx with op_type = ComputeReturn; amount = Z.zero; nonce = 3;
      ou = Z.of_int 1000; message = Some Digestif.SHA256.(to_hex (of_raw_string id))} in
    let env = {env with epoch_id = epoch + 6} in
    crash env returned;
    let refunded = apply env returned |> get in
    expect "compute return epoch not confirmed" (List.length refunded.artifacts.confirmed = 1);
    expect "compute return epoch fee differs" (refunded.artifacts.confirmed_fees = returned.ou);
    expect "compute return epoch supply differs" (Ledger.get_total_supply ledger = Z.of_int 50_000);
    expect "compute return epoch wallet differs" (balance ledger (address payer) = Z.of_int 39_000);
    expect "compute return epoch escrow differs" (balance ledger Funding.escrow = Z.zero);
    expect "compute return epoch fees missing" (balance ledger (address coordinator) = Z.of_int 11_000))

let payment_case ?(sequence = 1L) ?steps outcome =
  let steps = Option.value steps ~default:(match outcome with
    | Bill.Exhausted -> 5000L | Bill.Complete | Bill.Stopped _ -> 101L) in
  let call, measured, votes, verify = bill_inputs ~sequence outcome steps in
  call, get (verify votes measured)

let payment_env = Epoch.{chain_id; epoch_id = 43; epoch_ts = 430.;
  proposer_addr = address coordinator; validator_addrs = []; validator_pubkeys = [];
  prev_state_root = ""; ready_state_root_at = None; ready_max_lag = 0}

let payment_tx bills nonce =
  let message = Bill.encode_request ~fuel:(5000 * List.length bills)
    (List.map (fun (bill : Bill.t) -> Protocol.encode_header bill.header) bills) in
  let tx = Octra_core.Transaction.{from = address payer; to_ = Funding.escrow;
    amount = Z.zero; nonce; ou = Bill.fee message; timestamp = 430.; signature = "";
    public_key = Some (Base64.encode_exn (public payer)); message = Some message;
    encrypted_data = None; op_type = ComputeSettle} in
  Octra_core.Transaction.sign_with_privkey tx (Base64.encode_exn (Ed.priv_to_octets payer))

let with_payment overlay calls action = with_ledger (fun _ store ledger ->
  Ledger.credit ledger (address payer) (Z.of_int 20_000_000) |> get;
  run (Ledger.flush_dirty_lwt ledger);
  begin_epoch store ledger;
  List.iteri (fun index call ->
    hold ledger call (permission (Int64.of_int (index + 1)) call) |> get |> ignore) calls;
  let fold epoch = Result.map (fun context -> Epoch.{context with vm_bytes = Octra_core.Rule_graph.Active})
    (Epoch.prior_fold epoch) in
  let backend = if overlay then
    Epoch.make_overlay_backend ~fold store ledger (Ledger.Overlay.create ledger)
    else Epoch.make_live_backend ~fold store ledger in
  action store ledger backend)

let payment_balance (backend : Epoch.backend) node =
  match backend.ops.find_opt node with None -> Z.zero | Some account -> account.balance

let payment_state (backend : Epoch.backend) bills =
  let nodes = address payer :: Funding.escrow :: address coordinator ::
    List.concat_map (fun (bill : Bill.t) -> List.map fst bill.payments) bills
    |> List.sort_uniq String.compare in
  List.map (fun node -> node, backend.ops.find_opt node) nodes,
  backend.ops.total_supply (), run (Store.get_batch_tree_hash backend.store)

let payment_verify ?(epoch = 43L) ~seen raw checks reply : Bill.verifier =
  fun ~store:_ ~epoch:actual ~funded input ->
    incr seen;
    expect "payment verifier epoch differs" (actual = epoch);
    expect "payment verifier input differs" (input = raw);
    let open Lwt.Syntax in
    let* () = Lwt_list.iter_s (fun (header, expected) ->
      let* actual = funded header in
      expect "payment funding decision differs" (actual = expected);
      Lwt.return_unit) checks in
    Lwt.return reply

let payment_apply ?(env = payment_env) backend verify tx =
  let backend = Epoch.{backend with compute = Some verify} in
  run (Epoch.apply_compute_tx ~backend ~env tx)

let test_payment_gate () =
  let module Rule = Octra_core.Rule_graph in
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Rule.vm_bytes_activation_for_chain chain_id |> Option.get in
  let rules = Rule.create ~chain_id ~root_at:(fun _ -> Rule.Root plan.anchor_state_root) in
  let call, bill = payment_case Bill.Complete in
  List.iter (fun overlay -> with_payment overlay [call] (fun _ _ backend ->
    let tx = payment_tx [bill] 2 in
    let before = payment_state backend [bill] in
    let seen = ref 0 in
    List.iter (fun result ->
      let verify ~store:_ ~epoch:_ ~funded:_ _ = incr seen; Lwt.return result in
      let backend = Epoch.{backend with compute = Some verify} in
      List.iter (fun epoch ->
        let env = Epoch.{payment_env with chain_id; epoch_id = epoch} in
        let fold height = Result.map (fun context -> Epoch.{context with
          vm_bytes = Rule.vm_bytes_at ~chain_id ~epoch:height}) (Epoch.prior_fold height) in
        let backend = Epoch.{backend with fold} in
        let reason = if epoch < plan.activation_epoch then "compute transfers not active"
          else "compute settlement not active" in
        expect "unscheduled settlement reached executor"
          (run (Epoch.process_standard_tx ~backend ~env tx)
            = Error ("compute_request_refused", reason));
        expect "settlement history affected decision" (!seen = 0);
        expect "settlement changed state without activation" (payment_state backend [bill] = before);
        expect "settlement source accepted without activation"
          (Octra_core.Tx_envelope.check_rule rules ~epoch [tx] = Error reason);
        expect "settlement wire accepted without activation"
          (Octra_core.Tx_envelope.check_epoch ~chain_id ~epoch:(Int64.of_int epoch) [tx] = Error reason))
        [plan.activation_epoch - 1; plan.activation_epoch; plan.activation_epoch + 1; max_int])
      [Ok [bill]; Error (Bill.Unavailable "compute finality missing");
       Error (Bill.Unavailable "validator set missing")])) [false; true]

let test_payment_tx () =
  let valid = call 1L in
  List.iter (fun request ->
    let call = signed {valid with request} in
    expect "unpayable work funded" (Result.is_error (Funding.prepare
      ~call ~target:Funding.escrow ~nonce:1L)))
    [{valid.request with fuel_limit = Some (Bill.max_fuel + 1); fee_cap = Some 20_000_000L};
     {valid.request with memory_bytes = Some (Int64.to_int Bill.model_limit + 65536)}];
  expect "guest memory exceeds total" (Result.is_error
    (Bill.resources {valid.request with guest_bytes = Some 262144}));
  List.iter (fun overlay -> List.iter (fun outcome ->
    let call, bill = payment_case outcome in
    with_payment overlay [call] (fun store ledger backend ->
      let tx = payment_tx [bill] 2 in
      let seen = ref 0 in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Ok (Bill.Due 42L)] (Ok [bill]) in
      let before = payment_balance backend tx.from in
      let supply = backend.ops.total_supply () in
      let live = state ledger in
      let workers = List.map (fun (node, amount) -> node,
        Z.add (payment_balance backend node) amount) bill.payments in
      expect "payment transaction signature differs"
        (Octra_core.Transaction.verify tx (Option.get tx.public_key));
      expect "payment transaction refused" (payment_apply backend verify tx = Ok tx.ou);
      expect "payment verifier not called" (!seen = 1);
      expect "payment refund differs"
        (payment_balance backend tx.from = Z.sub (Z.add before bill.refund) tx.ou);
      expect "payment escrow retained reserve" (payment_balance backend Funding.escrow = Z.zero);
      List.iter (fun (node, amount) -> expect "payment worker credit differs"
        (payment_balance backend node = amount)) workers;
      expect "payment supply differs" (backend.ops.total_supply () = Z.sub supply tx.ou);
      expect "payment nonce differs" ((Option.get (backend.ops.find_opt tx.from)).nonce = 2);
      let paid = run (Funding.read store ~id:bill.measured.request_id) |> get |> Option.get in
      expect "payment marker differs" (paid.paid = Some (43L, bill.root) && paid.returned_epoch = None);
      let after = payment_state backend [bill] in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Ok (Bill.Paid 42L)] (Ok [bill]) in
      expect "payment repeated" (payment_apply backend verify {tx with nonce = 3}
        = Error ("compute_request_refused", "compute payment has no unpaid work"));
      expect "payment repeat changed state" (payment_state backend [bill] = after);
      let returned = Octra_core.Transaction.{tx with op_type = ComputeReturn;
        nonce = 3; ou = Z.of_int 1000;
        message = Some Digestif.SHA256.(to_hex (of_raw_string bill.measured.request_id))} in
      let returned = Octra_core.Transaction.sign_with_privkey returned
        (Base64.encode_exn (Ed.priv_to_octets payer)) in
      expect "paid reserve returned" (Result.is_error (payment_apply
        ~env:{payment_env with epoch_id = 51} backend verify returned));
      expect "paid return changed state" (payment_state backend [bill] = after);
      expect "return called payment verifier" (!seen = 2);
      if overlay then begin
        let account, reserve, supply, _ = live in
        expect "payment preview changed live accounts"
          (Ledger.find ledger tx.from = account && balance ledger Funding.escrow = reserve
            && Ledger.get_total_supply ledger = supply);
        List.iter (fun (node, _) -> expect "payment preview credited live worker"
          (balance ledger node = Z.zero)) workers
      end)) [Bill.Complete; Bill.Stopped 7; Bill.Exhausted]) [false; true]

let test_payment_wrong () =
  List.iter (fun overlay ->
    let call, bill = payment_case Bill.Complete in
    let _, other = payment_case ~sequence:2L Bill.Complete in
    with_payment overlay [call] (fun store _ backend ->
      let tx = payment_tx [bill] 2 in
      let seen = ref 0 in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Ok (Bill.Due 42L)] (Ok [bill]) in
      let before = payment_state backend [bill; other] in
      List.iter (fun tx ->
        expect "invalid payment envelope accepted" (Result.is_error (payment_apply backend verify tx));
        expect "invalid payment envelope changed state" (payment_state backend [bill; other] = before))
        [{tx with amount = Z.one}; {tx with to_ = address coordinator};
         {tx with from = Funding.escrow}; {tx with encrypted_data = Some "x"};
         {tx with ou = Z.pred tx.ou}; {tx with message = None}];
      expect "invalid envelope reached payment verifier" (!seen = 0);
      let deposit = run (Funding.read store ~id:bill.measured.request_id) |> get |> Option.get in
      let paid, credits = Funding.payment ~chain_id ~epoch:43L bill deposit |> get in
      expect "payment plan marker differs" (paid.paid = Some (43L, bill.root));
      expect "payment plan credits differ" (credits = (address payer, bill.refund) :: bill.payments);
      expect "payment plan changed state" (payment_state backend [bill; other] = before);
      expect "wrong payment plan accepted"
        (Result.is_error (Funding.payment ~chain_id ~epoch:43L other deposit));
      let reason = Bill.Rejected "compute deposit missing" in
      let verify = payment_verify ~seen (Option.get tx.message)
        [other.header, Error reason] (Error reason) in
      expect "missing funding accepted" (payment_apply backend verify tx
        = Error ("compute_request_refused", "compute deposit missing"));
      expect "missing funding changed state" (payment_state backend [bill; other] = before);
      let reason = Bill.Rejected "compute funding chain differs" in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Error reason] (Error reason) in
      expect "wrong payment chain accepted" (payment_apply
        ~env:{payment_env with chain_id = "other"} backend verify tx
          = Error ("compute_request_refused", "compute funding chain differs"));
      expect "wrong payment chain changed state" (payment_state backend [bill; other] = before);
      let checks = [bill.header, Ok (Bill.Due 42L)] in
      List.iter (fun (bills, reason) ->
        let count = !seen in
        let verify = payment_verify ~seen (Option.get tx.message) checks (Ok bills) in
        expect "wrong payment batch accepted" (payment_apply backend verify tx
          = Error ("compute_request_refused", reason));
        expect "wrong payment batch skipped verifier" (!seen = count + 1);
        expect "wrong payment batch changed state" (payment_state backend [bill; other] = before))
        [[other], "compute deposit missing"; [bill; other], "compute deposit missing";
         [bill; bill], "compute payment repeated request"; [], "compute payment has no unpaid work"];
      let verify = payment_verify ~seen (Option.get tx.message) checks (Ok [bill]) in
      expect "wrong payment nonce accepted"
        (Result.is_error (payment_apply backend verify {tx with nonce = 3}));
      expect "wrong payment nonce changed state" (payment_state backend [bill; other] = before);
      expect "valid payment after refusal failed" (payment_apply backend verify tx = Ok tx.ou)))
    [false; true]

let test_payment_prefix () =
  List.iter (fun overlay ->
    let first, bill = payment_case Bill.Complete in
    let second, next = payment_case ~sequence:2L (Bill.Stopped 7) in
    let _, changed = payment_case ~steps:102L Bill.Complete in
    expect "changed bill did not change certificate"
      (changed.root <> bill.root && changed.measured.request_id = bill.measured.request_id);
    with_payment overlay [first; second] (fun store _ backend ->
      let tx = payment_tx [bill] 3 in
      let seen = ref 0 in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Ok (Bill.Due 42L)] (Ok [bill]) in
      expect "prefix payment failed" (payment_apply backend verify tx = Ok tx.ou);
      expect "prefix consumed suffix reserve" (payment_balance backend Funding.escrow = Z.of_int 10_000);
      let after = payment_state backend [bill; next] in
      let batch = payment_tx [bill; next] 4 in
      let verify = payment_verify ~seen (Option.get batch.message)
        [next.header, Ok (Bill.Due 42L); changed.header, Ok (Bill.Paid 42L)] (Ok [next; changed]) in
      expect "changed paid prefix accepted" (payment_apply backend verify batch
        = Error ("compute_request_refused", "compute deposit paid"));
      expect "changed prefix paid suffix" (payment_state backend [bill; next] = after);
      let verify = payment_verify ~seen (Option.get batch.message)
        [bill.header, Ok (Bill.Paid 42L); next.header, Ok (Bill.Due 42L)] (Ok [bill; next]) in
      let before = payment_balance backend batch.from in
      let supply = backend.ops.total_supply () in
      let workers = List.map (fun (node, amount) -> node,
        Z.add (payment_balance backend node) amount) next.payments in
      expect "unpaid suffix refused" (payment_apply backend verify batch = Ok batch.ou);
      expect "suffix verifier call count differs" (!seen = 3);
      expect "suffix refund differs"
        (payment_balance backend batch.from = Z.sub (Z.add before next.refund) batch.ou);
      expect "suffix retained escrow" (payment_balance backend Funding.escrow = Z.zero);
      expect "suffix supply differs" (backend.ops.total_supply () = Z.sub supply batch.ou);
      List.iter (fun (node, amount) -> expect "prefix worker paid twice"
        (payment_balance backend node = amount)) workers;
      List.iter (fun (bill : Bill.t) ->
        let saved = run (Funding.read store ~id:bill.measured.request_id) |> get |> Option.get in
        expect "prefix or suffix marker differs" (saved.paid = Some (43L, bill.root))) [bill; next];
      let after = payment_state backend [bill; next] in
      let verify = payment_verify ~seen (Option.get batch.message)
        [bill.header, Ok (Bill.Paid 42L); next.header, Ok (Bill.Paid 42L)] (Ok [bill; next]) in
      expect "completed prefix repeated"
        (Result.is_error (payment_apply backend verify {batch with nonce = 5}));
      expect "completed prefix changed state" (payment_state backend [bill; next] = after)))
    [false; true]

let test_payment_refusal () =
  List.iter (fun overlay ->
    let call, bill = payment_case Bill.Complete in
    with_payment overlay [call] (fun store ledger backend ->
      let tx = payment_tx [bill] 2 in
      let seen = ref 0 in
      let before = payment_state backend [bill] in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Ok (Bill.Due 42L)] (Error (Bill.Rejected "payment proof refused")) in
      expect "proof refusal not preserved" (payment_apply backend verify tx
        = Error ("compute_request_refused", "payment proof refused"));
      expect "proof refusal changed state" (payment_state backend [bill] = before);
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Ok (Bill.Due 42L)] (Error (Bill.Unavailable "payment source unavailable")) in
      let unavailable apply = try ignore (apply ()); false with
        | Octra_core.Exec_resource.Unavailable Octra_core.Exec_resource.Host -> true in
      expect "unavailable proof became a refusal" (unavailable (fun () -> payment_apply backend verify tx));
      expect "unavailable proof changed state" (payment_state backend [bill] = before);
      expect "missing verifier accepted" (unavailable (fun () -> run
        (Epoch.apply_compute_tx ~backend ~env:payment_env tx)));
      expect "missing verifier changed state" (payment_state backend [bill] = before);
      let prior = Epoch.{backend with fold = Epoch.prior_fold} in
      expect "payment accepted before activation" (run
        (Epoch.process_standard_tx ~backend:prior ~env:payment_env tx)
        = Error ("compute_request_refused", "compute transfers not active"));
      expect "inactive payment reached verifier" (!seen = 2);
      expect "inactive payment changed state" (payment_state backend [bill] = before);
      let reason = Bill.Rejected "compute deposit epoch outside window" in
      let verify = payment_verify ~epoch:51L ~seen (Option.get tx.message)
        [bill.header, Error reason] (Error reason) in
      expect "expired payment accepted" (Result.is_error
        (payment_apply ~env:{payment_env with epoch_id = 51} backend verify tx));
      expect "expired payment changed state" (payment_state backend [bill] = before);
      if not overlay then begin
        let verify = payment_verify ~seen (Option.get tx.message)
          [bill.header, Ok (Bill.Due 42L)] (Ok [bill]) in
        let backend = Epoch.{backend with compute = Some verify} in
        let applied = run (Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
          let open Lwt.Syntax in
          let* result = Epoch.apply_compute_tx ~backend ~env:payment_env tx in
          match result with Error _ -> Lwt.return result
          | Ok _ -> Lwt.return_error ("test_payment", "reject after payment"))) in
        expect "post-payment refusal not preserved"
          (applied = Error ("test_payment", "reject after payment"));
        expect "post-payment refusal changed state" (payment_state backend [bill] = before)
      end;
      let returned = Octra_core.Transaction.{tx with op_type = ComputeReturn; ou = Z.of_int 1000;
        message = Some Digestif.SHA256.(to_hex (of_raw_string bill.measured.request_id))} in
      let returned = Octra_core.Transaction.sign_with_privkey returned
        (Base64.encode_exn (Ed.priv_to_octets payer)) in
      let amount = payment_balance backend returned.from in
      let supply = backend.ops.total_supply () in
      expect "unpaid reserve return failed" (payment_apply
        ~env:{payment_env with epoch_id = 51} backend verify returned = Ok returned.ou);
      expect "returned reserve remained in escrow" (payment_balance backend Funding.escrow = Z.zero);
      expect "unpaid reserve refund differs"
        (payment_balance backend returned.from = Z.sub (Z.add amount (Z.of_int 10_000)) returned.ou);
      expect "reserve return supply differs" (backend.ops.total_supply () = Z.sub supply returned.ou);
      let saved = run (Funding.read store ~id:bill.measured.request_id) |> get |> Option.get in
      expect "reserve return marker differs" (saved.returned_epoch = Some 51L && saved.paid = None);
      let before = payment_state backend [bill] in
      let reason = Bill.Rejected "compute deposit returned" in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Error reason] (Error reason) in
      expect "returned payment accepted" (payment_apply backend verify {tx with nonce = 3}
        = Error ("compute_request_refused", "compute deposit returned"));
      expect "returned payment changed state" (payment_state backend [bill] = before))) [false; true]

let test_payment_unreadable () =
  List.iter (fun overlay ->
    let call, bill = payment_case Bill.Complete in
    with_payment overlay [call] (fun store _ backend ->
      let tx = payment_tx [bill] 2 in
      let path = Funding.deposit_path bill.measured.request_id in
      let saved = run (Store.read store path) |> Option.get in
      run (Store.write store path "broken");
      let reason = match run (Funding.read store ~id:bill.measured.request_id) with
        | Error reason -> reason | Ok _ -> failwith "broken payment record accepted" in
      let seen = ref 0 in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Error (Bill.Unavailable reason)] (Error (Bill.Unavailable reason)) in
      let before = payment_state backend [bill] in
      let stopped = try ignore (payment_apply backend verify tx); false with
        | Octra_core.Exec_resource.Unavailable Octra_core.Exec_resource.Host -> true in
      expect "unreadable funding became a refusal" stopped;
      expect "unreadable funding skipped verifier" (!seen = 1);
      expect "unreadable funding changed state" (payment_state backend [bill] = before);
      run (Store.write store path saved);
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Ok (Bill.Due 42L)] (Ok [bill]) in
      expect "restored funding payment failed" (payment_apply backend verify tx = Ok tx.ou)))
    [false; true]

let test_payment_faults () =
  List.iter (fun point -> List.iter (fun kind ->
    let first, bill = payment_case Bill.Complete in
    let second, next = payment_case ~sequence:2L Bill.Exhausted in
    with_payment false [first; second] (fun store ledger backend ->
      let tx = payment_tx [bill; next] 3 in
      let seen = ref 0 in
      let verify = payment_verify ~seen (Option.get tx.message)
        [bill.header, Ok (Bill.Due 42L); next.header, Ok (Bill.Due 42L)] (Ok [bill; next]) in
      let backend = Epoch.{backend with compute = Some verify} in
      let apply () = run (Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
        Epoch.apply_compute_tx ~backend ~env:payment_env tx)) in
      let before = payment_state backend [bill; next] in
      let depth = List.length ledger.Ledger.epoch_journals in
      let stopped = with_fault point kind (fun () ->
        try ignore (apply ()); false with
        | Chaos.Chaos_injected_failure phase -> kind = "fail" && phase = point
        | Out_of_memory -> kind = "memory"
        | Stack_overflow -> kind = "stack"
        | Octra_core.Exec_resource.Unavailable Octra_core.Exec_resource.Memory -> kind = "memory"
        | Octra_core.Exec_resource.Unavailable Octra_core.Exec_resource.Stack -> kind = "stack"
        | Octra_core.Exec_resource.Unavailable Octra_core.Exec_resource.Host -> kind = "host"
        | Lwt.Canceled -> kind = "cancel") in
      expect "payment transaction fault not reached" stopped;
      expect "payment transaction fault changed state" (payment_state backend [bill; next] = before);
      expect "payment transaction fault left journal open"
        (List.length ledger.Ledger.epoch_journals = depth);
      expect "payment transaction fault blocked retry" (apply () = Ok tx.ou);
      expect "payment retry verifier count differs" (!seen = 2);
      expect "payment retry retained reserve" (payment_balance backend Funding.escrow = Z.zero);
      List.iter (fun (bill : Bill.t) ->
        let saved = run (Funding.read store ~id:bill.measured.request_id) |> get |> Option.get in
        expect "payment retry marker differs" (saved.paid = Some (43L, bill.root))) [bill; next]))
    ["fail"; "memory"; "stack"; "host"; "cancel"])
    ["compute_pay_after_debit"; "compute_pay_after_credit"; "compute_pay_after_record"]

let test_empty_inputs () = with_ledger (fun _ store ledger ->
  let module C = Octra_consensus.C_types in
  let module H = Octra_consensus.C_hash in
  begin_epoch store ledger;
  run (Store.set_meta store "last_epoch" "42");
  commit_epoch store ledger "compute empty source";
  let snapshot = run (Store.capture_read_snapshot store) |> get in
  let root = Digestif.SHA256.(to_raw_string (of_hex snapshot.state_root)) in
  let validators = C.make_validator_set [C.{address = address coordinator; pubkey = public coordinator}] in
  let header = C.{proto_version = proto_version_current; chain_id; epoch_id = 42L;
    prev_state_root = hash 'p'; tx_list_hash = H.tx_list_hash [];
    receipt_root = H.receipt_root []; proposed_state_root = root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = address coordinator;
    txid_hi = 0L; ts = 42.} in
  let parent = C.{certificate = {chain_id; epoch_id = 42L; commit_round = 0;
    header; proposal_id = H.proposal_id header; precommits = []}; validator_set = validators} in
  let original = call 1L in
  let call = signed {original with request = {original.request with state_root = root;
    model_epoch = 42L; model_state_root = root;
    validator_set_root = Octra_consensus.C_config.validator_set_hash validators}} in
  let header = Protocol.header call |> get in
  begin_epoch store ledger;
  run (Octra_core.Compute_input.remember store ~epoch:43 ~state:root parent);
  expect "empty input tree became technical failure"
    (run (Octra_core.Compute_input.prepare store ~max_fee:(Z.of_int 1_000_000) ~header)
      = Error "circle program snapshot missing");
  run (Store.write store ["compute"; "roots"; "42"; "data"; "changed"] "bytes");
  let refused = try
    ignore (run (Octra_core.Compute_input.prepare store ~max_fee:(Z.of_int 1_000_000) ~header)); false
    with Failure reason -> reason = "compute input state corrupt" in
  expect "changed input tree accepted" refused)

let test_payment_deadline () =
  let call = signed {(call 1L) with valid_until = Some 500L} in
  let header = Protocol.header call |> get in
  let terms = Funding.prepare_until ~expires_epoch:500L ~call ~target:Funding.escrow ~nonce:1L |> get in
  let permission = Funding.authorize terms ~signature:(Ed.sign ~key:payer (Funding.sign_bytes terms)) |> get in
  expect "extended funding accepted by prior rule"
    (Result.is_error (Funding.reserve ~chain_id ~epoch:42L ~nonce:1L ~header permission));
  let deposit = Funding.reserve ~extended:true ~chain_id ~epoch:42L ~nonce:1L ~header permission |> get in
  let raw = Funding.encode_deposit deposit in
  expect "payment reused execution expiry" (Result.is_ok (Funding.verify_deposit
    ~chain_id ~epoch:200L ~header raw));
  expect "payment after signed deadline" (Result.is_error (Funding.verify_deposit
    ~chain_id ~epoch:501L ~header raw));
  expect "refund before signed deadline" (Result.is_error (Funding.return_deposit ~chain_id ~epoch:500L deposit));
  expect "refund after signed deadline" (Result.is_ok (Funding.return_deposit ~chain_id ~epoch:501L deposit));
  expect "unsigned longer deadline" (Result.is_error (Funding.prepare_until
    ~expires_epoch:501L ~call ~target:Funding.escrow ~nonce:1L));
  expect "late work newly funded" (Result.is_error (Funding.reserve ~extended:true
    ~chain_id ~epoch:51L ~nonce:1L ~header permission))

let test_fee_refusal () =
  List.iter (fun overlay -> List.iter (fun host ->
    let call, bill = payment_case Bill.Complete in
    with_payment overlay [call] (fun _ _ backend ->
      let tx = payment_tx [bill] 2 in
      let before = payment_state backend [bill] in
      let verify ~store:_ ~epoch:_ ~funded:_ _ = Lwt.return_error
        (if host then Bill.Unavailable "worker unavailable" else Bill.Rejected "witness refused") in
      let backend = Epoch.{backend with compute = Some verify} in
      if host then begin
        let failed = try ignore (run (Epoch.apply_compute_effect ~backend ~env:payment_env tx)); false with
          | Octra_core.Exec_resource.Unavailable Octra_core.Exec_resource.Host -> true in
        expect "host failure became paid refusal" failed;
        expect "host failure changed funds" (payment_state backend [bill] = before)
      end else begin
        let account = backend.ops.find_opt tx.from |> Option.get in
        let escrow = payment_balance backend Funding.escrow in
        (match run (Epoch.apply_compute_effect ~backend ~env:payment_env tx) with
        | Ok (Epoch.Rejected_after_fee {fee; reason; _}) ->
          expect "refused witness wrong fee" (Z.equal fee tx.ou && reason = "witness refused")
        | _ -> failwith "refused witness not charged");
        let after = backend.ops.find_opt tx.from |> Option.get in
        expect "refused witness wrong nonce" (after.nonce = tx.nonce);
        expect "refused witness wrong balance" (Z.equal after.balance (Z.sub account.balance tx.ou));
        expect "refused witness changed escrow" (Z.equal escrow (payment_balance backend Funding.escrow));
        let before = payment_state backend [bill] in
        expect "refused witness replay accepted"
          (Result.is_error (run (Epoch.apply_compute_effect ~backend ~env:payment_env tx)));
        expect "refused witness replay charged twice" (payment_state backend [bill] = before)
      end)) [false; true]) [false; true]

let main () =
  test_empty_inputs ();
  test_payment_deadline ();
  test_fee_refusal ();
  test_payment_gate ();
  test_payment_tx ();
  test_payment_wrong ();
  test_payment_prefix ();
  test_payment_refusal ();
  test_payment_unreadable ();
  test_payment_faults ();
  test_transactions ();
  test_epoch_transfer ();
  test_context ();
  test_conservation ();
  test_amounts ();
  test_fees ();
  test_epoch_fee ();
  test_private_input ();
  test_faults ();
  test_outer_abort ();
  test_reopen ();
  test_bad_record ();
  test_funding_wait ();
  test_crash ();
  test_bill_checks ();
  test_settlement ();
  test_unfunded_bill ();
  test_settle_reopen ();
  test_settle_faults ();
  test_pay_crash ();
  print_endline "status = pass test = compute_deposit"

let () = match Array.to_list Sys.argv with
  | [_; "--pay-crash"; path; phase] -> pay_child path phase
  | [_; "--crash"; path; phase; mode] -> crash_child path phase mode
  | _ -> main ()