(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Octra_node_runtime.Node_rest_facade
module Ledger = Octra_core.Ledger
module Store_irmin = Octra_core.Store_irmin
module Transaction = Octra_core.Transaction
module Staging = Octra_core.Tx_staging

let fail msg =
  failwith ("test_rest_admit: " ^ msg)

let expect label cond =
  if not cond then fail label

let temp_dir () =
  Test_workspace.unique_path "octra_node_rest_facade"

let ledger () =
  let store = Lwt_main.run (Store_irmin.open_store (temp_dir ())) in
  Ledger.create store

let standard_tx () =
  Transaction.{
    from = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb";
    to_ = "oct5TWVJk7LZmzEeU73KAwd8HRuQjt2sdBiagm3rxcWDzYH";
    amount = Z.of_int 1;
    nonce = 1;
    ou = Z.of_int 1;
    timestamp = Unix.gettimeofday ();
    signature = "sig";
    public_key = None;
    message = None;
    op_type = Standard;
    encrypted_data = None;
  }

let runtime () =
  R.{
    swarm_ref = ref None;
    duty_head = (fun () -> None);
    preverify_admit = (fun _ -> Ok ());
    save_drops = ignore;
    find_drop = (fun _ -> None);
    drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
  }

let test_max_timestamp_drift () =
  expect "timestamp drift" (Float.equal R.max_timestamp_drift 300.0)

let test_missing_sender () =
  let runtime = runtime () in
  match R.validate_and_submit_tx runtime (ledger ()) (standard_tx ()) with
  | Error ("sender_not_found", _) -> ()
  | Error (tag, reason) ->
    fail ("unexpected error " ^ tag ^ ": " ^ reason)
  | Ok _ ->
    fail "missing sender accepted"

let with_env name value run =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv name (Option.value previous ~default:""))
    run

let test_observer_submission_policy () =
  with_env "OCTRA_CONSENSUS_MODE" "observer" (fun () ->
    with_env "OCTRA_OBSERVER_RELAY_SUBMITS" "" (fun () ->
      let runtime = runtime () in
      match R.validate_and_submit_tx runtime (ledger ()) (standard_tx ()) with
      | Error ("read_only_observer", _) -> ()
      | Error (tag, reason) ->
        fail ("unexpected observer error " ^ tag ^ ": " ^ reason)
      | Ok _ ->
        fail "read-only observer accepted submission");
    with_env "OCTRA_OBSERVER_RELAY_SUBMITS" "1" (fun () ->
      let runtime = runtime () in
      match R.validate_and_submit_tx runtime (ledger ()) (standard_tx ()) with
      | Error ("sender_not_found", _) -> ()
      | Error (tag, reason) ->
        fail ("unexpected relay observer error " ^ tag ^ ": " ^ reason)
      | Ok _ ->
        fail "relay observer accepted missing sender"))

let test_preverify () =
  Staging.clear ();
  let ledger = ledger () in
  let item = { (standard_tx ()) with Transaction.ou = Z.of_int 10_000 } in
  begin
    match Ledger.add_account ledger item.Transaction.from (Z.of_int 1_000_000) with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  let admitted = ref [] in
  let runtime =
    R.{
      swarm_ref = ref None;
      duty_head = (fun () -> None);
      preverify_admit = (fun tx ->
        admitted := Transaction.hash tx :: !admitted;
        Ok ());
      save_drops = ignore;
      find_drop = (fun _ -> None);
      drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
    }
  in
  let tx_hash =
    match R.add_tx_to_staging ~relay:false runtime ledger item with
    | Ok hash -> hash
    | Error reason -> fail ("staging rejected transaction " ^ reason)
  in
  expect "preverify started after staging admission"
    (!admitted = [tx_hash]);
  Staging.clear ();
  let runtime =
    R.{
      swarm_ref = ref None;
      duty_head = (fun () -> None);
      preverify_admit = (fun _ -> Error "pre_verify_busy pending = 6 limit = 6");
      save_drops = ignore;
      find_drop = (fun _ -> None);
      drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
    }
  in
  begin
    match R.add_tx_to_staging ~relay:false runtime ledger item with
    | Error "pre_verify_busy pending = 6 limit = 6" -> ()
    | Error reason -> fail ("unexpected preverify error " ^ reason)
    | Ok _ -> fail "busy preverify left transaction in staging"
  end;
  expect "busy preverify rolled staging back" (Staging.all () = []);
  Staging.clear ()

let test_eviction () =
  Staging.clear ();
  let ledger = ledger () in
  let first = { (standard_tx ()) with Transaction.ou = Z.of_int 10_000 } in
  let replacement = { first with Transaction.ou = Z.of_int 11_000 } in
  begin
    match Ledger.add_account ledger first.Transaction.from (Z.of_int 1_000_000) with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  let saved = ref [] in
  let runtime =
    R.{
      swarm_ref = ref None;
      duty_head = (fun () -> None);
      preverify_admit = (fun _ -> Ok ());
      save_drops = (fun drops -> saved := drops @ !saved);
      find_drop = (fun _ -> None);
      drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
    }
  in
  begin
    match R.add_tx_to_staging ~relay:false runtime ledger first with
    | Ok _ -> ()
    | Error reason -> fail reason
  end;
  begin
    match R.add_tx_to_staging ~relay:false runtime ledger replacement with
    | Ok _ -> ()
    | Error reason -> fail reason
  end;
  begin
    match !saved with
    | [drop] ->
      expect "persisted eviction reason"
        (drop.Staging.d_reason = Staging.Evicted);
      expect "persisted eviction hash"
        (String.equal drop.d_hash (Transaction.hash first))
    | _ -> fail "eviction outcome was not persisted once"
  end;
  Staging.clear ()

let test_duty_refresh () =
  Staging.clear ();
  let ledger = ledger () in
  let head = ref 10L in
  let saved = ref [] in
  let checked = ref 0 in
  let runtime = R.{
    (runtime ()) with
    duty_head = (fun () -> Some !head);
    save_drops = (fun drops -> saved := drops @ !saved);
    preverify_admit = (fun _ -> incr checked; Ok ());
  } in
  let duty epoch =
    let message = Yojson.Safe.to_string (`Assoc [
      "consensus_pubkey", `String "key";
      "head_epoch", `String (Int64.to_string epoch);
      "state_root", `String (String.make 64 'a');
    ]) in
    { (standard_tx ()) with Transaction.amount = Z.zero;
      ou = Z.of_int 1_000; message = Some message; op_type = ValidatorReady }
  in
  let prior = duty 10L in
  ignore (Ledger.add_account ledger prior.from (Z.of_int 1_000_000)
          |> Result.get_ok);
  let submit = R.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
  let hash = Transaction.hash prior in
  expect "initial duty admitted" (submit prior = Ok hash);
  expect "repeated duty is idempotent" (submit prior = Ok hash);
  expect "repeated duty not verified twice" (!checked = 1);
  head := 11L;
  let next = duty 11L in
  expect "next head replaces expired duty at equal fee"
    (submit next = Ok (Transaction.hash next));
  expect "prior head cannot return to queue"
    (submit prior = Error "validator ready head has expired");
  expect "one pending duty" (Staging.staging_size () = 1);
  expect "drop persisted" (List.map (fun d -> d.Staging.d_hash) !saved = [hash]);
  expect "refresh does not consume nonce" ((Ledger.find ledger prior.from).nonce = 0);
  expect "refresh does not debit fee"
    (Z.equal (Ledger.find ledger prior.from).balance (Z.of_int 1_000_000));
  head := 12L;
  R.expire_duty runtime ();
  expect "head change removes expired duty" (Staging.staging_size () = 0);
  let confirmed = ref 0 in
  for epoch = 13 to 92 do
    head := Int64.of_int epoch;
    R.expire_duty runtime ();
    let nonce = Staging.duty_nonce prior.from !confirmed |> Option.get in
    let next = { (duty !head) with Transaction.nonce } in
    let hash = Transaction.hash next in
    expect "epoch refresh admitted" (submit next = Ok hash);
    expect "epoch has one local duty" (Staging.staging_size () = 1);
    expect "queued duty blocks a second nonce"
      (Staging.duty_nonce prior.from !confirmed = None);
    if epoch mod 4 = 0 then begin
      ignore (Ledger.debit ledger prior.from next.ou nonce |> Result.get_ok);
      Staging.remove_processed [hash];
      confirmed := nonce
    end
  done;
  expect "only confirmations advance nonce" (!confirmed = 20);
  expect "retries do not charge fees"
    (Z.equal (Ledger.find ledger prior.from).balance (Z.of_int 980_000));
  Staging.clear ()

let () =
  test_max_timestamp_drift ();
  test_missing_sender ();
  test_observer_submission_policy ();
  test_preverify ();
  test_eviction ();
  test_duty_refresh ();
  print_endline "node runtime rest facade tests passed"