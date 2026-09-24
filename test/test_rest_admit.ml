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

let store_path () =
  Test_workspace.unique_path "octra_node_rest_facade"

let ledger () =
  let store = Lwt_main.run (Store_irmin.open_store (store_path ())) in
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
    duty_head = (fun () -> Some (!head, Octra_core.Rule_graph.Prior));
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

let test_duty_flood () =
  Staging.clear ();
  let ledger = ledger () in
  let runtime = R.{ (runtime ()) with
    duty_head = (fun () -> Some (10L, Octra_core.Rule_graph.Prior)) } in
  let message = Yojson.Safe.to_string (`Assoc [
    "consensus_pubkey", `String "key";
    "head_epoch", `String "10";
    "state_root", `String (String.make 64 'a');
  ]) in
  let duty nonce =
    { (standard_tx ()) with Transaction.nonce; amount = Z.zero;
      ou = Z.of_int 1_000; message = Some message; op_type = ValidatorReady }
  in
  let first = duty 1 in
  ignore (Ledger.add_account ledger first.from (Z.of_int 1_000_000)
          |> Result.get_ok);
  let submit = R.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
  for nonce = 2 to 732 do
    expect "duty cannot reserve future nonce"
      (submit (duty nonce) = Error "validator ready requires next confirmed nonce")
  done;
  expect "future duty flood leaves queue empty" (Staging.staging_size () = 0);
  let hash = Transaction.hash first in
  expect "next confirmed duty accepted" (submit first = Ok hash);
  expect "duty repeat remains idempotent" (submit first = Ok hash);
  let replacement = { first with Transaction.ou = Z.of_int 1_001 } in
  let validate tx = Result.map_error Octra_node_runtime.Tx_view.staging_error (submit tx) in
  let response = Lwt_main.run (Octra_node_runtime.Submit_rpc.submit ~validate
    (`List [Transaction.to_yojson replacement])) in
  (match response with
   | Error error ->
     let expected = "duplicate nonce (fee rate bump < 10%)" in
     expect "replacement RPC preserves admission cause"
       (error.Octra_core.Rpc.code = 106 && error.data = Some (`String expected));
     expect "replacement RPC retains original queue entry"
       (Staging.find_by_hash hash = Some first && Staging.staging_size () = 1)
   | Ok _ -> fail "underpaid replacement passed RPC");
  expect "second duty cannot consume another slot"
    (Result.is_error (submit (duty 2)) && Staging.staging_size () = 1);
  let raised = { first with Transaction.ou = Z.of_int 1_100 } in
  let raised_hash = Transaction.hash raised in
  expect "duty fee replacement remains available" (submit raised = Ok raised_hash);
  expect "replacement occupies same slot" (Staging.staging_size () = 1);
  let ordinary = { (standard_tx ()) with Transaction.nonce = 2; ou = Z.of_int 1_000 } in
  let ordinary_hash = Transaction.hash ordinary in
  expect "ordinary successor may queue" (submit ordinary = Ok ordinary_hash);
  expect "rejected duty preserves ordinary successor"
    (Result.is_error (submit (duty 3))
     && Staging.find_by_hash ordinary_hash = Some ordinary);
  ignore (Ledger.debit ledger first.from raised.ou 1 |> Result.get_ok);
  Staging.remove_processed [raised_hash];
  expect "duty does not replace ordinary transaction at equal fee"
    (Result.is_error (submit (duty 2))
     && Staging.find_by_hash ordinary_hash = Some ordinary);
  ignore (Ledger.debit ledger first.from ordinary.ou 2 |> Result.get_ok);
  Staging.remove_processed [ordinary_hash];
  let next = duty 3 in
  expect "confirmed progress reopens duty slot"
    (submit next = Ok (Transaction.hash next));
  Staging.clear ();
  let prior = R.{ runtime with duty_head = (fun () -> None) } in
  let future = duty 100 in
  expect "prior policy retains general queue behavior"
    (R.add_tx_to_staging ~relay:false ~bft_mode:true prior ledger future
     = Ok (Transaction.hash future));
  Staging.clear ()

let test_duty_window () =
  let module View = Octra_node_runtime.Tx_view in
  let base = standard_tx () in
  let cases = [
    None, 0, 732, Transaction.ValidatorReady, true;
    Some 10L, 0, 1, Transaction.ValidatorReady, true;
    Some 10L, 0, 2, Transaction.ValidatorReady, false;
    Some 10L, 4, 732, Transaction.ValidatorReady, false;
    Some 10L, 731, 732, Transaction.ValidatorReady, true;
    Some 10L, max_int - 1, max_int, Transaction.ValidatorReady, true;
    Some 10L, 0, max_int, Transaction.ValidatorReady, false;
    Some 10L, 0, 732, Transaction.Standard, true;
    Some 10L, 0, 732, Transaction.ValidatorExit, true;
    Some 10L, 0, 732, Transaction.ValidatorWithdraw, true;
  ] in
  List.iter (fun (head, confirmed_nonce, nonce, op_type, accepted) ->
    let item = { base with Transaction.nonce; op_type } in
    expect "duty nonce policy"
      (Result.is_ok (View.duty_nonce_admission ~head ~confirmed_nonce item) = accepted))
    cases;
  expect "duty gap is a queue error, not an execution rejection"
    (View.staging_error "validator ready requires next confirmed nonce"
     = ("nonce_too_far", "validator ready requires next confirmed nonce"))

let test_duty_peer_head () =
  Staging.clear ();
  let ledger = ledger () in
  let head = ref 10L in
  let runtime = R.{ (runtime ()) with
    duty_head = (fun () -> Some (!head, Octra_core.Rule_graph.Prior)) } in
  let message = Yojson.Safe.to_string (`Assoc [
    "consensus_pubkey", `String "key";
    "head_epoch", `String "11";
    "state_root", `String (String.make 64 'a');
  ]) in
  let item = { (standard_tx ()) with Transaction.nonce = 2;
    amount = Z.zero; ou = Z.of_int 1_000; message = Some message;
    op_type = ValidatorReady } in
  ignore (Ledger.add_account ledger item.from (Z.of_int 1_000_000)
          |> Result.get_ok);
  let submit = R.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
  let hash = Transaction.hash item in
  expect "behind peer may relay a newer head" (submit item = Ok hash);
  expect "relaying newer head does not consume local nonce"
    ((Ledger.find ledger item.from).nonce = 0);
  Staging.clear ();
  head := 11L;
  expect "matching head uses its own committed nonce"
    (submit item = Error "validator ready requires next confirmed nonce");
  ignore (Ledger.debit ledger item.from Z.zero 1 |> Result.get_ok);
  expect "matching nonce admits the same signed payload" (submit item = Ok hash);
  Staging.clear ();
  let cap = Int64.of_int Octra_consensus.C_catchup.range_epochs in
  List.iter (fun head_epoch ->
    List.iter (fun nonce ->
      let message = Yojson.Safe.to_string (`Assoc [
        "consensus_pubkey", `String "key";
        "head_epoch", `String (Int64.to_string head_epoch);
        "state_root", `String (String.make 64 'a');
      ]) in
      let item = { item with Transaction.message = Some message; nonce } in
      let accepted = head_epoch = Int64.add !head cap in
      expect "future duty head queue limit"
        (Result.is_ok (submit item) = accepted);
      expect "refused future duty leaves queue empty"
        (accepted || Staging.staging_size () = 0);
      Staging.clear ()) [2; 3; 732])
    [Int64.add !head cap; Int64.succ (Int64.add !head cap); Int64.max_int]

let test_delivery_retry () =
  Mirage_crypto_rng_unix.use_default ();
  let module V = Octra_node_runtime.Tx_view in
  let module G = Octra_core.Rule_graph in
  let secret, public = Mirage_crypto_ec.Ed25519.generate () in
  let public = Mirage_crypto_ec.Ed25519.pub_to_octets public |> Base64.encode_exn in
  let secret = Mirage_crypto_ec.Ed25519.priv_to_octets secret |> Base64.encode_exn in
  let address = Octra_core.Crypto.Address.address_from_pubkey public in
  let ledger = ledger () in
  ignore (Ledger.add_account ledger address (Z.of_int 1_000_000) |> Result.get_ok);
  let head = ref 100L in
  let runtime = R.{ (runtime ()) with duty_head = (fun () -> Some (!head, G.Active)) } in
  let duty epoch =
    let message = Yojson.Safe.to_string (`Assoc [
      "consensus_pubkey", `String public;
      "head_epoch", `String (Int64.to_string epoch);
      "head_proposal_id", `String (String.make 64 'a');
      "state_root", `String (String.make 64 'b');
    ]) in
    Transaction.sign_with_privkey
      { (standard_tx ()) with from = address; to_ = address;
        public_key = Some public; amount = Z.zero; ou = Z.of_int 1_000;
        timestamp = 1.; message = Some message; op_type = ValidatorReady } secret
  in
  let item = duty 100L in
  let hash = Transaction.hash item in
  let bytes = Transaction.to_yojson item in
  let submit = R.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
  let timed mode tx = V.pre_route_admission ~duty:(Some (!head, mode))
    ~now:10_000. ~max_timestamp_drift:300. ~observer_rpc_mode:false ~bft_mode:true tx in
  Staging.clear ();
  Fun.protect ~finally:Staging.clear (fun () ->
    expect "initial delayed duty" (submit item = Ok hash);
    head := 101L;
    expect "overlapping reference cannot replace" (Result.is_error (submit (duty 101L)));
    expect "overlap retains signed bytes" (Staging.find_by_hash hash = Some item);
    expect "old timestamp retry allowed" (timed G.Active item = Ok ());
    expect "prior timestamp check retained" (Result.is_error (timed G.Prior item));
    expect "ordinary timestamp check retained"
      (Result.is_error (timed G.Active {item with op_type = Standard}));
    expect "future timestamp refused"
      (Result.is_error (timed G.Active {item with timestamp = 20_000.}));
    expect "signature remains required"
      (V.signature_admission ~account_public_key:None item = Ok ()
       && Result.is_error (V.signature_admission ~account_public_key:None
          {item with message = (duty 101L).message}));
    ignore (Staging.remove_by_hash hash);
    expect "pool eviction retains valid repeat" (submit item = Ok hash);
    expect "same bytes after repeat" (Transaction.to_yojson (Option.get (Staging.find_by_hash hash)) = bytes);
    head := 102L;
    R.expire_duty runtime ();
    expect "last delivery epoch retained" (Staging.find_by_hash hash = Some item);
    head := 103L;
    expect "expired timestamp exception refused" (Result.is_error (timed G.Active item));
    expect "expired duty removed before replacement" (submit (duty 103L) = Ok (Transaction.hash (duty 103L)));
    expect "one reserved nonce" (Staging.staging_size () = 1);
    expect "no nonce or fee increase" ((Ledger.find ledger address).nonce = 0);
    let ordinary = {item with op_type = Standard; amount = Z.one} in
    head := 105L;
    expect "current duty keeps replacement fee rule" (Result.is_error (submit ordinary));
    let tail = {ordinary with nonce = 2} in
    expect "ordinary successor may wait" (Result.is_ok (submit tail));
    head := 106L;
    expect "ordinary replaces expired duty at equal fee" (Result.is_ok (submit ordinary));
    expect "successor preserved after expiry"
      (Staging.find_by_hash (Transaction.hash tail) = Some tail);
    let selected = Staging.ready_epoch_txs ~accept:(fun _ -> true)
      ~capacity:(Z.of_int 1_000_000) ~confirmed_nonce:(fun _ -> Some 0) in
    expect "replacement restores contiguous prefix"
      (List.map (fun tx -> tx.Transaction.nonce) selected = [1; 2]);
    Staging.clear ();
    let ordinary = {item with op_type = Standard; amount = Z.one} in
    expect "ordinary pending accepted" (Result.is_ok (submit ordinary));
    head := 106L;
    expect "ordinary protected from duty replacement" (Result.is_error (submit (duty 106L)));
    expect "ordinary still present" (Staging.find_by_hash (Transaction.hash ordinary) = Some ordinary))

let () =
  test_delivery_retry ();
  test_max_timestamp_drift ();
  test_missing_sender ();
  test_observer_submission_policy ();
  test_preverify ();
  test_eviction ();
  test_duty_refresh ();
  test_duty_window ();
  test_duty_flood ();
  test_duty_peer_head ();
  print_endline "node runtime rest facade tests passed"