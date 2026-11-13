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
  let secret = Base64.encode_exn (String.make 32 '\001') in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn secret) |> Result.get_ok in
  let public = Mirage_crypto_ec.Ed25519.pub_of_priv key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  Transaction.sign_with_privkey Transaction.{
    from = Octra_core.Crypto.Address.address_from_pubkey public;
    to_ = "oct5TWVJk7LZmzEeU73KAwd8HRuQjt2sdBiagm3rxcWDzYH";
    amount = Z.of_int 1;
    nonce = 1;
    ou = Z.of_int 1;
    timestamp = Unix.gettimeofday ();
    signature = "sig";
    public_key = Some public;
    message = None;
    op_type = Standard;
    encrypted_data = None;
  } secret

let runtime () =
  R.{
    swarm_ref = ref None;
    proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
    compute_mode = (fun () -> Octra_core.Rule_graph.Prior);
    settle_mode = (fun () -> Octra_core.Rule_graph.Prior);
    marks_mode = (fun () -> Octra_core.Rule_graph.Prior);
    signing = (fun () -> Octra_core.Tx_signature.Historical); queue_head = (fun () -> None);
    duty_head = (fun () -> None);
    preverify_admit = (fun _ -> Ok ());
    save_drops = ignore;
    find_drop = (fun _ -> None);
    drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
  }

let test_max_timestamp_drift () =
  expect "timestamp drift" (Float.equal R.max_timestamp_drift 300.0)

let test_relay_size () =
  Staging.clear ();
  let item = standard_tx () in
  let item = {item with Transaction.op_type = ProgramDeploy;
    message = Some (String.make 5_030_000 'a')} in
  let calls = ref 0 in
  let runtime = {(runtime ()) with R.preverify_admit = (fun _ -> incr calls; Ok ())} in
  let result = R.add_tx_to_staging runtime (ledger ()) item in
  expect "relay size rejection differs" (result = Error "transaction JSON exceeds relay limit");
  expect "relay size reached preverify" (!calls = 0);
  expect "relay size retained transaction" (Staging.find_by_hash (Transaction.hash item) = None)

let test_missing_sender () =
  let runtime = runtime () in
  match Lwt_main.run (R.validate_and_submit_tx runtime (ledger ()) (standard_tx ())) with
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
      match Lwt_main.run (R.validate_and_submit_tx runtime (ledger ()) (standard_tx ())) with
      | Error ("read_only_observer", _) -> ()
      | Error (tag, reason) ->
        fail ("unexpected observer error " ^ tag ^ ": " ^ reason)
      | Ok _ ->
        fail "read-only observer accepted submission");
    with_env "OCTRA_OBSERVER_RELAY_SUBMITS" "1" (fun () ->
      let runtime = runtime () in
      match Lwt_main.run (R.validate_and_submit_tx runtime (ledger ()) (standard_tx ())) with
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
      proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
      compute_mode = (fun () -> Octra_core.Rule_graph.Prior);
      settle_mode = (fun () -> Octra_core.Rule_graph.Prior);
      marks_mode = (fun () -> Octra_core.Rule_graph.Prior);
      signing = (fun () -> Octra_core.Tx_signature.Historical); queue_head = (fun () -> None);
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
      proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
      compute_mode = (fun () -> Octra_core.Rule_graph.Prior);
      settle_mode = (fun () -> Octra_core.Rule_graph.Prior);
      marks_mode = (fun () -> Octra_core.Rule_graph.Prior);
      signing = (fun () -> Octra_core.Tx_signature.Historical); queue_head = (fun () -> None);
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
  let saved = ref [] in
  let ready = {runtime with R.preverify_admit = (fun _ -> Ok ());
    save_drops = (fun drops -> saved := drops @ !saved)} in
  let original = match R.add_tx_to_staging ~relay:false ready ledger item with
    | Ok hash -> hash | Error reason -> fail reason in
  let replacement = {item with Transaction.ou = Z.of_int 20_000} in
  List.iter (fun preverify_admit ->
    let refused = {ready with R.preverify_admit} in
    expect "refused replacement accepted"
      (Result.is_error (R.add_tx_to_staging ~relay:false refused ledger replacement));
    expect "refused replacement removed original"
      (Staging.find_by_hash original = Some item);
    expect "refused replacement recorded drops" (!saved = []);
    expect "refused replacement changed size" (Staging.staging_size () = 1))
    [(fun _ -> Error "pre_verify_busy"); (fun _ -> failwith "preverify stopped")];
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
      proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
      compute_mode = (fun () -> Octra_core.Rule_graph.Prior);
      settle_mode = (fun () -> Octra_core.Rule_graph.Prior);
      marks_mode = (fun () -> Octra_core.Rule_graph.Prior);
      signing = (fun () -> Octra_core.Tx_signature.Historical); queue_head = (fun () -> None);
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
  let validate tx = Lwt.return (Result.map_error Octra_node_runtime.Tx_view.staging_error (submit tx)) in
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
      (Result.is_ok (View.duty_nonce_check ~head ~confirmed_nonce item) = accepted))
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
  let timed mode tx = V.pre_route_check ~duty:(Some (!head, mode))
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
      (V.signature_check ~signing:Octra_core.Tx_signature.Historical ~account_public_key:None item = Ok ()
       && Result.is_error (V.signature_check ~signing:Octra_core.Tx_signature.Historical ~account_public_key:None
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
    let suffix = {ordinary with nonce = 2} in
    expect "ordinary successor may wait" (Result.is_ok (submit suffix));
    head := 106L;
    expect "ordinary replaces expired duty at equal fee" (Result.is_ok (submit ordinary));
    expect "successor preserved after expiry"
      (Staging.find_by_hash (Transaction.hash suffix) = Some suffix);
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

let test_compute_paths () =
  let module G = Octra_core.Rule_graph in
  let module P = Octra_node_runtime.P2p_tx_handler in
  let chain_id = "octra-devnet-9871-cluster" in
  let activation = (Option.get (G.vm_bytes_activation_for_chain chain_id)).activation_epoch in
  let head = ref (activation - 2) in
  let count = ref 0 in
  let runtime = R.{(runtime ()) with
    compute_mode = (fun () -> G.vm_bytes_at ~chain_id ~epoch:(!head + 1));
    settle_mode = (fun () -> G.settle_at ~chain_id ~epoch:(!head + 1));
    marks_mode = (fun () -> Octra_core.Rule_graph.Prior);
    preverify_admit = (fun _ -> incr count; Ok ())} in
  let ledger = ledger () in
  let base = standard_tx () in
  Ledger.add_account ledger base.from (Z.of_int 20_000_000) |> Result.get_ok;
  let secret = Base64.encode_exn (String.make 32 '\001') in
  let check op_type =
    let tx = Transaction.sign_with_privkey {base with op_type;
      to_ = Octra_core.Compute_funding.escrow;
      amount = (if op_type = Transaction.ComputeFund then Z.one else Z.zero);
      ou = (if op_type = Transaction.ComputeSettle then Z.of_int 10_000_000 else Z.of_int 10_000);
      message = Some (String.make 64 'a')} secret in
    List.iter (fun height ->
      head := height;
      let active = height + 1 >= activation in
      let accepted = active && op_type <> Transaction.ComputeSettle in
      let reason = if active then "compute settlement not active" else "compute transfers not active" in
      List.iter (fun route ->
        Staging.clear (); count := 0;
        let before = Ledger.find ledger tx.from in
        (match route with
        | 0 ->
          let result = Lwt_main.run (R.validate_and_submit_tx runtime ledger tx) in
          expect "compute RPC activation" (Result.is_ok result = accepted);
          if not accepted then expect "compute RPC exact error"
            (result = Error ("malformed_transaction", reason))
        | 1 ->
          let result = R.add_tx_to_staging ~relay:false runtime ledger tx in
          expect "compute queue activation" (Result.is_ok result = accepted);
          if not accepted then expect "compute queue exact error"
            (result = Error reason)
        | _ ->
          let gossip = ref 0 in
          let json = Transaction.to_yojson tx |> Yojson.Safe.to_string in
          let payload = if route = 2 then Octra_net.P2p_tx_gossip.encode
              (Octra_net.P2p_tx_gossip.Tx {hash = Transaction.hash tx; tx_json = json})
            else Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer json) in
          Lwt_main.run (P.handle P.{guard = Octra_net.P2p_tx_gossip_guard.create (); payload;
            peer_id = "compute-peer"; has_tx = (fun _ -> false); find_tx = (fun _ -> None);
            sender_pk = (fun _ -> tx.public_key);
            find_account = Ledger.find_opt ledger;
            add_tx = R.add_tx_to_staging ~relay:false runtime ledger;
            send_payload = (fun _ -> fail "unexpected compute fetch");
            send_batch = (fun _ -> fail "unexpected compute fetch");
            broadcast_payload = (fun _ -> incr gossip);
            report_bad_peer = (fun _ -> fail "compute peer punished");
            close_peer = (fun () -> fail "compute peer closed");
            now = Unix.gettimeofday; signing = (fun () -> Octra_core.Tx_signature.Historical); max_drift = 300.});
          expect "compute gossip activation" (!gossip = if accepted then 1 else 0));
        expect "compute queue size" (Staging.staging_size () = if accepted then 1 else 0);
        expect "compute preverification activation" (!count = if accepted then 1 else 0);
        expect "compute receipt leaves balance" (Ledger.find ledger tx.from = before)) [0; 1; 2; 3])
      [activation - 2; activation - 1; activation] in
  Fun.protect ~finally:Staging.clear (fun () ->
    List.iter check [Transaction.ComputeFund; Transaction.ComputeReturn; Transaction.ComputeSettle])

let devnet_payloads () =
  let module F = Octra_core.Crypto.FheBalance in
  let public, secret =
    Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\041') in
  let cipher = Pvac_ffi.enc_value_seeded public secret 1L (Bytes.make 32 '\042') in
  let blinding = Bytes.make 32 '\043' in
  let zero_proof = Pvac_ffi.make_zero_proof_bound ~keyed:false
    public secret cipher 1L blinding |> F.encode_zero_proof in
  let amount_commitment = Pvac_ffi.pedersen_commit_amount 1L blinding
    |> Bytes.to_string |> Base64.encode_exn in
  let commitment = Pvac_ffi.commit_ct public cipher
    |> Bytes.to_string |> Base64.encode_exn in
  let cipher = F.encode_cipher cipher in
  let encrypt = Yojson.Safe.to_string (`Assoc [
    "cipher", `String cipher;
    "amount_commitment", `String amount_commitment;
    "zero_proof", `String zero_proof;
    "blinding", `String (Base64.encode_exn (Bytes.to_string blinding));
  ]) in
  let stealth = Yojson.Safe.to_string (`Assoc [
    "version", `Int 5;
    "delta_cipher", `String cipher;
    "commitment", `String commitment;
    "range_proof_delta", `String "rp_v1|YQ==";
    "range_proof_balance", `String "rp_v1|Yg==";
    "eph_pub", `String "eph";
    "stealth_tag", `String (String.make 32 '1');
    "enc_amount", `String "amount";
    "claim_pub", `String (String.make 64 '2');
    "amount_commitment", `String amount_commitment;
    "send_zero_proof", `String "";
  ]) in
  encrypt, stealth

let test_devnet_profile () =
  let module Submit = Octra_node_runtime.Submit_rpc in
  let module Rpc = Octra_core.Rpc in
  expect "devnet BFT mode" (Sys.getenv_opt "OCTRA_CONSENSUS_MODE" = Some "bft");
  expect "devnet profile selected"
    (Sys.getenv_opt "OCTRA_BFT_RELEASE_PROFILE" = Some "devnet_full_v1");
  let encrypt, stealth = devnet_payloads () in
  let base = standard_tx () in
  let secret = Base64.encode_exn (String.make 32 '\001') in
  let cases = [
    Transaction.Standard, base.to_, Z.one, None, None, true;
    Transaction.EncryptOp, base.from, Z.one, None, Some encrypt, true;
    Transaction.StealthOp, "stealth", Z.zero, None, Some stealth, true;
    Transaction.ComputeFund, Octra_core.Compute_funding.escrow, Z.one,
      Some (String.make 64 'a'), None, true;
    Transaction.PrivateOp, base.to_, Z.zero, None, None, false;
  ] in
  Test_workspace.with_dir "rest_devnet" (fun dir ->
    let store = Lwt_main.run (Store_irmin.open_store (Filename.concat dir "store")) in
    Fun.protect
      ~finally:(fun () -> Staging.clear (); Lwt_main.run (Store_irmin.close store))
      (fun () ->
        let ledger = Ledger.create store in
        Ledger.add_account ledger base.from (Z.of_int 1_000_000) |> Result.get_ok;
        let checked = ref [] in
        let drops = ref [] in
        let runtime = R.{ (runtime ()) with
          compute_mode = (fun () -> Octra_core.Rule_graph.Active);
          preverify_admit = (fun tx -> checked := tx :: !checked; Ok ());
          save_drops = (fun items -> drops := items @ !drops);
        } in
        let validate = R.validate_and_submit_tx runtime ledger in
        List.iter (fun (op_type, to_, amount, message, encrypted_data, accepted) ->
          let tx = Transaction.sign_with_privkey
            {base with op_type; to_; amount; message; encrypted_data;
              ou = Z.of_int 10_000; timestamp = Unix.gettimeofday ()} secret in
          let hash = Transaction.hash tx in
          let json = Transaction.to_yojson tx in
          let reason = Transaction.bft_reject_reason op_type in
          let name = "devnet " ^ Transaction.op_type_to_string op_type in
          let routes = [
            "validate", (fun () ->
              expect (name ^ " validate result")
                (Lwt_main.run (validate tx) = if accepted then Ok hash
                  else Error ("unsupported_operation", reason)));
            "submit", (fun () ->
              let expected = if accepted then Ok (`Assoc [
                "tx_hash", `String hash;
                "status", `String "accepted";
                "nonce", `Int tx.nonce;
                "ou_cost", `String (Z.to_string (Transaction.ou_cost tx));
              ]) else Error (Rpc.err 116 "unsupported operation" (Some (`String reason))) in
              expect (name ^ " submit result")
                (Lwt_main.run (Submit.submit ~validate (`List [json])) = expected));
            "private_transfer", (fun () ->
              let expected = if accepted then Ok (`Assoc [
                "tx_hash", `String hash;
                "status", `String "pending";
              ]) else Error (Rpc.err (-32000) ("unsupported_operation: " ^ reason) None) in
              expect (name ^ " private transfer result")
                (Lwt_main.run (Submit.private_transfer ~validate (`List [json])) = expected));
            "submit_batch", (fun () ->
              let row = if accepted then `Assoc [
                "status", `String "accepted";
                "tx_hash", `String hash;
                "nonce", `Int tx.nonce;
              ] else `Assoc [
                "status", `String "rejected";
                "reason", `String reason;
                "nonce", `Int tx.nonce;
              ] in
              expect (name ^ " batch result")
                (Lwt_main.run (Submit.submit_batch ~validate (`List [`List [json]]))
                  = Ok (`Assoc [
                    "total", `Int 1;
                    "accepted", `Int (if accepted then 1 else 0);
                    "rejected", `Int (if accepted then 0 else 1);
                    "results", `List [row];
                  ])));
          ] in
          List.iter (fun (route, submit) ->
            Staging.clear ();
            checked := [];
            drops := [];
            let before = Ledger.find ledger tx.from in
            let name = name ^ " " ^ route in
            submit ();
            expect (name ^ " queue contents")
              (Staging.staging_size () = (if accepted then 1 else 0)
               && Staging.find_by_hash hash = (if accepted then Some tx else None));
            expect (name ^ " preverify calls") (!checked = if accepted then [tx] else []);
            expect (name ^ " no drops") (!drops = []);
            expect (name ^ " sender unchanged") (Ledger.find ledger tx.from = before)) routes)
          cases))

let test_parse_wait () =
  let module Pool = Octra_core.Proof_pool in
  let module Memory = Octra_core.Compute_memory in
  let public, _ = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\045') in
  let body = Yojson.Safe.to_string (`Assoc [
    "new_pubkey", `String (Pvac_ffi.serialize_pubkey public |> Bytes.to_string |> Base64.encode_exn);
    "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
  ]) in
  List.iter (fun action ->
    Staging.clear ();
    let ledger = ledger () in
    let base = standard_tx () in
    Ledger.add_account ledger base.from (Z.of_int 20_000_000) |> Result.get_ok;
    let tx = Transaction.sign_with_privkey {base with
      to_ = base.from; amount = Z.zero; ou = Z.of_int 10_000;
      op_type = Transaction.KeySwitch; encrypted_data = Some body}
      (Base64.encode_exn (String.make 32 '\001')) in
    let signing = ref Octra_core.Tx_signature.Historical in
    let checked = ref [] in
    let runtime = {(runtime ()) with R.signing = (fun () -> !signing);
      preverify_admit = (fun tx -> checked := tx :: !checked; Ok ())} in
    let before = Memory.stats Memory.shared in
    let mutex = Mutex.create () and condition = Condition.create () in
    let released = ref false in
    let blockers = List.init Pool.capacity (fun _ -> Pool.try_run ~bytes:0 (fun () ->
      Mutex.lock mutex;
      while not !released do Condition.wait condition mutex done;
      Mutex.unlock mutex)) in
    let release () =
      Mutex.lock mutex;
      released := true;
      Condition.broadcast condition;
      Mutex.unlock mutex in
    Lwt_main.run (Lwt.finalize (fun () ->
      let open Lwt.Syntax in
      let work = R.validate_and_submit_tx runtime ledger tx in
      expect "RPC parser did not await worker" (Lwt.is_sleeping work);
      expect "RPC staged before parse" (!checked = [] && Staging.staging_size () = 0);
      begin match action with
      | "cancel" -> Lwt.cancel work
      | "signing" -> signing := Octra_core.Tx_signature.Network "changed-network"
      | "nonce" ->
        Ledger.transfer ledger ~from:base.from ~to_:base.to_ ~amount:Z.one ~fee:Z.zero 1
        |> Result.get_ok
      | _ -> ()
      end;
      release ();
      let* () = Lwt_list.iter_s (fun job -> Lwt.map (fun _ -> ()) job) blockers in
      let* result = Lwt.catch (fun () -> Lwt.map Option.some work)
        (function Lwt.Canceled -> Lwt.return_none | error -> Lwt.fail error) in
      begin match action, result with
      | "cancel", None -> ()
      | "signing", Some (Error ("invalid_signature", _)) -> ()
      | "nonce", Some (Error ("invalid_nonce", "nonce already used")) -> ()
      | "accept", Some (Ok hash) when hash = Transaction.hash tx -> ()
      | _ -> fail "RPC changed state or cancelled parse outcome changed"
      end;
      expect "RPC late staging after parse" (List.length !checked = if action = "accept" then 1 else 0);
      expect "RPC queue changed after refusal" (Staging.staging_size () = if action = "accept" then 1 else 0);
      Lwt.return_unit)
      (fun () -> release (); Lwt_list.iter_s (fun job -> Lwt.map (fun _ -> ()) job) blockers));
    expect "RPC parse leaked memory reservation" (Memory.stats Memory.shared = before);
    Staging.clear ()) ["accept"; "cancel"; "signing"; "nonce"]

let test_network_numbers () =
  let module View = Octra_node_runtime.Tx_view in
  let module Submit = Octra_node_runtime.Submit_rpc in
  let base = standard_tx () in
  let fields = match Transaction.to_yojson base with `Assoc fields -> fields | _ -> assert false in
  let numeric field value = `Assoc ((field, value) :: List.remove_assoc field fields) in
  let limit = Transaction.network_number_chars in
  expect "network number cap misses supply binary form"
    (Z.numbits Octra_core.Denomination.max_supply + 3 <= limit);
  let binary = "+0b" ^ String.concat "_" (List.init 256 (fun _ -> "1")) in
  expect "network number cap misses separated 256-bit form" (String.length binary <= limit);
  let allowed = ["1"; "+01"; "0x1"; "0X1"; "0o1"; "0O1"; "0b1"; "0B1";
    "1_"; "0__1"; binary; Z.to_string Octra_core.Denomination.max_supply;
    String.make (limit - 1) '0' ^ "1"] in
  List.iter (fun field ->
    List.iter (fun raw ->
      let json = numeric field (`String raw) in
      expect "network number changed historical decoding"
        (View.decode_network_tx json = Transaction.of_yojson json)) allowed;
    let raw = String.make (limit + 1) '1' in
    let json = numeric field (`String raw) in
    let reason = field ^ " exceeds network numeric character limit" in
    expect "historical numeric parser was restricted" (Result.is_ok (Transaction.of_yojson json));
    expect "network numeric parser accepted excess bytes" (View.decode_network_tx json = Error reason);
    expect "network numeric parser accepted excess literal"
      (View.decode_network_tx (numeric field (`Intlit raw)) = Error reason);
    expect "network numeric parser ignored duplicate field bytes"
      (View.decode_network_tx (`Assoc (fields @ [field, `String raw])) = Error reason);
    let validate _ = fail "oversized number reached submission validation" in
    List.iter (fun submit -> expect "oversized number reached RPC submission"
      (Result.is_error (Lwt_main.run (submit ())))) [
      (fun () -> Submit.submit ~validate (`List [json]));
      (fun () -> Submit.private_transfer ~validate (`List [json]));
      (fun () -> Submit.private_transfer ~validate (`List [`String (Yojson.Safe.to_string json)]))];
    let row = `Assoc ["status", `String "error"; "reason", `String reason] in
    expect "batch did not reject oversized number before validation"
      (Lwt_main.run (Submit.submit_batch ~validate (`List [`List [json]]))
        = Ok (`Assoc ["total", `Int 1; "accepted", `Int 0; "rejected", `Int 1;
          "results", `List [row]]))) ["amount"; "ou"]

let test_program_precheck () =
  with_env "OCTRA_CONSENSUS_MODE" "single" (fun () ->
  let module View = Octra_node_runtime.Tx_view in
  let module Memory = Octra_core.Compute_memory in
  Staging.clear ();
  let base = standard_tx () in
  let item = {base with Transaction.op_type = ProgramDeploy; amount = Z.zero;
    ou = Z.of_int 1_000_000; nonce = 2; encrypted_data = Some "not-a-package"} in
  let sign tx = Transaction.sign_with_privkey tx (Base64.encode_exn (String.make 32 '\001')) in
  expect "Program package is not isolated" (Option.is_some (View.payload_parse_bytes item));
  let before = Memory.stats Memory.shared in
  let ticket = Memory.reserve Memory.shared ~speculative:true
    (Memory.limit_of_getenv Sys.getenv_opt / 4) |> Option.get in
  Fun.protect ~finally:(fun () -> Memory.release ticket) (fun () ->
    List.iter (fun (balance, nonce, fee, expected) ->
      Test_workspace.with_dir "program_precheck" (fun dir ->
        let store = Lwt_main.run (Store_irmin.open_store (Filename.concat dir "store")) in
        Fun.protect ~finally:(fun () -> Lwt_main.run (Store_irmin.close store)) (fun () ->
          let ledger = Ledger.create store in
          Option.iter (fun balance ->
            Ledger.add_account ledger base.from balance |> Result.get_ok;
            List.iter (fun nonce -> Ledger.debit ledger base.from Z.zero nonce |> Result.get_ok)
              (List.init nonce (fun index -> index + 1))) balance;
          let tx = sign {item with ou = fee} in
          let result = Lwt_main.run (R.validate_and_submit_tx (runtime ()) ledger tx) in
          expect "Program precheck reached package parser"
            (match result with Error (code, _) -> code = expected | Ok _ -> false);
          expect "Program precheck staged transaction" (Staging.find_by_hash (Transaction.hash tx) = None))))
      [None, 0, item.ou, "sender_not_found";
       Some Z.zero, 0, item.ou, "insufficient_balance";
       Some (Z.of_int 10_000_000), 2, item.ou, "invalid_nonce";
       Some (Z.of_int 10_000_000), 0, Z.one, "malformed_transaction";
       Some (Z.of_int 10_000_000), 0, item.ou, "pre_verify_busy"]);
  expect "Program precheck leaked reservation" (Memory.stats Memory.shared = before);
  let module Package = Octra_vm.Program_package in
  let compiled = Package.compile ~main:"entry.aml"
    ~sources:[{path = "entry.aml"; body = "program Entry { view fn read(): int { return 7 } }"}]
    |> Result.map_error Package.error_message |> Result.get_ok in
  let body = Base64.encode_exn compiled.package in
  let encoded = {item with Transaction.encrypted_data = Some body} in
  expect "Program wire shape refused by parse queue"
    (Lwt_main.run (View.payload_check ~limits:View.payload_limits encoded) = Ok ());
  expect "Program success leaked reservation" (Memory.stats Memory.shared = before))

let test_default () =
  test_network_numbers ();
  test_program_precheck ();
  test_parse_wait ();
  test_relay_size ();
  test_compute_paths ();
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
  print_endline "status = pass test = rest_accept"

let () =
  match Array.to_list Sys.argv with
  | [_] -> test_default ()
  | [_; "--devnet-profile"] ->
    with_env "OCTRA_STEALTH_MIN_OU" "5000" test_devnet_profile;
    print_endline "status = pass test = rest_devnet"
  | _ -> fail "unknown test mode"