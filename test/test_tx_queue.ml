(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Octra_core.Transaction
module Pool = Octra_core.Tx_staging
module Ledger = Octra_core.Ledger
module Store = Octra_core.Store_irmin
module Rest = Octra_node_runtime.Node_rest_facade
module Proposal = Octra_node_runtime.Consensus_proposal
module Wiring = Octra_node_runtime.Consensus_driver_wiring

let expect label value = if not value then failwith label
let chain_id = "octra-devnet-9871-cluster"
let envelope_epoch =
  (Option.get (Octra_core.Rule_graph.tx_envelope_activation_for_chain chain_id)).activation_epoch

let signed ?(ou = Z.of_int 10_000) ?(duty = false) () =
  let secret, public = Mirage_crypto_ec.Ed25519.generate () in
  let secret = Mirage_crypto_ec.Ed25519.priv_to_octets secret |> Base64.encode_exn in
  let public = Mirage_crypto_ec.Ed25519.pub_to_octets public |> Base64.encode_exn in
  let from = Octra_core.Crypto.Address.address_from_pubkey public in
  let tx = Tx.{from; to_ = "oct5TWVJk7LZmzEeU73KAwd8HRuQjt2sdBiagm3rxcWDzYH";
    amount = (if duty then Z.zero else Z.one); nonce = 1; ou;
    timestamp = Unix.gettimeofday (); signature = ""; public_key = Some public;
    message = (if duty then Some {|{"consensus_pubkey":"key","head_epoch":"10","state_root":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}|} else None);
    op_type = (if duty then ValidatorReady else Standard); encrypted_data = None} in
  Tx.sign_with_privkey tx secret

let alias tx =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let bytes = Bytes.of_string tx.Tx.signature in
  let code = String.index alphabet tx.signature.[85] in
  Bytes.set bytes 85 alphabet.[code + 1];
  {tx with signature = Bytes.to_string bytes}

let head epoch = Octra_core.Head_manifest.{schema_version; generation = epoch;
  epoch_id = epoch; state_root = String.make 64 'a'; ledger_state_root = None;
  irmin_commit = None; txid_hi = -1L; txlog_seg = None; txlog_off = None;
  epochlog_off = None; commit_id = "queue-test"; ts = 89.; quorum_cert_hash = None;
  epoch_index_hash = None; epoch_index_root = None}

let test_intake () = List.iter (fun duty ->
  Test_workspace.with_dir "tx_queue" (fun dir ->
    let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (Store.close store)) (fun () ->
      let tx = signed ~duty () in
      let ledger = Ledger.create store in
      Ledger.add_account_with_pubkey ledger tx.from (Z.of_int 1_000_000)
        (Option.get tx.public_key) |> Result.get_ok;
      let seen = ref [] in
      let runtime = Rest.{swarm_ref = ref None;
        duty_head = (fun () -> Some (10L, Octra_core.Rule_graph.Prior));
        preverify_admit = (fun tx -> seen := tx :: !seen; Ok ());
        save_drops = ignore; find_drop = (fun _ -> None); drops_by_addr = (fun _ ~limit:_ ~offset:_ -> [])} in
      let submit = Rest.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
      let prepared = Rest.prepare_tx ledger (alias tx) |> Result.get_ok in
      expect "own signed identity remains valid" (prepared = tx && Tx.verify prepared (Option.get tx.public_key));
      Pool.clear ();
      let result = submit (alias tx) in
      expect ("direct intake identity: " ^ (match result with Ok hash -> hash | Error reason -> reason))
        (result = Ok (Tx.hash tx));
      expect "direct intake normalized before preverify" (!seen = [tx]);
      expect "direct intake stored identity" (Pool.find_by_hash (Tx.hash tx) = Some tx);
      if duty then begin
        expect "own duty retry keeps identity" (submit prepared = Ok (Tx.hash prepared));
        expect "duty retry does not repeat preverify" (!seen = [tx])
      end;
      Pool.clear ();
      seen := [];
      expect "invalid direct intake refused" (Result.is_error (submit {tx with signature = "bad"}));
      expect "invalid direct intake has no effects" (!seen = [] && Pool.staging_size () = 0)))) [false; true]

let test_selection () =
  let bad = alias (signed ~ou:(Z.of_int 20_000) ()) in
  let successor = {bad with Tx.nonce = 2; signature = (signed ()).signature} in
  let other = signed () in
  let current = ref (envelope_epoch - 2) in
  let adapters = Wiring.node_standard_adapters Wiring.{chain_id;
    getenv = (fun _ -> None); get_meta = (fun _ -> None);
    duty_state = (fun _ -> Ok Octra_core.Set_fold.empty);
    wallet_addr = other.from; wallet_pub = Option.get other.public_key;
    find_account = (fun _ -> Some {Ledger.empty_account with balance = Z.of_int 1_000_000});
    cached_head = (fun () -> Some (head !current));
    read_prev_ledger_root = (fun () -> Lwt.return_none); next_txid = (fun () -> 0L);
    proposal_state = Octra_node_runtime.Consensus_proposal_state.create ();
    catchup_active = ref false; staging_epoch_capacity = Tx.ou_cost other;
    write_pending = ignore; validator_pubkeys_for_epoch = (fun ~wallet_addr:_ ~wallet_pub:_ ~epoch:_ -> [])} in
  Pool.clear ();
  Fun.protect ~finally:Pool.clear (fun () ->
    List.iter (fun tx -> Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 1_000_000, 0)) tx
      |> Result.get_ok |> ignore) [bad; successor; other];
    List.iter (fun epoch ->
      current := epoch - 1;
      let selected = adapters.staging_epoch_txs () in
      if epoch >= envelope_epoch then
        expect "invalid envelope cannot consume capacity or release successor" (selected = [other])
      else expect "prior selection preserves old fee order" (selected = [bad]);
      expect "selection preserves staging evidence" (Pool.staging_size () = 3))
      [envelope_epoch - 1; envelope_epoch; envelope_epoch + 1])

let test_delivery () =
  let module Delivery = Octra_node_runtime.Set_delivery in
  let module Post = Octra_node_runtime.Set_post in
  let module Gossip = Octra_net.P2p_tx_gossip in
  List.iter (fun (form, failure) -> Test_workspace.with_dir "tx_delivery" (fun dir ->
    let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (Store.close store)) (fun () ->
      Pool.clear ();
      let tx = signed ~duty:true () in
      let public = Option.get tx.public_key in
      let ledger = Ledger.create store in
      Ledger.add_account_with_pubkey ledger tx.from (Z.of_int 1_000_000) public
        |> Result.get_ok;
      let checked = ref [] in
      let runtime = Rest.{swarm_ref = ref None;
        duty_head = (fun () -> Some (10L, Octra_core.Rule_graph.Prior));
        preverify_admit = (fun value ->
          checked := value :: !checked;
          if Tx.verify value public then Ok () else Error "invalid duty signature");
        save_drops = ignore; find_drop = (fun _ -> None);
        drops_by_addr = (fun _ ~limit:_ ~offset:_ -> [])} in
      let frames, sent, warnings = ref [], ref [], ref [] in
      let network = Delivery.{
        broadcast = (fun frame -> frames := frame :: !frames);
        post = (fun value ->
          sent := value :: !sent;
          if List.length !sent = 1 then Lwt.return_error failure
          else Lwt.return_ok ());
      } in
      let clock, consumed = ref 0., ref false in
      let post = Post.create Post.{
        now = (fun () -> !clock); wait = (fun _ -> fst (Lwt.task ()));
        staged = (fun hash -> Pool.find_by_hash hash <> None);
        landed = (fun _ -> !consumed);
        post = (fun _ -> failwith "duty delivery policy missing");
        warn = (fun reason -> warnings := reason :: !warnings);
      } in
      Fun.protect ~finally:(fun () -> Post.stop post) (fun () ->
        let stage = Delivery.stage ~bft_mode:true runtime ledger in
        expect "malformed duty has no retained item" (Result.is_error (stage {tx with signature = "bad"}));
        expect "malformed duty has no effects"
          (!checked = [] && Pool.staging_size () = 0 && not (Post.pending post));
        let input = form tx in
        expect "signed input remains valid" (Tx.verify input public);
        let staged = match stage input with Ok value -> value | Error reason -> failwith reason in
        expect "queued duty has one identity" (Pool.find_by_hash (Tx.hash tx) = Some tx);
        let retry = Post.{retain = true; eligible = (fun _ -> Eligible);
          post = (fun ~current value ->
            if current () then Delivery.retry ~bft_mode:true runtime ledger network value
            else Lwt.return_error (Wait "delivery replaced"));
        } in
        Delivery.put ~retry post staged;
        Lwt_main.run (Lwt.pause ());
        expect "first send retains prepared duty"
          (!sent = [tx] && !checked = [tx] && Post.pending post);
        Pool.clear ();
        clock := 1.;
        Post.tick post;
        Lwt_main.run (Lwt.pause ());
        expect "retry restores identical signed duty"
          (!sent = [tx; tx] && !checked = [tx; tx]
           && Pool.find_by_hash (Tx.hash tx) = Some tx && Post.pending post);
        expect "retry preserves transport warning" (!warnings = ["lost reply"]);
        expect "each send broadcasts once" (List.length !frames = 2);
        List.iter (fun frame ->
          expect "duty gossip frame" (frame.Octra_net.P2p_frame.msg_type = Octra_net.P2p_frame.msg_tx_gossip);
          match Gossip.decode frame.payload with
          | Gossip.Tx {hash; tx_json} ->
            expect "gossip and RPC use retained bytes"
              (hash = Tx.hash tx && tx_json = Yojson.Safe.to_string (Tx.to_yojson tx))
          | _ -> failwith "duty gossip decode") !frames;
        consumed := true;
        Post.tick post;
        expect "landed duty retires without resend"
          (not (Post.pending post) && !sent = [tx; tx]);
        let sent_count, frame_count = List.length !sent, List.length !frames in
        let refused = Lwt_main.run (Delivery.retry ~bft_mode:true runtime ledger network (alias tx)) in
        expect "changed retry identity is refused"
          (refused = Error (Post.Refused "validator duty staging hash mismatch"));
        expect "changed retry has no network effects"
          (List.length !sent = sent_count && List.length !frames = frame_count)))))
    (List.concat_map (fun form ->
      [form, Post.Retry "lost reply"; form, Post.Refused "lost reply"])
      [(fun tx -> tx); alias; (fun tx -> {tx with Tx.public_key = None})])

let test_proposal () =
  List.iter (fun epoch ->
    let good, bad = signed (), alias (signed ()) in
    let seen = ref [] in
    let deps = Proposal.{current = (fun () -> true); start_height = (fun _ -> Lwt.return_unit);
      current_epoch = (fun () -> epoch); state_attested = (fun () -> true);
      quarantine_active = (fun () -> false); quarantine_reason = (fun () -> "");
      read_prev_ledger_root = (fun () -> Lwt.return_some (String.make 32 'a'));
      cached_head = (fun () -> Some (head (epoch - 1))); current_round = (fun () -> 0);
      parent_commit = (fun ~epoch_id:_ -> Ok None); frozen_bundle = (fun _ -> None);
      store_bundle = (fun ~proposal_id:_ ~tx_hashes:_ ~txs:_ ~receipts_json ->
        expect "invalid envelope must not become a rejection receipt" (receipts_json = []));
      staging_txs = (fun () -> [bad; good]); admits_tx = (fun _ -> true);
      build_preverify_once = (fun ~state_root:_ ~tx_hashes:_ inputs ->
        Lwt.return Octra_core.Preverify_worker.{ready = List.map (fun tx -> {tx; receipt = None}) inputs; skipped = []});
      staging_total = (fun () -> 2); proposer = (fun () -> good.from);
      validator_pubkeys = (fun _ -> []);
      preview = (fun request ->
        seen := request.txs;
        Lwt.return (Result.map (fun () -> Octra_core.Epoch_exec.{
          post_state_root = String.make 32 'b'; artifacts = {
            confirmed = List.map (fun tx -> tx, 0) request.txs; rejected = [];
            confirmed_fees = Z.zero; tx_count = List.length request.txs}})
          (Octra_core.Tx_envelope.check_epoch ~chain_id ~epoch:request.epoch_id request.txs)));
      prev_eic_root = (fun () -> Octra_core.Epoch_index_commitment.genesis_root);
      next_txid = (fun () -> 0L); set_proposal = (fun _ _ -> ());
      head_txid_hi = (fun () -> Some (-1L)); freeze = (fun _ _ -> ());
      now = (fun () -> 99.); previous_epoch_ts = (fun _ -> Some 89.)} in
    let result = Lwt_main.run (Proposal.make_proposal deps ~chain_id
      ~root_to_raw32:(fun value -> value) ~limits:{max_txs = 10; max_bytes = 1_000_000;
        max_ou = Z.of_int 1_000_000} ~epoch_id:(Int64.of_int epoch)) in
    expect "one bad queue entry stalls a proposal" (Option.is_some result);
    expect "proposal filters before preview" (if epoch < envelope_epoch then List.length !seen = 2 else !seen = [good]))
    [envelope_epoch - 1; envelope_epoch; envelope_epoch + 1]

let () =
  Mirage_crypto_rng_unix.use_default ();
  let failed = List.filter_map (fun (name, run) ->
    try run (); Printf.printf "event = test name = %s status = passed\n%!" name; None
    with exn -> Printf.eprintf "event = test name = %s status = failed error = %s\n%!"
      name (Printexc.to_string exn); Some name)
    ["intake", test_intake; "selection", test_selection; "proposal", test_proposal;
     "delivery", test_delivery] in
  if failed <> [] then exit 1