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

let signed ?secret ?(nonce = 1) ?(ou = Z.of_int 10_000) ?(amount = Z.one) ?(duty = false) ?(op = Tx.Standard)
    ?(timestamp = Unix.gettimeofday ()) ?message ?encrypted_data () =
  let secret, public = match secret with
    | None -> Mirage_crypto_ec.Ed25519.generate ()
    | Some bytes ->
      let key = Mirage_crypto_ec.Ed25519.priv_of_octets bytes |> Result.get_ok in
      key, Mirage_crypto_ec.Ed25519.pub_of_priv key in
  let secret = Mirage_crypto_ec.Ed25519.priv_to_octets secret |> Base64.encode_exn in
  let public = Mirage_crypto_ec.Ed25519.pub_to_octets public |> Base64.encode_exn in
  let from = Octra_core.Crypto.Address.address_from_pubkey public in
  let tx = Tx.{from; to_ = "oct5TWVJk7LZmzEeU73KAwd8HRuQjt2sdBiagm3rxcWDzYH";
    amount = (if duty then Z.zero else amount); nonce; ou;
    timestamp; signature = ""; public_key = Some public;
    message = (if duty then Some {|{"consensus_pubkey":"key","head_epoch":"10","state_root":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}|} else message);
    op_type = (if duty then ValidatorReady else op); encrypted_data} in
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
        proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
        queue_head = (fun () -> None);
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
    save_drops = ignore;
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

let test_circle_intake () =
  let env = ["OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1"; "OCTRA_CONSENSUS_MODE", "bft"] in
  let prior = List.map (fun (name, _) -> name, Sys.getenv_opt name) env in
  List.iter (fun (name, value) -> Unix.putenv name value) env;
  Fun.protect ~finally:(fun () -> List.iter (fun (name, value) ->
    Unix.putenv name (Option.value ~default:"" value)) prior) (fun () ->
  Test_workspace.with_dir "circle_queue" (fun dir ->
    let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (Store.close store)) (fun () ->
      let module Lane = Octra_core.Resource_lanes in
      let budget = Lane.default_budget Lane.Circle_compute in
      let secret = String.make 32 '\001' in
      let call = signed ~message:"[]" ~encrypted_data:"run" ~op:Tx.CircleCall in
      let allowed = call ~secret ~ou:budget.max_ou () in
      let refused = call ~ou:(Z.succ budget.max_ou) () in
      let ledger = Ledger.create store in
      List.iter (fun tx ->
        Ledger.add_account_with_pubkey ledger tx.Tx.from (Z.of_int 100_000_000)
          (Option.get tx.public_key) |> Result.get_ok) [allowed; refused];
      let seen = ref [] in
      let drops = ref [] in
      let runtime = Rest.{swarm_ref = ref None; duty_head = (fun () -> None);
        proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
        queue_head = (fun () -> None);
        preverify_admit = (fun tx -> seen := tx :: !seen; Ok ());
        save_drops = (fun rows -> drops := rows @ !drops);
        find_drop = (fun _ -> None); drops_by_addr = (fun _ ~limit:_ ~offset:_ -> [])} in
      let submit = Rest.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
      Pool.clear ();
      let accepted = submit allowed in
      expect ("circle exact budget accepted: " ^
        (match accepted with Ok hash -> hash | Error reason -> reason))
        (accepted = Ok (Tx.hash allowed));
      expect "circle positive control entered preverify" (!seen = [allowed]);
      List.iter (fun encoding ->
        let fields = match Tx.to_yojson refused with `Assoc fields -> fields | _ -> assert false in
        let encoded = `Assoc (List.map (fun (key, value) ->
          key, if key = "ou" then encoding else value) fields) in
        let decoded = Tx.of_yojson encoded |> Result.get_ok in
        expect "circle OU encoding preserves signature"
          (decoded = refused && Tx.verify decoded (Option.get decoded.public_key));
        expect "circle over budget refused before preverify" (Result.is_error (submit decoded)))
        [`Int (Z.to_int refused.ou); `String (Z.to_string refused.ou);
         `String ("0x" ^ Z.format "%x" refused.ou)];
      expect "circle refusal preserves queue" (Pool.all () = [allowed] && !seen = [allowed] && !drops = []);
      let replacement = call ~secret ~ou:(Z.mul budget.max_ou (Z.of_int 2)) () in
      expect "circle replacement is signed" (Tx.verify replacement (Option.get allowed.public_key));
      expect "circle invalid replacement refused" (Result.is_error (submit replacement));
      expect "circle invalid replacement keeps original"
        (Pool.find_by_hash (Tx.hash allowed) = Some allowed && !drops = [] && !seen = [allowed]);
      Pool.clear ();
      let first = call ~secret ~ou:(Z.of_int 10_000) () in
      let next = call ~secret ~ou:(Z.of_int 11_000) () in
      expect "circle small call accepted" (submit first = Ok (Tx.hash first));
      expect "circle fee replacement accepted" (submit next = Ok (Tx.hash next));
      expect "circle fee replacement replaces only original"
        (Pool.all () = [next] && List.map (fun row -> row.Pool.d_hash) !drops = [Tx.hash first]);
      Pool.clear ();
      let huge = call ~secret ~ou:(Z.shift_left Z.one 100) () in
      expect "circle large OU is signed" (Tx.verify huge (Option.get huge.public_key));
      expect "circle large OU refused" (submit huge = Error "circle resource limit: ou");
      let empty = signed ~secret ~op:Tx.CircleCall ~encrypted_data:"run" ~message:"[\"\\n\"]" () in
      let count = budget.max_bytes - Lane.tx_bytes empty in
      let sized extra =
        {empty with Tx.message = Some ("[\"\\n" ^ String.make (count + extra) 'a' ^ "\"]")}
        |> fun tx -> Tx.sign_with_privkey tx (Base64.encode_exn secret) in
      let exact = sized 0 in
      let carried = {exact with Tx.public_key = Some (String.make 44 '"')} in
      expect "circle escaped signed size" (Lane.tx_bytes exact = budget.max_bytes);
      expect "circle carried key inflates wire size" (Lane.tx_bytes carried = budget.max_bytes + 44);
      expect "circle carried key resolves before admission" (Rest.prepare_tx ledger carried = Ok exact);
      expect "circle normalized RPC control accepted"
        (Rest.validate_and_submit_tx runtime ledger exact = Ok (Tx.hash exact));
      Pool.clear ();
      expect "circle RPC uses normalized byte size"
        (Rest.validate_and_submit_tx runtime ledger carried = Ok (Tx.hash exact));
      Pool.clear ();
      expect "circle staging uses normalized byte size" (submit carried = Ok (Tx.hash exact));
      let larger = sized 1 in
      expect "circle signed byte excess refused" (submit larger = Error "circle resource limit: bytes");
      expect "circle signed byte excess preserves accepted call" (Pool.all () = [exact]);
      Pool.clear ();
      expect "circle non-bft policy unchanged"
        (Rest.add_tx_to_staging ~relay:false runtime ledger refused = Ok (Tx.hash refused));
      expect "circle admission does not charge"
        ((Ledger.find ledger allowed.from).nonce = 0
         && Z.equal (Ledger.find ledger allowed.from).balance (Z.of_int 100_000_000)))))

let test_program_intake () =
  let module Lane = Octra_core.Resource_lanes in
  let budget = Lane.default_budget Lane.Program in
  let setting = "OCTRA_BFT_RELEASE_PROFILE" in
  let prior = Sys.getenv_opt setting in
  Fun.protect ~finally:(fun () -> Unix.putenv setting (Option.value ~default:"" prior)) (fun () ->
  Unix.putenv setting "devnet_full_v1";
  List.iter (fun op ->
    Test_workspace.with_dir "program_queue" (fun dir ->
      let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
      Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (Store.close store)) (fun () ->
        Pool.clear ();
        let call = signed ~secret:(String.make 32 '\003') ~op ~amount:Z.zero
          ~message:"[]" ~encrypted_data:"run" in
        let allowed = call ~ou:budget.max_ou () in
        let ledger = Ledger.create store in
        Ledger.add_account_with_pubkey ledger allowed.from (Z.of_int 100_000_000)
          (Option.get allowed.public_key) |> Result.get_ok;
        let seen = ref [] in
        let mode = ref Octra_core.Rule_graph.Active in
        let runtime = Rest.{swarm_ref = ref None; duty_head = (fun () -> None);
          proof_mode = (fun () -> !mode);
          queue_head = (fun () -> None);
          preverify_admit = (fun tx -> seen := tx :: !seen; Ok ());
          save_drops = (fun rows -> expect "unexpected queue drop" (rows = []));
          find_drop = (fun _ -> None); drops_by_addr = (fun _ ~limit:_ ~offset:_ -> [])} in
        let submit = Rest.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
        List.iter (fun ou ->
          let refused = call ~ou () in
          let result = submit refused in
          expect ("program effort exceeds queue capacity: " ^
            (match result with Ok _ -> "accepted" | Error reason -> reason))
            (result = Error "program resource limit: ou");
          expect "program refusal changed pending nonce" (Pool.staging_size () = 0 && !seen = []))
          [Z.succ budget.max_ou; Z.shift_left Z.one 100];
        expect "program exact allowance refused" (submit allowed = Ok (Tx.hash allowed));
        expect "program exact allowance skipped preverify" (!seen = [allowed]);
        let replacement = call ~ou:(Z.mul budget.max_ou (Z.of_int 2)) () in
        expect "oversized replacement changed queue" (Result.is_error (submit replacement)
          && Pool.all () = [allowed] && !seen = [allowed]);
        expect "program intake changed funds"
          ((Ledger.find ledger allowed.from).nonce = 0
           && Z.equal (Ledger.find ledger allowed.from).balance (Z.of_int 100_000_000));
        Pool.clear ();
        let refused = call ~ou:(Z.succ budget.max_ou) () in
        mode := Octra_core.Rule_graph.Prior;
        expect "prior program intake changed" (submit refused = Ok (Tx.hash refused));
        Pool.clear ();
        mode := Octra_core.Rule_graph.Active;
        expect "program non-bft policy changed"
          (Rest.add_tx_to_staging ~relay:false runtime ledger refused = Ok (Tx.hash refused)))))
    [Tx.ProgramExec; Tx.MultiExec])

let test_deploy_intake () =
  let module Lane = Octra_core.Resource_lanes in
  let module G = Octra_core.Rule_graph in
  let setting = "OCTRA_BFT_RELEASE_PROFILE" in
  let prior = Sys.getenv_opt setting in
  Fun.protect ~finally:(fun () -> Unix.putenv setting (Option.value ~default:"" prior)) (fun () ->
  Unix.putenv setting "devnet_full_v1";
  Test_workspace.with_dir "deploy_queue" (fun dir ->
    let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (Store.close store)) (fun () ->
      let ledger = Ledger.create store in
      let mode = ref G.Prior in
      let runtime = Rest.{swarm_ref = ref None; duty_head = (fun () -> None);
        proof_mode = (fun () -> !mode); preverify_admit = (fun _ -> Ok ());
        queue_head = (fun () -> None);
        save_drops = (fun rows -> expect "deploy replaced queue data" (rows = []));
        find_drop = (fun _ -> None); drops_by_addr = (fun _ ~limit:_ ~offset:_ -> [])} in
      let submit = Rest.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
      List.iter (fun op ->
        let secret = Mirage_crypto_ec.Ed25519.generate () |> fst
          |> Mirage_crypto_ec.Ed25519.priv_to_octets in
        let make = signed ~secret ~op ~amount:Z.zero ~ou:(Z.of_int 1_000_000)
          ~timestamp:(Unix.gettimeofday ()) in
        let base = make ~message:"" () in
        let budget = Lane.default_budget (Lane.of_op op) in
        let size = budget.max_bytes - Lane.tx_bytes base in
        let exact = make ~message:(String.make size 'a') () in
        let extra = make ~message:(String.make (size + 1) 'a') () in
        Ledger.add_account_with_pubkey ledger base.from (Z.of_int 100_000_000)
          (Option.get base.public_key) |> Result.get_ok;
        expect "deploy byte size differs" (Lane.tx_bytes exact = budget.max_bytes);
        expect "deploy byte excess differs" (Lane.tx_bytes extra = budget.max_bytes + 1);
        List.iter (fun (chain, epoch, active) ->
          Pool.clear ();
          mode := G.proof_exec_at ~chain_id:chain ~epoch;
          let result = submit extra in
          expect ("deploy intake activation differs: " ^
            (match result with Ok _ -> "accepted" | Error reason -> reason))
            (if active then result = Error (Lane.to_string (Lane.of_op op) ^ " resource limit: bytes")
             else result = Ok (Tx.hash extra));
          Pool.clear ();
          expect "deploy exact size was refused" (submit exact = Ok (Tx.hash exact));
          if active then begin
            expect "oversized deploy replacement accepted" (Result.is_error (submit extra));
            expect "oversized deploy replacement lost original" (Pool.all () = [exact])
          end)
          [chain_id, 1_662_999, false; chain_id, 1_663_000, true;
           chain_id, 1_663_001, true; "other", 1_663_000, false])
        [Tx.ProgramDeploy; Tx.CircleDeploy; Tx.CircleProgramUpdate])))

let test_circle_bytes () =
  let module Lane = Octra_core.Resource_lanes in
  let budget = Lane.default_budget Lane.Circle_compute in
  let empty = signed ~op:Tx.CircleCall ~message:"" () in
  let size = budget.max_bytes - Lane.tx_bytes empty in
  let exact = {empty with Tx.message = Some (String.make size 'a')} in
  let larger = {empty with Tx.message = Some (String.make (size + 1) 'a')} in
  expect "circle byte count matches gate" (Lane.tx_bytes exact = budget.max_bytes);
  expect "circle byte limit inclusive" (Lane.circle_admission exact = Ok ());
  expect "circle byte excess refused"
    (Lane.circle_admission larger = Error "circle resource limit: bytes");
  expect "circle policy leaves other lanes unchanged"
    (Lane.circle_admission {larger with Tx.op_type = ProgramExec; ou = Z.succ budget.max_ou} = Ok ())

let test_circle_selection () =
  let module Lane = Octra_core.Resource_lanes in
  let budget = Lane.default_budget Lane.Circle_compute in
  let secret = String.make 32 '\002' in
  let bad = signed ~secret ~op:Tx.CircleCall ~ou:(Z.succ budget.max_ou) () in
  let successor = signed ~secret ~nonce:2 () in
  let other = signed () in
  let current = ref (envelope_epoch - 2) in
  let adapters = Wiring.node_standard_adapters Wiring.{chain_id;
    getenv = (fun _ -> None); get_meta = (fun _ -> None);
    duty_state = (fun _ -> Ok Octra_core.Set_fold.empty);
    wallet_addr = other.from; wallet_pub = Option.get other.public_key;
    find_account = (fun _ -> Some {Ledger.empty_account with balance = Z.of_int 100_000_000});
    cached_head = (fun () -> Some (head !current));
    read_prev_ledger_root = (fun () -> Lwt.return_none); next_txid = (fun () -> 0L);
    proposal_state = Octra_node_runtime.Consensus_proposal_state.create ();
    catchup_active = ref false; staging_epoch_capacity = Z.mul (Z.of_int 2) (Tx.ou_cost other);
    save_drops = ignore;
    write_pending = ignore; validator_pubkeys_for_epoch = (fun ~wallet_addr:_ ~wallet_pub:_ ~epoch:_ -> [])} in
  Pool.clear ();
  Fun.protect ~finally:Pool.clear (fun () ->
    List.iter (fun tx -> Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 100_000_000, 0)) tx
      |> Result.get_ok |> ignore) [bad; successor; other];
    List.iter (fun epoch ->
      current := epoch - 1;
      expect "circle over budget cannot consume capacity or release successor"
        (adapters.staging_epoch_txs () = [other]);
      expect "circle selection preserves queue evidence" (Pool.staging_size () = 3))
      [envelope_epoch - 1; envelope_epoch; envelope_epoch + 1];
    Pool.clear ();
    let first = signed ~secret ~op:Tx.CircleCall ~ou:(Z.of_int 100_000) () in
    let successor = signed ~secret ~nonce:2 ~ou:(Z.of_int 50_000) () in
    List.iter (fun tx ->
      expect "circle nonce controls signed" (Tx.verify tx (Option.get tx.Tx.public_key));
      Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 100_000_000, 0)) tx
      |> Result.get_ok |> ignore) [first; successor];
    expect "circle nonce order preserved" (adapters.staging_epoch_txs () = [first; successor]);
    Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 100_000_000, 0)) other
    |> Result.get_ok |> ignore;
    expect "priced circle did not consume queue capacity"
      (adapters.staging_epoch_txs () = [first; successor]);
    expect "circle turn did not free queue capacity or released dependent nonce"
      (adapters.staging_epoch_txs ~circles:false () = [other]);
    expect "circle turn removed deferred transactions" (Pool.staging_size () = 3);
    Pool.clear ();
    let program = signed ~secret ~op:Tx.ProgramExec
      ~ou:(Z.succ (Lane.default_budget Lane.Program).max_ou) () in
    List.iter (fun tx -> Pool.add_smart
      ~lookup:(fun _ -> Some (Z.of_int 100_000_000, 0)) tx
      |> Result.get_ok |> ignore) [program; successor; other];
    List.iter (fun epoch ->
      current := epoch - 1;
      let selected = adapters.staging_epoch_txs () in
      expect "program selection ignored activation"
        (if epoch < 1_663_000 then List.mem program selected else selected = [other]);
      expect "program selection removed pending work" (Pool.staging_size () = 3))
      [1_662_999; 1_663_000; 1_663_001])

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
        proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
        queue_head = (fun () -> None);
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
      parent_txs = (fun _ -> None);
      store_bundle = (fun ~proposal_id:_ ~tx_hashes:_ ~txs:_ ~receipts_json ->
        expect "invalid envelope must not become a rejection receipt" (receipts_json = []));
      staging_txs = (fun ?(circles = true) () ->
        if circles then [bad; good] else Octra_node_runtime.Circle_refill.without [bad; good]);
      admits_tx = (fun _ -> true);
      evict_preview = (fun ?epoch:_ _ -> failwith "unexpected preview eviction");
      hold_preview = (fun ~epoch:_ _ -> failwith "unexpected preview hold");
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

let test_hold_state () =
  let module Hold = Pool.Preview in
  let first = String.make 64 'a' in
  let second = String.make 64 'b' in
  let third = String.make 64 'c' in
  let step = Hold.step ~limit:2 in
  let state, result = step Hold.empty (Refuse {head = 10L; epoch = 11L; hash = first}) in
  expect "preview refusal not recorded" (result = Ok ());
  let check state head hash = snd (step state (Check (head, hash))) in
  let ready state head hash = snd (step state (Ready (head, hash))) in
  List.iter (fun head -> expect "refused hash accepted before commit"
    (check state head first = Error "preview refused in this epoch"))
    [None; Some 9L; Some 10L];
  expect "replacement hash refused" (check state None second = Ok ());
  let state, result = step state (Refuse {head = 9L; epoch = 10L; hash = second}) in
  expect "old refusal accepted" (Result.is_error result);
  expect "old refusal changed remembered hash"
    (Result.is_error (check state None first) && check state None second = Ok ());
  let state, result = step state (Refuse {head = 10L; epoch = 12L; hash = second}) in
  expect "future refusal accepted" (Result.is_error result);
  let state, _ = step state (Refuse {head = 10L; epoch = 11L; hash = second}) in
  let state, _ = step state (Refuse {head = 10L; epoch = 11L; hash = first}) in
  expect "duplicate refusal consumed capacity" (check state None third = Ok ());
  let state, _ = step state (Refuse {head = 10L; epoch = 11L; hash = third}) in
  expect "full refusal set forgot a hash" (Result.is_error (check state None first));
  expect "full refusal set admitted new work"
    (check state None third = Error "staging full until epoch commit");
  expect "full refusal set suppressed queued work" (ready state None third = Ok ());
  expect "queued refusal lost its identity" (Result.is_error (ready state None first));
  let unchanged, result = step state (Refuse {head = 20L; epoch = 20L; hash = third}) in
  expect "invalid refusal advanced head"
    (Result.is_error result && Result.is_error (ready unchanged None first));
  List.iter (fun head ->
    let state, result = step state (Check (Some head, first)) in
    expect "commit retained refusal" (result = Ok ());
    let state, result = step state (Refuse {head = 10L; epoch = 11L; hash = first}) in
    expect "late response restored refusal"
      (Result.is_error result && check state None first = Ok ())) [11L; 20L]

let test_hold_intake () =
  List.iteri (fun index duty ->
    Test_workspace.with_dir "preview_queue" (fun dir ->
      let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
      Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (Store.close store)) (fun () ->
        let current = ref (1_700_000 + (2 * index)) in
        let secret = String.make 32 (Char.chr (index + 1)) in
        let tx = signed ~secret ~duty () in
        let ledger = Ledger.create store in
        Ledger.add_account_with_pubkey ledger tx.from (Z.of_int 1_000_000)
          (Option.get tx.public_key) |> Result.get_ok;
        let seen = ref [] in
        let rows = ref [] in
        let point () = Some (Int64.of_int !current) in
        let runtime = Rest.{swarm_ref = ref None; queue_head = point;
          proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
          duty_head = (fun () -> Some (10L, Octra_core.Rule_graph.Prior));
          preverify_admit = (fun tx -> seen := tx :: !seen; Ok ());
          save_drops = (fun drops -> rows := drops @ !rows);
          find_drop = (fun _ -> None); drops_by_addr = (fun _ ~limit:_ ~offset:_ -> [])} in
        let adapters = Wiring.node_standard_adapters Wiring.{chain_id;
          getenv = (fun _ -> None); get_meta = (fun _ -> None);
          duty_state = (fun _ -> Ok Octra_core.Set_fold.empty);
          wallet_addr = tx.from; wallet_pub = Option.get tx.public_key;
          find_account = Ledger.find_opt ledger;
          cached_head = (fun () -> Some (head !current));
          read_prev_ledger_root = (fun () -> Lwt.return_none); next_txid = (fun () -> 0L);
          proposal_state = Octra_node_runtime.Consensus_proposal_state.create ();
          catchup_active = ref false; staging_epoch_capacity = Tx.ou_cost tx;
          save_drops = runtime.save_drops; write_pending = ignore;
          validator_pubkeys_for_epoch = (fun ~wallet_addr:_ ~wallet_pub:_ ~epoch:_ -> [])} in
        let submit = Rest.add_tx_to_staging ~relay:false ~bft_mode:true runtime ledger in
        Pool.clear ();
        expect "initial queue intake refused" (submit (alias tx) = Ok (Tx.hash tx));
        if not duty then begin
          let queued = adapters.staging_epoch_txs () in
          expect "hold input not selected" (queued = [tx]);
          let epoch = Int64.succ (Option.get (point ())) in
          adapters.hold_preview ~epoch tx;
          List.iter (fun _ ->
            expect "held work selected again" (adapters.staging_epoch_txs () = []);
            expect "held work lost" (Pool.find_by_hash (Tx.hash tx) = Some tx)) [1; 2];
          expect "hold recorded a drop" (!rows = []);
          incr current;
          expect "commit did not release held work" (adapters.staging_epoch_txs () = queued)
        end;
        adapters.evict_preview tx;
        expect "prior preview changed intake" (submit tx = Ok (Tx.hash tx));
        let epoch = Int64.succ (Option.get (point ())) in
        adapters.evict_preview ~epoch tx;
        let refused = Error "preview refused in this epoch" in
        List.iter (fun value -> expect "facade accepted refused hash" (submit value = refused))
          [tx; alias tx; {tx with public_key = None}];
        expect "refused intake reached preverify" (!seen = [tx; tx]);
        expect "refused intake changed queue" (Pool.staging_size () = 0);
        Pool.clear ();
        expect "queue clear forgot refusal" (submit tx = refused);
        expect "direct insertion bypassed refusal"
          (Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 1_000_000, 0)) tx = refused);
        let corrected = signed ~secret ~duty ~timestamp:(tx.timestamp +. 1.) () in
        expect "corrected same nonce refused" (submit corrected = Ok (Tx.hash corrected));
        adapters.evict_preview ~epoch tx;
        expect "absent refusal changed queue" (Pool.find_by_hash (Tx.hash corrected) = Some corrected);
        expect "refusal error misclassified"
          (Octra_node_runtime.Tx_view.staging_error "preview refused in this epoch"
           = ("preview_refused", "retry after epoch commit or replace transaction at same nonce"));
        let validate item = Result.map_error Octra_node_runtime.Tx_view.staging_error (submit item) in
        let response = Octra_node_runtime.Submit_rpc.submit ~validate
          (`List [Tx.to_yojson tx]) |> Lwt_main.run in
        expect "refusal RPC became pending"
          (match response with
           | Error error -> error.Octra_core.Rpc.code = 118
             && error.message = "preview refused"
             && error.data = Some (`String "retry after epoch commit or replace transaction at same nonce")
           | Ok _ -> false);
        ignore (Pool.remove_by_hash (Tx.hash corrected));
        incr current;
        expect "commit did not release hash" (submit tx = Ok (Tx.hash tx));
        let late = try adapters.evict_preview ~epoch tx; false
          with Octra_core.Exec_resource.Unavailable Host -> true in
        expect "late refusal removed current work"
          (late && Pool.find_by_hash (Tx.hash tx) = Some tx)))) [false; true]

let test_hold_entry () =
  let module Preview = Pool.Preview in
  let current = 1_800_000L in
  let epoch = Int64.succ current in
  let secret = String.make 32 '\011' in
  let first = signed ~secret () in
  let second = signed ~secret ~nonce:2 () in
  let other = signed () in
  let lookup _ = Some (Z.of_int 1_000_000, 0) in
  let add tx = Pool.add_smart ~head:current ~lookup tx |> Result.get_ok |> ignore in
  let ready head tx = Preview.send (Ready (head, Tx.hash tx)) in
  let hold head epoch tx = Preview.send (Hold {head; epoch; hash = Tx.hash tx}) in
  let selected head = Pool.ready_epoch_txs
    ~accept:(fun tx -> ready (Some head) tx = Ok ())
    ~capacity:Pool.max_ou ~confirmed_nonce:(fun _ -> Some 0) in
  Pool.clear ();
  Fun.protect ~finally:Pool.clear (fun () ->
    List.iter add [first; second; other];
    let count = Pool.staging_size () in
    let cost = Pool.staging_total_ou () in
    let pending = Pool.all () in
    let lifetime = Pool.queue_state ~confirmed:0 (Tx.hash first) in
    expect "queued hold refused" (hold current epoch first = Ok ());
    expect "duplicate hold refused" (hold current epoch first = Ok ());
    List.iter (fun point -> expect "hold released without head advance"
      (Result.is_error (ready point first))) [None; Some (Int64.pred current); Some current];
    expect "hold admitted the same hash"
      (Preview.send (Check (Some current, Tx.hash first)) = Error "preview refused in this epoch");
    expect "hold changed queue ownership"
      (Pool.staging_size () = count && Z.equal (Pool.staging_total_ou ()) cost
       && Pool.all () = pending && Pool.queue_state ~confirmed:0 (Tx.hash first) = lifetime);
    expect "hold deleted transaction" (Pool.find_by_hash (Tx.hash first) = Some first);
    expect "hold recorded a drop" (Pool.lookup_dropped (Tx.hash first) = None);
    expect "held nonce did not stop successors" (selected current = [other]);
    List.iter (fun (head, epoch) ->
      expect "invalid hold accepted" (Result.is_error (hold head epoch other));
      expect "invalid hold released existing work" (Result.is_error (ready None first));
      expect "invalid hold suppressed other work" (ready None other = Ok ()))
      [Int64.pred current, current; current, Int64.succ epoch;
       Int64.add current 10L, Int64.add current 10L;
       -1L, 0L; Int64.max_int, Int64.min_int];
    let missing = signed () in
    expect "absent hold interrupted proposal"
      (hold current epoch missing = Ok ());
    expect "absent hold released current work" (Result.is_error (ready None first));
    let replacement = signed ~secret ~ou:(Z.of_int 20_000) () in
    add replacement;
    expect "replacement inherited a hold" (ready None replacement = Ok ());
    expect "old hash hold affected replacement"
      (hold current epoch first = Ok ()
       && Pool.find_by_hash (Tx.hash replacement) = Some replacement
       && ready None replacement = Ok ());
    expect "replacement hold refused" (hold current epoch replacement = Ok ());
    expect "head advance retained held work" (ready (Some epoch) replacement = Ok ());
    expect "head advance lost pending successors" (List.length (selected epoch) = count);
    expect "late generation changed replacement"
      (Result.is_error (hold current epoch replacement) && ready None replacement = Ok ());
    expect "next head hold refused" (hold epoch (Int64.succ epoch) replacement = Ok ());
    expect "held removal failed" (Pool.remove_by_hash (Tx.hash replacement));
    expect "removed entry retained a hold" (ready None replacement = Ok ());
    ignore (Pool.add_smart ~head:epoch ~lookup replacement |> Result.get_ok);
    expect "new entry inherited removed hold" (ready None replacement = Ok ());
    expect "reinserted hold refused" (hold epoch (Int64.succ epoch) replacement = Ok ());
    expect "absent next head interrupted proposal"
      (hold (Int64.succ epoch) (Int64.add epoch 2L) missing = Ok ());
    expect "absent next head lost generation"
      (ready None replacement = Ok ()
       && Result.is_error (hold epoch (Int64.succ epoch) replacement));
    Pool.clear ();
    expect "queue clear retained entry marker" (ready None replacement = Ok ()))

let test_hold_capacity ?(send = Pool.Preview.send) () =
  let module Preview = Pool.Preview in
  let current = 1_900_000L in
  let epoch = Int64.succ current in
  let secret = String.make 32 '\012' in
  let first = signed ~secret () in
  let second = signed ~secret ~nonce:2 () in
  let other = signed () in
  let lookup _ = Some (Z.of_int 1_000_000, 0) in
  let ready head tx = send (Preview.Ready (head, Tx.hash tx)) in
  Pool.clear ();
  Fun.protect ~finally:(fun () ->
    Pool.clear ();
    ignore (Preview.send (Ready (Some epoch, "")))) (fun () ->
    List.iter (fun tx -> ignore (Pool.add_smart ~head:current ~lookup tx |> Result.get_ok))
      [first; second; other];
    let pending = Pool.all () in
    let cost = Pool.staging_total_ou () in
    for index = 0 to Pool.max_staging_txs do
      let hash = Printf.sprintf "%064x" index in
      expect "capacity refusal failed"
        (Preview.send (Refuse {head = current; epoch; hash}) = Ok ())
    done;
    expect "capacity control did not close intake"
      (Preview.send (Check (Some current, Tx.hash other))
       = Error "staging full until epoch commit");
    expect "intake capacity suppressed queued work" (ready (Some current) other = Ok ());
    expect "full refusal quota blocked hold"
      (send (Hold {head = current; epoch; hash = Tx.hash first}) = Ok ());
    expect "successful hold was not remembered"
      (ready None first = Error "preview refused in this epoch");
    expect "full quota duplicate hold failed"
      (send (Hold {head = current; epoch; hash = Tx.hash first}) = Ok ());
    expect "full quota hold changed pending work"
      (Pool.all () = pending && Z.equal (Pool.staging_total_ou ()) cost
       && Pool.lookup_dropped (Tx.hash first) = None);
    let selected = Pool.ready_epoch_txs
      ~accept:(fun tx -> ready (Some current) tx = Ok ())
      ~capacity:Pool.max_ou ~confirmed_nonce:(fun _ -> Some 0) in
    expect "full quota hold suppressed unrelated sender" (selected = [other]);
    expect "full quota hold changed intake refusal"
      (Preview.send (Check (None, Tx.hash first)) = Error "preview refused in this epoch");
    expect "head advance retained capacity hold" (ready (Some epoch) first = Ok ());
    expect "head advance did not reopen intake"
      (Preview.send (Check (Some epoch, Tx.hash other)) = Ok ());
    expect "late capacity hold accepted"
      (Result.is_error (send (Hold {head = current; epoch; hash = Tx.hash first}))
       && ready None first = Ok ()))

let () =
  Mirage_crypto_rng_unix.use_default ();
  let cases = match Array.to_list Sys.argv with
    | [_; "--intake-ready"] ->
      ["hold_capacity", (fun () -> test_hold_capacity ~send:(function
        | Pool.Preview.Ready (head, hash) -> Pool.Preview.send (Check (head, hash))
        | message -> Pool.Preview.send message) ())]
    | [_; "--untracked-hold"] ->
      ["hold_capacity", (fun () -> test_hold_capacity ~send:(function
        | Pool.Preview.Hold {head; epoch; hash} -> Pool.Preview.send (Refuse {head; epoch; hash})
        | message -> Pool.Preview.send message) ())]
    | [_] ->
      ["intake", test_intake; "selection", test_selection; "proposal", test_proposal;
       "delivery", test_delivery; "circle_intake", test_circle_intake;
       "program_intake", test_program_intake;
       "circle_selection", test_circle_selection; "circle_bytes", test_circle_bytes;
       "deploy_intake", test_deploy_intake;
       "hold_state", test_hold_state; "hold_intake", test_hold_intake;
       "hold_entry", test_hold_entry; "hold_capacity", (fun () -> test_hold_capacity ())]
    | _ -> invalid_arg "queue test arguments" in
  let failed = List.filter_map (fun (name, run) ->
    try run (); Printf.printf "event = test name = %s status = passed\n%!" name; None
    with exn -> Printf.eprintf "event = test name = %s status = failed error = %s\n%!"
      name (Printexc.to_string exn); Some name)
    cases in
  if failed <> [] then exit 1