(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Octra_core.Transaction
module Pool = Octra_core.Tx_staging
module Ledger = Octra_core.Ledger
module Store = Octra_core.Store_irmin
module Rest = Octra_node_runtime.Node_rest_facade
module Payload = Octra_core.Tx_payload
module Delivery = Octra_node_runtime.Set_delivery
module Wiring = Octra_node_runtime.Consensus_driver_wiring

let expect label value = if not value then failwith label

let private_key =
  Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 'd') |> Result.get_ok

let secret =
  Mirage_crypto_ec.Ed25519.priv_to_octets private_key |> Base64.encode_exn

let public_key =
  Mirage_crypto_ec.Ed25519.pub_of_priv private_key
  |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn

let signed ?(key = private_key) ?(nonce = 1) ?(op = Tx.ValidatorReady) message =
  let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  let address = Octra_core.Crypto.Address.address_from_pubkey public_key in
  let tx = Tx.{
    from = address; to_ = address; amount = Z.zero; nonce;
    ou = Z.of_int 1000; timestamp = 1.; signature = "";
    public_key = Some public_key; message = Some message;
    op_type = op; encrypted_data = None;
  } in
  let key = Mirage_crypto_ec.Ed25519.priv_to_octets key |> Base64.encode_exn in
  Tx.sign_with_privkey tx key

let pulse padding =
  Yojson.Safe.to_string (`Assoc [
    "consensus_pubkey", `String public_key;
    "head_epoch", `String "10";
    "state_root", `String (String.make 64 'a');
    "padding", `String padding;
  ])

let at_size size =
  signed (pulse (String.make (size - String.length (pulse "")) 'a'))

let with_runtime run =
  Test_workspace.with_dir "duty_wire" (fun dir ->
    let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
    Fun.protect
      ~finally:(fun () -> Pool.clear (); Lwt_main.run (Store.close store))
      (fun () ->
        Pool.clear ();
        let checked, drops = ref [], ref [] in
        let runtime = Rest.{
          swarm_ref = ref None;
          proof_mode = (fun () -> Octra_core.Rule_graph.Prior);
          compute_mode = (fun () -> Octra_core.Rule_graph.Prior);
          settle_mode = (fun () -> Octra_core.Rule_graph.Prior);
          marks_mode = (fun () -> Octra_core.Rule_graph.Prior);
          queue_head = (fun () -> None);
          duty_head = (fun () -> Some (10L, Octra_core.Rule_graph.Prior));
          preverify_admit = (fun tx -> checked := tx :: !checked; Ok ());
          save_drops = (fun rows -> drops := rows @ !drops);
          find_drop = (fun _ -> None);
          drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
        } in
        let ledger = Ledger.create store in
        Ledger.add_account_with_pubkey ledger (at_size 256).from
          (Z.of_int 1_000_000) public_key |> Result.get_ok;
        run runtime ledger checked drops))

let check_local () = with_runtime (fun runtime ledger checked _ ->
  let limit = Payload.standard.message_validator_len in
  List.iter (fun bft_mode ->
    Pool.clear ();
    checked := [];
    let oversized = at_size (limit + 1) in
    expect "network rejects oversized duty" (Result.is_error (Payload.admit oversized));
    expect "local queue rejects oversized duty"
      (Result.is_error (Rest.add_tx_to_staging ~relay:false ~bft_mode runtime ledger oversized));
    expect "oversized duty has no effects" (Pool.staging_size () = 0 && !checked = []);
    let exact = at_size limit in
    expect "network accepts exact limit" (Payload.admit exact = Ok ());
    expect "local queue accepts exact limit"
      (Rest.add_tx_to_staging ~relay:false ~bft_mode runtime ledger exact = Ok (Tx.hash exact));
    expect "accepted duty checked once" (!checked = [exact])) [false; true])

let insert tx =
  Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 1_000_000, 0)) tx
  |> Result.get_ok |> ignore

let check_cleanup () = with_runtime (fun runtime _ _ drops ->
  let oversized = at_size 16_847 in
  let valid = signed ~nonce:2 (pulse "") in
  insert oversized;
  insert valid;
  expect "restored duty occupies nonce" (Pool.duty_nonce oversized.from 0 = None);
  Rest.expire_duty ~sender:oversized.from runtime ();
  expect "invalid restored duty removed" (Pool.find_by_hash (Tx.hash oversized) = None);
  expect "valid restored duty retained" (Pool.find_by_hash (Tx.hash valid) = Some valid);
  expect "invalid restored duty releases nonce" (Pool.duty_nonce oversized.from 0 = Some 1);
  expect "removal records reason"
    (match !drops with
     | [row] -> row.Pool.d_hash = Tx.hash oversized
       && row.d_reason = Pool.Evicted
       && row.d_detail = "encrypted_data or message exceeds size limit"
     | _ -> false))

let check_retry () = with_runtime (fun runtime ledger checked _ ->
  let calls = ref 0 in
  let network = Delivery.{
    broadcast = (fun _ -> incr calls);
    post = (fun _ -> incr calls; Lwt.return_ok ());
  } in
  let oversized = at_size 16_847 in
  expect "oversized stage refused"
    (Result.is_error (Delivery.stage ~bft_mode:true runtime ledger oversized));
  expect "oversized retry refused"
    (Result.is_error (Lwt_main.run (Delivery.retry ~bft_mode:true runtime ledger network oversized)));
  expect "oversized retry has no effects" (!calls = 0 && !checked = [] && Pool.staging_size () = 0))

let check_selection () =
  Pool.clear ();
  Fun.protect ~finally:Pool.clear (fun () ->
    let oversized = at_size 16_847 in
    let successor = signed ~nonce:2 ~op:Tx.Standard "next" in
    let key = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 'e') |> Result.get_ok in
    let other = signed ~key ~op:Tx.Standard "other" in
    let adapters = Wiring.node_standard_adapters Wiring.{
      chain_id = "duty-wire"; getenv = (fun _ -> None); get_meta = (fun _ -> None);
      duty_state = (fun _ -> Ok Octra_core.Set_fold.empty);
      wallet_addr = other.from; wallet_pub = Option.get other.public_key;
      find_account = (fun _ -> Some {Ledger.empty_account with balance = Z.of_int 1_000_000});
      cached_head = (fun () -> None); read_prev_ledger_root = (fun () -> Lwt.return_none);
      next_txid = (fun () -> 0L);
      proposal_state = Octra_node_runtime.Consensus_proposal_state.create ();
      catchup_active = ref false; staging_epoch_capacity = Z.of_int 1_000_000;
      save_drops = ignore; write_pending = ignore;
      validator_pubkeys_for_epoch = (fun ~wallet_addr:_ ~wallet_pub:_ ~epoch:_ -> []);
    } in
    List.iter insert [oversized; successor; other];
    expect "saved oversized duty excluded from proposal" (adapters.staging_epoch_txs () = [other]);
    expect "selection preserves queue evidence" (Pool.staging_size () = 3))

let check_sign () =
  let limit = Payload.standard.message_validator_len in
  let oversized = at_size (limit + 1) in
  let refused =
    try Result.is_error (Delivery.sign ~key:"not a key" {oversized with signature = ""})
    with _ -> false in
  expect "oversized message refused before reading key"
    refused;
  let exact = at_size limit in
  let draft = {exact with Tx.signature = ""} in
  let expected = Tx.sign_with_privkey {draft with ou = Tx.ou_cost draft} secret in
  let actual = Delivery.sign ~key:secret draft |> Result.get_ok in
  expect "valid signing bytes unchanged" (actual = expected);
  expect "valid signature verified" (Tx.verify actual public_key)

let () =
  let cases = ["local", check_local; "cleanup", check_cleanup;
    "retry", check_retry; "selection", check_selection; "sign", check_sign] in
  let selected = match Array.to_list Sys.argv with
    | [_] -> cases
    | [_; name] -> [name, List.assoc name cases]
    | _ -> invalid_arg "duty wire test arguments" in
  List.iter (fun (name, run) ->
    run ();
    Printf.printf "event = duty_wire case = %s status = passed\n%!" name) selected