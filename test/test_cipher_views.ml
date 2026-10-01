(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module F = Octra_core.Crypto.FheBalance
module S = Octra_core.Store_irmin
module L = Octra_core.Ledger
module R = Octra_node_runtime.Account_read_rpc

let expect name ok = if not ok then failwith name
let get = function Ok value -> value | Error reason -> failwith reason
let refused = function Error _ -> true | Ok _ -> false
let unreadable = function
  | Error error -> error.Octra_core.Rpc.code = -32005
    && error.message = "encrypted ciphertext unavailable" && error.data = None
  | Ok _ -> false

let with_store name action =
  Test_workspace.with_dir name (fun dir ->
    let store = Lwt_main.run (S.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store))
      (fun () -> action store))

let test_bad_balance () =
  List.iter (fun cipher ->
    let account = {Octra_core.Ledger_types.empty_account with
      encrypted_balance = Some cipher} in
    expect "unreadable balance returned as success"
      (unreadable (Lwt_main.run (R.encrypted_cipher ~addr:"owner" ~account))))
    ["hfhe_v1|broken"; "hfhe_v1|";
     "hfhe_v1|" ^ Base64.encode_exn "invalid cipher bytes"]

let insert store cipher =
  Lwt_main.run (S.insert_stealth_output store ~stealth_tag:"tag"
    ~eph_pub:"eph" ~enc_amount:"enc" ~amount:"1" ~epoch_id:1
    ~tx_hash:(String.make 64 'a') ~sender_addr:"owner" ~claim_pub:"claim"
    ~delta_cipher_stored:cipher ~amount_hash:"hash" ~amount_commitment:"point")
  |> get |> Int64.to_int

let test_bad_output () =
  with_store "cipher_output" (fun store ->
    let cipher = "hfhe_v1|" ^ Base64.encode_exn "invalid cipher bytes" in
    let older = insert store "0" in
    let id = insert store cipher in
    let newer = insert store "0" in
    let page response = match response with
      | Ok value -> value
      | Error _ -> failwith "one cipher blocked the page" in
    let field = Yojson.Safe.Util.member in
    let outputs response = field "outputs" response |> Yojson.Safe.Util.to_list in
    let check response =
      let rows = outputs (page response) in
      let damaged = List.find (fun row -> field "id" row = `Int id) rows in
      let error = field "error" damaged in
      expect "cipher error missing"
        (field "code" error = `Int (-32005)
         && field "message" error = `String "encrypted ciphertext unavailable");
      expect "damaged bytes exposed" (field "delta_cipher_stored" damaged = `Null);
      expect "damaged output presented as usable" (field "eph_pub" damaged = `Null);
      expect "valid outputs removed" (List.length rows = 3)
    in
    let results = [
      Lwt_main.run (R.stealth_outputs store ~params:(`List [`Int 0]));
      Lwt_main.run (R.stealth_outputs_page store ~params:(`List [`Int 0]));
      Lwt_main.run (R.stealth_outputs_by_id store
        ~params:(`List [`List [ `Int older; `Int id; `Int newer ]]));
    ] in
    List.iter check results;
    let first = Lwt_main.run (R.stealth_outputs_page store
      ~params:(`List [`Int 0; `Null; `Int 2])) |> page in
    expect "page cursor lost" (field "has_more" first = `Bool true);
    let cursor = field "next_before_id" first in
    let next = Lwt_main.run (R.stealth_outputs_page store
      ~params:(`List [`Int 0; cursor; `Int 2])) |> page in
    expect "older outputs unreachable"
      (List.map (field "id") (outputs next) = [`Int older]);
    expect "scan did not complete" (field "has_more" next = `Bool false);
    let outputs = Lwt_main.run (S.get_stealth_outputs_by_ids store [id]) in
    expect "stored cipher changed"
      (List.map (fun item -> item.Octra_core.Ledger_types.delta_cipher_stored) outputs = [cipher]))

let test_unknown_deposit () =
  with_store "cipher_deposit" (fun store ->
    let ledger = L.create store in
    let address = "owner" in
    get (L.add_account ledger address (Z.of_int 100));
    get (L.update_enc_balance ledger address "legacy-unknown");
    let before = L.find ledger address in
    let supply = L.get_total_supply ledger in
    let wallet = Base64.encode_exn (String.make 32 '\003') in
    let result = L.fhe_encrypt_balance ledger address (Z.of_int 2) wallet
      ~tx_hash:"deposit" ~epoch_id:1 in
    expect "unknown balance replaced" (refused result);
    expect "failed deposit changed account" (L.find ledger address = before);
    expect "failed deposit changed supply" (Z.equal supply (L.get_total_supply ledger)))

let test_valid_views () =
  let wallet = Base64.encode_exn (String.make 32 '\003') in
  let pk, sk = F.derive_pvac_keys wallet in
  let cipher = F.deposit pk sk ~current_cipher:None ~amount:(Z.of_int 7)
    ~tx_hash:"deposit" ~epoch_id:1 |> get in
  List.iter (fun raw ->
    let account = {Octra_core.Ledger_types.empty_account with encrypted_balance = raw} in
    expect "valid balance refused"
      (not (refused (Lwt_main.run (R.encrypted_cipher ~addr:"owner" ~account)))))
    [None; Some ""; Some "0"; Some cipher];
  with_store "cipher_valid" (fun store ->
    let id = insert store cipher in
    expect "valid stealth output refused"
      (not (refused (Lwt_main.run
        (R.stealth_outputs_by_id store ~params:(`List [`List [`Int id]]))))))

let test_legacy_bytes () =
  let cipher = "legacy-opaque" in
  let account = {Octra_core.Ledger_types.empty_account with encrypted_balance = Some cipher} in
  match Lwt_main.run (R.encrypted_cipher ~addr:"owner" ~account) with
  | Error _ -> failwith "legacy read refused"
  | Ok response -> expect "legacy bytes changed"
      (Yojson.Safe.Util.member "cipher" response = `String cipher)

let () =
  let cases = ["bad_balance", test_bad_balance; "bad_output", test_bad_output;
    "unknown_deposit", test_unknown_deposit; "valid_views", test_valid_views;
    "legacy_bytes", test_legacy_bytes] in
  let failures = List.filter_map (fun (name, action) ->
    match action () with
    | () -> Printf.printf "event = test name = %s status = passed\n%!" name; None
    | exception exn ->
      Printf.eprintf "event = test name = %s status = failed reason = %s\n%!"
        name (Printexc.to_string exn);
      Some name) cases in
  if failures <> [] then exit 1