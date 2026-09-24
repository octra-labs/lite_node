(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Octra_core.Pvac_legacy_public_replay
module A = Octra_core.Pvac_migration_admission
module S = Octra_core.Store_chaindata
module W = Test_workspace

let check name condition =
  if not condition then failwith ("test_history_total: " ^ name)

let unwrap = function
  | Ok value -> value
  | Error reason -> failwith reason

let addr = "octCixRsEcmuMHP1SHc4MMVJUSJZbUeQq9kBpNFNj1WKqeB"
let cipher = "hfhe_v1|history"

let encode point = Base64.encode_exn (Bytes.to_string point)

let point amount blinding =
  Pvac_ffi.pedersen_commit_amount amount (Bytes.make 32 blinding)

let tx ?(owner = addr) ?(amount = "0") ?payload op =
  let fields = [
    "from", `String owner;
    "to_", `String owner;
    "amount", `String amount;
    "op_type", `String op;
  ] in
  let fields =
    match payload with
    | None -> fields
    | Some value -> ("encrypted_data", value) :: fields
  in
  `Assoc fields

let claim ?(owner = addr) ?(extra = []) () =
  tx ~owner "claim" ~payload:(`Assoc ([
    "zero_proof", `String "zkzp_v2|proof";
    "claim_cipher", `String "hfhe_v1|claim";
    "output_id", `Int 1;
  ] @ extra))

let send =
  tx "stealth" ~payload:(`Assoc [
    "delta_cipher", `String "hfhe_v1|delta";
    "amount_commitment", `String (encode (point 4L '\002'));
    "send_zero_proof", `String "zkzp_v2|proof";
  ])

let history = [tx ~amount:"10" "encrypt"; tx ~amount:"3" "decrypt"]

let read values = R.replay_history ~addr values

let unresolved name decision =
  check (name ^ " public refusal") (not decision.R.can_public_migrate);
  check (name ^ " hidden class") (decision.audit_class = R.Hidden_witness);
  check (name ^ " no partial point") (decision.commitment_net = None)

let test_controls () =
  let public = read history in
  check "public amount" (public.public_net = Some (Z.of_int 7));
  check "public point" (public.commitment_net = Some (encode (point 7L '\000')));
  check "public allowed" public.can_public_migrate;
  check "empty point"
    ((read []).commitment_net = Some (encode (Pvac_ffi.pedersen_identity ())));
  check "other address ignored"
    (read (claim ~owner:"octOther" () :: history) =
     {public with R.effects = R.Neutral :: public.effects});
  let hidden = read (history @ [send]) in
  let expected = Pvac_ffi.pedersen_sub (point 7L '\000') (point 4L '\002') in
  check "outgoing point" (hidden.commitment_net = Some (encode expected));
  check "outgoing witness" (hidden.audit_class = R.Hidden_witness);
  check "outgoing public refusal" (not hidden.can_public_migrate)

let test_unresolved () =
  let cases = [
    "claim", claim ();
    "key_switch", tx "key_switch";
    "recrypt", tx "recrypt";
  ] in
  List.iter (fun (name, value) ->
    let sequences = [
      [value];
      value :: history;
      history @ [value];
      [List.hd history; value; List.hd (List.tl history)];
      value :: history @ [value];
      [send; value; send];
    ] in
    List.iter (fun values ->
      let decision = read values in
      unresolved name decision;
      check "unresolved reason" (decision.blockers <> [])) sequences) cases

let test_carried_points () =
  let points = [
    `Null;
    `String "bad";
    `String (Base64.encode_exn (String.make 32 '\255'));
    `String (encode (point 100L '\003'));
    `String (encode (Pvac_ffi.pedersen_identity ()));
  ] in
  List.iter (fun value ->
    let receipt = claim ~extra:["amount_commitment", value] () in
    unresolved "carried point" (read (history @ [receipt]))) points;
  let bad = read (history @ [tx "claim"]) in
  check "malformed class" (bad.audit_class = R.Poisoned);
  check "malformed point" (bad.commitment_net = None)

let test_payload_forms () =
  let receipt = claim () in
  let encoded =
    match receipt with
    | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
        if key = "encrypted_data" then key, `String (Yojson.Safe.to_string value)
        else key, value) fields)
    | _ -> failwith "claim shape"
  in
  List.iter (fun receipt ->
    unresolved "claim encoding" (read (history @ [receipt]))) [receipt; encoded];
  check "equivalent encoding" (read [receipt] = read [encoded])

let entry replay = A.{
  address = addr;
  source_cipher_hash = A.source_cipher_hash cipher;
  total = 3;
  decision = replay;
}

let artifact decision =
  A.create ~chain_id:"octra-devnet" ~snapshot_epoch:99
    ~state_root:(String.make 64 '1') ~activation_epoch:100 [entry decision]
  |> unwrap

let with_artifact decision action =
  W.with_dir "history_total" (fun root ->
    let path = A.state_path root in
    Unix.mkdir (Filename.dirname path) 0o700;
    let value = artifact decision in
    let json = A.to_yojson value |> unwrap in
    let out = open_out_bin path in
    Fun.protect ~finally:(fun () -> close_out out)
      (fun () -> output_string out (Yojson.Safe.to_string json));
    let hash = Option.get (A.root value) in
    let loaded = A.load_env ~chain_id:"octra-devnet" ~data_dir:root
      ~getenv:(function "OCTRA_PVAC_MIGRATION_ROOT" -> Some hash | _ -> None)
      |> unwrap in
    check "artifact root" (A.root loaded = Some hash);
    action loaded)

let test_artifact () =
  let decision = read (history @ [claim ()]) in
  with_artifact decision (fun loaded ->
    List.iter (fun epoch ->
      let restored = A.decision loaded ~epoch ~address:addr ~cipher in
      unresolved "restored claim" restored;
      check "restored blocker" (restored.blockers = decision.blockers)) [100; 101]);
  let prior = {decision with R.commitment_net = Some (encode (point 7L '\000'))} in
  check "changed artifact root" (A.root (artifact prior) <> A.root (artifact decision));
  with_artifact prior (fun loaded ->
    let restored = A.decision loaded ~epoch:100 ~address:addr ~cipher in
    check "configured artifact unchanged" (restored.commitment_net = prior.commitment_net))

let store root action =
  let handle = S.open_chaindata root in
  Fun.protect ~finally:(fun () -> S.close handle) (fun () -> action handle)

let save handle index value =
  let op = Yojson.Safe.Util.(value |> member "op_type" |> to_string) in
  let raw = Yojson.Safe.to_string value in
  S.save_tx handle ~hash:(Printf.sprintf "%064x" (index + 1)) ~epoch_id:0
    ~from_addr:addr ~to_addr:addr ~tx_json:raw ~op_type:op
    ~encrypted_data:"" ~message:""

let test_store () =
  W.with_dir "history_store" (fun root ->
    store root (fun handle ->
      S.begin_batch handle;
      List.iteri (save handle) (history @ [claim ()]);
      S.commit_batch handle);
    store root (fun handle ->
      let status = S.pvac_legacy_public_replay_by_addr handle addr ~max_txs:3 in
      check "stored complete"
        (status.complete && status.total = 3 && status.scanned = 3);
      unresolved "stored claim" status.decision;
      with_artifact status.decision (fun loaded ->
        unresolved "stored artifact" (A.decision loaded ~epoch:100 ~address:addr ~cipher));
      let capped = S.pvac_legacy_public_replay_by_addr handle addr ~max_txs:2 in
      check "cap incomplete" (not capped.complete);
      check "cap point" (capped.decision.commitment_net = None)))

let () =
  test_controls ();
  test_unresolved ();
  test_carried_points ();
  test_payload_forms ();
  test_artifact ();
  test_store ();
  print_endline "status = pass test = history_total"