(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_consensus.C_types
module P = Octra_consensus.C_quorum_policy
module H = Octra_consensus.C_hash
module A = Octra_bootstrap.Sync_anchor
module K = Quorum_pinned

let expect label value = if not value then failwith ("test_pair_site: " ^ label)

let chain_id = "pair-site"
let policy = P.make ~count_epoch:10L ~stake_epoch:(Some 20L) ~round_epoch:None
let epochs = [5L, P.Prior; 15L, P.Capped; 25L, P.Stake]
let rule_name = function P.Prior -> "prior" | P.Capped -> "capped" | P.Stake -> "stake"

let keys = List.init 4 (fun index ->
  let secret = String.make 32 (Char.chr (index + 61)) in
  let key = match Mirage_crypto_ec.Ed25519.priv_of_octets secret with
    | Ok key -> key | Error _ -> failwith "test_pair_site: key is invalid" in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
  T.{address; pubkey}, secret)

let validator_set =
  match T.make_weighted_validator_set (List.mapi (fun index (member, _) ->
    member, Z.of_int (if index = 0 then 100 else 1)) keys) with
  | Ok value -> value | Error reason -> failwith reason

let finalize epoch_id signers =
  let header = T.{proto_version = proto_version_current; chain_id; epoch_id;
    prev_state_root = String.make 32 'p'; tx_list_hash = H.tx_list_hash [];
    receipt_root = H.receipt_root []; proposed_state_root = String.make 32 's';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (fst (List.hd keys)).address; txid_hi = 0L; ts = Int64.to_float epoch_id} in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun index ->
    let (member : T.validator_info), secret = List.nth keys index in
    let vote = T.{chain_id; epoch_id; round = 0; vote_type = Precommit;
      proposal_id; validator = member.address; signature = ""} in
    T.{vote with signature = H.sign_ed25519 ~priv_raw:secret ~msg:(H.vote_sign_bytes vote)})
    signers in
  T.{chain_id; epoch_id; commit_round = 0; header; proposal_id; precommits;
    parent_commit = None}

let trusted = match A.encoded_validator_set validator_set with
  | Ok value -> value | Error reason -> failwith reason

let verdict epoch_id signers =
  A.make ~steps:[] ~finalize:(finalize epoch_id signers) ~validator_set
  |> A.encode
  |> A.verify_path ~policy ~validator_set:trusted
  |> function Ok _ -> "accepted" | Error reason -> reason

let pair = "finality qc signer quorum"
let quorum = "finality qc quorum"

let cases = [
  [0], [pair; quorum; quorum];
  [0; 1], [pair; quorum; quorum];
  [0; 1; 2], ["accepted"; "accepted"; "accepted"];
  [1; 2; 3], [quorum; "accepted"; quorum];
  [0; 1; 2; 3], ["accepted"; "accepted"; "accepted"];
]

let anchor () =
  List.iter (fun (signers, expected) ->
    List.iter2 (fun (epoch_id, rule) expected ->
      expect "policy does not select the rule" (P.select policy epoch_id = rule);
      let found = verdict epoch_id signers in
      let addresses = List.map (fun index -> (fst (List.nth keys index)).T.address) signers in
      let vote_set = K.C_types.validator_set_for_epoch ~policy ~chain_id ~epoch_id validator_set in
      let where = Printf.sprintf "rule = %s signers = %s found = %s expected = %s"
        (rule_name rule) (String.concat "," (List.map string_of_int signers)) found expected in
      expect ("anchor finality differs " ^ where) (String.equal found expected);
      expect ("anchor finality disagrees with the pinned pair " ^ where)
        (String.equal found "accepted"
          = (K.C_types.has_quorum_at ~policy ~chain_id ~epoch_id validator_set addresses
            && K.C_types.has_quorum vote_set addresses)))
      epochs expected)
    cases

let range first last = List.init (max 0 (last - first + 1)) (fun index -> first + index)

let contains text part =
  let size = String.length part in
  List.exists (fun start -> String.equal (String.sub text start size) part)
    (range 0 (String.length text - size))

let body text name =
  let head = "\nlet " ^ name ^ " " in
  match List.find_opt (fun start -> String.equal (String.sub text start (String.length head)) head)
    (range 0 (String.length text - String.length head)) with
  | None -> failwith ("test_pair_site: function is absent name = " ^ name)
  | Some start ->
    let rest = String.sub text (start + 1) (String.length text - start - 1) in
    let stop = List.find_opt (fun index -> String.equal (String.sub rest index 5) "\nlet ")
      (range 0 (String.length rest - 5)) in
    String.sub rest 0 (Option.value stop ~default:(String.length rest))

let asks = [
  "verify_anchor_finalize", "C_types.pair_at ?policy ~chain_id ~epoch_id:finalize.";
  "extend_path", "C_types.pair_at ?policy ~chain_id ~epoch_id:finalize.";
  "step_input", "C_relief.restore ?policy ~chain_id";
]

let compared = ["has_quorum"; ">= "; "Z.geq"; "Z.gt"; ".quorum"]

let sites () =
  let anchor = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  List.iter (fun (name, call) ->
    expect (Printf.sprintf "site does not ask by epoch function = %s call = %s" name call)
      (contains (body anchor name) call))
    asks;
  List.iter (fun name ->
    List.iter (fun part ->
      expect (Printf.sprintf "site compares beside the call function = %s text = %s" name part)
        (not (contains (body anchor name) part)))
      compared)
    ["verify_anchor_finalize"; "extend_path"];
  let certificate = In_channel.with_open_bin Sys.argv.(2) In_channel.input_all
    |> String.split_on_char '\n'
    |> List.concat_map (String.split_on_char ' ')
    |> List.filter (fun word -> word <> "")
    |> String.concat " " in
  List.iter (fun part ->
    expect ("certificate check holds a second quorum text = " ^ part)
      (not (contains certificate part)))
    ["strict"; "C_types.has_quorum "; "C_quorum_policy."];
  expect "certificate check does not ask by epoch"
    (contains certificate
      "C_types.has_quorum_at ?policy ~chain_id ~epoch_id:certificate.epoch_id validator_set addrs")

let () =
  anchor ();
  sites ();
  print_endline "status = pass test = pair_site"