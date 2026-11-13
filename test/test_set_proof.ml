(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Store_irmin
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module A = Octra_bootstrap.Sync_anchor
module U = Octra_core.Validator_set_update
module E = Octra_core.Validator_entry
module P = S.Store.Tree.Proof
module N = Irmin.Node.Make(S.Store.Hash)(S.Store.Path)(S.Store.Metadata)
open Yojson.Safe.Util

let get = function Ok value -> value | Error reason -> failwith reason
let run = Lwt_main.run
let hex value = Hex.of_string value |> Hex.show
let sha value = Digestif.SHA256.(digest_string value |> to_hex)
let raw value = Digestif.SHA256.(digest_string value |> to_raw_string)
let hash value = Irmin.Type.to_string S.Store.Hash.t value

let preimage typ value =
  let buffer = Buffer.create 128 in
  Irmin.Type.(unstage (pre_hash typ)) value (Buffer.add_string buffer);
  Buffer.contents buffer

let rec tree (value : P.tree) =
  match value with
  | Contents (value, ()) ->
    let bytes = preimage Irmin.Contents.String.t value in
    let digest = S.Store.Hash.hash (fun emit -> emit bytes) in
    `Contents (digest, ()), `Assoc ["contents", `String (hex value); "preimage", `String (hex bytes)]
  | Blinded_contents (digest, ()) -> `Contents (digest, ()), `Assoc ["value_hash", `String (hash digest)]
  | Blinded_node digest -> `Node digest, `Assoc ["node_hash", `String (hash digest)]
  | Node fields ->
    let fields = List.map (fun (name, value) -> name, tree value) fields in
    let node = N.of_list (List.map (fun (name, (value, _)) -> name, value) fields) in
    `Node (N.hash_exn node), `Assoc [
      "node", `List (List.map (fun (name, (_, value)) -> `List [`String name; value]) fields);
      "preimage", `String (hex (preimage N.t node))]
  | Inode _ | Extender _ -> failwith "Large Irmin proof unsupported"

let group offset = List.init 8 (fun index ->
  let secret = String.make 32 (Char.chr (offset + index)) in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets secret |> Result.get_ok in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
  C.{address; pubkey}, secret)

let set keys = get (C.make_weighted_validator_set (List.mapi (fun index (member, _) ->
  member, Z.of_int (if index = 0 then 80 else 10)) keys))

let members (set : C.validator_set) = List.map (fun (v : C.validator_info) ->
  `Assoc ["address", `String v.address; "key", `String (hex v.pubkey);
    "weight", `Intlit (Z.to_string (List.assoc v.address set.weights))]) set.validators

let votes (final : C.finalize) = List.map (fun (vote : C.vote) ->
  `Assoc ["address", `String vote.validator; "signature", `String (hex vote.signature)]) final.precommits

let make_step store chain keys next epoch source activate =
  let update = get (U.make_weighted ~source_epoch:(Int64.sub activate 4L) ~activate_epoch:activate
    (List.map (fun (v : C.validator_info) -> E.{address = v.address; pubkey = v.pubkey;
      weight = List.assoc v.address next.C.weights}) next.validators)) in
  let path = match source with A.Pending -> U.pending_meta_key | A.Active -> U.active_meta_key
    | A.Relief _ -> failwith "Relief unsupported" in
  run (S.set_meta store path (U.to_string update));
  let proof = get (run (S.merkle_proof store ["meta"; path])) in
  let ledger_state_root = proof.S.ledger_state_root in
  let epoch_index_root = sha (Int64.to_string epoch) in
  let folded = Octra_core.Epoch_index_commitment.folded_state_root ~ledger_state_root ~epoch_index_root in
  let header = C.{proto_version = proto_version_current; chain_id = chain; epoch_id = epoch;
    prev_state_root = raw "prior"; tx_list_hash = H.tx_list_hash []; receipt_root = H.receipt_root [];
    proposed_state_root = Digestif.SHA256.(of_hex folded |> to_raw_string);
    parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = (fst (List.hd keys)).address;
    txid_hi = 0L; ts = Int64.to_float epoch} in
  let proposal_id = H.proposal_id header in
  let precommits = List.filteri (fun index _ -> index < 6) keys |> List.map (fun (member, secret) ->
    let vote = C.{chain_id = chain; epoch_id = epoch; round = 2; vote_type = Precommit;
      proposal_id; validator = member.address; signature = ""} in
    C.{vote with signature = H.sign_ed25519 ~priv_raw:secret ~msg:(H.vote_sign_bytes vote)}) in
  let finalize = C.{chain_id = chain; epoch_id = epoch; commit_round = 2; header;
    proposal_id; precommits; parent_commit = None} in
  let step = A.{source; finalize; ledger_state_root; epoch_index_root;
    update = U.to_string update; proof = proof.proof} in
  let proof = proof.proof |> Base64.decode_exn |> S.proof_of_bin |> Result.get_ok in
  let root, value = tree (P.state proof) in
  if root <> P.before proof || P.before proof <> P.after proof then failwith "Exported proof root differs";
  let json = `Assoc [
    "source", `String (match source with A.Pending -> "Pending" | A.Active -> "Active" | _ -> assert false);
    "header", `String (hex (Octra_net.Oce1.encode (fun buffer -> Octra_consensus.C_codec.encode_epoch_header buffer header)));
    "round", `Int 2; "votes", `List (votes finalize); "ledger_root", `String ledger_state_root;
    "epoch_index_root", `String epoch_index_root; "source_epoch", `Intlit (Int64.to_string (Option.get update.source_epoch));
    "activate_epoch", `Intlit (Int64.to_string activate); "validators", `List (members next); "tree", value] in
  step, json

let () =
  if Array.length Sys.argv <> 5 then failwith "VM evidence, portable evidence, store and output required";
  let old = Yojson.Safe.from_file Sys.argv.(1) |> member "native_finality" |> member "anchor" |> to_string |> A.decode |> get in
  let final = A.finality old in
  let target = A.validator_set old in
  let keys = group 11 and middle_keys = group 31 in
  let initial = set keys and middle = set middle_keys in
  let store = run (S.open_store ~fresh:true Sys.argv.(3)) in
  let active, first, second = Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let active = make_step store final.chain_id keys initial 90L A.Active 88L in
    let first = make_step store final.chain_id keys middle 91L A.Pending 92L in
    let second = make_step store final.chain_id middle_keys target 95L A.Pending 96L in
    active, first, second) in
  let policy = Octra_consensus.C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:None in
  let trusted = get (A.encoded_validator_set initial) in
  let verify steps = A.make ~steps ~finalize:final ~validator_set:target |> A.encode
    |> A.verify_path ~policy ~validator_set:trusted in
  ignore (get (verify [fst first; fst second]));
  ignore (get (verify [fst active; fst first; fst second]));
  let broken = [
    "omitted", [fst second];
    "reverse", [fst second; fst first];
    "repeated", [fst first; fst first; fst second];
    "wrong_path", [{(fst first) with source = A.Active}; fst second];
    "wrong_value", [{(fst first) with update = (fst second).update}; fst second];
    "wrong_root", [{(fst first) with ledger_state_root = String.make 128 '0'}; fst second]
  ] in
  List.iter (fun (name, steps) -> if Result.is_ok (verify steps) then failwith ("Native path accepted: " ^ name)) broken;
  let portable = Yojson.Safe.from_file Sys.argv.(2) |> member "evidence" in
  let fields = portable |> member "config" |> to_assoc |> List.filter (fun (key, _) -> key <> "validators" && key <> "first_epoch") in
  let config = `Assoc (fields @ ["first_epoch", `Int 90; "validators", `List (members initial)]) in
  let finality = `Assoc (("config", config) :: (portable |> to_assoc |> List.remove_assoc "config")) in
  Yojson.Safe.to_file Sys.argv.(4) (`Assoc ["direction", `String "OctraPath";
    "evidence", `Assoc ["finality", finality; "steps", `List [snd first; snd second]]]);
  Yojson.Safe.to_file (Sys.argv.(4) ^ ".active") (snd active);
  Printf.printf "event = set_proof status = passed positive = 2 negative = %d storage = irmin\n" (List.length broken)