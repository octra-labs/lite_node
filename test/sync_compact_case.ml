(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module A = Octra_bootstrap.Sync_anchor
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module I = Octra_core.Store_irmin
module U = Octra_core.Validator_set_update

let get = function Ok value -> value | Error reason -> failwith reason
let check name value = if not value then failwith name
let chain_id = "octra-sync-test"
let hash s = Digestif.SHA256.(digest_string s |> to_raw_string)
let hex s = Digestif.SHA256.(digest_string s |> to_hex)
let keys = List.init 136 (fun index ->
  let secret = String.make 32 (Char.chr (100 + index)) in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets secret |> Result.get_ok in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
  C.{address; pubkey}, secret)

let members indices = List.map (fun index -> fst (List.nth keys index)) indices
let set entries = C.make_weighted_validator_set (List.map (fun member -> member, Z.one) entries) |> get

let final epoch validators root =
  let header = C.{proto_version = proto_version_current; chain_id; epoch_id = epoch;
    prev_state_root = hash "prior"; tx_list_hash = H.tx_list_hash [];
    receipt_root = H.receipt_root []; proposed_state_root = root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (List.hd validators.C.validators).address; txid_hi = 0L;
    ts = Int64.to_float epoch} in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun (member : C.validator_info) ->
    let secret = List.find (fun (key, _) -> key.C.pubkey = member.pubkey) keys |> snd in
    let vote = C.{chain_id; epoch_id = epoch; round = 0; vote_type = Precommit;
      proposal_id; validator = member.address; signature = ""} in
    C.{vote with signature = H.sign_ed25519 ~priv_raw:secret ~msg:(H.vote_sign_bytes vote)})
    validators.validators in
  C.{chain_id; epoch_id = epoch; commit_round = 0; header; proposal_id;
    precommits; parent_commit = None}

let path store first sets =
  let steps, last = List.fold_left (fun (steps, prior) (index, next) ->
    let epoch = 3 * (index + 1) in
    let update = U.make_weighted ~source_epoch:(Int64.of_int (epoch - 1))
      ~activate_epoch:(Int64.of_int (epoch + 1)) (List.map (fun (v : C.validator_info) ->
        Octra_core.Validator_entry.{address = v.address; pubkey = v.pubkey; weight = Z.one}) next)
      |> get |> U.to_string in
    Lwt_main.run (I.set_meta store U.pending_meta_key update);
    Lwt_main.run (I.tag_epoch store epoch);
    let proof = Lwt_main.run (I.merkle_proof_at_epoch store epoch ["meta"; U.pending_meta_key]) |> get in
    let index_root = hex (string_of_int epoch) in
    let root = Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:proof.ledger_state_root ~epoch_index_root:index_root
      |> Digestif.SHA256.of_hex |> Digestif.SHA256.to_raw_string in
    let step = A.{source = Pending; finalize = final (Int64.of_int epoch) prior root;
      ledger_state_root = proof.ledger_state_root; epoch_index_root = index_root;
      update; proof = proof.proof} in
    step :: steps, set next) ([], set first) (List.mapi (fun i x -> i, x) sets) in
  let epoch = Int64.of_int (3 * List.length sets + 2) in
  A.make ~steps:(List.rev steps) ~finalize:(final epoch last (hash "end")) ~validator_set:last

let large_path () =
  Test_workspace.with_dir "sync_budget" (fun root ->
    let store = Lwt_main.run (I.open_store ~fresh:true root) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (I.close store)) (fun () ->
      Lwt_main.run (I.set_meta store (String.make 800_000 'k') "kept");
      let first = members [0; 1; 2; 3] in
      let sets = List.init 32 (fun index -> members (List.init 4 (fun n -> 4 * (index + 1) + n))) in
      let anchor = path store first sets in
      let last = List.hd (List.rev (A.steps anchor)) in
      let epoch = (A.finality anchor).C.epoch_id in
      let validators = A.validator_set anchor in
      let finalize = final epoch validators last.finalize.header.proposed_state_root in
      let trusted = A.encoded_validator_set (set first) |> get in
      let module Head = Octra_core.Head_manifest in
      let head = Head.{schema_version; generation = Int64.to_int epoch;
        epoch_id = Int64.to_int epoch; state_root = Digestif.SHA256.(of_raw_string
          finalize.header.proposed_state_root |> to_hex);
        ledger_state_root = Some last.ledger_state_root; irmin_commit = Some (hex "commit");
        txid_hi = 0L; txlog_seg = Some 0; txlog_off = Some 0; epochlog_off = Some 0;
        commit_id = A.finalize_hash finalize; ts = finalize.header.ts;
        quorum_cert_hash = Some (A.finalize_hash finalize);
        epoch_index_hash = Some (hex "index"); epoch_index_root = Some last.epoch_index_root} in
      let record = Octra_node_runtime.Consensus_finality_journal.{finalize;
        validator_set = validators; bundle = None} in
      let result = Lwt_main.run (Octra_node_runtime.Sync_publish.prepare ~chain_id
        ~config_hash:(hex "config") ~trusted_validator_set:trusted ~validator_set:validators
        ~steps:(A.steps anchor) ~head record) in
      check "large necessary path reached capture"
        (result = Error "state sync finality path exceeds certificate budget before capture");
      Printf.printf "event = sync_budget bytes = %d status = pass\n%!" (String.length (A.encode anchor))))

let run () =
  let first = members [0; 1; 2; 3] in
  let changed = List.mapi (fun index (member : C.validator_info) ->
    C.{member with pubkey = (fst (List.nth keys (index + 8))).pubkey}) first in
  List.iter (fun (name, sets, count) ->
    Test_workspace.with_dir name (fun root ->
      let store = Lwt_main.run (I.open_store ~fresh:true root) in
      Fun.protect ~finally:(fun () -> Lwt_main.run (I.close store)) (fun () ->
        let trusted = A.encoded_validator_set (set first) |> get in
        let original = path store first sets in
        ignore (A.verify_path ~validator_set:trusted (A.encode original) |> get);
        let reduced = Lwt_main.run (A.compact_lwt ~validator_set:trusted original) |> get in
        check "path link count differs" (List.length (A.steps reduced) = count);
        check "path endpoint changed" (A.finality reduced = A.finality original);
        check "path size grew" (String.length (A.encode reduced) <= String.length (A.encode original));
        ignore (A.verify_path ~validator_set:trusted (A.encode reduced) |> get);
        let again = Lwt_main.run (A.compact_lwt ~validator_set:trusted reduced) |> get in
        check "path changes on repeat" (A.encode again = A.encode reduced);
        let broken = List.mapi (fun index (step : A.step) ->
          if index <> 0 then step else {step with proof = "invalid"}) (A.steps original) in
        let broken = A.make ~steps:broken ~finalize:(A.finality original)
          ~validator_set:(A.validator_set original) in
        check "omitted bad link was accepted"
          (Result.is_error (Lwt_main.run (A.compact_lwt ~validator_set:trusted broken)));
        if count = 1 then check "foreign vote retained"
          (List.for_all (fun (vote : C.vote) -> C.is_validator (set first) vote.validator)
            (List.hd (A.steps reduced)).finalize.precommits);
        Printf.printf "event = sync_compact case = %s links = %d status = pass\n%!" name count)))
    ["sync_overlap", [members [0; 1; 2; 4]; members [0; 1; 2; 5]], 1;
     "sync_rotation", [members [0; 4; 5; 6]; members [0; 4; 5; 7]], 2;
     "sync_keys", [changed; members [0; 4; 5; 6]], 2];
  large_path ()