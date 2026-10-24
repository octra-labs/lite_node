(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_consensus.C_types
module P = Octra_consensus.C_quorum_policy
module H = Octra_consensus.C_hash
module Config = Octra_consensus.C_config
module Anchor = Octra_bootstrap.Sync_anchor
module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module S = Octra_core.Store_irmin
module U = Octra_core.Validator_set_update
module Image = Octra_core.Ledger_image
module V = Octra_node_runtime.Validator_config

let get = function Ok value -> value | Error reason -> failwith reason
let check label condition = if not condition then failwith label
let run = Lwt_main.run
let sha value = Digestif.SHA256.(digest_string value |> to_hex)
let chain_id = "stake-sync"
let policy = P.make ~count_epoch:0L ~stake_epoch:(Some 20L) ~round_epoch:(Some 20L)

let keys = List.init 7 (fun index ->
  let secret = String.make 32 (Char.chr (index + 31)) in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets secret
    |> Result.map_error (fun _ -> "invalid key") |> get in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
  C.{address; pubkey}, secret)

let update epoch amount =
  let members = List.mapi (fun index ((member : C.validator_info), _) ->
    Octra_core.Validator_entry.{address = member.address; pubkey = member.pubkey;
      weight = Z.of_int (if index = 0 then amount else 2_000_000)}) keys in
  U.make_weighted ~source_epoch:(Int64.pred epoch) ~activate_epoch:epoch members |> get

let finality epoch (proof : S.merkle_proof) index_root signers =
  let root = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:proof.S.ledger_state_root ~epoch_index_root:index_root in
  let header = C.{proto_version = proto_version_current; chain_id; epoch_id = epoch;
    prev_state_root = String.make 32 'p'; tx_list_hash = H.tx_list_hash [];
    receipt_root = H.receipt_root []; proposed_state_root = Digestif.SHA256.(of_hex root |> to_raw_string);
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (fst (List.hd keys)).address; txid_hi = 0L; ts = Int64.to_float epoch} in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun ((member : C.validator_info), secret) ->
    let vote = C.{chain_id; epoch_id = epoch; round = 0; vote_type = Precommit;
      proposal_id; validator = member.address; signature = ""} in
    C.{vote with signature = H.sign_ed25519 ~priv_raw:secret ~msg:(H.vote_sign_bytes vote)}) signers in
  C.{chain_id; epoch_id = epoch; commit_round = 0; header; proposal_id;
    precommits; parent_commit = None}

let proof store epoch key =
  run (S.tag_epoch store (Int64.to_int epoch));
  run (S.merkle_proof_at_epoch store (Int64.to_int epoch) ["meta"; key]) |> get

let checkpoint final (proof : S.merkle_proof) index_root set = Checkpoint.{
  chain_id; epoch = final.C.epoch_id; state_root = Checkpoint.raw_to_hex final.header.proposed_state_root;
  ledger_state_root = proof.S.ledger_state_root; txid_hi = 0L; config_hash = sha "config";
  validator_set_hash = Checkpoint.raw_to_hex (Config.validator_set_hash set);
  quorum_cert_hash = Some (Anchor.finalize_hash final); epoch_index_hash = Some (sha "index");
  epoch_index_root = Some index_root; created_at = 0L; valid_until = 1000L}

let resume root store cp anchor trusted stake =
  let module M = Octra_bootstrap.State_sync_manifest in
  let module Chain = Octra_node_runtime.Sync_chain in
  let module Journal = Octra_node_runtime.Consensus_finality_journal in
  let checkpoint_hash = Checkpoint.hash cp |> get in
  let files = ["HEAD.json"; "ledger.dat"; Octra_bootstrap.Root_win.name; "state_root"]
    |> List.sort String.compare |> List.map (fun path -> M.{path; size = 1L;
      sha256 = sha "x"; chunks = [{index = 0; offset = 0L; size = 1; sha256 = sha "x"}]}) in
  let manifest = M.{checkpoint_hash; snapshot_id = checkpoint_hash; irmin_commit = None;
    chunk_size = chunk_size_min; total_size = 4L; file_count = 4; chunk_count = 4;
    chunks_root = chunks_root files; files} in
  let signatures = List.map (fun ((member : C.validator_info), secret) ->
    let wallet = Octra_core.Crypto.Wallet.{address = member.address;
      pub = Base64.encode_exn member.pubkey; priv = Base64.encode_exn secret} in
    M.make_exporter_signature ~wallet manifest |> get) keys
    |> List.sort (fun (left : Checkpoint.signature) right -> String.compare left.signer right.signer) in
  let omitted = run (Anchor.compact_lwt ~policy ~validator_set:trusted anchor) |> get in
  let cert = M.{checkpoint = cp; checkpoint_hash; manifest; manifest_hash = manifest_hash manifest |> get;
    authority = Finalized (Anchor.encode omitted); exporter_signatures = signatures} in
  ignore (M.verify_certificate ~validator_set:trusted ~exporter_set:trusted cert |> get);
  let path = Filename.concat root "certificate.json" in
  M.write_json path (M.certificate_json cert);
  let source = Filename.concat root "prior.dat" in
  let target = Filename.concat root "prior-store" in
  let commit = run (S.get_commit_hash store) |> Option.get in
  ignore (run (Image.write store ~commit ~path:source) |> get);
  ignore (run (Image.restore ~source ~target ~expected_root:cp.ledger_state_root) |> get);
  let restored = run (S.open_store target) in
  let chaindata = Octra_core.Store_chaindata.open_chaindata (Filename.concat root "empty-history") in
  Fun.protect ~finally:(fun () ->
    run (S.close restored); Octra_core.Store_chaindata.close chaindata) (fun () ->
    run (S.tag_epoch restored 19);
    run (S.tag_epoch restored 20);
    let deps = Chain.{data_dir = root; chain_id; store = restored; chaindata;
      read_finality = (fun _ -> Journal.Missing); certificate_path = (fun () -> path)} in
    let steps = run (Chain.build ~policy deps ~head_epoch:20L trusted stake) |> get in
    check "snapshot-only resume lost proof" (List.length steps = 2);
    check "snapshot-only resume guessed epoch" ((List.hd (List.rev steps)).Anchor.finalize.epoch_id = 19L);
    let changed = update 16L 22_000_000 in
    run (S.set_meta restored U.active_meta_key (U.to_string changed));
    run (S.tag_epoch restored 19);
    run (S.tag_epoch restored 20);
    check "snapshot-only resume accepted another state"
      (Result.is_error (run (Chain.build ~policy deps ~head_epoch:20L trusted (U.validator_set changed |> get))));
    steps)

let test_restore () = Test_workspace.with_dir "stake-sync" (fun root ->
  let store = run (S.open_store ~fresh:true (Filename.concat root "source")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let first = update 1L 16_000_000 |> U.validator_set |> get in
    let trusted = Anchor.encoded_validator_set first |> get in
    let prior = ref first in
    let steps = List.map (fun (epoch, amount) ->
      let next = update (Int64.succ epoch) amount in
      let raw = U.to_string next in
      run (S.set_meta store U.pending_meta_key raw);
      let state = proof store epoch U.pending_meta_key in
      let index_root = sha (Int64.to_string epoch) in
      let final = finality epoch state index_root (List.tl keys) in
      get (Anchor.verify_finalize ~policy ~chain_id ~validator_set:!prior final);
      let next_set = U.validator_set next |> get in
      check "raw changes lost" (Config.validator_set_hash next_set <> Config.validator_set_hash !prior);
      check "test cap changed" (Config.validator_set_hash (C.count_capped_set next_set)
        = Config.validator_set_hash (C.count_capped_set !prior));
      prior := next_set;
      run (S.set_meta store U.active_meta_key raw);
      Anchor.{source = Pending; finalize = final; ledger_state_root = state.ledger_state_root;
        epoch_index_root = index_root; update = raw; proof = state.proof}) [11L, 18_000_000; 15L, 20_000_000] in
    let stake = !prior in
    let old = List.hd steps in
    let old_set = C.validator_set_for_epoch ~policy ~chain_id
      ~epoch_id:old.finalize.epoch_id first in
    let expected = Octra_consensus.C_parent_commit.{chain_id; epoch_id = old.finalize.epoch_id;
      proposal_id = old.finalize.proposal_id; state_root = old.finalize.header.proposed_state_root;
      validator_set_hash = Config.validator_set_hash old_set} in
    let parent = C.{certificate = certificate_of_finalize old.finalize; validator_set = old_set} in
    check "historical certificate changed" (Octra_consensus.C_parent_commit.validate ~policy expected parent
      = Octra_consensus.C_parent_commit.Valid);
    check "historical certificate accepted current stake"
      (Octra_consensus.C_parent_commit.validate ~policy expected {parent with validator_set = stake}
       = Octra_consensus.C_parent_commit.Invalid "validator_set");
    let verify epoch signers =
      let state = proof store epoch U.active_meta_key in
      let index_root = sha (Int64.to_string epoch) in
      let final = finality epoch state index_root signers in
      let effective = C.validator_set_for_epoch ~policy ~chain_id ~epoch_id:epoch stake in
      let light = Octra_consensus.C_light_validator_set.of_validator_set ~chain_id
        ~config_hash:(Config.hash ~chain_id ~validator_set:effective ()) effective in
      check "light proof rejected" (Octra_consensus.C_light_validator_set.verify light);
      let changed = {light with validators = List.map
        (fun (member : Octra_consensus.C_light_validator_set.validator) ->
          {member with weight = Z.succ member.weight}) light.validators} in
      check "light proof accepted altered stake" (not (Octra_consensus.C_light_validator_set.verify changed));
      let anchor = Anchor.make ~steps ~finalize:final ~validator_set:effective in
      let cp = checkpoint final state index_root effective in
      let encoded = Anchor.encode anchor in
      check "unverified stake exposed" (Result.is_error (Anchor.transition_base anchor));
      let checked = Anchor.verify ~policy ~validator_set:trusted cp encoded |> get in
      let async = run (Anchor.verify_lwt ~policy ~validator_set:trusted cp encoded) |> get in
      List.iter (fun value ->
        check "verified bytes changed" (Anchor.encode value = encoded);
        let epoch, base = Anchor.transition_base value |> get in
        check "transition epoch changed" (epoch = 15L);
        check "verified stake changed" (Config.validator_set_hash base
          = Config.validator_set_hash stake)) [checked; async];
      let compact = run (Anchor.compact_lwt ~policy ~validator_set:trusted checked) |> get in
      check "transition compaction used wrong rule"
        (List.length (Anchor.steps compact) = 1);
      if epoch = 20L then check "stake compaction lost the latest raw stake"
        (Anchor.steps compact = [List.hd (List.rev steps)]);
      if epoch = 19L then begin
        let original = Anchor.make ~steps:[List.hd steps] ~finalize:final ~validator_set:effective in
        check "prior anchor bytes changed" (Anchor.encode compact = Anchor.encode original)
      end;
      ignore (Anchor.verify ~policy ~validator_set:trusted cp (Anchor.encode compact) |> get);
      if epoch = 20L then
        check "empty history accepted" (Result.is_error (Anchor.verify ~policy ~validator_set:trusted cp
          (Anchor.make ~steps:[] ~finalize:final ~validator_set:effective |> Anchor.encode)));
      if epoch = 19L then begin
        check "prior empty history accepted" (Result.is_error (Anchor.verify ~policy ~validator_set:trusted cp
          (Anchor.make ~steps:[] ~finalize:final ~validator_set:effective |> Anchor.encode)));
        let short = Anchor.verify ~policy ~validator_set:trusted cp (Anchor.encode compact) |> get in
        let base_epoch, base = Anchor.transition_base short |> get in
        check "omitted history pretends to be current" (base_epoch = 11L);
        check "omitted history changed proven stake" (Config.validator_set_hash base
          = Config.validator_set_hash (U.validator_set (update 12L 18_000_000) |> get))
      end;
      let bad = C.{final with precommits = List.map (fun (vote : C.vote) ->
        {vote with signature = String.make 64 '\000'}) final.precommits} in
      check "forged certificate accepted" (Result.is_error (Anchor.verify ~policy ~validator_set:trusted
        cp (Anchor.make ~steps ~finalize:bad ~validator_set:effective |> Anchor.encode)));
      if epoch = 20L then begin
        let light = finality epoch state index_root (List.tl keys) in
        check "count without stake accepted" (Result.is_error
          (Anchor.verify_finalize ~policy ~chain_id ~validator_set:stake light));
        let one = finality epoch state index_root [List.hd keys] in
        check "stake without count accepted" (Result.is_error
          (Anchor.verify_finalize ~policy ~chain_id ~validator_set:stake one))
      end;
      cp, checked in
    let old_cp, old_anchor = verify 19L (List.tl keys) in
    let resumed = resume root store old_cp old_anchor trusted stake in
    let cp, checked = verify 20L keys in
    ignore (Anchor.make ~steps:resumed ~finalize:(Anchor.finality checked)
      ~validator_set:stake |> Anchor.encode |> Anchor.verify ~policy ~validator_set:trusted cp |> get);
    let commit = run (S.get_commit_hash store) |> Option.get in
    List.iter (fun (format, name) ->
      let source = Filename.concat root (name ^ ".dat") in
      let target = Filename.concat root name in
      let written = run (Image.write ~format store ~commit ~path:source) |> get in
      let restored = run (Image.restore ~source ~target ~expected_root:cp.ledger_state_root) |> get in
      check "image accepted wrong root" (Result.is_error (run (Image.restore ~source
        ~target:(target ^ "-wrong") ~expected_root:(sha "wrong root"))));
      check "image root changed" (written.root = restored.root);
      let loaded = run (S.open_store target) in
      Fun.protect ~finally:(fun () -> run (S.close loaded)) (fun () ->
        let active_raw = run (S.get_meta loaded U.active_meta_key) in
        let config = V.build ~chain_id ~consensus_mode:true ~current_height:20L
          ~current_entries:[] ~next_entries:[] ~chain_pending_entries:[]
          ~next_activation_epoch:None ~program_trust_hash:None in
        check "missing stake metadata accepted"
          (V.bind_persistent_updates ~policy ~chain_id ~consensus_mode:true
            ~current_height:20L ~active_raw:None ~pending_raw:None config
           = Error "stake weights are missing");
        let config = V.bind_persistent_updates ~policy ~chain_id ~consensus_mode:true
          ~current_height:20L ~active_raw ~pending_raw:None config |> get in
        check "restored raw weights differ" (Config.validator_set_hash config.stake_vs
          = Config.validator_set_hash (Anchor.transition_base checked |> get |> snd));
        check "restored effective weights differ" (Config.validator_set_hash config.active_vs
          = Config.validator_set_hash (Anchor.validator_set checked))))
      [Image.Flat, "flat"; Image.Shared, "shared"]))

let test_profile () =
  let module Profile = Octra_node_runtime.Consensus_profile in
  let module Net = Octra_node_runtime.P2p_config in
  let chain_id = "octra-devnet-9871-cluster" in
  let policy = P.make ~count_epoch:1_306_547L ~stake_epoch:(Some 1_700_000L) ~round_epoch:(Some 1_700_000L) in
  let getenv name = Sys.getenv_opt name in
  let before = Profile.hash ~chain_id ~epoch:1_699_999 getenv in
  check "future stake changes prior profile"
    (Profile.hash ~policy ~chain_id ~epoch:1_699_999 getenv = before);
  let after = Profile.hash ~policy ~chain_id ~epoch:1_700_000 getenv in
  check "stake profile unchanged" (after <> Profile.hash ~chain_id ~epoch:1_700_000 getenv);
  check "stake switch missing" (Profile.switch_for policy ~chain_id ~applied_epoch:1_699_999);
  check "stake switch early" (not (Profile.switch_for policy ~chain_id ~applied_epoch:1_699_998));
  let plan = Net.standard_profiles ~policy ~chain_id getenv in
  check "stake profile not advertised" (List.exists (fun (row : Octra_net.P2p_swarm.profile) ->
    row.epoch = 1_700_000L && row.profile_hash = after) plan)

let test_same_set () = Test_workspace.with_dir "sync-same-set" (fun root ->
  let store = run (S.open_store ~fresh:true (Filename.concat root "store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let raw = update 12L 16_000_000 in
    let set = U.validator_set raw |> get in
    let trusted = Anchor.encoded_validator_set set |> get in
    let effective = C.count_capped_set set in
    check "same set lost weight contrast"
      (Config.validator_set_hash set <> Config.validator_set_hash effective);
    run (S.set_meta store U.pending_meta_key (U.to_string raw));
    let state = proof store 11L U.pending_meta_key in
    let index_root = sha "same-set-index" in
    let finalize = finality 11L state index_root (List.tl keys) in
    let link = Anchor.{source = Pending; finalize;
      ledger_state_root = state.ledger_state_root; epoch_index_root = index_root;
      update = U.to_string raw; proof = state.proof} in
    let final = finality 19L state index_root (List.tl keys) in
    let cp = checkpoint final state index_root effective in
    let anchor = Anchor.make ~steps:[link] ~finalize:final ~validator_set:effective in
    let checked = Anchor.verify ~policy ~validator_set:trusted cp (Anchor.encode anchor) |> get in
    check "empty proof kept prior weights"
      (Result.is_error (Anchor.verify ~policy ~validator_set:trusted cp
        (Anchor.make ~steps:[] ~finalize:final ~validator_set:effective |> Anchor.encode)));
    let compact = run (Anchor.compact_lwt ~policy ~validator_set:trusted checked) |> get in
    check "same set lost prior weight proof" (Anchor.encode compact = Anchor.encode anchor);
    ignore (Anchor.verify ~policy ~validator_set:trusted cp (Anchor.encode compact) |> get)))

let () = test_same_set (); test_restore (); test_profile (); print_endline "status = pass test = stake_sync"