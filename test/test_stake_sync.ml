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
module O = Octra_net.Oce1

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

let finality ?(chain_id = chain_id) ?(tx_list_hash = H.tx_list_hash [])
    epoch (proof : S.merkle_proof) index_root signers =
  let root = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:proof.S.ledger_state_root ~epoch_index_root:index_root in
  let header = C.{proto_version = proto_version_current; chain_id; epoch_id = epoch;
    prev_state_root = String.make 32 'p'; tx_list_hash;
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

let test_seat_start () =
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = Int64.of_int (Option.get (P.seatless_activation_for_chain chain_id)).activation_epoch in
  let config = V.build ~chain_id ~consensus_mode:true ~current_height:epoch
    ~current_entries:[] ~next_entries:[] ~chain_pending_entries:[]
    ~next_activation_epoch:None ~program_trust_hash:None in
  let safe = update epoch 2_000_000 in
  let heavy = update epoch 16_000_000 in
  let bind height active pending = V.bind_persistent_updates ~chain_id ~consensus_mode:true
    ~current_height:height ~active_raw:(Option.map U.to_string active)
    ~pending_raw:(Option.map U.to_string pending) config in
  let restored = bind epoch (Some safe) None |> get in
  let set = U.validator_set safe |> get in
  check "startup lost durable weights" (restored.stake_vs = set && restored.active_vs = set);
  check "startup accepted absent weights" (Result.is_error (bind epoch None None));
  check "startup accepted blocking seat" (Result.is_error (bind epoch (Some heavy) None));
  let after = Int64.succ epoch in
  let future = update after 16_000_000 in
  let waiting = bind epoch (Some safe) (Some future) |> get in
  check "startup exposed cap-bad pending set"
    (waiting.scheduled_driver_config = None && waiting.light_scheduled_validator_set = None
      && waiting.next_validator_list = []);
  check "ignored pending changed active state"
    (waiting.stake_vs = set && waiting.active_vs = set && waiting.active_update = Some safe
      && waiting.consensus_config_hash = restored.consensus_config_hash
      && waiting.allowed_pubkeys = restored.allowed_pubkeys && waiting.transition = restored.transition);
  let first = List.hd future.validators in
  let rest = List.tl future.validators in
  let malformed = [
    {future with validators = first :: future.validators};
    {future with validators = {first with weight = Z.zero} :: rest};
    {future with validators = {first with pubkey_b64 = Base64.encode_exn "short"} :: rest};
  ] in
  List.iter (fun pending -> check "startup ignored malformed pending state"
    (Result.is_error (bind epoch (Some safe) (Some pending)))) malformed;
  let next = update after 3_000_000 in
  let restored = bind epoch (Some safe) (Some next) |> get in
  let scheduled = Option.get restored.scheduled_driver_config in
  check "startup changed pending weights"
    (scheduled.activate_epoch = after && scheduled.validator_set = (U.validator_set next |> get));
  let prior = Int64.pred epoch in
  let prior_heavy = update prior 16_000_000 in
  let prior_heavy_set = U.validator_set prior_heavy |> get in
  let history = bind prior (Some prior_heavy) None |> get in
  check "startup changed pre-activation active state"
    (history.stake_vs = prior_heavy_set && history.active_vs = prior_heavy_set);
  let prior_active = update prior 2_000_000 in
  let prior_set = U.validator_set prior_active |> get in
  let before = bind prior (Some prior_active) (Some heavy) |> get in
  let scheduled = Option.get before.scheduled_driver_config in
  let heavy_set = U.validator_set heavy |> get in
  let light = Config.{activate_epoch = epoch;
    validator_set = C.validator_set_for_epoch ~chain_id ~epoch_id:epoch heavy_set} in
  let expected_hash = Config.hash ~chain_id ~validator_set:prior_set ~scheduled:light
    ?program_trust_hash:config.program_trust_hash ?runtime_profile_hash:config.runtime_profile_hash () in
  check "startup changed pre-activation pending view"
    (before.stake_vs = prior_set && before.active_vs = prior_set
      && scheduled.activate_epoch = epoch && scheduled.validator_set = heavy_set
      && before.light_scheduled_validator_set = Some light
      && before.next_validator_list = heavy_set.validators);
  check "startup changed pre-activation config hash" (before.consensus_config_hash = expected_hash)

let test_seat_checkpoint () =
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = Int64.of_int (Option.get (P.seatless_activation_for_chain chain_id)).activation_epoch in
  let members = List.filteri (fun index _ -> index < 4) keys in
  let set = C.make_weighted_validator_set (List.map (fun (member, _) -> member, Z.one) members) |> get in
  let encoded = Anchor.encoded_validator_set set |> get in
  let body = Checkpoint.{chain_id; epoch; state_root = sha "state"; ledger_state_root = sha "ledger";
    txid_hi = 0L; config_hash = sha "config"; validator_set_hash = Checkpoint.raw_to_hex (Config.validator_set_hash set);
    quorum_cert_hash = Some (sha "quorum"); epoch_index_hash = Some (sha "index");
    epoch_index_root = Some (sha "root"); created_at = 0L; valid_until = 1000L} in
  let signatures = List.filteri (fun index _ -> index < 3) members
    |> List.map (fun ((member : C.validator_info), secret) ->
      let wallet = Octra_core.Crypto.Wallet.{address = member.address;
        pub = Base64.encode_exn member.pubkey; priv = Base64.encode_exn secret} in
      Checkpoint.make_signature ~wallet body |> get)
    |> List.sort (fun (left : Checkpoint.signature) right -> String.compare left.signer right.signer) in
  ignore (Checkpoint.verify_quorum ~validator_set:encoded body signatures |> get);
  check "checkpoint accepted too few signatures"
    (Result.is_error (Checkpoint.verify_quorum ~validator_set:encoded body (List.tl signatures)));
  let broken = List.map (fun (item : Checkpoint.signature) ->
    {item with signature = Base64.encode_exn (String.make 64 '\000')}) signatures in
  check "checkpoint accepted forged signatures"
    (Result.is_error (Checkpoint.verify_quorum ~validator_set:encoded body broken))

let test_sync_shape () =
  let set = C.make_weighted_validator_set (List.map (fun (member, _) -> member, Z.one) keys) |> get in
  let encoded = Anchor.encoded_validator_set set |> get in
  check "trusted shape roundtrip differs" (Anchor.raw_validator_set encoded = Ok set);
  let first = List.hd encoded.validators in
  let faults = [
    {encoded with n = encoded.n + 1};
    {encoded with f = encoded.f + 1};
    {encoded with quorum = encoded.quorum + 1};
    {encoded with total_weight = Z.succ encoded.total_weight};
    {encoded with quorum_weight = Z.succ encoded.quorum_weight};
    {encoded with weights = encoded.weights @ ["extra", Z.one]};
    {encoded with validators = first :: encoded.validators};
    {encoded with authors = Some (30L, [first, Z.one])};
  ] in
  List.iter (fun set -> check "malformed trusted shape accepted"
    (Result.is_error (Anchor.raw_validator_set set))) faults

let large_entries count weight =
  let chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
  let places = [|1; 58; 3_364|] in
  List.init count (fun index ->
    let suffix = String.init 3 (fun offset -> chars.[(index / places.(offset)) mod 58]) in
    let pubkey = String.init 32 (fun offset -> Char.chr
      (if offset < 4 then (index lsr (offset * 8)) land 255 else offset)) in
    C.{address = "oct" ^ String.make 41 '1' ^ suffix; pubkey}, weight)

let eviction_proof_cap () =
  let count = Octra_consensus.C_codec.max_validators in
  let address = String.make Octra_consensus.C_codec.max_addr_bytes 'a' in
  let source_bytes = 5 + count * (12 + String.length address + 32 + 256) in
  check "source codec size formula differs" (U.eviction_source_limit = source_bytes);
  let epoch = `String (Int64.to_string Int64.max_int) in
  let entry = `Assoc ["address", `String address; "pubkey", `String (String.make 44 'A');
    "weight", `String (String.make 256 '9')] in
  let payload = `Assoc [
    "activate_epoch", epoch; "fingerprint", `String (String.make 64 '0');
    "validators", `List (List.init count (fun _ -> entry));
    "standard", `String Octra_core.Validator_policy.standard_name;
    "weighted", `Bool true; "source_epoch", epoch;
    "eviction", `Assoc ["epoch", epoch;
      "source", `String (String.make (4 * ((source_bytes + 2) / 3)) 'A');
      "offenders", `List (List.init count (fun _ -> `String address))];
  ] in
  let update_bytes = String.length (Yojson.Safe.to_string payload) + 2 in
  4_000_000 + 4 * ((update_bytes + 2) / 3)

let test_eviction_size () = Test_workspace.with_dir "sync-wire-size" (fun root ->
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = Int64.of_int (Option.get (P.seatless_activation_for_chain chain_id)).activation_epoch in
  let proof_cap = eviction_proof_cap () in
  let store = run (S.open_store ~fresh:true (Filename.concat root "store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
  let size_case count weight =
    let entries = large_entries count weight in
    let current = C.make_weighted_validator_set entries |> get in
    let plan, _ = U.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
      ~offenders:[(fst (List.hd entries)).C.address] current |> get in
    let raw = U.to_string plan in
    check "large eviction did not exceed legacy update limit" (String.length raw > 1_000_000);
    let next = U.validator_set plan |> get in
    let set_bytes = String.length (Octra_consensus.C_codec.encode_validator_set next) in
    if Z.numbits weight > 800 then
      check "maximum final set did not exceed legacy cap" (set_bytes > 1_000_000);
    run (S.set_meta store U.pending_meta_key raw);
    let state = proof store epoch U.pending_meta_key in
    check "large pending proof value differs" (S.verify_merkle_proof_det
      ~ledger_state_root:state.ledger_state_root ~path:["meta"; U.pending_meta_key]
      ~proof:state.proof = Ok (Some raw));
    check "large pending proof exceeds future cap" (String.length state.proof <= proof_cap);
    if Z.numbits weight > 800 then
      check "large pending proof did not exceed legacy cap" (String.length state.proof > 4_000_000);
    let index = sha "large-index" in
    let finalize = finality ~chain_id epoch state index [] in
    let step = Anchor.{source = Pending; finalize; ledger_state_root = state.ledger_state_root;
      epoch_index_root = index; update = raw; proof = state.proof} in
    let final = finality ~chain_id (Int64.succ epoch) state index [] in
    let wire step = Anchor.make ~steps:[step] ~finalize:final ~validator_set:next |> Anchor.encode in
    let unchecked_wire ?(set = next) (step : Anchor.step) =
      O.encode (fun buffer ->
        O.put_list (fun buffer (step : Anchor.step) ->
          O.put_u8 buffer (match step.source with Anchor.Pending -> 1 | Anchor.Active -> 2
            | Anchor.Relief _ -> assert false);
          O.put_bytes buffer (Octra_consensus.C_codec.encode_finalize step.finalize);
          O.put_string buffer step.ledger_state_root;
          O.put_string buffer step.epoch_index_root;
          O.put_bytes buffer step.update;
          O.put_bytes buffer step.proof) buffer [step];
        O.put_bytes buffer (Octra_consensus.C_codec.encode_finalize final);
        O.put_bytes buffer (Octra_consensus.C_codec.encode_validator_set set)) |> Base64.encode_exn in
    let refused step =
      let rejected = try ignore (wire step); false with Invalid_argument _ -> true in
      check "oversized step was serialized" rejected;
      check "oversized step was decoded" (Result.is_error (Anchor.decode (unchecked_wire step))) in
    let encoded = wire step in
    let decoded = Anchor.decode encoded |> get in
    check "future eviction wire size refused" (Anchor.encode decoded = encoded);
    check "maximum final set was not decoded" (Anchor.validator_set decoded = next);
    check "unchecked wire format differs" (unchecked_wire step = encoded);
    refused {step with update = String.make 1_000_001 'x'};
    ignore (Anchor.decode (wire {step with proof = String.make proof_cap 'x'}) |> get);
    refused {step with proof = String.make (proof_cap + 1) 'x'};
    let members = List.filteri (fun index _ -> index < 4) entries |> List.map (fun (member, weight) ->
      Octra_core.Validator_entry.{address = member.C.address; pubkey = member.pubkey; weight}) in
    let ordinary = U.make_weighted ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch) members |> get in
    let ordinary_step = {step with update = U.to_string ordinary; proof = String.make 4_000_001 'x'} in
    refused ordinary_step;
    ignore (Anchor.decode (unchecked_wire ~set:(U.validator_set ordinary |> get)
      {ordinary_step with proof = String.make 4_000_000 'x'}) |> get);
    let early_set = C.make_weighted_validator_set (List.filteri (fun index _ -> index < 4) entries) |> get in
    let early, _ = U.make_eviction ~source_epoch:(Int64.pred epoch) ~activate_epoch:epoch
      ~offenders:[(List.hd early_set.validators).address] early_set |> get in
    let early_step = {ordinary_step with update = U.to_string early;
      finalize = finality ~chain_id (Int64.pred epoch) state index []} in
    refused early_step;
    ignore (Anchor.decode (unchecked_wire ~set:(U.validator_set early |> get)
      {early_step with proof = String.make 4_000_000 'x'}) |> get);
    if set_bytes > 1_000_000 then begin
      refused {ordinary_step with proof = "unused"};
      refused {early_step with proof = "unused"}
    end in
  size_case Octra_consensus.C_codec.max_validators Z.one;
  let weight = Z.pow (Z.of_int 10) 255 in
  size_case Octra_consensus.C_codec.max_validators weight;
  let current = C.make_weighted_validator_set (large_entries 5 (Z.mul (Z.of_int 10) weight)) |> get in
  check "eviction source exceeded codec weight size" (Result.is_error
    (U.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
      ~offenders:[(List.hd current.validators).address] current))))

let test_eviction_path () = Test_workspace.with_dir "sync-eviction" (fun root ->
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = Int64.add 2L (Int64.of_int (Option.get
    (P.seatless_activation_for_chain chain_id)).activation_epoch) in
  let entries = List.mapi (fun index weight -> fst (List.nth keys index), Z.of_int weight)
    [3; 3; 2; 1; 1] in
  let current = C.make_weighted_validator_set entries |> get in
  let offender = (fst (List.nth keys 4)).C.address in
  let plan, _ = U.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
    ~offenders:[offender] current |> get in
  check "eviction witness absent" (Option.is_some plan.eviction);
  let next = U.validator_set plan |> get in
  check "eviction cap contrast absent" (C.validate_validator_set_for_epoch ~chain_id
    ~epoch_id:(Int64.succ epoch) next = Error C.Seat_cap);
  let authored = {current with authors = Some (epoch, [fst (List.nth keys 5), Z.one])} in
  let trusted = Anchor.encoded_validator_set authored |> get in
  let store = run (S.open_store ~fresh:true (Filename.concat root "store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let assemble ?(source = Anchor.Pending) ?(current = authored) plan =
      let at = match source with Anchor.Active -> Int64.succ epoch | _ -> epoch in
      let key = match source with Anchor.Active -> U.active_meta_key | _ -> U.pending_meta_key in
      let raw = U.to_string plan in
      run (S.set_meta store key raw);
      let state = proof store at key in
      let index = sha "eviction-index" in
      let signers set = List.filter (fun (member, _) -> C.is_validator set member.C.address) keys in
      let finalize = finality ~chain_id at state index (signers current) in
      let step = Anchor.{source; finalize; ledger_state_root = state.ledger_state_root;
        epoch_index_root = index; update = raw; proof = state.proof} in
      let set = U.validator_set plan |> get in
      let final = finality ~chain_id (Int64.add at 2L) state index (signers set) in
      let cp = { (checkpoint final state index set) with Checkpoint.chain_id = chain_id } in
      step, final, set, cp, state in
    let check_path ?(trusted = trusted) (step, final, set, cp, _) =
      let anchor = Anchor.make ~steps:[step] ~finalize:final ~validator_set:set in
      let wire = Anchor.encode anchor in
      let sync = Anchor.verify ~validator_set:trusted cp wire in
      let async = run (Anchor.verify_lwt ~validator_set:trusted cp wire) in
      check "eviction async verdict differs"
        (Result.map Anchor.encode sync = Result.map Anchor.encode async);
      Result.map (fun checked ->
        check "eviction proof bytes changed" (Anchor.encode checked = wire);
        check "eviction trusted stake differs"
          (snd (Anchor.transition_base checked |> get) = set);
        let compact = run (Anchor.compact_lwt ~validator_set:trusted checked) |> get in
        if step.source = Anchor.Pending && Option.is_some (U.of_string step.update |> get).eviction then
          check "eviction source link was omitted" (Anchor.steps compact = [step]);
        ignore (Anchor.verify ~validator_set:trusted cp (Anchor.encode compact) |> get)) sync in
    let reject reason bundle =
      match check_path bundle with
      | Error found -> check ("eviction rejection differs found = " ^ found) (found = reason)
      | Ok () -> failwith ("invalid eviction accepted reason = " ^ reason) in
    let reject_proof bundle = match check_path bundle with
      | Error reason -> check ("proof rejection differs found = " ^ reason)
        (reason = "validator transition state proof mismatch"
          || String.starts_with ~prefix:"proof mismatch:" reason
          || String.starts_with ~prefix:"proof root mismatch:" reason)
      | Ok () -> failwith "unproved eviction state accepted" in
    let base = assemble plan in
    ignore (check_path base |> get);
    let step, final, set, cp, state = base in
    let with_step step = step, final, set, cp, state in
    let missing = run (S.merkle_proof_at_epoch store (Int64.to_int epoch)
      ["meta"; U.active_meta_key]) |> get in
    reject_proof (with_step {step with proof = missing.proof});
    let other = C.make_weighted_validator_set (List.map (fun (member, weight) ->
      member, Z.mul (Z.of_int 2) weight) entries) |> get in
    let altered, _ = U.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
      ~offenders:[offender] other |> get in
    reject "validator eviction source set differs" (assemble altered);
    let other = C.make_weighted_validator_set (List.mapi (fun index (member, weight) ->
      (if index = 0 then {member with C.pubkey = (fst (List.nth keys 6)).pubkey} else member), weight)
      entries) |> get in
    let altered, _ = U.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
      ~offenders:[offender] other |> get in
    reject "validator eviction source set differs" (assemble altered);
    let old, _ = U.make_eviction ~source_epoch:(Int64.pred epoch) ~activate_epoch:epoch
      ~offenders:[offender] current |> get in
    reject "validator transition activation epoch mismatch" (assemble old);
    let _, _, _, _, foreign = assemble old in
    reject_proof (with_step {step with proof = foreign.proof});
    reject "validator transition folded root mismatch"
      (with_step {step with ledger_state_root = String.make 64 '0'});
    let ordinary set = U.make_weighted ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
      (C.weights_of_set set |> List.map (fun (member, weight) ->
        Octra_core.Validator_entry.{address = member.C.address; pubkey = member.pubkey; weight})) |> get in
    let changed = C.make_weighted_validator_set (List.mapi (fun index (member, weight) ->
      member, if index = 0 then Z.succ weight else weight) (C.weights_of_set next)) |> get in
    let changed = { (ordinary changed) with U.eviction = plan.eviction } in
    reject "validator eviction differs from exact removal" (assemble changed);
    reject "validator weight reaches blocking threshold" (assemble (ordinary next));
    ignore (check_path (assemble (ordinary current)) |> get);
    reject "validator transition state proof mismatch"
      (with_step {step with update = U.to_string (ordinary current)});
    reject "validator weight reaches blocking threshold"
      (assemble ~source:Anchor.Active (ordinary next));
    reject "active validator eviction is not trusted"
      (assemble ~source:Anchor.Active plan);
    let next_trusted = Anchor.encoded_validator_set next |> get in
    let active = assemble ~source:Anchor.Active ~current:next plan in
    ignore (check_path ~trusted:next_trusted active |> get);
    let _, trusted_final, trusted_set, trusted_cp, _ = active in
    let empty = Anchor.make ~steps:[] ~finalize:trusted_final ~validator_set:trusted_set |> Anchor.encode in
    ignore (Anchor.verify ~validator_set:next_trusted trusted_cp empty |> get);
    ignore (run (Anchor.verify_lwt ~validator_set:next_trusted trusted_cp empty) |> get);
    check "empty path accepted an untrusted heavy set" (Result.is_error
      (Anchor.verify ~validator_set:trusted trusted_cp empty));
    let bad_qc = C.{step.finalize with precommits = List.map (fun (vote : C.vote) ->
      {vote with signature = String.make 64 '\000'}) step.finalize.precommits} in
    reject "finality qc signature" (with_step {step with finalize = bad_qc});
    let future = P.for_chain chain_id |> fun policy ->
      P.with_seatless_epoch policy (Some (Int64.succ epoch)) in
    let anchor = Anchor.make ~steps:[step] ~finalize:final ~validator_set:set in
    check "eviction accepted before activation"
      (Anchor.verify_path ~policy:future ~validator_set:trusted (Anchor.encode anchor)
        = Error "validator eviction source epoch is invalid")))

let test_eviction_keep () = Test_workspace.with_dir "eviction-keep" (fun root ->
  let module Chain = Octra_node_runtime.Sync_chain in
  let module J = Octra_node_runtime.Consensus_finality_journal in
  let module Store = Octra_core.Store_chaindata in
  let module M = Octra_bootstrap.State_sync_manifest in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = Int64.add 2L (Int64.of_int (Option.get
    (P.seatless_activation_for_chain chain_id)).activation_epoch) in
  let current = C.make_weighted_validator_set (List.mapi (fun index weight ->
    fst (List.nth keys index), Z.of_int weight) [3; 3; 2; 1; 1]) |> get in
  let trusted = Anchor.encoded_validator_set current |> get in
  let plan, _ = U.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
    ~offenders:[(fst (List.nth keys 4)).address] current |> get in
  let next = U.validator_set plan |> get in
  let store = run (S.open_store ~fresh:true (Filename.concat root "store")) in
  let chaindata = Store.open_chaindata (Filename.concat root "chain") in
  Fun.protect ~finally:(fun () -> run (S.close store); Store.close chaindata) (fun () ->
    let signers set = List.filter (fun (member, _) -> C.is_validator set member.C.address) keys in
    let bundle = J.{tx_hashes = []; txs = []; receipts_json = []} in
    let save set final =
      J.persist_certificate root ~validator_set:set final;
      J.persist_bundle root final bundle;
      J.promote_applied root ~epoch:final.C.epoch_id ~state_root:final.header.proposed_state_root in
    run (S.set_meta store U.pending_meta_key (U.to_string plan));
    let state = proof store epoch U.pending_meta_key in
    let index = sha "retained-index" in
    let signed set at state = finality ~chain_id
      ~tx_list_hash:(Octra_consensus.C_engine.tx_list_hash_for_header bundle.tx_hashes)
      at state index (signers set) in
    let parent = signed current epoch state in
    save current parent;
    Store.set_epoch_index_commitment_direct chaindata ~epoch_id:(Int64.to_int epoch)
      ~epoch_hash:(sha "retained-entry") ~root:index;
    let deps = Chain.{data_dir = root; chain_id; store; chaindata;
      read_finality = (fun epoch -> J.read_committed_epoch ~chain_id ~epoch root);
      certificate_path = (fun () -> Filename.concat root "missing.json")} in
    let first = signed next (Int64.succ epoch) state in
    let refuse_source validator_set finalize label =
      let bad = {deps with read_finality = (fun _ -> J.Valid {
        finalize; validator_set; bundle = None})} in
      check label (Result.is_error (run (Chain.keep_relief bad next first))) in
    refuse_source next parent "retention trusted the target as source";
    let forged = {parent with C.precommits = List.map (fun (vote : C.vote) ->
      {vote with signature = String.make 64 '\000'}) parent.precommits} in
    refuse_source current forged "retention accepted forged source QC";
    check "retention accepted a different target"
      (Result.is_error (run (Chain.keep_relief deps current first)));
    check "retention accepted another epoch"
      (Result.is_error (run (Chain.keep_relief deps next
        (signed next (Int64.add epoch 2L) state))));
    ignore (run (Chain.keep_relief deps next first) |> get);
    let path = Filename.concat (Filename.concat root "relief_steps")
      (Printf.sprintf "%020Ld.proof" first.epoch_id) in
    check "pending eviction was not retained" (Sys.file_exists path);
    let saved = Yojson.Safe.from_file path in
    let check_damage steps =
      let step = List.hd steps in
      let broken = Anchor.make ~steps:[{step with Anchor.proof = "invalid"}]
        ~finalize:first ~validator_set:next |> Anchor.encode in
      let damaged = match saved with `Assoc fields ->
        `Assoc (List.map (fun (key, value) -> key,
          if key = "proof" then `String broken else value) fields)
        | _ -> failwith "retained proof is not an object" in
      M.write_json path damaged;
      check "retention accepted damaged state proof"
        (Result.is_error (run (Chain.build deps ~head_epoch:first.epoch_id trusted next)));
      M.write_json path saved in
    let cold = {deps with read_finality = (fun _ -> J.Missing)} in
    ignore (run (Chain.keep_relief cold next first) |> get);
    check "retention replay changed proof" (Yojson.Safe.from_file path = saved);
    check "retention accepted another target"
      (Result.is_error (run (Chain.keep_relief cold current first)));
    run (S.set_meta store U.active_meta_key (U.to_string plan));
    run (S.remove_path store ["meta"; U.pending_meta_key]);
    let expired = Int64.add epoch J.history_limit in
    save next (signed next expired state);
    check "retention test did not expire journal"
      (J.read_committed_epoch ~chain_id ~epoch root = J.Missing);
    let height = Int64.add epoch (Int64.of_int (S.gc_keep_epochs + 2)) in
    let tip = proof store height U.active_meta_key in
    run (S.cleanup_old_tags store (Int64.to_int height));
    check "retention test did not remove source tag"
      (run (S.read_at_epoch store (Int64.to_int epoch) ["meta"; U.pending_meta_key]) = None);
    let steps = run (Chain.build deps ~head_epoch:height trusted next) |> get in
    check "retained eviction lost Pending proof" (match steps with
      | [{Anchor.source = Pending; finalize; update; _}] ->
        finalize = parent && update = U.to_string plan
      | _ -> false);
    let final = signed next height tip in
    let cp = {(checkpoint final tip index next) with Checkpoint.chain_id = chain_id} in
    let anchor = Anchor.make ~steps ~finalize:final ~validator_set:next in
    ignore (Anchor.verify ~validator_set:trusted cp (Anchor.encode anchor) |> get);
    let other = C.make_weighted_validator_set (List.map (fun (member, weight) ->
      member, Z.mul (Z.of_int 2) weight) (C.weights_of_set current)) |> get
      |> Anchor.encoded_validator_set |> get in
    check "local proof bypassed trusted prefix"
      (Result.is_error (run (Chain.build deps ~head_epoch:height other next)));
    check_damage steps;
    ignore (run (Chain.build deps ~head_epoch:height trusted next) |> get)))

let test_import_eviction () = Test_workspace.with_dir "slash-reference" (fun root ->
  let module M = Octra_bootstrap.State_sync_manifest in
  let module Client = Octra_bootstrap.State_sync_client in
  let module Head = Octra_core.Head_manifest in
  let module Store = Octra_core.Store_chaindata in
  let module Registry = Octra_core.Validator_registry in
  let module Exec = Octra_core.Epoch_exec in
  let module Profile = Octra_node_runtime.Consensus_profile in
  let module Parent = Octra_node_runtime.Consensus_parent_commit in
  let module Journal = Octra_node_runtime.Consensus_finality_journal in
  let module Shell = Octra_node_runtime.Startup_p2p_shell in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = Int64.add 2L (Int64.of_int (Option.get
    (P.seatless_activation_for_chain chain_id)).activation_epoch) in
  let signers = List.filteri (fun index _ -> index < 5) keys in
  let members = List.map2 (fun ((member : C.validator_info), _) weight ->
    Octra_core.Validator_entry.{address = member.address; pubkey = member.pubkey; weight = Z.of_int weight})
    signers [3; 3; 2; 1; 1] in
  let active = U.make_weighted ~source_epoch:(Int64.sub epoch 2L)
    ~activate_epoch:(Int64.pred epoch) members |> get in
  let current = U.validator_set active |> get in
  let trusted = Anchor.encoded_validator_set current |> get in
  let offender = (List.nth members 4).address in
  let pending, _ = U.make_eviction ~source_epoch:epoch ~activate_epoch:(Int64.succ epoch)
    ~offenders:[offender] current |> get in
  let next = U.validator_set pending |> get in
  let reference = Filename.concat root "reference" in
  let imported = Filename.concat root "imported" in
  Unix.mkdir reference 0o700;
  Unix.mkdir imported 0o700;
  let source = run (S.open_store ~fresh:true (Filename.concat root "source")) in
  Fun.protect ~finally:(fun () -> run (S.close source)) (fun () ->
    run (S.set_meta source U.active_meta_key (U.to_string active));
    run (S.set_meta source U.pending_meta_key (U.to_string pending));
    let registry = match Registry.to_yojson Registry.empty with
      | `Assoc fields -> Registry.of_yojson (`Assoc (List.remove_assoc "slashes" fields @ [
        "slashes", `List [`Assoc ["id", `String (Digestif.SHA256.(digest_string "import-slash"
          |> to_raw_string |> Base64.encode_exn)); "offender", `String offender;
          "evidence_epoch", `String (Int64.to_string (Int64.pred epoch));
          "applied_epoch", `String (Int64.to_string epoch); "amount", `String "1"]]])) |> get
      | _ -> assert false in
    run (S.set_meta source Registry.meta_key (Registry.to_string registry));
    let getenv = function "OCTRA_BFT_ADMIT_OPS" -> Some "transfer" | _ -> None in
    let execution_profile ~chain_id ~epoch = Profile.execution_profile ~chain_id ~epoch getenv in
    check "import profile test has no environment difference"
      (execution_profile ~chain_id ~epoch:(Int64.to_int epoch)
        <> Profile.execution_profile ~chain_id ~epoch:(Int64.to_int epoch) (fun _ -> None));
    let marker = Exec.execution_marker_to_string Exec.{epoch = Int64.to_int epoch;
      profile = Option.get (execution_profile ~chain_id ~epoch:(Int64.to_int epoch))} |> get in
    run (S.set_meta source Exec.execution_meta_key marker);
    run (S.set_meta source "last_epoch" (Int64.to_string epoch));
    run (S.set_meta source "current_epoch" (Int64.to_string (Int64.succ epoch)));
    let state = proof source epoch U.pending_meta_key in
    let index = sha "import-index" in
    let final = finality ~chain_id epoch state index signers in
    let cp = {(checkpoint final state index current) with Checkpoint.chain_id = chain_id} in
    let head = Head.{schema_version; generation = 1; epoch_id = Int64.to_int epoch;
      state_root = cp.state_root; ledger_state_root = Some cp.ledger_state_root; irmin_commit = None;
      txid_hi = cp.txid_hi; txlog_seg = None; txlog_off = None; epochlog_off = None;
      commit_id = "slash-reference"; ts = 1.; quorum_cert_hash = cp.quorum_cert_hash;
      epoch_index_hash = cp.epoch_index_hash; epoch_index_root = cp.epoch_index_root} in
    Head.atomic_write reference head;
    Head.atomic_write imported head;
    let write path content = Out_channel.with_open_bin path (fun channel -> output_string channel content) in
    List.iter (fun directory ->
      write (Filename.concat directory "state_root") cp.state_root;
      write (Filename.concat directory Octra_bootstrap.Root_win.name)
        (Octra_bootstrap.Root_win.encode [])) [reference; imported];
    let image = Filename.concat reference "ledger.dat" in
    let commit = run (S.get_commit_hash source) |> Option.get in
    ignore (run (Image.write source ~commit ~path:image) |> get);
    let draft = M.build ~checkpoint:cp ~source_dir:reference ~chunk_size:M.chunk_size_min |> get in
    let signatures = List.map (fun ((member : C.validator_info), secret) ->
      let wallet = Octra_core.Crypto.Wallet.{address = member.address;
        pub = Base64.encode_exn member.pubkey; priv = Base64.encode_exn secret} in
      M.make_exporter_signature ~wallet draft.manifest |> get) signers
      |> List.sort (fun (left : Checkpoint.signature) right -> String.compare left.signer right.signer) in
    let certificate = M.{checkpoint = cp; checkpoint_hash = draft.checkpoint_hash;
      manifest = draft.manifest; manifest_hash = draft.manifest_hash;
      authority = Finalized (Anchor.make ~steps:[] ~finalize:final ~validator_set:current |> Anchor.encode);
      exporter_signatures = signatures} in
    ignore (M.verify_reference_certificate ~validator_set:trusted ~exporter_set:trusted certificate |> get);
    ignore (run (Image.restore ~source:image ~target:(Filename.concat imported "irmin_store")
      ~expected_root:cp.ledger_state_root) |> get);
    ignore (run (Client.seed_state certificate imported) |> get);
    M.write_json (Octra_bootstrap.State_sync.anchor_path imported) (M.certificate_json certificate);
    ignore (run (Octra_bootstrap.State_sync_verify.verify cp imported) |> get);
    let restored = run (S.open_store (Filename.concat imported "irmin_store")) in
    let chaindata = Store.open_chaindata (Filename.concat imported "chaindata") in
    Fun.protect ~finally:(fun () -> run (S.close restored); Store.close chaindata) (fun () ->
      let floor = Store.history_floor chaindata |> get |> Option.get in
      let parent = Parent.create ~chain_id ~data_dir:imported ~chaindata (fun _ -> None) |> get in
      let head = Head.load imported |> Option.get in
      ignore (run (Profile.check_execution_parent ~chain_id ~epoch:(head.epoch_id + 1)
        ~store:restored ~head:(Some head) getenv) |> get);
      check "startup accepted the recovery environment"
        (run (Profile.check_execution_parent ~chain_id ~epoch:(head.epoch_id + 1)
          ~store:restored ~head:(Some head) (fun _ -> None))
         = Error "execution profile hash differs");
      let missing () = Journal.read_at_head ~chain_id ~epoch ~state_root:head.state_root
        ~txid_hi:head.txid_hi imported = Journal.Missing && not (Journal.pending imported) in
      check "reference import unexpectedly seeded journal" (missing ());
      let resolved = ref [] in
      let resolve at = resolved := at :: !resolved; Parent.set_at_head parent ~epoch_id:at head in
      let read key () = run (S.get_meta restored key) in
      let member, secret = List.hd signers in
      let runtime = Shell.{getenv = (fun _ -> None); info = ignore; warn = ignore; fatal = failwith;
        read_active_validator_meta = read U.active_meta_key;
        read_pending_validator_meta = read U.pending_meta_key;
        read_head_hash = (fun () -> Some final.header.proposed_state_root); root_of_head_hash = Fun.id;
        install = {set_consensus_config_hash = ignore; set_consensus_validator_set = ignore;
          set_scheduled_validator_set = ignore; set_swarm = ignore};
        activation_epoch = (fun () -> None); chain_id; consensus_mode = true; consensus_port = 0;
        consensus_peers = []; address = member.address; priv_b64 = Base64.encode_exn secret;
        pub_b64 = Base64.encode_exn member.pubkey; voting = false; role_label = "observer";
        read_persistent_pending = (fun () -> S.get_meta restored U.pending_meta_key);
        current_height = (fun () -> epoch)} in
      let started = Shell.start_node ~eviction_source:resolve runtime in
      check "reference startup changed source voters"
        (started.view.active_vs = current && started.view.stake_vs = current);
      let scheduled = Option.get started.view.scheduled_validator_set_config in
      check "reference startup lost eviction target"
        (scheduled.activate_epoch = Int64.succ epoch && scheduled.validator_set = next
          && scheduled.eviction = pending.eviction);
      let loaded = run (started.load_scheduled_validator_set_config ()) |> Option.get in
      check "reference loader lost authenticated eviction" (loaded = scheduled);
      check "reference startup bypassed source resolver"
        (!resolved <> [] && List.for_all (Int64.equal epoch) !resolved);
      check "reference startup seeded journal before binder" (missing ());
      check "reference startup changed committed state"
        (run (S.get_head_hash restored) = Some cp.ledger_state_root
          && run (S.get_meta restored U.pending_meta_key) = Some (U.to_string pending)
          && Store.history_floor chaindata = Ok (Some floor)))))

let () = test_eviction_keep (); test_import_eviction (); test_sync_shape (); test_eviction_size (); test_eviction_path (); test_seat_checkpoint (); test_seat_start ();
  test_same_set (); test_restore (); test_profile (); print_endline "status = pass test = stake_sync"