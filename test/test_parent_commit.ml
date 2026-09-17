(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module C_hash = Octra_consensus.C_hash
module C_engine = Octra_consensus.C_engine
module C_config = Octra_consensus.C_config
module Parent = Octra_consensus.C_parent_commit
module Reward = Octra_node_runtime.Consensus_reward_attribution

let devnet = "octra-devnet-9871-cluster"
let standard_epoch = 1_500_000L

let () =
  Mirage_crypto_rng_unix.use_default ()

let fail message =
  failwith ("test_parent_commit: " ^ message)

let expect label condition =
  if not condition then fail label

let key address weight =
  let private_key, public_key = Mirage_crypto_ec.Ed25519.generate () in
  C_types.{
    address;
    pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets public_key;
  },
  private_key,
  Z.of_int weight

let keys = [
  key "octA" 4;
  key "octB" 3;
  key "octC" 2;
  key "octD" 1;
]

let validator_set =
  match
    C_types.make_weighted_validator_set
      (List.map
         (fun (validator, _, weight) -> validator, weight)
         keys)
  with
  | Ok value -> value
  | Error error -> fail error

let sign address message =
  let _, private_key, _ =
    List.find
      (fun (validator, _, _) -> validator.C_types.address = address)
      keys
  in
  Mirage_crypto_ec.Ed25519.sign ~key:private_key message

let header =
  C_types.{
    proto_version = proto_version_current;
    chain_id = "parent-test";
    epoch_id = 17L;
    prev_state_root = String.make 32 '\x11';
    tx_list_hash = C_engine.tx_list_hash_for_header [];
    receipt_root = C_hash.receipt_root [];
    proposed_state_root = String.make 32 '\x22';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "octA";
    txid_hi = 50L;
    ts = 1.;
  }

let vote ?(epoch_id = 17L) proposal_id address =
  let unsigned = C_types.{
    chain_id = "parent-test";
    epoch_id;
    round = 2;
    vote_type = Precommit;
    proposal_id;
    validator = address;
    signature = String.make 64 '\x00';
  } in
  {
    unsigned with
    signature = sign address (C_hash.vote_sign_bytes unsigned);
  }

let parent_for_header header addresses =
  let proposal_id = C_hash.proposal_id header in
  C_types.{
    validator_set;
    certificate = C_types.{
      chain_id = "parent-test";
      epoch_id = header.epoch_id;
      commit_round = 2;
      header;
      proposal_id;
      precommits =
        List.map
          (vote ~epoch_id:header.epoch_id proposal_id)
          addresses;
    };
  }

let parent addresses =
  parent_for_header header addresses

let expected (value : C_types.parent_commit) =
  Parent.{
    chain_id = "parent-test";
    epoch_id = 17L;
    proposal_id = value.certificate.proposal_id;
    state_root = header.proposed_state_root;
    validator_set_hash = C_config.validator_set_hash validator_set;
  }

let test_roundtrip () =
  let value = parent ["octB"; "octA"; "octC"] in
  let encoded = Parent.encode value in
  let decoded = Parent.decode encoded in
  expect "roundtrip hash" (Parent.hash decoded = Parent.hash value);
  expect "canonical participants"
    (Parent.participants decoded = ["octA"; "octB"; "octC"]);
  expect "signed weight"
    (Parent.signed_weight decoded = Some (Z.of_int 9));
  expect "canonical wire"
    (Parent.encode (parent ["octA"; "octB"; "octC"]) = encoded);
  expect "trailing bytes rejected"
    (try
       ignore (Parent.decode (encoded ^ "\x00"));
       false
     with _ -> true)

let test_validation () =
  let value = parent ["octA"; "octB"; "octC"] in
  expect "valid parent"
    (Parent.validate (expected value) value = Parent.Valid);
  let bad_expected =
    { (expected value) with Parent.validator_set_hash = String.make 32 '\x44' }
  in
  expect "set mismatch"
    (Parent.validate bad_expected value = Parent.Invalid "validator_set");
  let bad_votes =
    match value.certificate.precommits with
    | first :: rest ->
      { first with C_types.signature = String.make 64 '\x55' } :: rest
    | [] -> []
  in
  let invalid =
    {
      value with
      certificate =
        { value.certificate with precommits = bad_votes };
    }
  in
  expect "signature mismatch"
    (Parent.validate (expected invalid) invalid = Parent.Invalid "qc_signature")

let test_same_block () =
  let left = parent ["octA"; "octB"; "octC"] in
  let right = parent ["octA"; "octC"; "octD"] in
  expect "same block" (Parent.same_block left right);
  expect "different qc hash" (Parent.hash left <> Parent.hash right)

let test_reward_attribution () =
  let value = parent ["octB"; "octA"; "octC"] in
  Unix.putenv "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH" "0";
  let reward =
    Reward.resolve_for_epoch
      ~chain_id:"parent-test"
      ~epoch_id:18L
      ~proposer_addr:"octCurrent"
      ~validator_pubkeys:["octCurrent", "current-key"]
      (Some value)
    |> Result.get_ok
  in
  expect "reward parent proposer"
    (reward.Reward.proposer_addr = "octA");
  expect "reward parent proposer key"
    (reward.proposer_public_key
     = Some
         (Base64.encode_exn
            (List.hd validator_set.validators).C_types.pubkey));
  expect "reward parent participants"
    (List.map
       (fun (validator : Octra_core.Epoch_exec.reward_validator) ->
         validator.address, validator.weight)
       reward.validators
     = ["octA", Z.of_int 4; "octB", Z.of_int 3; "octC", Z.of_int 2]);
  expect "non-signer receives no finalized reward weight"
    (not
       (List.exists
          (fun (validator : Octra_core.Epoch_exec.reward_validator) ->
            String.equal validator.address "octD")
          reward.validators));
  expect "current reward source requires parent commit"
    (Reward.resolve_for_epoch
       ~chain_id:devnet
       ~epoch_id:standard_epoch
       ~proposer_addr:"octCurrent"
       ~validator_pubkeys:["octCurrent", "current-key"]
       None
     = Error "current reward parent commit is missing");
  let current_finalize = C_types.{
    chain_id = devnet;
    epoch_id = standard_epoch;
    commit_round = value.certificate.commit_round;
    header = value.certificate.header;
    proposal_id = value.certificate.proposal_id;
    precommits = value.certificate.precommits;
    parent_commit = None;
  } in
  expect "current finality reward requires parent commit"
    (Reward.bind_finality
       ~validator_set
       current_finalize
       (Reward.full_set
          ~proposer_addr:"octCurrent"
          ~validator_pubkeys:["octCurrent", "current-key"])
     = Error "current reward parent commit is missing");
  let genesis =
    Reward.resolve_for_epoch
      ~chain_id:devnet
      ~epoch_id:0L
      ~proposer_addr:"octCurrent"
      ~validator_pubkeys:["octCurrent", "current-key"]
      None
    |> Result.get_ok
  in
  expect "genesis reward proposer"
    (genesis.proposer_addr = "octCurrent");
  expect "genesis reward participant"
    (List.map
       (fun (validator : Octra_core.Epoch_exec.reward_validator) ->
         validator.address, validator.weight)
       genesis.validators
     = ["octCurrent", Z.one]);
  Unix.putenv "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH" "100";
  let legacy =
    Reward.resolve_for_epoch
      ~chain_id:"parent-test"
      ~epoch_id:99L
      ~proposer_addr:"octCurrent"
      ~validator_pubkeys:["octCurrent", "current-key"]
      None
    |> Result.get_ok
  in
  expect "legacy reward full set"
    (List.map
       (fun (validator : Octra_core.Epoch_exec.reward_validator) ->
         validator.address, validator.weight)
       legacy.validators
     = ["octCurrent", Z.one])

let test_reward_cutover () =
  Unix.putenv "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH" "0";
  let reward =
    Reward.resolve_for_epoch
      ~chain_id:devnet
      ~epoch_id:(Int64.pred standard_epoch)
      ~proposer_addr:"octPrior"
      ~validator_pubkeys:["octPrior", "prior-key"]
      None
    |> Result.get_ok
  in
  expect "prior reward source remains accepted"
    (List.map
       (fun (validator : Octra_core.Epoch_exec.reward_validator) ->
         validator.address, validator.weight)
       reward.validators
     = ["octPrior", Z.one]);
  expect "active reward source requires parent commit"
    (Reward.resolve_for_epoch
       ~chain_id:devnet
       ~epoch_id:standard_epoch
       ~proposer_addr:"octActive"
       ~validator_pubkeys:["octActive", "active-key"]
       None
     = Error "current reward parent commit is missing")

let test_epoch_reward_source () =
  Unix.putenv "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH" "100";
  let validator_pubkeys =
    List.map
      (fun validator ->
        validator.C_types.address,
        Base64.encode_exn validator.pubkey)
      validator_set.validators
  in
  let legacy = {
    Octra_core.Epochlog.empty_epoch_header with
    id = 99;
    finalized_by = "octA";
    proposer = {
      Octra_core.Epochlog.creator_addr = "octA";
      commit_round = 0;
    };
  } in
  let legacy_source =
    Reward.epoch_source
      ~validator_activation_epoch:(Some 200)
      ~validator_pubkeys
      legacy
    |> Result.get_ok
  in
  expect "legacy reward source full set"
    (List.map
       (fun member -> member.C_types.reward_address)
       legacy_source.reward_members
     = ["octA"; "octB"; "octC"; "octD"]);
  let current = {
    legacy with
    id = 100;
    reward_recipients = [
      {
        Octra_core.Epochlog.reward_addr = "octA";
        reward_role = "proposer_validator";
        reward_amount = "1";
      };
      {
        Octra_core.Epochlog.reward_addr = "octB";
        reward_role = "validator";
        reward_amount = "1";
      };
    ];
  } in
  let current_source =
    Reward.epoch_source
      ~validator_activation_epoch:(Some 200)
      ~validator_pubkeys
      current
    |> Result.get_ok
  in
  expect "current reward source exact participants"
    (List.map
       (fun member -> member.C_types.reward_address)
       current_source.reward_members
     = ["octA"; "octB"]);
  let weighted = { current with id = 200; reward_source = None } in
  expect "weighted source missing fails closed"
    (Reward.epoch_source
       ~validator_activation_epoch:(Some 200)
       ~validator_pubkeys
       weighted
     = Error "weighted reward source is missing");
  let persisted = { weighted with reward_source = Some current_source } in
  expect "weighted persisted source accepted"
    (Reward.epoch_source
       ~validator_activation_epoch:(Some 200)
       ~validator_pubkeys
       persisted
     = Ok current_source)

let test_legacy_parent () =
  Unix.putenv "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH" "0";
  let legacy_header = {
    header with
    C_types.proto_version = C_types.proto_version_parent_legacy;
  } in
  let value = parent_for_header legacy_header ["octA"; "octB"; "octC"] in
  let encoded = Parent.encode value in
  let decoded = Parent.decode encoded in
  expect "legacy parent roundtrip"
    (Parent.hash decoded = Parent.hash value);
  expect "legacy parent valid"
    (Parent.validate (expected value) value = Parent.Valid);
  let legacy_finalize = C_types.{
    chain_id = value.certificate.chain_id;
    epoch_id = value.certificate.epoch_id;
    commit_round = value.certificate.commit_round;
    header = value.certificate.header;
    proposal_id = value.certificate.proposal_id;
    precommits = value.certificate.precommits;
    parent_commit = None;
  } in
  let decoded_finalize =
    legacy_finalize
    |> Octra_consensus.C_codec.encode_finalize
    |> Octra_consensus.C_codec.decode_finalize
  in
  expect "legacy finalize roundtrip"
    (decoded_finalize = legacy_finalize);
  let verify_vote (vote : C_types.vote) =
    match C_types.pubkey_of_addr validator_set vote.validator with
    | None -> false
    | Some pubkey -> C_hash.verify_vote ~pubkey_raw:pubkey vote
  in
  expect "legacy live finalize rejected"
    (Octra_consensus.C_qc.validate_finalize
       ~chain_id:"parent-test"
       ~validator_set
       ~verify_vote
       legacy_finalize
     = Octra_consensus.C_qc.Invalid "header_proto_version")

let test_parent_tail () =
  let module Log = Octra_consensus.Finality_log in
  let module Journal = Octra_node_runtime.Consensus_finality_journal in
  let module Source = Octra_node_runtime.Consensus_parent_commit in
  let module Store = Octra_core.Store_chaindata in
  let base = Test_workspace.unique_dir "parent_tail" in
  let chaindata = Store.open_chaindata (Filename.concat base "chaindata") in
  Fun.protect
    ~finally:(fun () -> Store.close chaindata)
    (fun () ->
      let value =
        parent_for_header { header with epoch_id = 8192L }
          ["octA"; "octB"; "octC"]
      in
      let proof = value.certificate in
      let finalize = C_types.{
        chain_id = proof.chain_id;
        epoch_id = proof.epoch_id;
        commit_round = proof.commit_round;
        header = proof.header;
        proposal_id = proof.proposal_id;
        precommits = proof.precommits;
        parent_commit = None;
      } in
      Journal.persist_certificate base ~validator_set finalize;
      Journal.persist_bundle base finalize
        Journal.{ tx_hashes = []; txs = []; receipts_json = [] };
      Journal.promote base;
      let entry = Log.of_finalize finalize in
      Log.replace base
        (List.init 8192 (fun index -> { entry with height = index + 1 }));
      expect "tail matches full read"
        (Log.last_entry_fast base = Log.last base);
      let source =
        Source.create ~chain_id:"parent-test" ~data_dir:base ~chaindata
          (function
            | "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH" -> Some "0"
            | "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH" -> Some "99"
            | _ -> None)
        |> Result.get_ok
      in
      let before = Gc.allocated_bytes () in
      for _ = 1 to 8 do
        expect "verified parent from tail"
          (Source.verify source ~epoch_id:8193L (Some value) = Ok ())
      done;
      let allocated = Gc.allocated_bytes () -. before in
      expect "parent read allocation independent of history"
        (allocated < 16. *. 1024. *. 1024.);
      expect "parent ahead refused"
        (Source.load source ~epoch_id:8192L
         = Error "parent finality entry is ahead");
      expect "parent behind refused"
        (Source.load source ~epoch_id:8194L
         = Error "parent finality entry is behind");
      expect "missing parent refused"
        (Result.is_error (Source.verify source ~epoch_id:8193L None));
      let output =
        open_out_gen [Open_wronly; Open_append; Open_binary] 0o600 (Log.path base)
      in
      Fun.protect ~finally:(fun () -> close_out output)
        (fun () -> output_string output "{\"height\":");
      expect "incomplete tail refused"
        (try ignore (Source.load source ~epoch_id:8193L); false with _ -> true);
      Printf.printf "event = parent_tail reads = 8 allocated_bytes = %.0f\n" allocated)

let () =
  test_roundtrip ();
  test_validation ();
  test_same_block ();
  test_reward_attribution ();
  test_reward_cutover ();
  test_epoch_reward_source ();
  test_legacy_parent ();
  test_parent_tail ();
  print_endline "status = pass test = parent_commit"