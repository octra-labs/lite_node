(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Resource_attestations = Octra_consensus.Resource_attestations

let fail name =
  failwith ("resource attestation model failed = " ^ name)

let assert_true name value =
  if not value then fail name

let check_qc_intersection () =
  for total = 4 to 1000 do
    let total_weight = Z.of_int total in
    let max_byzantine = Z.of_int ((total - 1) / 3) in
    assert_true "qc intersection"
      (Resource_attestations.conflicting_qc_impossible ~total_weight ~byzantine_weight:max_byzantine)
  done

let hash char =
  String.make 32 char

let signature char =
  String.make 64 char

let attestation ~node_id ~kind ~weight ~score_char ~signature_char =
  Resource_attestations.{
    chain_id = "octra-resource-attestation-test";
    epoch_id = 42L;
    node_id;
    kind;
    commitment = "commitment:" ^ node_id;
    proof_hash = hash score_char;
    weight;
    score = hash score_char;
    signature = signature signature_char;
  }

let reward_amount node_id rewards =
  rewards
  |> List.find_opt (fun payout -> payout.Resource_attestations.node_id = node_id)
  |> Option.map (fun payout -> payout.Resource_attestations.amount)
  |> Option.value ~default:0L

let check_reward_conservation () =
  let attestations = [
    attestation ~node_id:"raspberry-pi" ~kind:Resource_attestations.PoStorage ~weight:1L ~score_char:'\030' ~signature_char:'\001';
    attestation ~node_id:"mac-mini" ~kind:Resource_attestations.PoUW ~weight:2L ~score_char:'\020' ~signature_char:'\002';
    attestation ~node_id:"home-node" ~kind:Resource_attestations.PoW ~weight:3L ~score_char:'\010' ~signature_char:'\003';
    attestation ~node_id:"finality-node" ~kind:Resource_attestations.Finality ~weight:4L ~score_char:'\040' ~signature_char:'\004';
    { (attestation ~node_id:"bad-node" ~kind:Resource_attestations.PoW ~weight:100L ~score_char:'\001' ~signature_char:'\005') with signature = "" };
  ] in
  let budget = 6000L in
  let rewards = Resource_attestations.distribute_resource_rewards ~budget attestations in
  let total = List.fold_left (fun total payout -> Int64.add total payout.Resource_attestations.amount) 0L rewards in
  assert_true "reward conservation" (total = budget);
  assert_true "pi reward" (reward_amount "raspberry-pi" rewards > 0L);
  assert_true "invalid reward" (reward_amount "bad-node" rewards = 0L)

let reward_entry index weight =
  Resource_attestations.unsigned_attestation
    ~chain_id:"local-reward-test" ~epoch_id:1L
    ~node_id:("node-" ^ string_of_int index) ~kind:Resource_attestations.PoUW
    ~commitment:"test work" ~proof_hash:(String.make 32 'p') ~weight

let check_reward_math () =
  let open Resource_attestations in
  let check budget values =
    let payouts = distribute_resource_rewards ~budget values in
    let expected = if budget > 0L && List.exists is_well_formed values
      then Z.of_int64 budget else Z.zero in
    let spent = List.fold_left (fun sum (value : payout) ->
      assert_true "negative resource payment" (value.amount >= 0L);
      Z.add sum (Z.of_int64 value.amount)) Z.zero payouts in
    assert_true "resource reward budget differs" (Z.equal spent expected);
    assert_true "resource reward order differs"
      (payouts = distribute_resource_rewards ~budget (List.rev values));
    payouts
  in
  ignore (check 1000L (List.init 3 (fun index -> reward_entry index Int64.max_int)));
  List.iter (fun budget ->
    List.iter (fun weights ->
      List.iter (fun shared ->
        List.mapi (fun index -> reward_entry (if shared then 0 else index)) weights
        |> check budget |> ignore) [false; true])
      [[]; [1L]; [1L; 2L; 3L]; [Int64.max_int];
       [Int64.max_int; Int64.max_int];
       [Int64.max_int; Int64.max_int; Int64.max_int];
       [Int64.max_int; 1L; 0L; -1L]; List.init 4096 (fun _ -> Int64.max_int)])
    [Int64.min_int; -1L; 0L; 1L; 2L; 1000L; Int64.max_int];
  let output = Buffer.create 65536 in
  for width = 1 to 32 do
    for budget = 1 to 100 do
      let values = List.init width (fun index ->
        reward_entry (index mod 7) (Int64.of_int (index + 1))) in
      Printf.bprintf output "%d:%d;" width budget;
      check (Int64.of_int budget) values
      |> List.iter (fun (value : payout) ->
        Printf.bprintf output "%s:%Ld;" value.node_id value.amount)
    done
  done;
  assert_true "ordinary resource rewards changed"
    (Digestif.SHA256.(digest_string (Buffer.contents output) |> to_hex) =
     "5ec2fe3297c0908d0f17c2f14a63a82a52f80dc418a4dce23db49c9122c4e3c6")

let check_reward_wire () =
  let open Resource_attestations in
  let value = reward_entry 0 Int64.max_int in
  let digest bytes = Digestif.SHA256.(digest_string bytes |> to_hex) in
  List.iter (fun (bytes, expected) ->
    assert_true "resource signed bytes changed" (digest bytes = expected))
    [encode_attestation value,
      "228312afdd225d3a707f8470169fd49a2756ebaa1835b8305cf56df950e5e98c";
     attestation_sign_bytes value,
      "521f72a87ab6443233d25912017f98d1f2babad02dd2ed3dadd4012b7b80bffd";
     attestation_id value,
      "749ba4698ddcadbd530ac1ad37dac4a36b4077952eb722a27789dddb8fa046bf";
     Octra_consensus.Resource_attestation_flow.committee_root
       ~target_epoch:2L ~source_epoch:1L ~challenge:(String.make 32 'c') [value],
      "34835c59c7e7d90af6ff1dbbb2b8db3c648d09f841eebbae927655ca52a09a14"]

let check_weight_sum () =
  let open Resource_attestations in
  List.iter (fun width ->
    let values = List.init width (fun index -> reward_entry index Int64.max_int) in
    let weight = sum_weight values in
    let expected = Z.mul (Z.of_int width) (Z.of_int64 Int64.max_int) in
    assert_true "resource total weight differs" (Z.equal weight expected);
    let quorum = quorum_weight weight in
    assert_true "resource quorum too small" (Z.gt Z.(of_int 3 * quorum) Z.(of_int 2 * weight));
    assert_true "resource quorum exceeds total" (Z.leq quorum weight);
    assert_true "resource quorum overlap"
      (conflicting_qc_impossible ~total_weight:weight
        ~byzantine_weight:Z.((weight - one) / of_int 3)))
    [1; 2; 3; 4096]

let check_sybil_weight_conservation () =
  let unsplit = [
    attestation ~node_id:"miner-a" ~kind:Resource_attestations.PoUW ~weight:6L ~score_char:'\030' ~signature_char:'\001';
    attestation ~node_id:"honest-b" ~kind:Resource_attestations.PoStorage ~weight:4L ~score_char:'\020' ~signature_char:'\002';
  ] in
  let split = [
    attestation ~node_id:"miner-a-1" ~kind:Resource_attestations.PoUW ~weight:1L ~score_char:'\030' ~signature_char:'\001';
    attestation ~node_id:"miner-a-2" ~kind:Resource_attestations.PoUW ~weight:2L ~score_char:'\031' ~signature_char:'\002';
    attestation ~node_id:"miner-a-3" ~kind:Resource_attestations.PoUW ~weight:3L ~score_char:'\032' ~signature_char:'\003';
    attestation ~node_id:"honest-b" ~kind:Resource_attestations.PoStorage ~weight:4L ~score_char:'\020' ~signature_char:'\004';
  ] in
  assert_true "sybil weight conservation"
    (Z.equal (Resource_attestations.sum_weight unsplit) (Resource_attestations.sum_weight split))

let check_wire_roundtrip () =
  let base =
    Resource_attestations.unsigned_attestation
      ~chain_id:"octra-resource-attestation-test"
      ~epoch_id:100L
      ~node_id:"raspberry-pi"
      ~kind:Resource_attestations.PoUW
      ~commitment:"pvac-kat-task"
      ~proof_hash:(hash '\006')
      ~weight:7L
  in
  let challenge = hash '\007' in
  let attestation = { (Resource_attestations.with_score ~challenge base) with signature = signature '\008' } in
  let decoded = Resource_attestations.decode_attestation (Resource_attestations.encode_attestation attestation) in
  assert_true "wire roundtrip" (decoded = attestation);
  assert_true "sign bytes length" (String.length (Resource_attestations.attestation_sign_bytes attestation) = 32);
  assert_true "attestation id length" (String.length (Resource_attestations.attestation_id attestation) = 32)

let check_signature_verifier () =
  let priv_raw = String.init 32 (fun index -> Char.chr (index + 1)) in
  let pubkey_raw =
    match Mirage_crypto_ec.Ed25519.priv_of_octets priv_raw with
    | Ok secret -> Mirage_crypto_ec.Ed25519.pub_to_octets (Mirage_crypto_ec.Ed25519.pub_of_priv secret)
    | Error _ -> fail "private key"
  in
  let challenge = hash '\012' in
  let base =
    Resource_attestations.unsigned_attestation
      ~chain_id:"octra-resource-attestation-test"
      ~epoch_id:101L
      ~node_id:"signed-node"
      ~kind:Resource_attestations.Finality
      ~commitment:"finality"
      ~proof_hash:(hash '\013')
      ~weight:1L
    |> Resource_attestations.with_score ~challenge
  in
  let signed = Resource_attestations.sign_attestation ~priv_raw base in
  let tampered = { signed with weight = 2L } in
  assert_true "signature verifier accepts" (Resource_attestations.verify_attestation_signature ~pubkey_raw signed);
  assert_true "signature verifier rejects tamper" (not (Resource_attestations.verify_attestation_signature ~pubkey_raw tampered))

let scored_attestation ~challenge attestation =
  { (Resource_attestations.with_score ~challenge attestation) with signature = signature '\088' }

let find_pow_nonce ~challenge ~difficulty_bits attestation =
  let rec loop nonce =
    let nonce_string = string_of_int nonce in
    let attempt_attestation = { attestation with Resource_attestations.proof_hash = Resource_attestations.pow_proof_hash ~nonce:nonce_string } in
    if Resource_attestations.leading_zero_bits (Resource_attestations.pow_attempt_hash ~challenge attempt_attestation ~nonce:nonce_string) >= difficulty_bits
    then nonce_string
    else loop (nonce + 1)
  in
  loop 0

let check_pow_verifier () =
  let challenge = hash '\021' in
  let placeholder =
    Resource_attestations.unsigned_attestation
      ~chain_id:"octra-resource-attestation-test"
      ~epoch_id:200L
      ~node_id:"mac-mini"
      ~kind:Resource_attestations.PoW
      ~commitment:"pow"
      ~proof_hash:(hash '\000')
      ~weight:1L
  in
  let difficulty_bits = 10 in
  let nonce = find_pow_nonce ~challenge ~difficulty_bits placeholder in
  let attestation =
    { placeholder with proof_hash = Resource_attestations.pow_proof_hash ~nonce }
    |> scored_attestation ~challenge
  in
  let verify = Resource_attestations.verify_pow_attestation
    ~challenge ~commitment:"pow" ~max_weight:1L in
  assert_true "pow verifier accepts" (verify ~difficulty_bits attestation ~nonce);
  assert_true "pow verifier rejects nonce" (not (verify ~difficulty_bits attestation ~nonce:"bad"));
  assert_true "pow verifier rejects difficulty" (not (verify ~difficulty_bits:(difficulty_bits + 64) attestation ~nonce))

let check_storage_verifier () =
  let challenge = hash '\031' in
  let left_chunk = "left chunk from local node" in
  let right_chunk = "right chunk from pi node" in
  let left_leaf = Resource_attestations.storage_leaf_hash left_chunk in
  let right_leaf = Resource_attestations.storage_leaf_hash right_chunk in
  let root = Resource_attestations.storage_parent_hash left_leaf right_leaf in
  let placeholder =
    Resource_attestations.unsigned_attestation
      ~chain_id:"octra-resource-attestation-test"
      ~epoch_id:201L
      ~node_id:"raspberry-pi"
      ~kind:Resource_attestations.PoStorage
      ~commitment:root
      ~proof_hash:(hash '\000')
      ~weight:1L
  in
  let leaf_count = 2L in
  let expected_index =
    match Resource_attestations.storage_challenge_index ~challenge ~leaf_count placeholder with
    | Some index -> index
    | None -> fail "storage expected index"
  in
  let evidence =
    if expected_index = 0L then
      Resource_attestations.{ leaf_index = 0L; leaf_count; chunk = left_chunk; path = [{ side = Right; sibling_hash = right_leaf }] }
    else
      Resource_attestations.{ leaf_index = 1L; leaf_count; chunk = right_chunk; path = [{ side = Left; sibling_hash = left_leaf }] }
  in
  let attestation =
    { placeholder with proof_hash = Resource_attestations.storage_evidence_hash evidence }
    |> scored_attestation ~challenge
  in
  let bad_evidence = Resource_attestations.{ evidence with chunk = "corrupted" } in
  let verify = Resource_attestations.verify_storage_attestation
    ~challenge ~commitment:root ~max_weight:1L ~leaf_count:2L in
  let single = Resource_attestations.{
    leaf_index = 0L; leaf_count = 1L; chunk = "not the assigned data"; path = [];
  } in
  let forged = Resource_attestations.{ attestation with
    commitment = storage_leaf_hash single.chunk;
    proof_hash = storage_evidence_hash single;
  } in
  assert_true "storage rejects self chosen tree"
    (not (verify forged single));
  assert_true "storage rejects count change"
    (not (verify attestation { evidence with leaf_count = 1L }));
  assert_true "storage rejects extra credit"
    (not (verify { attestation with weight = 2L } evidence));
  assert_true "storage verifier accepts" (verify attestation evidence);
  assert_true "storage verifier rejects chunk" (not (verify attestation bad_evidence))

let check_useful_work_verifier () =
  let challenge = hash '\041' in
  let input = "bounded deterministic computation task" in
  let iterations = 64 in
  let result =
    match Resource_attestations.useful_hash_chain_result ~input ~iterations with
    | Some result -> result
    | None -> fail "useful result"
  in
  let evidence = Resource_attestations.{ input; iterations; result } in
  let attestation =
    Resource_attestations.unsigned_attestation
      ~chain_id:"octra-resource-attestation-test"
      ~epoch_id:202L
      ~node_id:"home-node"
      ~kind:Resource_attestations.PoUW
      ~commitment:(Resource_attestations.useful_hash_chain_task_id ~challenge ~input ~iterations)
      ~proof_hash:(Resource_attestations.useful_hash_chain_evidence_hash evidence)
      ~weight:1L
    |> scored_attestation ~challenge
  in
  let bad_evidence = Resource_attestations.{ evidence with result = hash '\099' } in
  let oversized_evidence = Resource_attestations.{ evidence with iterations = 1_000_000_000 } in
  let verify = Resource_attestations.verify_useful_hash_chain_attestation
    ~challenge ~commitment:attestation.commitment ~max_weight:1L ~max_iterations:256 in
  assert_true "useful verifier accepts" (verify ~min_iterations:32 attestation evidence);
  assert_true "useful verifier rejects result" (not (verify ~min_iterations:32 attestation bad_evidence));
  assert_true "useful verifier rejects floor" (not (verify ~min_iterations:128 attestation evidence));
  assert_true "useful verifier rejects oversized work before hashing" (not (verify ~min_iterations:32 attestation oversized_evidence))

let plugin_attestation ~challenge ~node_id ~commitment ~proof_hash ~weight =
  Resource_attestations.unsigned_attestation
    ~chain_id:"octra-resource-attestation-test"
    ~epoch_id:300L
    ~node_id
    ~kind:Resource_attestations.PoUW
    ~commitment
    ~proof_hash
    ~weight
  |> scored_attestation ~challenge

let storage_pair_evidence ~challenge ~placeholder ~left_chunk ~right_chunk =
  let left_leaf = Resource_attestations.storage_leaf_hash left_chunk in
  let right_leaf = Resource_attestations.storage_leaf_hash right_chunk in
  let leaf_count = 2L in
  match Resource_attestations.storage_challenge_index ~challenge ~leaf_count placeholder with
  | None -> fail "storage plugin index"
  | Some 0L ->
      Resource_attestations.{ leaf_index = 0L; leaf_count; chunk = left_chunk; path = [{ side = Right; sibling_hash = right_leaf }] }
  | Some _ ->
      Resource_attestations.{ leaf_index = 1L; leaf_count; chunk = right_chunk; path = [{ side = Left; sibling_hash = left_leaf }] }

let check_pvac_kat_plugin () =
  let challenge = hash '\051' in
  let evidence_without_expected =
    Resource_attestations.{
      key_id = "pvac-main-key";
      pubkey_hash = hash '\052';
      kat_input_hash = hash '\053';
      kat_output = "kat-output";
      expected_output_hash = hash '\000';
      work_units = 17L;
    }
  in
  let evidence =
    Resource_attestations.{
      evidence_without_expected with
      expected_output_hash = Resource_attestations.pvac_kat_output_hash evidence_without_expected;
    }
  in
  let commitment =
    Resource_attestations.useful_plugin_task_id
      ~challenge
      ~plugin:Resource_attestations.PvacKat
      ~subject:evidence.key_id
      ~resource_hash:(Resource_attestations.pvac_kat_resource_hash evidence)
      ~work_units:evidence.work_units
  in
  let attestation =
    plugin_attestation
      ~challenge
      ~node_id:"pvac-worker"
      ~commitment
      ~proof_hash:(Resource_attestations.pvac_kat_evidence_hash evidence)
      ~weight:5L
  in
  let bad_evidence = Resource_attestations.{ evidence with kat_output = "wrong" } in
  let verify = Resource_attestations.verify_pvac_kat_attestation
    ~challenge ~commitment ~max_weight:5L ~output_hash:evidence.expected_output_hash in
  let invented = Resource_attestations.{ bad_evidence with
    expected_output_hash = pvac_kat_output_hash bad_evidence;
  } in
  let forged = Resource_attestations.{ attestation with
    proof_hash = pvac_kat_evidence_hash invented;
  } in
  assert_true "pvac kat rejects self certified output"
    (not (verify forged invented));
  assert_true "pvac kat accepts" (verify attestation evidence);
  assert_true "pvac kat rejects output" (not (verify attestation bad_evidence))

let check_fhe_receipt_plugin () =
  let challenge = hash '\061' in
  let priv_raw = String.init 32 (fun index -> Char.chr (80 + index)) in
  let pubkey_raw =
    match Mirage_crypto_ec.Ed25519.priv_of_octets priv_raw with
    | Ok secret -> Mirage_crypto_ec.Ed25519.pub_to_octets (Mirage_crypto_ec.Ed25519.pub_of_priv secret)
    | Error _ -> fail "fhe receipt key"
  in
  let unsigned_evidence =
    Resource_attestations.{
      verifier_pubkey = pubkey_raw;
      proof_kind = "bound_zero_v1";
      program_id = "program-fhe";
      input_hash = hash '\062';
      output_hash = hash '\063';
      cost_units = 23L;
      receipt_signature = signature '\000';
    }
  in
  let evidence =
    Resource_attestations.{
      unsigned_evidence with
      receipt_signature = Resource_attestations.sign_fhe_receipt_evidence ~priv_raw unsigned_evidence;
    }
  in
  let commitment =
    Resource_attestations.useful_plugin_task_id
      ~challenge
      ~plugin:Resource_attestations.FheProofReceipt
      ~subject:evidence.program_id
      ~resource_hash:(Resource_attestations.fhe_receipt_resource_hash evidence)
      ~work_units:evidence.cost_units
  in
  let attestation =
    plugin_attestation
      ~challenge
      ~node_id:"fhe-worker"
      ~commitment
      ~proof_hash:(Resource_attestations.fhe_receipt_evidence_hash evidence)
      ~weight:9L
  in
  let bad_evidence = Resource_attestations.{ evidence with output_hash = hash '\064' } in
  let verify = Resource_attestations.verify_fhe_receipt_attestation
    ~challenge ~commitment ~max_weight:9L ~verifier_pubkey:pubkey_raw in
  assert_true "fhe receipt accepts" (verify attestation evidence);
  assert_true "fhe receipt rejects tamper" (not (verify attestation bad_evidence));
  assert_true "fhe receipt rejects unassigned key"
    (not (Resource_attestations.verify_fhe_receipt_attestation
      ~challenge ~commitment ~max_weight:9L ~verifier_pubkey:(hash '\065')
      attestation evidence))

let other_leaf ~left ~right (proof : Resource_attestations.storage_evidence) =
  let chunk, side, sibling =
    if proof.leaf_index = 0L then right, Resource_attestations.Left, left
    else left, Resource_attestations.Right, right
  in
  Resource_attestations.{ proof with chunk;
    path = [{ side; sibling_hash = storage_leaf_hash sibling }] }

let check_circle_asset_plugin () =
  let challenge = hash '\071' in
  let left_chunk = "circle index asset chunk" in
  let right_chunk = "circle style asset chunk" in
  let asset_root =
    Resource_attestations.storage_parent_hash
      (Resource_attestations.storage_leaf_hash left_chunk)
      (Resource_attestations.storage_leaf_hash right_chunk)
  in
  let base_evidence =
    Resource_attestations.{
      circle_id = "octCircle";
      resource_path = "/index.html";
      asset_root;
      byte_count = 4096L;
      storage = { leaf_index = 0L; leaf_count = 2L; chunk = left_chunk; path = [] };
    }
  in
  let commitment =
    Resource_attestations.useful_plugin_task_id
      ~challenge
      ~plugin:Resource_attestations.CircleAssetAvailability
      ~subject:base_evidence.circle_id
      ~resource_hash:(Resource_attestations.circle_asset_resource_hash base_evidence)
      ~work_units:base_evidence.byte_count
  in
  let placeholder =
    plugin_attestation
      ~challenge
      ~node_id:"circle-worker"
      ~commitment
      ~proof_hash:(hash '\000')
      ~weight:11L
  in
  let evidence =
    Resource_attestations.{ base_evidence with storage = storage_pair_evidence ~challenge ~placeholder ~left_chunk ~right_chunk }
  in
  let attestation = { placeholder with proof_hash = Resource_attestations.circle_asset_evidence_hash evidence } in
  let bad_evidence = Resource_attestations.{ evidence with asset_root = hash '\072' } in
  let swapped = Resource_attestations.{ evidence with
    storage = other_leaf ~left:left_chunk ~right:right_chunk evidence.storage } in
  let forged = { attestation with proof_hash = Resource_attestations.circle_asset_evidence_hash swapped } in
  let verify = Resource_attestations.verify_circle_asset_attestation
    ~challenge ~commitment ~max_weight:11L ~leaf_count:2L in
  assert_true "circle asset rejects other leaf"
    (not (verify forged swapped));
  assert_true "circle asset accepts" (verify attestation evidence);
  assert_true "circle asset rejects root" (not (verify attestation bad_evidence))

let check_snapshot_availability_plugin () =
  let challenge = hash '\081' in
  let left_chunk = "snapshot left range" in
  let right_chunk = "snapshot right range" in
  let snapshot_root =
    Resource_attestations.storage_parent_hash
      (Resource_attestations.storage_leaf_hash left_chunk)
      (Resource_attestations.storage_leaf_hash right_chunk)
  in
  let base_evidence =
    Resource_attestations.{
      state_root = hash '\082';
      range_start = 1000L;
      range_end = 1999L;
      snapshot_root;
      byte_count = 8192L;
      storage = { leaf_index = 0L; leaf_count = 2L; chunk = left_chunk; path = [] };
    }
  in
  let commitment =
    Resource_attestations.useful_plugin_task_id
      ~challenge
      ~plugin:Resource_attestations.SnapshotAvailability
      ~subject:"1000:1999"
      ~resource_hash:(Resource_attestations.snapshot_availability_resource_hash base_evidence)
      ~work_units:base_evidence.byte_count
  in
  let placeholder =
    plugin_attestation
      ~challenge
      ~node_id:"snapshot-worker"
      ~commitment
      ~proof_hash:(hash '\000')
      ~weight:13L
  in
  let evidence =
    Resource_attestations.{ base_evidence with storage = storage_pair_evidence ~challenge ~placeholder ~left_chunk ~right_chunk }
  in
  let attestation = { placeholder with proof_hash = Resource_attestations.snapshot_availability_evidence_hash evidence } in
  let bad_evidence = Resource_attestations.{ evidence with range_end = 999L } in
  let swapped = Resource_attestations.{ evidence with
    storage = other_leaf ~left:left_chunk ~right:right_chunk evidence.storage } in
  let forged = { attestation with proof_hash = Resource_attestations.snapshot_availability_evidence_hash swapped } in
  let verify = Resource_attestations.verify_snapshot_availability_attestation
    ~challenge ~commitment ~max_weight:13L ~leaf_count:2L in
  assert_true "snapshot rejects other leaf"
    (not (verify forged swapped));
  assert_true "snapshot availability accepts" (verify attestation evidence);
  assert_true "snapshot availability rejects range" (not (verify attestation bad_evidence))

let check_deterministic_trace_plugin () =
  let challenge = hash '\091' in
  let left_step = "trace step 0" in
  let right_step = "trace step 1" in
  let trace_root =
    Resource_attestations.storage_parent_hash
      (Resource_attestations.storage_leaf_hash left_step)
      (Resource_attestations.storage_leaf_hash right_step)
  in
  let base_evidence =
    Resource_attestations.{
      runtime_id = "deterministic-runtime-v0";
      source_hash = hash '\092';
      input_hash = hash '\093';
      output_hash = hash '\094';
      step_limit = 2048L;
      trace_root;
      sampled_step = { leaf_index = 0L; leaf_count = 2L; chunk = left_step; path = [] };
    }
  in
  let commitment =
    Resource_attestations.useful_plugin_task_id
      ~challenge
      ~plugin:Resource_attestations.DeterministicComputationTrace
      ~subject:base_evidence.runtime_id
      ~resource_hash:(Resource_attestations.deterministic_trace_resource_hash base_evidence)
      ~work_units:base_evidence.step_limit
  in
  let placeholder =
    plugin_attestation
      ~challenge
      ~node_id:"research-worker"
      ~commitment
      ~proof_hash:(hash '\000')
      ~weight:21L
  in
  let evidence =
    Resource_attestations.{ base_evidence with sampled_step = storage_pair_evidence ~challenge ~placeholder ~left_chunk:left_step ~right_chunk:right_step }
  in
  let attestation = { placeholder with proof_hash = Resource_attestations.deterministic_trace_evidence_hash evidence } in
  let bad_evidence = Resource_attestations.{ evidence with output_hash = hash '\095' } in
  let swapped = Resource_attestations.{ evidence with
    sampled_step = other_leaf ~left:left_step ~right:right_step evidence.sampled_step } in
  let forged = { attestation with proof_hash = Resource_attestations.deterministic_trace_evidence_hash swapped } in
  let verify = Resource_attestations.verify_deterministic_trace_attestation
    ~challenge ~commitment ~max_weight:21L ~leaf_count:2L in
  assert_true "trace rejects other leaf"
    (not (verify forged swapped));
  assert_true "deterministic trace accepts" (verify attestation evidence);
  assert_true "deterministic trace rejects output" (not (verify attestation bad_evidence))

let find_perturbed_matrix_nonce ~challenge ~difficulty_bits evidence =
  let rec loop attempt =
    let candidate = Resource_attestations.{ evidence with opening_nonce = string_of_int attempt } in
    if Resource_attestations.leading_zero_bits (Resource_attestations.perturbed_matrix_opening_hash ~challenge candidate) >= difficulty_bits
    then candidate
    else loop (attempt + 1)
  in
  loop 0

let check_perturbed_matrix_trace_plugin () =
  let challenge = hash '\101' in
  let difficulty_bits = 8 in
  let evidence_without_trace =
    Resource_attestations.{
      runtime_id = "matrix-runtime-v0";
      matrix_a_root = hash '\102';
      matrix_b_root = hash '\103';
      config_hash = hash '\104';
      chain_state_hash = hash '\105';
      tile_row = 0L;
      tile_col = 1L;
      tile_depth = 3L;
      tile_trace_root = hash '\000';
      sampled_tile = { leaf_index = 0L; leaf_count = 1L; chunk = ""; path = [] };
      field_modulus = 97L;
      perturbed_left_values = [2L; 3L; 5L];
      perturbed_right_values = [7L; 11L; 13L];
      claimed_value = 15L;
      opening_nonce = "";
      difficulty_bits;
      work_units = 4096L;
    }
  in
  let tile_chunk = Resource_attestations.perturbed_matrix_cell_chunk ~challenge evidence_without_trace in
  let tile_trace_root = Resource_attestations.storage_leaf_hash tile_chunk in
  let base_evidence =
    Resource_attestations.{
      evidence_without_trace with
      tile_trace_root;
      sampled_tile = { leaf_index = 0L; leaf_count = 1L; chunk = tile_chunk; path = [] };
    }
  in
  let commitment =
    Resource_attestations.useful_plugin_task_id
      ~challenge
      ~plugin:Resource_attestations.PerturbedMatrixTrace
      ~subject:base_evidence.runtime_id
      ~resource_hash:(Resource_attestations.perturbed_matrix_trace_resource_hash base_evidence)
      ~work_units:base_evidence.work_units
  in
  let placeholder =
    plugin_attestation
      ~challenge
      ~node_id:"matrix-worker"
      ~commitment
      ~proof_hash:(hash '\000')
      ~weight:34L
  in
  let evidence = find_perturbed_matrix_nonce ~challenge ~difficulty_bits base_evidence in
  let attestation = { placeholder with proof_hash = Resource_attestations.perturbed_matrix_trace_evidence_hash evidence } in
  let bad_evidence = Resource_attestations.{ evidence with chain_state_hash = hash '\106' } in
  let bad_arithmetic = Resource_attestations.{ evidence with claimed_value = 16L } in
  let hard_evidence = Resource_attestations.{ evidence with difficulty_bits = 256 } in
  let index = Option.get (Resource_attestations.storage_challenge_index
    ~challenge ~leaf_count:2L attestation) in
  let truncated = Resource_attestations.{ evidence with
    sampled_tile = { evidence.sampled_tile with leaf_count = 2L; leaf_index = index } }
    |> find_perturbed_matrix_nonce ~challenge ~difficulty_bits in
  let forged = { attestation with proof_hash = Resource_attestations.perturbed_matrix_trace_evidence_hash truncated } in
  let verify = Resource_attestations.verify_perturbed_matrix_trace_attestation
    ~challenge ~commitment ~max_weight:34L ~leaf_count:1L ~difficulty_bits in
  assert_true "matrix rejects missing path"
    (not (verify forged truncated));
  let easy = Resource_attestations.{ evidence with difficulty_bits = 0 } in
  let forged = Resource_attestations.{ attestation with
    commitment = useful_plugin_task_id ~challenge ~plugin:PerturbedMatrixTrace
      ~subject:easy.runtime_id ~resource_hash:(perturbed_matrix_trace_resource_hash easy)
      ~work_units:easy.work_units;
    proof_hash = perturbed_matrix_trace_evidence_hash easy;
  } in
  assert_true "matrix rejects self chosen difficulty" (not (verify forged easy));
  assert_true "perturbed matrix trace accepts" (verify attestation evidence);
  assert_true "perturbed matrix trace rejects state" (not (verify attestation bad_evidence));
  assert_true "perturbed matrix trace rejects arithmetic" (not (verify attestation bad_arithmetic));
  assert_true "perturbed matrix trace rejects difficulty" (not (verify attestation hard_evidence))

type event =
  | AttestationSeen of Resource_attestations.attestation
  | EvidenceSeen of string
  | TimeoutSeen of int

let event_key = function
  | AttestationSeen attestation -> "attestation:" ^ Resource_attestations.attestation_id attestation
  | EvidenceSeen value -> "evidence:" ^ value
  | TimeoutSeen round -> "timeout:" ^ string_of_int round

let normalize_events events =
  List.sort (fun left right -> compare (event_key left) (event_key right)) events

let decision events =
  let attestations =
    events
    |> normalize_events
    |> List.filter_map (function
        | AttestationSeen attestation -> Some attestation
        | EvidenceSeen _ -> None
        | TimeoutSeen _ -> None)
  in
  let selected = Resource_attestations.select_committee ~size:3 attestations |> List.map Resource_attestations.attestation_id in
  let rewards =
    Resource_attestations.distribute_resource_rewards ~budget:999L attestations
    |> List.map (fun payout -> payout.Resource_attestations.node_id, payout.Resource_attestations.amount)
  in
  selected, rewards

let check_decision_determinism () =
  let events = [
    TimeoutSeen 2;
    AttestationSeen (attestation ~node_id:"c" ~kind:Resource_attestations.PoUW ~weight:3L ~score_char:'\011' ~signature_char:'\011');
    AttestationSeen (attestation ~node_id:"a" ~kind:Resource_attestations.PoW ~weight:1L ~score_char:'\009' ~signature_char:'\009');
    EvidenceSeen "late-vote";
    AttestationSeen (attestation ~node_id:"b" ~kind:Resource_attestations.PoStorage ~weight:2L ~score_char:'\010' ~signature_char:'\010');
  ] in
  let shuffled = [
    AttestationSeen (attestation ~node_id:"b" ~kind:Resource_attestations.PoStorage ~weight:2L ~score_char:'\010' ~signature_char:'\010');
    EvidenceSeen "late-vote";
    AttestationSeen (attestation ~node_id:"a" ~kind:Resource_attestations.PoW ~weight:1L ~score_char:'\009' ~signature_char:'\009');
    AttestationSeen (attestation ~node_id:"c" ~kind:Resource_attestations.PoUW ~weight:3L ~score_char:'\011' ~signature_char:'\011');
    TimeoutSeen 2;
  ] in
  assert_true "decision determinism" (decision events = decision shuffled)

let log_factorial n =
  let rec loop value total =
    if value <= 1 then total else loop (value - 1) (total +. log (float_of_int value))
  in
  loop n 0.0

let binomial_probability trials probability successes =
  let failures = trials - successes in
  exp
    (log_factorial trials
     -. log_factorial successes
     -. log_factorial failures
     +. (float_of_int successes *. log probability)
     +. (float_of_int failures *. log (1.0 -. probability)))

let capture_probability trials alpha =
  let threshold = (trials / 3) + 1 in
  let rec loop successes total =
    if successes > trials then total
    else loop (successes + 1) (total +. binomial_probability trials alpha successes)
  in
  loop threshold 0.0

let check_capture_probability () =
  let alpha_20_committee_256 = capture_probability 256 0.20 in
  let alpha_25_committee_512 = capture_probability 512 0.25 in
  let bound_20_committee_256 =
    Resource_attestations.committee_capture_bound
      ~committee_size:256
      ~adversarial_fraction:0.20
      ~capture_fraction:(1.0 /. 3.0)
  in
  let bound_25_committee_512 =
    Resource_attestations.committee_capture_bound
      ~committee_size:512
      ~adversarial_fraction:0.25
      ~capture_fraction:(1.0 /. 3.0)
  in
  Printf.printf "capture_probability_alpha_20_m_256 = %.12g\n" alpha_20_committee_256;
  Printf.printf "capture_probability_alpha_25_m_512 = %.12g\n" alpha_25_committee_512;
  Printf.printf "capture_bound_alpha_20_m_256 = %.12g\n" bound_20_committee_256;
  Printf.printf "capture_bound_alpha_25_m_512 = %.12g\n" bound_25_committee_512;
  assert_true "capture alpha 20" (alpha_20_committee_256 < 0.00001);
  assert_true "capture alpha 25" (alpha_25_committee_512 < 0.001);
  assert_true "bound covers alpha 20" (bound_20_committee_256 >= alpha_20_committee_256);
  assert_true "bound covers alpha 25" (bound_25_committee_512 >= alpha_25_committee_512)

let () =
  check_qc_intersection ();
  check_reward_conservation ();
  check_reward_math ();
  check_reward_wire ();
  check_weight_sum ();
  check_sybil_weight_conservation ();
  check_wire_roundtrip ();
  check_signature_verifier ();
  check_pow_verifier ();
  check_storage_verifier ();
  check_useful_work_verifier ();
  check_pvac_kat_plugin ();
  check_fhe_receipt_plugin ();
  check_circle_asset_plugin ();
  check_snapshot_availability_plugin ();
  check_deterministic_trace_plugin ();
  check_perturbed_matrix_trace_plugin ();
  check_decision_determinism ();
  check_capture_probability ();
  Printf.printf "resource_attestation_model = ok\n"