(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type attestation_kind = PoW | PoStorage | PoUW | Finality

type attestation = {
  chain_id : string;
  epoch_id : int64;
  node_id : string;
  kind : attestation_kind;
  commitment : string;
  proof_hash : string;
  weight : int64;
  score : string;
  signature : string;
}

type payout = {
  node_id : string;
  amount : int64;
}

type merkle_side = Left | Right

type merkle_step = {
  side : merkle_side;
  sibling_hash : string;
}

type storage_evidence = {
  leaf_index : int64;
  leaf_count : int64;
  chunk : string;
  path : merkle_step list;
}

type useful_hash_chain_evidence = {
  input : string;
  iterations : int;
  result : string;
}

type useful_plugin =
  | PvacKat
  | FheProofReceipt
  | CircleAssetAvailability
  | SnapshotAvailability
  | DeterministicComputationTrace
  | PerturbedMatrixTrace

type pvac_kat_evidence = {
  key_id : string;
  pubkey_hash : string;
  kat_input_hash : string;
  kat_output : string;
  expected_output_hash : string;
  work_units : int64;
}

type fhe_receipt_evidence = {
  verifier_pubkey : string;
  proof_kind : string;
  program_id : string;
  input_hash : string;
  output_hash : string;
  cost_units : int64;
  receipt_signature : string;
}

type circle_asset_evidence = {
  circle_id : string;
  resource_path : string;
  asset_root : string;
  byte_count : int64;
  storage : storage_evidence;
}

type snapshot_availability_evidence = {
  state_root : string;
  range_start : int64;
  range_end : int64;
  snapshot_root : string;
  byte_count : int64;
  storage : storage_evidence;
}

type deterministic_trace_evidence = {
  runtime_id : string;
  source_hash : string;
  input_hash : string;
  output_hash : string;
  step_limit : int64;
  trace_root : string;
  sampled_step : storage_evidence;
}

type perturbed_matrix_trace_evidence = {
  runtime_id : string;
  matrix_a_root : string;
  matrix_b_root : string;
  config_hash : string;
  chain_state_hash : string;
  tile_row : int64;
  tile_col : int64;
  tile_depth : int64;
  tile_trace_root : string;
  sampled_tile : storage_evidence;
  field_modulus : int64;
  perturbed_left_values : int64 list;
  perturbed_right_values : int64 list;
  claimed_value : int64;
  opening_nonce : string;
  difficulty_bits : int;
  work_units : int64;
}

module NodeMap = Map.Make (String)

let attestation_kind_to_u8 = function
  | PoW -> 1
  | PoStorage -> 2
  | PoUW -> 3
  | Finality -> 4

let attestation_kind_of_u8 = function
  | 1 -> PoW
  | 2 -> PoStorage
  | 3 -> PoUW
  | 4 -> Finality
  | _ -> failwith "bad resource attestation kind"

let attestation_kind_to_string = function
  | PoW -> "pow"
  | PoStorage -> "storage"
  | PoUW -> "useful"
  | Finality -> "finality"

let merkle_side_to_u8 = function
  | Left -> 1
  | Right -> 2

let merkle_side_of_u8 = function
  | 1 -> Left
  | 2 -> Right
  | _ -> failwith "bad merkle side"

let useful_plugin_to_u8 = function
  | PvacKat -> 1
  | FheProofReceipt -> 2
  | CircleAssetAvailability -> 3
  | SnapshotAvailability -> 4
  | DeterministicComputationTrace -> 5
  | PerturbedMatrixTrace -> 6

let useful_plugin_to_string = function
  | PvacKat -> "pvac_kat"
  | FheProofReceipt -> "fhe_proof_receipt"
  | CircleAssetAvailability -> "circle_asset_availability"
  | SnapshotAvailability -> "snapshot_availability"
  | DeterministicComputationTrace -> "deterministic_computation_trace"
  | PerturbedMatrixTrace -> "perturbed_matrix_trace"

let proof_payload_hash payload =
  Octra_net.Hash_domain.hash "octra:resource_proof_payload:v1" payload

let unsigned_attestation
    ~chain_id ~epoch_id ~node_id ~kind ~commitment ~proof_hash ~weight =
  {
    chain_id;
    epoch_id;
    node_id;
    kind;
    commitment;
    proof_hash;
    weight;
    score = Octra_net.Hash_domain.nil_hash;
    signature = String.make 64 '\000';
  }

let attestation_score ~challenge attestation =
  Octra_net.Hash_domain.hash_encoded "octra:resource_attestation_score:v1" (fun buf ->
    Octra_net.Oce1.put_string buf attestation.chain_id;
    Octra_net.Oce1.put_u64 buf attestation.epoch_id;
    Octra_net.Oce1.put_string buf attestation.node_id;
    Octra_net.Oce1.put_u8 buf (attestation_kind_to_u8 attestation.kind);
    Octra_net.Oce1.put_string buf attestation.commitment;
    Octra_net.Oce1.put_hash32 buf attestation.proof_hash;
    Octra_net.Oce1.put_u64 buf attestation.weight;
    Octra_net.Oce1.put_hash32 buf challenge)

let with_score ~challenge attestation =
  { attestation with score = attestation_score ~challenge attestation }

let attestation_sign_bytes attestation =
  Octra_net.Hash_domain.hash_encoded "octra:resource_attestation_sign:v1" (fun buf ->
    Octra_net.Oce1.put_string buf attestation.chain_id;
    Octra_net.Oce1.put_u64 buf attestation.epoch_id;
    Octra_net.Oce1.put_string buf attestation.node_id;
    Octra_net.Oce1.put_u8 buf (attestation_kind_to_u8 attestation.kind);
    Octra_net.Oce1.put_string buf attestation.commitment;
    Octra_net.Oce1.put_hash32 buf attestation.proof_hash;
    Octra_net.Oce1.put_u64 buf attestation.weight;
    Octra_net.Oce1.put_hash32 buf attestation.score)

let attestation_id attestation =
  Octra_net.Hash_domain.hash_encoded "octra:resource_attestation_id:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (attestation_sign_bytes attestation);
    Octra_net.Oce1.put_sig64 buf attestation.signature)

let sign_attestation ~priv_raw attestation =
  { attestation with signature = C_hash.sign_ed25519 ~priv_raw ~msg:(attestation_sign_bytes attestation) }

let verify_attestation_signature ~pubkey_raw attestation =
  C_hash.verify_ed25519 ~pubkey_raw ~msg:(attestation_sign_bytes attestation) ~signature:attestation.signature

let is_well_formed attestation =
  attestation.chain_id <> ""
  && attestation.node_id <> ""
  && attestation.weight > 0L
  && String.length attestation.proof_hash = 32
  && String.length attestation.score = 32
  && String.length attestation.signature = 64

let compare_attestation left right =
  let by_score = String.compare left.score right.score in
  if by_score <> 0 then by_score
  else
    let by_node = String.compare left.node_id right.node_id in
    if by_node <> 0 then by_node
    else
      let by_kind = compare (attestation_kind_to_u8 left.kind) (attestation_kind_to_u8 right.kind) in
      if by_kind <> 0 then by_kind
      else
        let by_proof = String.compare left.proof_hash right.proof_hash in
        if by_proof <> 0 then by_proof
        else String.compare left.commitment right.commitment

let rec take n xs =
  if n <= 0 then []
  else
    match xs with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

let select_committee ~size attestations =
  attestations
  |> List.filter is_well_formed
  |> List.sort compare_attestation
  |> take size

let sum_weight (attestations : attestation list) =
  List.fold_left Int64.add 0L (List.map (fun attestation -> attestation.weight) attestations)

let quorum_weight total_weight =
  let open Z in
  to_int64 (((of_int64 total_weight * of_int 2) / of_int 3) + one)

let qc_intersection_floor ~total_weight ~left_weight ~right_weight =
  let open Z in
  let overlap = of_int64 left_weight + of_int64 right_weight - of_int64 total_weight in
  if overlap <= zero then 0L else to_int64 overlap

let conflicting_qc_impossible ~total_weight ~byzantine_weight =
  let quorum = quorum_weight total_weight in
  qc_intersection_floor ~total_weight ~left_weight:quorum ~right_weight:quorum > byzantine_weight

let add_amount node_id amount rewards =
  let previous =
    match NodeMap.find_opt node_id rewards with
    | Some amount -> amount
    | None -> 0L
  in
  NodeMap.add node_id (Int64.add previous amount) rewards

let proportional_amount budget total_weight weight =
  let open Z in
  to_int64 ((of_int64 budget * of_int64 weight) / of_int64 total_weight)

let distribute_resource_rewards ~budget (attestations : attestation list) =
  let attestations = List.filter is_well_formed attestations in
  let total_weight = sum_weight attestations in
  if budget <= 0L || total_weight <= 0L then []
  else
    let rewards =
      List.fold_left
        (fun rewards (attestation : attestation) ->
          add_amount attestation.node_id (proportional_amount budget total_weight attestation.weight) rewards)
        NodeMap.empty
        attestations
    in
    let spent = NodeMap.fold (fun _ amount total -> Int64.add amount total) rewards 0L in
    let remainder = Int64.sub budget spent in
    let rec add_remainder left rewards (attestations : attestation list) =
      match left, attestations with
      | left, _ when left <= 0L -> rewards
      | _, [] -> rewards
      | left, attestation :: rest ->
          add_remainder (Int64.sub left 1L) (add_amount attestation.node_id 1L rewards) rest
    in
    add_remainder remainder rewards (List.sort compare_attestation attestations)
    |> NodeMap.bindings
    |> List.map (fun (node_id, amount) -> { node_id; amount })

let encode_attestation attestation =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf attestation.chain_id;
    Octra_net.Oce1.put_u64 buf attestation.epoch_id;
    Octra_net.Oce1.put_string buf attestation.node_id;
    Octra_net.Oce1.put_u8 buf (attestation_kind_to_u8 attestation.kind);
    Octra_net.Oce1.put_string buf attestation.commitment;
    Octra_net.Oce1.put_hash32 buf attestation.proof_hash;
    Octra_net.Oce1.put_u64 buf attestation.weight;
    Octra_net.Oce1.put_hash32 buf attestation.score;
    Octra_net.Oce1.put_sig64 buf attestation.signature)

let decode_attestation data =
  Octra_net.Oce1.decode
    (fun cursor ->
      let chain_id = Octra_net.Oce1.get_string cursor in
      let epoch_id = Octra_net.Oce1.get_u64 cursor in
      let node_id = Octra_net.Oce1.get_string cursor in
      let kind = attestation_kind_of_u8 (Octra_net.Oce1.get_u8 cursor) in
      let commitment = Octra_net.Oce1.get_string cursor in
      let proof_hash = Octra_net.Oce1.get_hash32 cursor in
      let weight = Octra_net.Oce1.get_u64 cursor in
      let score = Octra_net.Oce1.get_hash32 cursor in
      let signature = Octra_net.Oce1.get_sig64 cursor in
      { chain_id; epoch_id; node_id; kind; commitment; proof_hash; weight; score; signature })
    data

let attestations_root attestations =
  let sorted = List.sort compare_attestation attestations in
  Octra_net.Hash_domain.hash_encoded "octra:resource_attestations_root:v1" (fun buf ->
    Octra_net.Oce1.put_list
      (fun buf attestation -> Octra_net.Oce1.put_hash32 buf (attestation_id attestation))
      buf
      sorted)

let byte_leading_zero_bits byte =
  let rec loop bit count =
    if bit < 0 then count
    else if byte land (1 lsl bit) = 0 then loop (bit - 1) (count + 1)
    else count
  in
  loop 7 0

let leading_zero_bits digest =
  let rec loop index total =
    if index >= String.length digest then total
    else
      let count = byte_leading_zero_bits (Char.code digest.[index]) in
      if count = 8 then loop (index + 1) (total + 8) else total + count
  in
  loop 0 0

let encode_pow_evidence ~nonce =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf nonce)

let pow_proof_hash ~nonce =
  proof_payload_hash (encode_pow_evidence ~nonce)

let pow_attempt_hash ~challenge attestation ~nonce =
  Octra_net.Hash_domain.hash_encoded "octra:pow_attestation_attempt:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf challenge;
    Octra_net.Oce1.put_string buf attestation.chain_id;
    Octra_net.Oce1.put_u64 buf attestation.epoch_id;
    Octra_net.Oce1.put_string buf attestation.node_id;
    Octra_net.Oce1.put_string buf attestation.commitment;
    Octra_net.Oce1.put_hash32 buf attestation.proof_hash;
    Octra_net.Oce1.put_u64 buf attestation.weight;
    Octra_net.Oce1.put_string buf nonce)

let verify_pow_attestation ~challenge ~difficulty_bits attestation ~nonce =
  attestation.kind = PoW
  && difficulty_bits >= 0
  && difficulty_bits <= 256
  && attestation.proof_hash = pow_proof_hash ~nonce
  && leading_zero_bits (pow_attempt_hash ~challenge attestation ~nonce) >= difficulty_bits

let storage_leaf_hash chunk =
  Octra_net.Hash_domain.hash "octra:storage_chunk_leaf:v1" chunk

let storage_parent_hash left right =
  Octra_net.Hash_domain.hash_encoded "octra:storage_merkle_parent:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf left;
    Octra_net.Oce1.put_hash32 buf right)

let merkle_root_from_evidence evidence =
  let leaf = storage_leaf_hash evidence.chunk in
  List.fold_left
    (fun current step ->
      match step.side with
      | Left -> storage_parent_hash step.sibling_hash current
      | Right -> storage_parent_hash current step.sibling_hash)
    leaf
    evidence.path

let encode_storage_evidence evidence =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_u64 buf evidence.leaf_index;
    Octra_net.Oce1.put_u64 buf evidence.leaf_count;
    Octra_net.Oce1.put_string buf evidence.chunk;
    Octra_net.Oce1.put_list
      (fun buf step ->
        Octra_net.Oce1.put_u8 buf (merkle_side_to_u8 step.side);
        Octra_net.Oce1.put_hash32 buf step.sibling_hash)
      buf
      evidence.path)

let storage_evidence_hash evidence =
  proof_payload_hash (encode_storage_evidence evidence)

let u64_prefix digest =
  let value = ref 0L in
  for index = 0 to 7 do
    value := Int64.logor (Int64.shift_left !value 8) (Int64.of_int (Char.code digest.[index]))
  done;
  !value

let storage_challenge_index ~challenge ~leaf_count attestation =
  if leaf_count <= 0L then None
  else
    let digest =
      Octra_net.Hash_domain.hash_encoded "octra:storage_attestation_index:v1" (fun buf ->
        Octra_net.Oce1.put_hash32 buf challenge;
        Octra_net.Oce1.put_string buf attestation.chain_id;
        Octra_net.Oce1.put_u64 buf attestation.epoch_id;
        Octra_net.Oce1.put_string buf attestation.node_id;
        Octra_net.Oce1.put_string buf attestation.commitment)
    in
    Some (Int64.rem (Int64.abs (u64_prefix digest)) leaf_count)

let verify_storage_attestation ~challenge attestation evidence =
  match storage_challenge_index ~challenge ~leaf_count:evidence.leaf_count attestation with
  | None -> false
  | Some expected_index ->
      attestation.kind = PoStorage
      && String.length attestation.commitment = 32
      && evidence.leaf_index >= 0L
      && evidence.leaf_index < evidence.leaf_count
      && evidence.leaf_index = expected_index
      && attestation.proof_hash = storage_evidence_hash evidence
      && merkle_root_from_evidence evidence = attestation.commitment

let useful_hash_chain_task_id ~challenge ~input ~iterations =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_hash_chain_task:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf challenge;
    Octra_net.Oce1.put_string buf input;
    Octra_net.Oce1.put_u32_int buf iterations)

let useful_hash_chain_result ~input ~iterations =
  if iterations < 0 then None
  else
    let rec loop index current =
      if index >= iterations then current
      else
        loop (index + 1)
          (Octra_net.Hash_domain.hash_encoded "octra:pouw_hash_chain_step:v1" (fun buf ->
            Octra_net.Oce1.put_u32_int buf index;
            Octra_net.Oce1.put_hash32 buf current))
    in
    Some (loop 0 (Octra_net.Hash_domain.hash "octra:pouw_hash_chain_input:v1" input))

let encode_useful_hash_chain_evidence evidence =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf evidence.input;
    Octra_net.Oce1.put_u32_int buf evidence.iterations;
    Octra_net.Oce1.put_hash32 buf evidence.result)

let useful_hash_chain_evidence_hash evidence =
  proof_payload_hash (encode_useful_hash_chain_evidence evidence)

let verify_useful_hash_chain_attestation ~challenge ~min_iterations ~max_iterations attestation evidence =
  match useful_hash_chain_result ~input:evidence.input ~iterations:evidence.iterations with
  | None -> false
  | Some expected_result ->
      attestation.kind = PoUW
      && evidence.iterations >= min_iterations
      && evidence.iterations <= max_iterations
      && evidence.result = expected_result
      && attestation.commitment = useful_hash_chain_task_id ~challenge ~input:evidence.input ~iterations:evidence.iterations
      && attestation.proof_hash = useful_hash_chain_evidence_hash evidence

let useful_plugin_task_id ~challenge ~plugin ~subject ~resource_hash ~work_units =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_plugin_task:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf challenge;
    Octra_net.Oce1.put_u8 buf (useful_plugin_to_u8 plugin);
    Octra_net.Oce1.put_string buf subject;
    Octra_net.Oce1.put_hash32 buf resource_hash;
    Octra_net.Oce1.put_u64 buf work_units)

let nonempty_hash32 value =
  String.length value = 32

let attestation_weight_fits_resource attestation work_units =
  attestation.weight > 0L && work_units > 0L && attestation.weight <= work_units

let encode_pvac_kat_evidence evidence =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf evidence.key_id;
    Octra_net.Oce1.put_hash32 buf evidence.pubkey_hash;
    Octra_net.Oce1.put_hash32 buf evidence.kat_input_hash;
    Octra_net.Oce1.put_string buf evidence.kat_output;
    Octra_net.Oce1.put_hash32 buf evidence.expected_output_hash;
    Octra_net.Oce1.put_u64 buf evidence.work_units)

let pvac_kat_output_hash evidence =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_pvac_kat_output:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf evidence.pubkey_hash;
    Octra_net.Oce1.put_hash32 buf evidence.kat_input_hash;
    Octra_net.Oce1.put_string buf evidence.kat_output)

let pvac_kat_resource_hash evidence =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_pvac_kat_resource:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf evidence.pubkey_hash;
    Octra_net.Oce1.put_hash32 buf evidence.kat_input_hash)

let pvac_kat_evidence_hash evidence =
  proof_payload_hash (encode_pvac_kat_evidence evidence)

let verify_pvac_kat_attestation ~challenge attestation (evidence : pvac_kat_evidence) =
  attestation.kind = PoUW
  && nonempty_hash32 evidence.pubkey_hash
  && nonempty_hash32 evidence.kat_input_hash
  && attestation_weight_fits_resource attestation evidence.work_units
  && pvac_kat_output_hash evidence = evidence.expected_output_hash
  && attestation.commitment =
     useful_plugin_task_id
       ~challenge
       ~plugin:PvacKat
       ~subject:evidence.key_id
       ~resource_hash:(pvac_kat_resource_hash evidence)
       ~work_units:evidence.work_units
  && attestation.proof_hash = pvac_kat_evidence_hash evidence

let fhe_receipt_sign_bytes (evidence : fhe_receipt_evidence) =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_fhe_receipt_sign:v1" (fun buf ->
    Octra_net.Oce1.put_string buf evidence.proof_kind;
    Octra_net.Oce1.put_string buf evidence.program_id;
    Octra_net.Oce1.put_hash32 buf evidence.input_hash;
    Octra_net.Oce1.put_hash32 buf evidence.output_hash;
    Octra_net.Oce1.put_u64 buf evidence.cost_units)

let encode_fhe_receipt_evidence (evidence : fhe_receipt_evidence) =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf evidence.verifier_pubkey;
    Octra_net.Oce1.put_string buf evidence.proof_kind;
    Octra_net.Oce1.put_string buf evidence.program_id;
    Octra_net.Oce1.put_hash32 buf evidence.input_hash;
    Octra_net.Oce1.put_hash32 buf evidence.output_hash;
    Octra_net.Oce1.put_u64 buf evidence.cost_units;
    Octra_net.Oce1.put_sig64 buf evidence.receipt_signature)

let fhe_receipt_evidence_hash (evidence : fhe_receipt_evidence) =
  proof_payload_hash (encode_fhe_receipt_evidence evidence)

let sign_fhe_receipt_evidence ~priv_raw (evidence : fhe_receipt_evidence) =
  C_hash.sign_ed25519 ~priv_raw ~msg:(fhe_receipt_sign_bytes evidence)

let fhe_receipt_resource_hash (evidence : fhe_receipt_evidence) =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_fhe_receipt_resource:v1" (fun buf ->
    Octra_net.Oce1.put_string buf evidence.proof_kind;
    Octra_net.Oce1.put_hash32 buf evidence.input_hash;
    Octra_net.Oce1.put_hash32 buf evidence.output_hash)

let verify_fhe_receipt_attestation ~challenge attestation (evidence : fhe_receipt_evidence) =
  attestation.kind = PoUW
  && nonempty_hash32 evidence.input_hash
  && nonempty_hash32 evidence.output_hash
  && String.length evidence.verifier_pubkey = 32
  && String.length evidence.receipt_signature = 64
  && attestation_weight_fits_resource attestation evidence.cost_units
  && C_hash.verify_ed25519
       ~pubkey_raw:evidence.verifier_pubkey
       ~msg:(fhe_receipt_sign_bytes evidence)
       ~signature:evidence.receipt_signature
  && attestation.commitment =
     useful_plugin_task_id
       ~challenge
       ~plugin:FheProofReceipt
       ~subject:evidence.program_id
       ~resource_hash:(fhe_receipt_resource_hash evidence)
       ~work_units:evidence.cost_units
  && attestation.proof_hash = fhe_receipt_evidence_hash evidence

let encode_circle_asset_evidence (evidence : circle_asset_evidence) =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf evidence.circle_id;
    Octra_net.Oce1.put_string buf evidence.resource_path;
    Octra_net.Oce1.put_hash32 buf evidence.asset_root;
    Octra_net.Oce1.put_u64 buf evidence.byte_count;
    Octra_net.Oce1.put_string buf (encode_storage_evidence evidence.storage))

let circle_asset_resource_hash (evidence : circle_asset_evidence) =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_circle_asset_resource:v1" (fun buf ->
    Octra_net.Oce1.put_string buf evidence.circle_id;
    Octra_net.Oce1.put_string buf evidence.resource_path;
    Octra_net.Oce1.put_hash32 buf evidence.asset_root;
    Octra_net.Oce1.put_u64 buf evidence.byte_count)

let circle_asset_evidence_hash (evidence : circle_asset_evidence) =
  proof_payload_hash (encode_circle_asset_evidence evidence)

let verify_circle_asset_attestation ~challenge attestation (evidence : circle_asset_evidence) =
  match storage_challenge_index ~challenge ~leaf_count:evidence.storage.leaf_count attestation with
  | None -> false
  | Some expected_index ->
      attestation.kind = PoUW
      && evidence.circle_id <> ""
      && evidence.resource_path <> ""
      && evidence.byte_count > 0L
      && evidence.storage.leaf_index = expected_index
      && evidence.asset_root = merkle_root_from_evidence evidence.storage
      && attestation_weight_fits_resource attestation evidence.byte_count
      && attestation.commitment =
         useful_plugin_task_id
           ~challenge
           ~plugin:CircleAssetAvailability
           ~subject:evidence.circle_id
           ~resource_hash:(circle_asset_resource_hash evidence)
           ~work_units:evidence.byte_count
      && attestation.proof_hash = circle_asset_evidence_hash evidence

let encode_snapshot_availability_evidence (evidence : snapshot_availability_evidence) =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_hash32 buf evidence.state_root;
    Octra_net.Oce1.put_u64 buf evidence.range_start;
    Octra_net.Oce1.put_u64 buf evidence.range_end;
    Octra_net.Oce1.put_hash32 buf evidence.snapshot_root;
    Octra_net.Oce1.put_u64 buf evidence.byte_count;
    Octra_net.Oce1.put_string buf (encode_storage_evidence evidence.storage))

let snapshot_availability_resource_hash (evidence : snapshot_availability_evidence) =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_snapshot_resource:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf evidence.state_root;
    Octra_net.Oce1.put_u64 buf evidence.range_start;
    Octra_net.Oce1.put_u64 buf evidence.range_end;
    Octra_net.Oce1.put_hash32 buf evidence.snapshot_root;
    Octra_net.Oce1.put_u64 buf evidence.byte_count)

let snapshot_availability_evidence_hash (evidence : snapshot_availability_evidence) =
  proof_payload_hash (encode_snapshot_availability_evidence evidence)

let verify_snapshot_availability_attestation ~challenge attestation (evidence : snapshot_availability_evidence) =
  match storage_challenge_index ~challenge ~leaf_count:evidence.storage.leaf_count attestation with
  | None -> false
  | Some expected_index ->
      attestation.kind = PoUW
      && nonempty_hash32 evidence.state_root
      && evidence.range_start <= evidence.range_end
      && evidence.byte_count > 0L
      && evidence.storage.leaf_index = expected_index
      && evidence.snapshot_root = merkle_root_from_evidence evidence.storage
      && attestation_weight_fits_resource attestation evidence.byte_count
      && attestation.commitment =
         useful_plugin_task_id
           ~challenge
           ~plugin:SnapshotAvailability
           ~subject:(Int64.to_string evidence.range_start ^ ":" ^ Int64.to_string evidence.range_end)
           ~resource_hash:(snapshot_availability_resource_hash evidence)
           ~work_units:evidence.byte_count
      && attestation.proof_hash = snapshot_availability_evidence_hash evidence

let deterministic_trace_resource_hash (evidence : deterministic_trace_evidence) =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_deterministic_trace_resource:v1" (fun buf ->
    Octra_net.Oce1.put_string buf evidence.runtime_id;
    Octra_net.Oce1.put_hash32 buf evidence.source_hash;
    Octra_net.Oce1.put_hash32 buf evidence.input_hash;
    Octra_net.Oce1.put_hash32 buf evidence.output_hash;
    Octra_net.Oce1.put_hash32 buf evidence.trace_root;
    Octra_net.Oce1.put_u64 buf evidence.step_limit)

let encode_deterministic_trace_evidence (evidence : deterministic_trace_evidence) =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf evidence.runtime_id;
    Octra_net.Oce1.put_hash32 buf evidence.source_hash;
    Octra_net.Oce1.put_hash32 buf evidence.input_hash;
    Octra_net.Oce1.put_hash32 buf evidence.output_hash;
    Octra_net.Oce1.put_u64 buf evidence.step_limit;
    Octra_net.Oce1.put_hash32 buf evidence.trace_root;
    Octra_net.Oce1.put_string buf (encode_storage_evidence evidence.sampled_step))

let deterministic_trace_evidence_hash (evidence : deterministic_trace_evidence) =
  proof_payload_hash (encode_deterministic_trace_evidence evidence)

let verify_deterministic_trace_attestation ~challenge attestation (evidence : deterministic_trace_evidence) =
  match storage_challenge_index ~challenge ~leaf_count:evidence.sampled_step.leaf_count attestation with
  | None -> false
  | Some expected_index ->
      attestation.kind = PoUW
      && evidence.runtime_id <> ""
      && nonempty_hash32 evidence.source_hash
      && nonempty_hash32 evidence.input_hash
      && nonempty_hash32 evidence.output_hash
      && evidence.step_limit > 0L
      && evidence.sampled_step.leaf_index = expected_index
      && evidence.trace_root = merkle_root_from_evidence evidence.sampled_step
      && attestation_weight_fits_resource attestation evidence.step_limit
      && attestation.commitment =
         useful_plugin_task_id
           ~challenge
           ~plugin:DeterministicComputationTrace
           ~subject:evidence.runtime_id
           ~resource_hash:(deterministic_trace_resource_hash evidence)
           ~work_units:evidence.step_limit
      && attestation.proof_hash = deterministic_trace_evidence_hash evidence

let int64_of_list_length values =
  Int64.of_int (List.length values)

let field_value_is_valid ~modulus value =
  modulus > 1L && value >= 0L && value < modulus

let field_dot_mod ~modulus left_values right_values =
  if modulus <= 1L || left_values = [] || List.length left_values <> List.length right_values then None
  else if
    List.exists (fun value -> not (field_value_is_valid ~modulus value)) left_values
    || List.exists (fun value -> not (field_value_is_valid ~modulus value)) right_values
  then None
  else
    let open Z in
    let modulus_z = of_int64 modulus in
    let total =
      List.fold_left2
        (fun total left right ->
          erem (total + (of_int64 left * of_int64 right)) modulus_z)
        zero
        left_values
        right_values
    in
    Some (to_int64 total)

let perturbed_matrix_noise_seed ~challenge (evidence : perturbed_matrix_trace_evidence) =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_perturbed_matrix_noise_seed:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf challenge;
    Octra_net.Oce1.put_string buf evidence.runtime_id;
    Octra_net.Oce1.put_hash32 buf evidence.matrix_a_root;
    Octra_net.Oce1.put_hash32 buf evidence.matrix_b_root;
    Octra_net.Oce1.put_hash32 buf evidence.config_hash;
    Octra_net.Oce1.put_hash32 buf evidence.chain_state_hash;
    Octra_net.Oce1.put_u64 buf evidence.tile_row;
    Octra_net.Oce1.put_u64 buf evidence.tile_col;
    Octra_net.Oce1.put_u64 buf evidence.tile_depth)

let perturbed_matrix_trace_resource_hash (evidence : perturbed_matrix_trace_evidence) =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_perturbed_matrix_trace_resource:v1" (fun buf ->
    Octra_net.Oce1.put_string buf evidence.runtime_id;
    Octra_net.Oce1.put_hash32 buf evidence.matrix_a_root;
    Octra_net.Oce1.put_hash32 buf evidence.matrix_b_root;
    Octra_net.Oce1.put_hash32 buf evidence.config_hash;
    Octra_net.Oce1.put_hash32 buf evidence.chain_state_hash;
    Octra_net.Oce1.put_u64 buf evidence.tile_row;
    Octra_net.Oce1.put_u64 buf evidence.tile_col;
    Octra_net.Oce1.put_u64 buf evidence.tile_depth;
    Octra_net.Oce1.put_hash32 buf evidence.tile_trace_root;
    Octra_net.Oce1.put_u64 buf evidence.field_modulus;
    Octra_net.Oce1.put_u32_int buf evidence.difficulty_bits;
    Octra_net.Oce1.put_u64 buf evidence.work_units)

let put_u64_list buf values =
  Octra_net.Oce1.put_list (fun buf value -> Octra_net.Oce1.put_u64 buf value) buf values

let perturbed_matrix_cell_chunk ~challenge (evidence : perturbed_matrix_trace_evidence) =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_hash32 buf (perturbed_matrix_noise_seed ~challenge evidence);
    Octra_net.Oce1.put_string buf evidence.runtime_id;
    Octra_net.Oce1.put_u64 buf evidence.tile_row;
    Octra_net.Oce1.put_u64 buf evidence.tile_col;
    Octra_net.Oce1.put_u64 buf evidence.tile_depth;
    Octra_net.Oce1.put_u64 buf evidence.field_modulus;
    put_u64_list buf evidence.perturbed_left_values;
    put_u64_list buf evidence.perturbed_right_values;
    Octra_net.Oce1.put_u64 buf evidence.claimed_value)

let perturbed_matrix_opening_hash ~challenge (evidence : perturbed_matrix_trace_evidence) =
  Octra_net.Hash_domain.hash_encoded "octra:pouw_perturbed_matrix_opening:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf challenge;
    Octra_net.Oce1.put_hash32 buf (perturbed_matrix_noise_seed ~challenge evidence);
    Octra_net.Oce1.put_hash32 buf (perturbed_matrix_trace_resource_hash evidence);
    Octra_net.Oce1.put_u64 buf evidence.sampled_tile.leaf_index;
    Octra_net.Oce1.put_u64 buf evidence.sampled_tile.leaf_count;
    Octra_net.Oce1.put_hash32 buf (storage_leaf_hash evidence.sampled_tile.chunk);
    Octra_net.Oce1.put_string buf evidence.opening_nonce)

let encode_perturbed_matrix_trace_evidence (evidence : perturbed_matrix_trace_evidence) =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf evidence.runtime_id;
    Octra_net.Oce1.put_hash32 buf evidence.matrix_a_root;
    Octra_net.Oce1.put_hash32 buf evidence.matrix_b_root;
    Octra_net.Oce1.put_hash32 buf evidence.config_hash;
    Octra_net.Oce1.put_hash32 buf evidence.chain_state_hash;
    Octra_net.Oce1.put_u64 buf evidence.tile_row;
    Octra_net.Oce1.put_u64 buf evidence.tile_col;
    Octra_net.Oce1.put_u64 buf evidence.tile_depth;
    Octra_net.Oce1.put_hash32 buf evidence.tile_trace_root;
    Octra_net.Oce1.put_string buf (encode_storage_evidence evidence.sampled_tile);
    Octra_net.Oce1.put_u64 buf evidence.field_modulus;
    put_u64_list buf evidence.perturbed_left_values;
    put_u64_list buf evidence.perturbed_right_values;
    Octra_net.Oce1.put_u64 buf evidence.claimed_value;
    Octra_net.Oce1.put_string buf evidence.opening_nonce;
    Octra_net.Oce1.put_u32_int buf evidence.difficulty_bits;
    Octra_net.Oce1.put_u64 buf evidence.work_units)

let perturbed_matrix_trace_evidence_hash (evidence : perturbed_matrix_trace_evidence) =
  proof_payload_hash (encode_perturbed_matrix_trace_evidence evidence)

let verify_perturbed_matrix_trace_attestation ~challenge attestation (evidence : perturbed_matrix_trace_evidence) =
  match storage_challenge_index ~challenge ~leaf_count:evidence.sampled_tile.leaf_count attestation with
  | None -> false
  | Some expected_index ->
      attestation.kind = PoUW
      && evidence.runtime_id <> ""
      && nonempty_hash32 evidence.matrix_a_root
      && nonempty_hash32 evidence.matrix_b_root
      && nonempty_hash32 evidence.config_hash
      && nonempty_hash32 evidence.chain_state_hash
      && evidence.tile_row >= 0L
      && evidence.tile_col >= 0L
      && evidence.tile_depth >= 0L
      && evidence.tile_depth = int64_of_list_length evidence.perturbed_left_values
      && evidence.difficulty_bits >= 0
      && evidence.difficulty_bits <= 256
      && evidence.sampled_tile.leaf_index = expected_index
      && evidence.sampled_tile.chunk = perturbed_matrix_cell_chunk ~challenge evidence
      && evidence.tile_trace_root = merkle_root_from_evidence evidence.sampled_tile
      && field_dot_mod ~modulus:evidence.field_modulus evidence.perturbed_left_values evidence.perturbed_right_values = Some evidence.claimed_value
      && leading_zero_bits (perturbed_matrix_opening_hash ~challenge evidence) >= evidence.difficulty_bits
      && attestation_weight_fits_resource attestation evidence.work_units
      && attestation.commitment =
         useful_plugin_task_id
           ~challenge
           ~plugin:PerturbedMatrixTrace
           ~subject:evidence.runtime_id
           ~resource_hash:(perturbed_matrix_trace_resource_hash evidence)
           ~work_units:evidence.work_units
      && attestation.proof_hash = perturbed_matrix_trace_evidence_hash evidence

let kl_divergence ~p ~q =
  if p <= 0.0 || p >= 1.0 || q <= 0.0 || q >= 1.0 then invalid_arg "kl_divergence"
  else (p *. log (p /. q)) +. ((1.0 -. p) *. log ((1.0 -. p) /. (1.0 -. q)))

let committee_capture_bound ~committee_size ~adversarial_fraction ~capture_fraction =
  if committee_size <= 0 then invalid_arg "committee_capture_bound"
  else if adversarial_fraction <= 0.0 then 0.0
  else if adversarial_fraction >= capture_fraction then 1.0
  else
    exp (-. float_of_int committee_size *. kl_divergence ~p:capture_fraction ~q:adversarial_fraction)