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


type epoch_seed = {
  chain_id : string;
  epoch_id : int64;
  prev_state_root : string;
  prev_qc_randomness : string;
}

type gossip = {
  chain_id : string;
  seen_epoch : int64;
  attestation : Resource_attestations.attestation;
}

type committee_snapshot = {
  target_epoch : int64;
  source_epoch : int64;
  challenge : string;
  committee : Resource_attestations.attestation list;
  committee_root : string;
  total_weight : int64;
  quorum_weight : int64;
}

let encode_gossip (gossip : gossip) =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf gossip.chain_id;
    Octra_net.Oce1.put_u64 buf gossip.seen_epoch;
    Octra_net.Oce1.put_string buf (Resource_attestations.encode_attestation gossip.attestation))

let decode_gossip payload =
  let cursor = Octra_net.Oce1.make_cursor payload in
  let chain_id = Octra_net.Oce1.get_string cursor in
  let seen_epoch = Octra_net.Oce1.get_u64 cursor in
  let attestation = Resource_attestations.decode_attestation (Octra_net.Oce1.get_string cursor) in
  { chain_id; seen_epoch; attestation }

let challenge (seed : epoch_seed) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_challenge:v1" (fun buf ->
    Octra_net.Oce1.put_string buf seed.chain_id;
    Octra_net.Oce1.put_u64 buf seed.epoch_id;
    Octra_net.Oce1.put_hash32 buf seed.prev_state_root;
    Octra_net.Oce1.put_hash32 buf seed.prev_qc_randomness)

let activated_source_epoch ~activation_delay ~target_epoch =
  if activation_delay < 0 then invalid_arg "activation_delay"
  else
    let delay = Int64.of_int activation_delay in
    if target_epoch < delay then None
    else Some (Int64.sub target_epoch delay)

let matches_epoch_challenge ~source_epoch ~challenge attestation =
  attestation.Resource_attestations.epoch_id = source_epoch
  && Resource_attestations.is_well_formed attestation
  && attestation.score = Resource_attestations.attestation_score ~challenge attestation

let eligible_attestations ~source_epoch ~challenge attestations =
  attestations
  |> List.filter (matches_epoch_challenge ~source_epoch ~challenge)

let committee_root ~target_epoch ~source_epoch ~challenge committee =
  Octra_net.Hash_domain.hash_encoded "octra:resource_committee_root:v1" (fun buf ->
    Octra_net.Oce1.put_u64 buf target_epoch;
    Octra_net.Oce1.put_u64 buf source_epoch;
    Octra_net.Oce1.put_hash32 buf challenge;
    Octra_net.Oce1.put_hash32 buf (Resource_attestations.attestations_root committee))

let select_snapshot ~activation_delay ~committee_size ~target_epoch ~source_seed attestations =
  match activated_source_epoch ~activation_delay ~target_epoch with
  | None -> None
  | Some source_epoch when source_epoch <> source_seed.epoch_id -> None
  | Some source_epoch ->
      let challenge = challenge source_seed in
      let committee =
        attestations
        |> eligible_attestations ~source_epoch ~challenge
        |> Resource_attestations.select_committee ~size:committee_size
      in
      let total_weight = Resource_attestations.sum_weight committee in
      let quorum_weight =
        if total_weight <= 0L then 0L
        else Resource_attestations.quorum_weight total_weight
      in
      Some {
        target_epoch;
        source_epoch;
        challenge;
        committee;
        committee_root = committee_root ~target_epoch ~source_epoch ~challenge committee;
        total_weight;
        quorum_weight;
      }

let validator_set_of_committee ~pubkey_of_node (committee : Resource_attestations.attestation list) =
  let rec loop acc = function
    | [] -> Some (C_types.make_validator_set (List.rev acc))
    | (attestation : Resource_attestations.attestation) :: rest ->
        match pubkey_of_node attestation.Resource_attestations.node_id with
        | None -> None
        | Some pubkey ->
            let validator = C_types.{ address = attestation.node_id; pubkey } in
            loop (validator :: acc) rest
  in
  loop [] committee

let ready_for_voting ~minimum_weight snapshot =
  snapshot.total_weight >= minimum_weight && snapshot.committee <> []

let rewards_for_snapshot ~budget snapshot =
  Resource_attestations.distribute_resource_rewards ~budget snapshot.committee