(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type scheduled = {
  activate_epoch : int64;
  validator_set : C_types.validator_set;
}

let validator_set_hash (vs : C_types.validator_set) =
  let domain =
    if vs.weighted then "octra:consensus_validator_set:v2"
    else "octra:consensus_validator_set:v1"
  in
  Octra_net.Hash_domain.hash_encoded domain (fun buf ->
    Octra_net.Oce1.put_u32_int buf vs.n;
    Octra_net.Oce1.put_u32_int buf vs.f;
    Octra_net.Oce1.put_u32_int buf vs.quorum;
    if vs.weighted then begin
      Octra_net.Oce1.put_string buf (Z.to_string vs.total_weight);
      Octra_net.Oce1.put_string buf (Z.to_string vs.quorum_weight)
    end;
    Octra_net.Oce1.put_list (fun buf (v : C_types.validator_info) ->
      Octra_net.Oce1.put_string buf v.address;
      Octra_net.Oce1.put_raw buf v.pubkey;
      if vs.weighted then
        match C_types.weight_of_addr vs v.address with
        | None -> invalid_arg "validator weight missing"
        | Some weight ->
          Octra_net.Oce1.put_string buf (Z.to_string weight))
      buf vs.validators)

let network_hash ~chain_id ?program_trust_hash ?runtime_profile_hash () =
  Octra_net.Hash_domain.hash_encoded "octra:consensus_network" (fun buf ->
    Octra_net.Oce1.put_string buf chain_id;
    Octra_net.Oce1.put_u16 buf C_types.proto_version_current;
    Octra_net.Oce1.put_option
      Octra_net.Oce1.put_hash32
      buf
      program_trust_hash;
    Octra_net.Oce1.put_option
      Octra_net.Oce1.put_hash32
      buf
      runtime_profile_hash)

let hash ~chain_id ~(validator_set : C_types.validator_set) ?scheduled
    ?program_trust_hash ?runtime_profile_hash () =
  let weighted =
    validator_set.weighted
    || Option.fold
         ~none:false
         ~some:(fun value -> value.validator_set.weighted)
         scheduled
  in
  let domain =
    match weighted, runtime_profile_hash, program_trust_hash with
    | true, _, _ -> "octra:consensus_config:v4"
    | false, None, None -> "octra:consensus_config:v1"
    | false, None, Some _ -> "octra:consensus_config:v2"
    | false, Some _, _ -> "octra:consensus_config:v3"
  in
  Octra_net.Hash_domain.hash_encoded domain (fun buf ->
    Octra_net.Oce1.put_string buf chain_id;
    Octra_net.Oce1.put_u16 buf C_types.proto_version_current;
    (match weighted, runtime_profile_hash with
     | true, _ ->
       Octra_net.Oce1.put_option Octra_net.Oce1.put_hash32 buf program_trust_hash;
       Octra_net.Oce1.put_option Octra_net.Oce1.put_hash32 buf runtime_profile_hash
     | false, None ->
       Option.iter (Octra_net.Oce1.put_hash32 buf) program_trust_hash
     | false, Some profile_hash ->
       Octra_net.Oce1.put_option Octra_net.Oce1.put_hash32 buf program_trust_hash;
       Octra_net.Oce1.put_hash32 buf profile_hash);
    Octra_net.Oce1.put_hash32 buf (validator_set_hash validator_set);
    match scheduled with
    | None -> Octra_net.Oce1.put_u8 buf 0
    | Some s ->
      Octra_net.Oce1.put_u8 buf 1;
      Octra_net.Oce1.put_u64 buf s.activate_epoch;
      Octra_net.Oce1.put_hash32 buf (validator_set_hash s.validator_set))

let short h =
  String.concat "" (List.init (min 8 (String.length h)) (fun i ->
    Printf.sprintf "%02x" (Char.code h.[i])))