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


type scheduled = {
  activate_epoch : int64;
  validator_set : C_types.validator_set;
}

let validator_set_hash (vs : C_types.validator_set) =
  Octra_net.Hash_domain.hash_encoded "octra:consensus_validator_set:v1" (fun buf ->
    Octra_net.Oce1.put_u32_int buf vs.n;
    Octra_net.Oce1.put_u32_int buf vs.f;
    Octra_net.Oce1.put_u32_int buf vs.quorum;
    Octra_net.Oce1.put_list (fun buf (v : C_types.validator_info) ->
      Octra_net.Oce1.put_string buf v.address;
      Octra_net.Oce1.put_raw buf v.pubkey)
      buf vs.validators)

let hash ~chain_id ~(validator_set : C_types.validator_set) ?scheduled () =
  Octra_net.Hash_domain.hash_encoded "octra:consensus_config:v1" (fun buf ->
    Octra_net.Oce1.put_string buf chain_id;
    Octra_net.Oce1.put_u16 buf C_types.proto_version_current;
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