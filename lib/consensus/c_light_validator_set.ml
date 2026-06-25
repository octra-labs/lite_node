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


type validator = {
  address : string;
  pubkey : string;
}

type scheduled = {
  activate_epoch : int64;
  validators : validator list;
}

type t = {
  chain_id : string;
  config_hash : string;
  validator_set_hash : string;
  n : int;
  f : int;
  quorum : int;
  validators : validator list;
  scheduled : scheduled option;
}

let hash_ok s =
  String.length s = 32

let to_consensus_validator (v : validator) =
  C_types.{ address = v.address; pubkey = v.pubkey }

let of_consensus_validator (v : C_types.validator_info) =
  { address = v.address; pubkey = v.pubkey }

let to_validator_set validators =
  validators
  |> List.map to_consensus_validator
  |> C_types.make_validator_set

let scheduled_to_config (s : scheduled) =
  C_config.{
    activate_epoch = s.activate_epoch;
    validator_set = to_validator_set s.validators;
  }

let scheduled_of_config (s : C_config.scheduled) =
  {
    activate_epoch = s.C_config.activate_epoch;
    validators = List.map of_consensus_validator s.validator_set.validators;
  }

let of_validator_set ~chain_id ~config_hash ?scheduled (vs : C_types.validator_set) =
  let validators = List.map of_consensus_validator vs.validators in
  {
    chain_id;
    config_hash;
    validator_set_hash = C_config.validator_set_hash vs;
    n = vs.n;
    f = vs.f;
    quorum = vs.quorum;
    validators;
    scheduled = Option.map scheduled_of_config scheduled;
  }

let recomputed_config_hash t =
  let vs = to_validator_set t.validators in
  let scheduled = Option.map scheduled_to_config t.scheduled in
  C_config.hash ~chain_id:t.chain_id ~validator_set:vs ?scheduled ()

let verify t =
  let vs = to_validator_set t.validators in
  t.chain_id <> ""
  && hash_ok t.config_hash
  && hash_ok t.validator_set_hash
  && t.n = vs.n
  && t.f = vs.f
  && t.quorum = vs.quorum
  && t.validator_set_hash = C_config.validator_set_hash vs
  && t.config_hash = recomputed_config_hash t