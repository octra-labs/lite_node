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


type transition =
  | No_transition
  | Already_active of {
      activate_epoch : int64;
    }
  | Scheduled of {
      activate_epoch : int64;
      n : int;
      quorum : int;
      fingerprint : string;
    }

type t = {
  allowed_pubkeys : string list;
  current_validator_list : Octra_consensus.C_types.validator_info list;
  next_validator_list : Octra_consensus.C_types.validator_info list;
  active_validator_list : Octra_consensus.C_types.validator_info list;
  active_vs : Octra_consensus.C_types.validator_set;
  scheduled_driver_config : Octra_consensus.C_driver.scheduled_validator_set_config option;
  light_scheduled_validator_set : Octra_consensus.C_config.scheduled option;
  consensus_config_hash : string;
  transition : transition;
}

let validator_info_of_entry entry =
  match String.split_on_char ':' entry with
  | [addr; pub_b64] ->
    (match Validators.raw32_of_base64 pub_b64 with
     | Some raw32 ->
       Some (Octra_consensus.C_types.{ address = addr; pubkey = raw32 })
     | None ->
       Some (Octra_consensus.C_types.{ address = addr; pubkey = "" }))
  | _ -> Some (Octra_consensus.C_types.{ address = entry; pubkey = "" })

let validator_list_of_entries entries =
  List.filter_map validator_info_of_entry entries

let driver_config_of_update update =
  let validators =
    update.Octra_core.Validator_set_update.validators
    |> List.filter_map (fun v ->
      match Validators.raw32_of_base64 v.Octra_core.Validator_set_update.pubkey_b64 with
      | Some raw32 ->
        Some Octra_consensus.C_types.{
          address = v.address;
          pubkey = raw32;
        }
      | None -> None)
  in
  if validators = [] then None
  else
    let validator_set = Octra_consensus.C_engine.make_validator_set validators in
    Some Octra_consensus.C_driver.{
      activate_epoch = update.activate_epoch;
      validator_set;
      fingerprint = update.fingerprint;
    }

let readiness_missing ~runtime ~requirements
    ~(update : Octra_core.Validator_set_update.t) readiness =
  readiness
  |> List.filter_map (fun (v, marker_opt) ->
    match marker_opt with
    | None -> Some v.Octra_core.Validator_set_update.address
    | Some marker_raw ->
      match Octra_core.Validator_set_update.ready_ext_of_string marker_raw with
      | Ok marker when marker.ready.fingerprint = update.Octra_core.Validator_set_update.fingerprint ->
        begin
          match Octra_core.Validator_ready_policy.validate
            ~runtime
            ~requirements
            marker with
          | Ok () -> None
          | Error reason ->
            Some (v.Octra_core.Validator_set_update.address ^ ":" ^ reason)
        end
      | _ -> Some v.Octra_core.Validator_set_update.address)

let fingerprint_of_validator_set vs =
  let entries =
    vs.Octra_consensus.C_types.validators
    |> List.map (fun v ->
      Octra_core.Validator_set_update.{
        address = v.Octra_consensus.C_types.address;
        pubkey_b64 = Base64.encode_exn v.Octra_consensus.C_types.pubkey;
      })
  in
  let fingerprint = Octra_core.Validator_set_update.fingerprint entries in
  String.sub fingerprint 0 (min 16 (String.length fingerprint))

let light_scheduled_of_driver = function
  | None -> None
  | Some cfg ->
    Some Octra_consensus.C_config.{
      activate_epoch = cfg.Octra_consensus.C_driver.activate_epoch;
      validator_set = cfg.validator_set;
    }

let build ~chain_id ~consensus_mode ~current_height ~current_entries
    ~next_entries ~chain_pending_entries ~next_activation_epoch =
  let allowed_pubkeys =
    Validators.raw_pubkeys_of_entries
      (current_entries @ next_entries @ chain_pending_entries)
  in
  let current_validator_list = validator_list_of_entries current_entries in
  let next_validator_list = validator_list_of_entries next_entries in
  let active_validator_list, scheduled_driver_config, transition =
    match next_validator_list, next_activation_epoch with
    | [], _ -> current_validator_list, None, No_transition
    | _, None -> current_validator_list, None, No_transition
    | next_list, Some activate_epoch
      when Int64.compare current_height activate_epoch >= 0 ->
      next_list, None, Already_active { activate_epoch }
    | next_list, Some activate_epoch ->
      let next_vs = Octra_consensus.C_engine.make_validator_set next_list in
      let fingerprint = fingerprint_of_validator_set next_vs in
      current_validator_list,
      Some Octra_consensus.C_driver.{
        activate_epoch;
        validator_set = next_vs;
        fingerprint;
      },
      Scheduled {
        activate_epoch;
        n = next_vs.n;
        quorum = next_vs.quorum;
        fingerprint;
      }
  in
  let active_vs = Octra_consensus.C_engine.make_validator_set active_validator_list in
  let light_scheduled_validator_set =
    light_scheduled_of_driver scheduled_driver_config
  in
  let consensus_config_hash =
    if consensus_mode && active_validator_list <> [] then
      Octra_consensus.C_config.hash
        ~chain_id
        ~validator_set:active_vs
        ?scheduled:light_scheduled_validator_set
        ()
    else String.make 32 '\x00'
  in
  {
    allowed_pubkeys;
    current_validator_list;
    next_validator_list;
    active_validator_list;
    active_vs;
    scheduled_driver_config;
    light_scheduled_validator_set;
    consensus_config_hash;
    transition;
  }