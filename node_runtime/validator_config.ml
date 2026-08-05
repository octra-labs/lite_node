(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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
  identity_errors : string list;
  program_trust_hash : string option;
  runtime_profile_hash : string option;
  current_validator_list : Octra_consensus.C_types.validator_info list;
  next_validator_list : Octra_consensus.C_types.validator_info list;
  active_validator_list : Octra_consensus.C_types.validator_info list;
  active_vs : Octra_consensus.C_types.validator_set;
  scheduled_driver_config : Octra_consensus.C_driver.scheduled_validator_set_config option;
  light_scheduled_validator_set : Octra_consensus.C_config.scheduled option;
  consensus_config_hash : string;
  transition : transition;
}

type persistent_update_deps = {
  read_pending : unit -> string option Lwt.t;
  log_invalid : string -> unit;
}

type persistent_update_node_deps = {
  read_pending : unit -> string option Lwt.t;
  warn : string -> unit;
}

type self_membership =
  | Self_active
  | Self_scheduled
  | Self_observer of string
  | Self_refused of string

type quorum_admission =
  | Quorum_ok
  | Quorum_refused of string list
  | Quorum_unsafe_allowed of string

type startup_event =
  | Startup_info of string
  | Startup_warn of string
  | Startup_refuse of string list

type startup_event_deps = {
  info : string -> unit;
  warn : string -> unit;
  refuse : string list -> unit;
}

let validator_info_of_entry entry =
  Option.map
    (fun (address, pubkey) -> Octra_consensus.C_types.{ address; pubkey })
    (Validators.parsed_entry entry)

let validator_list_of_entries entries =
  List.filter_map validator_info_of_entry entries

let driver_config_of_update update =
  match Octra_core.Validator_set_update.validator_set update with
  | Error _ -> None
  | Ok validator_set ->
    Some Octra_consensus.C_driver.{
      activate_epoch = update.activate_epoch;
      validator_set;
      fingerprint = update.fingerprint;
    }

let pending_entries_of_raw = function
  | None -> []
  | Some raw ->
    match Octra_core.Validator_set_update.of_string raw with
    | Error _ -> []
    | Ok update when update.weighted ->
      List.map
        (fun v ->
          v.Octra_core.Validator_set_update.address ^ ":" ^ v.pubkey_b64)
        update.validators
    | Ok _ -> []

let load_persistent_update (deps : persistent_update_deps) =
  let open Lwt.Syntax in
  let* pending_opt = deps.read_pending () in
  match pending_opt with
  | None -> Lwt.return_none
  | Some raw ->
    match Octra_core.Validator_set_update.of_string raw with
    | Error e ->
      deps.log_invalid e;
      Lwt.return_none
    | Ok update when update.weighted ->
      begin
        match driver_config_of_update update with
        | Some config -> Lwt.return_some config
        | None ->
          deps.log_invalid "weighted validator set is invalid";
          Lwt.return_none
      end
    | Ok _ ->
      deps.log_invalid "manual validator updates are disabled";
      Lwt.return_none

let load_node_persistent_update (deps : persistent_update_node_deps) =
  let update_deps = {
    read_pending = deps.read_pending;
    log_invalid = (fun e ->
      deps.warn
        (Printf.sprintf "persistent validator_set_update ignored: %s" e));
  } in
  load_persistent_update update_deps

let fingerprint_of_validator_set vs =
  let entries =
    vs.Octra_consensus.C_types.validators
    |> List.map (fun v ->
      Octra_core.Validator_set_update.{
        address = v.Octra_consensus.C_types.address;
        pubkey_b64 = Base64.encode_exn v.Octra_consensus.C_types.pubkey;
        weight =
          Option.value
            ~default:Z.one
            (Octra_consensus.C_types.weight_of_addr vs v.address);
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

let persistent_update label = function
  | None -> Ok None
  | Some raw ->
    begin
      match Octra_core.Validator_set_update.of_string raw with
      | Ok update -> Ok (Some update)
      | Error error -> Error (label ^ " validator set is invalid: " ^ error)
    end

let required_driver_config label update =
  match driver_config_of_update update with
  | Some config -> Ok config
  | None -> Error (label ^ " validator set cannot build consensus config")

let validate_active_update ~current_height update =
  if not update.Octra_core.Validator_set_update.weighted then
    Error "active validator set must use bonded weights"
  else if Int64.compare update.activate_epoch current_height > 0 then
    Error "active validator set activation is in the future"
  else
    required_driver_config "active" update

let validate_pending_update ~current_height ~active
    (update : Octra_core.Validator_set_update.t) =
  if not update.Octra_core.Validator_set_update.weighted then
    Error "pending validator set must use bonded weights"
  else
    match active with
    | None ->
      if Int64.compare update.activate_epoch current_height < 0 then
        Error "activated pending validator set has no durable active marker"
      else
        Result.map Option.some (required_driver_config "pending" update)
    | Some (active_update : Octra_core.Validator_set_update.t) ->
      if String.equal update.fingerprint active_update.fingerprint then
        Ok None
      else if Int64.compare update.activate_epoch active_update.activate_epoch <= 0 then
        Error "pending validator set does not follow durable active set"
      else if Int64.compare update.activate_epoch current_height <= 0 then
        Error "pending validator set activation is not durable"
      else
        Result.map Option.some (required_driver_config "pending" update)

let validator_pubkeys validator_set =
  validator_set.Octra_consensus.C_types.validators
  |> List.map (fun validator ->
    validator.Octra_consensus.C_types.pubkey)

let transition_of_persistent ~active_config ~scheduled_config =
  match scheduled_config, active_config with
  | Some scheduled, _ ->
    Scheduled {
      activate_epoch = scheduled.Octra_consensus.C_driver.activate_epoch;
      n = scheduled.validator_set.n;
      quorum = scheduled.validator_set.quorum;
      fingerprint = scheduled.fingerprint;
    }
  | None, Some active ->
    Already_active { activate_epoch = active.Octra_consensus.C_driver.activate_epoch }
  | None, None -> No_transition

let bind_persistent_updates ~chain_id ~consensus_mode ~current_height
    ~active_raw ~pending_raw cfg =
  match
    persistent_update "active" active_raw,
    persistent_update "pending" pending_raw
  with
  | Error error, _
  | _, Error error -> Error error
  | _, Ok (Some pending) when not pending.weighted ->
    Error "manual validator updates are disabled"
  | Ok None, Ok None -> Ok cfg
  | Ok active_update, Ok pending_update ->
    let active_config =
      match active_update with
      | None -> Ok None
      | Some update ->
        Result.map Option.some (validate_active_update ~current_height update)
    in
    begin
      match active_config with
      | Error error -> Error error
      | Ok active_config ->
        let pending_config =
          match pending_update with
          | None -> Ok None
          | Some update ->
            validate_pending_update
              ~current_height
              ~active:active_update
              update
        in
        begin
          match pending_config with
          | Error error -> Error error
          | Ok pending_config ->
            let active_vs_raw =
              match active_config with
              | None -> cfg.active_vs
              | Some active -> active.validator_set
            in
            let active_vs =
              Octra_consensus.C_types.validator_set_for_epoch
                ~chain_id
                ~epoch_id:current_height
                active_vs_raw
            in
            let scheduled_driver_config_raw =
              match pending_config with
              | Some scheduled -> Some scheduled
              | None when Option.is_some active_config -> None
              | None -> cfg.scheduled_driver_config
            in
            let scheduled_driver_config =
              if
                Octra_consensus.C_quorum_policy.active
                  ~chain_id
                  ~epoch_id:current_height
              then
                Option.map
                  (fun
                    (scheduled :
                      Octra_consensus.C_driver.scheduled_validator_set_config) ->
                    {
                      scheduled with
                      validator_set =
                        Octra_consensus.C_types.validator_set_for_epoch
                          ~chain_id
                          ~epoch_id:scheduled.activate_epoch
                          scheduled.validator_set;
                    })
                  scheduled_driver_config_raw
              else
                scheduled_driver_config_raw
            in
            let light_scheduled_validator_set =
              light_scheduled_of_driver scheduled_driver_config
            in
            let active_validator_list = active_vs.validators in
            let next_validator_list =
              match scheduled_driver_config with
              | None -> []
              | Some scheduled -> scheduled.validator_set.validators
            in
            let allowed_pubkeys =
              validator_pubkeys active_vs
              @
              match scheduled_driver_config with
              | None -> []
              | Some scheduled ->
                validator_pubkeys scheduled.validator_set
              |> List.sort_uniq String.compare
            in
            let consensus_config_hash =
              if consensus_mode && active_validator_list <> [] then
                Octra_consensus.C_config.hash
                  ~chain_id
                  ~validator_set:active_vs
                  ?scheduled:light_scheduled_validator_set
                  ?program_trust_hash:cfg.program_trust_hash
                  ?runtime_profile_hash:cfg.runtime_profile_hash
                  ()
              else
                String.make 32 '\x00'
            in
            Ok {
              cfg with
              allowed_pubkeys;
              current_validator_list = active_validator_list;
              next_validator_list;
              active_validator_list;
              active_vs;
              scheduled_driver_config;
              light_scheduled_validator_set;
              consensus_config_hash;
              transition =
                transition_of_persistent
                  ~active_config
                  ~scheduled_config:scheduled_driver_config;
            }
        end
    end

let transition_message ~current_height cfg =
  match cfg.transition with
  | No_transition -> None
  | Already_active { activate_epoch } ->
    Some
      (Printf.sprintf
         "validator_set transition already active at height = %Ld activate_epoch = %Ld"
         current_height
         activate_epoch)
  | Scheduled { activate_epoch; n; quorum; fingerprint } ->
    Some
      (Printf.sprintf
         "validator_set transition scheduled activate_epoch = %Ld n = %d quorum = %d fingerprint = %s"
         activate_epoch
         n
         quorum
         fingerprint)

let contains_validator address validators =
  List.exists
    (fun p -> p.Octra_consensus.C_types.address = address)
    validators

let self_membership ?(permissionless = false) ~address ~voting ~role_label cfg =
  let active = contains_validator address cfg.active_validator_list in
  let scheduled = contains_validator address cfg.next_validator_list in
  match active, scheduled, voting with
  | true, _, _ -> Self_active
  | false, true, _ -> Self_scheduled
  | false, false, true when permissionless ->
    Self_observer
      "validator candidate follows finality until selected by the bonded validator lifecycle"
  | false, false, true ->
    Self_refused
      "REFUSING TO START: wallet is not in active or scheduled validator set"
  | false, false, false ->
    Self_observer
      (Printf.sprintf
         "wallet is not in active or scheduled validator set; role = %s follows finality without voting"
         role_label)

let scheduled_self_warning ~address ~voting cfg =
  if voting
     && not (contains_validator address cfg.active_validator_list)
     && contains_validator address cfg.next_validator_list then
    Some
      "wallet is not in active validator set yet; running catchup/non-voting until scheduled activation"
  else None

let quorum_admission ~allow_unsafe cfg =
  let vs = cfg.active_vs in
  if vs.n >= 4 then Quorum_ok
  else if allow_unsafe then
    Quorum_unsafe_allowed
      (Printf.sprintf
         "OCTRA_ALLOW_UNSAFE_QUORUM = 1 - running with n = %d (unsafe: not real BFT, do not use in production)"
         vs.n)
  else
    Quorum_refused [
      Printf.sprintf
        "REFUSING TO START: BFT consensus requires n >= 4 validators (current: n = %d, f = %d, quorum = %d)."
        vs.n vs.f vs.quorum;
      "With n < 4, BFT safety is broken (any single validator can finalize).";
      "For local single-validator: unset OCTRA_CONSENSUS_MODE.";
      "For testing/devnet only: set OCTRA_ALLOW_UNSAFE_QUORUM = 1 to bypass.";
    ]

let startup_admission ?(permissionless = false) ~address ~voting ~role_label
    ~allow_unsafe_quorum ~next_activation_epoch cfg =
  if cfg.identity_errors <> [] then
    [Startup_refuse cfg.identity_errors]
  else if cfg.allowed_pubkeys = [] then
    [Startup_refuse [
       "REFUSING TO START: consensus mode requires OCTRA_VALIDATORS with valid pubkeys. Set OCTRA_VALIDATORS=addr1:pub1,addr2:pub2,...";
     ]]
  else if cfg.current_validator_list = [] then
    [Startup_refuse [
       "REFUSING TO START: no valid peers parsed from OCTRA_VALIDATORS";
     ]]
  else if cfg.next_validator_list <> [] && next_activation_epoch = None then
    [Startup_refuse [
       "REFUSING TO START: OCTRA_VALIDATORS_NEXT requires OCTRA_VALIDATORS_ACTIVATE_EPOCH";
     ]]
  else
    let self_events =
      match
        self_membership
          ~permissionless
          ~address
          ~voting
          ~role_label
          cfg
      with
      | Self_active
      | Self_scheduled -> []
      | Self_observer message -> [Startup_info message]
      | Self_refused message -> [Startup_refuse [message]]
    in
    match self_events with
    | [Startup_refuse _] -> self_events
    | _ ->
      let scheduled_events =
        match scheduled_self_warning ~address ~voting cfg with
        | None -> []
        | Some message -> [Startup_warn message]
      in
      let quorum_events =
        match quorum_admission ~allow_unsafe:allow_unsafe_quorum cfg with
        | Quorum_ok -> []
        | Quorum_unsafe_allowed message -> [Startup_warn message]
        | Quorum_refused messages -> [Startup_refuse messages]
      in
      self_events @ scheduled_events @ quorum_events

let node_startup_admission ~getenv ~activation_epoch ~address ~voting
    ~role_label cfg =
  match Octra_core.Validator_policy.of_env getenv with
  | Error error -> [Startup_refuse [error]]
  | Ok policy ->
    let next_activation_epoch =
      match cfg.transition with
      | Scheduled { activate_epoch; _ }
      | Already_active { activate_epoch } -> Some activate_epoch
      | No_transition -> activation_epoch ()
    in
    startup_admission
      ~permissionless:(Octra_core.Validator_policy.lifecycle_enabled policy)
      ~address
      ~voting
      ~role_label
      ~allow_unsafe_quorum:(getenv "OCTRA_ALLOW_UNSAFE_QUORUM" = Some "1")
      ~next_activation_epoch
      cfg

let emit_startup_events deps =
  List.iter
    (function
      | Startup_info message -> deps.info message
      | Startup_warn message -> deps.warn message
      | Startup_refuse messages -> deps.refuse messages)

let build_bound ~chain_id ~consensus_mode ~current_height ~current_entries
    ~next_entries ~chain_pending_entries ~next_activation_epoch
    ~program_trust_hash ~runtime_profile_hash =
  let identity_errors =
    Validators.entry_errors ~label:"current" current_entries
    @ Validators.entry_errors ~label:"next" next_entries
    @ Validators.entry_errors ~label:"pending" chain_pending_entries
  in
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
  let active_vs =
    Octra_consensus.C_engine.make_validator_set active_validator_list
    |> Octra_consensus.C_types.validator_set_for_epoch
         ~chain_id
         ~epoch_id:current_height
  in
  let scheduled_driver_config =
    if
      Octra_consensus.C_quorum_policy.active
        ~chain_id
        ~epoch_id:current_height
    then
      Option.map
        (fun
          (scheduled :
            Octra_consensus.C_driver.scheduled_validator_set_config) ->
          {
            scheduled with
            validator_set =
              Octra_consensus.C_types.validator_set_for_epoch
                ~chain_id
                ~epoch_id:scheduled.activate_epoch
                scheduled.validator_set;
          })
        scheduled_driver_config
    else
      scheduled_driver_config
  in
  let light_scheduled_validator_set =
    light_scheduled_of_driver scheduled_driver_config
  in
  let consensus_config_hash =
    if consensus_mode && active_validator_list <> [] then
      Octra_consensus.C_config.hash
        ~chain_id
        ~validator_set:active_vs
        ?scheduled:light_scheduled_validator_set
        ?program_trust_hash
        ?runtime_profile_hash
        ()
    else String.make 32 '\x00'
  in
  {
    allowed_pubkeys;
    identity_errors;
    program_trust_hash;
    runtime_profile_hash;
    current_validator_list;
    next_validator_list;
    active_validator_list;
    active_vs;
    scheduled_driver_config;
    light_scheduled_validator_set;
    consensus_config_hash;
    transition;
  }

let build ~chain_id ~consensus_mode ~current_height ~current_entries
    ~next_entries ~chain_pending_entries ~next_activation_epoch
    ~program_trust_hash =
  build_bound
    ~chain_id
    ~consensus_mode
    ~current_height
    ~current_entries
    ~next_entries
    ~chain_pending_entries
    ~next_activation_epoch
    ~program_trust_hash
    ~runtime_profile_hash:None