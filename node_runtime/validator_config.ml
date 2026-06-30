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

type persistent_update_deps = {
  read_pending : unit -> string option Lwt.t;
  read_marker : string -> string option Lwt.t;
  log_invalid : string -> unit;
  log_waiting :
    activate_epoch:int64 ->
    missing:string list ->
    fingerprint:string ->
    unit;
}

type persistent_update_node_deps = {
  read_pending : unit -> string option Lwt.t;
  read_marker : string -> string option Lwt.t;
  warn : string -> unit;
  current_height : unit -> int64;
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

let pending_entries_of_raw = function
  | None -> []
  | Some raw ->
    match Octra_core.Validator_set_update.of_string raw with
    | Error _ -> []
    | Ok update ->
      List.map
        (fun v ->
          v.Octra_core.Validator_set_update.address ^ ":" ^ v.pubkey_b64)
        update.validators

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

let short_fingerprint fingerprint =
  String.sub fingerprint 0 (min 16 (String.length fingerprint))

let load_persistent_update (deps : persistent_update_deps) ~runtime
    ~requirements ~current_height =
  let open Lwt.Syntax in
  let* pending_opt = deps.read_pending () in
  match pending_opt with
  | None -> Lwt.return_none
  | Some raw ->
    match Octra_core.Validator_set_update.of_string raw with
    | Error e ->
      deps.log_invalid e;
      Lwt.return_none
    | Ok update ->
      let* readiness =
        Lwt_list.map_s
          (fun v ->
            let key =
              Octra_core.Validator_set_update.ready_meta_key
                ~fingerprint:update.fingerprint
                ~address:v.Octra_core.Validator_set_update.address
            in
            let* marker = deps.read_marker key in
            Lwt.return (v, marker))
          update.validators
      in
      let missing =
        readiness_missing
          ~runtime
          ~requirements
          ~update
          readiness
      in
      if missing <> [] then begin
        if Int64.compare current_height update.activate_epoch >= 0 then
          deps.log_waiting
            ~activate_epoch:update.activate_epoch
            ~missing
            ~fingerprint:(short_fingerprint update.fingerprint);
        Lwt.return_none
      end else
        Lwt.return (driver_config_of_update update)

let load_node_persistent_update (deps : persistent_update_node_deps) ~runtime
    ~requirements =
  let update_deps = {
    read_pending = deps.read_pending;
    read_marker = deps.read_marker;
    log_invalid = (fun e ->
      deps.warn
        (Printf.sprintf "persistent validator_set_update ignored: %s" e));
    log_waiting = (fun ~activate_epoch ~missing ~fingerprint ->
      deps.warn
        (Printf.sprintf
           "persistent validator_set_update waiting for readiness activate_epoch = %Ld missing = %s fingerprint = %s"
           activate_epoch
           (String.concat "," missing)
           fingerprint));
  } in
  load_persistent_update
    update_deps
    ~runtime
    ~requirements
    ~current_height:(deps.current_height ())

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

let self_membership ~address ~voting ~role_label cfg =
  let active = contains_validator address cfg.active_validator_list in
  let scheduled = contains_validator address cfg.next_validator_list in
  match active, scheduled, voting with
  | true, _, _ -> Self_active
  | false, true, _ -> Self_scheduled
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
      "wallet is not in active validator set yet; running catchup /non-voting until scheduled activation"
  else None

let quorum_admission ~allow_unsafe cfg =
  let vs = cfg.active_vs in
  if vs.n >= 4 then Quorum_ok
  else if allow_unsafe then
    Quorum_unsafe_allowed
      (Printf.sprintf
         "OCTRA_ALLOW_UNSAFE_QUORUM = 1 - running with n = %d (unsafe: not real lite-BFT, only for testing or [hidden], do not use in prod)"
         vs.n)
  else
    Quorum_refused [
      Printf.sprintf
        "REFUSING TO START: lite consensus requires n >= 4 validators (current: n = %d, f = %d, quorum = %d)."
        vs.n vs.f vs.quorum;
    ]



(*
  For all [o_dev_labs] - if you write a reason, write a link to the problem right away and immediately note whether it is closed or not 
  (if not closed, analyze it and add a commit to OC7 with an exact indication of the commit and also attach all the tests that led to the cause, 
  also try to add an opinion on the correction or improvement, do not make entries or corrections without a clear description. 

  It is not the first time an issue has been left open after the weekend without a clear definition.

  12.02.2025
*)

let startup_admission ~address ~voting ~role_label ~allow_unsafe_quorum
    ~next_activation_epoch cfg =
  if cfg.allowed_pubkeys = [] then
    [Startup_refuse [
       "REFUSING TO START: consensus mode requires OCTRA_VALIDATORS with valid pubkeys. Set OCTRA_VALIDATORS = addr1:pub1, addr2:pub2, ... (request a build with a version higher than the current one for D. & J.";
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
      match self_membership ~address ~voting ~role_label cfg with
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
  startup_admission
    ~address
    ~voting
    ~role_label
    ~allow_unsafe_quorum:(getenv "OCTRA_ALLOW_UNSAFE_QUORUM" = Some "1")
    ~next_activation_epoch:(activation_epoch ())
    cfg

let emit_startup_events deps =
  List.iter
    (function
      | Startup_info message -> deps.info message
      | Startup_warn message -> deps.warn message
      | Startup_refuse messages -> deps.refuse messages)

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