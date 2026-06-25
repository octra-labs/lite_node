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


type runtime = {
  chain_id : string;
  binary_hash : string;
  config_hash : string;
}

type requirements = {
  require_chain_id : bool;
  require_binary_hash : bool;
  require_config_hash : bool;
  require_catchup : bool;
  min_shadow_epochs : int;
}

let relaxed = {
  require_chain_id = false;
  require_binary_hash = false;
  require_config_hash = false;
  require_catchup = false;
  min_shadow_epochs = 0;
}

let strict ?(min_shadow_epochs = 0) () = {
  require_chain_id = true;
  require_binary_hash = true;
  require_config_hash = true;
  require_catchup = true;
  min_shadow_epochs;
}

let bind f result =
  match result with
  | Error e -> Error e
  | Ok v -> f v

let check_match ~required ~label ~expected = function
  | Some value when value = expected -> Ok ()
  | Some _ -> Error (label ^ " mismatch")
  | None when required -> Error (label ^ " missing")
  | None -> Ok ()

let check_catchup ~required ~head_epoch = function
  | Some epoch when Int64.compare epoch head_epoch >= 0 -> Ok ()
  | Some _ -> Error "catchup_head_epoch too low"
  | None when required -> Error "catchup_head_epoch missing"
  | None -> Ok ()

let check_shadow ~min_epochs = function
  | Some epochs when epochs >= min_epochs -> Ok ()
  | Some _ -> Error "shadow_epochs too low"
  | None when min_epochs > 0 -> Error "shadow_epochs missing"
  | None -> Ok ()

let validate ~runtime ~requirements marker =
  let ready = marker.Validator_set_update.ready in
  let extra = marker.Validator_set_update.extra in
  check_match
    ~required:requirements.require_chain_id
    ~label:"chain_id"
    ~expected:runtime.chain_id
    extra.chain_id
  |> bind (fun () ->
    check_match
      ~required:requirements.require_binary_hash
      ~label:"binary_hash"
      ~expected:runtime.binary_hash
      extra.binary_hash)
  |> bind (fun () ->
    check_match
      ~required:requirements.require_config_hash
      ~label:"config_hash"
      ~expected:runtime.config_hash
      extra.config_hash)
  |> bind (fun () ->
    check_catchup
      ~required:requirements.require_catchup
      ~head_epoch:ready.head_epoch
      extra.catchup_head_epoch)
  |> bind (fun () ->
    check_shadow
      ~min_epochs:requirements.min_shadow_epochs
      extra.shadow_epochs)