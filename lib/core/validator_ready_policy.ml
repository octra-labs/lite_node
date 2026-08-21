(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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

type claim = {
  head_epoch : int64;
  chain_id : string option;
  binary_hash : string option;
  config_hash : string option;
  catchup_head_epoch : int64 option;
  shadow_epochs : int option;
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
  require_binary_hash = false;
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

let validate ~(runtime : runtime) ~(requirements : requirements)
    (claim : claim) =
  check_match
    ~required:requirements.require_chain_id
    ~label:"chain_id"
    ~expected:runtime.chain_id
    claim.chain_id
  |> bind (fun () ->
    check_match
      ~required:requirements.require_binary_hash
      ~label:"binary_hash"
      ~expected:runtime.binary_hash
      claim.binary_hash)
  |> bind (fun () ->
    check_match
      ~required:requirements.require_config_hash
      ~label:"config_hash"
      ~expected:runtime.config_hash
      claim.config_hash)
  |> bind (fun () ->
    check_catchup
      ~required:requirements.require_catchup
      ~head_epoch:claim.head_epoch
      claim.catchup_head_epoch)
  |> bind (fun () ->
    check_shadow
      ~min_epochs:requirements.min_shadow_epochs
      claim.shadow_epochs)

let validate_marker ~runtime ~requirements marker =
  let ready = marker.Validator_set_update.ready in
  let extra = marker.Validator_set_update.extra in
  validate
    ~runtime
    ~requirements
    {
      head_epoch = ready.head_epoch;
      chain_id = extra.chain_id;
      binary_hash = extra.binary_hash;
      config_hash = extra.config_hash;
      catchup_head_epoch = extra.catchup_head_epoch;
      shadow_epochs = extra.shadow_epochs;
    }