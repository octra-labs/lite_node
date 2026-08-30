(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type runtime = {
  chain_id : string;
  config_hash : string;
}

type claim = {
  chain_id : string option;
  config_hash : string option;
  catchup_head_epoch : int64 option;
}

let bind f result =
  match result with
  | Error e -> Error e
  | Ok v -> f v

let check_match ~label ~expected = function
  | Some value when value = expected -> Ok ()
  | Some _ -> Error (label ^ " mismatch")
  | None -> Error (label ^ " missing")

let check_catchup ~head_epoch = function
  | Some epoch when Int64.equal epoch head_epoch -> Ok ()
  | Some _ -> Error "catchup_head_epoch mismatch"
  | None -> Error "catchup_head_epoch missing"

let check_prior_catchup ~head_epoch = function
  | Some epoch when Int64.compare epoch head_epoch >= 0 -> Ok ()
  | Some _ -> Error "catchup_head_epoch too low"
  | None -> Error "catchup_head_epoch missing"

let validate_with check ~(runtime : runtime) ~head_epoch (claim : claim) =
  check_match
    ~label:"chain_id"
    ~expected:runtime.chain_id
    claim.chain_id
  |> bind (fun () ->
    check_match
      ~label:"config_hash"
      ~expected:runtime.config_hash
      claim.config_hash)
  |> bind (fun () ->
    check ~head_epoch claim.catchup_head_epoch)

let validate ~(runtime : runtime) ~head_epoch (claim : claim) =
  validate_with check_catchup ~runtime ~head_epoch claim

let validate_prior ~(runtime : runtime) ~head_epoch (claim : claim) =
  validate_with check_prior_catchup ~runtime ~head_epoch claim