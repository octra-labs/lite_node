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

let window = 2L
let window_id = Printf.sprintf "proposal_delay%Ld_inclusion_first" window

let expired ~head ~reference =
  reference < 0L || head < 0L
  || reference <= head && Int64.sub head reference > window

let delivery ~epoch ~head =
  epoch > 0L && head >= 0L && head < epoch
  && not (expired ~head:(Int64.pred epoch) ~reference:head)

let reference ~epoch ~head ~proposal ~parent ~state =
  if not (delivery ~epoch ~head) then Error "head_epoch outside delivery window"
  else match proposal with
  | None -> Error "head_proposal_id missing or invalid"
  | Some proposal when String.length proposal <> 64
      || not (String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) proposal) ->
    Error "head_proposal_id missing or invalid"
  | Some proposal ->
    let expected =
      if head = Int64.pred epoch then
        match parent with
        | Some (parent : Octra_consensus.C_types.parent_commit)
          when parent.certificate.epoch_id = head ->
          Some parent.certificate.proposal_id
        | _ -> None
      else
        Set_fold.find_final head state
        |> Option.map (fun item -> item.Set_fold.proposal_id)
    in
    match expected with
    | None -> Error "head_proposal_id reference unavailable"
    | Some expected ->
      let hex = String.concat "" (List.init (String.length expected)
        (fun index -> Printf.sprintf "%02x" (Char.code expected.[index]))) in
      if proposal = hex then Ok ()
      else Error "head_proposal_id mismatch"

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