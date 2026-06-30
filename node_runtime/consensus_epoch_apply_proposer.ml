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


module Epochlog = Octra_core.Epochlog

type source =
  | Env
  | Override
  | Pending
  | Finalized_header
  | Epochlog_disk
  | Round_robin_fallback

type missing =
  | Missing_consensus_proposer
  | Missing_fallback_validator

type selected = {
  source : source;
  proposer : Epochlog.proposer_info;
}

type request = {
  epoch_id : int;
  consensus_mode : bool;
  active_validators : string list;
  env_fee_recipient : string option;
  override_proposer : Epochlog.proposer_info option;
  pending_proposer : Epochlog.proposer_info option;
  finalized_header_proposer : Epochlog.proposer_info option;
  disk_epoch : Epochlog.epoch_header option;
}

let source_label = function
  | Env -> "env"
  | Override -> "override"
  | Pending -> "pending"
  | Finalized_header -> "finalized_header"
  | Epochlog_disk -> "epochlog_disk"
  | Round_robin_fallback -> "round_robin_fallback"

let valid_addr addr =
  String.length addr > 3

let valid_proposer proposer =
  valid_addr proposer.Epochlog.creator_addr

let proposer_from_addr ?(commit_round = 0) creator_addr =
  if valid_addr creator_addr then
    Some { Epochlog.creator_addr; commit_round }
  else
    None

let valid_option = function
  | Some proposer when valid_proposer proposer -> Some proposer
  | _ -> None

let proposer_from_disk_epoch epoch =
  match valid_option (Some epoch.Epochlog.proposer) with
  | Some proposer -> Some proposer
  | None ->
    proposer_from_addr
      ~commit_round:epoch.proposer.commit_round
      epoch.finalized_by

let first_valid candidates =
  candidates
  |> List.find_map (fun (source, proposer) ->
    match valid_option proposer with
    | Some proposer -> Some { source; proposer }
    | None -> None)

let fallback request =
  match request.active_validators with
  | [] -> Error Missing_fallback_validator
  | validators ->
    let idx = request.epoch_id mod List.length validators in
    let creator_addr = List.nth validators idx in
    Ok {
      source = Round_robin_fallback;
      proposer = { Epochlog.creator_addr; commit_round = 0 };
    }

let choose request =
  match Option.bind request.env_fee_recipient proposer_from_addr with
  | Some proposer -> Ok { source = Env; proposer }
  | None ->
    let consensus_candidates =
      if request.consensus_mode then
        [
          Pending, request.pending_proposer;
          Finalized_header, request.finalized_header_proposer;
          Epochlog_disk, Option.bind request.disk_epoch proposer_from_disk_epoch;
        ]
      else
        []
    in
    match first_valid ((Override, request.override_proposer) :: consensus_candidates) with
    | Some selected -> Ok selected
    | None when request.consensus_mode -> Error Missing_consensus_proposer
    | None -> fallback request