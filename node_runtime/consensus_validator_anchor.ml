(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source = {
  getenv : string -> string option;
  chain_id : string;
  current_height : unit -> int64;
  active_raw : unit -> string option;
  pending_raw : unit -> string option;
  relief : int64 -> (Octra_consensus.C_relief.mark option, string) result;
}

let entries source name =
  match source.getenv name with
  | Some raw when String.length raw > 3 ->
    raw
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.filter (fun value -> String.length value > 3)
  | _ ->
    []

let activation_epoch source =
  let parse name =
    match source.getenv name with
    | Some raw ->
      begin
        try Some (Int64.of_string raw)
        with _ -> None
      end
    | None ->
      None
  in
  match parse "OCTRA_VALIDATORS_ACTIVATE_EPOCH" with
  | Some _ as value -> value
  | None -> parse "OCTRA_VALIDATORS_NEXT_EPOCH"

let config source =
  let current_entries = entries source "OCTRA_VALIDATORS" in
  let next_entries = entries source "OCTRA_VALIDATORS_NEXT" in
  let current_height = source.current_height () in
  let active_raw = source.active_raw () in
  let pending_raw = source.pending_raw () in
  let base =
    Validator_config.build
      ~chain_id:source.chain_id
      ~consensus_mode:true
      ~current_height
      ~current_entries
      ~next_entries
      ~chain_pending_entries:
        (Validator_config.pending_entries_of_raw pending_raw)
      ~next_activation_epoch:(activation_epoch source)
      ~program_trust_hash:None
  in
  match base.Validator_config.identity_errors with
  | error :: _ ->
    Error error
  | [] ->
    Validator_config.bind_persistent_updates
      ~chain_id:source.chain_id
      ~consensus_mode:true
      ~current_height
      ~active_raw
      ~pending_raw
      base

let select (config : Validator_config.t) ~epoch =
  match config.scheduled_driver_config with
  | Some scheduled when Int64.compare epoch scheduled.activate_epoch >= 0 ->
    Ok scheduled.validator_set
  | _ ->
    if config.active_vs.Octra_consensus.C_types.n = 0 then
      Error "trusted validator set is empty"
    else Ok config.stake_vs

let stake_set source ~epoch =
  Result.bind (config source) (select ~epoch)

let expected_set ?round source ~epoch =
  let ( let* ) = Result.bind in
  let* config = config source in
  let* stake = select config ~epoch in
  let effective epoch set =
    Octra_consensus.C_types.validator_set_for_epoch
      ~chain_id:source.chain_id ~epoch_id:epoch set in
  let current = effective epoch stake in
  match config.scheduled_driver_config with
  | Some plan when Int64.compare epoch plan.activate_epoch < 0 ->
    let* mark = source.relief epoch in
    (match mark with
     | None -> Ok current
     | Some mark when Int64.compare epoch mark.activate_epoch >= 0 -> Ok current
     | Some mark ->
       let* selected = Octra_consensus.C_relief.restore
         ~chain_id:source.chain_id ~height:epoch ~current
         ~activate_epoch:plan.activate_epoch
         ~target:(effective plan.activate_epoch plan.validator_set)
         ~fingerprint:plan.fingerprint mark in
       (match round with
        | Some round when round < 0 -> Error "pending round is negative"
        | Some round when Int64.equal epoch mark.height && round <= mark.round ->
          Ok current
        | _ -> Ok (Option.value selected ~default:current)))
  | _ -> Ok current