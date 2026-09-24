(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Update = Octra_core.Validator_set_update

type t = {
  epoch : int64;
  active : bool;
  scheduled : bool;
  activate_epoch : int64 option;
  next_set_epoch : int64 option;
  set_hash : string;
}

let ( let* ) = Result.bind

let read head_epoch = function
  | None -> Ok None
  | Some raw ->
    let* update = Update.of_string raw in
    if update.activate_epoch < 0L
       || Option.fold ~none:false ~some:(fun source -> source > head_epoch)
            update.source_epoch then
      Error "validator set epoch exceeds committed view"
    else Ok (Some update)

let contains ~address ~pubkey (update : Update.t) =
  match List.find_opt (fun (entry : Update.validator_entry) ->
    entry.address = address) update.validators with
  | None -> Ok false
  | Some entry when entry.pubkey_b64 = pubkey -> Ok true
  | Some _ -> Error "committed validator set public key differs"

let current ~head_epoch active pending =
  match active, pending with
  | Some active, _ when active.Update.activate_epoch > head_epoch ->
    Error "active validator set exceeds committed head"
  | Some active, Some pending
      when active.activate_epoch = pending.Update.activate_epoch
           && active.fingerprint <> pending.fingerprint ->
    Error "committed validator sets conflict"
  | active, Some pending when pending.activate_epoch <= head_epoch ->
    Ok (Some (match active with
      | Some active when active.activate_epoch > pending.activate_epoch -> active
      | Some _ | None -> pending))
  | active, _ -> Ok active

let of_values ~head_epoch ~address ~pubkey (active, pending) =
  if head_epoch < 0L then Error "invalid committed validator head"
  else
    let* active = read head_epoch active in
    let* pending = read head_epoch pending in
    let* selected = current ~head_epoch active pending in
    match selected with
    | None -> Ok None
    | Some selected ->
      let* active = contains ~address ~pubkey selected in
      let next = match pending with
        | Some update when update.activate_epoch > head_epoch -> Some update
        | Some _ | None -> None
      in
      let* scheduled = match next with
        | None -> Ok false
        | Some update -> contains ~address ~pubkey update
      in
      let* validators = Update.validator_set selected in
      let next_set_epoch = Option.map (fun (update : Update.t) ->
        update.activate_epoch) next in
      Ok (Some {
        epoch = head_epoch;
        active;
        scheduled;
        activate_epoch = if scheduled then next_set_epoch else None;
        next_set_epoch;
        set_hash = Octra_consensus.C_config.validator_set_hash validators;
      })