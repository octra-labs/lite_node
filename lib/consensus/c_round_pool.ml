(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  mutable epoch : int64 option;
  by_addr : (string, C_codec.round_sync) Hashtbl.t;
  replies : (string, C_codec.round_sync * float) Hashtbl.t;
}

let create () = {
  epoch = None;
  by_addr = Hashtbl.create 32;
  replies = Hashtbl.create 32;
}

let step_rank = function
  | C_types.ProposeStep -> 1
  | C_types.PrevoteStep -> 2
  | C_types.PrecommitStep -> 3

let later (left : C_codec.round_sync) (right : C_codec.round_sync) =
  Int64.compare left.C_codec.epoch_id right.C_codec.epoch_id > 0
  || (Int64.equal left.epoch_id right.epoch_id
      && (left.round > right.round
          || (left.round = right.round
              && ((left.request && not right.request)
                  || (Bool.equal left.request right.request
                      && step_rank left.step > step_rank right.step)))))

let replace t sync =
  match Hashtbl.find_opt t.by_addr sync.C_codec.validator with
  | Some prior when not (later sync prior) -> ()
  | None
  | Some _ -> Hashtbl.replace t.by_addr sync.validator sync

let replace_reply t sync =
  if not sync.C_codec.request then
    match Hashtbl.find_opt t.replies sync.validator with
    | Some (prior, _) when later prior sync -> ()
    | None
    | Some _ ->
      Hashtbl.replace t.replies sync.validator (sync, Unix.gettimeofday ())

let add t (sync : C_codec.round_sync) =
  match t.epoch with
  | None ->
    t.epoch <- Some sync.epoch_id;
    replace t sync;
    replace_reply t sync
  | Some epoch when Int64.equal epoch sync.epoch_id ->
    replace t sync;
    replace_reply t sync
  | Some epoch when Int64.compare sync.epoch_id epoch > 0 ->
    Hashtbl.clear t.by_addr;
    Hashtbl.clear t.replies;
    t.epoch <- Some sync.epoch_id;
    replace t sync;
    replace_reply t sync
  | Some _ -> ()

let reply t ~epoch_id ~validator =
  if not (Option.equal Int64.equal t.epoch (Some epoch_id)) then None
  else Hashtbl.find_opt t.replies validator

let weight validator_set sync =
  C_types.weight_of_addr validator_set sync.C_codec.validator

let order validator_set
    (left : C_codec.round_sync)
    (right : C_codec.round_sync) =
  match weight validator_set left, weight validator_set right with
  | Some left_weight, Some right_weight ->
    let by_weight = Z.compare right_weight left_weight in
    if by_weight <> 0 then by_weight
    else String.compare left.C_codec.validator right.C_codec.validator
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None ->
    String.compare left.C_codec.validator right.C_codec.validator

let take_witness ~chain_id ~epoch_id validator_set syncs =
  let rec take count signed_weight acc = function
    | _ when
        C_types.round_skip_reached_at
          ~chain_id
          ~epoch_id
          validator_set
          ~signer_count:count
          ~signed_weight ->
      List.rev acc
    | [] -> []
    | sync :: rest ->
      match weight validator_set sync with
      | None -> take count signed_weight acc rest
      | Some value ->
        take
          (count + 1)
          (Z.add signed_weight value)
          (sync :: acc)
          rest
  in
  syncs
  |> List.sort (order validator_set)
  |> take 0 Z.zero []

let witness t ~chain_id ~epoch_id ~after_round ~validator_set =
  if not (Option.equal Int64.equal t.epoch (Some epoch_id)) then []
  else
    let by_round = Hashtbl.create 8 in
    Hashtbl.iter
      (fun _ (sync : C_codec.round_sync) ->
        if sync.round > after_round
           && Option.is_some (weight validator_set sync) then
          let prior =
            Option.value
              ~default:[]
              (Hashtbl.find_opt by_round sync.round)
          in
          Hashtbl.replace by_round sync.round (sync :: prior))
      t.by_addr;
    Hashtbl.fold (fun round syncs rounds -> (round, syncs) :: rounds) by_round []
    |> List.sort (fun (left, _) (right, _) -> Int.compare right left)
    |> List.find_map (fun (_, syncs) ->
      match take_witness ~chain_id ~epoch_id validator_set syncs with
      | [] -> None
      | proof -> Some proof)
    |> Option.value ~default:[]