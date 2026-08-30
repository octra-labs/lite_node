(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  mutable epoch : int64 option;
  by_addr : (string, C_codec.round_sync) Hashtbl.t;
  by_round : (string, C_codec.round_sync) Hashtbl.t;
  replies : (string, C_codec.round_sync * float) Hashtbl.t;
  sent : (int, C_codec.round_sync) Hashtbl.t;
  mutable sent_top : int option;
}

let create () = {
  epoch = None;
  by_addr = Hashtbl.create 32;
  by_round = Hashtbl.create 128;
  replies = Hashtbl.create 32;
  sent = Hashtbl.create 64;
  sent_top = None;
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

let round_key (sync : C_codec.round_sync) =
  string_of_int sync.round ^ "\x00" ^ sync.validator

let remember_round t sync =
  let key = round_key sync in
  match Hashtbl.find_opt t.by_round key with
  | Some prior when not (later sync prior) -> ()
  | None
  | Some _ -> Hashtbl.replace t.by_round key sync

let trim_rounds t ~current =
  let floor = max 0 (current - C_engine.sync_history_limit) in
  let ceiling = current + C_engine.sync_history_limit in
  Hashtbl.filter_map_inplace
    (fun _ (sync : C_codec.round_sync) ->
      if sync.round >= floor && sync.round <= ceiling then Some sync else None)
    t.by_round

let remember t ~current (sync : C_codec.round_sync) =
  if sync.round >= max 0 (current - C_engine.sync_history_limit)
     && sync.round <= current + C_engine.sync_history_limit
  then begin
    remember_round t sync;
    trim_rounds t ~current
  end

let replace_reply t sync =
  if not sync.C_codec.request then
    match Hashtbl.find_opt t.replies sync.validator with
    | Some (prior, _) when later prior sync -> ()
    | None
    | Some _ ->
      Hashtbl.replace t.replies sync.validator (sync, Unix.gettimeofday ())

let replace_sent t sync =
  match Hashtbl.find_opt t.sent sync.C_codec.round with
  | Some prior when not (later sync prior) -> ()
  | None
  | Some _ -> Hashtbl.replace t.sent sync.round sync

let trim_sent t =
  match t.sent_top with
  | None -> ()
  | Some top ->
    let floor = top - C_engine.sync_history_limit in
    Hashtbl.filter_map_inplace
      (fun round sync -> if round >= floor then Some sync else None)
      t.sent

let keep_sent t sync =
  if not sync.C_codec.request then begin
    let top =
      match t.sent_top with
      | None -> sync.round
      | Some prior -> max prior sync.round
    in
    t.sent_top <- Some top;
    if sync.round >= top - C_engine.sync_history_limit then
      replace_sent t sync;
    trim_sent t
  end

let clear t =
  Hashtbl.clear t.by_addr;
  Hashtbl.clear t.by_round;
  Hashtbl.clear t.replies;
  Hashtbl.clear t.sent;
  t.sent_top <- None

let add_at t ~current (sync : C_codec.round_sync) =
  match t.epoch with
  | None ->
    t.epoch <- Some sync.epoch_id;
    replace t sync;
    replace_reply t sync;
    remember t ~current sync
  | Some epoch when Int64.equal epoch sync.epoch_id ->
    replace t sync;
    replace_reply t sync;
    remember t ~current sync
  | Some epoch when Int64.compare sync.epoch_id epoch > 0 ->
    clear t;
    t.epoch <- Some sync.epoch_id;
    replace t sync;
    replace_reply t sync;
    remember t ~current sync
  | Some _ -> ()

let add t sync =
  add_at t ~current:sync.C_codec.round sync

let add_local t sync =
  add t sync;
  if Option.equal Int64.equal t.epoch (Some sync.C_codec.epoch_id) then
    keep_sent t sync

let reply t ~epoch_id ~validator =
  if not (Option.equal Int64.equal t.epoch (Some epoch_id)) then None
  else Hashtbl.find_opt t.replies validator

let sent_reply t ~epoch_id ~after_round ~through_round =
  if not (Option.equal Int64.equal t.epoch (Some epoch_id)) then None
  else
    Hashtbl.fold
      (fun round sync prior ->
        if round <= after_round || round > through_round then prior
        else
          match prior with
          | None -> Some sync
          | Some saved when later sync saved -> Some sync
          | Some _ -> prior)
      t.sent
      None

let sent_range t ~epoch_id ~after_round ~through_round =
  if not (Option.equal Int64.equal t.epoch (Some epoch_id)) then []
  else
    Hashtbl.fold
      (fun round sync acc ->
        if round > after_round && round <= through_round then sync :: acc else acc)
      t.sent
      []
    |> List.sort (fun left right -> Int.compare left.C_codec.round right.round)

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

let witness t ~chain_id ~epoch_id ~after_round ~through_round ~validator_set =
  if not (Option.equal Int64.equal t.epoch (Some epoch_id)) then []
  else
    let by_round = Hashtbl.create 8 in
    Hashtbl.iter
      (fun _ (sync : C_codec.round_sync) ->
        if sync.round > after_round
           && sync.round <= through_round
           && Option.is_some (weight validator_set sync) then
          let prior =
            Option.value
              ~default:[]
              (Hashtbl.find_opt by_round sync.round)
          in
          Hashtbl.replace by_round sync.round (sync :: prior))
      t.by_round;
    Hashtbl.fold (fun round syncs rounds -> (round, syncs) :: rounds) by_round []
    |> List.sort (fun (left, _) (right, _) -> Int.compare right left)
    |> List.find_map (fun (_, syncs) ->
      match take_witness ~chain_id ~epoch_id validator_set syncs with
      | [] -> None
      | proof -> Some proof)
    |> Option.value ~default:[]