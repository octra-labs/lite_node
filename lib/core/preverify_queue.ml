(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type status =
  | Pending
  | Ready
  | Failed of string

type item = {
  tx : Transaction.t;
  hash : string;
  lane : Resource_lanes.lane;
  cost : Resource_lanes.used;
  added_at : float;
  status : status;
}

type receipt = {
  hash : string;
  lane : Resource_lanes.lane;
  ok : bool;
  detail : string;
}

type t = {
  budgets : Resource_lanes.lane -> Resource_lanes.budget;
  items : item list;
  used : (Resource_lanes.lane * Resource_lanes.used) list;
}

type admit =
  | Queued of t
  | Duplicate
  | Rejected of string

let create ?(budgets=Resource_lanes.default_budget) () = {
  budgets;
  items = [];
  used = List.map (fun lane -> lane, Resource_lanes.zero) Resource_lanes.all;
}

let used t lane =
  match List.assoc_opt lane t.used with
  | Some v -> v
  | None -> Resource_lanes.zero

let with_used t lane value = {
  t with
  used = List.map (fun (l, u) -> if l = lane then l, value else l, u) t.used;
}

let hash tx =
  Transaction.hash tx

let mem hash (t : t) =
  List.exists (fun (item : item) -> item.hash = hash) t.items

let needs_preverify tx =
  tx.Transaction.op_type
  |> Resource_lanes.of_op
  |> Resource_lanes.preverify_managed

let admit ?now t tx =
  if not (needs_preverify tx) then Rejected "lane_not_preverified"
  else
    let hash = hash tx in
    if mem hash t then Duplicate
    else
      let lane = Resource_lanes.of_op tx.Transaction.op_type in
      let cost = Resource_lanes.cost tx in
      let next = Resource_lanes.add (used t lane) cost in
      match Resource_lanes.over (t.budgets lane) next with
      | Some reason -> Rejected reason
      | None ->
        let item = {
          tx;
          hash;
          lane;
          cost;
          added_at = Option.value now ~default:0.0;
          status = Pending;
        } in
        Queued {
          t with
          items = t.items @ [item];
          used = (with_used t lane next).used;
        }

let pending t =
  t.items
  |> List.filter (fun (item : item) -> item.status = Pending)
  |> List.sort (fun a b ->
    let c = compare a.added_at b.added_at in
    if c <> 0 then c else String.compare a.hash b.hash)

let take_pending ~max_items t =
  if max_items <= 0 then []
  else
    let rec go left acc = function
      | [] -> List.rev acc
      | item :: rest ->
        if left = 0 then List.rev acc
        else go (left - 1) (item :: acc) rest in
    go max_items [] (pending t)

let status t hash =
  List.find_opt (fun (item : item) -> String.equal item.hash hash) t.items
  |> Option.map (fun item -> item.status)

let apply_receipt t receipt =
  let rec go seen acc = function
    | [] -> if seen then Some { t with items = List.rev acc } else None
    | (item : item) :: rest ->
      if item.hash = receipt.hash then
        if item.lane <> receipt.lane then None
        else
          let status =
            if receipt.ok then Ready else Failed receipt.detail in
          go true ({ item with status } :: acc) rest
      else
        go seen (item :: acc) rest in
  go false [] t.items

let apply_det t receipt =
  match Preverify_receipt.validate receipt with
  | Error _ -> None
  | Ok () ->
    apply_receipt t {
      hash = receipt.Preverify_receipt.tx_hash;
      lane = receipt.Preverify_receipt.lane;
      ok = receipt.Preverify_receipt.ok;
      detail = receipt.Preverify_receipt.reason;
    }

let ready_hashes t =
  t.items
  |> List.filter_map (fun (item : item) ->
    match item.status with
    | Ready -> Some item.hash
    | Pending | Failed _ -> None)
  |> List.sort String.compare

let failed t =
  t.items
  |> List.filter_map (fun (item : item) ->
    match item.status with
    | Failed reason -> Some (item.hash, reason)
    | Pending | Ready -> None)
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let rebuild t items =
  let used =
    List.fold_left (fun used (item : item) ->
      let lane_used =
        match List.assoc_opt item.lane used with
        | Some u -> u
        | None -> Resource_lanes.zero in
      let next = Resource_lanes.add lane_used item.cost in
      List.map (fun (lane, u) -> if lane = item.lane then lane, next else lane, u) used)
      (List.map (fun lane -> lane, Resource_lanes.zero) Resource_lanes.all)
      items in
  { t with items; used }

let remove t hashes =
  t.items
  |> List.filter (fun (item : item) ->
       not (List.exists (String.equal item.hash) hashes))
  |> rebuild t

let retain t keep =
  t.items
  |> List.filter keep
  |> rebuild t

let remove_ready t hashes =
  t.items
  |> List.filter (fun (item : item) ->
       match item.status with
       | Ready -> not (List.exists (String.equal item.hash) hashes)
       | Pending | Failed _ -> true)
  |> rebuild t