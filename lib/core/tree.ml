(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Map = Hashtbl.Make(struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

type performance = {
  node_id : string;
  solved_tasks : int;
  avg_time : float;
  reliability : float;
}

let performance_to_yojson p =
  `Assoc [
    "node_id", `String p.node_id; "solved_tasks", `Int p.solved_tasks;
    "avg_time", `Float p.avg_time; "reliability", `Float p.reliability;
  ]

let performance_of_yojson = function
  | `Assoc fields ->
    let str k = match List.assoc k fields with `String s -> Ok s | _ -> Error ("Expected string for " ^ k) in
    let int k = match List.assoc k fields with `Int i -> Ok i | _ -> Error ("Expected int for " ^ k) in
    let flt k = match List.assoc k fields with `Float f -> Ok f | _ -> Error ("Expected float for " ^ k) in
    (match str "node_id", int "solved_tasks", flt "avg_time", flt "reliability" with
     | Ok node_id, Ok solved_tasks, Ok avg_time, Ok reliability ->
       Ok { node_id; solved_tasks; avg_time; reliability }
     | _ -> Error "Invalid performance JSON")
  | _ -> Error "Expected JSON object for performance"

type t = {
  epoch_id : int;
  parent_commit : string;
  nodes : Node.t Map.t;
  mutable roots : string list;
  mutable root_count : int;
  finalized_by : string;
  performance_summary : performance list;
  mutable commit_signatures : (string * string) list;
  finalized_at : float;
  recent_tx_count : int;
}

let create ~epoch_id ~parent_commit =
  { epoch_id; parent_commit; nodes = Map.create 100; roots = []; root_count = 0;
    finalized_by = ""; performance_summary = []; commit_signatures = [];
    finalized_at = float_of_int (epoch_id * 10); recent_tx_count = 0 }

let add_node t n =
  if Node.score n.Node.metrics < 0.55 then false
  else
    let id = Node.hash n in
    Map.replace t.nodes id n;
    (match n.Node.parent with
     | None ->
       t.roots <- id :: t.roots;
       t.root_count <- t.root_count + 1
     | Some p when Map.mem t.nodes p ->
       let pnode = Map.find t.nodes p in
       Map.replace t.nodes p { pnode with children = id :: pnode.children }
     | _ -> ());
    true

let add_commit_signature t addr signature =
  if not (List.exists (fun (a, _) -> a = addr) t.commit_signatures) then
    t.commit_signatures <- (addr, signature) :: t.commit_signatures

let finalize ?finalized_at t validator perf =
  let finalized_at =
    Option.value finalized_at ~default:(float_of_int (t.epoch_id * 10))
  in
  { t with finalized_by = validator; performance_summary = perf;
    commit_signatures = List.sort (fun (a,_) (b,_) -> String.compare a b) t.commit_signatures;
    finalized_at;
    recent_tx_count = Map.fold (fun _ n acc -> acc + List.length n.Node.txs) t.nodes 0 }

let to_yojson t =
  `Assoc [
    "epoch_id", `Int t.epoch_id;
    "parent_commit", `String t.parent_commit;
    "roots", `List (List.map (fun s -> `String s) t.roots);
    "finalized_by", `String t.finalized_by;
    "commit_signatures", `List (List.map (fun (a, s) -> `List [`String a; `String s]) t.commit_signatures);
    "performance_summary", `List (List.map performance_to_yojson t.performance_summary);
    "nodes", `Assoc (Map.fold (fun k v acc -> (k, Node.to_yojson v) :: acc) t.nodes [] |> List.sort (fun (a,_) (b,_) -> String.compare a b));
    "finalized_at", `Float t.finalized_at;
    "recent_tx_count", `Int t.recent_tx_count;
  ]

let of_yojson = function
  | `Assoc fields ->
    let str k = match List.assoc k fields with `String s -> s | _ -> failwith ("Invalid string field: " ^ k) in
    let int k = match List.assoc k fields with `Int n -> n | _ -> failwith ("Invalid int field: " ^ k) in
    let flt k = match List.assoc k fields with `Float f -> f | _ -> 0. in
    let str_list k = match List.assoc k fields with
      | `List lst -> List.map (function `String s -> s | _ -> failwith "Invalid string list") lst
      | _ -> []
    in
    let commit_sigs k = match List.assoc k fields with
      | `List lst -> List.map (function `List [`String a; `String s] -> (a, s) | _ -> failwith "Invalid commit sig") lst
      | _ -> []
    in
    let perf_summary k = match List.assoc k fields with
      | `List lst -> List.map (fun j -> match performance_of_yojson j with Ok p -> p | _ -> failwith "Invalid performance") lst
      | _ -> []
    in
    let nodes k = match List.assoc k fields with
      | `Assoc node_map ->
        let tbl = Map.create 100 in
        List.iter (fun (id, json) -> match Node.of_yojson json with Ok node -> Map.replace tbl id node | Error _ -> ()) node_map;
        tbl
      | _ -> Map.create 0
    in
    let roots = str_list "roots" in
    Ok { epoch_id = int "epoch_id"; parent_commit = str "parent_commit";
         nodes = nodes "nodes"; roots; root_count = List.length roots;
         finalized_by = str "finalized_by"; performance_summary = perf_summary "performance_summary";
         commit_signatures = commit_sigs "commit_signatures";
         finalized_at = flt "finalized_at"; recent_tx_count = int "recent_tx_count" }
  | _ -> Error "Expected JSON object for tree"

let hash t =
  Digestif.SHA256.digest_string (to_yojson t |> Yojson.Safe.to_string)
  |> Digestif.SHA256.to_hex

let iter_nodes f t = Map.iter f t.nodes
let fold_nodes f t acc = Map.fold f t.nodes acc
let get_node t id = Map.find_opt t.nodes id
let node_ids t = Map.fold (fun k _ acc -> k :: acc) t.nodes []
let root_count t = t.root_count