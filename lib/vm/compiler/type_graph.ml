(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

external index : 'a -> 'a array -> Obj.t array = "octra_type_index"

let field graph id slot : int = Obj.obj graph.(id + 2 + slot)
let value graph id = Obj.obj graph.(id)
let at graph id path = List.fold_left (field graph) id path

type cursor = { at : int list -> int; items : int list -> int list option }

let cursor read child id =
  let equal id = let left, right = read id in left == right in
  let at path = List.fold_left (fun id slot ->
    if equal id then id else child id slot) id path in
  let rec items out id =
    if equal id then Some (List.rev (id :: out)) else
    let left, right = read id in
    match (Obj.obj left : Obj.t list), (Obj.obj right : Obj.t list) with
    | _ :: _, _ :: _ -> items (child id 0 :: out) (child id 1)
    | _ -> None in
  {at; items = (fun path -> items [] (at path))}

module Make (Key : sig type t end) = struct
  type t = Key.t
  let same_fields (left : t) (right : t) =
    let left = Obj.repr left and right = Obj.repr right in
    if not (Obj.is_block left && Obj.is_block right)
       || Obj.tag left <> Obj.tag right || Obj.size left <> Obj.size right then false
    else
      let rec equal slot = slot = Obj.size left
        || Obj.field left slot == Obj.field right slot && equal (slot + 1) in
      equal 0

  let mapper ?(saved = [||]) step root =
    let graph = index root (Array.map fst saved) in
    let nodes = Hashtbl.create 32 in
    let rec walk id =
      match Hashtbl.find_opt nodes id with
      | Some result -> result
      | None ->
        let seed : int = Obj.obj graph.(id + 1) in
        let result = if seed < 0 then snd saved.(-1 - seed)
          else step (fun path -> walk (at graph id path)) (value graph id : Key.t) in
        Hashtbl.add nodes id result;
        result
    in
    walk 0

  let compare step left right =
    if left == right || same_fields left right then true else
    let run budget read child seen =
      let rec walk remaining = function
        | [] -> Some true
        | _ when remaining = 0 -> None
        | id :: rest ->
          let left, right = read id in
          if left == right || seen id then walk (remaining - 1) rest else
          match step (cursor read child id) (Obj.obj left : Key.t) (Obj.obj right : Key.t) with
          | None -> Some false
          | Some children -> walk (remaining - 1) (List.rev_append (List.rev children) rest)
      in
      walk budget [0]
    in
    let nodes = Hashtbl.create 32 in
    let add pair = let id = Hashtbl.length nodes in Hashtbl.add nodes id pair; id in
    ignore (add (Obj.repr left, Obj.repr right));
    let read id = Hashtbl.find nodes id in
    let child id slot = let left, right = read id in
      add (Obj.field left slot, Obj.field right slot) in
    match run 64 read child (fun _ -> false) with
    | Some result -> result
    | None ->
      let left_graph = index left [||] and right_graph = index right [||] in
      let pairs = Hashtbl.create 32 and nodes = Hashtbl.create 32 in
      let add pair = match Hashtbl.find_opt pairs pair with
        | Some id -> id
        | None -> let id = Hashtbl.length pairs in
          Hashtbl.add pairs pair id; Hashtbl.add nodes id pair; id in
      ignore (add (0, 0));
      let read id = let left, right = Hashtbl.find nodes id in
        left_graph.(left), right_graph.(right) in
      let child id slot = let left, right = Hashtbl.find nodes id in
        add (field left_graph left slot, field right_graph right slot) in
      let seen = Hashtbl.create 32 in
      let visited id = if Hashtbl.mem seen id then true else (Hashtbl.add seen id (); false) in
      match run max_int read child visited with Some result -> result
      | None -> failwith "type comparison exceeded index capacity"
end

module type Graph = sig
  type t
  val mapper : ?saved:(t * 'a) array -> ((int list -> 'a) -> t -> 'a) -> t -> 'a
  val compare : (cursor -> t -> t -> int list option) -> t -> t -> bool
end

module Host = Make (struct type t = Oct_lang.typ end)
module Core = Make (struct type t = C_syn.typ end)