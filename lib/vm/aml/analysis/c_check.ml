(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Unknown of C_term.id
  | Used of C_term.id
  | Ghost of C_term.id
  | Shadow of C_term.id
  | Need of C_type.t * C_type.t
  | Bad of C_type.t
  | Byte_index of C_nat.t * C_nat.t
  | Vec_index of C_nat.t * C_nat.t
  | Seq_size of C_nat.t * C_nat.t
  | Range_type of C_type.t
  | Size
  | Mode of C_term.id * C_type.mul
  | Split of C_term.id list
  | Repeat of C_term.id list
  | Erase of C_term.id list
  | Unused of C_term.id
  | Impure of C_term.id
  | Access of C_type.t
  | Drop_res of C_type.t
  | Depth of int * int
  | Nodes of int * int
  | Inputs of int * int

type origin =
  | Direct
  | Held of C_nat.t

type site = {
  atom : C_eff.atom;
  payload : C_type.t;
  origin : origin;
}

type flow =
  | Pure
  | Action of site
  | Seq of flow * flow
  | Fork of flow * flow
  | Loop of C_nat.t * flow

type location = {
  index : int;
  site : site;
  count : Z.t;
}

type info = {
  typ : C_type.t;
  eff : C_eff.t;
  res : C_limit.t;
  flow : flow;
}

type selected = {
  rule : C_rule.id;
  info : info;
}

type failure = {
  error : error;
  term : C_term.t option;
}

type rule_error =
  | Rule of C_rule.error
  | Check of error

type slot = {
  bind : C_term.bind;
  live : bool;
}

let max_depth = C_rule.local.tm_depth
let max_nodes = C_rule.local.tm_nodes
let max_inputs = C_rule.local.inputs

let ( let* ) value next =
  match value with
  | Ok item -> next item
  | Error error -> Error error

let need expected actual =
  if not (C_type.valid expected) then Error (Bad expected)
  else if not (C_type.valid actual) then Error (Bad actual)
  else if C_type.equal expected actual then Ok ()
  else Error (Need (expected, actual))

let rec has id = function
  | [] -> false
  | slot :: rest -> C_nat.equal slot.bind.id id || has id rest

let rec use id left = function
  | [] -> Error (Unknown id)
  | slot :: right when C_nat.equal slot.bind.id id ->
    begin
      match slot.bind.mul, slot.live with
      | C_type.Zero, _ -> Error (Ghost id)
      | C_type.One, false -> Error (Used id)
      | C_type.One, true ->
        let next = { slot with live = false } in
        Ok (slot.bind.typ, List.rev_append left (next :: right))
      | C_type.Many, _ ->
        Ok (slot.bind.typ, List.rev_append left (slot :: right))
    end
  | slot :: right -> use id (slot :: left) right

let rec drop id left = function
  | [] -> List.rev left
  | slot :: right when C_nat.equal slot.bind.id id -> List.rev_append left right
  | slot :: right -> drop id (slot :: left) right

let rec find id = function
  | [] -> None
  | slot :: _ when C_nat.equal slot.bind.id id -> Some slot
  | _ :: rest -> find id rest

let slot_equal left right =
  C_nat.equal left.bind.id right.bind.id
  && left.bind.mul = right.bind.mul
  && C_type.equal left.bind.typ right.bind.typ
  && left.live = right.live

let rec env_equal left right =
  match left, right with
  | [], [] -> true
  | l :: ls, r :: rs -> slot_equal l r && env_equal ls rs
  | _ -> false

let split_ids left right =
  let changed slot =
    match find slot.bind.id right with
    | Some peer -> not (slot_equal slot peer)
    | None -> true
  in
  let left_ids = List.filter_map (fun slot -> if changed slot then Some slot.bind.id else None) left in
  let right_ids = List.filter_map (fun slot -> if has slot.bind.id left then None else Some slot.bind.id) right in
  List.sort_uniq C_nat.compare (left_ids @ right_ids)

let push depth terms stack =
  List.fold_left (fun rest term -> (depth, term) :: rest) stack terms

let shape term =
  let rec walk nodes = function
    | [] -> Ok ()
    | (depth, term) :: rest ->
      if depth > max_depth then Error (Depth (max_depth, depth))
      else if nodes >= max_nodes then Error (Nodes (max_nodes, nodes + 1))
      else
        let next = depth + 1 in
        let rest =
          match term with
          | C_term.Unit | C_term.Bool _ | C_term.Int _ | C_term.Narrow _
          | C_term.Bytes _
          | C_term.Var _ -> rest
          | C_term.Vec (_, values) | C_term.Seq (_, _, values) ->
            push next values rest
          | C_term.Let (_, value, body)
          | C_term.Pair (value, body)
          | C_term.Add (value, body)
          | C_term.Sub (value, body)
          | C_term.Mul (value, body)
          | C_term.Div (value, body)
          | C_term.Mod (value, body)
          | C_term.Eq (_, value, body)
          | C_term.Cmp (_, value, body)
          | C_term.Cat (value, body)
          | C_term.Vcat (value, body)
          | C_term.Step (value, body) ->
            (next, value) :: (next, body) :: rest
          | C_term.If (guard, yes, no) ->
            (next, guard) :: (next, yes) :: (next, no) :: rest
          | C_term.Unpair (pair, _, _, body) ->
            (next, pair) :: (next, body) :: rest
          | C_term.Fst value | C_term.Snd value
          | C_term.Inl (value, _) | C_term.Inr (_, value)
          | C_term.Act (_, value) | C_term.Neg value | C_term.Abs value
          | C_term.Fit (_, value) | C_term.Wide value | C_term.Length value
          | C_term.Take (_, value)
          | C_term.Drop (_, value) | C_term.At (_, value)
          | C_term.Uncons value | C_term.Close value ->
            (next, value) :: rest
          | C_term.Case (value, _, yes, _, no) ->
            (next, value) :: (next, yes) :: (next, no) :: rest
          | C_term.Vfold (vector, seed, fold) ->
            (next, vector) :: (next, seed) :: (next, fold.body) :: rest
        in
        walk (nodes + 1) rest
  in
  walk 0 [0, term]

let open_bind (bind : C_term.bind) env =
  if not (C_nat.valid bind.id) then Error Size
  else if not (C_type.valid bind.typ) then Error (Bad bind.typ)
  else if C_type.kind bind.typ = C_type.Res && bind.mul <> C_type.One then
    Error (Mode (bind.id, bind.mul))
  else if has bind.id env then Error (Shadow bind.id)
  else
    match bind.mul with
    | C_type.Zero -> Ok env
    | C_type.One | C_type.Many -> Ok ({ bind; live = true } :: env)

let close_bind (bind : C_term.bind) env =
  match bind.mul with
  | C_type.Zero -> Ok env
  | C_type.One ->
    begin
      match find bind.id env with
      | Some { live = true; _ } -> Error (Unused bind.id)
      | Some _ -> Ok (drop bind.id [] env)
      | None -> Error (Unknown bind.id)
    end
  | C_type.Many ->
    begin
      match find bind.id env with
      | Some _ -> Ok (drop bind.id [] env)
      | None -> Error (Unknown bind.id)
    end

let item typ res = { typ; eff = C_eff.empty; res; flow = Pure }

let leaf typ = item typ C_limit.one

let flow_seq left right =
  match left, right with
  | Pure, flow | flow, Pure -> flow
  | _ -> Seq (left, right)

let flow_fork left right =
  match left, right with
  | Pure, Pure -> Pure
  | _ -> Fork (left, right)

let flow_loop len flow =
  match flow with
  | Pure -> Pure
  | _ -> Loop (len, flow)

let seq typ left right =
  {
    typ;
    eff = C_eff.union left.eff right.eff;
    res = C_limit.succ (C_limit.add left.res right.res);
    flow = flow_seq left.flow right.flow;
  }

let join typ guard yes no =
  {
    typ;
    eff = C_eff.union guard.eff (C_eff.union yes.eff no.eff);
    res = C_limit.succ (C_limit.add guard.res (C_limit.max yes.res no.res));
    flow = flow_seq guard.flow (flow_fork yes.flow no.flow);
  }

let copy_seq typ len left right =
  {
    typ;
    eff = C_eff.union left.eff right.eff;
    res = C_limit.add (C_limit.make (C_nat.to_z len))
      (C_limit.succ (C_limit.add left.res right.res));
    flow = flow_seq left.flow right.flow;
  }

let copy_one typ len value =
  {
    typ;
    eff = value.eff;
    res = C_limit.add (C_limit.make (C_nat.to_z len)) (C_limit.succ value.res);
    flow = value.flow;
  }

let fold_info len vector seed body =
  let turns = C_limit.scale (C_nat.to_z len) (C_limit.succ body.res) in
  {
    typ = seed.typ;
    eff = C_eff.union vector.eff (C_eff.union seed.eff body.eff);
    res = C_limit.succ (C_limit.add vector.res (C_limit.add seed.res turns));
    flow = flow_seq vector.flow
      (flow_seq seed.flow (flow_loop len body.flow));
  }

let locations info =
  let rec walk index out = function
    | [] -> List.rev out
    | (_, Pure) :: rest -> walk index out rest
    | (count, Action site) :: rest ->
      walk (index + 1) ({ index; site; count } :: out) rest
    | (count, Seq (left, right)) :: rest
    | (count, Fork (left, right)) :: rest ->
      walk index out ((count, left) :: (count, right) :: rest)
    | (count, Loop (len, body)) :: rest ->
      let count = Z.mul count (C_nat.to_z len) in
      walk index out ((count, body) :: rest)
  in
  walk 0 [] [Z.one, info.flow]

let sites info = List.map (fun location -> location.site) (locations info)

exception Infer_error of error * C_term.t

let rec infer env term =
  match infer_node env term with
  | Ok value -> Ok value
  | Error error -> raise (Infer_error (error, term))

and infer_node env term =
  match term with
  | C_term.Unit -> Ok (leaf C_type.Unit, env)
  | C_term.Bool _ -> Ok (leaf C_type.Bool, env)
  | C_term.Int _ -> Ok (leaf C_type.Int, env)
  | C_term.Narrow (typ, value) ->
    begin
      match typ with
      | C_type.Num _ when C_type.valid typ && C_type.admits typ value ->
        Ok (leaf typ, env)
      | _ -> Error (Range_type typ)
    end
  | C_term.Bytes value ->
    let* len =
      match C_nat.of_int (String.length value) with
      | Some len -> Ok len
      | None -> Error Size
    in
    Ok (item (C_type.Bytes len)
      (C_limit.succ (C_limit.make (C_nat.to_z len))), env)
  | C_term.Vec (elem, values) ->
    let* () = if C_type.valid elem then Ok () else Error (Bad elem) in
    let* len =
      match C_nat.of_int (List.length values) with
      | Some len -> Ok len
      | None -> Error Size
    in
    let* eff, res, flow, next_env = infer_vec elem env values in
    Ok ({ typ = C_type.Vec (len, elem); eff; res; flow }, next_env)
  | C_term.Seq (cap, elem, values) ->
    let typ = C_type.Seq (cap, elem) in
    let* () = if C_type.valid typ then Ok () else Error (Bad typ) in
    let* len =
      match C_nat.of_int (List.length values) with
      | Some len -> Ok len
      | None -> Error Size
    in
    if not (C_nat.le len cap) then Error (Seq_size (len, cap))
    else
      let* eff, res, flow, next_env = infer_vec elem env values in
      let* pad =
        match C_nat.sub cap len with
        | Some pad -> Ok pad
        | None -> Error (Seq_size (len, cap))
      in
      let res = C_limit.add res
        (C_limit.make (Z.succ (C_nat.to_z pad))) in
      Ok ({ typ; eff; res; flow }, next_env)
  | C_term.Var id ->
    if not (C_nat.valid id) then Error Size
    else
      let* typ, next_env = use id [] env in
      Ok (leaf typ, next_env)
  | C_term.Let (bind, value, body) ->
    let* () = if C_type.valid bind.typ then Ok () else Error (Bad bind.typ) in
    let* value_info, value_env = infer env value in
    let* () = need bind.typ value_info.typ in
    begin
      match bind.mul with
      | C_type.Zero ->
        if not (C_eff.equal value_info.eff C_eff.empty) then Error (Impure bind.id)
        else if not (env_equal env value_env) then
          Error (Erase (split_ids env value_env))
        else
          let* body_env = open_bind bind env in
          let* body_info, next_env = infer body_env body in
          let* next_env = close_bind bind next_env in
          let flow = flow_seq value_info.flow body_info.flow in
          Ok ({ body_info with flow; res = C_limit.succ body_info.res }, next_env)
      | C_type.One | C_type.Many ->
        let* body_env = open_bind bind value_env in
        let* body_info, next_env = infer body_env body in
        let* next_env = close_bind bind next_env in
        let eff = C_eff.union value_info.eff body_info.eff in
        let res = C_limit.succ (C_limit.add value_info.res body_info.res) in
        let flow = flow_seq value_info.flow body_info.flow in
        Ok ({ body_info with eff; res; flow }, next_env)
    end
  | C_term.If (guard, yes, no) ->
    let* guard_info, guard_env = infer env guard in
    let* () = need C_type.Bool guard_info.typ in
    let* yes_info, yes_env = infer guard_env yes in
    let* no_info, no_env = infer guard_env no in
    let* () = need yes_info.typ no_info.typ in
    if env_equal yes_env no_env then
      Ok (join yes_info.typ guard_info yes_info no_info, yes_env)
    else Error (Split (split_ids yes_env no_env))
  | C_term.Pair (left, right) ->
    let* left_info, left_env = infer env left in
    let* right_info, right_env = infer left_env right in
    Ok (seq (C_type.Pair (left_info.typ, right_info.typ)) left_info right_info, right_env)
  | C_term.Unpair (pair, left, right, body) ->
    let* pair_info, pair_env = infer env pair in
    begin
      match pair_info.typ with
      | C_type.Pair (left_type, right_type) ->
        let* () = need left_type left.typ in
        let* () = need right_type right.typ in
        let* body_env = open_bind left pair_env in
        let* body_env = open_bind right body_env in
        let* body_info, next_env = infer body_env body in
        let* next_env = close_bind right next_env in
        let* next_env = close_bind left next_env in
        Ok (seq body_info.typ pair_info body_info, next_env)
      | actual -> Error (Need (C_type.Pair (C_type.Unit, C_type.Unit), actual))
    end
  | C_term.Fst pair ->
    let* pair_info, next_env = infer env pair in
    begin
      match pair_info.typ with
      | C_type.Pair (_, right) when C_type.kind right = C_type.Res ->
        Error (Drop_res right)
      | C_type.Pair (left, _) ->
        Ok ({ pair_info with typ = left; res = C_limit.succ pair_info.res }, next_env)
      | actual -> Error (Need (C_type.Pair (C_type.Unit, C_type.Unit), actual))
    end
  | C_term.Snd pair ->
    let* pair_info, next_env = infer env pair in
    begin
      match pair_info.typ with
      | C_type.Pair (left, _) when C_type.kind left = C_type.Res ->
        Error (Drop_res left)
      | C_type.Pair (_, right) ->
        Ok ({ pair_info with typ = right; res = C_limit.succ pair_info.res }, next_env)
      | actual -> Error (Need (C_type.Pair (C_type.Unit, C_type.Unit), actual))
    end
  | C_term.Inl (value, right) ->
    let* () = if C_type.valid right then Ok () else Error (Bad right) in
    let* value_info, next_env = infer env value in
    let info = {
      value_info with
      typ = C_type.Sum (value_info.typ, right);
      res = C_limit.succ value_info.res;
    } in
    Ok (info, next_env)
  | C_term.Inr (left, value) ->
    let* () = if C_type.valid left then Ok () else Error (Bad left) in
    let* value_info, next_env = infer env value in
    let info = {
      value_info with
      typ = C_type.Sum (left, value_info.typ);
      res = C_limit.succ value_info.res;
    } in
    Ok (info, next_env)
  | C_term.Case (value, left, yes, right, no) ->
    let* value_info, value_env = infer env value in
    begin
      match value_info.typ with
      | C_type.Sum (left_type, right_type) ->
        let* () = need left_type left.typ in
        let* () = need right_type right.typ in
        let* yes_env = open_bind left value_env in
        let* yes_info, yes_env = infer yes_env yes in
        let* yes_env = close_bind left yes_env in
        let* no_env = open_bind right value_env in
        let* no_info, no_env = infer no_env no in
        let* no_env = close_bind right no_env in
        let* () = need yes_info.typ no_info.typ in
        if env_equal yes_env no_env then
          Ok (join yes_info.typ value_info yes_info no_info, yes_env)
        else Error (Split (split_ids yes_env no_env))
      | actual -> Error (Need (C_type.Sum (C_type.Unit, C_type.Unit), actual))
    end
  | C_term.Act (atom, body) ->
    if not (C_eff.valid atom) then Error Size
    else
      let* body_info, next_env = infer env body in
      let info = {
        body_info with
        eff = C_eff.add atom body_info.eff;
        res = C_limit.succ body_info.res;
        flow = flow_seq
          (Action { atom; payload = body_info.typ; origin = Direct })
          body_info.flow;
      } in
      Ok (info, next_env)
  | C_term.Add (left, right)
  | C_term.Sub (left, right)
  | C_term.Mul (left, right)
  | C_term.Div (left, right)
  | C_term.Mod (left, right) ->
    let* left_info, left_env = infer env left in
    let* () = need C_type.Int left_info.typ in
    let* right_info, right_env = infer left_env right in
    let* () = need C_type.Int right_info.typ in
    Ok (seq C_type.Int left_info right_info, right_env)
  | C_term.Neg value | C_term.Abs value ->
    let* info, next_env = infer env value in
    let* () = need C_type.Int info.typ in
    Ok ({ info with res = C_limit.succ info.res }, next_env)
  | C_term.Fit (target, value) ->
    let* () =
      match target with
      | C_type.Num _ when C_type.valid target -> Ok ()
      | _ -> Error (Range_type target)
    in
    let* info, next_env = infer env value in
    let* () = need C_type.Int info.typ in
    let typ = C_type.Sum (target, C_type.Int) in
      let cost = C_limit.used ~steps:Z.one ~work:(Z.of_int 12) in
      let res = C_limit.add info.res cost in
    Ok ({ info with typ; res }, next_env)
  | C_term.Wide value ->
    let* info, next_env = infer env value in
    begin
      match info.typ with
      | C_type.Num _ ->
        Ok ({ info with typ = C_type.Int; res = C_limit.succ info.res }, next_env)
      | typ -> Error (Range_type typ)
    end
  | C_term.Length value ->
    let* info, next_env = infer env value in
    begin
      match info.typ with
      | C_type.Seq _ ->
        Ok ({ info with typ = C_type.Int; res = C_limit.succ info.res }, next_env)
      | typ -> Error (Need (C_type.Seq (C_nat.zero, C_type.Unit), typ))
    end
  | C_term.Eq (typ, left, right) ->
    let* left_info, left_env = infer env left in
    let* right_info, right_env = infer left_env right in
    let* () = need typ left_info.typ in
    let* () = need typ right_info.typ in
    if not (C_type.equatable typ) then Error (Access typ)
    else
      let info = seq C_type.Bool left_info right_info in
      let res = C_limit.add info.res (C_limit.effort (C_type.eq_work typ)) in
      Ok ({ info with res }, right_env)
  | C_term.Cmp (_, left, right) ->
    let* left_info, left_env = infer env left in
    let* () = need C_type.Int left_info.typ in
    let* right_info, right_env = infer left_env right in
    let* () = need C_type.Int right_info.typ in
    Ok (seq C_type.Bool left_info right_info, right_env)
  | C_term.Cat (left, right) ->
    let* left_info, left_env = infer env left in
    let* right_info, right_env = infer left_env right in
    begin
      match left_info.typ, right_info.typ with
      | C_type.Bytes left_len, C_type.Bytes right_len ->
        begin
          match C_type.add_len left_len right_len with
          | Some len ->
            Ok (copy_seq (C_type.Bytes len) len left_info right_info, right_env)
          | None -> Error Size
        end
      | C_type.Bytes _, actual -> Error (Need (C_type.Bytes C_nat.zero, actual))
      | actual, _ -> Error (Need (C_type.Bytes C_nat.zero, actual))
    end
  | C_term.Take (len, value) ->
    let* value_info, next_env = infer env value in
    begin
      match value_info.typ with
      | C_type.Bytes total when C_nat.le len total ->
        Ok (copy_one (C_type.Bytes len) len value_info, next_env)
      | C_type.Bytes total -> Error (Byte_index (len, total))
      | actual -> Error (Need (C_type.Bytes C_nat.zero, actual))
    end
  | C_term.Drop (len, value) ->
    let* value_info, next_env = infer env value in
    begin
      match value_info.typ with
      | C_type.Bytes total ->
        begin
          match C_nat.sub total len with
          | Some rest ->
            Ok (copy_one (C_type.Bytes rest) rest value_info, next_env)
          | None -> Error (Byte_index (len, total))
        end
      | actual -> Error (Need (C_type.Bytes C_nat.zero, actual))
    end
  | C_term.Vcat (left, right) ->
    let* left_info, left_env = infer env left in
    let* right_info, right_env = infer left_env right in
    begin
      match left_info.typ, right_info.typ with
      | C_type.Vec (left_len, left_elem), C_type.Vec (right_len, right_elem) ->
        let* () = need left_elem right_elem in
        begin
          match C_type.add_len left_len right_len with
          | Some len ->
            Ok (copy_seq (C_type.Vec (len, left_elem)) len left_info right_info, right_env)
          | None -> Error Size
        end
      | C_type.Vec _, actual ->
        Error (Need (C_type.Vec (C_nat.zero, C_type.Unit), actual))
      | actual, _ -> Error (Need (C_type.Vec (C_nat.zero, C_type.Unit), actual))
    end
  | C_term.At (index, value) ->
    let* value_info, next_env = infer env value in
    begin
      match value_info.typ with
      | C_type.Vec (len, _) when not (C_nat.lt index len) ->
        Error (Vec_index (index, len))
      | C_type.Vec (_, elem) when C_type.kind elem = C_type.Res ->
        Error (Access elem)
      | C_type.Vec (_, elem) ->
        Ok ({ value_info with typ = elem; res = C_limit.succ value_info.res }, next_env)
      | actual -> Error (Need (C_type.Vec (C_nat.zero, C_type.Unit), actual))
    end
  | C_term.Uncons value ->
    let* value_info, next_env = infer env value in
    begin
      match value_info.typ with
      | C_type.Vec (len, elem) ->
        begin
          match C_nat.sub len C_nat.one with
          | Some rest ->
            let typ = C_type.Pair (elem, C_type.Vec (rest, elem)) in
            Ok (copy_one typ rest value_info, next_env)
          | None -> Error (Vec_index (C_nat.zero, C_nat.zero))
        end
      | actual -> Error (Need (C_type.Vec (C_nat.zero, C_type.Unit), actual))
    end
  | C_term.Vfold (vector, seed, fold) ->
    let* vector_info, vector_env = infer env vector in
    begin
      match vector_info.typ with
      | C_type.Vec (len, elem) | C_type.Seq (len, elem) ->
        let* () = need elem fold.item.typ in
        let* seed_info, seed_env = infer vector_env seed in
        let* () = need seed_info.typ fold.state.typ in
        let* body_env = open_bind fold.item seed_env in
        let* body_env = open_bind fold.state body_env in
        let* body_info, body_env = infer body_env fold.body in
        let* () = need seed_info.typ body_info.typ in
        let* body_env = close_bind fold.state body_env in
        let* body_env = close_bind fold.item body_env in
        if env_equal seed_env body_env then
          Ok (fold_info len vector_info seed_info body_info, seed_env)
        else Error (Repeat (split_ids seed_env body_env))
      | actual -> Error (Need (C_type.Vec (C_nat.zero, C_type.Unit), actual))
    end
  | C_term.Step (cap, value) ->
    let* cap_info, cap_env = infer env cap in
    let* value_info, next_env = infer cap_env value in
    begin
      match cap_info.typ with
      | C_type.Cap kind ->
        let info = seq (C_type.Pair (cap_info.typ, value_info.typ)) cap_info value_info in
        let action = Action {
          atom = C_eff.Write kind;
          payload = value_info.typ;
          origin = Held kind;
        } in
        Ok ({ info with
          eff = C_eff.add (C_eff.Write kind) info.eff;
          flow = flow_seq info.flow action;
        }, next_env)
      | actual -> Error (Need (C_type.Cap C_nat.zero, actual))
    end
  | C_term.Close cap ->
    let* cap_info, next_env = infer env cap in
    begin
      match cap_info.typ with
      | C_type.Cap kind ->
        let info = {
          typ = C_type.Unit;
          eff = C_eff.add (C_eff.Close kind) cap_info.eff;
          res = C_limit.succ cap_info.res;
          flow = flow_seq cap_info.flow
            (Action {
              atom = C_eff.Close kind;
              payload = C_type.Unit;
              origin = Held kind;
            });
        } in
        Ok (info, next_env)
      | actual -> Error (Need (C_type.Cap C_nat.zero, actual))
    end

and infer_vec elem env = function
  | [] -> Ok (C_eff.empty, C_limit.one, Pure, env)
  | value :: rest ->
    let* value_info, next_env = infer env value in
    let* () = need elem value_info.typ in
    let* eff, res, flow, next_env = infer_vec elem next_env rest in
    Ok (C_eff.union value_info.eff eff,
      C_limit.succ (C_limit.add value_info.res res),
      flow_seq value_info.flow flow, next_env)

let rec open_all env = function
  | [] -> Ok env
  | bind :: rest ->
    let* env = open_bind bind env in
    open_all env rest

let rec pending = function
  | [] -> None
  | { bind = { id; mul = C_type.One; _ }; live = true } :: _ -> Some id
  | _ :: rest -> pending rest

let rec input_limit count = function
  | [] -> Ok ()
  | _ when count = max_inputs -> Error (Inputs (max_inputs, max_inputs + 1))
  | _ :: rest -> input_limit (count + 1) rest

let rec binding_term id = function
  | C_term.Let (bind, _, _) as term when C_nat.equal bind.id id -> Some term
  | C_term.Unpair (_, left, right, _) as term
      when C_nat.equal left.id id || C_nat.equal right.id id -> Some term
  | C_term.Case (_, left, _, right, _) as term
      when C_nat.equal left.id id || C_nat.equal right.id id -> Some term
  | C_term.Vfold (_, _, fold) as term
      when C_nat.equal fold.item.id id || C_nat.equal fold.state.id id -> Some term
  | term ->
    let children =
      match term with
      | C_term.Unit | C_term.Bool _ | C_term.Int _ | C_term.Narrow _
      | C_term.Bytes _
      | C_term.Var _ -> []
      | C_term.Vec (_, values) | C_term.Seq (_, _, values) -> values
      | C_term.Let (_, value, body)
      | C_term.Unpair (value, _, _, body)
      | C_term.Pair (value, body)
      | C_term.Add (value, body)
      | C_term.Sub (value, body)
      | C_term.Mul (value, body)
      | C_term.Div (value, body)
      | C_term.Mod (value, body)
      | C_term.Eq (_, value, body)
      | C_term.Cmp (_, value, body)
      | C_term.Cat (value, body)
      | C_term.Vcat (value, body)
      | C_term.Step (value, body) -> [value; body]
      | C_term.If (guard, yes, no) -> [guard; yes; no]
      | C_term.Fst value | C_term.Snd value | C_term.Inl (value, _)
      | C_term.Inr (_, value) | C_term.Act (_, value) | C_term.Neg value
      | C_term.Abs value | C_term.Fit (_, value) | C_term.Wide value
      | C_term.Length value | C_term.Take (_, value) | C_term.Drop (_, value)
      | C_term.At (_, value) | C_term.Uncons value | C_term.Close value -> [value]
      | C_term.Case (value, _, yes, _, no) -> [value; yes; no]
      | C_term.Vfold (vector, seed, fold) -> [vector; seed; fold.body]
    in
    List.find_map (binding_term id) children

let check_in_located binds term =
  match input_limit 0 binds with
  | Error error -> Error { error; term = None }
  | Ok () ->
    begin
      match shape term with
      | Error error -> Error { error; term = Some term }
      | Ok () ->
        begin
          match open_all [] binds with
          | Error error -> Error { error; term = None }
          | Ok env ->
            try
              match infer env term with
              | Error error -> Error { error; term = Some term }
              | Ok (info, env) ->
                begin
                  match pending env with
                  | None -> Ok info
                  | Some id -> Error {
                      error = Unused id;
                      term = binding_term id term;
                    }
                end
            with
            | Infer_error (error, failed) ->
              Error { error; term = Some failed }
        end
    end

let check_in binds term =
  Result.map_error (fun failure -> failure.error) (check_in_located binds term)

let check term = check_in [] term

let check_in_at schedule ~epoch binds term =
  match C_rule.select schedule ~epoch with
  | Error error -> Error (Rule error)
  | Ok rule ->
    begin
      match check_in binds term with
      | Error error -> Error (Check error)
      | Ok info -> Ok { rule = C_rule.id rule; info }
    end

let check_at schedule ~epoch term = check_in_at schedule ~epoch [] term

let ids_text ids =
  let out = C_text.make () in
  let rec walk first = function
    | [] -> ()
    | _ when C_text.full out -> ()
    | id :: rest ->
      if not first then C_text.add out ",";
      C_text.add out (C_nat.text id);
      walk false rest
  in
  walk true ids;
  C_text.get out

let raw = function
  | Unknown id -> "unknown id = " ^ C_nat.text id
  | Used id -> "used id = " ^ C_nat.text id
  | Ghost id -> "ghost id = " ^ C_nat.text id
  | Shadow id -> "shadow id = " ^ C_nat.text id
  | Need (expected, actual) ->
    "type expected = " ^ C_type.text expected ^ " actual = " ^ C_type.text actual
  | Bad typ -> "invalid type = " ^ C_type.text typ
  | Byte_index (len, total) ->
    "byte index = " ^ C_nat.text len ^ " size = " ^ C_nat.text total
  | Vec_index (index, total) ->
    "vec index = " ^ C_nat.text index ^ " size = " ^ C_nat.text total
  | Seq_size (actual, cap) ->
    "sequence size = " ^ C_nat.text actual ^ " capacity = " ^ C_nat.text cap
  | Range_type typ -> "range type invalid = " ^ C_type.text typ
  | Size -> "byte size overflow"
  | Mode (id, mul) ->
    "resource mode id = " ^ C_nat.text id ^ " mode = " ^ C_type.mul_text mul
  | Split ids ->
    "split ids = " ^ ids_text ids
  | Repeat ids ->
    "fold captures linear ids = " ^ ids_text ids
  | Erase ids ->
    "erased value captures linear ids = " ^ ids_text ids
  | Unused id -> "unused id = " ^ C_nat.text id
  | Impure id -> "zero binding has effects id = " ^ C_nat.text id
  | Access typ -> "data access expected type = " ^ C_type.text typ
  | Drop_res typ -> "resource component would be dropped type = " ^ C_type.text typ
  | Depth (limit, actual) ->
    "term depth limit = " ^ string_of_int limit ^ " actual = " ^ string_of_int actual
  | Nodes (limit, actual) ->
    "term node limit = " ^ string_of_int limit ^ " actual = " ^ string_of_int actual
  | Inputs (limit, actual) ->
    "input limit = " ^ string_of_int limit ^ " actual = " ^ string_of_int actual

let text error = C_text.clip (raw error)

let text_named find = function
  | Used id ->
    begin
      match find id with
      | Some name -> "linear value used more than once name = " ^ name
      | None -> text (Used id)
    end
  | Unused id ->
    begin
      match find id with
      | Some name -> "linear value is not consumed name = " ^ name
      | None -> text (Unused id)
    end
  | error -> text error

let rule_text = function
  | Rule error -> C_text.clip (C_rule.text error)
  | Check error -> text error