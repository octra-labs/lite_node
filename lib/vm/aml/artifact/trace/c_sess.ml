(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type scope = {
  chain : string;
  prog : string;
  root : string;
}

type token = {
  scope : scope;
  kind : C_nat.t;
  id : C_nat.t;
  rev : C_nat.t;
}

type cell = {
  kind : C_nat.t;
  id : C_nat.t;
  rev : C_nat.t;
  live : bool;
}

module Key = struct
  type t = C_nat.t * C_nat.t

  let compare (lk, li) (rk, ri) =
    let order = C_nat.compare lk rk in
    if order = 0 then C_nat.compare li ri else order
end

module Kmap = Map.Make (Key)
module Kset = Set.Make (Key)

type slot = {
  rev : C_nat.t;
  live : bool;
}

type state = {
  scope : scope;
  slots : slot Kmap.t;
  count : int;
}

type out = {
  eval : C_eval.out;
  state : state;
  tokens : token list;
}

type field =
  | Chain
  | Prog
  | Root
  | Kind
  | Id
  | Rev

type error =
  | Nat of field * Z.t
  | Length of field * int
  | Scope
  | Order of (C_nat.t * C_nat.t) * (C_nat.t * C_nat.t)
  | Exists of (C_nat.t * C_nat.t)
  | Absent of (C_nat.t * C_nat.t)
  | Stale of C_nat.t * C_nat.t * C_nat.t * C_nat.t
  | Tdup of (C_nat.t * C_nat.t)
  | Tmiss of (C_nat.t * C_nat.t)
  | Textra of (C_nat.t * C_nat.t)
  | Vdup of (C_nat.t * C_nat.t)
  | Vdepth of int * int
  | Vnodes of int * int
  | Icount of int * int
  | Scount of int * int
  | New of (C_nat.t * C_nat.t)
  | Rev_max of (C_nat.t * C_nat.t)
  | Eval of C_eval.error

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let scope_equal left right =
  String.equal left.chain right.chain
  && String.equal left.prog right.prog
  && String.equal left.root right.root

let scope ~chain ~prog ~root =
  let check field value =
    let size = String.length value in
    if size <= C_nat.max then Ok value else Error (Length (field, size))
  in
  let* chain = check Chain chain in
  let* prog = check Prog prog in
  let* root = check Root root in
  Ok { chain; prog; root }

let empty scope = { scope; slots = Kmap.empty; count = 0 }
let scope_of state = state.scope

let nat field value =
  match C_nat.make value with
  | Some value -> Ok value
  | None -> Error (Nat (field, value))

let token scope ~kind ~id ~rev =
  let* kind = nat Kind kind in
  let* id = nat Id id in
  let* rev = nat Rev rev in
  Ok { scope; kind; id; rev }

let cell ~kind ~id ~rev ~live =
  let* kind = nat Kind kind in
  let* id = nat Id id in
  let* rev = nat Rev rev in
  Ok { kind; id; rev; live }

let cells state =
  Kmap.bindings state.slots
  |> List.map (fun ((kind, id), slot) ->
    { kind; id; rev = slot.rev; live = slot.live })

let view state = state.scope, cells state

let of_cells scope cells =
  let rec walk prior slots count = function
    | [] -> Ok { scope; slots; count }
    | _ when count = C_check.max_nodes ->
        Error (Scount (C_check.max_nodes, C_check.max_nodes + 1))
    | cell :: rest ->
        let key = cell.kind, cell.id in
        begin
          match prior with
          | Some prior when Key.compare prior key >= 0 -> Error (Order (prior, key))
          | _ ->
              walk (Some key)
                (Kmap.add key { rev = cell.rev; live = cell.live } slots)
                (count + 1)
                rest
        end
  in
  walk None Kmap.empty 0 cells

let issue state ~kind ~id =
  let* kind = nat Kind kind in
  let* id = nat Id id in
  let key = kind, id in
  if Kmap.mem key state.slots then Error (Exists key)
  else if state.count = C_check.max_nodes then
    Error (Scount (C_check.max_nodes, C_check.max_nodes + 1))
  else
    let token = { scope = state.scope; kind; id; rev = C_nat.zero } in
    let slots = Kmap.add key { rev = token.rev; live = true } state.slots in
    Ok ({ state with slots; count = state.count + 1 }, token)

let current (state : state) (token : token) =
  if not (scope_equal state.scope token.scope) then false
  else match Kmap.find_opt (token.kind, token.id) state.slots with
  | Some slot -> slot.live && C_nat.equal slot.rev token.rev
  | None -> false

let take (state : state) (token : token) ~keep =
  let key = token.kind, token.id in
  if not (scope_equal state.scope token.scope) then Error Scope
  else match Kmap.find_opt key state.slots with
  | None -> Error (Absent key)
  | Some slot when not slot.live || not (C_nat.equal slot.rev token.rev) ->
      Error (Stale (token.kind, token.id, slot.rev, token.rev))
  | Some _ when not keep ->
      let slots = Kmap.add key { rev = token.rev; live = false } state.slots in
      Ok ({ state with slots }, None)
  | Some _ ->
      begin
        match C_nat.add token.rev C_nat.one with
        | None -> Error (Rev_max key)
        | Some rev ->
            let fresh = { token with rev } in
            let slots = Kmap.add key { rev; live = true } state.slots in
            Ok ({ state with slots }, Some fresh)
      end

type part =
  | Val of int * C_eval.value

let caps start nodes value =
  let rec walk nodes out = function
    | [] -> Ok (out, nodes)
    | Val (depth, _) :: _ when depth > C_check.max_depth ->
        Error (Vdepth (C_check.max_depth, depth))
    | _ when nodes >= C_check.max_nodes ->
        Error (Vnodes (C_check.max_nodes, nodes + 1))
    | Val (depth, value) :: rest ->
        let next = depth + 1 in
        begin
          match value with
          | C_eval.Unit | C_eval.Bool _ | C_eval.Int _ | C_eval.Bytes _
          | C_eval.Enc _ ->
              walk (nodes + 1) out rest
          | C_eval.Cap (kind, id) ->
              let key = kind, id in
              if not (C_nat.valid kind) then Error (Nat (Kind, C_nat.to_z kind))
              else if not (C_nat.valid id) then Error (Nat (Id, C_nat.to_z id))
              else if Kset.mem key out then Error (Vdup key)
              else walk (nodes + 1) (Kset.add key out) rest
          | C_eval.Vec values ->
              let rest =
                Array.fold_right (fun value rest -> Val (next, value) :: rest)
                  values rest
              in
              walk (nodes + 1) out rest
          | C_eval.Pair (left, right) ->
              walk (nodes + 1) out
                (Val (next, left) :: Val (next, right) :: rest)
          | C_eval.Inl value | C_eval.Inr value ->
              walk (nodes + 1) out (Val (next, value) :: rest)
        end
  in
  walk nodes start [Val (0, value)]

let input_caps inputs =
  let rec walk count nodes out = function
    | [] -> Ok out
    | _ when count = C_check.max_inputs ->
        Error (Icount (C_check.max_inputs, C_check.max_inputs + 1))
    | (_, value) :: rest ->
        let* out, nodes = caps out nodes value in
        walk (count + 1) nodes out rest
  in
  walk 0 0 Kset.empty inputs

let token_map (tokens : token list) =
  let rec walk count out = function
    | [] -> Ok out
    | _ when count = C_check.max_nodes ->
        Error (Vnodes (C_check.max_nodes, C_check.max_nodes + 1))
    | (token : token) :: rest ->
        let key = token.kind, token.id in
        if Kmap.mem key out then Error (Tdup key)
        else walk (count + 1) (Kmap.add key token out) rest
  in
  walk 0 Kmap.empty tokens

let verify (state : state) expected (supplied : token Kmap.t) =
  let rec needs = function
    | [] -> Ok ()
    | key :: rest ->
        begin
          match Kmap.find_opt key supplied with
          | None -> Error (Tmiss key)
          | Some token when not (scope_equal state.scope token.scope) ->
              Error Scope
          | Some token when current state token -> needs rest
          | Some token ->
              begin
                match Kmap.find_opt key state.slots with
                | None -> Error (Absent key)
                | Some slot -> Error (Stale (token.kind, token.id, slot.rev, token.rev))
              end
        end
  in
  let rec extras = function
    | [] -> Ok ()
    | (key, _) :: rest ->
        if Kset.mem key expected then extras rest else Error (Textra key)
  in
  let* () = needs (Kset.elements expected) in
  extras (Kmap.bindings supplied)

let advance (state : state) (supplied : token Kmap.t) input output =
  let rec check = function
    | [] -> Ok ()
    | key :: rest ->
        if Kset.mem key input then check rest else Error (New key)
  in
  let rec walk state tokens = function
    | [] -> Ok (state, List.rev tokens)
    | key :: rest ->
        let token = Kmap.find key supplied in
        let keep = Kset.mem key output in
        let* state, fresh = take state token ~keep in
        let tokens = match fresh with Some token -> token :: tokens | None -> tokens in
        walk state tokens rest
  in
  let* () = check (Kset.elements output) in
  walk state [] (Kset.elements input)

let run ?fuel state tokens inputs term =
  let* input = input_caps inputs in
  let* supplied = token_map tokens in
  let* () = verify state input supplied in
  let eval =
    match C_eval.run_in ?fuel inputs term with
    | Ok value -> Ok value
    | Error error -> Error (Eval error)
  in
  let* eval = eval in
  let* output, _ = caps Kset.empty 0 eval.value in
  let* state, tokens = advance state supplied input output in
  Ok { eval; state; tokens }

let token_text (token : token) =
  "token[" ^ C_nat.text token.kind ^ "," ^ C_nat.text token.id ^ ","
  ^ C_nat.text token.rev ^ "]"

let field_text = function
  | Chain -> "chain"
  | Prog -> "program"
  | Root -> "prior_root"
  | Kind -> "kind"
  | Id -> "id"
  | Rev -> "revision"

let key_text (kind, id) = C_nat.text kind ^ ":" ^ C_nat.text id

let text error =
  let raw =
    match error with
    | Nat (field, value) ->
        "session natural outside profile field = " ^ field_text field
        ^ " value = " ^ Z.to_string value
    | Length (field, actual) ->
        Printf.sprintf "session bytes limit field = %s limit = %d actual = %d"
          (field_text field) C_nat.max actual
    | Scope -> "session context differs"
    | Order (left, right) ->
        "session identity order differs left = " ^ key_text left
        ^ " right = " ^ key_text right
    | Exists key -> "session identity already issued = " ^ key_text key
    | Absent key -> "session identity absent = " ^ key_text key
    | Stale (kind, id, expected, actual) ->
        "session token revision differs identity = " ^ key_text (kind, id)
        ^ " expected = " ^ C_nat.text expected
        ^ " actual = " ^ C_nat.text actual
    | Tdup key -> "session token repeated = " ^ key_text key
    | Tmiss key -> "session token missing = " ^ key_text key
    | Textra key -> "session token has no input = " ^ key_text key
    | Vdup key -> "session value repeats identity = " ^ key_text key
    | Vdepth (limit, actual) ->
        Printf.sprintf "session value depth limit = %d actual = %d" limit actual
    | Vnodes (limit, actual) ->
        Printf.sprintf "session value nodes limit = %d actual = %d" limit actual
    | Icount (limit, actual) ->
        Printf.sprintf "session input count limit = %d actual = %d" limit actual
    | Scount (limit, actual) ->
        Printf.sprintf "session identity count limit = %d actual = %d" limit actual
    | New key -> "session output identity was not an input = " ^ key_text key
    | Rev_max key -> "session token revision limit identity = " ^ key_text key
    | Eval error -> "session evaluator refused reason = " ^ C_eval.text error
  in
  C_text.clip raw