(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Nil
  | Step
  | Close
  | Both

type op =
  | Use_step
  | Use_close

module Key = struct
  type t = C_nat.t
  let compare = C_nat.compare
end

module Pmap = Map.Make (Key)

type set = {
  size : int;
  map : t Pmap.t;
}

type error =
  | Nat of Z.t
  | Dup of C_nat.t
  | Count of int * int
  | Cut_miss of C_nat.t
  | Miss of C_nat.t * op
  | Deny of C_nat.t * op * t
  | Gain of C_nat.t * t * t
  | Low of C_low.error
  | Check of C_check.error

let step = function
  | Step | Both -> true
  | Nil | Close -> false

let close = function
  | Close | Both -> true
  | Nil | Step -> false

let le left right =
  (not (step left) || step right) && (not (close left) || close right)

let of_bits step close =
  match step, close with
  | false, false -> Nil
  | true, false -> Step
  | false, true -> Close
  | true, true -> Both

let meet left right =
  of_bits (step left && step right) (close left && close right)

let empty = { size = 0; map = Pmap.empty }

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let find kind set = Pmap.find_opt kind set.map

let add set raw perm =
  if set.size = C_check.max_inputs then
    Error (Count (C_check.max_inputs, set.size + 1))
  else
    match C_nat.make raw with
    | None -> Error (Nat raw)
    | Some kind when Pmap.mem kind set.map -> Error (Dup kind)
    | Some kind ->
        Ok { size = set.size + 1; map = Pmap.add kind perm set.map }

let set values =
  let rec walk out = function
    | [] -> Ok out
    | (raw, perm) :: rest ->
        let* out = add out raw perm in
        walk out rest
  in
  walk empty values

let vals set = Pmap.bindings set.map

let cut set raw target =
  match C_nat.make raw with
  | None -> Error (Nat raw)
  | Some kind ->
      begin
        match find kind set with
        | None -> Error (Cut_miss kind)
        | Some source when le target source ->
            Ok { set with map = Pmap.add kind target set.map }
        | Some source -> Error (Gain (kind, source, target))
      end

let op = function
  | C_eff.Write kind -> Some (kind, Use_step)
  | C_eff.Close kind -> Some (kind, Use_close)
  | C_eff.Read _ | C_eff.Emit _ | C_eff.Fail _ -> None

let has perm = function
  | Use_step -> step perm
  | Use_close -> close perm

let allow set atom =
  match op atom with
  | None -> Ok ()
  | Some (kind, use) ->
      begin
        match find kind set with
        | None -> Error (Miss (kind, use))
        | Some perm when has perm use -> Ok ()
        | Some perm -> Error (Deny (kind, use, perm))
      end

let check_info set (info : C_check.info) =
  let rec walk = function
    | [] -> Ok info
    | atom :: rest ->
        let* () = allow set atom in
        walk rest
  in
  walk (C_eff.to_list info.eff)

let check set inputs term =
  let* info =
    Result.map_error (fun error -> Check error) (C_check.check_in inputs term)
  in
  check_info set info

let prog set inputs term =
  let* prog = Result.map_error (fun error -> Low error) (C_low.prog inputs term) in
  let* info = check set prog.inputs prog.term in
  Ok (prog, info)

let perm_text = function
  | Nil -> "nil"
  | Step -> "step"
  | Close -> "close"
  | Both -> "both"

let op_text = function
  | Use_step -> "step"
  | Use_close -> "close"

let text error =
  let raw =
    match error with
    | Nat value -> "capability kind outside profile = " ^ Z.to_string value
    | Dup kind -> "duplicate capability policy kind = " ^ C_nat.text kind
    | Count (limit, actual) ->
        Printf.sprintf "capability policy count limit = %d actual = %d"
          limit actual
    | Cut_miss kind ->
        "capability policy has no kind = " ^ C_nat.text kind
    | Miss (kind, op) ->
        "capability policy missing kind = " ^ C_nat.text kind
        ^ " operation = " ^ op_text op
    | Deny (kind, op, perm) ->
        "capability operation denied kind = " ^ C_nat.text kind
        ^ " operation = " ^ op_text op ^ " permission = " ^ perm_text perm
    | Gain (kind, source, target) ->
        "capability permission gain kind = " ^ C_nat.text kind
        ^ " source = " ^ perm_text source ^ " target = " ^ perm_text target
    | Low error -> C_low.text error
    | Check error -> C_check.text error
  in
  C_text.clip raw