(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  regs : C_emit.lit array;
  cap : int;
  mode : Int_work.mode;
  mutable pc : int;
  mutable steps : int;
  mutable work : Z.t;
  mutable halted : bool;
  mutable closes : (C_nat.t * C_nat.t) list;
}

type error =
  | Work_cap of int
  | Input_count of int
  | Program_counter of int
  | Value_type of int
  | Integer_range of int
  | Divide_zero of int
  | Modulo_zero of int
  | Capability_kind of int
  | Capability_replay of int

let make ?(cap = 1_000_000) ?(activate = None) ?(epoch = Z.zero) () =
  if cap < 1 then invalid_arg "VM work cap is invalid";
  {
    regs = Array.make 64 (C_emit.Int Z.zero);
    cap;
    mode = Int_work.select ~activate ~epoch;
    pc = 0;
    steps = 0;
    work = Z.zero;
    halted = false;
    closes = [];
  }

let make_in ?cap ?activate ?epoch values =
  let count = List.length values in
  if count > Contract_vm.input_limit then Error (Input_count count)
  else
    let state = make ?cap ?activate ?epoch () in
    List.iteri (fun index value -> state.regs.(index + 1) <- value) values;
    Ok state

let pc state = state.pc
let steps state = state.steps
let work state = state.work
let halted state = state.halted
let value state reg = state.regs.(reg)
let result state = value state 0
let closes state = List.rev state.closes

let fixed = function
  | C_octb.Load _ | C_octb.Move _ | C_octb.Jump _ | C_octb.Mark _
  | C_octb.Noop | C_octb.Stop -> 1
  | C_octb.Same _ | C_octb.Different _ | C_octb.Less _
  | C_octb.Greater _ -> 2
  | C_octb.Plus _ | C_octb.Times _ | C_octb.Join _ | C_octb.Minus _
  | C_octb.Size _ -> 3
  | C_octb.Quotient _ | C_octb.Remainder _ | C_octb.Negate _
  | C_octb.Absolute _ | C_octb.Jump_if _ | C_octb.Slice _ -> 5
  | C_octb.Cap_close _ -> 50

let int_value state reg =
  match value state reg with
  | C_emit.Int value -> Some value
  | _ -> None

let cost state op =
  let binary kind left right =
    match int_value state left, int_value state right with
    | Some lhs, Some rhs -> Int_work.cost state.mode kind lhs rhs
    | _ -> Z.of_int (fixed op)
  in
  match op with
  | C_octb.Plus (_, left, right) -> binary Int_work.Add left right
  | C_octb.Minus (_, left, right) -> binary Int_work.Sub left right
  | C_octb.Times (_, left, right) -> binary Int_work.Mul left right
  | C_octb.Quotient (_, left, right) -> binary Int_work.Div left right
  | C_octb.Remainder (_, left, right) -> binary Int_work.Mod left right
  | C_octb.Negate (_, src) -> binary Int_work.Neg src src
  | C_octb.Absolute (_, src) -> binary Int_work.Abs src src
  | _ -> Z.of_int (fixed op)

let int state pc reg =
  match value state reg with
  | C_emit.Int value -> Ok value
  | _ -> Error (Value_type pc)

let bytes state pc reg =
  match value state reg with
  | C_emit.Bytes value -> Ok value
  | _ -> Error (Value_type pc)

let bool state pc reg =
  match value state reg with
  | C_emit.Bool value -> Ok value
  | _ -> Error (Value_type pc)

let set state reg value = state.regs.(reg) <- value

let charge state value =
  if Z.sign value < 0 || Z.gt (Z.add state.work value) (Z.of_int state.cap) then
    Error (Work_cap state.cap)
  else begin
    state.work <- Z.add state.work value;
    Ok ()
  end

let index pc value =
  if Z.sign value < 0 then Ok None
  else
    match C_nat.make value with
    | Some value -> Ok (Some (C_nat.to_int value))
    | None -> Error (Integer_range pc)

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let exec state at = function
  | C_octb.Load (dst, value) ->
    set state dst value;
    Ok true
  | C_octb.Move (dst, src) ->
    set state dst (value state src);
    Ok true
  | C_octb.Plus (dst, left, right) ->
    let* lhs = int state at left in
    let* rhs = int state at right in
    set state dst (C_emit.Int (Z.add lhs rhs));
    Ok true
  | C_octb.Times (dst, left, right) ->
    let* lhs = int state at left in
    let* rhs = int state at right in
    set state dst (C_emit.Int (Z.mul lhs rhs));
    Ok true
  | C_octb.Same (dst, left, right) ->
    set state dst (C_emit.Bool (C_mach.equal (value state left) (value state right)));
    Ok true
  | C_octb.Different (dst, left, right) ->
    let* lhs = int state at left in
    let* rhs = int state at right in
    set state dst (C_emit.Bool (not (Z.equal lhs rhs)));
    Ok true
  | C_octb.Less (dst, left, right) ->
    let* lhs = int state at left in
    let* rhs = int state at right in
    set state dst (C_emit.Bool (Z.lt lhs rhs));
    Ok true
  | C_octb.Greater (dst, left, right) ->
    let* lhs = int state at left in
    let* rhs = int state at right in
    set state dst (C_emit.Bool (Z.gt lhs rhs));
    Ok true
  | C_octb.Join (dst, left, right) ->
    let* lhs = bytes state at left in
    let* rhs = bytes state at right in
    set state dst (C_emit.Bytes (lhs ^ rhs));
    Ok true
  | C_octb.Minus (dst, left, right) ->
    let* lhs = int state at left in
    let* rhs = int state at right in
    set state dst (C_emit.Int (Z.sub lhs rhs));
    Ok true
  | C_octb.Quotient (dst, left, right) ->
    let* lhs = int state at left in
    let* rhs = int state at right in
    if Z.equal rhs Z.zero then Error (Divide_zero at)
    else begin
      set state dst (C_emit.Int (Z.div lhs rhs));
      Ok true
    end
  | C_octb.Remainder (dst, left, right) ->
    let* lhs = int state at left in
    let* rhs = int state at right in
    if Z.equal rhs Z.zero then Error (Modulo_zero at)
    else begin
      set state dst (C_emit.Int (Z.rem lhs rhs));
      Ok true
    end
  | C_octb.Negate (dst, src) ->
    let* value = int state at src in
    set state dst (C_emit.Int (Z.neg value));
    Ok true
  | C_octb.Absolute (dst, src) ->
    let* value = int state at src in
    set state dst (C_emit.Int (Z.abs value));
    Ok true
  | C_octb.Size (dst, src) ->
    let* raw = bytes state at src in
    set state dst (C_emit.Int (Z.of_int (String.length raw)));
    Ok true
  | C_octb.Slice (dst, src, first, count) ->
    let* raw = bytes state at src in
    let* first = int state at first in
    let* count = int state at count in
    let* first = index at first in
    let* count = index at count in
    let size = String.length raw in
    let* () = charge state (Z.of_int (size / 256)) in
    let out =
      match first, count with
      | Some first, Some count when first <= size ->
        String.sub raw first (Int.min count (size - first))
      | _ -> ""
    in
    set state dst (C_emit.Bytes out);
    Ok true
  | C_octb.Cap_close (kind, reg) ->
    begin
      match value state reg with
      | C_emit.Cap (found, id) when C_nat.equal kind found ->
        if List.exists
            (fun (prior, item) ->
              C_nat.equal prior kind && C_nat.equal item id)
            state.closes
        then Error (Capability_replay at)
        else begin
          state.closes <- (kind, id) :: state.closes;
          Ok true
        end
      | _ -> Error (Capability_kind at)
    end
  | C_octb.Jump target ->
    state.pc <- target;
    Ok true
  | C_octb.Jump_if (reg, target) ->
    let* yes = bool state at reg in
    if yes then state.pc <- target;
    Ok true
  | C_octb.Mark _ -> Ok true
  | C_octb.Noop -> Ok true
  | C_octb.Stop ->
    state.halted <- true;
    Ok false

let step state op =
  let at = state.pc in
  if state.halted then Ok false
  else
    let* () = charge state (cost state op) in
    state.pc <- at + 1;
    state.steps <- state.steps + 1;
    exec state at op

let run state code =
  let rec loop () =
    if state.halted then Ok ()
    else if state.pc < 0 || state.pc >= Array.length code then
      Error (Program_counter state.pc)
    else
      let* active = step state code.(state.pc) in
      if active then loop () else Ok ()
  in
  loop ()

let text = function
  | Work_cap value -> Printf.sprintf "VM work cap reached cap = %d" value
  | Input_count value ->
    Printf.sprintf "VM input count maximum = %d actual = %d"
      Contract_vm.input_limit value
  | Program_counter value ->
    Printf.sprintf "VM program counter is invalid pc = %d" value
  | Value_type value -> Printf.sprintf "VM value type is invalid pc = %d" value
  | Integer_range value ->
    Printf.sprintf "VM integer does not fit host index pc = %d" value
  | Divide_zero value ->
    Printf.sprintf "VM integer division rejected pc = %d" value
  | Modulo_zero value ->
    Printf.sprintf "VM integer remainder rejected pc = %d" value
  | Capability_kind value ->
    Printf.sprintf "VM capability kind differs pc = %d" value
  | Capability_replay value ->
    Printf.sprintf "VM capability repeats pc = %d" value