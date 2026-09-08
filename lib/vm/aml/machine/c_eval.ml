(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type value =
  | Unit
  | Bool of bool
  | Int of Z.t
  | Bytes of string
  | Vec of value array
  | Cap of C_nat.t * C_nat.t
  | Enc of C_nat.t * C_nat.t * C_fp.t
  | Pair of value * value
  | Inl of value
  | Inr of value

type origin =
  | Direct
  | Held of C_nat.t * C_nat.t

type action = {
  atom : C_eff.atom;
  payload : value;
  origin : origin;
}

type out = {
  value : value;
  plan : C_eff.atom list;
  actions : action list;
  steps : Z.t;
  work : Z.t;
  row : C_eff.t;
  limit : Z.t;
  work_limit : Z.t;
}

type exec = {
  rule : C_rule.id;
  out : out;
}

type error =
  | Static of C_check.error
  | Missing of C_term.id
  | Need_bool
  | Need_int
  | Divide_zero
  | Modulo_zero
  | Need_pair
  | Need_sum
  | Need_bytes
  | Need_vec
  | Need_cap
  | Need_seq
  | Byte_index of C_nat.t * C_nat.t
  | Vec_index of C_nat.t * C_nat.t
  | Input of C_term.id
  | Input_type of C_term.id * C_type.t
  | Input_depth of C_term.id * int * int
  | Input_nodes of C_term.id * int * int
  | Cap_dup of C_nat.t * C_nat.t
  | Fuel of Z.t * Z.t
  | Cost of Z.t * Z.t
  | Work of Z.t * Z.t
  | Effects of C_eff.t * C_eff.t

type rule_error =
  | Rule of C_rule.error
  | Run of error

type trace =
  | Nil
  | Atom of action
  | Cat of trace * trace

type done_ = {
  value : value;
  trace : trace;
  steps : Z.t;
  work : Z.t;
}

let max_cost = Z.of_int C_rule.local.fuel

let ( let* ) value next =
  match value with
  | Ok item -> next item
  | Error error -> Error error

let rec equal left right =
  match left, right with
  | Unit, Unit -> true
  | Bool left, Bool right -> left = right
  | Int left, Int right -> Z.equal left right
  | Bytes left, Bytes right -> String.equal left right
  | Vec left, Vec right ->
    Array.length left = Array.length right
    && Array.for_all2 equal left right
  | Cap (left_kind, left_id), Cap (right_kind, right_id) ->
    C_nat.equal left_kind right_kind && C_nat.equal left_id right_id
  | Enc (left_key, left_rem, left_field), Enc (right_key, right_rem, right_field) ->
    C_nat.equal left_key right_key
    && C_nat.equal left_rem right_rem
    && C_fp.equal left_field right_field
  | Pair (la, lb), Pair (ra, rb) ->
    equal la ra && equal lb rb
  | Inl left, Inl right | Inr left, Inr right ->
    equal left right
  | _ -> false

let array_for_all test values =
  let rec walk index =
    index = Array.length values
    || test index values.(index) && walk (index + 1)
  in
  walk 0

let rec zero = function
  | C_type.Unit -> Some Unit
  | C_type.Bool -> Some (Bool false)
  | C_type.Int | C_type.Num _ -> Some (Int Z.zero)
  | C_type.Bytes len -> Some (Bytes (String.make (C_nat.to_int len) '\000'))
  | C_type.Vec (len, elem) ->
    Option.map
      (fun value -> Vec (Array.make (C_nat.to_int len) value))
      (zero elem)
  | C_type.Seq (cap, elem) ->
    Option.map
      (fun value -> Pair (Int Z.zero, Vec (Array.make (C_nat.to_int cap) value)))
      (zero elem)
  | C_type.Pair (left, right) ->
    Option.bind (zero left) (fun left ->
      Option.map (fun right -> Pair (left, right)) (zero right))
  | C_type.Sum (left, right) ->
    begin
      match zero left with
      | Some value -> Some (Inl value)
      | None -> Option.map (fun value -> Inr value) (zero right)
    end
  | C_type.Cap _ | C_type.Enc _ -> None

let rec typed typ value =
  match typ, value with
  | C_type.Unit, Unit | C_type.Bool, Bool _ | C_type.Int, Int _ -> true
  | C_type.Num _, Int value -> C_type.admits typ value
  | C_type.Bytes len, Bytes value ->
    C_nat.to_int len = String.length value
  | C_type.Vec (len, elem), Vec values ->
    C_nat.to_int len = Array.length values && Array.for_all (typed elem) values
  | C_type.Seq (cap, elem), Pair (Int len, Vec values) ->
    let count = Array.length values in
    let high = C_nat.to_z cap in
    if count <> C_nat.to_int cap || Z.sign len < 0 || Z.gt len high then false
    else
      begin
        match zero elem with
        | None -> false
        | Some empty ->
          let used = Z.to_int len in
          array_for_all
            (fun index value -> typed elem value && (index < used || equal value empty))
            values
      end
  | C_type.Cap kind, Cap (actual, id) ->
    C_nat.equal kind actual && C_nat.valid id
  | C_type.Enc (key, rem), Enc (actual_key, actual_rem, _) ->
    C_nat.equal key actual_key && C_nat.equal rem actual_rem
  | C_type.Pair (left_type, right_type), Pair (left, right) ->
    typed left_type left && typed right_type right
  | C_type.Sum (left, _), Inl value -> typed left value
  | C_type.Sum (_, right), Inr value -> typed right value
  | _ -> false

let rec value_work typ value =
  match typ, value with
  | C_type.Unit, Unit | C_type.Bool, Bool _ | C_type.Int, Int _
  | C_type.Cap _, Cap _ | C_type.Enc _, Enc _ -> Z.one
  | C_type.Num _, Int _ -> Z.one
  | C_type.Bytes _, Bytes value -> Z.succ (Z.of_int (String.length value))
  | C_type.Vec (_, elem), Vec values ->
    Array.fold_left
      (fun work value -> Z.add work (value_work elem value))
      Z.one
      values
  | C_type.Seq (_, elem), Pair (Int _, Vec values) ->
    Array.fold_left
      (fun work value -> Z.add work (value_work elem value))
      (Z.of_int 2)
      values
  | C_type.Pair (left_ty, right_ty), Pair (left, right) ->
    Z.succ (Z.add (value_work left_ty left) (value_work right_ty right))
  | C_type.Sum (left, _), Inl value -> Z.succ (value_work left value)
  | C_type.Sum (_, right), Inr value -> Z.succ (value_work right value)
  | _ -> Z.one

let equal_run typ left right =
  equal left right, Z.max (value_work typ left) (value_work typ right)

type value_part =
  | Value of value
  | Value_text of string
  | Value_hex of string * int
  | Value_vec of value array * int

let int_text value =
  let bits = Z.numbits value in
  if bits <= 256 then Z.to_string value
  else
    let sign = if Z.sign value < 0 then "-" else "" in
    sign ^ "int[" ^ string_of_int bits ^ "]"

let byte_text byte =
  let digits = "0123456789abcdef" in
  let value = Char.code byte in
  String.init 2 (function
    | 0 -> digits.[value lsr 4]
    | _ -> digits.[value land 15])

let value_text value =
  let out = C_text.make () in
  let rec walk = function
    | [] -> ()
    | _ when C_text.full out -> ()
    | Value_text text :: rest ->
      C_text.add out text;
      walk rest
    | Value Unit :: rest -> C_text.add out "()"; walk rest
    | Value (Bool true) :: rest -> C_text.add out "true"; walk rest
    | Value (Bool false) :: rest -> C_text.add out "false"; walk rest
    | Value (Int value) :: rest -> C_text.add out (int_text value); walk rest
    | Value (Bytes value) :: rest ->
      C_text.add out "0x";
      walk (Value_hex (value, 0) :: rest)
    | Value (Vec values) :: rest ->
      C_text.add out "[";
      walk (Value_vec (values, 0) :: rest)
    | Value (Cap (kind, id)) :: rest ->
      C_text.add out ("cap[" ^ C_nat.text kind ^ "]#" ^ C_nat.text id);
      walk rest
    | Value (Enc (key, rem, _)) :: rest ->
      C_text.add out ("enc[" ^ C_nat.text key ^ "," ^ C_nat.text rem ^ "]");
      walk rest
    | Value (Pair (left, right)) :: rest ->
      C_text.add out "(";
      walk (Value left :: Value_text ", " :: Value right :: Value_text ")" :: rest)
    | Value (Inl value) :: rest ->
      C_text.add out "inl ";
      walk (Value value :: rest)
    | Value (Inr value) :: rest ->
      C_text.add out "inr ";
      walk (Value value :: rest)
    | Value_hex (value, index) :: rest ->
      if index = String.length value then walk rest
      else begin
        C_text.add out (byte_text value.[index]);
        walk (Value_hex (value, index + 1) :: rest)
      end
    | Value_vec (values, index) :: rest ->
      if index = Array.length values then begin
        C_text.add out "]";
        walk rest
      end else begin
        if index > 0 then C_text.add out ", ";
        walk (Value values.(index) :: Value_vec (values, index + 1) :: rest)
      end
  in
  walk [Value value];
  C_text.get out

let rec get id = function
  | [] -> Error (Missing id)
  | (key, value) :: _ when key = id -> Ok value
  | _ :: rest -> get id rest

let cat left right =
  match left, right with
  | Nil, trace | trace, Nil -> trace
  | _ -> Cat (left, right)

let trace_list trace =
  let rec walk out = function
    | [] -> List.rev out
    | Nil :: rest -> walk out rest
    | Atom atom :: rest -> walk (atom :: out) rest
    | Cat (left, right) :: rest -> walk out (left :: right :: rest)
  in
  walk [] [trace]

let atoms actions = List.map (fun action -> action.atom) actions
let direct atom payload = { atom; payload; origin = Direct }
let held atom payload kind id = { atom; payload; origin = Held (kind, id) }

let item value = { value; trace = Nil; steps = Z.one; work = Z.one }

let integer op reject left right =
  let actual = Int_work.cost Int_work.Active op left right in
  if Z.gt actual max_cost then Error (Work (max_cost, actual))
  else
    match Int_work.eval op left right with
    | Int_work.Value value -> Ok value
    | Int_work.Reject -> Error reject

let host_len value =
  match C_nat.of_int value with
  | Some len -> Ok len
  | None -> Error (Static C_check.Size)

let bytes value =
  let* len = host_len (String.length value) in
  Ok {
    value = Bytes value;
    trace = Nil;
    steps = Z.succ (C_nat.to_z len);
    work = Z.succ (C_nat.to_z len);
  }

let bind_env (bind : C_term.bind) value env =
  match bind.C_term.mul with
  | C_type.Zero -> env
  | C_type.One | C_type.Many -> (bind.id, value) :: env

let seq left right value =
  {
    value;
    trace = cat left.trace right.trace;
    steps = Z.succ (Z.add left.steps right.steps);
    work = Z.succ (Z.add left.work right.work);
  }

let copy_seq len left right value =
  {
    value;
    trace = cat left.trace right.trace;
    steps = Z.add (C_nat.to_z len) (Z.succ (Z.add left.steps right.steps));
    work = Z.add (C_nat.to_z len) (Z.succ (Z.add left.work right.work));
  }

let byte_one len value bytes =
  {
    value = Bytes bytes;
    trace = value.trace;
    steps = Z.add (C_nat.to_z len) (Z.succ value.steps);
    work = Z.add (C_nat.to_z len) (Z.succ value.work);
  }

module Cap_id = struct
  type t = C_nat.t * C_nat.t
  let compare (left_kind, left_id) (right_kind, right_id) =
    let order = C_nat.compare left_kind right_kind in
    if order = 0 then C_nat.compare left_id right_id else order
end

module Caps = Set.Make (Cap_id)
module Ids = Set.Make (C_nat)

let input_value (bind : C_term.bind) caps value =
  let bad () = Error (Input_type (bind.id, bind.typ)) in
  let rec walk nodes caps = function
    | [] -> Ok caps
    | (depth, typ, value) :: rest ->
      if depth > C_check.max_depth then
        Error (Input_depth (bind.id, C_check.max_depth, depth))
      else if nodes >= C_check.max_nodes then
        Error (Input_nodes (bind.id, C_check.max_nodes, nodes + 1))
      else
        let next = depth + 1 in
        match typ, value with
        | C_type.Unit, Unit | C_type.Bool, Bool _ | C_type.Int, Int _ ->
          walk (nodes + 1) caps rest
        | C_type.Num _, Int value when C_type.admits typ value ->
          walk (nodes + 1) caps rest
        | C_type.Bytes len, Bytes value ->
          begin
            match C_nat.of_int (String.length value) with
            | Some actual when C_nat.equal len actual ->
              walk (nodes + 1) caps rest
            | _ -> bad ()
          end
        | C_type.Vec (len, elem), Vec values ->
          begin
            match C_nat.of_int (Array.length values) with
            | Some actual when C_nat.equal len actual ->
              let rest =
                Array.fold_right
                  (fun value rest -> (next, elem, value) :: rest)
                  values
                  rest
              in
              walk (nodes + 1) caps rest
            | _ -> bad ()
          end
        | C_type.Seq (cap, elem), Pair (Int len, Vec values) ->
          let count = Array.length values in
          let high = C_nat.to_z cap in
          begin
            match zero elem with
            | Some empty when count = C_nat.to_int cap
                && Z.sign len >= 0 && Z.leq len high ->
              let used = Z.to_int len in
              if array_for_all
                  (fun index value -> index < used || equal value empty)
                  values
              then
                let rest =
                  Array.fold_right
                    (fun value rest -> (next, elem, value) :: rest)
                    values
                    rest
                in
                walk (nodes + 1) caps rest
              else bad ()
            | _ -> bad ()
          end
        | C_type.Cap kind, Cap (actual, id)
          when C_nat.equal kind actual && C_nat.valid id ->
          let key = kind, id in
          if Caps.mem key caps then Error (Cap_dup (kind, id))
          else walk (nodes + 1) (Caps.add key caps) rest
        | C_type.Enc (key, rem), Enc (actual_key, actual_rem, _) ->
          if C_nat.equal key actual_key && C_nat.equal rem actual_rem then
            walk (nodes + 1) caps rest
          else bad ()
        | C_type.Pair (left_type, right_type), Pair (left, right) ->
          walk
            (nodes + 1)
            caps
            ((next, left_type, left) :: (next, right_type, right) :: rest)
        | C_type.Sum (left, _), Inl value ->
          walk (nodes + 1) caps ((next, left, value) :: rest)
        | C_type.Sum (_, right), Inr value ->
          walk (nodes + 1) caps ((next, right, value) :: rest)
        | _ -> bad ()
  in
  walk 0 caps [0, bind.typ, value]

let rec eval env term =
  match term with
  | C_term.Unit -> Ok (item Unit)
  | C_term.Bool value -> Ok (item (Bool value))
  | C_term.Int value -> Ok (item (Int value))
  | C_term.Narrow (typ, value) ->
    if C_type.admits typ value then Ok (item (Int value))
    else Error Need_int
  | C_term.Bytes value -> bytes value
  | C_term.Vec (_, values) -> eval_vec env values
  | C_term.Seq (cap, elem, values) ->
    let count = List.length values in
    if count > C_nat.to_int cap then
      begin
        match C_nat.of_int count with
        | Some actual -> Error (Vec_index (actual, cap))
        | None -> Error Need_seq
      end
    else
      let* active = eval_vec env values in
      begin
        match active.value, zero elem with
        | Vec items, Some empty ->
          let full = Array.make (C_nat.to_int cap) empty in
          Array.blit items 0 full 0 count;
          let extra = Z.of_int (C_nat.to_int cap - count + 1) in
          Ok {
            active with
            value = Pair (Int (Z.of_int count), Vec full);
            steps = Z.add active.steps extra;
            work = Z.add active.work extra;
          }
        | _ -> Error Need_seq
      end
  | C_term.Var id ->
    let* value = get id env in
    Ok (item value)
  | C_term.Let (bind, value, body) ->
    begin
      match bind.mul with
      | C_type.Zero ->
        let* body = eval env body in
        Ok { body with steps = Z.succ body.steps; work = Z.succ body.work }
      | C_type.One | C_type.Many ->
        let* value = eval env value in
        let* body = eval ((bind.id, value.value) :: env) body in
        Ok (seq value body body.value)
    end
  | C_term.If (guard, yes, no) ->
    let* guard = eval env guard in
    begin
      match guard.value with
      | Bool true ->
        let* body = eval env yes in
        Ok (seq guard body body.value)
      | Bool false ->
        let* body = eval env no in
        Ok (seq guard body body.value)
      | _ -> Error Need_bool
    end
  | C_term.Pair (left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    Ok (seq left right (Pair (left.value, right.value)))
  | C_term.Unpair (pair, left, right, body) ->
    let* pair = eval env pair in
    begin
      match pair.value with
      | Pair (left_value, right_value) ->
        let body_env = bind_env right right_value (bind_env left left_value env) in
        let* body = eval body_env body in
        Ok (seq pair body body.value)
      | _ -> Error Need_pair
    end
  | C_term.Fst pair ->
    let* pair = eval env pair in
    begin
      match pair.value with
      | Pair (left, _) ->
        Ok { pair with value = left; steps = Z.succ pair.steps;
          work = Z.succ pair.work }
      | _ -> Error Need_pair
    end
  | C_term.Snd pair ->
    let* pair = eval env pair in
    begin
      match pair.value with
      | Pair (_, right) ->
        Ok { pair with value = right; steps = Z.succ pair.steps;
          work = Z.succ pair.work }
      | _ -> Error Need_pair
    end
  | C_term.Inl (value, _) ->
    let* value = eval env value in
    Ok { value with value = Inl value.value; steps = Z.succ value.steps;
      work = Z.succ value.work }
  | C_term.Inr (_, value) ->
    let* value = eval env value in
    Ok { value with value = Inr value.value; steps = Z.succ value.steps;
      work = Z.succ value.work }
  | C_term.Case (value, left, yes, right, no) ->
    let* value = eval env value in
    begin
      match value.value with
      | Inl payload ->
        let* body = eval (bind_env left payload env) yes in
        Ok (seq value body body.value)
      | Inr payload ->
        let* body = eval (bind_env right payload env) no in
        Ok (seq value body body.value)
      | _ -> Error Need_sum
    end
  | C_term.Act (atom, body) ->
    let* body = eval env body in
    let action = direct atom body.value in
    Ok { body with trace = cat (Atom action) body.trace;
      steps = Z.succ body.steps; work = Z.succ body.work }
  | C_term.Add (left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    begin
      match left.value, right.value with
      | Int left_int, Int right_int ->
        let* value = integer Int_work.Add Need_int left_int right_int in
        Ok (seq left right (Int value))
      | _ -> Error Need_int
    end
  | C_term.Sub (left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    begin
      match left.value, right.value with
      | Int left_int, Int right_int ->
        let* value = integer Int_work.Sub Need_int left_int right_int in
        Ok (seq left right (Int value))
      | _ -> Error Need_int
    end
  | C_term.Mul (left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    begin
      match left.value, right.value with
      | Int left_int, Int right_int ->
        let* value = integer Int_work.Mul Need_int left_int right_int in
        Ok (seq left right (Int value))
      | _ -> Error Need_int
    end
  | C_term.Div (left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    begin
      match left.value, right.value with
      | Int left_int, Int right_int ->
        let* value = integer Int_work.Div Divide_zero left_int right_int in
        Ok (seq left right (Int value))
      | _ -> Error Need_int
    end
  | C_term.Mod (left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    begin
      match left.value, right.value with
      | Int left_int, Int right_int ->
        let* value = integer Int_work.Mod Modulo_zero left_int right_int in
        Ok (seq left right (Int value))
      | _ -> Error Need_int
    end
  | C_term.Neg value ->
    let* value = eval env value in
    begin
      match value.value with
      | Int number ->
        let* result = integer Int_work.Neg Need_int number number in
        Ok { value with value = Int result;
          steps = Z.succ value.steps; work = Z.succ value.work }
      | _ -> Error Need_int
    end
  | C_term.Abs value ->
    let* value = eval env value in
    begin
      match value.value with
      | Int number ->
        let* result = integer Int_work.Abs Need_int number number in
        Ok { value with value = Int result;
          steps = Z.succ value.steps; work = Z.succ value.work }
      | _ -> Error Need_int
    end
  | C_term.Fit (target, value) ->
    let* value = eval env value in
    begin
      match value.value with
      | Int number ->
        let result =
          if C_type.admits target number then Inl value.value else Inr value.value
        in
        Ok { value with value = result; steps = Z.succ value.steps;
          work = Z.add value.work (Z.of_int 12) }
      | _ -> Error Need_int
    end
  | C_term.Wide value ->
    let* value = eval env value in
    begin
      match value.value with
      | Int _ ->
        Ok { value with steps = Z.succ value.steps; work = Z.succ value.work }
      | _ -> Error Need_int
    end
  | C_term.Length value ->
    let* value = eval env value in
    begin
      match value.value with
      | Pair (Int len, Vec items)
          when Z.sign len >= 0 && Z.leq len (Z.of_int (Array.length items)) ->
        Ok { value with value = Int len; steps = Z.succ value.steps;
          work = Z.succ value.work }
      | _ -> Error Need_seq
    end
  | C_term.Eq (typ, left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    let same, work = equal_run typ left.value right.value in
    let out = seq left right (Bool same) in
    Ok { out with work = Z.add out.work work }
  | C_term.Cmp (rel, left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    begin
      match left.value, right.value with
      | Int lhs, Int rhs ->
        let value =
          match rel with
          | C_term.Lt -> Z.lt lhs rhs
          | C_term.Le -> Z.leq lhs rhs
          | C_term.Gt -> Z.gt lhs rhs
          | C_term.Ge -> Z.geq lhs rhs
        in
        Ok (seq left right (Bool value))
      | _ -> Error Need_int
    end
  | C_term.Cat (left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    begin
      match left.value, right.value with
      | Bytes left_bytes, Bytes right_bytes ->
        let value = left_bytes ^ right_bytes in
        let* len = host_len (String.length value) in
        Ok (copy_seq len left right (Bytes value))
      | _ -> Error Need_bytes
    end
  | C_term.Take (len, value) ->
    let* value = eval env value in
    begin
      match value.value with
      | Bytes raw ->
        let* total = host_len (String.length raw) in
        if C_nat.le len total then
          let count = C_nat.to_int len in
          Ok (byte_one len value (String.sub raw 0 count))
        else Error (Byte_index (len, total))
      | _ -> Error Need_bytes
    end
  | C_term.Drop (len, value) ->
    let* value = eval env value in
    begin
      match value.value with
      | Bytes raw ->
        let* total = host_len (String.length raw) in
        begin
          match C_nat.sub total len with
          | Some rest ->
            Ok (byte_one rest value
              (String.sub raw (C_nat.to_int len) (C_nat.to_int rest)))
          | None -> Error (Byte_index (len, total))
        end
      | _ -> Error Need_bytes
    end
  | C_term.Vcat (left, right) ->
    let* left = eval env left in
    let* right = eval env right in
    begin
      match left.value, right.value with
      | Vec left_values, Vec right_values ->
        let values = Array.append left_values right_values in
        let* len = host_len (Array.length values) in
        Ok (copy_seq len left right (Vec values))
      | _ -> Error Need_vec
    end
  | C_term.At (index, value) ->
    let* value = eval env value in
    begin
      match value.value with
      | Vec values ->
        let* len = host_len (Array.length values) in
        if C_nat.lt index len then
          Ok { value with value = values.(C_nat.to_int index);
            steps = Z.succ value.steps; work = Z.succ value.work }
        else Error (Vec_index (index, len))
      | _ -> Error Need_vec
    end
  | C_term.Uncons value ->
    let* value = eval env value in
    begin
      match value.value with
      | Vec values when Array.length values > 0 ->
        let rest = Array.length values - 1 in
        let* len = host_len rest in
        let tail = Array.sub values 1 rest in
        Ok {
          value = Pair (values.(0), Vec tail);
          trace = value.trace;
          steps = Z.add (C_nat.to_z len) (Z.succ value.steps);
          work = Z.add (C_nat.to_z len) (Z.succ value.work);
        }
      | Vec values ->
        let* len = host_len (Array.length values) in
        Error (Vec_index (C_nat.zero, len))
      | _ -> Error Need_vec
    end
  | C_term.Vfold (vector, seed, fold) ->
    let* vector = eval env vector in
    let* seed = eval env seed in
    begin
      match vector.value with
      | Vec values ->
        let trace = cat vector.trace seed.trace in
        let steps = Z.succ (Z.add vector.steps seed.steps) in
        let work = Z.succ (Z.add vector.work seed.work) in
        eval_fold env fold values 0 seed.value trace steps work
      | Pair (Int len, Vec values)
          when Z.sign len >= 0 && Z.leq len (Z.of_int (Array.length values)) ->
        let trace = cat vector.trace seed.trace in
        let steps = Z.succ (Z.add vector.steps seed.steps) in
        let work = Z.succ (Z.add vector.work seed.work) in
        eval_fold env fold (Array.sub values 0 (Z.to_int len)) 0 seed.value
          trace steps work
      | _ -> Error Need_vec
    end
  | C_term.Step (cap, value) ->
    let* cap = eval env cap in
    let* value = eval env value in
    begin
      match cap.value with
      | Cap (kind, id) ->
        let out = seq cap value (Pair (cap.value, value.value)) in
        let action = {
          atom = C_eff.Write kind;
          payload = value.value;
          origin = Held (kind, id);
        } in
        Ok { out with trace = cat out.trace (Atom action) }
      | _ -> Error Need_cap
    end
  | C_term.Close cap ->
    let* cap = eval env cap in
    begin
      match cap.value with
      | Cap (kind, id) ->
        let action = {
          atom = C_eff.Close kind;
          payload = Unit;
          origin = Held (kind, id);
        } in
        Ok {
          value = Unit;
          trace = cat cap.trace (Atom action);
          steps = Z.succ cap.steps;
          work = Z.succ cap.work;
        }
      | _ -> Error Need_cap
    end

and eval_vec env values =
  let rec loop out trace steps work = function
    | [] ->
      Ok {
        value = Vec (Array.of_list (List.rev out));
        trace;
        steps;
        work;
      }
    | value :: rest ->
      let* value = eval env value in
      let trace = cat trace value.trace in
      let steps = Z.succ (Z.add steps value.steps) in
      let work = Z.succ (Z.add work value.work) in
      loop (value.value :: out) trace steps work rest
  in
  loop [] Nil Z.one Z.one values

and eval_fold env (fold : C_term.fold) values index state trace steps work =
  if index = Array.length values then
    Ok { value = state; trace; steps; work }
  else
    let body_env =
      bind_env fold.state state (bind_env fold.item values.(index) env)
    in
    let* body = eval body_env fold.body in
    let trace = cat trace body.trace in
    let steps = Z.succ (Z.add steps body.steps) in
    let work = Z.succ (Z.add work body.work) in
    eval_fold env fold values (index + 1) body.value trace steps work

let rec input_env env caps ids = function
  | [] -> Ok env
  | ((bind : C_term.bind), value) :: rest ->
    if Ids.mem bind.id ids then Error (Input bind.id)
    else
      let* caps = input_value bind caps value in
      input_env
        (bind_env bind value env)
        caps
        (Ids.add bind.id ids)
        rest

let rec input_binds count out = function
  | [] -> Ok (List.rev out)
  | _ when count = C_check.max_inputs ->
    Error (Static (C_check.Inputs (C_check.max_inputs, C_check.max_inputs + 1)))
  | (bind, _) :: rest -> input_binds (count + 1) (bind :: out) rest

let run_in ?(fuel = max_cost) inputs term =
  let* binds = input_binds 0 [] inputs in
  match C_check.check_in binds term with
  | Error error -> Error (Static error)
  | Ok info ->
    let limit = info.res.steps in
    if Z.sign fuel < 0 || Z.gt limit fuel then Error (Fuel (fuel, limit))
    else
    let* env = input_env [] Caps.empty Ids.empty inputs in
    let* done_ = eval env term in
    let actions = trace_list done_.trace in
    let plan = atoms actions in
    let used = C_eff.of_list plan in
    let actual = C_limit.used ~steps:done_.steps ~work:done_.work in
    if Z.gt done_.steps info.res.steps then
      Error (Cost (limit, done_.steps))
    else if Z.gt done_.work info.res.work then
      Error (Work (info.res.work, done_.work))
    else if not (C_limit.le actual info.res) then
      Error (Work (info.res.work, done_.work))
    else if not (C_eff.subset used info.eff) then Error (Effects (info.eff, used))
    else Ok {
      value = done_.value;
      plan;
      actions;
      steps = done_.steps;
      work = done_.work;
      row = info.eff;
      limit;
      work_limit = info.res.work;
    }

let run ?fuel term = run_in ?fuel [] term

let run_in_at schedule ~epoch inputs term =
  match C_rule.select schedule ~epoch with
  | Error error -> Error (Rule error)
  | Ok rule ->
    let fuel = Z.of_int (C_rule.rule_limits rule).fuel in
    begin
      match run_in ~fuel inputs term with
      | Error error -> Error (Run error)
      | Ok out -> Ok { rule = C_rule.id rule; out }
    end

let run_at schedule ~epoch term = run_in_at schedule ~epoch [] term

let text = function
  | Static error -> C_check.text error
  | Missing id -> "missing id = " ^ C_nat.text id
  | Need_bool -> "value expected = bool"
  | Need_int -> "value expected = int"
  | Divide_zero -> "integer division rejected = zero divisor"
  | Modulo_zero -> "integer remainder rejected = zero divisor"
  | Need_pair -> "value expected = pair"
  | Need_sum -> "value expected = sum"
  | Need_bytes -> "value expected = bytes"
  | Need_vec -> "value expected = vec"
  | Need_cap -> "value expected = cap"
  | Need_seq -> "value expected = sequence"
  | Byte_index (len, total) ->
    "byte index = " ^ C_nat.text len ^ " size = " ^ C_nat.text total
  | Vec_index (index, total) ->
    "vec index = " ^ C_nat.text index ^ " size = " ^ C_nat.text total
  | Input id -> "duplicate input id = " ^ C_nat.text id
  | Input_type (id, expected) ->
    "input id = " ^ C_nat.text id ^ " expected = " ^ C_type.text expected
  | Input_depth (id, limit, actual) ->
    "input id = " ^ C_nat.text id ^ " depth limit = " ^ string_of_int limit
    ^ " actual = " ^ string_of_int actual
  | Input_nodes (id, limit, actual) ->
    "input id = " ^ C_nat.text id ^ " node limit = " ^ string_of_int limit
    ^ " actual = " ^ string_of_int actual
  | Cap_dup (kind, id) ->
    "duplicate capability kind = " ^ C_nat.text kind ^ " id = " ^ C_nat.text id
  | Fuel (limit, needed) ->
    "fuel limit = " ^ Z.to_string limit ^ " needed = " ^ Z.to_string needed
  | Cost (limit, actual) ->
    "cost limit = " ^ Z.to_string limit ^ " actual = " ^ Z.to_string actual
  | Work (limit, actual) ->
    "work limit = " ^ Z.to_string limit ^ " actual = " ^ Z.to_string actual
  | Effects (row, used) ->
    "effects row = " ^ C_eff.text row ^ " actual = " ^ C_eff.text used

let rule_text = function
  | Rule error -> C_text.clip (C_rule.text error)
  | Run error -> text error