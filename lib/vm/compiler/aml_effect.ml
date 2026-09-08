(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type target =
  | State of string
  | Event of string
  | Fault of string

type bind = {
  atom : C_eff.atom;
  target : target;
}

type rollback =
  | Preserve
  | Journal
  | Record
  | Abort

type host =
  | Load of string * Contract_vm.v
  | Store of string * Contract_vm.v
  | Log of string * Contract_vm.v list
  | Reject of string * Contract_vm.v list

type item = {
  action : C_eval.action;
  host : host;
  rollback : rollback;
  ops : Contract_vm.instr list;
  cost : Z.t;
}

type entry =
  | State_entry of string * Oct_lang.typ
  | Event_entry of string * Oct_lang.typ list
  | Fault_entry of string * int * string

type t = (C_eff.atom * entry) list

module Key = struct
  type t = C_eff.atom * C_check.origin

  let compare (left_atom, left_origin) (right_atom, right_origin) =
    let order = C_eff.compare left_atom right_atom in
    if order <> 0 then order
    else
      match left_origin, right_origin with
      | C_check.Direct, C_check.Direct -> 0
      | C_check.Direct, C_check.Held _ -> -1
      | C_check.Held _, C_check.Direct -> 1
      | C_check.Held left, C_check.Held right -> C_nat.compare left right
end

module Quotas = Map.Make (Key)

type quota = {
  site : C_check.site;
  left : Z.t;
}

type sealed = {
  entries : t;
  quotas : quota Quotas.t;
  locations : C_check.location array;
  flow : C_check.flow;
  binds : C_term.bind list;
  term : C_term.t;
  typ : C_type.t;
}

type charge =
  | Exact
  | Storage

type block = {
  index : int;
  site : C_check.site;
  rollback : rollback;
  ops : Contract_vm.instr list;
  cost : Z.t;
  charge : charge;
}

type plan = {
  items : item list;
  cost : Z.t;
}

type prepared = {
  out : C_eval.out;
  plan : plan;
}

type error =
  | Program
  | Static of C_check.error
  | Run of C_eval.error
  | Input_count of int * int
  | Bind_limit of int
  | Atom_invalid of C_eff.atom
  | Atom_repeated of C_eff.atom
  | Target_kind of C_eff.atom * target
  | Target_absent of target
  | Target_repeated of target
  | Target_type of target * Oct_lang.typ
  | Binding_absent of C_eff.atom
  | Site_type of C_eff.atom * C_type.t * C_type.t
  | Site_payload of C_eff.atom * C_type.t
  | Site_origin of C_eff.atom * C_check.origin
  | Site_absent of C_eff.atom
  | Site_limit of C_eff.atom
  | Trace
  | Site_index of int
  | Register of int
  | Payload_regs of C_eff.atom * int * int
  | Scratch_regs of C_eff.atom * int * int
  | Register_alias of C_eff.atom * int
  | Payload of C_eff.atom
  | Origin of C_eff.atom * C_eval.origin
  | Host_limit of C_eff.atom * int
  | After_fail of C_eff.atom
  | Close_unavailable of C_nat.t * C_nat.t

let bind atom target = { atom; target }

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let core_type = function
  | Oct_lang.TInt -> Some C_type.Int
  | Oct_lang.TBool -> Some C_type.Bool
  | Oct_lang.TBytes32 ->
    Option.map (fun len -> C_type.Bytes len) (C_nat.of_int 32)
  | Oct_lang.TString
  | Oct_lang.TAddress
  | Oct_lang.TBytes
  | Oct_lang.TU64
  | Oct_lang.TU128
  | Oct_lang.TU256
  | Oct_lang.TCipher
  | Oct_lang.TPubKey
  | Oct_lang.TMap _
  | Oct_lang.TList _
  | Oct_lang.TStruct _
  | Oct_lang.TEnum _
  | Oct_lang.TOption _
  | Oct_lang.TTuple _
  | Oct_lang.TVoid -> None

let target_kind atom target =
  match atom, target with
  | (C_eff.Read _ | C_eff.Write _), State _
  | C_eff.Emit _, Event _
  | C_eff.Fail _, Fault _ -> true
  | C_eff.Close _, _
  | (C_eff.Read _ | C_eff.Write _), (Event _ | Fault _)
  | C_eff.Emit _, (State _ | Fault _)
  | C_eff.Fail _, (State _ | Event _) -> false

let target_one target select rows =
  match List.filter select rows with
  | [] -> Error (Target_absent target)
  | [item] -> Ok item
  | _ -> Error (Target_repeated target)

let target_entry program atom target =
  match target with
  | State name ->
    begin
      match target_one target
        (fun field -> String.equal field.Oct_lang.sf_name name)
        program.Oct_lang.state with
      | Error error -> Error error
      | Ok field ->
        begin
          match core_type field.sf_typ with
          | Some _ -> Ok (State_entry (name, field.sf_typ))
          | None -> Error (Target_type (target, field.sf_typ))
        end
    end
  | Event name ->
    begin
      match target_one target
        (fun event -> String.equal event.Oct_lang.ev_name name)
        program.Oct_lang.events with
      | Error error -> Error error
      | Ok event ->
        let types = List.map (fun (_, typ, _) -> typ) event.ev_fields in
        let rec check = function
          | [] -> Ok (Event_entry (name, types))
          | typ :: rest ->
            begin
              match core_type typ with
              | Some _ -> check rest
              | None -> Error (Target_type (target, typ))
            end
        in
        if List.length types > 64 then
          Error (Host_limit (atom, List.length types))
        else check types
    end
  | Fault name ->
    begin
      match target_one target
        (fun fault -> String.equal fault.Oct_lang.err_name name)
        program.Oct_lang.errors with
      | Error error -> Error error
      | Ok fault -> Ok (Fault_entry (name, fault.err_code, fault.err_msg))
    end

let make program binds =
  let count = List.length binds in
  if program.Oct_lang.declaration <> Oct_lang.ProgramDecl then Error Program
  else if count > C_check.max_nodes then Error (Bind_limit count)
  else
    let rec walk seen out = function
      | [] -> Ok (List.rev out)
      | item :: rest ->
        if not (C_eff.valid item.atom) then Error (Atom_invalid item.atom)
        else if List.exists (fun atom -> atom = item.atom) seen then
          Error (Atom_repeated item.atom)
        else if not (target_kind item.atom item.target) then
          Error (Target_kind (item.atom, item.target))
        else
          let* entry = target_entry program item.atom item.target in
          walk (item.atom :: seen) ((item.atom, entry) :: out) rest
    in
    walk [] [] binds

let find atom entries =
  List.find_opt (fun (found, _) -> found = atom) entries

let rec event_type = function
  | [] -> Some C_type.Unit
  | [typ] -> core_type typ
  | typ :: rest ->
    Option.bind (core_type typ) (fun head ->
      Option.map (fun tail -> C_type.Pair (head, tail)) (event_type rest))

let rec fault_type = function
  | C_type.Unit | C_type.Bool | C_type.Int | C_type.Num _ | C_type.Bytes _ -> true
  | C_type.Pair (left, right) -> fault_type left && fault_type right
  | C_type.Vec _ | C_type.Seq _ | C_type.Sum _ | C_type.Cap _
  | C_type.Enc _ -> false

let site_origin site =
  match site.C_check.atom, site.origin with
  | (C_eff.Read _ | C_eff.Write _ | C_eff.Emit _ | C_eff.Fail _),
    C_check.Direct -> Ok ()
  | C_eff.Write kind, C_check.Held actual
  | C_eff.Close kind, C_check.Held actual
      when C_nat.equal kind actual -> Ok ()
  | atom, found -> Error (Site_origin (atom, found))

let site_type expected site =
  if C_type.equal expected site.C_check.payload then Ok ()
  else Error (Site_type (site.atom, expected, site.payload))

let check_site entries site =
  let* () = site_origin site in
  match find site.C_check.atom entries with
  | None -> Error (Binding_absent site.atom)
  | Some (_, State_entry (_, typ)) ->
    begin
      match core_type typ with
      | Some expected -> site_type expected site
      | None -> Error (Site_payload (site.atom, site.payload))
    end
  | Some (_, Event_entry (_, types)) ->
    begin
      match event_type types with
      | Some expected -> site_type expected site
      | None -> Error (Site_payload (site.atom, site.payload))
    end
  | Some (_, Fault_entry _) ->
    if fault_type site.payload then Ok ()
    else Error (Site_payload (site.atom, site.payload))

let site_key site =
  site.C_check.atom, site.origin

let seal_info entries binds term info =
  let locations = C_check.locations info in
  let rec walk quotas = function
      | [] -> Ok {
          entries;
          quotas;
          locations = Array.of_list locations;
          flow = info.C_check.flow;
          binds;
          term;
          typ = info.typ;
        }
    | location :: rest ->
      let site = location.C_check.site in
      let* () = check_site entries site in
      let key = site_key site in
      let quota =
        match Quotas.find_opt key quotas with
        | Some found -> { found with left = Z.add found.left location.count }
        | None -> { site; left = location.count }
      in
      walk (Quotas.add key quota quotas) rest
  in
  walk Quotas.empty locations

let seal entries binds term =
  match C_check.check_in binds term with
  | Ok info -> seal_info entries binds term info
  | Error error -> Error (Static error)

let entries value = value
let registry sealed = sealed.entries
let prog sealed = { C_low.inputs = sealed.binds; term = sealed.term }
let typ sealed = sealed.typ
let flow sealed = sealed.flow
let locations sealed = sealed.locations

let scalar typ payload =
  match core_type typ with
  | None -> None
  | Some expected when not (C_feed.typed expected payload) -> None
  | Some _ ->
    begin
      match typ, payload with
      | Oct_lang.TInt, C_eval.Int value -> Some (Contract_vm.VInt value)
      | Oct_lang.TBool, C_eval.Bool value -> Some (Contract_vm.VBool value)
      | Oct_lang.TBytes32, C_eval.Bytes value ->
        Some (Contract_vm.VBytes32 value)
      | _ -> None
    end

let event_values types payload =
  let rec walk out types payload =
    match types, payload with
    | [], C_eval.Unit -> Some (List.rev out)
    | [typ], value ->
      Option.map (fun item -> List.rev (item :: out)) (scalar typ value)
    | typ :: rest, C_eval.Pair (head, tail) ->
      Option.bind (scalar typ head) (fun item -> walk (item :: out) rest tail)
    | [], _ | _ :: _, _ -> None
  in
  walk [] types payload

let fault_values payload =
  let rec walk out = function
    | [] -> Some (List.rev out)
    | C_eval.Unit :: rest -> walk out rest
    | C_eval.Int value :: rest -> walk (Contract_vm.VInt value :: out) rest
    | C_eval.Bool value :: rest -> walk (Contract_vm.VBool value :: out) rest
    | C_eval.Bytes value :: rest -> walk (Contract_vm.VBytes value :: out) rest
    | C_eval.Pair (left, right) :: rest -> walk out (left :: right :: rest)
    | C_eval.Vec _ :: _
    | C_eval.Cap _ :: _
    | C_eval.Enc _ :: _
    | C_eval.Inl _ :: _
    | C_eval.Inr _ :: _ -> None
  in
  if C_feed.shaped payload then walk [] [payload] else None

let origin action =
  match action.C_eval.atom, action.origin with
  | C_eff.Write _, C_eval.Direct
  | (C_eff.Read _ | C_eff.Emit _ | C_eff.Fail _), C_eval.Direct -> Ok ()
  | C_eff.Write kind, C_eval.Held (actual, _)
  | C_eff.Close kind, C_eval.Held (actual, _)
      when C_nat.equal kind actual -> Ok ()
  | atom, found -> Error (Origin (atom, found))

let op_cost ops =
  List.fold_left
    (fun total op -> Z.add total (Z.of_int (Contract_vm.effort_cost op)))
    Z.zero ops

let rec width = function
  | C_type.Unit -> Some 0
  | C_type.Bool | C_type.Int | C_type.Num _ | C_type.Bytes _ -> Some 1
  | C_type.Pair (left, right) ->
    Option.bind (width left) (fun first ->
      Option.map (fun second -> first + second) (width right))
  | C_type.Vec _ | C_type.Seq _ | C_type.Sum _ | C_type.Cap _
  | C_type.Enc _ -> None

let register reg =
  if reg >= 0 && reg < Contract_vm.register_count then Ok ()
  else Error (Register reg)

let payload_regs atom expected regs =
  let actual = List.length regs in
  if actual <> expected then Error (Payload_regs (atom, expected, actual))
  else
    let rec walk seen = function
      | [] -> Ok ()
      | reg :: rest ->
        let* () = register reg in
        if List.mem reg seen then Error (Register_alias (atom, reg))
        else walk (reg :: seen) rest
    in
    walk [] regs

let scratch_regs atom count payload regs =
  let actual = List.length regs in
  if actual <> count then Error (Scratch_regs (atom, count, actual))
  else
    let rec take left out = function
      | _ when left = 0 -> Ok (List.rev out)
      | [] -> Error (Scratch_regs (atom, count, actual))
      | reg :: rest ->
        let* () = register reg in
        if List.mem reg payload || List.mem reg out then
          Error (Register_alias (atom, reg))
        else take (left - 1) (reg :: out) rest
    in
    take count [] regs

let block index site rollback ops charge = {
  index;
  site;
  rollback;
  ops;
  cost = op_cost ops;
  charge;
}

let lower sealed ~index ~payload ~scratch =
  if index < 0 || index >= Array.length sealed.locations then
    Error (Site_index index)
  else
    let location = sealed.locations.(index) in
    let site = location.C_check.site in
    let atom = site.atom in
    let* () =
      if Z.gt location.count Z.zero then Ok () else Error (Site_limit atom)
    in
    let* count =
      match width site.payload with
      | Some value -> Ok value
      | None -> Error (Site_payload (atom, site.payload))
    in
    let* () = payload_regs atom count payload in
    match find atom sealed.entries with
    | None -> Error (Binding_absent atom)
    | Some (_, State_entry (name, _)) ->
      begin
        match atom, payload with
        | C_eff.Read _, [value] ->
          let* scratch = scratch_regs atom 2 payload scratch in
          begin
            match scratch with
            | [loaded; guard] ->
              let ops = [
                Contract_vm.SLOAD (loaded, name);
                Contract_vm.EQ (guard, loaded, value);
                Contract_vm.ASSERT guard;
              ] in
              Ok (block index site Preserve ops Exact)
            | _ -> Error (Scratch_regs (atom, 2, List.length scratch))
          end
        | C_eff.Write _, [value] ->
          let* _ = scratch_regs atom 0 payload scratch in
          let ops = [Contract_vm.SSTORE (name, value)] in
          Ok (block index site Journal ops Storage)
        | _ -> Error (Payload_regs (atom, 1, List.length payload))
      end
    | Some (_, Event_entry (name, _)) ->
      let* _ = scratch_regs atom 0 payload scratch in
      let ops = [Contract_vm.EMIT (name, payload)] in
      Ok (block index site Record ops Exact)
    | Some (_, Fault_entry (name, code, message)) ->
      let* scratch = scratch_regs atom 2 payload scratch in
      begin
        match scratch with
        | [code_reg; message_reg] ->
          let regs = code_reg :: message_reg :: payload in
          let ops = [
            Contract_vm.LDI (code_reg, Contract_vm.VInt (Z.of_int code));
            Contract_vm.LDI (message_reg, Contract_vm.VString message);
            Contract_vm.EMIT ("Error:" ^ name, regs);
            Contract_vm.REVERT;
          ] in
          Ok (block index site Abort ops Exact)
        | _ -> Error (Scratch_regs (atom, 2, List.length scratch))
      end

let item action host rollback ops dynamic = {
  action;
  host;
  rollback;
  ops;
  cost = Z.add (op_cost ops) dynamic;
}

let load action name typ =
  match scalar typ action.C_eval.payload with
  | None -> Error (Payload action.atom)
  | Some value ->
    let ops = [
      Contract_vm.SLOAD (0, name);
      Contract_vm.EQ (1, 0, 2);
      Contract_vm.ASSERT 1;
    ] in
    Ok (item action (Load (name, value)) Preserve ops Z.zero)

let store action name typ =
  match scalar typ action.C_eval.payload with
  | None -> Error (Payload action.atom)
  | Some value ->
    let size = String.length (Contract_vm.to_string value) in
    if size > Contract_vm.max_storage_value_len then
      Error (Host_limit (action.atom, size))
    else
      let ops = [Contract_vm.SSTORE (name, 0)] in
      let dynamic = Z.of_int (Contract_vm.storage_work value) in
      Ok (item action (Store (name, value)) Journal ops dynamic)

let emit action name types =
  match event_values types action.C_eval.payload with
  | None -> Error (Payload action.atom)
  | Some values ->
    let regs = List.init (List.length values) Fun.id in
    let ops = [Contract_vm.EMIT (name, regs)] in
    Ok (item action (Log (name, values)) Record ops Z.zero)

let fault action name code message =
  match fault_values action.C_eval.payload with
  | None -> Error (Payload action.atom)
  | Some values when List.length values > 62 ->
    Error (Host_limit (action.atom, List.length values + 2))
  | Some values ->
    let code_value = Contract_vm.VInt (Z.of_int code) in
    let message_value = Contract_vm.VString message in
    let regs = 0 :: 1 :: List.init (List.length values) (fun index -> index + 2) in
    let label = "Error:" ^ name in
    let ops = [
      Contract_vm.LDI (0, code_value);
      Contract_vm.LDI (1, message_value);
      Contract_vm.EMIT (label, regs);
      Contract_vm.REVERT;
    ] in
    Ok (item action (Reject (name, code_value :: message_value :: values))
      Abort ops Z.zero)

let action_key action =
  let origin =
    match action.C_eval.origin with
    | C_eval.Direct -> C_check.Direct
    | C_eval.Held (kind, _) -> C_check.Held kind
  in
  action.C_eval.atom, origin

let consume quotas action =
  let key = action_key action in
  match Quotas.find_opt key quotas with
  | Some (quota : quota)
      when not (C_feed.typed quota.site.payload action.C_eval.payload) ->
    Error (Payload action.atom)
  | Some (quota : quota) when Z.leq quota.left Z.zero ->
    Error (Site_limit action.atom)
  | Some (quota : quota) ->
    Ok (Quotas.add key { quota with left = Z.pred quota.left } quotas)
  | None -> Error (Site_absent action.atom)

module Pos = Set.Make (Int)

let same_site site action =
  let same_origin =
    match site.C_check.origin, action.C_eval.origin with
    | C_check.Direct, C_eval.Direct -> true
    | C_check.Held expected, C_eval.Held (actual, _) ->
      C_nat.equal expected actual
    | C_check.Direct, C_eval.Held _
    | C_check.Held _, C_eval.Direct -> false
  in
  site.atom = action.atom
  && same_origin
  && C_feed.typed site.payload action.payload

let trace_flow flow actions =
  let actions = Array.of_list actions in
  let rec walk flow positions =
    match flow with
    | C_check.Pure -> positions
    | C_check.Action site ->
      Pos.fold
        (fun index out ->
          if index < Array.length actions && same_site site actions.(index) then
            Pos.add (index + 1) out
          else out)
        positions Pos.empty
    | C_check.Seq (first, second) -> walk second (walk first positions)
    | C_check.Fork (first, second) ->
      Pos.union (walk first positions) (walk second positions)
    | C_check.Loop (count, body) ->
      let rec repeat left current =
        if left = 0 || Pos.is_empty current then current
        else repeat (left - 1) (walk body current)
      in
      repeat (C_nat.to_int count) positions
  in
  Pos.mem (Array.length actions) (walk flow (Pos.singleton 0))

let trace sealed actions = trace_flow sealed.flow actions

let plan_actions sealed actions =
  let entries = sealed.entries in
  let rec walk quotas out cost = function
    | [] ->
      let plan = { items = List.rev out; cost } in
      if trace sealed actions then Ok plan else Error Trace
    | action :: rest ->
      let* () = origin action in
      begin
        match action.C_eval.atom, action.origin with
        | C_eff.Close kind, C_eval.Held (_, id) ->
          Error (Close_unavailable (kind, id))
        | C_eff.Close _, C_eval.Direct ->
          Error (Origin (action.atom, action.origin))
        | atom, _ ->
          let* quotas = consume quotas action in
          begin
            match find atom entries with
            | None -> Error (Binding_absent atom)
            | Some (_, entry) ->
              let planned =
                match entry with
                | State_entry (name, typ) ->
                  begin
                    match atom with
                    | C_eff.Read _ -> load action name typ
                    | C_eff.Write _ -> store action name typ
                    | C_eff.Emit _ | C_eff.Fail _ | C_eff.Close _ ->
                      Error (Binding_absent atom)
                  end
                | Event_entry (name, types) -> emit action name types
                | Fault_entry (name, code, message) ->
                  fault action name code message
              in
              let* planned = planned in
              begin
                match atom, rest with
                | C_eff.Fail _, next :: _ -> Error (After_fail next.atom)
                | C_eff.Fail _, []
                | (C_eff.Read _ | C_eff.Write _ | C_eff.Emit _), _ ->
                  walk quotas (planned :: out) (Z.add cost planned.cost) rest
                | C_eff.Close _, _ -> Error (Binding_absent atom)
              end
          end
      end
  in
  walk sealed.quotas [] Z.zero actions

let prepare sealed values =
  let expected = List.length sealed.binds in
  let actual = List.length values in
  if expected <> actual then Error (Input_count (expected, actual))
  else
    let inputs = List.combine sealed.binds values in
    match C_eval.run_in inputs sealed.term with
    | Error error -> Error (Run error)
    | Ok out ->
      let* plan = plan_actions sealed out.actions in
      Ok { out; plan }

let target_text = function
  | State name -> "state:" ^ name
  | Event name -> "event:" ^ name
  | Fault name -> "error:" ^ name

let text = function
  | Program -> "effect host requires Program"
  | Static error -> "effect static check failed reason = " ^ C_check.text error
  | Run error -> "effect evaluation failed reason = " ^ C_eval.text error
  | Input_count (expected, actual) ->
    Printf.sprintf "effect input count differs expected = %d actual = %d"
      expected actual
  | Bind_limit actual ->
    Printf.sprintf "effect binding count limit = %d actual = %d"
      C_check.max_nodes actual
  | Atom_invalid atom -> "effect atom is invalid atom = " ^ C_eff.atom_text atom
  | Atom_repeated atom -> "effect atom is repeated atom = " ^ C_eff.atom_text atom
  | Target_kind (atom, target) ->
    "effect target kind differs atom = " ^ C_eff.atom_text atom
    ^ " target = " ^ target_text target
  | Target_absent target -> "effect target is absent target = " ^ target_text target
  | Target_repeated target ->
    "effect target is repeated target = " ^ target_text target
  | Target_type (target, typ) ->
    "effect target type is unsupported target = " ^ target_text target
    ^ " type = " ^ Oct_lang.typ_to_string typ
  | Binding_absent atom ->
    "effect binding is absent atom = " ^ C_eff.atom_text atom
  | Site_type (atom, expected, actual) ->
    "effect site type differs atom = " ^ C_eff.atom_text atom
    ^ " expected = " ^ C_type.text expected
    ^ " actual = " ^ C_type.text actual
  | Site_payload (atom, typ) ->
    "effect site payload is unsupported atom = " ^ C_eff.atom_text atom
    ^ " type = " ^ C_type.text typ
  | Site_origin (atom, _) ->
    "effect site origin differs atom = " ^ C_eff.atom_text atom
  | Site_absent atom ->
    "effect action is not proved atom = " ^ C_eff.atom_text atom
  | Site_limit atom ->
    "effect action exceeds proved count atom = " ^ C_eff.atom_text atom
  | Trace -> "effect action trace differs"
  | Site_index index ->
    Printf.sprintf "effect site index is invalid index = %d" index
  | Register reg ->
    Printf.sprintf "effect register is invalid register = %d" reg
  | Payload_regs (atom, expected, actual) ->
    Printf.sprintf
      "effect payload register count differs atom = %s expected = %d actual = %d"
      (C_eff.atom_text atom) expected actual
  | Scratch_regs (atom, expected, actual) ->
    Printf.sprintf
      "effect scratch register count differs atom = %s expected = %d actual = %d"
      (C_eff.atom_text atom) expected actual
  | Register_alias (atom, reg) ->
    Printf.sprintf "effect register aliases atom = %s register = %d"
      (C_eff.atom_text atom) reg
  | Payload atom -> "effect payload differs atom = " ^ C_eff.atom_text atom
  | Origin (atom, _) -> "effect origin differs atom = " ^ C_eff.atom_text atom
  | Host_limit (atom, actual) ->
    Printf.sprintf "effect host limit atom = %s actual = %d"
      (C_eff.atom_text atom) actual
  | After_fail atom ->
    "effect follows failure atom = " ^ C_eff.atom_text atom
  | Close_unavailable (kind, id) ->
    "effect close host is absent identity = " ^ C_nat.text kind
    ^ ":" ^ C_nat.text id