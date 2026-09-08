(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type op =
  | Mul of C_nat.t
  | Recrypt of C_nat.t

type enc = {
  key : C_nat.t;
  rem : C_nat.t;
}

type cfg = {
  full : C_nat.t;
  addw : C_nat.t;
  mulw : C_nat.t;
  rew : C_nat.t;
}

module Key = struct
  type t = C_nat.t
  let compare = C_nat.compare
end

module Pmap = Map.Make (Key)
module Smap = Map.Make (String)

type profile = cfg Pmap.t

type param = {
  id : C_nat.t;
  pkey : C_nat.t;
  cap : C_nat.t;
}

type catalog = {
  ids : unit Pmap.t;
  keys : param Pmap.t Pmap.t;
}

type input = {
  name : C_syn.name;
  raw_key : Z.t;
  raw_rem : Z.t;
}

type env = enc Smap.t

type t =
  | Var of C_syn.name
  | Trim of Z.t * t
  | Add of t * t
  | Mul_t of t * t
  | Recrypt_t of t

type node =
  | Nvar of C_syn.name
  | Ntrim of Z.t * t
  | Nadd of t * t
  | Nmul of t * t
  | Nre of t

type host =
  | Harg of C_syn.name * enc
  | Htrim of C_nat.t * host * enc
  | Hadd of host * host * enc
  | Hmul of host * host * enc
  | Hre of host * enc

type ins =
  | Load of C_syn.name * enc
  | Narrow of C_nat.t * enc
  | Add_cipher of enc
  | Mul_cipher of enc
  | Recrypt_cipher of enc

type pending =
  | Visit of host
  | Emit of ins

type trace =
  | Nil
  | Atom of op
  | Cat of trace * trace

type raw = {
  typ : enc;
  steps : C_nat.t;
  work : C_nat.t;
  peak : C_nat.t;
  trace : trace;
  host : host;
}

type info = {
  typ : enc;
  steps : C_nat.t;
  work : C_nat.t;
  peak : C_nat.t;
  plan : op list;
}

type field =
  | Key
  | Depth
  | Add_work
  | Mul_work
  | Re_work
  | Param_id
  | Param_cap

type axis =
  | Steps
  | Work

type error =
  | Nat of field * Z.t
  | Zero of field
  | Pdup of C_nat.t
  | Pcount of int * int
  | Cdup_id of C_nat.t
  | Cdup_cap of C_nat.t * C_nat.t
  | Ccount of int * int
  | No_param of C_nat.t * C_nat.t
  | No_key of C_nat.t
  | Idup of string
  | Icount of int * int
  | Idepth of C_nat.t * C_nat.t * C_nat.t
  | Free of string
  | Need of enc * enc
  | Mul_zero of C_nat.t
  | Re_need of C_nat.t * C_nat.t
  | Depth_max of int * int
  | Nodes of int * int
  | Stat of axis * Z.t

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let nat field value =
  match C_nat.make value with
  | Some value -> Ok value
  | None -> Error (Nat (field, value))

let positive field value =
  let* value = nat field value in
  if C_nat.equal value C_nat.zero then Error (Zero field) else Ok value

let cfg ~depth ~add ~mul ~recrypt =
  let* full = nat Depth depth in
  let* addw = positive Add_work add in
  let* mulw = positive Mul_work mul in
  let* rew = positive Re_work recrypt in
  Ok { full; addw; mulw; rew }

let profile values =
  let rec walk count out = function
    | [] -> Ok out
    | _ when count = C_rule.local.inputs ->
        Error (Pcount (C_rule.local.inputs, C_rule.local.inputs + 1))
    | (raw, cfg) :: rest ->
        let* key = nat Key raw in
        if Pmap.mem key out then Error (Pdup key)
        else walk (count + 1) (Pmap.add key cfg out) rest
  in
  walk 0 Pmap.empty values

let param ~id ~key ~cap =
  let* id = nat Param_id id in
  let* pkey = nat Key key in
  let* cap = nat Param_cap cap in
  Ok { id; pkey; cap }

let catalog values =
  let rec walk count ids keys = function
    | [] -> Ok { ids; keys }
    | _ when count = C_rule.local.inputs ->
        Error (Ccount (C_rule.local.inputs, C_rule.local.inputs + 1))
    | value :: rest ->
        if Pmap.mem value.id ids then Error (Cdup_id value.id)
        else
          let caps =
            match Pmap.find_opt value.pkey keys with
            | Some caps -> caps
            | None -> Pmap.empty
          in
          if Pmap.mem value.cap caps then
            Error (Cdup_cap (value.pkey, value.cap))
          else
            let ids = Pmap.add value.id () ids in
            let caps = Pmap.add value.cap value caps in
            let keys = Pmap.add value.pkey caps keys in
            walk (count + 1) ids keys rest
  in
  walk 0 Pmap.empty Pmap.empty values

let select catalog ~key ~need =
  if not (C_nat.valid key) then Error (Nat (Key, C_nat.to_z key))
  else if not (C_nat.valid need) then Error (Nat (Param_cap, C_nat.to_z need))
  else
    match Pmap.find_opt key catalog.keys with
    | None -> Error (No_param (key, need))
    | Some caps ->
        begin
          match Pmap.to_seq_from need caps () with
          | Seq.Nil -> Error (No_param (key, need))
          | Seq.Cons ((_, value), _) -> Ok value
        end

let params catalog info = select catalog ~key:info.typ.key ~need:info.peak

let input name ~key ~rem = { name; raw_key = key; raw_rem = rem }

let find_cfg profile key =
  match Pmap.find_opt key profile with
  | Some cfg -> Ok cfg
  | None -> Error (No_key key)

let full profile key =
  let* cfg = find_cfg profile key in
  Ok cfg.full

let legal profile typ =
  let* cfg = find_cfg profile typ.key in
  if C_nat.le typ.rem cfg.full then Ok cfg
  else Error (Idepth (typ.key, typ.rem, cfg.full))

let env profile values =
  let rec walk count out = function
    | [] -> Ok out
    | _ when count = C_rule.local.inputs ->
        Error (Icount (C_rule.local.inputs, C_rule.local.inputs + 1))
    | value :: rest ->
        let name = C_syn.name_text value.name in
        if Smap.mem name out then Error (Idup name)
        else
          let* key = nat Key value.raw_key in
          let* rem = nat Depth value.raw_rem in
          let typ = { key; rem } in
          let* _ = legal profile typ in
          walk (count + 1) (Smap.add name typ out) rest
  in
  walk 0 Smap.empty values

let var name = Var name
let trim rem term = Trim (rem, term)
let add left right = Add (left, right)
let mul left right = Mul_t (left, right)
let recrypt term = Recrypt_t term

let node = function
  | Var name -> Nvar name
  | Trim (rem, term) -> Ntrim (rem, term)
  | Add (left, right) -> Nadd (left, right)
  | Mul_t (left, right) -> Nmul (left, right)
  | Recrypt_t term -> Nre term

let same left right =
  C_nat.equal left.key right.key && C_nat.equal left.rem right.rem

let max left right = if C_nat.le left right then right else left

let used cfg rem =
  match C_nat.sub cfg.full rem with
  | Some value -> value
  | None -> C_nat.zero

let sum axis values =
  let value =
    List.fold_left (fun out value -> Z.add out (C_nat.to_z value)) Z.zero values
  in
  match C_nat.make value with
  | Some value -> Ok value
  | None -> Error (Stat (axis, value))

let cat left right =
  match left, right with
  | Nil, trace | trace, Nil -> trace
  | _ -> Cat (left, right)

let trace_list trace =
  let rec walk out = function
    | [] -> List.rev out
    | Nil :: rest -> walk out rest
    | Atom op :: rest -> walk (op :: out) rest
    | Cat (left, right) :: rest -> walk out (left :: right :: rest)
  in
  walk [] [trace]

let shape term =
  let rec walk nodes = function
    | [] -> Ok ()
    | (depth, _) :: _ when depth > C_rule.local.tm_depth ->
        Error (Depth_max (C_rule.local.tm_depth, depth))
    | _ when nodes >= C_rule.local.tm_nodes ->
        Error (Nodes (C_rule.local.tm_nodes, nodes + 1))
    | (depth, term) :: rest ->
        let next = depth + 1 in
        begin
          match term with
          | Var _ -> walk (nodes + 1) rest
          | Trim (_, term) | Recrypt_t term ->
              walk (nodes + 1) ((next, term) :: rest)
          | Add (left, right) | Mul_t (left, right) ->
              walk (nodes + 1) ((next, left) :: (next, right) :: rest)
        end
  in
  walk 0 [0, term]

let rec run profile env = function
  | Var name ->
      let text = C_syn.name_text name in
      begin
        match Smap.find_opt text env with
        | None -> Error (Free text)
        | Some typ ->
            let* cfg = legal profile typ in
            Ok {
              typ;
              steps = C_nat.zero;
              work = C_nat.zero;
              peak = used cfg typ.rem;
              trace = Nil;
              host = Harg (name, typ);
            }
      end
  | Trim (raw, term) ->
      let* out = run profile env term in
      let* rem = nat Depth raw in
      if not (C_nat.le rem out.typ.rem) then
        Error (Idepth (out.typ.key, rem, out.typ.rem))
      else
        let* cfg = find_cfg profile out.typ.key in
        let typ = { out.typ with rem } in
        Ok {
          out with
          typ;
          peak = max out.peak (used cfg rem);
          host = Htrim (rem, out.host, typ);
        }
  | Add (left, right) ->
      let* left = run profile env left in
      let* right = run profile env right in
      if not (same left.typ right.typ) then Error (Need (left.typ, right.typ))
      else
        let* cfg = find_cfg profile left.typ.key in
        let* steps = sum Steps [left.steps; right.steps; C_nat.one] in
        let* work = sum Work [left.work; right.work; cfg.addw] in
        Ok {
          typ = left.typ;
          steps;
          work;
          peak = max left.peak right.peak;
          trace = cat left.trace right.trace;
          host = Hadd (left.host, right.host, left.typ);
        }
  | Mul_t (left, right) ->
      let* left = run profile env left in
      let* right = run profile env right in
      if not (same left.typ right.typ) then Error (Need (left.typ, right.typ))
      else if C_nat.equal left.typ.rem C_nat.zero then
        Error (Mul_zero left.typ.key)
      else
        let* cfg = find_cfg profile left.typ.key in
        let rem =
          match C_nat.sub left.typ.rem C_nat.one with
          | Some value -> value
          | None -> C_nat.zero
        in
        let* steps = sum Steps [left.steps; right.steps; C_nat.one] in
        let* work = sum Work [left.work; right.work; cfg.mulw] in
        let typ = { left.typ with rem } in
        Ok {
          typ;
          steps;
          work;
          peak = max (max left.peak right.peak) (used cfg rem);
          trace = cat (cat left.trace right.trace) (Atom (Mul typ.key));
          host = Hmul (left.host, right.host, typ);
        }
  | Recrypt_t term ->
      let* out = run profile env term in
      if not (C_nat.equal out.typ.rem C_nat.zero) then
        Error (Re_need (out.typ.key, out.typ.rem))
      else
        let* cfg = find_cfg profile out.typ.key in
        let* steps = sum Steps [out.steps; C_nat.one] in
        let* work = sum Work [out.work; cfg.rew] in
        let typ = { out.typ with rem = cfg.full } in
        Ok {
          typ;
          steps;
          work;
          peak = max out.peak cfg.full;
          trace = cat out.trace (Atom (Recrypt out.typ.key));
          host = Hre (out.host, typ);
        }

let host_type = function
  | Harg (_, typ) | Htrim (_, _, typ) | Hadd (_, _, typ)
  | Hmul (_, _, typ) | Hre (_, typ) -> typ

let rec host_term = function
  | Harg (name, _) -> var name
  | Htrim (rem, term, _) -> trim (C_nat.to_z rem) (host_term term)
  | Hadd (left, right, _) -> add (host_term left) (host_term right)
  | Hmul (left, right, _) -> mul (host_term left) (host_term right)
  | Hre (term, _) -> recrypt (host_term term)

let rec host_eval profile env = function
  | Harg (name, out) ->
      begin
        match Smap.find_opt (C_syn.name_text name) env with
        | Some typ when same typ out -> Some out
        | _ -> None
      end
  | Htrim (rem, term, out) ->
      begin
        match host_eval profile env term with
        | Some prior when C_nat.le rem prior.rem ->
            begin
              match Pmap.find_opt prior.key profile with
              | Some _ ->
                  let typ = { prior with rem } in
                  if same typ out then Some out else None
              | None -> None
            end
        | _ -> None
      end
  | Hadd (left, right, out) ->
      begin
        match host_eval profile env left, host_eval profile env right with
        | Some left, Some right when same left right ->
            begin
              match Pmap.find_opt left.key profile with
              | Some _ when same left out -> Some out
              | _ -> None
            end
        | _, _ -> None
      end
  | Hmul (left, right, out) ->
      begin
        match host_eval profile env left, host_eval profile env right with
        | Some left, Some right
            when same left right && not (C_nat.equal left.rem C_nat.zero) ->
            begin
              match Pmap.find_opt left.key profile,
                    C_nat.sub left.rem C_nat.one with
              | Some _, Some rem ->
                  let typ = { left with rem } in
                  if same typ out then Some out else None
              | _, _ -> None
            end
        | _, _ -> None
      end
  | Hre (term, out) ->
      begin
        match host_eval profile env term with
        | Some prior when C_nat.equal prior.rem C_nat.zero ->
            begin
              match Pmap.find_opt prior.key profile with
              | Some cfg ->
                  let typ = { prior with rem = cfg.full } in
                  if same typ out then Some out else None
              | None -> None
            end
        | _ -> None
      end

let host_check profile env term =
  match host_eval profile env term with
  | Some typ -> same typ (host_type term)
  | None -> false

let emit term =
  let rec walk out = function
    | [] -> List.rev out
    | Emit op :: rest -> walk (op :: out) rest
    | Visit (Harg (name, typ)) :: rest -> walk (Load (name, typ) :: out) rest
    | Visit (Htrim (rem, body, typ)) :: rest ->
        walk out (Visit body :: Emit (Narrow (rem, typ)) :: rest)
    | Visit (Hadd (left, right, typ)) :: rest ->
        walk out
          (Visit left :: Visit right :: Emit (Add_cipher typ) :: rest)
    | Visit (Hmul (left, right, typ)) :: rest ->
        walk out
          (Visit left :: Visit right :: Emit (Mul_cipher typ) :: rest)
    | Visit (Hre (body, typ)) :: rest ->
        walk out (Visit body :: Emit (Recrypt_cipher typ) :: rest)
  in
  walk [] [Visit term]

let step profile env op stack =
  match op, stack with
  | Load (name, out), _ ->
      begin
        match Smap.find_opt (C_syn.name_text name) env with
        | Some typ when same typ out -> Some (out :: stack)
        | _ -> None
      end
  | Narrow (rem, out), prior :: rest when C_nat.le rem prior.rem ->
      begin
        match Pmap.find_opt prior.key profile with
        | Some _ ->
            let typ = { prior with rem } in
            if same typ out then Some (out :: rest) else None
        | None -> None
      end
  | Add_cipher out, right :: left :: rest when same left right ->
      begin
        match Pmap.find_opt left.key profile with
        | Some _ when same left out -> Some (out :: rest)
        | _ -> None
      end
  | Mul_cipher out, right :: left :: rest
      when same left right && not (C_nat.equal left.rem C_nat.zero) ->
      begin
        match Pmap.find_opt left.key profile, C_nat.sub left.rem C_nat.one with
        | Some _, Some rem ->
            let typ = { left with rem } in
            if same typ out then Some (out :: rest) else None
        | _, _ -> None
      end
  | Recrypt_cipher out, prior :: rest when C_nat.equal prior.rem C_nat.zero ->
      begin
        match Pmap.find_opt prior.key profile with
        | Some cfg ->
            let typ = { prior with rem = cfg.full } in
            if same typ out then Some (out :: rest) else None
        | None -> None
      end
  | _, _ -> None

let rec exec profile env code stack =
  match code with
  | [] -> Some stack
  | op :: rest ->
      begin
        match step profile env op stack with
        | Some next -> exec profile env rest next
        | None -> None
      end

let emit_check profile env term =
  match exec profile env (emit term) [] with
  | Some [typ] -> same typ (host_type term)
  | Some _ | None -> false

let host profile env term =
  let* () = shape term in
  let* out = run profile env term in
  Ok out.host

let check profile env term =
  let* () = shape term in
  let* out = run profile env term in
  Ok {
    typ = out.typ;
    steps = out.steps;
    work = out.work;
    peak = out.peak;
    plan = trace_list out.trace;
  }

let enc_text typ =
  "enc[" ^ C_nat.text typ.key ^ "," ^ C_nat.text typ.rem ^ "]"

let param_text value =
  "param[" ^ C_nat.text value.id ^ "," ^ C_nat.text value.pkey
  ^ "," ^ C_nat.text value.cap ^ "]"

let op_text = function
  | Mul key -> "fhe_mul<" ^ C_nat.text key ^ ">"
  | Recrypt key -> "recrypt<" ^ C_nat.text key ^ ">"

let field_text = function
  | Key -> "key"
  | Depth -> "depth"
  | Add_work -> "add work"
  | Mul_work -> "mul work"
  | Re_work -> "recrypt work"
  | Param_id -> "parameter id"
  | Param_cap -> "parameter capacity"

let axis_text = function
  | Steps -> "steps"
  | Work -> "work"

let text error =
  let raw =
    match error with
    | Nat (field, value) ->
        "fhe natural outside profile field = " ^ field_text field
        ^ " value = " ^ Z.to_string value
    | Zero field -> "fhe cost is zero field = " ^ field_text field
    | Pdup key -> "duplicate fhe key profile = " ^ C_nat.text key
    | Pcount (limit, actual) ->
        Printf.sprintf "fhe profile count limit = %d actual = %d" limit actual
    | Cdup_id id -> "duplicate fhe parameter id = " ^ C_nat.text id
    | Cdup_cap (key, cap) ->
        "duplicate fhe parameter capacity key = " ^ C_nat.text key
        ^ " capacity = " ^ C_nat.text cap
    | Ccount (limit, actual) ->
        Printf.sprintf "fhe parameter count limit = %d actual = %d" limit actual
    | No_param (key, need) ->
        "fhe parameter unavailable key = " ^ C_nat.text key
        ^ " required = " ^ C_nat.text need
    | No_key key -> "fhe key profile absent = " ^ C_nat.text key
    | Idup name -> "duplicate fhe input = " ^ name
    | Icount (limit, actual) ->
        Printf.sprintf "fhe input count limit = %d actual = %d" limit actual
    | Idepth (key, rem, full) ->
        "fhe depth exceeds allowance key = " ^ C_nat.text key
        ^ " remaining = " ^ C_nat.text rem ^ " allowance = " ^ C_nat.text full
    | Free name -> "fhe input is free = " ^ name
    | Need (left, right) ->
        "fhe operand type mismatch left = " ^ enc_text left
        ^ " right = " ^ enc_text right
    | Mul_zero key -> "fhe multiplication has no depth key = " ^ C_nat.text key
    | Re_need (key, rem) ->
        "recrypt requires zero remaining depth key = " ^ C_nat.text key
        ^ " remaining = " ^ C_nat.text rem
    | Depth_max (limit, actual) ->
        Printf.sprintf "fhe term depth limit = %d actual = %d" limit actual
    | Nodes (limit, actual) ->
        Printf.sprintf "fhe term nodes limit = %d actual = %d" limit actual
    | Stat (axis, value) ->
        "fhe static " ^ axis_text axis ^ " outside profile = " ^ Z.to_string value
  in
  C_text.clip raw