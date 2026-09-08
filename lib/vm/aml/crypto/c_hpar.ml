(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type set = {
  id : C_nat.t;
  key : C_nat.t;
  fbits : Z.t;
  basis : Z.t;
  rows : Z.t;
  cols : Z.t;
  hwt : Z.t;
  xwt : Z.t;
  ewt : Z.t;
  edges : Z.t;
  ln : Z.t;
  lt : Z.t;
  tn : Z.t;
  td : Z.t;
  prf : Z.t;
  adm : C_hfhe.adm;
  cert : C_hfhe.cert;
}

type seal = Seal of set

module Key = struct
  type t = C_nat.t
  let compare = C_nat.compare
end

module Kmap = Map.Make (Key)

type catalog = {
  ids : unit Kmap.t;
  keys : set Kmap.t;
}

type cap = {
  hbits : Z.t;
  hwords : Z.t;
  perms : Z.t;
  roots : Z.t;
  swords : Z.t;
  ywords : Z.t;
  twords : Z.t;
  ewords : Z.t;
  evals : Z.t;
}

type link = {
  set : set;
  param : C_fhe.param;
  fhe : C_fhe.info;
  hfhe : C_hfhe.info;
  cap : cap;
  graph : C_graph.cfg;
  ent : C_ent.plan;
}

type error =
  | Set
  | Cdup_id of C_nat.t
  | Cdup_key of C_nat.t
  | Ccount of int * int
  | No_set of C_nat.t
  | Fhe of C_fhe.error
  | Hfhe of C_hfhe.error
  | Graph of C_graph.error
  | Ent of C_ent.error
  | Key of C_nat.t * C_nat.t
  | Room of C_nat.t * C_nat.t

let fixed value =
  match C_nat.of_int value with
  | Some value -> value
  | None -> invalid_arg "hfhe parameter fixed natural"

let prod = {
  id = fixed 1;
  key = fixed 1;
  fbits = Z.of_int 127;
  basis = Z.of_int 337;
  rows = Z.of_int 8192;
  cols = Z.of_int 16384;
  hwt = Z.of_int 192;
  xwt = Z.of_int 128;
  ewt = Z.of_int 128;
  edges = Z.of_int 1_200_000;
  ln = Z.of_int 4096;
  lt = Z.of_int 16384;
  tn = Z.one;
  td = Z.of_int 8;
  prf = Z.of_int 4;
  adm = C_hfhe.prod;
  cert = C_hfhe.prod_cert;
}

let pos value = Z.gt value Z.zero

let rec pow acc base exp =
  if Z.equal exp Z.zero then acc
  else
    let acc = if Z.testbit exp 0 then Z.mul acc base else acc in
    pow acc (Z.mul base base) (Z.shift_right exp 1)

let root_ok set =
  pos set.fbits
  && pos set.basis
  && Z.equal
    (Z.rem (Z.sub (pow Z.one (Z.of_int 2) set.fbits) (Z.of_int 2)) set.basis)
    Z.zero

let valid set =
  C_nat.valid set.id
  && not (C_nat.equal set.id C_nat.zero)
  && C_nat.valid set.key
  && not (C_nat.equal set.key C_nat.zero)
  && C_nat.equal set.key set.adm.id
  && Z.equal set.fbits (Z.of_int 127)
  && Z.geq set.basis (Z.of_int 3)
  && Z.leq set.basis (Z.of_int 65_536)
  && Z.equal set.basis (C_nat.to_z C_ent.prod.basis)
  && root_ok set
  && pos set.rows
  && pos set.cols
  && Z.geq set.hwt Z.zero
  && Z.leq set.hwt set.rows
  && Z.geq set.xwt Z.zero
  && Z.leq set.xwt set.cols
  && Z.geq set.ewt Z.zero
  && Z.leq set.ewt set.rows
  && pos set.edges
  && pos set.ln
  && Z.geq set.lt (Z.of_int 127)
  && pos set.tn
  && pos set.td
  && Z.leq set.tn set.td
  && Z.equal set.prf (Z.of_int 4)
  && C_hfhe.adm_ok set.adm
  && C_hfhe.cert_ok set.cert
  && Z.leq set.cert.tags set.edges
  && Z.leq
    (Z.mul (C_nat.to_z set.adm.terms) (C_nat.to_z set.adm.slots))
    set.edges
  && Z.leq
    (Z.mul (C_nat.to_z set.adm.out) (C_nat.to_z set.adm.slots))
    set.cert.tags

let seal set =
  if valid set then Ok (Seal set) else Error Set

let catalog values =
  let rec walk count ids keys = function
    | [] -> Ok { ids; keys }
    | _ when count = C_rule.local.inputs ->
        Error (Ccount (C_rule.local.inputs, C_rule.local.inputs + 1))
    | Seal set :: rest ->
        if Kmap.mem set.id ids then Error (Cdup_id set.id)
        else if Kmap.mem set.key keys then Error (Cdup_key set.key)
        else
          walk (count + 1) (Kmap.add set.id () ids)
            (Kmap.add set.key set keys) rest
  in
  walk 0 Kmap.empty Kmap.empty values

let prod_cat =
  match seal prod with
  | Error _ -> invalid_arg "hfhe production set"
  | Ok sealed ->
      match catalog [sealed] with
      | Ok value -> value
      | Error _ -> invalid_arg "hfhe production catalog"

let select catalog key =
  match Kmap.find_opt key catalog.keys with
  | Some set -> Ok set
  | None -> Error (No_set key)

let div_up value unit =
  Z.div (Z.add value (Z.pred unit)) unit

let words bits = div_up bits (Z.of_int 64)

let caps set = {
  hbits = Z.mul set.rows set.cols;
  hwords = Z.mul set.cols (words set.rows);
  perms = Z.mul set.rows (Z.of_int 2);
  roots = set.basis;
  swords = words set.ln;
  ywords = words set.lt;
  twords = words (Z.add set.lt (Z.of_int 127));
  ewords = Z.mul set.edges (words set.rows);
  evals = Z.mul set.edges (C_nat.to_z set.adm.slots);
}

let graph_cfg set hfhe =
  match C_nat.make set.basis, C_nat.make set.rows with
  | Some basis, Some sigma ->
      begin
        match C_graph.cfg ~basis ~slots:hfhe.C_hfhe.shape.slots ~sigma
            ~edge_cap:set.edges with
        | Ok value -> Ok value
        | Error error -> Error (Graph error)
      end
  | _, _ -> Error (Graph C_graph.Cfg)

let finish set catalog hinputs term fhe =
  match C_fhe.params catalog fhe with
  | Error error -> Error (Fhe error)
  | Ok param ->
      if not (C_nat.equal fhe.typ.key set.key) then
        Error (Key (fhe.typ.key, set.key))
      else if not (C_nat.equal param.pkey set.key) then
        Error (Key (param.pkey, set.key))
      else if not (C_nat.le fhe.peak param.cap) then
        Error (Room (fhe.peak, param.cap))
      else
        begin match C_ent.plan C_ent.prod fhe.peak with
        | Error error -> Error (Ent error)
        | Ok ent ->
          match C_hfhe.env set.adm hinputs with
          | Error error -> Error (Hfhe error)
          | Ok henv ->
              begin
                match C_hfhe.check set.adm set.cert henv term with
                | Error error -> Error (Hfhe error)
                | Ok hfhe ->
                    begin
                      match graph_cfg set hfhe with
                      | Error error -> Error error
                      | Ok graph ->
                          Ok { set; param; fhe; hfhe;
                            cap = caps set; graph; ent }
                    end
              end
        end

let check set catalog profile fenv hinputs term =
  if not (valid set) then Error Set
  else
    match C_fhe.check profile fenv term with
    | Error error -> Error (Fhe error)
    | Ok fhe -> finish set catalog hinputs term fhe

let check_cat sets catalog profile fenv hinputs term =
  match C_fhe.check profile fenv term with
  | Error error -> Error (Fhe error)
  | Ok fhe ->
      begin
        match select sets fhe.typ.key with
        | Error error -> Error error
        | Ok set -> finish set catalog hinputs term fhe
      end

let text error =
  let raw =
    match error with
    | Set -> "hfhe parameter set is invalid"
    | Cdup_id id -> "hfhe set id is repeated id = " ^ C_nat.text id
    | Cdup_key key -> "hfhe set key is repeated key = " ^ C_nat.text key
    | Ccount (limit, actual) ->
        "hfhe set count exceeds profile limit = " ^ string_of_int limit
        ^ " actual = " ^ string_of_int actual
    | No_set key -> "hfhe set is absent key = " ^ C_nat.text key
    | Fhe error -> C_fhe.text error
    | Hfhe error -> C_hfhe.text error
    | Graph error -> C_graph.text error
    | Ent error -> C_ent.text error
    | Key (actual, expected) ->
        "hfhe parameter key mismatch actual = " ^ C_nat.text actual
        ^ " expected = " ^ C_nat.text expected
    | Room (need, cap) ->
        "hfhe parameter room is insufficient required = " ^ C_nat.text need
        ^ " capacity = " ^ C_nat.text cap
  in
  C_text.clip raw