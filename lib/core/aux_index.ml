(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Delta = Aux_delta
module Keys = Map.Make (struct type t = Delta.key let compare = compare end)

type t = {
  env : Lmdb.Env.t;
  meta : (string, string, [`Uni]) Lmdb.Map.t;
  receipts : (string, string, [`Uni]) Lmdb.Map.t;
  rejected : (string, string, [`Uni]) Lmdb.Map.t;
  addresses : (string, string, [`Dup | `Uni]) Lmdb.Map.t;
  epochs : (int32, string, [`Dup | `Uni]) Lmdb.Map.t;
}

exception Refused of string

let refuse reason = raise (Refused reason)
let pending_key = "aux_delta_pending"
let need = function Ok value -> value | Error reason -> refuse reason
let copy value = Bytes.to_string (Bytes.of_string value)

let get map txn key =
  try Some (copy (Lmdb.Map.get map ~txn key)) with Not_found -> None

let contains map (txn : [> `Read] Lmdb.Txn.t) key value =
  Lmdb.Cursor.go Lmdb.Ro map ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
    try
      ignore (Lmdb.Cursor.seek_dup cursor key value);
      let found_key, found_value = Lmdb.Cursor.current cursor in
      found_key = key && found_value = value
    with Not_found -> false)

let read t txn = function
  | Delta.Receipt key -> Delta.Value (get t.receipts txn key)
  | Delta.Rejected key -> Delta.Value (get t.rejected txn key)
  | Delta.Metadata key -> Delta.Value (get t.meta txn key)
  | Delta.Address (key, value) -> Delta.Member (contains t.addresses txn key value)
  | Delta.Epoch (key, value) -> Delta.Member (contains t.epochs txn (Int32.of_int key) value)

let put map txn key = function
  | None -> (try Lmdb.Map.remove map ~txn key with Not_found -> ())
  | Some value -> Lmdb.Map.set map ~txn key value

let put_pair map txn key value = function
  | false -> (try Lmdb.Map.remove map ~txn ~value key with Not_found -> ())
  | true -> (try Lmdb.Map.add map ~txn key value with Lmdb.Exists -> ())

let write t txn = function
  | Delta.Receipt key, Delta.Value value -> put t.receipts txn key value
  | Delta.Rejected key, Delta.Value value -> put t.rejected txn key value
  | Delta.Metadata key, Delta.Value value -> put t.meta txn key value
  | Delta.Address (key, value), Delta.Member present -> put_pair t.addresses txn key value present
  | Delta.Epoch (key, value), Delta.Member present -> put_pair t.epochs txn (Int32.of_int key) value present
  | _ -> refuse "auxiliary write cell type differs"

let journal t txn =
  Option.map (fun bytes ->
    let value = need (Delta.decode bytes) in
    if List.exists (fun row -> row.Delta.key = Delta.Metadata pending_key) value.rows then
      refuse "auxiliary journal includes its own key";
    if List.exists (fun row -> read t txn row.Delta.key <> row.next) value.rows then
      refuse "auxiliary values differ from journal";
    value) (get t.meta txn pending_key)

let retire t txn ~head =
  match journal t txn with
  | None -> ()
  | Some value ->
    (match need (Delta.decide ~head value) with
    | Delta.Restore -> refuse "auxiliary journal is ahead of durable HEAD"
    | Delta.Retire -> Lmdb.Map.remove t.meta ~txn pending_key)

let with_write t ~previous ~target ~writes action =
  let after = List.fold_left (fun values (key, cell) -> Keys.add key cell values) Keys.empty writes
    |> Keys.bindings in
  if List.mem_assoc (Delta.Metadata pending_key) after then
    invalid_arg "auxiliary write includes its journal key";
  match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
    retire t txn ~head:previous;
    let before = List.map (fun (key, _) -> key, read t txn key) after in
    let record = need (Delta.seal ~previous ~target ~before ~after) in
    let result = action txn in
    List.iter (write t txn) writes;
    Lmdb.Map.set t.meta ~txn pending_key (Delta.encode record);
    result) with
  | Some result -> result
  | None -> refuse "auxiliary write transaction returned no result"

let restoration t txn ~head =
  match journal t txn with
  | None -> []
  | Some value ->
    (match need (Delta.decide ~head value) with
    | Delta.Retire -> []
    | Delta.Restore ->
      let current = List.map (fun row -> row.Delta.key, read t txn row.key) value.rows in
      need (Delta.restore ~head ~current value))

let apply_restore t txn ~head =
  let pending = journal t txn in
  let rows = restoration t txn ~head in
  List.iter (write t txn) rows;
  (match pending with
  | None -> ()
  | Some value -> (match need (Delta.decide ~head value) with
    | Delta.Retire -> ()
    | Delta.Restore -> Lmdb.Map.remove t.meta ~txn pending_key))

let epoch_field table key field bytes =
  let fail reason = refuse (Printf.sprintf
    "auxiliary %s key = %s reason = %s" table key reason) in
  match Yojson.Safe.from_string bytes with
  | `Assoc fields ->
    (match List.filter (fun (name, _) -> name = field) fields with
    | [_, `Int epoch] when epoch >= 0 -> epoch
    | _ -> fail "epoch_field_invalid")
  | _ -> fail "row_encoding_invalid"
  | exception Yojson.Json_error _ -> fail "row_json_invalid"

let each map (txn : [> `Read] Lmdb.Txn.t) action =
  Lmdb.Cursor.go Lmdb.Ro map ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
    let rec loop = function
      | None -> ()
      | Some (key, value) ->
        action key value;
        loop (try Some (Lmdb.Cursor.next cursor) with Not_found -> None) in
    loop (try Some (Lmdb.Cursor.first cursor) with Not_found -> None))

let verify t txn ~head =
  let cap = match head with None -> -1 | Some value -> value.Delta.epoch in
  let rows = match journal t txn with
    | None -> []
    | Some value -> (match need (Delta.decide ~head value) with
      | Delta.Retire -> []
      | Delta.Restore -> restoration t txn ~head) in
  let projected = List.fold_left (fun values (key, cell) -> Keys.add key cell values) Keys.empty rows in
  let cell key current = Option.value (Keys.find_opt key projected) ~default:current in
  let check table key epoch =
    if epoch > cap then refuse (Printf.sprintf
      "auxiliary %s exceeds HEAD key = %s epoch = %d head = %d" table key epoch cap) in
  let check_value = function
    | (Delta.Receipt _ | Delta.Rejected _), Delta.Value None -> ()
    | (Delta.Receipt key as kind), Delta.Value (Some bytes)
    | (Delta.Rejected key as kind), Delta.Value (Some bytes) ->
      let kind = match kind with Delta.Receipt _ -> "receipt" | _ -> "rejected" in
      let field = if kind = "receipt" then "epoch" else "epoch_id" in
      check kind key (epoch_field kind key field bytes)
    | Delta.Epoch (epoch, hash), Delta.Member true ->
      if epoch < 0 then refuse "auxiliary epoch reference is negative";
      check "epoch_reference" hash epoch
    | Delta.Address (address, value), Delta.Member true ->
      if String.length value > 0 && value.[0] = '~' then begin
        if String.length value <= 22 || value.[21] <> ':' then
          refuse "auxiliary address reference is malformed";
        let epoch = try int_of_string (String.sub value 1 20) with Failure _ ->
          refuse "auxiliary address epoch is malformed" in
        if epoch < 0 then refuse "auxiliary address epoch is negative";
        check "address_reference" address epoch
      end
    | (Delta.Address _ | Delta.Epoch _), Delta.Member false
    | Delta.Metadata _, Delta.Value _ -> ()
    | _ -> refuse "auxiliary projected cell type differs" in
  each t.receipts txn (fun key value -> check_value
    (Delta.Receipt key, cell (Delta.Receipt key) (Delta.Value (Some value))));
  each t.rejected txn (fun key value -> check_value
    (Delta.Rejected key, cell (Delta.Rejected key) (Delta.Value (Some value))));
  each t.epochs txn (fun epoch hash ->
    let epoch = Int32.to_int epoch in
    check_value (Delta.Epoch (epoch, hash), cell (Delta.Epoch (epoch, hash)) (Delta.Member true)));
  each t.addresses txn (fun address value ->
    check_value (Delta.Address (address, value), cell (Delta.Address (address, value)) (Delta.Member true)));
  List.iter check_value rows

let inspect t ~head =
  match Lmdb.Txn.go Lmdb.Ro t.env (fun txn -> verify t txn ~head) with
  | Some () -> ()
  | None -> refuse "auxiliary inspection returned no result"