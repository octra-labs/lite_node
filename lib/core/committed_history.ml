(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Eic = Epoch_index_commitment
module Index = Chaindata_index

let ( let* ) = Result.bind

type proof =
  | Legacy of int
  | Eic_chain of { root : string; hash : string; legacy_epochs : int }

type cursor = {
  epochs : Epochlog.epoch_header list;
  next_txid : int64;
  proof : proof;
  items : Eic.item list;
  checked : int;
  repaired : int;
}

let origin floor epochs =
  match floor with
  | Some floor ->
      History_floor.epoch floor, History_floor.next_txid floor,
      Eic_chain { root = History_floor.epoch_index_root floor;
        hash = History_floor.epoch_index_hash floor; legacy_epochs = 0 }
  | None ->
      let first = match epochs with h :: _ -> h.Epochlog.id | [] -> 0 in
      (if first = 1 then 0 else -1), 0L, Legacy 0

let validate_ranges ~cap ~txid_hi ~epoch ~next_txid epochs =
  let rec walk previous next = function
    | [] ->
        if previous <> cap || Int64.pred next <> txid_hi then
          Error "committed epoch ranges differ from HEAD"
        else Ok ()
    | h :: rest ->
        let count = Int64.of_int h.Epochlog.tx_count in
        if previous = max_int || h.id <> previous + 1 || h.tx_count < 0
           || h.start_txid <> next || next < 0L
           || Int64.sub Int64.max_int next < count then
          Error (Printf.sprintf "invalid committed transaction range: epoch = %d" h.id)
        else walk h.id (Int64.add next count) rest
  in
  walk epoch next_txid epochs

let committed_epochs ~max_epoch epochs =
  let rec select previous selected = function
    | [] -> Ok (List.rev selected)
    | h :: rest ->
        if h.Epochlog.id < 0 || h.id <= previous then Error "epoch journal is out of order"
        else select h.id (if h.id <= max_epoch then h :: selected else selected) rest
  in
  select (-1) [] epochs

let initial io head ~max_epoch epochs =
  let* floor = match io.Index.read_meta "history_floor" with
    | None -> Ok None
    | Some bytes -> Result.map Option.some (History_floor.of_string bytes) in
  let* epochs = committed_epochs ~max_epoch epochs in
  let epoch, next_txid, proof = origin floor epochs in
  let* () = match head with
    | None when max_epoch = -1 && epochs = [] && floor = None -> Ok ()
    | None -> Error "committed repair requires HEAD"
    | Some h when h.Head_manifest.epoch_id <> max_epoch -> Error "repair epoch differs from HEAD"
    | Some h when (h.epoch_index_hash = None) <> (h.epoch_index_root = None) ->
        Error "committed repair requires complete HEAD index fields"
    | Some h -> validate_ranges ~cap:max_epoch ~txid_hi:h.txid_hi ~epoch ~next_txid epochs in
  let* () = match head, List.rev epochs, floor with
    | Some h, last :: _, _ when h.state_root <> last.Epochlog.state_root ->
        Error "committed epoch state root differs from HEAD"
    | Some h, [], Some floor when h.state_root <> History_floor.state_root floor ->
        Error "history floor state root differs from HEAD"
    | _ -> Ok () in
  Ok { epochs; next_txid; proof; items = []; checked = 0; repaired = 0 }

let epoch_proof proof ~epoch ~stored_hash ~stored_root items =
  match proof, stored_hash, stored_root with
  | Legacy count, None, None -> Ok (Legacy (count + 1))
  | _, Some stored_hash, Some stored_root ->
      let prev, legacy_epochs = match proof with
        | Legacy count -> Eic.genesis_root, count
        | Eic_chain chain -> chain.root, chain.legacy_epochs in
      let hash, root = Eic.next_root ~prev ~epoch_id:epoch items in
      if hash <> stored_hash || root <> stored_root then
        Error (Printf.sprintf "committed epoch index commitment differs: epoch = %d" epoch)
      else Ok (Eic_chain { root; hash; legacy_epochs })
  | Eic_chain _, None, None ->
      Error (Printf.sprintf "committed epoch index commitment missing: epoch = %d" epoch)
  | _, _, _ ->
      Error (Printf.sprintf "committed epoch index commitment incomplete: epoch = %d" epoch)

let rec advance io state =
  match state.epochs with
  | h :: rest when state.next_txid = Int64.add h.start_txid (Int64.of_int h.tx_count) ->
      let* proof = epoch_proof state.proof ~epoch:h.id
        ~stored_hash:(io.Index.read_meta (Printf.sprintf "eic_epoch_hash:%d" h.id))
        ~stored_root:(io.read_meta (Printf.sprintf "eic_epoch_root:%d" h.id)) state.items in
      advance io { state with epochs = rest; proof; items = [] }
  | _ -> Ok state

let txid_position io txid =
  match io.Index.read_txid txid with
  | Some bytes when String.length bytes = 16 ->
      let segment, offset, length = Index.decode_txid_loc bytes in
      if segment < 0 || offset < Txlog.header_size || length < 8
         || length > Txlog.max_record_len
         || Index.encode_txid_loc ~seg_id:segment ~offset ~len:length <> bytes then
        Error (Printf.sprintf "invalid committed transaction location: txid = %Ld" txid)
      else Ok (segment, offset, length)
  | _ -> Error (Printf.sprintf "missing or malformed transaction location: txid = %Ld" txid)

let record_hash payload =
  if String.length payload < 64 then Error "transaction hash is missing"
  else
    let hash = String.sub payload 0 64 in
    if not (String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) hash) then
      Error "transaction hash is invalid"
    else
      match Json_tree.read (String.sub payload 64 (String.length payload - 64)) with
      | `Assoc _ -> Ok hash
      | _ -> Error "transaction payload is not an object"
      | exception Yojson.Json_error _ -> Error "transaction payload is invalid JSON"

let consume io state (record : Txlog.scan_record) =
  let* state = advance io state in
  match state.epochs with
  | [] -> Ok state
  | h :: _ ->
      let* segment, offset, length = txid_position io state.next_txid in
      let order = compare (record.segment, record.offset) (segment, offset) in
      if order < 0 then Ok state
      else if order > 0 || length <> record.length || h.id <> record.epoch then
        Error (Printf.sprintf "committed transaction frame differs: txid = %Ld" state.next_txid)
      else
        let* hash = record_hash record.payload in
        let location = Index.encode_tx_loc ~seg_id:segment ~offset ~len:length ~epoch_id:h.id in
        let* repaired = match io.read_hash hash with
          | Some existing when existing = location -> Ok state.repaired
          | Some _ -> Error "transaction location differs from committed record"
          | None -> io.write_hash hash location; Ok (state.repaired + 1) in
        Ok { state with next_txid = Int64.succ state.next_txid;
          items = Eic.item ~txid:state.next_txid ~hash :: state.items;
          checked = state.checked + 1; repaired }

let journal_extent txlog epochlog head =
  match head with
  | None -> Ok None
  | Some h ->
      match h.Head_manifest.txlog_seg, h.txlog_off, h.epochlog_off with
      | Some segment, Some offset, Some epoch_offset
        when segment >= 0 && offset >= Txlog.header_size && epoch_offset >= Epochlog.header_size ->
          let epoch_end = match Epochlog.offset_after epochlog h.epoch_id with
            | Some offset -> offset
            | None -> Epochlog.header_size in
          let physical = Txlog.current_position txlog in
          if (segment, offset) > physical || epoch_offset <> epoch_end then
            Error "committed journal offsets differ from HEAD"
          else Ok (Some (segment, offset))
      | _ -> Error "committed repair requires journal offsets"

let verify_head head state =
  match head with
  | None -> Ok ()
  | Some h -> match state.proof with
      | Legacy _ ->
          if h.Head_manifest.epoch_index_hash <> None || h.epoch_index_root <> None then
            Error "HEAD index commitment is absent from history"
          else if Option.fold ~none:false ~some:(fun root -> root <> h.state_root) h.ledger_state_root then
            Error "legacy HEAD ledger root differs from state root"
          else Ok ()
      | Eic_chain chain ->
          if (h.Head_manifest.epoch_index_hash <> None && h.epoch_index_hash <> Some chain.hash)
             || (h.epoch_index_root <> None && h.epoch_index_root <> Some chain.root) then
            Error "committed index chain differs from HEAD"
          else match h.ledger_state_root with
            | Some ledger_state_root when Eic.folded_state_root
                ~ledger_state_root ~epoch_index_root:chain.root = h.state_root -> Ok ()
            | None when h.epoch_index_root <> None -> Ok ()
            | _ -> Error "committed index chain differs from folded HEAD root"

let verify ?(end_at_head = false) io ~head ~max_epoch ~txlog ~epochlog =
  let* extent = journal_extent txlog epochlog head in
  let epoch_end = if end_at_head then Option.bind head (fun h -> h.Head_manifest.epochlog_off) else None in
  let tx_end = if end_at_head then extent else None in
  let* epochs = Result.map_error Epochlog.read_error_message
    (Epochlog.read_all_strict ?end_at:epoch_end epochlog) in
  let* state = initial io head ~max_epoch epochs in
  let committed = state.epochs in
  let at_end = match extent with None -> true | Some (_, offset) -> offset = Txlog.header_size in
  let* state, at_end = Result.map_error Txlog.scan_error_message
    (Txlog.fold_strict ?end_at:tx_end txlog ~init:(state, at_end) ~f:(fun (state, seen) record ->
      let finish = record.Txlog.offset + 4 + record.length in
      let within, seen = match extent with
        | None -> false, seen
        | Some limit ->
            (record.segment, finish) <= limit, seen || (record.segment, finish) = limit in
      if within then Result.map (fun state -> state, seen) (consume io state record)
      else Ok (state, seen))) in
  let* state = advance io state in
  if state.epochs <> [] || not at_end then Error "committed transaction stream is incomplete"
  else if io.hash_count () <> state.checked then Error "transaction hash index contains unproven entries"
  else
    let* () = verify_head head state in
    let legacy_epochs = match state.proof with
      | Legacy count -> count
      | Eic_chain chain -> chain.legacy_epochs in
    Ok (state.checked, state.repaired, committed, legacy_epochs)