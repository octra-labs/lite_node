(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Store_irmin
module C = Store_chaindata
module I = Chaindata_index
module E = Epoch_index_commitment
module H = Claim_history
module T = Transaction
module A = Tx_archive
module K = Pvac_registry
module Hashes = Set.Make (String)

type pin = {
  epoch : int;
  state_root : string;
  index_root : string;
  next_txid : int64;
}

type t = {
  store : S.t;
  before : S.read_snapshot;
  after : S.read_snapshot;
  entries : (H.entry * T.t) list;
}

let ( let* ) = Result.bind
let require valid reason = if valid then Ok () else Error reason

let hash_ok raw =
  String.length raw = 64
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) raw

let snapshot store pin =
  let* () = require
    (pin.epoch >= 0 && pin.next_txid >= 0L
     && hash_ok pin.state_root && hash_ok pin.index_root)
    "history checkpoint invalid" in
  let* view = Lwt_main.run
    (S.capture_read_snapshot_epoch store ~epoch_id:(Int64.of_int pin.epoch)) in
  let root = E.folded_state_root ~ledger_state_root:view.state_root
    ~epoch_index_root:pin.index_root in
  let* () = require (root = pin.state_root) "history checkpoint root differs" in
  Ok view

let record archive epoch index =
  match I.get_txid_loc_raw archive.C.index index with
  | None -> Error "history transaction index missing"
  | Some (seg_id, offset, len) ->
    let* () = require
      (seg_id >= 0 && offset >= Txlog.header_size
       && len >= 8 && len <= C.max_txlog_record_len
       && offset <= max_int - 4 - len)
      "history transaction location invalid" in
    let actual, payload = Txlog.read_record archive.txlog ~seg_id ~offset ~len in
    let* () = require (epoch = actual) "history transaction epoch differs" in
    let hash, json = C.split_payload payload in
    let* decoded = A.decode ~hash ~json in
    let* () = require (A.proof decoded = A.Exact)
      "history hash does not authenticate encrypted payload" in
    let* () = require
      (I.get_tx_loc_raw archive.index hash = Some (seg_id, offset, len, epoch))
      "history transaction lookup differs" in
    let from, to_, extra = C.parse_tx_identity json in
    let addresses = C.dedupe_addrs (from :: to_ :: extra) in
    let* () = require
      (List.for_all (fun address -> I.addr_has_txid_raw archive.index address index) addresses)
      "history address index incomplete" in
    Ok (H.{epoch; index; hash; json}, A.tx decoded)

let read store archive ~before ~after ~max_txs =
  try
    let* () = require (before.epoch < max_int && after.epoch = before.epoch + 1)
      "history checkpoints are not consecutive" in
    let* initial = snapshot store before in
    let* final = snapshot store after in
    let* header = C.get_bound_epoch_header archive after.epoch in
    let* () = require
      (max_txs >= 0 && header.tx_count >= 0 && header.tx_count <= max_txs)
      "history epoch exceeds transaction limit" in
    let* hi = C.epoch_txid_hi header in
    let* () = require
      (header.id = after.epoch && header.start_txid = before.next_txid
       && hi < Int64.max_int && Int64.succ hi = after.next_txid
       && header.prev_state_root = before.state_root
       && header.state_root = after.state_root)
      "history epoch checkpoints differ" in
    let rec collect index left entries seen =
      if left = 0 then Ok (List.rev entries)
      else
        let* entry, tx = record archive after.epoch index in
        let* () = require (not (Hashes.mem entry.hash seen))
          "history transaction hash repeated" in
        collect (Int64.succ index) (left - 1) ((entry, tx) :: entries)
          (Hashes.add entry.hash seen)
    in
    let* entries = collect header.start_txid header.tx_count [] Hashes.empty in
    let items = List.map (fun ((entry : H.entry), _) ->
      E.item ~txid:entry.index ~hash:entry.hash) entries in
    let hash, root = E.next_root ~prev:before.index_root ~epoch_id:after.epoch items in
    let* () = require
      (root = after.index_root
       && C.get_epoch_index_commitment archive after.epoch = (Some hash, Some root))
      "history transaction commitment differs" in
    Ok {store; before = initial; after = final; entries}
  with exn -> Error ("history archive read failed: " ^ Printexc.to_string exn)

let key_hash snapshot address =
  match Lwt_main.run (S.read_snapshot snapshot (S.pvac_hash_path address)) with
  | None | Some "none" -> Ok None
  | Some hash when hash_ok hash -> Ok (Some hash)
  | Some _ -> Error "history key digest invalid"

let key_blob store hash =
  let channel = open_in_bin (S.pvac_blob_path store hash) in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    let length = in_channel_length channel in
    let* () = require (length <= K.max_pubkey_bytes) "history key exceeds size limit" in
    let blob = really_input_string channel length in
    let* () = require (K.full_key_hash blob = hash) "history key blob digest differs" in
    K.validate_shape blob)

let replacement tx =
  match tx.T.encrypted_data with
  | None -> Error "history key switch payload missing"
  | Some raw ->
    match Yojson.Safe.from_string raw with
    | `Assoc fields ->
      let names = List.map fst fields in
      let* () = require
        (List.length names = List.length (List.sort_uniq String.compare names))
        "history key switch field repeated" in
      begin match List.assoc_opt "new_pubkey" fields with
      | Some (`String encoded) ->
        let* blob = K.register_blob_of_b64 encoded in
        let* blob = K.canonicalize_blob blob in
        Ok (K.full_key_hash blob, blob)
      | _ -> Error "history key switch key missing"
      end
    | _ -> Error "history key switch object required"

let entry value index =
  match List.find_opt (fun ((entry : H.entry), _) -> entry.index = index) value.entries with
  | Some found -> Ok found
  | None -> Error "history transaction absent from epoch"

let find value hash =
  match List.find_opt (fun ((entry : H.entry), _) -> entry.hash = hash) value.entries with
  | Some (entry, _) -> Ok entry.index
  | None -> Error "history transaction hash absent from epoch"

let fold value ~init ~f =
  List.fold_left (fun result (entry, tx) ->
    let* acc = result in f acc entry tx) (Ok init) value.entries

let cipher_at store pin address =
  try
    let* view = snapshot store pin in
    let* value = Lwt_main.run (S.account_value
      (S.read_snapshot view) (S.Store.Tree.find_tree view.tree) address) in
    match value with
    | None -> Ok None
    | Some raw ->
      let* data = Account_pack.data raw in
      begin match data with
      | Account_pack.Old account -> Ok account.Ledger_types.encrypted_balance
      | Account_pack.Parts _ -> Error "history account cipher is not reconstructed"
      end
  with exn -> Error ("history account read failed: " ^ Printexc.to_string exn)

let key value ~address ~index ~math =
  try
    let* _ = entry value index in
    let* initial = key_hash value.before address in
    let* final = key_hash value.after address in
    let* current = match initial with
      | None -> Ok None
      | Some hash -> let* blob = key_blob value.store hash in Ok (Some (hash, blob)) in
    let rec scan current selected = function
      | [] ->
        let* () = require (Option.map fst current = final)
          "history key transitions differ from checkpoint" in
        begin match selected with
        | Some (hash, blob) -> Ok H.{hash; blob; math}
        | None -> Error "history transaction key unavailable"
        end
      | ((item : H.entry), tx) :: rest ->
        let selected = if item.index = index then current else selected in
        let* current =
          if tx.T.from = address && tx.op_type = T.KeySwitch then
            Result.map Option.some (replacement tx)
          else Ok current in
        scan current selected rest
    in
    scan current None value.entries
  with exn -> Error ("history key read failed: " ^ Printexc.to_string exn)

let output value index =
  let* _, tx = entry value index in
  let* () = require (tx.T.op_type = T.ClaimOp) "history operation is not a claim" in
  let* receipt = match tx.encrypted_data with
    | None -> Error "history claim payload missing"
    | Some raw -> Crypto.StealthClaimV5.of_json (Yojson.Safe.from_string raw) in
  let path = ["stealth"; string_of_int receipt.output_id] in
  let read view = Option.map Yojson.Safe.from_string (Lwt_main.run (S.read_snapshot view path)) in
  match read value.after with
  | None -> Error "history spent output missing"
  | Some after -> Ok (receipt.output_id, read value.before, after)

let source value index =
  try
    let* _, _, after = output value index in
    let open Yojson.Safe.Util in
    let epoch = after |> member "epoch_id" |> to_int in
    let hash = after |> member "tx_hash" |> to_string in
    let* () = require (epoch >= 0 && hash_ok hash) "history source reference invalid" in
    Ok (epoch, hash)
  with exn -> Error ("history source read failed: " ^ Printexc.to_string exn)

let sent value ~index ~math =
  let* item, tx = entry value index in
  let* key = key value ~address:tx.T.from ~index ~math in
  H.sent item key

let verify ~send ~claim ~send_index ~claim_index ~sender_math ~receiver_math =
  try
    let* send_entry, send_tx = entry send send_index in
    let* claim_entry, claim_tx = entry claim claim_index in
    let* sender_key = key send ~address:send_tx.from ~index:send_index ~math:sender_math in
    let* receiver_key = key claim ~address:claim_tx.from ~index:claim_index ~math:receiver_math in
    let* id, before_claim, output = output claim claim_index in
    let before_send = Lwt_main.run
      (S.read_snapshot send.before ["stealth"; string_of_int id])
      |> Option.map Yojson.Safe.from_string in
    H.verify ~send:send_entry ~claim:claim_entry ~before_send ~before_claim ~output
      ~sender_key ~receiver_key
  with exn -> Error ("history claim read failed: " ^ Printexc.to_string exn)