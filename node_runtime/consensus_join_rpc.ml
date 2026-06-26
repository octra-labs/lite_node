(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module Transaction = Octra_core.Transaction
module Runtime_text = Text

type head = {
  epoch : int64;
  root : string;
}

type record = {
  epoch_id : int64;
  prev_state_root : string;
  state_root : string;
  tx_list_hash : string;
  tx_hashes : string list;
  txs_json : string list;
  receipts_json : string list;
  receipt_root : string;
  creator_addr : string;
  commit_round : int;
}

type range =
  | Retry
  | Records of record list

type sync_plan =
  | Fetch_range of int64
  | Ready of {
      ready_epoch : int64;
      state_root : string;
    }
  | Local_ahead of {
      local_head : int64;
      leader_head : int64;
    }
  | Root_mismatch of {
      local_root : string;
      leader_root : string;
      epoch : int64;
    }

type cursor = {
  epoch : int64;
  prev_root : string;
  eic : string;
  txid : int64;
}

type prepared = {
  record : record;
  txs : Transaction.t list;
  expected_eic : string;
  next_cursor : cursor;
  epoch_int : int;
  proposer_info : Octra_core.Epochlog.proposer_info option;
  proposal_id : string;
}

type ready_marker = {
  path : string;
  tmp_path : string;
  payload : Yojson.Safe.t;
  ready_epoch : int64;
  state_root : string;
  records_verified : int;
}

let normalize_base s =
  if String.length s > 0 && s.[String.length s - 1] = '/' then
    String.sub s 0 (String.length s - 1)
  else
    s

let root_hex64 s =
  if String.length s > 64 then String.sub s 0 64 else s

let head_url base =
  base ^ "/state-sync/v1/head"

let range_url base ~from_epoch ~max_epochs =
  let query =
    Uri.encoded_of_query [
      "from_epoch", [Int64.to_string from_epoch];
      "max_epochs", [string_of_int max_epochs];
    ]
  in
  base ^ "/state-sync/v1/range?" ^ query

let int64_field json name =
  let module U = Yojson.Safe.Util in
  match json |> U.member name with
  | `String s -> Int64.of_string s
  | `Int i -> Int64.of_int i
  | `Intlit s -> Int64.of_string s
  | _ -> failwith ("join rpc missing " ^ name)

let string_field json name =
  let module U = Yojson.Safe.Util in
  json |> U.member name |> U.to_string

let string_list_field json name =
  let module U = Yojson.Safe.Util in
  json |> U.member name |> U.to_list |> List.map U.to_string

let optional_string_list_field json name =
  try string_list_field json name with _ -> []

let optional_root_field json name default_value =
  try root_hex64 (string_field json name) with _ -> default_value

let optional_string_field json name default_value =
  try string_field json name with _ -> default_value

let optional_int_field json name default_value =
  let module U = Yojson.Safe.Util in
  try json |> U.member name |> U.to_int with _ -> default_value

let parse_head json =
  {
    epoch = int64_field json "head_epoch";
    root = root_hex64 (string_field json "state_root");
  }

let parse_record json =
  {
    epoch_id = int64_field json "epoch_id";
    prev_state_root = root_hex64 (string_field json "prev_state_root");
    state_root = root_hex64 (string_field json "state_root");
    tx_list_hash = root_hex64 (string_field json "tx_list_hash");
    tx_hashes = string_list_field json "tx_hashes";
    txs_json = string_list_field json "txs_json";
    receipts_json = optional_string_list_field json "receipts_json";
    receipt_root =
      optional_root_field
        json
        "receipt_root"
        (Runtime_text.raw_to_hex (Octra_consensus.C_hash.receipt_root []));
    creator_addr = optional_string_field json "creator_addr" "";
    commit_round = optional_int_field json "commit_round" 0;
  }

let parse_range ~from_epoch json =
  let module U = Yojson.Safe.Util in
  let status = json |> U.member "status" |> U.to_string in
  if status = "not_found" then
    Retry
  else if status <> "ok" then
    failwith (Printf.sprintf "join range status = %s from = %Ld" status from_epoch)
  else
    match json |> U.member "records" |> U.to_list |> List.map parse_record with
    | [] -> Retry
    | records -> Records records

let sync_plan ~local_next ~local_root (head : head) =
  let local_head = Int64.sub local_next 1L in
  if Int64.compare local_next head.epoch > 0 then
    if Int64.compare local_head head.epoch > 0 then
      Local_ahead { local_head; leader_head = head.epoch }
    else if local_root <> head.root then
      Root_mismatch {
        local_root;
        leader_root = head.root;
        epoch = head.epoch;
      }
    else
      Ready { ready_epoch = local_head; state_root = local_root }
  else
    Fetch_range local_next

let parse_tx tx_json =
  match Yojson.Safe.from_string tx_json |> Transaction.of_yojson with
  | Ok tx -> tx
  | Error e -> failwith ("join bad tx_json: " ^ e)

let tx_list_hash tx_hashes =
  Octra_net.Hash_domain.hash "octra:tx_list:v1" (String.concat "" tx_hashes)
  |> Runtime_text.raw_to_hex

let receipt_root receipts_json =
  Octra_consensus.C_hash.receipt_root receipts_json |> Runtime_text.raw_to_hex

let proposer_info creator_addr commit_round =
  if String.length creator_addr > 3 then
    Some { Octra_core.Epochlog.creator_addr; commit_round }
  else
    None

let prepare_record ~cursor record =
  if record.epoch_id <> cursor.epoch then
    failwith
      (Printf.sprintf
         "join epoch break expected = %Ld got = %Ld"
         cursor.epoch
         record.epoch_id);
  if record.prev_state_root <> cursor.prev_root then
    failwith
      (Printf.sprintf
         "join root break epoch = %Ld expected_prev = %s got = %s"
         record.epoch_id
         cursor.prev_root
         record.prev_state_root);
  let txs = List.map parse_tx record.txs_json in
  let parsed_hashes = List.map Transaction.hash txs in
  if parsed_hashes <> record.tx_hashes then
    failwith (Printf.sprintf "join tx hash mismatch epoch = %Ld" record.epoch_id);
  if tx_list_hash record.tx_hashes <> record.tx_list_hash then
    failwith (Printf.sprintf "join tx_list_hash mismatch epoch = %Ld" record.epoch_id);
  if receipt_root record.receipts_json <> record.receipt_root then
    failwith (Printf.sprintf "join receipt_root mismatch epoch = %Ld" record.epoch_id);
  (match Octra_core.Preverify_receipt_policy.check
           ~epoch_id:(Int64.to_int record.epoch_id)
           ~receipts:record.receipts_json
           txs with
   | Stdlib.Ok () -> ()
   | Stdlib.Error e ->
     failwith
       (Printf.sprintf
          "join preverify mismatch epoch = %Ld reason = %s"
          record.epoch_id
          e));
  let _, expected_eic =
    Octra_core.Epoch_index_commitment.next_root_from_hashes
      ~prev:cursor.eic
      ~epoch_id:(Int64.to_int record.epoch_id)
      ~start_txid:cursor.txid
      record.tx_hashes
  in
  let next_cursor = {
    epoch = Int64.add record.epoch_id 1L;
    prev_root = record.state_root;
    eic = expected_eic;
    txid = Int64.add cursor.txid (Int64.of_int (List.length record.tx_hashes));
  } in
  let epoch_int = Int64.to_int record.epoch_id in
  {
    record;
    txs;
    expected_eic;
    next_cursor;
    epoch_int;
    proposer_info = proposer_info record.creator_addr record.commit_round;
    proposal_id =
      Octra_consensus.Finality_log.id_of_parts
        ~height:epoch_int
        ~prev_state_root:record.prev_state_root
        ~tx_list_hash:record.tx_list_hash
        ~state_root:record.state_root;
  }

let finality_entry ~ts prepared =
  let record = prepared.record in
  Octra_consensus.Finality_log.make
    ~height:prepared.epoch_int
    ~round:record.commit_round
    ~proposal_id:prepared.proposal_id
    ~tx_list_hash:record.tx_list_hash
    ~state_root:record.state_root
    ~creator_addr:record.creator_addr
    ~txid_hi:(-1L)
    ~ts
    ()

let ready_marker ~data_dir ~consensus_role ~leader_rpc ~chain_id ~validator
    ~validator_pubkey ~priv_b64 ~ready_epoch ~state_root ~records_verified
    ~generated_at =
  let priv_raw_full = Base64.decode_exn priv_b64 in
  let priv_raw =
    if String.length priv_raw_full >= 32 then String.sub priv_raw_full 0 32
    else priv_raw_full
  in
  let sign_payload =
    Printf.sprintf
      "octra:observer-ready:v1|%s|%s|%Ld|%s|%d"
      chain_id
      validator
      ready_epoch
      state_root
      records_verified
  in
  let signature =
    Octra_consensus.C_hash.sign_ed25519 ~priv_raw ~msg:sign_payload
    |> Base64.encode_exn
  in
  let path = Filename.concat data_dir "ready_to_vote.json" in
  {
    path;
    tmp_path = path ^ ".tmp";
    ready_epoch;
    state_root;
    records_verified;
    payload =
      Octra_bootstrap.State_sync.observer_ready_marker_json
        ~consensus_role
        ~leader_rpc
        ~chain_id
        ~validator
        ~validator_pubkey
        ~ready_epoch
        ~state_root
        ~records_verified
        ~sign_payload
        ~signature
        ~generated_at;
  }