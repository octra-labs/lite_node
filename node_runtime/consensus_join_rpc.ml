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

type ready_marker_config = {
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  generated_at : unit -> float;
}

type ready_marker_write_deps = {
  write_text : path:string -> contents:string -> unit;
  rename : src:string -> dst:string -> unit;
  log_written : ready_marker -> unit;
}

type apply_deps = {
  current_epoch : unit -> int;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root : int -> string -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply :
    txs:Transaction.t list ->
    receipts_json:string list ->
    proposer_info:Octra_core.Epochlog.proposer_info option ->
    unit Lwt.t;
  root : unit -> string;
  eic : unit -> string option;
  now : unit -> float;
}

type run_deps = {
  fetch_head : string -> Yojson.Safe.t Lwt.t;
  fetch_range :
    string ->
    from_epoch:int64 ->
    max_epochs:int ->
    Yojson.Safe.t Lwt.t;
  local_next : unit -> int64;
  local_root : unit -> string;
  cursor : from_epoch:int64 -> cursor;
  apply_range : cursor:cursor -> record list -> (cursor * int) Lwt.t;
  write_ready :
    base:string ->
    ready_epoch:int64 ->
    state_root:string ->
    records_verified:int ->
    unit;
  sleep : float -> unit Lwt.t;
  log_start : base:string -> unit;
  log_applied : applied:int -> unit;
}

type node_deps = {
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  local_root : unit -> string;
  base_eic_root : unit -> string;
  next_txid : unit -> int64;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root : int -> string -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply :
    txs:Transaction.t list ->
    receipts_json:string list ->
    proposer_info:Octra_core.Epochlog.proposer_info option ->
    unit Lwt.t;
  local_eic : unit -> string option;
  write_ready :
    base:string ->
    ready_epoch:int64 ->
    state_root:string ->
    records_verified:int ->
    unit;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
}

type node_runtime_deps = {
  env : string -> string option;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root_raw : int -> string -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply :
    txs:Transaction.t list ->
    receipts_json:string list ->
    proposer_info:Octra_core.Epochlog.proposer_info option ->
    unit Lwt.t;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
}

type node_runtime_wiring = {
  env : string -> string option;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply :
    txs:Transaction.t list ->
    receipts_json:string list ->
    proposer_info:Octra_core.Epochlog.proposer_info option ->
    unit Lwt.t;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
}

let configured_base = function
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let normalize_base s =
  if String.length s > 0 && s.[String.length s - 1] = '/' then
    String.sub s 0 (String.length s - 1)
  else
    s

let root_hex64 s =
  if String.length s > 64 then String.sub s 0 64 else s

let local_root_from_head = function
  | Some h -> root_hex64 h.Octra_core.Head_manifest.state_root
  | None -> ""

let base_eic_root_from_head = function
  | Some h ->
    (match h.Octra_core.Head_manifest.ledger_state_root,
           h.Octra_core.Head_manifest.epoch_index_root with
     | Some _, Some root -> root
     | _ -> Octra_core.Epoch_index_commitment.genesis_root)
  | None -> Octra_core.Epoch_index_commitment.genesis_root

let local_eic_from_head = function
  | Some h -> h.Octra_core.Head_manifest.epoch_index_root
  | None -> None

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

let http_get_json url =
  let open Lwt.Syntax in
  let* resp, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
  let code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  let* body_s = Cohttp_lwt.Body.to_string body in
  if code < 200 || code >= 300 then
    Lwt.fail_with (Printf.sprintf "GET %s failed HTTP %d: %s" url code body_s)
  else
    Lwt.return (Yojson.Safe.from_string body_s)

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

let apply_prepared (deps : apply_deps) prepared =
  let open Lwt.Syntax in
  let record = prepared.record in
  if prepared.epoch_int <> deps.current_epoch () then
    failwith
      (Printf.sprintf
         "join local epoch mismatch local = %d record = %d"
         (deps.current_epoch ())
         prepared.epoch_int);
  Option.iter (deps.put_proposer prepared.epoch_int) prepared.proposer_info;
  deps.put_root prepared.epoch_int record.state_root;
  deps.write_entry (finality_entry ~ts:(deps.now ()) prepared);
  let* () =
    deps.apply
      ~txs:prepared.txs
      ~receipts_json:record.receipts_json
      ~proposer_info:prepared.proposer_info
  in
  let root = deps.root () in
  if root <> record.state_root then
    failwith
      (Printf.sprintf
         "join post-apply root mismatch epoch = %Ld local = %s leader = %s"
         record.epoch_id
         root
         record.state_root);
  if deps.eic () <> Some prepared.expected_eic then
    failwith
      (Printf.sprintf
         "join post-apply EIC mismatch epoch = %Ld"
         record.epoch_id);
  Lwt.return_unit

let apply_records (deps : apply_deps) ~cursor records =
  let open Lwt.Syntax in
  Lwt_list.fold_left_s
    (fun (cursor, count) record ->
      let prepared = prepare_record ~cursor record in
      let* () = apply_prepared deps prepared in
      Lwt.return (prepared.next_cursor, count + 1))
    (cursor, 0)
    records

let run_catchup (deps : run_deps) base =
  let open Lwt.Syntax in
  let base = normalize_base base in
  deps.log_start ~base;
  let rec loop ~records_verified =
    let local_next = deps.local_next () in
    let* head_json = deps.fetch_head base in
    let head = parse_head head_json in
    match sync_plan ~local_next ~local_root:(deps.local_root ()) head with
    | Local_ahead p ->
      Lwt.fail_with
        (Printf.sprintf
           "join local head ahead of leader local = %Ld leader = %Ld"
           p.local_head
           p.leader_head)
    | Root_mismatch p ->
      Lwt.fail_with
        (Printf.sprintf
           "join ready root mismatch local = %s leader = %s epoch = %Ld"
           p.local_root
           p.leader_root
           p.epoch)
    | Ready p ->
      deps.write_ready
        ~base
        ~ready_epoch:p.ready_epoch
        ~state_root:p.state_root
        ~records_verified;
      Lwt.return_unit
    | Fetch_range from_epoch ->
      let* range_json = deps.fetch_range base ~from_epoch ~max_epochs:16 in
      match parse_range ~from_epoch range_json with
      | Retry ->
        let* () = deps.sleep 1.0 in
        loop ~records_verified
      | Records records ->
        let* _, applied =
          deps.apply_range ~cursor:(deps.cursor ~from_epoch) records in
        deps.log_applied ~applied;
        loop ~records_verified:(records_verified + applied)
  in
  loop ~records_verified:0

let node_cursor (deps : node_deps) ~from_epoch =
  {
    epoch = from_epoch;
    prev_root = deps.local_root ();
    eic = deps.base_eic_root ();
    txid = deps.next_txid ();
  }

let node_apply_deps (deps : node_deps) =
  ({
    current_epoch = deps.current_epoch;
    put_proposer = deps.put_proposer;
    put_root = deps.put_root;
    write_entry = deps.write_entry;
    apply = deps.apply;
    root = deps.local_root;
    eic = deps.local_eic;
    now = deps.now;
  } : apply_deps)

let run_node_catchup (deps : node_deps) base =
  let open Lwt.Syntax in
  let apply_deps = node_apply_deps deps in
  let run_deps = {
    fetch_head = (fun base -> deps.fetch_json (head_url base));
    fetch_range = (fun base ~from_epoch ~max_epochs ->
      deps.fetch_json (range_url base ~from_epoch ~max_epochs));
    local_next = (fun () -> Int64.of_int (deps.current_epoch ()));
    local_root = deps.local_root;
    cursor = node_cursor deps;
    apply_range = (fun ~cursor records ->
      apply_records apply_deps ~cursor records);
    write_ready = deps.write_ready;
    sleep = deps.sleep;
    log_start = (fun ~base ->
      Octra_log.info "join"
        "RPC catchup start leader = %s local_next = %d"
        base
        (deps.current_epoch ()));
    log_applied = (fun ~applied ->
      Octra_log.info "join"
        "applied catchup records = %d next_epoch = %d"
        applied
        (deps.current_epoch ()));
  } in
  let* () = run_catchup run_deps base in
  Octra_log.info "join"
    "RPC catchup complete local_next = %d"
    (deps.current_epoch ());
  Lwt.return_unit

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

let ready_marker_payload_text marker =
  Yojson.Safe.pretty_to_string marker.payload ^ "\n"

let write_ready_marker_with deps marker =
  deps.write_text ~path:marker.tmp_path ~contents:(ready_marker_payload_text marker);
  deps.rename ~src:marker.tmp_path ~dst:marker.path;
  deps.log_written marker

let write_text_file ~path ~contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let log_ready_marker marker =
  Octra_log.info "join"
    "READY_TO_VOTE_MARKER written path = %s epoch = %Ld root = %s records = %d"
    marker.path
    marker.ready_epoch
    marker.state_root
    marker.records_verified

let write_ready_marker
    (config : ready_marker_config)
    ~base
    ~ready_epoch
    ~state_root
    ~records_verified =
  let marker =
    ready_marker
      ~data_dir:config.data_dir
      ~consensus_role:config.consensus_role
      ~leader_rpc:base
      ~chain_id:config.chain_id
      ~validator:config.validator
      ~validator_pubkey:config.validator_pubkey
      ~priv_b64:config.priv_b64
      ~ready_epoch
      ~state_root
      ~records_verified
      ~generated_at:(config.generated_at ())
  in
  write_ready_marker_with
    {
      write_text = write_text_file;
      rename = (fun ~src ~dst -> Unix.rename src dst);
      log_written = log_ready_marker;
    }
    marker

let node_deps_of_runtime (deps : node_runtime_deps) =
  let ready_marker_config = {
    data_dir = deps.data_dir;
    consensus_role = deps.consensus_role;
    chain_id = deps.chain_id;
    validator = deps.validator;
    validator_pubkey = deps.validator_pubkey;
    priv_b64 = deps.priv_b64;
    generated_at = deps.now;
  } in
  {
    fetch_json = deps.fetch_json;
    current_epoch = deps.current_epoch;
    local_root = (fun () -> local_root_from_head (deps.head ()));
    base_eic_root = (fun () -> base_eic_root_from_head (deps.head ()));
    next_txid = deps.next_txid;
    put_proposer = deps.put_proposer;
    put_root = (fun epoch root ->
      deps.put_root_raw epoch (Runtime_text.hex_to_raw32_lossy root));
    write_entry = deps.write_entry;
    apply = deps.apply;
    local_eic = (fun () -> local_eic_from_head (deps.head ()));
    write_ready = write_ready_marker ready_marker_config;
    sleep = deps.sleep;
    now = deps.now;
  }

let node_runtime_deps (deps : node_runtime_wiring) =
  {
    env = deps.env;
    fetch_json = deps.fetch_json;
    current_epoch = deps.current_epoch;
    head = deps.head;
    next_txid = deps.next_txid;
    put_proposer = deps.finality.store_proposer;
    put_root_raw = (fun epoch root ->
      deps.finality.store_expected_root ~epoch ~root);
    write_entry = deps.write_entry;
    apply = deps.apply;
    sleep = deps.sleep;
    now = deps.now;
    data_dir = deps.data_dir;
    consensus_role = deps.consensus_role;
    chain_id = deps.chain_id;
    validator = deps.validator;
    validator_pubkey = deps.validator_pubkey;
    priv_b64 = deps.priv_b64;
  }

let run_configured_node_catchup (deps : node_runtime_deps) =
  match configured_base (deps.env "OCTRA_JOIN_RPC") with
  | None -> Lwt.return_unit
  | Some base ->
    run_node_catchup (node_deps_of_runtime deps) base

let run_configured_node_wiring deps =
  run_configured_node_catchup (node_runtime_deps deps)