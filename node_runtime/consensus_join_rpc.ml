(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module Runtime_text = Text
module C_types = Octra_consensus.C_types

exception Fetch_retry of string

type apply =
  txs:Transaction.t list ->
  receipts_json:string list ->
  proposer_info:Octra_core.Epochlog.proposer_info option ->
  reward:Consensus_reward_attribution.t ->
  epoch_ts:float ->
  parent_commit:C_types.parent_commit option ->
  unit Lwt.t

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
  epoch_ts : float;
  creator_addr : string;
  commit_round : int;
  reward_source : Octra_consensus.C_types.reward_source;
  finality : Octra_consensus.C_codec.catchup_finality;
}

type range =
  | Retry
  | Missing
  | Records of record list

type outcome =
  | Synced
  | Leader_stale of {
      local_head : int64;
      leader_head : int64;
    }
  | Need_range of {
      head : int64;
      target : int64;
    }

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
  reward : Consensus_reward_attribution.t;
}

type ready_marker = {
  path : string;
  staged_path : string;
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
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  current_epoch : unit -> int;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root : int -> string -> unit;
  stage_finality : prepared -> unit;
  promote_finality : unit -> unit;
  apply : apply;
  root : unit -> string;
  eic : unit -> string option;
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
  log_retry : phase:string -> delay:float -> error:string -> unit;
}

type node_deps = {
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  local_root : unit -> string;
  base_eic_root : unit -> string;
  next_txid : unit -> int64;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root : int -> string -> unit;
  stage_finality : prepared -> unit;
  promote_finality : unit -> unit;
  apply : apply;
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
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root_raw : int -> string -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply : apply;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  require_sync : Sync_need.t -> unit;
}

type node_runtime_wiring = {
  env : string -> string option;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply : apply;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  require_sync : Sync_need.t -> unit;
}

let configured_base = function
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let first_source = function
  | None -> None
  | Some raw ->
    raw
    |> String.split_on_char ','
    |> List.find_map (fun source -> configured_base (Some source))

let configured_join env =
  match configured_base (env "OCTRA_JOIN_RPC") with
  | Some _ as base -> base
  | None -> first_source (env "OCTRA_STATE_SYNC_SOURCES")

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
  base ^ "/state-sync/head"

let range_url base ~from_epoch ~max_epochs =
  let query =
    Uri.encoded_of_query [
      "from_epoch", [Int64.to_string from_epoch];
      "max_epochs", [string_of_int max_epochs];
    ]
  in
  base ^ "/state-sync/range?" ^ query

let http_get_json ?(timeout = 20.0) url =
  Lwt_unix.with_timeout timeout (fun () ->
    let open Lwt.Syntax in
    let* resp, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    let code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let* body_s = Cohttp_lwt.Body.to_string body in
    if code = 429 || code >= 500 then
      Lwt.fail
        (Fetch_retry
           (Printf.sprintf "GET %s failed HTTP %d: %s" url code body_s))
    else if code < 200 || code >= 300 then
      Lwt.fail_with (Printf.sprintf "GET %s failed HTTP %d: %s" url code body_s)
    else
      Lwt.return (Yojson.Safe.from_string body_s))

let retry_delay failures =
  let shift = min 4 (max 0 failures) in
  min 15.0 (float_of_int (1 lsl shift))

let fetch call =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* json = call () in
      Lwt.return (Ok json))
    (fun exn ->
      match exn with
      | Fetch_retry error -> Lwt.return (Error error)
      | Lwt_unix.Timeout
      | Unix.Unix_error _ ->
        Lwt.return (Error (Printexc.to_string exn))
      | _ -> Lwt.fail exn)

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

let epoch_ts_field json =
  let module U = Yojson.Safe.Util in
  let epoch_ts = json |> U.member "epoch_ts" |> U.to_number in
  match Octra_consensus.Epoch_time.of_seconds epoch_ts with
  | Ok _ -> epoch_ts
  | Error error -> failwith ("join epoch timestamp: " ^ error)

let reward_source_field json =
  let module U = Yojson.Safe.Util in
  match
    Octra_consensus.C_reward_source.of_yojson
      (json |> U.member "reward_source")
  with
  | Ok source -> source
  | Error error -> failwith ("join reward source: " ^ error)

let finality_field json =
  let module U = Yojson.Safe.Util in
  let value = json |> U.member "finality" in
  let finalize =
    value
    |> U.member "finalize"
    |> U.to_string
    |> Base64.decode_exn
    |> Octra_consensus.C_codec.decode_finalize
  in
  let validator_set =
    value
    |> U.member "validator_set"
    |> U.to_string
    |> Base64.decode_exn
    |> Octra_consensus.C_codec.decode_validator_set
  in
  Octra_consensus.C_codec.{ finalize; validator_set }

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
    epoch_ts = epoch_ts_field json;
    creator_addr = optional_string_field json "creator_addr" "";
    commit_round = optional_int_field json "commit_round" 0;
    reward_source = reward_source_field json;
    finality = finality_field json;
  }

let parse_range ~from_epoch json =
  let module U = Yojson.Safe.Util in
  let status = json |> U.member "status" |> U.to_string in
  if status = "not_found" then
    Missing
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
  match Yojson.Safe.from_string tx_json |> Octra_core.Tx_payload.decode with
  | Ok tx -> tx
  | Error e -> failwith ("join bad tx_json: " ^ e)

let tx_list_hash tx_hashes =
  Octra_net.Hash_domain.hash "octra:tx_list:v1" (String.concat "" tx_hashes)
  |> Runtime_text.raw_to_hex

let receipt_root receipts_json =
  Octra_consensus.C_hash.receipt_root receipts_json |> Runtime_text.raw_to_hex

let raw_hash name value =
  match Octra_net.Oce1.hash32_bytes value with
  | Ok raw -> raw
  | Error reason ->
    failwith (Printf.sprintf "join %s: %s" name reason)

let canonical_record (record : record) =
  Octra_consensus.C_codec.{
    epoch_id = record.epoch_id;
    prev_state_root = raw_hash "prev_state_root" record.prev_state_root;
    state_root = raw_hash "state_root" record.state_root;
    tx_list_hash = raw_hash "tx_list_hash" record.tx_list_hash;
    tx_hashes = record.tx_hashes;
    txs_json = record.txs_json;
    receipt_root = raw_hash "receipt_root" record.receipt_root;
    receipts_json = record.receipts_json;
    epoch_ts = record.epoch_ts;
    creator_addr = record.creator_addr;
    commit_round = record.commit_round;
    reward_source = Some record.reward_source;
    finality = Some record.finality;
  }

let range_response ~source ~from_epoch ~max_epochs = function
  | Records records when max_epochs > 0 && List.length records <= max_epochs ->
    let records = List.map canonical_record records in
    let next_epoch =
      match List.rev records with
      | last :: _ -> Some (Int64.succ last.Octra_consensus.C_codec.epoch_id)
      | [] -> None
    in
    Some Octra_consensus.C_driver.{
      responder_addr = source;
      request_id = "http:" ^ Int64.to_string from_epoch;
      status = "ok";
      records;
      next_epoch;
    }
  | Retry
  | Missing
  | Records _ -> None

let http_range ?(fetch_json = fun url -> http_get_json url) env ~from_epoch
    ~max_epochs =
  match configured_join env with
  | None -> Lwt.return_none
  | Some source ->
    let open Lwt.Syntax in
    Lwt.catch
      (fun () ->
        let source = normalize_base source in
        let* json = fetch_json (range_url source ~from_epoch ~max_epochs) in
        Lwt.return
          (range_response
             ~source
             ~from_epoch
             ~max_epochs
             (parse_range ~from_epoch json)))
      (fun exn ->
        Octra_log.warn "catchup"
          "event = range_http_unavailable from = %Ld error = %s"
          from_epoch
          (Printexc.to_string exn);
        Lwt.return_none)

let http_head ?(fetch_json = fun url -> http_get_json url) env =
  match configured_join env with
  | None -> Lwt.return_none
  | Some source ->
    let open Lwt.Syntax in
    Lwt.catch
      (fun () ->
        let source = normalize_base source in
        let* json = fetch_json (head_url source) in
        let epoch = (parse_head json).epoch in
        if Int64.compare epoch 0L < 0
           || Int64.compare epoch (Int64.of_int max_int) > 0 then
          Lwt.return_none
        else
          Lwt.return_some epoch)
      (fun exn ->
        Octra_log.warn "catchup"
          "event = head_http_unavailable error = %s"
          (Printexc.to_string exn);
        Lwt.return_none)

let prepare_record ~chain_id ~expected_validator_set_hash ~cursor record =
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
  let reward =
    match Consensus_reward_attribution.of_source record.reward_source with
    | Error error -> failwith ("join reward source: " ^ error)
    | Ok reward ->
      match
        Consensus_reward_attribution.bind_finality
          ~validator_set:record.finality.validator_set
          record.finality.finalize
          reward
      with
      | Ok bound -> bound
      | Error error -> failwith ("join reward binding: " ^ error)
  in
  let partition =
    match Octra_core.Tx_outcome.decode ~confirmed:txs record.receipts_json with
    | Stdlib.Ok value -> value
    | Stdlib.Error e ->
      failwith
        (Printf.sprintf
           "join outcome mismatch epoch = %Ld reason = %s"
           record.epoch_id
           e)
  in
  (match Octra_core.Preverify_receipt_policy.check
           ~epoch_id:(Int64.to_int record.epoch_id)
           ~receipts:partition.preverify
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
  begin
    match
      Octra_consensus.C_catchup.verify_record_finality
        ~chain_id
        ~expected_validator_set_hash
        ~expected_txid:next_cursor.txid
        ~record:(canonical_record record)
    with
    | Error error -> failwith ("join " ^ error)
    | Ok _ -> ()
  end;
  let proposer_info =
    match
      Consensus_epoch_apply_proposer.proposer_from_finalized
        record.finality.finalize
    with
    | Some proposer -> Some proposer
    | None -> failwith "join finality proposer is missing"
  in
  let epoch_int = Int64.to_int record.epoch_id in
  {
    record;
    txs;
    expected_eic;
    next_cursor;
    epoch_int;
    proposer_info;
    reward;
  }

let finality_entry ~chain_id prepared =
  let finalize = prepared.record.finality.finalize in
  if finalize.Octra_consensus.C_types.chain_id <> chain_id then
    failwith "join finality chain mismatch";
  Octra_consensus.Finality_log.of_finalize finalize

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
  deps.stage_finality prepared;
  let* () =
    deps.apply
      ~txs:prepared.txs
      ~receipts_json:record.receipts_json
      ~proposer_info:prepared.proposer_info
      ~reward:prepared.reward
      ~epoch_ts:record.epoch_ts
      ~parent_commit:record.finality.finalize.parent_commit
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
  deps.promote_finality ();
  Lwt.return_unit

let apply_records (deps : apply_deps) ~cursor records =
  let open Lwt.Syntax in
  Lwt_list.fold_left_s
    (fun (cursor, count) record ->
      let expected_validator_set_hash =
        match deps.expected_validator_set_hash record.epoch_id with
        | Ok hash -> hash
        | Error error -> failwith error
      in
      let prepared =
        prepare_record
          ~chain_id:deps.chain_id
          ~expected_validator_set_hash
          ~cursor
          record
      in
      let* () = apply_prepared deps prepared in
      Lwt.return (prepared.next_cursor, count + 1))
    (cursor, 0)
    records

let run_catchup (deps : run_deps) base =
  let open Lwt.Syntax in
  let base = normalize_base base in
  deps.log_start ~base;
  let rec retry ~phase ~records_verified ~missing ~failures error =
    let delay = retry_delay failures in
    deps.log_retry ~phase ~delay ~error;
    let* () = deps.sleep delay in
    loop ~records_verified ~missing ~failures:(failures + 1)
  and loop ~records_verified ~missing ~failures =
    let local_next = deps.local_next () in
    let* fetched_head = fetch (fun () -> deps.fetch_head base) in
    match fetched_head with
    | Error error ->
      retry ~phase:"head" ~records_verified ~missing ~failures error
    | Ok head_json ->
      let head = parse_head head_json in
      match sync_plan ~local_next ~local_root:(deps.local_root ()) head with
      | Local_ahead p ->
        Lwt.return
          (Leader_stale {
             local_head = p.local_head;
             leader_head = p.leader_head;
           })
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
        Lwt.return Synced
      | Fetch_range from_epoch ->
        let* fetched_range =
          fetch (fun () -> deps.fetch_range base ~from_epoch ~max_epochs:16)
        in
        begin
          match fetched_range with
          | Error error ->
            retry ~phase:"range" ~records_verified ~missing ~failures error
          | Ok range_json ->
            match parse_range ~from_epoch range_json with
            | Retry ->
              let* () = deps.sleep 1.0 in
              loop ~records_verified ~missing ~failures:0
            | Missing ->
              if missing + 1 >= 3 then
                Lwt.return
                  (Need_range {
                     head = Int64.pred from_epoch;
                     target = head.epoch;
                   })
              else
                let* () = deps.sleep 1.0 in
                loop
                  ~records_verified
                  ~missing:(missing + 1)
                  ~failures:0
            | Records records ->
              let* _, applied =
                deps.apply_range ~cursor:(deps.cursor ~from_epoch) records
              in
              deps.log_applied ~applied;
              loop
                ~records_verified:(records_verified + applied)
                ~missing:0
                ~failures:0
        end
  in
  loop ~records_verified:0 ~missing:0 ~failures:0

let node_cursor (deps : node_deps) ~from_epoch =
  {
    epoch = from_epoch;
    prev_root = deps.local_root ();
    eic = deps.base_eic_root ();
    txid = deps.next_txid ();
  }

let node_apply_deps (deps : node_deps) =
  ({
    chain_id = deps.chain_id;
    expected_validator_set_hash = deps.expected_validator_set_hash;
    current_epoch = deps.current_epoch;
    put_proposer = deps.put_proposer;
    put_root = deps.put_root;
    stage_finality = deps.stage_finality;
    promote_finality = deps.promote_finality;
    apply = deps.apply;
    root = deps.local_root;
    eic = deps.local_eic;
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
    log_retry = (fun ~phase ~delay ~error ->
      Octra_log.warn "join"
        "RPC catchup retry phase = %s delay_sec = %.0f error = %s"
        phase
        delay
        error);
  } in
  let* outcome = run_catchup run_deps base in
  begin
    match outcome with
    | Synced ->
        Octra_log.info "join"
          "RPC catchup complete local_next = %d"
          (deps.current_epoch ())
    | Leader_stale p ->
        Octra_log.warn "join"
          "event = join_source_stale local_head = %Ld source_head = %Ld action = continue_unattested"
          p.local_head
          p.leader_head
    | Need_range _ -> ()
  end;
  Lwt.return outcome

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
    staged_path = path ^ ".staged";
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
  deps.write_text ~path:marker.staged_path ~contents:(ready_marker_payload_text marker);
  deps.rename ~src:marker.staged_path ~dst:marker.path;
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
    chain_id = deps.chain_id;
    expected_validator_set_hash = deps.expected_validator_set_hash;
    fetch_json = deps.fetch_json;
    current_epoch = deps.current_epoch;
    local_root = (fun () -> local_root_from_head (deps.head ()));
    base_eic_root = (fun () -> base_eic_root_from_head (deps.head ()));
    next_txid = deps.next_txid;
    put_proposer = deps.put_proposer;
    put_root = (fun epoch root ->
      deps.put_root_raw epoch (Runtime_text.hex_to_raw32_lossy root));
    stage_finality = (fun prepared ->
      let finality = prepared.record.finality in
      let entry = finality_entry ~chain_id:deps.chain_id prepared in
      Octra_consensus.Finality_log.check_write deps.data_dir entry;
      Consensus_finality_journal.persist_certificate
        deps.data_dir
        ~validator_set:finality.validator_set
        finality.finalize;
      Consensus_finality_journal.persist_bundle
        deps.data_dir
        finality.finalize
        Consensus_finality_journal.{
          tx_hashes = prepared.record.tx_hashes;
          txs = prepared.txs;
          receipts_json = prepared.record.receipts_json;
        };
      deps.write_entry entry);
    promote_finality = (fun () ->
      Consensus_finality_journal.promote deps.data_dir);
    apply = deps.apply;
    local_eic = (fun () -> local_eic_from_head (deps.head ()));
    write_ready = write_ready_marker ready_marker_config;
    sleep = deps.sleep;
    now = deps.now;
  }

let node_runtime_deps (deps : node_runtime_wiring) =
  {
    env = deps.env;
    expected_validator_set_hash = deps.expected_validator_set_hash;
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
    require_sync = deps.require_sync;
  }

let range_need ~head ~target =
  if Int64.compare head 0L < 0
     || Int64.compare head (Int64.of_int max_int) > 0 then
    None
  else
    Sync_need.lost ~head:(Int64.to_int head) ~target

let run_configured_node_catchup (deps : node_runtime_deps) =
  match configured_join deps.env with
  | None -> Lwt.return_unit
  | Some base ->
    let open Lwt.Syntax in
    let* outcome = run_node_catchup (node_deps_of_runtime deps) base in
    match outcome with
    | Synced -> Lwt.return_unit
    | Leader_stale _ -> Lwt.return_unit
    | Need_range gap ->
        begin
          match range_need ~head:gap.head ~target:gap.target with
          | Some need ->
              deps.require_sync need;
              Lwt.fail_with "join range recovery returned"
          | None ->
              Lwt.fail_with "join range recovery boundary is invalid"
        end

let run_configured_node_wiring deps =
  run_configured_node_catchup (node_runtime_deps deps)