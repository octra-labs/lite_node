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
module C_types = Octra_consensus.C_types

type plan = {
  header : C_types.epoch_header;
  commit_round : int;
  finalize : C_types.finalize;
  txs : Transaction.t list;
  tx_hashes : string list;
  epoch : int;
  proposer_info : Octra_core.Epochlog.proposer_info option;
  expected_root : string option;
}

let zero32 = String.make 32 '\x00'

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | _ -> -1

let is_hex_string s =
  String.length s > 0
  && String.for_all (fun c -> hex_value c >= 0) s

let raw32_of_hex64 s =
  String.init 32 (fun i ->
    let hi = hex_value s.[i * 2] in
    let lo = hex_value s.[(i * 2) + 1] in
    Char.chr ((hi lsl 4) lor lo))

let int64_field json name =
  let module U = Yojson.Safe.Util in
  match json |> U.member name with
  | `Int i -> Int64.of_int i
  | `Intlit s -> Int64.of_string s
  | `String s -> Int64.of_string s
  | _ -> failwith (Printf.sprintf "replay header missing %s" name)

let int_field json name default_value =
  let module U = Yojson.Safe.Util in
  match json |> U.member name with
  | `Int i -> i
  | `Intlit s -> int_of_string s
  | `String s -> int_of_string s
  | _ -> default_value

let float_field json name default_value =
  let module U = Yojson.Safe.Util in
  match json |> U.member name with
  | `Float f -> f
  | `Int i -> float_of_int i
  | `Intlit s -> float_of_string s
  | `String s -> float_of_string s
  | _ -> default_value

let raw32_field json name =
  let module U = Yojson.Safe.Util in
  match json |> U.member name |> U.to_string_option with
  | Some s when String.length s = 64 && is_hex_string s ->
    raw32_of_hex64 s
  | Some s when String.length s = 32 ->
    s
  | Some _ ->
    failwith (Printf.sprintf "replay header bad %s" name)
  | None ->
    zero32

let receipt_root_field json =
  let module U = Yojson.Safe.Util in
  match json |> U.member "receipt_root" |> U.to_string_option with
  | Some _ -> raw32_field json "receipt_root"
  | None -> Octra_consensus.C_hash.receipt_root []

let parse_header ~default_chain_id json =
  let module U = Yojson.Safe.Util in
  let proto_version =
    match json |> U.member "proto_version" |> U.to_int_option with
    | Some v -> v
    | None -> C_types.proto_version_current
  in
  let chain_id =
    match json |> U.member "chain_id" |> U.to_string_option with
    | Some s when s <> "" -> s
    | _ -> default_chain_id
  in
  let header = C_types.{
    proto_version;
    chain_id;
    epoch_id = int64_field json "epoch_id";
    prev_state_root = raw32_field json "prev_state_root";
    tx_list_hash = raw32_field json "tx_list_hash";
    receipt_root = receipt_root_field json;
    proposed_state_root = raw32_field json "proposed_state_root";
    creator_addr = json |> U.member "creator_addr" |> U.to_string;
    txid_hi = int64_field json "txid_hi";
    ts = float_field json "ts" 0.0;
  } in
  header, int_field json "commit_round" 0

let parse_bundle json =
  let items =
    match json with
    | `List xs -> xs
    | _ -> failwith "replay bundle must be a JSON list"
  in
  List.map
    (fun item ->
       match Transaction.of_yojson item with
       | Ok tx -> tx
       | Error e -> failwith ("replay bundle tx decode: " ^ e))
    items

let proposer_info (header : C_types.epoch_header) commit_round =
  if String.length header.C_types.creator_addr > 3 then
    Some {
      Octra_core.Epochlog.creator_addr = header.creator_addr;
      commit_round;
    }
  else
    None

let expected_root (header : C_types.epoch_header) =
  if String.length header.C_types.proposed_state_root = 32
     && header.proposed_state_root <> zero32 then
    Some header.proposed_state_root
  else
    None

let finalize (header : C_types.epoch_header) commit_round =
  C_types.{
    chain_id = header.chain_id;
    epoch_id = header.epoch_id;
    commit_round;
    header;
    proposal_id = Octra_consensus.C_hash.proposal_id header;
    precommits = [];
  }

let build_plan ~(header : C_types.epoch_header) ~commit_round ~txs =
  {
    header;
    commit_round;
    finalize = finalize header commit_round;
    txs;
    tx_hashes = List.map Transaction.hash txs;
    epoch = Int64.to_int header.C_types.epoch_id;
    proposer_info = proposer_info header commit_round;
    expected_root = expected_root header;
  }

let load_plan ~default_chain_id ~header_path ~bundle_path =
  let header, commit_round =
    parse_header
      ~default_chain_id
      (Yojson.Safe.from_file header_path)
  in
  let txs =
    match bundle_path with
    | None -> []
    | Some path -> parse_bundle (Yojson.Safe.from_file path)
  in
  build_plan ~header ~commit_round ~txs