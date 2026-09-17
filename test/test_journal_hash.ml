(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Journal = Octra_node_runtime.Consensus_finality_journal
module Cache = Octra_node_runtime.Consensus_bundle_cache
module Types = Octra_consensus.C_types
module Codec = Octra_consensus.C_codec
module Hash = Octra_consensus.C_hash
module Tx = Octra_core.Transaction

let expect name value = if not value then failwith name

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
    let size = in_channel_length channel in
    if size > 16 * 1024 * 1024 then failwith "record size";
    really_input_string channel size)

let write_file path raw =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
    output_string channel raw)

let check_cache bundle =
  let cache = Cache.create ~cap:1 in
  ignore (Cache.store cache ~pid:"journal"
    ~tx_hashes:bundle.Journal.tx_hashes ~txs:bundle.txs
    ~receipts_json:bundle.receipts_json);
  match Cache.cached cache "journal" with
  | Cache.Cached decoded ->
    expect "recovered transaction hashes"
      (decoded.tx_hashes = List.map Tx.hash bundle.txs)
  | Cache.Missing -> failwith "recovered bundle missing"
  | Cache.Decode_error reason -> failwith reason

let check_record base chain validator_set =
  match Journal.read_validated ~chain_id:chain ~validator_set base with
  | Journal.Valid { bundle = Some bundle; _ } -> check_cache bundle
  | Journal.Invalid reason -> failwith reason
  | _ -> failwith "record unavailable"

let check_hashes () =
  Mirage_crypto_rng_unix.use_default ();
  let keys = List.init 4 (fun i ->
    let key, pub = Mirage_crypto_ec.Ed25519.generate () in
    Types.{ address = "member" ^ string_of_int i;
      pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets pub }, key) in
  let validator_set = Types.make_validator_set (List.map fst keys) in
  let tx = Tx.{ from = "sender"; to_ = "receiver"; amount = Z.one;
    nonce = 1; ou = Z.of_int 1_000; timestamp = 1.; signature = "signature";
    public_key = None; message = None; op_type = Standard; encrypted_data = None } in
  let hash = Tx.hash tx in
  let raw = Digestif.SHA256.(to_raw_string (of_hex hash)) in
  let header = Types.{
    proto_version = proto_version_current; chain_id = "journal-hash"; epoch_id = 12L;
    prev_state_root = String.make 32 'a';
    tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header [hash];
    receipt_root = Hash.receipt_root []; proposed_state_root = String.make 32 'b';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "member0"; txid_hi = 1L; ts = 1.;
  } in
  let proposal_id = Hash.proposal_id header in
  let votes = List.map (fun (validator, key) ->
    let vote = Types.{ chain_id = header.chain_id; epoch_id = 12L; round = 0;
      vote_type = Precommit; proposal_id; validator = validator.address; signature = "" } in
    { vote with signature = Mirage_crypto_ec.Ed25519.sign ~key (Hash.vote_sign_bytes vote) }) keys in
  let finalize = Types.{ chain_id = header.chain_id; epoch_id = 12L; commit_round = 0;
    header; proposal_id; precommits = votes; parent_commit = None } in
  let base = Test_workspace.unique_path "journal_hash" in
  Unix.mkdir base 0o750;
  Journal.persist_certificate base ~validator_set finalize;
  let bundle = Journal.{ tx_hashes = [raw]; txs = [tx]; receipts_json = [] } in
  Journal.persist_bundle base finalize bundle;
  Journal.persist_bundle base finalize { bundle with tx_hashes = [hash] };
  let file = Filename.concat base "finality/pending_finalized.json" in
  let saved = read_file file |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  expect "writer uses hexadecimal hashes"
    (saved |> member "bundle" |> member "tx_hashes" = `List [`String hash]);
  check_record base header.chain_id validator_set;
  let legacy digest =
    match saved with
    | `Assoc fields -> `Assoc (List.map (fun (name, value) ->
      if name <> "bundle" then name, value
      else match value with
        | `Assoc fields -> name, `Assoc (List.map (fun (name, value) ->
          name, if name = "tx_hashes" then `List [`String digest] else value) fields)
        | _ -> failwith "bundle object") fields)
    | _ -> failwith "record object"
  in
  write_file file (Yojson.Safe.to_string (legacy raw));
  check_record base header.chain_id validator_set;
  Journal.persist_bundle base finalize bundle;
  write_file file (Yojson.Safe.to_string (legacy (String.make 32 'x')));
  expect "different digest refused"
    (match Journal.read_validated ~chain_id:header.chain_id ~validator_set base with
     | Journal.Invalid _ -> true | _ -> false)

let check_saved record anchor =
  let json = read_file record |> Yojson.Safe.from_string in
  let trust = read_file anchor |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  let decode json = json |> member "finalize" |> to_string |> Base64.decode_exn |> Codec.decode_finalize in
  let cert = decode json in
  let known = decode trust in
  expect "independent certificate commitment" (Journal.same_block cert known);
  let validator_set = trust |> member "validator_set" |> to_string
    |> Base64.decode_exn |> Codec.decode_validator_set in
  let base = Test_workspace.unique_path "journal_record" in
  Unix.mkdir base 0o750;
  Journal.persist_certificate base ~validator_set cert;
  write_file (Filename.concat base "finality/pending_finalized.json") (read_file record);
  check_record base cert.chain_id validator_set;
  Printf.printf "status = pass epoch = %Ld gate = journal_record\n%!" cert.epoch_id

let () =
  check_hashes ();
  begin match Array.to_list Sys.argv with
  | [_] -> ()
  | [_; record; anchor] -> check_saved record anchor
  | _ -> invalid_arg "expected record and anchor paths"
  end;
  Printf.printf "status = pass test = journal_hash\n%!"