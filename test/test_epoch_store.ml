(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module R = Epoch_replay
module A = Epoch_store
module S = Octra_core.Store_irmin
module D = Octra_core.Store_chaindata
module L = Octra_core.Ledger
module C = Octra_consensus.C_types
module N = Octra_node_runtime
module G = Octra_core.Rule_graph
module H = Octra_consensus.C_hash
module F = Octra_core.Set_fold

let proposer = "octEpochStoreSample"
let expect = R.require
let reward = R.X.{
  proposer_addr = proposer;
  proposer_public_key = None;
  validators = [{ address = proposer; public_key = None; weight = Z.one }];
}

let sample ~chain_id ~parent_commit (cursor : R.J.cursor) state_root =
  let epoch_int = Int64.to_int cursor.epoch in
  let _, expected_eic = R.E.next_root_from_hashes_i64 ~prev:cursor.eic
    ~epoch_id:cursor.epoch ~start_txid:cursor.txid [] in
  let tx_list_hash = R.J.root_hex64 (Octra_consensus.C_hash.tx_list_hash []) in
  let receipt_root = R.J.root_hex64 (Octra_consensus.C_hash.receipt_root []) in
  let epoch_ts = float_of_int (epoch_int * 10) in
  let header, _ = N.Consensus_replay.parse_header ~default_chain_id:chain_id
    (`Assoc [
      "epoch_id", `Int epoch_int;
      "txid_hi", `Intlit (Int64.to_string (Int64.pred cursor.txid));
      "creator_addr", `String proposer;
      "prev_state_root", `String cursor.prev_root;
      "proposed_state_root", `String state_root;
      "parent_commit_hash", `String (R.J.root_hex64 (H.parent_commit_hash_opt parent_commit));
      "tx_list_hash", `String tx_list_hash;
      "receipt_root", `String receipt_root;
      "ts", `Float epoch_ts;
    ]) in
  let plan = N.Consensus_replay.build_plan
    ~parent_commit ~header ~commit_round:0 ~txs:[] in
  let validator_set = match parent_commit with
    | Some commit -> commit.C.validator_set
    | None -> C.make_validator_set []
  in
  let record = R.J.{
    epoch_id = cursor.epoch;
    prev_state_root = cursor.prev_root;
    state_root;
    tx_list_hash;
    tx_hashes = [];
    txs_json = [];
    receipts_json = [];
    receipt_root;
    epoch_ts;
    creator_addr = proposer;
    commit_round = 0;
    reward_source = R.get (N.Consensus_reward_attribution.to_source reward);
    finality = { finalize = plan.finalize; validator_set };
  } in
  R.J.{ record; txs = []; expected_eic; epoch_int;
    proposer_info = plan.proposer_info; reward;
    next_cursor = { epoch = Int64.succ cursor.epoch; prev_root = state_root;
      eic = expected_eic; txid = cursor.txid } }

let epoch (context : A.context) parent (cursor : R.J.cursor) =
  let parent_commit = parent cursor in
  let prepare = sample ~chain_id:context.chain_id ~parent_commit cursor in
  let provisional = prepare cursor.prev_root in
  let at = Int64.to_int cursor.epoch in
  let fold = R.get (N.Set_rule.bind context.rules ~chain_id:context.chain_id
    ~parent:parent_commit ~epoch:at) in
  let policy = R.get (fold at) in
  expect "replay graph phase differs"
    (policy.standard_mode = G.standard_at ~chain_id:context.chain_id ~epoch:at);
  let* duty = S.get_meta context.store F.meta_key in
  let reject label context prepared =
    match A.deps context ~cursor ~prepared with
    | exception Failure reason ->
      expect "replay refusal differs" (String.starts_with ~prefix:label reason)
    | _ -> failwith ("replay accepted " ^ label)
  in
  begin match parent_commit with
  | None -> ()
  | Some commit ->
    List.iter (fun (label, root) ->
      let rules = G.create ~chain_id:context.chain_id ~root_at:(fun _ -> root) in
      reject label { context with rules } provisional
    ) ["rule anchor missing", G.Missing;
       "rule anchor mismatch", G.Root (String.make 64 '0')];
    let first = List.hd commit.certificate.precommits in
    let invalid = { commit with certificate = { commit.certificate with
      precommits = { first with signature = String.make 64 '\000' }
        :: List.tl commit.certificate.precommits } } in
    let prepared = sample ~chain_id:context.chain_id
      ~parent_commit:(Some invalid) cursor cursor.prev_root in
    reject "validator duty commit is invalid" context prepared
  end;
  let deps = A.deps context ~cursor ~prepared:provisional in
  let* batch = deps.preverify [] in
  expect "empty preverify produced entries" (batch.ready = [] && batch.skipped = []);
  let gate = R.G.create (R.W.receipts_for_hashes batch.ready []) in
  let* preview = deps.preview gate [] in
  let preview = R.get preview in
  let state_root = R.E.folded_state_root ~ledger_state_root:preview.post_state_root
    ~epoch_index_root:provisional.expected_eic in
  let prepared = prepare state_root in
  let* trace = R.run (A.deps context ~cursor ~prepared) ~cursor ~prepared in
  expect "empty epoch artifacts differ"
    (trace.confirmed = [] && trace.rejections = [] && Z.equal trace.fees Z.zero);
  let* () =
    if duty = None && policy.mode = G.Active then
      let* stored = S.get_meta context.store F.meta_key in
      let value = match stored with
        | Some raw -> R.get (F.of_string raw) |> F.to_yojson
        | None -> failwith "validator duty state was not stored"
      in
      let cfg = match policy.standard_mode with
        | G.Prior -> F.standard
        | G.Active -> F.participating
      in
      let expected = Int64.add cursor.epoch (Int64.add cfg.window cfg.challenge) in
      expect "stored validator delay differs from epoch mode"
        (Yojson.Safe.Util.member "safe_after" value = `String (Int64.to_string expected));
      Lwt.return_unit
    else Lwt.return_unit
  in
  let* account = S.get_account context.store proposer in
  expect "inactive emission created a reward account" (account = None);
  let* supply = S.get_meta context.store "total_supply" in
  let* pool = S.get_meta context.store "emission_remaining" in
  expect "reward supply metadata differs"
    (supply = Some "0" && pool = Some "30000");
  let* last = S.get_meta context.store "last_epoch" in
  let* next = S.get_meta context.store "current_epoch" in
  expect "epoch metadata differs"
    (last = Some (Int64.to_string cursor.epoch)
     && next = Some (Int64.to_string (Int64.succ cursor.epoch)));
  Lwt.return (prepared.next_cursor, trace)

let with_stores ?(readonly = false) path f =
  let* store = S.open_store ~readonly (Filename.concat path "irmin_store") in
  Lwt.finalize (fun () ->
    let chaindata = D.open_chaindata ~readonly (Filename.concat path "chaindata") in
    Lwt.finalize (fun () -> f store chaindata)
      (fun () -> D.close chaindata; Lwt.return_unit)
  ) (fun () -> S.close store)

let run ~chain_id ~start ~count ~parent path =
  let* traces = with_stores path (fun store chaindata ->
    let* () = S.begin_epoch_batch store in
    let* () = Lwt_list.iter_s (fun (key, value) -> S.set_meta store key value)
      ["total_supply", "0"; "emission_remaining", "30000";
       "last_epoch", string_of_int (start - 1);
       "current_epoch", string_of_int start] in
    let* () = S.commit_epoch_batch store "epoch store sample" in
    let context = A.{
      store; chaindata; ledger = L.create store; chain_id;
      trust = Octra_vm.Program_trust.empty;
      rules = G.create_ready ~ready_config_hash:(String.make 64 'a') ~chain_id
        ~root_at:(fun epoch ->
          match G.root_after_floor ~chain_id ~floor_epoch:start ~epoch with
          | Some root -> G.Root root
          | None -> G.Missing);
      ready_root = (fun _ -> Lwt.return_none);
      legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ ->
        failwith "empty epoch requested private replay");
      result_policy = (fun _ -> Octra_core.Private_result_policy.Recoverable);
    } in
    let* initial = S.state_hash store in
    let cursor = R.J.{ epoch = Int64.of_int start; txid = 1L; eic = R.E.genesis_root;
      prev_root = R.E.folded_state_root ~ledger_state_root:initial
        ~epoch_index_root:R.E.genesis_root } in
    let rec steps cursor remaining traces =
      if remaining = 0 then Lwt.return (cursor, List.rev traces)
      else
        let* cursor, trace = epoch context parent cursor in
        steps cursor (remaining - 1) (trace :: traces)
    in
    let* cursor, traces = steps cursor count [] in
    let first = List.hd traces in
    let last = List.hd (List.rev traces) in
    expect "consecutive empty epochs did not advance"
      (cursor.epoch = Int64.of_int (start + count) && cursor.txid = 1L
       && first.ledger_root <> initial && last.ledger_root <> first.ledger_root
       && first.index_root <> last.index_root);
    Lwt.return traces) in
  let* () = with_stores ~readonly:true path (fun store chaindata ->
    let* actual = S.state_hash store in
    expect "reopened ledger root differs"
      (actual = (List.hd (List.rev traces)).ledger_root);
    List.iter (fun (trace : R.trace) ->
      let hash, root = D.get_epoch_index_commitment chaindata (Int64.to_int trace.epoch) in
      expect "reopened epoch index differs"
        (Option.is_some hash && root = Some trace.index_root)) traces;
    Lwt.return_unit) in
  List.iter (fun (trace : R.trace) ->
    let mode = match G.standard_at ~chain_id ~epoch:(Int64.to_int trace.epoch) with
      | G.Prior -> "prior"
      | G.Active -> "active"
    in
    Printf.printf
      "event = epoch_store auth = sample execution = production epoch = %Ld mode = %s ledger_root = %s index_root = %s state_root = %s\n"
      trace.epoch mode trace.ledger_root trace.index_root trace.state_root) traces;
  Lwt.return_unit

let parent ~chain_id keys (cursor : R.J.cursor) =
  let epoch_id = Int64.pred cursor.epoch in
  let header, _ = N.Consensus_replay.parse_header ~default_chain_id:chain_id
    (`Assoc [
      "epoch_id", `Intlit (Int64.to_string epoch_id);
      "txid_hi", `Intlit (Int64.to_string (Int64.pred cursor.txid));
      "creator_addr", `String "octEpochA";
      "prev_state_root", `String (String.make 64 '1');
      "proposed_state_root", `String cursor.prev_root;
    ]) in
  let plan = N.Consensus_replay.build_plan
    ~parent_commit:None ~header ~commit_round:0 ~txs:[] in
  let header = plan.finalize.header in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun (validator, key) ->
    let vote = C.{
      chain_id; epoch_id; round = 0; vote_type = Precommit; proposal_id;
      validator = validator.address; signature = "";
    } in
    { vote with signature = Mirage_crypto_ec.Ed25519.sign ~key (H.vote_sign_bytes vote) }
  ) keys in
  let commit = C.{
    validator_set = C.make_validator_set (List.map fst keys);
    certificate = { chain_id; epoch_id; commit_round = 0; header; proposal_id; precommits };
  } in
  ignore (R.get (Octra_core.Set_fold.read_parent ~chain_id commit));
  Some commit

let rec remove path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
    Unix.rmdir path
  | _ -> Unix.unlink path

let rec node_root path =
  if Sys.file_exists (Filename.concat path "lib/core/store_irmin.ml") then path
  else
    let parent = Filename.dirname path in
    expect "node source root is missing" (parent <> path);
    node_root parent

let rec with_env values f =
  match values with
  | [] -> f ()
  | (name, value) :: rest ->
    let previous = Sys.getenv_opt name in
    Unix.putenv name value;
    Fun.protect
      ~finally:(fun () -> Unix.putenv name (Option.value ~default:"" previous))
      (fun () -> with_env rest f)

let network key =
  let chain_id = "octra-devnet-9871-cluster" in
  let sample = ["OCTRA_CHAIN_ID", chain_id] in
  let getenv values name = List.assoc_opt name values in
  let identity values trust =
    Octra_consensus.C_config.network_hash ~chain_id
      ?program_trust_hash:(Octra_vm.Program_trust.config_hash trust)
      ~runtime_profile_hash:(N.Consensus_profile.compat_hash (getenv values)) ()
    |> Octra_bootstrap.State_sync_checkpoint.raw_to_hex
  in
  let config_hash = identity sample Octra_vm.Program_trust.empty in
  let configured_hash = config_hash in
  let check values = A.network ~getenv:(getenv values) ~chain_id ~config_hash ~configured_hash in
  expect "replay network rejected matching identity" (Result.is_ok (check sample));
  let reject label result =
    match result with
    | Error reason -> expect "replay network refusal differs" (reason = label)
    | Ok _ -> failwith ("replay network accepted " ^ label)
  in
  reject "replay chain configuration differs" (check []);
  reject "replay chain configuration differs" (check ["OCTRA_CHAIN_ID", "other"]);
  reject "unknown BFT release profile = other"
    (check (("OCTRA_BFT_RELEASE_PROFILE", "other") :: sample));
  reject "invalid proposal protocol activation epoch"
    (check (("OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH", "other") :: sample));
  reject "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH must be nonnegative"
    (check (("OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH", "-1") :: sample));
  reject "replay configured identity differs from checkpoint"
    (A.network ~getenv:(getenv sample) ~chain_id ~config_hash
      ~configured_hash:(String.make 64 '0'));
  reject "replay network identity differs from checkpoint"
    (A.network ~getenv:(getenv sample) ~chain_id
      ~config_hash:(String.make 64 '0') ~configured_hash:(String.make 64 '0'));
  reject "invalid program release key entry"
    (check (("OCTRA_PROGRAM_RELEASE_KEYS", "other") :: sample));
  let encoded = Mirage_crypto_ec.Ed25519.pub_of_priv key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  let trusted = ("OCTRA_PROGRAM_RELEASE_KEYS", "sample = " ^ encoded) :: sample in
  reject "replay network identity differs from checkpoint" (check trusted);
  let trust = Octra_vm.Program_trust.of_env (getenv trusted)
    |> Result.map_error Octra_vm.Program_trust.error_message |> R.get in
  let config_hash = identity trusted trust in
  let actual = R.get (A.network ~getenv:(getenv trusted) ~chain_id ~config_hash
    ~configured_hash:config_hash) in
  expect "replay network discarded release keys"
    (Octra_vm.Program_trust.keys actual = Octra_vm.Program_trust.keys trust);
  let config_hash = Octra_consensus.C_config.network_hash ~chain_id
    ~runtime_profile_hash:(N.Consensus_profile.standard_hash
      ~chain_id ~epoch:1_500_000 (getenv sample)) ()
    |> Octra_bootstrap.State_sync_checkpoint.raw_to_hex in
  reject "replay network identity differs from checkpoint"
    (A.network ~getenv:(getenv sample) ~chain_id ~config_hash ~configured_hash:config_hash);
  Printf.printf "event = replay_network status = pass cases = 12\n"

let () =
  let keys = List.mapi (fun index address ->
    let key = match Mirage_crypto_ec.Ed25519.priv_of_octets
      (String.make 32 (Char.chr (index + 1))) with
      | Ok key -> key
      | Error _ -> failwith "epoch signing key is invalid"
    in
    let public = Mirage_crypto_ec.Ed25519.pub_of_priv key in
    C.{ address; pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets public }, key
  ) ["octEpochA"; "octEpochB"; "octEpochC"; "octEpochD"] in
  network (snd (List.hd keys));
  let data = Filename.concat (node_root (Sys.getcwd ())) "runtime_data" in
  if not (Sys.file_exists data) then Unix.mkdir data 0o700;
  let path = Filename.concat data (Printf.sprintf "epoch-store-%d" (Unix.getpid ())) in
  with_env ["OCTRA_EMISSION_GUARD", "0"; "OCTRA_EMISSION_PROFILE", "";
    "OCTRA_EMISSION_ACTIVATION_EPOCH", "";
    "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH", "";
    "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH", "0"] (fun () ->
    let check ~chain_id ~start ~count ~parent =
      Unix.mkdir path 0o700;
      Lwt_main.run (Lwt.finalize (fun () -> run ~chain_id ~start ~count ~parent path)
        (fun () -> remove path; Lwt.return_unit))
    in
    check ~chain_id:"epoch-store-sample" ~start:1 ~count:2 ~parent:(fun _ -> None);
    let chain_id = "octra-devnet-9871-cluster" in
    check ~chain_id ~start:1_499_999 ~count:3 ~parent:(parent ~chain_id keys);
    check ~chain_id ~start:1_500_001 ~count:2 ~parent:(parent ~chain_id keys));
  Printf.printf "event = epoch_store status = pass scope = adapter_control auth = sample execution = production epochs = 7\n"