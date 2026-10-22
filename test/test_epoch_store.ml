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
module P = Pvac_ffi
module FB = Octra_core.Crypto.FheBalance
module T = Octra_core.Transaction

let proposer = "octEpochStoreSample"
let expect = R.require
let reward = R.X.{
  proposer_addr = proposer;
  proposer_public_key = None;
  validators = [{ address = proposer; public_key = None; weight = Z.one }];
}

let sample ?(txs = []) ?(receipts_json = []) ~chain_id ~parent_commit
    (cursor : R.J.cursor) state_root =
  let epoch_int = Int64.to_int cursor.epoch in
  let _, expected_eic = R.E.next_root_from_hashes_i64 ~prev:cursor.eic
    ~epoch_id:cursor.epoch ~start_txid:cursor.txid (R.hashes txs) in
  let tx_list_hash = R.J.root_hex64
    (Octra_net.Hash_domain.hash "octra:tx_list:v1"
      (String.concat "" (R.hashes txs))) in
  let receipt_root = R.J.root_hex64 (H.receipt_root receipts_json) in
  let epoch_ts = float_of_int (epoch_int * 10) in
  let header, _ = N.Consensus_replay.parse_header ~default_chain_id:chain_id
    (`Assoc [
      "epoch_id", `Int epoch_int;
      "txid_hi", `Intlit (Int64.to_string (Int64.add
        (Int64.pred cursor.txid) (Int64.of_int (List.length txs))));
      "creator_addr", `String proposer;
      "prev_state_root", `String cursor.prev_root;
      "proposed_state_root", `String state_root;
      "parent_commit_hash", `String (R.J.root_hex64 (H.parent_commit_hash_opt parent_commit));
      "tx_list_hash", `String tx_list_hash;
      "receipt_root", `String receipt_root;
      "ts", `Float epoch_ts;
    ]) in
  let plan = N.Consensus_replay.build_plan
    ~parent_commit ~header ~commit_round:0 ~receipts_json ~txs in
  let validator_set = match parent_commit with
    | Some commit -> commit.C.validator_set
    | None -> C.make_validator_set []
  in
  let record = R.J.{
    epoch_id = cursor.epoch;
    prev_state_root = cursor.prev_root;
    state_root;
    tx_list_hash;
    tx_hashes = R.hashes txs;
    txs_json = List.map (fun tx -> Yojson.Safe.to_string (T.to_yojson tx)) txs;
    receipts_json;
    receipt_root;
    epoch_ts;
    creator_addr = proposer;
    commit_round = 0;
    reward_source = R.get (N.Consensus_reward_attribution.to_source reward);
    finality = { finalize = plan.finalize; validator_set };
  } in
  R.J.{ record; txs; expected_eic; epoch_int;
    proposer_info = plan.proposer_info; reward;
    next_cursor = { epoch = Int64.succ cursor.epoch; prev_root = state_root;
      eic = expected_eic; txid = Int64.add cursor.txid (Int64.of_int (List.length txs)) } }

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
      "tx_list_hash", `String (Octra_net.Hash_domain.hash
        "octra:tx_list:v1" "");
      "receipt_root", `String (H.receipt_root []);
    ]) in
  let plan = N.Consensus_replay.build_plan
    ~parent_commit:None ~header ~commit_round:0 ~receipts_json:[] ~txs:[] in
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

let private_epoch keys path =
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch_id = (Option.get (Octra_core.Rule_graph.proof_key_activation_for_chain
    chain_id)).activation_epoch in
  let signing = Bytes.make 32 '\071' |> Bytes.to_string in
  let secret = Base64.encode_exn signing in
  let public_key = Mirage_crypto_ec.Ed25519.priv_of_octets signing
    |> Result.get_ok |> Mirage_crypto_ec.Ed25519.pub_of_priv
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  let address = Octra_core.Crypto.Address.address_from_pubkey public_key in
  let pk, sk = P.keygen_from_seed (P.default_params ()) (Bytes.make 32 '\072') in
  let cipher = P.enc_value_seeded pk sk 10L (Bytes.make 32 '\073') in
  let blind = Bytes.make 32 '\074' in
  let payload keyed =
    let proof = P.make_zero_proof_bound ~keyed ~math:true pk sk cipher 10L blind in
    Yojson.Safe.to_string (`Assoc [
      "cipher", `String (FB.encode_cipher cipher);
      "amount_commitment", `String (P.pedersen_commit_amount 10L blind
        |> Bytes.to_string |> Base64.encode_exn);
      "zero_proof", `String (FB.encode_zero_proof proof);
      "blinding", `String (Base64.encode_exn (Bytes.to_string blind));
    ]) in
  let tx keyed = T.sign_with_privkey T.{
    from = address; to_ = address; amount = Z.of_int 10; nonce = 1;
    ou = Z.of_int 3000; timestamp = float_of_int (epoch_id * 10);
    signature = ""; public_key = Some public_key; message = None;
    op_type = EncryptOp; encrypted_data = Some (payload keyed);
  } secret in
  let run_case keyed =
    Unix.mkdir path 0o700;
    Lwt_main.run (Lwt.finalize (fun () -> with_stores path (fun store chaindata ->
      let ledger = L.create store in
      L.add_account_with_pubkey ledger address (Z.of_int 1_000_000) public_key |> R.get;
      let* () = L.set_pvac_pubkey ledger address
        (P.serialize_pubkey pk |> Bytes.to_string) in
      let* () = L.flush_dirty_lwt ledger in
      let* () = S.begin_epoch_batch store in
      let* () = Lwt_list.iter_s (fun (name, value) -> S.set_meta store name value)
        ["total_supply", "1000000"; "emission_remaining", "0";
         "last_epoch", string_of_int (epoch_id - 1);
         "current_epoch", string_of_int epoch_id] in
      let* () = S.commit_epoch_batch store "private replay input" in
      let context = A.{store; chaindata; ledger; chain_id;
        trust = Octra_vm.Program_trust.empty;
        rules = G.create_ready ~ready_config_hash:(String.make 64 'a') ~chain_id
          ~root_at:(fun epoch -> match G.root_after_floor ~chain_id
            ~floor_epoch:epoch_id ~epoch with
            | Some root -> G.Root root
            | None -> G.Missing);
        ready_root = (fun _ -> Lwt.return_none);
        legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ ->
          failwith "private replay requested legacy history");
        result_policy = (fun _ -> Octra_core.Private_result_policy.Recoverable);
      } in
      let* initial = S.state_hash store in
      let cursor = R.J.{epoch = Int64.of_int epoch_id; txid = 1L;
        eic = R.E.genesis_root;
        prev_root = R.E.folded_state_root ~ledger_state_root:initial
          ~epoch_index_root:R.E.genesis_root} in
      let parent_commit = parent ~chain_id keys cursor in
      let transaction = tx keyed in
      expect "private replay transaction signature differs"
        (T.verify transaction public_key);
      let make ?(receipts_json = []) root = sample ~chain_id ~parent_commit
        ~txs:[transaction] ~receipts_json cursor root in
      let provisional = sample ~chain_id ~parent_commit cursor cursor.prev_root in
      let deps = A.deps context ~cursor ~prepared:provisional in
      let* batch = deps.preverify [transaction] in
      if not keyed then begin
        expect "prior proof accepted after key rule"
          (List.length batch.ready = 0 || List.length batch.skipped > 0);
        Lwt.return_unit
      end else begin
        expect "keyed private replay preverify refused"
          (List.length batch.ready = 1 && batch.skipped = []);
        let receipts = R.W.receipts_for_hashes batch.ready [T.hash transaction] in
        let receipts_json = R.O.encode (R.W.json_of_receipts receipts) [] in
        let gate = R.G.create receipts in
        let* preview = deps.preview gate [transaction] in
        let preview = R.get preview in
        let _, index_root = R.E.next_root_from_hashes_i64 ~prev:cursor.eic
          ~epoch_id:cursor.epoch ~start_txid:cursor.txid [T.hash transaction] in
        let root = R.E.folded_state_root ~ledger_state_root:preview.post_state_root
          ~epoch_index_root:index_root in
        let prepared = make ~receipts_json root in
        let* trace = R.run (A.deps context ~cursor ~prepared) ~cursor ~prepared in
        expect "private replay omitted confirmed transaction"
          (trace.confirmed = [T.hash transaction]);
        let* stored = S.get_account store address in
        let account = Option.get stored in
        expect "private replay nonce differs" (account.nonce = 1);
        expect "private replay public balance differs"
          (Z.equal account.balance (Z.of_int 996_990));
        let cipher = Option.get account.encrypted_balance
          |> FB.decode_cipher ~strict:true |> R.get in
        expect "private replay encrypted balance differs"
          (P.dec_value pk sk cipher = 10L);
        Lwt.return_unit
      end)) (fun () -> remove path; Lwt.return_unit))
  in
  run_case false;
  run_case true;
  Printf.printf "event = epoch_store_private status = pass epoch = %d\n" epoch_id

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

let circle_epoch keys path =
  let chain_id = "octra-devnet-9871-cluster" in
  let start = (Option.get (G.batch_activation_for_chain chain_id)).activation_epoch in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 '\075')
    |> Result.get_ok in
  let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  let secret = Mirage_crypto_ec.Ed25519.priv_to_octets key |> Base64.encode_exn in
  let owner = Octra_core.Crypto.Address.address_from_pubkey public_key in
  let target = "oct" ^ String.make 44 '2' in
  let compiled = Octra_vm.Oct_compile.compile {|
Program ReplayOrder {
  state { observed: int }
  fn watch(who: address): int {
    self.observed = balance(who)
    return self.observed
  }
}
|} in
  Option.iter failwith compiled.error;
  let run_case epoch_id payment_first =
    Unix.mkdir path 0o700;
    Lwt_main.run (Lwt.finalize (fun () -> with_stores path (fun store chaindata ->
      let ledger = L.create store in
      L.add_account_with_pubkey ledger owner (Z.of_int 1_000_000) public_key |> R.get;
      let module Circle = Octra_core.Circles in
      let info = Circle.{circle_id = target; runtime = Octb; version = 1L; owner;
        code_hash = sha256_hex compiled.bytecode; stable_root = zero_hash_hex;
        assets_root = zero_hash_hex; privacy_class = Public; browser_mode = Native_sealed;
        resource_mode = Public_resources; policy_hash = None; members_root = None;
        export_policy = None; limits = default_limits} in
      let* () = S.deploy_circle store info in
      let* () = S.save_circle_program_code_b64 store target
        (Base64.encode_exn compiled.bytecode) in
      let* () = L.flush_dirty_lwt ledger in
      let* () = S.begin_epoch_batch store in
      let* () = Lwt_list.iter_s (fun (name, value) -> S.set_meta store name value)
        ["total_supply", "1000000"; "emission_remaining", "0";
         "last_epoch", string_of_int (epoch_id - 1);
         "current_epoch", string_of_int epoch_id] in
      let* () = S.commit_epoch_batch store "circle replay input" in
      let context = A.{store; chaindata; ledger; chain_id;
        trust = Octra_vm.Program_trust.empty;
        rules = G.create_ready ~ready_config_hash:(String.make 64 'a') ~chain_id
          ~root_at:(fun epoch -> match G.root_after_floor ~chain_id
            ~floor_epoch:epoch_id ~epoch with
            | Some root -> G.Root root
            | None -> G.Missing);
        ready_root = (fun _ -> Lwt.return_none);
        legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ ->
          failwith "circle replay requested private history");
        result_policy = (fun _ -> Octra_core.Private_result_policy.Recoverable)} in
      let transaction ~nonce ~circle =
        T.sign_with_privkey T.{from = owner; to_ = if circle then target else owner;
          nonce; amount = if circle then Z.zero else Z.one; ou = Z.of_int 100_000;
          timestamp = float_of_int (epoch_id * 10); signature = "";
          public_key = Some public_key; op_type = if circle then CircleCall else Standard;
          message = Some (if circle then Yojson.Safe.to_string (`List [`String owner]) else "");
          encrypted_data = if circle then Some "watch" else None} secret in
      let active = epoch_id >= start in
      let txs = if not active then [transaction ~nonce:1 ~circle:true]
        else [transaction ~nonce:1 ~circle:(not payment_first);
          transaction ~nonce:2 ~circle:payment_first] in
      let* initial = S.state_hash store in
      let cursor = R.J.{epoch = Int64.of_int epoch_id; txid = 1L; eic = R.E.genesis_root;
        prev_root = R.E.folded_state_root ~ledger_state_root:initial
          ~epoch_index_root:R.E.genesis_root} in
      let parent_commit = parent ~chain_id keys cursor in
      let make ?(receipts_json = []) root =
        sample ~chain_id ~parent_commit ~txs ~receipts_json cursor root in
      let provisional = sample ~chain_id ~parent_commit cursor cursor.prev_root in
      let deps = A.deps context ~cursor ~prepared:provisional in
      let* batch = deps.preverify txs in
      expect "circle replay omitted an input"
        (batch.skipped = [] && R.hashes (R.W.txs batch) = R.hashes txs);
      let receipts = R.W.receipts_for_hashes batch.ready (R.hashes txs) in
      expect "circle replay receipt absent" (List.length receipts = 1);
      let gate = R.G.create receipts in
      let* preview = deps.preview gate txs in
      let preview = R.get preview in
      expect "circle replay execution refused"
        (R.hashes (List.map fst preview.artifacts.confirmed) = R.hashes txs
         && preview.artifacts.rejected = []);
      let* unchanged = S.state_hash store in
      expect "circle replay preparation wrote live state" (unchanged = initial);
      let _, index_root = R.E.next_root_from_hashes_i64 ~prev:cursor.eic
        ~epoch_id:cursor.epoch ~start_txid:cursor.txid (R.hashes txs) in
      let root = R.E.folded_state_root ~ledger_state_root:preview.post_state_root
        ~epoch_index_root:index_root in
      let prepared = make ~receipts_json:(R.O.encode (R.W.json_of_receipts receipts) []) root in
      let bad = {prepared with R.J.record = {prepared.record with receipts_json = []}} in
      let* rejected = Lwt.catch
        (fun () -> let* _ = R.run (A.deps context ~cursor ~prepared:bad)
          ~cursor ~prepared:bad in Lwt.return_false)
        (function Failure reason when reason = "replay preverify receipts differ" -> Lwt.return_true
          | error -> Lwt.fail error) in
      expect "circle replay accepted missing receipt" rejected;
      let* unchanged = S.state_hash store in
      expect "circle replay refusal wrote live state" (unchanged = initial);
      let* trace = R.run (A.deps context ~cursor ~prepared) ~cursor ~prepared in
      expect "circle replay changed confirmed order" (trace.confirmed = R.hashes txs);
      expect "circle replay fees differ"
        (Z.equal trace.fees (Z.of_int (100_000 * List.length txs)));
      let* stored = S.get_account store owner in
      expect "circle replay lost following nonce"
        ((Option.get stored).nonce = List.length txs);
      let* storage = S.load_circle_stable_storage store target in
      let expected = if payment_first then "800000" else "900000" in
      expect "circle replay observed wrong transaction state"
        (Hashtbl.find_opt (R.get storage) "observed" = Some expected);
      Lwt.return_unit)) (fun () -> remove path; Lwt.return_unit)) in
  run_case (start - 1) false;
  run_case start false;
  run_case (start + 1) true;
  Printf.printf "event = epoch_store_circle status = pass cases = 3\n"

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
    "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH", "0";
    "OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1"] (fun () ->
    let check ~chain_id ~start ~count ~parent =
      Unix.mkdir path 0o700;
      Lwt_main.run (Lwt.finalize (fun () -> run ~chain_id ~start ~count ~parent path)
        (fun () -> remove path; Lwt.return_unit))
    in
    check ~chain_id:"epoch-store-sample" ~start:1 ~count:2 ~parent:(fun _ -> None);
    let chain_id = "octra-devnet-9871-cluster" in
    check ~chain_id ~start:1_499_999 ~count:3 ~parent:(parent ~chain_id keys);
    check ~chain_id ~start:1_500_001 ~count:2 ~parent:(parent ~chain_id keys);
    circle_epoch keys path;
    private_epoch keys path);
  Printf.printf "event = epoch_store status = pass scope = adapter_control auth = sample execution = production epochs = 7\n"