(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Core = Octra_core
module Node = Octra_node_runtime
module C = Node.Consensus_proposal
module T = Core.Transaction
module S = Core.Store_irmin
module L = Core.Ledger
module X = Core.Epoch_exec
module G = Core.Rule_graph
module W = Core.Preverify_worker
module CT = Octra_consensus.C_types
module H = Octra_consensus.C_hash

let expect label ok = if not ok then failwith label
let unwrap = function Ok value -> value | Error error -> failwith error
let chain = "octra-devnet-9871-cluster"
let config = String.make 64 'c'

type identity = {
  address : string;
  secret : Mirage_crypto_ec.Ed25519.priv;
  public : string;
}

let identity index =
  let secret = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 (Char.chr index))
    |> function Ok key -> key | Error _ -> failwith "private key rejected" in
  let public = Mirage_crypto_ec.Ed25519.pub_of_priv secret
    |> Mirage_crypto_ec.Ed25519.pub_to_octets in
  let address = Core.Crypto.Address.address_from_pubkey (Base64.encode_exn public) in
  {address; secret; public}

let make_parent ?(txs = []) ?(state_root = String.make 32 'b') epoch identities =
  let validator_set = CT.make_validator_set (List.map (fun (item : identity) ->
    CT.{address = item.address; pubkey = item.public}) identities) in
  let header = CT.{
    proto_version = proto_version_current; chain_id = chain; epoch_id = epoch;
    prev_state_root = String.make 32 'a'; tx_list_hash = C.tx_list_hash (List.map T.hash txs);
    receipt_root = H.receipt_root []; proposed_state_root = state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (List.hd identities).address; txid_hi = 0L; ts = 1.;
  } in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun (item : identity) ->
    let vote = CT.{chain_id = chain; epoch_id = epoch; round = 0; vote_type = Precommit;
      proposal_id; validator = item.address; signature = ""} in
    {vote with signature = Mirage_crypto_ec.Ed25519.sign ~key:item.secret
      (H.vote_sign_bytes vote)}) identities in
  CT.{validator_set; certificate = {chain_id = chain; epoch_id = epoch;
    commit_round = 0; header; proposal_id; precommits}}

let signed ?(ou = 100_000) identity ~op_type ~to_ ~nonce ~message ~method_ =
  let key = Base64.encode_exn identity.public in
  let tx = T.{from = identity.address; to_; nonce; op_type;
    amount = (if op_type = T.Standard then Z.one else Z.zero);
    ou = Z.of_int ou; timestamp = 0.; signature = "";
    public_key = Some key; message = Some message; encrypted_data = method_} in
  let secret = Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets identity.secret) in
  let tx = T.sign_with_privkey tx secret in
  expect "circle transaction signature invalid" (T.verify tx key);
  match Core.Tx_envelope.normalize ~sender_pk:(Some key) tx with
  | Ok tx -> tx
  | Error (_, error) -> failwith error

let install store owner circle_id runtime code =
  let info = Core.Circles.{circle_id; runtime; version = 1L; owner;
    code_hash = sha256_hex code; stable_root = zero_hash_hex;
    assets_root = zero_hash_hex; privacy_class = Public; browser_mode = Native_sealed;
    resource_mode = Public_resources; policy_hash = None; members_root = None;
    export_policy = None; limits = default_limits} in
  Lwt_main.run (S.deploy_circle store info);
  let policy = Hashtbl.create 1 in
  Hashtbl.add policy Core.Circle_hfhe_policy.require_live_key_policy_key "false";
  ignore (Lwt_main.run (S.save_circle_stable_storage store circle_id policy));
  Lwt_main.run (S.save_circle_program_code_b64 store circle_id (Base64.encode_exn code));
  circle_id

let deploy store owner =
  let compiled = Octra_vm.Oct_compile.compile {|
Program CircleRetry {
  state { count: int }
  constructor() { self.count = 0 }
  fn refuse(): int {
    self.count = 7
    require(false, "refused")
    return 7
  }
  fn accept(): int {
    self.count = 9
    return 9
  }
  fn exhaust(): int {
    while true { self.count = self.count + 1 }
    return self.count
  }
  fn advance(): int {
    require(self.count == 9, "order")
    self.count = 10
    return 10
  }
  fn fresh(): int {
    require(self.count == 0, "rollback")
    self.count = 11
    return 11
  }
  fn clock(): int {
    self.count = epoch_time
    return self.count
  }
  fn watch(who: address): int {
    self.count = balance(who)
    return self.count
  }
}
|} in
  Option.iter failwith compiled.error;
  install store owner ("oct" ^ String.make 44 '2') Core.Circles.Octb compiled.bytecode

let host_code () =
  let key, _ = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\071') in
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"fhe_verify_zero"
    [`String (Pvac_ffi.serialize_pubkey key |> Bytes.to_string |> Base64.encode_exn);
     `String "ciphertext"; `String "proof"] |> unwrap in
  let byte value = String.make 1 (Char.chr value) in
  let rec number limit value =
    if value < limit then byte value
    else byte ((value land 127) lor 128) ^ number limit (value lsr 7) in
  let size = number 128 in
  let string value = size (String.length value) ^ value in
  let vector values = size (List.length values) ^ String.concat "" values in
  let section tag value = byte tag ^ string value in
  let const value = "\065" ^ number 64 value in
  let export name kind index = string name ^ byte kind ^ size index in
  let body instructions = string ("\000" ^ instructions ^ "\011") in
  let key_pos = String.length request in
  let data = request ^ "counter1" in
  let data_end = String.length data in
  String.concat "" [
    "\000asm\001\000\000\000";
    section 1 (vector ["\096\002\127\127\001\127"; "\096\001\127\001\127";
      "\096\004\127\127\127\127\001\127"]);
    section 2 (vector [string "octra" ^ string "host_hfhe_invoke_len" ^ "\000\000";
      string "octra" ^ string "host_kv_put" ^ "\000\002"]);
    section 3 (vector [size 1; size 0]);
    section 5 (vector ["\000" ^ size ((data_end + 65_535 + 128) / 65_536)]);
    section 7 (vector [export "memory" 2 0; export "octra_alloc" 0 2;
      export "octra_update" 0 3]);
    section 10 (vector [body (const data_end);
      body (const key_pos ^ const 7 ^ const (key_pos + 7) ^ const 1 ^ "\016\001\026"
        ^ const 0 ^ const key_pos ^ "\016\000\026" ^ const 0)]);
    section 11 (vector ["\000" ^ const 0 ^ "\011" ^ string data]);
  ]

let with_host_busy action =
  let lane = Core.Circle_wasm_hfhe_backend.verifier_lane in
  Mutex.lock lane;
  Fun.protect ~finally:(fun () -> Mutex.unlock lane) action

let root_raw root =
  let root = Core.Epoch_index_commitment.root_hex root in
  String.init 32 (fun i -> Char.chr (int_of_string ("0x" ^ String.sub root (2 * i) 2)))

let test_work () =
  let module Lane = Core.Resource_lanes in
  let module Gate = Core.Preverify_commit in
  let tx = signed (identity 1) ~op_type:T.CircleCall ~to_:(identity 8).address
    ~nonce:1 ~message:"[]" ~method_:(Some "accept") in
  let consume check inputs = List.fold_left (fun used item ->
    Result.bind used (fun used -> check (Gate.create []) used item)) (Ok []) inputs in
  expect "empty usage loses lane consumption"
    (consume Gate.check_work [tx; {tx with nonce = 2}]
     = Error "lane_budget:circle_compute:tx_count");
  let state = Octra_vm.Contract_vm.create_state ~caller:tx.from ~origin:tx.from
    ~address:tx.to_ ~value:Z.zero ~storage:(Hashtbl.create 1) () in
  let floor = state.effort_limit in
  expect "constructor allowance differs" (floor = Lane.vm_floor);
  let fees = List.map Z.of_int [min_int; -1; 0; 1; 999_999; 1_000_000; 1_000_001; max_int]
    @ [Z.pred (Z.of_int min_int); Z.succ (Z.of_int max_int); Z.shift_left Z.one 200] in
  List.iter (fun ou ->
    let expected = max 1_000_000 (if Z.fits_int ou then Z.to_int ou else 1_000_000) in
    expect "vm allowance changed" (Octra_vm.Call_plan.effort_limit ou = expected);
    List.iter (fun op_type ->
      let item = {tx with T.ou; op_type} in
      let declared = Lane.cost item in
      let reserved = Lane.work item in
      expect "work changed declared fields"
        (reserved.txs = declared.txs && reserved.bytes = declared.bytes
         && reserved.proof = declared.proof);
      expect "work omits execution allowance" (Z.geq reserved.ou (Z.of_int expected));
      expect "work reduced declared cost" (Z.geq reserved.ou declared.ou))
      [T.ProgramExec; T.MultiExec; T.CircleCall]) fees;
  let program nonce = {tx with T.op_type = ProgramExec; nonce; ou = Z.of_int 10_000} in
  let ten = List.init 10 (fun i -> program (i + 1)) in
  let exact = consume Gate.check_work ten |> unwrap in
  expect "vm work total differs"
    (Z.equal (List.assoc Lane.Program exact).ou (Z.of_int 10_000_000));
  expect "vm work over capacity admitted"
    (consume Gate.check_work (ten @ [program 11]) = Error "lane_budget:program:ou");
  expect "declared admission changed"
    (Result.is_ok (consume Gate.check_budget (ten @ [program 11])));
  let deploy = {tx with T.op_type = ProgramDeploy; ou = Z.one} in
  expect "constructor work omitted" (Z.geq (Lane.work deploy).ou (Z.of_int floor));
  List.iter (fun op_type ->
    let item = {tx with T.op_type} in
    expect "non-vm receipt cost changed" (Lane.work item = Lane.cost item))
    [T.Standard; T.ValidatorReady; T.EncryptOp; T.ClaimOp; T.StealthOp; T.KeySwitch]

let test_select () =
  let make sender op_type nonce = signed (identity sender) ~op_type
    ~to_:(identity 8).address ~nonce ~message:"[]" ~method_:None in
  let bad = make 1 T.CircleCall 2 in
  let prior = make 1 T.Standard 1 in
  let dependent = make 1 T.Standard 3 in
  let other = make 2 T.CircleBalanceCellPut 1 in
  let later = make 2 T.Standard 2 in
  let payment = make 3 T.Standard 1 in
  let inputs = [prior; bad; dependent; other; later; payment] in
  let select = Node.Circle_refill.select in
  expect "deferred nonce retained later work"
    (Node.Circle_refill.before ~excluded:[bad] inputs = [prior; other; later; payment]);
  expect "deferred nonce depends on input order"
    (Node.Circle_refill.before ~excluded:[bad] (List.rev inputs)
     = List.rev [prior; other; later; payment]);
  let rec subsets = function
    | [] -> [[]]
    | item :: rest ->
      let sets = subsets rest in
      sets @ List.map (fun set -> item :: set) sets in
  List.iter (fun excluded ->
    expect "refusal retained nonce successors"
      (Node.Circle_refill.through ~rejected:excluded inputs = List.filter (fun tx ->
        List.for_all (fun cut -> cut.T.from <> tx.T.from || tx.nonce <= cut.nonce) excluded) inputs);
    expect "first refusal differs"
      (Node.Circle_refill.first excluded = List.filter (fun tx ->
        List.for_all (fun other -> tx.T.from <> other.T.from || tx.nonce <= other.nonce) excluded) excluded);
    let expected = List.filter (fun tx -> List.for_all (fun cut ->
      cut.T.from <> tx.T.from || tx.nonce < cut.nonce) excluded) inputs in
    let selected = Node.Circle_refill.before ~excluded inputs in
    expect "nonce selection differs from exclusions" (selected = expected);
    expect "nonce selection is not repeatable"
      (Node.Circle_refill.before ~excluded selected = selected);
    expect "nonce selection depends on exclusion order"
      (Node.Circle_refill.before ~excluded:(List.rev excluded) inputs = selected)) (subsets inputs);
  expect "refill omitted prerequisites or kept dependent nonces"
    (select ~selected:[bad] ~confirmed:[] ~rejected:1 inputs = Some [prior; payment]);
  expect "refill changed successful circle"
    (select ~selected:[bad] ~confirmed:[bad] ~rejected:0 inputs = None);
  expect "refill changed resource failure"
    (select ~selected:[bad] ~confirmed:[] ~rejected:0 inputs = None);
  expect "refill changed ordinary rejection"
    (select ~selected:[payment] ~confirmed:[] ~rejected:1 inputs = None);
  expect "refill retried empty alternatives"
    (select ~selected:[bad] ~confirmed:[] ~rejected:1 [bad; dependent] = None);
  expect "refill changed multi-input rejection"
    (select ~selected:[bad; payment] ~confirmed:[] ~rejected:2 inputs = None);
  expect "refill retained register cell"
    (select ~selected:[bad] ~confirmed:[] ~rejected:1
      [bad; make 2 T.CircleRegisterCellPut 1; later; payment] = Some [payment])

let test_drop () =
  let module Pool = Core.Tx_staging in
  Pool.clear ();
  Fun.protect ~finally:Pool.clear (fun () ->
    let sender = identity 1 in
    let tx ~ou ~nonce = signed ~ou sender ~op_type:T.CircleCall
      ~to_:(identity 2).address ~nonce ~message:"[]" ~method_:(Some "run") in
    let old = tx ~ou:100_000 ~nonce:1 in
    let replacement = tx ~ou:200_000 ~nonce:1 in
    let dependent = tx ~ou:100_000 ~nonce:2 in
    let add tx = Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 1_000_000_000, 0)) tx
      |> unwrap |> ignore in
    List.iter add [old; replacement; dependent];
    expect "preview removed replacement" (Pool.drop_preview old = None);
    expect "replacement missing" (Pool.find_by_hash (T.hash replacement) = Some replacement);
    expect "preview drop missing" (Option.is_some (Pool.drop_preview replacement));
    expect "preview drop repeated" (Pool.drop_preview replacement = None);
    expect "preview drop removed dependent" (Pool.find_by_hash (T.hash dependent) = Some dependent);
    let ready () = Pool.ready_epoch_txs ~accept:(fun _ -> true)
      ~capacity:Pool.max_ou_per_epoch ~confirmed_nonce:(fun _ -> Some 0) in
    expect "dropped nonce released successor" (ready () = []);
    add replacement;
    expect "replacement cannot recover gap" (ready () = [replacement; dependent]);
    Pool.remove_processed [T.hash replacement; T.hash dependent];
    expect "preview drop leaked queue accounting"
      (Pool.staging_size () = 0 && Z.equal !(Pool.total_ou) Z.zero);
    add old;
    expect "preview drop kept virtual nonce" (ready () = [old]))

let test_turn () =
  let module Turn = Node.Circle_turn in
  let who = identity 1 in
  let circle = signed who ~op_type:T.CircleCall ~to_:(identity 2).address
    ~nonce:1 ~message:"[]" ~method_:(Some "accept") in
  let transfer who nonce = signed who ~op_type:T.Standard ~to_:(identity 2).address
    ~nonce ~message:"" ~method_:None in
  let dependent = transfer who 2 in
  let payment = transfer (identity 3) 1 in
  let inputs = [circle; dependent; payment] in
  let select ~epoch ~previous inputs =
    let ordinary = Node.Circle_refill.without inputs in
    Turn.select ~ordered:false ~epoch ~previous ~ordinary inputs in
  List.iter (fun previous ->
    expect "circle repeated or used unknown parent"
      (select ~epoch:12L ~previous inputs = [payment])) [None; Some true];
  expect "circle turn removed ordinary work"
    (select ~epoch:12L ~previous:(Some false) inputs = inputs);
  let ready head =
    let message = `Assoc [
      "consensus_pubkey", `String (Base64.encode_exn who.public);
      "head_epoch", `String (Int64.to_string head);
      "head_proposal_id", `String (String.make 64 'a');
      "state_root", `String (String.make 64 'b');
      "chain_id", `String chain; "config_hash", `String config;
      "catchup_head_epoch", `String (Int64.to_string head);
    ] |> Yojson.Safe.to_string in
    signed (identity 4) ~op_type:T.ValidatorReady ~to_:who.address ~nonce:1
      ~message ~method_:None in
  let urgent = ready 9L in
  expect "circle consumed last ready epoch"
    (select ~epoch:12L ~previous:(Some false) (inputs @ [urgent]) = [payment; urgent]);
  let module Graph = Octra_core.Rule_graph in
  let graph = Graph.create ~chain_id:"octra-devnet-9871-cluster" ~root_at:(fun _ ->
    Graph.Root "8e7f0e5a6e582070c040a07e7439caf532973fa09cddc79357a5ed964468065d") in
  List.iter (fun epoch ->
    let ordered = Result.get_ok (Graph.circle_batch graph ~epoch) = Graph.Active in
    let system = ready (Int64.of_int (epoch - 1)) in
    let txs = inputs @ [system] in
    let chosen = Turn.select ~ordered ~epoch:(Int64.of_int epoch)
      ~previous:(Some true) ~ordinary:(Node.Circle_refill.without txs) txs in
    expect "activation lost system work" (List.mem system chosen);
    expect "activation did not select mixed work"
      (chosen = if epoch < 1_663_000 then [payment; system] else txs))
    [1_648_974; 1_662_999; 1_663_000; 1_663_001];
  List.iter (fun previous ->
    let choose inputs = Turn.select ~ordered:true ~epoch:12L ~previous
      ~ordinary:(Node.Circle_refill.without inputs) inputs in
    expect "mixed call lost its turn" (choose inputs = inputs);
    expect "mixed call consumed last ready epoch"
      (choose (inputs @ [urgent]) = [payment; urgent]);
    List.iter (fun op_type ->
      let cell = signed who ~op_type ~to_:circle.to_ ~nonce:1
        ~message:"{}" ~method_:None in
      let cells = [cell; dependent; payment] in
      expect "exclusive cell lost turn protection"
        (choose cells = if previous = Some false then cells else [payment]);
      expect "exclusive cell consumed last ready epoch"
        (choose (cells @ [urgent]) = [payment; urgent]))
      [T.CircleBalanceCellPut; T.CircleRegisterCellPut])
    [None; Some false; Some true];
  List.iter (fun head ->
    let txs = inputs @ [ready head] in
    expect "ready outside last epoch changed circle turn"
      (select ~epoch:12L ~previous:(Some false) txs = txs))
    [-1L; 8L; 10L; 11L; 12L; Int64.max_int];
  let previous txs = make_parent ~txs 11L [who] in
  let prior = previous [circle] in
  let lookup pid =
    expect "parent lookup identity differs" (pid = prior.certificate.proposal_id);
    Some [circle] in
  expect "parent circle not recognized"
    (Turn.previous ~chain_id:chain ~epoch:12L ~lookup (Some prior) = Some true);
  expect "other epoch accepted for circle turn"
    (Turn.previous ~chain_id:chain ~epoch:13L ~lookup (Some prior) = None);
  expect "other chain accepted for circle turn"
    (Turn.previous ~chain_id:"other" ~epoch:12L ~lookup (Some prior) = None);
  List.iter (fun txs ->
    expect "parent cache mismatch accepted"
      (Turn.previous ~chain_id:chain ~epoch:12L ~lookup:(fun _ -> txs)
        (Some prior) = None)) [None; Some []; Some [payment]];
  let prior = {prior with certificate = {prior.certificate with proposal_id = String.make 32 'x'}} in
  expect "parent identity mismatch accepted"
    (Turn.previous ~chain_id:chain ~epoch:12L ~lookup (Some prior) = None);
  expect "empty parent needs local cache"
    (Turn.previous ~chain_id:chain ~epoch:12L
      ~lookup:(fun _ -> failwith "empty parent lookup") (Some (previous [])) = Some false);
  expect "ordinary parent denied circle turn"
    (Turn.previous ~chain_id:chain ~epoch:12L ~lookup:(fun _ -> Some [payment])
      (Some (previous [payment])) = Some false)

let run_epoch ~make_deps ~verify_deps ~limits epoch =
  ignore (unwrap (Core.Tx_staging.Preview.send
    (Check (Some (Int64.of_int (epoch - 1)), ""))));
  Test_workspace.with_dir "circle_proposal" (fun dir ->
    let store = Lwt_main.run (S.open_store (Filename.concat dir "irmin")) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
      let identities = List.init 8 (fun i -> identity (i + 1))
        |> List.sort (fun a b -> String.compare a.address b.address) in
      let caller = List.nth identities 0 in
      let payer = List.nth identities 6 in
      let member = List.nth identities 7 in
      let other = List.nth identities 5 in
      let validators = List.init 4 (fun i -> List.nth identities (i + 1)) in
      let proposer = List.hd validators in
      let ledger = L.create store in
      List.iter (fun item ->
        unwrap (L.add_account ledger item.address (Z.of_int 10_000_000_000))) identities;
      unwrap (L.add_account ledger Core.Validator_registry.escrow_address
        Core.Validator_policy.min_bond);
      Lwt_main.run (L.flush_dirty_lwt ledger);
      Lwt_main.run (S.set_meta store "total_supply" (Z.to_string (L.get_total_supply ledger)));
      Lwt_main.run (S.set_meta store "emission_remaining" "0");
      let registry = `Assoc [
        "standard", `String Core.Validator_policy.standard_name;
        "candidates", `List [`Assoc [
          "address", `String member.address;
          "consensus_pubkey", `String (Base64.encode_exn member.public);
          "bond", `String (Z.to_string Core.Validator_policy.min_bond);
          "bonded_epoch", `String "100";
          "ready_epoch", `Null; "exit_epoch", `Null;
        ]]; "slashes", `List [];
      ] |> Core.Validator_registry.of_yojson |> unwrap in
      Lwt_main.run (S.set_meta store Core.Validator_registry.meta_key
        (Core.Validator_registry.to_string registry));
      let circle = deploy store caller.address in
      let program = (identity 10).address in
      let code = Octra_vm.Oct_compile.compile {|
program WorkCase {
  state { count: int }
  fn accept(): int {
    self.count = 1
    return 7
  }
  fn fresh(): int {
    require(self.count == 0, "count")
    return 7
  }
}
|} in
      expect "work program failed compilation" (code.error = None);
      Lwt_main.run (S.deploy_contract store ~address:program ~owner:caller.address
        ~ctype:"CUSTOM" ~version:"1" ~admission:"binary"
        ~code_hash:(Core.Circles.sha256_hex code.bytecode)
        ~bytecode_b64:(Base64.encode_exn code.bytecode));
      let wasm = install store caller.address ("oct" ^ String.make 44 '3')
        Core.Circles.Wasm_v1 (host_code ()) in
      let root = Lwt_main.run (L.hash ledger) in
      let commit = Lwt_main.run (S.get_commit_hash store) in
      let parent = make_parent ~state_root:(root_raw root) (Int64.of_int (epoch - 1)) validators in
      ignore (unwrap (Core.Set_fold.read_parent ~chain_id:chain parent));
      let rules = G.create_ready ~chain_id:chain ~ready_config_hash:config
        ~root_at:(fun at -> match G.root_after_floor ~chain_id:chain
          ~floor_epoch:epoch ~epoch:at with
          | Some value -> G.Root value | None -> G.Missing) in
      let pubkeys = List.map (fun item -> item.address, item.public) validators in
      let env = X.{chain_id = chain; epoch_id = epoch; proposer_addr = proposer.address;
        validator_addrs = List.map fst pubkeys; validator_pubkeys = pubkeys;
        prev_state_root = root; epoch_ts = 99.;
        ready_state_root_at = Some (fun _ -> failwith "ready read local history");
        ready_max_lag = 0} in
      let runtime = Node.Consensus_circle_preverify.{store; ledger;
        program_trust = Octra_vm.Program_trust.empty; rules;
        env = (fun ~pre_state_root -> {env with prev_state_root = pre_state_root})} in
      let batches = ref [] in
      let checked ~state_root ~tx_hashes txs =
        expect "preverify hashes differ from inputs" (tx_hashes = List.map T.hash txs);
        expect "preverify changed root" (state_root = root || state_root = root_raw root);
        batches := txs :: !batches;
        W.run_many ~field_policy:Core.Private_ledger.Unique_fields ~strict:false ~ledger
          ~circle_preverify:(Node.Consensus_circle_preverify.run runtime) txs in
      let backend = Node.Consensus_proposal_preview_shell.node_backend
        ~program_trust:Octra_vm.Program_trust.empty ~rules
        ~legacy_replay:(fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "legacy replay")
        ~private_result_policy:(fun _ -> Core.Private_result_policy.Recoverable)
        ~max_fhe:1 ~max_stealth:1 store ledger in
      let preview_runtime = Node.Consensus_proposal_preview_shell.{
        chain_id = chain; program_trust = Octra_vm.Program_trust.empty; backend;
        ready_state_root_at = (fun _ -> failwith "preview read local history");
        ready_max_lag = 0; warn = (fun error -> failwith error)} in
      let execute = Node.Consensus_proposal_preview_shell.run preview_runtime in
      let captures = ref [] in
      let prepare request =
        captures := request :: !captures;
        Node.Consensus_proposal_preview_shell.prepare preview_runtime request in
      let errors = ref [] in
      let preview request =
        Lwt.map (fun result ->
          (match result with
           | Error error -> errors := error :: !errors
           | Ok value -> List.iter (fun row ->
               errors := row.X.reason :: !errors) value.X.artifacts.rejected);
          result) (execute request) in
      let unchanged () =
        expect "circle preview changed ledger" (Lwt_main.run (L.hash ledger) = root);
        expect "circle preview changed commit" (Lwt_main.run (S.get_commit_hash store) = commit) in
      let circle_tx method_ nonce = signed ~ou:19_000_000 caller ~op_type:T.CircleCall ~to_:circle ~nonce
        ~message:"[]" ~method_:(Some method_) in
      let bad = circle_tx "refuse" 1 in
      let good = circle_tx "accept" 1 in
      let calls count = List.init count (fun index ->
        signed ~ou:10_000 caller ~op_type:T.ProgramExec ~to_:(identity 9).address
          ~nonce:(index + 1) ~message:"[]" ~method_:(Some "absent")) in
      let transfer identity nonce = signed identity ~op_type:T.Standard
        ~to_:proposer.address ~nonce ~message:"" ~method_:None in
      let payment = transfer payer 1 in
      let dependent = transfer caller 2 in
      let head = string_of_int (epoch - 1) in
      let ready_message = `Assoc [
        "consensus_pubkey", `String (Base64.encode_exn member.public);
        "head_epoch", `String head;
        "head_proposal_id", `String (Octra_bootstrap.State_sync_checkpoint.raw_to_hex
          parent.certificate.proposal_id);
        "state_root", `String root; "chain_id", `String chain;
        "config_hash", `String config; "catchup_head_epoch", `String head;
      ] |> Yojson.Safe.to_string in
      let ready = signed member ~op_type:T.ValidatorReady ~to_:member.address ~nonce:1
        ~message:ready_message ~method_:None in
      let ordered () =
        let module Gate = Core.Preverify_commit in
        let module Receipt = Core.Preverify_receipt in
        let advance = circle_tx "advance" 2 in
        let inputs = [ready; advance; payment; good] in
        let budgets lane =
          let budget = Core.Resource_lanes.default_budget lane in
          if lane = Core.Resource_lanes.Circle_compute then
            {budget with max_txs = 2; max_proof = 2; max_ou = Z.of_int 40_000_000}
          else budget in
        let preverify = Gate.create ~budgets [] in
        let reward = Node.Consensus_reward_attribution.of_parent_commit parent |> unwrap in
        let prepare preverify txs =
          backend.prepare ~epoch_id:epoch ~proposal_id:"circle-order"
            ~expected_prev_root:(Some root) ~preverify ~parent_commit:(Some parent)
            ~reward ~env ~txs |> Lwt_main.run in
        let replay preverify txs =
          backend.run ~epoch_id:epoch ~proposal_id:"circle-replay"
            ~expected_prev_root:(Some root) ~preverify ~parent_commit:(Some parent)
            ~reward ~env ~txs |> Lwt_main.run in
        let prepared = prepare preverify inputs |> unwrap in
        unchanged ();
        let artifacts = prepared.execution.X.artifacts in
        expect ("ordered circle rejected: " ^ String.concat "; "
          (List.map (fun row -> row.X.reason) artifacts.rejected))
          (artifacts.rejected = []);
        expect "ordered circle lost ordinary or system work"
          (List.map fst artifacts.confirmed = T.consensus_order inputs);
        let fees = List.fold_left (fun fee tx -> Z.add fee tx.T.ou) Z.zero inputs in
        expect "ordered fees differ" (Z.equal artifacts.confirmed_fees fees);
        let receipt tx = Gate.receipt_for_tx prepared.preverify tx |> unwrap in
        let first = Option.get (receipt good).Receipt.circle in
        let second = Option.get (receipt advance).Receipt.circle in
        expect "circle preparation used one storage version"
          (first.stable_root <> second.stable_root);
        expect "circle preparation changed epoch binding"
          (first.snapshot_hash = W.state_hash root && first.snapshot_hash = second.snapshot_hash);
        expect "ordered receipts do not replay"
          (replay prepared.preverify inputs = Ok prepared.execution);
        unchanged ();
        expect "preparation depends on input order"
          (prepare preverify (T.consensus_order inputs)
           |> Result.map (fun result -> result.Node.Consensus_proposal_preview_shell.execution)
           = Ok prepared.execution);
        unchanged ();
        let old = match W.circle_receipt advance {second with stable_root = first.stable_root} with
          | W.Ready receipt -> receipt
          | W.Skip error | W.Defer error -> failwith error in
        let bad_gate = {prepared.preverify with Gate.receipts = List.map (fun receipt ->
          if receipt.Receipt.tx_hash = T.hash advance then old else receipt)
          prepared.preverify.receipts} in
        if G.proof_exec_at ~chain_id:chain ~epoch = G.Active then begin
          let refused = try Result.is_error (replay bad_gate inputs) with
            | Octra_vm.Direct_exec.Receipt_mismatch hash -> hash = T.hash advance in
          expect "receipt mismatch became a fee rejection" refused
        end else begin
        let refused = replay bad_gate inputs |> unwrap in
        expect "previous storage receipt confirmed"
          (List.map fst refused.artifacts.confirmed = T.consensus_order [ready; payment; good]);
        expect "previous storage receipt accepted"
          (match refused.artifacts.rejected with
           | [row] -> row.X.tx = advance && row.error_type = "circle_call_exception"
             && String.ends_with
               ~suffix:("(\"circle receipt mismatch for " ^ T.hash advance ^ "\")") row.reason
           | _ -> false);
        expect "previous storage receipt changed no result"
          (refused.post_state_root <> prepared.execution.post_state_root);
        end;
        unchanged ();
        expect "duplicate circle admitted" (Result.is_error (prepare preverify [good; good]));
        unchanged ();
        expect "circle budget ignored"
          (Result.is_error (prepare (Gate.create []) inputs));
        unchanged ();
        let no_standard lane =
          let budget = budgets lane in
          if lane = Core.Resource_lanes.Standard then {budget with max_txs = 0} else budget in
        expect "ordinary work budget ignored"
          (Result.is_error (prepare (Gate.create ~budgets:no_standard []) inputs));
        unchanged ();
        expect "vm allowance exceeds declared work budget"
          (prepare (Gate.create []) (calls 11)
           = Error "preverify_commit_gate:lane_budget:program:ou");
        unchanged ();
        expect "prepared receipt accepted as an unchecked input"
          (Result.is_error (prepare prepared.preverify inputs));
        unchanged ();
        let failed = prepare preverify [bad; payment; ready] |> unwrap in
        expect "circle refusal lost independent work"
          (List.map fst failed.execution.artifacts.confirmed = T.consensus_order [payment; ready]);
        expect "circle refusal missing" (List.map (fun row -> row.X.tx)
          failed.execution.artifacts.rejected = [bad]);
        expect "circle refusal differs from replay"
          (replay failed.preverify [bad; payment; ready] = Ok failed.execution);
        unchanged ();
        let fresh = circle_tx "fresh" 2 in
        let after_refusal = prepare preverify [fresh; bad; payment; ready] |> unwrap in
        expect "circle refusal kept reverted writes"
          (List.map fst after_refusal.execution.artifacts.confirmed
           = T.consensus_order [fresh; payment; ready]);
        expect "circle refusal changed the rejected input"
          (List.map (fun row -> row.X.tx) after_refusal.execution.artifacts.rejected = [bad]);
        expect "circle rollback differs from replay"
          (replay after_refusal.preverify [fresh; bad; payment; ready] = Ok after_refusal.execution);
        unchanged ();
        let missing_prefix = prepare preverify [advance; payment; ready] |> unwrap in
        expect "removed prefix retained its effects"
          (List.map fst missing_prefix.execution.artifacts.confirmed
           = T.consensus_order [payment; ready]);
        expect "removed prefix lost dependent rejection"
          (List.map (fun row -> row.X.tx) missing_prefix.execution.artifacts.rejected = [advance]);
        unchanged ();
        let retried = prepare preverify [payment; ready] |> unwrap in
        expect "ordinary retry differs from replay"
          (replay retried.preverify [payment; ready] = Ok retried.execution);
        unchanged ()
      in
      ordered ();
      let drops = ref [] in
      let head = Core.Head_manifest.{
        schema_version = 3; generation = 1; epoch_id = epoch - 1;
        state_root = root; ledger_state_root = Some root; irmin_commit = commit;
        txid_hi = 6L; txlog_seg = None; txlog_off = None; epochlog_off = None;
        commit_id = Option.get commit; ts = 1.; quorum_cert_hash = None;
        epoch_index_hash = None; epoch_index_root = None;
      } in
      let standard = Node.Consensus_driver_wiring.{
        chain_id = chain; duty_state = (fun _ -> Ok Core.Set_fold.empty);
        getenv = (fun _ -> None); get_meta = (fun _ -> None);
        wallet_addr = proposer.address; wallet_pub = proposer.public;
        find_account = L.find_opt ledger; cached_head = (fun () -> None);
        read_prev_ledger_root = (fun () -> Lwt.return_some root);
        next_txid = (fun () -> 7L);
        proposal_state = Node.Consensus_proposal_state.create ();
        catchup_active = ref false; staging_epoch_capacity = limits.C.max_ou;
        save_drops = (fun rows -> drops := rows @ !drops);
        write_pending = (fun _ -> failwith "preview wrote WAL");
        validator_pubkeys_for_epoch = (fun ~wallet_addr:_ ~wallet_pub:_ ~epoch:_ -> pubkeys);
      } in
      let adapters = Node.Consensus_driver_wiring.node_standard_adapters standard in
      let driver_batch ~active ~cached inputs expected =
        let module Driver = Octra_consensus.C_driver in
        let module Cache = Node.Consensus_bundle_cache in
        let module Role = Node.Consensus_preverify_role in
        Core.Tx_staging.clear ();
        List.iter (fun tx -> ignore (unwrap (Core.Tx_staging.add_smart
          ~lookup:(fun address -> Option.map (fun account ->
            account.L.balance, account.nonce) (L.find_opt ledger address)) tx))) inputs;
        let anchor = ref (G.Root root) in
        let plan = G.{anchor_epoch = epoch - 1; anchor_state_root = root;
          activation_epoch = if active then epoch else epoch + 1} in
        let reads = ref 0 in
        let mode = G.activation_mode ~root_at:(fun at ->
          expect "driver read wrong anchor" (at = plan.anchor_epoch);
          incr reads;
          !anchor) (Some plan) in
        let prepare_at = Node.Consensus_proposal_preview_shell.prepare_at ~mode preview_runtime in
        expect "driver accepted negative epoch" (Result.is_error (prepare_at (-1L)));
        expect "driver accepted overflowing epoch" (Result.is_error (prepare_at Int64.max_int));
        let bundles = Cache.create ~cap:8 in
        let saved = ref None in
        let store_bundle ~proposal_id ~tx_hashes ~txs ~receipts_json =
          saved := Some (txs, receipts_json);
          if cached then Cache.store_with_log bundles ~pid:proposal_id ~tx_hashes ~txs ~receipts_json in
        let runner state_root txs = checked ~state_root ~tx_hashes:(List.map T.hash txs) txs in
        let driver_ref = ref None in
        let standard = {standard with cached_head = (fun () -> Some head)} in
        let validator_set = CT.make_validator_set (List.map (fun (item : identity) ->
          CT.{address = item.address; pubkey = item.public}) validators) in
        let gates = Node.Consensus_driver_wiring.{
          consensus_mode = (fun () -> true); voting = (fun () -> true);
          state_attested = (fun () -> true); p2p_upgrade_ready = (fun () -> true);
          catchup_active = (fun () -> false); catchup_gap_active = (fun () -> false);
          pending_finalized = (fun () -> false); quarantine_active = (fun () -> false);
          quarantine_reason = (fun () -> ""); mark_quarantine = failwith;
          clear_quarantine = (fun _ -> ()); prev_root_streak = (fun () -> 0);
          set_prev_root_streak = (fun _ -> ()); state_root_streak = (fun () -> 0);
          set_state_root_streak = (fun _ -> ());
        } in
        let sign_fn = Mirage_crypto_ec.Ed25519.sign ~key:proposer.secret in
        let driver_read_deps = Node.Consensus_driver_read.{
          chain_id = chain; get_epoch_json = (fun _ -> None);
          epoch_time = (fun _ -> Some 1.); get_tx_by_txid = (fun _ -> None);
          read_receipts = (fun _ -> []); root_to_raw32 = root_raw;
          reward_source = (fun _ _ -> Error "not stored");
          read_finality = (fun _ -> None); head_epoch = (fun () -> Some (epoch - 1));
          lookup_bundle = Cache.peek_raw bundles;
        } in
        let config = Node.Consensus_driver_wiring.node_driver_config {
          prepare_at; standard; chain_id = chain; my_addr = proposer.address;
          sign_fn; validator_set; gates; proposal_limits = limits;
          read_local_root_raw = (fun () -> Lwt.return (root_raw root));
          read_local_ledger_root_raw = (fun () -> Lwt.return root);
          sleep = (fun _ -> Lwt.return_unit); quarantine_mismatch_threshold = 3;
          build_preverify = Role.build runner; validate_preverify = Role.validate runner;
          proposal_bundles = bundles; store_bundle; driver_ref;
          proposal_preview = execute; root_to_raw32 = root_raw;
          current_epoch = (fun () -> epoch); current_round = (fun () -> 3);
          committed_head_epoch = (fun () -> epoch - 1);
          load_parent_commit = (fun ~epoch_id:_ -> Ok (Some parent));
          verify_parent_commit = (fun ~epoch_id:_ -> function
            | Some value -> Core.Set_fold.read_parent ~chain_id:chain value
              |> Result.map (fun _ -> ())
            | None -> Error "missing parent");
          finality = Node.Consensus_finality_state.callbacks
            (Node.Consensus_finality_state.create ());
          cached_head = standard.cached_head; now = (fun () -> 99.); observer_mode = false;
          queue_catchup_target = (fun ~target_epoch:_ ~reason:_ -> failwith "unexpected catchup");
          run_catchup_to_target = (fun _ ~target_epoch:_ ~reason:_ -> failwith "unexpected catchup");
          apply_finalized = (fun ~validator_set:_ _ -> failwith "unexpected apply");
          replay_stashed_while_safe = (fun ~source:_ -> Lwt.return_unit);
          driver_read_deps; scheduled_validator_set_config = None;
          load_scheduled_validator_set_config = (fun () -> Lwt.return_none);
        } in
        let swarm = Octra_net.P2p_swarm.create {
          listen_port = 0; chain_id = chain;
          node_id = Octra_net.P2p_handshake.node_id_of_pubkey proposer.public;
          node_addr = proposer.address; pubkey_raw = proposer.public;
          consensus_config_hash = root_raw root; binary_hash = root_raw root;
          require_binary_hash = false; upgrade_plan = None; profile_plan = [];
          allowed_pubkeys = []; bootstrap_peers = []; max_peers = 1; sign_fn;
          best_epoch_fn = (fun () -> Int64.of_int (epoch - 1));
          best_root_fn = (fun () -> root_raw root);
        } in
        let driver = Driver.create ~config ~validator_set ~swarm ~start_height:(Int64.of_int epoch)
          ~sync_log:(Octra_consensus.C_sync_log.memory ())
          ~relief_log:(Octra_consensus.C_relief_log.memory ())
          ~vote_log:(Octra_consensus.C_vote_log.memory ()) in
        driver.running <- true;
        driver_ref := Some driver;
        Fun.protect ~finally:(fun () ->
          driver.running <- false;
          Core.Tx_staging.clear ()) (fun () ->
          let proposal = config.make_proposal (Int64.of_int epoch) |> Lwt_main.run
            |> function Some value -> value | None -> failwith "driver proposal unavailable" in
          expect "driver selection differs" (proposal.tx_hashes = List.map T.hash expected);
          let wire = CT.{chain_id = chain; epoch_id = Int64.of_int epoch; round = 3;
            header = proposal.header; tx_hashes = proposal.tx_hashes; parent_commit = Some parent;
            valid_round = None; proposer = proposer.address; signature = ""} in
          expect "driver refused its proposal"
            (config.verify_proposal wire |> Lwt_main.run = Driver.Proposal_accept);
          let txs, receipts = Option.get !saved in
          let admitted = Core.Tx_outcome.split_admit receipts |> unwrap in
          let preverify = Core.Preverify_commit.receipts_of_strings admitted.preverify
            |> unwrap |> Core.Preverify_commit.create in
          let result = execute C.{epoch_id = wire.epoch_id; epoch_ts = wire.header.ts;
            proposal_id = H.proposal_id wire.header; expected_prev_root = root;
            prev_state_root = root; parent_commit = Some parent; proposer = proposer.address;
            validator_pubkeys = pubkeys; preverify; txs} |> Lwt_main.run |> unwrap in
          let replayed = C.preview_decision ~root_to_raw32:root_raw ~epoch_id:wire.epoch_id
            ~tx_hashes:proposal.tx_hashes ~tx_count:result.artifacts.tx_count
            ~start_txid:7L ~prev_eic_root:(C.prev_eic_root_from_head None)
            ~local_ledger_root:root ~proposed_state_root:proposal.header.proposed_state_root
            ~preview:(C.preview_status_of_result (Ok result)) in
          expect "driver replay root differs"
            (match replayed with C.Preview_accept _ -> true | _ -> false);
          unchanged ();
          expect "driver anchor read differs" (if active then !reads > 0 else !reads = 0);
          if active then List.iter (fun fault ->
            anchor := fault;
            expect "driver used cached proposal without anchor"
              (config.make_proposal wire.epoch_id |> Lwt_main.run = None);
            expect "driver verified without anchor"
              (config.verify_proposal wire |> Lwt_main.run = Driver.Proposal_wait);
            unchanged ()) [G.Missing; G.Root "different"; G.Unreadable "read failure"])
      in
      driver_batch ~active:false ~cached:true [good; payment; ready] [good];
      driver_batch ~active:true ~cached:true [good; payment; ready]
        (T.consensus_order [good; payment; ready]);
      driver_batch ~active:true ~cached:false [good; payment; ready]
        (T.consensus_order [good; payment; ready]);
      let check_work () =
        expect "proposal exceeded preview credits" (List.length !captures <= 6)
      in
      let run ?next ?fault ?(live = false) ?(starved = []) ?(after_refusal = false)
          ?(after_preview = false) ?(held = []) ?(resume = false) ?(cut = false)
          ?(busy = false) ?(ordered = false) ?(cached = true) ?(rejects = 0)
          ?(removed = []) ?(drain = false)
          ?(history = []) ?(missing = false) ?(excluded = []) txs expected =
        let adapters = if live then Node.Consensus_driver_wiring.node_standard_adapters
          {standard with cached_head = (fun () -> Some head)} else adapters in
        let prepare = if ordered then Some prepare else None in
        let parent = if history = [] then parent
          else make_parent ~txs:history (Int64.of_int (epoch - 1)) validators in
        Core.Tx_staging.clear ();
        if live then ignore (unwrap (Core.Tx_staging.Preview.send
          (Check (Some (Int64.of_int head.epoch_id), ""))));
        drops := [];
        List.iter (fun tx ->
          ignore (unwrap (Core.Tx_staging.add_smart
            ~lookup:(fun address -> Option.map (fun account ->
              account.L.balance, account.nonce) (L.find_opt ledger address)) tx))) txs;
        batches := [];
        captures := [];
        errors := [];
        let bundles = ref None in
        let frozen = ref None in
        let deps = make_deps ~epoch ~staging:txs ~root in
        let deps = C.{deps with
          hold_preview = (fun ~epoch tx ->
            expect "unexpected preview hold" (List.mem tx held);
            if live then adapters.hold_preview ~epoch tx);
          evict_preview = (fun ?epoch tx ->
            adapters.evict_preview ?epoch:(if live then epoch else None) tx);
          staging_txs = (match next with
            | None -> (fun ?(circles = true) () ->
              if circles then txs else Node.Circle_refill.without txs)
            | Some _ -> adapters.staging_epoch_txs);
          parent_commit = (fun ~epoch_id:_ -> Ok (Some parent));
          parent_txs = (fun pid ->
            expect "proposal used another parent bundle" (pid = parent.certificate.proposal_id);
            if missing then None else Some history);
          proposer = (fun () -> proposer.address); validator_pubkeys = (fun _ -> pubkeys);
          build_preverify_once = (fun ~state_root ~tx_hashes inputs ->
            expect "exclusive circle reached preverify"
              (not (List.exists (fun tx -> List.mem tx excluded) inputs));
            checked ~state_root ~tx_hashes inputs);
          preview;
          frozen_bundle = (fun _ -> !frozen);
          freeze = (fun _ bundle -> frozen := Some bundle);
          store_bundle = (fun ~proposal_id:_ ~tx_hashes ~txs ~receipts_json ->
            bundles := Some (tx_hashes, txs, receipts_json));
        } in
        let unavailable = ref true in
        let checking = ref true in
        let build = Option.map (fun prepare request ->
          if !checking then expect "retry retained rejected nonce successors"
            (List.for_all (fun tx -> List.for_all (fun row ->
              tx.T.from <> row.Core.Tx_staging.d_from || tx.nonce < row.d_nonce) !drops)
              request.C.txs);
          match List.find_opt (fun tx -> !unavailable && List.mem tx starved
            && (not after_preview || !captures <> [])
            && (not after_refusal || !drops <> [])) request.C.txs with
          | Some tx ->
            captures := request :: !captures;
            Lwt.fail (Core.Exec_resource.Exhausted (T.hash tx, Memory))
          | None ->
            Lwt.map (Result.map (fun result ->
              match fault with
              | None -> result
              | Some error_type ->
                let execution = result.Node.Consensus_proposal_preview_shell.execution in
                let artifacts = execution.X.artifacts in
                let rejected = List.map (fun row ->
                  if row.X.tx = bad then {row with X.error_type} else row) artifacts.rejected in
                {result with execution = {execution with artifacts = {artifacts with rejected}}}))
              (prepare request)) prepare in
        let make ?(occupied = busy) deps =
          let action () = C.make_proposal ?prepare:build deps ~chain_id:chain ~root_to_raw32:root_raw
            ~limits ~epoch_id:(Int64.of_int epoch) |> Lwt_main.run in
          if occupied then with_host_busy action else action () in
        let proposal = match make deps with
          | Some value -> value | None -> failwith "circle proposal unavailable" in
        checking := false;
        if ordered then check_work ();
        unchanged ();
        expect ("circle selection mismatch: " ^ String.concat "; " !errors)
          (proposal.tx_hashes = List.map T.hash expected);
        if not ordered && expected <> [] && List.exists (fun tx -> T.hash tx = T.hash bad) txs then begin
          expect "failed circle remains staged"
            (Core.Tx_staging.find_by_hash (T.hash bad) = None);
          expect "preview drop was not recorded locally"
            (List.map (fun row -> row.Core.Tx_staging.d_hash) !drops = [T.hash bad]);
          expect "preview drop falsely reports chain rejection"
            (match Core.Tx_staging.lookup_dropped (T.hash bad) with
             | Some ("evicted", "preview rejected", _, _, _, _, _, _) -> true
             | _ -> false)
        end else expect "unexpected queue eviction"
          (List.sort String.compare (List.map (fun row -> row.Core.Tx_staging.d_hash) !drops)
           = List.sort String.compare (List.map T.hash removed));
        let hashes, confirmed, receipts = Option.get !bundles in
        expect "cached hash list differs" (hashes = proposal.tx_hashes);
        let prepared = Core.Tx_outcome.split_admit receipts |> unwrap in
        if not ordered && expected <> [] then
          expect "refill carried circle rejection" (prepared.rejections = []);
        let reads = List.length !batches in
        let preparations = List.length !captures in
        expect "frozen proposal changed"
          (make {deps with staging_txs = (fun ?circles:_ () -> [payment; ready; good])} = Some proposal);
        expect "frozen proposal reran checks" (List.length !batches = reads);
        expect "frozen proposal reran execution" (List.length !captures = preparations);
        let build_deps = deps in
        let deps = verify_deps ~staging:txs ~root:(root_raw root) ~ledger_root:root in
        let deps = C.{deps with
          root_to_raw32 = root_raw; validator_pubkeys = (fun _ -> pubkeys);
          validate_preverify_once = checked; preview;
          verify_address_pubkey = (fun ~addr ~pubkey ->
            Core.Crypto.Address.address_from_pubkey pubkey = addr);
          verify_tx_signature = (fun tx ~pubkey -> T.verify tx pubkey);
          verify_parent_commit = (fun ~epoch_id:_ -> function
            | Some value -> Core.Set_fold.read_parent ~chain_id:chain value
              |> Result.map (fun _ -> ())
            | None -> Error "missing parent");
          cached_bundle = (fun ~proposal_id:_ ->
            if not cached then None else Some Node.Consensus_bundle_fetch.{
              txs = confirmed; receipts_json = receipts; rejections = prepared.rejections});
        } in
        let wire = CT.{chain_id = chain; epoch_id = Int64.of_int epoch; round = 3;
          header = proposal.header; tx_hashes = proposal.tx_hashes; parent_commit = Some parent;
          valid_round = None; proposer = proposer.address; signature = ""} in
        let verdict = C.verify_proposal ?prepare deps ~chain_id:chain wire |> Lwt_main.run in
        expect "validator refused circle selection"
          (verdict = Octra_consensus.C_driver.Proposal_accept);
        if ordered then expect "deferred work became a chain rejection"
          (List.length prepared.rejections = rejects);
        if live then List.iter (fun tx ->
          let result = Core.Tx_staging.Preview.send
            (Check (Some (Int64.of_int head.epoch_id), T.hash tx)) in
          expect "live refusal differs from stable witness"
            (result = if List.mem tx (removed @ held) then Error "preview refused in this epoch" else Ok ())) txs;
        if held <> [] then begin
          captures := [];
          frozen := None;
          let again = make {build_deps with current_round = (fun () -> 4);
            staging_txs = adapters.staging_epoch_txs} in
          expect "held sender blocked independent work"
            (match again with
             | Some value -> List.for_all (fun tx -> List.mem (T.hash tx) value.tx_hashes) expected
             | None -> false);
          expect "held sender executed twice"
            (List.for_all (fun request -> List.for_all (fun tx ->
              List.for_all (fun item -> tx.T.from <> item.T.from || tx.nonce < item.nonce) held)
              request.C.txs) !captures);
          expect "hold deleted pending work"
            (List.for_all (fun tx -> Core.Tx_staging.find_by_hash (T.hash tx) = Some tx) held);
          check_work ()
        end;
        unchanged ();
        if ordered then begin
          expect "ordered execution changed its context" (List.for_all (fun request ->
            request.C.epoch_id = wire.epoch_id && request.epoch_ts = wire.header.ts
            && request.proposer = proposer.address && request.parent_commit = Some parent
            && request.validator_pubkeys = pubkeys
            && (request.expected_prev_root = root || request.expected_prev_root = root_raw root)) !captures);
          if prepared.rejections <> [] then begin
            let saw inputs = List.exists (fun request ->
              T.consensus_order request.C.txs = T.consensus_order inputs) !captures in
            let inputs = confirmed @ List.map (fun row ->
              row.Core.Tx_outcome.tx) prepared.rejections in
            expect "rejection inputs were not prepared" (saw inputs);
            expect "remaining inputs were not prepared" (saw expected)
          end;
          List.iter (fun tx ->
            expect "ordered selection changed pending work"
              (Core.Tx_staging.find_by_hash (T.hash tx)
               = if List.mem tx removed then None else Some tx)) txs;
          if List.exists (fun tx -> tx.T.to_ = wasm) confirmed then begin
            expect "busy host rejected a valid proposal"
              (with_host_busy (fun () -> C.verify_proposal ?prepare deps ~chain_id:chain wire
                 |> Lwt_main.run) = Octra_consensus.C_driver.Proposal_wait);
            unchanged ();
            expect "released host retained proposal refusal"
              (C.verify_proposal ?prepare deps ~chain_id:chain wire |> Lwt_main.run
               = Octra_consensus.C_driver.Proposal_accept);
            unchanged ()
          end;
          if busy then begin
            frozen := None;
            let retried = make ~occupied:false
              {build_deps with current_round = (fun () -> 4)} in
            expect "released host lost pending work"
              (Option.map (fun (value : Octra_consensus.C_driver.proposal_plan) -> value.tx_hashes)
                retried = Some (List.map T.hash (T.consensus_order txs)));
            unchanged ()
          end;
          let unavailable _ = Lwt.fail (Core.Exec_resource.Exhausted (T.hash good, Memory)) in
          expect "ordered resource failure rejected a proposal"
            (C.verify_proposal ~prepare:unavailable deps ~chain_id:chain wire |> Lwt_main.run
             = Octra_consensus.C_driver.Proposal_wait);
          unchanged ();
          let delayed ~state_root:_ ~tx_hashes:_ inputs =
            Lwt.return (W.defer "worker unavailable" inputs) in
          expect "dependency changed unavailable work to invalid"
            (C.verify_proposal ?prepare {deps with validate_preverify_once = delayed}
               ~chain_id:chain wire |> Lwt_main.run
             = if confirmed = [] then Octra_consensus.C_driver.Proposal_accept
               else Octra_consensus.C_driver.Proposal_wait);
          unchanged ();
          if prepared.rejections <> [] then begin
            let outcomes request =
              if List.exists (fun tx -> not (List.mem tx confirmed)) request.C.txs then
                unavailable request
              else (Option.get prepare) request in
            expect "outcome resource failure rejected a proposal"
              (C.verify_proposal ~prepare:outcomes deps ~chain_id:chain wire |> Lwt_main.run
               = Octra_consensus.C_driver.Proposal_wait);
            unchanged ()
          end;
          if List.exists (fun tx -> tx.T.encrypted_data = Some "clock") confirmed then begin
            let later = wire.header.ts +. 1. in
            let changed = {wire with header = {wire.header with ts = later}} in
            let verdict = C.verify_proposal ?prepare {deps with now = (fun () -> later)}
              ~chain_id:chain changed |> Lwt_main.run in
            expect "timestamp check skipped execution"
              (List.exists (fun request -> request.C.epoch_ts = later) !captures);
            expect "timestamp reused the earlier result" (verdict = Octra_consensus.C_driver.Proposal_reject);
            unchanged ()
          end
        end;
        Option.iter (fun tx ->
          expect "next circle was not retained"
            (Core.Tx_staging.find_by_hash (T.hash tx) = Some tx);
          frozen := None;
          let again = make {build_deps with current_round = (fun () -> 4)} in
          expect "failed circle blocked next selection"
            (Option.map (fun (value : Octra_consensus.C_driver.proposal_plan) -> value.tx_hashes) again
             = Some [T.hash tx]);
          expect "next selection repeated drop" (List.length !drops = 1);
          unchanged ()) next;
        if cut then begin
          let completed = !captures in
          frozen := None;
          captures := [];
          bundles := None;
          let stopped = {build_deps with current = (fun () -> List.length !captures < 2)} in
          expect "reselection ignored round cancellation" (make stopped = None);
          expect "cancelled reselection published a bundle" (!bundles = None);
          unchanged ();
          captures := completed
        end;
        if drain then begin
          let rec rounds index =
            if index > 3 then failwith "rejected queue did not drain";
            let prior = List.map (fun row -> row.Core.Tx_staging.d_hash) !drops in
            captures := [];
            frozen := None;
            let next = make {build_deps with current_round = (fun () -> 3 + index);
              staging_txs = adapters.staging_epoch_txs} in
            check_work ();
            expect "next round repeated a rejected call"
              (List.for_all (fun request -> List.for_all (fun tx ->
                not (List.mem (T.hash tx) prior)) request.C.txs) !captures);
            expect "next round failed to propose" (Option.is_some next);
            unchanged ();
            if List.length !drops <> List.length prior then rounds (index + 1)
          in
          rounds 1
        end;
        if resume then begin
          List.iter (fun round ->
            frozen := None;
            captures := [];
            let proposal = make {build_deps with current_round = (fun () -> round);
              staging_txs = adapters.staging_epoch_txs} in
            expect "technical failures prevented ordinary proposal"
              (Option.map (fun (value : Octra_consensus.C_driver.proposal_plan) -> value.tx_hashes)
                proposal = Some (List.map T.hash expected));
            check_work ();
            expect "technical failures removed pending work" (!drops = []);
            unchanged ()) [4; 5];
          unavailable := false;
          frozen := None;
          let proposal = make {build_deps with current_round = (fun () -> 6);
            staging_txs = adapters.staging_epoch_txs} in
          expect "recovered calls stayed excluded"
            (Option.map (fun (value : Octra_consensus.C_driver.proposal_plan) -> value.tx_hashes)
              proposal = Some (List.map T.hash (T.consensus_order txs)));
          unchanged ()
        end;
        Core.Tx_staging.clear ()
      in
      run [payment; ready] [payment; ready];
      run [good; payment; ready] [good];
      run ~history:[good] [good; payment; dependent] [payment];
      run ~history:[good] [good; dependent] [];
      run ~history:[payment] [good; payment] [good];
      run ~history:[payment] ~missing:true [good; payment] [payment];
      run [bad] [];
      run [bad; payment; ready; dependent] [payment; ready];
      run [bad; circle_tx "refuse" 2; payment; ready] [payment; ready];
      let next = signed ~ou:16_000_000 other ~op_type:T.CircleCall ~to_:circle ~nonce:1
        ~message:"[]" ~method_:(Some "accept") in
      run ~next [bad; next; transfer other 2; payment; ready] [payment; ready];
      run ~ordered:true [ready; payment; good] (T.consensus_order [ready; payment; good]);
      let host_call = signed ~ou:19_000_000 caller ~op_type:T.CircleCall ~to_:wasm ~nonce:1
        ~message:"[]" ~method_:(Some "fhe_verify_zero") in
      run ~ordered:true [host_call; payment; ready]
        (T.consensus_order [host_call; payment; ready]);
      run ~ordered:true ~cached:false [host_call; payment; ready]
        (T.consensus_order [host_call; payment; ready]);
      run ~ordered:true ~busy:true [host_call; dependent; payment; ready]
        (T.consensus_order [payment; ready]);
      let first = transfer caller 1 in
      let second = signed ~ou:19_000_000 caller ~op_type:T.CircleCall ~to_:wasm ~nonce:2
        ~message:"[]" ~method_:(Some "fhe_verify_zero") in
      run ~ordered:true ~busy:true [first; second; transfer caller 3; payment; ready]
        (T.consensus_order [first; payment; ready]);
      List.iter (fun op_type ->
        let cell = signed other ~op_type ~to_:circle ~nonce:1
          ~message:"{}" ~method_:None in
        run ~ordered:true [cell; transfer other 2; good; payment; ready]
          (T.consensus_order [good; payment; ready]);
        run ~ordered:true [payment; ready; cell; transfer other 2; good]
          (T.consensus_order [good; payment; ready]);
        run ~ordered:true ~history:[good] ~excluded:[cell]
          [cell; transfer other 2; good; payment] [payment])
        [T.CircleBalanceCellPut; T.CircleRegisterCellPut];
      let first = transfer caller 1 in
      let second = circle_tx "accept" 2 in
      run ~ordered:true [first; second; payment; ready]
        (T.consensus_order [first; second; payment; ready]);
      run ~ordered:true ~history:[good] [payment; good]
        (T.consensus_order [payment; good]);
      let denied = signed caller ~op_type:T.ProgramExec ~to_:program ~nonce:1
        ~message:"[]" ~method_:(Some "absent") in
      let set = signed caller ~op_type:T.ProgramExec ~to_:program ~nonce:2
        ~message:"[]" ~method_:(Some "accept") in
      let read = signed other ~op_type:T.ProgramExec ~to_:program ~nonce:1
        ~message:"[]" ~method_:(Some "fresh") in
      let later = signed caller ~op_type:T.ProgramExec ~to_:program ~nonce:3
        ~message:"[]" ~method_:(Some "absent") in
      let independent = signed other ~op_type:T.ProgramExec ~to_:program ~nonce:1
        ~message:"[]" ~method_:(Some "accept") in
      run ~ordered:true ~rejects:1 ~removed:[denied]
        [denied; dependent; later; independent; payment; ready]
        (T.consensus_order [independent; payment; ready]);
      let independent = signed other ~op_type:T.CircleCall ~to_:circle ~nonce:1
        ~message:"[]" ~method_:(Some "accept") in
      run ~ordered:true ~rejects:1 ~removed:[denied]
        [denied; dependent; later; independent; payment; ready]
        (T.consensus_order [independent; payment; ready]);
      run ~ordered:true ~rejects:1 ~removed:[denied] [denied; dependent; later; payment; ready]
        (T.consensus_order [payment; ready]);
      run ~ordered:true ~rejects:1 ~removed:[denied]
        [denied; set; read; payment; ready] (T.consensus_order [read; payment; ready]);
      run ~ordered:true ~rejects:1 ~removed:[bad] [bad] [];
      run ~ordered:true ~rejects:1 ~removed:[bad] [bad; payment; ready] (T.consensus_order [payment; ready]);
      run ~ordered:true ~rejects:1 ~removed:(List.filter (fun tx -> tx.T.nonce = 1) (calls 11)) (calls 11 @ [payment; ready])
        (T.consensus_order [payment; ready]);
      run ~ordered:true ~starved:[good] [good; dependent; payment; ready]
        (T.consensus_order [payment; ready]);
      let blocked = List.init 4 (fun index ->
        signed ~ou:1_000_000 (List.nth identities index) ~op_type:T.ProgramExec
          ~to_:program ~nonce:1 ~message:"[]" ~method_:(Some "accept")) in
      run ~ordered:true ~starved:blocked ~resume:true
        (blocked @ [dependent; payment; ready]) (T.consensus_order [payment; ready]);
      let independent = signed other ~op_type:T.ProgramExec ~to_:program ~nonce:1
        ~message:"[]" ~method_:(Some "accept") in
      run ~ordered:true ~starved:blocked
        (blocked @ [dependent; independent; payment; ready])
        (T.consensus_order [independent; payment; ready]);
      let cell = signed other ~op_type:T.CircleBalanceCellPut ~to_:circle ~nonce:1
        ~message:"{}" ~method_:None in
      let reserve_parent = make_parent ~txs:[good] (Int64.of_int (epoch - 1)) validators in
      let reserve_message = match Yojson.Safe.from_string ready_message with
        | `Assoc fields -> `Assoc (List.map (fun (name, value) ->
            if name = "head_proposal_id" then name,
              `String (Octra_bootstrap.State_sync_checkpoint.raw_to_hex
                reserve_parent.certificate.proposal_id)
            else name, value) fields) |> Yojson.Safe.to_string
        | _ -> failwith "ready payload invalid" in
      let reserve_ready = signed member ~op_type:T.ValidatorReady ~to_:member.address
        ~nonce:1 ~message:reserve_message ~method_:None in
      run ~ordered:true ~starved:blocked ~history:[good] ~excluded:[cell]
        (blocked @ [cell; transfer other 2; dependent; payment; reserve_ready])
        (T.consensus_order [payment; reserve_ready]);
      let independent = signed other ~op_type:T.CircleCall ~to_:circle ~nonce:1
        ~message:"[]" ~method_:(Some "accept") in
      run ~ordered:true ~starved:blocked
        (blocked @ [dependent; independent; payment; ready])
        (T.consensus_order [independent; payment; ready]);
      run ~ordered:true [good; next; transfer other 2; payment; ready]
        (T.consensus_order [good; payment; ready]);
      let clock = circle_tx "clock" 1 in
      run ~ordered:true ~cached:false [clock; payment; ready]
        (T.consensus_order [clock; payment; ready]);
      let absent = signed caller ~op_type:T.ProgramExec ~to_:(identity 9).address ~nonce:1
        ~message:"[]" ~method_:(Some "absent") in
      let watch = signed ~ou:19_000_000 other ~op_type:T.CircleCall ~to_:circle ~nonce:1
        ~message:(Yojson.Safe.to_string (`List [`String caller.address])) ~method_:(Some "watch") in
      run ~ordered:true ~rejects:1 ~removed:[absent] [absent; watch; payment; ready]
        (T.consensus_order [watch; payment; ready]);
      let large = signed ~ou:10_000_000 caller ~op_type:T.ProgramExec
        ~to_:(identity 9).address ~nonce:1 ~message:"[]" ~method_:(Some "absent") in
      let replacement = signed other ~op_type:T.ProgramExec
        ~to_:program ~nonce:1 ~message:"[]" ~method_:(Some "accept") in
      run ~ordered:true ~after_refusal:true ~starved:[replacement] ~removed:[large]
        [large; replacement; payment; ready] (T.consensus_order [payment; ready]);
      run ~ordered:true ~removed:[large] [large; replacement; payment; ready]
        (T.consensus_order [replacement; payment; ready]);
      expect "rejected work retained lane reservation"
        (List.exists (fun request -> List.mem replacement request.C.txs) !captures);
      let rejected op_type to_ method_ ou = List.init 5 (fun index ->
        signed ~ou (List.nth identities index) ~op_type ~to_ ~nonce:1
          ~message:"[]" ~method_:(Some method_)) in
      let refused = rejected T.ProgramExec (identity 9).address "absent" 10_000_000 in
      run ~ordered:true ~cut:true ~drain:true ~rejects:1 ~removed:(List.filteri (fun i _ -> i < 2) refused)
        (refused @ [replacement; payment; ready]) (T.consensus_order [payment; ready]);
      let alternate = signed ~ou:19_000_000 other ~op_type:T.CircleCall ~to_:circle
        ~nonce:1 ~message:"[]" ~method_:(Some "accept") in
      let refused = rejected T.CircleCall circle "exhaust" 19_000_000 in
      run ~ordered:true ~drain:true ~rejects:1 ~removed:(List.filteri (fun i _ -> i < 2) refused)
        (refused @ [alternate; payment; ready]) (T.consensus_order [payment; ready]);
      run ~ordered:true ~rejects:1 ~removed:[bad] [bad; alternate; dependent; payment; ready]
        (T.consensus_order [payment; ready]);
      List.iter (fun fault ->
        run ~ordered:true ~fault [bad; dependent; payment; ready]
          (T.consensus_order [payment; ready]))
        ["vm_transition_incomplete"; "vm_transition_exception"];
      run ~ordered:true ~live:true ~after_refusal:true ~starved:[replacement] ~removed:[large]
        [large; replacement; payment; ready] (T.consensus_order [payment; ready]);
      if epoch >= 1_663_000 then begin
        let blocked = List.init 3 (fun index ->
          signed (List.nth identities (index + 1)) ~op_type:T.ProgramExec
            ~to_:program ~nonce:1 ~message:"[]" ~method_:(Some "accept")) in
        run ~ordered:true ~live:true ~after_preview:true ~starved:blocked ~held:[denied]
          ([denied; dependent; later; replacement; payment] @ blocked)
          (T.consensus_order [replacement; payment])
      end))

let run ~make_deps ~verify_deps ~limits =
  test_work ();
  test_select ();
  test_drop ();
  test_turn ();
  let settings = [Core.Validator_policy.env_name, "100";
    "OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1"] in
  let before = List.map (fun (name, _) ->
    name, Option.value (Sys.getenv_opt name) ~default:"") settings in
  Fun.protect ~finally:(fun () -> List.iter (fun (name, value) ->
    Unix.putenv name value) before) (fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) settings;
    List.iter (run_epoch ~make_deps ~verify_deps ~limits)
      [1_614_499; 1_614_500; 1_662_999; 1_663_000; 1_663_001])