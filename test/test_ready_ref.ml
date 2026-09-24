(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module X = Octra_core.Epoch_exec
module R = Octra_core.Rule_graph
module O = Octra_core.Tx_outcome
module P = Octra_node_runtime.Consensus_proposal
module F = Octra_core.Set_fold
module C = Octra_consensus.C_types
module V = Octra_core.Validator_ready_policy

let expect name value = if not value then failwith name
let chain_id = "octra-devnet-9871-cluster"
let root = String.make 64 'a'
let epoch = 1_567_000
let head = Int64.of_int (epoch - 1)
let proposal = String.make 32 'p'
let proposal_id = String.concat "" (List.init 32 (fun _ -> "70"))

let parent = C.{
  validator_set = make_validator_set [];
  certificate = {
    chain_id; epoch_id = head; commit_round = 0; proposal_id = proposal;
    precommits = [];
    header = {
      proto_version = proto_version_current; chain_id; epoch_id = head;
      prev_state_root = String.make 32 'a'; tx_list_hash = String.make 32 't';
      receipt_root = String.make 32 'r'; proposed_state_root = String.make 32 'a';
      parent_commit_hash = String.make 32 'c'; creator_addr = "proposer";
      txid_hi = 0L; ts = 1.;
    };
  };
}

let state = List.fold_left (fun state epoch ->
  F.note_final F.participating ~at:(Int64.succ epoch) ~active:[] ~signers:[]
    ~final:F.{epoch; proposal_id = proposal; set_hash = String.make 32 's'} state
  |> Result.get_ok) F.empty [Int64.sub head 2L; Int64.pred head]

let env lag lookup = X.{
  chain_id;
  epoch_id = epoch;
  proposer_addr = "oct_proposer";
  validator_addrs = [];
  validator_pubkeys = [];
  prev_state_root = root;
  epoch_ts = 1.;
  ready_state_root_at = lookup;
  ready_max_lag = lag;
}

let verify ?(parent = Some parent) ?(state = state) ?(id = Some proposal_id)
    mode env head_epoch state_root =
  Lwt_main.run (X.validate_validator_ready_reference ~mode ~env ~head_epoch
    ~state_root ~parent ~state ~proposal_id:id)

let test_reference () =
  let calls = ref 0 in
  let reads = [None; Some (fun _ -> incr calls; Lwt.return_none);
    Some (fun _ -> incr calls; Lwt.return_some "wrong");
    Some (fun _ -> incr calls; failwith "unexpected history read")] in
  List.iter (fun lag -> List.iter (fun lookup ->
    let env = env lag lookup in
    List.iter (fun lag ->
      let submitted = Int64.sub head lag in
      expect "valid reference rejected" (verify R.Active env submitted root = Ok ());
      expect "wrong proposal accepted" (Result.is_error
        (verify ~id:(Some (String.make 64 'a')) R.Active env submitted root));
      expect "missing proposal accepted" (Result.is_error
        (verify ~id:None R.Active env submitted root));
      expect "root used as proposal" (verify R.Active env submitted "informational" = Ok ()))
      [0L; 1L; 2L];
    List.iter (fun submitted ->
      expect "wrong head accepted"
        (verify R.Active env submitted root = Error "head_epoch outside delivery window"))
      [Int64.min_int; -1L; 0L; Int64.sub head 3L; Int64.succ head; Int64.max_int];
    expect "missing parent accepted" (Result.is_error (verify ~parent:None R.Active env head root));
    expect "current head used history"
      (verify ~state:F.empty R.Active env head root = Ok ());
    expect "missing history accepted"
      (Result.is_error (verify ~state:F.empty R.Active env (Int64.pred head) root));
    let wrong = {parent with certificate = {parent.certificate with epoch_id = Int64.pred head}} in
    expect "wrong parent epoch accepted"
      (Result.is_error (verify ~parent:(Some wrong) R.Active env head root));
    expect "nonpositive epoch accepted"
      (Result.is_error (verify R.Active {env with epoch_id = 0} (-1L) root))) reads)
    [min_int; -1; 0; 1; 64; max_int];
  expect "local history was consulted" (!calls = 0);
  List.iter (fun epoch -> List.iter (fun head ->
    if head >= 0L && head < epoch then
      expect "pool and execution ages differ"
        (V.delivery ~epoch ~head = not (V.expired ~head:(Int64.pred epoch) ~reference:head)))
    [0L; 1L; 2L; Int64.pred epoch; Int64.max_int]) [1L; 2L; 4L; Int64.max_int]

let test_prior () =
  let known = Some (fun _ -> Lwt.return_some root) in
  expect "prior lag changed"
    (verify R.Prior (env 0 known) (Int64.pred head) root = Error "head_epoch too stale");
  expect "prior history changed"
    (verify R.Prior (env 64 None) (Int64.pred head) root = Error "state_root reference unavailable");
  expect "prior reference changed"
    (verify R.Prior (env 64 known) (Int64.pred head) root = Ok ());
  expect "prior future changed"
    (verify R.Prior (env 64 known) (Int64.succ head) root = Error "head_epoch is in the future")

let test_activation () =
  let plan = Option.get (R.ready_exec_activation_for_chain chain_id) in
  expect "activation epoch differs" (plan.activation_epoch = epoch);
  let graph read = R.create ~chain_id ~root_at:read in
  let missing = graph (fun _ -> R.Missing) in
  expect "early activation" (R.ready_exec missing ~epoch:(epoch - 1) = Ok R.Prior);
  expect "early profile" (R.ready_exec_at ~chain_id ~epoch:(epoch - 1) = R.Prior);
  List.iter (fun epoch ->
    List.iter (fun read ->
      let rules = graph (fun _ -> read) in
      expect "anchor accepted" (Result.is_error (R.ready_exec rules ~epoch)))
      [R.Missing; R.Root "wrong"; R.Unreadable "read"];
    let rules = graph (fun key ->
      expect "anchor epoch differs" (key = plan.anchor_epoch);
      R.Root plan.anchor_state_root) in
    expect "active mode absent" (R.ready_exec rules ~epoch = Ok R.Active);
    expect "active profile absent" (R.ready_exec_at ~chain_id ~epoch = R.Active))
    [epoch; epoch + 1; max_int];
  let rules = graph (fun key ->
    match R.root_after_floor ~chain_id ~floor_epoch:epoch ~epoch:key with
    | None -> R.Missing | Some root -> R.Root root) in
  expect "snapshot mode differs" (R.ready_exec rules ~epoch = Ok R.Active);
  let policy = Octra_node_runtime.Set_rule.policy rules epoch in
  expect "execution mode missing"
    (match policy with Ok value -> value.ready_exec_mode = R.Active | Error _ -> false);
  expect "program execution mode missing"
    (match policy with Ok value -> value.program_mode = R.Active | Error _ -> false);
  expect "program execution selected early"
    (match Octra_node_runtime.Set_rule.policy rules (epoch - 1) with
     | Ok value -> value.program_mode = R.Prior | Error _ -> false);
  List.iter (fun chain_id ->
    let rules = R.create ~chain_id ~root_at:(fun _ -> failwith "unexpected anchor read") in
    expect "unknown chain activated" (R.ready_exec rules ~epoch:max_int = Ok R.Prior))
    ["octra-mainnet"; "other"]

let tx = Octra_core.Transaction.{
  from = "oct_sender";
  to_ = "oct_sender";
  amount = Z.zero;
  nonce = 1;
  ou = Z.of_int 1_000;
  timestamp = 1.;
  signature = "signature";
  public_key = Some "pubkey";
  message = None;
  op_type = ValidatorReady;
  encrypted_data = None;
}

let verdict mode env =
  match verify mode env (Int64.sub head 3L) root with
  | Error reason -> reason
  | Ok () -> "head_epoch mismatch"

let test_partition () =
  let left = env 0 (Some (fun _ -> Lwt.return_some root)) in
  let right = env 64 None in
  List.iter (fun mode ->
    let reason = verdict mode left in
    let other = verdict mode right in
    let error_type = "validator_ready_rejected" in
    let rejections = [O.{position = 0; tx; error_type; reason}] in
    let artifacts = X.{
      confirmed = [];
      rejected = [{tx; error_type; reason = other}];
      confirmed_fees = Z.zero;
      tx_count = 0;
    } in
    let result = P.verify_preview_partition
      ~candidates:[tx] ~confirmed:[] ~rejections
      (Ok X.{post_state_root = root; artifacts}) in
    match mode with
    | R.Prior -> expect "reproduction missing" (result = Error "preview_rejection_mismatch")
    | R.Active -> expect "rejection commitments differ" (Result.is_ok result))
    [R.Prior; R.Active]

let () =
  test_reference ();
  test_prior ();
  test_activation ();
  test_partition ()