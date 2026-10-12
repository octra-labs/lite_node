(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Chain = Octra_node_runtime.Sync_chain
module Publish = Octra_node_runtime.Sync_publish
module Head = Octra_core.Head_manifest
module Journal = Octra_node_runtime.Consensus_finality_journal
module Anchor = Octra_bootstrap.Sync_anchor
module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Manifest = Octra_bootstrap.State_sync_manifest
module Irmin = Octra_core.Store_irmin
module Store = Octra_core.Store_chaindata
module Update = Octra_core.Validator_set_update
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module Protocol = Octra_consensus.Resource_compute_protocol
module Capability = Octra_consensus.Resource_compute_capability
module Selection = Octra_consensus.Resource_compute_selection
module Record = Octra_node_runtime.Compute_record
module Funding = Octra_core.Compute_funding
module Ledger = Octra_core.Ledger

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let check label value =
  if not value then failwith label

let sha value = Digestif.SHA256.(digest_string value |> to_hex)
let raw value = Digestif.SHA256.(digest_string value |> to_raw_string)

let private_key = String.make 32 '\123'

let wallet =
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets private_key
    |> Result.map_error (fun _ -> "invalid test key") |> get in
  let pub = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets)
    |> Base64.encode_exn in
  Octra_core.Crypto.Wallet.{
    address = Octra_core.Crypto.Address.address_from_pubkey pub;
    pub;
    priv = Base64.encode_exn private_key;
  }

let trusted = C.make_validator_set [C.{ address = wallet.address; pubkey = wallet.pub }]
let initial = get (Anchor.raw_validator_set trusted)
let chain_id = "octra-sync-test"

let with_store path action =
  let store = Lwt_main.run (Irmin.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (Irmin.close store))
    (fun () -> action store)

let set store key value =
  Lwt_main.run (Irmin.set_meta store key value)

let proof store epoch key =
  Lwt_main.run (Irmin.tag_epoch store epoch);
  get (Lwt_main.run (Irmin.merkle_proof_at_epoch store epoch ["meta"; key]))

let update ?weight index =
  let epoch = 3 * index in
  let member = Octra_core.Validator_admission.{
    address = wallet.address;
    pubkey = Base64.decode_exn wallet.pub;
    weight = Z.of_int (Option.value weight ~default:(index + 1));
  } in
  get (Update.make_weighted ~source_epoch:(Int64.of_int (epoch - 1))
    ~activate_epoch:(Int64.of_int (epoch + 1)) [member])

let finality ?index_root epoch validator_set (proof : Irmin.merkle_proof) =
  let epoch_id = Int64.of_int epoch in
  let index_root = Option.value index_root ~default:(sha ("index-" ^ string_of_int epoch)) in
  let state_root = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:proof.ledger_state_root ~epoch_index_root:index_root in
  let header = C.{
    proto_version = proto_version_current;
    chain_id;
    epoch_id;
    prev_state_root = raw "parent";
    tx_list_hash = H.tx_list_hash [];
    receipt_root = H.receipt_root [];
    proposed_state_root = Digestif.SHA256.(of_hex state_root |> to_raw_string);
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = wallet.address;
    txid_hi = 0L;
    ts = float_of_int epoch;
  } in
  let proposal_id = H.proposal_id header in
  let vote = C.{
    chain_id; epoch_id; round = 0; vote_type = Precommit; proposal_id;
    validator = wallet.address; signature = String.make 64 '\000';
  } in
  let vote = C.{ vote with signature = H.sign_ed25519 ~priv_raw:private_key
    ~msg:(H.vote_sign_bytes vote) } in
  let finalize = C.{
    chain_id; epoch_id; commit_round = 0; header; proposal_id;
    precommits = [vote]; parent_commit = None;
  } in
  get (Anchor.verify_finalize ~chain_id ~validator_set finalize);
  Journal.{ finalize; validator_set; bundle = None }, index_root

let test_prepare ?kept steps record checkpoint =
  let finalize = record.Journal.finalize in
  let head = Head.{
    schema_version;
    generation = Int64.to_int checkpoint.Checkpoint.epoch;
    epoch_id = Int64.to_int checkpoint.epoch;
    state_root = checkpoint.state_root;
    ledger_state_root = Some checkpoint.ledger_state_root;
    irmin_commit = Some (sha "commit");
    txid_hi = checkpoint.txid_hi;
    txlog_seg = Some 0;
    txlog_off = Some 0;
    epochlog_off = Some 0;
    commit_id = Anchor.finalize_hash finalize;
    ts = finalize.header.ts;
    quorum_cert_hash = checkpoint.quorum_cert_hash;
    epoch_index_hash = checkpoint.epoch_index_hash;
    epoch_index_root = checkpoint.epoch_index_root;
  } in
  let prepare ~steps ~head record =
    Publish.prepare ~chain_id ~config_hash:checkpoint.config_hash
      ~trusted_validator_set:trusted ~validator_set:record.Journal.validator_set
      ~steps ~head record in
  let task = prepare ~steps ~head record in
  check "producer verification completed without yielding"
    (match Lwt.state task with Lwt.Sleep -> true | _ -> false);
  let prepared = get (Lwt_main.run task) in
  let expected = Checkpoint.{ checkpoint with
    valid_until = Int64.add checkpoint.created_at 2_678_400L } in
  check "producer checkpoint changed" (prepared.checkpoint = expected);
  check "producer checkpoint hash changed"
    (prepared.checkpoint_hash = get (Checkpoint.hash expected));
  let expected_steps = Option.value kept ~default:steps in
  let expected = Anchor.make ~steps:expected_steps ~finalize ~validator_set:record.validator_set in
  check "producer retained repeated validator path"
    (Anchor.encode prepared.anchor = Anchor.encode expected);
  let repeated = Lwt_main.run (Anchor.compact_lwt ~validator_set:trusted prepared.anchor) |> get in
  check "producer path is not idempotent" (Anchor.encode repeated = Anchor.encode prepared.anchor);
  ignore (get (Anchor.verify ~validator_set:trusted prepared.checkpoint
    (Anchor.encode prepared.anchor)));
  let anchor = Anchor.make ~steps ~finalize ~validator_set:record.validator_set in
  let reference = Anchor.derive ~validator_set:trusted anchor in
  check "producer derived set differs"
    (Lwt_main.run (Anchor.derive_lwt ~validator_set:trusted anchor) = reference);
  let stopped = ref false in
  let task = Anchor.derive_lwt ~cancelled:(fun () -> !stopped)
    ~validator_set:trusted anchor in
  stopped := true;
  check "producer cancellation was ignored"
    (Lwt_main.run task = Error "state sync verification cancelled");
  stopped := false;
  let task = Anchor.compact_lwt ~cancelled:(fun () -> !stopped) ~validator_set:trusted anchor in
  stopped := true;
  check "path cancellation was ignored"
    (Lwt_main.run task = Error "state sync verification cancelled");
  let reject ~steps ~head record expected =
    let actual = Lwt_main.run (prepare ~steps ~head record) in
    let status = function Ok _ -> "accepted" | Error reason -> reason in
    check (Printf.sprintf "producer refusal differs expected = %s actual = %s"
      (status expected) (status actual)) (actual = expected) in
  reject ~steps ~head:{ head with state_root = sha "different" } record
    (Error "state sync HEAD folded root is invalid");
  let damage (value : C.finalize) = C.{ value with
    precommits = List.map (fun (vote : C.vote) ->
      { vote with signature = String.make 64 '\000' }) value.precommits } in
  let broken = Journal.{ record with finalize = damage finalize } in
  let task = prepare ~steps ~head broken in
  check "producer accepted damaged checkpoint" (Result.is_error (Lwt_main.run task));
  List.iteri (fun index first ->
    if Option.is_some kept || index = 0 then
      List.iter (fun changed ->
        let steps = List.mapi (fun offset step ->
          if offset = index then changed else step) steps in
        let anchor = Anchor.make ~steps ~finalize ~validator_set:record.validator_set in
        let expected = match Anchor.derive ~validator_set:trusted anchor with
          | Error reason -> Error reason
          | Ok _ -> failwith "damaged producer chain verified" in
        reject ~steps ~head record expected;
        check "async damaged chain refusal differs"
          (Lwt_main.run (Anchor.derive_lwt ~validator_set:trusted anchor) = expected))
        [Anchor.{ first with finalize = damage first.finalize };
         Anchor.{ first with proof = "invalid" }]) steps;
  prepared

let certificate ?kept steps record index_root ledger_root =
  let finalize = record.Journal.finalize in
  let checkpoint = Checkpoint.{
    chain_id; epoch = finalize.epoch_id;
    state_root = Octra_node_runtime.Text.raw_to_hex finalize.header.proposed_state_root;
    ledger_state_root = ledger_root;
    txid_hi = 0L; config_hash = sha "config";
    validator_set_hash = Octra_consensus.C_config.validator_set_hash record.validator_set
      |> Octra_node_runtime.Text.raw_to_hex;
    quorum_cert_hash = Some (Anchor.finalize_hash finalize);
    epoch_index_hash = Some (sha "index");
    epoch_index_root = Some index_root;
    created_at = finalize.epoch_id;
    valid_until = Int64.add finalize.epoch_id 100_000L;
  } in
  let anchor = Anchor.make ~steps ~finalize ~validator_set:record.validator_set in
  let prepared = test_prepare ?kept steps record checkpoint in
  let encoded = Anchor.encode anchor in
  let verified = get (Anchor.verify ~validator_set:trusted checkpoint encoded) in
  let ticks = ref 0 in
  let running = ref true in
  let rec pulse () =
    let open Lwt.Syntax in
    let* () = Lwt.pause () in
    if !running then begin
      incr ticks;
      pulse ()
    end else Lwt.return_unit
  in
  let checked = Lwt_main.run (
    Lwt.async pulse;
    Lwt.finalize
      (fun () -> Anchor.verify_lwt ~validator_set:trusted checkpoint encoded)
      (fun () -> running := false; Lwt.return_unit)) |> get in
  check "async validator chain differs" (Anchor.encode verified = Anchor.encode checked);
  check "validator chain blocked event loop" (!ticks >= List.length steps);
  let checkpoint_hash = get (Checkpoint.hash checkpoint) in
  let files = ["HEAD.json"; "ledger.dat"; Octra_bootstrap.Root_win.name; "state_root"]
    |> List.sort String.compare
    |> List.map (fun path -> Manifest.{
      path; size = 1L; sha256 = sha "x";
      chunks = [{ index = 0; offset = 0L; size = 1; sha256 = sha "x" }];
    }) in
  let manifest = Manifest.{
    checkpoint_hash; snapshot_id = checkpoint_hash; irmin_commit = None;
    chunk_size = chunk_size_min; total_size = 4L; file_count = 4;
    chunk_count = 4; chunks_root = chunks_root files; files;
  } in
  let certificate = Manifest.{
    checkpoint; checkpoint_hash; authority = Finalized encoded; manifest;
    manifest_hash = get (manifest_hash manifest);
    exporter_signatures = [get (make_exporter_signature ~wallet manifest)];
  } in
  let verified = get (Manifest.verify_certificate ~validator_set:trusted ~exporter_set:trusted certificate) in
  let reduced = Manifest.{ certificate with authority = Finalized (Anchor.encode prepared.anchor) } in
  ignore (get (Manifest.verify_certificate ~validator_set:trusted ~exporter_set:trusted reduced));
  let checked = Lwt_main.run (Manifest.verify_certificate_lwt
    ~validator_set:trusted ~exporter_set:trusted certificate) |> get in
  check "async certificate differs" (Manifest.certificate_json verified = Manifest.certificate_json checked);
  verified

let test_repeated_sets () =
  List.iter (fun (weights, indices) ->
    Test_workspace.with_dir "sync_sets" (fun root ->
      with_store (Filename.concat root "history") (fun store ->
        let validator_set, reversed = List.fold_left (fun (prior, steps) (index, weight) ->
          let value = update ~weight index in
          let encoded = Update.to_string value in
          set store Update.pending_meta_key encoded;
          let proof = proof store (3 * index) Update.pending_meta_key in
          let record, index_root = finality (3 * index) prior proof in
          let step = Anchor.{ source = Pending; finalize = record.finalize;
            ledger_state_root = proof.ledger_state_root; epoch_index_root = index_root;
            update = encoded; proof = proof.proof } in
          set store Update.active_meta_key encoded;
          get (Update.validator_set value), step :: steps)
            (initial, []) (List.mapi (fun offset weight -> offset + 1, weight) weights) in
        let steps = List.rev reversed in
        let kept = List.map (fun index -> List.nth steps (index - 1)) indices in
        let epoch = 3 * List.length steps + 2 in
        let proof = proof store epoch Update.active_meta_key in
        let record, index_root = finality epoch validator_set proof in
        ignore (certificate ~kept steps record index_root proof.ledger_state_root))))
    [[2; 2; 2], [1]; [2; 3; 2; 4; 3; 5], [1; 2; 6]; [2; 3; 4; 2], [1]]

let saved ?(count = Anchor.max_steps) ?(width = 0) path =
  with_store path (fun store ->
    if width > 0 then set store (String.make width 'k') "preserved";
    let rec loop index validator_set acc =
      if index > count then List.rev acc, validator_set
      else
        let value = update index in
        let encoded = Update.to_string value in
        set store Update.pending_meta_key encoded;
        let proof = proof store (3 * index) Update.pending_meta_key in
        let record, index_root = finality (3 * index) validator_set proof in
        let step = Anchor.{
          source = Pending; finalize = record.finalize;
          ledger_state_root = proof.ledger_state_root;
          epoch_index_root = index_root; update = encoded; proof = proof.proof;
        } in
        set store Update.active_meta_key encoded;
        loop (index + 1) (get (Update.validator_set value)) (step :: acc)
    in
    let steps, validator_set = loop 1 initial [] in
    let epoch = 3 * count + 2 in
    let proof = proof store epoch Update.active_meta_key in
    let record, index_root = finality epoch validator_set proof in
    certificate steps record index_root proof.ledger_state_root)

let funding_case ?state_root ?epoch_id ?(offer_memory = 131072L) (origin : Anchor.origin) =
  let epoch_id = Option.value epoch_id ~default:origin.epoch_id in
  let state_root = Option.value state_root ~default:origin.state_root in
  let signed message = H.sign_ed25519 ~priv_raw:private_key ~msg:message in
  let offer = Protocol.{chain_id = origin.chain_id; offered_epoch = Int64.sub epoch_id 3L;
    expires_epoch = Int64.add epoch_id 8L; coordinator = wallet.address;
    validator_set_root = Octra_consensus.C_config.validator_set_hash origin.validators;
    circle_id = wallet.address; model_epoch = epoch_id; model_state_root = state_root;
    graph_root = raw "graph"; model_root = raw "model"; program_root = raw "program";
    executor_root = raw "executor"; min_memory_bytes = offer_memory;
    request_bytes = 2; response_bytes = 256; signature = String.make 64 '\000'} in
  let offer = {offer with signature = signed (Protocol.offer_sign_bytes offer)} in
  let capability = Capability.{chain_id = offer.chain_id; observed_epoch = offer.offered_epoch;
    valid_until = offer.expires_epoch; validator_set_root = offer.validator_set_root;
    node_id = wallet.address; circle_id = offer.circle_id; model_epoch = offer.model_epoch;
    model_state_root = offer.model_state_root; graph_root = offer.graph_root;
    model_root = offer.model_root; program_root = offer.program_root; executor_root = offer.executor_root;
    evidence_root = raw "evidence"; accelerator = Cpu; lanes = 1; memory_bytes = 262144L;
    max_request_bytes = 256; max_response_bytes = 256; signature = String.make 64 '\000'} in
  let capability = {capability with signature = signed (Capability.sign_bytes capability)} in
  let commitment = Selection.{chain_id = offer.chain_id; offer_id = Protocol.offer_id offer;
    commit_epoch = Int64.sub epoch_id 2L; node_id = wallet.address; graph_root = offer.graph_root;
    model_root = offer.model_root; program_root = offer.program_root; executor_root = offer.executor_root;
    nonce_hash = Selection.nonce_hash "nonce"; signature = String.make 64 '\000'} in
  let commitment = {commitment with signature = signed (Selection.commitment_sign_bytes commitment)} in
  let reveal = Selection.{chain_id = offer.chain_id; offer_id = commitment.offer_id;
    commit_epoch = commitment.commit_epoch; reveal_epoch = Int64.pred epoch_id;
    node_id = wallet.address; graph_root = offer.graph_root; model_root = offer.model_root;
    program_root = offer.program_root; executor_root = offer.executor_root; nonce = "nonce";
    evidence_root = capability.evidence_root; signature = String.make 64 '\000'} in
  let reveal = {reveal with signature = signed (Selection.reveal_sign_bytes commitment reveal)} in
  let selected = Selection.select ~chain_id:offer.chain_id ~offer_id:commitment.offer_id
    ~target_epoch:epoch_id ~challenge:(Selection.challenge ~chain_id:offer.chain_id
      ~source_epoch:epoch_id ~finality_hash:state_root) ~minimum_delay:1L ~committee_size:1
    ~verify_commitment_signature:(fun value -> H.verify_ed25519
      ~pubkey_raw:(Base64.decode_exn wallet.pub) ~msg:(Selection.commitment_sign_bytes value)
      ~signature:value.signature)
    ~verify_reveal_signature:(fun prior value -> H.verify_ed25519
      ~pubkey_raw:(Base64.decode_exn wallet.pub) ~msg:(Selection.reveal_sign_bytes prior value)
      ~signature:value.signature)
    ~verify_evidence:(fun value -> value.evidence_root = capability.evidence_root)
    ~commitments:[commitment] ~reveals:[reveal] in
  let request = Octra_consensus.Resource_compute_certificate.{unit_price = Some 2L; fee_cap = Some 2000L;
    memory_bytes = Some 131072; guest_bytes = Some 65536; fuel_limit = Some 1000;
    chain_id = offer.chain_id; epoch_id; expires_epoch = offer.expires_epoch;
    validator_set_root = offer.validator_set_root; selection_root = selected.root;
    circle_id = offer.circle_id; state_root; model_epoch = offer.model_epoch;
    model_state_root = offer.model_state_root; graph_root = offer.graph_root;
    model_root = offer.model_root; program_root = offer.program_root; executor_root = offer.executor_root;
    caller = wallet.address; session_id = raw "session"; sequence = 1L;
    input_hash = Protocol.input_hash ~method_name:"predict" ~params_json:"[]";
    input_bytes = 2; max_output_bytes = 256} in
  let call = Protocol.{request; valid_until = Some offer.expires_epoch; method_name = "predict";
    params_json = "[]"; caller_public_key = Base64.decode_exn wallet.pub;
    caller_signature = String.make 64 '\000'; coordinator = wallet.address;
    signature = String.make 64 '\000'} in
  let call = {call with caller_signature = signed (Protocol.intent_sign_bytes call)} in
  let call = {call with signature = signed (Protocol.call_sign_bytes call)} in
  call, offer, capability, commitment, reveal, selected

let test_session origin (call : Protocol.call) offer capability commitment reveal =
  let module Bill = Octra_core.Compute_bill in
  let module Certificate = Octra_consensus.Resource_compute_certificate in
  let sign message = H.sign_ed25519 ~priv_raw:private_key ~msg:message in
  let resign call =
    let call = {call with Protocol.caller_signature = sign (Protocol.intent_sign_bytes call)} in
    {call with Protocol.signature = sign (Protocol.call_sign_bytes call)} in
  let signed_result (measured : Bill.measured) output =
    let vote = Certificate.{request_id = measured.request_id; node_id = wallet.address;
      output_hash = measured.output_hash; output_bytes = measured.output_bytes;
      trace_root = measured.trace_root; steps = measured.steps; operations = measured.operations;
      signature = String.make 64 '\000'} in
    let vote = {vote with signature = sign (Certificate.vote_sign_bytes vote)} in
    Protocol.{vote; output_json = output} in
  let step ?(expires = call.request.expires_epoch) sequence =
    let call = resign {call with Protocol.request = {call.request with sequence; expires_epoch = expires}} in
    let measured = Bill.measure ~request:call.request ~output:"null" ~trace_root:(raw "trace")
      ~steps:3L ~operations:1L ~outcome:Bill.Complete |> get in
    let record = Record.{offer; capabilities = [capability]; commitments = [commitment];
      reveals = [reveal]; call; results = [signed_result measured "null"]; validators = None} in
    (origin, record), measured in
  let first, first_result = step 1L in
  let second, second_result = step 2L in
  let verify records = Record.Session.verify ~epoch:origin.Anchor.epoch_id ~model:origin records in
  let session = verify [first; second] |> get in
  check "session call order changed"
    (Record.Session.calls session = [(snd first).call; (snd second).call]);
  check "session model origin changed" (Record.Session.model session = origin);
  let bills = Record.Session.bills session [first_result; second_result] |> get in
  check "session payment count differs" (List.length bills = 2);
  let old, old_result = step ~expires:origin.epoch_id 1L in
  let later = Int64.succ origin.epoch_id in
  check "expired unpaid prefix accepted" (Result.is_error
    (Record.Session.verify ~epoch:later ~model:origin [old; second]));
  let at call = if call.Protocol.request.sequence = 1L then origin.epoch_id else later in
  let paid = Record.Session.verify ~at ~epoch:later ~model:origin [old; second] |> get in
  check "expired paid prefix blocked suffix"
    (List.length (Record.Session.bills paid [old_result; second_result] |> get) = 2);
  check "future payment epoch accepted" (Result.is_error
    (Record.Session.verify ~at:(fun _ -> Int64.succ later) ~epoch:later ~model:origin [first; second]));
  check "session payment amount differs"
    (List.for_all (fun (bill : Bill.t) -> Z.equal bill.charged (Z.of_int 6)) bills);
  List.iter (fun records ->
    check "session invalid sequence accepted" (Result.is_error (verify records)))
    [[]; [second]; [first; first]; [second; first]; [first; fst (step 3L)];
     List.init 65 (fun _ -> first)];
  let changed update =
    let record = snd second in
    origin, {record with Record.call = update record.call} in
  List.iter (fun record ->
    check "session invalid scope accepted" (Result.is_error (verify [first; record])))
    [changed (fun value -> {value with signature = String.make 64 '\000'});
     changed (fun value -> resign {value with request = {value.request with model_root = raw "other"}});
     changed (fun value -> resign {value with request = {value.request with session_id = raw "other"}});
     changed (fun value -> resign {value with request = {value.request with model_state_root = raw "other"}});
     changed (fun value -> resign {value with request = {value.request with model_epoch = Int64.pred origin.epoch_id}})];
  check "session missing result accepted"
    (Result.is_error (Record.Session.bills session [first_result]));
  check "session reordered results accepted"
    (Result.is_error (Record.Session.bills session [second_result; first_result]));
  let forged = Bill.measure ~request:(snd first).call.request ~output:"false" ~trace_root:(raw "trace")
    ~steps:3L ~operations:1L ~outcome:Bill.Complete |> get in
  check "session unverified output accepted"
    (Result.is_error (Record.Session.bills session [forged; second_result]));
  let stopped = Bill.measure ~request:(snd first).call.request
    ~output:"{\"compute_stop\":7}" ~trace_root:(raw "trace")
    ~steps:3L ~operations:1L ~outcome:(Bill.Stopped 7) |> get in
  let stopped_record = {(snd first) with Record.results = [signed_result stopped "{\"compute_stop\":7}"]} in
  let ended = verify [origin, stopped_record] |> get in
  let paid = Record.Session.bills ended [stopped] |> get in
  check "session stopped work refused"
    (List.length paid = 1 && (List.hd paid).measured.outcome = Bill.Stopped 7);
  let continued = verify [(origin, stopped_record); second] |> get in
  check "session continued after stopped call"
    (Record.Session.bills continued [stopped; second_result]
      = Error "compute session prior call stopped")

let test_funding_origin origin anchor =
  let call, offer, capability, commitment, reveal, expected = funding_case origin in
  test_session origin call offer capability commitment reveal;
  let header = Protocol.header call |> get in
  let verify ?(header = header) ?(epoch = origin.Anchor.epoch_id) ?(offer = offer)
      ?(capabilities = [capability]) ?(commitments = [commitment]) ?(reveals = [reveal]) () =
    Record.verify_selection ~origin ~epoch ~header ~offer ~capabilities ~commitments ~reveals in
  check "funding selection refused" (verify () = Ok expected);
  let decoded = Protocol.decode_header (Protocol.encode_header header) |> get in
  check "funding header transport changed" (verify ~header:decoded () = Ok expected);
  List.iter (fun call ->
    let header = Protocol.header call |> get in
    check "funding accepted wrong origin" (Result.is_error (verify ~header ())))
    [(let call, _, _, _, _, _ = funding_case ~state_root:(raw "other root") origin in call);
     (let call, _, _, _, _, _ = funding_case ~epoch_id:(Int64.succ origin.epoch_id) origin in call)];
  let no_signature = String.make 64 '\000' in
  let invalid_call = {call with caller_signature = no_signature} |> Protocol.header |> get in
  check "funding accepted unsigned caller" (Result.is_error (verify ~header:invalid_call ()));
  let invalid_call = {call with signature = no_signature} |> Protocol.header |> get in
  check "funding accepted unsigned coordinator" (Result.is_error (verify ~header:invalid_call ()));
  check "funding accepted unsigned offer"
    (Result.is_error (verify ~offer:{offer with signature = no_signature} ()));
  check "funding accepted unsigned capacity"
    (Result.is_error (verify ~capabilities:[{capability with signature = no_signature}] ()));
  check "funding accepted unsigned commitment"
    (Result.is_error (verify ~commitments:[{commitment with signature = no_signature}] ()));
  check "funding accepted unsigned reveal"
    (Result.is_error (verify ~reveals:[{reveal with signature = no_signature}] ()));
  let sign message = H.sign_ed25519 ~priv_raw:private_key ~msg:message in
  let resign call =
    let call = {call with Protocol.caller_signature = sign (Protocol.intent_sign_bytes call)} in
    {call with Protocol.signature = sign (Protocol.call_sign_bytes call)} in
  let changed = resign {call with request = {call.request with selection_root = raw "other choice"}} in
  check "funding accepted altered selection"
    (Result.is_error (verify ~header:(Protocol.header changed |> get) ()));
  let unpriced = resign {call with request = {call.request with unit_price = None; fee_cap = None}} in
  check "funding accepted missing price"
    (Result.is_error (verify ~header:(Protocol.header unpriced |> get) ()));
  let small = {capability with memory_bytes = 65536L} in
  let small = {small with signature = sign (Capability.sign_bytes small)} in
  check "funding accepted insufficient memory" (Result.is_error (verify ~capabilities:[small] ()));
  List.iter (fun (offer_memory, memory_bytes, message) ->
    let call, offer, capacity, commitment, reveal, _ = funding_case ~offer_memory origin in
    let capacity = {capacity with memory_bytes} in
    let capacity = {capacity with signature = sign (Capability.sign_bytes capacity)} in
    let header = Protocol.header call |> get in
    check message (Result.is_error (Record.verify_selection ~origin ~epoch:origin.epoch_id
      ~header ~offer ~capabilities:[capacity] ~commitments:[commitment] ~reveals:[reveal])))
    [65536L, 65536L, "funding accepted request memory";
     262144L, 131072L, "funding accepted offer memory"];
  List.iter (fun capacity ->
    let capacity = {capacity with Capability.signature = sign (Capability.sign_bytes capacity)} in
    check "funding accepted mismatched capacity" (Result.is_error (verify ~capabilities:[capacity] ())))
    [{capability with max_request_bytes = 1}; {capability with max_response_bytes = 128};
     {capability with model_root = raw "other model"};
     {capability with executor_root = raw "other executor"};
     {capability with validator_set_root = raw "other set"}];
  let spare = {capability with evidence_root = raw "spare evidence"} in
  let spare = {spare with signature = sign (Capability.sign_bytes spare)} in
  check "funding capacity order changed selection"
    (verify ~capabilities:[capability; spare] () = Ok expected
     && verify ~capabilities:[spare; capability] () = Ok expected);
  let changed_offer = {offer with model_root = raw "other model"} in
  let changed_offer = {changed_offer with signature = sign (Protocol.offer_sign_bytes changed_offer)} in
  check "funding accepted mismatched offer" (Result.is_error (verify ~offer:changed_offer ()));
  let changed_reveal = {reveal with reveal_epoch = origin.epoch_id} in
  let changed_reveal = {changed_reveal with
    signature = sign (Selection.reveal_sign_bytes commitment changed_reveal)} in
  check "funding accepted late reveal" (Result.is_error (verify ~reveals:[changed_reveal] ()));
  let expired = {capability with valid_until = origin.epoch_id} in
  let expired = {expired with signature = sign (Capability.sign_bytes expired)} in
  check "funding accepted expired capacity"
    (Result.is_error (verify ~epoch:(Int64.succ origin.epoch_id) ~capabilities:[expired] ()));
  check "funding accepted expired call"
    (Result.is_error (verify ~epoch:(Int64.succ call.request.expires_epoch) ()));
  check "funding accepted future call"
    (Result.is_error (verify ~epoch:(Int64.pred origin.epoch_id) ()));
  check "funding expiry edge refused" (verify ~epoch:call.request.expires_epoch () = Ok expected);
  check "funding accepted missing capacity" (Result.is_error (verify ~capabilities:[] ()));
  check "funding accepted missing commitment" (Result.is_error (verify ~commitments:[] ()));
  check "funding accepted missing reveal" (Result.is_error (verify ~reveals:[] ()));
  check "funding accepted repeated capacity"
    (Result.is_error (verify ~capabilities:[capability; capability] ()));
  check "funding accepted repeated commitment"
    (Result.is_error (verify ~commitments:[commitment; commitment] ()));
  check "funding accepted repeated reveal" (Result.is_error (verify ~reveals:[reveal; reveal] ()));
  check "funding accepted oversized evidence"
    (Result.is_error (verify ~capabilities:(List.init 257 (fun _ -> capability)) ()));
  let keys = private_key :: List.init 5 (fun index -> String.make 32 (Char.chr (index + 1))) in
  let signers = List.map (fun key ->
    let secret = Mirage_crypto_ec.Ed25519.priv_of_octets key
      |> Result.map_error (fun _ -> "invalid committee key") |> get in
    let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv secret |> pub_to_octets) in
    let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
    key, C.{address; pubkey}) keys in
  let validators = C.make_validator_set (List.map snd signers) in
  let trusted = C.make_validator_set (List.map (fun (_, (node : C.validator_info)) ->
    C.{node with pubkey = Base64.encode_exn node.pubkey}) signers) in
  let finalize = Anchor.finality anchor in
  let vote = List.hd finalize.precommits in
  let precommits = List.map (fun (key, (node : C.validator_info)) ->
    let vote = C.{vote with validator = node.address} in
    C.{vote with signature = H.sign_ed25519 ~priv_raw:key ~msg:(H.vote_sign_bytes vote)}) signers in
  let encoded = Anchor.make ~steps:[] ~finalize:C.{finalize with precommits}
    ~validator_set:validators |> Anchor.encode in
  let larger = Anchor.verify_origin ~max_bytes:(String.length encoded) ~trusted
    ~chain_id:origin.chain_id ~epoch_id:origin.epoch_id ~state_root:origin.state_root
    ~set_root:(Octra_consensus.C_config.validator_set_hash validators) encoded |> get in
  let call, offer, capability, commitment, reveal, _ = funding_case larger in
  let header = Protocol.header call |> get in
  check "funding accepted undersized committee"
    (Result.is_error (Record.verify_selection ~origin:larger ~epoch:larger.epoch_id ~header ~offer
      ~capabilities:[capability] ~commitments:[commitment] ~reveals:[reveal]))

let funding_head source anchor epoch proof =
  let record, index_root = finality epoch source.Anchor.validators proof in
  let encoded = Anchor.make ~steps:(Anchor.steps anchor) ~finalize:record.finalize
    ~validator_set:source.validators |> Anchor.encode in
  let head = Anchor.verify_origin ~max_bytes:(String.length encoded) ~trusted
    ~chain_id:source.chain_id ~epoch_id:(Int64.of_int epoch)
    ~state_root:record.finalize.header.proposed_state_root
    ~set_root:(Octra_consensus.C_config.validator_set_hash source.validators) encoded |> get in
  head, index_root

let test_funding_state source anchor =
  Test_workspace.with_dir "funding_state" (fun root ->
    with_store (Filename.concat root "ledger") (fun store ->
      let run = Lwt_main.run in
      let ledger = Ledger.create_from_store store in
      Ledger.add_account ledger wallet.address (Z.of_int 50_000) |> get;
      run (Ledger.flush_dirty_lwt ledger);
      let call, offer, capability, commitment, reveal, selected = funding_case source in
      let header = Protocol.header call |> get in
      let id = Octra_consensus.Resource_compute_certificate.request_id call.request in
      let path = Funding.deposit_path id in
      let terms = Funding.prepare_header ~header ~target:Funding.escrow ~nonce:1L |> get in
      let permission = Funding.authorize terms
        ~signature:(H.sign_ed25519 ~priv_raw:private_key ~msg:(Funding.sign_bytes terms)) |> get in
      let verify ?(epoch = Int64.succ source.epoch_id) ?max_bytes head index_root
          (proof : Irmin.merkle_proof) =
        let max_bytes = Option.value max_bytes ~default:(String.length proof.proof) in
        Record.verify_funding ~origin:source ~head ~epoch ~header ~offer
          ~capabilities:[capability] ~commitments:[commitment] ~reveals:[reveal]
          ~max_bytes ~ledger_root:proof.ledger_state_root ~index_root ~proof:proof.proof in
      let epoch = Int64.to_int source.epoch_id + 1 in
      run (Irmin.tag_epoch store epoch);
      let absent = run (Irmin.merkle_proof_at_epoch store epoch path) |> get in
      let empty_head, empty_index = funding_head source anchor epoch absent in
      check "funding accepted absent deposit" (Result.is_error (verify empty_head empty_index absent));
      run (Irmin.begin_epoch_batch store);
      Ledger.begin_journal ledger |> get;
      check "funding hold failed"
        (run (Funding.hold_header ~ledger ~chain_id ~epoch:(Int64.of_int epoch)
          ~fee:Z.zero ~header permission) = Ok (id, Z.zero));
      let hidden = run (Irmin.merkle_proof_at_epoch store epoch path) |> get in
      check "uncommitted funding became visible"
        (hidden.value = None && Result.is_error (verify empty_head empty_index hidden));
      run (Irmin.commit_epoch_batch store "funding-state");
      Ledger.commit_journal ledger |> get;
      run (Irmin.tag_epoch store epoch);
      let proof = run (Irmin.merkle_proof_at_epoch store epoch path) |> get in
      let head, index_root = funding_head source anchor epoch proof in
      let accepted = verify head index_root proof |> get in
      let chosen, deposit = accepted in
      check "funding selection changed" (chosen = selected);
      check "funding deposit changed"
        (deposit.request_id = id && deposit.opened_epoch = Int64.of_int epoch
         && deposit.returned_epoch = None);
      let end_head, end_index = funding_head source anchor
        (Int64.to_int call.request.expires_epoch) proof in
      check "funding expiry edge refused"
        (verify ~epoch:call.request.expires_epoch end_head end_index proof = Ok accepted);
      let max_bytes = String.length proof.proof in
      check "funding accepted old head"
        (Result.is_error (verify ~epoch:(Int64.of_int (epoch + 1)) head index_root proof));
      check "funding accepted wrong index"
        (Result.is_error (verify head (sha "other index") proof));
      check "funding accepted short proof cap"
        (Result.is_error (verify ~max_bytes:(max_bytes - 1) head index_root proof));
      check "funding accepted corrupt proof"
        (Result.is_error (verify head index_root {proof with proof = String.make max_bytes '?'}));
      check "funding accepted malformed proof"
        (Result.is_error (verify head index_root {proof with proof = Base64.encode_exn "invalid"}));
      check "funding accepted foreign ledger root"
        (Result.is_error (verify head index_root {proof with ledger_state_root = sha "other ledger"}));
      check "state proof accepted empty path"
        (Result.is_error (Anchor.read_state ~max_bytes ~origin:head
          ~ledger_root:proof.ledger_state_root ~index_root ~path:[] ~proof:proof.proof));
      let verified = run (Anchor.read_state_lwt ~max_bytes ~origin:head
        ~ledger_root:proof.ledger_state_root ~index_root ~path ~proof:proof.proof ()) in
      check "async state proof differs" (verified = Ok proof.value);
      let async ?(cancelled = fun () -> false) ?(max_bytes = max_bytes) () =
        Record.verify_funding_lwt ~cancelled ~origin:source ~head ~epoch:(Int64.of_int epoch)
          ~header ~offer ~capabilities:[capability] ~commitments:[commitment] ~reveals:[reveal]
          ~max_bytes ~ledger_root:proof.ledger_state_root ~index_root ~proof:proof.proof () in
      check "async funding proof differs" (run (async ()) = Ok accepted);
      check "funding size check was deferred"
        (match Lwt.state (async ~max_bytes:0 ()) with
        | Lwt.Return (Error "state proof size exceeds limit") -> true
        | _ -> false);
      let stopped = ref false in
      let task = async ~cancelled:(fun () -> !stopped) () in
      stopped := true;
      check "funding cancellation was ignored"
        (run task = Error "compute funding verification cancelled");
      check "pre-cancelled funding was accepted"
        (run (async ~cancelled:(fun () -> true) ()) = Error "compute funding verification cancelled");
      let changed = {call with request = {call.request with sequence = Int64.succ call.request.sequence}} in
      let changed = {changed with caller_signature = H.sign_ed25519 ~priv_raw:private_key
        ~msg:(Protocol.intent_sign_bytes changed)} in
      let changed = {changed with signature = H.sign_ed25519 ~priv_raw:private_key
        ~msg:(Protocol.call_sign_bytes changed)} in
      let raw = Option.get proof.value in
      check "funding accepted another request"
        (Result.is_error (Funding.verify_deposit ~chain_id ~epoch:(Int64.of_int epoch)
          ~header:(Protocol.header changed |> get) raw));
      let expired = Int64.succ call.request.expires_epoch in
      run (Irmin.begin_epoch_batch store);
      Ledger.begin_journal ledger |> get;
      check "funding refund failed"
        (run (Funding.refund ~ledger ~chain_id ~epoch:expired ~id) = Ok (Z.of_int64 terms.amount));
      run (Irmin.commit_epoch_batch store "funding-return");
      Ledger.commit_journal ledger |> get;
      run (Irmin.tag_epoch store (Int64.to_int expired));
      let returned = run (Irmin.merkle_proof_at_epoch store (Int64.to_int expired) path) |> get in
      let closed_head, closed_index = funding_head source anchor (Int64.to_int expired) returned in
      let closed = Anchor.read_state ~max_bytes:(String.length returned.proof)
        ~origin:closed_head ~ledger_root:returned.ledger_state_root ~index_root:closed_index
        ~path ~proof:returned.proof |> get |> Option.get in
      check "funding accepted returned deposit"
        (Funding.verify_deposit ~chain_id ~epoch:expired ~header closed
         = Error "compute deposit returned");
      check "funding accepted proof after return"
        (Result.is_error (verify ~epoch:expired closed_head closed_index returned));
      check "funding read changed balance"
        ((Ledger.find ledger wallet.address).balance = Z.of_int 50_000
         && (Ledger.find ledger wallet.address).nonce = 1
         && Ledger.get_total_supply ledger = Z.of_int 50_000)))

let test_origin () =
  Test_workspace.with_dir "compute_origin" (fun root ->
    let certificate = saved ~count:2 (Filename.concat root "history") in
    let encoded = match certificate.Manifest.authority with
      | Finalized value -> value
      | Checkpoint_quorum _ -> failwith "origin proof is missing" in
    let anchor = Anchor.decode encoded |> get in
    let epoch_id = certificate.checkpoint.epoch in
    let state_root = (Anchor.finality anchor).header.proposed_state_root in
    let set_root = Octra_consensus.C_config.validator_set_hash (Anchor.validator_set anchor) in
    let max_bytes = String.length encoded in
    let verify ?(chain_id = chain_id) ?(epoch_id = epoch_id) ?(state_root = state_root)
        ?(set_root = set_root) ?(max_bytes = max_bytes) ?(trusted = trusted) value =
      Anchor.verify_origin ~max_bytes ~trusted ~chain_id ~epoch_id ~state_root ~set_root value in
    let expected = verify encoded |> get in
    test_funding_origin expected anchor;
    test_funding_state expected anchor;
    check "origin chain changed" (expected.chain_id = chain_id);
    check "origin epoch changed" (expected.epoch_id = epoch_id);
    check "origin state changed" (expected.state_root = state_root);
    check "origin validators changed"
      (Octra_consensus.C_config.validator_set_hash expected.validators = set_root);
    List.iter (fun result -> check "origin accepted another context" (Result.is_error result))
      [verify ~chain_id:"other-chain" encoded; verify ~epoch_id:(Int64.succ epoch_id) encoded;
       verify ~state_root:(raw "other state") encoded; verify ~set_root:(raw "other set") encoded;
       verify ~max_bytes:(max_bytes - 1) encoded; verify ~max_bytes:0 encoded;
       verify ~epoch_id:(-1L) encoded; verify ~state_root:"short" encoded];
    let proof ~steps ~finalize ~validator_set = Anchor.make ~steps ~finalize ~validator_set |> Anchor.encode in
    let steps = Anchor.steps anchor in
    let finalize = Anchor.finality anchor in
    let validators = Anchor.validator_set anchor in
    let damaged = C.{finalize with precommits = List.map (fun (vote : C.vote) ->
      {vote with signature = String.make 64 '\000'}) finalize.precommits} in
    let first = List.hd steps in
    let altered = [
      "missing transitions", proof ~steps:[] ~finalize ~validator_set:validators;
      "unsigned finality", proof ~steps ~finalize:damaged ~validator_set:validators;
      "invalid state proof", proof ~steps:(Anchor.{first with proof = "invalid"} :: List.tl steps)
        ~finalize ~validator_set:validators;
      "repeated transition", proof ~steps:(first :: steps) ~finalize ~validator_set:validators;
      "reversed transitions", proof ~steps:(List.rev steps) ~finalize ~validator_set:validators;
      "altered final set", proof ~steps ~finalize ~validator_set:initial] in
    List.iter (fun (label, value) ->
      check ("origin accepted " ^ label)
        (Result.is_error (verify ~max_bytes:(String.length value) value));
      let checked = Lwt_main.run (Anchor.verify_origin_lwt ~max_bytes:(String.length value)
        ~trusted ~chain_id ~epoch_id ~state_root ~set_root value) in
      check ("async origin accepted " ^ label) (Result.is_error checked)) altered;
    let changed_root = raw "unsigned final state" in
    let changed = C.{finalize with header = {finalize.header with proposed_state_root = changed_root}} in
    let changed = proof ~steps ~finalize:changed ~validator_set:validators in
    check "origin accepted unsigned final state"
      (Result.is_error (verify ~state_root:changed_root ~max_bytes:(String.length changed) changed));
    let checked = Lwt_main.run (Anchor.verify_origin_lwt ~max_bytes:(String.length changed)
      ~trusted ~chain_id ~epoch_id ~state_root:changed_root ~set_root changed) in
    check "async origin accepted unsigned final state" (Result.is_error checked);
    let key = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 '\124')
      |> Result.map_error (fun _ -> "invalid origin test key") |> get in
    let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) |> Base64.encode_exn in
    let address = Octra_core.Crypto.Address.address_from_pubkey pubkey in
    let foreign = C.make_validator_set [C.{address; pubkey}] in
    ignore (Anchor.raw_validator_set foreign |> get);
    check "origin trusted supplied history set" (Result.is_error (verify ~trusted:foreign encoded));
    let task = Anchor.verify_origin_lwt ~max_bytes ~trusted ~chain_id ~epoch_id
      ~state_root ~set_root encoded in
    check "origin verification blocked caller" (Lwt.state task = Lwt.Sleep);
    check "async origin changed" (Lwt_main.run task = Ok expected);
    let stopped = Anchor.verify_origin_lwt ~cancelled:(fun () -> true)
      ~max_bytes ~trusted ~chain_id ~epoch_id ~state_root ~set_root encoded in
    check "origin started after cancellation"
      (Lwt.state stopped = Lwt.Return (Error "state sync verification cancelled"));
    let cancelled = ref false in
    let task = Anchor.verify_origin_lwt ~cancelled:(fun () -> !cancelled)
      ~max_bytes ~trusted ~chain_id ~epoch_id ~state_root ~set_root encoded in
    cancelled := true;
    check "origin cancellation was ignored"
      (Lwt_main.run task = Error "state sync verification cancelled"))

let test_proof_work root certificate =
  let encoded = match certificate.Manifest.authority with
    | Finalized encoded -> encoded
    | Checkpoint_quorum _ -> failwith "missing finality proof" in
  let anchor = get (Anchor.decode encoded) in
  with_store (Filename.concat root "proof") (fun store ->
    set store Update.pending_meta_key (String.make 2_999_000 'x');
    let proof = proof store 3 Update.pending_meta_key in
    let record, index_root = finality 3 initial proof in
    let transitions = Anchor.steps anchor in
    let step = Anchor.{ (List.hd transitions) with
      finalize = record.finalize; ledger_state_root = proof.ledger_state_root;
      epoch_index_root = index_root; proof = proof.proof } in
    let encoded = Anchor.make ~steps:(step :: List.tl transitions)
      ~finalize:(Anchor.finality anchor) ~validator_set:(Anchor.validator_set anchor)
      |> Anchor.encode in
    let expected = Error "validator transition state proof mismatch" in
    check "large proof rejection differs"
      (Anchor.verify ~validator_set:trusted certificate.checkpoint encoded = expected);
    let running = ref true in
    let previous = ref (Unix.gettimeofday ()) in
    let maximum = ref 0. in
    let rec pulse () =
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 0.001 in
      let now = Unix.gettimeofday () in
      maximum := max !maximum (now -. !previous);
      previous := now;
      if !running then pulse () else Lwt.return_unit in
    let result = Lwt_main.run (
      Lwt.async pulse;
      Lwt.finalize (fun () ->
        let open Lwt.Syntax in
        let* result = Anchor.verify_lwt ~validator_set:trusted certificate.checkpoint encoded in
        let* () = Lwt_unix.sleep 0.002 in
        Lwt.return result)
        (fun () -> running := false; Lwt.return_unit)) in
    check "async large proof rejection differs" (result = expected);
    Printf.printf "event = proof_work bytes = %d pause_ms = %.3f\n%!"
      (String.length proof.proof) (!maximum *. 1000.))

let test_limit () =
  Test_workspace.with_dir "sync_chain" (fun root ->
    let saved = saved (Filename.concat root "history") in
    test_proof_work root saved;
    let old_epoch = Int64.to_int saved.Manifest.checkpoint.epoch in
    let path = Filename.concat root "certificate.json" in
    Manifest.write_json path (Manifest.certificate_json saved);
    with_store (Filename.concat root "current") (fun store ->
      let chaindata = Store.open_chaindata (Filename.concat root "chaindata") in
      Fun.protect ~finally:(fun () -> Store.close chaindata) (fun () ->
        let old = update Anchor.max_steps in
        let old_raw = Update.to_string old in
        let old_set = get (Update.validator_set old) in
        set store Update.active_meta_key old_raw;
        set store Update.pending_meta_key old_raw;
        let old_proof = proof store old_epoch Update.active_meta_key in
        let old_record, old_index = finality old_epoch old_set old_proof in
        check "restored ledger root differs"
          (old_proof.ledger_state_root = saved.checkpoint.ledger_state_root);
        let entries = ref [old_record, old_index] in
        let deps = Chain.{
          data_dir = root; chain_id; store; chaindata;
          certificate_path = (fun () -> path);
          read_finality = (fun epoch ->
            match List.find_opt (fun (record, _) -> record.Journal.finalize.epoch_id = epoch) !entries with
            | Some (record, _) -> Journal.Valid record
            | None -> Journal.Missing);
        } in
        let build epoch validator_set = Lwt_main.run
          (Chain.build deps ~head_epoch:(Int64.of_int epoch) trusted validator_set) in
        check "stored chain at limit refused"
          (List.length (get (build old_epoch old_set)) = Anchor.max_steps);
        let next = update (Anchor.max_steps + 1) in
        let next_raw = Update.to_string next in
        let next_set = get (Update.validator_set next) in
        let epoch = old_epoch + 1 in
        set store Update.pending_meta_key next_raw;
        let pending = proof store epoch Update.pending_meta_key in
        let next_record, next_index = finality epoch old_set pending in
        entries := (next_record, next_index) :: !entries;
        List.iter (fun (record, root) ->
          Store.set_epoch_index_commitment_direct chaindata
            ~epoch_id:(Int64.to_int record.Journal.finalize.epoch_id)
            ~epoch_hash:(sha "index") ~root) !entries;
        set store Update.active_meta_key next_raw;
        let target = epoch + 2 in
        let current = proof store target Update.active_meta_key in
        let record, index_root = finality target next_set current in
        let steps = get (build target next_set) in
        check "stored extension exceeds limit instead of retrying trusted base"
          (List.length steps = 2);
        let compressed = certificate steps record index_root current.ledger_state_root in
        Manifest.write_json path (Manifest.certificate_json compressed);
        let reloaded = get (Manifest.load_certificate path) in
        ignore (get (Manifest.verify_certificate ~validator_set:trusted
          ~exporter_set:trusted reloaded));
        check "restarted chain changed" (get (build target next_set) = steps);
        Manifest.write_json path (Manifest.certificate_json saved);
        let invalid = Journal.{ old_record with
          finalize = C.{ old_record.finalize with
            precommits = List.map (fun (vote : C.vote) ->
              C.{ vote with signature = String.make 64 '\000' })
              old_record.finalize.precommits;
          };
        } in
        entries := [next_record, next_index; invalid, old_index];
        begin match build target next_set with
        | Error _ -> ()
        | Ok _ -> failwith "invalid bridge signature was accepted"
        end;
        entries := [next_record, next_index];
        begin match build target next_set with
        | Error _ -> ()
        | Ok _ -> failwith "missing short proof was accepted"
        end;
        let unchanged = get (Manifest.load_certificate path) in
        check "failed build replaced saved certificate"
          (Manifest.certificate_json unchanged = Manifest.certificate_json saved))))

exception Cycle_complete

exception Progress_failed

let test_watch deps prepared =
  let open Lwt.Infix in
  let module Archive = Octra_bootstrap.Sync_archive in
  List.iter (fun mode ->
    Test_workspace.with_dir "sync_watch" (fun root ->
      let started = Atomic.make false in
      let allowed = Atomic.make false in
      let finished = Atomic.make false in
      let clock = Mtime_clock.counter () in
      let tick, wake = Lwt.task () in
      let physical = ref (Lwt.return_ok 0L) in
      let watched = ref (Lwt.return_ok 0L) in
      let ticks = ref 0 in
      let deps = Publish.{ deps with
        sleep = (fun _ -> if mode = "clock" then raise Progress_failed else tick);
        info = (fun _ -> incr ticks; if mode = "log" then raise Progress_failed);
      } in
      let work = Archive.run_lwt root (fun owner ->
        let target = Archive.path owner "writing" in
        let stage = target ^ ".next" in
        Unix.mkdir stage 0o750;
        Archive.mark_stage owner "writing";
        let capture = Lwt_preemptive.detach (fun () ->
          Atomic.set started true;
          while not (Atomic.get allowed)
            && Mtime.Span.to_float_ns (Mtime_clock.count clock) < 5e9 do
            Thread.delay 0.001
          done;
          if not (Atomic.get allowed) then failwith "writer release timed out";
          In_channel.with_open_bin (Filename.concat stage ".writer")
            (fun channel -> ignore (In_channel.input_all channel));
          let output = open_out_bin (Filename.concat stage "ledger.dat") in
          Fun.protect ~finally:(fun () -> close_out_noerr output)
            (fun () -> output_string output "complete"; flush output;
              Unix.fsync (Unix.descr_of_out_channel output));
          Atomic.set finished true;
          if mode = "write" then failwith "writer failed";
          Ok 42L) () in
        physical := capture;
        let monitor = Publish.monitor_capture deps prepared target capture in
        watched := monitor;
        monitor) in
      let rec wait count =
        if Atomic.get started then Lwt.return_unit
        else if count = 0 then Lwt.fail_with "writer did not start"
        else Lwt_unix.sleep 0.001 >>= fun () -> wait (count - 1) in
      let settle task =
        Lwt.catch (fun () -> Lwt.protected task >|= fun _ -> ())
          (fun _ -> Lwt.return_unit) in
      Lwt_main.run (Lwt.finalize
        (fun () ->
          wait 2000 >>= fun () ->
          begin match mode with
          | "log" -> Lwt.wakeup wake ()
          | "timer" -> Lwt.wakeup_exn wake Progress_failed
          | "cancel" -> Lwt.cancel !watched
          | _ -> ()
          end;
          Lwt.pause () >>= fun () ->
          check "capture observer released active archive"
            (Archive.run root (fun _ -> ()) = Error "state sync archive is in use");
          if List.mem mode ["cancel"; "log"; "timer"; "clock"] then
            Lwt.cancel !watched;
          Lwt.pause () >>= fun () ->
          check "capture observer cancellation released active archive"
            (Archive.run root (fun _ -> ()) = Error "state sync archive is in use");
          check "capture observer completed before writer"
            (Lwt.state work = Lwt.Sleep && not (Atomic.get finished));
          Atomic.set allowed true;
          work >|= fun result ->
          check "capture observer lost writer completion" (Atomic.get finished);
          begin match mode, result with
          | ("log" | "timer" | "clock"), Error reason ->
              check "capture observer hid progress failure"
                (reason = Printexc.to_string Progress_failed)
          | "cancel", Error reason ->
              check "capture observer lost cancellation" (reason = Printexc.to_string Lwt.Canceled)
          | "write", Error reason ->
              check "capture observer hid write failure" (reason = Printexc.to_string (Failure "writer failed"))
          | "ok", Ok 42L -> ()
          | _ -> failwith "capture observer result differs"
          end;
          check "capture observer retained archive after completion"
            (Archive.run root (fun _ -> ()) = Ok ());
          check "capture observer lost written bytes"
            (In_channel.with_open_bin (Filename.concat root "writing.next/ledger.dat")
              In_channel.input_all = "complete");
          check "capture observer missed progress failure" (mode <> "log" || !ticks = 1))
        (fun () ->
          Atomic.set allowed true;
          settle !physical >>= fun () -> settle work))))
    ["log"; "timer"; "clock"; "cancel"; "write"; "ok"]

let test_publisher () =
  Test_workspace.with_dir "sync_publisher" (fun root ->
    with_store (Filename.concat root "irmin") (fun store ->
      set store "producer" "preserved";
      let proof = proof store 0 "producer" in
      let chain = Store.open_chaindata (Filename.concat root "chaindata") in
      Fun.protect ~finally:(fun () -> Store.close chain) (fun () ->
        let txid = Store.next_txid chain in
        check "producer input txid differs" (txid = 0L);
        let hash = sha "producer transaction" in
        let module Eic = Octra_core.Epoch_index_commitment in
        let index_hash, index_root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:0
          [Eic.item ~txid ~hash] in
        let record, _ = finality ~index_root 0 initial proof in
        Store.begin_batch chain;
        Store.save_tx chain ~hash ~epoch_id:0 ~from_addr:wallet.address
          ~to_addr:wallet.address ~tx_json:"{}" ~op_type:"standard"
          ~encrypted_data:"" ~message:"";
        Store.set_epoch chain Octra_core.Epochlog.{ empty_epoch_header with
          id = 0; start_txid = txid; tx_count = 1; finalized_at = 0.;
          state_root = Checkpoint.raw_to_hex record.finalize.header.proposed_state_root;
          proposer = { creator_addr = wallet.address; commit_round = 0 } };
        Store.set_epoch_index_commitment chain ~epoch_id:0 ~epoch_hash:index_hash ~root:index_root;
        Store.commit_batch chain;
        Store.fsync chain;
        let head = Lwt_main.run (Publish.epoch_head ~store ~chaindata:chain 0L record) |> get in
        let path = Filename.concat root "certificate.json" in
        let reads = ref 0 in
        let available = ref true in
        let deps = Publish.{
          data_dir = root; chain_id; store; chaindata = chain; wallet;
          force_publish = false;
          config_hash = (fun () -> Ok (sha "config"));
          trusted_validator_set = (fun () -> Ok trusted);
          head = (fun () -> Some head);
          read_finality = (fun epoch ->
            incr reads;
            if !available && epoch = 0L then Journal.Valid record else Journal.Missing);
          read_root = (fun _ -> failwith "genesis has no parent roots");
          exporter_set = (fun () -> Ok trusted);
          certificate_path = (fun () -> path);
          now = (fun () -> 1.);
          sleep = (fun seconds ->
            if seconds = 15. then Lwt.fail Cycle_complete else Lwt_unix.sleep seconds);
          info = ignore;
          warn = ignore;
        } in
        let run deps = Lwt_main.run (Lwt.catch
          (fun () -> Publish.run deps)
          (function Cycle_complete -> Lwt.return_unit | exn -> Lwt.fail exn)) in
        run deps;
        let certificate = Manifest.load_certificate path |> get in
        ignore (Manifest.verify_certificate ~validator_set:trusted ~exporter_set:trusted certificate |> get);
        check "producer captured wrong epoch" (certificate.checkpoint.epoch = 0L);
        check "producer changed ledger root" (certificate.checkpoint.ledger_state_root = proof.ledger_state_root);
        let count = !reads in
        run deps;
        check "producer did not reuse verified publication" (!reads = count);
        check "producer restart changed certificate"
          (Manifest.load_certificate path |> get = certificate);
        let run_cycle epoch published =
          let requested = ref [] in
          let deps = Publish.{ deps with
            chain_id = "octra-devnet-9871-cluster";
            head = (fun () -> Some Head.{ head with epoch_id = epoch });
            read_finality = (fun epoch ->
              requested := epoch :: !requested;
              Journal.Missing);
          } in
          Lwt_main.run (Lwt.catch
            (fun () -> Publish.run_loop deps ~force:false trusted
              (Octra_bootstrap.Sync_cycle.init ~published:(Some published)))
            (function Cycle_complete -> Lwt.return_unit | exn -> Lwt.fail exn));
          check "failed activation capture changed certificate"
            (Manifest.load_certificate path |> get = certificate);
          List.rev !requested
        in
        check "publisher requested a future activation"
          (run_cycle 1_662_999 1_662_840L = []);
        check "publisher waited past activation"
          (run_cycle 1_663_000 1_662_840L = [1_663_000L]);
        check "publisher requested image below activation"
          (run_cycle 1_663_001 1_662_840L = [1_663_001L; 1_663_000L]);
        check "publisher duplicated activation capture"
          (run_cycle 1_663_001 1_663_000L = []);
        let prepared = Lwt_main.run (Publish.prepare ~chain_id
          ~config_hash:(sha "config") ~trusted_validator_set:trusted
          ~validator_set:initial ~steps:[] ~head record) |> get in
        test_watch deps prepared;
        check "capture observer changed published certificate"
          (Manifest.load_certificate path |> get = certificate);
        Sync_retry_case.retry ~run ~validators:trusted deps certificate;
        Sync_retry_case.retention ~run deps certificate;
        Sync_retry_case.publication ~run ~validators:trusted deps certificate;
        Sync_retry_case.stages ~run deps certificate;
        Sync_retry_case.links ~run deps certificate;
        let prior = saved ~count:1 (Filename.concat root "prior") in
        Sync_retry_case.archived_links ~run deps prior;
        let count = !reads in
        let invalid = Manifest.{ certificate with authority = Finalized "invalid" } in
        Manifest.write_json path (Manifest.certificate_json invalid);
        available := false;
        run deps;
        check "producer accepted invalid stored authority" (!reads > count);
        check "failed producer replaced stored certificate"
          (Manifest.load_certificate path |> get = invalid))))

let () =
  match Array.to_list Sys.argv with
  | [_; "--origin"] ->
    test_origin ();
    print_endline "status = pass test = compute_origin"
  | [_; command] ->
    test_origin ();
    Test_workspace.with_dir "sync_cli" (fun root ->
      let certificate = saved ~count:5 ~width:650_000 (Filename.concat root "history") in
      Sync_cli_case.run ~command ~root ~validators:trusted certificate);
    test_repeated_sets ();
    test_limit ();
    test_publisher ();
    print_endline "status = pass test = sync_chain"
  | _ -> failwith "manifest cli path is required"