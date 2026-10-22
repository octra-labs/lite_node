(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module F = Octra_core.Set_fold
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module S = Octra_core.Store_irmin
module E = Octra_core.Epoch_exec
module G = Octra_core.Rule_graph
module D = Octra_node_runtime.Set_delivery

let run = Lwt_main.run
let ok = function Ok value -> value | Error reason -> failwith reason
let expect label value = if not value then failwith label
let cfg = F.participating
let chain_id = "marks-test"
let keys = List.init 4 (fun index ->
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 (Char.chr (65 + index)))
    |> Result.get_ok in
  let pubkey = Mirage_crypto_ec.Ed25519.pub_of_priv key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets in
  let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
  C.{address; pubkey}, key)
let validators = C.make_weighted_validator_set
  (List.map (fun (validator, _) -> validator, Z.one) keys) |> ok
let address = (fst (List.nth keys 3)).C.address
let active = List.map (fun (validator, _) -> validator.C.address) keys

let vote ?(round = 2) ?(kind = C.Precommit) ?(chain = chain_id) ~epoch ~proposal_id who =
  let _, key = List.find (fun (validator, _) -> validator.C.address = who) keys in
  let value = C.{chain_id = chain; epoch_id = epoch; round; vote_type = kind;
    proposal_id; validator = who; signature = ""} in
  {value with signature = Mirage_crypto_ec.Ed25519.sign ~key (H.vote_sign_bytes value)}

let parent ?(root = String.make 32 's') epoch =
  let header = C.{proto_version = proto_version_current; chain_id; epoch_id = epoch;
    prev_state_root = String.make 32 'p'; tx_list_hash = H.tx_list_hash [];
    receipt_root = H.receipt_root []; proposed_state_root = root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = List.hd active; txid_hi = 0L; ts = 0.} in
  let proposal_id = H.proposal_id header in
  C.{validator_set = validators; certificate = {chain_id; epoch_id = epoch;
    commit_round = 2; header; proposal_id;
    precommits = List.init 3 (fun index -> vote ~epoch ~proposal_id (List.nth active index))}}

let proof epoch =
  let commit = parent epoch in
  F.{commit; vote = vote ~epoch ~proposal_id:commit.certificate.proposal_id address}

let record ?(marks = true) state commit =
  let final, signers, active = F.read_parent ~marks ~chain_id commit |> ok in
  F.note_final cfg ~at:(Int64.succ final.epoch) ~active ~final ~signers state |> ok

let state = List.init 15 (fun index -> Int64.of_int (84 + index))
  |> List.fold_left (fun state epoch -> record state (parent epoch)) F.empty
let claims = List.init 15 (fun index ->
  let proof = proof (Int64.of_int (84 + index)) in
  F.{epoch = proof.vote.epoch_id; signature = proof.vote.signature})
let sent = List.filter (fun (claim : F.mark_claim) -> claim.epoch >= 86L) claims
let sets = List.map (fun (claim : F.mark_claim) -> claim.epoch, chain_id, validators) claims
let apply ?(at = 100L) ?(keys = sets) claims =
  F.apply_mark_claims cfg ~chain_id ~epoch:at ~active:true ~address ~sets:keys claims state

let check_wire () =
  let encoded = F.encode_marks claims in
  expect "claim binary size changed" (String.length (Base64.decode_exn encoded) = 15 * 72);
  expect "claim round trip differs" (F.decode_marks cfg encoded = Ok claims);
  List.iter (fun raw -> expect "invalid claim encoding accepted"
    (Result.is_error (F.decode_marks cfg raw)))
    [""; encoded ^ "\n"; Base64.encode_exn "a";
     F.encode_marks (claims @ [List.hd claims])];
  let fields = D.mark_fields ~epoch:100L ~chain_id ~address ~state
    (List.init 15 (fun index -> proof (Int64.of_int (84 + index)))) |> ok in
  let selected = match fields with
    | ["duty_marks", `String value] -> F.decode_marks cfg value |> ok
    | _ -> failwith "validator marks are missing" in
  expect "mark expiring during delivery was selected"
    (List.map (fun (claim : F.mark_claim) -> claim.epoch) selected
      = List.init 13 (fun index -> Int64.of_int (86 + index)));
  let message = Yojson.Safe.to_string (`Assoc (fields @ [
    "consensus_pubkey", `String (Base64.encode_exn (fst (List.nth keys 3)).pubkey);
    "head_epoch", `String "99"; "state_root", `String (String.make 64 'a');
    "chain_id", `String chain_id; "config_hash", `String (String.make 64 'c');
    "catchup_head_epoch", `String "99"; "head_proposal_id", `String (String.make 64 'b')])) in
  expect "claims not encoded" (F.evidence_of_message ~marks:true cfg (Some message) = Ok (F.Marks sent));
  expect "prior behavior changed" (F.evidence_of_message ~marks:false cfg (Some message) = Ok F.Pulse);
  let overlap = Yojson.Safe.to_string (`Assoc (fields @ D.proof_fields (proof 98L))) in
  expect "overlapping proofs accepted"
    (Result.is_error (F.evidence_of_message ~marks:true cfg (Some overlap)));
  let duplicate = Yojson.Safe.to_string (`Assoc (fields @ fields)) in
  expect "duplicate fields accepted"
    (Result.is_error (F.evidence_of_message ~marks:true cfg (Some duplicate)));
  let tx = Octra_core.Transaction.{from = address; to_ = address; amount = Z.zero;
    nonce = 1; ou = Z.of_int 1000; timestamp = 1.; signature = "";
    public_key = Some (Base64.encode_exn (fst (List.nth keys 3)).pubkey);
    message = Some message; op_type = ValidatorReady; encrypted_data = None} in
  let secret = Mirage_crypto_ec.Ed25519.priv_to_octets (snd (List.nth keys 3))
    |> Base64.encode_exn in
  let tx = Octra_core.Transaction.sign_with_privkey tx secret in
  expect "outer signature invalid" (Octra_core.Transaction.verify tx (Option.get tx.public_key));
  expect "claim message exceeds published wire rule" (Octra_core.Tx_payload.admit tx = Ok ());
  expect "marks expired before challenge"
    (not (Octra_core.Tx_staging.duty_expired ~marks:true ~mode:G.Active ~head:(Some 99L) tx));
  expect "marks expired before delivery window"
    (not (Octra_core.Tx_staging.duty_expired ~marks:true ~mode:G.Active
      ~head:(Some 100L) tx));
  expect "marks survived challenge"
    (Octra_core.Tx_staging.duty_expired ~marks:true ~mode:G.Active ~head:(Some 102L) tx);
  expect "marks changed prior selection"
    (not (Octra_core.Tx_staging.duty_expired ~marks:false ~mode:G.Active ~head:(Some 100L) tx));
  Printf.printf "event = duty_marks count = 15 message_bytes = %d tx_bytes = %d\n"
    (String.length message) (String.length (Yojson.Safe.to_string (Octra_core.Transaction.to_yojson tx)))

let check_large () =
  let added = List.init 196 (fun index ->
    let seed = Digestif.SHA256.(digest_string ("mark-key-" ^ string_of_int index) |> to_raw_string) in
    let key = Mirage_crypto_ec.Ed25519.priv_of_octets seed |> Result.get_ok in
    let pubkey = Mirage_crypto_ec.Ed25519.pub_of_priv key
      |> Mirage_crypto_ec.Ed25519.pub_to_octets in
    C.{address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey); pubkey}, key) in
  let members = keys @ added in
  let validators = C.make_weighted_validator_set
    (List.map (fun (validator, _) -> validator, Z.one) members) |> ok in
  let proofs = List.map (fun (claim : F.mark_claim) ->
    let small = proof claim.epoch in
    let signers = List.filter (fun (validator, _) -> validator.C.address <> address) members
      |> List.mapi (fun index value -> index, value)
      |> List.filter_map (fun (index, value) -> if index < 134 then Some value else None) in
    let precommits = List.map (fun (validator, key) ->
      let vote = {small.vote with C.validator = validator.C.address} in
      {vote with C.signature = Mirage_crypto_ec.Ed25519.sign ~key (H.vote_sign_bytes vote)}) signers in
    F.{small with commit = {validator_set = validators;
      certificate = {small.commit.certificate with precommits}}}) claims in
  let state = List.fold_left (fun state (proof : F.proof) -> record state proof.commit) F.empty proofs in
  let fields = D.mark_fields ~epoch:100L ~chain_id ~address ~state proofs |> ok in
  expect "wire grows with validator set" (fields = ["duty_marks", `String (F.encode_marks sent)]);
  let sets = List.map (fun (claim : F.mark_claim) -> claim.epoch, chain_id, validators) claims in
  let checked = F.apply_mark_claims cfg ~chain_id ~epoch:100L ~active:true ~address ~sets claims state |> ok in
  expect "large set marks missing" (List.length (F.receipt ~address checked).marked = 15);
  let old = List.nth proofs 13 in
  let current = List.nth proofs 14 in
  let mixed = record (record ~marks:false F.empty old.commit) current.commit in
  let selected = D.mark_fields ~epoch:100L ~chain_id ~address ~state:mixed [old; current] |> ok in
  expect "oversized commit blocks available marks" (List.mem_assoc "duty_marks" selected)

let check_signatures () =
  let next = apply claims |> ok in
  expect "batch marks missing" ((F.receipt ~address next).marked = List.map
    (fun (claim : F.mark_claim) -> claim.epoch) claims);
  let repeated = F.apply_mark_claims cfg ~chain_id ~epoch:100L ~active:true ~address
    ~sets claims next |> ok in
  expect "repeat added state" (F.to_string next = F.to_string repeated);
  expect "descending claims accepted" (Result.is_error (apply (List.rev claims)));
  expect "duplicate epoch accepted" (Result.is_error (apply [List.hd claims; List.hd claims]));
  expect "expired claim accepted" (Result.is_error (apply ~at:101L claims));
  let newest = List.hd (List.rev claims) in
  expect "one epoch old claim accepted" (Result.is_error (apply ~at:99L [newest]));
  let good = List.hd claims in
  let other ?round ?kind ?chain ?(proposal_id = (parent good.epoch).certificate.proposal_id) who =
    let value = vote ?round ?kind ?chain ~epoch:good.epoch ~proposal_id who in
    {good with F.signature = value.signature} in
  List.iter (fun bad ->
    let before = F.to_string state in
    expect "wrong signature accepted" (Result.is_error (apply (bad :: List.tl claims)));
    expect "failed batch changed marks" (before = F.to_string state))
    [{good with signature = String.make 64 '\000'};
     other ~round:3 address; other ~kind:C.Prevote address;
     other ~chain:"other-chain" address; other ~proposal_id:(String.make 32 'x') address;
     other (List.hd active)];
  expect "missing set accepted" (Result.is_error (apply ~keys:[] [good]));
  expect "wrong slot epoch accepted" (Result.is_error (apply
    ~keys:[Int64.succ good.epoch, chain_id, validators] [good]));
  expect "wrong set chain accepted" (Result.is_error (apply
    ~keys:[good.epoch, "other-chain", validators] [good]));
  let changed = C.make_weighted_validator_set (List.map (fun (validator, _) ->
    (if validator.C.address = address then {validator with C.pubkey = String.make 32 'x'}
     else validator), Z.one) keys) |> ok in
  expect "changed historic key accepted" (Result.is_error (apply
    ~keys:[good.epoch, chain_id, changed] [good]));
  let weighted = C.make_weighted_validator_set (List.mapi (fun index (validator, _) ->
    validator, Z.of_int (index + 1)) keys) |> ok in
  expect "different historical set hash accepted" (Result.is_error (apply
    ~keys:[good.epoch, chain_id, weighted] [good]));
  let wire = F.to_string state in
  expect "final round restore differs" (F.to_string (F.of_string wire |> ok) = wire);
  let prior = record ~marks:false F.empty (parent 98L) in
  expect "prior final gained round" ((Option.get (F.find_final 98L prior)).round = None);
  let finals = Yojson.Safe.Util.member "finals" (F.to_yojson prior) in
  expect "prior final bytes changed" (finals = `List [`Assoc [
    "epoch", `String "98";
    "proposal_id", `String (Base64.encode_exn (parent 98L).certificate.proposal_id);
    "set_hash", `String (Base64.encode_exn (Octra_consensus.C_config.validator_set_hash validators))]]);
  let after = record prior (parent 99L) in
  expect "activation parent round missing" ((Option.get (F.find_final 99L after)).round = Some 2);
  let encoded = D.mark_fields ~epoch:101L ~chain_id ~address ~state:after [proof 98L; proof 99L] |> ok in
  expect "mark held behind earlier commit" (encoded = ["duty_marks", `String
    (F.encode_marks [{epoch = 99L; signature = (proof 99L).vote.signature}])]);
  let earlier = D.mark_fields ~epoch:101L ~chain_id ~address ~state:after [proof 98L] |> ok in
  expect "prior commit lost" (List.mem_assoc "duty_commit" earlier);
  List.iter (fun chain_id -> expect "unapproved activation"
    (G.duty_marks_at ~chain_id ~epoch:max_int = G.Prior))
    [chain_id; "octra-mainnet"];
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Option.get (G.duty_marks_activation_for_chain chain_id) in
  expect "devnet marks activate"
    (G.duty_marks_at ~chain_id ~epoch:plan.activation_epoch = G.Active)

let with_store path f =
  let store = run (S.open_store path) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () -> f store)

let check_snapshot () = Test_workspace.with_dir "marks-snapshot" (fun dir ->
  with_store (Filename.concat dir "state") (fun store ->
    run (S.set_meta store F.meta_key (F.to_string state));
    run (S.set_meta store "last_epoch" "99");
    run (S.tag_epoch store 99);
    let ledger_root = run (S.state_hash store) in
    let index_root = String.make 64 'a' in
    let state_root = Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:ledger_root ~epoch_index_root:index_root in
    expect "test requires different roots" (ledger_root <> state_root);
    let head = Octra_core.Head_manifest.{schema_version; generation = 1; epoch_id = 99;
      state_root; ledger_state_root = Some ledger_root; irmin_commit = None;
      txid_hi = 0L; txlog_seg = None; txlog_off = None; epochlog_off = None;
      commit_id = "marks-snapshot"; ts = 1.; quorum_cert_hash = None;
      epoch_index_hash = None; epoch_index_root = Some index_root} in
    let current = ref (Some head) in
    let proofs = List.map (fun (claim : F.mark_claim) -> proof claim.epoch) claims in
    let build ?(read = fun () -> !current) () = run (D.mark_fields_at ~store
      ~head:read ~epoch:100L ~state_root ~chain_id ~address proofs) in
    let fields = build () |> ok in
    expect "snapshot marks differ" (fields = ["duty_marks", `String (F.encode_marks sent)]);
    List.iter (fun value ->
      current := value;
      expect "invalid snapshot accepted" (Result.is_error (build ())))
      [None; Some {head with epoch_id = 98}; Some {head with state_root = String.make 64 'b'};
       Some {head with ledger_state_root = None};
       Some {head with ledger_state_root = Some (String.make 64 'c')}];
    current := Some head;
    let reads = ref 0 in
    expect "moving head accepted" (Result.is_error (build ~read:(fun () ->
      incr reads;
      Some (if !reads = 1 then head else {head with generation = 2})) ()))))

let env = E.{chain_id; epoch_id = 100; proposer_addr = List.hd active;
  validator_addrs = active; validator_pubkeys = []; prev_state_root = String.make 64 '0';
  epoch_ts = 1.; ready_state_root_at = None; ready_max_lag = -1}

let backend store =
  let fold epoch = E.prior_fold epoch |> Result.map (fun ctx ->
    {ctx with E.mode = G.Active; standard_mode = G.Active; duty_marks = G.Active;
      vm_bytes = G.Active; members = active}) in
  E.make_live_backend ~fold store (Octra_core.Ledger.create store)

let check_restore () = Test_workspace.with_dir "duty_marks" (fun dir ->
  with_store (Filename.concat dir "source") (fun store ->
    run (S.set_meta store "last_epoch" "98");
    let root = run (S.get_head_hash store) |> Option.get
      |> fun hash -> Digestif.SHA256.(digest_string hash |> to_hex) in
    let root_raw = Digestif.SHA256.(to_raw_string (of_hex root)) in
    let commit = parent ~root:root_raw 98L in
    let state = record F.empty commit in
    let claim = F.{epoch = 98L; signature = (vote ~epoch:98L
      ~proposal_id:commit.certificate.proposal_id address).signature} in
    run (S.begin_epoch_batch store);
    run (Octra_core.Compute_input.remember store ~epoch:99 ~state:root commit);
    run (S.set_meta store F.meta_key (F.to_string state));
    run (S.set_meta store "last_epoch" "99");
    run (S.commit_epoch_batch store "marks state");
    let apply store evidence = run (E.update_ready_fold ~backend:(backend store) ~env
      ~head_epoch:99L ~address evidence) |> ok in
    let expected = apply store (F.Marks [claim]) |> Option.get |> F.to_string in
    let prior = record ~marks:false F.empty commit in
    run (S.set_meta store F.meta_key (F.to_string prior));
    let legacy = F.Commit F.{commit; vote = vote ~epoch:98L
      ~proposal_id:commit.certificate.proposal_id address} in
    expect "transition lost commit evidence" (Option.is_some (apply store legacy));
    expect "transition accepted mark without saved round" (Result.is_error
      (run (E.update_ready_fold ~backend:(backend store) ~env ~head_epoch:99L
        ~address (F.Marks [claim]))));
    expect "expired transition evidence accepted" (Result.is_error
      (run (E.update_ready_fold ~backend:(backend store) ~env:{env with epoch_id = 115}
        ~head_epoch:114L ~address legacy)));
    run (S.set_meta store F.meta_key (F.to_string state));
    expect "mark with final round retained commit proof"
      (Result.is_error (run (E.update_ready_fold ~backend:(backend store) ~env
        ~head_epoch:99L ~address (F.Commit F.{commit; vote = vote ~epoch:98L
          ~proposal_id:commit.certificate.proposal_id address}))));
    let commit_hash = run (S.get_commit_hash store) |> Option.get in
    let expected_root = run (S.get_head_hash store) |> Option.get in
    let roots = ref [] in
    List.iteri (fun index format ->
      let image = Filename.concat dir (Printf.sprintf "image-%d" index) in
      let target = Filename.concat dir (Printf.sprintf "restored-%d" index) in
      ignore (run (Octra_core.Ledger_image.write ~format store ~commit:commit_hash ~path:image) |> ok);
      ignore (run (Octra_core.Ledger_image.restore ~source:image ~target ~expected_root) |> ok);
      with_store target (fun restored ->
        expect "snapshot verification differs" (apply restored (F.Marks [claim])
          |> Option.get |> F.to_string = expected);
        run (S.set_meta restored F.meta_key expected);
        roots := run (S.get_head_hash restored) :: !roots;
        run (S.remove_path restored ["compute"; "roots";
          string_of_int (98 mod Octra_core.Compute_input.window)]);
        expect "missing historic state accepted" (Result.is_error
          (run (E.update_ready_fold ~backend:(backend restored) ~env ~head_epoch:99L
            ~address (F.Marks [claim]))))))
      [Octra_core.Ledger_image.Flat; Octra_core.Ledger_image.Shared];
    expect "replayed state roots differ" (match !roots with
      | [Some left; Some right] -> left = right
      | _ -> false)))

let check_actor () =
  let module A = Octra_node_runtime.Set_actor in
  let sample = ref A.{epoch = 99L; active = false; bonded = Ok false} in
  let sent = ref [] in
  let actor = A.create ~batch:(fun () -> true) A.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ -> Lwt.return_ok F.{marked = []; pulse = None});
    peers = (fun () -> 1);
    send = (fun ~epoch:_ action -> sent := action :: !sent; Lwt.return_ok ());
    warn = (fun _ reason -> failwith reason);
  } in
  let rec turns n = if n = 0 then Lwt.return_unit else
    Lwt.bind (Lwt.pause ()) (fun () -> turns (n - 1)) in
  List.iter (fun (claim : F.mark_claim) ->
    let proof = proof claim.epoch in
    expect "actor notification refused"
      (A.notify actor ~epoch:99L (Some (proof.vote, proof.commit)) = A.Accepted)) claims;
  run (turns 100);
  expect "actor lost saved votes" ((run (A.stats actor)).appeals = 15);
  sample := A.{epoch = 100L; active = true; bonded = Ok true};
  ignore (A.wake actor ~head:99);
  run (turns 100);
  expect "actor did not group saved votes" (match !sent with
    | [A.Appeals proofs] -> List.map (fun (proof : F.proof) -> proof.vote.epoch_id) proofs
      = List.map (fun (claim : F.mark_claim) -> claim.epoch) claims
    | _ -> false);
  run (A.shutdown actor)

let () =
  check_wire ();
  check_large ();
  check_signatures ();
  check_snapshot ();
  check_restore ();
  check_actor ();
  print_endline "event = duty_marks status = pass"