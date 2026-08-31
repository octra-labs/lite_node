(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Fold = Octra_core.Set_fold
module C_types = Octra_consensus.C_types
module C_hash = Octra_consensus.C_hash

let () = Mirage_crypto_rng_unix.use_default ()

let expect label condition =
  if not condition then failwith ("set_fold: " ^ label)

let key address =
  let private_key, public_key = Mirage_crypto_ec.Ed25519.generate () in
  C_types.{
    address;
    pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets public_key;
  }, private_key

let keys = List.map key ["octA"; "octB"; "octC"; "octD"]

let validator_set =
  C_types.make_weighted_validator_set
    (List.map (fun (item, _) -> item, Z.one) keys)
  |> Result.get_ok

let sign address message =
  let _, private_key =
    List.find (fun (item, _) -> item.C_types.address = address) keys
  in
  Mirage_crypto_ec.Ed25519.sign ~key:private_key message

let header epoch =
  C_types.{
    proto_version = proto_version_current;
    chain_id = "fold-test";
    epoch_id = epoch;
    prev_state_root = String.make 32 '\x11';
    tx_list_hash = C_hash.receipt_root [];
    receipt_root = C_hash.receipt_root [];
    proposed_state_root = String.make 32 '\x22';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "octB";
    txid_hi = 0L;
    ts = 0.;
  }

let signed_vote ~epoch ~round ~proposal_id address =
  let vote = C_types.{
    chain_id = "fold-test";
    epoch_id = epoch;
    round;
    vote_type = Precommit;
    proposal_id;
    validator = address;
    signature = String.make 64 '\x00';
  } in
  { vote with signature = sign address (C_hash.vote_sign_bytes vote) }

let parent epoch signers =
  let header = header epoch in
  let proposal_id = C_hash.proposal_id header in
  let round = 2 in
  C_types.{
    validator_set;
    certificate = {
      chain_id = "fold-test";
      epoch_id = epoch;
      commit_round = round;
      header;
      proposal_id;
      precommits =
        List.map (signed_vote ~epoch ~round ~proposal_id) signers;
    };
  }

let rotating_keys =
  List.map key
    ["octR0"; "octR1"; "octR2"; "octR3"; "octR4"; "octR5"; "octR6"]

let rotating_addresses =
  List.map (fun (item, _) -> item.C_types.address) rotating_keys

let rotating_key address =
  List.find
    (fun (item, _) -> String.equal item.C_types.address address)
    rotating_keys

let rotating_set addresses =
  C_types.make_weighted_validator_set
    (List.map
       (fun address ->
         let item, _ = rotating_key address in
         item, Z.one)
       addresses)
  |> Result.get_ok

let rotating_vote ~epoch ~round ~proposal_id address =
  let _, private_key = rotating_key address in
  let vote = C_types.{
    chain_id = "fold-test";
    epoch_id = epoch;
    round;
    vote_type = Precommit;
    proposal_id;
    validator = address;
    signature = String.make 64 '\x00';
  } in
  {
    vote with
    signature =
      Mirage_crypto_ec.Ed25519.sign
        ~key:private_key
        (C_hash.vote_sign_bytes vote);
  }

let rotating_parent validator_set epoch signers =
  let header = { (header epoch) with creator_addr = List.hd signers } in
  let proposal_id = C_hash.proposal_id header in
  let round = 2 in
  C_types.{
    validator_set;
    certificate = {
      chain_id = "fold-test";
      epoch_id = epoch;
      commit_round = round;
      header;
      proposal_id;
      precommits =
        List.map (rotating_vote ~epoch ~round ~proposal_id) signers;
    };
  }

let addresses = ["octA"; "octB"; "octC"; "octD"]

let candidate address =
  let item, _ =
    List.find (fun (item, _) -> item.C_types.address = address) keys
  in
  Octra_core.Validator_admission.{
    address;
    pubkey = item.pubkey;
    bond = Z.one;
    bonded_epoch = 0L;
    ready_epoch = Some 0L;
    exit_epoch = None;
  }

let rotating_candidate address =
  let item, _ = rotating_key address in
  Octra_core.Validator_admission.{
    address;
    pubkey = item.pubkey;
    bond = Z.one;
    bonded_epoch = 0L;
    ready_epoch = Some 0L;
    exit_epoch = None;
  }

let names candidates =
  List.map
    (fun (item : Octra_core.Validator_admission.candidate) -> item.address)
    candidates

let member_count state =
  match Fold.to_yojson state with
  | `Assoc fields ->
    begin
      match List.assoc_opt "members" fields with
      | Some (`List members) -> List.length members
      | _ -> failwith "set_fold: members encoding missing"
    end
  | _ -> failwith "set_fold: state encoding invalid"

let note_parent at parent state =
  let final, signers, _ = Fold.read_parent ~chain_id:"fold-test" parent |> Result.get_ok in
  Fold.note_final
    Fold.standard
    ~at
    ~active:addresses
    ~final
    ~signers
    state
  |> Result.get_ok

let note_parent_cfg cfg at parent state =
  let final, signers, _ =
    Fold.read_parent ~chain_id:"fold-test" parent |> Result.get_ok
  in
  Fold.note_final
    cfg
    ~at
    ~active:addresses
    ~final
    ~signers
    state
  |> Result.get_ok

let note_commit_cfg cfg at parent state =
  let final, signers, active =
    Fold.read_parent ~chain_id:"fold-test" parent |> Result.get_ok
  in
  Fold.note_final cfg ~at ~active ~final ~signers state |> Result.get_ok

let check_window_and_proof () =
  let state =
    Fold.note_set Fold.standard ~epoch:100L ~active:addresses Fold.empty
    |> Result.get_ok
  in
  let parent = parent 150L ["octB"; "octC"; "octD"] in
  let state = note_parent 151L parent state in
  let warm, _ =
    Fold.filter
      Fold.standard
      ~start:100L
      ~source:211L
      (List.map candidate addresses)
      state
  in
  expect "warm window keeps set" (names warm = addresses);
  let closed, _ =
    Fold.filter
      Fold.standard
      ~start:100L
      ~source:212L
      (List.map candidate addresses)
      state
  in
  expect "closed window shadows omitted signer"
    (names closed = ["octB"; "octC"; "octD"]);
  let proposal_id = parent.C_types.certificate.proposal_id in
  let vote = signed_vote ~epoch:150L ~round:2 ~proposal_id "octA" in
  let proof = Fold.{ vote; commit = parent } in
  let restored =
    Fold.apply_proof
      Fold.standard
      ~chain_id:"fold-test"
      ~epoch:160L
      ~active:true
      ~address:"octA"
      proof
      state
    |> Result.get_ok
  in
  let allowed, _ =
    Fold.filter
      Fold.standard
      ~start:100L
      ~source:212L
      (List.map candidate addresses)
      restored
  in
  expect "omitted vote restores signer" (names allowed = addresses);
  expect "expired proof rejected"
    (Result.is_error
       (Fold.apply_proof
          Fold.standard
          ~chain_id:"fold-test"
          ~epoch:167L
          ~active:true
          ~address:"octA"
          proof
          state));
  let wrong_vote = { vote with C_types.proposal_id = String.make 32 '\x33' } in
  let wrong_vote = {
    wrong_vote with
    signature = sign "octA" (C_hash.vote_sign_bytes wrong_vote);
  } in
  expect "wrong proposal rejected"
    (Result.is_error
       (Fold.apply_proof
          Fold.standard
          ~chain_id:"fold-test"
          ~epoch:160L
          ~active:true
          ~address:"octA"
          Fold.{ vote = wrong_vote; commit = parent }
          state));
  let bad_sig = { vote with C_types.signature = String.make 64 '\x44' } in
  expect "wrong signature rejected"
    (Result.is_error
       (Fold.apply_proof
          Fold.standard
          ~chain_id:"fold-test"
          ~epoch:160L
          ~active:true
          ~address:"octA"
          Fold.{ vote = bad_sig; commit = parent }
          state));
  let other_set =
    C_types.make_weighted_validator_set
      (List.map (fun (item, _) -> item, Z.of_int 2) keys)
    |> Result.get_ok
  in
  let wrong_set = { parent with C_types.validator_set = other_set } in
  expect "wrong set rejected"
    (Result.is_error
       (Fold.apply_proof
          Fold.standard
          ~chain_id:"fold-test"
          ~epoch:160L
          ~active:true
          ~address:"octA"
          Fold.{ vote; commit = wrong_set }
          state));
  let repeated = note_parent 151L parent state in
  expect "parent update is idempotent"
    (Fold.to_string repeated = Fold.to_string state)

let check_rejoin_and_delay () =
  let state =
    Fold.note_set Fold.standard ~epoch:100L ~active:addresses Fold.empty
    |> Result.get_ok
  in
  let state =
    Fold.note_set
      Fold.standard
      ~epoch:213L
      ~active:["octB"; "octC"; "octD"]
      state
    |> Result.get_ok
  in
  let rec pulse epoch state =
    if Int64.compare epoch 278L > 0 then state
    else
      Fold.note_pulse
        Fold.standard
        ~epoch
        ~active:false
        ~address:"octA"
        state
      |> Result.get_ok
      |> pulse (Int64.add epoch 8L)
  in
  let state = pulse 214L state in
  expect "stable pulse returns shadow member"
    (Fold.allows
       Fold.standard
       ~start:100L
       ~source:278L
       ~address:"octA"
       state);
  let delayed = Fold.delay Fold.standard ~at:200L state |> Result.get_ok in
  expect "gap delays live exclusion"
    (Fold.allows
       Fold.standard
       ~start:100L
       ~source:300L
       ~address:"octB"
       delayed);
  let decoded = Fold.of_string (Fold.to_string delayed) |> Result.get_ok in
  expect "state encoding is stable"
    (Fold.to_string decoded = Fold.to_string delayed)

let check_scale () =
  let candidates =
    List.init 200 (fun index ->
      Octra_core.Validator_admission.{
        address = Printf.sprintf "oct%03d" index;
        pubkey = String.make 32 (Char.chr (index land 255));
        bond = Z.one;
        bonded_epoch = 0L;
        ready_epoch = Some 0L;
        exit_epoch = None;
      })
  in
  let active = names candidates in
  let state =
    Fold.note_set Fold.standard ~epoch:0L ~active Fold.empty
    |> Result.get_ok
  in
  let allowed, counts =
    Fold.filter Fold.standard ~start:0L ~source:1L candidates state
  in
  expect "two hundred members retained" (List.length allowed = 200);
  expect "scale count retained" (counts.Fold.allowed = 200)

let check_compaction () =
  let cfg = Fold.{
    window = 64L;
    challenge = 8L;
    rejoin_span = 32L;
    pulse_gap = 4L;
    cadence = 8L;
    delay = 8L;
    max_members = 4;
    minimum = Octra_core.Validator_participation.Any;
  } in
  let first = ["octA"; "octB"; "octC"; "octD"] in
  let second = ["octE"; "octF"; "octG"; "octH"] in
  let state = Fold.note_set cfg ~epoch:0L ~active:first Fold.empty |> Result.get_ok in
  let final, signers, _ =
    Fold.read_parent ~chain_id:"fold-test" (parent 0L first)
    |> Result.get_ok
  in
  let state =
    Fold.note_final cfg ~at:1L ~active:first ~final ~signers state
    |> Result.get_ok
  in
  let state = Fold.note_set cfg ~epoch:2L ~active:second state |> Result.get_ok in
  expect "live challenge evidence retained" (member_count state = 8);
  let state = Fold.note_set cfg ~epoch:75L ~active:second state |> Result.get_ok in
  expect "expired shadow evidence compacted" (member_count state = 4);
  let state =
    Fold.note_pulse cfg ~epoch:76L ~active:false ~address:"octI" state
    |> Result.get_ok
  in
  expect "fresh pulse retained" (member_count state = 5);
  let state = Fold.note_set cfg ~epoch:80L ~active:second state |> Result.get_ok in
  expect "pulse retained at gap boundary" (member_count state = 5);
  let state = Fold.note_set cfg ~epoch:81L ~active:second state |> Result.get_ok in
  expect "stale pulse compacted" (member_count state = 4);
  let state = Fold.note_set cfg ~epoch:82L ~active:first state |> Result.get_ok in
  expect "compacted member reenters as live" (member_count state = 4);
  expect "compacted member remains eligible after reentry"
    (Fold.allows cfg ~start:0L ~source:82L ~address:"octA" state)

let participation_state signed =
  let cfg = Fold.participating in
  let state =
    Fold.note_set cfg ~epoch:100L ~active:addresses Fold.empty
    |> Result.get_ok
  in
  let rec loop epoch state =
    if Int64.compare epoch 132L > 0 then state
    else
      let signers =
        if Int64.compare epoch (Int64.add 100L (Int64.of_int signed)) <= 0 then
          ["octA"; "octB"; "octC"]
        else
          ["octB"; "octC"; "octD"]
      in
      note_parent_cfg cfg (Int64.succ epoch) (parent epoch signers) state
      |> loop (Int64.succ epoch)
  in
  loop 101L state

let check_participation_profile () =
  let cfg = Fold.participating in
  expect "profile is valid" (Fold.validate_cfg cfg = Ok ());
  expect "production evidence window is fixed"
    (Int64.equal cfg.window 32L);
  expect "production appeal window is fixed"
    (Int64.equal cfg.challenge 16L);
  expect "production snapshot cadence is fixed"
    (Int64.equal cfg.cadence 4L);
  expect "production activation delay is fixed"
    (Int64.equal cfg.delay 8L);
  expect "half window requires sixteen"
    (Octra_core.Validator_participation.required
       cfg.minimum
       ~epochs:32 = 16);
  expect "production replacement limit is fixed"
    (Int64.equal (Fold.replacement_limit cfg) 59L);
  let fifteen = participation_state 15 in
  let warm, _ =
    Fold.filter
      cfg
      ~start:100L
      ~source:147L
      (List.map candidate addresses)
      fifteen
  in
  expect "profile warmup retains every incumbent" (names warm = addresses);
  let excluded, _ =
    Fold.filter
      cfg
      ~start:100L
      ~source:148L
      (List.map candidate addresses)
      fifteen
  in
  expect "fifteen of thirty two is excluded"
    (names excluded = ["octB"; "octC"; "octD"]);
  let sixteen = participation_state 16 in
  let admitted, _ =
    Fold.filter
      cfg
      ~start:100L
      ~source:148L
      (List.map candidate addresses)
      sixteen
  in
  expect "sixteen of thirty two is admitted" (names admitted = addresses);
  let omitted_parent = parent 132L ["octB"; "octC"; "octD"] in
  let omitted_vote =
    signed_vote
      ~epoch:132L
      ~round:2
      ~proposal_id:omitted_parent.C_types.certificate.proposal_id
      "octA"
  in
  let restored =
    Fold.apply_proof
      cfg
      ~chain_id:"fold-test"
      ~epoch:134L
      ~active:true
      ~address:"octA"
      Fold.{ vote = omitted_vote; commit = omitted_parent }
      fifteen
    |> Result.get_ok
  in
  let appealed, _ =
    Fold.filter
      cfg
      ~start:100L
      ~source:148L
      (List.map candidate addresses)
      restored
  in
  expect "omitted signed vote satisfies participation" (names appealed = addresses)

let check_slow_signer_survives_finalized_cutoff () =
  let cfg = Fold.participating in
  let state =
    Fold.note_set cfg ~epoch:100L ~active:addresses Fold.empty
    |> Result.get_ok
  in
  let finish = Int64.add 148L (Int64.mul cfg.window 10L) in
  let rec loop epoch pending snapshots state =
    if Int64.compare epoch finish > 0 then snapshots, state
    else
      let commit = parent epoch ["octB"; "octC"; "octD"] in
      let at = Int64.succ epoch in
      let state = note_parent_cfg cfg at commit state in
      let state =
        match pending with
        | None -> state
        | Some proof ->
          Fold.apply_proof
            cfg
            ~chain_id:"fold-test"
            ~epoch:at
            ~active:true
            ~address:"octA"
            proof
            state
          |> Result.get_ok
      in
      let snapshots =
        if Int64.compare at 148L >= 0
           && Int64.rem (Int64.sub at 148L) cfg.cadence = 0L
        then begin
          let selected, _ =
            Fold.filter
              cfg
              ~start:100L
              ~source:at
              (List.map candidate addresses)
              state
          in
          expect "slow finalized signer remains selected"
            (names selected = addresses);
          snapshots + 1
        end else
          snapshots
      in
      let certificate = commit.C_types.certificate in
      let proof = Fold.{
        vote =
          signed_vote
            ~epoch
            ~round:certificate.commit_round
            ~proposal_id:certificate.proposal_id
            "octA";
        commit;
      } in
      loop (Int64.succ epoch) (Some proof) snapshots state
  in
  let snapshots, _ = loop 101L None 0 state in
  expect "slow signer survives ten evidence windows" (snapshots >= 80)

let check_single_removal_does_not_ratchet () =
  let cfg = Fold.participating in
  let offline = List.hd rotating_addresses in
  let survivors = List.tl rotating_addresses in
  let initial_set = rotating_set rotating_addresses in
  let survivor_set = rotating_set survivors in
  let state =
    Fold.note_set cfg ~epoch:100L ~active:rotating_addresses Fold.empty
    |> Result.get_ok
  in
  let rec establish epoch state =
    if Int64.compare epoch 155L > 0 then state
    else
      rotating_parent initial_set epoch survivors
      |> fun commit -> note_commit_cfg cfg (Int64.succ epoch) commit state
      |> establish (Int64.succ epoch)
  in
  let state = establish 101L state in
  let selected, _ =
    Fold.filter
      cfg
      ~start:100L
      ~source:148L
      (List.map rotating_candidate rotating_addresses)
      state
  in
  expect "one inactive validator is removed"
    (names selected = survivors && not (List.mem offline (names selected)));
  let state =
    Fold.note_set cfg ~epoch:156L ~active:survivors state |> Result.get_ok
  in
  let first_snapshot = 204L in
  let last_snapshot =
    Int64.add first_snapshot (Int64.mul 9L cfg.cadence)
  in
  let finish = Int64.add last_snapshot 1L in
  let rec rotate epoch pending snapshots state =
    if Int64.compare epoch finish > 0 then snapshots
    else
      let omitted_index =
        Int64.rem
          (Int64.sub epoch 156L)
          (Int64.of_int (List.length survivors))
        |> Int64.to_int
      in
      let omitted = List.nth survivors omitted_index in
      let signers = List.filter (fun address -> address <> omitted) survivors in
      let commit = rotating_parent survivor_set epoch signers in
      let at = Int64.succ epoch in
      let state = note_commit_cfg cfg at commit state in
      let state =
        match pending with
        | None -> state
        | Some (address, proof) ->
          Fold.apply_proof
            cfg
            ~chain_id:"fold-test"
            ~epoch:at
            ~active:true
            ~address
            proof
            state
          |> Result.get_ok
      in
      let snapshots =
        if Int64.compare at first_snapshot >= 0
           && Int64.compare at last_snapshot <= 0
           && Int64.rem (Int64.sub at first_snapshot) cfg.cadence = 0L
        then begin
          let next, _ =
            Fold.filter
              cfg
              ~start:100L
              ~source:at
              (List.map rotating_candidate survivors)
              state
          in
          expect "rotating finalized cutoff does not shrink survivors"
            (names next = survivors);
          snapshots + 1
        end else
          snapshots
      in
      let certificate = commit.C_types.certificate in
      let proof = Fold.{
        vote =
          rotating_vote
            ~epoch
            ~round:certificate.commit_round
            ~proposal_id:certificate.proposal_id
            omitted;
        commit;
      } in
      rotate (Int64.succ epoch) (Some (omitted, proof)) snapshots state
  in
  expect "ten stable snapshots follow one removal"
    (rotate 156L None 0 state = 10)

let check_advance () =
  expect "missing parent rejected"
    (Fold.advance
       Fold.standard
       ~chain_id:"fold-test"
       ~start:100L
       ~at:100L
       ~parent:None
       Fold.empty
     = Error "validator duty parent commit missing");
  let parent_99 = parent 99L ["octA"; "octB"; "octC"] in
  let state, reason, changed =
    Fold.advance
      Fold.standard
      ~chain_id:"fold-test"
      ~start:100L
      ~at:100L
      ~parent:(Some parent_99)
      Fold.empty
    |> Result.get_ok
  in
  expect "first advance changes state" changed;
  expect "first advance has no delay" (reason = None);
  expect "parent set defines members" (member_count state = 4);
  let repeated, repeated_reason, repeated_changed =
    Fold.advance
      Fold.standard
      ~chain_id:"fold-test"
      ~start:100L
      ~at:100L
      ~parent:(Some parent_99)
      state
    |> Result.get_ok
  in
  expect "same epoch advance is unchanged" (not repeated_changed);
  expect "same epoch advance has no delay" (repeated_reason = None);
  expect "same epoch bytes are stable"
    (Fold.to_string repeated = Fold.to_string state);
  let parent_101 = parent 101L ["octA"; "octB"; "octC"] in
  let skipped, skipped_reason, _ =
    Fold.advance
      Fold.standard
      ~chain_id:"fold-test"
      ~start:100L
      ~at:102L
      ~parent:(Some parent_101)
      state
    |> Result.get_ok
  in
  expect "height gap is visible" (skipped_reason = Some "epoch_gap");
  expect "height gap preserves live member"
    (Fold.allows
       Fold.standard
       ~start:100L
       ~source:200L
       ~address:"octD"
       skipped)

let () =
  check_window_and_proof ();
  check_rejoin_and_delay ();
  check_scale ();
  check_compaction ();
  check_participation_profile ();
  check_slow_signer_survives_finalized_cutoff ();
  check_single_removal_does_not_ratchet ();
  check_advance ();
  Printf.printf "status = pass test = set_fold\n%!"