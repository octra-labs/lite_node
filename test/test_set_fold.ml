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

let reload state =
  let encoded = Fold.to_string state in
  let restored = Fold.of_string encoded |> Result.get_ok in
  expect "serialized fold bytes survive restoration"
    (Fold.to_string restored = encoded);
  restored

let pulse_fields address state =
  let open Yojson.Safe.Util in
  let pulse =
    Fold.to_yojson state
    |> member "members"
    |> to_list
    |> List.find (fun item -> member "address" item = `String address)
    |> member "phase"
    |> member "pulse"
  in
  (member "first" pulse |> to_string |> Int64.of_string),
  (member "last" pulse |> to_string |> Int64.of_string),
  (member "count" pulse |> to_int)

let credit_pulse epoch credit state =
  Fold.note_pulse ~credit Fold.participating ~epoch ~active:false
    ~address:"octA" state
  |> Result.get_ok
  |> reload

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
  let encoded = Fold.to_string restored in
  expect "receipt reads committed appeal"
    ((Fold.receipt ~address:"octA" restored).marked = [150L]);
  expect "receipt reads certificate mark"
    ((Fold.receipt ~address:"octB" restored).marked = [150L]);
  expect "absent member has no receipt"
    (Fold.receipt ~address:"missing" restored = Fold.{ marked = []; pulse = None });
  let pulse = Fold.note_pulse Fold.standard ~epoch:161L ~active:false
    ~address:"octA" restored |> Result.get_ok in
  expect "receipt reads confirmed pulse"
    (Fold.receipt ~address:"octA" pulse = Fold.{ marked = [150L]; pulse = Some 161L });
  expect "receipt does not mutate state" (Fold.to_string restored = encoded);
  expect "receipt survives codec"
    (Fold.receipt ~address:"octA" (Fold.of_string (Fold.to_string pulse) |> Result.get_ok)
     = Fold.receipt ~address:"octA" pulse);
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
  expect "expired pulse compacted" (member_count state = 4);
  let state = Fold.note_set cfg ~epoch:82L ~active:first state |> Result.get_ok in
  expect "compacted member reenters as live" (member_count state = 4);
  expect "compacted member remains eligible after reentry"
    (Fold.allows cfg ~start:0L ~source:82L ~address:"octA" state)

let pulse_load count =
  let cfg = Fold.participating in
  let senders = List.init count (fun index ->
    index mod 4, Printf.sprintf "octQueue%04d" index) in
  let initial =
    Fold.note_set cfg ~epoch:0L ~active:addresses Fold.empty
    |> Result.get_ok
  in
  let state =
    List.init 100 (fun step -> step + 100)
    |> List.fold_left (fun state height ->
      let epoch = Int64.of_int height in
      senders
      |> List.filter (fun (slot, _) -> slot = height mod 4)
      |> List.fold_left (fun state (_, address) ->
        Fold.note_pulse ~cap_mode:Fold.Prune cfg ~epoch ~active:false
          ~address state
        |> Result.get_ok) state
      |> Fold.to_string
      |> Fold.of_string
      |> Result.get_ok) initial
  in
  let eligible =
    List.filter
      (fun (_, address) -> Fold.allows cfg ~start:0L ~source:199L ~address state)
      senders
    |> List.length
  in
  let records = member_count state in
  expect "pulse load preserves the configured record limit"
    (records <= cfg.max_members * 4);
  let members =
    Fold.to_yojson state
    |> Yojson.Safe.Util.member "members"
    |> Yojson.Safe.Util.to_list
  in
  expect "pulse load preserves live members"
    (List.for_all (fun address ->
      List.exists (fun member ->
        Yojson.Safe.Util.member "address" member = `String address
        && (member
            |> Yojson.Safe.Util.member "phase"
            |> Yojson.Safe.Util.member "kind") = `String "live") members)
      addresses);
  Printf.printf
    "event = duty_load participants = %d eligible = %d records = %d epochs = 100\n%!"
    count eligible records;
  eligible

let check_pulse_load () =
  expect "confirmed pulses retain progress below capacity" (pulse_load 32 = 32);
  expect "current overflow policy resets every returning series"
    (pulse_load 1_000 = 0)

let check_credit_first () =
  let cfg = Fold.participating in
  let live = Fold.note_set cfg ~epoch:200L ~active:addresses Fold.empty
    |> Result.get_ok in
  let shadow = note_parent_cfg cfg 200L (parent 199L addresses) live
    |> Fold.note_set cfg ~epoch:200L ~active:["octB"; "octC"; "octD"]
    |> Result.get_ok |> reload in
  List.iter (fun initial ->
    let state = credit_pulse 201L 199L initial in
    expect "delayed first credit starts at execution"
      (pulse_fields "octA" state = (201L, 201L, 1));
    let state = credit_pulse 201L 200L state |> credit_pulse 201L 201L in
    expect "first epoch replay cannot add progress"
      (pulse_fields "octA" state = (201L, 201L, 1));
    let state = List.init 15 (fun index -> Int64.of_int (205 + index * 4))
      |> List.fold_left (fun state epoch -> credit_pulse epoch epoch state) state in
    expect "delayed first cannot finish before sixty four epochs"
      (not (Fold.allows cfg ~start:0L ~source:264L ~address:"octA" state));
    let state = credit_pulse 265L 265L state in
    expect "full execution span permits rejoin"
      (Fold.allows cfg ~start:0L ~source:265L ~address:"octA" state);
    expect "full span counts only new credit"
      (pulse_fields "octA" state = (201L, 265L, 17)))
    [Fold.empty; live; shadow]

let check_credit_duplicates () =
  let state = credit_pulse 100L 100L Fold.empty |> credit_pulse 104L 103L in
  expect "continuation uses credit rather than execution"
    (pulse_fields "octA" state = (100L, 103L, 2));
  List.iter (fun credit ->
    let next = credit_pulse 104L credit state in
    expect "duplicate and older credit preserve bytes"
      (Fold.to_string next = Fold.to_string state);
    expect "duplicate and older credit preserve count"
      (pulse_fields "octA" next = (100L, 103L, 2))) [103L; 102L];
  let later = credit_pulse 105L 103L state in
  expect "later duplicate does not extend the series"
    (pulse_fields "octA" later = (100L, 103L, 2));
  let reset = credit_pulse 112L 103L later in
  expect "old credit after a gap starts at execution"
    (pulse_fields "octA" reset = (112L, 112L, 1));
  List.iter (fun initial ->
    List.iter (fun active ->
      List.iter (fun credit ->
        expect "credit outside execution is rejected"
          (Result.is_error (Fold.note_pulse ~credit Fold.participating
            ~epoch:105L ~active ~address:"octA" initial)))
        [Int64.min_int; -1L; 106L; Int64.max_int]) [false; true])
    [Fold.empty; state];
  expect "execution cannot precede confirmed credit"
    (Result.is_error (Fold.note_pulse ~credit:102L Fold.participating
      ~epoch:102L ~active:false ~address:"octA" state))

let check_credit_gap () =
  let cfg = Fold.participating in
  expect "credit policy keeps gap eight and span sixty four"
    (cfg.pulse_gap = 8L && cfg.rejoin_span = 64L);
  let state = List.init 17 (fun index -> Int64.of_int (100 + index * 4))
    |> List.fold_left (fun state epoch -> credit_pulse epoch epoch state) Fold.empty in
  let delayed = credit_pulse 170L 168L state in
  expect "two epoch delivery without loss preserves credit"
    (pulse_fields "octA" delayed = (100L, 168L, 18));
  let loss_one = credit_pulse 172L 171L state in
  expect "one loss and one delay continues at gap eight"
    (pulse_fields "octA" loss_one = (100L, 171L, 18));
  expect "gap eight retains earned rejoin"
    (Fold.allows cfg ~start:0L ~source:172L ~address:"octA" loss_one);
  let loss_two = credit_pulse 173L 171L state in
  expect "one loss and two delay resets at gap nine"
    (pulse_fields "octA" loss_two = (173L, 173L, 1));
  expect "gap nine loses rejoin progress"
    (not (Fold.allows cfg ~start:0L ~source:173L ~address:"octA" loss_two))

let check_credit_timely () =
  let epochs =
    List.init 17 (fun index -> Int64.of_int (100 + index * 4))
    @ [164L; 172L; 181L; 181L; 185L]
  in
  ignore (List.fold_left (fun (prior, credited) epoch ->
    let prior = Fold.note_pulse Fold.participating ~epoch ~active:false
      ~address:"octA" prior |> Result.get_ok |> reload in
    let credited = credit_pulse epoch epoch credited in
    expect "explicit timely credit preserves default state bytes"
      (Fold.to_string credited = Fold.to_string prior);
    expect "explicit timely credit preserves eligibility"
      (Fold.allows Fold.participating ~start:0L ~source:epoch ~address:"octA" prior
       = Fold.allows Fold.participating ~start:0L ~source:epoch ~address:"octA" credited);
    prior, credited) (Fold.empty, Fold.empty) epochs)

let check_credit_capacity () =
  let cfg = Fold.participating in
  let live = List.init 36 (fun index -> Printf.sprintf "octLive%03d" index) in
  let shadow = List.init 764 (fun index -> Printf.sprintf "octShadow%04d" index) in
  let initial = Fold.note_set cfg ~epoch:100L ~active:live Fold.empty
    |> Result.get_ok in
  let state = List.fold_left (fun state address ->
    Fold.note_pulse ~credit:98L cfg ~epoch:100L ~active:false ~address state
    |> Result.get_ok) initial shadow |> reload in
  expect "capacity is eight hundred total records" (member_count state = 800);
  let shape state =
    let open Yojson.Safe.Util in
    let members = Fold.to_yojson state |> member "members" |> to_list in
    let active = List.filter (fun item ->
      (item |> member "phase" |> member "kind") = `String "live") members in
    expect "all thirty six live records survive capacity handling"
      (List.length active = 36 && List.for_all (fun address ->
        List.exists (fun item -> member "address" item = `String address) active) live);
    expect "capacity retains seven hundred sixty four shadow records"
      (List.length members - List.length active = 764)
  in
  shape state;
  let update cap_mode = List.fold_left (fun state address ->
    Fold.note_pulse ~cap_mode ~credit:102L cfg ~epoch:104L ~active:false
      ~address state |> Result.get_ok) state shadow |> reload in
  let reject = update Fold.Reject in
  let prune = update Fold.Prune in
  expect "full capacity modes agree without overflow"
    (Fold.to_string reject = Fold.to_string prune);
  List.iter (fun address ->
    expect "every shadow keeps delayed credit after restoration"
      (pulse_fields address prune = (100L, 102L, 2))) shadow;
  shape prune;
  let encoded = Fold.to_string reject in
  expect "record eight hundred one is rejected without pruning"
    (Result.is_error (Fold.note_pulse ~credit:102L cfg ~epoch:104L ~active:false
      ~address:"octNew" reject));
  expect "capacity rejection does not mutate original state"
    (Fold.to_string reject = encoded);
  let pruned = Fold.note_pulse ~cap_mode:Fold.Prune ~credit:102L cfg
    ~epoch:104L ~active:false ~address:"octNew" prune
    |> Result.get_ok |> reload in
  expect "overflow pruning keeps eight hundred total records"
    (member_count pruned = 800);
  shape pruned;
  expect "new shadow at capacity begins at execution"
    (pulse_fields "octNew" pruned = (104L, 104L, 1))

let check_delivery_pool () =
  let module Ready = Octra_core.Validator_ready_policy in
  List.iter (fun (epoch, head, accepted) ->
    expect "delivery admits only parent lag zero through two"
      (Ready.delivery ~epoch ~head = accepted))
    [0L, 0L, false; 1L, 0L, true; 1L, -1L, false;
     10L, 9L, true; 10L, 8L, true; 10L, 7L, true;
     10L, 6L, false; 10L, 10L, false; 10L, 11L, false;
     Int64.max_int, Int64.pred Int64.max_int, true];
  List.iter (fun reference ->
    expect "pool expiry matches next execution for nonfuture references"
      (Ready.expired ~head:9L ~reference = not (Ready.delivery ~epoch:10L ~head:reference)))
    [-1L; 0L; 6L; 7L; 8L; 9L];
  expect "future relay is held without becoming executable"
    (not (Ready.expired ~head:9L ~reference:10L)
     && not (Ready.delivery ~epoch:10L ~head:10L));
  expect "negative pool head is rejected" (Ready.expired ~head:(-1L) ~reference:0L)

let check_appeal_quorum () =
  let cfg = Fold.participating in
  let start = 100L in
  let keys = List.init 36 (fun index -> key (Printf.sprintf "octDuty%02d" index)) in
  let active = List.map (fun (item, _) -> item.C_types.address) keys in
  let signers = List.filteri (fun index _ -> index < 25) active in
  let missing = List.filteri (fun index _ -> index >= 25) active in
  let validator_set = C_types.make_weighted_validator_set
    (List.map (fun (item, _) -> item, Z.one) keys) |> Result.get_ok in
  let vote epoch proposal_id address =
    let _, private_key = List.find (fun (item, _) ->
      item.C_types.address = address) keys in
    let vote = C_types.{
      chain_id = "fold-test";
      epoch_id = epoch;
      round = 2;
      vote_type = Precommit;
      proposal_id;
      validator = address;
      signature = String.make 64 '\x00';
    } in
    { vote with signature = Mirage_crypto_ec.Ed25519.sign ~key:private_key
      (C_hash.vote_sign_bytes vote) }
  in
  let commit epoch =
    let header = { (header epoch) with creator_addr = List.hd signers } in
    let proposal_id = C_hash.proposal_id header in
    C_types.{ validator_set; certificate = {
      chain_id = "fold-test";
      epoch_id = epoch;
      commit_round = 2;
      header;
      proposal_id;
      precommits = List.map (vote epoch proposal_id) signers;
    } }
  in
  let advance epoch parent state =
    let state, reason, changed = Fold.advance cfg ~chain_id:"fold-test"
      ~start ~at:epoch ~parent:(Some parent) state |> Result.get_ok in
    expect "contiguous quorum advancement has no grace extension"
      (reason = None && changed);
    reload state
  in
  let warm = Int64.add start (Int64.add cfg.window cfg.challenge) in
  let measured = Int64.add warm cfg.delay in
  let total = Int64.to_int (Int64.sub measured start) + 160 in
  let _, _, _, checked = List.init total (fun index -> Int64.add start (Int64.of_int index))
    |> List.fold_left (fun (state, control, commits, checked) epoch ->
      let parent = commit (Int64.pred epoch) in
      expect "certificate contains exactly twenty five votes"
        (List.length parent.C_types.certificate.precommits = 25);
      if epoch = start then begin
        let short = { parent with C_types.certificate = {
          parent.certificate with precommits = List.tl parent.certificate.precommits
        } } in
        expect "twenty four of thirty six cannot certify a parent"
          (Result.is_error (Fold.read_parent ~chain_id:"fold-test" short))
      end;
      let control = advance epoch parent control in
      let commits = (Int64.pred epoch, parent) :: commits in
      let target = Int64.sub epoch 16L in
      let state = match List.assoc_opt target commits with
        | None -> state
        | Some proof_commit ->
          let head = Int64.sub epoch 3L in
          let reference = List.assoc head commits in
          let raw = reference.C_types.certificate.proposal_id in
          let proposal = String.concat "" (List.init (String.length raw)
            (fun index -> Printf.sprintf "%02x" (Char.code raw.[index]))) in
          expect "appeal delivery is exactly two epochs behind parent"
            (Int64.sub (Int64.pred epoch) head = 2L);
          expect "delayed proposal identity matches restored fold history"
            (Octra_core.Validator_ready_policy.reference ~epoch ~head
              ~proposal:(Some proposal) ~parent:(Some parent) ~state = Ok ());
          List.fold_left (fun state address ->
            let vote = vote target proof_commit.C_types.certificate.proposal_id address in
            let proof = Fold.{ vote; commit = proof_commit } in
            expect "appeal arrives at proof age sixteen"
              (Int64.sub epoch vote.C_types.epoch_id = cfg.challenge);
            expect "age seventeen is rejected independently of delivery"
              (Result.is_error (Fold.apply_proof cfg ~chain_id:"fold-test"
                ~epoch:(Int64.succ epoch) ~active:true ~address proof state));
            if epoch = Int64.add start 15L then begin
              let invalid = Fold.{ proof with vote = {
                vote with signature = String.make 64 '\x00'
              } } in
              expect "appeal requires the omitted signer signature"
                (Result.is_error (Fold.apply_proof cfg ~chain_id:"fold-test"
                  ~epoch ~active:true ~address invalid state))
            end;
            let state = Fold.apply_proof cfg ~chain_id:"fold-test" ~epoch
              ~active:true ~address proof state |> Result.get_ok in
            expect "authenticated appeal records the missing vote"
              (List.mem target (Fold.receipt ~address state).marked);
            state) state missing
      in
      let state = advance epoch parent state in
      let checked = if epoch < measured then checked else begin
        let safe_after = Fold.to_yojson state
          |> Yojson.Safe.Util.member "safe_after"
          |> Yojson.Safe.Util.to_string |> Int64.of_string in
        expect "eligibility is checked after warmup and delay"
          (epoch >= warm && Int64.sub epoch warm >= cfg.delay);
        expect "eligibility is not supplied by safety grace"
          (safe_after = 0L && epoch >= safe_after);
        expect "all thirty six remain eligible after authenticated appeals"
          (List.for_all (fun address -> Fold.allows cfg ~start ~source:epoch
            ~address state) active);
        expect "without appeals all eleven missing signers are excluded"
          (List.for_all (fun address -> not (Fold.allows cfg ~start ~source:epoch
            ~address control)) missing);
        expect "certificate signers remain eligible in the control"
          (List.for_all (fun address -> Fold.allows cfg ~start ~source:epoch
            ~address control) signers);
        expect "appeals do not create extra members" (member_count state = 36);
        checked + 1
      end in
      let commits = List.filter (fun (at, _) -> at >= target) commits in
      state, control, commits, checked) (Fold.empty, Fold.empty, [], 0)
  in
  expect "one hundred sixty post grace epochs have no exclusion" (checked = 160)

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

let check_slow_signer_cutoff () =
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

let check_removal_no_ratchet () =
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
  check_pulse_load ();
  check_credit_first ();
  check_credit_duplicates ();
  check_credit_gap ();
  check_credit_timely ();
  check_credit_capacity ();
  check_delivery_pool ();
  check_appeal_quorum ();
  check_participation_profile ();
  check_slow_signer_cutoff ();
  check_removal_no_ratchet ();
  check_advance ();
  Printf.printf "status = pass test = set_fold\n%!"