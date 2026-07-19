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


open C_types


type validator_info = C_types.validator_info
type validator_set = C_types.validator_set

let make_validator_set = C_types.make_validator_set
let leader_of = C_types.leader_of

let short_hex_raw s =
  let hex = Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string s) in
  String.sub hex 0 (min 16 (String.length hex))

let short_addr s =
  String.sub s 0 (min 14 (String.length s))

let hash32_to_hex h =
  if String.length h = 64 then String.lowercase_ascii h
  else String.concat "" (List.init (String.length h) (fun i ->
    Printf.sprintf "%02x" (Char.code h.[i])))

let log_pending_header_quorum () =
  match Sys.getenv_opt "OCTRA_BFT_LOG_PENDING_HEADER_QUORUM" with
  | Some v ->
    let v = String.lowercase_ascii (String.trim v) in
    v = "1" || v = "true" || v = "yes"
  | None -> false

let log_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.stdout "component = consensus node = %s module = engine %s\n%!" addr msg)
    fmt

let err_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.stderr "component = consensus node = %s module = engine %s\n%!" addr msg)
    fmt

let tx_list_hash_for_header tx_hashes =
  Octra_net.Hash_domain.hash
    "octra:tx_list:v1"
    (String.concat "" (List.map hash32_to_hex tx_hashes))


type vote_set = {
  votes : (string, vote) Hashtbl.t;
  by_pid : (string, int) Hashtbl.t;
  mutable count_nil : int;
  mutable count_total : int;
}

type vote_add_result =
  [ `Duplicate
  | `Added
  | `QuorumAny
  | `QuorumOf of string ]

let create_vote_set () = {
  votes = Hashtbl.create 8;
  by_pid = Hashtbl.create 4;
  count_nil = 0;
  count_total = 0;
}


let count_for_pid vs pid =
  match Hashtbl.find_opt vs.by_pid pid with Some c -> c | None -> 0

let add_vote vs (v : vote) ~quorum =
  if Hashtbl.mem vs.votes v.validator then `Duplicate
  else begin
    Hashtbl.replace vs.votes v.validator v;
    vs.count_total <- vs.count_total + 1;
    let is_nil = Octra_net.Hash_domain.is_nil v.proposal_id in
    if is_nil then begin
      vs.count_nil <- vs.count_nil + 1;
      if vs.count_total >= quorum then `QuorumAny else `Added
    end else begin

      let prev = match Hashtbl.find_opt vs.by_pid v.proposal_id with Some c -> c | None -> 0 in
      let next = prev + 1 in
      Hashtbl.replace vs.by_pid v.proposal_id next;
      if next >= quorum then `QuorumOf v.proposal_id
      else if vs.count_total >= quorum then `QuorumAny
      else `Added
    end
  end


type output =
  | SendPropose of propose
  | SendVote of vote
  | SendFinalize of finalize
  | ScheduleTimeout of { step : round_step; round : int; delay_ms : int; generation : int }
  | Finalized of { epoch_id : int64; finalize : finalize }


let int_env name fallback =
  match Sys.getenv_opt name with
  | Some raw -> (try max 0 (int_of_string raw) with _ -> fallback)
  | None -> fallback

let timeout_ms ~round ~step =
  let base = int_env "OCTRA_BFT_TIMEOUT_BASE_MS" 3000 in
  let per_round = int_env "OCTRA_BFT_TIMEOUT_ROUND_MS" 1000 in
  let step_mult = match step with
    | ProposeStep -> 1
    | PrevoteStep -> 1
    | PrecommitStep -> 1
  in
  (base + round * per_round) * step_mult

type proposal_cache_entry = {
  header : epoch_header;
  tx_hashes : string list option;
}


type t = {
  chain_id : string;
  my_addr : string;
  mutable vs : validator_set;
  mutable state : engine_state;
  mutable prevotes : vote_set;
  mutable precommits : vote_set;
  mutable current_proposal : propose option;
  mutable proposal_cache : (string, proposal_cache_entry) Hashtbl.t;
  mutable polc_by_round : (int, string) Hashtbl.t;
  mutable higher_round_evidence : (int, (string, unit) Hashtbl.t) Hashtbl.t;
  mutable outputs : output list;
  mutable finalized_height : int64;
  mutable generation : int;
  can_vote : unit -> bool;
}

let create ~chain_id ~my_addr ~validator_set ~start_height ~can_vote = {
  chain_id;
  my_addr;
  vs = validator_set;
  state = initial_engine_state start_height;
  prevotes = create_vote_set ();
  precommits = create_vote_set ();
  current_proposal = None;
  proposal_cache = Hashtbl.create 8;
  polc_by_round = Hashtbl.create 4;
  higher_round_evidence = Hashtbl.create 4;
  outputs = [];
  finalized_height = Int64.pred start_height;
  generation = 0;
  can_vote;
}

let replace_validator_set t validator_set =
  t.vs <- validator_set

let is_pristine t =
  t.generation = 0
  && t.state.round = 0
  && t.state.step = ProposeStep
  && t.current_proposal = None
  && Hashtbl.length t.prevotes.votes = 0
  && Hashtbl.length t.precommits.votes = 0
  && t.outputs = []

let cache_proposal_bundle t pid header tx_hashes =
  Hashtbl.replace t.proposal_cache pid { header; tx_hashes = Some tx_hashes }

let cache_proposal_header_only t pid header =
  if not (Hashtbl.mem t.proposal_cache pid) then
    Hashtbl.replace t.proposal_cache pid { header; tx_hashes = None }

let find_header t pid =
  match Hashtbl.find_opt t.proposal_cache pid with
  | Some e -> Some e.header
  | None -> None

let find_tx_hashes t pid =
  match Hashtbl.find_opt t.proposal_cache pid with
  | Some e -> e.tx_hashes
  | None -> None

let emit t o = t.outputs <- o :: t.outputs

let drain_outputs t =
  let out = List.rev t.outputs in
  t.outputs <- [];
  out

type local_vote_outcome =
  | LocalVoteCast of vote_add_result
  | LocalVoteAlreadySame
  | LocalVoteConflict of vote

let vote_type_name = function
  | Prevote -> "prevote"
  | Precommit -> "precommit"

let vote_set_for t = function
  | Prevote -> t.prevotes
  | Precommit -> t.precommits

let cast_local_vote t ~sign_fn ~vote_type ~proposal_id =
  let vs = vote_set_for t vote_type in
  match Hashtbl.find_opt vs.votes t.my_addr with
  | Some prior when prior.proposal_id = proposal_id ->
    LocalVoteAlreadySame
  | Some prior ->
    err_node t.my_addr
      "event = refuse_conflicting_local_vote type = %s height = %Ld round = %d prior = %s next = %s"
      (vote_type_name vote_type)
      t.state.height
      t.state.round
      (short_hex_raw prior.proposal_id)
      (short_hex_raw proposal_id);
    LocalVoteConflict prior
  | None ->
    let vote = {
      chain_id = t.chain_id;
      epoch_id = t.state.height;
      round = t.state.round;
      vote_type;
      proposal_id;
      validator = t.my_addr;
      signature = String.make 64 '\x00';
    } in
    let sign_bytes = C_hash.vote_sign_bytes vote in
    let vote = { vote with signature = sign_fn sign_bytes } in
    let result = add_vote vs vote ~quorum:t.vs.quorum in
    emit t (SendVote vote);
    LocalVoteCast result

let precommits_for_pid t pid =
  Hashtbl.fold (fun _ v acc ->
    if v.vote_type = Precommit && v.proposal_id = pid then v :: acc
    else acc)
    t.precommits.votes []
  |> List.sort (fun (a : vote) (b : vote) -> String.compare a.validator b.validator)

let make_finalize t ~header ~proposal_id ~round =
  {
    chain_id = t.chain_id;
    epoch_id = t.state.height;
    commit_round = round;
    header;
    proposal_id;
    precommits = precommits_for_pid t proposal_id;
  }

let emit_finalized ?(send = true) t finalize =
  t.finalized_height <- finalize.epoch_id;
  if send then emit t (SendFinalize finalize);
  emit t (Finalized { epoch_id = finalize.epoch_id; finalize })


let am_i_leader t =
  let l = leader_of t.vs ~epoch_id:t.state.height ~round:t.state.round in
  l.address = t.my_addr

let am_i_validator t =
  C_types.is_validator t.vs t.my_addr

let local_voting_allowed t =
  am_i_validator t && t.can_vote ()

let can_prevote t =
  match t.state.step with
  | ProposeStep | PrevoteStep -> true
  | PrecommitStep -> false


let start_round t round =
  t.state <- { t.state with round; step = ProposeStep };
  t.prevotes <- create_vote_set ();
  t.precommits <- create_vote_set ();
  t.current_proposal <- None;
  emit t (ScheduleTimeout {
    step = ProposeStep;
    round;
    delay_ms = timeout_ms ~round ~step:ProposeStep;
    generation = t.generation;
  })


let start_height t height =
  t.generation <- t.generation + 1;
  t.state <- initial_engine_state height;
  t.proposal_cache <- Hashtbl.create 8;
  t.polc_by_round <- Hashtbl.create 4;
  t.higher_round_evidence <- Hashtbl.create 4;
  start_round t 0

let record_polc t round proposal_id =
  if not (Octra_net.Hash_domain.is_nil proposal_id) then
    Hashtbl.replace t.polc_by_round round proposal_id

let record_higher_round t ~round ~validator =
  if round > t.state.round then begin
    let set =
      match Hashtbl.find_opt t.higher_round_evidence round with
      | Some s -> s
      | None ->
        let s = Hashtbl.create 4 in
        Hashtbl.replace t.higher_round_evidence round s;
        s
    in
    Hashtbl.replace set validator ()
  end

let try_round_skip t =
  if Int64.compare t.state.height t.finalized_height <= 0 then ()
  else begin
    let threshold = t.vs.f + 1 in
    let best =
      Hashtbl.fold (fun round set acc ->
        if round <= t.state.round then acc
        else if Hashtbl.length set < threshold then acc
        else
          match acc with
          | None -> Some round
          | Some r when round > r -> Some round
          | _ -> acc
      ) t.higher_round_evidence None
    in
    match best with
    | None -> ()
    | Some evidence_round ->
      let target = evidence_round in
      log_node t.my_addr
        "event = round_skip height = %Ld old_round = %d new_round = %d evidence_round = %d"
        t.state.height t.state.round target evidence_round;
      t.higher_round_evidence <- Hashtbl.create 4;
      start_round t target
  end

let lock_allows t (proposal_id : string) (valid_round : int option) : bool =
  if t.state.locked_round < 0 then true
  else
    let locked_pid = match t.state.locked_value with
      | Some h -> C_hash.proposal_id h
      | None -> ""
    in
    if proposal_id = locked_pid then true
    else
      match valid_round with
      | None -> false
      | Some vr ->
        if vr <= t.state.locked_round then false
        else if vr >= t.state.round then false
        else
          match Hashtbl.find_opt t.polc_by_round vr with
          | Some pid when pid = proposal_id -> true
          | _ -> false

let allow_valid_value_reuse t =
  t.vs.quorum >= 1


let do_propose t (header : epoch_header) (tx_hashes : string list) ~sign_fn =
  if not (local_voting_allowed t) then ()
  else if not (am_i_leader t) then ()
  else if header.epoch_id <> t.state.height then begin
    err_node t.my_addr
      "event = drop_do_propose reason = stale_header header_epoch = %Ld state_height = %Ld"
      header.epoch_id t.state.height
  end
  else if header.chain_id <> t.chain_id then begin
    err_node t.my_addr
      "event = drop_do_propose reason = chain_id_mismatch header = %s ours = %s"
      header.chain_id t.chain_id
  end
  else if t.state.step <> ProposeStep then begin
    err_node t.my_addr
      "event = drop_do_propose reason = non_propose_step height = %Ld round = %d"
      t.state.height t.state.round
  end
  else begin
    let resolved =
      if allow_valid_value_reuse t then
        match t.state.valid_value with
        | Some v when t.state.valid_round >= 0 ->
          let v_pid = C_hash.proposal_id v in
          (match find_tx_hashes t v_pid with
           | None ->
             err_node t.my_addr
               "event = refuse_repropose_valid_value reason = missing_tx_hashes valid_round = %d pid = %s"
               t.state.valid_round
               (Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string v_pid));
             None
           | Some cached_hashes ->
             let recomputed = tx_list_hash_for_header cached_hashes in
             if recomputed <> v.tx_list_hash then begin
               err_node t.my_addr
                 "event = refuse_repropose_valid_value reason = tx_list_hash_mismatch";
               None
             end else begin
               log_node t.my_addr
                 "event = repropose_valid_value valid_round = %d locked_round = %d"
                 t.state.valid_round t.state.locked_round;
               Some (v, cached_hashes, Some t.state.valid_round)
             end)
        | _ -> Some (header, tx_hashes, None)
      else
        Some (header, tx_hashes, None)
    in
    match resolved with
    | None -> ()
    | Some (header, tx_hashes, valid_round) ->
    let proposal_id = C_hash.proposal_id header in
    cache_proposal_bundle t proposal_id header tx_hashes;
    let propose = {
      chain_id = t.chain_id;
      epoch_id = t.state.height;
      round = t.state.round;
      valid_round;
      header;
      tx_hashes;
      proposer = t.my_addr;
      signature = String.make 64 '\x00';
    } in
    let sign_bytes = C_hash.propose_sign_bytes propose in
    let propose = { propose with signature = sign_fn sign_bytes } in
    t.current_proposal <- Some propose;
    emit t (SendPropose propose);

    t.state <- { t.state with step = PrevoteStep };
    let prevote_outcome =
      cast_local_vote t ~sign_fn ~vote_type:Prevote ~proposal_id
    in

    (match prevote_outcome with
     | LocalVoteCast (`QuorumOf _) ->
       t.state <- { t.state with
         locked_round = t.state.round;
         locked_value = Some header;
         valid_round = t.state.round;
         valid_value = Some header;
       };
       t.state <- { t.state with step = PrecommitStep };
       let pc_outcome =
         cast_local_vote t ~sign_fn ~vote_type:Precommit ~proposal_id
       in
       (match pc_outcome with
        | LocalVoteCast (`QuorumOf _) ->
          log_node t.my_addr
            "event = local_finalize_self height = %Ld round = %d pid = %s creator = %s lock_round = %d valid_round = %d"
            t.state.height t.state.round
            (short_hex_raw proposal_id)
            (short_addr header.creator_addr)
            t.state.locked_round t.state.valid_round;
          let finalize = make_finalize t ~header ~proposal_id ~round:t.state.round in
          emit_finalized t finalize
        | _ ->
          emit t (ScheduleTimeout {
            step = PrecommitStep; round = t.state.round;
            delay_ms = timeout_ms ~round:t.state.round ~step:PrecommitStep;
            generation = t.generation;
          }))
     | LocalVoteCast _
     | LocalVoteAlreadySame ->
       emit t (ScheduleTimeout {
         step = PrevoteStep; round = t.state.round;
         delay_ms = timeout_ms ~round:t.state.round ~step:PrevoteStep;
         generation = t.generation;
       })
     | LocalVoteConflict _ -> ())
  end


let on_propose t (p : propose) ~verify_fn ~execute_fn ~sign_fn =
  if p.epoch_id <= t.finalized_height then ()
  else if p.epoch_id <> t.state.height then ()
  else begin
    if p.round > t.state.round then begin
      record_higher_round t ~round:p.round ~validator:p.proposer;
      try_round_skip t
    end;
  if p.round <> t.state.round then ()
  else begin
    let proposal_id = C_hash.proposal_id p.header in
    cache_proposal_bundle t proposal_id p.header p.tx_hashes;
    t.current_proposal <- Some p;
    let sign_bytes = C_hash.propose_sign_bytes p in
    let proposer_valid = verify_fn p.proposer sign_bytes p.signature in
    let expected_leader = leader_of t.vs ~epoch_id:p.epoch_id ~round:p.round in
    let is_leader = expected_leader.address = p.proposer in
    let root_valid = execute_fn p in
    let lock_ok = lock_allows t proposal_id p.valid_round in
    let accept = proposer_valid && is_leader && root_valid && lock_ok in
    let vote_proposal_id = if accept then proposal_id
      else Octra_net.Hash_domain.nil_hash in
    if not (local_voting_allowed t) || not (can_prevote t) then ()
    else begin
      t.state <- { t.state with step = PrevoteStep };
      let prevote_outcome =
        cast_local_vote t ~sign_fn ~vote_type:Prevote
          ~proposal_id:vote_proposal_id
      in
      (match prevote_outcome with
     | LocalVoteCast (`QuorumOf _)
       when not (Octra_net.Hash_domain.is_nil vote_proposal_id) ->
       let hdr = p.header in
       record_polc t t.state.round proposal_id;
       t.state <- { t.state with
         locked_round = t.state.round;
         locked_value = Some hdr;
         valid_round = t.state.round;
         valid_value = Some hdr;
       };
       t.state <- { t.state with step = PrecommitStep };
       let pc_outcome =
         cast_local_vote t ~sign_fn ~vote_type:Precommit
           ~proposal_id:vote_proposal_id
       in
       (match pc_outcome with
        | LocalVoteCast (`QuorumOf _) ->
          log_node t.my_addr
            "event = local_finalize_propose height = %Ld round = %d pid = %s creator = %s lock_round = %d valid_round = %d"
            t.state.height t.state.round
            (short_hex_raw vote_proposal_id)
            (short_addr hdr.creator_addr)
            t.state.locked_round t.state.valid_round;
          let finalize =
            make_finalize t ~header:hdr ~proposal_id:vote_proposal_id
              ~round:t.state.round in
          emit_finalized t finalize
        | _ ->
          emit t (ScheduleTimeout {
            step = PrecommitStep; round = t.state.round;
            delay_ms = timeout_ms ~round:t.state.round ~step:PrecommitStep;
            generation = t.generation;
          }))
     | LocalVoteCast _
     | LocalVoteAlreadySame ->
       emit t (ScheduleTimeout {
         step = PrevoteStep; round = t.state.round;
         delay_ms = timeout_ms ~round:t.state.round ~step:PrevoteStep;
         generation = t.generation;
       })
     | LocalVoteConflict _ -> ())
    end
  end
  end

let find_header_or_fallback t proposal_id =
  match find_header t proposal_id with
  | Some h -> Some h
  | None ->
    match t.current_proposal with
    | Some p when C_hash.proposal_id p.header = proposal_id -> Some p.header
    | _ ->
      match t.state.locked_value with
      | Some h when C_hash.proposal_id h = proposal_id -> Some h
      | _ ->
        match t.state.valid_value with
        | Some h when C_hash.proposal_id h = proposal_id -> Some h
        | _ -> None


let on_vote t (v : vote) ~sign_fn =
  if v.epoch_id <= t.finalized_height then ()
  else if v.epoch_id <> t.state.height then ()
  else begin
    if v.round > t.state.round then begin
      record_higher_round t ~round:v.round ~validator:v.validator;
      try_round_skip t
    end;
  if v.round <> t.state.round then ()
  else
    match v.vote_type with
    | Prevote ->
      let result = add_vote t.prevotes v ~quorum:t.vs.quorum in
      (match result with
       | `QuorumOf proposal_id when t.state.step = PrevoteStep ->
         (if not (Octra_net.Hash_domain.is_nil proposal_id) then begin
           record_polc t t.state.round proposal_id;
           let locked_header = find_header_or_fallback t proposal_id in
           t.state <- { t.state with
             locked_round = t.state.round;
             locked_value = (match locked_header with Some h -> Some h | None -> t.state.locked_value);
             valid_round = t.state.round;
             valid_value = (match locked_header with Some h -> Some h | None -> t.state.valid_value);
           }
         end);

         if local_voting_allowed t then begin
           t.state <- { t.state with step = PrecommitStep };
           match cast_local_vote t ~sign_fn ~vote_type:Precommit
                   ~proposal_id with
           | LocalVoteCast _
           | LocalVoteAlreadySame ->
             emit t (ScheduleTimeout {
               step = PrecommitStep;
               round = t.state.round;
               delay_ms = timeout_ms ~round:t.state.round ~step:PrecommitStep;
               generation = t.generation;
             })
           | LocalVoteConflict _ -> ()
         end
       | `QuorumAny when t.state.step = PrevoteStep ->

         emit t (ScheduleTimeout {
           step = PrecommitStep;
           round = t.state.round;
           delay_ms = timeout_ms ~round:t.state.round ~step:PrecommitStep;
           generation = t.generation;
         })
       | _ -> ())

    | Precommit ->
      let result = add_vote t.precommits v ~quorum:t.vs.quorum in
      (match result with
       | `QuorumOf proposal_id when local_voting_allowed t ->
         (match find_header_or_fallback t proposal_id with
          | None ->
            if log_pending_header_quorum () then
              log_node t.my_addr
                "event = pending_precommit_quorum reason = missing_local_header pid = %s"
                (String.sub proposal_id 0 (min 16 (String.length proposal_id)))
          | Some header ->
            log_node t.my_addr
              "event = local_finalize_vote height = %Ld round = %d pid = %s creator = %s lock_round = %d valid_round = %d"
              t.state.height t.state.round
              (short_hex_raw proposal_id)
              (short_addr header.creator_addr)
              t.state.locked_round t.state.valid_round;
            let finalize = make_finalize t ~header ~proposal_id ~round:t.state.round in
            emit_finalized t finalize)
       | _ -> ())
  end

let accept_finalize_batch t (f : finalize) =
  if f.epoch_id <= t.finalized_height then false
  else if f.epoch_id <> t.state.height then false
  else begin
    let proposal_id = f.proposal_id in
    let header_pid = C_hash.proposal_id f.header in
    if proposal_id <> header_pid then false
    else begin
      let lock_conflict =
        match t.state.locked_value with
        | Some h ->
          let locked_pid = C_hash.proposal_id h in
          proposal_id <> locked_pid && f.commit_round <= t.state.locked_round
        | None -> false
      in
      if lock_conflict then false
      else begin
        log_node t.my_addr
          "event = accept_finalize_batch height = %Ld current_round = %d commit_round = %d pid = %s creator = %s lock_round = %d valid_round = %d"
          f.epoch_id t.state.round f.commit_round
          (short_hex_raw proposal_id)
          (short_addr f.header.creator_addr)
          t.state.locked_round t.state.valid_round;
        cache_proposal_header_only t proposal_id f.header;
        record_polc t f.commit_round proposal_id;
        t.state <- {
          t.state with
          locked_round = max t.state.locked_round f.commit_round;
          locked_value = Some f.header;
          valid_round = f.commit_round;
          valid_value = Some f.header;
        };
        emit_finalized ~send:false t f;
        true
      end
    end
  end


let on_timeout t ~step ~round ~generation ~sign_fn =
  if generation <> t.generation then ()
  else if round <> t.state.round then ()
  else if step <> t.state.step then ()
  else
    match step with
    | ProposeStep when local_voting_allowed t ->
      t.state <- { t.state with step = PrevoteStep };
      (match cast_local_vote t ~sign_fn ~vote_type:Prevote
               ~proposal_id:Octra_net.Hash_domain.nil_hash with
       | LocalVoteCast _
       | LocalVoteAlreadySame ->
         emit t (ScheduleTimeout {
           step = PrevoteStep;
           round = t.state.round;
           delay_ms = timeout_ms ~round:t.state.round ~step:PrevoteStep;
           generation = t.generation;
         })
       | LocalVoteConflict _ -> ())
    | PrevoteStep when local_voting_allowed t ->
      t.state <- { t.state with step = PrecommitStep };
      (match cast_local_vote t ~sign_fn ~vote_type:Precommit
               ~proposal_id:Octra_net.Hash_domain.nil_hash with
       | LocalVoteCast _
       | LocalVoteAlreadySame ->
         emit t (ScheduleTimeout {
           step = PrecommitStep;
           round = t.state.round;
           delay_ms = timeout_ms ~round:t.state.round ~step:PrecommitStep;
           generation = t.generation;
         })
       | LocalVoteConflict _ -> ())
    | ProposeStep | PrevoteStep | PrecommitStep ->
      start_round t (t.state.round + 1)