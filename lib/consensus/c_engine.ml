(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types

type validator_info = C_types.validator_info
type validator_set = C_types.validator_set

let make_validator_set = C_types.make_validator_set
let leader_of = C_types.leader_of
let max_round_ahead = 64
let max_sync_ahead = 1024
let round_history_limit = 64
let sync_history_limit = max_sync_ahead
let max_timeout_ms = 120_000
let max_propose_timeout_ms = 300_000

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
      Octra_log.info "consensus" "node = %s module = engine %s" addr msg)
    fmt

let err_node addr fmt =
  Printf.ksprintf
    (fun msg ->
      Octra_log.error "consensus" "node = %s module = engine %s" addr msg)
    fmt

let tx_list_hash_for_header tx_hashes =
  Octra_net.Hash_domain.hash
    "octra:tx_list:v1"
    (String.concat "" (List.map hash32_to_hex tx_hashes))

type vote_set = {
  votes : (string, vote) Hashtbl.t;
  by_pid : (string, int) Hashtbl.t;
  by_pid_weight : (string, Z.t) Hashtbl.t;
  mutable count_nil : int;
  mutable count_total : int;
  mutable weight_nil : Z.t;
  mutable weight_total : Z.t;
}

type vote_add_result =
  [ `Duplicate
  | `Rejected
  | `Added
  | `QuorumAny
  | `QuorumOf of string ]

let create_vote_set () = {
  votes = Hashtbl.create 8;
  by_pid = Hashtbl.create 4;
  by_pid_weight = Hashtbl.create 4;
  count_nil = 0;
  count_total = 0;
  weight_nil = Z.zero;
  weight_total = Z.zero;
}

let count_for_pid vs pid =
  match Hashtbl.find_opt vs.by_pid pid with Some c -> c | None -> 0

let weight_for_pid vs pid =
  match Hashtbl.find_opt vs.by_pid_weight pid with
  | Some weight -> weight
  | None -> Z.zero

let add_vote vs (v : vote) ~validator_set =
  if Hashtbl.mem vs.votes v.validator then `Duplicate
  else
    match C_types.weight_of_addr validator_set v.validator with
    | None -> `Rejected
    | Some weight ->
      Hashtbl.replace vs.votes v.validator v;
      vs.count_total <- vs.count_total + 1;
      vs.weight_total <- Z.add vs.weight_total weight;
      let is_nil = Octra_net.Hash_domain.is_nil v.proposal_id in
      if is_nil then begin
        vs.count_nil <- vs.count_nil + 1;
        vs.weight_nil <- Z.add vs.weight_nil weight;
        if C_types.quorum_reached_at
             ~chain_id:v.chain_id
             ~epoch_id:v.epoch_id
             validator_set
             ~signer_count:vs.count_total
             ~signed_weight:vs.weight_total then
          `QuorumAny
        else
          `Added
      end else begin
        let count = count_for_pid vs v.proposal_id + 1 in
        let proposal_weight = Z.add (weight_for_pid vs v.proposal_id) weight in
        Hashtbl.replace vs.by_pid v.proposal_id count;
        Hashtbl.replace vs.by_pid_weight v.proposal_id proposal_weight;
        if C_types.quorum_reached_at
             ~chain_id:v.chain_id
             ~epoch_id:v.epoch_id
             validator_set
             ~signer_count:count
             ~signed_weight:proposal_weight then
          `QuorumOf v.proposal_id
        else if C_types.quorum_reached_at
                  ~chain_id:v.chain_id
                  ~epoch_id:v.epoch_id
                  validator_set
                  ~signer_count:vs.count_total
                  ~signed_weight:vs.weight_total then
          `QuorumAny
        else
          `Added
      end

type output =
  | SendPropose of propose
  | SendVote of vote * propose option
  | SendFinalize of finalize
  | RequestProposal of { round : int; proposal_id : string }
  | RequestRoundEvidence of int
  | ScheduleTimeout of { step : round_step; round : int; delay_ms : int; generation : int }
  | Finalized of { epoch_id : int64; finalize : finalize }

let missing_proposal_request ~proposal_known (vote : vote) result =
  match result with
  | `Duplicate
  | `Rejected ->
    None
  | `Added
  | `QuorumAny
  | `QuorumOf _ ->
    if Octra_net.Hash_domain.is_nil vote.proposal_id || proposal_known then
      None
    else
      Some (vote.round, vote.proposal_id)

let bounded_int raw ~fallback ~minimum ~limit =
  try
    let value = int_of_string raw in
    if value < minimum || value > limit then fallback else value
  with _ -> fallback

let int_env name ~fallback ~minimum ~limit =
  match Sys.getenv_opt name with
  | Some raw -> bounded_int raw ~fallback ~minimum ~limit
  | None -> fallback

let timeout_ms_with ~round ~step ~base ~propose ~per_round =
  let step_base, cap = match step with
    | ProposeStep ->
      max propose (Int64.to_int Epoch_time.proposal_wait_budget_ms),
      max_propose_timeout_ms
    | PrevoteStep
    | PrecommitStep -> base, max_timeout_ms
  in
  let span = max 0 round in
  let raw =
    Int64.add
      (Int64.of_int step_base)
      (Int64.mul (Int64.of_int span) (Int64.of_int per_round))
  in
  Int64.to_int (Int64.min (Int64.of_int cap) raw)

let timeout_ms ~round ~step =
  let base =
    int_env "OCTRA_BFT_TIMEOUT_BASE_MS"
      ~fallback:3000
      ~minimum:100
      ~limit:120_000
  in
  let propose =
    int_env "OCTRA_BFT_PROPOSE_TIMEOUT_MS"
      ~fallback:base
      ~minimum:100
      ~limit:max_propose_timeout_ms
  in
  let per_round =
    int_env "OCTRA_BFT_TIMEOUT_ROUND_MS"
      ~fallback:1000
      ~minimum:100
      ~limit:30_000
  in
  timeout_ms_with ~round ~step ~base ~propose ~per_round

type proposal_cache_entry = {
  header : epoch_header;
  tx_hashes : string list option;
  parent_commit : parent_commit option;
}

type t = {
  chain_id : string;
  my_addr : string;
  mutable vs : validator_set;
  mutable state : engine_state;
  mutable prevotes : vote_set;
  mutable prevotes_by_round : (int, vote_set) Hashtbl.t;
  mutable precommits : vote_set;
  mutable precommits_by_round : (int, vote_set) Hashtbl.t;
  mutable current_proposal : propose option;
  mutable pending_prevote : (int64 * int * string) option;
  mutable proposal_cache : (string, proposal_cache_entry) Hashtbl.t;
  mutable proposal_messages : ((string * int), propose) Hashtbl.t;
  mutable polc_by_round : (int, string) Hashtbl.t;
  mutable polc_requests : (int, int) Hashtbl.t;
  mutable higher_round_evidence : (string, int) Hashtbl.t;
  mutable outputs : output list;
  mutable finalized_height : int64;
  mutable generation : int;
  can_vote : unit -> bool;
  mutable round_skip_ready : unit -> bool;
}

let create ~chain_id ~my_addr ~validator_set ~start_height ~can_vote =
  let prevotes = create_vote_set () in
  let prevotes_by_round = Hashtbl.create 8 in
  let precommits = create_vote_set () in
  let precommits_by_round = Hashtbl.create 8 in
  Hashtbl.add prevotes_by_round 0 prevotes;
  Hashtbl.add precommits_by_round 0 precommits;
  {
    chain_id;
    my_addr;
    vs = validator_set;
    state = initial_engine_state start_height;
    prevotes;
    prevotes_by_round;
    precommits;
    precommits_by_round;
    current_proposal = None;
    pending_prevote = None;
    proposal_cache = Hashtbl.create 8;
    proposal_messages = Hashtbl.create 8;
    polc_by_round = Hashtbl.create 4;
    polc_requests = Hashtbl.create 4;
    higher_round_evidence = Hashtbl.create 4;
    outputs = [];
    finalized_height = Int64.pred start_height;
    generation = 0;
    can_vote;
    round_skip_ready = (fun () -> true);
  }

let set_round_skip_ready t ready =
  t.round_skip_ready <- ready

let round_skip_ready t =
  try t.round_skip_ready () with _ -> false

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

let cache_proposal_bundle t pid header tx_hashes ~parent_commit =
  Hashtbl.replace t.proposal_cache pid {
    header;
    tx_hashes = Some tx_hashes;
    parent_commit;
  }

let cache_proposal_header_only t pid header =
  if not (Hashtbl.mem t.proposal_cache pid) then
    Hashtbl.replace t.proposal_cache pid {
      header;
      tx_hashes = None;
      parent_commit = None;
    }

let find_header t pid =
  match Hashtbl.find_opt t.proposal_cache pid with
  | Some e -> Some e.header
  | None -> None

let find_tx_hashes t pid =
  match Hashtbl.find_opt t.proposal_cache pid with
  | Some e -> e.tx_hashes
  | None -> None

let find_parent_commit t pid =
  match Hashtbl.find_opt t.proposal_cache pid with
  | Some entry -> entry.parent_commit
  | None -> None

let promote_known_header t round proposal_id =
  if round > t.state.valid_round then
    match find_header t proposal_id with
    | None -> ()
    | Some header ->
      t.state <- {
        t.state with
        valid_round = round;
        valid_value = Some header;
      }

let record_polc t round proposal_id =
  if not (Octra_net.Hash_domain.is_nil proposal_id) then begin
    Hashtbl.replace t.polc_by_round round proposal_id;
    Hashtbl.remove t.polc_requests round;
    promote_known_header t round proposal_id
  end

let promote_cached_polc t proposal_id =
  Hashtbl.iter
    (fun round pid ->
      if pid = proposal_id then promote_known_header t round proposal_id)
    t.polc_by_round

let cache_proposal_message t (proposal : propose) =
  let pid = C_hash.proposal_id proposal.header in
  cache_proposal_bundle
    t
    pid
    proposal.header
    proposal.tx_hashes
    ~parent_commit:proposal.parent_commit;
  Hashtbl.replace t.proposal_messages (pid, proposal.round) proposal;
  promote_cached_polc t pid

let find_proposal_message t ~proposal_id ~round =
  Hashtbl.find_opt t.proposal_messages (proposal_id, round)

let emit t o = t.outputs <- o :: t.outputs

let drain_outputs t =
  let out = List.rev t.outputs in
  t.outputs <- [];
  out

type local_vote_outcome =
  | LocalVoteCast of vote_add_result
  | LocalVoteAlreadySame
  | LocalVoteConflict of vote
  | LocalVoteDeferred

let vote_type_name = function
  | Prevote -> "prevote"
  | Precommit -> "precommit"

let vote_set_for t = function
  | Prevote -> t.prevotes
  | Precommit -> t.precommits

let vote_set_for_round t vote_type round =
  match vote_type with
  | Prevote -> Hashtbl.find_opt t.prevotes_by_round round
  | Precommit -> Hashtbl.find_opt t.precommits_by_round round

let conflicting_vote t (vote : vote) =
  if vote.epoch_id <> t.state.height then None
  else
    match vote_set_for_round t vote.vote_type vote.round with
    | None -> None
    | Some vote_set ->
      match Hashtbl.find_opt vote_set.votes vote.validator with
      | Some prior when prior.proposal_id <> vote.proposal_id -> Some prior
      | _ -> None

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
    let proposal =
      match vote_type with
      | Precommit when not (Octra_net.Hash_domain.is_nil proposal_id) ->
        find_proposal_message t ~proposal_id ~round:t.state.round
      | Prevote
      | Precommit -> None
    in
    if vote_type = Precommit
       && not (Octra_net.Hash_domain.is_nil proposal_id)
       && Option.is_none proposal then begin
      err_node t.my_addr
        "event = defer_local_precommit reason = proposal_missing height = %Ld round = %d pid = %s"
        t.state.height
        t.state.round
        (short_hex_raw proposal_id);
      emit t (RequestProposal { round = t.state.round; proposal_id });
      LocalVoteDeferred
    end else
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
    let result = add_vote vs vote ~validator_set:t.vs in
    emit t (SendVote (vote, proposal));
    LocalVoteCast result

let precommits_for_pid t ~round pid =
  match Hashtbl.find_opt t.precommits_by_round round with
  | None -> []
  | Some votes ->
    Hashtbl.fold (fun _ v acc ->
      if v.vote_type = Precommit && v.proposal_id = pid then v :: acc
      else acc)
      votes.votes []
    |> List.sort (fun (a : vote) (b : vote) ->
      String.compare a.validator b.validator)

type finalize_block =
  | Parent_commit_missing
  | Parent_commit_mismatch

let parent_commit_for_finalize t ~header ~proposal_id =
  let parent_commit = find_parent_commit t proposal_id in
  let got = C_hash.parent_commit_hash_opt parent_commit in
  if got = header.parent_commit_hash then
    Ok parent_commit
  else if Option.is_none parent_commit then
    Error Parent_commit_missing
  else
    Error Parent_commit_mismatch

let make_finalize t ~header ~proposal_id ~round =
  Result.map
    (fun parent_commit -> {
      chain_id = t.chain_id;
      epoch_id = t.state.height;
      commit_round = round;
      header;
      proposal_id;
      precommits = precommits_for_pid t ~round proposal_id;
      parent_commit;
    })
    (parent_commit_for_finalize t ~header ~proposal_id)

let emit_finalized ?(send = true) t finalize =
  t.finalized_height <- finalize.epoch_id;
  if send then emit t (SendFinalize finalize);
  emit t (Finalized { epoch_id = finalize.epoch_id; finalize })

let finalize_block_name = function
  | Parent_commit_missing -> "parent_commit_missing"
  | Parent_commit_mismatch -> "parent_commit_mismatch"

let emit_local_finalize t ~header ~proposal_id ~round =
  match make_finalize t ~header ~proposal_id ~round with
  | Ok finalize ->
    emit_finalized t finalize;
    true
  | Error reason ->
    err_node t.my_addr
      "event = defer_local_finalize reason = %s height = %Ld round = %d pid = %s"
      (finalize_block_name reason)
      t.state.height
      round
      (short_hex_raw proposal_id);
    emit t (RequestProposal { round; proposal_id });
    false

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

let round_retained t round =
  let recent_floor = max 0 (t.state.round - round_history_limit) in
  round >= recent_floor
  || round = t.state.locked_round
  || round = t.state.valid_round
  || Hashtbl.mem t.polc_requests round

let oldest_polc_request t =
  Hashtbl.fold
    (fun round _ oldest ->
      match oldest with
      | None -> Some round
      | Some value -> Some (min value round))
    t.polc_requests
    None

let remember_polc_request t round =
  if round < 0
     || round >= t.state.round
     || round <= t.state.locked_round
     || Hashtbl.mem t.polc_by_round round
  then
    false
  else if Hashtbl.mem t.polc_requests round then
    true
  else begin
    if Hashtbl.length t.polc_requests >= round_history_limit then
      Option.iter (Hashtbl.remove t.polc_requests) (oldest_polc_request t);
    Hashtbl.replace t.polc_requests round (-1);
    true
  end

let emit_polc_request t round =
  Hashtbl.replace t.polc_requests round t.state.round;
  emit t (RequestRoundEvidence round)

let request_polc t round =
  if remember_polc_request t round then
    match Hashtbl.find_opt t.polc_requests round with
    | Some last_sent when last_sent < t.state.round ->
      emit_polc_request t round
    | Some _
    | None -> ()

let least_recent_polc_request t ~except =
  Hashtbl.fold
    (fun round last_sent selected ->
      if Some round = except || last_sent >= t.state.round then selected
      else
        match selected with
        | None -> Some (round, last_sent)
        | Some (best_round, best_sent)
          when last_sent < best_sent
               || (last_sent = best_sent && round > best_round) ->
          Some (round, last_sent)
        | Some _ -> selected)
    t.polc_requests
    None

let retry_polc_request t ~except =
  match least_recent_polc_request t ~except with
  | None -> ()
  | Some (round, _) ->
    emit t (RequestRoundEvidence round);
    Hashtbl.replace t.polc_requests round t.state.round

let prune_round_history t =
  let prune table =
    Hashtbl.filter_map_inplace
      (fun round vote_set ->
        if round_retained t round then Some vote_set else None)
      table
  in
  prune t.prevotes_by_round;
  prune t.precommits_by_round

let start_round t round =
  let prevotes = create_vote_set () in
  let precommits = create_vote_set () in
  t.state <- { t.state with round; step = ProposeStep };
  let previous = if round > 0 then Some (round - 1) else None in
  Option.iter (request_polc t) previous;
  retry_polc_request t ~except:previous;
  t.prevotes <- prevotes;
  Hashtbl.replace t.prevotes_by_round round prevotes;
  t.precommits <- precommits;
  Hashtbl.replace t.precommits_by_round round precommits;
  prune_round_history t;
  t.current_proposal <- None;
  t.pending_prevote <- None;
  emit t (ScheduleTimeout {
    step = ProposeStep;
    round;
    delay_ms = timeout_ms ~round ~step:ProposeStep;
    generation = t.generation;
  })

let realign_round t round =
  if round <= t.state.round then
    invalid_arg "consensus round must advance";
  t.generation <- t.generation + 1;
  start_round t round

let project_votes validator_set votes =
  let projected = create_vote_set () in
  Hashtbl.iter
    (fun _ vote -> ignore (add_vote projected vote ~validator_set))
    votes.votes;
  projected

let project_rounds validator_set rounds =
  let projected = Hashtbl.create (Hashtbl.length rounds) in
  Hashtbl.iter
    (fun round votes ->
      Hashtbl.add projected round (project_votes validator_set votes))
    rounds;
  projected

let quorum_pid ~chain_id ~epoch_id validator_set votes =
  Hashtbl.fold
    (fun proposal_id signed_weight found ->
      let signer_count = count_for_pid votes proposal_id in
      if
        Octra_net.Hash_domain.is_nil proposal_id
        || not
             (C_types.quorum_reached_at
                ~chain_id
                ~epoch_id
                validator_set
                ~signer_count
                ~signed_weight)
      then found
      else
        match found with
        | None -> Some proposal_id
        | Some prior ->
          if String.compare prior proposal_id <= 0 then Some prior
          else Some proposal_id)
    votes.by_pid_weight
    None

let projected_polc t validator_set rounds =
  let polc = Hashtbl.create (Hashtbl.length rounds) in
  Hashtbl.iter
    (fun round votes ->
      Option.iter
        (fun proposal_id -> Hashtbl.add polc round proposal_id)
        (quorum_pid
           ~chain_id:t.chain_id
           ~epoch_id:t.state.height
           validator_set
           votes))
    rounds;
  polc

let projected_finalize t validator_set rounds =
  Hashtbl.fold
    (fun round votes found ->
      match
        quorum_pid
          ~chain_id:t.chain_id
          ~epoch_id:t.state.height
          validator_set
          votes,
        found
      with
      | None, _ -> found
      | Some proposal_id, None -> Some (round, proposal_id)
      | Some proposal_id, Some (prior_round, prior_id) ->
        if round > prior_round
           || (round = prior_round && String.compare proposal_id prior_id < 0)
        then Some (round, proposal_id)
        else found)
    rounds
    None

let reconfigure_validator_set t ~round validator_set =
  if round <= t.state.round then
    invalid_arg "consensus reconfiguration round must advance";
  let prevotes_by_round = project_rounds validator_set t.prevotes_by_round in
  let precommits_by_round = project_rounds validator_set t.precommits_by_round in
  let polc_by_round = projected_polc t validator_set prevotes_by_round in
  let finalize = projected_finalize t validator_set precommits_by_round in
  let valid =
    match t.state.valid_value with
    | Some header when t.state.valid_round >= 0 ->
      begin
        match Hashtbl.find_opt polc_by_round t.state.valid_round with
        | Some proposal_id when String.equal proposal_id (C_hash.proposal_id header) ->
          t.state.valid_round, Some header
        | Some _
        | None -> -1, None
      end
    | Some _
    | None -> -1, None
  in
  t.vs <- validator_set;
  t.generation <- t.generation + 1;
  t.outputs <- [];
  t.state <- {
    t.state with
    valid_round = fst valid;
    valid_value = snd valid;
  };
  t.polc_by_round <- polc_by_round;
  t.polc_requests <- Hashtbl.create 4;
  t.prevotes_by_round <- prevotes_by_round;
  t.precommits_by_round <- precommits_by_round;
  t.higher_round_evidence <- Hashtbl.create 4;
  begin
    match finalize with
    | Some (commit_round, proposal_id) ->
      begin
        match find_header t proposal_id with
        | Some header ->
          ignore
            (emit_local_finalize
               t
               ~header
               ~proposal_id
               ~round:commit_round)
        | None -> emit t (RequestProposal { round = commit_round; proposal_id })
      end
    | None -> ()
  end;
  start_round t round

let start_height t height =
  t.generation <- t.generation + 1;
  t.outputs <- [];
  t.state <- initial_engine_state height;
  t.proposal_cache <- Hashtbl.create 8;
  t.proposal_messages <- Hashtbl.create 8;
  t.polc_by_round <- Hashtbl.create 4;
  t.polc_requests <- Hashtbl.create 4;
  t.prevotes_by_round <- Hashtbl.create 8;
  t.precommits_by_round <- Hashtbl.create 8;
  t.higher_round_evidence <- Hashtbl.create 4;
  start_round t 0

let polc_matches t ~round ~proposal_id =
  match Hashtbl.find_opt t.polc_by_round round with
  | Some known -> known = proposal_id
  | None -> false

let polc_request_pending t round =
  Hashtbl.mem t.polc_requests round

let historical_prevote_known t ~round ~validator =
  match Hashtbl.find_opt t.prevotes_by_round round with
  | None -> false
  | Some vote_set -> Hashtbl.mem vote_set.votes validator

let polc_votes_for_round t round =
  match Hashtbl.find_opt t.polc_by_round round,
        Hashtbl.find_opt t.prevotes_by_round round with
  | Some proposal_id, Some vote_set ->
    Hashtbl.fold
      (fun _ (vote : vote) votes ->
        if vote.proposal_id = proposal_id then vote :: votes else votes)
      vote_set.votes
      []
    |> List.sort (fun (left : vote) (right : vote) ->
      String.compare left.validator right.validator)
  | _ -> []

let record_historical_prevote t (vote : vote) =
  if vote.round >= 0
     && vote.round < t.state.round
     && round_retained t vote.round then begin
    let vote_set =
      match Hashtbl.find_opt t.prevotes_by_round vote.round with
      | Some value -> value
      | None ->
        let value = create_vote_set () in
        Hashtbl.add t.prevotes_by_round vote.round value;
        value
    in
    match add_vote vote_set vote ~validator_set:t.vs with
    | `QuorumOf proposal_id -> record_polc t vote.round proposal_id
    | `Duplicate
    | `Rejected
    | `Added
    | `QuorumAny -> ()
  end

let restored_precommit_lock t (proposal : propose) =
  let proposal_id = C_hash.proposal_id proposal.header in
  let same_value = function
    | Some header -> String.equal (C_hash.proposal_id header) proposal_id
    | None -> false
  in
  Int64.equal proposal.epoch_id t.state.height
  && t.state.locked_round = proposal.round
  && t.state.valid_round = proposal.round
  && t.state.round >= proposal.round + 1
  && same_value t.state.locked_value
  && same_value t.state.valid_value

let restore_precommit_lock t (proposal : propose) =
  if proposal.chain_id <> t.chain_id then
    Error "pending proposal chain mismatch"
  else if proposal.epoch_id <> t.state.height then
    Error "pending proposal height mismatch"
  else if
    proposal.header.proto_version
    <> C_protocol.version_for_epoch proposal.epoch_id
  then
    Error "pending proposal protocol mismatch"
  else if proposal.header.chain_id <> proposal.chain_id then
    Error "pending proposal header chain mismatch"
  else if proposal.header.epoch_id <> proposal.epoch_id then
    Error "pending proposal header height mismatch"
  else if restored_precommit_lock t proposal then
    Ok ()
  else if not (is_pristine t) then
    Error "consensus engine is not pristine"
  else begin
    let proposal_id = C_hash.proposal_id proposal.header in
    cache_proposal_message t proposal;
    record_polc t proposal.round proposal_id;
    t.state <- {
      t.state with
      locked_round = proposal.round;
      locked_value = Some proposal.header;
      valid_round = proposal.round;
      valid_value = Some proposal.header;
    };
    realign_round t (proposal.round + 1);
    Ok ()
  end

let record_higher_round t ~max_ahead ~round ~validator =
  let within_limit =
    match max_ahead with
    | None -> true
    | Some limit -> round <= t.state.round + limit
  in
  if C_types.is_validator t.vs validator
     && round > t.state.round
     && within_limit then
    match Hashtbl.find_opt t.higher_round_evidence validator with
    | Some prior when prior >= round -> ()
    | _ -> Hashtbl.replace t.higher_round_evidence validator round

let round_skip_target t =
  let reports =
    Hashtbl.fold
      (fun validator round acc ->
        if round <= t.state.round then acc
        else
          match C_types.weight_of_addr t.vs validator with
          | None -> acc
          | Some weight -> (round, weight) :: acc)
      t.higher_round_evidence
      []
    |> List.sort (fun (left, _) (right, _) -> Int.compare right left)
  in
  let rec select signer_count signed_weight = function
    | [] -> None
    | (round, validator_weight) :: rest ->
      let signer_count = signer_count + 1 in
      let signed_weight = Z.add signed_weight validator_weight in
      if
        C_types.round_skip_reached_at
          ~chain_id:t.chain_id
          ~epoch_id:t.state.height
          t.vs
          ~signer_count
          ~signed_weight
      then Some round
      else select signer_count signed_weight rest
  in
  select 0 Z.zero reports

let try_round_skip t =
  if not (round_skip_ready t) then false
  else if Int64.compare t.state.height t.finalized_height <= 0 then false
  else
    match round_skip_target t with
    | None -> false
    | Some target ->
      log_node t.my_addr
        "event = round_skip height = %Ld old_round = %d new_round = %d evidence = weighted_floor"
        t.state.height t.state.round target;
      t.higher_round_evidence <- Hashtbl.create 4;
      realign_round t target;
      true

let on_round_sync t ~round ~validator =
  if C_types.is_validator t.vs validator
     && round > t.state.round then begin
    record_higher_round t ~max_ahead:(Some max_sync_ahead) ~round ~validator;
    try_round_skip t
  end else
    false

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

let quorum_result votes ~chain_id ~epoch_id ~validator_set =
  let proposal_id =
    Hashtbl.fold
      (fun pid weight found ->
        match found with
        | Some _ -> found
        | None when
            C_types.quorum_reached_at
              ~chain_id
              ~epoch_id
              validator_set
              ~signer_count:(count_for_pid votes pid)
              ~signed_weight:weight -> Some pid
        | None -> None)
      votes.by_pid_weight
      None
  in
  match proposal_id with
  | Some pid -> `QuorumOf pid
  | None when
      C_types.quorum_reached_at
        ~chain_id
        ~epoch_id
        validator_set
        ~signer_count:votes.count_total
        ~signed_weight:votes.weight_total ->
    `QuorumAny
  | None -> `Added

let remember_pending_prevote t proposal_id =
  match t.pending_prevote with
  | Some _ -> ()
  | None ->
    if Hashtbl.mem t.prevotes.votes t.my_addr then ()
    else
      t.pending_prevote <-
        Some (t.state.height, t.state.round, proposal_id)

let schedule_step_timeout t step =
  emit t (ScheduleTimeout {
    step;
    round = t.state.round;
    delay_ms = timeout_ms ~round:t.state.round ~step;
    generation = t.generation;
  })

let finalize_quorum t proposal_id =
  match find_header t proposal_id with
  | None -> false
  | Some header ->
    log_node t.my_addr
      "event = local_finalize_ready height = %Ld round = %d pid = %s creator = %s lock_round = %d valid_round = %d"
      t.state.height t.state.round
      (short_hex_raw proposal_id)
      (short_addr header.creator_addr)
      t.state.locked_round t.state.valid_round;
    emit_local_finalize
      t
      ~header
      ~proposal_id
      ~round:t.state.round

let resume_voting t ~sign_fn =
  if not (local_voting_allowed t) then ()
  else begin
    (match t.pending_prevote with
     | Some (height, round, proposal_id)
       when height = t.state.height
         && round = t.state.round
         && can_prevote t ->
       t.pending_prevote <- None;
       t.state <- { t.state with step = PrevoteStep };
       (match
          cast_local_vote t ~sign_fn ~vote_type:Prevote ~proposal_id
        with
        | LocalVoteCast _
        | LocalVoteAlreadySame ->
          schedule_step_timeout t PrevoteStep
        | LocalVoteConflict _
        | LocalVoteDeferred -> ())
     | Some _ ->
       t.pending_prevote <- None
     | None -> ());
    (match
       quorum_result
         t.prevotes
         ~chain_id:t.chain_id
         ~epoch_id:t.state.height
         ~validator_set:t.vs
     with
     | `QuorumOf proposal_id
       when t.state.step <> PrecommitStep
         && not (Octra_net.Hash_domain.is_nil proposal_id) ->
       (match
          find_proposal_message
            t
            ~proposal_id
            ~round:t.state.round
        with
        | None -> ()
        | Some proposal ->
          record_polc t t.state.round proposal_id;
          t.state <- {
            t.state with
            locked_round = t.state.round;
            locked_value = Some proposal.header;
            valid_round = t.state.round;
            valid_value = Some proposal.header;
            step = PrecommitStep;
          };
          (match
             cast_local_vote t ~sign_fn ~vote_type:Precommit ~proposal_id
           with
           | LocalVoteCast _
           | LocalVoteAlreadySame
           | LocalVoteDeferred ->
             schedule_step_timeout t PrecommitStep
           | LocalVoteConflict _ -> ()))
     | _ -> ());
    if Int64.compare t.state.height t.finalized_height > 0 then
      match
        quorum_result
          t.precommits
          ~chain_id:t.chain_id
          ~epoch_id:t.state.height
          ~validator_set:t.vs
      with
      | `QuorumOf proposal_id
        when not (Octra_net.Hash_domain.is_nil proposal_id) ->
        ignore (finalize_quorum t proposal_id)
      | _ -> ()
  end

let on_ready t ~sign_fn =
  resume_voting t ~sign_fn

let do_propose ?parent_commit t (header : epoch_header)
    (tx_hashes : string list) ~sign_fn =
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
  else if
    header.proto_version <> C_protocol.version_for_epoch header.epoch_id
  then begin
    err_node t.my_addr
      "event = drop_do_propose reason = protocol_mismatch height = %Ld version = %d"
      header.epoch_id
      header.proto_version
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
               let cached_parent_commit = find_parent_commit t v_pid in
               Some
                 (v,
                  cached_hashes,
                  Some t.state.valid_round,
                  cached_parent_commit)
             end)
        | _ -> Some (header, tx_hashes, None, parent_commit)
      else
        Some (header, tx_hashes, None, parent_commit)
    in
    match resolved with
    | None -> ()
    | Some (header, _, _, resolved_parent)
      when
        C_hash.parent_commit_hash_opt resolved_parent
        <> header.parent_commit_hash ->
      err_node t.my_addr
        "event = drop_do_propose reason = parent_commit_hash_mismatch height = %Ld round = %d"
        t.state.height
        t.state.round
    | Some (header, tx_hashes, valid_round, parent_commit) ->
    let proposal_id = C_hash.proposal_id header in
    cache_proposal_bundle
      t
      proposal_id
      header
      tx_hashes
      ~parent_commit;
    let propose = {
      chain_id = t.chain_id;
      epoch_id = t.state.height;
      round = t.state.round;
      valid_round;
      header;
      tx_hashes;
      parent_commit;
      proposer = t.my_addr;
      signature = String.make 64 '\x00';
    } in
    let sign_bytes = C_hash.propose_sign_bytes propose in
    let propose = { propose with signature = sign_fn sign_bytes } in
    cache_proposal_message t propose;
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
          ignore
            (emit_local_finalize
               t
               ~header
               ~proposal_id
               ~round:t.state.round)
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
     | LocalVoteConflict _
     | LocalVoteDeferred -> ())
  end

let on_propose t (p : propose) ~verify_fn ~execute_fn ~sign_fn =
  if p.epoch_id <= t.finalized_height then ()
  else if p.epoch_id <> t.state.height then ()
  else if p.round > t.state.round + max_round_ahead then ()
  else begin
    if p.round > t.state.round then begin
      record_higher_round
        t
        ~max_ahead:(Some max_round_ahead)
        ~round:p.round
        ~validator:p.proposer;
      ignore (try_round_skip t)
    end;
  if p.round <> t.state.round then ()
  else begin
    let proposal_id = C_hash.proposal_id p.header in
    let sign_bytes = C_hash.propose_sign_bytes p in
    let proposer_valid = verify_fn p.proposer sign_bytes p.signature in
    let envelope_valid =
      proposal_is_well_formed
        ~chain_id:t.chain_id
        ~validator_set:t.vs
        p
      && C_hash.parent_commit_hash_opt p.parent_commit
         = p.header.parent_commit_hash
    in
    let root_valid = envelope_valid && execute_fn p in
    let proposal_valid = proposer_valid && envelope_valid && root_valid in
    let lock_ok = proposal_valid && lock_allows t proposal_id p.valid_round in
    let accept = proposal_valid && lock_ok in
    if proposal_valid then begin
      cache_proposal_message t p;
      if accept then t.current_proposal <- Some p;
      if not lock_ok then Option.iter (request_polc t) p.valid_round
    end;
    let finalized_from_cache =
      proposal_valid
      && local_voting_allowed t
      &&
      match
        quorum_result
          t.precommits
          ~chain_id:t.chain_id
          ~epoch_id:t.state.height
          ~validator_set:t.vs
      with
      | `QuorumOf pid when pid = proposal_id ->
        finalize_quorum t proposal_id
      | `QuorumOf _
      | `QuorumAny
      | `Added ->
        false
    in
    let vote_proposal_id = if accept then proposal_id
      else Octra_net.Hash_domain.nil_hash in
    if finalized_from_cache then ()
    else if not (can_prevote t) then ()
    else if not (local_voting_allowed t) then
      remember_pending_prevote t vote_proposal_id
    else begin
      t.state <- { t.state with step = PrevoteStep };
      let prevote_outcome =
        cast_local_vote t ~sign_fn ~vote_type:Prevote
          ~proposal_id:vote_proposal_id
      in
      let proposal_quorum =
        match
          quorum_result
            t.prevotes
            ~chain_id:t.chain_id
            ~epoch_id:t.state.height
            ~validator_set:t.vs
        with
        | `QuorumOf pid -> pid = vote_proposal_id
        | `QuorumAny
        | `Added -> false
      in
      if accept && proposal_quorum then begin
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
          ignore
            (emit_local_finalize
               t
               ~header:hdr
               ~proposal_id:vote_proposal_id
               ~round:t.state.round)
        | _ ->
          emit t (ScheduleTimeout {
            step = PrecommitStep; round = t.state.round;
            delay_ms = timeout_ms ~round:t.state.round ~step:PrecommitStep;
            generation = t.generation;
          }))
      end else
        match prevote_outcome with
        | LocalVoteCast _
        | LocalVoteAlreadySame ->
          emit t (ScheduleTimeout {
            step = PrevoteStep; round = t.state.round;
            delay_ms = timeout_ms ~round:t.state.round ~step:PrevoteStep;
            generation = t.generation;
          })
        | LocalVoteConflict _
        | LocalVoteDeferred -> ()
    end
  end
  end

let find_known_header t proposal_id =
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

let record_historical_precommit t (vote : vote) =
  if vote.round >= 0
     && vote.round < t.state.round
     && round_retained t vote.round then begin
    let votes =
      match Hashtbl.find_opt t.precommits_by_round vote.round with
      | Some value -> value
      | None ->
        let value = create_vote_set () in
        Hashtbl.add t.precommits_by_round vote.round value;
        value
    in
    let result = add_vote votes vote ~validator_set:t.vs in
    Option.iter
      (fun (round, proposal_id) ->
        emit t (RequestProposal { round; proposal_id }))
      (missing_proposal_request
         ~proposal_known:
           (Option.is_some
              (find_proposal_message
                 t
                 ~proposal_id:vote.proposal_id
                 ~round:vote.round))
         vote
         result);
    match result with
    | `QuorumOf proposal_id when local_voting_allowed t ->
      (match find_known_header t proposal_id with
       | None -> ()
       | Some header ->
         log_node t.my_addr
           "event = late_precommit_quorum height = %Ld round = %d current_round = %d pid = %s"
           t.state.height
           vote.round
           t.state.round
           (short_hex_raw proposal_id);
         ignore
           (emit_local_finalize
              t
              ~header
              ~proposal_id
              ~round:vote.round))
    | `Duplicate
    | `Rejected
    | `Added
    | `QuorumAny
    | `QuorumOf _ -> ()
  end

let on_vote t (v : vote) ~sign_fn =
  if v.epoch_id <= t.finalized_height then ()
  else if v.epoch_id <> t.state.height then ()
  else if v.round < 0 then ()
  else if v.round > t.state.round + max_round_ahead then ()
  else if v.round < t.state.round then
    match v.vote_type with
    | Prevote -> record_historical_prevote t v
    | Precommit -> record_historical_precommit t v
  else begin
    if v.round > t.state.round then begin
      record_higher_round
        t
        ~max_ahead:(Some max_round_ahead)
        ~round:v.round
        ~validator:v.validator;
      ignore (try_round_skip t)
    end;
  if v.round <> t.state.round then ()
  else
    match v.vote_type with
    | Prevote ->
      let result = add_vote t.prevotes v ~validator_set:t.vs in
      Option.iter
        (fun (round, proposal_id) ->
          emit t (RequestProposal { round; proposal_id }))
        (missing_proposal_request
           ~proposal_known:
             (Option.is_some
                (find_proposal_message
                   t
                   ~proposal_id:v.proposal_id
                   ~round:v.round))
           v
           result);
      (match result with
       | `QuorumOf proposal_id when t.state.step = PrevoteStep ->
         let nil = Octra_net.Hash_domain.is_nil proposal_id in
         let proposal =
           if nil then None
           else
             find_proposal_message
               t
               ~proposal_id
               ~round:t.state.round
         in
         (match nil, proposal with
          | false, None ->
            log_node t.my_addr
              "event = defer_precommit reason = proposal_pending height = %Ld round = %d"
              t.state.height
              t.state.round
          | _ ->
            (match proposal with
             | Some accepted ->
               record_polc t t.state.round proposal_id;
               t.state <- {
                 t.state with
                 locked_round = t.state.round;
                 locked_value = Some accepted.header;
                 valid_round = t.state.round;
                 valid_value = Some accepted.header;
               }
             | None -> ());
            if local_voting_allowed t then begin
              t.state <- { t.state with step = PrecommitStep };
              match cast_local_vote t ~sign_fn ~vote_type:Precommit
                      ~proposal_id with
              | LocalVoteCast _
              | LocalVoteAlreadySame
              | LocalVoteDeferred ->
                emit t (ScheduleTimeout {
                  step = PrecommitStep;
                  round = t.state.round;
                  delay_ms = timeout_ms ~round:t.state.round ~step:PrecommitStep;
                  generation = t.generation;
                })
              | LocalVoteConflict _ -> ()
            end)
       | `QuorumAny when t.state.step = PrevoteStep ->
         emit t (ScheduleTimeout {
           step = PrecommitStep;
           round = t.state.round;
           delay_ms = timeout_ms ~round:t.state.round ~step:PrecommitStep;
           generation = t.generation;
         })
       | _ -> ())

    | Precommit ->
      let result = add_vote t.precommits v ~validator_set:t.vs in
      Option.iter
        (fun (round, proposal_id) ->
          emit t (RequestProposal { round; proposal_id }))
        (missing_proposal_request
           ~proposal_known:
             (Option.is_some
                (find_proposal_message
                   t
                   ~proposal_id:v.proposal_id
                   ~round:v.round))
           v
           result);
      (match result with
       | `QuorumOf proposal_id when local_voting_allowed t ->
         (match find_known_header t proposal_id with
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
            ignore
              (emit_local_finalize
                 t
                 ~header
                 ~proposal_id
                 ~round:t.state.round))
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
       | LocalVoteConflict _
       | LocalVoteDeferred -> ())
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
       | LocalVoteConflict _
       | LocalVoteDeferred -> ())
    | ProposeStep | PrevoteStep | PrecommitStep ->
      start_round t (t.state.round + 1)