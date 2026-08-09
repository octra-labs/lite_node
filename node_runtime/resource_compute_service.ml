(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Capability = Octra_consensus.Resource_compute_capability
module Certificate = Octra_consensus.Resource_compute_certificate
module Engine = Resource_compute_provider_engine
module Protocol = Octra_consensus.Resource_compute_protocol
module Provider_config = Resource_compute_provider_config
module Provider_rpc = Resource_compute_provider_rpc
module Selection = Octra_consensus.Resource_compute_selection

type head = {
  epoch_id : int64;
  state_root : string;
}

type provider = {
  limits : Provider_config.limits;
  engine : Engine.t;
}

type deps = {
  chain_id : string;
  node_id : string;
  sign : string -> string;
  validator_set : unit -> Octra_consensus.C_types.validator_set;
  head : unit -> head option;
  state_root_at : int64 -> string option;
  self_test : unit -> (Provider_rpc.self_test, string) result Lwt.t;
  nonce : unit -> string;
  broadcast : string -> unit Lwt.t;
  relay : source:string -> string -> unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

type request = {
  model_epoch : int64;
  model_state_root : string;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  caller : string;
  caller_public_key : string;
  caller_signature : string;
  session_id : string;
  sequence : int64;
  method_name : string;
  params_json : string;
  max_output_bytes : int;
}

type response = {
  output_json : string;
  request : Certificate.request;
  certificate : Certificate.certificate;
  selection : Selection.selection;
}

type job_status =
  | Waiting
  | Finished of response
  | Refused of string

type cancellation = {
  caller : string;
  caller_public_key : string;
  caller_signature : string;
  session_id : string;
}

type job = {
  caller : string;
  session_id : string;
  mutable lease_active : bool;
  result : (response, string) result Lwt.t;
}

type offer_state = {
  offer : Protocol.offer;
  capabilities : (string, Capability.t) Hashtbl.t;
  commitments : (string, Selection.commitment) Hashtbl.t;
  reveals : (string, Selection.reveal) Hashtbl.t;
  pending_reveals : (string, Selection.reveal) Hashtbl.t;
  mutable local_started : bool;
}

type request_state = {
  call : Protocol.call;
  selection : Selection.selection;
  validator_set : Octra_consensus.C_types.validator_set;
  votes : (string, Protocol.result) Hashtbl.t;
}

type offer_snapshot = {
  offer : Protocol.offer;
  capabilities : Capability.t list;
  commitments : Selection.commitment list;
  reveals : Selection.reveal list;
}

type committee_lease = {
  caller : string;
  caller_public_key : string;
  session_id : string;
  circle_id : string;
  model_epoch : int64;
  model_state_root : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  validator_set_root : string;
  request_epoch : int64;
  request_state_root : string;
  expires_epoch : int64;
  max_request_bytes : int;
  max_response_bytes : int;
  selection : Selection.selection;
}

type certificate_progress =
  | Certificate_pending
  | Certificate_finished of response
  | Certificate_refused of string

type self_test = {
  executor_root : string;
  evidence_root : string;
  profile : string;
}

type _ actor_message =
  | Apply_protocol : {
      wire_key : string;
      message : Protocol.message;
    } -> bool actor_message
  | Admit_request : request -> ((string * job), string) result actor_message
  | Complete_job : {
      job_id : string;
      caller : string;
    } -> unit actor_message
  | Cancel_session : cancellation -> (int, string) result actor_message
  | Complete_background : int -> unit actor_message
  | Remember_committee : committee_lease -> unit actor_message
  | Query_status : {
      caller : string;
      job_id : string;
    } -> job_status option actor_message
  | Query_active_jobs : int actor_message
  | Query_offer : string -> offer_snapshot option actor_message
  | Query_certificate : string -> certificate_progress actor_message
  | Ensure_self_test : (self_test, string) result actor_message
  | Stop : unit actor_message

type actor_command =
  | Command : {
      generation : int64;
      message : 'reply actor_message;
      reply : 'reply Lwt.u;
    } -> actor_command

type t = {
  deps : deps;
  provider : provider option;
  seen : Octra_consensus.C_seen.t;
  executed : Octra_consensus.C_seen.t;
  offers : (string, offer_state) Hashtbl.t;
  requests : (string, request_state) Hashtbl.t;
  sequences : (string, int64) Hashtbl.t;
  jobs : (string, job) Hashtbl.t;
  job_order : string Queue.t;
  caller_jobs : (string, int) Hashtbl.t;
  cancelled_sessions : (string, int64) Hashtbl.t;
  committee_leases : (string, committee_lease) Hashtbl.t;
  background_tasks : (int, unit Lwt.t) Hashtbl.t;
  mutable next_background_id : int;
  data_channel : actor_command Queue.t;
  control_channel : actor_command Queue.t;
  channel_ready : unit Lwt_condition.t;
  channel_space : unit Lwt_condition.t;
  mutable channel_open : bool;
  mutable actor_generation : int64;
  mutable self_test : self_test option;
}

let max_committee_members = 5
let reveal_delay = 1L
let offer_ttl = 16L
let call_ttl = 64L
let capability_ttl = 16L
let max_offers = 64
let max_requests = 128
let max_jobs = 32
let max_cancelled_sessions = 256
let max_committee_leases = 256
let max_offer_records = 256
let max_pending_reveals_per_node = 4
let data_channel_limit = 512
let control_channel_limit = 64
let zero32 = String.make 32 '\000'
let zero64 = String.make 64 '\000'

let committee_members validator_set =
  min max_committee_members validator_set.Octra_consensus.C_types.n

let actor_message_name : type reply. reply actor_message -> string = function
  | Apply_protocol _ -> "apply_protocol"
  | Admit_request _ -> "admit_request"
  | Complete_job _ -> "complete_job"
  | Cancel_session _ -> "cancel_session"
  | Complete_background _ -> "complete_background"
  | Remember_committee _ -> "remember_committee"
  | Query_status _ -> "query_status"
  | Query_active_jobs -> "query_active_jobs"
  | Query_offer _ -> "query_offer"
  | Query_certificate _ -> "query_certificate"
  | Ensure_self_test -> "ensure_self_test"
  | Stop -> "stop"

let actor_message_is_control : type reply. reply actor_message -> bool = function
  | Complete_job _
  | Cancel_session _
  | Complete_background _
  | Remember_committee _
  | Stop -> true
  | Apply_protocol _
  | Admit_request _
  | Query_status _
  | Query_active_jobs
  | Query_offer _
  | Query_certificate _
  | Ensure_self_test -> false

let enqueue_now t (Command command as envelope) =
  let channel, limit =
    if actor_message_is_control command.message then
      t.control_channel, control_channel_limit
    else
      t.data_channel, data_channel_limit in
  if not t.channel_open || Queue.length channel >= limit then false
  else begin
    Queue.push envelope channel;
    Lwt_condition.signal t.channel_ready ();
    true
  end

let rec enqueue_wait t command =
  if enqueue_now t command then Lwt.return_true
  else if not t.channel_open then Lwt.return_false
  else
    let* () = Lwt_condition.wait t.channel_space in
    enqueue_wait t command

let actor_call_with enqueue t message =
  let result, set_result = Lwt.wait () in
  let command = Command {
    generation = t.actor_generation;
    message;
    reply = set_result;
  } in
  let* admitted = enqueue t command in
  if admitted then result
  else Lwt.fail_with "resource compute actor is stopping"

let actor_call t message =
  actor_call_with enqueue_wait t message

let spawn_background t kind run =
  let task_id = t.next_background_id in
  t.next_background_id <- t.next_background_id + 1;
  let task =
    Lwt.finalize
      (fun () ->
        Lwt.catch
          run
          (function
           | Lwt.Canceled -> Lwt.return_unit
           | Failure message
             when message = "resource compute actor is stopping"
               || message = "resource compute actor stopped" ->
             Lwt.return_unit
           | error ->
             Log.warn "compute"
               "event = background status = failed kind = %s reason = %s"
               kind
               (Printexc.to_string error);
             Lwt.return_unit))
      (fun () ->
        Lwt.catch
          (fun () -> actor_call t (Complete_background task_id))
          (fun _ -> Lwt.return_unit)) in
  Hashtbl.add t.background_tasks task_id task

let actor_try_call t message =
  actor_call_with
    (fun t command -> Lwt.return (enqueue_now t command))
    t
    message

let provider_enabled t =
  Option.is_some t.provider

let raw_of_hex value =
  if Text.is_hex_len 64 value then Ok (Text.hex_to_string value)
  else Error "resource compute hash must be lowercase hex64"

let hex_of_raw value =
  Text.raw_to_hex value

let hash32 value =
  String.length value = 32

let signature64 value =
  String.length value = 64

let current_head t =
  match t.deps.head () with
  | Some head when head.epoch_id >= 0L && hash32 head.state_root -> Ok head
  | _ -> Error "resource compute finalized head unavailable"

let current_validator_set t =
  let validator_set = t.deps.validator_set () in
  let root = Octra_consensus.C_config.validator_set_hash validator_set in
  validator_set, root

let verify_node validator_set node_id message signature =
  match Octra_consensus.C_types.pubkey_of_addr validator_set node_id with
  | None -> false
  | Some pubkey_raw ->
    Octra_consensus.C_hash.verify_ed25519
      ~pubkey_raw
      ~msg:message
      ~signature

let wire_id payload =
  Octra_net.Hash_domain.hash "octra:resource_compute_wire" payload

let offer_id offer =
  Protocol.offer_id offer

let request_id call =
  Certificate.request_id call.Protocol.request

let offer_model_matches offer ~circle_id ~model_epoch ~model_state_root
    ~graph_root ~model_root ~program_root ~executor_root =
  offer.Protocol.circle_id = circle_id
  && offer.model_epoch = model_epoch
  && offer.model_state_root = model_state_root
  && offer.graph_root = graph_root
  && offer.model_root = model_root
  && offer.program_root = program_root
  && offer.executor_root = executor_root

let offer_matches_capability offer (capability : Capability.t) =
  offer_model_matches
    offer
    ~circle_id:capability.circle_id
    ~model_epoch:capability.model_epoch
    ~model_state_root:capability.model_state_root
    ~graph_root:capability.graph_root
    ~model_root:capability.model_root
    ~program_root:capability.program_root
    ~executor_root:capability.executor_root
  && capability.validator_set_root = offer.validator_set_root
  && capability.observed_epoch >= offer.offered_epoch
  && capability.valid_until <= offer.expires_epoch
  && capability.memory_bytes >= offer.min_memory_bytes
  && capability.max_request_bytes >= offer.request_bytes
  && capability.max_response_bytes >= offer.response_bytes

let offer_matches_commitment offer (commitment : Selection.commitment) =
  commitment.chain_id = offer.Protocol.chain_id
  && commitment.offer_id = offer_id offer
  && commitment.commit_epoch >= offer.offered_epoch
  && commitment.commit_epoch <= offer.expires_epoch
  && commitment.graph_root = offer.graph_root
  && commitment.model_root = offer.model_root
  && commitment.program_root = offer.program_root
  && commitment.executor_root = offer.executor_root

let matching_offers t predicate =
  Hashtbl.fold
    (fun _ (state : offer_state) values ->
      if predicate state.offer then state :: values else values)
    t.offers
    []

let table_values table =
  Hashtbl.fold (fun _ value values -> value :: values) table []

let offer_snapshot (state : offer_state) = {
  offer = state.offer;
  capabilities = table_values state.capabilities;
  commitments = table_values state.commitments;
  reveals = table_values state.reveals;
}

let add_exact table key value =
  match Hashtbl.find_opt table key with
  | None ->
    Hashtbl.add table key value;
    true
  | Some current -> current = value

let add_limited table key value =
  match Hashtbl.find_opt table key with
  | Some current -> current = value
  | None when Hashtbl.length table >= max_offer_records -> false
  | None ->
    Hashtbl.add table key value;
    true

let pending_reveal_key reveal =
  Protocol.Reveal reveal |> Protocol.encode |> wire_id

let pending_reveal_count state node_id =
  Hashtbl.fold
    (fun _ (reveal : Selection.reveal) count ->
      if reveal.Selection.node_id = node_id then count + 1 else count)
    state.pending_reveals
    0

let ensure_self_test_direct t =
  match t.self_test with
  | Some value -> Lwt.return (Ok value)
  | None ->
    let* result = t.deps.self_test () in
    begin
      match result with
      | Error _ as error -> Lwt.return error
      | Ok value ->
        begin
          match raw_of_hex value.Provider_rpc.executor_root,
                raw_of_hex value.evidence_root with
          | Ok executor_root, Ok evidence_root ->
            let parsed = { executor_root; evidence_root; profile = value.profile } in
            t.self_test <- Some parsed;
            Lwt.return (Ok parsed)
          | Error message, _ | _, Error message -> Lwt.return (Error message)
        end
    end

let verify_offer t offer =
  match current_head t with
  | Error _ -> false
  | Ok head ->
    let validator_set, validator_set_root = current_validator_set t in
    Protocol.offer_shape offer
    && offer.chain_id = t.deps.chain_id
    && offer.validator_set_root = validator_set_root
    && t.deps.state_root_at offer.model_epoch = Some offer.model_state_root
    && offer.offered_epoch <= head.epoch_id
    && head.epoch_id <= offer.expires_epoch
    && Int64.sub offer.expires_epoch offer.offered_epoch <= offer_ttl
    && Octra_consensus.C_types.is_validator validator_set offer.coordinator
    && verify_node
         validator_set
         offer.coordinator
         (Protocol.offer_sign_bytes offer)
         offer.signature

let verify_capability t capability =
  let validator_set, validator_set_root = current_validator_set t in
  let current_epoch =
    match current_head t with
    | Ok head -> head.epoch_id
    | Error _ -> -1L in
  let* test_result = ensure_self_test_direct t in
  match test_result with
  | Error _ -> Lwt.return_false
  | Ok test ->
    let result =
      Capability.validate
        ~chain_id:t.deps.chain_id
        ~current_epoch
        ~max_ttl:capability_ttl
        ~is_validator:(Octra_consensus.C_types.is_validator validator_set)
        ~verify_signature:(fun value ->
          verify_node
            validator_set
            value.Capability.node_id
            (Capability.sign_bytes value)
            value.signature)
        ~verify_evidence:(fun value ->
          value.Capability.validator_set_root = validator_set_root
          && value.executor_root = test.executor_root
          && value.evidence_root = test.evidence_root)
        capability in
    Lwt.return (Result.is_ok result)

let verify_commitment validator_set (commitment : Selection.commitment) =
  signature64 commitment.Selection.signature
  && verify_node
       validator_set
       commitment.node_id
       (Selection.commitment_sign_bytes commitment)
       commitment.signature

let capabilities_for (state : offer_state) node_id =
  table_values state.capabilities
  |> List.filter (fun value -> value.Capability.node_id = node_id)

let commitments_for (state : offer_state) node_id =
  table_values state.commitments
  |> List.filter
       (fun (value : Selection.commitment) -> value.Selection.node_id = node_id)

type reveal_validation =
  | Reveal_wait
  | Reveal_valid of string
  | Reveal_invalid

let validate_pending_reveal t state (reveal : Selection.reveal) =
  let validator_set, _ = current_validator_set t in
  let commitments = commitments_for state reveal.Selection.node_id in
  let matching_commitments =
    List.filter
      (fun (commitment : Selection.commitment) ->
        commitment.offer_id = reveal.offer_id
        && commitment.commit_epoch = reveal.commit_epoch
        && commitment.graph_root = reveal.graph_root
        && commitment.model_root = reveal.model_root
        && commitment.program_root = reveal.program_root
        && commitment.executor_root = reveal.executor_root
        && commitment.nonce_hash = Selection.nonce_hash reveal.nonce)
      commitments in
  let capabilities = capabilities_for state reveal.node_id in
  match matching_commitments, capabilities, current_head t with
  | [], _, _ ->
    if commitments = [] then Reveal_wait else Reveal_invalid
  | _, [], _ -> Reveal_wait
  | [commitment], capabilities, Ok head ->
    let result =
      Selection.validate
        ~chain_id:t.deps.chain_id
        ~target_epoch:(Int64.succ head.epoch_id)
        ~minimum_delay:reveal_delay
        ~verify_commitment_signature:(verify_commitment validator_set)
        ~verify_reveal_signature:(fun commitment value ->
          verify_node
            validator_set
            value.Selection.node_id
            (Selection.reveal_sign_bytes commitment value)
            value.signature)
        ~verify_evidence:(fun value ->
          List.exists
            (fun capability ->
              value.Selection.evidence_root = capability.Capability.evidence_root)
            capabilities)
        commitment
        reveal in
    begin
      match result with
      | Ok _ -> Reveal_valid (Selection.reveal_id commitment reveal)
      | Error Selection.Evidence -> Reveal_wait
      | Error _ -> Reveal_invalid
    end
  | _ :: _ :: _, _, _ -> Reveal_invalid
  | _, _, Error _ -> Reveal_wait

let promote_pending_reveals t state node_id =
  let pending =
    Hashtbl.fold
      (fun key (reveal : Selection.reveal) values ->
        if reveal.Selection.node_id = node_id then (key, reveal) :: values else values)
      state.pending_reveals
      [] in
  List.iter
    (fun (key, reveal) ->
      match validate_pending_reveal t state reveal with
      | Reveal_wait -> ()
      | Reveal_valid id ->
        if add_limited state.reveals id reveal then
          Hashtbl.remove state.pending_reveals key
      | Reveal_invalid -> Hashtbl.remove state.pending_reveals key)
    pending

let verify_caller call =
  let request = call.Protocol.request in
  let public_key_b64 = Base64.encode_exn call.caller_public_key in
  Octra_core.Crypto.Address.verify_address_pubkey request.caller public_key_b64
  && Octra_ed25519.safe call.caller_public_key
  && Octra_ed25519.verify
       ~pub:call.caller_public_key
       ~msg:(Protocol.intent_sign_bytes call)
       call.caller_signature

let selection_for t (state : offer_state) request =
  let validator_set, _ = current_validator_set t in
  let capabilities = table_values state.capabilities in
  let commitments = table_values state.commitments in
  let reveals = table_values state.reveals in
  let committee_size = committee_members validator_set in
  let capability_for_node node_id =
    List.find_opt
      (fun capability -> capability.Capability.node_id = node_id)
      capabilities in
  let challenge =
    Selection.challenge
      ~chain_id:t.deps.chain_id
      ~source_epoch:request.Certificate.epoch_id
      ~finality_hash:request.state_root in
  Selection.select
    ~chain_id:t.deps.chain_id
    ~offer_id:(offer_id state.offer)
    ~target_epoch:request.epoch_id
    ~challenge
    ~minimum_delay:reveal_delay
    ~committee_size
    ~verify_commitment_signature:(verify_commitment validator_set)
    ~verify_reveal_signature:(fun commitment reveal ->
      verify_node
        validator_set
        reveal.Selection.node_id
        (Selection.reveal_sign_bytes commitment reveal)
        reveal.signature)
    ~verify_evidence:(fun reveal ->
      match capability_for_node reveal.Selection.node_id with
      | Some capability -> reveal.evidence_root = capability.Capability.evidence_root
      | None -> false)
    ~commitments
    ~reveals

let selection_for_snapshot t validator_set (snapshot : offer_snapshot) request =
  let committee_size = committee_members validator_set in
  let capability_for_node node_id =
    List.find_opt
      (fun capability -> capability.Capability.node_id = node_id)
      snapshot.capabilities in
  let challenge =
    Selection.challenge
      ~chain_id:t.deps.chain_id
      ~source_epoch:request.Certificate.epoch_id
      ~finality_hash:request.state_root in
  Selection.select
    ~chain_id:t.deps.chain_id
    ~offer_id:(offer_id snapshot.offer)
    ~target_epoch:request.epoch_id
    ~challenge
    ~minimum_delay:reveal_delay
    ~committee_size
    ~verify_commitment_signature:(verify_commitment validator_set)
    ~verify_reveal_signature:(fun commitment reveal ->
      verify_node
        validator_set
        reveal.Selection.node_id
        (Selection.reveal_sign_bytes commitment reveal)
        reveal.signature)
    ~verify_evidence:(fun reveal ->
      match capability_for_node reveal.Selection.node_id with
      | Some capability -> reveal.evidence_root = capability.Capability.evidence_root
      | None -> false)
    ~commitments:snapshot.commitments
    ~reveals:snapshot.reveals

let selected_ids selection =
  List.map (fun member -> member.Selection.node_id) selection.Selection.members

let signer_floor count =
  count - ((count - 1) / 3)

let weight_floor validator_set selected =
  let total =
    List.fold_left
      (fun value node_id ->
        match Octra_consensus.C_types.weight_of_addr validator_set node_id with
        | Some weight -> Z.add value weight
        | None -> value)
      Z.zero
      selected in
  Z.(add (div (mul total (of_int 2)) (of_int 3)) one)

let capability_supports_request
    (state : offer_state)
    request
    current_epoch
    (member : Selection.member) =
  table_values state.capabilities
  |> List.exists (fun capability ->
    capability.Capability.node_id = member.node_id
    && capability.evidence_root = member.evidence_root
    && capability.validator_set_root = request.Certificate.validator_set_root
    && capability.observed_epoch <= request.epoch_id
    && current_epoch <= capability.valid_until
    && capability.max_request_bytes >= request.input_bytes
    && capability.max_response_bytes >= request.max_output_bytes)

let selection_supports_request state request current_epoch selection =
  List.for_all
    (capability_supports_request state request current_epoch)
    selection.Selection.members

let find_offer_for_request t request current_epoch =
  matching_offers t (fun offer ->
    offer.Protocol.validator_set_root = request.Certificate.validator_set_root
    && offer.offered_epoch <= request.epoch_id
    && request.epoch_id <= offer.expires_epoch
    && current_epoch <= offer.expires_epoch
    && offer_model_matches
         offer
         ~circle_id:request.circle_id
         ~model_epoch:request.model_epoch
         ~model_state_root:request.model_state_root
         ~graph_root:request.graph_root
         ~model_root:request.model_root
         ~program_root:request.program_root
         ~executor_root:request.executor_root)
  |> List.filter_map (fun state ->
    try
      let selection = selection_for t state request in
      if
        selection.Selection.root = request.selection_root
        && selection_supports_request state request current_epoch selection
      then Some (offer_id state.offer, state, selection)
      else None
    with Invalid_argument _ -> None)
  |> List.sort (fun (left, _, _) (right, _, _) -> String.compare left right)
  |> function
  | (_, state, selection) :: _ -> Some (state, selection)
  | [] -> None

let verify_call t call =
  if not (Protocol.call_shape call) then Error "resource compute call shape refused"
  else
    let request = call.Protocol.request in
    match current_head t with
    | Error message -> Error message
    | Ok head ->
      let validator_set, validator_set_root = current_validator_set t in
      if request.chain_id <> t.deps.chain_id then
        Error "resource compute call chain differs"
      else if
        request.epoch_id > head.epoch_id
        || t.deps.state_root_at request.epoch_id <> Some request.state_root
        || request.expires_epoch < head.epoch_id
        || Int64.sub request.expires_epoch request.epoch_id > call_ttl
      then
        Error "resource compute call epoch differs"
      else if request.validator_set_root <> validator_set_root then
        Error "resource compute validator set differs"
      else if not (Octra_consensus.C_types.is_validator validator_set call.coordinator) then
        Error "resource compute coordinator is not a validator"
      else if not
          (verify_node
             validator_set
             call.coordinator
             (Protocol.call_sign_bytes call)
             call.signature)
      then
        Error "resource compute coordinator signature refused"
      else if not (verify_caller call) then
        Error "resource compute caller signature refused"
      else
        match find_offer_for_request t request head.epoch_id with
        | None -> Error "resource compute offer unavailable"
        | Some (state, selection) ->
          if
            List.length selection.Selection.members
            <> committee_members validator_set
            || selection.root <> request.selection_root
          then
            Error "resource compute selection differs"
          else
            Ok (state, selection, validator_set)

let request_state t call selection validator_set =
  let id = request_id call in
  match Hashtbl.find_opt t.requests id with
  | Some state -> state
  | None ->
    if Hashtbl.length t.requests >= max_requests then
      invalid_arg "resource compute request table full";
    let state = {
      call;
      selection;
      validator_set;
      votes = Hashtbl.create max_committee_members;
    } in
    Hashtbl.add t.requests id state;
    state

let session_key call =
  call.Protocol.request.caller ^ ":" ^ hex_of_raw call.request.session_id

let sequence_admitted t call =
  let key = session_key call in
  match Hashtbl.find_opt t.sequences key with
  | None ->
    Hashtbl.replace t.sequences key call.Protocol.request.sequence;
    true
  | Some sequence when call.request.sequence > sequence ->
    Hashtbl.replace t.sequences key call.request.sequence;
    true
  | Some _ -> false

let record_capability t capability =
  let states = matching_offers t (fun offer -> offer_matches_capability offer capability) in
  List.fold_left
    (fun accepted (state : offer_state) ->
      let added = add_limited state.capabilities (Capability.id capability) capability in
      if added then promote_pending_reveals t state capability.node_id;
      added || accepted)
    false
    states

let record_commitment t commitment =
  let validator_set, _ = current_validator_set t in
  if not (verify_commitment validator_set commitment) then false
  else
    match Hashtbl.find_opt t.offers commitment.Selection.offer_id with
    | Some state when offer_matches_commitment state.offer commitment ->
        let added =
          add_limited
            state.commitments
            (Selection.commitment_id commitment)
            commitment in
        if added then promote_pending_reveals t state commitment.node_id;
        added
    | Some _
    | None -> false

let record_reveal t reveal =
  let validator_set, _ = current_validator_set t in
  if
    not (Selection.reveal_is_well_formed reveal)
    || not (Octra_consensus.C_types.is_validator validator_set reveal.node_id)
  then false
  else
    match Hashtbl.find_opt t.offers reveal.Selection.offer_id with
    | Some state
      when state.offer.Protocol.chain_id = reveal.chain_id
           && state.offer.graph_root = reveal.graph_root
           && state.offer.model_root = reveal.model_root
           && state.offer.program_root = reveal.program_root
           && state.offer.executor_root = reveal.executor_root ->
        let key = pending_reveal_key reveal in
        let existing = Hashtbl.find_opt state.pending_reveals key in
        let admitted =
          match existing with
          | Some current -> current = reveal
          | None
            when Hashtbl.length state.pending_reveals >= max_offer_records
              || pending_reveal_count state reveal.node_id
                 >= max_pending_reveals_per_node ->
            false
          | None ->
            Hashtbl.add state.pending_reveals key reveal;
            true in
        if admitted then promote_pending_reveals t state reveal.node_id;
        admitted
    | Some _
    | None -> false

let record_result t result =
  if not (Protocol.result_shape result) then false
  else
    let vote = result.Protocol.vote in
    match Hashtbl.find_opt t.requests vote.Certificate.request_id with
    | None -> false
    | Some state ->
      let selected = selected_ids state.selection in
      if not (List.mem vote.node_id selected) then false
      else if not
          (verify_node
             state.validator_set
             vote.node_id
             (Certificate.vote_sign_bytes vote)
             vote.signature)
      then false
      else add_exact state.votes vote.node_id result

let provider_accelerator = function
  | Provider_config.Cpu -> Capability.Cpu
  | Provider_config.Cuda -> Capability.Cuda
  | Provider_config.Metal -> Capability.Metal
  | Provider_config.Rocm -> Capability.Rocm

let signed_capability
    t
    (offer : Protocol.offer)
    (test : self_test)
    (limits : Provider_config.limits)
    observed_epoch =
  let unsigned = Capability.{
    chain_id = offer.Protocol.chain_id;
    observed_epoch;
    valid_until = offer.expires_epoch;
    validator_set_root = offer.validator_set_root;
    node_id = t.deps.node_id;
    circle_id = offer.circle_id;
    model_epoch = offer.model_epoch;
    model_state_root = offer.model_state_root;
    graph_root = offer.graph_root;
    model_root = offer.model_root;
    program_root = offer.program_root;
    executor_root = offer.executor_root;
    evidence_root = test.evidence_root;
    accelerator = provider_accelerator limits.Provider_config.accelerator;
    lanes = limits.lanes;
    memory_bytes = limits.memory_bytes;
    max_request_bytes = limits.max_request_bytes;
    max_response_bytes = limits.max_response_bytes;
    signature = zero64;
  } in
  { unsigned with signature = t.deps.sign (Capability.sign_bytes unsigned) }

let signed_commitment t offer epoch nonce =
  let unsigned = Selection.{
    chain_id = offer.Protocol.chain_id;
    offer_id = offer_id offer;
    commit_epoch = epoch;
    node_id = t.deps.node_id;
    graph_root = offer.graph_root;
    model_root = offer.model_root;
    program_root = offer.program_root;
    executor_root = offer.executor_root;
    nonce_hash = Selection.nonce_hash nonce;
    signature = zero64;
  } in
  { unsigned with signature = t.deps.sign (Selection.commitment_sign_bytes unsigned) }

let signed_reveal t (commitment : Selection.commitment) nonce evidence_root reveal_epoch =
  let unsigned : Selection.reveal = Selection.{
    chain_id = commitment.chain_id;
    offer_id = commitment.offer_id;
    commit_epoch = commitment.commit_epoch;
    reveal_epoch;
    node_id = commitment.node_id;
    graph_root = commitment.graph_root;
    model_root = commitment.model_root;
    program_root = commitment.program_root;
    executor_root = commitment.executor_root;
    nonce;
    evidence_root;
    signature = zero64;
  } in
  { unsigned with
    signature =
      t.deps.sign (Selection.reveal_sign_bytes commitment unsigned) }

let prepare_request offer = Provider_rpc.{
  model_epoch = offer.Protocol.model_epoch;
  model_state_root = hex_of_raw offer.model_state_root;
  circle_id = offer.circle_id;
  graph_root = hex_of_raw offer.graph_root;
  model_root = hex_of_raw offer.model_root;
  program_root = hex_of_raw offer.program_root;
  executor_root = hex_of_raw offer.executor_root;
}

let execute_request call =
  let request = call.Protocol.request in
  Provider_rpc.{
    request_id = hex_of_raw (Certificate.request_id request);
    session_id = hex_of_raw request.session_id;
    epoch_id = request.epoch_id;
    state_root = hex_of_raw request.state_root;
    model_epoch = request.model_epoch;
    model_state_root = hex_of_raw request.model_state_root;
    circle_id = request.circle_id;
    graph_root = hex_of_raw request.graph_root;
    model_root = hex_of_raw request.model_root;
    program_root = hex_of_raw request.program_root;
    executor_root = hex_of_raw request.executor_root;
    caller = request.caller;
    method_name = call.method_name;
    params_json = call.params_json;
    program_b64 = "";
    program_runtime = "wasm_v1";
    storage = [];
    max_output_bytes = request.max_output_bytes;
  }

let signed_vote t call executed =
  let unsigned = Certificate.{
    request_id = Certificate.request_id call.Protocol.request;
    node_id = t.deps.node_id;
    output_hash = Text.hex_to_string executed.Provider_rpc.output_hash;
    trace_root = Text.hex_to_string executed.trace_root;
    output_bytes = String.length executed.output_json;
    steps = executed.steps;
    operations = executed.operations;
    signature = zero64;
  } in
  { unsigned with signature = t.deps.sign (Certificate.vote_sign_bytes unsigned) }

let protocol_message_name = function
  | Protocol.Offer _ -> "offer"
  | Protocol.Capability _ -> "capability"
  | Protocol.Commitment _ -> "commitment"
  | Protocol.Reveal _ -> "reveal"
  | Protocol.Call _ -> "call"
  | Protocol.Result _ -> "result"

let rec publish t message =
  let payload = Protocol.encode message in
  let* accepted =
    actor_call t (Apply_protocol { wire_key = wire_id payload; message }) in
  if accepted then t.deps.broadcast payload
  else begin
    Log.warn "compute"
      "event = local_protocol status = refused message = %s"
      (protocol_message_name message);
    Lwt.return_unit
  end

and apply_message_direct t message =
  match message with
  | Protocol.Offer offer ->
    if not (verify_offer t offer) then Lwt.return_false
    else
      let id = offer_id offer in
      begin
        match Hashtbl.find_opt t.offers id with
        | Some _ -> Lwt.return_true
        | None when Hashtbl.length t.offers >= max_offers -> Lwt.return_false
        | None ->
          let state = {
            offer;
            capabilities = Hashtbl.create max_committee_members;
            commitments = Hashtbl.create max_committee_members;
            reveals = Hashtbl.create max_committee_members;
            pending_reveals = Hashtbl.create max_committee_members;
            local_started = false;
          } in
          Hashtbl.add t.offers id state;
          begin
            match t.provider with
            | None -> ()
            | Some _ ->
              state.local_started <- true;
              spawn_background t "provider_flow" (fun () -> local_provider_flow t offer)
          end;
          Lwt.return_true
      end
  | Protocol.Capability capability ->
    let* valid = verify_capability t capability in
    Lwt.return (valid && record_capability t capability)
  | Protocol.Commitment commitment ->
    Lwt.return (record_commitment t commitment)
  | Protocol.Reveal reveal ->
    Lwt.return (record_reveal t reveal)
  | Protocol.Call call ->
    begin
      match verify_call t call with
      | Error reason ->
        Log.warn "compute"
          "event = call status = refused reason = %s"
          reason;
        Lwt.return_false
      | Ok (_, selection, validator_set) ->
        let _ = request_state t call selection validator_set in
        let selected = List.mem t.deps.node_id (selected_ids selection) in
        let sequence_ok = selected && sequence_admitted t call in
        let executable =
          sequence_ok
          && Octra_consensus.C_seen.remember t.executed (request_id call) in
        Log.info "compute"
          "event = call status = accepted selected = %b executable = %b"
          selected
          executable;
        if executable then
          spawn_background t "provider_execution" (fun () -> execute_selected_call t call);
        Lwt.return_true
    end
  | Protocol.Result result ->
    Lwt.return (record_result t result)

and local_provider_flow t offer =
  match t.provider, current_head t with
  | Some provider, Ok head
    when head.epoch_id <= offer.Protocol.expires_epoch ->
    let validator_set, _ = current_validator_set t in
    if not (Octra_consensus.C_types.is_validator validator_set t.deps.node_id) then
      Lwt.return_unit
    else if
      provider.limits.Provider_config.memory_bytes < offer.min_memory_bytes
      || provider.limits.max_request_bytes < offer.request_bytes
      || provider.limits.max_response_bytes < offer.response_bytes
    then
      Lwt.return_unit
    else
      let () =
        Log.info "compute"
          "event = provider_candidate status = checking phase = self_test" in
      let* test_result =
        actor_call t Ensure_self_test in
      begin
        match test_result with
        | Error reason ->
          Log.warn "compute"
            "event = provider_candidate status = refused phase = self_test reason = %s"
            reason;
          Lwt.return_unit
        | Ok test when test.executor_root <> offer.executor_root ->
          Lwt.return_unit
        | Ok test ->
          Log.info "compute"
            "event = provider_candidate status = checking phase = prepare";
          let* prepared_result =
            Engine.prepare provider.engine (prepare_request offer) in
          begin
            match prepared_result, current_head t with
            | Ok _, Ok current when current.epoch_id <= offer.expires_epoch ->
              let capability =
                signed_capability t offer test provider.limits current.epoch_id in
              let nonce = t.deps.nonce () in
              if String.length nonce = 0 || String.length nonce > 128 then
                Lwt.return_unit
              else
                let commitment = signed_commitment t offer current.epoch_id nonce in
              let* () = publish t (Protocol.Capability capability) in
              let* () = publish t (Protocol.Commitment commitment) in
              Log.info "compute"
                "event = provider_candidate status = committed commit_epoch = %Ld"
                current.epoch_id;
              schedule_reveal
                  t
                  offer.expires_epoch
                  commitment
                  test.evidence_root
                  nonce
            | Error reason, _ ->
              Log.warn "compute"
                "event = provider_candidate status = refused phase = prepare reason = %s"
                reason;
              Lwt.return_unit
            | Ok _, Error reason ->
              Log.warn "compute"
                "event = provider_candidate status = refused phase = head reason = %s"
                reason;
              Lwt.return_unit
            | Ok _, Ok _ -> Lwt.return_unit
          end
      end
  | _ -> Lwt.return_unit

and schedule_reveal t expires_epoch commitment evidence_root nonce =
  match current_head t with
  | Error _ -> Lwt.return_unit
  | Ok head when head.epoch_id > expires_epoch -> Lwt.return_unit
  | Ok head when Int64.sub head.epoch_id commitment.Selection.commit_epoch >= reveal_delay ->
    let reveal = signed_reveal t commitment nonce evidence_root head.epoch_id in
    Log.info "compute"
      "event = provider_candidate status = revealed reveal_epoch = %Ld"
      head.epoch_id;
    publish t (Protocol.Reveal reveal)
  | Ok _ ->
    let* () = t.deps.sleep 0.5 in
    schedule_reveal t expires_epoch commitment evidence_root nonce

and execute_selected_call t call =
  match t.provider with
  | None -> Lwt.return_unit
  | Some provider ->
    let session = hex_of_raw call.Protocol.request.session_id in
    Log.info "compute"
      "event = provider_execution status = started method = %s session = %s sequence = %Ld"
      call.method_name
      (String.sub session 0 12)
      call.request.sequence;
    let* executed_result =
      Engine.execute provider.engine (execute_request call) in
    begin
      match executed_result with
      | Error reason ->
        Log.warn "compute"
          "event = provider_execution status = refused reason = %s"
          reason;
        Lwt.return_unit
      | Ok executed ->
        Log.info "compute"
          "event = provider_execution status = finished effort = %d steps = %Ld operations = %Ld"
          executed.effort_used
          executed.steps
          executed.operations;
        let vote = signed_vote t call executed in
        publish t (Protocol.Result { vote; output_json = executed.output_json })
    end

let on_message t ~source payload =
  match Protocol.decode payload with
  | exception _ -> Lwt.return_unit
  | message ->
    let* accepted =
      Lwt.catch
        (fun () ->
          actor_try_call t
            (Apply_protocol { wire_key = wire_id payload; message }))
        (fun _ -> Lwt.return_false) in
    if accepted then t.deps.relay ~source payload else Lwt.return_unit

let ready_nodes snapshot =
  let commitments =
    List.map
      (fun (value : Selection.commitment) -> value.Selection.node_id)
      snapshot.commitments in
  let reveals =
    List.map
      (fun (value : Selection.reveal) -> value.Selection.node_id)
      snapshot.reveals in
  snapshot.capabilities
  |> List.filter_map (fun capability ->
    let node_id = capability.Capability.node_id in
    if List.mem node_id commitments && List.mem node_id reveals then Some node_id
    else None)
  |> List.sort_uniq String.compare

let rec wait_for_candidates t offer_key =
  let* snapshot_result = actor_call t (Query_offer offer_key) in
  match snapshot_result, current_head t with
  | None, _ -> Lwt.return (Error "resource compute offer unavailable")
  | _, Error message -> Lwt.return (Error message)
  | Some snapshot, Ok head when head.epoch_id > snapshot.offer.Protocol.expires_epoch ->
    Lwt.return (Error "resource compute committee formation expired")
  | Some snapshot, Ok head ->
    let nodes = ready_nodes snapshot in
    let validator_set, _ = current_validator_set t in
    let committee_size = committee_members validator_set in
    let latest_reveal =
      List.fold_left
        (fun epoch reveal -> Int64.max epoch reveal.Selection.reveal_epoch)
        (-1L)
        snapshot.reveals in
    if
      committee_size > 0
      && List.length nodes >= committee_size
      && head.epoch_id > latest_reveal
    then
      let () =
        Log.info "compute"
          "event = committee status = ready candidates = %d target = %d"
          (List.length nodes)
          committee_size in
      Lwt.return (Ok (head, snapshot))
    else
      let* () = t.deps.sleep 0.1 in
      wait_for_candidates t offer_key

let decision state =
  let request = state.call.Protocol.request in
  let selected = selected_ids state.selection in
  let votes =
    Hashtbl.fold
      (fun _ result values -> result.Protocol.vote :: values)
      state.votes
      [] in
  Certificate.decide
    ~chain_id:request.Certificate.chain_id
    ~current_epoch:request.epoch_id
    ~max_ttl:call_ttl
    ~selected
    ~signer_floor:(signer_floor (List.length selected))
    ~weight_floor:(weight_floor state.validator_set selected)
    ~weight_of:(Octra_consensus.C_types.weight_of_addr state.validator_set)
    ~verify_signature:(fun vote ->
      verify_node
        state.validator_set
        vote.Certificate.node_id
        (Certificate.vote_sign_bytes vote)
        vote.signature)
    request
    votes

let output_for_certificate state certificate =
  Hashtbl.fold
    (fun _ result output ->
      if
        result.Protocol.vote.output_hash = certificate.Certificate.output_hash
        && result.vote.trace_root = certificate.trace_root
        && result.vote.output_bytes = certificate.output_bytes
        && result.vote.steps = certificate.steps
        && result.vote.operations = certificate.operations
      then Some result.output_json
      else output)
    state.votes
    None

let certificate_progress t state =
  match current_head t with
  | Error message -> Certificate_refused message
  | Ok head when head.epoch_id > state.call.Protocol.request.expires_epoch ->
    Certificate_refused "resource compute certificate expired"
  | Ok _ ->
    begin
      match decision state with
      | Certificate.Agreed certificate ->
        begin
          match output_for_certificate state certificate with
          | Some output_json ->
            Certificate_finished {
              output_json;
              request = state.call.request;
              certificate;
              selection = state.selection;
            }
          | None -> Certificate_refused "resource compute output unavailable"
        end
      | Certificate.Conflicting ->
        Certificate_refused "resource compute outputs conflict"
      | Certificate.Rejected _ ->
        Certificate_refused "resource compute certificate refused"
      | Certificate.Insufficient -> Certificate_pending
    end

let rec wait_for_certificate t request_key =
  let* progress = actor_call t (Query_certificate request_key) in
  match progress with
  | Certificate_finished response -> Lwt.return (Ok response)
  | Certificate_refused message -> Lwt.return (Error message)
  | Certificate_pending ->
    let* () = t.deps.sleep 0.1 in
    wait_for_certificate t request_key

let request_input_bytes (request : request) =
  String.length request.method_name + String.length request.params_json

let unsigned_offer t (request : request) (head : head) validator_set_root =
  Protocol.{
    chain_id = t.deps.chain_id;
    offered_epoch = head.epoch_id;
    expires_epoch = Int64.add head.epoch_id offer_ttl;
    coordinator = t.deps.node_id;
    validator_set_root;
    circle_id = request.circle_id;
    model_epoch = request.model_epoch;
    model_state_root = request.model_state_root;
    graph_root = request.graph_root;
    model_root = request.model_root;
    program_root = request.program_root;
    executor_root = request.executor_root;
    min_memory_bytes = 1_073_741_824L;
    request_bytes = request_input_bytes request;
    response_bytes = request.max_output_bytes;
    signature = zero64;
  }

let sign_offer t (offer : Protocol.offer) =
  { offer with Protocol.signature = t.deps.sign (Protocol.offer_sign_bytes offer) }

let unsigned_request_at
    t
    (request : request)
    ~epoch_id
    ~state_root
    ~expires_epoch
    validator_set_root
    selection_root =
  Certificate.{
    chain_id = t.deps.chain_id;
    epoch_id;
    expires_epoch;
    validator_set_root;
    selection_root;
    circle_id = request.circle_id;
    state_root;
    model_epoch = request.model_epoch;
    model_state_root = request.model_state_root;
    graph_root = request.graph_root;
    model_root = request.model_root;
    program_root = request.program_root;
    executor_root = request.executor_root;
    caller = request.caller;
    session_id = request.session_id;
    sequence = request.sequence;
    input_hash =
      Protocol.input_hash
        ~method_name:request.method_name
        ~params_json:request.params_json;
    input_bytes = request_input_bytes request;
    max_output_bytes = request.max_output_bytes;
  }

let unsigned_request t request (head : head) validator_set_root selection_root =
  unsigned_request_at
    t
    request
    ~epoch_id:head.epoch_id
    ~state_root:head.state_root
    ~expires_epoch:(Int64.add head.epoch_id call_ttl)
    validator_set_root
    selection_root

let unsigned_call t request head validator_set_root selection_root =
  Protocol.{
    request = unsigned_request t request head validator_set_root selection_root;
    method_name = request.method_name;
    params_json = request.params_json;
    caller_public_key = request.caller_public_key;
    caller_signature = request.caller_signature;
    coordinator = t.deps.node_id;
    signature = zero64;
  }

let unsigned_call_for_lease t request lease =
  Protocol.{
    request =
      unsigned_request_at
        t
        request
        ~epoch_id:lease.request_epoch
        ~state_root:lease.request_state_root
        ~expires_epoch:lease.expires_epoch
        lease.validator_set_root
        lease.selection.Selection.root;
    method_name = request.method_name;
    params_json = request.params_json;
    caller_public_key = request.caller_public_key;
    caller_signature = request.caller_signature;
    coordinator = t.deps.node_id;
    signature = zero64;
  }

let sign_call t call =
  { call with Protocol.signature = t.deps.sign (Protocol.call_sign_bytes call) }

let request_shape (request : request) =
  request.model_epoch >= 0L
  && hash32 request.model_state_root
  && Octra_core.Crypto.Address.is_valid_address request.circle_id
  && hash32 request.graph_root
  && hash32 request.model_root
  && hash32 request.program_root
  && hash32 request.executor_root
  && Octra_core.Crypto.Address.is_valid_address request.caller
  && hash32 request.caller_public_key
  && signature64 request.caller_signature
  && hash32 request.session_id
  && request.sequence >= 0L
  && String.length request.method_name > 0
  && String.length request.method_name <= 64
  && String.length request.params_json <= 65_536
  && request.max_output_bytes > 0
  && request.max_output_bytes <= 262_144
  && match Yojson.Safe.from_string request.params_json with
     | `List _ -> true
     | _ -> false
     | exception _ -> false

let intent_id call =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_intent_id" (fun buffer ->
    Octra_net.Oce1.put_hash32 buffer (Protocol.intent_sign_bytes call);
    Octra_net.Oce1.put_sig64 buffer call.Protocol.caller_signature)

let cancellation_sign_bytes
    ~chain_id
    ~caller
    ~caller_public_key
    ~session_id =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_cancel" (fun buffer ->
    Octra_net.Oce1.put_string buffer chain_id;
    Octra_net.Oce1.put_string buffer caller;
    Octra_net.Oce1.put_hash32 buffer caller_public_key;
    Octra_net.Oce1.put_hash32 buffer session_id)

let cancellation_key caller session_id =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_cancel_key" (fun buffer ->
    Octra_net.Oce1.put_string buffer caller;
    Octra_net.Oce1.put_hash32 buffer session_id)

let committee_lease_matches_request
    t
    (request : request)
    (head : head)
    validator_set_root
    (lease : committee_lease) =
  lease.caller = request.caller
  && lease.caller_public_key = request.caller_public_key
  && lease.session_id = request.session_id
  && lease.circle_id = request.circle_id
  && lease.model_epoch = request.model_epoch
  && lease.model_state_root = request.model_state_root
  && lease.graph_root = request.graph_root
  && lease.model_root = request.model_root
  && lease.program_root = request.program_root
  && lease.executor_root = request.executor_root
  && lease.validator_set_root = validator_set_root
  && head.epoch_id <= lease.expires_epoch
  && t.deps.state_root_at lease.request_epoch = Some lease.request_state_root
  && request_input_bytes request <= lease.max_request_bytes
  && request.max_output_bytes <= lease.max_response_bytes

let prune_committee_leases t epoch_id =
  Hashtbl.filter_map_inplace
    (fun _ lease ->
      if Int64.compare lease.expires_epoch epoch_id < 0 then None
      else Some lease)
    t.committee_leases

let evict_committee_lease t key =
  if
    Hashtbl.length t.committee_leases >= max_committee_leases
    && not (Hashtbl.mem t.committee_leases key)
  then begin
    let oldest =
      Hashtbl.fold
        (fun current_key lease current ->
          match current with
          | None -> Some (current_key, lease.expires_epoch)
          | Some (oldest_key, oldest_epoch) ->
            if
              Int64.compare lease.expires_epoch oldest_epoch < 0
              || (Int64.equal lease.expires_epoch oldest_epoch
                  && String.compare current_key oldest_key < 0)
            then Some (current_key, lease.expires_epoch)
            else current)
        t.committee_leases
        None in
    Option.iter (fun (oldest_key, _) -> Hashtbl.remove t.committee_leases oldest_key) oldest
  end

let remember_committee_direct t (lease : committee_lease) =
  match current_head t with
  | Error _ -> ()
  | Ok head when head.epoch_id > lease.expires_epoch -> ()
  | Ok head ->
    prune_committee_leases t head.epoch_id;
    let key = cancellation_key lease.caller lease.session_id in
    evict_committee_lease t key;
    Hashtbl.replace t.committee_leases key lease

let committee_lease_for_request
    t
    (request : request)
    (head : head)
    validator_set_root =
  prune_committee_leases t head.epoch_id;
  let key = cancellation_key request.caller request.session_id in
  match Hashtbl.find_opt t.committee_leases key with
  | Some lease
    when committee_lease_matches_request t request head validator_set_root lease ->
    Some lease
  | Some _ ->
    Hashtbl.remove t.committee_leases key;
    None
  | None -> None

let capability_for_member snapshot (member : Selection.member) =
  let capabilities =
    snapshot.capabilities
    |> List.filter (fun capability ->
      capability.Capability.node_id = member.node_id
      && capability.evidence_root = member.evidence_root)
    |> List.sort (fun left right ->
      String.compare (Capability.id left) (Capability.id right)) in
  match capabilities with
  | capability :: _ -> Some capability
  | [] -> None

let committee_lease_of_response
    (request : request)
    (snapshot : offer_snapshot)
    (response : response) =
  let capabilities =
    List.map (capability_for_member snapshot) response.selection.Selection.members in
  if List.exists Option.is_none capabilities then None
  else
    let capabilities = List.filter_map Fun.id capabilities in
    let expires_epoch =
      List.fold_left
        (fun epoch capability -> Int64.min epoch capability.Capability.valid_until)
        snapshot.offer.Protocol.expires_epoch
        capabilities in
    let max_request_bytes =
      List.fold_left
        (fun limit capability -> min limit capability.Capability.max_request_bytes)
        max_int
        capabilities in
    let max_response_bytes =
      List.fold_left
        (fun limit capability -> min limit capability.Capability.max_response_bytes)
        max_int
        capabilities in
    if
      capabilities = []
      || expires_epoch < response.request.Certificate.epoch_id
      || request_input_bytes request > max_request_bytes
      || request.max_output_bytes > max_response_bytes
    then None
    else
      Some {
        caller = request.caller;
        caller_public_key = request.caller_public_key;
        session_id = request.session_id;
        circle_id = request.circle_id;
        model_epoch = request.model_epoch;
        model_state_root = request.model_state_root;
        graph_root = request.graph_root;
        model_root = request.model_root;
        program_root = request.program_root;
        executor_root = request.executor_root;
        validator_set_root = response.request.validator_set_root;
        request_epoch = response.request.epoch_id;
        request_state_root = response.request.state_root;
        expires_epoch;
        max_request_bytes;
        max_response_bytes;
        selection = response.selection;
      }

let caller_admitted t caller =
  Option.value ~default:0 (Hashtbl.find_opt t.caller_jobs caller) = 0

let set_caller_active t caller delta =
  let count = Option.value ~default:0 (Hashtbl.find_opt t.caller_jobs caller) + delta in
  if count <= 0 then Hashtbl.remove t.caller_jobs caller
  else Hashtbl.replace t.caller_jobs caller count

let release_job_lease t (job : job) =
  if job.lease_active then begin
    job.lease_active <- false;
    set_caller_active t job.caller (-1)
  end

let prune_cancelled_sessions t epoch_id =
  Hashtbl.filter_map_inplace
    (fun _ expires_epoch ->
      if Int64.compare expires_epoch epoch_id < 0 then None
      else Some expires_epoch)
    t.cancelled_sessions

let evict_cancelled_session t =
  if Hashtbl.length t.cancelled_sessions >= max_cancelled_sessions then begin
    let oldest =
      Hashtbl.fold
        (fun key expires_epoch current ->
          match current with
          | None -> Some (key, expires_epoch)
          | Some (current_key, current_epoch) ->
            if
              Int64.compare expires_epoch current_epoch < 0
              || (Int64.equal expires_epoch current_epoch
                  && String.compare key current_key < 0)
            then Some (key, expires_epoch)
            else current)
        t.cancelled_sessions
        None in
    Option.iter (fun (key, _) -> Hashtbl.remove t.cancelled_sessions key) oldest
  end

let session_cancelled t caller session_id =
  Hashtbl.mem t.cancelled_sessions (cancellation_key caller session_id)

let verify_cancellation t (cancellation : cancellation) =
  let public_key_b64 = Base64.encode_exn cancellation.caller_public_key in
  Octra_core.Crypto.Address.verify_address_pubkey cancellation.caller public_key_b64
  && Octra_ed25519.safe cancellation.caller_public_key
  && Octra_ed25519.verify
       ~pub:cancellation.caller_public_key
       ~msg:
         (cancellation_sign_bytes
            ~chain_id:t.deps.chain_id
            ~caller:cancellation.caller
            ~caller_public_key:cancellation.caller_public_key
            ~session_id:cancellation.session_id)
       cancellation.caller_signature

let prune_finished_jobs t =
  let count = Queue.length t.job_order in
  let rec loop remaining =
    if remaining <= 0 || Hashtbl.length t.jobs < max_jobs then ()
    else
      match Queue.take_opt t.job_order with
      | None -> ()
      | Some id ->
        begin
          match Hashtbl.find_opt t.jobs id with
          | None -> loop (remaining - 1)
          | Some job ->
            begin
              match Lwt.state job.result with
              | Lwt.Sleep ->
                Queue.push id t.job_order;
                loop (remaining - 1)
              | Lwt.Return _ | Lwt.Fail _ ->
                Hashtbl.remove t.jobs id;
                loop (remaining - 1)
            end
        end in
  loop count

let complete_job t ~job_id ~caller =
  actor_call t (Complete_job { job_id; caller })

let cancel_session_direct t (cancellation : cancellation) =
  if
    not (Octra_core.Crypto.Address.is_valid_address cancellation.caller)
    || not (hash32 cancellation.caller_public_key)
    || not (signature64 cancellation.caller_signature)
    || not (hash32 cancellation.session_id)
  then Error "resource compute cancellation refused"
  else if not (verify_cancellation t cancellation) then
    Error "resource compute cancellation signature refused"
  else
    match current_head t with
    | Error message -> Error message
    | Ok head ->
      prune_cancelled_sessions t head.epoch_id;
      evict_cancelled_session t;
      Hashtbl.replace
        t.cancelled_sessions
        (cancellation_key cancellation.caller cancellation.session_id)
        (Int64.add head.epoch_id call_ttl);
      Hashtbl.remove
        t.committee_leases
        (cancellation_key cancellation.caller cancellation.session_id);
      let cancelled =
        Hashtbl.fold
          (fun _ (job : job) count ->
            if
              job.caller = cancellation.caller
              && job.session_id = cancellation.session_id
              && Lwt.is_sleeping job.result
            then begin
              release_job_lease t job;
              Lwt.cancel job.result;
              count + 1
            end else count)
          t.jobs
          0 in
      Ok cancelled

let compute_new t request validator_set_root offer =
  Log.info "compute" "event = committee status = offered";
  let* () = publish t (Protocol.Offer offer) in
  let offer_key = offer_id offer in
  let* candidates = wait_for_candidates t offer_key in
  match candidates with
  | Error _ as error -> Lwt.return error
  | Ok (call_head, snapshot) ->
    let validator_set, current_root = current_validator_set t in
    if current_root <> validator_set_root then
      Lwt.return (Error "resource compute validator set changed")
    else
      let preview = unsigned_request t request call_head current_root zero32 in
      let selection = selection_for_snapshot t validator_set snapshot preview in
      if
        List.length selection.Selection.members
        <> committee_members validator_set
      then
        Lwt.return (Error "resource compute committee unavailable")
      else
        let call =
          unsigned_call t request call_head current_root selection.root
          |> sign_call t in
        let request_key = request_id call in
        Log.info "compute"
          "event = committee status = selected members = %d"
          (List.length selection.Selection.members);
        let* () = publish t (Protocol.Call call) in
        let* result = wait_for_certificate t request_key in
        begin
          match result with
          | Error _ -> Lwt.return result
          | Ok response ->
            begin
              match committee_lease_of_response request snapshot response with
              | None -> Lwt.return result
              | Some lease ->
                let* () = actor_call t (Remember_committee lease) in
                Log.info "compute"
                  "event = committee status = leased until_epoch = %Ld capacity = %d"
                  lease.expires_epoch
                  lease.max_request_bytes;
                Lwt.return result
            end
        end

let compute_reused t request lease =
  let call = unsigned_call_for_lease t request lease |> sign_call t in
  let request_key = request_id call in
  Log.info "compute"
    "event = committee status = reused members = %d until_epoch = %Ld"
    (List.length lease.selection.Selection.members)
    lease.expires_epoch;
  let* () = publish t (Protocol.Call call) in
  wait_for_certificate t request_key

let start_job_direct t request =
  if not (request_shape request) then
    Error "resource compute request refused"
  else
    match current_head t with
    | Error message -> Error message
    | Ok head ->
      prune_cancelled_sessions t head.epoch_id;
      let validator_set, validator_set_root = current_validator_set t in
      if
        request.model_epoch > head.epoch_id
        || t.deps.state_root_at request.model_epoch <> Some request.model_state_root
      then
        Error "resource compute model snapshot unavailable"
      else if not (Octra_consensus.C_types.is_validator validator_set t.deps.node_id) then
        Error "resource compute coordinator is not a validator"
      else if session_cancelled t request.caller request.session_id then
        Error "resource compute session cancelled"
      else
        let preview_call =
          unsigned_call t request head validator_set_root zero32 in
        if not (verify_caller preview_call) then
          Error "resource compute caller signature refused"
        else
          let id = intent_id preview_call in
          prune_finished_jobs t;
          match Hashtbl.find_opt t.jobs id with
          | Some job -> Ok (id, job)
          | None when Hashtbl.length t.jobs >= max_jobs ->
            Error "resource compute job table full"
          | None when not (caller_admitted t request.caller) ->
            Error "resource compute caller already active"
          | None ->
            let computation =
              match committee_lease_for_request t request head validator_set_root with
              | Some lease -> fun () -> compute_reused t request lease
              | None ->
                let offer =
                  unsigned_offer t request head validator_set_root |> sign_offer t in
                fun () -> compute_new t request validator_set_root offer in
            set_caller_active t request.caller 1;
            let job =
              Lwt.finalize
                (fun () ->
                  Lwt.catch
                    computation
                    (fun error ->
                      Lwt.return
                        (Error
                           ("resource compute actor failed: "
                            ^ Printexc.to_string error))))
                (fun () ->
                  Lwt.catch
                    (fun () -> complete_job t ~job_id:id ~caller:request.caller)
                    (fun _ -> Lwt.return_unit)) in
            let entry = {
              caller = request.caller;
              session_id = request.session_id;
              lease_active = true;
              result = job;
            } in
            Hashtbl.add t.jobs id entry;
            Queue.push id t.job_order;
            Ok (id, entry)

let submit t request =
  let* result = actor_call t (Admit_request request) in
  match result with
  | Error _ as error -> Lwt.return error
  | Ok (id, _) -> Lwt.return (Ok (hex_of_raw id))

let status_direct t ~caller ~job_id =
  if not (Text.is_hex_len 64 job_id) then None
  else
    let id = Text.hex_to_string job_id in
    match Hashtbl.find_opt t.jobs id with
    | Some job when job.caller = caller ->
      begin
        match Lwt.state job.result with
        | Lwt.Sleep -> Some Waiting
        | Lwt.Return (Ok response) -> Some (Finished response)
        | Lwt.Return (Error message) -> Some (Refused message)
        | Lwt.Fail error ->
          Some (Refused ("resource compute actor failed: " ^ Printexc.to_string error))
      end
    | _ -> None

let status t ~caller ~job_id =
  actor_call t (Query_status { caller; job_id })

let cancel t cancellation =
  actor_call t (Cancel_session cancellation)

let compute t request =
  let* started = actor_call t (Admit_request request) in
  match started with
  | Error message -> Lwt.return (Error message)
  | Ok (_, job) -> job.result

let reject_command error (Command command) =
  Lwt.wakeup_later_exn command.reply error

let drain_commands queue error =
  let rec loop () =
    match Queue.take_opt queue with
    | None -> ()
    | Some command ->
      reject_command error command;
      loop () in
  loop ()

let stop_direct t =
  Hashtbl.iter
    (fun _ (job : job) ->
      release_job_lease t job;
      if Lwt.is_sleeping job.result then Lwt.cancel job.result)
    t.jobs;
  Hashtbl.iter (fun _ task -> Lwt.cancel task) t.background_tasks;
  Hashtbl.clear t.background_tasks;
  t.channel_open <- false;
  t.actor_generation <- Int64.succ t.actor_generation;
  let error = Failure "resource compute actor stopped" in
  drain_commands t.control_channel error;
  drain_commands t.data_channel error;
  Lwt_condition.broadcast t.channel_ready ();
  Lwt_condition.broadcast t.channel_space ()

let handle_actor_message : type reply. t -> reply actor_message -> reply Lwt.t =
  fun t message ->
    match message with
    | Apply_protocol { wire_key; message } ->
      if not (Octra_consensus.C_seen.remember t.seen wire_key) then
        Lwt.return_false
      else
        apply_message_direct t message
    | Admit_request request ->
      Lwt.return (start_job_direct t request)
    | Complete_job { job_id; caller } ->
      begin
        match Hashtbl.find_opt t.jobs job_id with
        | Some job when job.caller = caller -> release_job_lease t job
        | _ -> ()
      end;
      Lwt.return_unit
    | Cancel_session cancellation ->
      Lwt.return (cancel_session_direct t cancellation)
    | Complete_background task_id ->
      Hashtbl.remove t.background_tasks task_id;
      Lwt.return_unit
    | Remember_committee lease ->
      remember_committee_direct t lease;
      Lwt.return_unit
    | Query_status { caller; job_id } ->
      Lwt.return (status_direct t ~caller ~job_id)
    | Query_active_jobs ->
      Lwt.return
        (Hashtbl.fold (fun _ count total -> total + count) t.caller_jobs 0)
    | Query_offer offer_key ->
      Lwt.return
        (Hashtbl.find_opt t.offers offer_key |> Option.map offer_snapshot)
    | Query_certificate request_key ->
      Lwt.return
        (match Hashtbl.find_opt t.requests request_key with
         | Some state -> certificate_progress t state
         | None -> Certificate_refused "resource compute request unavailable")
    | Ensure_self_test -> ensure_self_test_direct t
    | Stop ->
      stop_direct t;
      Lwt.return_unit

let take_actor_command t =
  match Queue.take_opt t.control_channel with
  | Some _ as command -> command
  | None -> Queue.take_opt t.data_channel

let rec actor_loop t =
  match take_actor_command t with
  | Some (Command command) ->
    Lwt_condition.broadcast t.channel_space ();
    if command.generation <> t.actor_generation then begin
      Lwt.wakeup_later_exn command.reply
        (Failure "resource compute actor generation changed");
      actor_loop t
    end else
      Lwt.catch
        (fun () ->
          let* value = handle_actor_message t command.message in
          Lwt.wakeup_later command.reply value;
          actor_loop t)
        (fun error ->
          Log.warn "compute"
            "event = actor_message status = failed kind = %s reason = %s"
            (actor_message_name command.message)
            (Printexc.to_string error);
          Lwt.wakeup_later_exn command.reply error;
          actor_loop t)
  | None when not t.channel_open -> Lwt.return_unit
  | None ->
    let* () = Lwt_condition.wait t.channel_ready in
    actor_loop t

let create ~deps ~provider =
  let t = {
    deps;
    provider;
    seen = Octra_consensus.C_seen.create ~capacity:8192;
    executed = Octra_consensus.C_seen.create ~capacity:2048;
    offers = Hashtbl.create 32;
    requests = Hashtbl.create 64;
    sequences = Hashtbl.create 128;
    jobs = Hashtbl.create 32;
    job_order = Queue.create ();
    caller_jobs = Hashtbl.create 32;
    cancelled_sessions = Hashtbl.create 64;
    committee_leases = Hashtbl.create 64;
    background_tasks = Hashtbl.create 32;
    next_background_id = 0;
    data_channel = Queue.create ();
    control_channel = Queue.create ();
    channel_ready = Lwt_condition.create ();
    channel_space = Lwt_condition.create ();
    channel_open = true;
    actor_generation = 0L;
    self_test = None;
  } in
  Lwt.async (fun () -> actor_loop t);
  t

let active_jobs t =
  actor_call t Query_active_jobs

let shutdown t =
  actor_call t Stop