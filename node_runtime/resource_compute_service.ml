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
module Record = Compute_record
module Funding = Octra_core.Compute_funding

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
  funding : head -> Protocol.header -> (Funding.funded option, string) result Lwt.t;
  self_test : unit -> (Provider_rpc.self_test, string) result Lwt.t;
  nonce : unit -> string;
  broadcast : string -> unit Lwt.t;
  send : peer:string -> string -> unit Lwt.t;
  cancel_rounds : unit -> bool;
  now : unit -> int64;
  relay : source:string -> string -> unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

type request = {
  unit_price : int64 option;
  fee_cap : int64 option;
  memory_bytes : int option;
  guest_bytes : int option;
  fuel_limit : int option;
  valid_until : int64 option;
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

type cancel_status = {
  selection_root : string;
  members : string list;
  missing : string list;
  receipts : Protocol.cancel_ack list;
  expires : int64 option;
}

type job_status =
  | Waiting
  | Funding of Protocol.header
  | Finished of response
  | Refused of string
  | Cancelled of cancel_status

type cancellation = {
  caller : string;
  caller_public_key : string;
  caller_signature : string;
  session_id : string;
  until : int64 option;
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

type offer_snapshot = {
  offer : Protocol.offer;
  capabilities : Capability.t list;
  commitments : Selection.commitment list;
  reveals : Selection.reveal list;
}

type request_state = {
  call : Protocol.call;
  selection : Selection.selection;
  validator_set : Octra_consensus.C_types.validator_set;
  pinned : bool;
  proof : offer_snapshot;
  votes : (string, Protocol.result) Hashtbl.t;
}

type committee_lease = {
  memory_bytes : int64;
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

type execution = Execute | Retained of Protocol.result | Skipped

type _ actor_message =
  | Apply_protocol : {
      wire_key : string;
      source : string option;
      message : Protocol.message;
      funds : Funding.funded option;
    } -> bool actor_message
  | Check_funding : Protocol.call -> (head option, string) result actor_message
  | Drop_funding : string -> unit actor_message
  | Fund_call : Protocol.call * Funding.funded -> (bool, string) result actor_message
  | Admit_request : request -> ((string * job), string) result actor_message
  | Complete_job : {
      job_id : string;
      caller : string;
    } -> unit actor_message
  | Cancel_session : cancellation -> (int, string) result actor_message
  | Poll_cancel : (string * string) option ->
      (Octra_core.Compute_history.notice option, string) result actor_message
  | Read_cancels : cancellation -> (Protocol.cancel_ack list, string) result actor_message
  | Cancel_keys : Protocol.cancel_round -> (string * string) option actor_message
  | Cancel_path : Protocol.cancel_round -> string option actor_message
  | Wake_reveals : unit actor_message
  | Sent_reveal : string * bool -> unit actor_message
  | Save_result : Protocol.call * Protocol.result -> (bool, string) result actor_message
  | Issue_call : Protocol.call -> (unit, string) result actor_message
  | Save_job : string * response -> (unit, string) result actor_message
  | Complete_background : int -> unit actor_message
  | Remember_committee : committee_lease -> unit actor_message
  | Query_status : Compute_read.t -> (job_status option, string) result actor_message
  | Query_active_jobs : int actor_message
  | Query_offer : string -> offer_snapshot option actor_message
  | Query_certificate : string -> certificate_progress actor_message
  | Request_replies : string -> ((int * Protocol.fetch list) option, string) result actor_message
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
  history : (Octra_core.Compute_history.t, string) result;
  seen : Octra_consensus.C_seen.t;
  mutable relay_pending : string option;
  relay_retry : (string, int64) Hashtbl.t;
  offers : (string, offer_state) Hashtbl.t;
  requests : (string, request_state) Hashtbl.t;
  jobs : (string, job) Hashtbl.t;
  job_order : string Queue.t;
  caller_jobs : (string, int) Hashtbl.t;
  cancelled_sessions : (string, string * int64) Hashtbl.t;
  cancel_paths : (string, string * int64) Hashtbl.t;
  committee_leases : (string, committee_lease) Hashtbl.t;
  read_budget : (string, int64 * int) Hashtbl.t;
  funding_reads : (string, unit) Hashtbl.t;
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

let max_committee_members = Protocol.max_members
let reveal_delay = Protocol.reveal_delay
let offer_ttl = Protocol.offer_ttl
let call_ttl = Protocol.call_ttl
let capability_ttl = Protocol.capacity_ttl
let max_offers = 64
let max_requests = 128
let max_jobs = 32
let max_cancelled_sessions = 256
let max_caller_sessions = max_jobs
let max_committee_leases = 256
let max_offer_records = Protocol.max_records
let max_pending_reveals_per_node = 4
let data_channel_limit = 512
let control_channel_limit = 64
let max_read_messages = 32
let zero32 = String.make 32 '\000'
let zero64 = String.make 64 '\000'

let committee_members validator_set =
  min max_committee_members validator_set.Octra_consensus.C_types.n

let actor_message_name : type reply. reply actor_message -> string = function
  | Apply_protocol _ -> "apply_protocol"
  | Check_funding _ -> "check_funding"
  | Drop_funding _ -> "drop_funding"
  | Fund_call _ -> "fund_call"
  | Admit_request _ -> "admit_request"
  | Complete_job _ -> "complete_job"
  | Cancel_session _ -> "cancel_session"
  | Poll_cancel _ -> "poll_cancel"
  | Read_cancels _ -> "read_cancels"
  | Cancel_keys _ -> "cancel_keys"
  | Cancel_path _ -> "cancel_path"
  | Wake_reveals -> "wake_reveals"
  | Sent_reveal _ -> "sent_reveal"
  | Save_result _ -> "save_result"
  | Issue_call _ -> "issue_call"
  | Save_job _ -> "save_job"
  | Complete_background _ -> "complete_background"
  | Remember_committee _ -> "remember_committee"
  | Query_status _ -> "query_status"
  | Query_active_jobs -> "query_active_jobs"
  | Query_offer _ -> "query_offer"
  | Query_certificate _ -> "query_certificate"
  | Request_replies _ -> "request_replies"
  | Ensure_self_test -> "ensure_self_test"
  | Stop -> "stop"

let actor_message_is_control : type reply. reply actor_message -> bool = function
  | Apply_protocol { message = Protocol.Cancel _; _ }
  | Apply_protocol { message = Protocol.Cancel_round _; _ }
  | Apply_protocol { message = Protocol.Cancel_ack _; _ }
  | Complete_job _
  | Drop_funding _
  | Fund_call _
  | Cancel_session _
  | Poll_cancel _
  | Read_cancels _
  | Cancel_keys _
  | Cancel_path _
  | Sent_reveal _
  | Save_result _
  | Issue_call _
  | Save_job _
  | Complete_background _
  | Remember_committee _
  | Stop -> true
  | Apply_protocol _
  | Check_funding _
  | Admit_request _
  | Query_status _
  | Query_active_jobs
  | Query_offer _
  | Query_certificate _
  | Request_replies _
  | Wake_reveals
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
  let result, set_result = Lwt.task () in
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

let intent_id = Protocol.intent_id

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

let verify_offer_at ?validators t epoch offer =
  let validator_set = match validators with
    | Some value -> value
    | None -> t.deps.validator_set () in
  let validator_set_root = Octra_consensus.C_config.validator_set_hash validator_set in
  Protocol.offer_shape offer
  && offer.chain_id = t.deps.chain_id
  && offer.validator_set_root = validator_set_root
  && t.deps.state_root_at offer.model_epoch = Some offer.model_state_root
  && offer.offered_epoch <= epoch
  && epoch <= offer.expires_epoch
  && Int64.sub offer.expires_epoch offer.offered_epoch <= offer_ttl
  && Octra_consensus.C_types.is_validator validator_set offer.coordinator
  && verify_node validator_set offer.coordinator
       (Protocol.offer_sign_bytes offer) offer.signature

let verify_offer t offer =
  match current_head t with
  | Error _ -> false
  | Ok head -> verify_offer_at t head.epoch_id offer

let verify_capability_at ?validators t current_epoch capability =
  let validator_set = match validators with
    | Some value -> value
    | None -> t.deps.validator_set () in
  let validator_set_root = Octra_consensus.C_config.validator_set_hash validator_set in
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

let verify_capability t capability =
  match current_head t with
  | Error _ -> Lwt.return_false
  | Ok head -> verify_capability_at t head.epoch_id capability

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

let read_validators t call =
  match Hashtbl.find_opt t.requests (request_id call) with
  | Some state when state.call = call -> state.validator_set
  | _ -> fst (current_validator_set t)

let reply_validators t (fetch : Protocol.fetch) saved =
  match saved with
  | Some {Octra_core.Compute_history.context = Some context; _}
      when fetch.target = t.deps.node_id ->
    (try Some (Record.decode_read fetch.call context) with _ -> None)
  | _ -> Some (read_validators t fetch.call)

let fetch_window t (fetch : Protocol.fetch) =
  match current_head t with
  | Error _ -> false
  | Ok head ->
    let call = fetch.call in
    let request = call.request in
    Protocol.fetch_shape fetch
    && request.chain_id = t.deps.chain_id
    && fetch.epoch_id <= head.epoch_id && head.epoch_id <= fetch.expires_epoch
    && Int64.sub request.expires_epoch request.epoch_id <= call_ttl
    && t.deps.state_root_at request.epoch_id = Some request.state_root
    && t.deps.state_root_at request.model_epoch = Some request.model_state_root
    && verify_caller call

let valid_fetch t validators (fetch : Protocol.fetch) =
  let call = fetch.call in
  fetch_window t fetch
  && call.request.validator_set_root = Octra_consensus.C_config.validator_set_hash validators
  && Octra_consensus.C_types.is_validator validators fetch.target
  && verify_node validators call.coordinator (Protocol.call_sign_bytes call) call.signature
  && verify_node validators call.coordinator (Protocol.fetch_sign_bytes fetch) fetch.signature

let take_read t ~answer (fetch : Protocol.fetch) =
  match current_head t with
  | Error _ -> false
  | Ok head ->
    Hashtbl.filter_map_inplace (fun _ (epoch, count) ->
      if epoch = head.epoch_id then Some (epoch, count) else None) t.read_budget;
    let key = (if answer then "answer:" else "fetch:") ^ fetch.call.coordinator in
    let count = match Hashtbl.find_opt t.read_budget key with
      | Some (_, count) -> count | None -> 0 in
    if count >= max_read_messages
        || count = 0 && Hashtbl.length t.read_budget >= 2 * max_requests then false
    else (Hashtbl.replace t.read_budget key (head.epoch_id, count + 1); true)

let valid_answer validators (fetch : Protocol.fetch) (result : Protocol.result) =
  Protocol.result_shape result
  && Certificate.vote_budget fetch.call.request result.vote
  && result.vote.node_id = fetch.target
  && result.vote.request_id = request_id fetch.call
  && result.vote.output_bytes = String.length result.output_json
  && result.vote.output_bytes <= fetch.call.request.max_output_bytes
  && verify_node validators result.vote.node_id
    (Certificate.vote_sign_bytes result.vote) result.vote.signature

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
    && capability.max_response_bytes >= request.max_output_bytes
    && (match request.memory_bytes with None -> true
      | Some bytes -> Int64.of_int bytes <= capability.memory_bytes))

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
  else if Option.is_none call.request.unit_price then Error "resource compute caller fee required"
  else if Option.is_none call.request.memory_bytes then Error "resource compute caller memory required"
  else if Option.is_none call.request.guest_bytes then Error "resource compute caller pages required"
  else if Option.is_none call.request.fuel_limit then Error "resource compute caller fuel required"
  else if (match call.Protocol.valid_until with
    | None -> true | Some epoch -> call.request.expires_epoch > epoch) then
    Error "resource compute caller expiry refused"
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

let request_state t call selection validator_set proof =
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
      pinned = true;
      proof;
      votes = Hashtbl.create max_committee_members;
    } in
    Hashtbl.add t.requests id state;
    state

let record_of_state state results =
  let proof = state.proof in
  Record.{offer = proof.offer; capabilities = proof.capabilities;
    commitments = proof.commitments; reveals = proof.reveals; call = state.call;
    validators = if state.pinned then Some state.validator_set else None;
    results = List.sort (fun left right -> String.compare
      left.Protocol.vote.Certificate.node_id right.Protocol.vote.Certificate.node_id) results}

let use_history t action =
  match t.history with
  | Error reason -> Lwt.return (Error reason)
  | Ok history -> Lwt.no_cancel (Lwt_preemptive.detach action history)

let saved_reply t call =
  let request = call.Protocol.request in
  use_history t (fun history ->
    match Octra_core.Compute_history.cancelled history
      ~caller:request.caller ~session:request.session_id with
    | Ok false -> Octra_core.Compute_history.saved history ~caller:request.caller
        ~session:request.session_id ~sequence:request.sequence ~request:(request_id call)
    | Ok true -> Error "compute session cancelled"
    | Error _ as error -> error)

let reserve_call t call validators =
  match t.provider with
  | None -> Lwt.return (Ok Skipped)
  | Some _ ->
    let request = call.Protocol.request in
    let room = Record.reply_space call validators t.deps.node_id in
    let* saved = use_history t (fun history ->
      match Octra_core.Compute_history.reply history ~caller:request.caller
        ~session:request.session_id ~sequence:request.sequence ~request:(request_id call) with
      | Error _ as error -> error
      | Ok (Some wire) -> Ok (Some wire, false)
      | Ok None ->
        Result.map (fun accepted -> None, accepted)
          (Octra_core.Compute_history.reserve history ~caller:request.caller
            ~session:request.session_id ~sequence:request.sequence ~request:(request_id call)
            ~scope:(Record.encode_scope call validators)
            ~room)) in
    Lwt.return (match saved with
      | Error _ as error -> error
      | Ok (None, true) -> Ok Execute
      | Ok (None, false) -> Ok Skipped
      | Ok (Some wire, _) ->
        match Protocol.decode wire with
        | Protocol.Result result when result.vote.node_id = t.deps.node_id
            && result.vote.request_id = request_id call -> Ok (Retained result)
        | _ -> Error "resource compute saved result differs"
        | exception _ -> Error "resource compute saved result unreadable")

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
        admitted && (match validate_pending_reveal t state reveal with
          | Reveal_valid _ -> true
          | Reveal_wait | Reveal_invalid -> false)
    | Some _
    | None -> false

let result_valid state result =
  if not (Protocol.result_shape result) then false
  else
    let vote = result.Protocol.vote in
    vote.request_id = request_id state.call
    && Certificate.vote_budget state.call.request vote
    && List.mem vote.node_id (selected_ids state.selection)
    && vote.output_bytes = String.length result.output_json
    && vote.output_bytes <= state.call.request.max_output_bytes
    && verify_node state.validator_set vote.node_id
         (Certificate.vote_sign_bytes vote) vote.signature

let result_state t result =
  match Hashtbl.find_opt t.requests result.Protocol.vote.Certificate.request_id with
  | Some state when result_valid state result -> Some state
  | _ -> None

let record_result t result =
  match result_state t result with
  | None -> Lwt.return_false
  | Some state ->
    let node = result.Protocol.vote.Certificate.node_id in
    begin match Hashtbl.find_opt state.votes node with
    | Some prior -> Lwt.return (prior = result)
    | None when state.call.coordinator <> t.deps.node_id ->
      Lwt.return (add_exact state.votes node result)
    | None ->
      let before = Record.encode (record_of_state state (table_values state.votes)) in
      let after = Record.encode (record_of_state state (result :: table_values state.votes)) in
      let request = state.call.request in
      let own = node = t.deps.node_id in
      let* saved = use_history t (fun history ->
        let id = intent_id state.call in
        match Octra_core.Compute_history.work history ~caller:request.caller ~id with
        | Error _ as error -> error
        | Ok None when own ->
          begin match Octra_core.Compute_history.load_job history ~caller:request.caller ~id with
          | Error _ as error -> error
          | Ok (Some body) when (Record.decode body).call = state.call ->
            Result.map (fun reply -> false, reply = Some (Protocol.encode (Protocol.Result result)))
              (Octra_core.Compute_history.reply history ~caller:request.caller
                ~session:request.session_id ~sequence:request.sequence ~request:(request_id state.call))
          | _ -> Ok (false, false)
          end
        | Ok None -> Ok (false, false)
        | Ok (Some _) ->
          Result.map (fun saved -> saved, false)
            (Octra_core.Compute_history.advance_job history ~caller:request.caller
              ~session:request.session_id ~id
              ~before:(Octra_core.Compute_history.Issued before)
              ~after:(Octra_core.Compute_history.Issued after))) in
      Lwt.return (match saved with
        | Ok (true, _) -> add_exact state.votes node result
        | Ok (false, replay) -> replay
        | Error _ -> false)
    end

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
    memory_bytes = (match request.memory_bytes with Some bytes -> bytes
      | None -> invalid_arg "resource compute caller memory required");
    guest_bytes = (match request.guest_bytes with Some bytes -> bytes
      | None -> invalid_arg "resource compute caller pages required");
    fuel_limit = (match request.fuel_limit with Some fuel -> fuel
      | None -> invalid_arg "resource compute caller fuel required");
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

let wire_cancel t (cancel : cancellation) = Protocol.{
  chain_id = t.deps.chain_id;
  caller = cancel.caller;
  caller_public_key = cancel.caller_public_key;
  session_id = cancel.session_id;
  caller_signature = cancel.caller_signature;
  until = cancel.until;
}

let cancellation_sign_bytes ~chain_id ~caller ~caller_public_key ~session_id =
  Protocol.cancel_sign_bytes Protocol.{
    chain_id; caller; caller_public_key; session_id; caller_signature = zero64; until = None;
  }

let cancellation_key caller session_id =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_cancel_key" (fun buffer ->
    Octra_net.Oce1.put_string buffer caller;
    Octra_net.Oce1.put_hash32 buffer session_id)

let cancel_route (value : Protocol.cancel_round) =
  value.origin ^ cancellation_key value.cancel.caller value.cancel.session_id

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
    (fun _ ((_, expires_epoch) as entry) ->
      if Int64.compare expires_epoch epoch_id < 0 then None
      else Some entry)
    t.cancelled_sessions

let session_cancelled t caller session_id =
  Hashtbl.mem t.cancelled_sessions (cancellation_key caller session_id)

let cancel_available t caller session_id =
  let count, own = Hashtbl.fold (fun _ (owner, _) (count, own) ->
    count + 1, own + (if owner = caller then 1 else 0))
    t.cancelled_sessions (0, 0) in
  let count, own, reserved = Hashtbl.fold (fun _ (job : job) (count, own, reserved) ->
    if job.lease_active && not (session_cancelled t job.caller job.session_id) then
      count + 1, own + (if job.caller = caller then 1 else 0),
      reserved || (job.caller = caller && job.session_id = session_id)
    else count, own, reserved) t.jobs (count, own, false) in
  session_cancelled t caller session_id || reserved
  || (count < max_cancelled_sessions && own < max_caller_sessions)

let verify_cancellation t (cancel : cancellation) =
  let public_key_b64 = Base64.encode_exn cancel.caller_public_key in
  Octra_core.Crypto.Address.verify_address_pubkey cancel.caller public_key_b64
  && Octra_ed25519.safe cancel.caller_public_key
  && Octra_ed25519.verify ~pub:cancel.caller_public_key
       ~msg:(Protocol.cancel_sign_bytes (wire_cancel t cancel)) cancel.caller_signature

let verify_cancel_ack t (ack : Protocol.cancel_ack) =
  let delivery = ack.delivery in
  let cancel = delivery.cancel in
  let signed address key message signature =
    Octra_core.Crypto.Address.verify_address_pubkey address (Base64.encode_exn key)
    && Octra_ed25519.safe key
    && Octra_ed25519.verify ~pub:key ~msg:message signature in
  Protocol.cancel_ack_shape ack && cancel.chain_id = t.deps.chain_id
  && verify_cancellation t {
    caller = cancel.caller; caller_public_key = cancel.caller_public_key;
    caller_signature = cancel.caller_signature; session_id = cancel.session_id; until = cancel.until}
  && signed delivery.origin ack.origin_key (Protocol.cancel_round_sign delivery) delivery.signature
  && signed ack.node_id ack.node_key (Protocol.cancel_ack_sign ack) ack.signature

let cancel_scope t (cancel : Protocol.cancellation) origin accept =
  let chain_id = t.deps.chain_id in
  let matches (call : Protocol.call) validators =
    Protocol.call_shape call
    && call.request.chain_id = chain_id && cancel.chain_id = chain_id
    && call.request.caller = cancel.caller && call.caller_public_key = cancel.caller_public_key
    && call.request.session_id = cancel.session_id && call.coordinator = origin
    && call.request.validator_set_root = Octra_consensus.C_config.validator_set_hash validators
    && verify_caller call
    && verify_node validators origin (Protocol.call_sign_bytes call) call.signature
    && accept validators in
  let decode (scope : Octra_core.Compute_history.scope) =
    try
      let call, validators = Record.decode_scope scope.body in
      if scope.sequence = call.request.sequence && scope.request = request_id call
          && matches call validators then Some validators else None
    with _ -> None in
  let known = Hashtbl.fold (fun _ state found -> match found with
    | Some _ -> found
    | None when matches state.call state.validator_set -> Some state.validator_set
    | None -> None) t.requests None in
  match known with
  | Some _ -> Lwt.return known
  | None ->
    let* saved = use_history t (fun history ->
      Octra_core.Compute_history.find_scope history ~caller:cancel.caller ~session:cancel.session_id
        (fun scope -> Option.is_some (decode scope))) in
    Lwt.return (match saved with
      | Ok (Some scope) -> decode scope
      | _ -> None)

let round_validators ?(member = false) t (value : Protocol.cancel_round) =
  let node_id = t.deps.node_id in
  let signed validators = verify_node validators value.origin
    (Protocol.cancel_round_sign value) value.signature
    && (not member
        || Option.is_some (Octra_consensus.C_types.pubkey_of_addr validators node_id)) in
  let+ prior = cancel_scope t value.cancel value.origin signed in
  match prior with
  | Some validators when signed validators -> prior
  | _ ->
    let current, _ = current_validator_set t in
    if signed current then Some current else None

let verify_notice t (notice : Octra_core.Compute_history.notice) =
  try match Protocol.decode notice.body with
  | Protocol.Cancel cancel when cancel.chain_id = t.deps.chain_id
      && cancel.caller = notice.caller && cancel.session_id = notice.session
      && (match cancel.until with None -> true | Some until -> until = notice.expires) ->
    Protocol.cancel_shape cancel
    && (Octra_consensus.C_seen.known t.seen (wire_id notice.body)
      || verify_cancellation t {
        caller = cancel.caller; caller_public_key = cancel.caller_public_key;
        caller_signature = cancel.caller_signature; session_id = cancel.session_id; until = cancel.until;
      })
  | _ -> false
  with Invalid_argument _ | Failure _ -> false

let read_cancel_acks t ~caller ~session =
  match current_head t with
  | Error reason -> Lwt.return (Error reason)
  | Ok head ->
    let* stored = use_history t (fun history ->
      Octra_core.Compute_history.cancel_acks history ~epoch:head.epoch_id ~caller ~session) in
    Lwt.return (Result.bind stored (function
      | None -> Ok None
      | Some (notice, rows) ->
        try
          if not (verify_notice t notice) then invalid_arg "compute cancel notice differs";
          let acks = List.map (fun wire -> match Protocol.decode wire with
            | Protocol.Cancel_ack ack when verify_cancel_ack t ack
                && ack.delivery.origin = t.deps.node_id
                && ack.delivery.expires = notice.expires
                && ack.delivery.round <= notice.round
                && Protocol.encode (Protocol.Cancel ack.delivery.cancel) = notice.body -> ack
            | _ -> invalid_arg "compute cancel receipt differs") rows in
          let nodes = List.map (fun ack -> ack.Protocol.node_id) acks in
          if List.length nodes <> List.length (List.sort_uniq String.compare nodes) then
            Error "compute cancel receipt repeated"
          else Ok (Some (notice, acks))
        with _ -> Error "compute cancel receipts unreadable"))

let refresh_cancels t epoch =
  let* result = use_history t (fun history ->
    match Octra_core.Compute_history.prune_cancels history ~epoch with
    | Error _ as error -> error
    | Ok _ -> Octra_core.Compute_history.pending_notices history ~epoch) in
  match result with
  | Error _ as error -> Lwt.return error
  | Ok rows when not (List.for_all (verify_notice t) rows) ->
    Lwt.return (Error "compute saved cancellation differs")
  | Ok rows ->
    prune_cancelled_sessions t epoch;
    List.iter (fun (notice : Octra_core.Compute_history.notice) ->
      ignore (Octra_consensus.C_seen.remember t.seen (wire_id notice.body));
      let key = cancellation_key notice.caller notice.session in
      match Hashtbl.find_opt t.cancelled_sessions key with
      | Some (_, expires) when expires >= notice.expires -> ()
      | _ -> Hashtbl.replace t.cancelled_sessions key (notice.caller, notice.expires)) rows;
    Lwt.return (Ok ())

let cancel_session_direct ?(send = false) ?round t (cancel : cancellation) =
  if not (Octra_core.Crypto.Address.is_valid_address cancel.caller)
     || not (Protocol.cancel_shape (wire_cancel t cancel)) then
    Lwt.return (Error "resource compute cancellation refused")
  else if not (verify_cancellation t cancel) then
    Lwt.return (Error "resource compute cancellation signature refused")
  else match current_head t with
  | Error message -> Lwt.return (Error message)
  | Ok head when (match cancel.until with None -> false | Some until ->
      until < head.epoch_id || Int64.sub until head.epoch_id > call_ttl) ->
    Lwt.return_error "resource compute cancellation expired or too distant"
  | Ok head ->
    let* loaded = refresh_cancels t head.epoch_id in
    begin match loaded with
    | Error _ as error -> Lwt.return error
    | Ok () ->
    let key = cancellation_key cancel.caller cancel.session_id in
    let* known = use_history t (fun history -> Octra_core.Compute_history.known_job history
        ~caller:cancel.caller ~session:cancel.session_id) in
    let pending = if known = Ok true then None else Hashtbl.fold (fun _ state found ->
      if state.call.request.caller = cancel.caller
          && state.call.request.session_id = cancel.session_id then
        Some (match found with None -> state.call.request.expires_epoch
          | Some expiry -> Int64.max expiry state.call.request.expires_epoch)
      else found) t.requests None in
    let known = Result.map (fun saved -> saved || Option.is_some pending) known in
    match known with
    | Error _ as error -> Lwt.return error
    | Ok false when send -> Lwt.return_error "resource compute session unknown"
    | Ok false -> Lwt.return_ok 0
    | Ok _ ->
    let* deadline = use_history t (fun history -> Octra_core.Compute_history.cancel_deadline history
      ~caller:cancel.caller ~session:cancel.session_id) in
    let deadline = Result.map (function None -> pending | value -> value) deadline in
    match deadline with
    | Error _ as error -> Lwt.return error
    | Ok (Some until) when cancel.until <> Some until ->
      Lwt.return_error "resource compute cancellation deadline required"
    | Ok _ ->
    if not (cancel_available t cancel.caller cancel.session_id) then
      Lwt.return (Error "resource compute cancellation table full")
    else
      let body = Protocol.encode (Protocol.Cancel (wire_cancel t cancel)) in
      let* saved = use_history t (fun history ->
        match round with
        | Some _ -> Octra_core.Compute_history.cancel ?expires:cancel.until history
            ~caller:cancel.caller ~session:cancel.session_id
        | None when send ->
          if head.epoch_id > Int64.sub Int64.max_int call_ttl then
            Error "compute cancel expiry refused"
          else Octra_core.Compute_history.queue_cancel ?until:cancel.until history
            ~epoch:head.epoch_id ~expires:(Option.value cancel.until ~default:(Int64.add head.epoch_id call_ttl))
            ~caller:cancel.caller ~session:cancel.session_id
            ~body
        | None -> Octra_core.Compute_history.cancel ?expires:cancel.until history
          ~caller:cancel.caller ~session:cancel.session_id) in
      match saved with
      | Error reason -> Lwt.return (Error reason)
      | Ok false -> Lwt.return (Error "resource compute cancellation not recorded")
      | Ok true ->
        let expires = match round with
          | Some value -> value.Protocol.expires
          | None -> Option.value cancel.until ~default:(Int64.add head.epoch_id call_ttl) in
        Hashtbl.replace t.cancelled_sessions key (cancel.caller, expires);
        Hashtbl.remove t.committee_leases key;
        let count = Hashtbl.fold (fun _ (job : job) count ->
          if job.caller = cancel.caller && job.session_id = cancel.session_id
             && Lwt.is_sleeping job.result then begin
            release_job_lease t job;
            Lwt.cancel job.result;
            count + 1
          end else count) t.jobs 0 in
        Lwt.return (Ok count)
    end

let protocol_message_name = function
  | Protocol.Offer _ -> "offer"
  | Protocol.Capability _ -> "capability"
  | Protocol.Commitment _ -> "commitment"
  | Protocol.Reveal _ -> "reveal"
  | Protocol.Call _ -> "call"
  | Protocol.Result _ -> "result"
  | Protocol.Cancel _ -> "cancel"
  | Protocol.Cancel_round _ -> "cancel_round"
  | Protocol.Cancel_ack _ -> "cancel_ack"
  | Protocol.Fetch _ -> "fetch"
  | Protocol.Answer _ -> "answer"

let funding_matches t call (funds : Funding.funded) =
  match current_head t, Protocol.header call with
  | Ok head, Ok header ->
    header = funds.header && head.epoch_id = funds.epoch
    && hex_of_raw head.state_root = funds.state_root
    && header.request.chain_id = t.deps.chain_id
  | _ -> false

let read_funding t call =
  let* checked = actor_call t (Check_funding call) in
  match checked with
  | Error _ as error -> Lwt.return error
  | Ok None -> Lwt.return_ok None
  | Ok (Some head) ->
    let observed = Lwt.finalize
      (fun () ->
        match Protocol.header call with
        | Error _ as error -> Lwt.return error
        | Ok header ->
          let+ observed = Lwt.catch
            (fun () -> t.deps.funding head header)
            (function Lwt.Canceled -> Lwt.fail Lwt.Canceled
              | _ -> Lwt.return_error "compute funding read unavailable") in
          Ok (Result.value observed ~default:None))
      (fun () -> Lwt.catch
        (fun () -> actor_call t (Drop_funding (request_id call)))
        (fun _ -> Lwt.return_unit)) in
    Lwt.pick [Lwt.protected observed;
      (let* () = t.deps.sleep 2.0 in Lwt.return_ok None)]

let message_funding t = function
  | Protocol.Call call -> read_funding t call
  | _ -> Lwt.return_ok None

let rec publish t message =
  let+ _ = publish_checked t message in
  ()

and publish_checked t message =
  let payload = Protocol.encode message in
  let* observed = message_funding t message in
  let* accepted = match observed with
    | Error _ -> Lwt.return_false
    | Ok funds -> actor_call t (Apply_protocol { wire_key = wire_id payload; source = None; message; funds }) in
  if accepted then
    let* () = match message with
      | Protocol.Reveal _ -> Lwt.return_unit
      | _ -> t.deps.broadcast payload in
    let+ () = forward_reveals t () in
    true
  else begin
    Log.warn "compute"
      "event = local_protocol status = refused message = %s"
      (protocol_message_name message);
    Lwt.return_false
  end

and forward_reveals t () =
  actor_call t Wake_reveals

and send_reveal t payload =
  Lwt.protected (
    let sent = ref false in
    Lwt.finalize
      (fun () -> Lwt.catch
        (fun () ->
          let+ () = Lwt.pick [t.deps.broadcast payload;
            (let* () = t.deps.sleep Octra_net.P2p_swarm.broadcast_deadline in
              Lwt.fail_with "compute reveal send expired")] in
          sent := true)
        (function Lwt.Canceled -> Lwt.fail Lwt.Canceled | _ -> Lwt.return_unit))
      (fun () -> actor_call t (Sent_reveal (payload, !sent))))

and apply_message_direct t funds message =
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
      let checked = match Hashtbl.find_opt t.requests (request_id call) with
        | Some state when state.call = call -> Ok (state.proof, state.selection, state.validator_set)
        | _ -> Result.map (fun (offer, selection, validators) ->
            offer_snapshot offer, selection, validators) (verify_call t call) in
      match checked with
      | Error reason ->
        Log.warn "compute"
          "event = call status = refused reason = %s"
          reason;
        Lwt.return_false
      | Ok _ when not (Option.fold ~none:false ~some:(funding_matches t call) funds) ->
        Lwt.return_false
      | Ok (proof, selection, validator_set) ->
        let _ = request_state t call selection validator_set proof in
        let selected = List.mem t.deps.node_id (selected_ids selection) in
        let* reserved = if selected then reserve_call t call validator_set else Lwt.return (Ok Skipped) in
        begin match reserved with
        | Error reason ->
          Log.warn "compute" "event = call status = unavailable reason = %s" reason;
          Lwt.return_false
        | Ok (Retained result) when Option.is_none (result_state t result) ->
          Log.warn "compute" "event = result_replay status = refused reason = saved_result_signature";
          Lwt.return_false
        | Ok execution ->
          Log.info "compute"
            "event = call status = accepted selected = %b executable = %b"
            selected (execution = Execute);
          begin match execution with
          | Execute ->
            spawn_background t "provider_execution" (fun () -> execute_selected_call t call)
          | Retained result ->
            spawn_background t "result_replay" (fun () -> publish t (Protocol.Result result))
          | Skipped -> ()
          end;
          Lwt.return_true
        end
    end
  | Protocol.Result result ->
    begin match Hashtbl.find_opt t.requests result.vote.Certificate.request_id with
    | None -> Lwt.return_false
    | Some state ->
      let* cancelled = use_history t (fun history ->
        Octra_core.Compute_history.cancelled history
          ~caller:state.call.request.caller ~session:state.call.request.session_id) in
      if cancelled <> Ok false then Lwt.return_false else record_result t result
    end
  | Protocol.Cancel cancel ->
    if cancel.chain_id <> t.deps.chain_id then Lwt.return_false
    else
      let* result = cancel_session_direct t {
        caller = cancel.caller; caller_public_key = cancel.caller_public_key;
        session_id = cancel.session_id; caller_signature = cancel.caller_signature; until = cancel.until;
      } in
      Lwt.return (Result.is_ok result)
  | Protocol.Cancel_round value ->
    begin match current_head t with
    | Error _ -> Lwt.return_false
    | Ok head ->
      if not (Protocol.cancel_round_shape value)
         || value.cancel.chain_id <> t.deps.chain_id
         || value.epoch > head.epoch_id || value.expires < head.epoch_id
         || not (verify_cancellation t {
           caller = value.cancel.caller; caller_public_key = value.cancel.caller_public_key;
           session_id = value.cancel.session_id; caller_signature = value.cancel.caller_signature;
           until = value.cancel.until}) then
        Lwt.return_false
      else
        let* validators = round_validators t value in
        if Option.is_none validators then Lwt.return_false else
        let* result = cancel_session_direct ~round:value t {
          caller = value.cancel.caller; caller_public_key = value.cancel.caller_public_key;
          session_id = value.cancel.session_id; caller_signature = value.cancel.caller_signature;
          until = value.cancel.until;
        } in
        Lwt.return (Result.is_ok result)
    end
  | Protocol.Cancel_ack ack ->
    begin match current_head t with
    | Error _ -> Lwt.return_false
    | Ok head ->
      let validators, _ = current_validator_set t in
      let delivery = ack.delivery in
      if ack.epoch > head.epoch_id || delivery.expires < head.epoch_id
         || not (verify_cancel_ack t ack) then
        Lwt.return_false
      else
        let matches validators =
          Octra_consensus.C_types.pubkey_of_addr validators delivery.origin = Some ack.origin_key
          && Octra_consensus.C_types.pubkey_of_addr validators ack.node_id = Some ack.node_key in
        let* allowed = if matches validators then Lwt.return_true else
          let+ prior = cancel_scope t delivery.cancel delivery.origin matches in
          Option.fold ~none:false ~some:matches prior in
        if not allowed then Lwt.return_false
        else if delivery.origin <> t.deps.node_id then Lwt.return_true
        else
        let cancel = delivery.cancel in
        let* saved = use_history t (fun history ->
          Octra_core.Compute_history.confirm_cancel history ~epoch:head.epoch_id
            ~expires:delivery.expires ~round:delivery.round
            ~caller:cancel.caller ~session:cancel.session_id
            ~cancel:(Protocol.encode (Protocol.Cancel cancel)) ~node:ack.node_id
            ~body:(Protocol.encode (Protocol.Cancel_ack ack))) in
        Lwt.return (saved = Ok true)
    end
  | Protocol.Fetch fetch ->
    if not (fetch_window t fetch) then
      Lwt.return_false
    else
      let call = fetch.call in
      let* saved = saved_reply t call in
      begin match saved with
      | Error _ -> Lwt.return_false
      | Ok saved ->
        begin match reply_validators t fetch saved with
        | Some validators when valid_fetch t validators fetch
            && take_read t ~answer:false fetch ->
          begin match saved with
          | Some saved when fetch.target = t.deps.node_id ->
            begin match Protocol.decode saved.reply with
            | Protocol.Result result when valid_answer validators fetch result ->
              spawn_background t "result_answer"
                (fun () -> publish t (Protocol.Answer (fetch, result)));
              Lwt.return_true
            | _ -> Lwt.return_false
            | exception _ -> Lwt.return_false
            end
          | _ -> Lwt.return_true
          end
        | _ -> Lwt.return_false
        end
      end
  | Protocol.Answer (fetch, result) ->
    if not (fetch_window t fetch) then Lwt.return_false
    else
      let* saved = saved_reply t fetch.call in
      let validators = match saved with
        | Error _ -> None
        | Ok saved -> reply_validators t fetch saved in
      match validators with
      | None -> Lwt.return_false
      | Some validators when not (valid_fetch t validators fetch && valid_answer validators fetch result)
          || not (take_read t ~answer:true fetch) -> Lwt.return_false
      | Some _ ->
      let request = fetch.call.request in
      let* cancelled = use_history t (fun history ->
        Octra_core.Compute_history.cancelled history
          ~caller:request.caller ~session:request.session_id) in
      if cancelled <> Ok false then Lwt.return_false
      else if fetch.call.coordinator <> t.deps.node_id then Lwt.return_true
      else match Hashtbl.find_opt t.requests (request_id fetch.call) with
        | Some state when state.call = fetch.call -> record_result t result
        | _ -> Lwt.return_false

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
          "event = provider_check status = checking phase = self_test" in
      let* test_result =
        actor_call t Ensure_self_test in
      begin
        match test_result with
        | Error reason ->
          Log.warn "compute"
            "event = provider_check status = refused phase = self_test reason = %s"
            reason;
          Lwt.return_unit
        | Ok test when test.executor_root <> offer.executor_root ->
          Lwt.return_unit
        | Ok test ->
          Log.info "compute"
            "event = provider_check status = checking phase = prepare";
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
                "event = provider_check status = committed commit_epoch = %Ld"
                current.epoch_id;
              schedule_reveal
                  t
                  offer.expires_epoch
                  commitment
                  test.evidence_root
                  nonce
            | Error reason, _ ->
              Log.warn "compute"
                "event = provider_check status = refused phase = prepare reason = %s"
                reason;
              Lwt.return_unit
            | Ok _, Error reason ->
              Log.warn "compute"
                "event = provider_check status = refused phase = head reason = %s"
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
      "event = provider_check status = revealed reveal_epoch = %Ld"
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
    let request = call.request in
    let* prepared = Engine.prepare provider.engine Provider_rpc.{
      model_epoch = request.model_epoch;
      model_state_root = hex_of_raw request.model_state_root;
      circle_id = request.circle_id;
      graph_root = hex_of_raw request.graph_root;
      model_root = hex_of_raw request.model_root;
      program_root = hex_of_raw request.program_root;
      executor_root = hex_of_raw request.executor_root;
    } in
    let* executed_result =
      match prepared with
      | Error _ as error -> Lwt.return error
      | Ok _ -> Engine.execute provider.engine (execute_request call) in
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
        let result = Protocol.{vote; output_json = executed.output_json} in
        let* saved = actor_call t (Save_result (call, result)) in
        begin match saved with
        | Ok true -> publish t (Protocol.Result result)
        | Ok false ->
          Log.warn "compute" "event = result_save status = refused reason = request_changed";
          Lwt.return_unit
        | Error reason ->
          Log.warn "compute" "event = result_save status = unavailable reason = %s" reason;
          Lwt.return_unit
        end
    end

let on_message t ~source payload =
  match Protocol.decode payload with
  | exception _ -> Lwt.return_unit
  | message ->
    let generation = t.actor_generation in
    let* accepted =
      Lwt.catch
        (fun () ->
          let* observed = message_funding t message in
          match observed with
          | Error _ -> Lwt.return_false
          | Ok funds -> actor_try_call t
              (Apply_protocol { wire_key = wire_id payload; source = Some source; message; funds }))
        (fun _ -> Lwt.return_false) in
    if not accepted then Lwt.return_unit
    else
      let* () = match message with
        | Protocol.Reveal _ -> Lwt.return_unit
        | Protocol.Cancel_ack ack ->
          let* path = actor_call t (Cancel_path ack.delivery) in
          (match path with
          | Some peer -> Lwt.catch (fun () -> Lwt.pick [t.deps.send ~peer payload;
              (let* () = t.deps.sleep 2.0 in Lwt.return_unit)])
              (fun _ -> Lwt.return_unit)
          | None -> Lwt.return_unit)
        | _ -> t.deps.relay ~source payload in
      let* () = match message, current_head t with
      | Protocol.Cancel_round delivery, Ok head when t.channel_open
          && generation = t.actor_generation && head.epoch_id <= delivery.expires ->
        let* keys = Lwt.catch (fun () -> actor_call t (Cancel_keys delivery))
          (fun _ -> Lwt.return_none) in
        begin match keys with
        | Some (origin_key, node_key) when t.channel_open && generation = t.actor_generation ->
          let ack = Protocol.{delivery; origin_key; node_id = t.deps.node_id;
            node_key; epoch = head.epoch_id; signature = zero64} in
          let ack = {ack with signature = t.deps.sign (Protocol.cancel_ack_sign ack)} in
          Lwt.catch
            (fun () -> Lwt.pick [
              t.deps.send ~peer:source (Protocol.encode (Protocol.Cancel_ack ack));
              (let* () = t.deps.sleep 2.0 in Lwt.return_unit)])
            (fun _ -> Lwt.return_unit)
        | _ -> Lwt.return_unit
        end
      | _ -> Lwt.return_unit in
      forward_reveals t ()

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

let rec wait_for_providers t offer_key =
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
          "event = committee status = ready entries = %d target = %d"
          (List.length nodes)
          committee_size in
      Lwt.return (Ok (head, snapshot))
    else
      let* () = t.deps.sleep 0.1 in
      wait_for_providers t offer_key

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

let wait_for_certificate t request_key =
  let rec loop ticks round next =
    let* progress = actor_call t (Query_certificate request_key) in
    match progress with
    | Certificate_finished response -> Lwt.return (Ok response)
    | Certificate_refused message -> Lwt.return (Error message)
    | Certificate_pending ->
      let* step = if ticks < next || round >= Protocol.fetch_rounds then
          Lwt.return (Ok (round, next))
        else
          let* claimed = actor_call t (Request_replies request_key) in
          begin match claimed with
          | Error _ as error -> Lwt.return error
          | Ok None -> Lwt.return (Ok (Protocol.fetch_rounds, next))
          | Ok (Some (saved, reads)) ->
            let* () = Lwt_list.iter_s (fun fetch -> Lwt.catch
              (fun () -> Lwt.pick [publish t (Protocol.Fetch fetch);
                (let* () = t.deps.sleep 2.0 in Lwt.fail_with "compute fetch send expired")])
              (function Lwt.Canceled -> Lwt.fail Lwt.Canceled | _ -> Lwt.return_unit)) reads in
            Lwt.return (Ok (saved + 1, ticks + (10 lsl (saved + 1))))
          end in
      begin match step with
      | Error _ as error -> Lwt.return error
      | Ok (round, next) ->
        let* () = t.deps.sleep 0.1 in
        loop (ticks + 1) round next
      end in
  loop 0 0 10

let request_input_bytes (request : request) =
  String.length request.method_name + String.length request.params_json

let unsigned_offer t (request : request) (head : head) validator_set_root =
  Protocol.{
    chain_id = t.deps.chain_id;
    offered_epoch = head.epoch_id;
    expires_epoch = (match request.valid_until with None -> Int64.add head.epoch_id offer_ttl
      | Some epoch -> Int64.min epoch (Int64.add head.epoch_id offer_ttl));
    coordinator = t.deps.node_id;
    validator_set_root;
    circle_id = request.circle_id;
    model_epoch = request.model_epoch;
    model_state_root = request.model_state_root;
    graph_root = request.graph_root;
    model_root = request.model_root;
    program_root = request.program_root;
    executor_root = request.executor_root;
    min_memory_bytes = (match request.memory_bytes with
      | None -> 1_073_741_824L | Some bytes -> Int64.max 1_073_741_824L (Int64.of_int bytes));
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
    unit_price = request.unit_price;
    fee_cap = request.fee_cap;
    memory_bytes = request.memory_bytes;
    guest_bytes = request.guest_bytes;
    fuel_limit = request.fuel_limit;
    chain_id = t.deps.chain_id;
    epoch_id;
    expires_epoch = (match request.valid_until with None -> expires_epoch
      | Some epoch -> Int64.min epoch expires_epoch);
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
    valid_until = request.valid_until;
    method_name = request.method_name;
    params_json = request.params_json;
    caller_public_key = request.caller_public_key;
    caller_signature = request.caller_signature;
    coordinator = t.deps.node_id;
    signature = zero64;
  }

let unsigned_call_for_lease t request lease =
  Protocol.{
    valid_until = request.valid_until;
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

let sign_call t (call : Protocol.call) =
  { call with signature = t.deps.sign (Protocol.call_sign_bytes call) }

let request_shape (request : request) =
  Certificate.fee_valid ~fuel:request.fuel_limit ~price:request.unit_price ~cap:request.fee_cap
  && (match request.unit_price with None -> true | Some _ ->
    Result.is_ok (Octra_core.Compute_bill.resource_limits ~fuel:request.fuel_limit
      ~memory:request.memory_bytes ~guest:request.guest_bytes))
  && (match request.memory_bytes with None -> true | Some bytes ->
    Option.is_some request.guest_bytes && bytes > 0 && bytes <= Certificate.max_memory_bytes)
  && (match request.guest_bytes with None -> true | Some bytes ->
    Option.is_some request.fuel_limit && bytes >= 65_536 && bytes <= 67_108_864
    && bytes mod 65_536 = 0)
  && (match request.fuel_limit with None -> true | Some fuel -> fuel > 0 && fuel <= 2_000_000_000)
  && (match request.valid_until with None -> true | Some epoch -> epoch >= 0L)
  && request.model_epoch >= 0L
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

let request_of_call (call : Protocol.call) =
  let value = call.request in
  {unit_price = value.unit_price; fee_cap = value.fee_cap;
    memory_bytes = value.memory_bytes; guest_bytes = value.guest_bytes;
    fuel_limit = value.fuel_limit; valid_until = call.valid_until;
    model_epoch = value.model_epoch; model_state_root = value.model_state_root;
    circle_id = value.circle_id; graph_root = value.graph_root; model_root = value.model_root;
    program_root = value.program_root; executor_root = value.executor_root;
    caller = value.caller; caller_public_key = call.caller_public_key;
    caller_signature = call.caller_signature; session_id = value.session_id;
    sequence = value.sequence; method_name = call.method_name; params_json = call.params_json;
    max_output_bytes = value.max_output_bytes}

let read_plan t ~caller ~id body =
  try
    let record = Record.decode body in
    let call = record.call in
    let value = call.Protocol.request in
    let request = request_of_call call in
    let validators, root = current_validator_set t in
    if call.valid_until = None || call.request.fuel_limit = None || call.request.guest_bytes = None
       || call.request.memory_bytes = None || call.request.unit_price = None
       || record.validators <> None || record.capabilities <> []
       || record.commitments <> [] || record.reveals <> []
       || record.results <> [] || not (Protocol.call_shape call && request_shape request)
       || caller <> request.caller || id <> intent_id call
       || value.chain_id <> t.deps.chain_id || call.coordinator <> t.deps.node_id
       || value.validator_set_root <> root || value.selection_root <> zero32
       || value.epoch_id <> record.offer.offered_epoch
       || t.deps.state_root_at value.epoch_id <> Some value.state_root
       || not (verify_caller call)
       || not (verify_node validators call.coordinator (Protocol.call_sign_bytes call) call.signature)
       || record.offer.coordinator <> call.coordinator
       || not (verify_offer_at t value.epoch_id record.offer)
       || not (offer_model_matches record.offer ~circle_id:request.circle_id
         ~model_epoch:request.model_epoch ~model_state_root:request.model_state_root
         ~graph_root:request.graph_root ~model_root:request.model_root
         ~program_root:request.program_root ~executor_root:request.executor_root) then
      Error "compute saved plan refused"
    else Ok (request, record.offer)
  with _ -> Error "compute saved plan unreadable"

let saved_response state =
  match decision state with
  | Certificate.Agreed certificate ->
    begin match output_for_certificate state certificate with
    | Some output_json -> Ok {output_json; request = state.call.request;
        certificate; selection = state.selection}
    | None -> Error "compute job output absent"
    end
  | _ -> Error "compute job certificate refused"

let restore_state t ~caller ~id body =
  Lwt.catch (fun () ->
    let record = Record.decode body in
    let call = record.call in
    let request = call.Protocol.request in
    let validator_set = match record.validators with
      | Some value -> value
      | None -> t.deps.validator_set () in
    let root = Octra_consensus.C_config.validator_set_hash validator_set in
    let snapshot = {offer = record.offer; capabilities = record.capabilities;
      commitments = record.commitments; reveals = record.reveals} in
    let valid =
      Protocol.call_shape call && caller = request.caller && intent_id call = id
      && request.chain_id = t.deps.chain_id && root = request.validator_set_root
      && call.coordinator = t.deps.node_id
      && t.deps.state_root_at request.epoch_id = Some request.state_root
      && t.deps.state_root_at request.model_epoch = Some request.model_state_root
      && verify_caller call
      && verify_node validator_set call.coordinator (Protocol.call_sign_bytes call) call.signature
      && verify_offer_at ~validators:validator_set t request.epoch_id record.offer
      && Int64.sub request.expires_epoch request.epoch_id <= call_ttl
      && offer_model_matches record.offer ~circle_id:request.circle_id
           ~model_epoch:request.model_epoch ~model_state_root:request.model_state_root
           ~graph_root:request.graph_root ~model_root:request.model_root
           ~program_root:request.program_root ~executor_root:request.executor_root
      && List.for_all (fun value -> offer_matches_commitment record.offer value
           && verify_commitment validator_set value) record.commitments in
    if not valid then Lwt.return (Error "compute job identity refused")
    else
      let* capabilities = Lwt_list.for_all_s (fun value ->
        if not (offer_matches_capability record.offer value) then Lwt.return_false
        else verify_capability_at ~validators:validator_set t request.epoch_id value) record.capabilities in
      if not capabilities then Lwt.return (Error "compute job capability refused")
      else
        let selection = selection_for_snapshot t validator_set snapshot request in
        let offer = {offer = record.offer; capabilities = Hashtbl.create 8;
          commitments = Hashtbl.create 8; reveals = Hashtbl.create 8;
          pending_reveals = Hashtbl.create 8; local_started = false} in
        List.iter (fun value -> Hashtbl.replace offer.capabilities (Capability.id value) value)
          record.capabilities;
        let state = {call; selection; validator_set; pinned = Option.is_some record.validators;
          proof = snapshot; votes = Hashtbl.create 8} in
        let nodes = List.map (fun result -> result.Protocol.vote.Certificate.node_id) record.results in
        if selection.root <> request.selection_root
           || List.length selection.members <> committee_members validator_set
           || not (selection_supports_request offer request request.epoch_id selection)
           || List.length nodes <> List.length (List.sort_uniq String.compare nodes)
           || not (List.for_all (result_valid state) record.results) then
          Lwt.return (Error "compute job evidence refused")
        else begin
          List.iter (fun result -> Hashtbl.add state.votes result.Protocol.vote.node_id result)
            record.results;
          Lwt.return (Ok state)
        end)
    (fun _ -> Lwt.return (Error "compute job record unreadable"))

let restore_job t ~caller ~id body =
  let+ state = restore_state t ~caller ~id body in
  Result.bind state saved_response

let retained_job t ~caller ~id =
  let* stored = use_history t (fun history ->
    Octra_core.Compute_history.load_job history ~caller ~id) in
  match stored with
  | Error _ as error -> Lwt.return error
  | Ok None -> Lwt.return (Ok None)
  | Ok (Some body) ->
    let+ result = restore_job t ~caller ~id body in
    Result.map Option.some result

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
  && (match request.memory_bytes with None -> true
    | Some bytes -> Int64.of_int bytes <= lease.memory_bytes)

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
    let memory_bytes = List.fold_left
      (fun bytes capability -> Int64.min bytes capability.Capability.memory_bytes)
      Int64.max_int capabilities in
    if
      capabilities = []
      || expires_epoch < response.request.Certificate.epoch_id
      || request_input_bytes request > max_request_bytes
      || request.max_output_bytes > max_response_bytes
      || (match request.memory_bytes with None -> false
        | Some bytes -> Int64.of_int bytes > memory_bytes)
    then None
    else
      Some {
        memory_bytes;
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

let start_work t (request : request) id computation =
  set_caller_active t request.caller 1;
  let result = Lwt.finalize
    (fun () ->
      let* result = Lwt.catch computation (fun error ->
        Lwt.return (Error ("resource compute actor failed: " ^ Printexc.to_string error))) in
      match result with
      | Error _ -> Lwt.return result
      | Ok response ->
        let+ saved = actor_call t (Save_job (id, response)) in
        Result.map (fun () -> response) saved)
    (fun () -> Lwt.catch
      (fun () -> complete_job t ~job_id:id ~caller:request.caller)
      (fun _ -> Lwt.return_unit)) in
  let entry = {caller = request.caller; session_id = request.session_id;
    lease_active = true; result} in
  Hashtbl.add t.jobs id entry;
  Queue.push id t.job_order;
  entry

let await_funding t call =
  let clock = Mtime_clock.counter () in
  let rec wait ticks =
    if ticks = 3000 || Mtime.Span.to_float_ns (Mtime_clock.count clock) >= 300_000_000_000.
    then Lwt.return_error "compute funding wait expired"
    else
      let* observed = read_funding t call in
      let* released = match observed with
        | Error _ as error -> Lwt.return error
        | Ok None -> Lwt.return (Ok false)
        | Ok (Some funds) -> actor_call t (Fund_call (call, funds)) in
      match released, current_head t with
      | Error _ as error, _ -> Lwt.return error
      | Ok true, _ ->
        let* sent = publish_checked t (Protocol.Call call) in
        if sent then wait_for_certificate t (request_id call)
        else let* () = t.deps.sleep 0.1 in wait (ticks + 1)
      | Ok false, Ok head when head.epoch_id <= call.Protocol.request.expires_epoch ->
        let* () = t.deps.sleep 0.1 in
        wait (ticks + 1)
      | _, _ -> Lwt.return_error "compute funding wait expired" in
  wait 0

let send_call t call =
  let* saved = actor_call t (Issue_call call) in
  match saved with
  | Error _ as error -> Lwt.return error
  | Ok () -> await_funding t call

let compute_new t request validator_set_root offer =
  Log.info "compute" "event = committee status = offered";
  let* () = publish t (Protocol.Offer offer) in
  let offer_key = offer_id offer in
  let* providers = wait_for_providers t offer_key in
  match providers with
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
        Log.info "compute"
          "event = committee status = selected members = %d"
          (List.length selection.Selection.members);
        let* result = send_call t call in
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
  Log.info "compute"
    "event = committee status = reused members = %d until_epoch = %Ld"
    (List.length lease.selection.Selection.members)
    lease.expires_epoch;
  send_call t call

let prune_jobs t epoch =
  let chain_id = t.deps.chain_id in
  use_history t (fun history ->
    Octra_core.Compute_history.prune_jobs history ~expired:(fun ~caller ~session ~id body ->
      let record = Record.decode body in
      let call = record.Record.call in
      call.request.chain_id = chain_id && call.request.caller = caller
      && call.request.session_id = session && intent_id call = id
      && call.request.expires_epoch < epoch))

let start_job_direct ?(resume = false) t request =
  if not (request_shape request) then
    Lwt.return (Error "resource compute request refused")
  else if not resume && request.unit_price = None then
    Lwt.return (Error "resource compute caller fee required")
  else if not resume && request.memory_bytes = None then
    Lwt.return (Error "resource compute caller memory required")
  else if not resume && request.guest_bytes = None then
    Lwt.return (Error "resource compute caller pages required")
  else if not resume && request.fuel_limit = None then
    Lwt.return (Error "resource compute caller fuel required")
  else if not resume && request.valid_until = None then
    Lwt.return (Error "resource compute caller expiry required")
  else
    match current_head t with
    | Error message -> Lwt.return (Error message)
    | Ok head ->
      let* loaded = refresh_cancels t head.epoch_id in
      begin match loaded with
      | Error _ as error -> Lwt.return error
      | Ok () ->
      let validator_set, validator_set_root = current_validator_set t in
      if
        request.model_epoch > head.epoch_id
        || t.deps.state_root_at request.model_epoch <> Some request.model_state_root
      then
        Lwt.return (Error "resource compute model snapshot unavailable")
      else if not resume
          && not (Octra_consensus.C_types.is_validator validator_set t.deps.node_id) then
        Lwt.return (Error "resource compute coordinator is not a validator")
      else if session_cancelled t request.caller request.session_id then
        Lwt.return (Error "resource compute session cancelled")
      else
        let preview_call =
          unsigned_call t request head validator_set_root zero32 in
        if not (verify_caller preview_call) then
          Lwt.return (Error "resource compute caller signature refused")
        else
          let id = intent_id preview_call in
          let* pruned = prune_jobs t head.epoch_id in
          match pruned with
          | Error reason -> Lwt.return_error reason
          | Ok _ ->
          let* loaded = refresh_cancels t head.epoch_id in
          match loaded with
          | Error reason -> Lwt.return_error reason
          | Ok () ->
          prune_finished_jobs t;
          let existing = Hashtbl.find_opt t.jobs id in
          let* retained = if Option.is_some existing then Lwt.return (Ok None)
            else retained_job t ~caller:request.caller ~id in
          match retained with
          | Error reason -> Lwt.return (Error reason)
          | Ok (Some response) -> Lwt.return (Ok (id, {
              caller = request.caller; session_id = request.session_id;
              lease_active = false; result = Lwt.return (Ok response)}))
          | Ok None ->
          if Option.is_none existing && not resume
             && (match request.valid_until with None -> true | Some epoch ->
                epoch < head.epoch_id || Int64.sub epoch head.epoch_id > call_ttl) then
            Lwt.return (Error "resource compute caller expiry refused")
          else
          if Option.is_none existing && Hashtbl.length t.jobs >= max_jobs then
            Lwt.return (Error "resource compute job table full")
          else if Option.is_none existing && not (caller_admitted t request.caller) then
            Lwt.return (Error "resource compute caller already active")
          else if Option.is_none existing
              && not (cancel_available t request.caller request.session_id) then
            Lwt.return (Error "resource compute cancellation table full")
          else
            let* saved = use_history t (fun history ->
              Result.map not (Octra_core.Compute_history.cancelled history
                ~caller:request.caller ~session:request.session_id)) in
            match saved, existing with
            | Error reason, _ -> Lwt.return (Error reason)
            | Ok false, _ -> Lwt.return (Error "resource compute session cancelled")
            | Ok true, Some job -> Lwt.return (Ok (id, job))
            | Ok true, None ->
            let* pending = use_history t (fun history ->
              Octra_core.Compute_history.work history ~caller:request.caller ~id) in
            let start computation = Lwt.return (Ok (id, start_work t request id computation)) in
            begin match pending with
            | Error _ as error -> Lwt.return error
            | Ok (Some (Octra_core.Compute_history.Planned body)) ->
              if not (Octra_consensus.C_types.is_validator validator_set t.deps.node_id) then
                Lwt.return (Error "resource compute coordinator is not a validator")
              else begin match read_plan t ~caller:request.caller ~id body with
              | Error _ as error -> Lwt.return error
              | Ok (saved, offer) when saved = request && head.epoch_id <= offer.expires_epoch ->
                start (fun () -> compute_new t request validator_set_root offer)
              | _ -> Lwt.return (Error "compute saved plan expired or changed")
              end
            | Ok (Some (Octra_core.Compute_history.Funding body)) ->
              let* restored = restore_state t ~caller:request.caller ~id body in
              begin match restored with
              | Ok state when request_of_call state.call = request ->
                start (fun () -> await_funding t state.call)
              | Ok _ -> Lwt.return_error "compute saved intent differs"
              | Error _ as error -> Lwt.return error
              end
            | Ok (Some (Octra_core.Compute_history.Issued body)) ->
              let* restored = restore_state t ~caller:request.caller ~id body in
              begin match restored with
              | Error _ as error -> Lwt.return error
              | Ok state when Hashtbl.length t.requests < max_requests ->
                Hashtbl.replace t.requests (request_id state.call) state;
                start (fun () ->
                  match saved_response state with
                  | Ok _ as done_ -> Lwt.return done_
                  | Error _ -> wait_for_certificate t (request_id state.call))
              | Ok _ -> Lwt.return (Error "resource compute request table full")
              end
            | Ok None ->
              if not (Octra_consensus.C_types.is_validator validator_set t.deps.node_id) then
                Lwt.return (Error "resource compute coordinator is not a validator")
              else if request.unit_price = None then
                Lwt.return (Error "resource compute caller fee required")
              else if request.memory_bytes = None then
                Lwt.return (Error "resource compute caller memory required")
              else if request.guest_bytes = None then
                Lwt.return (Error "resource compute caller pages required")
              else if request.fuel_limit = None then
                Lwt.return (Error "resource compute caller fuel required")
              else if request.valid_until = None then
                Lwt.return (Error "resource compute caller expiry required")
              else
              let* ordered = use_history t (fun history ->
                Result.bind (Octra_core.Compute_history.scope history
                    ~caller:request.caller ~session:request.session_id)
                  (function
                    | Some saved when request.sequence <= saved.sequence ->
                      Error "resource compute sequence already issued"
                    | _ -> Ok ())) in
              match ordered with
              | Error _ as error -> Lwt.return error
              | Ok () ->
              let offer = unsigned_offer t request head validator_set_root |> sign_offer t in
              let record = Record.{offer; call = sign_call t preview_call; capabilities = [];
                commitments = []; reveals = []; results = []; validators = None} in
              let* saved = use_history t (fun history ->
                Octra_core.Compute_history.plan_job ?expires:request.valid_until history ~caller:request.caller
                  ~session:request.session_id ~id ~body:(Record.encode record)) in
              begin match saved with
              | Error _ as error -> Lwt.return error
              | Ok false -> Lwt.return (Error "compute saved plan changed")
              | Ok true ->
                start (match committee_lease_for_request t request head validator_set_root with
                  | Some lease -> fun () -> compute_reused t request lease
                  | None -> fun () -> compute_new t request validator_set_root offer)
              end
            end

      end

let submit t request =
  let* result = actor_call t (Admit_request request) in
  match result with
  | Error _ as error -> Lwt.return error
  | Ok (id, _) -> Lwt.return (Ok (hex_of_raw id, t.deps.node_id))

let status_of job =
  match Lwt.state job.result with
  | Lwt.Sleep -> Waiting
  | Lwt.Return (Ok response) -> Finished response
  | Lwt.Return (Error message) -> Refused message
  | Lwt.Fail error -> Refused ("resource compute actor failed: " ^ Printexc.to_string error)

let job_status_direct t ~caller ~id =
  match Hashtbl.find_opt t.jobs id with
  | Some job when job.caller = caller ->
    begin match status_of job with
    | Waiting ->
      let+ pending = use_history t (fun history ->
        Octra_core.Compute_history.work history ~caller ~id) in
      begin match pending with
      | Ok (Some (Octra_core.Compute_history.Funding body)) ->
        (try Some (Funding (Record.decode body |> fun record ->
          Protocol.header record.call |> Result.get_ok))
         with _ -> Some (Refused "compute saved funding unreadable"))
      | _ -> Some Waiting
      end
    | status -> Lwt.return (Some status)
    end
  | Some _ -> Lwt.return_none
  | None ->
    let* result = retained_job t ~caller ~id in
    begin match result with
    | Ok (Some response) -> Lwt.return (Some (Finished response))
    | Error reason -> Lwt.return (Some (Refused reason))
    | Ok None ->
      let* pending = use_history t (fun history -> Octra_core.Compute_history.work history ~caller ~id) in
      match pending with
      | Error reason -> Lwt.return (Some (Refused reason))
      | Ok None -> Lwt.return_none
      | Ok (Some (Octra_core.Compute_history.Funding body)) ->
        Lwt.catch (fun () ->
          let record = Record.decode body in
          if record.call.request.caller <> caller || intent_id record.call <> id then
            Lwt.return (Some (Refused "compute restored job differs"))
          else
            let* closed = use_history t (fun history ->
              Octra_core.Compute_history.cancelled history ~caller
                ~session:record.call.request.session_id) in
            Lwt.return (Some (match closed, current_head t with
              | Error reason, _ | _, Error reason -> Refused reason
              | Ok true, _ -> Refused "resource compute session cancelled"
              | Ok false, Ok head when head.epoch_id > record.call.request.expires_epoch ->
                Refused "compute funding wait expired"
              | Ok false, Ok _ ->
                match Protocol.header record.call with
                | Ok header -> Funding header
                | Error reason -> Refused reason)))
          (fun _ -> Lwt.return (Some (Refused "compute saved work unreadable")))
      | Ok (Some (Octra_core.Compute_history.Planned body | Octra_core.Compute_history.Issued body)) ->
        Lwt.catch (fun () ->
          let record = Record.decode body in
          if record.call.request.caller <> caller || intent_id record.call <> id then
            Lwt.return (Some (Refused "compute restored job differs"))
          else
            let* restored = start_job_direct ~resume:true t (request_of_call record.call) in
            Lwt.return (Some (match restored with
              | Ok (saved, job) when saved = id -> status_of job
              | Ok _ -> Refused "compute restored job differs"
              | Error reason -> Refused reason)))
          (fun _ -> Lwt.return (Some (Refused "compute saved work unreadable")))
    end

let status_direct t ~caller ~id =
  let* status = job_status_direct t ~caller ~id in
  match status with
  | Some (Refused _) ->
    let* work = use_history t (fun history ->
      Octra_core.Compute_history.work history ~caller ~id) in
    begin match work with
    | Error reason -> Lwt.return (Some (Refused reason))
    | Ok (Some (Octra_core.Compute_history.Issued body)) ->
      let* restored = restore_state t ~caller ~id body in
      begin match restored with
      | Error reason -> Lwt.return (Some (Refused reason))
      | Ok state ->
        let* closed = use_history t (fun history ->
          Octra_core.Compute_history.cancelled history ~caller ~session:state.call.request.session_id) in
        begin match closed with
        | Error reason -> Lwt.return (Some (Refused reason))
        | Ok false -> Lwt.return status
        | Ok true ->
          let members = selected_ids state.selection |> List.sort_uniq String.compare in
          let* rows = read_cancel_acks t ~caller ~session:state.call.request.session_id in
          Lwt.return (Some (match rows with
            | Error reason -> Refused reason
            | Ok stored ->
              let receipts = match stored with
                | None -> []
                | Some (_, acks) -> Record.select_cancels ~call:state.call
                    ~validators:state.validator_set ~members acks in
              let missing = List.filter (fun node -> not (List.exists
                (fun (ack : Protocol.cancel_ack) -> ack.node_id = node) receipts)) members in
              Cancelled {selection_root = state.selection.root; members; missing; receipts;
                expires = Option.map (fun (notice, _) -> notice.Octra_core.Compute_history.expires) stored}))
        end
      end
    | Ok _ -> Lwt.return status
    end
  | _ -> Lwt.return status

let status t read =
  actor_call t (Query_status read)

let send_cancel t target =
  let* next = actor_call t (Poll_cancel target) in
  match next with
  | Error reason ->
    Log.warn "compute" "event = cancel_queue status = unavailable reason = %s" reason;
    Lwt.return_unit
  | Ok None -> Lwt.return_unit
  | Ok (Some notice) ->
    let* () = Lwt.catch
      (fun () ->
        let body = if notice.round = 1 then notice.body else
          match current_head t, Protocol.decode notice.body with
          | Ok head, Protocol.Cancel cancel ->
            let value = Protocol.{cancel; origin = t.deps.node_id; epoch = head.epoch_id;
              expires = notice.expires; round = notice.round; signature = zero64} in
            Protocol.encode (Protocol.Cancel_round {value with
              signature = t.deps.sign (Protocol.cancel_round_sign value)})
          | _ -> failwith "compute cancel retry context" in
        Lwt.pick [
        t.deps.broadcast body;
        (let* () = t.deps.sleep 2.0 in Lwt.fail_with "resource compute cancel send expired");
      ])
      (fun error ->
        Log.warn "compute" "event = cancel_send status = unavailable reason = %s"
          (Printexc.to_string error);
        Lwt.return_unit) in
    Lwt.return_unit

let cancel t cancellation =
  let* result = actor_call t (Cancel_session cancellation) in
  match result with
  | Error _ -> Lwt.return result
  | Ok _ ->
    let* () = send_cancel t (Some (cancellation.caller, cancellation.session_id)) in
    Lwt.return result

let cancel_receipts t cancellation = actor_call t (Read_cancels cancellation)

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

let prune_work t =
  match current_head t with
  | Error _ -> ()
  | Ok head ->
    prune_cancelled_sessions t head.epoch_id;
    Hashtbl.filter_map_inplace (fun _ (state : offer_state) ->
      if head.epoch_id <= state.offer.Protocol.expires_epoch then Some state
      else None) t.offers;
    Hashtbl.filter_map_inplace (fun _ (state : request_state) ->
      if head.epoch_id <= state.call.Protocol.request.expires_epoch then Some state
      else None) t.requests

let wake_reveals t =
  prune_work t;
  if t.relay_pending <> None then Lwt.return_unit else
  let now = t.deps.now () in
  let reveals = Hashtbl.fold (fun _ (state : offer_state) saved ->
    Hashtbl.fold (fun _ reveal saved ->
      let payload = Protocol.encode (Protocol.Reveal reveal) in
      if Octra_consensus.C_seen.known t.seen (wire_id payload) then saved
      else (reveal.Selection.node_id = t.deps.node_id, payload) :: saved) state.reveals saved) t.offers []
    |> List.sort_uniq (fun (own, left) (other, right) ->
      let order = Bool.compare other own in
      if order = 0 then String.compare left right else order)
    |> List.map snd in
  let live = Hashtbl.create (List.length reveals) in
  List.iter (fun payload -> Hashtbl.replace live (wire_id payload) ()) reveals;
  Hashtbl.filter_map_inplace (fun key deadline ->
    if now >= deadline || not (Hashtbl.mem live key) then None else Some deadline) t.relay_retry;
  match List.filter (fun payload -> not (Hashtbl.mem t.relay_retry (wire_id payload))) reveals with
  | [] -> Lwt.return_unit
  | payload :: _ ->
    let local = match Protocol.decode payload with
      | Protocol.Reveal reveal -> reveal.Selection.node_id = t.deps.node_id
      | _ -> false in
    let now = t.deps.now () in
    let* allowed = use_history t (fun history ->
      Octra_core.Compute_history.claim_send ~local history ~now) in
    if allowed = Ok true then begin
      t.relay_pending <- Some payload;
      spawn_background t "reveal_send" (fun () -> send_reveal t payload)
    end;
    Lwt.return_unit

let handle_actor_message : type reply. t -> reply actor_message -> reply Lwt.t =
  fun t message ->
    match message with
    | Apply_protocol { wire_key; source; message; funds } ->
      prune_work t;
      let epoch = match current_head t with Ok head -> head.epoch_id | Error _ -> Int64.max_int in
      Hashtbl.filter_map_inplace (fun _ ((_, expires) as path) ->
        if expires >= epoch then Some path else None) t.cancel_paths;
      let round_allowed = match message with
        | Protocol.Cancel_round value -> t.deps.cancel_rounds ()
            && (Hashtbl.length t.cancel_paths < 1024 || Hashtbl.mem t.cancel_paths (cancel_route value))
        | Protocol.Cancel_ack ack -> t.deps.cancel_rounds ()
            && (ack.delivery.origin = t.deps.node_id || Hashtbl.mem t.cancel_paths
              (cancel_route ack.delivery))
        | _ -> true in
      let cancelled = match message with
        | Protocol.Call call ->
          session_cancelled t call.request.caller call.request.session_id
        | _ -> false in
      if not round_allowed || cancelled then
        Lwt.return_false
      else if Octra_consensus.C_seen.known t.seen wire_key then
        (match message with
        | Protocol.Cancel _ | Protocol.Cancel_round _ ->
          let+ _ = apply_message_direct t funds message in false
        | _ -> Lwt.return_false)
      else
        let* accepted = apply_message_direct t funds message in
        if not accepted then Lwt.return_false else
        let* allowed = match source, message with
        | _, Protocol.Reveal _ -> Lwt.return_true
        | _, Protocol.Cancel_round _ -> Lwt.return_true
        | Some _, Protocol.Cancel_ack ack when ack.delivery.origin = t.deps.node_id ->
          Lwt.return_true
        | _, Protocol.Cancel cancel when t.deps.cancel_rounds () && not
            (session_cancelled t cancel.caller cancel.session_id) -> Lwt.return_false
        | _ ->
          let claim = match message with
            | Protocol.Cancel_ack ack ->
              if session_cancelled t ack.delivery.cancel.caller ack.delivery.cancel.session_id
              then Octra_core.Compute_history.claim_relay else Octra_core.Compute_history.claim_transit
            | Protocol.Cancel cancel ->
              if session_cancelled t cancel.caller cancel.session_id
              then Octra_core.Compute_history.claim_relay else Octra_core.Compute_history.claim_transit
            | _ -> Octra_core.Compute_history.claim_send ~local:(source = None) in
          let now = t.deps.now () in
          let+ allowed = use_history t (fun history ->
            claim history ~now) in
          allowed = Ok true in
        let* allowed = match allowed, message with
          | true, Protocol.Cancel_round value ->
            let transit = not (session_cancelled t value.cancel.caller value.cancel.session_id) in
            let now = t.deps.now () in
            let+ saved = use_history t (fun history ->
              Octra_core.Compute_history.relay_cancel ~close:false history
                ~now
                ~transit
                ~epoch ~expires:value.expires ~round:value.round ~origin:value.origin
                ~id:(wire_id (Protocol.encode (Protocol.Cancel value.cancel)))
                ~caller:value.cancel.caller ~session:value.cancel.session_id) in
            saved = Ok true
          | _ -> Lwt.return allowed in
        (match allowed, source, message with
        | true, Some peer, Protocol.Cancel_round value when value.origin <> t.deps.node_id ->
          Hashtbl.replace t.cancel_paths (cancel_route value) (peer, value.expires)
        | _ -> ());
        if allowed && (match message with Protocol.Reveal _ -> false | _ -> true) then
          ignore (Octra_consensus.C_seen.remember t.seen wire_key);
        Lwt.return allowed
    | Wake_reveals -> wake_reveals t
    | Sent_reveal (payload, sent) ->
      if t.relay_pending = Some payload then begin
        t.relay_pending <- None;
        if sent then ignore (Octra_consensus.C_seen.remember t.seen (wire_id payload))
        else Hashtbl.replace t.relay_retry (wire_id payload)
          (Int64.add (t.deps.now ()) 3000L)
      end;
      wake_reveals t
    | Check_funding call ->
      prune_work t;
      let key = request_id call in
      if Hashtbl.length t.funding_reads >= max_requests
          || Hashtbl.mem t.funding_reads key
          || Hashtbl.length t.requests >= max_requests && not (Hashtbl.mem t.requests key) then
        Lwt.return_ok None
      else
      let* saved = use_history t (fun history ->
        Octra_core.Compute_history.work history ~caller:call.request.caller ~id:(intent_id call)) in
      let* checked = match saved with
        | Error _ as error -> Lwt.return error
        | Ok (Some (Octra_core.Compute_history.Funding body | Issued body)) ->
          let+ restored = restore_state t ~caller:call.request.caller ~id:(intent_id call) body in
          Result.bind restored (fun state ->
            if state.call = call then Ok state else Error "compute funding call differs")
        | _ -> Lwt.return (Result.map (fun (offer, selection, validators) ->
            request_state t call selection validators (offer_snapshot offer)) (verify_call t call)) in
      begin match checked with
      | Error _ as error -> Lwt.return error
      | Ok state ->
        let* closed = use_history t (fun history ->
          Octra_core.Compute_history.cancelled history
            ~caller:call.request.caller ~session:call.request.session_id) in
        begin match closed, current_head t with
        | Error reason, _ | _, Error reason -> Lwt.return_error reason
        | Ok true, _ -> Lwt.return_error "resource compute session cancelled"
        | Ok false, Ok head when head.epoch_id < call.request.epoch_id
            || head.epoch_id > call.request.expires_epoch ->
          Lwt.return_error "compute funding epoch outside window"
        | Ok false, Ok head ->
          Hashtbl.replace t.requests key state;
          Hashtbl.add t.funding_reads key ();
          Lwt.return_ok (Some head)
        end
      end
    | Drop_funding key ->
      Hashtbl.remove t.funding_reads key;
      Lwt.return_unit
    | Fund_call (call, funds) ->
      if not (funding_matches t call funds) then Lwt.return_ok false
      else if Hashtbl.length t.requests >= max_requests
          && not (Hashtbl.mem t.requests (request_id call)) then
        Lwt.return_ok false
      else begin
        let id = intent_id call in
        let request = call.request in
        let* pending = use_history t (fun history ->
          Octra_core.Compute_history.work history ~caller:request.caller ~id) in
        begin match pending with
        | Ok (Some (Octra_core.Compute_history.Funding body | Issued body)) ->
          let* restored = restore_state t ~caller:request.caller ~id body in
          begin match restored with
          | Error _ as error -> Lwt.return error
          | Ok state when state.call <> call -> Lwt.return_error "compute funded call differs"
          | Ok _ when not (funding_matches t call funds) -> Lwt.return_ok false
          | Ok state ->
            let* released = use_history t (fun history ->
              Octra_core.Compute_history.fund_job history ~epoch:funds.epoch
                ~state_root:funds.state_root ~id ~body funds) in
            begin match released with
            | Ok true ->
              Hashtbl.replace t.requests (request_id call) state;
              Lwt.return_ok true
            | other -> Lwt.return other
            end
          end
        | Error _ as error -> Lwt.return error
        | _ -> Lwt.return_error "compute funded wait absent"
        end
      end
    | Admit_request request ->
      start_job_direct t request
    | Complete_job { job_id; caller } ->
      begin
        match Hashtbl.find_opt t.jobs job_id with
        | Some job when job.caller = caller -> release_job_lease t job
        | _ -> ()
      end;
      Lwt.return_unit
    | Cancel_session cancellation ->
      cancel_session_direct ~send:true t cancellation
    | Poll_cancel target ->
      begin match current_head t with
      | Error _ as error -> Lwt.return error
      | Ok head ->
        let* pruned = prune_jobs t head.epoch_id in
        (match pruned with
        | Error reason -> Log.warn "compute" "event = job_expiry status = unavailable reason = %s" reason
        | Ok _ -> ());
        let* loaded = refresh_cancels t head.epoch_id in
        begin match loaded with
        | Error _ as error -> Lwt.return error
        | Ok () ->
          let now = t.deps.now () in
          let rounds = t.deps.cancel_rounds () in
          let* next = use_history t (fun history ->
            Octra_core.Compute_history.claim_notice ?target ~now
              ~rounds history ~epoch:head.epoch_id) in
          let* () = match next with
            | Ok (Some notice) when notice.round > 1 ->
              begin match Protocol.decode notice.body with
              | Protocol.Cancel cancel ->
                let delivery = Protocol.{cancel; origin = t.deps.node_id; epoch = head.epoch_id;
                  expires = notice.expires; round = notice.round; signature = zero64} in
                let delivery = {delivery with signature = t.deps.sign (Protocol.cancel_round_sign delivery)} in
                let* selected = round_validators ~member:true t delivery in
                let key = Option.bind selected (fun validators ->
                  Octra_consensus.C_types.pubkey_of_addr validators t.deps.node_id) in
                begin match key with
                | None -> Lwt.return_unit
                | Some node_key ->
                let ack = Protocol.{delivery; origin_key = node_key; node_id = t.deps.node_id;
                  node_key; epoch = head.epoch_id; signature = zero64} in
                let ack = {ack with signature = t.deps.sign (Protocol.cancel_ack_sign ack)} in
                let* _ = apply_message_direct t None (Protocol.Cancel_ack ack) in
                Lwt.return_unit
                end
              | _ -> Lwt.return_unit
              end
            | _ -> Lwt.return_unit in
          Lwt.return next
        end
      end
    | Cancel_path delivery ->
      let path = if delivery.origin = t.deps.node_id then None else Hashtbl.find_opt t.cancel_paths
        (cancel_route delivery) in
      Lwt.return (Option.map fst path)
    | Cancel_keys delivery ->
      let* closed = use_history t (fun history ->
        Octra_core.Compute_history.cancelled history ~caller:delivery.cancel.caller
          ~session:delivery.cancel.session_id) in
      if closed <> Ok true then Lwt.return_none else
      let* selected = round_validators ~member:true t delivery in
      let keys = Option.bind selected (fun validators ->
        match Octra_consensus.C_types.pubkey_of_addr validators delivery.origin,
            Octra_consensus.C_types.pubkey_of_addr validators t.deps.node_id with
        | Some origin_key, Some node_key -> Some (origin_key, node_key)
        | _ -> None) in
      if Option.is_none keys then Lwt.return_none else
      let now = t.deps.now () in
      let+ allowed = use_history t (fun history ->
        Octra_core.Compute_history.claim_relay history ~now) in
      if allowed = Ok true then keys else None
    | Read_cancels cancel ->
      begin match current_head t with
      | Error _ as error -> Lwt.return error
      | Ok _ when not (Protocol.cancel_shape (wire_cancel t cancel))
          || not (verify_cancellation t cancel) ->
        Lwt.return (Error "compute cancel read signature refused")
      | Ok _ ->
        let* stored = read_cancel_acks t ~caller:cancel.caller ~session:cancel.session_id in
        Lwt.return (Result.bind stored (function
          | None -> Ok []
          | Some (notice, acks) when notice.body = Protocol.encode (Protocol.Cancel (wire_cancel t cancel)) ->
            Ok acks
          | Some _ -> Error "compute cancel request differs"))
      end
    | Save_result (call, result) ->
      begin match result_state t result with
      | Some state when state.call = call
          && result.Protocol.vote.Certificate.node_id = t.deps.node_id
          && result.vote.request_id = request_id call ->
        let context = Record.encode_read call state.validator_set in
        use_history t (fun history ->
          Octra_core.Compute_history.finish ~context history ~caller:call.Protocol.request.caller
            ~session:call.request.session_id ~sequence:call.request.sequence
            ~request:(request_id call) ~reply:(Protocol.encode (Protocol.Result result)))
      | _ -> Lwt.return (Error "resource compute result identity differs")
      end
    | Issue_call call ->
      prune_work t;
      begin match verify_call t call with
      | Error _ as error -> Lwt.return error
      | Ok (offer, selection, validator_set) ->
        let id = intent_id call in
        let request = call.Protocol.request in
        if not (Hashtbl.mem t.jobs id) then Lwt.return (Error "compute local job absent")
        else if Hashtbl.length t.requests >= max_requests
            && not (Hashtbl.mem t.requests (request_id call)) then
          Lwt.return (Error "resource compute request table full")
        else
          let* previous = use_history t (fun history ->
            Octra_core.Compute_history.work history ~caller:request.caller ~id) in
          let state = {call; selection; validator_set; pinned = true; proof = offer_snapshot offer;
            votes = Hashtbl.create max_committee_members} in
          let record = record_of_state state [] in
          let after = Record.encode record in
          begin match previous with
          | Error _ as error -> Lwt.return error
          | Ok None -> Lwt.return (Error "compute local plan absent")
          | Ok (Some (Octra_core.Compute_history.Funding prior)) when prior = after ->
            Lwt.return (Ok ())
          | Ok (Some (Octra_core.Compute_history.Funding _)) ->
            Lwt.return_error "compute funding call differs"
          | Ok (Some (Octra_core.Compute_history.Issued prior)) when prior = after ->
            Lwt.return (Ok ())
          | Ok (Some (Octra_core.Compute_history.Issued _)) ->
            Lwt.return (Error "compute local call already issued")
          | Ok (Some (Octra_core.Compute_history.Planned body)) ->
            begin match read_plan t ~caller:request.caller ~id body with
            | Error _ as error -> Lwt.return error
            | Ok (saved, _) when saved <> request_of_call call ->
              Lwt.return (Error "compute local intent differs")
            | Ok _ ->
              let* committed = use_history t (fun history ->
                let scope = Octra_core.Compute_history.{sequence = request.sequence;
                  request = request_id call; body = Record.encode_scope call validator_set} in
                Octra_core.Compute_history.wait_job history ~caller:request.caller
                  ~session:request.session_id ~id ~before:body ~body:after ~scope
                  ~room:(Record.job_space record (selected_ids selection))) in
              begin match committed with
              | Error _ as error -> Lwt.return error
              | Ok false -> Lwt.return (Error "compute local plan changed")
              | Ok true -> Lwt.return (Ok ())
              end
            end
          end
      end
    | Save_job (id, response) ->
      begin match Hashtbl.find_opt t.requests (Certificate.request_id response.request) with
      | Some state when intent_id state.call = id ->
        let before = Record.encode (record_of_state state (table_values state.votes)) in
        let results = Record.select response.certificate.votes (table_values state.votes) in
        let votes = Hashtbl.create max_committee_members in
        List.iter (fun result -> Hashtbl.add votes result.Protocol.vote.node_id result) results;
        let state = {state with votes} in
        begin match saved_response state with
        | Ok expected when expected = response ->
          let record = record_of_state state results in
          let* saved = use_history t (fun history ->
            let caller = state.call.request.caller in
            match Octra_core.Compute_history.work history ~caller ~id with
            | Ok (Some (Octra_core.Compute_history.Issued body))
                when body = before ->
              Octra_core.Compute_history.save_job history ~caller
                ~session:state.call.request.session_id ~id ~body:(Record.encode record)
            | Error _ as error -> error
            | _ -> Error "compute saved votes differ") in
          Lwt.return (match saved with
            | Ok true -> Ok ()
            | Ok false -> Error "compute job changed or cancelled"
            | Error _ as error -> error)
        | _ -> Lwt.return (Error "compute job response differs")
        end
      | _ -> Lwt.return (Error "compute job request absent")
      end
    | Complete_background task_id ->
      Hashtbl.remove t.background_tasks task_id;
      Lwt.return_unit
    | Remember_committee lease ->
      remember_committee_direct t lease;
      Lwt.return_unit
    | Query_status read ->
      begin match current_head t with
      | Error reason -> Lwt.return (Error reason)
      | Ok head ->
        if not (Compute_read.verify ~chain_id:t.deps.chain_id
            ~node_id:t.deps.node_id ~epoch:head.epoch_id read) then
          Lwt.return (Error "compute read signature or scope refused")
        else
          let+ result = status_direct t ~caller:read.caller ~id:read.job_id in
          Ok result
      end
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
    | Request_replies request_key ->
      begin match Hashtbl.find_opt t.requests request_key, current_head t with
        | Some state, Ok head when state.call.coordinator = t.deps.node_id
            && head.epoch_id >= state.call.request.epoch_id
            && head.epoch_id <= state.call.request.expires_epoch ->
          begin match Hashtbl.find_opt t.jobs (intent_id state.call) with
          | Some job when Lwt.is_sleeping job.result ->
            let missing = selected_ids state.selection
              |> List.filter (fun target -> not (Hashtbl.mem state.votes target)) in
            if missing = [] then Lwt.return (Ok None)
            else
              let* claimed = use_history t (fun history ->
                Octra_core.Compute_history.claim_fetch history
                  ~caller:state.call.request.caller ~session:state.call.request.session_id
                  ~id:(intent_id state.call) ~limit:Protocol.fetch_rounds) in
              Lwt.return (match claimed with
                | Error _ as error -> error
                | Ok None -> Ok None
                | Ok (Some round) ->
                  let expiry = state.call.request.expires_epoch in
                  let expires_epoch = if Int64.sub expiry head.epoch_id <= Protocol.fetch_span
                    then expiry else Int64.add head.epoch_id Protocol.fetch_span in
                  let reads = List.map (fun target ->
                    let fetch = Protocol.{call = state.call; target; epoch_id = head.epoch_id;
                      expires_epoch; round; signature = zero64} in
                    {fetch with signature = t.deps.sign (Protocol.fetch_sign_bytes fetch)}) missing in
                  Ok (Some (round, reads)))
          | _ -> Lwt.return (Ok None)
          end
        | _ -> Lwt.return (Ok None)
      end
    | Ensure_self_test -> ensure_self_test_direct t
    | Stop ->
      stop_direct t;
      Lwt.finalize
        (fun () -> match t.provider with
          | None -> Lwt.return_unit
          | Some provider -> Engine.stop provider.engine)
        (fun () -> match t.history with
          | Error _ -> Lwt.return_unit
          | Ok history -> Lwt.no_cancel (Lwt_preemptive.detach
              Octra_core.Compute_history.close history))

let take_actor_command t =
  match Queue.take_opt t.control_channel with
  | Some _ as command -> command
  | None -> Queue.take_opt t.data_channel

let actor_turn t () =
  match take_actor_command t with
  | Some (Command command) ->
    Lwt_condition.broadcast t.channel_space ();
    if command.generation <> t.actor_generation then begin
      Lwt.wakeup_later_exn command.reply
        (Failure "resource compute actor generation changed");
      Lwt.return (Octra_consensus.C_loop.Next ())
    end else
      let* result =
        Lwt.catch
          (fun () ->
            let+ value = handle_actor_message t command.message in
            Ok value)
          (fun error -> Lwt.return (Error error))
      in
      begin
        match result with
        | Ok value -> Lwt.wakeup_later command.reply value
        | Error error ->
          Log.warn "compute"
            "event = actor_message status = failed kind = %s reason = %s"
            (actor_message_name command.message)
            (Printexc.to_string error);
          Lwt.wakeup_later_exn command.reply error
      end;
      Lwt.return (Octra_consensus.C_loop.Next ())
  | None when not t.channel_open ->
    Lwt.return Octra_consensus.C_loop.Stop
  | None ->
    let* () = Lwt_condition.wait t.channel_ready in
    Lwt.return (Octra_consensus.C_loop.Next ())

let actor_loop t =
  Octra_consensus.C_loop.run (actor_turn t) ()

let create ~deps ~provider ~history =
  let t = {
    deps;
    provider;
    history;
    seen = Octra_consensus.C_seen.create ~capacity:8192;
    relay_pending = None;
    relay_retry = Hashtbl.create max_offers;
    offers = Hashtbl.create 32;
    requests = Hashtbl.create 64;
    jobs = Hashtbl.create 32;
    job_order = Queue.create ();
    caller_jobs = Hashtbl.create 32;
    cancelled_sessions = Hashtbl.create 64;
    cancel_paths = Hashtbl.create 64;
    committee_leases = Hashtbl.create 64;
    read_budget = Hashtbl.create 64;
    funding_reads = Hashtbl.create 32;
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
  spawn_background t "cancel_send" (fun () ->
    let rec loop () =
      let* () = t.deps.sleep 3.0 in
      if not t.channel_open then Lwt.return_unit else
      let* () = send_cancel t None in
      let* () = forward_reveals t () in
      loop () in
    loop ());
  t

let active_jobs t =
  actor_call t Query_active_jobs

let shutdown t =
  actor_call t Stop