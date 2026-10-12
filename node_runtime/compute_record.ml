(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Protocol = Octra_consensus.Resource_compute_protocol
module Certificate = Octra_consensus.Resource_compute_certificate
module Capability = Octra_consensus.Resource_compute_capability
module Selection = Octra_consensus.Resource_compute_selection
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module Anchor = Octra_bootstrap.Sync_anchor
module Funding = Octra_core.Compute_funding

type t = {
  offer : Protocol.offer;
  capabilities : Capability.t list;
  commitments : Selection.commitment list;
  reveals : Selection.reveal list;
  call : Protocol.call;
  results : Protocol.result list;
  validators : Octra_consensus.C_types.validator_set option;
}

let check_set call validators =
  if Octra_consensus.C_config.validator_set_hash validators
     <> call.Protocol.request.validator_set_root then
    invalid_arg "compute record validator root differs"

let encode_read call validators =
  check_set call validators;
  let raw = Octra_net.Oce1.encode (fun buffer ->
    Octra_net.Oce1.put_hash32 buffer (Protocol.call_sign_bytes call);
    Octra_net.Oce1.put_sig64 buffer call.Protocol.signature;
    Octra_net.Oce1.put_string buffer
      (Octra_consensus.C_codec.encode_validator_set validators)) in
  if String.length raw > Octra_core.Compute_history.max_context_bytes then
    invalid_arg "compute read context size";
  raw

let decode_read call raw =
  if String.length raw > Octra_core.Compute_history.max_context_bytes then
    invalid_arg "compute read context size";
  Octra_net.Oce1.decode (fun cursor ->
    let digest = Octra_net.Oce1.get_hash32 cursor in
    let signature = Octra_net.Oce1.get_sig64 cursor in
    if digest <> Protocol.call_sign_bytes call || signature <> call.Protocol.signature then
      invalid_arg "compute read call differs";
    let validators = Octra_net.Oce1.get_string_bounded
      ~max:Octra_core.Compute_history.max_context_bytes cursor
      |> Octra_consensus.C_codec.decode_validator_set in
    check_set call validators;
    validators) raw

let encode_scope call validators =
  let raw = Octra_net.Oce1.encode (fun buffer ->
    Octra_net.Oce1.put_string buffer (Protocol.encode (Protocol.Call call));
    Octra_net.Oce1.put_string buffer (encode_read call validators)) in
  if String.length raw > Octra_core.Compute_history.max_context_bytes then
    invalid_arg "compute scope size";
  raw

let result_frame call node_id =
  let vote = Octra_consensus.Resource_compute_certificate.{
    request_id = request_id call.Protocol.request;
    node_id; output_hash = String.make 32 '\000'; trace_root = String.make 32 '\000';
    output_bytes = 0; steps = 0L; operations = 0L; signature = String.make 64 '\000'} in
  Protocol.encode (Protocol.Result {vote; output_json = ""})

let reply_space call validators node_id =
  if not (Protocol.call_shape call) then invalid_arg "compute reply request shape";
  let frame = result_frame call node_id in
  let size = String.length frame + call.request.max_output_bytes in
  if size > Octra_core.Compute_history.max_reply_bytes then
    invalid_arg "compute reply exceeds history limit";
  size + String.length (encode_read call validators)

let decode_scope raw =
  if String.length raw > Octra_core.Compute_history.max_context_bytes then
    invalid_arg "compute scope size";
  Octra_net.Oce1.decode (fun cursor ->
    let call = Octra_net.Oce1.get_string_bounded
      ~max:Octra_core.Compute_history.max_context_bytes cursor
      |> Protocol.decode |> function Protocol.Call call -> call
        | _ -> invalid_arg "compute scope call" in
    let context = Octra_net.Oce1.get_string_bounded
      ~max:Octra_core.Compute_history.max_context_bytes cursor in
    call, decode_read call context) raw

let select votes results =
  List.map (fun vote ->
    match List.find_opt (fun result -> result.Protocol.vote = vote) results with
    | Some result -> result
    | None -> invalid_arg "compute record vote absent") votes

let select_cancels ~call ~validators ~members acks =
  check_set call validators;
  List.filter (fun (ack : Protocol.cancel_ack) ->
    List.mem ack.node_id members
    && ack.delivery.cancel.chain_id = call.Protocol.request.chain_id
    && ack.delivery.cancel.caller = call.request.caller
    && ack.delivery.cancel.session_id = call.request.session_id
    && ack.delivery.cancel.caller_public_key = call.caller_public_key
    && (match ack.delivery.cancel.until with None -> true
        | Some until -> call.valid_until = Some until)
    && ack.delivery.origin = call.coordinator
    && Octra_consensus.C_types.pubkey_of_addr validators ack.node_id = Some ack.node_key
    && Octra_consensus.C_types.pubkey_of_addr validators call.coordinator = Some ack.origin_key) acks
  |> List.sort (fun a b -> String.compare a.Protocol.node_id b.Protocol.node_id)

let encode record =
  let put buffer message = Octra_net.Oce1.put_string buffer (Protocol.encode message) in
  let put_list buffer limit values message =
    if List.length values > limit then invalid_arg "compute record count";
    Octra_net.Oce1.put_u16 buffer (List.length values);
    List.iter (fun value -> put buffer (message value)) values in
  let wire = Octra_net.Oce1.encode (fun buffer ->
    put buffer (Protocol.Offer record.offer);
    put_list buffer Protocol.max_records record.capabilities (fun value -> Protocol.Capability value);
    put_list buffer Protocol.max_records record.commitments (fun value -> Protocol.Commitment value);
    put_list buffer Protocol.max_records record.reveals (fun value -> Protocol.Reveal value);
    put buffer (Protocol.Call record.call);
    put_list buffer Protocol.max_members record.results (fun value -> Protocol.Result value);
    Option.iter (fun validators ->
      check_set record.call validators;
      Octra_net.Oce1.put_string buffer "validators";
      Octra_net.Oce1.put_string buffer
        (Octra_consensus.C_codec.encode_validator_set validators)) record.validators) in
  if String.length wire > Octra_core.Compute_history.max_job_bytes then
    invalid_arg "compute record size";
  wire

let job_space record members =
  if not (Protocol.call_shape record.call) || record.results <> []
     || members = [] || List.length members > Protocol.max_members
     || List.length (List.sort_uniq String.compare members) <> List.length members then
    invalid_arg "compute job allocation shape";
  let size = List.fold_left (fun size node ->
    let frame = result_frame record.call node in
    let frame = Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer frame) in
    size + String.length frame + record.call.request.max_output_bytes)
      (String.length (encode record)) members in
  if size > Octra_core.Compute_history.max_job_bytes then
    invalid_arg "compute job exceeds history limit";
  size

let decode wire =
  if String.length wire > Octra_core.Compute_history.max_job_bytes then
    invalid_arg "compute record size";
  Octra_net.Oce1.decode (fun cursor ->
    let get () = Protocol.decode
      (Octra_net.Oce1.get_string_bounded ~max:263_168 cursor) in
    let get_list limit convert =
      let count = Octra_net.Oce1.get_u16 cursor in
      if count > limit then invalid_arg "compute record count";
      List.init count (fun _ -> convert (get ())) in
    let offer = match get () with
      | Protocol.Offer value -> value
      | _ -> invalid_arg "compute record offer" in
    let capabilities = get_list Protocol.max_records (function
      | Protocol.Capability value -> value
      | _ -> invalid_arg "compute record capability") in
    let commitments = get_list Protocol.max_records (function
      | Protocol.Commitment value -> value
      | _ -> invalid_arg "compute record commitment") in
    let reveals = get_list Protocol.max_records (function
      | Protocol.Reveal value -> value
      | _ -> invalid_arg "compute record reveal") in
    let call = match get () with
      | Protocol.Call value -> value
      | _ -> invalid_arg "compute record call" in
    let results = get_list Protocol.max_members (function
      | Protocol.Result value -> value
      | _ -> invalid_arg "compute record result") in
    let validators =
      if Octra_net.Oce1.remaining cursor = 0 then None
      else begin
        if Octra_net.Oce1.get_string_bounded ~max:10 cursor <> "validators" then
          invalid_arg "compute record extension";
        let raw = Octra_net.Oce1.get_string_bounded
          ~max:Octra_core.Compute_history.max_job_bytes cursor in
        let validators = Octra_consensus.C_codec.decode_validator_set raw in
        check_set call validators;
        Some validators
      end in
    {offer; capabilities; commitments; reveals; call; results; validators}) wire

let verify_node validators node message signature =
  match C.pubkey_of_addr validators node with
  | None -> false
  | Some pubkey_raw -> H.verify_ed25519 ~pubkey_raw ~msg:message ~signature

let offer_matches request (offer : Protocol.offer) =
  offer.chain_id = request.Certificate.chain_id
  && offer.validator_set_root = request.validator_set_root
  && offer.circle_id = request.circle_id
  && offer.model_epoch = request.model_epoch
  && offer.model_state_root = request.model_state_root
  && offer.graph_root = request.graph_root && offer.model_root = request.model_root
  && offer.program_root = request.program_root && offer.executor_root = request.executor_root

let capacity_matches request (offer : Protocol.offer) epoch (value : Capability.t) =
  value.chain_id = request.Certificate.chain_id
  && value.validator_set_root = request.validator_set_root
  && value.observed_epoch >= offer.offered_epoch
  && value.observed_epoch <= request.epoch_id && epoch <= value.valid_until
  && value.valid_until <= offer.expires_epoch
  && Int64.sub value.valid_until value.observed_epoch <= Protocol.capacity_ttl
  && value.circle_id = request.circle_id && value.model_epoch = request.model_epoch
  && value.model_state_root = request.model_state_root
  && value.graph_root = request.graph_root && value.model_root = request.model_root
  && value.program_root = request.program_root && value.executor_root = request.executor_root
  && value.memory_bytes >= offer.min_memory_bytes
  && Option.fold ~none:false ~some:(fun size -> Int64.of_int size <= value.memory_bytes)
       request.memory_bytes
  && value.max_request_bytes >= max offer.request_bytes request.input_bytes
  && value.max_response_bytes >= max offer.response_bytes request.max_output_bytes

let verify_selection ~(origin : Octra_bootstrap.Sync_anchor.origin) ~epoch
    ~(header : Protocol.header) ~(offer : Protocol.offer) ~capabilities ~commitments ~reveals =
  let request = header.request in
  let validators = origin.validators in
  let key node message signature = verify_node validators node message signature in
  try
    if List.length capabilities > Protocol.max_records
        || List.length commitments > Protocol.max_records
        || List.length reveals > Protocol.max_records then
      Error "compute proof count exceeds limit"
    else if request.chain_id <> origin.chain_id || request.epoch_id <> origin.epoch_id
        || request.state_root <> origin.state_root
        || request.validator_set_root <> Octra_consensus.C_config.validator_set_hash validators then
      Error "compute proof origin differs"
    else if epoch < request.epoch_id || request.expires_epoch < epoch
        || Int64.sub request.expires_epoch request.epoch_id > Protocol.call_ttl then
      Error "compute proof epoch outside window"
    else if Option.is_none header.valid_until || Option.is_none request.unit_price
        || Option.is_none request.memory_bytes || Option.is_none request.guest_bytes
        || Option.is_none request.fuel_limit then
      Error "compute proof terms missing"
    else if not (Octra_core.Crypto.Address.verify_address_pubkey request.caller
        (Base64.encode_exn header.caller_public_key)
      && H.verify_ed25519 ~pubkey_raw:header.caller_public_key
        ~msg:(Protocol.header_intent header) ~signature:header.caller_signature
      && key header.coordinator (Protocol.header_sign header) header.signature) then
      Error "compute proof call signature refused"
    else if not (Protocol.offer_shape offer && offer_matches request offer
      && offer.offered_epoch <= request.epoch_id && request.epoch_id <= offer.expires_epoch
      && Int64.sub offer.expires_epoch offer.offered_epoch <= Protocol.offer_ttl
      && key offer.coordinator (Protocol.offer_sign_bytes offer) offer.signature) then
      Error "compute proof offer refused"
    else
      let capacity_ok value =
        capacity_matches request offer epoch value
        && key value.Capability.node_id (Capability.sign_bytes value) value.signature in
      if not (List.for_all capacity_ok capabilities) then Error "compute proof capacity refused"
      else
        let id = Protocol.offer_id offer in
        let commitment_ok (value : Selection.commitment) =
          value.chain_id = request.chain_id && value.offer_id = id
          && offer.offered_epoch <= value.commit_epoch && value.commit_epoch < request.epoch_id
          && value.graph_root = request.graph_root && value.model_root = request.model_root
          && value.program_root = request.program_root && value.executor_root = request.executor_root
          && key value.node_id (Selection.commitment_sign_bytes value) value.signature in
        if not (List.for_all commitment_ok commitments) then Error "compute proof commitment refused"
        else
          let evidence_ok (value : Selection.reveal) =
            List.exists (fun (capacity : Capability.t) ->
              capacity.node_id = value.node_id && capacity.evidence_root = value.evidence_root)
              capabilities in
          let reveal_signature commitment (value : Selection.reveal) =
            key value.node_id (Selection.reveal_sign_bytes commitment value) value.signature in
          let reveal_ok (value : Selection.reveal) =
            List.exists (fun commitment -> Result.is_ok (Selection.validate
              ~chain_id:request.chain_id ~target_epoch:request.epoch_id
              ~minimum_delay:Protocol.reveal_delay
              ~verify_commitment_signature:commitment_ok
              ~verify_reveal_signature:reveal_signature ~verify_evidence:evidence_ok
              commitment value)) commitments in
          if not (List.for_all reveal_ok reveals) then Error "compute proof reveal refused"
          else
            let unique values id_of =
              let ids = List.map id_of values in
              List.length (List.sort_uniq String.compare ids) = List.length ids in
            let reveal_id (value : Selection.reveal) = Protocol.encode (Protocol.Reveal value) in
            if not (unique capabilities Capability.id
                && unique commitments Selection.commitment_id && unique reveals reveal_id) then
              Error "compute proof repeated evidence"
            else
              let committee_size = min Protocol.max_members validators.n in
              let challenge = Selection.challenge ~chain_id:request.chain_id
                ~source_epoch:request.epoch_id ~finality_hash:request.state_root in
              let selection = Selection.select ~chain_id:request.chain_id ~offer_id:id
                ~target_epoch:request.epoch_id ~challenge
                ~minimum_delay:Protocol.reveal_delay ~committee_size
                ~verify_commitment_signature:commitment_ok
                ~verify_reveal_signature:reveal_signature ~verify_evidence:evidence_ok
                ~commitments ~reveals in
              if List.length selection.members <> committee_size
                  || selection.root <> request.selection_root then
                Error "compute proof selection differs"
              else Ok selection
  with
  | Invalid_argument _ | Failure _ -> Error "compute proof malformed"

let verify_funding ~origin ~(head : Anchor.origin) ~epoch ~header ~offer
    ~capabilities ~commitments ~reveals ~max_bytes ~ledger_root ~index_root ~proof =
  if max_bytes <= 0 || String.length proof > max_bytes then Error "state proof size exceeds limit"
  else if epoch <> head.epoch_id then Error "compute funding head epoch differs"
  else if origin.Anchor.chain_id <> head.chain_id then Error "compute funding head chain differs"
  else
    match verify_selection ~origin ~epoch ~header ~offer ~capabilities ~commitments ~reveals with
    | Error _ as error -> error
    | Ok selection ->
      let path = Funding.deposit_path (Certificate.request_id header.Protocol.request) in
      match Anchor.read_state ~max_bytes ~origin:head ~ledger_root ~index_root ~path ~proof with
      | Error _ as error -> error
      | Ok None -> Error "compute deposit is absent from state"
      | Ok (Some raw) ->
        Result.map (fun deposit -> selection, deposit)
          (Funding.verify_deposit ~chain_id:head.chain_id ~epoch ~header raw)

let verify_bill ~origin ~epoch ~header ~offer ~capabilities ~commitments ~reveals
    ~votes ~measured =
  match verify_selection ~origin ~epoch ~header ~offer ~capabilities ~commitments ~reveals with
  | Error _ as error -> error
  | Ok selection ->
    let selected = List.map (fun (member : Selection.member) -> member.node_id) selection.members in
    Octra_core.Compute_bill.verify ~epoch ~header ~validators:origin.Anchor.validators
      ~selected ~votes ~measured

module Session : sig
  type record = t
  type t

  val verify :
    ?at:(Protocol.call -> int64) -> epoch:int64 -> model:Anchor.origin -> (Anchor.origin * record) list ->
    (t, string) result
  val calls : t -> Protocol.call list
  val model : t -> Anchor.origin
  val bills : t -> Octra_core.Compute_bill.measured list ->
    (Octra_core.Compute_bill.t list, string) result
end = struct
  type record = t
  type entry = { origin : Anchor.origin; record : record; selected : string list; epoch : int64 }
  type t = { epoch : int64; model : Anchor.origin; entries : entry list }

  let verify ?at ~epoch ~(model : Anchor.origin) records =
    let at = Option.value at ~default:(fun _ -> epoch) in
    let rec check first sequence previous fuel bytes saved = function
      | [] -> Ok {epoch; model; entries = List.rev saved}
      | ((origin : Anchor.origin), record) :: rest ->
        let call = record.call in
        let request = call.request in
        let checked = at call in
        if not (Protocol.call_shape call) then Error "compute session call shape"
        else if checked < request.epoch_id || checked > epoch then Error "compute session payment epoch"
        else if request.sequence <> sequence then Error "compute session sequence differs"
        else if request.epoch_id < previous || request.epoch_id < model.epoch_id then
          Error "compute session epoch order differs"
        else if request.chain_id <> model.chain_id || request.caller <> first.Certificate.caller
            || request.session_id <> first.session_id || request.circle_id <> first.circle_id
            || request.model_epoch <> model.epoch_id || request.model_state_root <> model.state_root
            || request.graph_root <> first.graph_root || request.model_root <> first.model_root
            || request.program_root <> first.program_root || request.executor_root <> first.executor_root then
          Error "compute session model or owner differs"
        else
          let used = Option.value ~default:10_000_001 request.fuel_limit in
          let size = String.length (encode record) in
          if used <= 0 || used > 10_000_000 - fuel then Error "compute session fuel limit"
          else if size > 8_000_000 - bytes then Error "compute session input limit"
          else match Protocol.header call with
          | Error _ as error -> error
          | Ok header ->
            match verify_selection ~origin ~epoch:checked ~header ~offer:record.offer
                ~capabilities:record.capabilities ~commitments:record.commitments ~reveals:record.reveals with
            | Error _ as error -> error
            | Ok selection ->
              let selected = List.map (fun (member : Selection.member) -> member.node_id) selection.members in
              check first (Int64.succ sequence) request.epoch_id (fuel + used) (bytes + size)
                ({origin; record; selected; epoch = checked} :: saved) rest in
    match records with
    | [] -> Error "compute session empty"
    | _ when List.length records > 64 -> Error "compute session step count"
    | (_, first) :: _ ->
      try check first.call.request 1L model.epoch_id 0 0 [] records with
      | Invalid_argument _ | Failure _ -> Error "compute session encoding refused"

  let calls session = List.map (fun entry -> entry.record.call) session.entries
  let model session = session.model

  let bills session measured =
    let rec check saved entries measured = match entries, measured with
      | [], [] -> Ok (List.rev saved)
      | entry :: rest, (measured : Octra_core.Compute_bill.measured) :: remaining ->
        if rest <> [] && measured.outcome <> Octra_core.Compute_bill.Complete then
          Error "compute session prior call stopped"
        else
          let call = entry.record.call in
          (match Protocol.header call with
          | Error _ as error -> error
          | Ok header ->
            let votes = List.map (fun result -> result.Protocol.vote) entry.record.results in
            match Octra_core.Compute_bill.verify ~epoch:entry.epoch ~header
                ~validators:entry.origin.validators ~selected:entry.selected ~votes ~measured with
            | Error _ as error -> error
            | Ok bill -> check (bill :: saved) rest remaining)
      | _ -> Error "compute session measurement count" in
    check [] session.entries measured
end

let verify_funding_lwt ?(cancelled = fun () -> false) ~origin ~head ~epoch ~header ~offer
    ~capabilities ~commitments ~reveals ~max_bytes ~ledger_root ~index_root ~proof () =
  let open Lwt.Syntax in
  if cancelled () then Lwt.return_error "compute funding verification cancelled"
  else if max_bytes <= 0 || String.length proof > max_bytes then
    Lwt.return_error "state proof size exceeds limit"
  else
    let* result = Lwt_preemptive.detach (fun () ->
      verify_funding ~origin ~head ~epoch ~header ~offer ~capabilities ~commitments ~reveals
        ~max_bytes ~ledger_root ~index_root ~proof) () in
    if cancelled () then Lwt.return_error "compute funding verification cancelled"
    else Lwt.return result