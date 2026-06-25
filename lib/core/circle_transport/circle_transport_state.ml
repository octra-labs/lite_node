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


open Lwt.Syntax

type delivery_key_status = {
  key_id : string option;
  state : Circle_key_state.t;
}

let get_outbox_claim store circle_id intent_id =
  Store_irmin.get_circle_outbox_claim store circle_id intent_id

let get_outbox_claims store circle_id intent_id =
  Store_irmin.list_circle_outbox_claims store circle_id intent_id

let get_outbox_claim_for_relay store circle_id intent_id relay_id =
  Store_irmin.get_circle_outbox_claim_for_relay store circle_id intent_id relay_id

let get_outbox_claim_resolutions store circle_id intent_id =
  Store_irmin.list_circle_outbox_claim_resolutions store circle_id intent_id

let get_outbox_resolution store circle_id intent_id =
  Store_irmin.get_circle_outbox_resolution store circle_id intent_id

let cancelled_relay_ids resolutions =
  resolutions
  |> List.filter_map (fun (resolution : Circles.outbox_resolution) -> resolution.actor_id)
  |> List.sort_uniq String.compare

let active_outbox_claims store circle_id intent_id current_epoch =
  let* claims = get_outbox_claims store circle_id intent_id in
  let* resolutions = get_outbox_claim_resolutions store circle_id intent_id in
  Lwt.return
    (Circle_transport_claim_set.live_claims
       ~current_epoch
       ~cancelled_relays:(cancelled_relay_ids resolutions)
       claims)

let active_outbox_claim store circle_id intent_id current_epoch =
  let* claims = active_outbox_claims store circle_id intent_id current_epoch in
  Lwt.return (Circle_transport_claim_set.primary_active_claim ~current_epoch claims)

let active_outbox_claim_for_relay store circle_id intent_id current_epoch relay_id =
  let* claims = active_outbox_claims store circle_id intent_id current_epoch in
  Lwt.return (Circle_transport_claim_set.find_claim_by_relay claims relay_id)

let claim_ready ~transport_policy claims =
  Circle_transport_topology.claim_ready
    transport_policy
    ~active_claims:claims

let delivery_key_status store circle_id current_epoch (intent : Circles.outbox_intent) =
  match intent.delivery_key_id with
  | None ->
    Lwt.return {
      key_id = None;
      state = Circle_key_state.Live;
    }
  | Some key_id ->
    let* key_policy = Circle_policy_store.load_key_policy store circle_id key_id in
    Lwt.return {
      key_id = Some key_id;
      state = Circle_key_state.classify key_policy (Int64.of_int current_epoch);
    }

let delivery_key_live store circle_id current_epoch intent =
  let* key_status = delivery_key_status store circle_id current_epoch intent in
  Lwt.return (Circle_key_state.live key_status.state, key_status.key_id)

let delivery_key_deliverable_for_deadline store circle_id current_epoch deadline_epoch (intent : Circles.outbox_intent) =
  match intent.delivery_key_id with
  | None ->
    Lwt.return true
  | Some key_id ->
    let* key_policy = Circle_policy_store.load_key_policy store circle_id key_id in
    Lwt.return
      (Circle_key_state.deliverable_by_deadline
         ~current_epoch:(Int64.of_int current_epoch)
         ~deadline_epoch
         key_policy)

let evaluate_outbox
    ~current_epoch
    ~transport_policy
    ~(intent : Circles.outbox_intent)
    ~stored_status
    ~stored_resolution
    ~active_claims
    ~(delivery_key_status : delivery_key_status) =
  let open Circle_transport_resolution in
  let synthetic ?related_key_id reason =
    make
      ?related_key_id
      ~intent_id:intent.intent_id
      ~resolved_epoch:(Int64.of_int current_epoch)
      reason
  in
  match stored_status with
  | Circles.Fulfilled
  | Cancelled ->
    {
      effective_status = stored_status;
      effective_resolution = stored_resolution;
    }
  | _ when Circle_key_state.terminal_for_delivery delivery_key_status.state ->
    let key_reason =
      match Circle_key_resolution.outbox_reason_of_state delivery_key_status.state with
      | Some reason -> reason
      | None -> Circles.Delivery_key_inactive
    in
    {
      effective_status = Circles.Cancelled;
      effective_resolution =
        begin
          match stored_resolution, delivery_key_status.key_id with
          | Some resolution, _ -> Some resolution
          | None, related_key_id -> Some (synthetic ?related_key_id key_reason)
        end;
    }
  | _ when Int64.compare intent.expiry_epoch (Int64.of_int current_epoch) <= 0 ->
    {
      effective_status = Circles.Expired;
      effective_resolution =
        begin
          match stored_resolution with
          | Some resolution -> Some resolution
          | None -> Some (synthetic Circles.Intent_expired)
        end;
    }
  | Circles.Claimed ->
    if active_claims <> [] then
      {
        effective_status = Circles.Claimed;
        effective_resolution = stored_resolution;
      }
    else
      let effective_status =
        Circle_transport_lease.expired_claim_status transport_policy in
      let effective_resolution =
        match effective_status, stored_resolution with
        | Circles.Expired, Some resolution -> Some resolution
        | Circles.Expired, None -> Some (synthetic Circles.Claim_expired)
        | Circles.Cancelled, Some resolution -> Some resolution
        | Circles.Cancelled, None -> Some (synthetic Circles.Claim_set_exhausted)
        | _, resolution -> resolution
      in
      {
        effective_status;
        effective_resolution;
      }
  | other ->
    {
      effective_status = other;
      effective_resolution = stored_resolution;
    }

let effective_outbox_status
    ~current_epoch
    ~transport_policy
    ~intent
    ~stored_status
    ~stored_resolution
    ~active_claims
    ~delivery_key_status =
  (evaluate_outbox
     ~current_epoch
     ~transport_policy
     ~intent
     ~stored_status
     ~stored_resolution
     ~active_claims
     ~delivery_key_status).effective_status