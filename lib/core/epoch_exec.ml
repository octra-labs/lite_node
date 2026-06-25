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


type env = {
  chain_id : string;
  epoch_id : int;
  proposer_addr : string;
  validator_addrs : string list;
  validator_pubkeys : (string * string) list;
  prev_state_root : string;
  epoch_ts : float;
  ready_state_root_at : (int -> string option Lwt.t) option;
  ready_max_lag : int;
}

type tx_reject = {
  tx : Transaction.t;
  error_type : string;
  reason     : string;
}

type artifacts = {
  confirmed : (Transaction.t * int) list;
  rejected : tx_reject list;
  confirmed_fees : Z.t;
  tx_count : int;
}

type exec_result = {
  post_state_root : string;
  artifacts : artifacts;
}

type account_ops = {
  mem : string -> bool;
  find_opt : string -> Ledger_types.account option;
  debit : string -> Z.t -> int -> (unit, string) Stdlib.result;
  credit : string -> Z.t -> (unit, string) Stdlib.result;
  add_account : string -> Z.t -> (unit, string) Stdlib.result;
  add_account_with_pubkey : string -> Z.t -> string -> (unit, string) Stdlib.result;
  register_public_key : string -> string -> unit;
  apply_op01_burn : from:string -> to_:string -> Z.t -> int ->
    (unit, string) Stdlib.result;
}

let ledger_ops (l : Ledger.t) : account_ops = {
  mem = (fun a -> Ledger.mem l a);
  find_opt = (fun a -> Ledger.find_opt l a);
  debit = (fun a amt n -> Ledger.debit l a amt n);
  credit = (fun a amt -> Ledger.credit l a amt);
  add_account = (fun a amt -> Ledger.add_account l a amt);
  add_account_with_pubkey = (fun a amt pk -> Ledger.add_account_with_pubkey l a amt pk);
  register_public_key = (fun a pk -> Ledger.register_public_key l a pk);
  apply_op01_burn = (fun ~from ~to_ amt n ->
    Ledger.apply_op01_burn l ~from ~to_ amt n);
}

let overlay_ops (o : Ledger.Overlay.overlay) : account_ops = {
  mem = (fun a -> Ledger.Overlay.mem o a);
  find_opt = (fun a -> Ledger.Overlay.find_opt o a);
  debit = (fun a amt n -> Ledger.Overlay.debit o a amt n);
  credit = (fun a amt -> Ledger.Overlay.credit o a amt);
  add_account = (fun a amt -> Ledger.Overlay.add_account o a amt);
  add_account_with_pubkey = (fun a amt pk -> Ledger.Overlay.add_account_with_pubkey o a amt pk);
  register_public_key = (fun a pk -> Ledger.Overlay.register_public_key o a pk);
  apply_op01_burn = (fun ~from ~to_:_ _amt _n ->
    ignore from;
    Stdlib.Error "op01_burn not supported via overlay (use live ledger)");
}

type backend = {
  store : Store_irmin.t;
  ledger : Ledger.t;
  ops : account_ops;
  begin_batch : unit -> unit Lwt.t;
  commit_batch: unit -> unit Lwt.t;
  flush_dirty : unit -> unit Lwt.t;
  get_head_hash: unit -> string option Lwt.t;
  set_meta : string -> string -> unit Lwt.t;
}

let make_live_backend store ledger = {
  store;
  ledger;
  ops = ledger_ops ledger;
  begin_batch = (fun () -> Store_irmin.begin_epoch_batch store);
  commit_batch = (fun () -> Store_irmin.commit_epoch_batch store "epoch");
  flush_dirty = (fun () -> Ledger.flush_dirty_lwt ledger);
  get_head_hash = (fun () -> Store_irmin.get_head_hash store);
  set_meta = (fun k v -> Store_irmin.set_meta store k v);
}

let make_overlay_backend store ledger overlay = {
  store;
  ledger;
  ops = overlay_ops overlay;
  begin_batch = (fun () -> Store_irmin.begin_epoch_batch store);
  commit_batch = (fun () -> Store_irmin.commit_epoch_batch store "epoch_overlay");
  flush_dirty = (fun () -> Lwt.return_unit);
  get_head_hash = (fun () -> Store_irmin.get_head_hash store);
  set_meta = (fun k v -> Store_irmin.set_meta store k v);
}

let emission_divisor = Z.of_int 18_198_732
let emission_tail = Z.of_int 10_000

let compute_base_reward ~emission_remaining =
  if Z.leq emission_remaining Z.zero then Z.zero
  else
    let raw = Z.div emission_remaining emission_divisor in
    let reward = Z.max raw emission_tail in
    Z.min reward emission_remaining

type reward_plan = {
  base_reward : Z.t;
  total_reward : Z.t;
  proposer_total : Z.t;
  each_validator : Z.t;
  remainder : Z.t;
  new_emission_remaining : Z.t;
  new_total_supply : Z.t;
}

let build_reward_plan ~validator_count ~emission_remaining ~confirmed_fees ~prev_supply =
  let base_reward = compute_base_reward ~emission_remaining in
  let total_reward = Z.add base_reward confirmed_fees in
  let n = if validator_count <= 0 then 1 else validator_count in
  let proposer_cut = Z.div (Z.mul total_reward (Z.of_int 7000)) (Z.of_int 10000) in
  let validator_pool = Z.sub total_reward proposer_cut in
  let each_validator = Z.div validator_pool (Z.of_int n) in
  let remainder = Z.sub validator_pool (Z.mul each_validator (Z.of_int n)) in
  let proposer_total = Z.add proposer_cut remainder in
  let new_emission_remaining =
    if Z.gt base_reward Z.zero then Z.sub emission_remaining base_reward
    else emission_remaining in
  let new_total_supply =
    if Z.gt base_reward Z.zero then Z.add prev_supply base_reward
    else prev_supply in
  { base_reward; total_reward; proposer_total; each_validator; remainder;
    new_emission_remaining; new_total_supply }

let ensure_reward_account ~backend ~env addr =
  let known_pk = List.assoc_opt addr env.validator_pubkeys in
  match backend.ops.find_opt addr with
  | Some a ->
    (match a.Ledger_types.public_key, known_pk with
     | None, Some pk -> backend.ops.register_public_key addr pk
     | _ -> ());
    Lwt.return_unit
  | None ->
    ignore (
      match known_pk with
      | Some pk -> backend.ops.add_account_with_pubkey addr Z.zero pk
      | None -> backend.ops.add_account addr Z.zero
    );
    Lwt.return_unit

let apply_epoch_footer ~backend ~env ~plan =
  let open Lwt.Syntax in
  let* () =
    if Z.leq plan.total_reward Z.zero then Lwt.return_unit
    else begin
      let reward_addrs =
        List.sort_uniq String.compare (env.proposer_addr :: env.validator_addrs) in
      let* () = Lwt_list.iter_s (ensure_reward_account ~backend ~env) reward_addrs in
      let* () = Lwt_list.iter_s (fun addr ->
        if Z.gt plan.each_validator Z.zero then
          match backend.ops.credit addr plan.each_validator with
          | Ok () -> Lwt.return_unit
          | Error _ -> Lwt.return_unit
        else Lwt.return_unit
      ) env.validator_addrs in
      (match backend.ops.credit env.proposer_addr plan.proposer_total with
       | Ok () -> ()
       | Error _ -> ());
      if Z.gt plan.base_reward Z.zero then begin
        let* () = backend.set_meta "emission_remaining" (Z.to_string plan.new_emission_remaining) in
        let* () = backend.set_meta "total_supply" (Z.to_string plan.new_total_supply) in
        Lwt.return_unit
      end else
        Lwt.return_unit
    end
  in
  let* () = backend.set_meta "last_epoch" (string_of_int env.epoch_id) in
  let* () = backend.set_meta "current_epoch" (string_of_int (env.epoch_id + 1)) in
  Lwt.return_unit

let parse_circle_deploy_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "deploy_circle requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.deploy_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_program_update_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_program_update requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.program_update_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_asset_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_asset_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.asset_put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_asset_put_encrypted_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_asset_put_encrypted requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.encrypted_asset_put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_sealed_slot_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_sealed_slot_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.sealed_slot_put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_slot_policy_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_slot_policy_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.slot_policy_put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_state_descriptor_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_state_descriptor_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.state_descriptor_put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_balance_cell_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_balance_cell_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circle_balance_cell.put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_register_cell_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_register_cell_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circle_register_cell.put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_transport_policy_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_transport_policy_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.transport_policy_put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_hfhe_policy_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_hfhe_policy_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.hfhe_policy_put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_key_policy_put_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_key_policy_put requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.key_policy_put_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_key_grant_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_key_grant requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.key_grant_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_key_extend_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_key_extend requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.key_extend_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_key_revoke_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_key_revoke requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.key_revoke_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_key_erase_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_key_erase requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.key_erase_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_outbox_open_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_outbox_open requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.outbox_open_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_relay_claim_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_relay_claim requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.relay_claim_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_relay_cancel_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_relay_cancel requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.relay_cancel_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let parse_circle_ingress_commit_payload tx =
  match tx.Transaction.message with
  | None -> Stdlib.Error ("malformed_transaction", "circle_ingress_commit requires message payload")
  | Some payload_json ->
    begin
      try
        match Circles.ingress_commit_payload_of_yojson (Yojson.Safe.from_string payload_json) with
        | Ok payload -> Stdlib.Ok payload
        | Error e -> Stdlib.Error ("malformed_transaction", e)
      with e ->
        Stdlib.Error ("malformed_transaction", Printexc.to_string e)
    end

let resolve_encrypted_circle_locator circle_id (payload : Circles.encrypted_asset_put_payload) =
  match payload.Circles.path, payload.Circles.slot_ref, payload.Circles.state_ref with
  | Some _, Some _, _
  | Some _, _, Some _
  | None, Some _, Some _ ->
    Stdlib.Error ("invalid_circle_locator", "provide exactly one of path, slot_ref, or state_ref")
  | None, None, None ->
    Stdlib.Error ("invalid_circle_locator", "path, slot_ref, or state_ref is required")
  | Some raw_path, None, None ->
    begin
      match Circles.path_key_of_raw_path raw_path with
      | Ok (canonical_path, path_key) ->
        Stdlib.Ok (canonical_path, path_key, Circles.resource_key_of_path ~circle_id ~canonical_path, Circles.Path_locator, None)
      | Error e ->
        Stdlib.Error ("invalid_circle_path", e)
    end
  | None, Some raw_slot_ref, None ->
    begin
      match Circles.path_key_of_slot_ref raw_slot_ref with
      | Ok (slot_ref, canonical_path, path_key) ->
        Stdlib.Ok (canonical_path, path_key, Circles.resource_key_of_slot_ref ~circle_id ~slot_ref, Circles.Slot_locator, Some slot_ref)
      | Error e ->
        Stdlib.Error ("invalid_circle_slot_ref", e)
    end
  | None, None, Some raw_state_ref ->
    begin
      match Circles.path_key_of_state_ref raw_state_ref with
      | Ok (_state_ref, canonical_path, path_key) ->
        Stdlib.Ok (canonical_path, path_key, Circles.resource_key_of_state_ref ~circle_id ~state_ref:raw_state_ref, Circles.State_locator, None)
      | Error e ->
        Stdlib.Error ("invalid_circle_state_ref", e)
    end

let resolve_sealed_object_locator circle_id (payload : Circles.sealed_slot_put_payload) =
  match payload.slot_ref, payload.state_ref with
  | Some raw_slot_ref, None ->
    begin
      match Circles.path_key_of_slot_ref raw_slot_ref with
      | Ok (slot_ref, canonical_path, path_key) ->
        Stdlib.Ok (canonical_path, path_key, Circles.resource_key_of_slot_ref ~circle_id ~slot_ref, Circles.Slot_locator, Some slot_ref)
      | Error e ->
        Stdlib.Error ("invalid_circle_slot_ref", e)
    end
  | None, Some raw_state_ref ->
    begin
      match Circles.path_key_of_state_ref raw_state_ref with
      | Ok (state_ref, canonical_path, path_key) ->
        Stdlib.Ok (canonical_path, path_key, Circles.resource_key_of_state_ref ~circle_id ~state_ref, Circles.State_locator, None)
      | Error e ->
        Stdlib.Error ("invalid_circle_state_ref", e)
    end
  | Some _, Some _ ->
    Stdlib.Error ("invalid_circle_locator", "sealed object payload requires exactly one of slot_ref or state_ref")
  | None, None ->
    Stdlib.Error ("invalid_circle_locator", "sealed object payload requires slot_ref or state_ref")

let set_or_clear_slot_policy_key storage_tbl key value_opt =
  match value_opt with
  | Some value -> Hashtbl.replace storage_tbl key value
  | None -> Hashtbl.remove storage_tbl key

let apply_slot_policy_snapshot storage_tbl locator_mode path_key (payload : Circles.slot_policy_put_payload) =
  let delivery_key_key, activate_after_key, expire_after_key, tombstone_key, revoked_key =
    match locator_mode with
    | Circles.State_locator ->
      ( Circles.state_policy_delivery_key_key path_key,
        Circles.state_policy_activate_after_key path_key,
        Circles.state_policy_expire_after_key path_key,
        Circles.state_policy_tombstone_key path_key,
        Circles.state_policy_revoked_key path_key )
    | Circles.Path_locator
    | Circles.Slot_locator ->
      ( Circles.slot_policy_delivery_key_key path_key,
        Circles.slot_policy_activate_after_key path_key,
        Circles.slot_policy_expire_after_key path_key,
        Circles.slot_policy_tombstone_key path_key,
        Circles.slot_policy_revoked_key path_key )
  in
  set_or_clear_slot_policy_key
    storage_tbl
    delivery_key_key
    payload.delivery_key_id;
  set_or_clear_slot_policy_key
    storage_tbl
    activate_after_key
    (Option.map Int64.to_string payload.activate_after_epoch);
  set_or_clear_slot_policy_key
    storage_tbl
    expire_after_key
    (Option.map Int64.to_string payload.expire_after_epoch);
  set_or_clear_slot_policy_key
    storage_tbl
    tombstone_key
    (if payload.tombstone then Some "true" else None);
  set_or_clear_slot_policy_key
    storage_tbl
    revoked_key
    (if payload.revoked then Some "true" else None);
  Hashtbl.remove storage_tbl (Circles.mailbox_activate_after_key path_key);
  Hashtbl.remove storage_tbl (Circles.mailbox_expire_after_key path_key);
  Hashtbl.remove storage_tbl (Circles.mailbox_tombstone_key path_key);
  Hashtbl.remove storage_tbl (Circles.mailbox_revoked_key path_key)

let validate_address_list name values =
  if List.for_all Crypto.Address.is_valid_address values then
    Stdlib.Ok ()
  else
    Stdlib.Error (name, name ^ " must contain valid octra addresses")

let validate_epoch_window name activate_after_epoch expire_after_epoch =
  match activate_after_epoch, expire_after_epoch with
  | Some activate_after_epoch, Some expire_after_epoch ->
    if Int64.compare expire_after_epoch activate_after_epoch <= 0 then
      Stdlib.Error (name, "expire_after_epoch must be greater than activate_after_epoch")
    else
      Stdlib.Ok ()
  | _ ->
    Stdlib.Ok ()

let decode_circle_body_b64 tx =
  match tx.Transaction.encrypted_data with
  | None -> Stdlib.Error ("malformed_transaction", "circle asset body missing")
  | Some body_b64 ->
    if String.length body_b64 > Transaction.circle_asset_max_encrypted_data_len then
      Stdlib.Error ("malformed_transaction", "circle asset body exceeds max encoded size")
    else
      begin
        try
          Stdlib.Ok (body_b64, Base64.decode_exn body_b64)
        with e ->
          Stdlib.Error ("malformed_transaction", Printexc.to_string e)
      end

let debit_fee ~(backend : backend) (tx : Transaction.t) =
  match backend.ops.debit tx.from tx.ou tx.nonce with
  | Ok () -> Stdlib.Ok ()
  | Error err -> Stdlib.Error ("insufficient_balance", err)

let process_circle_deploy_tx ~(backend : backend) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_deploy_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let src = Circle_deploy.Direct { deployer = tx.from; nonce = tx.nonce } in
    let circle_id = Circle_deploy.circle_id_of_source src payload in
    if tx.to_ <> circle_id then
      Lwt.return (Stdlib.Error ("circle_id_mismatch", "recipient does not match derived circle id"))
    else
      let* checked = Circle_deploy.check_available backend.store src payload in
      match checked with
      | Stdlib.Error e ->
        Lwt.return (Stdlib.Error e)
      | Stdlib.Ok prepared ->
        match debit_fee ~backend tx with
        | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
        | Stdlib.Ok () ->
          let* written =
            Circle_deploy.write_prepared backend.store src prepared payload in
          match written with
          | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
          | Stdlib.Ok _ -> Lwt.return (Stdlib.Ok tx.ou)

let process_circle_program_update_tx ~(backend : backend) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_program_update_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can update circle code"))
        else
          begin
            try
              let code_raw = Base64.decode_exn payload.Circles.code_b64 in
              let code_size = Int64.of_int (String.length code_raw) in
              if Int64.compare code_size info.limits.max_wasm_bytes > 0 then
                Lwt.return (Stdlib.Error ("circle_code_too_large", "circle code exceeds declared max_wasm_bytes"))
              else
                let* runtime_ok =
                  match info.runtime with
                  | Circles.Octb ->
                    Lwt.return (Stdlib.Ok ())
                  | Circles.Wasm_v1 ->
                    let* validate_result =
                      Circle_wasm_host.validate payload.code_b64 in
                    begin
                      match validate_result with
                      | Ok _ ->
                        Lwt.return (Stdlib.Ok ())
                      | Error e ->
                        Lwt.return
                          (Stdlib.Error
                             ("circle_runtime_invalid", e))
                    end
                in
                begin
                  match runtime_ok with
                  | Stdlib.Error e ->
                    Lwt.return (Stdlib.Error e)
                  | Stdlib.Ok () ->
                    match debit_fee ~backend tx with
                    | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
                    | Stdlib.Ok () ->
                      let next_version = Int64.succ info.version in
                      let code_hash =
                        if String.length code_raw = 0 then Circles.zero_hash_hex
                        else Circles.sha256_hex code_raw in
                      let* () =
                        Store_irmin.update_circle_program
                          backend.store
                          tx.to_
                          ~version:next_version
                          ~code_hash
                          ~code_b64:payload.code_b64 in
                      Lwt.return (Stdlib.Ok tx.ou)
                end
            with e ->
              Lwt.return (Stdlib.Error ("malformed_transaction", Printexc.to_string e))
          end
    end

let process_circle_asset_put_tx ~(backend : backend) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_asset_put_payload tx, decode_circle_body_b64 tx with
  | Stdlib.Error e, _ -> Lwt.return (Stdlib.Error e)
  | _, Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload, Stdlib.Ok (body_b64, raw_body) ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle assets"))
        else if info.resource_mode = Circles.Sealed_read then
          Lwt.return (Stdlib.Error ("circle_mode_invalid", "sealed_read circles require encrypted asset updates"))
        else
          match Circles.path_key_of_raw_path payload.path with
          | Stdlib.Error e ->
            Lwt.return (Stdlib.Error ("invalid_circle_path", e))
          | Stdlib.Ok (canonical_path, path_key) ->
            let size_bytes = Int64.of_int (String.length raw_body) in
            if Int64.compare size_bytes info.limits.max_assets_bytes > 0 then
              Lwt.return (Stdlib.Error ("circle_asset_too_large", "asset exceeds circle asset limit"))
            else if String.length payload.content_type = 0 then
              Lwt.return (Stdlib.Error ("invalid_circle_content_type", "content_type must not be empty"))
            else
              let* existing_meta_opt = Store_irmin.get_circle_asset_meta backend.store tx.to_ path_key in
              let old_size =
                match existing_meta_opt with
                | Some meta -> meta.size_bytes
                | None -> 0L
              in
              let* usage_bytes = Store_irmin.get_circle_asset_usage_bytes backend.store tx.to_ in
              let next_usage = Int64.add (Int64.sub usage_bytes old_size) size_bytes in
              if Int64.compare next_usage info.limits.max_assets_bytes > 0 then
                Lwt.return (Stdlib.Error ("circle_assets_limit_exceeded", "asset update exceeds circle asset budget"))
              else
                match debit_fee ~backend tx with
                | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
                | Stdlib.Ok () ->
                    let meta = {
                      Circles.path_key;
                      canonical_path;
                      content_type = payload.content_type;
                      encoding = Option.value ~default:"identity" payload.encoding;
                      size_bytes;
                      blob_hash = Circles.asset_blob_hash raw_body;
                    body_mode = Circles.Public_resources;
                    plaintext_hash = Some (Circles.sha256_hex raw_body);
                    key_id = None;
                    padding_class = None;
                    resource_key = Circles.resource_key_of_path ~circle_id:tx.to_ ~canonical_path;
                    locator_mode = Circles.Path_locator;
                    slot_ref = None;
                    activate_after_epoch = None;
                    expire_after_epoch = None;
                    metadata_mode = Circles.Metadata_reveal;
                  } in
                  let* () = Store_irmin.save_circle_asset_meta backend.store tx.to_ meta in
                  let* () = Store_irmin.save_circle_asset_body_b64 backend.store tx.to_ path_key body_b64 in
                  let* () = Store_irmin.save_circle_asset_resource_index backend.store tx.to_ meta.resource_key path_key in
                  let* () = Store_irmin.set_circle_asset_usage_bytes backend.store tx.to_ next_usage in
                  let* assets_root_opt =
                    Store_irmin.get_tree_hash_at_path backend.store ["circles"; tx.to_; "assets"; "by_hash"]
                  in
                  let assets_root = Option.value ~default:Circles.zero_hash_hex assets_root_opt in
                  let* () = Store_irmin.set_circle_assets_root backend.store tx.to_ assets_root in
                  Lwt.return (Stdlib.Ok tx.ou)
    end

let process_circle_asset_put_encrypted_tx ~(backend : backend) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_asset_put_encrypted_payload tx, decode_circle_body_b64 tx with
  | Stdlib.Error e, _ -> Lwt.return (Stdlib.Error e)
  | _, Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload, Stdlib.Ok (ciphertext_b64, ciphertext_raw) ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle assets"))
        else if info.resource_mode <> Circles.Sealed_read then
          Lwt.return (Stdlib.Error ("circle_mode_invalid", "encrypted asset updates require sealed_read circle mode"))
        else
          match resolve_encrypted_circle_locator tx.to_ payload with
          | Stdlib.Error e ->
            Lwt.return (Stdlib.Error e)
          | Stdlib.Ok (canonical_path, path_key, resource_key, locator_mode, slot_ref) ->
            let size_bytes = Int64.of_int (String.length ciphertext_raw) in
            let metadata_mode = Option.value ~default:Circles.Metadata_reveal payload.metadata_mode in
            if Int64.compare size_bytes info.limits.max_assets_bytes > 0 then
              Lwt.return (Stdlib.Error ("circle_asset_too_large", "asset exceeds circle asset limit"))
            else if String.length payload.content_type = 0 then
              Lwt.return (Stdlib.Error ("invalid_circle_content_type", "content_type must not be empty"))
            else if String.length payload.key_id = 0 then
              Lwt.return (Stdlib.Error ("invalid_circle_key_id", "key_id must not be empty"))
            else if String.length payload.plaintext_hash <> 64 then
              Lwt.return (Stdlib.Error ("invalid_circle_plaintext_hash", "plaintext_hash must be a 64-char hex string"))
            else if
              match payload.activate_after_epoch, payload.expire_after_epoch with
              | Some activate_after_epoch, Some expire_after_epoch ->
                Int64.compare expire_after_epoch activate_after_epoch <= 0
              | _ ->
                false
            then
              Lwt.return (Stdlib.Error ("invalid_circle_access_window", "expire_after_epoch must be greater than activate_after_epoch"))
            else
              let* existing_meta_opt = Store_irmin.get_circle_asset_meta backend.store tx.to_ path_key in
              let old_size =
                match existing_meta_opt with
                | Some meta -> meta.size_bytes
                | None -> 0L
              in
              let* usage_bytes = Store_irmin.get_circle_asset_usage_bytes backend.store tx.to_ in
              let next_usage = Int64.add (Int64.sub usage_bytes old_size) size_bytes in
              if Int64.compare next_usage info.limits.max_assets_bytes > 0 then
                Lwt.return (Stdlib.Error ("circle_assets_limit_exceeded", "asset update exceeds circle asset budget"))
              else
                match debit_fee ~backend tx with
                | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
                | Stdlib.Ok () ->
                  let meta = {
                    Circles.path_key;
                    canonical_path;
                    content_type = payload.content_type;
                    encoding = Option.value ~default:"identity" payload.encoding;
                    size_bytes;
                    blob_hash = Circles.asset_blob_hash ciphertext_raw;
                    body_mode = Circles.Sealed_read;
                    plaintext_hash = Some payload.plaintext_hash;
                    key_id = Some payload.key_id;
                    padding_class = payload.padding_class;
                    resource_key;
                    locator_mode;
                    slot_ref;
                    activate_after_epoch = payload.activate_after_epoch;
                    expire_after_epoch = payload.expire_after_epoch;
                    metadata_mode;
                  } in
                  let* () = Store_irmin.save_circle_asset_meta backend.store tx.to_ meta in
                  let* () = Store_irmin.save_circle_asset_ciphertext_b64 backend.store tx.to_ path_key ciphertext_b64 in
                  let* () = Store_irmin.save_circle_asset_resource_index backend.store tx.to_ meta.resource_key path_key in
                  let* () = Store_irmin.set_circle_asset_usage_bytes backend.store tx.to_ next_usage in
                  let* assets_root_opt =
                    Store_irmin.get_tree_hash_at_path backend.store ["circles"; tx.to_; "assets"; "by_hash"]
                  in
                  let assets_root = Option.value ~default:Circles.zero_hash_hex assets_root_opt in
                  let* () = Store_irmin.set_circle_assets_root backend.store tx.to_ assets_root in
                  Lwt.return (Stdlib.Ok tx.ou)
    end

let process_circle_sealed_slot_put_tx ~(backend : backend) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_sealed_slot_put_payload tx, decode_circle_body_b64 tx with
  | Stdlib.Error e, _ -> Lwt.return (Stdlib.Error e)
  | _, Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload, Stdlib.Ok (ciphertext_b64, ciphertext_raw) ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle sealed slots"))
        else if info.resource_mode <> Circles.Sealed_read then
          Lwt.return (Stdlib.Error ("circle_mode_invalid", "sealed slot updates require sealed_read circle mode"))
        else
          match resolve_sealed_object_locator tx.to_ payload with
          | Stdlib.Error e ->
            Lwt.return (Stdlib.Error e)
          | Stdlib.Ok (canonical_path, path_key, resource_key, locator_mode, slot_ref) ->
            let metadata_mode = Option.value ~default:Circles.Metadata_opaque payload.metadata_mode in
            let size_bytes = Int64.of_int (String.length ciphertext_raw) in
            if Int64.compare size_bytes info.limits.max_assets_bytes > 0 then
              Lwt.return (Stdlib.Error ("circle_asset_too_large", "sealed slot payload exceeds circle asset limit"))
            else if String.length payload.content_type = 0 then
              Lwt.return (Stdlib.Error ("invalid_circle_content_type", "content_type must not be empty"))
            else if String.length payload.key_id = 0 then
              Lwt.return (Stdlib.Error ("invalid_circle_key_id", "key_id must not be empty"))
            else if String.length payload.plaintext_hash <> 64 then
              Lwt.return (Stdlib.Error ("invalid_circle_plaintext_hash", "plaintext_hash must be a 64-char hex string"))
            else if
              match payload.activate_after_epoch, payload.expire_after_epoch with
              | Some activate_after_epoch, Some expire_after_epoch ->
                Int64.compare expire_after_epoch activate_after_epoch <= 0
              | _ ->
                false
            then
              Lwt.return (Stdlib.Error ("invalid_circle_access_window", "expire_after_epoch must be greater than activate_after_epoch"))
            else
              let* existing_meta_opt = Store_irmin.get_circle_asset_meta backend.store tx.to_ path_key in
              let old_size =
                match existing_meta_opt with
                | Some meta -> meta.size_bytes
                | None -> 0L
              in
              let* usage_bytes = Store_irmin.get_circle_asset_usage_bytes backend.store tx.to_ in
              let next_usage = Int64.add (Int64.sub usage_bytes old_size) size_bytes in
              if Int64.compare next_usage info.limits.max_assets_bytes > 0 then
                Lwt.return (Stdlib.Error ("circle_assets_limit_exceeded", "sealed slot update exceeds circle asset budget"))
              else
                match debit_fee ~backend tx with
                | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
                | Stdlib.Ok () ->
                  let meta = {
                    Circles.path_key;
                    canonical_path;
                    content_type = payload.content_type;
                    encoding = Option.value ~default:"identity" payload.encoding;
                    size_bytes;
                    blob_hash = Circles.asset_blob_hash ciphertext_raw;
                    body_mode = Circles.Sealed_read;
                    plaintext_hash = Some payload.plaintext_hash;
                    key_id = Some payload.key_id;
                    padding_class = payload.padding_class;
                    resource_key;
                    locator_mode;
                    slot_ref;
                    activate_after_epoch = payload.activate_after_epoch;
                    expire_after_epoch = payload.expire_after_epoch;
                    metadata_mode;
                  } in
                  let* () = Store_irmin.save_circle_asset_meta backend.store tx.to_ meta in
                  let* () = Store_irmin.save_circle_asset_ciphertext_b64 backend.store tx.to_ path_key ciphertext_b64 in
                  let* () = Store_irmin.save_circle_asset_resource_index backend.store tx.to_ meta.resource_key path_key in
                  let* () = Store_irmin.set_circle_asset_usage_bytes backend.store tx.to_ next_usage in
                  let* assets_root_opt =
                    Store_irmin.get_tree_hash_at_path backend.store ["circles"; tx.to_; "assets"; "by_hash"]
                  in
                  let assets_root = Option.value ~default:Circles.zero_hash_hex assets_root_opt in
                  let* () = Store_irmin.set_circle_assets_root backend.store tx.to_ assets_root in
                  Lwt.return (Stdlib.Ok tx.ou)
    end

let process_circle_slot_policy_put_tx ~(backend : backend) (tx : Transaction.t) ~(current_epoch : int) =
  let open Lwt.Syntax in
  ignore current_epoch;
  match parse_circle_slot_policy_put_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle slot policy"))
        else if info.resource_mode <> Circles.Sealed_read then
          Lwt.return (Stdlib.Error ("circle_mode_invalid", "slot policy updates require sealed_read circle mode"))
        else
          match resolve_sealed_object_locator
                  tx.to_
                  {
                    Circles.slot_ref = payload.slot_ref;
                    state_ref = payload.state_ref;
                    content_type = "";
                    encoding = None;
                    key_id = "";
                    plaintext_hash = "";
                    padding_class = None;
                    activate_after_epoch = None;
                    expire_after_epoch = None;
                    metadata_mode = None;
                  } with
          | Stdlib.Error e ->
            Lwt.return (Stdlib.Error e)
          | Stdlib.Ok (_canonical_path, path_key, _resource_key, locator_mode, _slot_ref) ->
            if
              match payload.delivery_key_id with
              | Some key_id -> String.length key_id = 0
              | None -> false
            then
              Lwt.return (Stdlib.Error ("invalid_circle_delivery_key_id", "delivery_key_id must not be empty"))
            else if
              match payload.activate_after_epoch, payload.expire_after_epoch with
              | Some activate_after_epoch, Some expire_after_epoch ->
                Int64.compare expire_after_epoch activate_after_epoch <= 0
              | _ ->
                false
            then
              Lwt.return (Stdlib.Error ("invalid_circle_access_window", "expire_after_epoch must be greater than activate_after_epoch"))
            else
              match debit_fee ~backend tx with
              | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
              | Stdlib.Ok () ->
                let* storage_result = Store_irmin.load_circle_stable_storage backend.store tx.to_ in
                begin
                  match storage_result with
                  | Error e ->
                    Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                  | Ok storage_tbl ->
                    apply_slot_policy_snapshot storage_tbl locator_mode path_key payload;
                  let* save_result =
                    Store_irmin.save_circle_stable_storage_checked
                      backend.store
                      tx.to_
                      info.limits
                      storage_tbl in
                    begin
                      match save_result with
                      | Ok _ ->
                        Lwt.return (Stdlib.Ok tx.ou)
                      | Error e ->
                        Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                    end
                end
    end

let process_circle_state_descriptor_put_tx ~(backend : backend) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_state_descriptor_put_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle state descriptors"))
        else if info.resource_mode <> Circles.Sealed_read then
          Lwt.return (Stdlib.Error ("circle_mode_invalid", "state descriptor updates require sealed_read circle mode"))
        else
          begin
            match Circle_state_descriptor.resolve_put_payload payload with
            | Stdlib.Error e ->
              Lwt.return (Stdlib.Error ("invalid_circle_state_descriptor", e))
            | Stdlib.Ok _ ->
              begin
                match Circles.path_key_of_state_ref payload.state_ref with
                | Stdlib.Error e ->
                  Lwt.return (Stdlib.Error ("invalid_circle_state_ref", e))
                | Stdlib.Ok (_state_ref, _canonical_path, path_key) ->
                  match debit_fee ~backend tx with
                  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
                  | Stdlib.Ok () ->
                    let* storage_result = Store_irmin.load_circle_stable_storage backend.store tx.to_ in
                    begin
                      match storage_result with
                      | Error e ->
                        Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                      | Ok storage_tbl ->
                        Circle_state_descriptor.write_snapshot storage_tbl path_key payload;
                        let* save_result =
                          Store_irmin.save_circle_stable_storage_checked
                            backend.store
                            tx.to_
                            info.limits
                            storage_tbl in
                        begin
                          match save_result with
                          | Ok _ ->
                            Lwt.return (Stdlib.Ok tx.ou)
                          | Error e ->
                            Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                        end
                    end
              end
          end
    end

let process_private_state_cell_write
    ~(backend : backend)
    ~(tx : Transaction.t)
    ~(resolved :
       < state_ref : string
       ; content_type : string
       ; encoding : string option
       ; key_id : string
       ; plaintext_hash : string
       ; padding_class : string option
       ; delivery_key_id : string option
       ; activate_after_epoch : int64 option
       ; expire_after_epoch : int64 option
       ; metadata_mode : Circles.metadata_mode
       ; descriptor : Circle_state_descriptor.t
       >)
    ~write_cell_snapshot =
  let open Lwt.Syntax in
  let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
  match info_opt with
  | None ->
    Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
  | Some info ->
    if info.owner <> tx.from then
      Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle private state cells"))
    else if info.resource_mode <> Circles.Sealed_read then
      Lwt.return (Stdlib.Error ("circle_mode_invalid", "private state cells require sealed_read circle mode"))
    else
      match Circles.path_key_of_state_ref resolved#state_ref with
      | Stdlib.Error e ->
        Lwt.return (Stdlib.Error ("invalid_circle_state_ref", e))
      | Stdlib.Ok (_, canonical_path, path_key) ->
        begin
          match debit_fee ~backend tx with
          | Stdlib.Error e ->
            Lwt.return (Stdlib.Error e)
          | Stdlib.Ok () ->
            let ciphertext_raw =
              match tx.Transaction.encrypted_data with
              | Some body_b64 -> Base64.decode_exn body_b64
              | None -> ""
            in
            let size_bytes = Int64.of_int (String.length ciphertext_raw) in
            let* existing_meta_opt =
              Store_irmin.get_circle_asset_meta backend.store tx.to_ path_key in
            let old_size =
              match existing_meta_opt with
              | Some meta -> meta.size_bytes
              | None -> 0L in
            let* usage_bytes =
              Store_irmin.get_circle_asset_usage_bytes backend.store tx.to_ in
            let next_usage =
              Int64.add (Int64.sub usage_bytes old_size) size_bytes in
            if Int64.compare size_bytes info.limits.max_assets_bytes > 0 then
              Lwt.return (Stdlib.Error ("circle_asset_too_large", "private state cell exceeds circle asset limit"))
            else if Int64.compare next_usage info.limits.max_assets_bytes > 0 then
              Lwt.return (Stdlib.Error ("circle_assets_limit_exceeded", "private state cell exceeds circle asset budget"))
            else
              let meta = {
                Circles.path_key;
                canonical_path;
                content_type = resolved#content_type;
                encoding = Option.value ~default:"identity" resolved#encoding;
                size_bytes;
                blob_hash = Circles.asset_blob_hash ciphertext_raw;
                body_mode = Circles.Sealed_read;
                plaintext_hash = Some resolved#plaintext_hash;
                key_id = Some resolved#key_id;
                padding_class = resolved#padding_class;
                resource_key = Circles.resource_key_of_state_ref ~circle_id:tx.to_ ~state_ref:resolved#state_ref;
                locator_mode = Circles.State_locator;
                slot_ref = None;
                activate_after_epoch = resolved#activate_after_epoch;
                expire_after_epoch = resolved#expire_after_epoch;
                metadata_mode = resolved#metadata_mode;
              } in
              let* storage_result =
                Store_irmin.load_circle_stable_storage backend.store tx.to_ in
              begin
                match storage_result with
                | Error e ->
                  Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                | Ok storage_tbl ->
                  Circle_state_descriptor.write_snapshot
                    storage_tbl
                    path_key
                    (Circle_private_common.descriptor_payload_of_resolved
                       resolved#state_ref
                       resolved#descriptor);
                  apply_slot_policy_snapshot
                    storage_tbl
                    Circles.State_locator
                    path_key
                    (Circle_private_common.state_policy_payload_of_resolved
                       resolved#state_ref
                       resolved#delivery_key_id
                       resolved#activate_after_epoch
                       resolved#expire_after_epoch);
                  write_cell_snapshot storage_tbl path_key;
                  let* save_result =
                    Store_irmin.save_circle_stable_storage_checked
                      backend.store
                      tx.to_
                      info.limits
                      storage_tbl in
                  begin
                    match save_result with
                    | Error e ->
                      Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                    | Ok _ ->
                      let* () = Store_irmin.save_circle_asset_meta backend.store tx.to_ meta in
                      let* () =
                        Store_irmin.save_circle_asset_ciphertext_b64
                          backend.store
                          tx.to_
                          path_key
                          (Option.value ~default:"" tx.Transaction.encrypted_data) in
                      let* () =
                        Store_irmin.save_circle_asset_resource_index
                          backend.store
                          tx.to_
                          meta.resource_key
                          path_key in
                      let* () =
                        Store_irmin.set_circle_asset_usage_bytes
                          backend.store
                          tx.to_
                          next_usage in
                      let* assets_root_opt =
                        Store_irmin.get_tree_hash_at_path
                          backend.store
                          ["circles"; tx.to_; "assets"; "by_hash"] in
                      let assets_root =
                        Option.value ~default:Circles.zero_hash_hex assets_root_opt in
                      let* () =
                        Store_irmin.set_circle_assets_root backend.store tx.to_ assets_root in
                      Lwt.return (Stdlib.Ok tx.ou)
                  end
              end
        end

let process_circle_balance_cell_put_tx ~(backend : backend) (tx : Transaction.t) =
  match parse_circle_balance_cell_put_payload tx, decode_circle_body_b64 tx with
  | Stdlib.Error e, _ -> Lwt.return (Stdlib.Error e)
  | _, Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload, Stdlib.Ok _ ->
    begin
      match Circle_balance_cell.resolve_put_payload payload with
      | Stdlib.Error e ->
        Lwt.return (Stdlib.Error ("invalid_circle_balance_cell", e))
      | Stdlib.Ok request ->
        process_private_state_cell_write
          ~backend
          ~tx
          ~resolved:(object
            method state_ref = request.state_ref
            method content_type = request.content_type
            method encoding = request.encoding
            method key_id = request.key_id
            method plaintext_hash = request.plaintext_hash
            method padding_class = request.padding_class
            method delivery_key_id = request.delivery_key_id
            method activate_after_epoch = request.activate_after_epoch
            method expire_after_epoch = request.expire_after_epoch
            method metadata_mode = request.metadata_mode
            method descriptor = request.descriptor
          end)
          ~write_cell_snapshot:(fun storage_tbl path_key ->
            Circle_balance_cell.write_snapshot storage_tbl path_key request.cell)
    end

let process_circle_register_cell_put_tx ~(backend : backend) (tx : Transaction.t) =
  match parse_circle_register_cell_put_payload tx, decode_circle_body_b64 tx with
  | Stdlib.Error e, _ -> Lwt.return (Stdlib.Error e)
  | _, Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload, Stdlib.Ok _ ->
    begin
      match Circle_register_cell.resolve_put_payload payload with
      | Stdlib.Error e ->
        Lwt.return (Stdlib.Error ("invalid_circle_register_cell", e))
      | Stdlib.Ok request ->
        process_private_state_cell_write
          ~backend
          ~tx
          ~resolved:(object
            method state_ref = request.state_ref
            method content_type = request.content_type
            method encoding = request.encoding
            method key_id = request.key_id
            method plaintext_hash = request.plaintext_hash
            method padding_class = request.padding_class
            method delivery_key_id = request.delivery_key_id
            method activate_after_epoch = request.activate_after_epoch
            method expire_after_epoch = request.expire_after_epoch
            method metadata_mode = request.metadata_mode
            method descriptor = request.descriptor
          end)
          ~write_cell_snapshot:(fun storage_tbl path_key ->
            Circle_register_cell.write_snapshot storage_tbl path_key request.cell)
    end

let process_circle_transport_policy_put_tx ~(backend : backend) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_transport_policy_put_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle transport policy"))
        else
          begin
            match Circle_transport_write.transport_policy_put payload with
            | Stdlib.Error e ->
              Lwt.return (Stdlib.Error e)
            | Stdlib.Ok _plan ->
              match debit_fee ~backend tx with
              | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
              | Stdlib.Ok () ->
                let* storage_result = Store_irmin.load_circle_stable_storage backend.store tx.to_ in
                begin
                  match storage_result with
                  | Error e ->
                    Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                  | Ok storage_tbl ->
                    Circle_transport_policy.write_snapshot storage_tbl payload;
                    let* save_result =
                      Store_irmin.save_circle_stable_storage_checked
                        backend.store
                        tx.to_
                        info.limits
                        storage_tbl in
                    begin
                      match save_result with
                      | Ok _ ->
                        Lwt.return (Stdlib.Ok tx.ou)
                      | Error e ->
                        Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                    end
                end
          end
    end

let process_circle_hfhe_policy_put_tx ~(backend : backend) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_hfhe_policy_put_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        let pk_allowlist = Option.value ~default:[] payload.pk_allowlist in
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle hfhe policy"))
        else
          begin
            match validate_address_list "invalid_circle_hfhe_allowlist" pk_allowlist with
            | Stdlib.Error e ->
              Lwt.return (Stdlib.Error e)
            | Stdlib.Ok () ->
              match debit_fee ~backend tx with
              | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
              | Stdlib.Ok () ->
                let* storage_result = Store_irmin.load_circle_stable_storage backend.store tx.to_ in
                begin
                  match storage_result with
                  | Error e ->
                    Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                  | Ok storage_tbl ->
                    Circle_hfhe_policy.write_snapshot storage_tbl payload;
                    let* save_result =
                      Store_irmin.save_circle_stable_storage_checked
                        backend.store
                        tx.to_
                        info.limits
                        storage_tbl in
                    begin
                      match save_result with
                      | Ok _ ->
                        Lwt.return (Stdlib.Ok tx.ou)
                      | Error e ->
                        Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                    end
                end
          end
    end

let materialize_circle_delivery_key_shutdown
    ~(backend : backend)
    ~(circle_id : string)
    ~(key_id : string)
    ~(current_epoch : int)
    (policy : Circle_key_policy.t) =
  let open Lwt.Syntax in
  match
    Circle_key_resolution.outbox_reason_of_state
      (Circle_key_state.classify policy (Int64.of_int current_epoch))
  with
  | None ->
    Lwt.return_unit
  | Some reason ->
    let resolved_epoch = Int64.of_int current_epoch in
    let* intent_ids =
      Store_irmin.list_circle_outbox_intents_by_delivery_key
        backend.store
        circle_id
        key_id in
    Lwt_list.iter_s
      (fun intent_id ->
        let* intent_opt =
          Store_irmin.get_circle_outbox_intent backend.store circle_id intent_id in
        match intent_opt with
        | None ->
          Lwt.return_unit
        | Some _ ->
          let* status_opt =
            Store_irmin.get_circle_outbox_status backend.store circle_id intent_id in
          let status = Option.value ~default:Circles.Open status_opt in
          begin
            match status with
            | Circles.Fulfilled
            | Cancelled
            | Expired ->
              Lwt.return_unit
            | Open
            | Claimed ->
              let resolution =
                Circle_transport_resolution.make
                  ~intent_id
                  ~resolved_epoch
                  ~related_key_id:key_id
                  reason in
              let* active_claims =
                Circle_transport_state.active_outbox_claims
                  backend.store
                  circle_id
                  intent_id
                  current_epoch in
              let* () =
                Lwt_list.iter_s
                  (fun (claim : Circles.relay_claim) ->
                    Store_irmin.save_circle_outbox_claim_resolution
                      backend.store
                      circle_id
                      claim.relay_id
                      resolution)
                  active_claims in
              let* () =
                Store_irmin.save_circle_outbox_resolution
                  backend.store
                  circle_id
                  resolution in
              Store_irmin.set_circle_outbox_status
                backend.store
                circle_id
                intent_id
                Circles.Cancelled
          end)
      intent_ids

let process_circle_key_policy_put_tx ~(backend : backend) ~(current_epoch : int) (tx : Transaction.t) =
  let open Lwt.Syntax in
  match parse_circle_key_policy_put_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle key policy"))
        else if String.length payload.key_id = 0 then
          Lwt.return (Stdlib.Error ("invalid_circle_key_id", "key_id must not be empty"))
        else
          begin
            match validate_epoch_window "invalid_circle_key_window" payload.activate_after_epoch payload.expire_after_epoch with
            | Stdlib.Error e ->
              Lwt.return (Stdlib.Error e)
            | Stdlib.Ok () ->
              match debit_fee ~backend tx with
              | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
              | Stdlib.Ok () ->
                let* storage_result = Store_irmin.load_circle_stable_storage backend.store tx.to_ in
                begin
                  match storage_result with
                  | Error e ->
                    Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                  | Ok storage_tbl ->
                    Circle_key_policy.write_snapshot storage_tbl payload;
                    let* save_result =
                      Store_irmin.save_circle_stable_storage_checked
                        backend.store
                        tx.to_
                        info.limits
                        storage_tbl in
                    begin
                      match save_result with
                      | Ok _ ->
                        let policy = {
                          Circle_key_policy.activate_after_epoch = payload.activate_after_epoch;
                          expire_after_epoch = payload.expire_after_epoch;
                          revoked = payload.revoked;
                          erased = payload.erased;
                        } in
                        let* () =
                          materialize_circle_delivery_key_shutdown
                            ~backend
                            ~circle_id:tx.to_
                            ~key_id:payload.key_id
                            ~current_epoch
                            policy in
                        Lwt.return (Stdlib.Ok tx.ou)
                      | Error e ->
                        Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
                    end
                end
          end
    end

let load_circle_key_policy store circle_id key_id =
  Circle_policy_store.load_key_policy store circle_id key_id

let save_circle_key_policy_state ~backend ~circle_id ~limits key_id (policy : Circle_key_policy.t) =
  let open Lwt.Syntax in
  let* storage_result = Store_irmin.load_circle_stable_storage backend.store circle_id in
  match storage_result with
  | Error e ->
    Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
  | Ok storage_tbl ->
    Circle_key_policy.write_snapshot storage_tbl (Circle_key_policy.put_payload_of_t key_id policy);
    let* save_result =
      Store_irmin.save_circle_stable_storage_checked
        backend.store
        circle_id
        limits
        storage_tbl in
    begin
      match save_result with
      | Ok _ ->
        Lwt.return (Stdlib.Ok ())
      | Error e ->
        Lwt.return (Stdlib.Error ("circle_stable_storage_invalid", e))
    end

let process_circle_key_transition_tx
    ~(backend : backend)
    ~(current_epoch : int)
    (tx : Transaction.t)
    ~(key_id : string)
    ~(apply :
       current_epoch:Int64.t ->
       Circle_key_policy.t ->
       ((Circle_key_policy.t, string * string) Stdlib.result)) =
  let open Lwt.Syntax in
  let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
  match info_opt with
  | None ->
    Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
  | Some info ->
    if info.owner <> tx.from then
      Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can modify circle key policy"))
    else if String.length key_id = 0 then
      Lwt.return (Stdlib.Error ("invalid_circle_key_id", "key_id must not be empty"))
    else
      match debit_fee ~backend tx with
      | Stdlib.Error e ->
        Lwt.return (Stdlib.Error e)
      | Stdlib.Ok () ->
        let current_epoch_i64 = Int64.of_int current_epoch in
        let* current_policy = load_circle_key_policy backend.store tx.to_ key_id in
        begin
          match apply ~current_epoch:current_epoch_i64 current_policy with
          | Stdlib.Error e ->
            Lwt.return (Stdlib.Error e)
          | Stdlib.Ok next_policy ->
            let* save_result =
              save_circle_key_policy_state
                ~backend
                ~circle_id:tx.to_
                ~limits:info.limits
                key_id
                next_policy in
            begin
              match save_result with
              | Stdlib.Ok () ->
                let* () =
                  materialize_circle_delivery_key_shutdown
                    ~backend
                    ~circle_id:tx.to_
                    ~key_id
                    ~current_epoch
                    next_policy in
                Lwt.return (Stdlib.Ok tx.ou)
              | Stdlib.Error e ->
                Lwt.return (Stdlib.Error e)
            end
        end

let process_circle_key_grant_tx ~(backend : backend) ~(current_epoch : int) (tx : Transaction.t) =
  match parse_circle_key_grant_payload tx with
  | Stdlib.Error e ->
    Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    process_circle_key_transition_tx
      ~backend
      ~current_epoch
      tx
      ~key_id:payload.key_id
      ~apply:(fun ~current_epoch policy ->
        Circle_key_transition.grant ~current_epoch policy payload)

let process_circle_key_extend_tx ~(backend : backend) ~(current_epoch : int) (tx : Transaction.t) =
  match parse_circle_key_extend_payload tx with
  | Stdlib.Error e ->
    Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    process_circle_key_transition_tx
      ~backend
      ~current_epoch
      tx
      ~key_id:payload.key_id
      ~apply:(fun ~current_epoch policy ->
        Circle_key_transition.extend ~current_epoch policy payload)

let process_circle_key_revoke_tx ~(backend : backend) ~(current_epoch : int) (tx : Transaction.t) =
  match parse_circle_key_revoke_payload tx with
  | Stdlib.Error e ->
    Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    process_circle_key_transition_tx
      ~backend
      ~current_epoch
      tx
      ~key_id:payload.key_id
      ~apply:(fun ~current_epoch:_ policy ->
        Circle_key_transition.revoke policy)

let process_circle_key_erase_tx ~(backend : backend) ~(current_epoch : int) (tx : Transaction.t) =
  match parse_circle_key_erase_payload tx with
  | Stdlib.Error e ->
    Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    process_circle_key_transition_tx
      ~backend
      ~current_epoch
      tx
      ~key_id:payload.key_id
      ~apply:(fun ~current_epoch:_ policy ->
        Circle_key_transition.erase policy)

let process_circle_outbox_open_tx ~(backend : backend) (tx : Transaction.t) ~(current_epoch : int) =
  let open Lwt.Syntax in
  match parse_circle_outbox_open_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        if info.owner <> tx.from then
          Lwt.return (Stdlib.Error ("circle_access_denied", "only the owner can open circle outbox intents"))
        else
          match Circle_transport_write.open_envelope ~current_epoch payload with
          | Stdlib.Error e ->
            Lwt.return (Stdlib.Error e)
          | Stdlib.Ok () ->
          let* transport_policy = Circle_policy_store.load_transport_policy backend.store tx.to_ in
          let* delivery_key_deliverable =
            match payload.delivery_key_id with
            | Some key_id ->
              let* key_policy = Circle_policy_store.load_key_policy backend.store tx.to_ key_id in
              Lwt.return
                (Circle_key_state.deliverable_by_deadline
                   ~current_epoch:(Int64.of_int current_epoch)
                   ~deadline_epoch:payload.expiry_epoch
                   key_policy)
            | None ->
              Lwt.return true
          in
          let* existing_intent = Store_irmin.get_circle_outbox_intent backend.store tx.to_ payload.intent_id in
          match
            Circle_transport_write.open_intent
              ~current_epoch
              ~transport_policy
              ~delivery_key_deliverable
              ~existing_intent:(Option.is_some existing_intent)
              payload
          with
          | Stdlib.Error e ->
            Lwt.return (Stdlib.Error e)
          | Stdlib.Ok plan ->
              match debit_fee ~backend tx with
              | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
              | Stdlib.Ok () ->
                let* () = Store_irmin.save_circle_outbox_intent backend.store tx.to_ plan.intent in
                Lwt.return (Stdlib.Ok tx.ou)
    end

let process_circle_relay_claim_tx ~(backend : backend) (tx : Transaction.t) ~(current_epoch : int) =
  let open Lwt.Syntax in
  match parse_circle_relay_claim_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok claim ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some _info ->
        match Circle_transport_write.relay_claim_envelope ~sender:tx.from ~current_epoch claim with
        | Stdlib.Error e ->
          Lwt.return (Stdlib.Error e)
        | Stdlib.Ok () ->
          let* transport_policy = Circle_policy_store.load_transport_policy backend.store tx.to_ in
          if not (Circle_transport_policy.relay_allowed transport_policy claim.relay_id) then
            Lwt.return (Stdlib.Error ("circle_transport_policy_violation", "relay is not allowed by circle transport policy"))
          else if
            match transport_policy.max_claim_window_epochs with
            | Some max_claim_window_epochs ->
              Int64.compare (Int64.sub claim.claim_expiry_epoch claim.claim_epoch) max_claim_window_epochs > 0
            | None ->
              false
          then
            Lwt.return (Stdlib.Error ("circle_transport_policy_violation", "relay claim window exceeds circle transport policy"))
          else
          begin
            match Circle_transport_verify.verify_claim_signature backend.ledger tx.to_ claim with
            | Stdlib.Error e ->
              Lwt.return (Stdlib.Error e)
            | Stdlib.Ok () ->
              let* outbox_intent_opt = Store_irmin.get_circle_outbox_intent backend.store tx.to_ claim.intent_id in
              begin
                match outbox_intent_opt with
                | None ->
                  Lwt.return (Stdlib.Error ("circle_outbox_not_found", "outbox intent does not exist"))
                | Some outbox_intent ->
                  let* status_opt = Store_irmin.get_circle_outbox_status backend.store tx.to_ claim.intent_id in
                  let status = Option.value ~default:Circles.Open status_opt in
                  let* existing_claims = Circle_transport_state.get_outbox_claims backend.store tx.to_ claim.intent_id in
                  let* active_claims = Circle_transport_state.active_outbox_claims backend.store tx.to_ claim.intent_id current_epoch in
                  let* delivery_key_deliverable =
                    Circle_transport_state.delivery_key_deliverable_for_deadline
                      backend.store
                      tx.to_
                      current_epoch
                      claim.claim_expiry_epoch
                      outbox_intent in
                  begin
                    match Circle_transport_write.relay_claim
                      ~current_epoch
                      ~transport_policy
                      ~intent:outbox_intent
                      ~status
                      ~delivery_key_deliverable
                      ~existing_claims
                      ~active_claims
                      claim with
                    | Stdlib.Error e ->
                      Lwt.return (Stdlib.Error e)
                    | Stdlib.Ok plan ->
                      match debit_fee ~backend tx with
                      | Stdlib.Error e ->
                        Lwt.return (Stdlib.Error e)
                      | Stdlib.Ok () ->
                        let* () = Store_irmin.save_circle_outbox_claim backend.store tx.to_ plan.claim in
                        let* () = Store_irmin.set_circle_outbox_status backend.store tx.to_ plan.claim.intent_id plan.status in
                        Lwt.return (Stdlib.Ok tx.ou)
                  end
              end
          end
    end

let process_circle_relay_cancel_tx ~(backend : backend) (tx : Transaction.t) ~(current_epoch : int) =
  let open Lwt.Syntax in
  match parse_circle_relay_cancel_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok cancel ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some _info ->
        match Circle_transport_write.relay_cancel_envelope ~sender:tx.from ~current_epoch cancel with
        | Stdlib.Error e ->
          Lwt.return (Stdlib.Error e)
        | Stdlib.Ok () ->
          begin
            match Circle_transport_verify.verify_cancel_signature backend.ledger tx.to_ cancel with
            | Stdlib.Error e ->
              Lwt.return (Stdlib.Error e)
            | Stdlib.Ok () ->
              let* outbox_intent_opt = Store_irmin.get_circle_outbox_intent backend.store tx.to_ cancel.intent_id in
              begin
                match outbox_intent_opt with
                | None ->
                  Lwt.return (Stdlib.Error ("circle_outbox_not_found", "outbox intent does not exist"))
                | Some outbox_intent ->
                  let* status_opt = Store_irmin.get_circle_outbox_status backend.store tx.to_ cancel.intent_id in
                  let status = Option.value ~default:Circles.Open status_opt in
                  let* existing_claims = Circle_transport_state.get_outbox_claims backend.store tx.to_ cancel.intent_id in
                  let* active_claims = Circle_transport_state.active_outbox_claims backend.store tx.to_ cancel.intent_id current_epoch in
                  let* delivery_key_status =
                    Circle_transport_state.delivery_key_status backend.store tx.to_ current_epoch outbox_intent in
                  begin
                    match Circle_transport_write.relay_cancel
                      ~current_epoch
                      ~intent:outbox_intent
                      ~status
                      ~delivery_key_status
                      ~existing_claims
                      ~active_claims
                      cancel with
                    | Stdlib.Error e ->
                      Lwt.return (Stdlib.Error e)
                    | Stdlib.Ok plan ->
                      match debit_fee ~backend tx with
                      | Stdlib.Error e ->
                        Lwt.return (Stdlib.Error e)
                      | Stdlib.Ok () ->
                        let* () =
                          Store_irmin.save_circle_outbox_claim_resolution backend.store tx.to_ cancel.relay_id plan.claim_resolution in
                        let* () =
                          match plan.outbox_resolution with
                          | Some resolution ->
                            Store_irmin.save_circle_outbox_resolution backend.store tx.to_ resolution
                          | None ->
                            Lwt.return_unit in
                        let* () =
                          Store_irmin.set_circle_outbox_status backend.store tx.to_ cancel.intent_id plan.status in
                        Lwt.return (Stdlib.Ok tx.ou)
                  end
              end
          end
    end

let process_circle_ingress_commit_tx ~(backend : backend) (tx : Transaction.t) ~(current_epoch : int) =
  let open Lwt.Syntax in
  match parse_circle_ingress_commit_payload tx with
  | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
  | Stdlib.Ok payload ->
    let* info_opt = Store_irmin.get_circle_info backend.store tx.to_ in
    begin
      match info_opt with
      | None ->
        Lwt.return (Stdlib.Error ("circle_not_found", "circle does not exist"))
      | Some info ->
        ignore info;
        match Circle_transport_write.ingress_envelope ~sender:tx.from payload with
        | Stdlib.Error e ->
          Lwt.return (Stdlib.Error e)
        | Stdlib.Ok () ->
          begin
            match Circle_transport_verify.verify_ingress_signature backend.ledger tx.to_ payload with
            | Stdlib.Error ("circle_relay_signature_invalid", reason) ->
              Lwt.return (Stdlib.Error ("circle_ingress_signature_invalid", reason))
            | Stdlib.Error e ->
              Lwt.return (Stdlib.Error e)
            | Stdlib.Ok () ->
              let* outbox_intent_opt = Store_irmin.get_circle_outbox_intent backend.store tx.to_ payload.intent_id in
              begin
                match outbox_intent_opt with
                | None ->
                  Lwt.return (Stdlib.Error ("circle_outbox_not_found", "outbox intent does not exist"))
                | Some outbox_intent ->
                  let* status_opt = Store_irmin.get_circle_outbox_status backend.store tx.to_ payload.intent_id in
                  let status = Option.value ~default:Circles.Open status_opt in
                  let* active_claims = Circle_transport_state.active_outbox_claims backend.store tx.to_ payload.intent_id current_epoch in
                  let* transport_policy = Circle_policy_store.load_transport_policy backend.store tx.to_ in
                  let* delivery_key_status =
                    Circle_transport_state.delivery_key_status backend.store tx.to_ current_epoch outbox_intent in
                  begin
                    let* existing_packet = Store_irmin.get_circle_ingress_packet backend.store tx.to_ payload.intent_id in
                    match Circle_transport_write.ingress_commit
                      ~current_epoch
                      ~transport_policy
                      ~intent:outbox_intent
                      ~status
                      ~delivery_key_status
                      ~active_claims
                      ~existing_packet:(Option.is_some existing_packet)
                      payload with
                    | Stdlib.Error e ->
                      Lwt.return (Stdlib.Error e)
                    | Stdlib.Ok plan ->
                      match debit_fee ~backend tx with
                      | Stdlib.Error e -> Lwt.return (Stdlib.Error e)
                      | Stdlib.Ok () ->
                        let* () = Store_irmin.save_circle_ingress_packet backend.store tx.to_ plan.packet in
                        let* () = Store_irmin.save_circle_outbox_resolution backend.store tx.to_ plan.resolution in
                        let* () = Store_irmin.set_circle_outbox_status backend.store tx.to_ payload.intent_id plan.status in
                        Lwt.return (Stdlib.Ok tx.ou)
                  end
              end
          end
    end

let process_validator_set_update_tx ~backend ~env tx =
  let open Lwt.Syntax in
  if not (List.mem tx.Transaction.from env.validator_addrs) then
    Lwt.return (Stdlib.Error ("validator_set_update_rejected", "sender is not an active validator"))
  else if Z.sign tx.Transaction.amount <> 0 then
    Lwt.return (Stdlib.Error ("validator_set_update_rejected", "amount must be zero"))
  else
    match Validator_set_update.of_message tx.Transaction.message with
    | Error e -> Lwt.return (Stdlib.Error ("malformed_transaction", e))
    | Ok update ->
      if Int64.compare update.Validator_set_update.activate_epoch (Int64.of_int env.epoch_id) <= 0 then
        Lwt.return (Stdlib.Error ("validator_set_update_rejected", "activation epoch must be in the future"))
      else
        match backend.ops.debit tx.from tx.ou tx.nonce with
        | Error err -> Lwt.return (Stdlib.Error ("insufficient_balance", err))
        | Ok () ->
          let* () =
            backend.set_meta
              Validator_set_update.pending_meta_key
              (Validator_set_update.to_string update)
          in
          Lwt.return (Stdlib.Ok tx.ou)

let process_validator_ready_tx ~backend ~env tx =
  let open Lwt.Syntax in
  let normalize_ready_state_root s =
    if String.length s = 32 then
      String.concat "" (List.init 32 (fun i ->
        Printf.sprintf "%02x" (Char.code s.[i])))
    else if String.length s >= 64 then
      String.sub s 0 64
    else s
  in
  let current_head = env.epoch_id - 1 in
  let env_prev_state_root = normalize_ready_state_root env.prev_state_root in
  let root_for_ready_head ready_head =
    match env.ready_state_root_at with
    | Some lookup -> lookup ready_head
    | None ->
      if ready_head = current_head then Lwt.return_some env_prev_state_root
      else Lwt.return_none
  in
  if Z.sign tx.Transaction.amount <> 0 then
    Lwt.return (Stdlib.Error ("validator_ready_rejected", "amount must be zero"))
  else
    match Validator_set_update.ready_ext_of_message tx.Transaction.message with
    | Error e -> Lwt.return (Stdlib.Error ("malformed_transaction", e))
    | Ok ready_ext ->
      let ready = ready_ext.Validator_set_update.ready in
      let* pending_opt =
        Store_irmin.get_meta backend.store Validator_set_update.pending_meta_key
      in
      match pending_opt with
      | None -> Lwt.return (Stdlib.Error ("validator_ready_rejected", "no pending validator set update"))
      | Some pending_raw ->
        match Validator_set_update.of_string pending_raw with
        | Error e -> Lwt.return (Stdlib.Error ("validator_ready_rejected", e))
        | Ok update ->
          let in_pending =
            List.exists
              (fun v -> String.equal v.Validator_set_update.address tx.from)
              update.Validator_set_update.validators
          in
          if not in_pending then
            Lwt.return (Stdlib.Error ("validator_ready_rejected", "sender is not in pending validator set"))
          else if ready.Validator_set_update.fingerprint <> update.Validator_set_update.fingerprint then
            Lwt.return (Stdlib.Error ("validator_ready_rejected", "fingerprint does not match pending validator set"))
          else
            let ready_head = Int64.to_int ready.Validator_set_update.head_epoch in
            if ready_head > current_head then
              Lwt.return (Stdlib.Error ("validator_ready_rejected", "head_epoch is in the future"))
            else if env.ready_max_lag >= 0 && current_head - ready_head > env.ready_max_lag then
              Lwt.return (Stdlib.Error ("validator_ready_rejected", "head_epoch too stale"))
            else
              let* expected_root_opt = root_for_ready_head ready_head in
              match expected_root_opt with
              | None ->
                Lwt.return (Stdlib.Error ("validator_ready_rejected", "state_root reference unavailable"))
              | Some expected_root ->
                if normalize_ready_state_root ready.Validator_set_update.state_root
                   <> normalize_ready_state_root expected_root then
                  Lwt.return (Stdlib.Error ("validator_ready_rejected", "state_root does not match referenced chain head"))
                else
                  match backend.ops.debit tx.from tx.ou tx.nonce with
                  | Error err -> Lwt.return (Stdlib.Error ("insufficient_balance", err))
                  | Ok () ->
                    let key =
                      Validator_set_update.ready_meta_key
                        ~fingerprint:update.Validator_set_update.fingerprint
                        ~address:tx.from
                    in
                    let* () =
                      backend.set_meta key (Validator_set_update.ready_ext_to_string ready_ext)
                    in
                    Lwt.return (Stdlib.Ok tx.ou)

let process_circle_operation_tx ~(backend : backend) ~(current_epoch : int) (tx : Transaction.t) =
  let open Transaction in
  match tx.op_type with
  | CircleDeploy ->
    process_circle_deploy_tx ~backend tx
  | CircleProgramUpdate ->
    process_circle_program_update_tx ~backend tx
  | CircleAssetPut ->
    process_circle_asset_put_tx ~backend tx
  | CircleAssetPutEncrypted ->
    process_circle_asset_put_encrypted_tx ~backend tx
  | CircleSealedSlotPut ->
    process_circle_sealed_slot_put_tx ~backend tx
  | CircleSlotPolicyPut ->
    process_circle_slot_policy_put_tx ~backend tx ~current_epoch
  | CircleStateDescriptorPut ->
    process_circle_state_descriptor_put_tx ~backend tx
  | CircleBalanceCellPut ->
    process_circle_balance_cell_put_tx ~backend tx
  | CircleRegisterCellPut ->
    process_circle_register_cell_put_tx ~backend tx
  | CircleTransportPolicyPut ->
    process_circle_transport_policy_put_tx ~backend tx
  | CircleHfhePolicyPut ->
    process_circle_hfhe_policy_put_tx ~backend tx
  | CircleKeyPolicyPut ->
    process_circle_key_policy_put_tx ~backend ~current_epoch tx
  | CircleKeyGrant ->
    process_circle_key_grant_tx ~backend ~current_epoch tx
  | CircleKeyExtend ->
    process_circle_key_extend_tx ~backend ~current_epoch tx
  | CircleKeyRevoke ->
    process_circle_key_revoke_tx ~backend ~current_epoch tx
  | CircleKeyErase ->
    process_circle_key_erase_tx ~backend ~current_epoch tx
  | CircleOutboxOpen ->
    process_circle_outbox_open_tx ~backend tx ~current_epoch
  | CircleRelayClaim ->
    process_circle_relay_claim_tx ~backend tx ~current_epoch
  | CircleRelayCancel ->
    process_circle_relay_cancel_tx ~backend tx ~current_epoch
  | CircleIngressCommit ->
    process_circle_ingress_commit_tx ~backend tx ~current_epoch
  | _ ->
    Lwt.return (Stdlib.Error ("malformed_transaction", "unsupported circle operation"))

let process_standard_tx ~(backend : backend) ~(env : env) (tx : Transaction.t) =
  ignore env;
  let open Transaction in
  match tx.op_type with
  | Standard ->
    let fee = tx.ou in
    let total_cost = Z.add tx.amount fee in
    (match backend.ops.debit tx.from total_cost tx.nonce with
     | Error err ->
       Lwt.return (Stdlib.Error ("insufficient_balance", err))
     | Ok () ->
       if not (backend.ops.mem tx.to_) then
         ignore (backend.ops.add_account tx.to_ Z.zero);
       (match backend.ops.credit tx.to_ tx.amount with
        | Error err ->
          Lwt.return (Stdlib.Error ("supply_violation", err))
        | Ok () ->
          Lwt.return (Stdlib.Ok fee)))
  | ContractDeploy | ContractCall | ProgramExec | MultiExec | ContractUpgrade | CircleCall ->
    let fee = tx.ou in
    (match backend.ops.debit tx.from fee tx.nonce with
     | Error err -> Lwt.return (Stdlib.Error ("insufficient_balance", err))
     | Ok () -> Lwt.return (Stdlib.Ok fee))
  | CircleDeploy | CircleProgramUpdate | CircleAssetPut | CircleAssetPutEncrypted
  | CircleSealedSlotPut | CircleSlotPolicyPut | CircleStateDescriptorPut
  | CircleBalanceCellPut | CircleRegisterCellPut | CircleTransportPolicyPut
  | CircleHfhePolicyPut | CircleKeyPolicyPut | CircleKeyGrant | CircleKeyExtend
  | CircleKeyRevoke | CircleKeyErase | CircleOutboxOpen | CircleRelayClaim
  | CircleRelayCancel | CircleIngressCommit ->
    process_circle_operation_tx ~backend ~current_epoch:env.epoch_id tx
  | ValidatorSetUpdate ->
    process_validator_set_update_tx ~backend ~env tx
  | ValidatorReady ->
    process_validator_ready_tx ~backend ~env tx
  | EncryptOp | DecryptOp ->
    let fee = tx.ou in
    (match backend.ops.debit tx.from fee tx.nonce with
     | Error err -> Lwt.return (Stdlib.Error ("insufficient_balance", err))
     | Ok () -> Lwt.return (Stdlib.Ok fee))
  | PrivateOp | StealthOp | ClaimOp | RecryptOp | KeySwitch ->
    let fee = tx.ou in
    (match backend.ops.debit tx.from fee tx.nonce with
     | Error err -> Lwt.return (Stdlib.Error ("insufficient_balance", err))
     | Ok () -> Lwt.return (Stdlib.Ok fee))
  | Op01Burn ->
    (match backend.ops.apply_op01_burn ~from:tx.from ~to_:tx.to_ tx.amount tx.nonce with
     | Error err -> Lwt.return (Stdlib.Error ("op01_burn_rejected", err))
     | Ok () -> Lwt.return (Stdlib.Ok tx.ou))

let run_core ~preverify ~backend ~env ~(txs : Transaction.t list)
    ~(process_tx : backend:backend -> env:env -> Transaction.t ->
        (Z.t, string * string) Stdlib.result Lwt.t) =
  let open Lwt.Syntax in
  Ledger.clear_spent_nonces backend.ledger;
  let gate_ok =
    match preverify with
    | None -> Ok ()
    | Some gate -> Preverify_commit.check gate txs in
  match gate_ok with
  | Error e -> Lwt.return (Error ("preverify_commit_gate:" ^ e))
  | Ok () ->
  let txs_canonical = List.sort (fun (a : Transaction.t) (b : Transaction.t) ->
    let c = String.compare a.from b.from in
    if c <> 0 then c else compare a.nonce b.nonce
  ) txs in
  let* () = backend.begin_batch () in
  Lwt.catch
    (fun () ->
      let confirmed = ref [] in
      let rejected = ref [] in
      let confirmed_fees = ref Z.zero in
      let pos = ref 0 in
      let* () = Lwt_list.iter_s (fun tx ->
        let* result = process_tx ~backend ~env tx in
        (match result with
         | Ok fee ->
           confirmed := (tx, !pos) :: !confirmed;
           confirmed_fees := Z.add !confirmed_fees fee
         | Error (etype, reason) ->
           rejected := { tx; error_type = etype; reason } :: !rejected);
        incr pos;
        Lwt.return_unit
      ) txs_canonical in
      let* emission_remaining_opt = Store_irmin.get_meta backend.store "emission_remaining" in
      let emission_remaining =
        try Z.of_string (Option.value ~default:"0" emission_remaining_opt)
        with _ -> Z.zero in
      let* prev_supply_opt = Store_irmin.get_meta backend.store "total_supply" in
      let prev_supply =
        try Z.of_string (Option.value ~default:"0" prev_supply_opt)
        with _ -> Z.zero in
      let plan = build_reward_plan
        ~validator_count:(List.length env.validator_addrs)
        ~emission_remaining
        ~confirmed_fees:!confirmed_fees
        ~prev_supply in
      let* () = apply_epoch_footer ~backend ~env ~plan in
      let* () = backend.flush_dirty () in
      if List.length txs > 0 then
        Octra_log.stderr "[pv] ep=%d txs_in=%d conf=%d rej=%d fees=%s base=%s\n%!"
          env.epoch_id (List.length txs) (List.length !confirmed) (List.length !rejected)
          (Z.to_string !confirmed_fees) (Z.to_string plan.base_reward);
      let* batch_h = Store_irmin.get_batch_tree_hash backend.store in
      let post_state_root = match batch_h with
        | Some h -> h
        | None -> "" in
      let* () = backend.commit_batch () in
      Ledger.clear_spent_nonces backend.ledger;
      Lwt.return (Ok {
        post_state_root;
        artifacts = {
          confirmed = List.rev !confirmed;
          rejected = List.rev !rejected;
          confirmed_fees = !confirmed_fees;
          tx_count = List.length txs;
        }
      }))
    (fun exn ->
      Ledger.clear_spent_nonces backend.ledger;
      Lwt.return (Error (Printexc.to_string exn)))

let run ~backend ~env ~(txs : Transaction.t list) ~process_tx =
  run_core ~preverify:None ~backend ~env ~txs ~process_tx

let run_checked ~preverify ~backend ~env ~(txs : Transaction.t list) ~process_tx =
  run_core ~preverify:(Some preverify) ~backend ~env ~txs ~process_tx
