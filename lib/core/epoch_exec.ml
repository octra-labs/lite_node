(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type reward_validator = {
  address : string;
  public_key : string option;
  weight : Z.t;
}

type reward_attribution = {
  proposer_addr : string;
  proposer_public_key : string option;
  validators : reward_validator list;
}

type env = {
  chain_id        : string;
  epoch_id        : int;
  proposer_addr   : string;
  validator_addrs : string list;
  validator_pubkeys : (string * string) list;
  prev_state_root : string;
  epoch_ts        : float;
  ready_state_root_at : (int -> string option Lwt.t) option;
  ready_max_lag : int;
}

type tx_reject = {
  tx         : Transaction.t;
  error_type : string;
  reason     : string;
}

type tx_effect =
  | Confirmed of Z.t
  | Rejected_after_fee of {
      fee : Z.t;
      error_type : string;
      reason : string;
    }

type artifacts = {
  confirmed      : (Transaction.t * int) list;
  rejected       : tx_reject list;
  confirmed_fees : Z.t;
  tx_count       : int;
}

type exec_result = {
  post_state_root : string;
  artifacts       : artifacts;
}

type account_ops = {
  mem : string -> bool;
  find_opt : string -> Ledger_types.account option;
  total_supply : unit -> Z.t;
  debit : string -> Z.t -> int -> (unit, string) Stdlib.result;
  debit_amount_only : string -> Z.t -> (unit, string) Stdlib.result;
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
  total_supply = (fun () -> Ledger.get_total_supply l);
  debit = (fun a amt n -> Ledger.debit l a amt n);
  debit_amount_only = (fun a amt -> Ledger.debit_amount_only l a amt);
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
  total_supply = (fun () -> Ledger.Overlay.total_supply o);
  debit = (fun a amt n -> Ledger.Overlay.debit o a amt n);
  debit_amount_only = (fun a amt -> Ledger.Overlay.debit_amount_only o a amt);
  credit = (fun a amt -> Ledger.Overlay.credit o a amt);
  add_account = (fun a amt -> Ledger.Overlay.add_account o a amt);
  add_account_with_pubkey = (fun a amt pk -> Ledger.Overlay.add_account_with_pubkey o a amt pk);
  register_public_key = (fun a pk -> Ledger.Overlay.register_public_key o a pk);
  apply_op01_burn = (fun ~from ~to_ amt nonce ->
    Ledger.Overlay.apply_op01_burn o ~from ~to_ amt nonce);
}

type backend = {
  store       : Store_irmin.t;
  ledger      : Ledger.t;
  ops         : account_ops;
  emission_policy : Emission_policy.t;
  emission_schedule : Emission_schedule.t;
  legacy_total_supply : string option;
  sender_key_activation_epoch : int option;
  validator_policy : Validator_policy.t;
  begin_batch : unit -> unit Lwt.t;
  commit_batch: unit -> unit Lwt.t;
  flush_dirty : unit -> unit Lwt.t;
  get_head_hash: unit -> string option Lwt.t;
  set_meta    : string -> string -> unit Lwt.t;
}

let resolve_legacy_total = function
  | Some _ as value -> value
  | None -> Emission_policy.legacy_total Sys.getenv_opt

let resolve_sender_key_activation = function
  | Some epoch -> Some epoch
  | None -> Sender_key_policy.activation_epoch_exn Sys.getenv_opt

let make_live_backend ?emission_policy ?emission_schedule ?legacy_total_supply
    ?sender_key_activation_epoch ?validator_policy store ledger = {
  store;
  ledger;
  ops = ledger_ops ledger;
  emission_policy = Option.value emission_policy ~default:(Emission_policy.of_env Sys.getenv_opt);
  emission_schedule =
    Option.value
      emission_schedule
      ~default:(Emission_schedule.of_env_exn Sys.getenv_opt);
  legacy_total_supply = resolve_legacy_total legacy_total_supply;
  sender_key_activation_epoch =
    resolve_sender_key_activation sender_key_activation_epoch;
  validator_policy =
    Option.value
      validator_policy
      ~default:(Validator_policy.of_env_exn Sys.getenv_opt);
  begin_batch = (fun () -> Store_irmin.begin_epoch_batch store);
  commit_batch = (fun () -> Store_irmin.commit_epoch_batch store "epoch");
  flush_dirty = (fun () -> Ledger.flush_dirty_lwt ledger);
  get_head_hash = (fun () -> Store_irmin.get_head_hash store);
  set_meta = (fun k v -> Store_irmin.set_meta store k v);
}

let make_overlay_backend ?emission_policy ?emission_schedule
    ?legacy_total_supply ?sender_key_activation_epoch ?validator_policy
    store ledger overlay = {
  store;
  ledger;
  ops = overlay_ops overlay;
  emission_policy = Option.value emission_policy ~default:(Emission_policy.of_env Sys.getenv_opt);
  emission_schedule =
    Option.value
      emission_schedule
      ~default:(Emission_schedule.of_env_exn Sys.getenv_opt);
  legacy_total_supply = resolve_legacy_total legacy_total_supply;
  sender_key_activation_epoch =
    resolve_sender_key_activation sender_key_activation_epoch;
  validator_policy =
    Option.value
      validator_policy
      ~default:(Validator_policy.of_env_exn Sys.getenv_opt);
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
  fees_burned : Z.t;
  fees_rewarded : Z.t;
  total_reward : Z.t;
  proposer_total : Z.t;
  each_validator : Z.t;
  remainder : Z.t;
  new_emission_remaining : Z.t;
  new_total_supply : Z.t;
  new_supply_retired : Z.t;
  supply_tracking_active : bool;
}

let build_reward_plan_with_base ~fee_burn_active ~supply_retired
    ~base_reward ~validator_count ~emission_remaining ~confirmed_fees
    ~prev_supply =
  if validator_count <= 0 then
    Error "reward plan requires active validators"
  else if Z.sign supply_retired < 0 then
    Error "negative retired supply"
  else
    match Fee_policy.split ~active:fee_burn_active confirmed_fees with
    | Error error -> Error error
    | Ok fee_split ->
      match Emission_policy.validate_state
        ~emission_remaining
        ~total_supply:prev_supply with
      | Error error -> Error error
      | Ok headroom ->
      if Z.sign base_reward < 0 then
        Error "negative base reward"
      else if Z.gt base_reward emission_remaining then
        Error "base reward exceeds emission reserve"
      else if Z.gt base_reward headroom then
        Error "base reward exceeds supply headroom"
      else
        let total_reward = Z.add base_reward fee_split.rewarded in
        let n = Z.of_int validator_count in
        let proposer_cut =
          Z.div (Z.mul total_reward (Z.of_int 7000)) (Z.of_int 10000) in
        let validator_pool = Z.sub total_reward proposer_cut in
        let each_validator = Z.div validator_pool n in
        let remainder = Z.sub validator_pool (Z.mul each_validator n) in
        let proposer_total = Z.add proposer_cut remainder in
        let new_emission_remaining = Z.sub emission_remaining base_reward in
        let new_total_supply =
          Z.sub
            (Z.add prev_supply base_reward)
            fee_split.burned
        in
        let new_supply_retired = Z.add supply_retired fee_split.burned in
        if Z.sign new_total_supply < 0 then
          Error "fee burn exceeds total supply"
        else if fee_burn_active
                && not
                  (Z.equal
                     (Z.add
                        new_total_supply
                        (Z.add new_emission_remaining new_supply_retired))
                     Denomination.max_supply) then
          Error "supply envelope transition mismatch"
        else
        Ok {
          base_reward;
          fees_burned = fee_split.burned;
          fees_rewarded = fee_split.rewarded;
          total_reward;
          proposer_total;
          each_validator;
          remainder;
          new_emission_remaining;
          new_total_supply;
          new_supply_retired;
          supply_tracking_active = fee_burn_active;
        }

let build_reward_plan ~fee_burn_active ~supply_retired ~validator_count
    ~emission_remaining ~confirmed_fees ~prev_supply =
  build_reward_plan_with_base
    ~fee_burn_active
    ~supply_retired
    ~base_reward:(compute_base_reward ~emission_remaining)
    ~validator_count
    ~emission_remaining
    ~confirmed_fees
    ~prev_supply

let reward_public_key env (reward : reward_attribution) addr =
  if String.equal addr reward.proposer_addr then
    reward.proposer_public_key
  else
    match
      reward.validators
      |> List.find_opt (fun validator -> String.equal validator.address addr)
    with
    | Some validator ->
      begin
        match validator.public_key with
        | Some _ as public_key -> public_key
        | None -> List.assoc_opt addr env.validator_pubkeys
      end
    | None -> List.assoc_opt addr env.validator_pubkeys

let ensure_reward_account ~backend ~env
    ~(reward : reward_attribution) addr =
  let known_pk = reward_public_key env reward addr in
  match backend.ops.find_opt addr with
  | Some a ->
    (match a.Ledger_types.public_key, known_pk with
     | None, Some pk -> backend.ops.register_public_key addr pk
     | _ -> ());
    Lwt.return_unit
  | None ->
    let result =
      match known_pk with
      | Some pk -> backend.ops.add_account_with_pubkey addr Z.zero pk
      | None -> backend.ops.add_account addr Z.zero
    in
    match result with
    | Ok () -> Lwt.return_unit
    | Error error ->
      Lwt.fail_with (Printf.sprintf "reward account failed addr = %s reason = %s" addr error)

let check_reward_supply backend plan =
  if Z.leq plan.total_reward Z.zero then
    Lwt.return_unit
  else if Z.gt (Z.add (backend.ops.total_supply ()) plan.total_reward)
      Denomination.max_supply then
    Lwt.fail_with "reward supply cap rejected"
  else
    Lwt.return_unit

let credit_reward backend addr amount =
  if Z.leq amount Z.zero then
    Lwt.return_unit
  else
    match backend.ops.credit addr amount with
    | Ok () -> Lwt.return_unit
    | Error error ->
      Lwt.fail_with (Printf.sprintf "reward credit failed addr = %s reason = %s" addr error)

let check_reward_credits backend ~before plan =
  let actual = backend.ops.total_supply () in
  let expected = Z.add before plan.total_reward in
  if Z.equal actual expected then
    Lwt.return_unit
  else
    Lwt.fail_with
      (Printf.sprintf
         "reward credit mismatch expected = %s actual = %s"
         (Z.to_string expected)
         (Z.to_string actual))

type reward_credit = {
  address : string;
  proposer : bool;
  validator : bool;
  amount : Z.t;
}

let canonical_reward_validators (validators : reward_validator list) =
  List.sort
    (fun (left : reward_validator) (right : reward_validator) ->
      String.compare left.address right.address)
    validators

let reward_credits (reward : reward_attribution) plan =
  let validators = canonical_reward_validators reward.validators in
  let addresses =
    List.map
      (fun (validator : reward_validator) -> validator.address)
      validators
  in
  if validators = [] then
    Error "reward distribution requires validators"
  else if addresses <> List.sort_uniq String.compare addresses then
    Error "reward distribution has duplicate validators"
  else if
    List.exists
      (fun (validator : reward_validator) -> Z.sign validator.weight <= 0)
      validators
  then
    Error "reward distribution has non-positive weight"
  else
    let total_weight =
      List.fold_left
        (fun total (validator : reward_validator) ->
          Z.add total validator.weight)
        Z.zero
        validators
    in
    if Z.sign total_weight <= 0 then
      Error "reward distribution total weight is not positive"
    else
      let validator_count = Z.of_int (List.length validators) in
      let validator_pool =
        Z.add
          (Z.mul plan.each_validator validator_count)
          plan.remainder
      in
      let proposer_base = Z.sub plan.proposer_total plan.remainder in
      if Z.sign proposer_base < 0 then
        Error "reward proposer base is negative"
      else
        let shares =
          List.map
            (fun (validator : reward_validator) ->
              validator,
              Z.div
                (Z.mul validator_pool validator.weight)
                total_weight)
            validators
        in
        let distributed =
          List.fold_left
            (fun total (_, amount) -> Z.add total amount)
            Z.zero
            shares
        in
        let proposer_amount =
          Z.add proposer_base (Z.sub validator_pool distributed)
        in
        let table = Hashtbl.create (List.length validators + 1) in
        let add address ~proposer ~validator amount =
          let prior =
            Option.value
              ~default:{
                address;
                proposer = false;
                validator = false;
                amount = Z.zero;
              }
              (Hashtbl.find_opt table address)
          in
          Hashtbl.replace table address {
            address;
            proposer = prior.proposer || proposer;
            validator = prior.validator || validator;
            amount = Z.add prior.amount amount;
          }
        in
        List.iter
          (fun ((validator : reward_validator), amount) ->
            add
              validator.address
              ~proposer:false
              ~validator:true
              amount)
          shares;
        add
          reward.proposer_addr
          ~proposer:true
          ~validator:false
          proposer_amount;
        let credits =
          Hashtbl.to_seq_values table
          |> List.of_seq
          |> List.filter (fun credit -> Z.sign credit.amount > 0)
          |> List.sort (fun left right -> String.compare left.address right.address)
        in
        let credited =
          List.fold_left
            (fun total credit -> Z.add total credit.amount)
            Z.zero
            credits
        in
        if not (Z.equal credited plan.total_reward) then
          Error "reward distribution total mismatch"
        else
          Ok credits

let default_reward (env : env) : reward_attribution =
  let validators =
    match env.validator_addrs with
    | [] -> [env.proposer_addr]
    | values -> values
  in
  {
    proposer_addr = env.proposer_addr;
    proposer_public_key =
      List.assoc_opt env.proposer_addr env.validator_pubkeys;
    validators =
      List.map
        (fun address -> {
          address;
          public_key = List.assoc_opt address env.validator_pubkeys;
          weight = Z.one;
        })
        validators;
  }

let apply_epoch_footer_with_reward ~reward ~backend ~env ~plan =
  let open Lwt.Syntax in
  let* () =
    match Emission_policy.check_reward backend.emission_policy plan.base_reward with
    | Ok () -> Lwt.return_unit
    | Error error -> Lwt.fail_with error
  in
  let* () = check_reward_supply backend plan in
  let supply_before_rewards = backend.ops.total_supply () in
  let* () =
    if Z.leq plan.total_reward Z.zero then Lwt.return_unit
    else begin
      let credits =
        match reward_credits reward plan with
        | Ok credits -> credits
        | Error error -> failwith error
      in
      let reward_addrs =
        List.map (fun credit -> credit.address) credits
      in
      let* () =
        Lwt_list.iter_s
          (ensure_reward_account ~backend ~env ~reward)
          reward_addrs
      in
      let* () =
        Lwt_list.iter_s
          (fun credit -> credit_reward backend credit.address credit.amount)
          credits
      in
      let* () = check_reward_credits backend ~before:supply_before_rewards plan in
      if Z.gt plan.base_reward Z.zero || Z.gt plan.fees_burned Z.zero then begin
        let* () = backend.set_meta "emission_remaining" (Z.to_string plan.new_emission_remaining) in
        let* () = backend.set_meta "total_supply" (Z.to_string plan.new_total_supply) in
        if plan.supply_tracking_active then
          backend.set_meta "supply_retired" (Z.to_string plan.new_supply_retired)
        else
          Lwt.return_unit
      end else
        Lwt.return_unit
    end
  in
  let* () = backend.set_meta "last_epoch" (string_of_int env.epoch_id) in
  let* () = backend.set_meta "current_epoch" (string_of_int (env.epoch_id + 1)) in
  Lwt.return_unit

let apply_epoch_footer ~backend ~env ~plan =
  apply_epoch_footer_with_reward
    ~reward:(default_reward env)
    ~backend
    ~env
    ~plan

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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
        Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
          Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e)
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
              Lwt.return (Stdlib.Error ("malformed_transaction_exception", Printexc.to_string e))
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

let circle_cell_plan ~backend ~current_epoch ~expected_transition_hash tx =
  let open Lwt.Syntax in
  match expected_transition_hash with
  | None -> Lwt.return_error "circle cell preverify receipt is required"
  | Some expected ->
    let* plan =
      Circle_cell_transition.prepare
        ~store:backend.store
        ~ledger:backend.ledger
        ~current_epoch
        tx
    in
    begin
      match plan with
      | Error error -> Lwt.return_error error
      | Ok plan when plan.Circle_cell_transition.transition_hash = expected ->
        Lwt.return_ok plan
      | Ok _ -> Lwt.return_error "circle cell transition hash mismatch"
    end

let process_circle_balance_cell_put_tx
    ~(backend : backend)
    ~current_epoch
    ?expected_transition_hash
    (tx : Transaction.t) =
  let open Lwt.Syntax in
  let* plan =
    circle_cell_plan
      ~backend
      ~current_epoch
      ~expected_transition_hash
      tx
  in
  match plan with
  | Error error ->
    Lwt.return (Stdlib.Error ("invalid_circle_balance_cell", error))
  | Ok { Circle_cell_transition.cell = Register _; _ } ->
    Lwt.return
      (Stdlib.Error
         ("invalid_circle_balance_cell", "circle cell plan type mismatch"))
  | Ok { cell = Balance request; _ } ->
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

let process_circle_register_cell_put_tx
    ~(backend : backend)
    ~current_epoch
    ?expected_transition_hash
    (tx : Transaction.t) =
  let open Lwt.Syntax in
  let* plan =
    circle_cell_plan
      ~backend
      ~current_epoch
      ~expected_transition_hash
      tx
  in
  match plan with
  | Error error ->
    Lwt.return (Stdlib.Error ("invalid_circle_register_cell", error))
  | Ok { Circle_cell_transition.cell = Balance _; _ } ->
    Lwt.return
      (Stdlib.Error
         ("invalid_circle_register_cell", "circle cell plan type mismatch"))
  | Ok { cell = Register request; _ } ->
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

let load_validator_registry backend =
  let open Lwt.Syntax in
  let* stored =
    Store_irmin.get_meta backend.store Validator_registry.meta_key
  in
  match stored with
  | None -> Lwt.return (Ok Validator_registry.empty)
  | Some raw -> Lwt.return (Validator_registry.of_string raw)

let save_validator_registry backend registry =
  backend.set_meta
    Validator_registry.meta_key
    (Validator_registry.to_string registry)

let ensure_validator_escrow backend =
  if backend.ops.mem Validator_registry.escrow_address then
    Ok ()
  else
    backend.ops.add_account Validator_registry.escrow_address Z.zero

let process_validator_bond_tx ~backend ~env tx =
  let open Lwt.Syntax in
  match backend.validator_policy with
  | Validator_policy.Inactive ->
    Lwt.return
      (Stdlib.Error
         ("validator_bond_rejected", "validator admission is inactive"))
  | Validator_policy.Bonded policy ->
    if not (String.equal tx.Transaction.to_ Validator_registry.escrow_address) then
      Lwt.return
        (Stdlib.Error
           ("validator_bond_rejected", "recipient must be validator escrow"))
    else
      match Validator_registry.bond_payload_of_message tx.Transaction.message with
      | Error error ->
        Lwt.return (Stdlib.Error ("malformed_transaction", error))
      | Ok payload ->
        let* registry_result = load_validator_registry backend in
        begin
          match registry_result with
          | Error error ->
            Lwt.return
              (Stdlib.Error ("validator_registry_corrupt", error))
          | Ok registry ->
            begin
              match
                Validator_registry.apply_bond
                  policy.parameters
                  ~chain_id:env.chain_id
                  ~epoch:(Int64.of_int env.epoch_id)
                  ~address:tx.from
                  ~sender_pubkey_b64:
                    (Option.value tx.public_key ~default:"")
                  ~amount:tx.amount
                  ~nonce:tx.nonce
                  payload
                  registry
              with
              | Error error ->
                Lwt.return
                  (Stdlib.Error ("validator_bond_rejected", error))
              | Ok next ->
                let debit = Z.add tx.amount tx.ou in
                begin
                  match backend.ops.debit tx.from debit tx.nonce with
                  | Error error ->
                    Lwt.return
                      (Stdlib.Error ("insufficient_balance", error))
                  | Ok () ->
                    begin
                      match ensure_validator_escrow backend with
                      | Error error ->
                        Lwt.return
                          (Stdlib.Error ("validator_escrow_rejected", error))
                      | Ok () ->
                        begin
                          match
                            backend.ops.credit
                              Validator_registry.escrow_address
                              tx.amount
                          with
                          | Error error ->
                            Lwt.return
                              (Stdlib.Error
                                 ("validator_escrow_rejected", error))
                          | Ok () ->
                            let* () = save_validator_registry backend next in
                            Lwt.return (Stdlib.Ok tx.ou)
                        end
                    end
                end
            end
        end

let process_validator_exit_tx ~backend ~env tx =
  let open Lwt.Syntax in
  match backend.validator_policy with
  | Validator_policy.Inactive ->
    Lwt.return
      (Stdlib.Error
         ("validator_exit_rejected", "validator admission is inactive"))
  | Validator_policy.Bonded _ ->
    if Z.sign tx.Transaction.amount <> 0 then
      Lwt.return
        (Stdlib.Error
           ("validator_exit_rejected", "amount must be zero"))
    else if not (String.equal tx.from tx.to_) then
      Lwt.return
        (Stdlib.Error
           ("validator_exit_rejected", "validator exit must be self-directed"))
    else
      let* registry_result = load_validator_registry backend in
      begin
        match registry_result with
        | Error error ->
          Lwt.return (Stdlib.Error ("validator_registry_corrupt", error))
        | Ok registry ->
          begin
            match
              Validator_registry.request_exit
                ~epoch:(Int64.of_int env.epoch_id)
                ~address:tx.from
                registry
            with
            | Error error ->
              Lwt.return
                (Stdlib.Error ("validator_exit_rejected", error))
            | Ok next ->
              begin
                match backend.ops.debit tx.from tx.ou tx.nonce with
                | Error error ->
                  Lwt.return
                    (Stdlib.Error ("insufficient_balance", error))
                | Ok () ->
                  let* () = save_validator_registry backend next in
                  Lwt.return (Stdlib.Ok tx.ou)
              end
          end
      end

let process_validator_withdraw_tx ~backend ~env tx =
  let open Lwt.Syntax in
  match backend.validator_policy with
  | Validator_policy.Inactive ->
    Lwt.return
      (Stdlib.Error
         ("validator_withdraw_rejected", "validator admission is inactive"))
  | Validator_policy.Bonded policy ->
    if Z.sign tx.Transaction.amount <> 0 then
      Lwt.return
        (Stdlib.Error
           ("validator_withdraw_rejected", "amount must be zero"))
    else if not (String.equal tx.from tx.to_) then
      Lwt.return
        (Stdlib.Error
           ("validator_withdraw_rejected",
            "validator withdrawal must be self-directed"))
    else
      let* registry_result = load_validator_registry backend in
      begin
        match registry_result with
        | Error error ->
          Lwt.return (Stdlib.Error ("validator_registry_corrupt", error))
        | Ok registry ->
          begin
            match
              Validator_registry.withdraw
                policy.parameters
                ~current_epoch:(Int64.of_int env.epoch_id)
                ~active_addresses:env.validator_addrs
                ~address:tx.from
                registry
            with
            | Error error ->
              Lwt.return
                (Stdlib.Error ("validator_withdraw_rejected", error))
            | Ok (next, amount) ->
              begin
                match backend.ops.debit tx.from tx.ou tx.nonce with
                | Error error ->
                  Lwt.return
                    (Stdlib.Error ("insufficient_balance", error))
                | Ok () ->
                  begin
                    match
                      backend.ops.debit_amount_only
                        Validator_registry.escrow_address
                        amount
                    with
                    | Error error ->
                      Lwt.return
                        (Stdlib.Error ("validator_escrow_rejected", error))
                    | Ok () ->
                      begin
                        match backend.ops.credit tx.from amount with
                        | Error error ->
                          Lwt.return
                            (Stdlib.Error
                               ("validator_withdraw_rejected", error))
                        | Ok () ->
                          let* () = save_validator_registry backend next in
                          Lwt.return (Stdlib.Ok tx.ou)
                      end
                  end
              end
          end
      end

let parse_nonnegative_meta label = function
  | None -> Error (label ^ " is unavailable")
  | Some raw ->
    begin
      try
        let value = Z.of_string raw in
        if Z.sign value < 0 then Error (label ^ " is negative")
        else Ok value
      with _ ->
        Error ("invalid " ^ label)
    end

let load_slash_supply backend =
  let open Lwt.Syntax in
  let* total =
    Store_irmin.get_meta backend.store "total_supply"
  in
  let* remaining =
    Store_irmin.get_meta backend.store "emission_remaining"
  in
  let* retired =
    Store_irmin.get_meta
      backend.store
      Emission_schedule.retired_key
  in
  match
    parse_nonnegative_meta "total supply" total,
    parse_nonnegative_meta "emission remaining" remaining,
    parse_nonnegative_meta "retired supply" retired
  with
  | Ok total, Ok remaining, Ok retired ->
    if
      Z.equal
        (Z.add total (Z.add remaining retired))
        Denomination.max_supply
    then
      Lwt.return (Ok (total, retired))
    else
      Lwt.return (Error "validator slash supply envelope mismatch")
  | Error error, _, _
  | _, Error error, _
  | _, _, Error error -> Lwt.return (Error error)

let process_validator_evidence_tx ~backend ~env tx =
  let open Lwt.Syntax in
  match backend.validator_policy with
  | Validator_policy.Inactive ->
    Lwt.return
      (Stdlib.Error
         ("validator_evidence_rejected", "validator admission is inactive"))
  | Validator_policy.Bonded policy ->
    let current_epoch = Int64.of_int env.epoch_id in
    if env.epoch_id < policy.activation_epoch then
      Lwt.return
        (Stdlib.Error
           ("validator_evidence_rejected",
            "validator admission is not active"))
    else if Z.sign tx.Transaction.amount <> 0 then
      Lwt.return
        (Stdlib.Error
           ("validator_evidence_rejected", "amount must be zero"))
    else
      match Validator_evidence.proof_of_message tx.Transaction.message with
      | Error error ->
        Lwt.return (Stdlib.Error ("malformed_transaction", error))
      | Ok proof ->
        let* registry_result = load_validator_registry backend in
        begin
          match registry_result with
          | Error error ->
            Lwt.return
              (Stdlib.Error ("validator_registry_corrupt", error))
          | Ok registry ->
            begin
              match Validator_registry.find tx.to_ registry with
              | None ->
                Lwt.return
                  (Stdlib.Error
                     ("validator_evidence_rejected",
                      "validator bond not found"))
              | Some candidate ->
                begin
                  match
                    Validator_evidence.verify
                      ~chain_id:env.chain_id
                      ~current_epoch
                      ~evidence_epochs:policy.evidence_epochs
                      ~bonded_epoch:
                        candidate.Validator_admission.bonded_epoch
                      ~address:tx.to_
                      ~pubkey:candidate.Validator_admission.pubkey
                      proof
                  with
                  | Error error ->
                    Lwt.return
                      (Stdlib.Error
                         ("validator_evidence_rejected", error))
                  | Ok evidence ->
                    begin
                      match
                        Validator_registry.apply_slash
                          ~current_epoch
                          ~evidence_epochs:policy.evidence_epochs
                          evidence
                          registry
                      with
                      | Error error ->
                        Lwt.return
                          (Stdlib.Error
                             ("validator_evidence_rejected", error))
                      | Ok (next, amount) ->
                        let* supply = load_slash_supply backend in
                        begin
                          match supply with
                          | Error error ->
                            Lwt.return
                              (Stdlib.Error
                                 ("validator_evidence_rejected", error))
                          | Ok (total, retired) ->
                            if Z.lt total amount then
                              Lwt.return
                                (Stdlib.Error
                                   ("validator_evidence_rejected",
                                    "validator slash exceeds total supply"))
                            else
                              begin
                                match
                                  backend.ops.debit
                                    tx.from
                                    tx.ou
                                    tx.nonce
                                with
                                | Error error ->
                                  Lwt.return
                                    (Stdlib.Error
                                       ("insufficient_balance", error))
                                | Ok () ->
                                  begin
                                    match
                                      backend.ops.debit_amount_only
                                        Validator_registry.escrow_address
                                        amount
                                    with
                                    | Error error ->
                                      Lwt.return
                                        (Stdlib.Error
                                           ("validator_escrow_rejected",
                                            error))
                                    | Ok () ->
                                      let next_total =
                                        Z.sub total amount
                                      in
                                      let next_retired =
                                        Z.add retired amount
                                      in
                                      let* () =
                                        save_validator_registry
                                          backend
                                          next
                                      in
                                      let* () =
                                        backend.set_meta
                                          "total_supply"
                                          (Z.to_string next_total)
                                      in
                                      let* () =
                                        backend.set_meta
                                          Emission_schedule.retired_key
                                          (Z.to_string next_retired)
                                      in
                                      Octra_log.warn
                                        "validator"
                                        "event = bond_slashed offender = %s evidence = %s evidence_epoch = %Ld apply_epoch = %Ld amount = %s"
                                        evidence.offender
                                        evidence.id
                                        evidence.evidence_epoch
                                        current_epoch
                                        (Z.to_string amount);
                                      Lwt.return (Stdlib.Ok tx.ou)
                                  end
                              end
                        end
                    end
                end
            end
        end

let process_validator_set_update_tx ~backend ~env tx =
  let open Lwt.Syntax in
  if not
       (Validator_policy.manual_update_allowed
          backend.validator_policy
          ~epoch:env.epoch_id)
  then
    Lwt.return
      (Stdlib.Error
         ("validator_set_update_rejected",
          "manual validator updates are disabled"))
  else if not (List.mem tx.Transaction.from env.validator_addrs) then
    Lwt.return (Stdlib.Error ("validator_set_update_rejected", "sender is not an active validator"))
  else if Z.sign tx.Transaction.amount <> 0 then
    Lwt.return (Stdlib.Error ("validator_set_update_rejected", "amount must be zero"))
  else
    match Validator_set_update.of_message tx.Transaction.message with
    | Error e -> Lwt.return (Stdlib.Error ("malformed_transaction", e))
    | Ok update ->
      if update.Validator_set_update.weighted then
        Lwt.return
          (Stdlib.Error
             ("validator_set_update_rejected",
              "weighted validator updates are protocol generated"))
      else if Int64.compare update.Validator_set_update.activate_epoch (Int64.of_int env.epoch_id) <= 0 then
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

let normalize_ready_state_root value =
  if String.length value = 32 then
    String.concat "" (List.init 32 (fun index ->
      Printf.sprintf "%02x" (Char.code value.[index])))
  else if String.length value >= 64 then
    String.sub value 0 64
  else
    value

let validate_validator_ready_reference ~env ~head_epoch ~state_root =
  let open Lwt.Syntax in
  let current_head = env.epoch_id - 1 in
  let env_prev_state_root = normalize_ready_state_root env.prev_state_root in
  let root_for_ready_head ready_head =
    match env.ready_state_root_at with
    | Some lookup -> lookup ready_head
    | None ->
      if ready_head = current_head then Lwt.return_some env_prev_state_root
      else Lwt.return_none
  in
  let ready_head = Int64.to_int head_epoch in
  if ready_head > current_head then
    Lwt.return (Error "head_epoch is in the future")
  else if env.ready_max_lag >= 0 && current_head - ready_head > env.ready_max_lag then
    Lwt.return (Error "head_epoch too stale")
  else
    let* expected_root_opt = root_for_ready_head ready_head in
    match expected_root_opt with
    | None -> Lwt.return (Error "state_root reference unavailable")
    | Some expected_root ->
      if normalize_ready_state_root state_root
         <> normalize_ready_state_root expected_root then
        Lwt.return (Error "state_root does not match referenced chain head")
      else
        Lwt.return (Ok ())

let process_bonded_validator_ready_tx ~backend ~env tx
    (ready : Validator_registry.ready_payload) =
  let open Lwt.Syntax in
  if not (String.equal tx.Transaction.from tx.to_) then
    Lwt.return
      (Stdlib.Error
         ("validator_ready_rejected",
          "bonded validator readiness must be self-directed"))
  else
    let* reference =
      validate_validator_ready_reference
        ~env
        ~head_epoch:ready.head_epoch
        ~state_root:ready.state_root
    in
    match reference with
    | Error error ->
      Lwt.return (Stdlib.Error ("validator_ready_rejected", error))
    | Ok () ->
      let* registry_result = load_validator_registry backend in
      begin
        match registry_result with
        | Error error ->
          Lwt.return (Stdlib.Error ("validator_registry_corrupt", error))
        | Ok registry ->
          begin
            match
              Validator_registry.mark_ready
                ~epoch:(Int64.of_int env.epoch_id)
                ~address:tx.from
                ~consensus_pubkey_b64:ready.consensus_pubkey_b64
                registry
            with
            | Error error ->
              Lwt.return
                (Stdlib.Error ("validator_ready_rejected", error))
            | Ok next ->
              begin
                match backend.ops.debit tx.from tx.ou tx.nonce with
                | Error error ->
                  Lwt.return
                    (Stdlib.Error ("insufficient_balance", error))
                | Ok () ->
                  let* () = save_validator_registry backend next in
                  Lwt.return (Stdlib.Ok tx.ou)
              end
          end
      end

let process_legacy_validator_ready_tx ~backend ~env tx =
  let open Lwt.Syntax in
  match Validator_set_update.ready_ext_of_message tx.Transaction.message with
  | Error error ->
    Lwt.return (Stdlib.Error ("malformed_transaction", error))
  | Ok ready_ext ->
    let ready = ready_ext.Validator_set_update.ready in
    let* pending_opt =
      Store_irmin.get_meta backend.store Validator_set_update.pending_meta_key
    in
    match pending_opt with
    | None ->
      Lwt.return
        (Stdlib.Error
           ("validator_ready_rejected", "no pending validator set update"))
    | Some pending_raw ->
      match Validator_set_update.of_string pending_raw with
      | Error error ->
        Lwt.return (Stdlib.Error ("validator_ready_rejected", error))
      | Ok update ->
        let in_pending =
          List.exists
            (fun validator ->
              String.equal
                validator.Validator_set_update.address
                tx.from)
            update.Validator_set_update.validators
        in
        if not in_pending then
          Lwt.return
            (Stdlib.Error
               ("validator_ready_rejected",
                "sender is not in pending validator set"))
        else if
          not
            (String.equal
               ready.Validator_set_update.fingerprint
               update.Validator_set_update.fingerprint)
        then
          Lwt.return
            (Stdlib.Error
               ("validator_ready_rejected",
                "fingerprint does not match pending validator set"))
        else
          let* reference =
            validate_validator_ready_reference
              ~env
              ~head_epoch:ready.Validator_set_update.head_epoch
              ~state_root:ready.Validator_set_update.state_root
          in
          match reference with
          | Error error ->
            Lwt.return
              (Stdlib.Error ("validator_ready_rejected", error))
          | Ok () ->
            match backend.ops.debit tx.from tx.ou tx.nonce with
            | Error error ->
              Lwt.return (Stdlib.Error ("insufficient_balance", error))
            | Ok () ->
              let key =
                Validator_set_update.ready_meta_key
                  ~fingerprint:update.Validator_set_update.fingerprint
                  ~address:tx.from
              in
              let* () =
                backend.set_meta
                  key
                  (Validator_set_update.ready_ext_to_string ready_ext)
              in
              Lwt.return (Stdlib.Ok tx.ou)

let process_validator_ready_tx ~backend ~env tx =
  if Z.sign tx.Transaction.amount <> 0 then
    Lwt.return (Stdlib.Error ("validator_ready_rejected", "amount must be zero"))
  else
    match
      backend.validator_policy,
      Validator_registry.ready_payload_of_message tx.Transaction.message
    with
    | Validator_policy.Bonded _, Ok ready ->
      process_bonded_validator_ready_tx ~backend ~env tx ready
    | _ ->
      process_legacy_validator_ready_tx ~backend ~env tx

let process_circle_operation_tx ?expected_transition_hash
    ~(backend : backend) ~(current_epoch : int) (tx : Transaction.t) =
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
    process_circle_balance_cell_put_tx
      ~backend
      ~current_epoch
      ?expected_transition_hash
      tx
  | CircleRegisterCellPut ->
    process_circle_register_cell_put_tx
      ~backend
      ~current_epoch
      ?expected_transition_hash
      tx
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
  | ContractDeploy | ProgramDeploy | ContractCall | ProgramExec | MultiExec
  | ContractUpgrade | CircleCall ->
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
  | ValidatorBond ->
    process_validator_bond_tx ~backend ~env tx
  | ValidatorExit ->
    process_validator_exit_tx ~backend ~env tx
  | ValidatorWithdraw ->
    process_validator_withdraw_tx ~backend ~env tx
  | ValidatorEvidence ->
    process_validator_evidence_tx ~backend ~env tx
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

let schedule_validator_snapshot ~backend ~env =
  let open Lwt.Syntax in
  let source_epoch = Int64.of_int env.epoch_id in
  match
    Validator_policy.snapshot_activation
      backend.validator_policy
      ~source_epoch
  with
  | None -> Lwt.return_unit
  | Some activate_epoch ->
    let* registry_result = load_validator_registry backend in
    begin
      match registry_result with
      | Error error ->
        failwith ("validator registry corrupt: " ^ error)
      | Ok registry ->
        begin
          match backend.validator_policy with
          | Validator_policy.Inactive -> Lwt.return_unit
          | Validator_policy.Bonded policy ->
            match
              Validator_registry.snapshot
                policy.parameters
                ~activate_epoch
                registry
            with
            | Error "validator snapshot has fewer than four eligible members" ->
              Octra_log.info
                "validator"
                "event = snapshot_skipped source_epoch = %Ld activate_epoch = %Ld reason = insufficient_members"
                source_epoch
                activate_epoch;
              Lwt.return_unit
            | Error error ->
              failwith ("validator snapshot rejected: " ^ error)
            | Ok snapshot ->
              begin
                match
                  Validator_set_update.make_weighted
                    ~source_epoch:snapshot.Validator_admission.source_epoch
                    ~activate_epoch:snapshot.Validator_admission.activate_epoch
                    snapshot.Validator_admission.validators
                with
                | Error error ->
                  failwith ("validator snapshot rejected: " ^ error)
                | Ok update ->
                  let* () =
                    backend.set_meta
                      Validator_set_update.pending_meta_key
                      (Validator_set_update.to_string update)
                  in
                  Octra_log.info
                    "validator"
                    "event = snapshot_scheduled source_epoch = %Ld activate_epoch = %Ld validators = %d total_weight = %s quorum_weight = %s fingerprint = %s"
                    snapshot.source_epoch
                    snapshot.activate_epoch
                    (List.length snapshot.validators)
                    (Z.to_string snapshot.total_weight)
                    (Z.to_string snapshot.quorum_weight)
                    snapshot.fingerprint;
                  Lwt.return_unit
              end
        end
    end

let promote_active_validator_set ~backend ~env =
  let open Lwt.Syntax in
  let* pending =
    Store_irmin.get_meta
      backend.store
      Validator_set_update.pending_meta_key
  in
  match pending with
  | None -> Lwt.return_unit
  | Some raw ->
    begin
      match Validator_set_update.of_string raw with
      | Error error ->
        failwith ("pending validator set corrupt: " ^ error)
      | Ok update ->
        if update.Validator_set_update.weighted
           && Int64.compare
                update.activate_epoch
                (Int64.of_int env.epoch_id) <= 0 then
          backend.set_meta
            Validator_set_update.active_meta_key
            raw
        else
          Lwt.return_unit
    end

let advance_validator_set ~backend ~env =
  let open Lwt.Syntax in
  let* () = promote_active_validator_set ~backend ~env in
  schedule_validator_snapshot ~backend ~env

let register_confirmed_sender_key ~backend ~env (tx : Transaction.t) =
  let stored =
    match backend.ops.find_opt tx.from with
    | Some account -> account.Ledger_types.public_key
    | None -> None
  in
  match
    Sender_key_policy.decide
      ~activation_epoch:backend.sender_key_activation_epoch
      ~epoch:env.epoch_id
      ~stored
      ~carried:tx.public_key
  with
  | Sender_key_policy.Keep -> ()
  | Sender_key_policy.Register key ->
    backend.ops.register_public_key tx.from key

let run_core ~reward ~preverify ~backend ~env ~(txs : Transaction.t list)
    ~(process_tx : backend:backend -> env:env -> Transaction.t ->
        (tx_effect, string * string) Stdlib.result Lwt.t) =
  let open Lwt.Syntax in
  Ledger.clear_spent_nonces backend.ledger;
  let* gate_ok =
    match preverify with
    | None -> Lwt.return_ok ()
    | Some gate -> Preverify_commit.check_bound backend.ledger gate txs in
  match gate_ok with
  | Error e -> Lwt.return (Error ("preverify_commit_gate:" ^ e))
  | Ok () ->
  let ordered_txs = Transaction.consensus_order txs in
  match Ledger.begin_journal backend.ledger with
  | Error error -> Lwt.return (Error error)
  | Ok () ->
    Lwt.catch
      (fun () ->
      let* () = backend.begin_batch () in
      let confirmed = ref [] in
      let rejected = ref [] in
      let confirmed_fees = ref Z.zero in
      let pos = ref 0 in
      let* () = Lwt_list.iter_s (fun tx ->
        let* result =
          Tx_savepoint.run
            ~ledger:backend.ledger
            ~store:backend.store
            (fun () ->
              let* result = process_tx ~backend ~env tx in
              begin
                match result with
                | Ok (Confirmed _) ->
                  register_confirmed_sender_key ~backend ~env tx
                | Ok (Rejected_after_fee _)
                | Error _ -> ()
              end;
              Lwt.return result)
        in
        (match result with
         | Ok (Confirmed fee) ->
           confirmed := (tx, !pos) :: !confirmed;
           confirmed_fees := Z.add !confirmed_fees fee
         | Ok (Rejected_after_fee { fee; error_type; reason }) ->
           rejected := { tx; error_type; reason } :: !rejected;
           confirmed_fees := Z.add !confirmed_fees fee
         | Error (etype, reason) ->
           rejected := { tx; error_type = etype; reason } :: !rejected);
        incr pos;
        Lwt.return_unit
      ) ordered_txs in
      let* () = advance_validator_set ~backend ~env in
      let* emission_remaining_opt = Store_irmin.get_meta backend.store "emission_remaining" in
      let emission_remaining =
        match Emission_policy.remaining emission_remaining_opt with
        | Ok value -> value
        | Error error -> failwith error in
      let* prev_supply_opt = Store_irmin.get_meta backend.store "total_supply" in
      let prev_supply =
        match Emission_policy.resolve_total
          ~policy:backend.emission_policy
          ~stored:prev_supply_opt
          ~legacy:backend.legacy_total_supply
          ~public_supply:(backend.ops.total_supply ()) with
        | Ok value -> value
        | Error error -> failwith error in
      let headroom = Z.sub Denomination.max_supply prev_supply in
      let* schedule_standard =
        Store_irmin.get_meta backend.store Emission_schedule.standard_key in
      let* schedule_activation =
        Store_irmin.get_meta backend.store Emission_schedule.activation_key in
      let* schedule_initial =
        Store_irmin.get_meta backend.store Emission_schedule.initial_key in
      let* supply_retired =
        Store_irmin.get_meta backend.store Emission_schedule.retired_key in
      let schedule_binding =
        match
          Emission_schedule.bind
            ~schedule:backend.emission_schedule
            ~epoch_id:env.epoch_id
            ~remaining:emission_remaining
            ~headroom
            ~stored_standard:schedule_standard
            ~stored_activation:schedule_activation
            ~stored_initial:schedule_initial
            ~stored_retired:supply_retired
        with
        | Ok value -> value
        | Error error -> failwith error
      in
      let base_reward =
        match
          Emission_schedule.reward
            ~schedule:backend.emission_schedule
            ~epoch_id:env.epoch_id
            schedule_binding
        with
        | Ok value -> value
        | Error error -> failwith error
      in
      let emission_remaining = schedule_binding.Emission_schedule.remaining in
      let reward = Option.value ~default:(default_reward env) reward in
      let plan =
        match
          match base_reward with
          | Some base_reward ->
            build_reward_plan_with_base
              ~base_reward
              ~fee_burn_active:schedule_binding.Emission_schedule.active
              ~supply_retired:schedule_binding.Emission_schedule.retired
              ~validator_count:(List.length reward.validators)
              ~emission_remaining
              ~confirmed_fees:!confirmed_fees
              ~prev_supply
          | None ->
            build_reward_plan
              ~fee_burn_active:schedule_binding.Emission_schedule.active
              ~supply_retired:schedule_binding.Emission_schedule.retired
              ~validator_count:(List.length reward.validators)
              ~emission_remaining
              ~confirmed_fees:!confirmed_fees
              ~prev_supply
        with
        | Ok plan -> plan
        | Error error -> failwith error in
      let* () =
        apply_epoch_footer_with_reward
          ~reward
          ~backend
          ~env
          ~plan
      in
      let* () =
        Lwt_list.iter_s
          (fun (key, value) -> backend.set_meta key value)
          schedule_binding.Emission_schedule.writes
      in
      let* () =
        if schedule_binding.Emission_schedule.activated
           || Option.is_none emission_remaining_opt then
          backend.set_meta
            "emission_remaining"
            (Z.to_string plan.new_emission_remaining)
        else
          Lwt.return_unit
      in
      let* () =
        if Option.is_none prev_supply_opt then
          backend.set_meta "total_supply" (Z.to_string plan.new_total_supply)
        else
          Lwt.return_unit
      in
      let* () =
        if schedule_binding.Emission_schedule.active
           && Option.is_none supply_retired then
          backend.set_meta
            Emission_schedule.retired_key
            (Z.to_string plan.new_supply_retired)
        else
          Lwt.return_unit
      in
      let* () = backend.flush_dirty () in
      if List.length txs > 0 then
        Octra_log.info "epoch"
          "event = apply_summary epoch = %d txs = %d confirmed = %d rejected = %d fees = %s fee_burn = %s base_reward = %s"
          env.epoch_id (List.length txs) (List.length !confirmed) (List.length !rejected)
          (Z.to_string !confirmed_fees) (Z.to_string plan.fees_burned)
          (Z.to_string plan.base_reward);
      let* batch_h = Store_irmin.get_batch_tree_hash backend.store in
      let post_state_root = match batch_h with
        | Some h -> h
        | None -> "" in
      let* () = backend.commit_batch () in
      begin
        match Ledger.commit_journal backend.ledger with
        | Ok () -> ()
        | Error error -> failwith error
      end;
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
        Store_irmin.abort_epoch_batch backend.store;
        ignore (Ledger.abort_journal backend.ledger);
        Lwt.return (Error (Printexc.to_string exn)))

let confirmed_process process_tx ~backend ~env tx =
  let open Lwt.Syntax in
  let* result = process_tx ~backend ~env tx in
  Lwt.return (Result.map (fun fee -> Confirmed fee) result)

let run ~backend ~env ~(txs : Transaction.t list) ~process_tx =
  run_core
    ~reward:None
    ~preverify:None
    ~backend
    ~env
    ~txs
    ~process_tx:(confirmed_process process_tx)

let run_checked ~preverify ~backend ~env ~(txs : Transaction.t list)
    ~process_tx =
  run_core
    ~reward:None
    ~preverify:(Some preverify)
    ~backend
    ~env
    ~txs
    ~process_tx:(confirmed_process process_tx)

let run_transition_checked ~preverify ~backend ~env
    ~(txs : Transaction.t list) ~process_tx =
  run_core
    ~reward:None
    ~preverify:(Some preverify)
    ~backend
    ~env
    ~txs
    ~process_tx

let run_transition_rewarded ~reward ~preverify ~backend ~env
    ~(txs : Transaction.t list) ~process_tx =
  run_core
    ~reward:(Some reward)
    ~preverify:(Some preverify)
    ~backend
    ~env
    ~txs
    ~process_tx