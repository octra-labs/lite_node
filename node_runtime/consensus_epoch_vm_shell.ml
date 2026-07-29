(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Contract = Octra_vm.Contract
module ContractVM = Octra_vm.Contract_vm
module Admission = Octra_vm.Admission
module Call_plan = Octra_vm.Call_plan
module Direct_exec = Octra_vm.Direct_exec
module Multi_exec = Octra_vm.Multi_exec
module Receipt_view = Octra_vm.Receipt_view
module Transaction = Octra_core.Transaction
module Circle_exec = Octra_circle_runtime.Circle_exec
module Ledger = Octra_core.Ledger
module Store_chaindata = Octra_core.Store_chaindata
module Value_journal = Octra_vm.Value_journal
module Program_journal = Octra_vm.Program_journal
module Program_package = Octra_vm.Program_package
module Program_upgrade = Octra_vm.Program_upgrade
module Program_trust = Octra_vm.Program_trust
module Tx_effects = Octra_vm.Tx_effects

type ('value_snapshot, 'program_snapshot) deps = {
  get_balance : string -> Z.t;
  transfer : from_addr:string -> to_addr:string -> amount:Z.t -> bool;
  snapshot_value : unit -> 'value_snapshot;
  restore_value : 'value_snapshot -> unit;
  snapshot_program : unit -> 'program_snapshot;
  restore_program : 'program_snapshot -> unit;
  execute_call :
    ctx:ContractVM.exec_ctx ->
    depth:int ->
    target:string ->
    method_name:string ->
    params:Yojson.Safe.t list ->
    caller:string ->
    amount:Z.t ->
    Contract.exec_result;
  deploy_internal :
    ctx:ContractVM.exec_ctx ->
    depth:int ->
    params:ContractVM.v list ->
    deployer:string ->
    bytecode_raw:string ->
    nonce:int ->
    (ContractVM.spawn_result, string) result;
  get_fhe_pubkey : string -> Pvac_ffi.pubkey option;
  current_epoch : int;
  epoch_time_ms : int64;
  tree_hash : string;
  node_id : string;
  tx_hash : string;
}

type deploy_result = {
  contract_addr : string;
  receipt : Contract.exec_result;
}

type receipt_deps = {
  save :
    tx_hash:string ->
    contract_addr:string ->
    method_name:string ->
    success:bool ->
    effort_used:int ->
    events_json:Yojson.Safe.t ->
    error:string option ->
    epoch_id:int ->
    unit;
  epoch : unit -> int;
}

type tx_reject =
  ?consume_nonce:bool ->
  ?notify_reason:string ->
  ?persist_state:bool ->
  string ->
  string ->
  unit Lwt.t

type multi_exec_deps = {
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  execute_call :
    ctx:ContractVM.exec_ctx ->
    limit:int ->
    target:string ->
    method_name:string ->
    params:Yojson.Safe.t list ->
    caller:string ->
    amount:Z.t ->
    Contract.exec_result;
  save_receipt_raw : tx_hash:string -> json:string -> unit;
  commit_effects : unit -> unit;
  log_success : calls:int -> effort:int -> unit;
  log_failed : string -> unit;
  reject_malformed : string -> unit Lwt.t;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
  now : unit -> float;
}

type 'result direct_call_deps = {
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  log_failed : Receipt_view.direct_call_meta -> string -> string -> string -> unit;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  exec : ctx:ContractVM.exec_ctx -> Call_plan.direct_exec -> 'result Lwt.t;
  receipt_of_result : 'result -> Contract.exec_result;
  save : tx_hash:string -> Call_plan.direct_exec -> 'result -> unit;
  ok : Receipt_view.direct_call_meta -> Call_plan.direct_exec -> 'result -> unit Lwt.t;
}

type circle_call_deps = {
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  log_failed : Receipt_view.direct_call_meta -> string -> string -> string -> unit;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  exec : ctx:ContractVM.exec_ctx -> Call_plan.direct_exec -> Circle_exec.call_result Lwt.t;
  save : tx_hash:string -> Call_plan.direct_exec -> Circle_exec.call_result -> unit;
  commit : Circle_exec.call_result -> (unit, string) result Lwt.t;
  commit_effects : unit -> unit;
  log_ok : Receipt_view.direct_call_meta -> Call_plan.direct_exec -> Circle_exec.call_result -> unit;
  confirm : unit -> unit Lwt.t;
}

type program_call_deps = {
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  log_failed : Receipt_view.direct_call_meta -> string -> string -> string -> unit;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  exec : ctx:ContractVM.exec_ctx -> Call_plan.direct_exec -> Contract.exec_result Lwt.t;
  save : tx_hash:string -> Call_plan.direct_exec -> Contract.exec_result -> unit;
  commit_effects : unit -> unit;
  log_ok : Receipt_view.direct_call_meta -> Call_plan.direct_exec -> Contract.exec_result -> unit;
  confirm : unit -> unit Lwt.t;
}

type call_runtime = {
  handle_deploy_reject : Call_plan.deploy_reject -> unit Lwt.t;
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  log_failed : Receipt_view.direct_call_meta -> string -> string -> string -> unit;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  commit_effects : unit -> unit;
  confirm : unit -> unit Lwt.t;
  log_deployed : string -> int -> unit;
  log_constructor_failed : string -> string -> unit;
}

type vm_tx_deps = {
  runtime : call_runtime;
  trusted_program_keys : Program_trust.t;
  deploy_balance : Transaction.t -> Z.t option;
  deploy_and_save :
    Transaction.t ->
    admitted:Admission.t option ->
    params:Yojson.Safe.t list ->
    bytecode:ContractVM.instr array ->
    bytecode_raw:string ->
    deploy_result;
  program_prepare :
    Transaction.t ->
    (Program_package.admitted, string) result Lwt.t;
  ensure_account : string -> unit;
  circle_exec :
    Transaction.t ->
    ctx:ContractVM.exec_ctx ->
    Call_plan.direct_exec ->
    Circle_exec.call_result Lwt.t;
  circle_save :
    Transaction.t ->
    tx_hash:string ->
    Call_plan.direct_exec ->
    Circle_exec.call_result ->
    unit;
  circle_commit :
    Transaction.t ->
    Circle_exec.call_result ->
    (unit, string) result Lwt.t;
  circle_log_ok :
    Transaction.t ->
    Receipt_view.direct_call_meta ->
    Call_plan.direct_exec ->
    Circle_exec.call_result ->
    unit;
  program_exec :
    Transaction.t ->
    ctx:ContractVM.exec_ctx ->
    Call_plan.direct_exec ->
    Contract.exec_result Lwt.t;
  program_save :
    Transaction.t ->
    tx_hash:string ->
    Call_plan.direct_exec ->
    Contract.exec_result ->
    unit;
  program_log_ok :
    Transaction.t ->
    Receipt_view.direct_call_meta ->
    Call_plan.direct_exec ->
    Contract.exec_result ->
    unit;
  multi_execute_call :
    ctx:ContractVM.exec_ctx ->
    limit:int ->
    target:string ->
    method_name:string ->
    params:Yojson.Safe.t list ->
    caller:string ->
    amount:Z.t ->
    Contract.exec_result;
  save_receipt_raw : tx_hash:string -> json:string -> unit;
  reject_malformed : string -> unit Lwt.t;
  max_multi_exec_calls : int;
  epoch : int;
  now : unit -> float;
}

type live_vm_tx_args = {
  runtime : call_runtime;
  trusted_program_keys : Program_trust.t;
  program_journal : Program_journal.t;
  ledger : Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Store_chaindata.t;
  receipt_epoch : unit -> int;
  ctx_for_hash : string -> ContractVM.exec_ctx;
  ensure_account : string -> unit;
  reject_malformed : string -> unit Lwt.t;
  max_multi_exec_calls : int;
  epoch : int;
  now : unit -> float;
}

type live_call_runtime_args = {
  reject : tx_reject;
  debit : string -> Z.t -> int -> (unit, string) result;
  tx : Transaction.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  discard_effects : unit -> unit;
  discard_fee : Z.t -> unit;
  commit_effects : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

type live_contract_ctx_args = {
  value_journal : Value_journal.t;
  program_journal : Program_journal.t;
  trusted_program_keys : Program_trust.t;
  store : Octra_core.Store_irmin.t;
  get_fhe_pubkey : string -> Pvac_ffi.pubkey option;
  current_epoch : int;
  epoch_time_ms : int64;
  tree_hash : string;
  node_id : string;
  tx_hash : string;
}

type live_value_effects = {
  value_journal : Value_journal.t;
  program_journal : Program_journal.t;
  balance : string -> Z.t;
  ensure_account : string -> unit;
  discard : unit -> unit;
  discard_fee : Z.t -> unit;
  commit : unit -> unit;
  apply : Call_plan.value_effect -> unit;
}

type live_value_effect_args = {
  ledger : Ledger.t;
  store : Octra_core.Store_irmin.t;
  add_fee : Z.t -> unit;
}

type live_sender_vm_tx_args = {
  value_journal : Value_journal.t;
  program_journal : Program_journal.t;
  trusted_program_keys : Program_trust.t;
  ledger : Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Store_chaindata.t;
  tx : Transaction.t;
  current_epoch : unit -> int;
  epoch_time_ms : int64;
  pre_state_hash : string;
  node_id : string;
  reject : tx_reject;
  balance : string -> Z.t;
  ensure_account : string -> unit;
  apply_value_effect : Call_plan.value_effect -> unit;
  discard_effects : unit -> unit;
  discard_fee : Z.t -> unit;
  commit_effects : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

let make_circle_call_deps (runtime : call_runtime) ~exec ~save ~commit ~log_ok :
    circle_call_deps =
  {
    with_debited_fee = runtime.with_debited_fee;
    make_ctx = runtime.make_ctx;
    balance = runtime.balance;
    apply_value_effect = runtime.apply_value_effect;
    log_failed = runtime.log_failed;
    reject_after_fee = runtime.reject_after_fee;
    reject = runtime.reject;
    exec;
    save;
    commit;
    commit_effects = runtime.commit_effects;
    log_ok;
    confirm = runtime.confirm;
  }

let make_program_call_deps (runtime : call_runtime) ~exec ~save ~log_ok :
    program_call_deps =
  {
    with_debited_fee = runtime.with_debited_fee;
    make_ctx = runtime.make_ctx;
    balance = runtime.balance;
    apply_value_effect = runtime.apply_value_effect;
    log_failed = runtime.log_failed;
    reject_after_fee = runtime.reject_after_fee;
    reject = runtime.reject;
    exec;
    save;
    commit_effects = runtime.commit_effects;
    log_ok;
    confirm = runtime.confirm;
  }

let make_multi_exec_deps (runtime : call_runtime) ~execute_call
    ~save_receipt_raw ~log_success ~log_failed ~reject_malformed ~now :
    multi_exec_deps =
  {
    with_debited_fee = runtime.with_debited_fee;
    make_ctx = runtime.make_ctx;
    balance = runtime.balance;
    apply_value_effect = runtime.apply_value_effect;
    execute_call;
    save_receipt_raw;
    commit_effects = runtime.commit_effects;
    log_success;
    log_failed;
    reject_malformed;
    reject_after_fee = runtime.reject_after_fee;
    confirm = runtime.confirm;
    now;
  }

let restore deps value program =
  deps.restore_value value;
  deps.restore_program program

let subcall_result (r : Contract.exec_result) =
  match Contract.exec_result_to_result r with
  | Ok value ->
    Ok {
      ContractVM.return_value = value;
      effort_used = r.effort_used;
      events = r.events;
    }
  | Error e ->
    Error e

let make_contract_ctx deps =
  let rec ctx =
    {
      ContractVM.default_ctx with
      get_balance = deps.get_balance;
      do_transfer = (fun from_addr to_addr amount ->
        deps.transfer ~from_addr ~to_addr ~amount);
      call_contract = (fun caller target method_name args depth ->
        let value = deps.snapshot_value () in
        let program = deps.snapshot_program () in
        let params = List.map Receipt_view.nested_call_arg_json args in
        let r =
          deps.execute_call
            ~ctx
            ~depth
            ~target
            ~method_name
            ~params
            ~caller
            ~amount:Z.zero
        in
        if not r.success then
          restore deps value program;
        subcall_result r);
      deploy_contract = (fun deployer bytecode_raw nonce depth params ->
        let value = deps.snapshot_value () in
        let program = deps.snapshot_program () in
        match
          deps.deploy_internal
            ~ctx
            ~depth
            ~params
            ~deployer
            ~bytecode_raw
            ~nonce
        with
        | Ok spawn ->
          Ok spawn
        | Error e ->
          restore deps value program;
          Error e);
      get_fhe_pubkey = deps.get_fhe_pubkey;
      get_fhe_keypair = (fun _ -> None);
      allow_fhe_capability = (fun _ -> true);
      current_epoch = deps.current_epoch;
      epoch_time_ms = deps.epoch_time_ms;
      tree_hash = deps.tree_hash;
      node_id = deps.node_id;
      tx_hash = deps.tx_hash;
    }
  in
  ctx

let make_live_contract_ctx (args : live_contract_ctx_args) =
  let ctx = make_contract_ctx
    {
      get_balance = Value_journal.effective args.value_journal;
      transfer = (fun ~from_addr ~to_addr ~amount ->
        Value_journal.transfer args.value_journal ~from_addr ~to_addr ~amount);
      snapshot_value = (fun () -> Value_journal.snapshot args.value_journal);
      restore_value = (fun snapshot ->
        Value_journal.restore args.value_journal snapshot);
      snapshot_program = (fun () ->
        Program_journal.snapshot args.program_journal);
      restore_program = (fun snapshot ->
        Program_journal.restore args.program_journal snapshot);
      execute_call = (fun ~ctx ~depth ~target ~method_name ~params ~caller
          ~amount ->
        Contract.execute_call
          ~trusted:(Program_trust.keys args.trusted_program_keys)
          ~journal:args.program_journal ~ctx ~depth
          args.store target method_name params caller amount);
      deploy_internal = (fun ~ctx ~depth ~params ~deployer ~bytecode_raw
          ~nonce ->
        Contract.deploy_internal
          ~trusted:(Program_trust.keys args.trusted_program_keys)
          ~journal:args.program_journal ~ctx ~depth
          ~params args.store ~deployer ~bytecode_raw ~nonce);
      get_fhe_pubkey = args.get_fhe_pubkey;
      current_epoch = args.current_epoch;
      epoch_time_ms = args.epoch_time_ms;
      tree_hash = args.tree_hash;
      node_id = args.node_id;
      tx_hash = args.tx_hash;
    }
  in
  { ctx with epoch_time_ms = args.epoch_time_ms }

let live_fhe_pubkey store addr =
  match Node_rest_facade.run_s (Octra_core.Store_irmin.get_pvac_pubkey store addr) with
  | None -> None
  | Some blob ->
    match Octra_core.Pvac_registry.load_pubkey blob with
    | Ok pk -> Some pk
    | Error _ -> None

let make_live_value_effects (args : live_value_effect_args) =
  let tx = Tx_effects.create ~ledger:args.ledger ~store:args.store in
  let value_journal = Tx_effects.value tx in
  let program_journal = Tx_effects.program tx in
  let discard () = Tx_effects.discard tx in
  {
    value_journal;
    program_journal;
    balance = Tx_effects.balance tx;
    ensure_account = (fun address ->
      match Tx_effects.ensure_account tx address with
      | Ok () -> ()
      | Error error -> raise (Tx_effects.Commit_failed error));
    discard;
    discard_fee = (fun fee ->
      discard ();
      args.add_fee fee);
    commit = (fun () -> Tx_effects.commit_exn tx);
    apply = (fun effect -> ignore (Tx_effects.apply tx effect));
  }

let direct_exec_spec_of_tx ~domain ~reject_domain ~balance (tx : Transaction.t) =
  {
    Direct_exec.domain;
    reject_domain;
    method_name = tx.encrypted_data;
    params_json = tx.message;
    from_addr = tx.from;
    target = tx.to_;
    amount = tx.amount;
    balance;
    ou = tx.ou;
  }

let run_direct_exec spec ~fee ~target ~apply_value_effect ~log_failed
    ~reject_after_fee ~reject ~exec ~receipt ~save ~ok =
  Direct_exec.run spec {
    Direct_exec.apply = apply_value_effect;
    exec;
    receipt;
    save;
    ok;
    fail = (fun meta call error ->
      log_failed meta target call.method_name error;
      reject_after_fee fee meta.failure_type error);
    reject;
    crash = (fun meta err ->
      reject_after_fee fee meta.exception_type err);
  }

let run_direct_call (deps : 'result direct_call_deps) ~domain ~reject_domain
    (tx : Transaction.t) =
  let fee = tx.ou in
  deps.with_debited_fee fee (fun () ->
    let tx_hash = Transaction.hash tx in
    let ctx = deps.make_ctx tx_hash in
    run_direct_exec
      (direct_exec_spec_of_tx
         ~domain
         ~reject_domain
         ~balance:(deps.balance tx.from)
         tx)
      ~fee
      ~target:tx.to_
      ~apply_value_effect:deps.apply_value_effect
      ~log_failed:deps.log_failed
      ~reject_after_fee:deps.reject_after_fee
      ~reject:deps.reject
      ~exec:(deps.exec ~ctx)
      ~receipt:deps.receipt_of_result
      ~save:(fun call result -> deps.save ~tx_hash call result)
      ~ok:deps.ok)

let run_circle_call_tx (deps : circle_call_deps) tx =
  run_direct_call
    {
      with_debited_fee = deps.with_debited_fee;
      make_ctx = deps.make_ctx;
      balance = deps.balance;
      apply_value_effect = deps.apply_value_effect;
      log_failed = deps.log_failed;
      reject_after_fee = deps.reject_after_fee;
      reject = deps.reject;
      exec = deps.exec;
      receipt_of_result = (fun result -> result.Circle_exec.receipt);
      save = deps.save;
      ok = (fun meta call result ->
        let open Lwt.Syntax in
        let* commit_result = deps.commit result in
        match commit_result with
        | Ok _ ->
          deps.commit_effects ();
          deps.log_ok meta call result;
          deps.confirm ()
        | Error e ->
          deps.reject_after_fee tx.Transaction.ou "circle_call_commit_failed" e);
    }
    ~domain:Receipt_view.Circle_call
    ~reject_domain:Call_plan.Circle_exec
    tx

let run_program_call_tx (deps : program_call_deps) tx =
  run_direct_call
    {
      with_debited_fee = deps.with_debited_fee;
      make_ctx = deps.make_ctx;
      balance = deps.balance;
      apply_value_effect = deps.apply_value_effect;
      log_failed = deps.log_failed;
      reject_after_fee = deps.reject_after_fee;
      reject = deps.reject;
      exec = deps.exec;
      receipt_of_result = (fun receipt -> receipt);
      save = deps.save;
      ok = (fun meta call receipt ->
        deps.commit_effects ();
        deps.log_ok meta call receipt;
        deps.confirm ());
    }
    ~domain:Receipt_view.Program_call
    ~reject_domain:Call_plan.Program_exec
    tx

let run_program_upgrade_tx (runtime : call_runtime) ~trusted_program_keys
    ~program_journal ~store (tx : Transaction.t) =
  runtime.with_debited_fee tx.ou (fun () ->
    match Program_upgrade.parse tx with
    | Error error ->
      runtime.reject_after_fee tx.ou "program_upgrade_rejected" error
    | Ok payload ->
      begin
        match
          Contract.upgrade
            ~journal:program_journal
            ~trusted:(Program_trust.keys trusted_program_keys)
            store
            ~address:tx.to_
            ~caller:tx.from
            ~expected_code_hash:payload.expected_code_hash
            ~bytecode_raw:payload.bytecode_raw
        with
        | Error error ->
          runtime.reject_after_fee tx.ou "program_upgrade_rejected" error
        | Ok result ->
          runtime.commit_effects ();
          Octra_log.info
            "program"
            "event = program_upgraded addr = %s old = %s new = %s"
            tx.to_
            result.old_code_hash
            result.new_code_hash;
          runtime.confirm ()
      end)

let run_contract_deploy ~trusted_program_keys ~fee ~balance ~bytecode_b64_opt ~deployer ~nonce
    ~target ~message ~handle_reject ~with_debited_fee ~reject_after_fee
    ~deploy_and_save ~ensure_account ~commit_effects ~log_deployed
    ~log_constructor_failed ~confirm =
  match Call_plan.plan_deploy_fee ~balance ~fee with
  | Call_plan.Deploy_fee_rejected reject ->
    handle_reject reject
  | Call_plan.Deploy_fee_ready ->
    with_debited_fee fee (fun () ->
      match
        Call_plan.plan_deploy_input_with_keys
          ~trusted:(Program_trust.keys trusted_program_keys)
          ~bytecode_b64_opt
          ~deployer
          ~nonce
          ~target
      with
      | Call_plan.Deploy_input_rejected reject ->
        handle_reject reject
      | Call_plan.Deploy_input_exception err ->
        reject_after_fee fee "contract_deploy_failed" err
      | Call_plan.Deploy_input_ready deploy ->
        let params = Call_plan.parse_deploy_params message in
        let result =
          deploy_and_save
            ~params
            ~bytecode:deploy.bytecode
            ~bytecode_raw:deploy.bytecode_raw
        in
        if result.receipt.success then begin
          ensure_account result.contract_addr;
          commit_effects ();
          log_deployed result.contract_addr result.receipt.effort_used;
          confirm ()
        end else
          let err =
            Receipt_view.constructor_revert_message
              result.receipt.events
              result.receipt.error
          in
          log_constructor_failed result.contract_addr err;
          reject_after_fee fee "constructor_failed" err)

let run_deploy_tx ~trusted_program_keys ~balance (tx : Transaction.t) ~handle_reject
    ~with_debited_fee ~reject_after_fee ~deploy_and_save ~ensure_account
    ~commit_effects ~log_deployed ~log_constructor_failed ~confirm =
  run_contract_deploy
    ~trusted_program_keys
    ~fee:tx.ou
    ~balance
    ~bytecode_b64_opt:tx.encrypted_data
    ~deployer:tx.from
    ~nonce:tx.nonce
    ~target:tx.to_
    ~message:tx.message
    ~handle_reject
    ~with_debited_fee
    ~reject_after_fee
    ~deploy_and_save
    ~ensure_account
    ~commit_effects
    ~log_deployed
    ~log_constructor_failed
    ~confirm

let run_deploy_tx_runtime ~trusted_program_keys (runtime : call_runtime) ~balance tx ~deploy_and_save
    ~ensure_account =
  run_deploy_tx
    ~trusted_program_keys
    ~balance
    tx
    ~handle_reject:runtime.handle_deploy_reject
    ~with_debited_fee:runtime.with_debited_fee
    ~reject_after_fee:runtime.reject_after_fee
    ~deploy_and_save
    ~ensure_account
    ~commit_effects:runtime.commit_effects
    ~log_deployed:runtime.log_deployed
    ~log_constructor_failed:runtime.log_constructor_failed
    ~confirm:runtime.confirm

let run_program_deploy_tx (deps : vm_tx_deps) tx =
  let runtime = deps.runtime in
  match Call_plan.plan_deploy_fee
    ~balance:(deps.deploy_balance tx)
    ~fee:tx.Transaction.ou with
  | Call_plan.Deploy_fee_rejected reject ->
    runtime.handle_deploy_reject reject
  | Call_plan.Deploy_fee_ready ->
    runtime.with_debited_fee tx.ou (fun () ->
      let open Lwt.Syntax in
      let* prepared = deps.program_prepare tx in
      match prepared with
      | Error reason ->
        runtime.reject_after_fee tx.ou "program_deploy_rejected" reason
      | Ok package ->
        let expected =
          Contract.addr_from_code package.envelope tx.from tx.nonce
        in
        if not (String.equal expected tx.to_) then
          runtime.reject_after_fee
            tx.ou
            "program_address_mismatch"
            "Program address does not match source package"
        else
          let params = Call_plan.parse_deploy_params tx.message in
          let result =
            deps.deploy_and_save
              tx
              ~admitted:(Some package.program)
              ~params
              ~bytecode:(Admission.code package.program)
              ~bytecode_raw:package.envelope
          in
          if result.receipt.success then begin
            deps.ensure_account result.contract_addr;
            runtime.commit_effects ();
            runtime.log_deployed
              result.contract_addr
              result.receipt.effort_used;
            runtime.confirm ()
          end else
            let reason =
              Receipt_view.constructor_revert_message
                result.receipt.events
                result.receipt.error
            in
            runtime.log_constructor_failed result.contract_addr reason;
            runtime.reject_after_fee tx.ou "constructor_failed" reason)

let prepare_program_package (tx : Transaction.t) =
  match tx.encrypted_data with
  | None -> Lwt.return_error "Program package missing"
  | Some encoded ->
    Lwt_preemptive.detach
      (fun () ->
        match Program_package.admit_base64 encoded with
        | Ok package -> Ok package
        | Error error -> Error (Program_package.error_message error))
      ()

let run_multi_exec (deps : multi_exec_deps) ~max_calls ~epoch ~tx_hash
    ~from_addr ~message ~fee =
  match message with
  | None ->
    deps.reject_malformed "multi_exec requires calls payload"
  | Some calls_json ->
    match Call_plan.parse_multi_exec_calls ~max_calls calls_json with
    | Error err ->
      deps.reject_malformed err
    | Ok calls ->
      deps.with_debited_fee fee (fun () ->
        try
          let ctx = deps.make_ctx tx_hash in
          let result =
            Multi_exec.run
              ~from_addr
              ~calls
              ~effort_limit:(Call_plan.effort_limit fee)
              ~balance:deps.balance
              ~exec:(fun step ->
                deps.apply_value_effect step.value_effect;
                let call = step.call in
                deps.execute_call
                  ~ctx
                  ~limit:step.remaining_effort
                  ~target:call.target
                  ~method_name:call.method_name
                  ~params:call.params
                  ~caller:from_addr
                  ~amount:call.amount)
          in
          let trace = result.trace in
          let receipt_json success error =
            Receipt_view.multi_exec_receipt_json
              ~epoch
              ~now:(deps.now ())
              ~effort:trace.effort
              ~calls:trace.calls
              ~events:trace.events
              ~success
              ~error
          in
          match result.outcome with
          | Ok () ->
            deps.save_receipt_raw ~tx_hash ~json:(receipt_json true None);
            deps.commit_effects ();
            deps.log_success ~calls:(List.length calls) ~effort:trace.effort;
            deps.confirm ()
          | Error err ->
            deps.save_receipt_raw ~tx_hash ~json:(receipt_json false (Some err));
            deps.log_failed err;
            deps.reject_after_fee fee "multi_exec_failed" err
        with
        | Tx_effects.Commit_failed _ as error -> raise error
        | error ->
          deps.reject_after_fee fee "multi_exec_exception"
            (Printexc.to_string error))

let run_multi_exec_tx deps ~max_calls ~epoch (tx : Transaction.t) =
  run_multi_exec
    deps
    ~max_calls
    ~epoch
    ~tx_hash:(Transaction.hash tx)
    ~from_addr:tx.from
    ~message:tx.message
    ~fee:tx.ou

let handle_deploy_reject ~(reject : tx_reject) r =
  reject
    ~consume_nonce:r.Call_plan.deploy_consume_nonce
    ~notify_reason:r.deploy_notify_reason
    r.deploy_error_type
    r.deploy_log_reason

let handle_direct_exec_reject ~discard ~(reject : tx_reject) r =
  if r.Call_plan.discard_effects then
    discard ();
  reject
    ~consume_nonce:r.consume_nonce
    ~notify_reason:r.notify_reason
    r.error_type
    r.log_reason

let with_debited_fee ~debit ~reject tx fee on_ok =
  match debit tx.Transaction.from fee tx.nonce with
  | Error err ->
    reject "insufficient_balance" err
  | Ok () ->
    on_ok ()

let reject_after_fee ~discard_fee ~(reject : tx_reject) fee error_type reason =
  discard_fee fee;
  reject ~consume_nonce:true ~persist_state:true error_type reason

let save_receipt (deps : receipt_deps) ?(program = false) ~tx_hash ~contract_addr
    ~method_name receipt =
  let receipt_save = Receipt_view.receipt_save ~program receipt in
  deps.save
    ~tx_hash
    ~contract_addr
    ~method_name
    ~success:receipt_save.success
    ~effort_used:receipt_save.effort_used
    ~events_json:receipt_save.events_json
    ~error:receipt_save.error
    ~epoch_id:(deps.epoch ())

let log_deployed addr effort =
  Log.info "program" "event = deployed addr = %s effort = %d" addr effort

let log_constructor_failed addr reason =
  Log.warn "program"
    "event = deploy_constructor_failed addr = %s reason = %s"
    addr reason

let log_multi_exec_success ~calls ~effort =
  Log.info "program" "event = multi_exec_ok calls = %d effort = %d"
    calls effort

let log_multi_exec_failed err =
  Log.warn "program" "event = multi_exec_failed err = %s" err

let program_log_scope meta =
  if String.equal meta.Receipt_view.scope "contract" then "program"
  else meta.Receipt_view.scope

let log_direct_call_ok meta target method_name effort =
  Log.info (program_log_scope meta)
    "event = call_ok %s = %s method = %s effort = %d"
    meta.target_key target method_name effort

let log_direct_call_failed meta target method_name error =
  Log.warn (program_log_scope meta)
    "event = call_failed %s = %s method = %s err = %s"
    meta.target_key target method_name error

let log_program_call_ok target meta call receipt =
  log_direct_call_ok meta target call.Call_plan.method_name receipt.Contract.effort_used

let log_circle_call_ok target meta call call_result =
  log_direct_call_ok meta target call.Call_plan.method_name
    call_result.Circle_exec.receipt.Contract.effort_used

let make_live_call_runtime (args : live_call_runtime_args) =
  {
    handle_deploy_reject = handle_deploy_reject ~reject:args.reject;
    with_debited_fee =
      with_debited_fee
        ~debit:args.debit
        ~reject:(fun tag reason -> args.reject tag reason)
        args.tx;
    make_ctx = args.make_ctx;
    balance = args.balance;
    apply_value_effect = args.apply_value_effect;
    log_failed = log_direct_call_failed;
    reject_after_fee =
      reject_after_fee
        ~discard_fee:args.discard_fee
        ~reject:args.reject;
    reject =
      handle_direct_exec_reject
        ~discard:args.discard_effects
        ~reject:args.reject;
    commit_effects = args.commit_effects;
    confirm = args.confirm;
    log_deployed;
    log_constructor_failed;
  }

let make_live_vm_tx_deps (args : live_vm_tx_args) =
  let save_receipt =
    save_receipt
      {
        save = Store_chaindata.save_receipt args.chaindata;
        epoch = args.receipt_epoch;
      }
  in
  {
    runtime = args.runtime;
    trusted_program_keys = args.trusted_program_keys;
    deploy_balance = (fun tx ->
      Option.map (fun acc -> acc.Ledger.balance)
        (Ledger.find_opt args.ledger tx.Transaction.from));
    deploy_and_save = (fun tx ~admitted ~params ~bytecode ~bytecode_raw ->
      let tx_hash = Transaction.hash tx in
      let ctx_for_tx = args.ctx_for_hash tx_hash in
      let contract_addr, receipt =
        Contract.deploy
          ~trusted:(Program_trust.keys args.trusted_program_keys)
          ?admitted
          ~journal:args.program_journal ~ctx:ctx_for_tx ~params
          args.store tx.from "CUSTOM" bytecode bytecode_raw tx.nonce
      in
      save_receipt ~tx_hash ~contract_addr ~method_name:"constructor" receipt;
      { contract_addr; receipt });
    program_prepare = prepare_program_package;
    ensure_account = args.ensure_account;
    circle_exec = (fun tx ~ctx call ->
      let ctx = { ctx with ContractVM.node_id = tx.to_ } in
      Circle_exec.execute_call
        ~trusted:(Program_trust.keys args.trusted_program_keys)
        ~ctx ~limit:call.effort_limit args.store tx.to_
        call.method_name call.params tx.from tx.amount);
    circle_save = (fun tx ~tx_hash call call_result ->
      save_receipt ~tx_hash ~contract_addr:tx.to_
        ~method_name:call.method_name call_result.receipt);
    circle_commit = (fun tx call_result ->
      Circle_exec.commit_call_result args.store tx.to_ call_result);
    circle_log_ok = (fun tx -> log_circle_call_ok tx.to_);
    program_exec = (fun tx ~ctx call ->
      Lwt.return
        (Contract.execute_call
           ~trusted:(Program_trust.keys args.trusted_program_keys)
           ~journal:args.program_journal ~ctx
           ~limit:call.effort_limit args.store tx.to_ call.method_name
           call.params tx.from tx.amount));
    program_save = (fun tx ~tx_hash call receipt ->
      save_receipt ~program:true ~tx_hash ~contract_addr:tx.to_
        ~method_name:call.method_name receipt);
    program_log_ok = (fun tx -> log_program_call_ok tx.to_);
    multi_execute_call = (fun ~ctx ~limit ~target ~method_name ~params ~caller
        ~amount ->
      Contract.execute_call
        ~trusted:(Program_trust.keys args.trusted_program_keys)
        ~journal:args.program_journal ~ctx ~limit args.store
        target method_name params caller amount);
    save_receipt_raw = Store_chaindata.save_receipt_raw args.chaindata;
    reject_malformed = args.reject_malformed;
    max_multi_exec_calls = args.max_multi_exec_calls;
    epoch = args.epoch;
    now = args.now;
  }

let run_vm_tx (deps : vm_tx_deps) tx =
  match tx.Transaction.op_type with
  | Transaction.ProgramDeploy ->
    run_program_deploy_tx deps tx
  | Transaction.ContractDeploy ->
    run_deploy_tx_runtime
      ~trusted_program_keys:deps.trusted_program_keys
      deps.runtime
      ~balance:(deps.deploy_balance tx)
      tx
      ~deploy_and_save:(fun ~params ~bytecode ~bytecode_raw ->
        deps.deploy_and_save
          tx
          ~admitted:None
          ~params
          ~bytecode
          ~bytecode_raw)
      ~ensure_account:deps.ensure_account
  | Transaction.CircleCall ->
    run_circle_call_tx
      (make_circle_call_deps
         deps.runtime
         ~exec:(deps.circle_exec tx)
         ~save:(deps.circle_save tx)
         ~commit:(deps.circle_commit tx)
         ~log_ok:(deps.circle_log_ok tx))
      tx
  | Transaction.ContractCall | Transaction.ProgramExec ->
    run_program_call_tx
      (make_program_call_deps
         deps.runtime
         ~exec:(deps.program_exec tx)
         ~save:(deps.program_save tx)
         ~log_ok:(deps.program_log_ok tx))
      tx
  | Transaction.MultiExec ->
    run_multi_exec_tx
      (make_multi_exec_deps
         deps.runtime
         ~execute_call:deps.multi_execute_call
         ~save_receipt_raw:deps.save_receipt_raw
         ~log_success:log_multi_exec_success
         ~log_failed:log_multi_exec_failed
         ~reject_malformed:deps.reject_malformed
         ~now:deps.now)
      ~max_calls:deps.max_multi_exec_calls
      ~epoch:deps.epoch
      tx
  | _ ->
    deps.reject_malformed "invalid vm operation"

let max_multi_exec_calls ~env =
  Startup_runtime_limits.multi_exec_max {
    int_value = (fun name fallback ->
      match env name with
      | Some raw -> (try int_of_string raw with _ -> fallback)
      | None -> fallback);
    opt = env;
  }

let make_live_sender_vm_tx_deps (args : live_sender_vm_tx_args) =
  let ctx_for_hash tx_hash =
    make_live_contract_ctx
      {
        value_journal = args.value_journal;
        program_journal = args.program_journal;
        trusted_program_keys = args.trusted_program_keys;
        store = args.store;
        get_fhe_pubkey = live_fhe_pubkey args.store;
        current_epoch = args.current_epoch ();
        epoch_time_ms = args.epoch_time_ms;
        tree_hash = args.pre_state_hash;
        node_id = args.node_id;
        tx_hash;
      }
  in
  let runtime =
    make_live_call_runtime {
      reject = args.reject;
      debit = Ledger.debit args.ledger;
      tx = args.tx;
      make_ctx = ctx_for_hash;
      balance = args.balance;
      apply_value_effect = args.apply_value_effect;
      discard_effects = args.discard_effects;
      discard_fee = args.discard_fee;
      commit_effects = args.commit_effects;
      confirm = args.confirm;
    }
  in
  make_live_vm_tx_deps {
    runtime;
    trusted_program_keys = args.trusted_program_keys;
    program_journal = args.program_journal;
    ledger = args.ledger;
    store = args.store;
    chaindata = args.chaindata;
    receipt_epoch = args.current_epoch;
    ctx_for_hash;
    ensure_account = args.ensure_account;
    reject_malformed = (fun reason ->
      args.reject "malformed_transaction" reason);
    max_multi_exec_calls = max_multi_exec_calls ~env:Sys.getenv_opt;
    epoch = args.current_epoch ();
    now = Unix.gettimeofday;
  }