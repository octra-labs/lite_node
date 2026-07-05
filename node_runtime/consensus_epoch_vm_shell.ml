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


module Contract = Octra_vm.Contract
module ContractVM = Octra_vm.Contract_vm
module Call_plan = Octra_vm.Call_plan
module Direct_exec = Octra_vm.Direct_exec
module Multi_exec = Octra_vm.Multi_exec
module Receipt_view = Octra_vm.Receipt_view
module Transaction = Octra_core.Transaction
module Circle_exec = Octra_circle_runtime.Circle_exec
module Ledger = Octra_core.Ledger
module Store_chaindata = Octra_core.Store_chaindata
module Value_journal = Octra_vm.Value_journal

type ('journal_snapshot, 'pending_snapshot) deps = {
  get_balance : string -> Z.t;
  transfer : from_addr:string -> to_addr:string -> amount:Z.t -> bool;
  snapshot_journal : unit -> 'journal_snapshot;
  restore_journal : 'journal_snapshot -> unit;
  snapshot_pending : unit -> 'pending_snapshot;
  restore_pending : 'pending_snapshot -> unit;
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
  deploy_balance : Transaction.t -> Z.t option;
  deploy_and_save :
    Transaction.t ->
    params:Yojson.Safe.t list ->
    bytecode:ContractVM.instr array ->
    bytecode_raw:string ->
    deploy_result;
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
  ledger : Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Store_chaindata.t;
  receipt_epoch : unit -> int;
  ctx_for_hash : string -> ContractVM.exec_ctx;
  reject_malformed : string -> unit Lwt.t;
  max_multi_exec_calls : int;
  epoch : int;
  now : unit -> float;
}

type live_contract_ctx_args = {
  journal : Value_journal.t;
  store : Octra_core.Store_irmin.t;
  get_fhe_pubkey : string -> Pvac_ffi.pubkey option;
  current_epoch : int;
  tree_hash : string;
  node_id : string;
  tx_hash : string;
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

let restore deps journal pending =
  deps.restore_journal journal;
  deps.restore_pending pending

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
        let journal = deps.snapshot_journal () in
        let pending = deps.snapshot_pending () in
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
          restore deps journal pending;
        subcall_result r);
      deploy_contract = (fun deployer bytecode_raw nonce depth params ->
        let journal = deps.snapshot_journal () in
        let pending = deps.snapshot_pending () in
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
          restore deps journal pending;
          Error e);
      get_fhe_pubkey = deps.get_fhe_pubkey;
      get_fhe_keypair = (fun _ -> None);
      allow_fhe_capability = (fun _ -> true);
      current_epoch = deps.current_epoch;
      tree_hash = deps.tree_hash;
      node_id = deps.node_id;
      tx_hash = deps.tx_hash;
    }
  in
  ctx

let make_live_contract_ctx args =
  make_contract_ctx
    {
      get_balance = Value_journal.effective args.journal;
      transfer = (fun ~from_addr ~to_addr ~amount ->
        Value_journal.transfer args.journal ~from_addr ~to_addr ~amount);
      snapshot_journal = (fun () -> Value_journal.snapshot args.journal);
      restore_journal = (fun snapshot ->
        Value_journal.restore args.journal snapshot);
      snapshot_pending = (fun () ->
        !(Contract.pending_deploys), Hashtbl.copy Contract.pending_storage);
      restore_pending = (fun (pending, storage) ->
        Contract.pending_deploys := pending;
        Hashtbl.reset Contract.pending_storage;
        Hashtbl.iter (Hashtbl.replace Contract.pending_storage) storage);
      execute_call = (fun ~ctx ~depth ~target ~method_name ~params ~caller
          ~amount ->
        Contract.execute_call ~ctx ~depth args.store target method_name params
          caller amount);
      deploy_internal = (fun ~ctx ~depth ~params ~deployer ~bytecode_raw
          ~nonce ->
        Contract.deploy_internal ~ctx ~depth ~params args.store ~deployer
          ~bytecode_raw ~nonce);
      get_fhe_pubkey = args.get_fhe_pubkey;
      current_epoch = args.current_epoch;
      tree_hash = args.tree_hash;
      node_id = args.node_id;
      tx_hash = args.tx_hash;
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

let run_contract_deploy ~fee ~balance ~bytecode_b64_opt ~deployer ~nonce
    ~target ~message ~handle_reject ~with_debited_fee ~reject_after_fee
    ~deploy_and_save ~ensure_account ~commit_effects ~log_deployed
    ~log_constructor_failed ~confirm =
  match Call_plan.plan_deploy_fee ~balance ~fee with
  | Call_plan.Deploy_fee_rejected reject ->
    handle_reject reject
  | Call_plan.Deploy_fee_ready ->
    with_debited_fee fee (fun () ->
      match
        Call_plan.plan_deploy_input
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

let run_deploy_tx ~balance (tx : Transaction.t) ~handle_reject
    ~with_debited_fee ~reject_after_fee ~deploy_and_save ~ensure_account
    ~commit_effects ~log_deployed ~log_constructor_failed ~confirm =
  run_contract_deploy
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

let run_deploy_tx_runtime (runtime : call_runtime) ~balance tx ~deploy_and_save
    ~ensure_account =
  run_deploy_tx
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
        with e ->
          deps.reject_after_fee fee "multi_exec_exception" (Printexc.to_string e))

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
  reject ~consume_nonce:true error_type reason

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
  Log.info "contract" "event = deployed addr = %s effort = %d" addr effort

let log_constructor_failed addr reason =
  Log.warn "contract"
    "event = deploy_constructor_failed addr = %s reason = %s"
    addr reason

let log_multi_exec_success ~calls ~effort =
  Log.info "program" "event = multi_exec_ok calls = %d effort = %d"
    calls effort

let log_multi_exec_failed err =
  Log.warn "program" "event = multi_exec_failed err = %s" err

let log_direct_call_ok meta target method_name effort =
  Log.info meta.Receipt_view.scope
    "event = call_ok %s = %s method = %s effort = %d"
    meta.target_key target method_name effort

let log_direct_call_failed meta target method_name error =
  Log.warn meta.Receipt_view.scope
    "event = call_failed %s = %s method = %s err = %s"
    meta.target_key target method_name error

let log_program_call_ok target meta call receipt =
  log_direct_call_ok meta target call.Call_plan.method_name receipt.Contract.effort_used

let log_circle_call_ok target meta call call_result =
  log_direct_call_ok meta target call.Call_plan.method_name
    call_result.Circle_exec.receipt.Contract.effort_used

let make_live_vm_tx_deps args =
  let save_receipt =
    save_receipt
      {
        save = Store_chaindata.save_receipt args.chaindata;
        epoch = args.receipt_epoch;
      }
  in
  {
    runtime = args.runtime;
    deploy_balance = (fun tx ->
      Option.map (fun acc -> acc.Ledger.balance)
        (Ledger.find_opt args.ledger tx.Transaction.from));
    deploy_and_save = (fun tx ~params ~bytecode ~bytecode_raw ->
      let tx_hash = Transaction.hash tx in
      let ctx_for_tx = args.ctx_for_hash tx_hash in
      let contract_addr, receipt =
        Contract.deploy ~ctx:ctx_for_tx ~params args.store tx.from "CUSTOM"
          bytecode bytecode_raw tx.nonce
      in
      save_receipt ~tx_hash ~contract_addr ~method_name:"constructor" receipt;
      { contract_addr; receipt });
    ensure_account = (fun addr ->
      if not (Ledger.mem args.ledger addr) then
        ignore (Ledger.add_account args.ledger addr Z.zero));
    circle_exec = (fun tx ~ctx call ->
      Circle_exec.execute_call ~ctx ~limit:call.effort_limit args.store tx.to_
        call.method_name call.params tx.from tx.amount);
    circle_save = (fun tx ~tx_hash call call_result ->
      save_receipt ~tx_hash ~contract_addr:tx.to_
        ~method_name:call.method_name call_result.receipt);
    circle_commit = (fun tx call_result ->
      Circle_exec.commit_call_result args.store tx.to_ call_result);
    circle_log_ok = (fun tx -> log_circle_call_ok tx.to_);
    program_exec = (fun tx ~ctx call ->
      Lwt.return
        (Contract.execute_call ~ctx ~limit:call.effort_limit args.store tx.to_
           call.method_name call.params tx.from tx.amount));
    program_save = (fun tx ~tx_hash call receipt ->
      save_receipt ~program:true ~tx_hash ~contract_addr:tx.to_
        ~method_name:call.method_name receipt);
    program_log_ok = (fun tx -> log_program_call_ok tx.to_);
    multi_execute_call = (fun ~ctx ~limit ~target ~method_name ~params ~caller
        ~amount ->
      Contract.execute_call ~ctx ~limit args.store target method_name params caller
        amount);
    save_receipt_raw = Store_chaindata.save_receipt_raw args.chaindata;
    reject_malformed = args.reject_malformed;
    max_multi_exec_calls = args.max_multi_exec_calls;
    epoch = args.epoch;
    now = args.now;
  }

let run_vm_tx (deps : vm_tx_deps) tx =
  match tx.Transaction.op_type with
  | Transaction.ContractDeploy ->
    run_deploy_tx_runtime
      deps.runtime
      ~balance:(deps.deploy_balance tx)
      tx
      ~deploy_and_save:(deps.deploy_and_save tx)
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
  match env "OCTRA_MULTI_EXEC_MAX_CALLS" with
  | Some s ->
    (try max 1 (int_of_string s) with _ -> 8)
  | None ->
    8