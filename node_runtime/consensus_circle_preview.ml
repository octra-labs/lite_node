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


module Call_plan = Octra_vm.Call_plan
module Circle_exec = Octra_circle_runtime.Circle_exec
module Contract = Octra_vm.Contract
module Transaction = Octra_core.Transaction
module Tx_effects = Octra_vm.Tx_effects
module Vm = Consensus_epoch_vm_shell

let fail effects tag reason =
  try
    Tx_effects.discard effects;
    Lwt.return (Error (tag, reason))
  with error ->
    Lwt.return
      (Error
         ("circle_call_rollback_failed", Printexc.to_string error))

let reject_plan effects rejected =
  let rejected = Call_plan.direct_exec_reject Call_plan.Circle_exec rejected in
  fail effects rejected.error_type rejected.log_reason

let commit effects store circle_id result =
  let open Lwt.Syntax in
  let* committed = Circle_exec.commit_call_result store circle_id result in
  match committed with
  | Error reason ->
    fail effects "circle_call_commit_failed" reason
  | Ok () ->
    begin
      match Tx_effects.commit effects with
      | Ok () -> Lwt.return (Ok ())
      | Error reason ->
        Lwt.return (Error ("circle_call_effect_commit_failed", reason))
    end

let execute ~program_trust backend env tx effects call =
  let ctx =
    Vm.make_live_contract_ctx
      {
        value_journal = Tx_effects.value effects;
        program_journal = Tx_effects.program effects;
        trusted_program_keys = program_trust;
        store = backend.Octra_core.Epoch_exec.store;
        get_fhe_pubkey = Vm.live_fhe_pubkey backend.store;
        current_epoch = env.Octra_core.Epoch_exec.epoch_id;
        epoch_time_ms =
          (match Octra_consensus.Epoch_time.of_seconds env.epoch_ts with
           | Ok value -> value
           | Error _ -> 0L);
        tree_hash = env.prev_state_root;
        node_id = tx.Transaction.to_;
        tx_hash = Transaction.hash tx;
      }
  in
  let open Lwt.Syntax in
  let* result =
    Circle_exec.execute_call
      ~trusted:(Octra_vm.Program_trust.keys program_trust)
      ~ctx
      ~limit:call.Call_plan.effort_limit
      backend.store
      tx.to_
      call.method_name
      call.params
      tx.from
      tx.amount
  in
  if not result.receipt.Contract.success then
    fail effects
      "circle_call_failed"
      (Option.value ~default:"execution reverted" result.receipt.error)
  else
    commit effects backend.store tx.to_ result

let run ~program_trust backend env tx =
  let effects =
    Tx_effects.create
      ~ledger:backend.Octra_core.Epoch_exec.ledger
      ~store:backend.store
  in
  let run_open () =
    match Tx_effects.debit effects tx.Transaction.from tx.ou tx.nonce with
    | Error reason ->
      fail effects "insufficient_balance" reason
    | Ok () ->
      let spec =
        Vm.direct_exec_spec_of_tx
          ~domain:Octra_vm.Receipt_view.Circle_call
          ~reject_domain:Call_plan.Circle_exec
          ~balance:(Tx_effects.balance effects tx.from)
          tx
      in
      begin
        match Octra_vm.Direct_exec.plan spec with
        | Call_plan.Direct_exec_ready call ->
          if Tx_effects.apply effects call.value_effect then
            let open Lwt.Syntax in
            let* executed = execute ~program_trust backend env tx effects call in
            begin
              match executed with
              | Ok () -> Lwt.return (Ok tx.ou)
              | Error _ as error -> Lwt.return error
            end
          else
            fail effects "insufficient_balance" "value transfer rejected"
        | (Call_plan.Direct_exec_invalid_format
          | Call_plan.Direct_exec_insufficient_value) as rejected ->
          reject_plan effects rejected
      end
  in
  Lwt.catch run_open (fun error ->
    fail effects "circle_call_exception" (Printexc.to_string error))

let process_tx ~backend ~env ~program_trust tx =
  match tx.Transaction.op_type with
  | Transaction.CircleCall -> run ~program_trust backend env tx
  | _ -> Octra_core.Epoch_exec.process_standard_tx ~backend ~env tx