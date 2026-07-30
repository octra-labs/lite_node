(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Call_plan = Octra_vm.Call_plan
module Circle_exec = Octra_circle_runtime.Circle_exec
module Contract = Octra_vm.Contract
module ContractVM = Octra_vm.Contract_vm
module Epoch_exec = Octra_core.Epoch_exec
module Preverify_commit = Octra_core.Preverify_commit
module Preverify_receipt = Octra_core.Preverify_receipt
module Program_trust = Octra_vm.Program_trust
module Receipt_view = Octra_vm.Receipt_view
module Transcript = Octra_core.Circle_hfhe_transcript
module Transaction = Octra_core.Transaction
module Tx_effects = Octra_vm.Tx_effects
module Vm = Consensus_epoch_vm_shell

exception Circle_receipt_mismatch of string

let finish outcome value =
  match !outcome with
  | None ->
    outcome := Some value;
    Lwt.return_unit
  | Some _ ->
    Lwt.fail_with "vm transition completed more than once"

let context ~program_trust backend env (tx : Transaction.t) effects tx_hash =
  Vm.make_live_contract_ctx
    {
      value_journal = Tx_effects.value effects;
      program_journal = Tx_effects.program effects;
      trusted_program_keys = program_trust;
      store = backend.Epoch_exec.store;
      get_fhe_pubkey = Vm.live_fhe_pubkey backend.store;
      current_epoch = env.Epoch_exec.epoch_id;
      epoch_time_ms =
        (match Octra_consensus.Epoch_time.of_seconds env.epoch_ts with
         | Ok value -> value
         | Error _ -> 0L);
      tree_hash = env.prev_state_root;
      node_id = tx.Transaction.to_;
      tx_hash;
    }

let run ?(hfhe_mode=Transcript.Direct) ?circle_capture ?expected_circle
    ?(save_receipt_raw=(fun ~tx_hash:_ ~json:_ -> ()))
    ~program_trust backend env (tx : Transaction.t) =
  let effects =
    Tx_effects.create
      ~ledger:backend.Epoch_exec.ledger
      ~store:backend.store
  in
  let staged_receipts = ref [] in
  let stage_receipt ~tx_hash ~json =
    if List.exists (fun (hash, _) -> String.equal hash tx_hash) !staged_receipts
    then
      raise
        (Tx_effects.Commit_failed
           ("duplicate VM receipt for " ^ tx_hash))
    else
      staged_receipts := (tx_hash, json) :: !staged_receipts
  in
  let stage_direct_receipt ?(program = false) ~tx_hash ~target ~method_name
      receipt =
    stage_receipt
      ~tx_hash
      ~json:
        (Receipt_view.direct_receipt_json
           ~program
           ~epoch:env.Epoch_exec.epoch_id
           ~now:env.epoch_ts
           ~target
           ~method_name
           receipt)
  in
  let flush_receipts () =
    List.rev !staged_receipts
    |> List.iter (fun (tx_hash, json) -> save_receipt_raw ~tx_hash ~json)
  in
  let outcome = ref None in
  let fee_persisted = ref false in
  let discard () = Tx_effects.discard effects in
  let reject ?(consume_nonce = false) ?notify_reason
      ?(persist_state = false) error_type reason =
    ignore consume_nonce;
    ignore notify_reason;
    if persist_state && not !fee_persisted then
      Lwt.fail_with "vm transition fee state missing"
    else begin
      if not persist_state then discard ();
      let value =
        if persist_state then
          Ok
            (Epoch_exec.Rejected_after_fee {
               fee = tx.ou;
               error_type;
               reason;
             })
        else
          Error (error_type, reason)
      in
      finish outcome value
    end
  in
  let discard_fee fee =
    discard ();
    match backend.ops.debit tx.from fee tx.nonce with
    | Error reason ->
      raise
        (Tx_effects.Commit_failed
           ("vm transition fee failed: " ^ reason))
    | Ok () ->
      fee_persisted := true
  in
  let runtime =
    Vm.make_live_call_runtime
      {
        reject;
        debit = Tx_effects.debit effects;
        tx;
        make_ctx = context ~program_trust backend env tx effects;
        balance = Tx_effects.balance effects;
        apply_value_effect = (fun effect ->
          ignore (Tx_effects.apply effects effect));
        discard_effects = discard;
        discard_fee;
        commit_effects = (fun () -> Tx_effects.commit_exn effects);
        confirm = (fun () ->
          finish outcome (Ok (Epoch_exec.Confirmed tx.ou)));
      }
  in
  let deploy_and_save ~admitted ~params ~bytecode ~bytecode_raw =
    let contract_addr, receipt =
      Contract.deploy
        ~trusted:(Program_trust.keys program_trust)
        ?admitted
        ~journal:(Tx_effects.program effects)
        ~ctx:(context
          ~program_trust
          backend
          env
          tx
          effects
          (Transaction.hash tx))
        ~params
        backend.store
        tx.from
        "CUSTOM"
        bytecode
        bytecode_raw
        tx.nonce
    in
    stage_direct_receipt
      ~program:
        (match tx.op_type with
         | Transaction.ProgramDeploy -> true
         | _ -> false)
      ~tx_hash:(Transaction.hash tx)
      ~target:contract_addr
      ~method_name:"constructor"
      receipt;
    {
      Vm.contract_addr;
      receipt;
    }
  in
  let deps : Vm.vm_tx_deps =
    {
      runtime;
      trusted_program_keys = program_trust;
      deploy_balance = (fun current ->
        Option.map
          (fun account -> account.Octra_core.Ledger_types.balance)
          (backend.ops.find_opt current.Transaction.from));
      deploy_and_save = (fun _ ~admitted ~params ~bytecode ~bytecode_raw ->
        deploy_and_save ~admitted ~params ~bytecode ~bytecode_raw);
      program_prepare = Vm.prepare_program_package;
      ensure_account = (fun address ->
        match Tx_effects.ensure_account effects address with
        | Ok () -> ()
        | Error reason -> raise (Tx_effects.Commit_failed reason));
      circle_exec = (fun current ~ctx call ->
        let ctx = { ctx with ContractVM.node_id = current.to_ } in
        let open Lwt.Syntax in
        let* result =
          Circle_exec.execute_call
            ~trusted:(Program_trust.keys program_trust)
            ~ctx
            ~limit:call.Call_plan.effort_limit
            ~hfhe_mode
            backend.store
            current.to_
            call.method_name
            call.params
            current.from
            current.amount
        in
        Option.iter
          (fun capture -> capture := Some result.Circle_exec.hfhe_binding)
          circle_capture;
        begin
          match expected_circle with
          | None -> Lwt.return result
          | Some expected ->
            let expected_binding : Circle_exec.hfhe_binding = {
              circle_id = expected.Preverify_receipt.circle_id;
              code_hash = expected.code_hash;
              stable_root = expected.stable_root;
              public_reads_hash = expected.public_reads_hash;
              context_hash = expected.context_hash;
              transcript = expected.transcript;
            } in
            if
              Circle_exec.hfhe_binding_equal
                expected_binding
                result.hfhe_binding
            then
              Lwt.return result
            else
              Lwt.fail
                (Circle_receipt_mismatch
                   ("circle receipt mismatch for "
                    ^ Transaction.hash current))
        end);
      circle_save = (fun current ~tx_hash call result ->
        stage_direct_receipt
          ~tx_hash
          ~target:current.Transaction.to_
          ~method_name:call.Call_plan.method_name
          result.Circle_exec.receipt);
      circle_commit = (fun current result ->
        Circle_exec.commit_call_result backend.store current.to_ result);
      circle_log_ok = (fun _ _ _ _ -> ());
      program_exec = (fun current ~ctx call ->
        Lwt.return
          (Contract.execute_call
             ~trusted:(Program_trust.keys program_trust)
             ~journal:(Tx_effects.program effects)
             ~ctx
             ~limit:call.effort_limit
             backend.store
             current.to_
             call.method_name
             call.params
             current.from
             current.amount));
      program_save = (fun current ~tx_hash call receipt ->
        stage_direct_receipt
          ~program:true
          ~tx_hash
          ~target:current.Transaction.to_
          ~method_name:call.Call_plan.method_name
          receipt);
      program_log_ok = (fun _ _ _ _ -> ());
      multi_execute_call = (fun ~ctx ~limit ~target ~method_name ~params
          ~caller ~amount ->
        Contract.execute_call
          ~trusted:(Program_trust.keys program_trust)
          ~journal:(Tx_effects.program effects)
          ~ctx
          ~limit
          backend.store
          target
          method_name
          params
          caller
          amount);
      save_receipt_raw = stage_receipt;
      reject_malformed = (fun reason ->
        reject "malformed_transaction" reason);
      max_multi_exec_calls = Vm.max_multi_exec_calls ~env:Sys.getenv_opt;
      epoch = env.epoch_id;
      now = (fun () -> env.epoch_ts);
    }
  in
  let open Lwt.Syntax in
  let* result =
    Lwt.catch
      (fun () ->
        let* () =
          match tx.op_type with
          | Transaction.ContractUpgrade ->
            Vm.run_program_upgrade_tx
              runtime
              ~trusted_program_keys:program_trust
              ~program_journal:(Tx_effects.program effects)
              ~store:backend.store
              tx
          | _ ->
            Vm.run_vm_tx deps tx
        in
        match !outcome with
        | Some value -> Lwt.return value
        | None ->
          discard ();
          Lwt.return
            (Error
               ("vm_transition_incomplete", "VM transition produced no outcome")))
      (fun error ->
        discard ();
        match error with
        | Circle_receipt_mismatch _ -> Lwt.fail error
        | _ ->
          Lwt.return
            (Error
               ("vm_transition_exception", Printexc.to_string error)))
  in
  begin
    match result with
    | Ok _ -> flush_receipts ()
    | Error _ -> ()
  end;
  Lwt.return result

let circle_receipt preverify tx =
  match preverify with
  | None -> Ok None
  | Some gate ->
    begin
      match Preverify_commit.receipt_for_tx gate tx with
      | Error e -> Error e
      | Ok receipt ->
        begin
          match receipt.Preverify_receipt.circle with
          | Some circle -> Ok (Some circle)
          | None -> Error "circle_receipt_binding_missing"
        end
    end

let circle_cell_transition_hash preverify tx =
  match preverify with
  | None -> Error "circle_cell_preverify_receipt_required"
  | Some gate ->
    begin
      match Preverify_commit.receipt_for_tx gate tx with
      | Error error -> Error error
      | Ok receipt ->
        begin
          match receipt.Preverify_receipt.state, receipt.circle with
          | Some { transition_hash = Some transition_hash; _ }, None ->
            Ok transition_hash
          | _ -> Error "circle_cell_transition_binding_missing"
        end
    end

let preverify_circle ~backend ~env ~program_trust tx =
  let open Lwt.Syntax in
  let capture = ref None in
  let* result =
    run
      ~hfhe_mode:Transcript.Capture
      ~circle_capture:capture
      ~program_trust
      backend
      env
      tx
  in
  match result, !capture with
  | Ok (Epoch_exec.Confirmed _), Some binding -> Lwt.return_ok binding
  | Ok (Epoch_exec.Rejected_after_fee rejected), _ ->
    Lwt.return_error
      (rejected.error_type ^ ":" ^ rejected.reason)
  | Error (error_type, reason), _ ->
    Lwt.return_error (error_type ^ ":" ^ reason)
  | Ok (Epoch_exec.Confirmed _), None ->
    Lwt.return_error "circle_receipt_capture_missing"

let process_tx ?preverify ?save_receipt_raw ~backend
    ~(env : Epoch_exec.env) ~program_trust tx =
  match tx.Transaction.op_type with
  | Transaction.CircleBalanceCellPut
  | Transaction.CircleRegisterCellPut ->
    begin
      match circle_cell_transition_hash preverify tx with
      | Error error ->
        Lwt.return_error ("circle_cell_preverify_receipt", error)
      | Ok expected_transition_hash ->
        let open Lwt.Syntax in
        let* result =
          Epoch_exec.process_circle_operation_tx
            ~expected_transition_hash
            ~backend
            ~current_epoch:env.epoch_id
            tx
        in
        Lwt.return
          (Result.map (fun fee -> Epoch_exec.Confirmed fee) result)
    end
  | Transaction.CircleCall ->
    begin
      match circle_receipt preverify tx with
      | Error e ->
        Lwt.return_error ("circle_preverify_receipt", e)
      | Ok None ->
        run ?save_receipt_raw ~program_trust backend env tx
      | Ok (Some circle) ->
        run
          ~hfhe_mode:(Transcript.Consume circle.transcript)
          ~expected_circle:circle
          ?save_receipt_raw
          ~program_trust
          backend
          env
          tx
    end
  | Transaction.ContractDeploy
  | Transaction.ProgramDeploy
  | Transaction.ContractCall
  | Transaction.ProgramExec
  | Transaction.MultiExec
  | Transaction.ContractUpgrade ->
    run ?save_receipt_raw ~program_trust backend env tx
  | Transaction.CircleDeploy
  | Transaction.CircleProgramUpdate ->
    let open Lwt.Syntax in
    let* admitted =
      Consensus_circle_code_admission.admit
        ~store:backend.Epoch_exec.store
        ~program_trust
        tx
    in
    begin
      match admitted with
      | Error error -> Lwt.return_error error
      | Ok () ->
        let* result = Epoch_exec.process_standard_tx ~backend ~env tx in
        Lwt.return
          (Result.map (fun fee -> Epoch_exec.Confirmed fee) result)
    end
  | _ ->
    let open Lwt.Syntax in
    let* result = Epoch_exec.process_standard_tx ~backend ~env tx in
    Lwt.return (Result.map (fun fee -> Epoch_exec.Confirmed fee) result)