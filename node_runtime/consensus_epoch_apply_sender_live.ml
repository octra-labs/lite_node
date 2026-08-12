(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Sender = Consensus_epoch_apply_sender
module Transaction = Octra_core.Transaction

type deps = {
  ledger : Octra_core.Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  program_trust : Octra_vm.Program_trust.t;
  wallet_addr : string;
  pre_state_hash : string;
  fold : int -> (Octra_core.Epoch_exec.fold_ctx, string) result;
  standard_env : unit -> Octra_core.Epoch_exec.env;
  current_epoch : unit -> int;
  max_fhe_per_epoch : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  stealth_inline_verify_allowed : bool;
  fhe_in_epoch_counter : int ref;
  stealth_in_epoch_counter : int ref;
  stealth_defer_count : (string, int) Hashtbl.t;
  deferred_stealth_txs : Transaction.t list ref;
  confirmed_fees : Z.t ref;
  short_addr : string -> string;
  log_rejected : Transaction.t -> string -> string -> unit;
  confirm_tx : Transaction.t -> unit;
  notify_new_account : string -> unit;
  notify_confirmed : Transaction.t -> int -> unit;
  notify_rejected : Transaction.t -> string -> unit;
  legacy_replay :
    epoch:int ->
    address:string ->
    cipher:string ->
    Octra_core.Pvac_legacy_public_replay.decision;
  private_field_policy : Octra_core.Private_ledger.field_policy;
  private_result_policy :
    int ->
    Octra_core.Private_result_policy.t;
}

let trace_enc_balance ~short op_name addr current_str new_str =
  Log.trace "enc_bal" "event = balance_cipher op = %s addr = %s cur_bytes = %d new_bytes = %d"
    op_name
    (short addr)
    (String.length current_str)
    (String.length new_str)

let run deps sender_txs =
  Octra_core.Chaos.inject "process_sender_txs:enter";
  let had_encrypted_debit = ref false in
  let handle_sender_tx (raw_ctx : Sender.tx_context) =
    let open Lwt.Syntax in
    let completed = ref false in
    let accepted = ref false in
    let persist_state = ref false in
    let complete ~accept ~persist =
      if !completed then
        failwith "sender transaction completed more than once"
      else begin
        completed := true;
        accepted := accept;
        persist_state := persist
      end
    in
    let ctx : Sender.tx_context = {
      tx = raw_ctx.tx;
      confirm = (fun () ->
        complete ~accept:true ~persist:true;
        raw_ctx.confirm ());
      reject = (fun ?consume_nonce ?notify_reason ?(persist_state = false)
          tag reason ->
        complete ~accept:false ~persist:persist_state;
        raw_ctx.reject
          ?consume_nonce
          ?notify_reason
          ~persist_state
          tag
          reason);
      continue_after_reject = (fun ~consume_nonce ->
        complete ~accept:false ~persist:false;
        raw_ctx.continue_after_reject ~consume_nonce);
    } in
    let tx = ctx.tx in
    let open Transaction in
    let vm_value =
      Consensus_epoch_vm_shell.make_live_value_effects
        {
          ledger = deps.ledger;
          store = deps.store;
          add_fee = (fun fee ->
            deps.confirmed_fees := Z.add !(deps.confirmed_fees) fee);
        }
    in
    let confirm_current_tx = ctx.confirm in
    let reject_current_tx = ctx.reject in
    let reject_private_gate (r : Consensus_epoch_apply_private_gate.reject) =
      reject_current_tx r.tag r.reason
    in
    let fhe_gate () =
      Consensus_epoch_apply_private_gate.fhe_cap
        ~current:!(deps.fhe_in_epoch_counter)
        ~max:deps.max_fhe_per_epoch
    in
    let debit_gate () =
      Consensus_epoch_apply_private_gate.encrypted_debit_limit
        ~had_debit:!had_encrypted_debit
    in
    let reject_vm ?consume_nonce ?notify_reason ?persist_state tag reason =
      reject_current_tx
        ?consume_nonce
        ?notify_reason
        ?persist_state
        tag
        reason
    in
    let vm_tx_deps =
      Consensus_epoch_vm_shell.make_live_sender_vm_tx_deps
        Consensus_epoch_vm_shell.{
          value_journal = vm_value.value_journal;
          program_journal = vm_value.program_journal;
          trusted_program_keys = deps.program_trust;
          ledger = deps.ledger;
          store = deps.store;
          chaindata = deps.chaindata;
          tx;
          current_epoch = deps.current_epoch;
          epoch_time_ms =
            (match Octra_consensus.Epoch_time.of_seconds
               (deps.standard_env ()).Octra_core.Epoch_exec.epoch_ts with
             | Ok value -> value
             | Error _ -> 0L);
          pre_state_hash = deps.pre_state_hash;
          node_id = deps.wallet_addr;
          reject = reject_vm;
          balance = vm_value.balance;
          ensure_account = vm_value.ensure_account;
          apply_value_effect = vm_value.apply;
          discard_effects = vm_value.discard;
          discard_fee = vm_value.discard_fee;
          commit_effects = vm_value.commit;
          confirm = confirm_current_tx;
        }
    in
    let epoch_exec_deps =
      Sender.live_epoch_exec_deps
        ~backend:(fun () ->
          Octra_core.Epoch_exec.make_live_backend
            ~fold:deps.fold
            deps.store
            deps.ledger)
        ~standard_env:deps.standard_env
        ~reject:reject_current_tx
        ~confirm:confirm_current_tx
    in
    let public_tx_deps =
      Sender.live_public_deps
        ~apply:(Octra_core.Ledger_apply.apply_epoch_public deps.ledger)
        ~notify_created:deps.notify_new_account
        ~reject:(fun ~notify_reason tag reason ->
          reject_current_tx ~notify_reason tag reason)
        ~confirm:confirm_current_tx
        ~short:deps.short_addr
    in
    let handle_circle_tx () =
      let* admitted =
        Consensus_circle_code_admission.admit
          ~store:deps.store
          ~program_trust:deps.program_trust
          tx
      in
      match admitted with
      | Error (tag, reason) ->
        reject_current_tx tag reason
      | Ok () ->
        Sender.run_circle_epoch_exec
          epoch_exec_deps
          ~process:(fun ~backend ~current_epoch tx ->
            Octra_core.Epoch_exec.process_circle_operation_tx
              ~backend
              ~current_epoch
              tx)
          ~current_epoch:(deps.current_epoch ())
          tx
    in
    let balance_op_deps ~gate ~plan ~mark_debit ~log_failure =
      Consensus_epoch_private_balance_shell.live_ledger_balance_op_deps
        Consensus_epoch_private_balance_shell.{
          ledger = deps.ledger;
          tx;
          gate;
          plan;
          log_failure;
          trace_cipher = trace_enc_balance ~short:deps.short_addr;
          mark_debit;
          incr_fhe = (fun () -> incr deps.fhe_in_epoch_counter);
          reject_gate = reject_private_gate;
          reject_failure = (fun e ->
            reject_current_tx ~notify_reason:e.user_reason e.tag e.reason);
          short_addr = deps.short_addr;
          confirm = confirm_current_tx;
        }
    in
    let* _ =
      Octra_core.Tx_savepoint.run
        ~ledger:deps.ledger
        ~store:deps.store
        (fun () ->
          let* () =
            match tx.op_type with
            | CircleDeploy | CircleProgramUpdate | CircleAssetPut
            | CircleAssetPutEncrypted | CircleSealedSlotPut
            | CircleSlotPolicyPut | CircleStateDescriptorPut
            | CircleBalanceCellPut | CircleRegisterCellPut
            | CircleTransportPolicyPut | CircleHfhePolicyPut
            | CircleKeyPolicyPut | CircleKeyGrant | CircleKeyExtend
            | CircleKeyRevoke | CircleKeyErase | CircleOutboxOpen
            | CircleRelayClaim | CircleRelayCancel | CircleIngressCommit ->
              handle_circle_tx ()
            | CircleCall | ContractDeploy | ProgramDeploy | ContractCall
            | ProgramExec | MultiExec ->
              Consensus_epoch_vm_shell.run_vm_tx vm_tx_deps tx
            | EncryptOp when tx.from = tx.to_ ->
              Consensus_epoch_private_balance_shell.run_encrypt
                (balance_op_deps
                   ~gate:fhe_gate
                   ~plan:(fun () ->
                     Octra_core.Private_ledger.apply_encrypt
                       ~field_policy:deps.private_field_policy
                       ~result_policy:
                         (deps.private_result_policy (deps.current_epoch ()))
                       deps.ledger
                       tx)
                   ~mark_debit:ignore
                   ~log_failure:(fun e ->
                     if
                       String.equal
                         e.Octra_core.Private_ledger.tag
                         "encrypt_balance_failed"
                     then
                       Log.error "epoch"
                         "event = encrypt_balance_failed addr = %s reason = %s"
                         (deps.short_addr tx.from)
                         e.reason))
            | DecryptOp when tx.from = tx.to_ ->
              Consensus_epoch_private_balance_shell.run_decrypt
                (balance_op_deps
                   ~gate:(fun () ->
                     Consensus_epoch_apply_private_gate.first_reject [
                       debit_gate ();
                       fhe_gate ();
                     ])
                   ~plan:(fun () ->
                     Octra_core.Private_ledger.apply_decrypt
                       ~field_policy:deps.private_field_policy
                       ~result_policy:
                         (deps.private_result_policy (deps.current_epoch ()))
                       deps.ledger
                       tx)
                   ~mark_debit:(fun () -> had_encrypted_debit := true)
                   ~log_failure:ignore)
            | KeySwitch when tx.from = tx.to_ ->
              Consensus_epoch_key_switch_shell.run_live_ledger_tx
                {
                  ledger = deps.ledger;
                  field_policy = deps.private_field_policy;
                  legacy_replay = (fun address ->
                    let cipher =
                      match Octra_core.Ledger.find_opt deps.ledger address with
                      | Some account ->
                        Option.value
                          ~default:"0"
                          account.Octra_core.Ledger_types.encrypted_balance
                      | None -> "0"
                    in
                    deps.legacy_replay
                      ~epoch:(deps.current_epoch ())
                      ~address
                      ~cipher);
                  gate = fhe_gate;
                  reject_gate = reject_private_gate;
                  record_rejected = deps.log_rejected;
                  continue_after_reject = ctx.continue_after_reject;
                  short_addr = deps.short_addr;
                  incr_fhe = (fun () -> incr deps.fhe_in_epoch_counter);
                  confirm = confirm_current_tx;
                }
                tx
            | EncryptOp | DecryptOp | KeySwitch
            | RecryptOp | Standard | Op01Burn | PrivateOp ->
              Sender.run_public_tx public_tx_deps tx
            | StealthOp ->
              Consensus_epoch_stealth_shell.run_tx
                (Consensus_epoch_stealth_shell.live_ledger_tx_deps
                {
                  ledger = deps.ledger;
                  field_policy = deps.private_field_policy;
                  stealth_count = !(deps.stealth_in_epoch_counter);
                  max_stealth_per_epoch = deps.max_stealth_per_epoch;
                  max_stealth_defer = deps.max_stealth_defer;
                  inline_verify_allowed = deps.stealth_inline_verify_allowed;
                  current_epoch = deps.current_epoch;
                  private_result_policy = deps.private_result_policy;
                  debit_gate;
                  fhe_gate;
                  defer_count = (fun tx_hash ->
                    match Hashtbl.find_opt deps.stealth_defer_count tx_hash with
                    | Some count -> count
                    | None -> 0);
                  set_defer_count = Hashtbl.replace deps.stealth_defer_count;
                  clear_defer_count = Hashtbl.remove deps.stealth_defer_count;
                  defer_tx = (fun stealth_tx ->
                    deps.deferred_stealth_txs :=
                      stealth_tx :: !(deps.deferred_stealth_txs);
                    ctx.continue_after_reject ~consume_nonce:true);
                  reject = (fun tag reason -> reject_current_tx tag reason);
                  trace_cipher = trace_enc_balance ~short:deps.short_addr;
                  short_addr = deps.short_addr;
                  mark_debit = (fun () -> had_encrypted_debit := true);
                  incr_stealth = (fun () -> incr deps.stealth_in_epoch_counter);
                  incr_fhe = (fun () -> incr deps.fhe_in_epoch_counter);
                  confirm = confirm_current_tx;
                })
                tx
            | ClaimOp ->
              Consensus_epoch_claim_shell.run_tx
                (Consensus_epoch_claim_shell.live_ledger_tx_deps
                {
                  ledger = deps.ledger;
                  field_policy = deps.private_field_policy;
                  private_result_policy = deps.private_result_policy;
                  current_epoch = deps.current_epoch;
                  trace_cipher = trace_enc_balance ~short:deps.short_addr;
                  short_addr = deps.short_addr;
                  reject = (fun tag reason -> reject_current_tx tag reason);
                  confirm = confirm_current_tx;
                })
                tx
            | ValidatorSetUpdate | ValidatorReady
            | ValidatorBond | ValidatorExit | ValidatorWithdraw
            | ValidatorEvidence ->
              Sender.run_standard_epoch_exec
                epoch_exec_deps
                ~process:Octra_core.Epoch_exec.process_standard_tx
                tx
            | ContractUpgrade ->
              Consensus_epoch_vm_shell.run_program_upgrade_tx
                vm_tx_deps.runtime
                ~trusted_program_keys:deps.program_trust
                ~program_journal:vm_value.program_journal
                ~store:deps.store
                tx
          in
          if not !completed then
            Lwt.fail_with "sender transaction did not complete"
          else
            Lwt.return
              (Sender.savepoint_result
                 ~accepted:!accepted
                 ~persist_state:!persist_state))
    in
    Lwt.return_unit
  in
  Sender.run_nonce_loop
    ~account_nonce:(fun addr ->
      Octra_core.Ledger.find_opt deps.ledger addr
      |> Option.map (fun acc -> acc.Octra_core.Ledger.nonce))
    ~nonce_mismatch:(fun tx ~expected ~got ->
      deps.log_rejected tx "nonce_mismatch"
        (Sender.nonce_mismatch_reason ~expected ~got);
      deps.notify_rejected tx "Nonce mismatch";
      Lwt.return_unit)
    ~confirm:(fun tx ->
      deps.confirm_tx tx;
      deps.notify_confirmed tx (deps.current_epoch ());
      Lwt.return_unit)
    ~reject:(fun tx ~notify_reason ~error_type ~reason ->
      deps.log_rejected tx error_type reason;
      deps.notify_rejected tx (Option.value ~default:reason notify_reason);
      Lwt.return_unit)
    ~handle:handle_sender_tx
    sender_txs