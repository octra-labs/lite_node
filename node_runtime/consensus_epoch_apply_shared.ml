(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module Epoch_exec = Octra_core.Epoch_exec
module Sender = Consensus_epoch_apply_sender
module Sender_live = Consensus_epoch_apply_sender_live

type process =
  Transaction.t ->
  (Epoch_exec.tx_effect, string * string) result Lwt.t

type deps = {
  process : process;
  confirm : Transaction.t -> unit;
  reject : Transaction.t -> error_type:string -> reason:string -> unit;
  reject_after_fee :
    Transaction.t ->
    fee:Z.t ->
    error_type:string ->
    reason:string ->
    unit;
}

type standard_or_sender_deps = {
  log_shared : tx_count:int -> unit;
  shared : deps;
  sender : Sender.deps;
}

type runtime = {
  consensus_mode : bool;
  current_epoch : unit -> int;
  log_shared : tx_count:int -> unit;
  short : string -> string;
  fatal : string -> unit;
  exit : unit -> unit;
  backend : unit -> Epoch_exec.backend;
  env : unit -> Epoch_exec.env;
  max_fhe_per_epoch : int;
  max_stealth_per_epoch : int;
  process_sender : Transaction.t list -> unit Lwt.t;
  confirm_tx : Transaction.t -> unit;
  reject_tx : Transaction.t -> string -> string -> unit;
  notify_confirmed : Transaction.t -> int -> unit;
  notify_rejected : Transaction.t -> string -> unit;
  program_trust : Octra_vm.Program_trust.t;
  circle_mode : Octra_core.Rule_graph.mode;
  legacy_replay :
    epoch:int ->
    address:string ->
    cipher:string ->
    Octra_core.Pvac_legacy_public_replay.decision;
  private_result_policy :
    int ->
    Octra_core.Private_result_policy.t;
  add_rejected_fee : Z.t -> unit;
  advance_validator_set : unit -> unit Lwt.t;
}

type node_runtime = {
  ledger : Octra_core.Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  program_trust : Octra_vm.Program_trust.t;
  rules : Octra_core.Rule_graph.t;
  wallet_addr : string;
  pre_state_hash : string;
  standard_env : unit -> Epoch_exec.env;
  current_epoch : unit -> int;
  consensus_mode : bool;
  max_fhe_per_epoch : int;
  max_stealth_per_epoch : int;
  max_stealth_defer : int;
  stealth_inline_verify_allowed : bool;
  fhe_in_epoch_counter : int ref;
  stealth_in_epoch_counter : int ref;
  stealth_defer_count : (string, int) Hashtbl.t;
  pending_tx_saves : (Transaction.t * int) list ref;
  total_tx_count : int ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
  short : string -> string;
  log_shared : tx_count:int -> unit;
  fatal : string -> unit;
  exit : unit -> unit;
  notify_new_account : string -> unit;
  notify_confirmed : Transaction.t -> int -> unit;
  notify_rejected : Transaction.t -> string -> unit;
  legacy_replay :
    epoch:int ->
    address:string ->
    cipher:string ->
    Octra_core.Pvac_legacy_public_replay.decision;
  private_result_policy :
    int ->
    Octra_core.Private_result_policy.t;
}

type node_result = {
  deferred_stealth_txs : Transaction.t list ref;
}

let is_shared_bft_tx (tx : Transaction.t) =
  Transaction.bft_consensus_admits_op tx.Transaction.op_type

let all_shared_bft txs =
  List.for_all is_shared_bft_tx txs

let first_disabled_bft_tx txs =
  List.find_opt
    (fun tx -> not (is_shared_bft_tx tx))
    txs

let consensus_order txs =
  Transaction.consensus_order txs

let process_standard ~backend ~env tx =
  let open Lwt.Syntax in
  let* result = Epoch_exec.process_standard_tx ~backend ~env tx in
  match result with
  | Ok _ -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error e)

let run deps txs =
  Lwt_list.iter_s
    (fun tx ->
       let open Lwt.Syntax in
       let* result = deps.process tx in
       match result with
       | Ok (Epoch_exec.Confirmed _) ->
         deps.confirm tx;
         Lwt.return_unit
       | Ok (Epoch_exec.Rejected_after_fee rejected) ->
         deps.reject_after_fee
           tx
           ~fee:rejected.fee
           ~error_type:rejected.error_type
           ~reason:rejected.reason;
         Lwt.return_unit
       | Error (error_type, reason) ->
         deps.reject tx ~error_type ~reason;
         Lwt.return_unit)
    (consensus_order txs)

let run_standard_or_sender ~consensus_mode (deps : standard_or_sender_deps) txs =
  match consensus_mode, first_disabled_bft_tx txs with
  | true, Some tx ->
    Lwt.fail_with
      (Transaction.bft_reject_reason tx.Transaction.op_type)
  | true, None ->
    deps.log_shared ~tx_count:(List.length txs);
    run deps.shared txs
  | false, _ ->
    Sender.run deps.sender txs

let runtime_shared ?preverify ?save_receipt_raw (runtime : runtime) =
  let backend = lazy (runtime.backend ()) in
  let env = lazy (runtime.env ()) in
  let private_transition =
    lazy
      (Octra_core.Private_transition.create
        ~preverify
        ~ledger:(Lazy.force backend).Epoch_exec.ledger
        ~epoch_id:(Lazy.force env).Epoch_exec.epoch_id
        ~result_policy:
          (runtime.private_result_policy
             (Lazy.force env).Epoch_exec.epoch_id)
        ~legacy_replay:runtime.legacy_replay
        ~limits:Octra_core.Private_transition.{
          max_fhe = runtime.max_fhe_per_epoch;
          max_stealth = runtime.max_stealth_per_epoch;
        })
  in
  {
    process = (fun tx ->
      let open Lwt.Syntax in
      let* result =
        Octra_core.Tx_savepoint.run
          ~ledger:(Lazy.force backend).Epoch_exec.ledger
          ~store:(Lazy.force backend).Epoch_exec.store
          (fun () ->
            let* result =
              if Transaction.bft_crypto_active ()
                && Transaction.bft_crypto_op tx.Transaction.op_type
              then
                let* private_result =
                  Octra_core.Private_transition.process
                    (Lazy.force private_transition)
                    ~backend:(Lazy.force backend)
                    ~env:(Lazy.force env)
                    tx in
                Lwt.return
                  (Result.map
                     (fun fee -> Epoch_exec.Confirmed fee)
                     private_result)
              else
                Consensus_vm_transition.process_tx
                  ?preverify
                  ?save_receipt_raw
                  ~backend:(Lazy.force backend)
                  ~env:(Lazy.force env)
                  ~circle_mode:runtime.circle_mode
                  ~program_trust:runtime.program_trust
                  tx
            in
            begin
              match result with
              | Ok (Epoch_exec.Confirmed _) ->
                Epoch_exec.register_confirmed_sender_key
                  ~backend:(Lazy.force backend)
                  ~env:(Lazy.force env)
                  tx
              | Ok (Epoch_exec.Rejected_after_fee _)
              | Error _ -> ()
            end;
            Lwt.return result)
      in
      Lwt.return result);
    confirm = (fun tx ->
      runtime.confirm_tx tx;
      runtime.notify_confirmed tx (runtime.current_epoch ()));
    reject = (fun tx ~error_type ~reason ->
      runtime.reject_tx tx error_type reason;
      runtime.notify_rejected tx reason);
    reject_after_fee = (fun tx ~fee ~error_type ~reason ->
      runtime.add_rejected_fee fee;
      runtime.reject_tx tx error_type reason;
      runtime.notify_rejected tx reason);
  }

let runtime_sender (runtime : runtime) =
  {
    Sender.process = runtime.process_sender;
    fatal = (fun ~sender exn ->
      Sender.fatal_lines
        ~short:runtime.short
        ~epoch_id:(runtime.current_epoch ())
        ~sender
        exn
      |> List.iter runtime.fatal;
      runtime.exit ());
  }

let runtime_deps ?preverify ?save_receipt_raw (runtime : runtime) =
  {
    log_shared = runtime.log_shared;
    shared = runtime_shared ?preverify ?save_receipt_raw runtime;
    sender = runtime_sender runtime;
  }

let run_runtime ?preverify ?save_receipt_raw (runtime : runtime) txs =
  let open Lwt.Syntax in
  let* () =
    run_standard_or_sender
      ~consensus_mode:runtime.consensus_mode
      (runtime_deps ?preverify ?save_receipt_raw runtime)
      txs
  in
  runtime.advance_validator_set ()

let run_node ?preverify (runtime : node_runtime) ordered_txs =
  let open Lwt.Syntax in
  let tx_sink =
    Sender.live_tx_sink
      {
        chaindata = runtime.chaindata;
        current_epoch = runtime.current_epoch;
        pending_tx_saves = runtime.pending_tx_saves;
        total_tx_count = runtime.total_tx_count;
        confirmed_fees = runtime.confirmed_fees;
        processed_hashes = runtime.processed_hashes;
      }
  in
  let log_rejected tx error_type reason =
    tx_sink.reject tx error_type reason
  in
  let confirm_tx tx =
    tx_sink.confirm tx
  in
  let deferred_stealth_txs = ref [] in
  let process_sender sender_txs =
    Sender_live.run
      {
        ledger = runtime.ledger;
        store = runtime.store;
        chaindata = runtime.chaindata;
        program_trust = runtime.program_trust;
        wallet_addr = runtime.wallet_addr;
        pre_state_hash = runtime.pre_state_hash;
        standard_env = runtime.standard_env;
        current_epoch = runtime.current_epoch;
        max_fhe_per_epoch = runtime.max_fhe_per_epoch;
        max_stealth_per_epoch = runtime.max_stealth_per_epoch;
        max_stealth_defer = runtime.max_stealth_defer;
        stealth_inline_verify_allowed = runtime.stealth_inline_verify_allowed;
        fhe_in_epoch_counter = runtime.fhe_in_epoch_counter;
        stealth_in_epoch_counter = runtime.stealth_in_epoch_counter;
        stealth_defer_count = runtime.stealth_defer_count;
        deferred_stealth_txs;
        confirmed_fees = runtime.confirmed_fees;
        short_addr = runtime.short;
        log_rejected;
        confirm_tx;
        notify_new_account = runtime.notify_new_account;
        notify_confirmed = runtime.notify_confirmed;
        notify_rejected = runtime.notify_rejected;
        legacy_replay = runtime.legacy_replay;
        private_result_policy = runtime.private_result_policy;
      }
      sender_txs
  in
  let* gate =
    match preverify with
    | None -> Lwt.return_ok ()
    | Some gate ->
      Octra_core.Preverify_commit.check_bound
        runtime.ledger
        gate
        ordered_txs
  in
  match gate with
  | Error e ->
    Lwt.fail_with ("finalized preverify gate failed: " ^ e)
  | Ok () ->
    let circle_mode =
      match
        Octra_core.Rule_graph.circle
          runtime.rules
          ~epoch:(runtime.current_epoch ())
      with
      | Ok mode -> mode
      | Error fault ->
        failwith (Octra_core.Rule_graph.fault_message fault)
    in
    let* () =
      run_runtime
        ?preverify
        ~save_receipt_raw:(fun ~tx_hash ~json ->
          Octra_core.Store_chaindata.save_receipt_raw
            runtime.chaindata
            ~tx_hash
            ~json)
        {
        consensus_mode = runtime.consensus_mode;
        current_epoch = runtime.current_epoch;
        log_shared = runtime.log_shared;
        short = runtime.short;
        fatal = runtime.fatal;
        exit = runtime.exit;
        backend = (fun () ->
          Epoch_exec.make_live_backend runtime.store runtime.ledger);
        env = runtime.standard_env;
        max_fhe_per_epoch = runtime.max_fhe_per_epoch;
        max_stealth_per_epoch = runtime.max_stealth_per_epoch;
        process_sender;
        confirm_tx;
        reject_tx = log_rejected;
        notify_confirmed = runtime.notify_confirmed;
        notify_rejected = runtime.notify_rejected;
        program_trust = runtime.program_trust;
        circle_mode;
        legacy_replay = runtime.legacy_replay;
        private_result_policy = runtime.private_result_policy;
        add_rejected_fee = (fun fee ->
          runtime.confirmed_fees := Z.add !(runtime.confirmed_fees) fee);
        advance_validator_set = (fun () ->
          let backend =
            Epoch_exec.make_live_backend runtime.store runtime.ledger
          in
          Epoch_exec.advance_validator_set
            ~backend
            ~env:(runtime.standard_env ()));
      }
        ordered_txs
    in
    Lwt.return { deferred_stealth_txs }