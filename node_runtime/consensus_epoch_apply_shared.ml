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


module Transaction = Octra_core.Transaction
module Epoch_exec = Octra_core.Epoch_exec
module Sender = Consensus_epoch_apply_sender
module Sender_live = Consensus_epoch_apply_sender_live

type process = Transaction.t -> (unit, string * string) result Lwt.t

type deps = {
  process : process;
  confirm : Transaction.t -> unit;
  reject : Transaction.t -> error_type:string -> reason:string -> unit;
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
  process_sender : Transaction.t list -> unit Lwt.t;
  confirm_tx : Transaction.t -> unit;
  reject_tx : Transaction.t -> string -> string -> unit;
  notify_confirmed : Transaction.t -> int -> unit;
  notify_rejected : Transaction.t -> string -> unit;
}

type node_runtime = {
  ledger : Octra_core.Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  program_trust : Octra_vm.Program_trust.t;
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
}

type node_result = {
  deferred_stealth_txs : Transaction.t list ref;
}

let is_shared_bft_tx (tx : Transaction.t) =
  match tx.Transaction.op_type with
  | Transaction.Standard
  | Transaction.ValidatorSetUpdate
  | Transaction.ValidatorReady -> true
  | _ -> false

let all_shared_bft txs =
  List.for_all is_shared_bft_tx txs

let canonical_order txs =
  List.sort
    (fun (a : Transaction.t) (b : Transaction.t) ->
       let c = String.compare a.Transaction.from b.Transaction.from in
       if c <> 0 then c else compare a.Transaction.nonce b.Transaction.nonce)
    txs

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
       | Ok () ->
         deps.confirm tx;
         Lwt.return_unit
       | Error (error_type, reason) ->
         deps.reject tx ~error_type ~reason;
         Lwt.return_unit)
    (canonical_order txs)

let run_standard_or_sender ~consensus_mode (deps : standard_or_sender_deps) txs =
  if consensus_mode && all_shared_bft txs then begin
    deps.log_shared ~tx_count:(List.length txs);
    run deps.shared txs
  end else
    Sender.run deps.sender txs

let runtime_shared (runtime : runtime) =
  let backend = lazy (runtime.backend ()) in
  let env = lazy (runtime.env ()) in
  {
    process = (fun tx ->
      process_standard
        ~backend:(Lazy.force backend)
        ~env:(Lazy.force env)
        tx);
    confirm = (fun tx ->
      runtime.confirm_tx tx;
      runtime.notify_confirmed tx (runtime.current_epoch ()));
    reject = (fun tx ~error_type ~reason ->
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

let runtime_deps (runtime : runtime) =
  {
    log_shared = runtime.log_shared;
    shared = runtime_shared runtime;
    sender = runtime_sender runtime;
  }

let run_runtime (runtime : runtime) txs =
  run_standard_or_sender
    ~consensus_mode:runtime.consensus_mode
    (runtime_deps runtime)
    txs

let run_node (runtime : node_runtime) ordered_txs =
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
      }
      sender_txs
  in
  let* () =
    run_runtime
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
        process_sender;
        confirm_tx;
        reject_tx = log_rejected;
        notify_confirmed = runtime.notify_confirmed;
        notify_rejected = runtime.notify_rejected;
      }
      ordered_txs
  in
  Lwt.return { deferred_stealth_txs }