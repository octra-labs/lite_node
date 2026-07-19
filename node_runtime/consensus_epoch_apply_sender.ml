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

type batch = {
  sender : string;
  txs : Transaction.t list;
}

type deps = {
  process : Transaction.t list -> unit Lwt.t;
  fatal : sender:string -> exn -> unit;
}

type tx_context = {
  tx : Transaction.t;
  confirm : unit -> unit Lwt.t;
  reject :
    ?consume_nonce:bool ->
    ?notify_reason:string ->
    string ->
    string ->
    unit Lwt.t;
  continue_after_reject : consume_nonce:bool -> unit Lwt.t;
}

type rejected_record = {
  hash : string;
  from_addr : string;
  to_addr : string;
  amount : string;
  nonce : int;
  error_type : string;
  reason : string;
  epoch_id : int;
  ts : float;
}

type confirmed_record = {
  hash : string;
  tx_json : string;
  from_addr : string;
  to_addr : string;
  op_type : string;
  encrypted_data : string;
  message : string;
  fee : Z.t;
}

type tx_sink_refs = {
  pending_tx_saves : (Transaction.t * int) list ref;
  total_tx_count : int ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
}

type tx_sink_effects = {
  now : unit -> float;
  epoch : unit -> int;
  save_rejected : rejected_record -> unit;
  save_tx : confirmed_record -> epoch_id:int -> unit;
  record_tx : Transaction.t -> unit;
  warn_rejected : string -> unit;
}

type tx_sink = {
  reject : Transaction.t -> string -> string -> unit;
  confirm : Transaction.t -> unit;
}

type live_tx_sink_deps = {
  chaindata : Octra_core.Store_chaindata.t;
  current_epoch : unit -> int;
  pending_tx_saves : (Transaction.t * int) list ref;
  total_tx_count : int ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
}

type ('backend, 'env) epoch_exec_deps = {
  backend : unit -> 'backend;
  standard_env : unit -> 'env;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type public_deps = {
  apply : Transaction.t -> Octra_core.Ledger_apply.outcome;
  notify_created : string -> unit;
  reject : notify_reason:string -> string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
  log_burn : Transaction.t -> unit;
}

let live_epoch_exec_deps ~backend ~standard_env ~reject ~confirm =
  {
    backend;
    standard_env;
    reject;
    confirm;
  }

let nonce_order txs =
  List.sort
    (fun (a : Transaction.t) (b : Transaction.t) ->
       compare a.Transaction.nonce b.Transaction.nonce)
    txs

let group txs =
  let by_sender = Hashtbl.create 50 in
  List.iter
    (fun tx ->
       let sender = tx.Transaction.from in
       let txs =
         match Hashtbl.find_opt by_sender sender with
         | Some txs -> txs
         | None -> []
       in
       Hashtbl.replace by_sender sender (tx :: txs))
    txs;
  Hashtbl.fold
    (fun sender txs acc -> { sender; txs = nonce_order txs } :: acc)
    by_sender
    []
  |> List.sort (fun a b -> String.compare a.sender b.sender)

let short_hash hash =
  String.sub hash 0 (min 16 (String.length hash))

let rejected_line ~hash ~error_type ~reason =
  Printf.sprintf
    "event = tx_rejected hash = %s type = %s reason = %s"
    (short_hash hash)
    error_type
    reason

let rejected_record ~epoch_id ~ts ~error_type ~reason (tx : Transaction.t) =
  {
    hash = Transaction.hash tx;
    from_addr = tx.from;
    to_addr = tx.to_;
    amount = Z.to_string tx.amount;
    nonce = tx.nonce;
    error_type;
    reason;
    epoch_id;
    ts;
  }

let confirmed_record (tx : Transaction.t) =
  {
    hash = Transaction.hash tx;
    tx_json = Yojson.Safe.to_string (Transaction.to_yojson tx);
    from_addr = tx.from;
    to_addr = tx.to_;
    op_type = Transaction.op_type_to_string tx.op_type;
    encrypted_data = Option.value ~default:"" tx.encrypted_data;
    message = Option.value ~default:"" tx.message;
    fee = tx.ou;
  }

let make_tx_sink (refs : tx_sink_refs) (effects : tx_sink_effects) =
  let reject tx error_type reason =
    let rejected =
      rejected_record
        ~epoch_id:(effects.epoch ())
        ~ts:(effects.now ())
        ~error_type
        ~reason
        tx
    in
    effects.save_rejected rejected;
    refs.processed_hashes := rejected.hash :: !(refs.processed_hashes);
    effects.warn_rejected
      (rejected_line
         ~hash:rejected.hash
         ~error_type:rejected.error_type
         ~reason:rejected.reason)
  in
  let confirm tx =
    let confirmed = confirmed_record tx in
    let epoch_id = effects.epoch () in
    effects.save_tx confirmed ~epoch_id;
    refs.pending_tx_saves := (tx, epoch_id) :: !(refs.pending_tx_saves);
    effects.record_tx tx;
    incr refs.total_tx_count;
    refs.confirmed_fees := Z.add !(refs.confirmed_fees) confirmed.fee;
    refs.processed_hashes := confirmed.hash :: !(refs.processed_hashes)
  in
  {
    reject;
    confirm;
  }

let live_tx_sink deps =
  make_tx_sink
    ({
      pending_tx_saves = deps.pending_tx_saves;
      total_tx_count = deps.total_tx_count;
      confirmed_fees = deps.confirmed_fees;
      processed_hashes = deps.processed_hashes;
    } : tx_sink_refs)
    ({
      now = Unix.gettimeofday;
      epoch = deps.current_epoch;
      save_rejected = (fun rejected ->
        Octra_core.Store_chaindata.save_rejected deps.chaindata
          ~hash:rejected.hash
          ~from_addr:rejected.from_addr
          ~to_addr:rejected.to_addr
          ~amount:rejected.amount
          ~nonce:rejected.nonce
          ~error_type:rejected.error_type
          ~reason:rejected.reason
          ~epoch_id:rejected.epoch_id
          ~ts:rejected.ts);
      save_tx = (fun confirmed ~epoch_id ->
        Octra_core.Store_chaindata.save_tx deps.chaindata
          ~hash:confirmed.hash
          ~epoch_id
          ~from_addr:confirmed.from_addr
          ~to_addr:confirmed.to_addr
          ~tx_json:confirmed.tx_json
          ~op_type:confirmed.op_type
          ~encrypted_data:confirmed.encrypted_data
          ~message:confirmed.message);
      record_tx = Octra_core.Metrics.record_tx;
      warn_rejected = Log.warn "tx" "%s";
    } : tx_sink_effects)

let nonce_mismatch_reason ~expected ~got =
  Printf.sprintf "expected_nonce = %d got_nonce = %d" expected got

let next_nonce ~expected ~consume =
  if consume then expected + 1 else expected

let initial_nonce ~account_nonce = function
  | [] -> None
  | tx :: _ ->
    Some (
      match account_nonce tx.Transaction.from with
      | Some nonce -> nonce + 1
      | None -> 1)

let run_nonce_loop ~account_nonce ~nonce_mismatch ~confirm ~reject ~handle txs =
  let rec loop expected_nonce = function
    | [] ->
      Lwt.return_unit
    | tx :: rest ->
      if tx.Transaction.nonce <> expected_nonce then
        let open Lwt.Syntax in
        let* () =
          nonce_mismatch tx ~expected:expected_nonce ~got:tx.Transaction.nonce
        in
        loop expected_nonce rest
      else
        let confirm () =
          let open Lwt.Syntax in
          let* () = confirm tx in
          loop (expected_nonce + 1) rest
        in
        let continue_after_reject ~consume_nonce =
          loop (next_nonce ~expected:expected_nonce ~consume:consume_nonce) rest
        in
        let reject ?(consume_nonce = false) ?notify_reason error_type reason =
          let open Lwt.Syntax in
          let* () = reject tx ~notify_reason ~error_type ~reason in
          continue_after_reject ~consume_nonce
        in
        handle {
          tx;
          confirm;
          reject;
          continue_after_reject;
        }
  in
  match initial_nonce ~account_nonce txs with
  | None -> Lwt.return_unit
  | Some expected -> loop expected txs

let handle_public_result ~notify_created ~reject ~confirm ~log_burn = function
  | Octra_core.Ledger_apply.Rejected r ->
    Option.iter notify_created r.created_account;
    reject ~notify_reason:r.notify_reason r.tag r.reason
  | Octra_core.Ledger_apply.Accepted a ->
    Option.iter notify_created a.created_account;
    begin
      match a.kind with
      | Octra_core.Ledger_apply.Applied_standard -> ()
      | Octra_core.Ledger_apply.Applied_op01_burn -> log_burn ()
    end;
    confirm ()

let log_op01_burn ~short (tx : Transaction.t) =
  Log.info "op01_burn" "event = op01_burn amount = %s signer = %s"
    (Z.to_string tx.amount)
    (short tx.from)

let live_public_deps ~apply ~notify_created ~reject ~confirm ~short =
  {
    apply;
    notify_created;
    reject;
    confirm;
    log_burn = log_op01_burn ~short;
  }

let run_public_tx deps tx =
  handle_public_result
    ~notify_created:deps.notify_created
    ~reject:deps.reject
    ~confirm:deps.confirm
    ~log_burn:(fun () -> deps.log_burn tx)
    (deps.apply tx)

let handle_epoch_exec_result ~reject ~confirm = function
  | Ok _ ->
    confirm ()
  | Error (tag, reason) ->
    reject tag reason

let run_epoch_exec ~process ~reject ~confirm =
  let open Lwt.Syntax in
  let* result = process () in
  handle_epoch_exec_result ~reject ~confirm result

let run_circle_epoch_exec (deps : ('backend, 'env) epoch_exec_deps) ~process
    ~current_epoch tx =
  run_epoch_exec
    ~process:(fun () ->
      let backend = deps.backend () in
      process ~backend ~current_epoch tx)
    ~reject:deps.reject
    ~confirm:deps.confirm

let run_standard_epoch_exec (deps : ('backend, 'env) epoch_exec_deps) ~process tx =
  run_epoch_exec
    ~process:(fun () ->
      let backend = deps.backend () in
      let env = deps.standard_env () in
      process ~backend ~env tx)
    ~reject:deps.reject
    ~confirm:deps.confirm

let reject_contract_upgrade ~reject =
  reject
    ~notify_reason:"Contract upgrade not implemented"
    "contract_upgrade_unsupported"
    "not implemented"

let fatal_lines ~short ~epoch_id ~sender exn =
  [
    Printf.sprintf
      "event = sender_uncaught sender = %s epoch = %d err = %s"
      (short sender)
      epoch_id
      (Printexc.to_string exn);
    "event = partial_commit_refused";
    "event = restart_required source = irmin";
  ]

let run deps txs =
  Lwt_list.iter_s
    (fun batch ->
       Lwt.catch
         (fun () -> deps.process batch.txs)
         (fun exn ->
            deps.fatal ~sender:batch.sender exn;
            Lwt.return_unit))
    (group txs)