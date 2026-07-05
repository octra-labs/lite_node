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