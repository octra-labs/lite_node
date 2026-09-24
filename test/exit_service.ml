(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Exit_case

module Actor = Octra_node_runtime.Set_actor
module Control = Octra_core.Validator_control
module Status = Octra_node_runtime.Status_read_rpc
module Staging = Octra_core.Tx_staging
module History = Octra_core.Store_chaindata
module Http = Octra_node_runtime.Rpc_http
module Dispatch = Octra_node_runtime.Rpc_dispatch
module Rest = Octra_node_runtime.Node_rest_facade

type loss = Before | Queued | Committed

let field = Yojson.Safe.Util.member

type t = {
  data_dir : string;
  database : string;
  mutable store : S.t;
  mutable ledger : L.t;
  mutable history : History.t;
  loss : loss;
  identity : Octra_core.Validator_intent.identity;
  mutable lost_reply : bool;
  confirmed : (string, T.t * int) Hashtbl.t;
  losses : (string, unit) Hashtbl.t;
  mutable view : view;
  mutable head : int;
  mutable control : Control.t;
  mutable duty : Actor.t option;
  mutable submitted : T.t list;
  mutable sent : int;
}

let bond_entry view =
  view.registry |> Option.get |> R.of_string |> get "operator registry"
  |> R.find owner.address

let identity epoch = Octra_core.Validator_intent.{
  chain_id; address = owner.address; pubkey = Base64.encode_exn owner.public;
  bonded_epoch = Int64.of_int epoch;
}

let actor (node : t) = Actor.create Actor.{
  sample = (fun () -> {
    epoch = Int64.of_int (node.head + 1); active = false;
    bonded = Octra_node_runtime.Set_control.bonded ~control:node.control
      ~address:owner.address ~pubkey:node.identity.pubkey
      (Ok Status.{
        head_epoch = node.head; state_root = node.view.root; chain_id; config_hash = "";
        head_proposal_id = None;
        candidate = bond_entry node.view; duty = None; sets = (None, None);
      });
  });
  read = (fun ~epoch:_ -> Lwt.return_ok
    Octra_core.Set_fold.{ marked = []; pulse = None });
  peers = (fun () -> 1);
  send = (fun ~epoch _ ->
    let permit = match bond_entry node.view with
      | None -> Error "validator bond missing"
      | Some entry -> Ok { node.identity with bonded_epoch = entry.bonded_epoch }
    in
    Result.bind permit (fun identity -> Control.guard node.control identity (fun () ->
      Ok (match Staging.duty_nonce owner.address node.view.account.nonce with
        | None -> Error "validator duty nonce is pending"
        | Some nonce ->
          ignore (transaction ~epoch:(Int64.to_int epoch) ~nonce T.ValidatorReady);
          node.sent <- node.sent + 1;
          Ok ())))
    |> (function
      | Error error -> Error (Actor.Control, error)
      | Ok result -> Result.map_error (fun error -> Actor.Send, error) result)
    |> Lwt.return);
  warn = (fun _ _ -> ());
}

let wake node =
  Option.iter (fun duty -> ignore (Actor.wake duty ~head:node.head)) node.duty

let stop_duty node =
  match node.duty with
  | None -> Lwt.return_unit
  | Some duty -> node.duty <- None; Actor.shutdown duty

let restart node =
  let open Lwt.Syntax in
  let* () = stop_duty node in
  let* () = S.close node.store in
  History.close node.history;
  let* store = S.open_store node.database in
  node.store <- store;
  node.ledger <- L.create store;
  node.history <- History.open_chaindata (node.database ^ ".history");
  let* view = view_lwt node.store node.ledger in
  expect "reopened ledger matches committed state" (view = node.view);
  node.view <- view;
  node.control <- Control.create ~data_dir:node.data_dir;
  node.duty <- Some (actor node);
  Lwt.return_unit

let create ~data_dir ~database ~loss ~epoch =
  let open Lwt.Syntax in
  let* store = S.open_store database in
  let ledger = L.create store in
  let* view = view_lwt store ledger in
  let node = {
    data_dir; database; store; ledger; view; loss; identity = identity epoch;
    lost_reply = false;
    history = History.open_chaindata (database ^ ".history"); head = epoch;
    control = Control.create ~data_dir; duty = None; sent = 0; submitted = [];
    confirmed = Hashtbl.create 4; losses = Hashtbl.create 2;
  } in
  node.duty <- Some (actor node);
  Lwt.return node

let close node =
  let open Lwt.Syntax in
  let* () = stop_duty node in
  let* () = S.close node.store in
  History.close node.history;
  Staging.clear ();
  Lwt.return_unit

let apply node epoch txs =
  let open Lwt.Syntax in
  let* artifacts, view = apply_lwt node.store node.ledger (step epoch txs) in
  expect "operator transactions applied" (List.length artifacts.X.confirmed = List.length txs);
  node.view <- view;
  node.head <- epoch;
  Staging.remove_processed (List.map T.hash txs);
  wake node;
  Lwt.return_unit

let set_value head =
  let members = List.map (fun (key : key) -> Octra_core.Validator_admission.{
    address = key.address; pubkey = key.public; weight = Z.one;
  }) peers in
  Octra_core.Validator_set_update.make_weighted
    ~source_epoch:(Int64.of_int (head - 1)) ~activate_epoch:(Int64.of_int head) members
  |> get "operator set" |> Octra_core.Validator_set_update.to_string

let enrollment node =
  let open Lwt.Syntax in
  let pubkey = Base64.encode_exn owner.public in
  let snapshot = Status.{
    head_epoch = node.head; state_root = node.view.root; chain_id; config_hash = "";
    head_proposal_id = None;
    duty = Some Octra_core.Set_fold.{ marked = []; pulse = None };
    candidate = bond_entry node.view; sets = Some (set_value node.head), None;
  } in
  let* result = Status.validator_enrollment ~snapshot:(Ok snapshot)
    ~validator_address:owner.address ~validator_pubkey:pubkey in
  match result with
  | Ok (`Assoc fields) ->
    let control = Status.local_control ~data_dir:node.data_dir ~snapshot
      ~address:owner.address ~pubkey in
    Lwt.return_ok (`Assoc (fields @ ["local_control", control]))
  | result -> Lwt.return result

let accept node tx =
  let open Lwt.Syntax in
  let* () = apply node (node.head + 1) [tx] in
  History.begin_batch node.history;
  History.save_tx node.history ~hash:(T.hash tx) ~epoch_id:node.head
    ~from_addr:tx.from ~to_addr:tx.to_
    ~tx_json:(Yojson.Safe.to_string (T.to_yojson tx))
    ~op_type:(T.op_type_to_string tx.op_type)
    ~encrypted_data:(Option.value ~default:"" tx.encrypted_data)
    ~message:(Option.value ~default:"" tx.message);
  History.commit_batch node.history;
  Hashtbl.add node.confirmed (T.hash tx) (tx, node.head);
  Lwt.return_unit

let saved node tx =
  expect "operator signature" (T.verify tx (Base64.encode_exn owner.public));
  expect "operator operation"
    (tx.op_type = T.ValidatorBond || tx.op_type = T.ValidatorExit || tx.op_type = T.ValidatorWithdraw);
  let operation = T.op_type_to_string tx.op_type in
  let latest = Filename.concat node.data_dir "validator-control/last.json"
    |> Yojson.Safe.from_file in
  let epoch = if tx.op_type = T.ValidatorBond then
    field "head_epoch" latest |> Yojson.Safe.Util.to_int |> Int64.of_int
    else node.identity.bonded_epoch in
  let outbox = Filename.concat node.data_dir
    (Printf.sprintf "validator-control/%s-%Ld.json" operation epoch) in
  let saved = Yojson.Safe.from_file outbox in
  let stored = T.of_yojson (field "tx" saved) |> get "saved operator transaction" in
  expect "signed bytes persisted before POST" (stored = tx);
  expect "saved identity matches POST" (field "tx_hash" saved = `String (T.hash tx));
  expect "attempt reference persisted before POST" (field "tx_hash" latest = `String (T.hash tx));
  let prior = List.find_opt (fun item -> item.T.op_type = tx.op_type) node.submitted in
  Option.iter (fun old ->
    let rejected = History.get_rejected_tx node.history (T.hash old) <> None in
    let same_payment = { tx with T.timestamp = old.timestamp; signature = old.signature } = old in
    expect "retry keeps bytes or renews a rejected payment"
      (old = tx || (rejected && same_payment))) prior;
  node.submitted <- tx :: node.submitted;
  let first = not (Hashtbl.mem node.losses operation) in
  if first then Hashtbl.add node.losses operation ();
  first

let admit node =
  let runtime = Rest.{
    swarm_ref = ref None;
    duty_head = (fun () -> Some (Int64.of_int node.head,
      Octra_core.Rule_graph.ready_exec_at ~chain_id ~epoch:(node.head + 1)));
    preverify_admit = (fun tx ->
      expect "exit path needs no heavy preverification"
        (tx.T.op_type = T.ValidatorBond || tx.op_type = T.ValidatorExit || tx.op_type = T.ValidatorWithdraw);
      Ok ());
    save_drops = (fun drops -> expect "exit does not replace queued payments" (drops = []));
    find_drop = (fun _ -> None);
    drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
  } in
  Rest.validate_and_submit_tx runtime node.ledger

let submit node params =
  let open Lwt.Syntax in
  match Octra_node_runtime.Tx_view.submit_params params with
  | Error error -> Lwt.return_error error
  | Ok tx ->
  let first = saved node tx in
  node.lost_reply <- first;
  let* result =
    if first && node.loss = Before then Lwt.return_ok `Null
    else Octra_node_runtime.Submit_rpc.submit ~validate:(admit node) params in
  expect "operator passes production admission" (Result.is_ok result);
  let* () =
    if not first || node.loss = Committed then accept node tx else Lwt.return_unit in
  Lwt.return result

let routes = [
  "octra_validatorEnrollment", (fun _ node -> enrollment node);
  "node_status", (fun _ node -> Lwt.return_ok (`Assoc [
    "head_epoch", `Int node.head; "state_root", `String node.view.root;
  ]));
  "octra_balance", (fun params node ->
    Octra_node_runtime.Account_read_rpc.balance node.ledger ~params);
  "octra_transaction", (fun params node ->
    Octra_node_runtime.History_read_rpc.transaction
      ~find_drop:(fun _ -> None)
      ~account_nonce:(fun address ->
        Option.map (fun account -> account.L.nonce) (L.find_opt node.ledger address))
      node.history ~params);
  "octra_submit", (fun params node -> submit node params);
]

let callback node _ request body =
  let open Lwt.Syntax in
  wake node;
  node.lost_reply <- false;
  let* response = Http.handle_rpc_post
    ~process:(fun meta body -> Dispatch.process_body meta body node routes) request body in
  if node.lost_reply then
    Cohttp_lwt_unix.Server.respond_string ~status:`Service_unavailable ~body:"reply lost" ()
  else Lwt.return response

let serve node run =
  let open Lwt.Syntax in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  Lwt_unix.listen socket 8;
  let port = match Lwt_unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> failwith "operator listener" in
  let stop, finish = Lwt.wait () in
  let server = Cohttp_lwt_unix.Server.create ~stop ~mode:(`TCP (`Socket socket))
    (Cohttp_lwt_unix.Server.make ~callback:(callback node) ()) in
  Lwt.finalize (fun () -> run port)
    (fun () -> Lwt.wakeup_later finish (); server)