(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  budgets : Resource_lanes.lane -> Resource_lanes.budget;
  receipts : Preverify_receipt.t list;
}

let create ?(budgets=Resource_lanes.default_budget) receipts = {
  budgets;
  receipts;
}

let same_cost a b =
  a.Resource_lanes.txs = b.Resource_lanes.txs
  && a.bytes = b.bytes
  && Z.equal a.ou b.ou
  && a.proof = b.proof

let tx_hash tx =
  Transaction.hash tx

let heavy tx =
  Preverify_queue.needs_preverify tx

let dup_by f xs =
  let rec go seen = function
    | [] -> None
    | x :: rest ->
      let key = f x in
      if List.exists (String.equal key) seen then Some key
      else go (key :: seen) rest in
  go [] xs

let receipt_hash r =
  r.Preverify_receipt.tx_hash

let find_receipt hash receipts =
  match List.filter (fun r -> String.equal (receipt_hash r) hash) receipts with
  | [r] -> Ok r
  | [] -> Error ("missing_receipt:" ^ hash)
  | _ -> Error ("duplicate_receipt:" ^ hash)

let receipt_for_tx gate tx =
  find_receipt (Transaction.hash tx) gate.receipts

let find_tx hash txs =
  List.find_opt (fun tx -> String.equal (tx_hash tx) hash) txs

let check_receipt tx r =
  match Preverify_receipt.validate r with
  | Error e -> Error ("invalid_receipt:" ^ e)
  | Ok () ->
    let hash = tx_hash tx in
    let lane = Resource_lanes.of_op tx.Transaction.op_type in
    let cost = Resource_lanes.cost tx in
    let input_hash, output_hash =
      match r.Preverify_receipt.state, r.circle with
      | None, None ->
        Preverify_worker.input_hash tx,
        Preverify_worker.output_hash tx "ok"
      | Some state, None ->
        Preverify_worker.bound_input_hash tx state,
        Preverify_worker.bound_output_hash tx state "ok"
      | None, Some circle ->
        Preverify_worker.circle_input_hash tx circle,
        Preverify_worker.circle_output_hash tx circle "ok"
      | Some _, Some _ ->
        "", ""
    in
    if not (String.equal r.Preverify_receipt.tx_hash hash) then Error "receipt_tx_mismatch"
    else if r.lane <> lane then Error ("receipt_lane_mismatch:" ^ hash)
    else if not r.ok then Error ("receipt_failed:" ^ hash)
    else if Transaction.bft_crypto_active ()
      && Transaction.bft_crypto_op tx.Transaction.op_type
      && Option.is_none r.state
    then Error ("receipt_state_binding_missing:" ^ hash)
    else if not (String.equal r.input_hash input_hash) then
      Error ("receipt_input_mismatch:" ^ hash)
    else if not (String.equal r.output_hash output_hash) then
      Error ("receipt_output_mismatch:" ^ hash)
    else if not (same_cost r.cost cost) then Error ("receipt_cost_mismatch:" ^ hash)
    else Ok ()

let same_source
    (left : Preverify_receipt.state)
    (right : Preverify_receipt.state) =
  String.equal left.Preverify_receipt.pre_state_hash right.pre_state_hash
  && String.equal left.source_cipher_hash right.source_cipher_hash
  && String.equal left.pvac_key_hash right.pvac_key_hash

let check_state ledger tx r =
  let open Lwt.Syntax in
  match r.Preverify_receipt.state, r.circle with
  | None, None -> Lwt.return_ok ()
  | Some state, None ->
    let* root = Ledger.hash ledger in
    if not (String.equal (Preverify_worker.state_hash root) state.pre_state_hash) then
      Lwt.return_error ("receipt_state_mismatch:" ^ tx_hash tx)
    else
      let* source =
        Preverify_worker.source_binding ledger state.pre_state_hash tx
      in
      begin
        match source with
        | Error e -> Lwt.return_error ("receipt_state_unavailable:" ^ e)
        | Ok expected when same_source expected state -> Lwt.return_ok ()
        | Ok _ ->
          Lwt.return_error ("receipt_source_mismatch:" ^ tx_hash tx)
      end
  | None, Some circle ->
    let* root = Ledger.hash ledger in
    if
      String.equal
        (Preverify_worker.state_hash root)
        circle.Preverify_receipt.snapshot_hash
    then
      Lwt.return_ok ()
    else
      Lwt.return_error ("receipt_state_mismatch:" ^ tx_hash tx)
  | Some _, Some _ ->
    Lwt.return_error ("receipt_binding_conflict:" ^ tx_hash tx)

let add_used used lane cost =
  let current =
    match List.assoc_opt lane used with
    | Some value -> value
    | None -> Resource_lanes.zero in
  let next = Resource_lanes.add current cost in
  List.map (fun (l, u) -> if l = lane then l, next else l, u) used

let check_budget gate used tx =
  let lane = Resource_lanes.of_op tx.Transaction.op_type in
  let cost = Resource_lanes.cost tx in
  let next = Resource_lanes.add (Option.value (List.assoc_opt lane used) ~default:Resource_lanes.zero) cost in
  match Resource_lanes.over (gate.budgets lane) next with
  | Some e -> Error ("lane_budget:" ^ Resource_lanes.to_string lane ^ ":" ^ e)
  | None -> Ok (add_used used lane cost)

let check_orphan txs r =
  match find_tx r.Preverify_receipt.tx_hash txs with
  | None -> Error ("orphan_receipt:" ^ r.tx_hash)
  | Some tx ->
    if heavy tx then Ok ()
    else Error ("receipt_for_light_tx:" ^ r.tx_hash)

let check gate txs =
  match dup_by tx_hash txs with
  | Some hash -> Error ("duplicate_tx:" ^ hash)
  | None ->
    match dup_by receipt_hash gate.receipts with
    | Some hash -> Error ("duplicate_receipt:" ^ hash)
    | None ->
      let rec receipts = function
        | [] -> Ok ()
        | r :: rest ->
          begin
            match Preverify_receipt.validate r with
            | Error e -> Error ("invalid_receipt:" ^ e)
            | Ok () ->
              match check_orphan txs r with
              | Error e -> Error e
              | Ok () -> receipts rest
          end in
      let rec tx_loop used = function
        | [] -> Ok ()
        | tx :: rest ->
          if not (heavy tx) then tx_loop used rest
          else
            let hash = tx_hash tx in
            match find_receipt hash gate.receipts with
            | Error e -> Error e
            | Ok r ->
              match check_receipt tx r with
              | Error e -> Error e
              | Ok () ->
                match check_budget gate used tx with
                | Error e -> Error e
                | Ok used -> tx_loop used rest in
      match receipts gate.receipts with
      | Error e -> Error e
      | Ok () ->
        tx_loop (List.map (fun lane -> lane, Resource_lanes.zero) Resource_lanes.all) txs

let check_bound ledger gate txs =
  match check gate txs with
  | Error e -> Lwt.return_error e
  | Ok () ->
    Lwt_list.fold_left_s
      (fun result tx ->
        match result with
        | Error _ -> Lwt.return result
        | Ok () when not (heavy tx) -> Lwt.return_ok ()
        | Ok () ->
          begin
            match find_receipt (tx_hash tx) gate.receipts with
            | Error e -> Lwt.return_error e
            | Ok receipt -> check_state ledger tx receipt
          end)
      (Ok ())
      txs

let receipts_of_strings raws =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | raw :: rest ->
      match Preverify_receipt.of_string raw with
      | Ok receipt -> go (receipt :: acc) rest
      | Error e -> Error e in
  go [] raws

let gate_of_strings ~required raws =
  match receipts_of_strings raws with
  | Error _ as error -> error
  | Ok [] when not required -> Ok None
  | Ok receipts -> Ok (Some (create receipts))

let check_strings ?budgets raws txs =
  match receipts_of_strings raws with
  | Error e -> Error ("receipt_parse:" ^ e)
  | Ok receipts ->
    let gate =
      match budgets with
      | Some budgets -> create ~budgets receipts
      | None -> create receipts in
    check gate txs