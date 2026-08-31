(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_core

let check name value =
  if not value then failwith name

let sender index =
  Printf.sprintf "oct%044d" index

let transaction ?(fee = 1_000) ?message
    ?(op_type = Transaction.Standard) from nonce =
  Transaction.{
    from;
    to_ = "oct99999999999999999999999999999999999999999999";
    amount = Z.zero;
    nonce;
    ou = Z.of_int fee;
    timestamp = 0.;
    signature = from ^ string_of_int nonce;
    public_key = None;
    message;
    op_type;
    encrypted_data = None;
  }

let lookup _ = Some (Z.of_int 1_000_000_000, 0)

let add tx =
  match Tx_staging.add_smart ~lookup tx with
  | Ok _ -> ()
  | Error reason -> failwith reason

let fill sender_count tx_count =
  for index = 1 to sender_count do
    let from = sender index in
    for nonce = 1 to tx_count do
      add (transaction from nonce)
    done
  done

let check_nonce_order batch =
  let next = Hashtbl.create 200 in
  List.iter
    (fun (tx : Transaction.t) ->
      let expected =
        match Hashtbl.find_opt next tx.from with
        | None -> 1
        | Some nonce -> nonce
      in
      check "sender nonce order" (tx.nonce = expected);
      Hashtbl.replace next tx.from (expected + 1))
    batch

let identity (tx : Transaction.t) =
  tx.from, tx.nonce

let select capacity transactions =
  Tx_staging.clear ();
  List.iter add transactions;
  Tx_staging.get_epoch_txs ~capacity

let check_fee_order () =
  let low_first = transaction (sender 1) 1 in
  let low_second = transaction ~fee:10_000 (sender 1) 2 in
  let high = transaction ~fee:4_000 (sender 2) 1 in
  let medium = transaction ~fee:2_000 (sender 3) 1 in
  let actual =
    select
      Tx_staging.max_ou_per_epoch
      [low_first; high; low_second; medium]
    |> List.map identity
  in
  let expected =
    List.map identity [high; medium; low_first; low_second]
  in
  check "fee rate order" (actual = expected)

let check_cost_rate_order () =
  let low = transaction (sender 4) 1 in
  let deployment =
    transaction
      ~fee:400_000
      ~op_type:Transaction.ContractDeploy
      (sender 5)
      1
  in
  let high = transaction ~fee:4_000 (sender 6) 1 in
  let actual =
    select
      Tx_staging.max_ou_per_epoch
      [low; deployment; high]
    |> List.map identity
  in
  let expected = List.map identity [high; deployment; low] in
  check "cost adjusted fee order" (actual = expected)

let check_capacity_skip () =
  let high = transaction ~fee:4_000 (sender 7) 1 in
  let deployment =
    transaction
      ~fee:400_000
      ~op_type:Transaction.ContractDeploy
      (sender 8)
      1
  in
  let low = transaction (sender 9) 1 in
  let actual =
    select (Z.of_int 2_000) [low; deployment; high]
    |> List.map identity
  in
  let expected = List.map identity [high; low] in
  check "capacity skip" (actual = expected)

let check_insertion_independence () =
  let transactions =
    List.init 500 (fun index ->
      transaction
        ~fee:(1_000 + (index mod 17))
        (sender (index + 20))
        1)
  in
  let capacity = Z.of_int 500_000 in
  let first = select capacity transactions |> List.map identity in
  let second = select capacity (List.rev transactions) |> List.map identity in
  check "insertion independent selection" (first = second)

let check_pool_eviction () =
  let low_a = transaction (sender 600) 1 in
  let low_b = transaction (sender 601) 1 in
  let low_c = transaction (sender 602) 1 in
  let high = transaction ~fee:4_000 (sender 603) 1 in
  let run first second =
    Tx_staging.clear ();
    let add_limited tx =
      Tx_staging.add_smart ~tx_limit:2 ~lookup tx
    in
    ignore (add_limited first |> Result.get_ok);
    ignore (add_limited second |> Result.get_ok);
    check "full pool rejects equal fee rate"
      (add_limited low_c
       = Error "staging full (insufficient evictable capacity)");
    let evicted = add_limited high |> Result.get_ok in
    check "higher fee rate evicts one entry" (List.length evicted = 1);
    check "pool size remains limited" (Tx_staging.staging_size () = 2);
    check "higher fee rate remains staged"
      (Option.is_some (Tx_staging.find_by_hash (Transaction.hash high)));
    Tx_staging.get_epoch_txs ~capacity:Tx_staging.max_ou_per_epoch
    |> List.map identity
  in
  let forward = run low_a low_b in
  let reverse = run low_b low_a in
  check "eviction is independent of insertion order" (forward = reverse)

let check_selection_time () =
  let sender_count = 200 in
  let tx_count = 50 in
  let expected = sender_count * tx_count in
  Tx_staging.clear ();
  fill sender_count tx_count;
  let started = Unix.gettimeofday () in
  let first =
    Tx_staging.get_epoch_txs ~capacity:Tx_staging.max_ou_per_epoch
  in
  let elapsed_ms = (Unix.gettimeofday () -. started) *. 1_000. in
  let second =
    Tx_staging.get_epoch_txs ~capacity:Tx_staging.max_ou_per_epoch
  in
  check "selected transaction count" (List.length first = expected);
  check "deterministic selection" (List.map identity first = List.map identity second);
  check_nonce_order first;
  check "selection deadline" (elapsed_ms < 100.);
  Printf.printf
    "status = pass senders = %d transactions = %d elapsed_ms = %.3f\n"
    sender_count
    expected
    elapsed_ms

let check_payload_independence () =
  let count = 1_500 in
  let payload = String.make 65_536 'x' in
  let transactions =
    List.init count (fun index ->
      transaction
        ~message:(payload ^ string_of_int index)
        (sender (index + 1_000))
        1)
  in
  Tx_staging.clear ();
  List.iter add transactions;
  let started = Unix.gettimeofday () in
  let selected =
    Tx_staging.get_epoch_txs ~capacity:Tx_staging.max_ou_per_epoch
  in
  let elapsed_ms = (Unix.gettimeofday () -. started) *. 1_000. in
  check "payload selection count" (List.length selected = count);
  check "payload independent selection deadline" (elapsed_ms < 100.);
  Printf.printf
    "status = pass payload_bytes = %d transactions = %d elapsed_ms = %.3f\n"
    (String.length payload)
    count
    elapsed_ms

let () =
  check_fee_order ();
  check_cost_rate_order ();
  check_capacity_skip ();
  check_insertion_independence ();
  check_pool_eviction ();
  check_selection_time ();
  check_payload_independence ()