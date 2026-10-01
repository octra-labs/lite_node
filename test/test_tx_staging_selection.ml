(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_core

let check name value =
  if not value then failwith name

let sender index =
  Printf.sprintf "oct%044d" index

let transaction ?(fee = 1_000) ?message ?(timestamp = 0.)
    ?(op_type = Transaction.Standard) from nonce =
  Transaction.{
    from;
    to_ = "oct99999999999999999999999999999999999999999999";
    amount = Z.zero;
    nonce;
    ou = Z.of_int fee;
    timestamp;
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

let check_queue_state () =
  Tx_staging.clear ();
  let from = sender 700 in
  let first_tx = transaction from 1 in
  let second_tx = transaction from 2 in
  let third_tx = transaction from 3 in
  let hash = Transaction.hash third_tx in
  let started = Unix.gettimeofday () in
  add third_tx;
  begin
    match Tx_staging.queue_state ~confirmed:0 hash with
    | Some (Tx_staging.Wait_nonce { expected = 1; expires_at }) ->
      check "queue expiry" (expires_at > started)
    | _ -> failwith "queue first nonce"
  end;
  add first_tx;
  begin
    match Tx_staging.queue_state ~confirmed:0 hash with
    | Some (Tx_staging.Wait_nonce { expected = 2; _ }) -> ()
    | _ -> failwith "queue second nonce"
  end;
  add second_tx;
  begin
    match Tx_staging.queue_state ~confirmed:0 hash with
    | Some (Tx_staging.Ready _) -> ()
    | _ -> failwith "queue ready"
  end;
  begin
    match Tx_staging.queue_state ~confirmed:3 hash with
    | Some (Tx_staging.Nonce_used { confirmed = 3; _ }) -> ()
    | _ -> failwith "queue nonce used"
  end;
  check "nonce gap expiry reason"
    (Tx_staging.expiry_reason ~confirmed:1320 ~received:1323
     = "TTL exceeded: waiting for nonce 1321 before nonce 1323");
  check "ready expiry reason"
    (Tx_staging.expiry_reason ~confirmed:1322 ~received:1323
     = "TTL exceeded: transaction was not included")

let check_pending_nonce () =
  Tx_staging.clear ();
  let from = sender 705 in
  let first = transaction from 215 in
  let last = transaction from 217 in
  check "empty pending nonce" (Tx_staging.pending_nonce from 214 = 214);
  add first;
  check "inserted pending nonce" (Tx_staging.pending_nonce from 214 = 215);
  add last;
  add (transaction (sender 706) 300);
  check "sender pending maximum" (Tx_staging.pending_nonce from 214 = 217);
  check "pending confirmed floor" (Tx_staging.pending_nonce from 218 = 218);
  check "pending read preserves entries" (Tx_staging.pending_nonce from 214 = 217);
  check "remove highest nonce" (Tx_staging.remove_by_hash (Transaction.hash last));
  check "remaining pending nonce" (Tx_staging.pending_nonce from 214 = 215);
  check "remove final nonce" (Tx_staging.remove_by_hash (Transaction.hash first));
  check "removed pending nonce" (Tx_staging.pending_nonce from 214 = 214);
  check "empty confirmed floor" (Tx_staging.pending_nonce from 218 = 218)

let check_duty_nonce () =
  Tx_staging.clear ();
  let from = sender 707 in
  let duty nonce = transaction ~op_type:Transaction.ValidatorReady from nonce in
  let suffix = List.init 5 (fun index -> duty (436 + index)) in
  List.iter add suffix;
  let nonce = Tx_staging.first_missing_nonce from 349 in
  check "duty fills confirmed gap" (nonce = 350);
  check "pending maximum is not next duty nonce"
    (Tx_staging.pending_nonce from 349 = 440);
  add (duty nonce);
  check "duty does not replace occupied nonce"
    (Tx_staging.first_missing_nonce from 349 = 351);
  check "duty retains existing transactions"
    (List.for_all (fun tx ->
      Tx_staging.find_by_hash (Transaction.hash tx) = Some tx) suffix);
  check "duty follows committed nonce"
    (Tx_staging.first_missing_nonce from 440 = 441);
  Tx_staging.clear ()

let check_recent_order () =
  Tx_staging.clear ();
  let first = transaction ~timestamp:2. (sender 710) 1 in
  let second = transaction ~timestamp:1. (sender 711) 1 in
  let third = transaction ~timestamp:3. (sender 712) 1 in
  List.iter add [first; second; third];
  let actual =
    Tx_staging.recent 3
    |> List.map (fun (_, tx) -> tx.Transaction.timestamp)
  in
  check "recent transaction order" (actual = [3.; 2.; 1.])

let check_duty_head () =
  Tx_staging.clear ();
  let from = sender 708 in
  let duty epoch nonce =
    let message = Yojson.Safe.to_string (`Assoc [
      "consensus_pubkey", `String "key";
      "head_epoch", `String (Int64.to_string epoch);
      "state_root", `String (String.make 64 'a');
    ]) in
    transaction ~op_type:Transaction.ValidatorReady ~message from nonce
  in
  let old = duty 99L 1 in
  let current = duty 100L 2 in
  let future = duty 101L 3 in
  let ordinary = transaction ~message:(Option.get old.message) from 4 in
  let malformed = transaction ~op_type:Transaction.ValidatorReady from 5 in
  let others = transaction (sender 709) 1 in
  List.iter add [old; current; future; ordinary; malformed; others];
  check "prior rule retains ready" (not (Tx_staging.duty_expired ~head:None old));
  check "current head retained" (not (Tx_staging.duty_expired ~head:(Some 100L) current));
  check "future head retained" (not (Tx_staging.duty_expired ~head:(Some 100L) future));
  check "ordinary payload retained" (not (Tx_staging.duty_expired ~head:(Some 100L) ordinary));
  check "malformed payload not guessed" (not (Tx_staging.duty_expired ~head:(Some 100L) malformed));
  check "duty cannot queue behind occupied nonce"
    (Tx_staging.duty_nonce from 0 = None);
  check "prior expiry disabled" (Tx_staging.expire_duty ~head:None () = []);
  let removed = Tx_staging.expire_duty ~head:(Some 100L) () in
  check "only old duty expires"
    (List.map (fun row -> row.Tx_staging.d_hash) removed = [Transaction.hash old]);
  check "old duty absent from indices"
    (Tx_staging.find_by_hash (Transaction.hash old) = None
     && not (List.mem_assoc (Transaction.hash old) (Tx_staging.recent 10)));
  check "duty uses confirmed successor" (Tx_staging.duty_nonce from 0 = Some 1);
  let total = List.fold_left (fun total tx -> Z.add total (Transaction.ou_cost tx))
    Z.zero [current; future; ordinary; malformed; others] in
  check "expiry releases queue cost" (Z.equal total (Tx_staging.staging_total_ou ()));
  check "user transactions retained"
    (List.for_all (fun tx -> Tx_staging.find_by_hash (Transaction.hash tx) = Some tx)
       [ordinary; malformed; others]);
  check "expiry is idempotent" (Tx_staging.expire_duty ~head:(Some 100L) () = []);
  check "sender expiry isolated"
    (Tx_staging.expire_duty ~sender:(sender 709) ~head:(Some 102L) () = []);
  check "exhausted nonce has no successor" (Tx_staging.duty_nonce from max_int = None);
  Tx_staging.clear ()

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

let check_ready_gap () =
  Tx_staging.clear ();
  let first = transaction ~fee:2_000 (sender 1) 1 in
  let third = transaction ~op_type:Transaction.KeySwitch (sender 1) 3 in
  let other = transaction (sender 2) 2 in
  let used = transaction (sender 3) 1 in
  let next = transaction (sender 3) 2 in
  let later = transaction (sender 3) 4 in
  let unknown = transaction (sender 4) 1 in
  List.iter add [third; other; later; unknown; used; next; first];
  let confirmed_nonce addr =
    if addr = sender 4 then None
    else Some (if addr = sender 3 then 1 else 0)
  in
  let read capacity =
    Tx_staging.ready_epoch_txs ~accept:(fun _ -> true) ~capacity ~confirmed_nonce
    |> List.map identity
  in
  let capacity = Tx_staging.max_ou_per_epoch in
  let before = Tx_staging.staging_size () in
  let expected = List.map identity [first; next] in
  check "ready prefix only" (read capacity = expected);
  check "selection leaves queue intact" (Tx_staging.staging_size () = before);
  check "used nonce omitted" (not (List.mem (identity used) (read capacity)));
  check "ready prefix deterministic" (read capacity = read capacity);
  check "zero capacity empty" (read Z.zero = []);
  check "capacity stops sender suffix"
    (read (Transaction.ou_cost first) = [identity first]);
  let second = transaction (sender 1) 2 in
  add second;
  let filled = read capacity in
  check "missing nonce restores chain"
    (List.filter (fun (addr, _) -> addr = sender 1) filled
     = List.map identity [first; second; third]);
  check "independent sender retained" (List.mem (identity next) filled);
  let snapshot () =
    Tx_staging.get_epoch_txs ~capacity |> List.map Transaction.hash
  in
  let contents = snapshot () in
  let moved =
    Tx_staging.ready_epoch_txs ~accept:(fun _ -> true) ~capacity
      ~confirmed_nonce:(fun addr ->
        if addr = sender 1 then Some 1 else confirmed_nonce addr)
    |> List.map identity
  in
  check "confirmed nonce advances prefix"
    (List.filter (fun (addr, _) -> addr = sender 1) moved
     = List.map identity [second; third]);
  check "selection preserves contents" (snapshot () = contents);
  Tx_staging.clear ();
  List.iter add [first; next; used; unknown; later; other; third; second];
  check "ready order ignores insertion" (read capacity = filled);
  check "nonce limit has no successor"
    (Tx_staging.ready_epoch_txs ~accept:(fun _ -> true) ~capacity
       ~confirmed_nonce:(fun _ -> Some max_int) = [])

let check_ready_cost () =
  Tx_staging.clear ();
  let first = transaction ~fee:4_000 (sender 1) 1 in
  let second = transaction ~fee:400_000 ~op_type:Transaction.KeySwitch (sender 1) 2 in
  let third = transaction (sender 1) 3 in
  let other = transaction (sender 2) 1 in
  List.iter add [third; second; other; first];
  let capacity = Z.add (Transaction.ou_cost first) (Transaction.ou_cost other) in
  check "middle cost exceeds budget" (Z.gt (Transaction.ou_cost second) capacity);
  let selected =
    Tx_staging.ready_epoch_txs ~accept:(fun _ -> true) ~capacity
      ~confirmed_nonce:(fun _ -> Some 0)
    |> List.map identity
  in
  check "over budget suffix omitted" (selected = List.map identity [first; other])

let check_private_slots () =
  let low = transaction ~fee:100_000 ~op_type:Transaction.EncryptOp (sender 1) 1 in
  let high = transaction ~fee:200_000 ~op_type:Transaction.EncryptOp (sender 99) 1 in
  let later = transaction ~fee:1_000_000 (sender 1) 2 in
  let public = transaction (sender 50) 1 in
  let inputs = select Tx_staging.max_ou_per_epoch [low; later; public; high] in
  let choose limits ready = Private_slots.select ~limits ~inputs ~ready in
  let one = Private_slots.{fhe = 1; stealth = 1} in
  let picked = choose one inputs in
  check "private price wins over sender address" (List.mem high picked && not (List.mem low picked));
  check "private deferred nonce suffix remains queued" (not (List.mem later picked)
    && Tx_staging.staging_size () = 4);
  check "public transaction retains capacity" (List.mem public picked);
  let failed = choose one (List.filter (fun item -> item <> high) inputs) in
  check "invalid private proof cannot consume slot" (List.mem low failed && List.mem later failed);
  check "zero private budget preserves public traffic"
    (choose {one with fhe = 0} inputs = [public]);
  check "independent senders use separate slots"
    (List.length (choose {one with fhe = 2} inputs) = 4);
  let decrypt = transaction ~op_type:Transaction.DecryptOp (sender 3) 1 in
  let stealth = transaction ~op_type:Transaction.StealthOp (sender 3) 2 in
  let suffix = transaction (sender 3) 3 in
  let slots = Private_slots.create {fhe = 8; stealth = 4} in
  let first = Option.get (Private_slots.reserve slots decrypt) in
  check "same sender encrypted debit is not duplicated" (Private_slots.reserve first stealth = None);
  check "independent budget has no side effects" (Option.is_some (Private_slots.reserve slots stealth));
  check "blocked private debit cannot skip nonce"
    (Private_slots.select ~limits:{fhe = 8; stealth = 4}
      ~inputs:[decrypt; stealth; suffix] ~ready:[decrypt; stealth; suffix] = [decrypt]);
  List.iter (fun max ->
    for count = 0 to 12 do
      let inputs = List.init count (fun i ->
        transaction ~op_type:Transaction.StealthOp (sender (100 + i)) 1) in
      let result = Private_slots.select ~limits:{fhe = 8; stealth = max} ~inputs ~ready:inputs in
      check "private cap exceeded" (List.length result = min count (min 8 max))
    done) [0; 1; 2; 4; 8];
  Tx_staging.clear ()

let () =
  check_fee_order ();
  check_cost_rate_order ();
  check_capacity_skip ();
  check_insertion_independence ();
  check_pool_eviction ();
  check_queue_state ();
  check_pending_nonce ();
  check_duty_nonce ();
  check_duty_head ();
  check_recent_order ();
  check_ready_gap ();
  check_ready_cost ();
  check_private_slots ();
  check_selection_time ();
  check_payload_independence ()