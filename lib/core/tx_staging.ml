(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let max_ou = Z.of_int 10_000_000_000
let max_staging_txs = 100_000

let public_balance_cost (tx : Transaction.t) =
  match tx.op_type with
  | DecryptOp | ClaimOp | StealthOp | PrivateOp -> tx.ou
  | _ -> Z.add tx.amount tx.ou

type entry = {
  tx : Transaction.t;
  ou : Z.t;
  key : string;
  added_at : float;
  hash : string;
}

let compare_entry_rate a b =
  Z.compare
    (Z.mul b.tx.Transaction.ou a.ou)
    (Z.mul a.tx.Transaction.ou b.ou)

module Index_key = struct
  type t = string * int * string

  let compare (af, an, ah) (bf, bn, bh) =
    let sender = String.compare af bf in
    if sender <> 0 then sender
    else
      let nonce = Int.compare an bn in
      if nonce <> 0 then nonce else String.compare ah bh
end

module Index = Map.Make(Index_key)

module Sender_queue = Map.Make(String)

module Selection_head = Set.Make(struct
  type t = string * entry

  let compare (sender_a, a) (sender_b, b) =
    let rate = compare_entry_rate a b in
    if rate <> 0 then rate
    else
      let hash = String.compare a.hash b.hash in
      if hash <> 0 then hash else String.compare sender_a sender_b
end)

let staging : (string, entry) Hashtbl.t = Hashtbl.create 200
let hash_index : (string, entry) Hashtbl.t = Hashtbl.create 200
let view_index = ref Index.empty
let total_ou = ref Z.zero

let virtual_balances : (string, Z.t) Hashtbl.t = Hashtbl.create 100
let virtual_nonces : (string, int) Hashtbl.t = Hashtbl.create 100

type drop_reason = Evicted | Expired

type drop_record = {
  d_hash : string;
  d_from : string;
  d_to : string;
  d_nonce : int;
  d_ou : Z.t;
  d_op_type : Transaction.op_type;
  d_reason : drop_reason;
  d_detail : string;
  d_ts : float;
}

let dropped_cache : (string, drop_record) Hashtbl.t = Hashtbl.create 200
let dropped_max = 10_000
let dropped_ttl = 600.

let drop_row record =
  Tx_drop.{
    hash = record.d_hash;
    from_addr = record.d_from;
    to_addr = record.d_to;
    nonce = record.d_nonce;
    ou = record.d_ou;
    op_type = record.d_op_type;
    reason =
      (match record.d_reason with
       | Evicted -> "evicted"
       | Expired -> "expired");
    detail = record.d_detail;
    dropped_at = record.d_ts;
  }

let record_drop hash tx reason detail =
  if Hashtbl.length dropped_cache >= dropped_max then begin
    let oldest = Hashtbl.fold (fun k v acc ->
      match acc with
      | None -> Some (k, v.d_ts)
      | Some (_, t) -> if v.d_ts < t then Some (k, v.d_ts) else acc
    ) dropped_cache None in
    match oldest with Some (k, _) -> Hashtbl.remove dropped_cache k | None -> ()
  end;
  let record =
    { d_hash = hash; d_from = tx.Transaction.from; d_to = tx.to_;
      d_nonce = tx.nonce; d_ou = tx.ou; d_op_type = tx.op_type;
      d_reason = reason; d_detail = detail; d_ts = Unix.gettimeofday () }
  in
  Hashtbl.replace dropped_cache hash record;
  record

let cleanup_dropped () =
  let now = Unix.gettimeofday () in
  let old = Hashtbl.fold (fun k v acc ->
    if now -. v.d_ts > dropped_ttl then k :: acc else acc
  ) dropped_cache [] in
  List.iter (Hashtbl.remove dropped_cache) old

let lookup_dropped hash =
  match Hashtbl.find_opt dropped_cache hash with
  | Some d ->
    let reason_str =
      match d.d_reason with
      | Evicted -> "evicted"
      | Expired -> "expired"
    in
    Some (reason_str, d.d_detail, d.d_ts, d.d_from, d.d_to, d.d_nonce, d.d_ou, d.d_op_type)
  | None -> None

let all () =
  Hashtbl.fold (fun _ e acc -> e.tx :: acc) staging []
  |> Transaction.consensus_order

let rec first limit seq acc =
  if limit <= 0 then List.rev acc
  else
    match seq () with
    | Seq.Nil -> List.rev acc
    | Seq.Cons (value, rest) -> first (limit - 1) rest (value :: acc)

let index_key entry =
  entry.tx.Transaction.from, entry.tx.Transaction.nonce, entry.hash

let sender_entries sender =
  let rec collect sequence entries =
    match sequence () with
    | Seq.Nil -> List.rev entries
    | Seq.Cons (((entry_sender, _, _), entry), rest) ->
      if String.equal entry_sender sender then collect rest (entry :: entries)
      else List.rev entries
  in
  Index.to_seq_from (sender, min_int, "") !view_index
  |> fun sequence -> collect sequence []

let sender_has_pending sender =
  match Index.to_seq_from (sender, min_int, "") !view_index () with
  | Seq.Cons (((entry_sender, _, _), _), _) ->
    String.equal entry_sender sender
  | Seq.Nil -> false

let sample limit =
  first (max 0 limit) (Index.to_seq !view_index) []
  |> List.map (fun (_, entry) -> entry.hash, entry.tx)

let clear () =
  List.iter Hashtbl.clear [staging; hash_index];
  view_index := Index.empty;
  Hashtbl.clear virtual_balances;
  Hashtbl.clear virtual_nonces;
  total_ou := Z.zero

let find_by_hash h =
  Hashtbl.find_opt hash_index h |> Option.map (fun e -> e.tx)

let insert tx =
  let ou = Transaction.ou_cost tx in
  let key = tx.Transaction.from ^ string_of_int tx.nonce in
  let hash = Transaction.hash tx in
  let entry = { tx; ou; key; added_at = Unix.gettimeofday (); hash } in
  Hashtbl.add staging key entry;
  Hashtbl.add hash_index hash entry;
  view_index := Index.add (index_key entry) entry !view_index;
  total_ou := Z.add !total_ou ou;
  Hashtbl.replace virtual_balances tx.from
    (Z.sub (Hashtbl.find virtual_balances tx.from) (public_balance_cost tx));
  Hashtbl.replace virtual_nonces tx.from
    (max (Hashtbl.find virtual_nonces tx.from) tx.nonce)

let evict entry =
  Hashtbl.remove staging entry.key;
  Hashtbl.remove hash_index entry.hash;
  view_index := Index.remove (index_key entry) !view_index;
  total_ou := Z.sub !total_ou entry.ou;
  Hashtbl.replace virtual_balances entry.tx.from
    (Z.add (Hashtbl.find virtual_balances entry.tx.from)
       (public_balance_cost entry.tx))

let evict_nonce_dependents sender nonce =
  sender_entries sender
  |> List.filter (fun entry -> entry.tx.Transaction.nonce > nonce)
  |> List.map (fun e ->
    evict e;
    record_drop e.hash e.tx Evicted "nonce gap from eviction"
  )

let init_virtual_state ~lookup addr =
  if not (sender_has_pending addr) then begin
    let balance, nonce = match lookup addr with
      | Some (b, n) -> (b, n)
      | None -> (Z.zero, 0)
    in
    Hashtbl.replace virtual_balances addr balance;
    Hashtbl.replace virtual_nonces addr nonce;
    Ok ()
  end else if Hashtbl.mem virtual_balances addr
              && Hashtbl.mem virtual_nonces addr then
    Ok ()
  else
    Error "staging sender state is inconsistent"

let check_virtual_balance addr amount =
  match Hashtbl.find_opt virtual_balances addr with
  | Some b -> Z.geq b amount
  | None -> false

let get_expected_nonce addr =
  match Hashtbl.find_opt virtual_nonces addr with
  | Some n -> n + 1 | None -> 1

let pending_nonce addr confirmed =
  match Hashtbl.find_opt virtual_nonces addr with
  | Some n -> n | None -> confirmed

let rbf_bump_ok new_tx old_tx =
  let cn = Transaction.ou_cost new_tx in
  let co = Transaction.ou_cost old_tx in
  Z.geq (Z.mul (Z.mul new_tx.Transaction.ou co) (Z.of_int 100))
        (Z.mul (Z.mul old_tx.Transaction.ou cn) (Z.of_int 110))

let add_smart ?(ou_limit=max_ou) ?(tx_limit=max_staging_txs) ~lookup tx =
  let ou = Transaction.ou_cost tx in
  let key = tx.Transaction.from ^ string_of_int tx.nonce in
  let total_cost = public_balance_cost tx in
  match init_virtual_state ~lookup tx.from with
  | Error _ as error -> error
  | Ok () ->
  let base_nonce = match lookup tx.from with
    | Some (_, n) -> n | None -> 0
  in
  if Z.sign ou_limit <= 0 || tx_limit <= 0 then
    Error "staging limits must be positive"
  else if tx.nonce <= base_nonce then Error "nonce too low (already used)"
  else if tx.nonce > base_nonce + 1000 then Error "nonce too far ahead"
  else if Z.gt ou ou_limit then
    Error "transaction too large for staging capacity"
  else
    match Hashtbl.find_opt staging key with
    | Some existing ->
      if not (rbf_bump_ok tx existing.tx) then
        Error "duplicate nonce (fee rate bump < 10%)"
      else
        let delta = Z.sub total_cost (public_balance_cost existing.tx) in
        if Z.gt delta Z.zero && not (check_virtual_balance tx.from delta) then
          Error "insufficient balance for replacement"
        else begin
          evict existing;
          let dropped =
            record_drop
              existing.hash
              existing.tx
              Evicted
              "replaced by higher fee-rate"
          in
          insert tx;
          Ok [dropped]
        end
    | None ->
      if not (check_virtual_balance tx.from total_cost) then
        Error "insufficient balance (including pending)"
      else
        let needs_eviction =
          Z.gt (Z.add !total_ou ou) ou_limit || Hashtbl.length staging >= tx_limit in
        if needs_eviction then begin
          let evictable_ou = Hashtbl.fold (fun _ e acc ->
            if Transaction.better_fee_rate tx e.tx then Z.add acc e.ou else acc
          ) staging Z.zero in
          let evictable_count = Hashtbl.fold (fun _ e acc ->
            if Transaction.better_fee_rate tx e.tx then acc + 1 else acc
          ) staging 0 in
          let ou_deficit = Z.max Z.zero (Z.sub (Z.add !total_ou ou) ou_limit) in
          let count_deficit = max 0 (Hashtbl.length staging + 1 - tx_limit) in
          if Z.lt evictable_ou ou_deficit || evictable_count < count_deficit then
            Error "staging full (insufficient evictable capacity)"
          else
            let sorted_by_rate = Hashtbl.fold (fun _ e acc -> e :: acc) staging []
              |> List.sort (fun a b ->
                let rate = -(Transaction.cmp_fee_rate_desc a.tx b.tx) in
                if rate <> 0 then rate else String.compare a.hash b.hash) in
            let rec evict_loop evicted_list = function
              | [] -> evicted_list
              | candidate :: rest ->
                if not (Hashtbl.mem staging candidate.key) then
                  evict_loop evicted_list rest
                else if
                  Z.leq (Z.add !total_ou ou) ou_limit
                  && Hashtbl.length staging < tx_limit
                then
                  evicted_list
                else if not (Transaction.better_fee_rate tx candidate.tx) then
                  evicted_list
                else begin
                  evict candidate;
                  let dropped =
                    record_drop
                      candidate.hash
                      candidate.tx
                      Evicted
                      "outbid by higher fee-rate"
                  in
                  let deps = evict_nonce_dependents candidate.tx.Transaction.from candidate.tx.Transaction.nonce in
                  evict_loop (dropped :: deps @ evicted_list) rest
                end
            in
            let evicted_list = evict_loop [] sorted_by_rate in
            insert tx;
            Ok evicted_list
        end
        else begin insert tx; Ok [] end

let get_ordered_txs () =
  let by_sender = Hashtbl.create 50 in
  List.iter (fun (tx : Transaction.t) ->
    let prev = match Hashtbl.find_opt by_sender tx.from with
      | Some l -> l | None -> [] in
    Hashtbl.replace by_sender tx.from (tx :: prev)
  ) (all ());
  let sorted_senders = Hashtbl.fold (fun k _ acc -> k :: acc) by_sender []
    |> List.sort String.compare in
  List.concat_map (fun sender ->
    match Hashtbl.find_opt by_sender sender with
    | Some txs -> List.sort (fun (a : Transaction.t) b -> compare a.nonce b.nonce) txs
    | None -> []
  ) sorted_senders

let max_ou_per_epoch = Z.of_int 10_000_000_000

let compare_sender_entry a b =
  let nonce = Int.compare a.tx.Transaction.nonce b.tx.Transaction.nonce in
  if nonce <> 0 then nonce else String.compare a.hash b.hash

let sender_queues () =
  Hashtbl.fold
    (fun _ entry queues ->
      Sender_queue.update
        entry.tx.Transaction.from
        (function
          | None -> Some [entry]
          | Some queue -> Some (entry :: queue))
        queues)
    staging
    Sender_queue.empty
  |> Sender_queue.map (List.sort compare_sender_entry)

let add_selection_head sender queue heads =
  match queue with
  | entry :: _ -> Selection_head.add (sender, entry) heads
  | [] -> heads

let selection_heads queues =
  Sender_queue.fold add_selection_head queues Selection_head.empty

let rec select_epoch_txs capacity queues heads used batch =
  match Selection_head.min_elt_opt heads with
  | None -> List.rev batch
  | Some (sender, entry) ->
    let heads = Selection_head.remove (sender, entry) heads in
    match Sender_queue.find_opt sender queues with
    | Some (current :: rest) when String.equal current.hash entry.hash ->
      if Z.gt (Z.add used entry.ou) capacity then
        select_epoch_txs
          capacity
          (Sender_queue.remove sender queues)
          heads
          used
          batch
      else
        let queues =
          match rest with
          | [] -> Sender_queue.remove sender queues
          | _ -> Sender_queue.add sender rest queues
        in
        select_epoch_txs
          capacity
          queues
          (add_selection_head sender rest heads)
          (Z.add used entry.ou)
          (entry.tx :: batch)
    | _ -> invalid_arg "staging selection state"

let get_epoch_txs ~capacity =
  let queues = sender_queues () in
  select_epoch_txs
    capacity
    queues
    (selection_heads queues)
    Z.zero
    []

let remove_by_hash h =
  match Hashtbl.find_opt hash_index h with
  | Some entry -> evict entry; true
  | None -> false

let clear_virtual_state touched =
  Hashtbl.iter (fun addr () ->
    if not (sender_has_pending addr) then begin
      Hashtbl.remove virtual_balances addr;
      Hashtbl.remove virtual_nonces addr
    end
  ) touched

let remove_processed hashes =
  let touched = Hashtbl.create 16 in
  List.iter (fun h ->
    (match Hashtbl.find_opt hash_index h with
     | Some entry -> Hashtbl.replace touched entry.tx.Transaction.from ()
     | None -> ());
    ignore (remove_by_hash h)
  ) hashes;
  clear_virtual_state touched

let staging_ttl = 600.

let expire_old () =
  let now = Unix.gettimeofday () in
  let expired = Hashtbl.fold (fun _ (e : entry) acc ->
    if now -. e.added_at > staging_ttl then e :: acc else acc
  ) staging [] in
  let touched = Hashtbl.create 16 in
  let records =
    List.map (fun (e : entry) ->
      Hashtbl.replace touched e.tx.Transaction.from ();
      evict e;
      record_drop e.hash e.tx Expired "TTL exceeded"
    ) expired
  in
  clear_virtual_state touched;
  records

let staging_total_ou () = !total_ou

let staging_size () = Hashtbl.length staging

let staging_usage_pct () =
  if Z.equal max_ou Z.zero then 0
  else Z.to_int (Z.div (Z.mul !total_ou (Z.of_int 100)) max_ou)

let min_relay_fee tx =
  let usage = staging_usage_pct () in
  let base = Transaction.ou_cost tx in
  if usage < 50 then base
  else if usage < 80 then base
  else if usage < 95 then Z.mul base (Z.of_int 2)
  else Z.mul base (Z.of_int 5)

let stats_of txs =
  let by_sender = Hashtbl.create 50 in
  List.iter (fun (tx : Transaction.t) ->
    let cost = public_balance_cost tx in
    let count, total = match Hashtbl.find_opt by_sender tx.from with
      | Some (c, t) -> (c + 1, Z.add t cost)
      | None -> (1, cost)
    in
    Hashtbl.replace by_sender tx.from (count, total)
  ) txs;
  by_sender

let get_stats () =
  let txs = all () in
  txs, stats_of txs