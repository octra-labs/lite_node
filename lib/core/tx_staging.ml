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

let staging : (string, entry) Hashtbl.t = Hashtbl.create 200
let hash_index : (string, entry) Hashtbl.t = Hashtbl.create 200
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

let record_drop hash tx reason detail =
  if Hashtbl.length dropped_cache >= dropped_max then begin
    let oldest = Hashtbl.fold (fun k v acc ->
      match acc with
      | None -> Some (k, v.d_ts)
      | Some (_, t) -> if v.d_ts < t then Some (k, v.d_ts) else acc
    ) dropped_cache None in
    match oldest with Some (k, _) -> Hashtbl.remove dropped_cache k | None -> ()
  end;
  Hashtbl.replace dropped_cache hash
    { d_hash = hash; d_from = tx.Transaction.from; d_to = tx.to_;
      d_nonce = tx.nonce; d_ou = tx.ou; d_op_type = tx.op_type;
      d_reason = reason; d_detail = detail; d_ts = Unix.gettimeofday () }

let cleanup_dropped () =
  let now = Unix.gettimeofday () in
  let old = Hashtbl.fold (fun k v acc ->
    if now -. v.d_ts > dropped_ttl then k :: acc else acc
  ) dropped_cache [] in
  List.iter (Hashtbl.remove dropped_cache) old

let lookup_dropped hash =
  match Hashtbl.find_opt dropped_cache hash with
  | Some d ->
    let reason_str = match d.d_reason with Evicted -> "evicted" | Expired -> "expired" in
    Some (reason_str, d.d_detail, d.d_ts, d.d_from, d.d_to, d.d_nonce, d.d_ou, d.d_op_type)
  | None -> None

let all () =
  Hashtbl.fold (fun _ e acc -> e.tx :: acc) staging []
  |> List.sort (fun (a : Transaction.t) (b : Transaction.t) ->
    let c = String.compare a.from b.from in
    if c <> 0 then c
    else let c = compare a.nonce b.nonce in
    if c <> 0 then c
    else String.compare (Transaction.hash a) (Transaction.hash b))

let clear () =
  List.iter Hashtbl.clear [staging; hash_index];
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
  total_ou := Z.add !total_ou ou;
  Hashtbl.replace virtual_balances tx.from
    (Z.sub (Hashtbl.find virtual_balances tx.from) (public_balance_cost tx));
  Hashtbl.replace virtual_nonces tx.from
    (max (Hashtbl.find virtual_nonces tx.from) tx.nonce)

let evict entry =
  Hashtbl.remove staging entry.key;
  Hashtbl.remove hash_index entry.hash;
  total_ou := Z.sub !total_ou entry.ou;
  Hashtbl.replace virtual_balances entry.tx.from
    (Z.add (Hashtbl.find virtual_balances entry.tx.from)
       (public_balance_cost entry.tx))

let evict_nonce_dependents sender nonce =
  let dependents = Hashtbl.fold (fun _ e acc ->
    if e.tx.Transaction.from = sender && e.tx.Transaction.nonce > nonce then e :: acc else acc
  ) staging [] in
  let sorted = List.sort (fun a b -> compare a.tx.Transaction.nonce b.tx.Transaction.nonce) dependents in
  List.map (fun e ->
    evict e;
    record_drop e.hash e.tx Evicted "nonce gap from eviction";
    (e.hash, e.tx)
  ) sorted

let init_virtual_state ~lookup addr =
  let has_pending = Hashtbl.fold (fun _ e found ->
    found || e.tx.Transaction.from = addr) staging false in

  if not (Hashtbl.mem virtual_balances addr) || not has_pending then begin
    let balance, nonce = match lookup addr with
      | Some (b, n) -> (b, n)
      | None -> (Z.zero, 0)
    in
    Hashtbl.replace virtual_balances addr balance;
    Hashtbl.replace virtual_nonces addr nonce;

    if has_pending then
      Hashtbl.iter (fun _ e ->
        if e.tx.Transaction.from = addr then begin
          Hashtbl.replace virtual_balances addr
            (Z.sub (Hashtbl.find virtual_balances addr) (public_balance_cost e.tx));
          Hashtbl.replace virtual_nonces addr
            (max (Hashtbl.find virtual_nonces addr) e.tx.Transaction.nonce)
        end
      ) staging
  end

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

let add_smart ~lookup tx =
  let ou = Transaction.ou_cost tx in
  let key = tx.Transaction.from ^ string_of_int tx.nonce in
  let total_cost = public_balance_cost tx in
  init_virtual_state ~lookup tx.from;
  let base_nonce = match lookup tx.from with
    | Some (_, n) -> n | None -> 0
  in
  if tx.nonce <= base_nonce then Error "nonce too low (already used)"
  else if tx.nonce > base_nonce + 1000 then Error "nonce too far ahead"
  else if Z.gt ou max_ou then
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
          record_drop existing.hash existing.tx Evicted "replaced by higher fee-rate";
          insert tx;
          Ok [(existing.hash, existing.tx)]
        end
    | None ->
      if not (check_virtual_balance tx.from total_cost) then
        Error "insufficient balance (including pending)"
      else
        let needs_eviction =
          Z.gt (Z.add !total_ou ou) max_ou || Hashtbl.length staging >= max_staging_txs in
        if needs_eviction then begin
          let evictable_ou = Hashtbl.fold (fun _ e acc ->
            if Transaction.better_fee_rate tx e.tx then Z.add acc e.ou else acc
          ) staging Z.zero in
          let evictable_count = Hashtbl.fold (fun _ e acc ->
            if Transaction.better_fee_rate tx e.tx then acc + 1 else acc
          ) staging 0 in
          let ou_deficit = Z.max Z.zero (Z.sub (Z.add !total_ou ou) max_ou) in
          let count_deficit = max 0 (Hashtbl.length staging + 1 - max_staging_txs) in
          if Z.lt evictable_ou ou_deficit || evictable_count < count_deficit then
            Error "staging full (insufficient evictable capacity)"
          else
            let sorted_by_rate = Hashtbl.fold (fun _ e acc -> e :: acc) staging []
              |> List.sort (fun a b -> Transaction.cmp_fee_rate_desc a.tx b.tx |> ( * ) (-1)) in
            let rec evict_loop evicted_list = function
              | [] -> evicted_list
              | candidate :: rest ->
                if not (Hashtbl.mem staging candidate.key) then
                  evict_loop evicted_list rest
                else if Z.leq (Z.add !total_ou ou) max_ou && Hashtbl.length staging < max_staging_txs then
                  evicted_list
                else if not (Transaction.better_fee_rate tx candidate.tx) then
                  evicted_list
                else begin
                  evict candidate;
                  record_drop candidate.hash candidate.tx Evicted "outbid by higher fee-rate";
                  let deps = evict_nonce_dependents candidate.tx.Transaction.from candidate.tx.Transaction.nonce in
                  evict_loop ((candidate.hash, candidate.tx) :: deps @ evicted_list) rest
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

let get_epoch_txs ~capacity =
  let by_sender = Hashtbl.create 50 in
  List.iter (fun (tx : Transaction.t) ->
    let prev = match Hashtbl.find_opt by_sender tx.from with
      | Some l -> l | None -> [] in
    Hashtbl.replace by_sender tx.from (tx :: prev)
  ) (all ());
  let queues = Hashtbl.create 50 in
  let sorted_senders = Hashtbl.fold (fun k _ acc -> k :: acc) by_sender []
    |> List.sort String.compare in
  List.iter (fun sender ->
    match Hashtbl.find_opt by_sender sender with
    | Some txs ->
      let sorted = List.sort (fun (a : Transaction.t) b -> compare a.nonce b.nonce) txs in
      Hashtbl.add queues sender sorted
    | None -> ()
  ) sorted_senders;
  let next_tx () =
    let best = ref None in
    let senders = Hashtbl.fold (fun k _ acc -> k :: acc) queues []
      |> List.sort String.compare in
    List.iter (fun sender ->
      match Hashtbl.find_opt queues sender with
      | None | Some [] -> ()
      | Some ((tx : Transaction.t) :: _) ->
        (match !best with
         | None -> best := Some (sender, tx)
         | Some (_, btx) ->
           if Transaction.better_fee_rate tx btx then best := Some (sender, tx)
           else if Transaction.cmp_fee_rate_desc tx btx = 0
                   && String.compare (Transaction.hash tx) (Transaction.hash btx) < 0 then
             best := Some (sender, tx))
    ) senders;
    !best
  in
  let consume sender =
    let rest = match Hashtbl.find queues sender with
      | _ :: tl -> tl | [] -> [] in
    if rest = [] then Hashtbl.remove queues sender
    else Hashtbl.replace queues sender rest
  in
  let batch = ref [] in
  let used = ref Z.zero in
  let continue = ref true in
  while !continue do
    match next_tx () with
    | None -> continue := false
    | Some (sender, tx) ->
      let cost = Transaction.ou_cost tx in
      if Z.gt (Z.add !used cost) capacity then begin
        Hashtbl.remove queues sender;
        if Hashtbl.length queues = 0 then continue := false
      end else begin
        consume sender;
        batch := tx :: !batch;
        used := Z.add !used cost
      end
  done;
  List.rev !batch

let remove_by_hash h =
  match Hashtbl.find_opt hash_index h with
  | Some entry -> evict entry; true
  | None -> false

let remove_processed hashes =
  let touched = Hashtbl.create 16 in
  List.iter (fun h ->
    (match Hashtbl.find_opt hash_index h with
     | Some entry -> Hashtbl.replace touched entry.tx.Transaction.from ()
     | None -> ());
    ignore (remove_by_hash h)
  ) hashes;
  Hashtbl.iter (fun addr () ->
    let has_pending = Hashtbl.fold (fun _ e found ->
      found || e.tx.Transaction.from = addr) staging false in
    if not has_pending then begin
      Hashtbl.remove virtual_balances addr;
      Hashtbl.remove virtual_nonces addr
    end
  ) touched

let staging_ttl = 600.

let expire_old () =
  let now = Unix.gettimeofday () in
  let expired = Hashtbl.fold (fun _ e acc ->
    if now -. e.added_at > staging_ttl then e :: acc else acc
  ) staging [] in
  let touched = Hashtbl.create 16 in
  List.iter (fun e ->
    Hashtbl.replace touched e.tx.Transaction.from ();
    evict e;
    record_drop e.hash e.tx Expired "TTL exceeded"
  ) expired;

  Hashtbl.iter (fun addr () ->
    let has_pending = Hashtbl.fold (fun _ e found ->
      found || e.tx.Transaction.from = addr) staging false in
    if not has_pending then begin
      Hashtbl.remove virtual_balances addr;
      Hashtbl.remove virtual_nonces addr
    end
  ) touched;
  List.map (fun e -> (e.hash, e.tx)) expired

let staging_total_ou () = !total_ou

let staging_size () = Hashtbl.length staging

let staging_usage_pct () =
  if Z.equal max_ou Z.zero then 0
  else Z.to_int (Z.div (Z.mul !total_ou (Z.of_int 100)) max_ou)

let min_relay_fee tx =
  let usage = staging_usage_pct () in
  let base = Transaction.ou_cost tx in
  if usage < 50 then Z.one
  else if usage < 80 then base
  else if usage < 95 then Z.mul base (Z.of_int 2)
  else Z.mul base (Z.of_int 5)

let get_stats () =
  let txs = all () in
  let by_sender = Hashtbl.create 50 in
  List.iter (fun (tx : Transaction.t) ->
    let cost = public_balance_cost tx in
    let count, total = match Hashtbl.find_opt by_sender tx.from with
      | Some (c, t) -> (c + 1, Z.add t cost)
      | None -> (1, cost)
    in
    Hashtbl.replace by_sender tx.from (count, total)
  ) txs;
  (txs, by_sender)