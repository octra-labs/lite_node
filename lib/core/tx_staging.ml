(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let max_ou = Z.of_int 10_000_000_000
let max_staging_txs = 100_000
let staging_ttl = 600.
let byte_config = Staging_bytes.of_env Sys.getenv_opt

let check_config () = Result.map (fun _ -> ()) byte_config

type preview_hold = {head : int64; count : int; after : int64}
type hold_mode = Grow | Renew

let hold_after ?(mode = Grow) head previous =
  match previous with
  | Some previous when previous.head = head -> previous
  | _ ->
    let count = match mode, previous with
      | Grow, _ -> min 6 (1 + Option.fold ~none:0 ~some:(fun value -> value.count) previous)
      | Renew, Some value -> value.count
      | Renew, None -> invalid_arg "preview hold missing" in
    let delay = Int64.of_int (1 lsl (count - 1)) in
    let after = if head > Int64.sub Int64.max_int delay then Int64.max_int
      else Int64.add head delay in
    {head; count; after}

type entry = {
  tx : Transaction.t;
  ou : Z.t;
  key : string;
  added_at : float;
  hash : string;
  bytes : int;
  usage : Staging_bytes.usage;
  mutable preview : preview_hold option;
}

let staging : (string, entry) Hashtbl.t = Hashtbl.create 200
let hash_index : (string, entry) Hashtbl.t = Hashtbl.create 200

module Preview : sig
  type state
  type message =
    | Check of int64 option * string
    | Ready of int64 option * string
    | Refuse of { head : int64; epoch : int64; hash : string }
    | Hold of { head : int64; epoch : int64; hash : string }
    | Renew of { head : int64; epoch : int64; hash : string }
    | Sweep
  val empty : state
  val step : limit:int -> state -> message -> state * (unit, string) result
  val send : message -> (unit, string) result
end = struct
  module Hashes = Set.Make (String)
  type state = { head : int64; hashes : Hashes.t; size : int; full : bool }
  type message =
    | Check of int64 option * string
    | Ready of int64 option * string
    | Refuse of { head : int64; epoch : int64; hash : string }
    | Hold of { head : int64; epoch : int64; hash : string }
    | Renew of { head : int64; epoch : int64; hash : string }
    | Sweep

  let empty = { head = -1L; hashes = Hashes.empty; size = 0; full = false }

  let advance state = function
    | Some head when head > state.head -> { empty with head }
    | _ -> state

  let step ~limit state = function
    | Sweep -> {state with full = false}, Ok ()
    | Check (head, hash) ->
      let state = advance state head in
      let result =
        if Hashes.mem hash state.hashes then Error "preview refused in this epoch"
        else if state.full then Error "staging full until epoch commit"
        else Ok () in
      state, result
    | Ready (head, hash) ->
      let state = advance state head in
      state, (if Hashes.mem hash state.hashes then Error "preview refused in this epoch"
        else Ok ())
    | Refuse { head; epoch; hash } ->
      if head < 0L || head = Int64.max_int || head < state.head
         || epoch <> Int64.succ head then
        state, Error "preview epoch changed"
      else
        let state = advance state (Some head) in
        if Hashes.mem hash state.hashes then state, Ok ()
        else if state.size >= limit then
          { state with full = true }, Ok ()
        else { state with hashes = Hashes.add hash state.hashes; size = state.size + 1 }, Ok ()
    | Hold { head; epoch; hash = _ }
    | Renew { head; epoch; hash = _ } ->
      if head < 0L || head = Int64.max_int || head < state.head
         || epoch <> Int64.succ head then
        state, Error "preview epoch changed"
      else advance state (Some head), Ok ()

  let send =
    let state = ref empty in
    let lock = Mutex.create () in
    fun message ->
      Mutex.lock lock;
      Fun.protect ~finally:(fun () -> Mutex.unlock lock) (fun () ->
        let next, result = step ~limit:max_staging_txs !state message in
        let next, result = match message, result with
          | Hold {head; hash; _}, Ok () ->
            begin match Hashtbl.find_opt hash_index hash with
            | None -> next, Ok ()
            | Some entry -> entry.preview <- Some (hold_after head entry.preview); next, Ok ()
            end
          | Renew {head; hash; _}, Ok () ->
            begin match Hashtbl.find_opt hash_index hash with
            | Some ({preview = Some _; _} as entry) ->
              entry.preview <- Some (hold_after ~mode:Renew head entry.preview);
              next, Ok ()
            | _ -> next, Error "preview hold missing"
            end
          | (Check (_, hash) | Ready (_, hash)), _ ->
            let held = match Hashtbl.find_opt hash_index hash with
              | Some entry -> Option.fold ~none:false
                  ~some:(fun held -> next.head < held.after) entry.preview
              | None -> false in
            next, (if held then Error "preview refused in this epoch" else result)
          | _ -> next, result in
        state := next;
        result)
end

let public_balance_cost (tx : Transaction.t) =
  match tx.op_type with
  | DecryptOp | ClaimOp | StealthOp | PrivateOp -> tx.ou
  | _ -> Z.add tx.amount tx.ou

type queue_state =
  | Ready of { expires_at : float }
  | Wait_preview of { after : int64; attempts : int; expires_at : float }
  | Wait_nonce of { expected : int; expires_at : float }
  | Nonce_used of { confirmed : int; expires_at : float }

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

module Evict_index = Set.Make(struct
  type t = entry

  let compare a b =
    let rate = compare_entry_rate b a in
    if rate <> 0 then rate
    else
      let hash = String.compare a.hash b.hash in
      if hash <> 0 then hash else String.compare a.key b.key
end)

module Byte_index = Set.Make(struct
  type t = entry

  let compare a b =
    let rate = Z.compare
      (Z.mul a.tx.Transaction.ou (Z.of_int b.bytes))
      (Z.mul b.tx.Transaction.ou (Z.of_int a.bytes)) in
    if rate <> 0 then rate else String.compare a.key b.key
end)

module Recent_index = Set.Make(struct
  type t = entry

  let compare a b =
    let timestamp =
      Float.compare b.tx.Transaction.timestamp a.tx.Transaction.timestamp
    in
    if timestamp <> 0 then timestamp
    else
      let received = Float.compare b.added_at a.added_at in
      if received <> 0 then received else String.compare a.hash b.hash
end)

let view_index = ref Index.empty
let evict_index = ref Evict_index.empty
let byte_index = ref Byte_index.empty
let recent_index = ref Recent_index.empty
let total_ou = ref Z.zero
let byte_usage = ref Staging_bytes.zero
let sender_byte_usage : (string, Staging_bytes.usage) Hashtbl.t = Hashtbl.create 100

let sender_bytes sender =
  Option.value (Hashtbl.find_opt sender_byte_usage sender) ~default:Staging_bytes.zero

let staging_bytes () = !byte_usage

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

let first_missing_nonce sender confirmed =
  let rec loop expected = function
    | [] -> expected
    | entry :: rest ->
      let nonce = entry.tx.Transaction.nonce in
      if nonce < expected then loop expected rest
      else if nonce = expected then loop (expected + 1) rest
      else expected
  in
  loop (confirmed + 1) (sender_entries sender)

let duty_nonce sender confirmed =
  if confirmed < 0 || confirmed = max_int then None
  else
    let nonce = confirmed + 1 in
    if List.exists (fun entry -> entry.tx.Transaction.nonce = nonce)
         (sender_entries sender)
    then None
    else Some nonce

let duty_expired ?(marks = false) ?(mode = Rule_graph.Prior) ~head (tx : Transaction.t) =
  match head, tx.op_type with
  | Some head, ValidatorReady ->
    begin
      match Validator_registry.ready_payload_of_message tx.message with
      | Ok ready ->
        begin match mode with
        | Rule_graph.Prior -> Int64.compare ready.head_epoch head < 0
        | Rule_graph.Active ->
          ready.head_proposal_id = None
          || Validator_ready_policy.expired ~head ~reference:ready.head_epoch
          || (match Set_fold.evidence_of_message ~marks Set_fold.participating tx.message with
            | Error _ -> true
            | Ok Set_fold.Pulse -> false
            | Ok (Set_fold.Commit proof) ->
              let epoch = proof.vote.epoch_id in
              epoch < 0L || epoch <= head
                && Int64.sub head epoch >= Set_fold.participating.challenge
            | Ok (Set_fold.Marks claims) ->
              List.exists (fun (claim : Set_fold.mark_claim) ->
                claim.epoch < 0L || claim.epoch <= head
                  && Int64.sub head claim.epoch >= Set_fold.participating.challenge) claims)
        end
      | Error _ -> mode = Rule_graph.Active
    end
  | _ -> false

let preview_limit hash base =
  match Hashtbl.find_opt hash_index hash with
  | Some {preview = Some held; _} -> min 90. (base *. float_of_int (1 lsl held.count))
  | _ -> base

let queue_state ?head ~confirmed hash =
  match Hashtbl.find_opt hash_index hash with
  | None -> None
  | Some entry ->
    let expires_at = entry.added_at +. staging_ttl in
    if entry.tx.Transaction.nonce <= confirmed then
      Some (Nonce_used { confirmed; expires_at })
    else
      let expected = first_missing_nonce entry.tx.from confirmed in
      if expected < entry.tx.nonce then
        Some (Wait_nonce { expected; expires_at })
      else match entry.preview with
        | Some held when Option.fold ~none:true ~some:(fun head -> head < held.after) head ->
          Some (Wait_preview { after = held.after; attempts = held.count; expires_at })
        | _ -> Some (Ready { expires_at })

let sample limit =
  first (max 0 limit) (Index.to_seq !view_index) []
  |> List.map (fun (_, entry) -> entry.hash, entry.tx)

let recent limit =
  first (max 0 limit) (Recent_index.to_seq !recent_index) []
  |> List.map (fun entry -> entry.hash, entry.tx)

let clear () =
  List.iter Hashtbl.clear [staging; hash_index];
  view_index := Index.empty;
  evict_index := Evict_index.empty;
  byte_index := Byte_index.empty;
  recent_index := Recent_index.empty;
  Hashtbl.clear virtual_balances;
  Hashtbl.clear virtual_nonces;
  total_ou := Z.zero;
  byte_usage := Staging_bytes.zero;
  Hashtbl.clear sender_byte_usage

let find_by_hash h =
  Hashtbl.find_opt hash_index h |> Option.map (fun e -> e.tx)

let insert ~bytes ~usage tx ou =
  let key = tx.Transaction.from ^ string_of_int tx.nonce in
  let hash = Transaction.hash tx in
  let added = usage in
  let total = Staging_bytes.add !byte_usage added in
  let sender = Staging_bytes.add (sender_bytes tx.from) added in
  let entry = { tx; ou; key; added_at = Unix.gettimeofday (); hash; bytes; usage;
    preview = None } in
  Hashtbl.add staging key entry;
  Hashtbl.add hash_index hash entry;
  view_index := Index.add (index_key entry) entry !view_index;
  evict_index := Evict_index.add entry !evict_index;
  if usage.regular > 0 then
    byte_index := Byte_index.add entry !byte_index;
  recent_index := Recent_index.add entry !recent_index;
  total_ou := Z.add !total_ou ou;
  byte_usage := total;
  Hashtbl.replace sender_byte_usage tx.from sender;
  Hashtbl.replace virtual_balances tx.from
    (Z.sub (Hashtbl.find virtual_balances tx.from) (public_balance_cost tx));
  Hashtbl.replace virtual_nonces tx.from
    (max (Hashtbl.find virtual_nonces tx.from) tx.nonce)

let evict entry =
  let removed = entry.usage in
  let total = Staging_bytes.remove !byte_usage removed in
  let sender = Staging_bytes.remove (sender_bytes entry.tx.from) removed in
  Hashtbl.remove staging entry.key;
  Hashtbl.remove hash_index entry.hash;
  view_index := Index.remove (index_key entry) !view_index;
  evict_index := Evict_index.remove entry !evict_index;
  byte_index := Byte_index.remove entry !byte_index;
  recent_index := Recent_index.remove entry !recent_index;
  total_ou := Z.sub !total_ou entry.ou;
  byte_usage := total;
  if sender = Staging_bytes.zero then Hashtbl.remove sender_byte_usage entry.tx.from
  else Hashtbl.replace sender_byte_usage entry.tx.from sender;
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
  sender_entries addr
  |> List.fold_left
    (fun nonce entry -> max nonce entry.tx.Transaction.nonce)
    confirmed

let better_rate fee cost entry =
  Z.gt
    (Z.mul fee entry.ou)
    (Z.mul entry.tx.Transaction.ou cost)

let rbf_bump_ok new_tx new_ou old =
  Z.geq
    (Z.mul (Z.mul new_tx.Transaction.ou old.ou) (Z.of_int 100))
    (Z.mul (Z.mul old.tx.Transaction.ou new_ou) (Z.of_int 110))

let pending_balance_check ~balance ~confirmed tx =
  let existing = Hashtbl.find_opt staging (tx.Transaction.from ^ string_of_int tx.nonce) in
  if Option.fold ~none:false ~some:(fun old ->
      not (rbf_bump_ok tx (Transaction.ou_cost tx) old)) existing then
    Error "duplicate nonce (fee rate bump < 10%)"
  else
    let reserved = sender_entries tx.from |> List.fold_left (fun total entry ->
      if entry.tx.nonce > confirmed && entry.tx.nonce <> tx.nonce then
        Z.add total (public_balance_cost entry.tx) else total) Z.zero in
    if Z.gt (Z.add reserved (public_balance_cost tx)) balance then
      Error "insufficient balance (including pending)"
    else Ok ()

let byte_plan limits ~ou_limit ~tx_limit ~added tx ou bytes =
  let sender = sender_bytes tx.Transaction.from in
  let check total = Staging_bytes.plan limits ~total ~sender
    ~removed:Staging_bytes.zero ~added in
  match check sender with
  | Error _ as error -> error
  | Ok () ->
    match check !byte_usage with
    | Ok () -> Ok []
    | Error reason when added.duty > 0 -> Error reason
    | Error reason ->
      let compute_full = Z.gt (Z.add !total_ou ou) ou_limit
        || Hashtbl.length staging >= tx_limit in
      let better entry =
        entry.usage.regular > 0
        && not (String.equal tx.from entry.tx.from)
        && Z.gt (Z.mul tx.ou (Z.of_int entry.bytes))
          (Z.mul entry.tx.ou (Z.of_int bytes))
        && (not compute_full || better_rate tx.ou ou entry) in
      let last index sender =
        match Index.find_last_opt (fun (address, _, _) -> String.compare address sender <= 0) index with
        | Some (_, entry) when String.equal entry.tx.from sender -> Some entry
        | _ -> None in
      let rec choose pending index selected total cost count =
        if check total = Ok () && Z.leq (Z.add cost ou) ou_limit
           && count < tx_limit then Ok (Byte_index.elements selected)
        else match Byte_index.min_elt_opt pending with
        | None -> Error reason
        | Some entry ->
          let pending = Byte_index.remove entry pending in
          let final = match last index entry.tx.from with
            | Some final -> String.equal entry.key final.key
            | None -> false in
          if not final || not (better entry) then
            choose pending index selected total cost count
          else
            let index = Index.remove (index_key entry) index in
            let pending = match last index entry.tx.from with
              | Some previous when previous.usage.regular > 0 ->
                Byte_index.add previous pending
              | _ -> pending in
            choose pending index (Byte_index.add entry selected)
              (Staging_bytes.remove total entry.usage)
              (Z.sub cost entry.ou) (count - 1) in
      choose !byte_index !view_index Byte_index.empty
        !byte_usage !total_ou (Hashtbl.length staging)

let add_planned ?head ?(ou_limit = max_ou) ?(tx_limit = max_staging_txs) ?byte_limits
    ~accept ~lookup tx =
  let ( let* ) = Result.bind in
  let* byte_limits = match byte_limits with
    | Some limits -> Ok limits
    | None -> byte_config in
  let* bytes = Staging_bytes.measure tx in
  let* () = Preview.send (Check (head, Transaction.hash tx)) in
  let ou = Transaction.ou_cost tx in
  let key = tx.Transaction.from ^ string_of_int tx.nonce in
  let total_cost = public_balance_cost tx in
  let existing = Hashtbl.find_opt staging key in
  let base_nonce = match lookup tx.from with
    | Some (_, n) -> n | None -> 0 in
  let usage =
    if tx.op_type = Transaction.ValidatorReady
       && Int64.of_int tx.nonce <> Int64.succ (Int64.of_int base_nonce) then
      Staging_bytes.{regular = bytes; duty = 0}
    else Staging_bytes.of_tx tx bytes in
  let check_bytes removed = Staging_bytes.plan byte_limits
    ~total:!byte_usage ~sender:(sender_bytes tx.from) ~removed
    ~added:usage in
  match init_virtual_state ~lookup tx.from with
  | Error _ as error -> error
  | Ok () ->
  if Z.sign ou_limit <= 0 || tx_limit <= 0 then
    Error "staging limits must be positive"
  else if tx.nonce <= base_nonce then Error "nonce too low (already used)"
  else if tx.nonce > base_nonce + 1000 then Error "nonce too far ahead"
  else if Z.gt ou ou_limit then
    Error "transaction too large for staging capacity"
  else
    match existing with
    | Some existing ->
      if not (rbf_bump_ok tx ou existing) then
        Error "duplicate nonce (fee rate bump < 10%)"
      else
        let next_ou = Z.add (Z.sub !total_ou existing.ou) ou in
        let delta = Z.sub total_cost (public_balance_cost existing.tx) in
        if Z.gt next_ou ou_limit || Hashtbl.length staging > tx_limit then
          Error "staging full (insufficient evictable capacity)"
        else if Z.gt delta Z.zero && not (check_virtual_balance tx.from delta) then
          Error "insufficient balance for replacement"
        else
          let* () = check_bytes existing.usage in
          let* () = accept () in
          begin
          evict existing;
          let dropped =
            record_drop
              existing.hash
              existing.tx
              Evicted
              "replaced by higher fee-rate"
          in
          insert ~bytes ~usage tx ou;
          Ok [dropped]
        end
    | None ->
      if not (check_virtual_balance tx.from total_cost) then
        Error "insufficient balance (including pending)"
      else
      let* removals = byte_plan byte_limits ~ou_limit ~tx_limit ~added:usage tx ou bytes in
      if removals <> [] then begin
        let* () = accept () in
        let dropped = List.map (fun entry ->
          evict entry;
          record_drop entry.hash entry.tx Evicted "outbid by higher byte fee") removals in
        insert ~bytes ~usage tx ou;
        Ok dropped
      end
      else
        let needs_eviction =
          Z.gt (Z.add !total_ou ou) ou_limit || Hashtbl.length staging >= tx_limit in
        if needs_eviction then begin
          let ou_deficit = Z.max Z.zero (Z.sub (Z.add !total_ou ou) ou_limit) in
          let count_deficit = max 0 (Hashtbl.length staging + 1 - tx_limit) in
          match Evict_index.min_elt_opt !evict_index with
          | Some entry
            when count_deficit <= 1
                 && Z.geq entry.ou ou_deficit
                 && better_rate tx.Transaction.ou ou entry ->
            let* () = accept () in
            evict entry;
            let dropped =
              record_drop
                entry.hash
                entry.tx
                Evicted
                "outbid by higher fee-rate"
            in
            let deps =
              evict_nonce_dependents
                entry.tx.Transaction.from
                entry.tx.Transaction.nonce
            in
            insert ~bytes ~usage tx ou;
            Ok (dropped :: deps)
          | _ ->
            Error "staging full (insufficient evictable capacity)"
        end
        else let* () = accept () in insert ~bytes ~usage tx ou; Ok []

let add_smart ?head ?ou_limit ?tx_limit ?byte_limits ?(accept = fun () -> Ok ()) ~lookup tx =
  let clean sender =
    if not (sender_has_pending sender) then begin
      Hashtbl.remove virtual_balances sender;
      Hashtbl.remove virtual_nonces sender
    end in
  Fun.protect ~finally:(fun () -> clean tx.Transaction.from) (fun () ->
    let result = add_planned ?head ?ou_limit ?tx_limit ?byte_limits ~accept ~lookup tx in
    begin match result with
    | Ok drops -> List.iter (fun drop -> clean drop.d_from) drops
    | Error _ -> ()
    end;
    result)

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

let ready_queue ~confirmed queue =
  let rec take expected kept = function
    | [] -> List.rev kept
    | entry :: rest ->
      let nonce = entry.tx.Transaction.nonce in
      if nonce < expected then take expected kept rest
      else if nonce > expected then List.rev kept
      else if nonce = max_int then List.rev (entry :: kept)
      else take (expected + 1) (entry :: kept) rest
  in
  if confirmed < 0 || confirmed = max_int then []
  else take (confirmed + 1) [] queue

let ready_epoch_txs ~accept ~capacity ~confirmed_nonce =
  let queues =
    sender_queues ()
    |> Sender_queue.mapi (fun sender queue ->
      match confirmed_nonce sender with
      | None -> []
      | Some confirmed ->
        List.filter (fun entry -> accept entry.tx) queue
        |> ready_queue ~confirmed)
  in
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

let drop_preview tx =
  let hash = Transaction.hash tx in
  match Hashtbl.find_opt hash_index hash with
  | None -> None
  | Some entry ->
    remove_processed [hash];
    Some (record_drop hash entry.tx Evicted "preview rejected")

let expire_duty ?sender ?(marks = false) ?(mode = Rule_graph.Prior) ~head () =
  match head with
  | None -> []
  | Some _ ->
    let entries =
      match sender with
      | Some sender -> sender_entries sender
      | None -> Index.bindings !view_index |> List.map snd
    in
    let removed =
      List.filter (fun entry ->
        entry.tx.Transaction.op_type = Transaction.ValidatorReady
        && (not (Tx_payload.size_ok ~limits:Tx_payload.standard entry.tx)
            || duty_expired ~marks ~mode ~head entry.tx)) entries
    in
    let records =
      List.map (fun entry ->
        if Tx_payload.size_ok ~limits:Tx_payload.standard entry.tx then
          record_drop entry.hash entry.tx Expired "validator ready head has expired"
        else
          record_drop entry.hash entry.tx Evicted
            "encrypted_data or message exceeds size limit") removed
    in
    remove_processed (List.map (fun entry -> entry.hash) removed);
    records

let expiry_reason ~confirmed ~received =
  let gap = Int64.sub (Int64.of_int received) (Int64.of_int confirmed) in
  if Int64.compare gap 1L > 0 then
    Printf.sprintf
      "TTL exceeded: waiting for nonce %d before nonce %d"
      (confirmed + 1)
      received
  else if Int64.compare gap 0L <= 0 then
    Printf.sprintf
      "TTL exceeded: nonce %d was already consumed at %d"
      received
      confirmed
  else
    "TTL exceeded: transaction was not included"

let expire_old ?(confirmed_nonce = fun _ -> None) ?now () =
  let now = match now with Some now -> now | None -> Unix.gettimeofday () in
  let expired = Hashtbl.fold (fun _ (e : entry) acc ->
    if now -. e.added_at > staging_ttl then e :: acc else acc
  ) staging [] in
  let touched = Hashtbl.create 16 in
  let confirmed = Hashtbl.create 16 in
  let find_confirmed sender =
    match Hashtbl.find_opt confirmed sender with
    | Some value -> value
    | None ->
      let value = confirmed_nonce sender in
      Hashtbl.add confirmed sender value;
      value
  in
  let records =
    List.map (fun (e : entry) ->
      Hashtbl.replace touched e.tx.Transaction.from ();
      evict e;
      let detail =
        match find_confirmed e.tx.from with
        | Some confirmed ->
          expiry_reason ~confirmed ~received:e.tx.Transaction.nonce
        | None ->
          "TTL exceeded: transaction was not included"
      in
      record_drop e.hash e.tx Expired detail
    ) expired
  in
  clear_virtual_state touched;
  if records <> [] then ignore (Preview.send Sweep);
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

let stats_of ?(sender = fun (tx : Transaction.t) -> tx.from) txs =
  let by_sender = Hashtbl.create 50 in
  List.iter (fun (tx : Transaction.t) ->
    let cost = public_balance_cost tx in
    let address = sender tx in
    let count, total = match Hashtbl.find_opt by_sender address with
      | Some (c, t) -> (c + 1, Z.add t cost)
      | None -> (1, cost)
    in
    Hashtbl.replace by_sender address (count, total)
  ) txs;
  by_sender

let get_stats () =
  let txs = all () in
  txs, stats_of txs