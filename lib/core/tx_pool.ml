(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let max_ou = Z.of_int 1_000_000_000

type entry = {
  tx : Transaction.t;
  ou : Z.t;
  key : string;
  added_at : float;
  hash : string;
}

let pool : (string, entry) Hashtbl.t = Hashtbl.create 200
let hash_index : (string, entry) Hashtbl.t = Hashtbl.create 200
let total_ou = ref Z.zero

let virtual_balances : (string, Z.t) Hashtbl.t = Hashtbl.create 100
let virtual_nonces : (string, int) Hashtbl.t = Hashtbl.create 100

let all () = Hashtbl.fold (fun _ e acc -> e.tx :: acc) pool []

let clear () =
  List.iter Hashtbl.clear [pool; hash_index];
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
  Hashtbl.add pool key entry;
  Hashtbl.add hash_index hash entry;
  total_ou := Z.add !total_ou ou;
  Hashtbl.replace virtual_balances tx.from
    (Z.sub (Hashtbl.find virtual_balances tx.from) (Z.add tx.amount ou));
  Hashtbl.replace virtual_nonces tx.from
    (max (Hashtbl.find virtual_nonces tx.from) tx.nonce)

let evict entry =
  Hashtbl.remove pool entry.key;
  Hashtbl.remove hash_index entry.hash;
  total_ou := Z.sub !total_ou entry.ou;
  Hashtbl.replace virtual_balances entry.tx.from
    (Z.add (Hashtbl.find virtual_balances entry.tx.from)
       (Z.add entry.tx.amount entry.ou))

let init_virtual_state ~lookup addr =
  if not (Hashtbl.mem virtual_balances addr) then
    let balance, nonce = match lookup addr with
      | Some (b, n) -> (b, n)
      | None -> (Z.zero, 0)
    in
    Hashtbl.add virtual_balances addr balance;
    Hashtbl.add virtual_nonces addr nonce

let check_virtual_balance addr amount =
  match Hashtbl.find_opt virtual_balances addr with
  | Some b -> Z.geq b amount | None -> false

let get_expected_nonce addr =
  match Hashtbl.find_opt virtual_nonces addr with
  | Some n -> n + 1 | None -> 1

let add_smart ~lookup tx =
  let ou = Transaction.ou_cost tx in
  let key = tx.Transaction.from ^ string_of_int tx.nonce in
  let total_cost = Z.add tx.amount ou in
  init_virtual_state ~lookup tx.from;
  let base_nonce = match lookup tx.from with
    | Some (_, n) -> n | None -> 0
  in
  if Hashtbl.mem pool key then Error "Duplicate transaction"
  else if tx.nonce <= base_nonce then Error "Nonce too low (already used)"
  else if tx.nonce > base_nonce + 5 then Error "Nonce too far ahead"
  else if not (check_virtual_balance tx.from total_cost) then
    Error "Insufficient balance (including pending)"
  else if Z.gt (Z.add !total_ou ou) max_ou then
    let candidates =
      Hashtbl.fold (fun _ e acc ->
        if Transaction.compare_by_priority e.tx tx > 0 then e :: acc else acc
      ) pool []
    in
    match candidates with
    | [] -> Error "Staging pool full (low priority)"
    | _ ->
      let victim = candidates
        |> List.sort (fun a b -> Transaction.compare_by_priority a.tx b.tx)
        |> List.hd in
      evict victim; insert tx; Ok ()
  else (insert tx; Ok ())

let get_ordered_txs () =
  let by_sender = Hashtbl.create 50 in
  List.iter (fun (tx : Transaction.t) ->
    let prev = match Hashtbl.find_opt by_sender tx.from with
      | Some l -> l | None -> [] in
    Hashtbl.replace by_sender tx.from (tx :: prev)
  ) (all ());
  Hashtbl.fold (fun _ txs acc ->
    List.sort (fun (a : Transaction.t) b -> compare a.nonce b.nonce) txs @ acc
  ) by_sender []

let remove_by_hash h =
  match Hashtbl.find_opt hash_index h with
  | Some entry -> evict entry; true
  | None -> false

let get_stats () =
  let txs = all () in
  let by_sender = Hashtbl.create 50 in
  List.iter (fun (tx : Transaction.t) ->
    let cost = Z.add tx.amount (Transaction.ou_cost tx) in
    let count, total = match Hashtbl.find_opt by_sender tx.from with
      | Some (c, t) -> (c + 1, Z.add t cost)
      | None -> (1, cost)
    in
    Hashtbl.replace by_sender tx.from (count, total)
  ) txs;
  (txs, by_sender)