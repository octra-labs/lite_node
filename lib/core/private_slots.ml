(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Senders = Set.Make (String)

type limits = { fhe : int; stealth : int }
type t = { left : limits; debits : Senders.t }

let create limits =
  { left = {fhe = max 0 limits.fhe; stealth = max 0 limits.stealth};
    debits = Senders.empty }

let reserve t (tx : Transaction.t) =
  let fhe, stealth, debit = match tx.op_type with
    | EncryptOp | ClaimOp | KeySwitch -> true, false, false
    | DecryptOp -> true, false, true
    | StealthOp -> true, true, true
    | _ -> false, false, false in
  if (fhe && t.left.fhe = 0)
     || (stealth && t.left.stealth = 0)
     || (debit && Senders.mem tx.from t.debits)
  then None
  else Some {
    left = {
      fhe = t.left.fhe - (if fhe then 1 else 0);
      stealth = t.left.stealth - (if stealth then 1 else 0);
    };
    debits = if debit then Senders.add tx.from t.debits else t.debits;
  }

let select ~limits ~inputs ~ready =
  let verified = List.fold_left (fun set tx ->
    Senders.add (Transaction.hash tx) set) Senders.empty ready in
  let rec next slots blocked selected = function
    | [] -> List.rev selected
    | (tx : Transaction.t) :: rest ->
      if Senders.mem tx.from blocked then next slots blocked selected rest
      else if not (Senders.mem (Transaction.hash tx) verified) then
        next slots (Senders.add tx.from blocked) selected rest
      else match reserve slots tx with
        | None -> next slots (Senders.add tx.from blocked) selected rest
        | Some slots -> next slots blocked (tx :: selected) rest in
  next (create limits) Senders.empty [] inputs