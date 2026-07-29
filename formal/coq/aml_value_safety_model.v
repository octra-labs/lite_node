(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith.ZArith.
From Stdlib Require Import Lia.

Open Scope Z_scope.

Definition signed_sender_after sender_balance amount :=
  sender_balance - amount.

Definition unsigned_entry_accepts amount :=
  0 <=? amount.

Definition unsigned_sender_after sender_balance amount :=
  if unsigned_entry_accepts amount then
    Some (sender_balance - amount)
  else
    None.

Theorem signed_negative_amount_increases_sender :
  forall sender_balance amount,
    amount < 0 ->
    signed_sender_after sender_balance amount > sender_balance.
Proof.
  intros sender_balance amount negative.
  unfold signed_sender_after.
  lia.
Qed.

Theorem unsigned_entry_rejects_negative :
  forall amount,
    amount < 0 ->
    unsigned_entry_accepts amount = false.
Proof.
  intros amount negative.
  unfold unsigned_entry_accepts.
  apply Z.leb_gt.
  lia.
Qed.

Theorem unsigned_sender_rejects_negative :
  forall sender_balance amount,
    amount < 0 ->
    unsigned_sender_after sender_balance amount = None.
Proof.
  intros sender_balance amount negative.
  unfold unsigned_sender_after.
  rewrite unsigned_entry_rejects_negative by exact negative.
  reflexivity.
Qed.

Theorem unsigned_debit_preserves_nonnegative :
  forall sender_balance amount,
    0 <= sender_balance ->
    0 <= amount ->
    amount <= sender_balance ->
    0 <= sender_balance - amount.
Proof.
  intros sender_balance amount balance_nonnegative amount_nonnegative sufficient.
  lia.
Qed.

Definition transfer_sender_after sender_balance amount :=
  sender_balance - amount.

Definition transfer_receiver_after receiver_balance amount :=
  receiver_balance + amount.

Definition mint_supply_after total_supply amount :=
  total_supply + amount.

Definition burn_supply_after total_supply amount :=
  total_supply - amount.

Theorem transfer_preserves_pair_sum :
  forall sender_balance receiver_balance amount,
    transfer_sender_after sender_balance amount
    + transfer_receiver_after receiver_balance amount
    = sender_balance + receiver_balance.
Proof.
  intros sender_balance receiver_balance amount.
  unfold transfer_sender_after, transfer_receiver_after.
  lia.
Qed.

Theorem mint_preserves_supply_balance_delta :
  forall total_supply receiver_balance amount,
    mint_supply_after total_supply amount - total_supply
    = (receiver_balance + amount) - receiver_balance.
Proof.
  intros total_supply receiver_balance amount.
  unfold mint_supply_after.
  lia.
Qed.

Theorem burn_preserves_supply_balance_delta :
  forall total_supply sender_balance amount,
    total_supply - burn_supply_after total_supply amount
    = sender_balance - (sender_balance - amount).
Proof.
  intros total_supply sender_balance amount.
  unfold burn_supply_after.
  lia.
Qed.