(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Record budget := Budget { since : nat; spent : nat; pause : nat }.

Definition debit window now duration state :=
  if duration =? 0 then state else
  let fresh := window <=? now - since state in
  let used := (if fresh then 0 else spent state) + Nat.min window duration in
  Budget (if fresh then now else since state) used
    (if window <? used then now + window else pause state).

Definition allowed known banned now state :=
  (known || negb banned) && (pause state <=? now).

Theorem pause_applies_to_known : forall known banned now state,
  now < pause state -> allowed known banned now state = false.
Proof.
  intros. unfold allowed.
  assert (pause state <=? now = false) as no by (apply Nat.leb_gt; lia).
  now rewrite no, andb_false_r.
Qed.

Theorem charge_exact : forall window now duration state,
  0 < duration -> now - since state < window ->
  spent (debit window now duration state) = spent state + Nat.min window duration.
Proof.
  intros. unfold debit.
  assert (duration =? 0 = false) as positive by (apply Nat.eqb_neq; lia).
  assert (window <=? now - since state = false) as same by (apply Nat.leb_gt; lia).
  now rewrite positive, same.
Qed.

Theorem excess_pauses : forall window now duration state,
  0 < window -> 0 < duration -> now - since state < window ->
  window < spent state + Nat.min window duration ->
  forall known banned, allowed known banned now (debit window now duration state) = false.
Proof.
  intros. apply pause_applies_to_known. unfold debit.
  assert (duration =? 0 = false) as positive by (apply Nat.eqb_neq; lia).
  assert (window <=? now - since state = false) as same by (apply Nat.leb_gt; lia).
  assert (window <? spent state + Nat.min window duration = true) as over by
    (apply Nat.ltb_lt; lia).
  rewrite positive, same, over. simpl. lia.
Qed.

Definition charge window now durations state :=
  fold_left (fun state duration => debit window now duration state) durations state.

Example subsecond_counts :
  let next := charge 10000 2700 (repeat 900 12) (Budget 0 0 0) in
  spent next = 10800 /\ allowed false false 2700 next = false /\
  allowed true false 2700 next = false /\ allowed true false 12700 next = true.
Proof. vm_compute. auto. Qed.

Theorem expiry_reopens : forall now state,
  pause state <= now -> allowed true true now state = true.
Proof. intros. unfold allowed. simpl. now apply Nat.leb_le. Qed.

Theorem finish_overshoot : forall used window active duration slots wait,
  used <= window -> active <= slots -> duration <= wait ->
  used + active * duration <= window + slots * wait.
Proof. intros. apply Nat.add_le_mono; [assumption|]. now apply Nat.mul_le_mono. Qed.

Lemma debit_spent_cap : forall window now duration state,
  spent (debit window now duration state) <= spent state + duration.
Proof.
  intros. unfold debit. destruct (duration =? 0); [lia|].
  destruct (window <=? now - since state); simpl;
    pose proof (Nat.le_min_r window duration); lia.
Qed.

Lemma charge_spent_cap : forall durations window now state,
  spent (charge window now durations state) <= spent state + fold_right Nat.add 0 durations.
Proof.
  induction durations as [|duration rest IH]; intros window now state; [simpl; unfold charge; simpl; lia|].
  unfold charge. simpl. fold (charge window now rest (debit window now duration state)).
  pose proof (IH window now (debit window now duration state)).
  pose proof (debit_spent_cap window now duration state). lia.
Qed.

Lemma sum_wait_cap : forall durations wait,
  Forall (fun duration => duration <= wait) durations ->
  fold_right Nat.add 0 durations <= length durations * wait.
Proof. intros durations wait all. induction all; simpl; lia. Qed.

Theorem finish_charge_cap : forall durations window now state,
  spent state <= window -> length durations <= 4 ->
  Forall (fun duration => duration <= 5000) durations ->
  spent (charge window now durations state) <= window + 20000.
Proof.
  intros durations window now state used count times.
  eapply Nat.le_trans; [apply charge_spent_cap|].
  eapply Nat.le_trans; [apply Nat.add_le_mono_l; apply sum_wait_cap; exact times|].
  replace 20000 with (4 * 5000) by reflexivity.
  apply Nat.add_le_mono; [exact used|]. now apply Nat.mul_le_mono_r.
Qed.