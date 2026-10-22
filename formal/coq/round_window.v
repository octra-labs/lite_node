(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Bool Arith Lia.

Definition keep_vote active recent saved current old :=
  (old <? current) && (recent || (active && saved)).

Theorem prior_window : forall recent saved current old,
  keep_vote false recent saved current old = ((old <? current) && recent).
Proof. intros. unfold keep_vote. simpl. rewrite orb_false_r. reflexivity. Qed.

Theorem old_vote_needs_proof : forall recent saved current old,
  recent = false -> keep_vote true recent saved current old = true ->
  old < current /\ saved = true.
Proof.
  intros recent saved current old absent accepted.
  unfold keep_vote in accepted. rewrite absent in accepted. simpl in accepted.
  apply andb_true_iff in accepted as [earlier proof].
  apply Nat.ltb_lt in earlier. auto.
Qed.

Theorem saved_vote_kept : forall current old,
  old < current -> keep_vote true false true current old = true.
Proof. intros. unfold keep_vote. simpl. rewrite andb_true_r. apply Nat.ltb_lt. auto. Qed.

Theorem future_vote_refused : forall active recent saved current old,
  current <= old -> keep_vote active recent saved current old = false.
Proof.
  intros. unfold keep_vote. assert ((old <? current) = false) as later by (apply Nat.ltb_ge; lia).
  rewrite later. reflexivity.
Qed.

Print Assumptions prior_window.
Print Assumptions old_vote_needs_proof.
Print Assumptions saved_vote_kept.
Print Assumptions future_vote_refused.