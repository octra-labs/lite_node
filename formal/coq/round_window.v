(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Bool Arith Lia List.
Import ListNotations.

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

Record reply_state := { generation : nat; proof : option nat }.

Definition finish_reply sent value state :=
  if sent =? generation state then
    {| generation := generation state;
       proof := Some (match proof state with None => value | Some prior => Nat.max prior value end) |}
  else state.

Fixpoint finish_replies replies state :=
  match replies with
  | nil => state
  | (sent, value) :: rest => finish_replies rest (finish_reply sent value state)
  end.

Theorem old_replies_no_writes : forall replies state,
  List.Forall (fun entry => fst entry < generation state) replies ->
  finish_replies replies state = state.
Proof.
  induction replies as [|[sent value] rest IH]; intros state old; simpl; auto.
  inversion old as [|entry remaining before more]; subst. simpl in before.
  unfold finish_reply. assert ((sent =? generation state) = false) as different
    by (apply Nat.eqb_neq; lia).
  rewrite different. apply IH. exact more.
Qed.

Theorem completed_generation_ignored : forall replies current saved,
  List.Forall (fun entry => fst entry <= current) replies ->
  finish_replies replies {| generation := S current; proof := saved |} =
    {| generation := S current; proof := saved |}.
Proof.
  intros replies current saved prior. apply old_replies_no_writes.
  eapply List.Forall_impl; [|exact prior]. intros [sent value] earlier. simpl in *. lia.
Qed.

Print Assumptions old_replies_no_writes.
Print Assumptions completed_generation_ignored.

Definition proof_round state := match proof state with None => 0 | Some value => value end.

Theorem reply_keeps_proof : forall sent value state,
  proof_round state <= proof_round (finish_reply sent value state).
Proof.
  intros. unfold finish_reply. destruct (sent =? generation state); [|lia].
  destruct state as [epoch [prior|]]; cbn [proof_round]; [apply Nat.le_max_l|apply Nat.le_0_l].
Qed.

Theorem completion_order_keeps_proof : forall replies state,
  proof_round state <= proof_round (finish_replies replies state).
Proof.
  induction replies as [|[sent value] rest IH]; intros state; simpl; [lia|].
  eapply Nat.le_trans; [apply reply_keeps_proof|apply IH].
Qed.

Definition finish_fault (round : nat) (success : bool) (fault : option nat) :=
  match fault with
  | Some prior => if success then
      if prior =? round then None else fault
    else Some (Nat.max prior round)
  | None => if success then None else Some round
  end.

Theorem newer_fault_preserved : forall round success prior,
  round < prior -> finish_fault round success (Some prior) = Some prior.
Proof.
  intros. unfold finish_fault. destruct success.
  - assert ((prior =? round) = false) as unequal by (apply Nat.eqb_neq; lia).
    rewrite unequal. reflexivity.
  - rewrite Nat.max_l by lia. reflexivity.
Qed.

Print Assumptions reply_keeps_proof.
Print Assumptions completion_order_keeps_proof.
Print Assumptions newer_fault_preserved.