(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith Bool List Lia.
Import ListNotations.

Inductive recovery := Waiting | Released | Held.
Inductive answer := Peer_wait | Local_fault | Ready.

Fixpoint recover (answers : list answer) : recovery :=
  match answers with
  | [] => Waiting
  | Ready :: _ => Released
  | Local_fault :: _ => Held
  | Peer_wait :: rest => recover rest
  end.

Theorem release_has_evidence : forall answers,
  recover answers = Released -> In Ready answers.
Proof.
  induction answers as [|value rest ih]; simpl; intros accepted; try discriminate.
  destruct value; simpl in *; try discriminate; auto.
Qed.

Theorem peer_wait_is_not_fault : forall waits,
  recover (repeat Peer_wait waits) = Waiting.
Proof. induction waits; simpl; auto. Qed.

Theorem ready_after_waits : forall waits rest,
  recover (repeat Peer_wait waits ++ Ready :: rest) = Released.
Proof.
  induction waits; intros rest; simpl; auto.
Qed.

Theorem fault_cannot_release : forall waits rest,
  recover (repeat Peer_wait waits ++ Local_fault :: rest) = Held.
Proof.
  induction waits; intros rest; simpl; auto.
Qed.

Inductive writer := Idle | Writing (epoch : nat) | Stopped (epoch : nat).
Definition deadline (w : writer) :=
  match w with Writing epoch => Stopped epoch | _ => w end.
Definition begin_write (epoch : nat) (w : writer) :=
  match w with Idle => Writing epoch | _ => w end.
Definition saved (w : writer) :=
  match w with Idle => None | Writing epoch | Stopped epoch => Some epoch end.

Theorem timeout_keeps_journal : forall epoch,
  saved (deadline (Writing epoch)) = Some epoch.
Proof. reflexivity. Qed.

Theorem stopped_cannot_start : forall epoch next,
  begin_write next (deadline (Writing epoch)) = Stopped epoch.
Proof. reflexivity. Qed.

Definition target_next (grow recent : bool) (initial target epoch : nat) :=
  if grow && (initial <=? epoch) then
    if recent then epoch
    else if target <=? epoch then epoch + 16 else target
  else target.

Theorem fixed_target : forall recent initial target epoch,
  target_next false recent initial target epoch = target.
Proof. reflexivity. Qed.

Theorem growth_requires_applied : forall grow recent initial target epoch,
  target < target_next grow recent initial target epoch -> target <= epoch.
Proof.
  intros grow recent initial target epoch.
  unfold target_next. destruct (grow && (initial <=? epoch)); try lia.
  destruct recent; try lia.
  destruct (target <=? epoch) eqn:reached; try lia.
  apply Nat.leb_le in reached. lia.
Qed.

Theorem growth_below_goal : forall grow initial target epoch,
  epoch < target -> target_next grow false initial target epoch = target.
Proof.
  intros. unfold target_next. destruct (grow && (initial <=? epoch)); auto.
  assert ((target <=? epoch) = false) by (apply Nat.leb_gt; lia).
  rewrite H0. reflexivity.
Qed.

Record stamp := Stamp { path_id : nat; inode : nat; size : nat; changed : nat; trust : nat }.
Definition stamp_same a b :=
  (path_id a =? path_id b) && (inode a =? inode b) &&
  (size a =? size b) && (changed a =? changed b) && (trust a =? trust b).

Theorem cache_identity : forall a b,
  stamp_same a b = true -> a = b.
Proof.
  intros [ap ai az ac atrust] [bp bi bz bc btrust] same.
  unfold stamp_same in same. simpl in same.
  repeat rewrite Bool.andb_true_iff in same.
  destruct same as [[[[hp hi] hz] hc] ht].
  apply Nat.eqb_eq in hp, hi, hz, hc, ht. subst. reflexivity.
Qed.

Definition serve saved_head epoch := epoch <=? saved_head.

Theorem range_stops_at_saved : forall saved_head live_head epoch,
  saved_head <= live_head -> serve saved_head epoch = true -> epoch <= saved_head.
Proof. intros. apply Nat.leb_le. exact H0. Qed.

Record applied := Applied { epoch : nat; root : nat; block_id : nat }.
Definition same_block a b :=
  (epoch a =? epoch b) && (root a =? root b) && (block_id a =? block_id b).
Definition complete head history item :=
  (epoch item <=? head) && existsb (same_block item) history.
Definition finish_wait head history item (pending : option applied) :=
  (if complete head history item then None else Some item, pending).

Theorem same_block_exact : forall a b,
  same_block a b = true <-> a = b.
Proof.
  intros [ae ar ab] [be br bb]. unfold same_block. simpl.
  repeat rewrite Bool.andb_true_iff.
  repeat rewrite Nat.eqb_eq. split.
  intros [[? ?] ?]. subst. reflexivity.
  intros equal. inversion equal. auto.
Qed.

Theorem completion_has_record : forall head history item,
  complete head history item = true <-> epoch item <= head /\ In item history.
Proof.
  intros head history item. unfold complete.
  rewrite Bool.andb_true_iff, Nat.leb_le, existsb_exists.
  split.
  intros [height [record [present equal]]].
  apply same_block_exact in equal. subst. auto.
  intros [height present]. split; auto.
  exists item. split; auto. apply same_block_exact. reflexivity.
Qed.

Theorem completion_keeps_next : forall head history item (next : option applied),
  epoch item <= head -> In item history ->
  finish_wait head history item next = (None, next).
Proof.
  intros head history item next height present.
  unfold finish_wait.
  assert (complete head history item = true) as done.
  { apply completion_has_record. auto. }
  rewrite done. reflexivity.
Qed.

Theorem absent_record_waits : forall head history item (pending : option applied),
  ~ In item history ->
  finish_wait head history item pending = (Some item, pending).
Proof.
  intros head history item pending absent. unfold finish_wait.
  destruct (complete head history item) eqn:done; auto.
  apply completion_has_record in done. destruct done as [_ present]. contradiction.
Qed.

Print Assumptions release_has_evidence.
Print Assumptions peer_wait_is_not_fault.
Print Assumptions ready_after_waits.
Print Assumptions fault_cannot_release.
Print Assumptions timeout_keeps_journal.
Print Assumptions stopped_cannot_start.
Print Assumptions fixed_target.
Print Assumptions growth_requires_applied.
Print Assumptions growth_below_goal.
Print Assumptions cache_identity.
Print Assumptions range_stops_at_saved.
Print Assumptions same_block_exact.
Print Assumptions completion_has_record.
Print Assumptions completion_keeps_next.
Print Assumptions absent_record_waits.

Definition range_window extended target next count cap :=
  (extended || (target <? next), count <? cap).

Theorem full_response_clears_short extended target next count cap :
  cap <= count -> snd (range_window extended target next count cap) = false.
Proof. intros full. unfold range_window. simpl. apply Nat.ltb_ge. exact full. Qed.

Theorem extension_survives_full target next count cap :
  fst (range_window true target next count cap) = true.
Proof. reflexivity. Qed.

Print Assumptions full_response_clears_short.
Print Assumptions extension_survives_full.

Definition probe_goal allowed old current last recent :=
  if allowed && (old <=? current) &&
      ((current + 2 <=? last) || ((last =? S current) && negb recent))
  then Some last else None.

Theorem probe_goal_ahead : forall allowed old current last recent selected,
  probe_goal allowed old current last recent = Some selected ->
  allowed = true /\ old <= current /\ current < selected /\ selected = last.
Proof.
  intros allowed old current last recent selected accepted.
  unfold probe_goal in accepted.
  destruct (allowed && (old <=? current) &&
    ((current + 2 <=? last) || ((last =? S current) && negb recent))) eqn:choice;
    inversion accepted; subst.
  repeat rewrite Bool.andb_true_iff in choice.
  destruct choice as [[permission forward] gap].
  apply Nat.leb_le in forward. apply Bool.orb_true_iff in gap.
  destruct gap as [gap|gap].
  apply Nat.leb_le in gap. repeat split; auto; lia.
  apply Bool.andb_true_iff in gap. destruct gap as [gap _].
  apply Nat.eqb_eq in gap. repeat split; auto; lia.
Qed.

Theorem moving_probe_keeps_goal : forall old before current last recent,
  old <= before -> before <= current -> current + 2 <= last ->
  probe_goal true old current last recent = Some last.
Proof.
  intros old before current last recent first moved gap.
  unfold probe_goal.
  assert ((old <=? current) = true) as forward by (apply Nat.leb_le; lia).
  assert ((current + 2 <=? last) = true) as ahead by (apply Nat.leb_le; lia).
  rewrite forward, ahead. reflexivity.
Qed.

Theorem consumed_probe_stops : forall allowed old current last recent,
  last <= current -> probe_goal allowed old current last recent = None.
Proof.
  intros allowed old current last recent consumed.
  destruct (probe_goal allowed old current last recent) eqn:choice; auto.
  apply probe_goal_ahead in choice. destruct choice as [_ [_ [ahead equal]]]. lia.
Qed.

Print Assumptions probe_goal_ahead.
Print Assumptions moving_probe_keeps_goal.
Print Assumptions consumed_probe_stops.