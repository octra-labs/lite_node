(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith.
Import ListNotations.

Definition store := nat -> option nat.
Definition put (data : store) (key : nat) (value : option nat) : store :=
  fun target => if Nat.eq_dec target key then value else data target.

Fixpoint apply (data : store) (writes : list (nat * option nat)) : store :=
  match writes with
  | [] => data
  | (key, value) :: rest => apply (put data key value) rest
  end.

Definition capture (data : store) (keys : list nat) :=
  map (fun key => (key, data key)) keys.

Lemma capture_keys : forall data keys,
  map fst (capture data keys) = keys.
Proof.
  intros data keys. induction keys as [|key rest induction]; simpl; congruence.
Qed.

Theorem apply_outside : forall writes data key,
  ~ In key (map fst writes) -> apply data writes key = data key.
Proof.
  induction writes as [|[changed value] rest induction]; intros data key outside; simpl; auto.
  rewrite induction.
  - unfold put. destruct (Nat.eq_dec key changed); subst; simpl in outside; intuition.
  - simpl in outside. intuition.
Qed.

Theorem restore_covered : forall keys old current key,
  In key keys -> apply current (capture old keys) key = old key.
Proof.
  induction keys as [|first rest induction]; intros old current key inside.
  - contradiction.
  - destruct (in_dec Nat.eq_dec key rest) as [remaining|absent].
    + simpl. apply induction. exact remaining.
    + simpl in inside. destruct inside as [same|remaining]; [|contradiction].
      subst first. simpl. rewrite apply_outside.
      * unfold put. destruct (Nat.eq_dec key key); congruence.
      * rewrite capture_keys. exact absent.
Qed.

Theorem restore_exact : forall keys old current,
  (forall key, ~ In key keys -> current key = old key) ->
  forall key, apply current (capture old keys) key = old key.
Proof.
  intros keys old current unchanged key.
  destruct (in_dec Nat.eq_dec key keys) as [inside|outside].
  - apply restore_covered. exact inside.
  - rewrite apply_outside.
    + apply unchanged. exact outside.
    + rewrite capture_keys. exact outside.
Qed.

Theorem undo_writes : forall writes old key,
  apply (apply old writes) (capture old (map fst writes)) key = old key.
Proof.
  intros writes old key. apply restore_exact.
  intros target outside. apply apply_outside. exact outside.
Qed.

Theorem undo_superset : forall writes keys old,
  (forall key, In key (map fst writes) -> In key keys) ->
  forall key, apply (apply old writes) (capture old keys) key = old key.
Proof.
  intros writes keys old covers key. apply restore_exact.
  intros target outside. apply apply_outside.
  intro inside. apply outside. apply covers. exact inside.
Qed.

Theorem restore_repeat : forall writes old key,
  apply (apply (apply old writes) (capture old (map fst writes)))
    (capture old (map fst writes)) key = old key.
Proof.
  intros writes old key. apply restore_exact.
  intros target outside. apply undo_writes.
Qed.

Print Assumptions apply_outside.
Print Assumptions restore_covered.
Print Assumptions restore_exact.
Print Assumptions undo_writes.
Print Assumptions undo_superset.
Print Assumptions restore_repeat.