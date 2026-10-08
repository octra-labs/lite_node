(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia.
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

Lemma apply_append : forall first second data,
  apply data (first ++ second) = apply (apply data first) second.
Proof.
  induction first as [|[key value] rest induction]; intros second data;
    simpl; [reflexivity|apply induction].
Qed.

Theorem child_abort : forall parent child old key,
  apply (apply (apply old parent) child)
    (capture (apply old parent) (map fst child)) key = apply old parent key.
Proof.
  intros parent child old key. apply undo_writes.
Qed.

Theorem parent_abort : forall before child after old key,
  apply (apply (apply (apply old before) child) after)
    (capture old (map fst (before ++ child ++ after))) key = old key.
Proof.
  intros before child after old key.
  pose proof (undo_writes (before ++ child ++ after) old key) as restored.
  repeat rewrite apply_append in restored. exact restored.
Qed.

Theorem reentry_keeps_writes : forall first nested last old key,
  ~ In key (map fst last) ->
  apply (apply (apply old first) nested) last key = apply (apply old first) nested key.
Proof.
  intros first nested last old key absent. apply apply_outside. exact absent.
Qed.

Theorem namespace_unchanged : forall first nested last old key,
  ~ In key (map fst (first ++ nested ++ last)) ->
  apply (apply (apply old first) nested) last key = old key.
Proof.
  intros first nested last old key absent.
  repeat rewrite <- apply_append. apply apply_outside. exact absent.
Qed.

Definition io_cost (bytes entries levels : nat) :=
  (bytes + 15) / 16 + entries * (16 + levels).

Theorem io_volume : forall bytes entries levels,
  bytes <= 16 * io_cost bytes entries levels.
Proof.
  intros bytes entries levels.
  pose proof (Nat.div_mod (bytes + 15) 16) as split.
  pose proof (Nat.mod_upper_bound (bytes + 15) 16) as rest.
  unfold io_cost. nia.
Qed.

Theorem io_entries : forall bytes entries levels,
  entries * 16 + entries * levels <= io_cost bytes entries levels.
Proof.
  intros. unfold io_cost. nia.
Qed.

Theorem io_sort_volume : forall keys values entries levels,
  keys + values <= 16 * io_cost (keys * (1 + levels) + values) entries levels.
Proof.
  intros keys values entries levels.
  pose proof (io_volume (keys * (1 + levels) + values) entries levels). nia.
Qed.

Fixpoint io_trace (work : list (nat * nat * nat)) : nat * nat :=
  match work with
  | [] => (0, 0)
  | (bytes, entries, levels) :: rest =>
    let '(volume, effort) := io_trace rest in
    (bytes + volume, io_cost bytes entries levels + effort)
  end.

Theorem io_trace_volume : forall work,
  fst (io_trace work) <= 16 * snd (io_trace work).
Proof.
  induction work as [|[[bytes entries] levels] rest induction]; simpl; [lia|].
  destruct (io_trace rest) as [volume effort]. simpl in *.
  pose proof (io_volume bytes entries levels). nia.
Qed.

Theorem io_budget : forall work limit,
  snd (io_trace work) <= limit -> fst (io_trace work) <= 16 * limit.
Proof.
  intros work limit allowed. pose proof (io_trace_volume work). nia.
Qed.

Print Assumptions apply_outside.
Print Assumptions restore_covered.
Print Assumptions restore_exact.
Print Assumptions undo_writes.
Print Assumptions undo_superset.
Print Assumptions restore_repeat.
Print Assumptions child_abort.
Print Assumptions parent_abort.
Print Assumptions reentry_keeps_writes.
Print Assumptions namespace_unchanged.