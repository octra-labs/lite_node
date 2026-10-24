(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith Bool List Lia.
Import ListNotations.

Record state := State {
  height : nat;
  round : nat;
  generation : nat;
  lock : option nat;
  evidence : list nat
}.

Record timer := Timer {
  timer_round : nat;
  timer_generation : nat;
  due : nat
}.

Definition advance target s :=
  if height s <? target then
    State target 0 (S (generation s)) None []
  else s.

Fixpoint poll targets s :=
  match targets with
  | [] => s
  | target :: rest => poll rest (advance target s)
  end.

Definition fire now t s :=
  if (due t <=? now) && (timer_round t =? round s)
     && (timer_generation t =? generation s) then
    State (height s) (S (round s)) (generation s) (lock s) (evidence s)
  else s.

Lemma old_height_unchanged : forall s target,
  target <= height s -> advance target s = s.
Proof.
  intros s target order. unfold advance.
  destruct (height s <? target) eqn:later; auto.
  apply Nat.ltb_lt in later. lia.
Qed.

Theorem retries_preserve_state : forall targets s,
  Forall (fun target => target <= height s) targets -> poll targets s = s.
Proof.
  induction targets as [|target rest ih]; intros s allowed; simpl; auto.
  inversion allowed; subst. rewrite old_height_unchanged by assumption.
  apply ih. assumption.
Qed.

Theorem retry_timer_progress : forall targets s t now,
  Forall (fun target => target <= height s) targets ->
  due t <= now -> timer_round t = round s ->
  timer_generation t = generation s ->
  round (fire now t (poll targets s)) = S (round s).
Proof.
  intros targets s t now allowed ready same_round same_generation.
  rewrite retries_preserve_state by assumption.
  unfold fire. rewrite same_round, same_generation, !Nat.eqb_refl.
  rewrite (proj2 (Nat.leb_le _ _) ready). reflexivity.
Qed.

Theorem retries_keep_lock : forall targets s t now,
  Forall (fun target => target <= height s) targets ->
  lock (fire now t (poll targets s)) = lock s /\
  evidence (fire now t (poll targets s)) = evidence s.
Proof.
  intros targets s t now allowed.
  rewrite retries_preserve_state by assumption.
  unfold fire. destruct (_ && _ && _); split; reflexivity.
Qed.

Theorem future_height_resets : forall s target,
  height s < target -> height (advance target s) = target /\
  generation (advance target s) = S (generation s) /\
  round (advance target s) = 0.
Proof.
  intros s target later. unfold advance.
  rewrite (proj2 (Nat.ltb_lt _ _) later). repeat split; reflexivity.
Qed.

Definition restarted s :=
  State (height s) (round s) (S (generation s)) (lock s) [].

Example reset_loses_due_timer :
  let s := State 1728100 117 5 (Some 110) [118] in
  let t := Timer 117 5 120 in
  round (fire 120 t (restarted s)) = 117 /\
  round (fire 120 t (advance 1728100 s)) = 118.
Proof. vm_compute. auto. Qed.

Print Assumptions retries_preserve_state.
Print Assumptions retry_timer_progress.
Print Assumptions retries_keep_lock.
Print Assumptions future_height_resets.
Print Assumptions reset_loses_due_timer.