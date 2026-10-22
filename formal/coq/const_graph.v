(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia Bool.
Import ListNotations.

Definition step (keys active : list nat) name : option (list nat) :=
  if in_dec Nat.eq_dec name keys then
    if in_dec Nat.eq_dec name active then None
    else Some (name :: active)
  else None.

Theorem step_fresh : forall keys active name next,
  step keys active name = Some next ->
  In name keys /\ ~ In name active /\ next = name :: active.
Proof.
  intros keys active name next accepted.
  unfold step in accepted.
  destruct (in_dec Nat.eq_dec name keys) as [member|absent]; try discriminate.
  destruct (in_dec Nat.eq_dec name active) as [repeated|fresh]; try discriminate.
  inversion accepted. subst. repeat split; assumption.
Qed.

Theorem step_finite : forall keys active name next,
  NoDup active -> incl active keys ->
  step keys active name = Some next ->
  NoDup next /\ incl next keys /\ length next <= length keys.
Proof.
  intros keys active name next distinct inside accepted.
  destruct (step_fresh keys active name next accepted) as [member [fresh equal]].
  subst next.
  assert (NoDup (name :: active)) as distinct_next by (constructor; assumption).
  split; [exact distinct_next|].
  - assert (incl (name :: active) keys) as next_inside.
    { intros value found. simpl in found.
      destruct found as [same|found].
      - subst. exact member.
      - now apply inside. }
    split; [exact next_inside|].
    now apply NoDup_incl_length.
Qed.

Theorem full_refuses : forall keys active name,
  NoDup active -> incl active keys -> length active = length keys ->
  step keys active name = None.
Proof.
  intros keys active name distinct inside full.
  destruct (step keys active name) as [next|] eqn:accepted; auto.
  destruct (step_fresh keys active name next accepted) as [_ [_ equal]].
  pose proof (step_finite keys active name next distinct inside accepted)
    as [_ [_ size]].
  subst next. simpl in size. lia.
Qed.

Print Assumptions step_fresh.
Print Assumptions step_finite.
Print Assumptions full_refuses.

Definition compile_gate {A : Type} start epoch graph_ok (result : A) : option A :=
  if (start <=? epoch) && negb graph_ok then None else Some result.

Theorem compile_before : forall A start epoch graph_ok (result : A),
  epoch < start -> compile_gate start epoch graph_ok result = Some result.
Proof.
  intros A start epoch graph_ok result before.
  unfold compile_gate.
  assert ((start <=? epoch) = false) as inactive by (apply Nat.leb_gt; lia).
  now rewrite inactive.
Qed.

Theorem compile_checked : forall A start epoch (result : A),
  start <= epoch -> compile_gate start epoch false result = None.
Proof.
  intros A start epoch result active.
  unfold compile_gate.
  assert ((start <=? epoch) = true) as enabled by (apply Nat.leb_le; lia).
  now rewrite enabled.
Qed.

Theorem compile_valid : forall A start epoch (result : A),
  compile_gate start epoch true result = Some result.
Proof.
  intros A start epoch result.
  unfold compile_gate. destruct (start <=? epoch); reflexivity.
Qed.

Print Assumptions compile_before.
Print Assumptions compile_checked.
Print Assumptions compile_valid.

Inductive compile_path := Apply | Preview | Sender.

Definition prepare_path {A : Type} (path : compile_path)
    start epoch graph_ok (result : A) : option A :=
  match path with
  | Apply | Preview | Sender => compile_gate start epoch graph_ok result
  end.

Theorem compile_paths_agree : forall A left right start epoch graph_ok (result : A),
  prepare_path left start epoch graph_ok result =
  prepare_path right start epoch graph_ok result.
Proof. intros. destruct left, right; reflexivity. Qed.

Theorem deploy_checked : forall A path start epoch (result : A),
  start <= epoch -> prepare_path path start epoch false result = None.
Proof. intros. destruct path; now apply compile_checked. Qed.

Print Assumptions compile_paths_agree.
Print Assumptions deploy_checked.