(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia.
Import ListNotations.

Record edge := Edge {
  origin : nat;
  target : nat;
  epoch : nat
}.

Definition scope limit (steps : list edge) :=
  Forall (fun step => epoch step <= limit) steps.

Inductive reaches (auth : edge -> Prop) (seed : nat) : list edge -> nat -> Prop :=
| empty_path : reaches auth seed [] seed
| extend_path : forall steps current step,
    reaches auth seed steps current ->
    origin step = current ->
    auth step ->
    Forall (fun prior => epoch prior < epoch step) steps ->
    reaches auth seed (steps ++ [step]) (target step).

Definition shorter (prior next : list edge) :=
  if length prior <=? length next then prior else next.

Theorem shorter_reaches : forall auth seed current prior next,
  reaches auth seed prior current -> reaches auth seed next current ->
  reaches auth seed (shorter prior next) current.
Proof.
  intros. unfold shorter. destruct (length prior <=? length next); assumption.
Qed.

Theorem shorter_size : forall prior next,
  length (shorter prior next) <= length next.
Proof.
  intros. unfold shorter. destruct (length prior <=? length next) eqn:order.
  apply Nat.leb_le in order. exact order. lia.
Qed.

Theorem shorter_scope : forall limit prior next,
  scope limit prior -> scope limit next -> scope limit (shorter prior next).
Proof.
  intros. unfold shorter. destruct (length prior <=? length next); assumption.
Qed.

Theorem scope_increases : forall first next steps,
  first <= next -> scope first steps -> scope next steps.
Proof.
  unfold scope. intros first next steps order valid.
  eapply Forall_impl with (P := fun step => epoch step <= first); [|exact valid].
  intros step present. lia.
Qed.

Theorem reuse_path : forall auth seed prior saved frontier step,
  reaches auth seed prior (origin step) ->
  reaches auth seed saved (target step) ->
  scope frontier prior -> scope frontier saved ->
  frontier < epoch step -> auth step ->
  reaches auth seed (shorter saved (prior ++ [step])) (target step) /\
  length (shorter saved (prior ++ [step])) <= length prior + 1 /\
  scope (epoch step) (shorter saved (prior ++ [step])).
Proof.
  intros auth seed prior saved frontier step path stored before old order signed.
  assert (chronology : Forall (fun item => epoch item < epoch step) prior).
  { eapply Forall_impl with (P := fun item => epoch item <= frontier); [|exact before].
    intros item present. lia. }
  assert (extended : reaches auth seed (prior ++ [step]) (target step)).
  { eapply extend_path; eauto. }
  split. apply shorter_reaches; assumption.
  split. pose proof (shorter_size saved (prior ++ [step])). rewrite length_app in H.
  simpl in H. exact H.
  apply shorter_scope.
  apply (scope_increases frontier (epoch step) saved); [lia|exact old].
  unfold scope. apply Forall_app. split.
  apply (scope_increases frontier (epoch step) prior); [lia|exact before].
  constructor; [lia|constructor].
Qed.

Theorem shorter_existing : forall prior next item,
  In item (shorter prior next) -> In item prior \/ In item next.
Proof.
  intros prior next item. unfold shorter.
  destruct (length prior <=? length next); auto.
Qed.