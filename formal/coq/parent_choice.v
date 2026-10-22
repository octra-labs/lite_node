(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import Arith Bool List Lia.
Import ListNotations.

Definition history := nat -> option nat.

Definition choice (activation epoch : nat) (read : history)
    : option (option nat) :=
  if (epoch =? 0) || (epoch <? activation) then Some None
  else match read (epoch - 1) with
    | None => None
    | Some hash => Some (Some hash)
  end.

Lemma choice_current : forall activation epoch read,
  0 < epoch -> activation <= epoch ->
  choice activation epoch read =
    match read (epoch - 1) with
    | None => None
    | Some hash => Some (Some hash)
    end.
Proof.
  intros activation epoch read positive active.
  unfold choice.
  assert (zero : (epoch =? 0) = false) by (apply Nat.eqb_neq; lia).
  assert (before : (epoch <? activation) = false) by (apply Nat.ltb_ge; lia).
  rewrite zero, before. reflexivity.
Qed.

Theorem genesis_omits : forall activation read,
  choice activation 0 read = Some None.
Proof. reflexivity. Qed.

Theorem legacy_omits : forall activation epoch read,
  epoch < activation -> choice activation epoch read = Some None.
Proof.
  intros activation epoch read before.
  unfold choice.
  apply Nat.ltb_lt in before.
  rewrite before, orb_true_r. reflexivity.
Qed.

Theorem current_requires_history : forall activation epoch read parent,
  0 < epoch -> activation <= epoch ->
  choice activation epoch read = Some parent ->
  exists hash, read (epoch - 1) = Some hash /\ parent = Some hash.
Proof.
  intros activation epoch read parent positive active accepted.
  rewrite choice_current in accepted by assumption.
  destruct (read (epoch - 1)) as [hash|] eqn:stored; try discriminate.
  inversion accepted; subst. exists hash. split; reflexivity.
Qed.

Theorem missing_refuses : forall activation epoch read,
  0 < epoch -> activation <= epoch -> read (epoch - 1) = None ->
  choice activation epoch read = None.
Proof.
  intros activation epoch read positive active missing.
  rewrite choice_current by assumption. rewrite missing. reflexivity.
Qed.

Theorem current_equivalence : forall activation epoch read hash,
  0 < epoch -> activation <= epoch ->
  choice activation epoch read = Some (Some hash) <->
  read (epoch - 1) = Some hash.
Proof.
  intros activation epoch read hash positive active.
  rewrite choice_current by assumption.
  destruct (read (epoch - 1)) as [stored|]; split; intro equal;
    try discriminate; inversion equal; reflexivity.
Qed.

Inductive chain (activation : nat) (read : history)
    : nat -> list (nat * option nat) -> Prop :=
| Begin : forall epoch, chain activation read epoch []
| Extend : forall epoch parent events,
    chain activation read epoch events ->
    choice activation epoch read = Some parent ->
    chain activation read (S epoch) (events ++ [(epoch, parent)]).

Definition linked (activation : nat) (read : history)
    (event : nat * option nat) :=
  let '(epoch, parent) := event in
  0 < epoch -> activation <= epoch ->
  exists hash, read (epoch - 1) = Some hash /\ parent = Some hash.

Theorem chain_links : forall activation read epoch events,
  chain activation read epoch events -> Forall (linked activation read) events.
Proof.
  intros activation read epoch events steps. induction steps.
  - constructor.
  - apply Forall_app. split; [assumption|].
    constructor; [|constructor]. unfold linked.
    intros positive active.
    eapply current_requires_history; eauto.
Qed.

Theorem wrong_hash_refuses : forall activation epoch read expected supplied,
  0 < epoch -> activation <= epoch ->
  read (epoch - 1) = Some expected -> expected <> supplied ->
  choice activation epoch read <> Some (Some supplied).
Proof.
  intros activation epoch read expected supplied positive active stored differs accepted.
  apply current_equivalence in accepted; try assumption.
  rewrite stored in accepted. inversion accepted. contradiction.
Qed.

Print Assumptions chain_links.
Print Assumptions current_equivalence.
Print Assumptions wrong_hash_refuses.