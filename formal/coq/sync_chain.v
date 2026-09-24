(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia.
Import ListNotations.

Section Chain.
Context {Step : Type}.

Definition accept (cap : nat) (xs : list Step) : option (list Step) :=
  if length xs <=? cap then Some xs else None.

Definition choose (cap : nat) (saved base : option (list Step))
  : option (list Step) :=
  let retry := match base with Some xs => accept cap xs | None => None end in
  match saved with
  | Some xs => match accept cap xs with Some ys => Some ys | None => retry end
  | None => retry
  end.

Lemma accept_limit : forall cap xs ys,
  accept cap xs = Some ys -> length ys <= cap /\ xs = ys.
Proof.
  intros cap xs ys result. unfold accept in result.
  destruct (length xs <=? cap) eqn:size; try discriminate.
  inversion result; subst. apply Nat.leb_le in size. auto.
Qed.

Lemma choose_limit : forall cap saved base xs,
  choose cap saved base = Some xs -> length xs <= cap.
Proof.
  intros cap saved base xs result. unfold choose in result.
  destruct saved as [ys |].
  - destruct (accept cap ys) as [zs |] eqn:prior.
    + inversion result; subst. apply accept_limit in prior. tauto.
    + destruct base as [zs |]; try discriminate.
      apply accept_limit in result. tauto.
  - destruct base as [zs |]; try discriminate.
    apply accept_limit in result. tauto.
Qed.

Lemma choose_whole : forall cap saved base xs,
  choose cap saved base = Some xs -> saved = Some xs \/ base = Some xs.
Proof.
  intros cap saved base xs result. unfold choose in result.
  destruct saved as [ys |].
  - destruct (accept cap ys) as [zs |] eqn:prior.
    + inversion result; subst. apply accept_limit in prior as [_ same].
      subst. auto.
    + destruct base as [zs |]; try discriminate.
      apply accept_limit in result as [_ same]. subst. auto.
  - destruct base as [zs |]; try discriminate.
    apply accept_limit in result as [_ same]. subst. auto.
Qed.

Lemma retry_long : forall cap xs base,
  cap < length xs ->
  choose cap (Some xs) base = choose cap None base.
Proof.
  intros cap xs base size. unfold choose, accept.
  assert (length xs <=? cap = false) as denied by (apply Nat.leb_gt; lia).
  rewrite denied. reflexivity.
Qed.

Lemma keep_valid : forall cap xs base,
  length xs <= cap -> choose cap (Some xs) base = Some xs.
Proof.
  intros cap xs base size. unfold choose, accept.
  apply Nat.leb_le in size. rewrite size. reflexivity.
Qed.

Lemma no_chain : forall cap,
  choose cap None None = None.
Proof. reflexivity. Qed.

End Chain.