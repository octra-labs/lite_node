(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia.
Open Scope Z_scope.

Definition meets (quorum members : Z) : bool :=
  (0 <=? quorum) && (0 <=? members) && (quorum <=? members).

Theorem meets_exact : forall quorum members,
  meets quorum members = true <-> 0 <= quorum /\ quorum <= members.
Proof.
  intros quorum members. unfold meets.
  repeat rewrite andb_true_iff.
  repeat rewrite Z.leb_le. lia.
Qed.

Theorem excess_refused : forall quorum members,
  members < quorum -> meets quorum members = false.
Proof.
  intros quorum members excess.
  destruct (meets quorum members) eqn:accepted; [|reflexivity].
  apply meets_exact in accepted. lia.
Qed.

Theorem negative_refused : forall quorum members,
  quorum < 0 \/ members < 0 -> meets quorum members = false.
Proof.
  intros quorum members negative.
  destruct (meets quorum members) eqn:accepted; [|reflexivity].
  apply meets_exact in accepted. lia.
Qed.

Theorem members_increase : forall quorum first next,
  meets quorum first = true -> first <= next -> meets quorum next = true.
Proof.
  intros quorum first next accepted order.
  apply meets_exact in accepted. apply meets_exact. lia.
Qed.

Theorem zero_members : forall quorum,
  meets quorum 0 = true <-> quorum = 0.
Proof.
  intro quorum. rewrite meets_exact. lia.
Qed.

Definition members (bootstrap : bool) (current next : Z) : Z :=
  if bootstrap then next else current.

Theorem bootstrap_counts_next : forall quorum current next,
  meets quorum (members true current next) = meets quorum next.
Proof. reflexivity. Qed.

Theorem existing_counts_current : forall quorum current next,
  meets quorum (members false current next) = meets quorum current.
Proof. reflexivity. Qed.

Definition apply_if {state : Type} (quorum count : Z) (before after : state) : state :=
  if meets quorum count then after else before.

Theorem refusal_preserves_state : forall (state : Type) quorum count (before after : state),
  count < quorum -> apply_if quorum count before after = before.
Proof.
  intros state quorum count before after excess. unfold apply_if.
  rewrite (excess_refused quorum count excess). reflexivity.
Qed.