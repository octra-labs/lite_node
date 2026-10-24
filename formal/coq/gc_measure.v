(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import Arith Bool Lia.

Inductive decision := Defer (need : nat) | Measure | Known (need : nat).

Definition select reserve free epoch commit saved :=
  if free <? reserve then Defer reserve
  else match saved with
  | Some (saved_epoch, saved_commit, need) =>
    if (epoch =? saved_epoch) && (commit =? saved_commit) then Known need else Measure
  | None => Measure
  end.

Theorem reserve_refusal_never_scans : forall reserve free epoch commit saved,
  free < reserve -> select reserve free epoch commit saved = Defer reserve.
Proof.
  intros; unfold select; rewrite (proj2 (Nat.ltb_lt _ _) H); reflexivity.
Qed.

Theorem same_snapshot_is_not_measured : forall reserve free epoch commit need,
  reserve <= free ->
  select reserve free epoch commit (Some (epoch, commit, need)) = Known need.
Proof.
  intros; unfold select; rewrite (proj2 (Nat.ltb_ge _ _) H).
  now rewrite !Nat.eqb_refl.
Qed.

Theorem reused_size_names_same_snapshot : forall reserve free epoch commit se sc size need,
  select reserve free epoch commit (Some (se, sc, size)) = Known need ->
  epoch = se /\ commit = sc /\ need = size.
Proof.
  intros reserve free epoch commit se sc size need H; unfold select in H.
  destruct (free <? reserve); [discriminate|].
  destruct ((epoch =? se) && (commit =? sc)) eqn:E; [|discriminate].
  inversion H; subst; apply andb_true_iff in E.
  destruct E as [He Hc]; apply Nat.eqb_eq in He; apply Nat.eqb_eq in Hc; auto.
Qed.

Definition can_start free need := need <=? free.

Theorem recheck_refuses_consumed_space : forall before after need,
  can_start before need = true -> after < need -> can_start after need = false.
Proof. intros; unfold can_start; now apply Nat.leb_gt. Qed.

Theorem space_recovery_allows_retry : forall before after need,
  can_start before need = false -> need <= after -> can_start after need = true.
Proof. intros; unfold can_start; now apply Nat.leb_le. Qed.