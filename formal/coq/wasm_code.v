(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia.
Import ListNotations.

Definition live cache calls := nodup Nat.eq_dec (cache ++ calls).

Theorem code_roots : forall cache calls engine,
  In engine (live cache calls) <-> In engine cache \/ In engine calls.
Proof.
  intros. unfold live. rewrite nodup_In, in_app_iff. reflexivity.
Qed.

Theorem code_slots : forall cache calls cap jobs,
  length cache <= cap -> length calls <= jobs -> length (live cache calls) <= cap + jobs.
Proof.
  intros. unfold live.
  pose proof (NoDup_nodup Nat.eq_dec (cache ++ calls)) as distinct.
  assert (incl (nodup Nat.eq_dec (cache ++ calls)) (cache ++ calls)) as included.
  { intros x member. apply nodup_In in member. exact member. }
  pose proof (NoDup_incl_length distinct included).
  rewrite app_length in H1. lia.
Qed.

Theorem retired_code_gone : forall cache calls engine,
  ~ In engine cache -> ~ In engine calls -> ~ In engine (live cache calls).
Proof. intros cache calls engine absent idle kept. apply code_roots in kept. tauto. Qed.

Theorem active_code_kept : forall cache calls engine,
  In engine calls -> In engine (live cache calls).
Proof. intros. apply code_roots. auto. Qed.

Theorem release_is_local : forall cache before after engine other,
  In other (live cache (before ++ engine :: after)) -> other <> engine ->
  In other (live cache (before ++ after)).
Proof.
  intros cache before after engine other present different.
  apply code_roots in present. apply code_roots.
  destruct present as [cached|running]; [auto|right].
  apply in_app_iff in running. apply in_app_iff.
  destruct running as [earlier|[same|later]]; auto. congruence.
Qed.

Print Assumptions code_roots.
Print Assumptions code_slots.
Print Assumptions retired_code_gone.
Print Assumptions active_code_kept.
Print Assumptions release_is_local.