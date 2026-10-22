(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Definition begin_read key active :=
  if existsb (Nat.eqb key) active then active
  else if length active <? 2 then key :: active else active.

Definition finish_read key active := filter (fun item => negb (item =? key)) active.

Definition cache_read key cache :=
  key :: firstn 127 (filter (fun item => negb (item =? key)) cache).

Theorem start_capacity : forall key active,
  length active <= 2 -> length (begin_read key active) <= 2.
Proof.
  intros key active valid. unfold begin_read.
  destruct (existsb (Nat.eqb key) active); auto.
  destruct (length active <? 2) eqn:room; auto.
  apply Nat.ltb_lt in room. cbn. lia.
Qed.

Theorem completion_capacity : forall key active,
  length active <= 2 -> length (finish_read key active) <= 2.
Proof.
  intros key active valid. unfold finish_read.
  eapply Nat.le_trans; [apply filter_length_le|exact valid].
Qed.

Theorem result_cache_capacity : forall key cache,
  length (cache_read key cache) <= 128.
Proof.
  intros key cache. unfold cache_read.
  change (S (length (firstn 127 (filter (fun item => negb (item =? key)) cache))) <= 128).
  rewrite length_firstn. pose proof (Nat.le_min_l 127
    (length (filter (fun item => negb (item =? key)) cache))). lia.
Qed.

Theorem completion_removes_key : forall key active,
  ~ In key (finish_read key active).
Proof.
  intros key active found. unfold finish_read in found.
  apply filter_In in found as [_ different].
  rewrite Nat.eqb_refl in different. discriminate.
Qed.

Theorem repeated_read_no_work : forall key active,
  In key active -> begin_read key active = active.
Proof.
  intros key active present. unfold begin_read.
  assert (existsb (Nat.eqb key) active = true) as found.
  { apply existsb_exists. exists key. split; auto. apply Nat.eqb_refl. }
  rewrite found. reflexivity.
Qed.

Print Assumptions start_capacity.
Print Assumptions completion_capacity.
Print Assumptions result_cache_capacity.
Print Assumptions completion_removes_key.
Print Assumptions repeated_read_no_work.