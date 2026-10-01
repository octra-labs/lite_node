(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia.
Open Scope Z_scope.

Definition reserve (used limit cost : Z) : option Z :=
  if (0 <=? used) && (0 <=? cost) && (used + cost <=? limit)
  then Some (used + cost) else None.

Definition projected (cells layers words : Z) : bool :=
  (0 <=? cells) && (cells <=? 65536) &&
  (0 <=? layers) && (layers <=? 4096) &&
  (0 <=? words) && (words <=? 1048576).

Definition storage_cost (cells words : Z) := 16 * cells + words.

Theorem reserve_limit : forall used limit cost next,
  reserve used limit cost = Some next ->
  0 <= used /\ 0 <= cost /\ used <= next /\ next <= limit /\ next = used + cost.
Proof.
  intros used limit cost next accepted.
  unfold reserve in accepted.
  destruct ((0 <=? used) && (0 <=? cost) && (used + cost <=? limit)) eqn:ok;
    [|discriminate].
  repeat rewrite andb_true_iff in ok.
  destruct ok as [[u c] total].
  apply Z.leb_le in u. apply Z.leb_le in c. apply Z.leb_le in total.
  injection accepted as same. subst next. repeat split; lia.
Qed.

Theorem child_budget : forall used limit child,
  0 <= used -> used <= limit -> 0 <= child -> child <= limit - used ->
  reserve used limit child = Some (used + child).
Proof.
  intros used limit child u l c r. unfold reserve.
  assert (ok : (0 <=? used) && (0 <=? child) && (used + child <=? limit) = true).
  { repeat rewrite andb_true_iff. repeat split; apply Z.leb_le; lia. }
  rewrite ok. reflexivity.
Qed.

Theorem repeated_work : forall used limit left middle right final,
  reserve used limit left = Some middle -> reserve middle limit right = Some final ->
  final = used + left + right /\ final <= limit.
Proof.
  intros used limit left middle right final first second.
  apply reserve_limit in first. apply reserve_limit in second.
  destruct first as [a [b [c [d e]]]].
  destruct second as [f [g [h [i j]]]]. split; lia.
Qed.

Theorem storage_range : forall cells layers words,
  projected cells layers words = true -> 0 <= storage_cost cells words <= 2097152.
Proof.
  intros cells layers words accepted. unfold projected in accepted.
  repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[[[[c0 c1] l0] l1] w0] w1].
  apply Z.leb_le in c0. apply Z.leb_le in c1.
  apply Z.leb_le in w0. apply Z.leb_le in w1.
  unfold storage_cost. lia.
Qed.

Definition selected (activation : option Z) (epoch : Z) : bool :=
  match activation with None => false | Some start => start <=? epoch end.

Theorem prior_unchanged : forall start epoch,
  epoch < start -> selected (Some start) epoch = false.
Proof. intros start epoch prior. unfold selected. apply Z.leb_gt. exact prior. Qed.

Theorem unscheduled_inactive : forall epoch, selected None epoch = false.
Proof. reflexivity. Qed.

Definition retained (used cost : Z) : option Z :=
  if (0 <=? cost) && (cost <=? 536870912 - used)
  then Some (used + cost) else None.

Theorem retained_limit : forall used cost next,
  0 <= used -> retained used cost = Some next ->
  used <= next /\ next <= 536870912 /\ next = used + cost.
Proof.
  intros used cost next u ok. unfold retained in ok.
  destruct ((0 <=? cost) && (cost <=? 536870912 - used)) eqn:finite;
    [|discriminate].
  apply andb_true_iff in finite. destruct finite as [c upper].
  apply Z.leb_le in c. apply Z.leb_le in upper.
  injection ok as same. subst next. repeat split; lia.
Qed.

Theorem retained_shared : forall used left middle right final,
  0 <= used -> retained used left = Some middle ->
  retained middle right = Some final ->
  final = used + left + right /\ final <= 536870912.
Proof.
  intros used left middle right final u first second.
  apply retained_limit in first; [|exact u].
  destruct first as [a [b c]].
  apply retained_limit in second; [|lia].
  destruct second as [d [e f]]. split; lia.
Qed.

Theorem retained_exhausted : forall cost,
  0 < cost -> retained 536870912 cost = None.
Proof.
  intros cost positive. unfold retained.
  assert (over : (cost <=? 536870912 - 536870912) = false).
  { apply Z.leb_gt. lia. }
  rewrite over. rewrite andb_false_r. reflexivity.
Qed.

Print Assumptions reserve_limit.
Print Assumptions child_budget.
Print Assumptions repeated_work.
Print Assumptions storage_range.
Print Assumptions prior_unchanged.
Print Assumptions unscheduled_inactive.
Print Assumptions retained_limit.
Print Assumptions retained_shared.
Print Assumptions retained_exhausted.