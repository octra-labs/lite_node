(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia List.
Import ListNotations.
Open Scope Z_scope.

Definition reserve (used limit cost : Z) : option Z :=
  if (0 <=? used) && (0 <=? cost) && (used + cost <=? limit)
  then Some (used + cost) else None.

Definition projected (cells layers words : Z) : bool :=
  (0 <=? cells) && (cells <=? 65536) &&
  (0 <=? layers) && (layers <=? 4096) &&
  (0 <=? words) && (words <=? 1048576).

Definition storage_cost (cells words : Z) := 16 * cells + words.

Definition key_cost (volume : Z) := (volume + 63) / 64.

Theorem key_cost_covers : forall volume,
  0 <= volume -> volume <= 64 * key_cost volume < volume + 64.
Proof.
  intros volume positive. unfold key_cost.
  pose proof (Z.div_mod (volume + 63) 64 ltac:(lia)).
  pose proof (Z.mod_pos_bound (volume + 63) 64 ltac:(lia)). lia.
Qed.

Theorem key_volume_limit : forall volumes budget,
  Forall (fun volume => 0 <= volume) volumes ->
  fold_right (fun volume cost => key_cost volume + cost) 0 volumes <= budget ->
  fold_right Z.add 0 volumes <= 64 * budget.
Proof.
  intros volumes budget positive capacity.
  assert (covers : fold_right Z.add 0 volumes <=
    64 * fold_right (fun volume cost => key_cost volume + cost) 0 volumes).
  { clear capacity. induction positive as [|volume rest valid all step]; cbn [fold_right]; [lia|].
    pose proof (key_cost_covers volume valid). lia. }
  lia.
Qed.

Definition transfer_cost (volume : Z) := (volume + 15) / 16.

Theorem transfer_covers : forall volume,
  0 <= volume -> volume <= 16 * transfer_cost volume < volume + 16.
Proof.
  intros volume positive. unfold transfer_cost.
  pose proof (Z.div_mod (volume + 15) 16 ltac:(lia)).
  pose proof (Z.mod_pos_bound (volume + 15) 16 ltac:(lia)). lia.
Qed.

Theorem transfer_limit : forall volumes budget,
  Forall (fun volume => 0 <= volume) volumes ->
  fold_right (fun volume cost => transfer_cost volume + cost) 0 volumes <= budget ->
  fold_right Z.add 0 volumes <= 16 * budget.
Proof.
  intros volumes budget positive capacity.
  assert (covers : fold_right Z.add 0 volumes <=
    16 * fold_right (fun volume cost => transfer_cost volume + cost) 0 volumes).
  { clear capacity. induction positive as [|volume rest valid all step]; cbn [fold_right]; [lia|].
    pose proof (transfer_covers volume valid). lia. }
  lia.
Qed.

Print Assumptions transfer_covers.
Print Assumptions transfer_limit.

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

Theorem key_execution_limit : forall volumes used limit next work,
  Forall (fun volume => 0 <= volume) volumes ->
  reserve used limit
    (fold_right (fun volume cost => transfer_cost volume + cost) 0 volumes) = Some next ->
  0 <= work <= limit - next ->
  next + work <= limit /\
  fold_right Z.add 0 volumes <= 16 * (limit - used - work).
Proof.
  intros volumes used limit next work positive accepted execution.
  apply reserve_limit in accepted.
  destruct accepted as [u [c [step [cap exact]]]].
  split; [lia|].
  apply transfer_limit; [exact positive|]. lia.
Qed.

Print Assumptions key_execution_limit.

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

Module Vm.

Definition floor : Z := 1000000.

Definition allowance (minimum maximum fee : Z) : Z :=
  Z.max floor
    (if (minimum <=? fee) && (fee <=? maximum) then fee else floor).

Definition cost (minimum maximum fee : Z) : Z :=
  Z.max fee (allowance minimum maximum fee).

Fixpoint admit minimum maximum used limit fees : option Z :=
  match fees with
  | [] => Some used
  | fee :: rest =>
    match reserve used limit (cost minimum maximum fee) with
    | None => None
    | Some next => admit minimum maximum next limit rest
    end
  end.

Definition total minimum maximum fees :=
  fold_right (fun fee sum => cost minimum maximum fee + sum) 0 fees.

Theorem allowance_floor : forall minimum maximum fee,
  floor <= allowance minimum maximum fee.
Proof. intros. unfold allowance. apply Z.le_max_l. Qed.

Theorem cost_covers : forall minimum maximum fee,
  fee <= cost minimum maximum fee /\
  allowance minimum maximum fee <= cost minimum maximum fee /\
  0 <= cost minimum maximum fee.
Proof.
  intros. pose proof (allowance_floor minimum maximum fee).
  unfold cost. pose proof (Z.le_max_l fee (allowance minimum maximum fee)).
  pose proof (Z.le_max_r fee (allowance minimum maximum fee)).
  unfold floor in *. repeat split; lia.
Qed.

Theorem admit_exact : forall fees minimum maximum used limit final,
  0 <= used <= limit -> admit minimum maximum used limit fees = Some final ->
  final = used + total minimum maximum fees /\ used <= final <= limit.
Proof.
  induction fees as [|fee rest step]; intros minimum maximum used limit final initial ok.
  - simpl in ok. injection ok as same. subst final. unfold total. simpl. lia.
  - simpl in ok.
    destruct (reserve used limit (cost minimum maximum fee)) as [next|] eqn:accepted;
      [|discriminate].
    apply reserve_limit in accepted.
    destruct accepted as [u [c [inc [cap sum]]]].
    specialize (step minimum maximum next limit final ltac:(lia) ok).
    destruct step as [exact range]. unfold total in *. simpl. split; lia.
Qed.

Theorem total_positive : forall fees minimum maximum,
  0 <= total minimum maximum fees.
Proof.
  induction fees as [|fee rest step]; intros minimum maximum; unfold total in *; simpl.
  - lia.
  - specialize (step minimum maximum).
    pose proof (cost_covers minimum maximum fee) as [_ [_ positive]]. lia.
Qed.

Theorem admit_capacity : forall fees minimum maximum used limit,
  0 <= used -> used + total minimum maximum fees <= limit ->
  admit minimum maximum used limit fees = Some (used + total minimum maximum fees).
Proof.
  induction fees as [|fee rest step]; intros minimum maximum used limit positive capacity.
  - unfold total. simpl. f_equal. lia.
  - simpl. pose proof (cost_covers minimum maximum fee) as [_ [_ nonnegative]].
    pose proof (total_positive rest minimum maximum) as remaining.
    assert (accepted : reserve used limit (cost minimum maximum fee) =
      Some (used + cost minimum maximum fee)).
    { apply child_budget; unfold total in *; simpl in *; lia. }
    rewrite accepted. rewrite step; unfold total in *; simpl in *; try lia.
    f_equal. lia.
Qed.

Theorem work_capacity : forall fees spent minimum maximum used limit final,
  Forall2 (fun fee work => 0 <= work <= allowance minimum maximum fee) fees spent ->
  0 <= used <= limit -> admit minimum maximum used limit fees = Some final ->
  used + fold_right Z.add 0 spent <= final /\ final <= limit.
Proof.
  intros fees spent minimum maximum used limit final execution initial accepted.
  apply admit_exact in accepted; [|exact initial].
  destruct accepted as [exact [_ cap]].
  assert (actual : fold_right Z.add 0 spent <= total minimum maximum fees).
  { clear exact cap initial.
    induction execution as [|fee work fees spent within rest step].
    - unfold total. simpl. lia.
    - unfold total in *. simpl in *.
      pose proof (cost_covers minimum maximum fee) as [_ [covers _]]. lia. }
  split; lia.
Qed.

Example exact_capacity :
  admit (-4611686018427387904) 4611686018427387903 0 10000000
    (repeat 10000 10) = Some 10000000.
Proof. reflexivity. Qed.

Example over_capacity :
  admit (-4611686018427387904) 4611686018427387903 0 10000000
    (repeat 10000 11) = None.
Proof. reflexivity. Qed.

End Vm.

Print Assumptions Vm.admit_exact.
Print Assumptions Vm.admit_capacity.
Print Assumptions Vm.work_capacity.
Print Assumptions reserve_limit.
Print Assumptions key_cost_covers.
Print Assumptions key_volume_limit.
Print Assumptions child_budget.
Print Assumptions repeated_work.
Print Assumptions storage_range.
Print Assumptions prior_unchanged.
Print Assumptions unscheduled_inactive.
Print Assumptions retained_limit.
Print Assumptions retained_shared.
Print Assumptions retained_exhausted.