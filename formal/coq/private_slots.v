(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia List Sorting.Permutation.
Open Scope Z_scope.

Record slots := Slots { fhe : Z; stealth : Z }.
Record cost := Cost { fhe_cost : Z; stealth_cost : Z }.

Definition valid s := 0 <= fhe s /\ 0 <= stealth s.

Definition reserve s c : option slots :=
  if (0 <=? fhe_cost c) && (fhe_cost c <=? fhe s) &&
     (0 <=? stealth_cost c) && (stealth_cost c <=? stealth s)
  then Some (Slots (fhe s - fhe_cost c) (stealth s - stealth_cost c))
  else None.

Theorem reserve_exact : forall s c next,
  reserve s c = Some next ->
  valid next /\ fhe next <= fhe s /\ stealth next <= stealth s /\
  fhe next + fhe_cost c = fhe s /\
  stealth next + stealth_cost c = stealth s.
Proof.
  intros s c next accepted. unfold reserve in accepted.
  destruct ((0 <=? fhe_cost c) && (fhe_cost c <=? fhe s) &&
    (0 <=? stealth_cost c) && (stealth_cost c <=? stealth s)) eqn:ok;
    [|discriminate].
  repeat rewrite andb_true_iff in ok.
  destruct ok as [[[f0 f1] s0] s1].
  apply Z.leb_le in f0. apply Z.leb_le in f1.
  apply Z.leb_le in s0. apply Z.leb_le in s1.
  injection accepted as same. subst next.
  unfold valid. simpl. repeat split; lia.
Qed.

Fixpoint consume s costs : option slots :=
  match costs with
  | nil => Some s
  | cons c rest =>
    match reserve s c with
    | Some next => consume next rest
    | None => None
    end
  end.

Fixpoint total_fhe costs :=
  match costs with nil => 0 | cons c rest => fhe_cost c + total_fhe rest end.

Fixpoint total_stealth costs :=
  match costs with nil => 0 | cons c rest => stealth_cost c + total_stealth rest end.

Theorem batch_exact : forall costs s final,
  valid s -> consume s costs = Some final ->
  valid final /\
  fhe final + total_fhe costs = fhe s /\
  stealth final + total_stealth costs = stealth s.
Proof.
  induction costs as [|c rest IH]; intros s final safe accepted.
  - simpl in accepted. injection accepted as same. subst final.
    split; [exact safe|]. simpl. split; lia.
  - simpl in accepted.
    destruct (reserve s c) as [next|] eqn:step; [|discriminate].
    apply reserve_exact in step.
    destruct step as [next_safe [f [t [f_exact t_exact]]]].
    specialize (IH next final next_safe accepted).
    destruct IH as [final_safe [f_rest t_rest]].
    split; [exact final_safe|]. simpl. split; lia.
Qed.

Theorem no_overbook : forall costs s final,
  valid s -> consume s costs = Some final ->
  total_fhe costs <= fhe s /\ total_stealth costs <= stealth s.
Proof.
  intros costs s final safe accepted.
  pose proof (batch_exact costs s final safe accepted) as result.
  destruct result as [[f0 t0] [f t]]. split; lia.
Qed.

Theorem zero_budget : forall s c,
  (fhe s = 0 /\ 0 < fhe_cost c) \/
  (stealth s = 0 /\ 0 < stealth_cost c) ->
  reserve s c = None.
Proof.
  intros s c zero. destruct (reserve s c) eqn:result; [|reflexivity].
  apply reserve_exact in result.
  destruct result as [[f0 t0] [f [t [f_exact t_exact]]]].
  destruct zero as [[a b]|[a b]]; lia.
Qed.

Definition preview_measure pool inputs phase :=
  2 * (2 * pool - inputs) + phase.

Theorem preview_nonnegative : forall pool inputs phase,
  0 <= inputs <= pool -> 0 <= phase <= 1 ->
  0 <= preview_measure pool inputs phase.
Proof. intros. unfold preview_measure. lia. Qed.

Theorem preview_refill : forall pool inputs growth phase,
  0 < growth -> 0 <= phase <= 1 ->
  preview_measure pool (inputs + growth) 1 < preview_measure pool inputs phase.
Proof. intros. unfold preview_measure. lia. Qed.

Theorem preview_remove : forall pool inputs removed phase,
  0 < removed -> 0 <= phase <= 1 ->
  preview_measure (pool - removed) (inputs - removed) 1 <
  preview_measure pool inputs phase.
Proof. intros. unfold preview_measure. lia. Qed.

Theorem preview_reject : forall pool inputs,
  preview_measure pool inputs 0 < preview_measure pool inputs 1.
Proof. intros. unfold preview_measure. lia. Qed.

Theorem preview_maximum : forall pool inputs phase,
  0 <= inputs <= pool -> 0 <= phase <= 1 ->
  preview_measure pool inputs phase <= 4 * pool + 1.
Proof. intros. unfold preview_measure. lia. Qed.

Definition valid_cost c := 0 <= fhe_cost c /\ 0 <= stealth_cost c.

Theorem consume_enough : forall costs s,
  Forall valid_cost costs -> valid s ->
  total_fhe costs <= fhe s -> total_stealth costs <= stealth s ->
  consume s costs = Some
    (Slots (fhe s - total_fhe costs) (stealth s - total_stealth costs)).
Proof.
  induction costs as [|c rest IH]; intros s safe initial f t.
  - destruct s. simpl. f_equal. f_equal; lia.
  - inversion safe as [|c' rest' first remaining]; subst.
    destruct first as [cf ct].
    assert (nonnegative : forall xs,
      Forall valid_cost xs -> 0 <= total_fhe xs /\ 0 <= total_stealth xs).
    { intros xs values. induction values as [|x xs one all both].
      - simpl. lia.
      - destruct one. destruct both. simpl. lia. }
    destruct (nonnegative rest remaining) as [rf rt].
    simpl in f, t. simpl. unfold reserve.
    assert (admit : (0 <=? fhe_cost c) && (fhe_cost c <=? fhe s) &&
      (0 <=? stealth_cost c) && (stealth_cost c <=? stealth s) = true).
    { repeat rewrite andb_true_iff. repeat split; apply Z.leb_le; lia. }
    rewrite admit.
    rewrite IH; try assumption; unfold valid; simpl; try lia.
    f_equal. f_equal; lia.
Qed.

Theorem totals_order : forall xs ys,
  Permutation xs ys ->
  total_fhe xs = total_fhe ys /\ total_stealth xs = total_stealth ys.
Proof.
  intros xs ys order. induction order; simpl in *; intuition lia.
Qed.

Theorem consume_order : forall xs ys s,
  Permutation xs ys -> Forall valid_cost xs -> valid s ->
  consume s xs = consume s ys.
Proof.
  intros xs ys s order safe initial.
  pose proof (totals_order xs ys order) as [f t].
  assert (other : Forall valid_cost ys).
  { eapply Permutation_Forall; eauto. }
  destruct (consume s xs) as [left|] eqn:a;
    destruct (consume s ys) as [right|] eqn:b; try reflexivity.
  - pose proof (batch_exact xs s left initial a) as [_ [af ast]].
    pose proof (batch_exact ys s right initial b) as [_ [bf bt]].
    destruct left. destruct right. simpl in *. f_equal. f_equal; lia.
  - pose proof (no_overbook xs s left initial a) as [af ast].
    rewrite f in af. rewrite t in ast.
    rewrite (consume_enough ys s other initial af ast) in b. discriminate.
  - pose proof (no_overbook ys s right initial b) as [af ast].
    rewrite <- f in af. rewrite <- t in ast.
    rewrite (consume_enough xs s safe initial af ast) in a. discriminate.
Qed.

Fixpoint applied (attempts : list (bool * cost)) : list cost :=
  match attempts with
  | nil => nil
  | cons (ok, c) rest => if ok then cons c (applied rest) else applied rest
  end.

Fixpoint settle s (attempts : list (bool * cost)) : option slots :=
  match attempts with
  | nil => Some s
  | cons (ok, c) rest =>
    if ok then
      match reserve s c with
      | None => None
      | Some next => settle next rest
      end
    else settle s rest
  end.

Theorem settle_applied : forall attempts s,
  settle s attempts = consume s (applied attempts).
Proof.
  induction attempts as [|[ok c] rest IH]; intros s; simpl; [reflexivity|].
  destruct ok; simpl; [destruct (reserve s c)|]; auto.
Qed.

Theorem settle_exact : forall attempts s final,
  valid s -> settle s attempts = Some final ->
  valid final /\
  fhe final + total_fhe (applied attempts) = fhe s /\
  stealth final + total_stealth (applied attempts) = stealth s.
Proof.
  intros attempts s final initial accepted.
  rewrite settle_applied in accepted. eapply batch_exact; eauto.
Qed.

Theorem settle_order : forall xs ys s,
  Permutation (applied xs) (applied ys) ->
  Forall valid_cost (applied xs) -> valid s -> settle s xs = settle s ys.
Proof.
  intros. repeat rewrite settle_applied. apply consume_order; assumption.
Qed.