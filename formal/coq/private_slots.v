(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia List.
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