(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia.
Import ListNotations.

Fixpoint digits base count value :=
  match count with
  | 0 => 0
  | S rest => value mod base + base * digits base rest (value / base)
  end.

Theorem digits_exact : forall base count value,
  0 < base -> value < base ^ count -> digits base count value = value.
Proof.
  intros base count. induction count as [|count step]; intros value positive small.
  - simpl in *. lia.
  - simpl. rewrite step.
    + pose proof (Nat.div_mod value base ltac:(lia)). nia.
    + exact positive.
    + apply Nat.div_lt_upper_bound; [lia|].
      simpl in small. nia.
Qed.

Fixpoint direct (entries : list (nat * nat)) :=
  match entries with
  | [] => 0
  | (value, point) :: rest => value * point + direct rest
  end.

Fixpoint grouped base count (entries : list (nat * nat)) :=
  match entries with
  | [] => 0
  | (value, point) :: rest => digits base count value * point + grouped base count rest
  end.

Theorem grouped_exact : forall base count entries,
  0 < base -> Forall (fun entry => fst entry < base ^ count) entries ->
  grouped base count entries = direct entries.
Proof.
  intros base count entries positive size.
  induction size as [|[value point] rest small more step]; simpl in *; auto.
  rewrite digits_exact by assumption. now rewrite step.
Qed.

Corollary grouped_mod : forall base count entries modulus,
  0 < base -> Forall (fun entry => fst entry < base ^ count) entries ->
  grouped base count entries mod modulus = direct entries mod modulus.
Proof. intros. now rewrite grouped_exact by assumption. Qed.

Fixpoint bucket_weight start entries :=
  match entries with
  | [] => 0
  | point :: rest => start * point + bucket_weight (S start) rest
  end.

Fixpoint bucket_sum entries :=
  match entries with
  | [] => 0
  | _ :: rest => fold_right Nat.add 0 entries + bucket_sum rest
  end.

Lemma bucket_step : forall entries start,
  bucket_weight (S start) entries =
  fold_right Nat.add 0 entries + bucket_weight start entries.
Proof.
  induction entries as [|point rest step]; intros start; simpl; auto.
  rewrite step. nia.
Qed.

Theorem bucket_exact : forall entries,
  bucket_sum entries = bucket_weight 1 entries.
Proof.
  induction entries as [|point rest step]; simpl; auto.
  rewrite step, (bucket_step rest 1). lia.
Qed.

Corollary bucket_mod : forall entries modulus,
  bucket_sum entries mod modulus = bucket_weight 1 entries mod modulus.
Proof. intros. now rewrite bucket_exact. Qed.

Print Assumptions digits_exact.
Print Assumptions grouped_exact.
Print Assumptions grouped_mod.
Print Assumptions bucket_step.
Print Assumptions bucket_exact.
Print Assumptions bucket_mod.