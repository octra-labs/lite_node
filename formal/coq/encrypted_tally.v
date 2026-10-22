(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List ZArith Lia.
Import ListNotations.
Open Scope Z_scope.

Section Tally.
Variable cipher : Type.
Variable modulus : Z.
Variable decode : cipher -> Z.
Variable add : cipher -> cipher -> cipher.
Hypothesis modulus_positive : 0 < modulus.
Hypothesis decode_reduced : forall c, decode c mod modulus = decode c.
Hypothesis add_correct : forall a b,
  decode (add a b) = (decode a + decode b) mod modulus.

Record tally := {
  total : option cipher;
  contributions : nat
}.

Definition value (s : tally) :=
  match total s with None => 0 | Some c => decode c end.

Definition absorb (input : cipher) (s : tally) : tally :=
  {| total := Some (match total s with None => input | Some c => add c input end);
     contributions := S (contributions s) |}.

Definition step (input : option cipher) (s : tally) : tally :=
  match input with None => s | Some c => absorb c s end.

Fixpoint execute (inputs : list (option cipher)) (s : tally) : tally :=
  match inputs with [] => s | c :: rest => execute rest (step c s) end.

Fixpoint sum (inputs : list (option cipher)) : Z :=
  match inputs with
  | [] => 0
  | None :: rest => sum rest
  | Some c :: rest => decode c + sum rest
  end.

Fixpoint count (inputs : list (option cipher)) : nat :=
  match inputs with
  | [] => O
  | None :: rest => count rest
  | Some _ :: rest => S (count rest)
  end.

Lemma value_reduced : forall s, value s mod modulus = value s.
Proof.
  intros [stored n]. unfold value; simpl.
  destruct stored; [apply decode_reduced | apply Z.mod_0_l; lia].
Qed.

Lemma absorb_value : forall input s,
  value (absorb input s) = (value s + decode input) mod modulus.
Proof.
  intros input [stored n]. unfold value, absorb; simpl.
  destruct stored; simpl; [apply add_correct |].
  symmetry. apply decode_reduced.
Qed.

Theorem total_correct : forall inputs s,
  value (execute inputs s) = (value s + sum inputs) mod modulus.
Proof.
  induction inputs as [|input rest later]; intros s; simpl.
  - rewrite Z.add_0_r. symmetry. apply value_reduced.
  - destruct input; simpl; rewrite later; [|reflexivity].
    rewrite absorb_value, Z.add_mod_idemp_l by lia.
    f_equal. lia.
Qed.

Theorem count_correct : forall inputs s,
  contributions (execute inputs s) = (contributions s + count inputs)%nat.
Proof.
  induction inputs as [|input rest later]; intros s; simpl; [lia |].
  destruct input; simpl; rewrite later; simpl; lia.
Qed.

Theorem failure_unchanged : forall s, step None s = s.
Proof. reflexivity. Qed.

Theorem resumes : forall left right s,
  execute (left ++ right) s = execute right (execute left s).
Proof.
  induction left as [|input rest later]; intros right s; simpl; auto.
Qed.

End Tally.

Print Assumptions total_correct.
Print Assumptions count_correct.
Print Assumptions failure_unchanged.
Print Assumptions resumes.