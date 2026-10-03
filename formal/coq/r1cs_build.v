(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia ZArith.
Import ListNotations.

Inductive slot := One | Committed (index : nat) | Left (index : nat)
  | Right (index : nat) | Product (index : nat).

Definition expression := list (slot * Z).

Record witness := Witness {
  commitments : list Z;
  gates : list (Z * Z * Z);
  relations : list expression
}.

Record shape := Shape {
  commit_count : nat;
  gate_count : nat;
  constraints : list expression
}.

Definition project value := Shape (length (commitments value))
  (length (gates value)) (relations value).

Definition value_at (state : witness) place :=
  match place with
  | One => 1%Z
  | Committed index => nth index (commitments state) 0%Z
  | Left index => let '(a, _, _) := nth index (gates state) (0%Z, 0%Z, 0%Z) in a
  | Right index => let '(_, b, _) := nth index (gates state) (0%Z, 0%Z, 0%Z) in b
  | Product index => let '(_, _, out) := nth index (gates state) (0%Z, 0%Z, 0%Z) in out
  end.

Definition evaluate state expr :=
  fold_right (fun term total => (value_at state (fst term) * snd term + total)%Z) 0%Z expr.

Inductive action := Commit (value : Z) | Allocate (left right : Z)
  | Multiply (left right : expression) | Constrain (expr : expression).

Definition prove_step state input :=
  match input with
  | Commit value =>
      (Witness (commitments state ++ [value]) (gates state) (relations state),
       [Committed (length (commitments state))])
  | Allocate a b =>
      let index := length (gates state) in
      (Witness (commitments state) (gates state ++ [(a, b, (a * b)%Z)])
        (relations state), [Left index; Right index; Product index])
  | Multiply x y =>
      let index := length (gates state) in
      let a := evaluate state x in
      let b := evaluate state y in
      (Witness (commitments state) (gates state ++ [(a, b, (a * b)%Z)])
        (relations state ++ [x ++ [(Left index, (-1)%Z)]; y ++ [(Right index, (-1)%Z)]]),
       [Left index; Right index; Product index])
  | Constrain expr =>
      (Witness (commitments state) (gates state) (relations state ++ [expr]), [])
  end.

Definition check_step state input :=
  match input with
  | Commit _ =>
      (Shape (S (commit_count state)) (gate_count state) (constraints state),
       [Committed (commit_count state)])
  | Allocate _ _ =>
      let index := gate_count state in
      (Shape (commit_count state) (S index) (constraints state),
       [Left index; Right index; Product index])
  | Multiply x y =>
      let index := gate_count state in
      (Shape (commit_count state) (S index)
        (constraints state ++ [x ++ [(Left index, (-1)%Z)]; y ++ [(Right index, (-1)%Z)]]),
       [Left index; Right index; Product index])
  | Constrain expr =>
      (Shape (commit_count state) (gate_count state) (constraints state ++ [expr]), [])
  end.

Theorem step_projection : forall state input,
  check_step (project state) input =
    (project (fst (prove_step state input)), snd (prove_step state input)).
Proof.
  intros [commits values equations] input. destruct input;
    unfold check_step, prove_step, project; simpl;
    repeat rewrite length_app; simpl;
    repeat rewrite Nat.add_1_r; reflexivity.
Qed.

Fixpoint prove_run state inputs :=
  match inputs with
  | [] => (state, [])
  | input :: rest =>
      let '(next, variables) := prove_step state input in
      let '(final, later) := prove_run next rest in
      (final, variables :: later)
  end.

Fixpoint check_run state inputs :=
  match inputs with
  | [] => (state, [])
  | input :: rest =>
      let '(next, variables) := check_step state input in
      let '(final, later) := check_run next rest in
      (final, variables :: later)
  end.

Theorem run_projection : forall inputs state,
  check_run (project state) inputs =
    (project (fst (prove_run state inputs)), snd (prove_run state inputs)).
Proof.
  induction inputs as [|input rest induction]; intro state; [reflexivity|].
  simpl. rewrite step_projection.
  destruct (prove_step state input) as [next variables]. simpl.
  rewrite induction.
  destruct (prove_run next rest) as [final later]. reflexivity.
Qed.

Theorem witness_independent : forall inputs left right,
  project left = project right ->
  project (fst (prove_run left inputs)) = project (fst (prove_run right inputs)) /\
  snd (prove_run left inputs) = snd (prove_run right inputs).
Proof.
  intros inputs left right same.
  pose proof (run_projection inputs left) as a.
  pose proof (run_projection inputs right) as b.
  rewrite same in a. rewrite b in a.
  split.
  - exact (eq_sym (f_equal fst a)).
  - exact (eq_sym (f_equal snd a)).
Qed.

Theorem verifier_input : forall (verify : shape -> bool) inputs state,
  verify (fst (check_run (project state) inputs)) =
    verify (project (fst (prove_run state inputs))).
Proof. intros. rewrite run_projection. reflexivity. Qed.