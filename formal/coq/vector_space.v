(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Lia.
Open Scope Z_scope.

Definition array cells := if cells =? 0 then 0 else 8 * (cells + 1).

Definition arrays copies cells := copies * array cells.

Definition attention tokens heads keys width :=
  arrays 2 (heads * width) + arrays 4 (tokens * keys * width) +
  heads * (tokens * array width + arrays 4 tokens + array width +
    width * array tokens) + arrays 2 heads + array (heads * width).

Theorem array_positive : forall cells,
  0 < cells -> array cells = 8 * (cells + 1).
Proof.
  intros cells positive. unfold array.
  destruct (cells =? 0) eqn:empty; [apply Z.eqb_eq in empty; lia | reflexivity].
Qed.

Theorem attention_exact : forall tokens heads keys width,
  0 < tokens -> 0 < heads -> 0 < keys -> 0 < width ->
  attention tokens heads keys width =
    8 * (2 * heads * tokens * width + 4 * heads * tokens +
      4 * heads * width + 4 * tokens * keys * width + 2 * heads +
      heads * (tokens + width + 5) + 9).
Proof.
  intros. unfold attention, arrays.
  repeat rewrite array_positive by nia. ring.
Qed.

Theorem matrix_exact : forall rows inner cols,
  0 < rows -> 0 < inner -> 0 < cols ->
  arrays 2 (rows * inner) + arrays 2 (inner * cols) + array (rows * cols) =
    8 * (2 * rows * inner + 2 * inner * cols + rows * cols + 5).
Proof.
  intros. unfold arrays.
  repeat rewrite array_positive by nia. ring.
Qed.

Theorem plan_prefix : forall budget spent rest,
  0 <= spent -> 0 <= rest -> spent + rest <= budget ->
  0 <= budget - spent /\ rest <= budget - spent.
Proof. intros. lia. Qed.

Theorem repeat_cost : forall budget copies cells,
  0 < copies -> 0 < cells -> arrays copies cells <= budget ->
  forall done_count, 0 <= done_count <= copies ->
    0 <= budget - arrays done_count cells /\
    arrays (copies - done_count) cells <= budget - arrays done_count cells.
Proof.
  intros budget copies cells count size fits done_count progress.
  unfold arrays in *. rewrite array_positive in * by assumption. nia.
Qed.