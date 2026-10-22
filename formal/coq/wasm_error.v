(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith.
Import ListNotations.

Inductive reason := Element (offset amount : nat) | ImportKind.
Record local_error := Local_error { input : reason; store_id : nat; item_id : nat }.

Definition render (active : bool) value :=
  (input value, if active then None else Some (store_id value, item_id value)).

Fixpoint run active store (items : list (reason * nat)) :=
  match items with
  | [] => []
  | (value, item) :: rest => render active (Local_error value store item) :: run active (S store) rest
  end.

Theorem active_trace : forall items first second,
  run true first items = run true second items.
Proof.
  induction items as [|[value item] rest IH]; intros first second; simpl; [reflexivity|].
  f_equal. apply IH.
Qed.

Theorem active_fields : forall items store,
  map fst (run true store items) = map fst items.
Proof.
  induction items as [|[value item] rest IH]; intro store; simpl; [reflexivity|].
  f_equal. apply IH.
Qed.

Theorem active_hides_ids : forall items store,
  map snd (run true store items) = repeat None (length items).
Proof.
  induction items as [|[value item] rest IH]; intro store; simpl; [reflexivity|].
  f_equal. apply IH.
Qed.

Theorem prior_keeps_ids : forall value store item,
  render false (Local_error value store item) = (value, Some (store, item)).
Proof. reflexivity. Qed.