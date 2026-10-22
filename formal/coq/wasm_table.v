(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith Lia.

Inductive mode := Prior | Active.

Definition max_elements := 65536.

Definition allow mode elements :=
  match mode with
  | Prior => true
  | Active => elements <=? max_elements
  end.

Inductive table_trace (mode : mode) : nat -> Prop :=
| table_empty : table_trace mode 0
| table_grow : forall current next,
    table_trace mode current ->
    current <= next ->
    allow mode next = true ->
    table_trace mode next.

Theorem active_trace_limit : forall elements,
  table_trace Active elements -> elements <= max_elements.
Proof.
  intros elements trace.
  induction trace as [|current next _ _ _ accepted].
  - unfold max_elements. lia.
  - simpl in accepted. apply Nat.leb_le in accepted. exact accepted.
Qed.

Theorem active_large_refused : forall elements,
  max_elements < elements -> allow Active elements = false.
Proof.
  intros elements large.
  simpl. apply Nat.leb_gt. exact large.
Qed.

Theorem prior_accepts : forall elements,
  table_trace Prior elements.
Proof.
  intro elements.
  econstructor 2 with (current := 0).
  - constructor.
  - lia.
  - reflexivity.
Qed.

Theorem active_slot_limit : forall tables elements,
  tables <= 1 -> table_trace Active elements ->
  tables * elements <= max_elements.
Proof.
  intros tables elements count trace.
  pose proof (active_trace_limit elements trace) as size.
  destruct tables as [|[|tables]]; simpl; lia.
Qed.