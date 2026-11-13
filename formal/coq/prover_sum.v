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

Theorem digits_mod : forall base count value,
  0 < base -> digits base count value = value mod base ^ count.
Proof.
  intros base count. induction count as [|count step]; intros value positive.
  - simpl. reflexivity.
  - cbn [digits Nat.pow]. rewrite step by assumption.
    symmetry. apply Nat.Div0.mod_mul_r.
Qed.

Lemma clip_div : forall value scale size,
  0 < scale -> 0 < size ->
  (value mod (scale * size)) / scale = (value / scale) mod size.
Proof.
  intros value scale size positive nonzero.
  rewrite Nat.Div0.mod_mul_r.
  replace (value mod scale + scale * ((value / scale) mod size))
    with (((value / scale) mod size) * scale + value mod scale) by nia.
  rewrite Nat.div_add_l by lia.
  rewrite (Nat.div_small (value mod scale) scale) by (apply Nat.mod_upper_bound; lia).
  lia.
Qed.

Lemma clip_mod : forall value scale size,
  (value mod (scale * size)) mod scale = value mod scale.
Proof.
  intros. rewrite Nat.Div0.mod_mul_r.
  rewrite (Nat.mul_comm scale).
  rewrite Nat.Div0.mod_add. apply Nat.Div0.mod_mod.
Qed.

Lemma clip_shift : forall value offset width,
  offset + width <= 24 ->
  ((value mod 2 ^ 24) / 2 ^ offset) mod 2 ^ width =
  (value / 2 ^ offset) mod 2 ^ width.
Proof.
  intros value offset width size.
  replace 24 with (offset + (24 - offset)) at 1 by lia.
  rewrite Nat.pow_add_r.
  rewrite clip_div by (apply Nat.neq_0_lt_0; apply Nat.pow_nonzero; lia).
  replace (24 - offset) with (width + (24 - offset - width)) by lia.
  rewrite Nat.pow_add_r. apply clip_mod.
Qed.

Definition byte_window (value bit width : nat) :=
  (digits 256 3 (value / 2 ^ (8 * (bit / 8))) / 2 ^ (bit mod 8))
  mod 2 ^ width.

Example byte_window_carry : byte_window (2 ^ 16) 7 16 = 512.
Proof. vm_compute. reflexivity. Qed.

Example byte_window_mask : byte_window 255 0 1 = 1.
Proof. vm_compute. reflexivity. Qed.

Theorem byte_past_end : forall value index,
  value < 256 ^ 32 -> 32 <= index -> (value / 256 ^ index) mod 256 = 0.
Proof.
  intros value index small outside.
  rewrite Nat.div_small; [reflexivity|].
  eapply Nat.lt_le_trans; [exact small|].
  apply Nat.pow_le_mono_r; lia.
Qed.

Theorem byte_window_exact : forall value bit width,
  bit < 256 -> 0 < width <= 16 ->
  byte_window value bit width = (value / 2 ^ bit) mod 2 ^ width.
Proof.
  intros value bit width valid size. unfold byte_window.
  rewrite digits_mod by lia.
  assert (power : 256 ^ 3 = 2 ^ 24).
  { replace 256 with (2 ^ 8) by reflexivity.
    rewrite <- Nat.pow_mul_r. reflexivity. }
  rewrite power.
  rewrite clip_shift by (pose proof (Nat.mod_upper_bound bit 8 ltac:(lia)); lia).
  rewrite Nat.Div0.div_div, <- Nat.pow_add_r.
  pose proof (Nat.div_mod bit 8 ltac:(lia)) as split.
  replace (8 * (bit / 8) + bit mod 8) with bit by lia.
  reflexivity.
Qed.

Print Assumptions digits_mod.
Print Assumptions clip_div.
Print Assumptions clip_mod.
Print Assumptions clip_shift.
Print Assumptions byte_past_end.
Print Assumptions byte_window_exact.