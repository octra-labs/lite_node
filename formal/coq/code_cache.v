(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Record entry := { key : nat; weight : nat; value : nat }.

Fixpoint total (rows : list entry) :=
  match rows with [] => 0 | row :: rest => weight row + total rest end.

Definition fits cap slots rows := (total rows <=? cap) && (length rows <=? slots).

Fixpoint trim cap slots rows :=
  match rows with
  | [] => []
  | row :: rest => if fits cap slots rows then rows else trim cap slots rest
  end.

Theorem trim_limits : forall rows cap slots,
  total (trim cap slots rows) <= cap /\ length (trim cap slots rows) <= slots.
Proof.
  induction rows as [|row rest IH]; intros cap slots; simpl; [lia|].
  destruct (fits cap slots (row :: rest)) eqn:room.
  - unfold fits in room. apply andb_true_iff in room.
    destruct room as [bytes count]. apply Nat.leb_le in bytes.
    apply Nat.leb_le in count. auto.
  - apply IH.
Qed.

Theorem trim_subset : forall rows cap slots item,
  In item (trim cap slots rows) -> In item rows.
Proof.
  induction rows as [|row rest IH]; intros cap slots item found; simpl in *; [contradiction|].
  destruct (fits cap slots (row :: rest)); [exact found|].
  right. now apply (IH cap slots).
Qed.

Definition put cap slots ready item rows :=
  if ready && (weight item <=? cap)
  then trim cap slots (if existsb (fun row => key row =? key item) rows
    then rows else rows ++ [item])
  else rows.

Theorem put_limits : forall rows cap slots ready item,
  total rows <= cap -> length rows <= slots ->
  total (put cap slots ready item rows) <= cap /\
  length (put cap slots ready item rows) <= slots.
Proof.
  intros. unfold put. destruct (ready && (weight item <=? cap)); [apply trim_limits|auto].
Qed.

Lemma trim_fits : forall rows cap slots,
  fits cap slots rows = true -> trim cap slots rows = rows.
Proof. intros [|row rest] cap slots ok; [reflexivity|simpl; now rewrite ok]. Qed.

Theorem put_keeps_present : forall rows cap slots ready item,
  fits cap slots rows = true -> existsb (fun row => key row =? key item) rows = true ->
  put cap slots ready item rows = rows.
Proof.
  intros. unfold put. destruct (ready && (weight item <=? cap)); [|reflexivity].
  rewrite H0. now apply trim_fits.
Qed.

Definition seed_reply (view retained seeded : bool) := if view then seeded else retained.

Lemma view_seed_spec : forall retained seeded,
  seed_reply true retained seeded = seeded.
Proof. reflexivity. Qed.

Definition sound compile rows := Forall (fun row => value row = compile (key row)) rows.

Theorem put_sound : forall compile rows cap slots ready item,
  sound compile rows -> value item = compile (key item) ->
  sound compile (put cap slots ready item rows).
Proof.
  intros compile rows cap slots ready item valid item_valid.
  unfold put. destruct (ready && (weight item <=? cap)); [|exact valid].
  unfold sound in *. apply Forall_forall. intros row found.
  apply trim_subset in found.
  destruct (existsb (fun row => key row =? key item) rows) eqn:present.
  - rewrite Forall_forall in valid. now apply valid.
  - apply in_app_iff in found. destruct found as [found|found].
    + rewrite Forall_forall in valid. now apply valid.
    + simpl in found. destruct found as [same|absurd]; [now subst|contradiction].
Qed.

Fixpoint lookup id rows :=
  match rows with
  | [] => None
  | row :: rest => if key row =? id then Some (value row) else lookup id rest
  end.

Definition fetch compile id rows :=
  match lookup id rows with Some answer => answer | None => compile id end.

Theorem cache_transparent : forall compile rows id,
  sound compile rows -> fetch compile id rows = compile id.
Proof.
  intros compile rows. induction rows as [|row rest IH]; intros id valid; [reflexivity|].
  inversion valid as [|first remainder correct others]; subst.
  unfold fetch. simpl. destruct (key row =? id) eqn:same.
  - apply Nat.eqb_eq in same. now rewrite correct, same.
  - apply IH. exact others.
Qed.

Theorem failed_not_saved : forall cap slots item rows,
  put cap slots false item rows = rows.
Proof. reflexivity. Qed.

Theorem large_not_saved : forall cap slots item rows ready,
  cap < weight item -> put cap slots ready item rows = rows.
Proof.
  intros. unfold put. assert (weight item <=? cap = false) as over by (apply Nat.leb_gt; lia).
  now rewrite over, andb_false_r.
Qed.

Record work := Work { compiler : option nat; guests : list nat }.

Inductive step : work -> work -> Prop :=
| begin_code : forall id running, step (Work None running) (Work (Some id) running)
| end_code : forall id running, step (Work (Some id) running) (Work None (id :: running))
| end_guest : forall busy running id,
    step (Work busy running) (Work busy (remove Nat.eq_dec id running)).

Inductive guest_steps : work -> work -> Prop :=
| guest_idle : forall s, guest_steps s s
| guest_next : forall busy running id final,
    guest_steps (Work busy (remove Nat.eq_dec id running)) final ->
    guest_steps (Work busy running) final.

Lemma guests_preserve_compiler : forall first last,
  guest_steps first last -> compiler first = compiler last.
Proof. intros first last flow. induction flow; simpl in *; auto. Qed.

Theorem start_does_not_own_compiler : forall id running last other,
  guest_steps (Work None (id :: running)) last ->
  step last (Work (Some other) (guests last)).
Proof.
  intros id running [busy live] other flow.
  apply guests_preserve_compiler in flow. simpl in flow. subst busy. constructor.
Qed.

Definition pending_put cap (id : nat) rows :=
  if length rows <? cap then id :: rows else rows.

Theorem pending_cap : forall cap id rows,
  length rows <= cap -> length (pending_put cap id rows) <= cap.
Proof.
  intros. unfold pending_put. destruct (length rows <? cap) eqn:room; [|assumption].
  apply Nat.ltb_lt in room. simpl. lia.
Qed.