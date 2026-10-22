(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Inductive role := Validator | Observer.

Record peer := { kind : role; live : bool }.

Lemma filter_size : forall A (keep : A -> bool) rows,
  length (filter keep rows) <= length rows.
Proof.
  intros A keep rows. induction rows as [|row rest IH]; simpl.
  - lia.
  - destruct (keep row); simpl; lia.
Qed.

Definition select rows := filter live rows.

Theorem select_exact : forall rows row,
  In row (select rows) <-> In row rows /\ live row = true.
Proof. intros. apply filter_In. Qed.

Definition reclass f rows :=
  map (fun row => {| kind := f (kind row); live := live row |}) (select rows).

Theorem reclass_live : forall f rows row,
  In row rows -> live row = true ->
  In {| kind := f (kind row); live := true |} (reclass f rows).
Proof.
  intros f rows row found alive. unfold reclass.
  apply in_map_iff. exists row. split.
  - now rewrite alive.
  - apply select_exact. auto.
Qed.

Theorem reclass_size : forall f rows,
  length (reclass f rows) <= length rows.
Proof.
  intros. unfold reclass. rewrite map_length. apply filter_size.
Qed.

Definition join cap allowed (row : peer) rows :=
  if allowed && (length rows <? cap) then row :: rows else rows.

Theorem join_limit : forall cap allowed row rows,
  length rows <= cap -> length (join cap allowed row rows) <= cap.
Proof.
  intros. unfold join. destruct allowed; simpl; [|assumption].
  destruct (length rows <? cap) eqn:room; [simpl; apply Nat.ltb_lt in room; lia|assumption].
Qed.

Definition spare row :=
  match kind row with Validator => false | Observer => live row end.

Fixpoint drop_spare rows :=
  match rows with
  | [] => []
  | row :: rest => if spare row then rest else row :: drop_spare rest
  end.

Lemma drop_spare_size : forall rows,
  existsb spare rows = true -> S (length (drop_spare rows)) = length rows.
Proof.
  induction rows as [|row rest IH]; simpl; [discriminate|].
  destruct (spare row); simpl; [reflexivity|].
  intro found. now rewrite (IH found).
Qed.

Lemma drop_keeps_validator : forall rows row,
  kind row = Validator -> In row rows -> In row (drop_spare rows).
Proof.
  induction rows as [|first rest IH]; intros row vote found; simpl in *.
  - contradiction.
  - destruct (spare first) eqn:cut.
    + destruct found as [same|found].
      * subst first. unfold spare in cut. rewrite vote in cut. discriminate.
      * exact found.
    + destruct found as [same|found]; [now left|right; now apply IH].
Qed.

Definition replace rows := {| kind := Validator; live := true |} :: drop_spare rows.

Theorem replace_size : forall rows,
  existsb spare rows = true -> length (replace rows) = length rows.
Proof. intros. unfold replace. simpl. now apply drop_spare_size. Qed.

Theorem replace_keeps_vote : forall rows row,
  kind row = Validator -> In row rows -> In row (replace rows).
Proof. intros. right. now apply drop_keeps_validator. Qed.

Theorem replace_adds_vote : forall rows,
  In {| kind := Validator; live := true |} (replace rows).
Proof. intros. now left. Qed.

Inductive peer_trace cap : list peer -> Prop :=
| peer_empty : peer_trace cap []
| peer_join : forall rows allowed row,
    peer_trace cap rows -> peer_trace cap (join cap allowed row rows)
| peer_replace : forall rows,
    peer_trace cap rows -> existsb spare rows = true -> peer_trace cap (replace rows)
| peer_change : forall rows f,
    peer_trace cap rows -> peer_trace cap (reclass f rows).

Theorem peer_limit : forall cap rows,
  peer_trace cap rows -> length rows <= cap.
Proof.
  intros cap rows trace. induction trace.
  - simpl. lia.
  - apply join_limit. assumption.
  - rewrite replace_size; assumption.
  - pose proof (reclass_size f rows). lia.
Qed.

Definition reserve cap id slots :=
  if existsb (Nat.eqb id) slots then slots
  else if length slots <? cap then id :: slots else slots.

Definition release id slots := filter (fun key => negb (key =? id)) slots.

Theorem reserve_limit : forall cap id slots,
  length slots <= cap -> length (reserve cap id slots) <= cap.
Proof.
  intros cap id slots size. unfold reserve.
  destruct (existsb (Nat.eqb id) slots); [exact size|].
  destruct (length slots <? cap) eqn:room; [simpl; apply Nat.ltb_lt in room; lia|exact size].
Qed.

Theorem release_limit : forall id slots,
  length (release id slots) <= length slots.
Proof.
  intros. apply filter_size.
Qed.

Theorem release_twice : forall id slots,
  release id (release id slots) = release id slots.
Proof.
  intros id slots. induction slots as [|key rest IH]; [reflexivity|].
  unfold release in *. simpl.
  destruct (negb (key =? id)) eqn:keep; simpl; rewrite ?keep, IH; reflexivity.
Qed.

Theorem release_other : forall id key slots,
  key <> id -> In key slots -> In key (release id slots).
Proof.
  intros id key slots different found.
  apply filter_In. split; [exact found|].
  apply negb_true_iff. apply Nat.eqb_neq. exact different.
Qed.

Inductive slots_trace cap : list nat -> Prop :=
| slots_empty : slots_trace cap []
| slots_reserve : forall slots id,
    slots_trace cap slots -> slots_trace cap (reserve cap id slots)
| slots_release : forall slots id,
    slots_trace cap slots -> slots_trace cap (release id slots)
| slots_expire : forall slots keep,
    slots_trace cap slots -> slots_trace cap (filter keep slots).

Theorem trace_limit : forall cap slots,
  slots_trace cap slots -> length slots <= cap.
Proof.
  intros cap slots trace. induction trace.
  - simpl. lia.
  - apply reserve_limit. exact IHtrace.
  - pose proof (release_limit id slots). lia.
  - pose proof (filter_size nat keep slots). lia.
Qed.

Theorem free_slot_used : forall cap id slots,
  length slots < cap -> ~ In id slots ->
  reserve cap id slots = id :: slots.
Proof.
  intros cap id slots room fresh. unfold reserve.
  assert (existsb (Nat.eqb id) slots = false) as absent.
  { destruct (existsb (Nat.eqb id) slots) eqn:present; [|reflexivity].
    apply existsb_exists in present. destruct present as [key [found same]].
    apply Nat.eqb_eq in same. exfalso. apply fresh. now rewrite same. }
  rewrite absent. apply Nat.ltb_lt in room. now rewrite room.
Qed.