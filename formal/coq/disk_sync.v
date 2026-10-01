(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool.
Import ListNotations.

Inductive operation := FileSync | DirectorySync.

Definition plan := [FileSync; DirectorySync].

Inductive completed : list operation -> list bool -> Prop :=
| Done : completed [] []
| Synced : forall action actions results,
    completed actions results -> completed (action :: actions) (true :: results).

Fixpoint acknowledge (actions : list operation) (results : list bool) : bool :=
  match actions, results with
  | [], [] => true
  | _ :: actions, true :: results => acknowledge actions results
  | _, _ => false
  end.

Theorem successful_calls : forall actions results,
  acknowledge actions results = true -> completed actions results.
Proof.
  induction actions as [|action actions IH]; intros results accepted.
  - destruct results; [constructor|discriminate].
  - destruct results as [|result results]; [discriminate|].
    destruct result; [|discriminate].
    constructor. apply IH. exact accepted.
Qed.

Theorem failure_refused : forall actions results,
  In false results -> acknowledge actions results = false.
Proof.
  induction actions as [|action actions IH]; intros results failed.
  - destruct results; [contradiction|reflexivity].
  - destruct results as [|result results]; [contradiction|].
    destruct result; [|reflexivity].
    simpl. apply IH. simpl in failed. destruct failed as [impossible|failed].
    + discriminate.
    + exact failed.
Qed.

Theorem both_barriers : forall results,
  acknowledge plan results = true -> results = [true; true].
Proof.
  intros results accepted.
  destruct results as [|file results]; [discriminate|].
  destruct file; [|discriminate].
  destruct results as [|directory results]; [discriminate|].
  destruct directory; [|discriminate].
  destruct results; [reflexivity|discriminate].
Qed.

Definition retry (_visible : bool) := plan.

Theorem visible_is_not_durable : forall visible results,
  acknowledge (retry visible) results = true -> results = [true; true].
Proof. intros. apply both_barriers. exact H. Qed.

Theorem missing_directory_refused : acknowledge plan [true] = false.
Proof. reflexivity. Qed.

Theorem ignored_failure_rejected : acknowledge plan [false; true] = false.
Proof. reflexivity. Qed.

Theorem synced_accepted : acknowledge plan [true; true] = true.
Proof. reflexivity. Qed.

Definition journal_plan := [FileSync; DirectorySync; DirectorySync].

Theorem journal_barriers : forall results,
  acknowledge journal_plan results = true -> results = [true; true; true].
Proof.
  intros results accepted.
  destruct results as [|file results]; [discriminate|].
  destruct file; [|discriminate].
  destruct results as [|directory results]; [discriminate|].
  destruct directory; [|discriminate].
  destruct results as [|parent results]; [discriminate|].
  destruct parent; [|discriminate].
  destruct results; [reflexivity|discriminate].
Qed.

Theorem journal_parent_required : acknowledge journal_plan [true; true] = false.
Proof. reflexivity. Qed.

Theorem journal_retry : forall (visible : bool) results,
  acknowledge (if visible then journal_plan else journal_plan) results = true ->
  results = [true; true; true].
Proof. intros visible results accepted. destruct visible; apply journal_barriers; exact accepted. Qed.