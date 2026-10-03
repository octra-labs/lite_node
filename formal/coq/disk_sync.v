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

Module Snapshot.

Record state := Image {
  files : bool;
  visible : bool;
  archive : bool;
  marked : bool;
  ready : bool
}.

Inductive event := FilesAck | MoveAck | ArchiveAck | UnmarkAck | ImageAck | Failure.

Definition initial := Image false false false true false.

Definition step value input :=
  match input with
  | FilesAck => Image true (visible value) (archive value) (marked value) (ready value)
  | MoveAck =>
      if files value then Image true true (archive value) (marked value) (ready value)
      else value
  | ArchiveAck =>
      if visible value then Image (files value) true true (marked value) (ready value)
      else value
  | UnmarkAck =>
      if archive value then Image (files value) (visible value) true false (ready value)
      else value
  | ImageAck =>
      if archive value && negb (marked value)
      then Image (files value) (visible value) true false true
      else value
  | Failure => value
  end.

Definition safe value :=
  (visible value = true -> files value = true) /\
  (archive value = true -> visible value = true) /\
  (marked value = false -> archive value = true) /\
  (ready value = true -> archive value = true /\ marked value = false).

Definition run value inputs := fold_left step inputs value.

Theorem early_unmark_refused : step initial UnmarkAck = initial.
Proof. reflexivity. Qed.

Theorem initial_safe : safe initial.
Proof. unfold safe, initial. simpl. intuition discriminate. Qed.

Theorem step_safe : forall value input,
  safe value -> safe (step value input).
Proof.
  intros [file moved parent owned done] input valid.
  destruct file, moved, parent, owned, done, input;
    unfold safe, step in *; simpl in *; intuition discriminate.
Qed.

Theorem trace_safe : forall inputs value,
  safe value -> safe (run value inputs).
Proof.
  induction inputs as [|input rest induction]; intros value valid.
  - exact valid.
  - simpl. apply induction. apply step_safe. exact valid.
Qed.

Theorem failure_preserves : forall value, step value Failure = value.
Proof. reflexivity. Qed.

Theorem successful_order :
  run initial [FilesAck; MoveAck; ArchiveAck; UnmarkAck; ImageAck]
  = Image true true true false true.
Proof. reflexivity. Qed.

Print Assumptions early_unmark_refused.
Print Assumptions step_safe.
Print Assumptions trace_safe.
Print Assumptions failure_preserves.
Print Assumptions successful_order.

End Snapshot.

Module Publication.

Record state := Phase {
  certificate : bool;
  files : bool;
  moved : bool;
  archive : bool;
  published : bool
}.

Inductive event := SealAck | FilesAck | MoveAck | ArchiveAck | PublishAck | Failure.

Definition initial := Phase false false false false false.

Definition step value input :=
  match input with
  | SealAck => Phase true (files value) (moved value) (archive value) (published value)
  | FilesAck =>
      if certificate value
      then Phase true true (moved value) (archive value) (published value)
      else value
  | MoveAck =>
      if files value
      then Phase (certificate value) true true (archive value) (published value)
      else value
  | ArchiveAck =>
      if moved value
      then Phase (certificate value) (files value) true true (published value)
      else value
  | PublishAck =>
      if archive value
      then Phase (certificate value) (files value) (moved value) true true
      else value
  | Failure => value
  end.

Definition safe value :=
  (files value = true -> certificate value = true) /\
  (moved value = true -> files value = true) /\
  (archive value = true -> moved value = true) /\
  (published value = true -> archive value = true).

Definition run value inputs := fold_left step inputs value.

Theorem initial_safe : safe initial.
Proof. unfold safe, initial. simpl. intuition discriminate. Qed.

Theorem step_safe : forall value input,
  safe value -> safe (step value input).
Proof.
  intros [sealed synced placed parent announced] input valid.
  destruct sealed, synced, placed, parent, announced, input;
    unfold safe, step in *; simpl in *; intuition discriminate.
Qed.

Theorem trace_safe : forall inputs value,
  safe value -> safe (run value inputs).
Proof.
  induction inputs as [|input rest induction]; intros value valid.
  - exact valid.
  - simpl. apply induction. apply step_safe. exact valid.
Qed.

Theorem duplicate_event : forall value input,
  step (step value input) input = step value input.
Proof.
  intros [sealed synced placed parent announced] input.
  destruct sealed, synced, placed, parent, announced, input; reflexivity.
Qed.

Theorem unsigned_refused :
  run initial [FilesAck; MoveAck; ArchiveAck; PublishAck] = initial.
Proof. reflexivity. Qed.

Theorem parent_required :
  published (run initial [SealAck; FilesAck; MoveAck; PublishAck]) = false.
Proof. reflexivity. Qed.

Theorem successful_order :
  run initial [SealAck; FilesAck; MoveAck; ArchiveAck; PublishAck]
  = Phase true true true true true.
Proof. reflexivity. Qed.

Print Assumptions trace_safe.
Print Assumptions duplicate_event.
Print Assumptions unsigned_refused.
Print Assumptions parent_required.

End Publication.