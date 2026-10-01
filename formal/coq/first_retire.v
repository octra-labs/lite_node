(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool.
Import ListNotations.

Inductive phase :=
  Inspect | Verified | IndexWritten | ChainSynced | IndexSynced | IrminSynced
  | AbortWritten | AbortSynced | JournalSynced | Retired | Ready.

Inductive event :=
  ProofAck | IndexWriteAck | ChainSyncAck | IndexSyncAck | IrminSyncAck
  | AbortWriteAck | AbortSyncAck | DirectorySyncAck | MarkerCleanupAck
  | BootChecksAck | Failure.

Record state := State {
  current_phase : phase;
  abort_visible : bool;
  boot_guard : bool
}.

Definition next value stage :=
  State stage (abort_visible value) (boot_guard value).

Definition step value input :=
  match current_phase value, input with
  | Inspect, ProofAck => next value Verified
  | Verified, IndexWriteAck => next value IndexWritten
  | IndexWritten, ChainSyncAck => next value ChainSynced
  | ChainSynced, IndexSyncAck => next value IndexSynced
  | IndexSynced, IrminSyncAck => next value IrminSynced
  | IrminSynced, AbortWriteAck => State AbortWritten true (boot_guard value)
  | AbortWritten, AbortSyncAck => next value AbortSynced
  | AbortSynced, DirectorySyncAck => next value JournalSynced
  | JournalSynced, MarkerCleanupAck => next value Retired
  | Retired, BootChecksAck => State Ready (abort_visible value) false
  | _, _ => value
  end.

Definition run value inputs := fold_left step inputs value.
Definition restart value := State Inspect (abort_visible value) true.

Theorem failure_preserves : forall value,
  step value Failure = value.
Proof. intros [stage visible guard]. destruct stage; reflexivity. Qed.

Theorem abort_requires_irmin : forall value input,
  current_phase value <> AbortWritten ->
  current_phase (step value input) = AbortWritten ->
  current_phase value = IrminSynced /\ input = AbortWriteAck.
Proof.
  intros [stage visible guard] input before after.
  destruct stage; destruct input; simpl in *; try congruence; auto.
Qed.

Theorem retire_requires_directory : forall value input,
  current_phase value <> Retired ->
  current_phase (step value input) = Retired ->
  current_phase value = JournalSynced /\ input = MarkerCleanupAck.
Proof.
  intros [stage visible guard] input before after.
  destruct stage; destruct input; simpl in *; try congruence; auto.
Qed.

Theorem directory_requires_file : forall value input,
  current_phase value <> JournalSynced ->
  current_phase (step value input) = JournalSynced ->
  current_phase value = AbortSynced /\ input = DirectorySyncAck.
Proof.
  intros [stage visible guard] input before after.
  destruct stage; destruct input; simpl in *; try congruence; auto.
Qed.

Theorem guard_requires_boot_checks : forall value input,
  boot_guard value = true -> boot_guard (step value input) = false ->
  current_phase value = Retired /\ input = BootChecksAck.
Proof.
  intros [stage visible guard] input before after.
  destruct stage; destruct input; simpl in *; try congruence; auto.
Qed.

Theorem restart_requires_inspection : forall value,
  current_phase (restart value) = Inspect /\ boot_guard (restart value) = true.
Proof. intros. split; reflexivity. Qed.

Theorem visible_abort_not_durable : forall stage visible guard,
  step (restart (State stage visible guard)) DirectorySyncAck =
    restart (State stage visible guard).
Proof. reflexivity. Qed.

Theorem retired_guard_retained : forall visible,
  run (State Inspect visible true)
    [ProofAck; IndexWriteAck; ChainSyncAck; IndexSyncAck; IrminSyncAck;
     AbortWriteAck; AbortSyncAck; DirectorySyncAck; MarkerCleanupAck] =
    State Retired true true.
Proof. reflexivity. Qed.

Theorem complete_after_boot : forall visible,
  run (State Inspect visible true)
    [ProofAck; IndexWriteAck; ChainSyncAck; IndexSyncAck; IrminSyncAck;
     AbortWriteAck; AbortSyncAck; DirectorySyncAck; MarkerCleanupAck; BootChecksAck] =
    State Ready true false.
Proof. reflexivity. Qed.

Print Assumptions failure_preserves.
Print Assumptions abort_requires_irmin.
Print Assumptions retire_requires_directory.
Print Assumptions directory_requires_file.
Print Assumptions guard_requires_boot_checks.
Print Assumptions restart_requires_inspection.
Print Assumptions visible_abort_not_durable.
Print Assumptions retired_guard_retained.
Print Assumptions complete_after_boot.