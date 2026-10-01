(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool.
Import ListNotations.

Inductive phase :=
  Inspect | Verified | IndexWritten | ChainSynced | IndexSynced
  | IrminSynced | HeadWritten | HeadSynced | JournalSynced | Complete.

Inductive event :=
  ProofAck | IndexWriteAck | ChainSyncAck | IndexSyncAck | IrminSyncAck
  | HeadWriteAck | HeadSyncAck | JournalSyncAck | CleanupAck | Failure.

Record state := State {
  current_phase : phase;
  visible_head : nat;
  wal_present : bool
}.

Definition next (value : state) (stage : phase) :=
  State stage (visible_head value) (wal_present value).

Definition step (target : nat) (value : state) (input : event) :=
  match current_phase value, input with
  | Inspect, ProofAck => next value Verified
  | Verified, IndexWriteAck => next value IndexWritten
  | IndexWritten, ChainSyncAck => next value ChainSynced
  | ChainSynced, IndexSyncAck => next value IndexSynced
  | IndexSynced, IrminSyncAck => next value IrminSynced
  | IrminSynced, HeadWriteAck => State HeadWritten target (wal_present value)
  | HeadWritten, HeadSyncAck => next value HeadSynced
  | HeadSynced, JournalSyncAck => next value JournalSynced
  | JournalSynced, CleanupAck => State Complete (visible_head value) false
  | _, _ => value
  end.

Definition run target value inputs := fold_left (step target) inputs value.

Theorem failure_preserves : forall target value,
  step target value Failure = value.
Proof. intros target [stage head wal]. destruct stage; reflexivity. Qed.

Theorem head_requires_irmin : forall target value input,
  visible_head (step target value input) <> visible_head value ->
  current_phase value = IrminSynced /\ input = HeadWriteAck.
Proof.
  intros target [stage head wal] input changed.
  destruct stage; destruct input; simpl in *; try contradiction; auto.
Qed.

Theorem cleanup_requires_journal_sync : forall target value input,
  wal_present value = true -> wal_present (step target value input) = false ->
  current_phase value = JournalSynced /\ input = CleanupAck.
Proof.
  intros target [stage head wal] input before after.
  destruct stage; destruct input; simpl in *; try congruence; auto.
Qed.

Theorem journal_requires_head_sync : forall target value input,
  current_phase value <> JournalSynced ->
  current_phase (step target value input) = JournalSynced ->
  current_phase value = HeadSynced /\ input = JournalSyncAck.
Proof.
  intros target [stage head wal] input before after.
  destruct stage; destruct input; simpl in *; try congruence; auto.
Qed.

Theorem write_keeps_wal : forall target value,
  wal_present (step target value HeadWriteAck) = wal_present value.
Proof. intros target [stage head wal]. destruct stage; reflexivity. Qed.

Theorem head_is_old_or_new : forall target value input,
  visible_head (step target value input) = visible_head value \/
  visible_head (step target value input) = target.
Proof.
  intros target [stage head wal] input. destruct stage; destruct input; simpl; auto.
Qed.

Theorem trace_head : forall inputs target value,
  visible_head (run target value inputs) = visible_head value \/
  visible_head (run target value inputs) = target.
Proof.
  induction inputs as [|input rest induction]; intros target value; simpl.
  - left. reflexivity.
  - unfold run in *. simpl.
    specialize (induction target (step target value input)).
    destruct induction as [unchanged | published].
    + rewrite unchanged. apply head_is_old_or_new.
    + right. exact published.
Qed.

Theorem complete_is_fixed : forall target head input,
  step target (State Complete head false) input = State Complete head false.
Proof. intros target head input. destruct input; reflexivity. Qed.

Theorem restart_retains_wal : forall target head input,
  wal_present (step target (State Inspect head true) input) = true.
Proof. intros target head input. destruct input; reflexivity. Qed.

Theorem successful_order : forall target head,
  run target (State Inspect head true)
    [ProofAck; IndexWriteAck; ChainSyncAck; IndexSyncAck; IrminSyncAck;
     HeadWriteAck; HeadSyncAck; JournalSyncAck; CleanupAck] = State Complete target false.
Proof. reflexivity. Qed.

Print Assumptions failure_preserves.
Print Assumptions head_requires_irmin.
Print Assumptions cleanup_requires_journal_sync.
Print Assumptions journal_requires_head_sync.
Print Assumptions write_keeps_wal.
Print Assumptions head_is_old_or_new.
Print Assumptions trace_head.
Print Assumptions complete_is_fixed.
Print Assumptions restart_retains_wal.
Print Assumptions successful_order.