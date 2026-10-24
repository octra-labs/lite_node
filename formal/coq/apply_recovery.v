(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith Bool List Lia.
Import ListNotations.

Module Worker.
Inductive answer := Success (value : nat) | Rejected (reason : nat) | Host_failure.
Definition receipt answer := match answer with
  | Success value => Some (inl value)
  | Rejected reason => Some (inr reason)
  | Host_failure => None
  end.
Fixpoint execute answers :=
  match answers with
  | [] => Some []
  | answer :: rest => match receipt answer, execute rest with
    | Some value, Some values => Some (value :: values)
    | _, _ => None
    end
  end.
Definition publish previous answers :=
  match execute answers with Some values => previous ++ values | None => previous end.

Theorem host_aborts_batch : forall before after,
  execute (before ++ Host_failure :: after) = None.
Proof.
  induction before; intros; simpl; auto. rewrite IHbefore.
  destruct (receipt a); reflexivity.
Qed.

Theorem host_preserves_receipts : forall before after previous,
  publish previous (before ++ Host_failure :: after) = previous.
Proof. intros. unfold publish. now rewrite host_aborts_batch. Qed.

Theorem completed_batch : forall answers values,
  execute answers = Some values ->
  Forall2 (fun answer value => receipt answer = Some value) answers values.
Proof.
  induction answers; intros; simpl in H.
  - inversion H; constructor.
  - destruct (receipt a) eqn:E; [|discriminate].
    destruct (execute answers) eqn:R; [|discriminate].
    inversion H; subst. constructor; auto.
Qed.

Print Assumptions host_preserves_receipts.
Print Assumptions completed_batch.
End Worker.

Inductive phase := Ready | Running | Done.
Inductive event :=
  | Start
  | Unavailable
  | Expired
  | Checked (valid : bool)
  | Restored (valid : bool).
Record state := State { stage : phase; commits : nat }.

Definition step (s : state) (input : event) : state :=
  match stage s, input with
  | Ready, Start => State Running (commits s)
  | Ready, Restored true => State Done (S (commits s))
  | Running, Checked true => State Done (S (commits s))
  | Running, Unavailable | Running, Checked false =>
    State Ready (commits s)
  | _, _ => s
  end.

Definition retained (s : state) : bool :=
  match stage s with Done => false | _ => true end.

Definition valid (s : state) : Prop :=
  match stage s with Done => commits s = 1 | _ => commits s = 0 end.

Fixpoint run (s : state) (events : list event) : state :=
  match events with
  | [] => s
  | input :: rest => run (step s input) rest
  end.

Theorem step_valid : forall s input, valid s -> valid (step s input).
Proof.
  intros [p n] input h. destruct p; destruct input as [| | |[]|[]];
    cbn [valid step stage commits] in *; subst n; reflexivity.
Qed.

Theorem run_valid : forall events s, valid s -> valid (run s events).
Proof.
  induction events as [|input rest ih]; intros s h; simpl; auto.
  apply ih. apply step_valid. exact h.
Qed.

Theorem at_most_once : forall events,
  commits (run (State Ready 0) events) <= 1.
Proof.
  assert (forall s, valid s -> commits s <= 1) as limit.
  { intros [p n] h. destruct p; cbn [valid commits stage] in *; subst n; lia. }
  intros events. apply limit. apply run_valid. reflexivity.
Qed.

Theorem failure_releases : forall n why,
  why = Unavailable \/ why = Checked false ->
  step (State Running n) why = State Ready n /\
  retained (step (State Running n) why) = true.
Proof.
  intros n why [h|h]; subst; split; reflexivity.
Qed.

Theorem timeout_retains_worker : forall n,
  step (State Running n) Expired = State Running n /\
  retained (step (State Running n) Expired) = true.
Proof. intro n. split; reflexivity. Qed.

Theorem timed_work_not_restarted : forall n count,
  run (State Running n) (repeat Expired count ++ [Start]) = State Running n.
Proof.
  intros n count. induction count; simpl; auto.
Qed.

Theorem invalid_keeps_state : forall s,
  commits (step s (Checked false)) = commits s /\
  retained (step s (Checked false)) = retained s.
Proof. intros [p n]. destruct p; split; reflexivity. Qed.

Fixpoint failures (count : nat) : list event :=
  match count with
  | 0 => []
  | S n => Start :: Unavailable :: failures n
  end.

Theorem failures_keep_record : forall n,
  run (State Ready 0) (failures n) = State Ready 0.
Proof. induction n; simpl; auto. Qed.

Lemma run_app : forall left right s,
  run s (left ++ right) = run (run s left) right.
Proof.
  induction left as [|input rest ih]; intros right s; simpl; auto.
Qed.

Theorem recovers_after_failures : forall n,
  run (State Ready 0) (failures n ++ [Start; Checked true]) = State Done 1.
Proof. intros. rewrite run_app, failures_keep_record. reflexivity. Qed.

Lemma done_stable : forall events n,
  run (State Done n) events = State Done n.
Proof.
  induction events as [|input rest ih]; intros n; simpl; auto.
  destruct input; simpl; apply ih.
Qed.

Theorem checked_catchup_completes : forall n events,
  run (State Ready 0) (failures n ++ Restored true :: events) = State Done 1.
Proof.
  intros. rewrite run_app, failures_keep_record. simpl. apply done_stable.
Qed.

Theorem unchecked_catchup_retains : forall count n,
  run (State Ready n) (repeat (Restored false) count) = State Ready n.
Proof.
  induction count; intros; simpl; auto.
  cbn [step stage commits]. apply IHcount.
Qed.

Theorem catchup_does_not_retire_work : forall count n,
  run (State Running n) (repeat (Restored true) count) = State Running n.
Proof.
  induction count; intros; simpl; auto.
  cbn [step stage commits]. apply IHcount.
Qed.

Inductive hold := Missing (epoch : nat) | Read_error | Manual.
Inductive evidence := Unchecked | Checked_head (epoch : nat).

Definition release (reason : option hold) (proof : evidence) : option hold :=
  match reason, proof with
  | Some (Missing wanted), Checked_head head =>
    if wanted <=? head then None else reason
  | Some Read_error, Checked_head _ => None
  | _, _ => reason
  end.

Fixpoint recover (reason : option hold) (proofs : list evidence) : option hold :=
  match proofs with
  | [] => reason
  | proof :: rest => recover (release reason proof) rest
  end.

Theorem missing_release_checked : forall wanted proof,
  release (Some (Missing wanted)) proof = None ->
  exists head, proof = Checked_head head /\ wanted <= head.
Proof.
  intros wanted [|head] accepted; simpl in accepted; try discriminate.
  destruct (wanted <=? head) eqn:enough; try discriminate.
  exists head. split; auto. apply Nat.leb_le. exact enough.
Qed.

Theorem incomplete_recovery_retains : forall proofs wanted,
  Forall (fun proof => match proof with
    Unchecked => True | Checked_head head => head < wanted end) proofs ->
  recover (Some (Missing wanted)) proofs = Some (Missing wanted).
Proof.
  induction proofs as [|proof rest ih]; intros wanted early; simpl; auto.
  inversion early as [|p ps first remaining]; subst.
  destruct proof as [|head]; simpl; [apply ih; exact remaining|].
  assert ((wanted <=? head) = false) as short by (apply Nat.leb_gt; exact first).
  rewrite short. apply ih. exact remaining.
Qed.

Theorem manual_hold_retained : forall proofs,
  recover (Some Manual) proofs = Some Manual.
Proof.
  induction proofs as [|proof rest ih]; simpl; auto.
Qed.

Inductive apply_hold := Sync_fault | Bundle_fault (epoch : nat) | Operator_hold.
Inductive apply_evidence := Unproved | Finalized_head (epoch : nat).

Definition release_after_apply (reason : apply_hold) (proof : apply_evidence)
  : option apply_hold :=
  match reason, proof with
  | Sync_fault, Finalized_head _ => None
  | Bundle_fault wanted, Finalized_head head =>
    if wanted <=? head then None else Some reason
  | _, _ => Some reason
  end.

Theorem bundle_apply_requires_head : forall wanted proof,
  release_after_apply (Bundle_fault wanted) proof = None ->
  exists head, proof = Finalized_head head /\ wanted <= head.
Proof.
  intros wanted [|head] accepted; simpl in accepted; try discriminate.
  destruct (wanted <=? head) eqn:enough; try discriminate.
  exists head. split; auto. apply Nat.leb_le. exact enough.
Qed.

Theorem sync_apply_requires_finality : forall proof,
  release_after_apply Sync_fault proof = None ->
  exists head, proof = Finalized_head head.
Proof. intros [|head] accepted; simpl in accepted; try discriminate.
  now exists head.
Qed.

Theorem operator_hold_after_apply : forall proof,
  release_after_apply Operator_hold proof = Some Operator_hold.
Proof. intros [|head]; reflexivity. Qed.

Record writers := Writers {
  syncing : bool;
  final_busy : bool;
  range_busy : bool;
  applied : nat
}.

Inductive writer_event :=
| SyncStart
| FinalStart (sample : nat)
| RangeStart
| FinalDone
| RangeDone
| SyncEnd
| Probe.

Definition writer_step w input :=
  match input with
  | SyncStart => Writers true (final_busy w) (range_busy w) (applied w)
  | FinalStart sample =>
    if negb (syncing w || final_busy w || range_busy w) && (sample =? applied w)
    then Writers false true false (applied w) else w
  | RangeStart =>
    if syncing w && negb (final_busy w || range_busy w)
    then Writers true false true (applied w) else w
  | FinalDone =>
    if final_busy w
    then Writers (syncing w) false (range_busy w) (S (applied w)) else w
  | RangeDone =>
    if range_busy w
    then Writers (syncing w) (final_busy w) false (S (applied w)) else w
  | SyncEnd =>
    if final_busy w || range_busy w then w
    else Writers false false false (applied w)
  | Probe => w
  end.

Definition writer_safe w :=
  final_busy w && range_busy w = false /\
  (range_busy w = true -> syncing w = true).

Theorem writer_step_safe : forall w input,
  writer_safe w -> writer_safe (writer_step w input).
Proof.
  intros [sync final range head] input safe.
  unfold writer_safe in *.
  destruct sync, final, range, input; simpl in *; try (intuition congruence).
  all: repeat match goal with
    | |- context [if ?test then _ else _] => destruct test
    end; simpl; intuition congruence.
Qed.

Theorem writer_trace_safe : forall inputs w,
  writer_safe w -> writer_safe (fold_left writer_step inputs w).
Proof.
  induction inputs as [|input rest ih]; intros w safe; simpl; auto.
  apply ih. apply writer_step_safe. exact safe.
Qed.

Theorem sync_waits_for_commit : forall sync range head,
  writer_step (Writers sync true range head) RangeStart =
    Writers sync true range head.
Proof. intros [] [] head; reflexivity. Qed.

Theorem active_read_does_not_write : forall final range head sample,
  writer_step (Writers true final range head) (FinalStart sample) =
    Writers true final range head.
Proof. reflexivity. Qed.

Theorem changed_head_does_not_write : forall w sample,
  sample <> applied w -> writer_step w (FinalStart sample) = w.
Proof.
  intros [sync final range head] sample different.
  cbn in different. apply Nat.eqb_neq in different.
  unfold writer_step. cbn. rewrite different, andb_false_r. reflexivity.
Qed.

Theorem probe_preserves_owner : forall w,
  writer_step w Probe = w.
Proof. reflexivity. Qed.

Theorem sync_end_waits_for_writer : forall sync final range head,
  final || range = true ->
  writer_step (Writers sync final range head) SyncEnd =
    Writers sync final range head.
Proof. intros sync final range head busy. simpl. rewrite busy. reflexivity. Qed.

Theorem drain_progress : forall head through,
  head < through -> through - S head < through - head.
Proof. intros. lia. Qed.

Inductive finish := Continue (target : nat) | Released.

Definition finish_work head queued :=
  if head <? queued then Continue queued else Released.

Definition queue_work queued arrival := Nat.max queued arrival.

Theorem arrival_retained : forall head queued arrival,
  head < arrival ->
  finish_work head (queue_work queued arrival) =
    Continue (queue_work queued arrival).
Proof.
  intros head queued arrival ahead. unfold finish_work, queue_work.
  assert (head <? Nat.max queued arrival = true) as pending.
  { apply Nat.ltb_lt. lia. }
  rewrite pending. reflexivity.
Qed.

Theorem release_has_no_work : forall head queued,
  finish_work head queued = Released -> queued <= head.
Proof.
  intros head queued done. unfold finish_work in done.
  destruct (head <? queued) eqn:pending; try discriminate.
  apply Nat.ltb_ge. exact pending.
Qed.

Theorem queue_keeps_target : forall arrivals queued,
  queued <= fold_left queue_work arrivals queued.
Proof.
  induction arrivals as [|arrival rest ih]; intro queued; simpl; try lia.
  specialize (ih (queue_work queued arrival)).
  unfold queue_work in *. lia.
Qed.

Record jobs := Jobs { issued : nat; settled : nat }.
Definition jobs_safe j := settled j <= issued j.
Definition reserve j := Jobs (S (issued j)) (settled j).
Definition settle j :=
  if settled j <? issued j then Jobs (issued j) (S (settled j)) else j.
Definition idle j := issued j =? settled j.

Theorem reserve_keeps_busy : forall j,
  jobs_safe j -> idle (reserve j) = false.
Proof.
  intros [total done] safe. unfold jobs_safe, idle, reserve in *. simpl in *.
  change ((S total =? done) = false).
  apply Nat.eqb_neq. lia.
Qed.

Theorem settle_keeps_count : forall j,
  jobs_safe j -> jobs_safe (settle j).
Proof.
  intros [total done] safe. unfold jobs_safe, settle in *. simpl in *.
  destruct (done <? total) eqn:remaining; simpl; auto.
  apply Nat.ltb_lt in remaining. lia.
Qed.

Theorem queued_commit_keeps_busy : forall total done,
  S done < total -> idle (settle (Jobs total done)) = false.
Proof.
  intros total done queued. unfold idle, settle. simpl.
  assert ((done <? total) = true) as active by (apply Nat.ltb_lt; lia).
  rewrite active. simpl. apply Nat.eqb_neq. lia.
Qed.

Theorem idle_accounts_for_all : forall j,
  idle j = true -> issued j = settled j.
Proof. intros j ready. apply Nat.eqb_eq. exact ready. Qed.

Record range_checks := Range_checks {
  signed_reply : bool;
  linked_root : bool;
  finality_checked : bool;
  reward_checked : bool
}.

Definition one_source_ok checks :=
  signed_reply checks && linked_root checks &&
  finality_checked checks && reward_checked checks.

Theorem one_source_needs_checks : forall checks,
  one_source_ok checks = true ->
  signed_reply checks = true /\ linked_root checks = true /\
  finality_checked checks = true /\ reward_checked checks = true.
Proof.
  intros [signed linked finality reward] accepted.
  destruct signed, linked, finality, reward; simpl in *;
    try discriminate; repeat split; reflexivity.
Qed.

Record saved_head_checks := Saved_head_checks {
  epoch_grew : bool;
  generation_grew : bool;
  saved_root_matches : bool;
  saved_txid_matches : bool
}.

Definition can_continue_range checks :=
  epoch_grew checks && generation_grew checks &&
  saved_root_matches checks && saved_txid_matches checks.

Theorem advanced_head_needs_saved_record : forall checks,
  can_continue_range checks = true ->
  saved_root_matches checks = true /\ saved_txid_matches checks = true.
Proof.
  intros [epoch generation root txid] accepted.
  destruct epoch, generation, root, txid; simpl in *;
    try discriminate; split; reflexivity.
Qed.

Print Assumptions step_valid.
Print Assumptions at_most_once.
Print Assumptions failure_releases.
Print Assumptions timeout_retains_worker.
Print Assumptions timed_work_not_restarted.
Print Assumptions invalid_keeps_state.
Print Assumptions failures_keep_record.
Print Assumptions recovers_after_failures.
Print Assumptions checked_catchup_completes.
Print Assumptions unchecked_catchup_retains.
Print Assumptions catchup_does_not_retire_work.
Print Assumptions missing_release_checked.
Print Assumptions incomplete_recovery_retains.
Print Assumptions manual_hold_retained.
Print Assumptions writer_trace_safe.
Print Assumptions sync_waits_for_commit.
Print Assumptions active_read_does_not_write.
Print Assumptions changed_head_does_not_write.
Print Assumptions probe_preserves_owner.
Print Assumptions sync_end_waits_for_writer.
Print Assumptions drain_progress.
Print Assumptions arrival_retained.
Print Assumptions release_has_no_work.
Print Assumptions queue_keeps_target.
Print Assumptions reserve_keeps_busy.
Print Assumptions settle_keeps_count.
Print Assumptions queued_commit_keeps_busy.
Print Assumptions idle_accounts_for_all.
Print Assumptions one_source_needs_checks.
Print Assumptions advanced_head_needs_saved_record.