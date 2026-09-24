(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Lists.List.
From Stdlib Require Import Lia.

Import ListNotations.

Inductive vote_phase :=
  | Prevote
  | Precommit.

Record vote := {
  vote_epoch : nat;
  vote_round : nat;
  vote_phase_value : vote_phase;
  vote_validator : nat;
  vote_proposal : nat
}.

Inductive event :=
  | VoteObserved : vote -> event
  | FinalizedObserved : nat -> nat -> nat -> event.

Definition trace := list event.

Definition prefix (short full : trace) :=
  exists suffix, full = short ++ suffix.

Definition cylinder (sample : trace) (world : trace) :=
  firstn (length sample) world = sample.

Theorem prefix_is_cylinder :
  forall sample world,
    prefix sample world -> cylinder sample world.
Proof.
  intros sample world [suffix extension].
  subst world.
  unfold cylinder.
  rewrite firstn_app.
  rewrite firstn_all.
  replace (length sample - length sample) with 0 by lia.
  simpl.
  rewrite app_nil_r.
  reflexivity.
Qed.

Theorem observed_event_is_monotone :
  forall sample world value,
    prefix sample world ->
    In value sample ->
    In value world.
Proof.
  intros sample world value [suffix extension] observed.
  subst world.
  apply in_or_app.
  left.
  exact observed.
Qed.

Definition finalized_event epoch sample :=
  exists proposal root,
    In (FinalizedObserved epoch proposal root) sample.

Definition fork_event epoch sample :=
  exists left_proposal left_root right_proposal right_root,
    In (FinalizedObserved epoch left_proposal left_root) sample /\
    In (FinalizedObserved epoch right_proposal right_root) sample /\
    left_root <> right_root.

Definition same_vote_slot left right :=
  vote_epoch left = vote_epoch right /\
  vote_round left = vote_round right /\
  vote_phase_value left = vote_phase_value right /\
  vote_validator left = vote_validator right.

Definition vote_conflict left right :=
  same_vote_slot left right /\
  vote_proposal left <> vote_proposal right.

Definition equivocation_event sample :=
  exists left right,
    In (VoteObserved left) sample /\
    In (VoteObserved right) sample /\
    vote_conflict left right.

Theorem finalized_event_is_monotone :
  forall epoch sample world,
    prefix sample world ->
    finalized_event epoch sample ->
    finalized_event epoch world.
Proof.
  intros epoch sample world extension [proposal [root observed]].
  exists proposal, root.
  eapply observed_event_is_monotone.
  exact extension.
  exact observed.
Qed.

Theorem fork_event_is_monotone :
  forall epoch sample world,
    prefix sample world ->
    fork_event epoch sample ->
    fork_event epoch world.
Proof.
  intros epoch sample world extension
    [left_proposal [left_root [right_proposal [right_root facts]]]].
  destruct facts as [left_observed [right_observed roots_differ]].
  exists left_proposal, left_root, right_proposal, right_root.
  split.
  eapply observed_event_is_monotone.
  exact extension.
  exact left_observed.
  split.
  eapply observed_event_is_monotone.
  exact extension.
  exact right_observed.
  exact roots_differ.
Qed.

Theorem equivocation_event_is_monotone :
  forall sample world,
    prefix sample world ->
    equivocation_event sample ->
    equivocation_event world.
Proof.
  intros sample world extension [left [right facts]].
  destruct facts as [left_observed [right_observed conflict]].
  exists left, right.
  split.
  eapply observed_event_is_monotone.
  exact extension.
  exact left_observed.
  split.
  eapply observed_event_is_monotone.
  exact extension.
  exact right_observed.
  exact conflict.
Qed.

Definition honest_intersection_min total quorum byzantine :=
  quorum + quorum - total - byzantine.

Theorem safe_quorums_have_honest_intersection :
  forall total quorum byzantine,
    total + byzantine < quorum + quorum ->
    0 < honest_intersection_min total quorum byzantine.
Proof.
  intros total quorum byzantine safe.
  unfold honest_intersection_min.
  lia.
Qed.

Definition weighted_qc_overlap total left_weight right_weight :=
  left_weight + right_weight - total.

Theorem conflicting_weighted_quorums_expose_one_third :
  forall total left_weight right_weight,
    0 < total ->
    2 * total < 3 * left_weight ->
    2 * total < 3 * right_weight ->
    total < 3 * weighted_qc_overlap total left_weight right_weight.
Proof.
  intros total left_weight right_weight total_positive left_quorum right_quorum.
  unfold weighted_qc_overlap.
  lia.
Qed.

Theorem conflicting_weighted_quorums_expose_slashable_stake :
  forall total left_weight right_weight slashable_weight,
    0 < total ->
    2 * total < 3 * left_weight ->
    2 * total < 3 * right_weight ->
    weighted_qc_overlap total left_weight right_weight <= slashable_weight ->
    total < 3 * slashable_weight.
Proof.
  intros total left_weight right_weight slashable_weight
    total_positive left_quorum right_quorum slashable_covers_overlap.
  unfold weighted_qc_overlap in slashable_covers_overlap.
  lia.
Qed.

Definition prefix_equivalent count (left right : trace) :=
  firstn count left = firstn count right.

Definition adapted_at {Output : Type} count (selection : trace -> Output) :=
  forall left right,
    prefix_equivalent count left right ->
    selection left = selection right.

Theorem adapted_composition :
  forall (Intermediate Output : Type)
    count
    (selection : trace -> Intermediate)
    (project : Intermediate -> Output),
    adapted_at count selection ->
    adapted_at count (fun history => project (selection history)).
Proof.
  intros Intermediate Output count selection project adapted left right same_prefix.
  unfold adapted_at in adapted.
  rewrite (adapted left right same_prefix).
  reflexivity.
Qed.

Section CommittedReplay.

Context {Input State Output : Type}.
Variable advance : State -> Input -> State.
Variable observe : State -> Output.

Definition replay_state initial history :=
  fold_left advance history initial.

Definition state_at count initial history :=
  replay_state initial (firstn count history).

Definition decision_at count initial history :=
  observe (state_at count initial history).

Theorem decision_uses_prefix :
  forall count initial left right,
    firstn count left = firstn count right ->
    decision_at count initial left = decision_at count initial right.
Proof.
  intros count initial left right same_prefix.
  unfold decision_at, state_at.
  rewrite same_prefix.
  reflexivity.
Qed.

Theorem future_preserves_decision :
  forall initial history future,
    decision_at (length history) initial (history ++ future) =
    observe (replay_state initial history).
Proof.
  intros initial history future.
  unfold decision_at, state_at.
  rewrite firstn_app, firstn_all.
  replace (length history - length history) with 0 by lia.
  simpl.
  rewrite app_nil_r.
  reflexivity.
Qed.

Theorem replay_resumes :
  forall initial history future,
    replay_state initial (history ++ future) =
    replay_state (replay_state initial history) future.
Proof.
  intros initial history future.
  unfold replay_state.
  apply fold_left_app.
Qed.

Theorem restore_preserves_future :
  forall initial history restored future,
    restored = replay_state initial history ->
    replay_state restored future = replay_state initial (history ++ future).
Proof.
  intros initial history restored future same_state.
  rewrite same_state, replay_resumes.
  reflexivity.
Qed.

Theorem state_is_sufficient :
  forall initial left right future,
    replay_state initial left = replay_state initial right ->
    observe (replay_state initial (left ++ future)) =
    observe (replay_state initial (right ++ future)).
Proof.
  intros initial left right future same_state.
  rewrite !replay_resumes, same_state.
  reflexivity.
Qed.

End CommittedReplay.

Print Assumptions decision_uses_prefix.
Print Assumptions future_preserves_decision.
Print Assumptions replay_resumes.
Print Assumptions restore_preserves_future.
Print Assumptions state_is_sufficient.