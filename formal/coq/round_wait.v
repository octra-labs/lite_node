(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Bool Lia.
Import ListNotations.

Definition arm (required received now delay : nat) (pending : option nat) :=
  match pending with
  | Some deadline => Some deadline
  | None => if required <=? received then Some (now + delay) else None
  end.

Theorem arm_needs_quorum : forall required received now delay deadline,
  arm required received now delay None = Some deadline -> required <= received.
Proof.
  intros required received now delay deadline H.
  unfold arm in H.
  destruct (required <=? received) eqn:Hcount; try discriminate.
  now apply Nat.leb_le.
Qed.

Theorem repeated_votes_keep_deadline : forall required received now delay deadline,
  arm required received now delay (Some deadline) = Some deadline.
Proof. reflexivity. Qed.

Theorem missing_votes_do_not_arm : forall required received now delay,
  received < required -> arm required received now delay None = None.
Proof.
  intros required received now delay H.
  unfold arm.
  destruct (required <=? received) eqn:Hcount; auto.
  apply Nat.leb_le in Hcount; lia.
Qed.

Record clock := {
  round : nat;
  lock_round : nat;
  deadline : option nat
}.

Definition fire required received now state :=
  match state.(deadline) with
  | Some due =>
    if (required <=? received) && (due <=? now) then
      {| round := S state.(round); lock_round := state.(lock_round); deadline := None |}
    else state
  | None => state
  end.

Theorem premature_timer_keeps_round : forall required received now state,
  received < required -> fire required received now state = state.
Proof.
  intros required received now [r l d] H.
  unfold fire; simpl; destruct d; auto.
  assert (required <=? received = false) as Hcount by (apply Nat.leb_gt; lia).
  now rewrite Hcount.
Qed.

Theorem timer_preserves_lock : forall required received now state,
  (fire required received now state).(lock_round) = state.(lock_round).
Proof.
  intros required received now [r l d]; unfold fire; simpl.
  destruct d; auto.
  destruct ((required <=? received) && (n <=? now)); reflexivity.
Qed.

Theorem due_quorum_progresses : forall required received now r l due,
  required <= received -> due <= now ->
  (fire required received now {| round := r; lock_round := l; deadline := Some due |}).(round)
    = S r.
Proof.
  intros required received now r l due Hcount Htime.
  unfold fire; simpl.
  rewrite (proj2 (Nat.leb_le _ _) Hcount), (proj2 (Nat.leb_le _ _) Htime).
  reflexivity.
Qed.

Definition spread_one (online : list nat) (rounds : nat -> nat) :=
  forall a b, In a online -> In b online -> rounds a <= S (rounds b).

Definition advance (rounds : nat -> nat) who :=
  fun peer => if Nat.eq_dec peer who then S (rounds peer) else rounds peer.

Theorem full_online_quorum : forall (online votes : list nat),
  NoDup votes -> incl votes online -> length online <= length votes -> incl online votes.
Proof.
  intros online votes Hdistinct Hinside Hcount.
  now apply NoDup_length_incl.
Qed.

Theorem quorum_prevents_runaway : forall online votes rounds who,
  NoDup votes -> incl votes online -> length online <= length votes ->
  (forall peer, In peer votes -> rounds who <= rounds peer) ->
  spread_one online rounds -> spread_one online (advance rounds who).
Proof.
  intros online votes rounds who Hdistinct Hinside Hcount Hsigned Hspread.
  assert (incl online votes) as Hall by (eapply full_online_quorum; eauto).
  intros a b Ha Hb; unfold advance.
  destruct (Nat.eq_dec a who); destruct (Nat.eq_dec b who); subst; try lia.
  - specialize (Hsigned b (Hall b Hb)); lia.
  - specialize (Hspread a who Ha Hb); lia.
  - now apply Hspread.
Qed.

Inductive moves (online : list nat) : (nat -> nat) -> (nat -> nat) -> Prop :=
| stay : forall rounds, moves online rounds rounds
| step : forall rounds final who votes,
    NoDup votes -> incl votes online -> length online <= length votes ->
    (forall peer, In peer votes -> rounds who <= rounds peer) ->
    moves online (advance rounds who) final -> moves online rounds final.

Theorem repeated_quorum_steps_keep_span : forall online initial final,
  moves online initial final -> spread_one online initial -> spread_one online final.
Proof.
  intros online initial final Hmoves; induction Hmoves; intros Hspread; auto.
  apply IHHmoves.
  eapply quorum_prevents_runaway; eauto.
Qed.

Theorem quorum_intersection : forall n required faults left right common,
  required <= left -> required <= right -> left + right <= n + common ->
  n + faults < required + required -> faults < common.
Proof. intros; lia. Qed.

Definition permitted required received current limit :=
  (required <=? received) ||
  match limit with Some stop => current <? stop | None => false end.

Theorem no_plan_requires_quorum : forall required received current,
  permitted required received current None = true -> required <= received.
Proof.
  intros required received current H.
  unfold permitted in H; rewrite Bool.orb_false_r in H.
  now apply Nat.leb_le.
Qed.

Theorem relief_step_stays_inside : forall required received current stop,
  received < required -> permitted required received current (Some stop) = true ->
  S current <= stop.
Proof.
  intros required received current stop Hsmall H.
  unfold permitted in H.
  assert (required <=? received = false) as Hcount by (apply Nat.leb_gt; lia).
  rewrite Hcount in H; simpl in H; apply Nat.ltb_lt in H; lia.
Qed.

Fixpoint empty_steps stop count current :=
  match count with
  | 0 => current
  | S remaining =>
    if current <? stop then empty_steps stop remaining (S current) else current
  end.

Theorem relief_cannot_run_forever : forall count stop current,
  current <= stop -> empty_steps stop count current <= stop.
Proof.
  induction count; intros stop current H; simpl; auto.
  destruct (current <? stop) eqn:Hstep; auto.
  apply IHcount; apply Nat.ltb_lt in Hstep; lia.
Qed.