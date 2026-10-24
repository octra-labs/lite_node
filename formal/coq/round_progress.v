(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Bool Lia.
Import ListNotations.

Inductive phase := Propose | Prevote | Precommit.

Definition receive_phase required received missing current :=
  if (required <=? received) && negb missing then Precommit else current.

Theorem few_votes_keep_phase : forall required received missing current,
  received < required -> receive_phase required received missing current = current.
Proof.
  intros required received missing current H.
  unfold receive_phase.
  rewrite (proj2 (Nat.leb_gt _ _) H); reflexivity.
Qed.

Theorem phase_change_requires_quorum : forall required received missing current,
  receive_phase required received missing current <> current -> required <= received.
Proof.
  intros required received missing current H.
  unfold receive_phase in H.
  destruct (required <=? received) eqn:Hcount; simpl in H; try contradiction.
  now apply Nat.leb_le.
Qed.

Theorem missing_header_keeps_phase : forall required received current,
  receive_phase required received true current = current.
Proof.
  intros; unfold receive_phase; rewrite Bool.andb_false_r; reflexivity.
Qed.

Definition record := (nat * nat)%type.

Definition reply own (memory journal : list record) :=
  filter (fun vote => negb (Nat.eqb (fst vote) own)) memory ++ journal.

Theorem own_reply_is_durable : forall own memory journal statement,
  In (own, statement) (reply own memory journal) -> In (own, statement) journal.
Proof.
  intros own memory journal statement H.
  unfold reply in H; apply in_app_or in H; destruct H as [H | H]; auto.
  apply filter_In in H; destruct H as [_ H]; simpl in H.
  rewrite Nat.eqb_refl in H; discriminate.
Qed.

Theorem reply_cannot_add_conflict : forall own memory journal a b,
  (forall x y, In (own, x) journal -> In (own, y) journal -> x = y) ->
  In (own, a) (reply own memory journal) ->
  In (own, b) (reply own memory journal) -> a = b.
Proof.
  intros own memory journal a b Hunique Ha Hb.
  apply Hunique; eapply own_reply_is_durable; eauto.
Qed.

Theorem missing_record_cannot_escape : forall own memory statement,
  ~ In (own, statement) (reply own memory []).
Proof.
  intros own memory statement H.
  apply own_reply_is_durable in H; contradiction.
Qed.

Record state := { current_round : nat; locked_round : nat }.

Definition progress (target : option nat) s :=
  match target with
  | None => s
  | Some r => if s.(current_round) <? r then
      {| current_round := r; locked_round := s.(locked_round) |} else s
  end.

Fixpoint retries count target s :=
  match count with
  | 0 => s
  | S rest => retries rest target (progress target s)
  end.

Theorem repeated_requests_keep_round : forall count s,
  retries count None s = s.
Proof. induction count; intros; simpl; auto. Qed.

Theorem progress_keeps_lock : forall target s,
  (progress target s).(locked_round) = s.(locked_round).
Proof.
  intros [r |] [current lock]; simpl; auto.
  destruct (current <? r); reflexivity.
Qed.

Theorem repeated_progress_keeps_lock : forall count target s,
  (retries count target s).(locked_round) = s.(locked_round).
Proof.
  induction count; intros; simpl; auto.
  rewrite IHcount; apply progress_keeps_lock.
Qed.

Definition stalled_step now last (reports : list nat) s :=
  if (last + 180 <=? now) && existsb (Nat.ltb s.(current_round)) reports then
    ({| current_round := S s.(current_round); locked_round := s.(locked_round) |}, now)
  else (s, last).

Theorem signed_step_is_single : forall now last reports s,
  current_round (fst (stalled_step now last reports s)) <= S s.(current_round).
Proof.
  intros; unfold stalled_step; destruct (_ && _); simpl; lia.
Qed.

Theorem signed_step_keeps_lock : forall now last reports s,
  locked_round (fst (stalled_step now last reports s)) = s.(locked_round).
Proof. intros; unfold stalled_step; destruct (_ && _); reflexivity. Qed.

Theorem signed_step_needs_report : forall now last reports s,
  fst (stalled_step now last reports s) <> s ->
  exists r, In r reports /\ s.(current_round) < r.
Proof.
  intros now last reports s H; unfold stalled_step in H.
  destruct (_ && _) eqn:E; [|contradiction].
  apply andb_true_iff in E; destruct E as [_ E].
  apply existsb_exists in E; destruct E as [r [Hin Hr]].
  exists r; split; auto; now apply Nat.ltb_lt.
Qed.

Theorem signed_step_interval : forall now next last reports later s,
  fst (stalled_step now last reports s) <> s -> next < now + 180 ->
  stalled_step next (snd (stalled_step now last reports s)) later
    (fst (stalled_step now last reports s)) = (fst (stalled_step now last reports s), now).
Proof.
  intros now next last reports later s H Htime.
  unfold stalled_step in *; destruct (_ && _) eqn:E; simpl in *; [|contradiction].
  rewrite (proj2 (Nat.leb_gt _ _) Htime); reflexivity.
Qed.

Fixpoint step_ticks count target s :=
  match count with
  | 0 => s
  | S rest => step_ticks rest target (fst (stalled_step 180 0 [target] s))
  end.

Theorem fixed_witness_is_reached : forall count target s,
  target = count + s.(current_round) ->
  (step_ticks count target s).(current_round) = target.
Proof.
  induction count; intros target [r lock] H; simpl in *; [lia|].
  unfold stalled_step; simpl.
  assert (r <? target = true) as Hr by (apply Nat.ltb_lt; lia).
  rewrite Hr; simpl; apply IHcount; simpl; lia.
Qed.

Lemma outside_dec : forall (xs ys : list nat),
  incl xs ys \/ (exists x, In x xs /\ ~ In x ys).
Proof.
  induction xs as [|a xs IH]; intros ys.
  - left; intros x H; contradiction.
  - destruct (in_dec Nat.eq_dec a ys) as [Ha | Ha].
    + destruct (IH ys) as [Hxs | [x [Hx Hout]]].
      * left; intros x [H | H]; [now subst | now apply Hxs].
      * right; exists x; split; [now right | exact Hout].
    + right; exists a; split; [now left | exact Ha].
Qed.

Theorem signed_floor_has_honest : forall (reports faulty : list nat) target rounds,
  NoDup reports -> length faulty < length reports ->
  (forall peer, In peer reports -> target <= rounds peer) ->
  exists peer, In peer reports /\ ~ In peer faulty /\ target <= rounds peer.
Proof.
  intros reports faulty target rounds Hunique Hcount Hsigned.
  destruct (outside_dec reports faulty) as [Hinside | [peer [Hin Hhonest]]].
  - pose proof (NoDup_incl_length Hunique Hinside); lia.
  - exists peer; repeat split; auto.
Qed.