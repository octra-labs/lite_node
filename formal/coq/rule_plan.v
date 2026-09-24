(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool Arith ZArith Lia.
Import ListNotations.

Record plan := {
  start : nat;
  anchor : nat
}.

Definition present (p : option plan) : bool :=
  match p with Some _ => true | None => false end.

Definition complete (ps : list (option plan)) : bool :=
  forallb present ps.

Inductive mode := Prior | Active.

Definition replay (p : option plan) (epoch : nat) (verified : bool)
  : option mode :=
  match p with
  | None => Some Prior
  | Some p =>
    if epoch <? start p then Some Prior
    else if verified then Some Active else None
  end.

Definition live (ps : list (option plan)) (p : option plan)
  (epoch : nat) (verified : bool) : option mode :=
  if complete ps then replay p epoch verified else None.

Lemma missing_plan : forall left right,
  complete (left ++ None :: right) = false.
Proof.
  intros left right. unfold complete.
  rewrite forallb_app. simpl. apply andb_false_r.
Qed.

Lemma live_requires_plan : forall ps p epoch verified,
  In None ps -> live ps p epoch verified = None.
Proof.
  intros ps p epoch verified missing.
  apply in_split in missing as [left [right ->]].
  unfold live. rewrite missing_plan. reflexivity.
Qed.

Lemma complete_has_plans : forall ps p,
  complete ps = true -> In p ps -> exists value, p = Some value.
Proof.
  intros ps p ready member.
  apply forallb_forall with (x := p) in ready; auto.
  destruct p; simpl in ready; [eauto | discriminate].
Qed.

Lemma replay_unknown : forall epoch verified,
  replay None epoch verified = Some Prior.
Proof. reflexivity. Qed.

Lemma live_preserves_replay : forall ps p epoch verified,
  complete ps = true -> live ps p epoch verified = replay p epoch verified.
Proof.
  intros ps p epoch verified ready. unfold live. rewrite ready. reflexivity.
Qed.

Lemma active_needs_anchor : forall p epoch verified,
  replay p epoch verified = Some Active -> verified = true.
Proof.
  intros [p |] epoch verified result; simpl in result; try discriminate.
  destruct (epoch <? start p); try discriminate.
  destruct verified; congruence.
Qed.

Definition anchored (p : plan) (roots : nat -> option nat)
  (expected epoch : nat) : option mode :=
  replay (Some p) epoch
    (match roots (anchor p) with
     | Some actual => actual =? expected
     | None => false
     end).

Lemma active_root : forall p roots expected epoch,
  anchored p roots expected epoch = Some Active ->
  roots (anchor p) = Some expected.
Proof.
  intros p roots expected epoch result.
  unfold anchored in result. apply active_needs_anchor in result.
  destruct (roots (anchor p)) eqn:read; try discriminate.
  apply Nat.eqb_eq in result. subst. reflexivity.
Qed.

Module Program.

Definition select {A : Type} (p : option plan) (epoch : nat)
  (verified : bool) (prior active : A) : option A :=
  match replay p epoch verified with
  | Some Prior => Some prior
  | Some Active => Some active
  | None => None
  end.

Lemma before {A : Type} : forall p epoch verified (prior active : A),
  epoch < start p -> select (Some p) epoch verified prior active = Some prior.
Proof.
  intros p epoch verified prior active earlier. unfold select, replay.
  apply Nat.ltb_lt in earlier. rewrite earlier. reflexivity.
Qed.

Lemma after {A : Type} : forall p epoch (prior active : A),
  start p <= epoch -> select (Some p) epoch true prior active = Some active.
Proof.
  intros p epoch prior active reached. unfold select, replay.
  apply Nat.ltb_ge in reached. rewrite reached. reflexivity.
Qed.

Lemma unverified_refused {A : Type} : forall p epoch (prior active : A),
  start p <= epoch -> select (Some p) epoch false prior active = None.
Proof.
  intros p epoch prior active reached. unfold select, replay.
  apply Nat.ltb_ge in reached. rewrite reached. reflexivity.
Qed.

Lemma snapshot {A : Type} : forall p epoch left right (prior active : A),
  replay p epoch left = replay p epoch right ->
  select p epoch left prior active = select p epoch right prior active.
Proof.
  intros p epoch left right prior active same. unfold select.
  rewrite same. reflexivity.
Qed.

Lemma active_verified {A : Type} : forall p epoch verified (prior active : A),
  prior <> active -> select p epoch verified prior active = Some active ->
  verified = true.
Proof.
  intros p epoch verified prior active distinct chosen. unfold select in chosen.
  destruct (replay p epoch verified) as [[|]|] eqn:mode; try congruence.
  now apply active_needs_anchor in mode.
Qed.

End Program.

Module Deploy.

Definition admit (p : plan) (epoch : nat) (verified old new : bool) : bool :=
  match replay (Some p) epoch verified with
  | Some Prior => old
  | Some Active => new || ((epoch - start p <? 64) && old)
  | None => false
  end.

Lemma before : forall p epoch verified old new,
  epoch < start p -> admit p epoch verified old new = old.
Proof.
  intros p epoch verified old new earlier. unfold admit, replay.
  apply Nat.ltb_lt in earlier. rewrite earlier. reflexivity.
Qed.

Lemma overlap : forall p epoch old new,
  start p <= epoch < start p + 64 ->
  admit p epoch true old new = (new || old).
Proof.
  intros p epoch old new inside. unfold admit, replay.
  assert (reached : (epoch <? start p) = false) by (apply Nat.ltb_ge; lia).
  assert (open_window : (epoch - start p <? 64) = true) by (apply Nat.ltb_lt; lia).
  rewrite reached, open_window. reflexivity.
Qed.

Lemma closed : forall p epoch old new,
  start p + 64 <= epoch -> admit p epoch true old new = new.
Proof.
  intros p epoch old new ended. unfold admit, replay.
  assert (reached : (epoch <? start p) = false) by (apply Nat.ltb_ge; lia).
  assert (closed_window : (epoch - start p <? 64) = false) by (apply Nat.ltb_ge; lia).
  rewrite reached, closed_window. simpl. apply orb_false_r.
Qed.

Lemma unknown_root : forall p epoch old new,
  start p <= epoch -> admit p epoch false old new = false.
Proof.
  intros p epoch old new reached. unfold admit, replay.
  apply Nat.ltb_ge in reached. rewrite reached. reflexivity.
Qed.

Lemma changed_image : forall p epoch verified,
  admit p epoch verified false false = false.
Proof.
  intros p epoch verified. unfold admit, replay.
  destruct (epoch <? start p); simpl; auto.
  destruct verified; simpl; auto. apply andb_false_r.
Qed.

Print Assumptions before.
Print Assumptions overlap.
Print Assumptions closed.
Print Assumptions unknown_root.
Print Assumptions changed_image.

End Deploy.

Definition ref_ok (epoch head parent claimed : nat) : bool :=
  (0 <? epoch) && (S head =? epoch) && (parent =? claimed).

Lemma ref_parent : forall epoch head parent claimed,
  ref_ok epoch head parent claimed = true ->
  S head = epoch /\ parent = claimed.
Proof.
  intros epoch head parent claimed result.
  unfold ref_ok in result. apply andb_true_iff in result as [step root].
  apply andb_true_iff in step as [_ step].
  apply Nat.eqb_eq in step. apply Nat.eqb_eq in root. auto.
Qed.

Lemma ref_current : forall head parent,
  ref_ok (S head) head parent parent = true.
Proof.
  intros head parent. unfold ref_ok.
  rewrite !Nat.eqb_refl. reflexivity.
Qed.

Module Window.
Local Open Scope Z_scope.

Record pulse := {
  first : Z;
  last : Z;
  count : Z
}.

Definition valid (epoch : Z) (p : pulse) : Prop :=
  0 <= first p /\ first p <= last p /\ last p <= epoch /\
  1 <= count p <= last p - first p + 1.

Definition fresh (epoch : Z) : pulse :=
  {| first := epoch; last := epoch; count := 1 |}.

Definition advance (credit : Z) (p : pulse) : pulse :=
  {| first := first p; last := credit; count := count p + 1 |}.

Definition credit_ok (epoch credit : Z) : bool :=
  (0 <=? credit) && (credit <=? epoch).

Definition step (epoch credit : Z) (prior : option pulse) : option pulse :=
  if credit_ok epoch credit then
    match prior with
    | None => Some (fresh epoch)
    | Some p =>
      if epoch <? last p then None
      else if 8 <? epoch - last p then Some (fresh epoch)
      else if last p <? credit then Some (advance credit p)
      else Some p
    end
  else None.

Lemma credit_range : forall epoch credit,
  credit_ok epoch credit = true <-> 0 <= credit <= epoch.
Proof.
  intros epoch credit. unfold credit_ok.
  rewrite andb_true_iff, !Z.leb_le. tauto.
Qed.

Lemma credit_rejected : forall epoch credit prior,
  credit < 0 \/ epoch < credit -> step epoch credit prior = None.
Proof.
  intros epoch credit prior outside. unfold step.
  destruct (credit_ok epoch credit) eqn:check; auto.
  apply credit_range in check. lia.
Qed.

Lemma fresh_valid : forall epoch,
  0 <= epoch -> valid epoch (fresh epoch).
Proof. intros epoch positive. unfold valid, fresh. simpl. lia. Qed.

Lemma valid_later : forall epoch later p,
  valid epoch p -> epoch <= later -> valid later p.
Proof. intros epoch later p series ordered. unfold valid in *. lia. Qed.

Lemma step_valid : forall epoch credit prior next,
  (forall p, prior = Some p -> valid epoch p) ->
  step epoch credit prior = Some next -> valid epoch next.
Proof.
  intros epoch credit prior next series result. unfold step in result.
  destruct (credit_ok epoch credit) eqn:check; try discriminate.
  apply credit_range in check.
  destruct prior as [p |].
  - specialize (series p eq_refl).
    destruct (epoch <? last p) eqn:back; try discriminate.
    destruct (8 <? epoch - last p) eqn:gap.
    + inversion result; subst. apply fresh_valid. lia.
    + destruct (last p <? credit) eqn:more; inversion result; subst; auto.
      apply Z.ltb_lt in more. unfold valid, advance in *. simpl in *. lia.
  - inversion result; subst. apply fresh_valid. lia.
Qed.

Lemma first_execution : forall epoch credit,
  0 <= credit <= epoch -> step epoch credit None = Some (fresh epoch).
Proof.
  intros epoch credit check. unfold step.
  apply credit_range in check. rewrite check. reflexivity.
Qed.

Lemma gap_reset : forall epoch credit p,
  0 <= credit <= epoch -> 8 < epoch - last p ->
  step epoch credit (Some p) = Some (fresh epoch).
Proof.
  intros epoch credit p check gap. unfold step.
  apply credit_range in check. rewrite check.
  destruct (epoch <? last p) eqn:back.
  - apply Z.ltb_lt in back. lia.
  - destruct (8 <? epoch - last p) eqn:expired; auto.
    apply Z.ltb_ge in expired. lia.
Qed.

Lemma duplicate_credit : forall epoch credit p,
  0 <= credit <= epoch -> 0 <= epoch - last p <= 8 -> credit <= last p ->
  step epoch credit (Some p) = Some p.
Proof.
  intros epoch credit p check gap old. unfold step.
  apply credit_range in check. rewrite check.
  destruct (epoch <? last p) eqn:back.
  - apply Z.ltb_lt in back. lia.
  - destruct (8 <? epoch - last p) eqn:expired.
    + apply Z.ltb_lt in expired. lia.
    + destruct (last p <? credit) eqn:more; auto.
      apply Z.ltb_lt in more. lia.
Qed.

Lemma credit_progress : forall epoch credit p,
  0 <= credit <= epoch -> 0 <= epoch - last p <= 8 -> last p < credit ->
  step epoch credit (Some p) = Some (advance credit p).
Proof.
  intros epoch credit p check gap newer. unfold step.
  apply credit_range in check. rewrite check.
  destruct (epoch <? last p) eqn:back.
  - apply Z.ltb_lt in back. lia.
  - destruct (8 <? epoch - last p) eqn:expired.
    + apply Z.ltb_lt in expired. lia.
    + destruct (last p <? credit) eqn:more; auto.
      apply Z.ltb_ge in more. lia.
Qed.

Lemma continued_gap : forall epoch credit p next,
  step epoch credit (Some p) = Some next -> 1 < count next ->
  0 <= epoch - last p <= 8.
Proof.
  intros epoch credit p next result continued. unfold step in result.
  destruct (credit_ok epoch credit); try discriminate.
  destruct (epoch <? last p) eqn:back; try discriminate.
  apply Z.ltb_ge in back.
  destruct (8 <? epoch - last p) eqn:gap.
  - inversion result; subst. simpl in continued. lia.
  - apply Z.ltb_ge in gap. lia.
Qed.

Lemma no_acceleration : forall epoch p,
  valid epoch p -> 64 <= last p - first p -> first p + 64 <= epoch.
Proof. intros epoch p series span. unfold valid in series. lia. Qed.

Definition prior_step (epoch : Z) (prior : option pulse) : option pulse :=
  match prior with
  | None => Some (fresh epoch)
  | Some p =>
    if epoch <? last p then None
    else if epoch =? last p then Some p
    else if epoch - last p <=? 8 then Some (advance epoch p)
    else Some (fresh epoch)
  end.

Lemma timely_identity : forall epoch prior,
  0 <= epoch -> step epoch epoch prior = prior_step epoch prior.
Proof.
  intros epoch prior positive. unfold step, prior_step.
  assert (check : credit_ok epoch epoch = true).
  { apply credit_range. lia. }
  rewrite check. destruct prior as [p |]; auto.
  destruct (epoch <? last p) eqn:back; auto.
  apply Z.ltb_ge in back.
  destruct (epoch =? last p) eqn:same.
  - apply Z.eqb_eq in same. rewrite same, Z.sub_diag. simpl.
    rewrite Z.ltb_irrefl. reflexivity.
  - apply Z.eqb_neq in same.
    destruct (8 <? epoch - last p) eqn:gap;
      destruct (epoch - last p <=? 8) eqn:near;
      try apply Z.ltb_lt in gap; try apply Z.ltb_ge in gap;
      try apply Z.leb_le in near; try apply Z.leb_gt in near; try lia; auto.
    destruct (last p <? epoch) eqn:more; auto.
    apply Z.ltb_ge in more. lia.
Qed.

Definition delivery (epoch head : Z) : bool :=
  (0 <=? head) && (head <? epoch) && (epoch - 1 - head <=? 2).

Lemma delivery_range : forall epoch head,
  delivery epoch head = true <-> 0 <= head /\ 0 <= epoch - 1 - head <= 2.
Proof.
  intros epoch head. unfold delivery.
  rewrite !andb_true_iff, Z.leb_le, Z.ltb_lt, Z.leb_le. lia.
Qed.

Lemma delivery_credit : forall epoch head,
  delivery epoch head = true -> credit_ok epoch (head + 1) = true.
Proof.
  intros epoch head accepted. apply delivery_range in accepted.
  apply credit_range. lia.
Qed.

Definition pool_expired (head ref : Z) : bool :=
  (ref <? 0) || (head <? 0) || ((ref <=? head) && (2 <? head - ref)).

Lemma pool_range : forall head ref,
  0 <= ref <= head -> pool_expired head ref = (2 <? head - ref).
Proof.
  intros head ref range. unfold pool_expired.
  assert (ref_ok : (ref <? 0) = false) by (apply Z.ltb_ge; lia).
  assert (head_ok : (head <? 0) = false) by (apply Z.ltb_ge; lia).
  assert (past : (ref <=? head) = true) by (apply Z.leb_le; lia).
  rewrite ref_ok, head_ok, past. reflexivity.
Qed.

Lemma pool_matches : forall head ref,
  0 <= ref <= head ->
  pool_expired head ref = negb (delivery (head + 1) ref).
Proof.
  intros head ref range.
  destruct (pool_expired head ref) eqn:expired;
    destruct (delivery (head + 1) ref) eqn:accepted; simpl; auto.
  - rewrite pool_range in expired by assumption. apply Z.ltb_lt in expired.
    apply delivery_range in accepted. lia.
  - rewrite pool_range in expired by assumption. apply Z.ltb_ge in expired.
    assert (ready : delivery (head + 1) ref = true).
    { apply delivery_range. lia. }
    congruence.
Qed.

Lemma future_held : forall head ref,
  0 <= head < ref ->
  pool_expired head ref = false /\ delivery (head + 1) ref = false.
Proof.
  intros head ref future. unfold pool_expired, delivery.
  assert (ref_ok : (ref <? 0) = false) by (apply Z.ltb_ge; lia).
  assert (head_ok : (head <? 0) = false) by (apply Z.ltb_ge; lia).
  assert (past : (ref <=? head) = false) by (apply Z.leb_gt; lia).
  assert (ahead : (ref <? head + 1) = false) by (apply Z.ltb_ge; lia).
  rewrite ref_ok, head_ok, past, ahead, andb_false_r. auto.
Qed.

Definition identity (p : option plan) (epoch : nat) (verified : bool)
  (prior active : list nat) : option (list nat) :=
  match replay p epoch verified with
  | Some Prior => Some prior
  | Some Active => Some (prior ++ active)
  | None => None
  end.

Lemma prior_identity : forall p epoch verified prior active,
  (epoch < start p)%nat ->
  identity (Some p) epoch verified prior active = Some prior.
Proof.
  intros p epoch verified prior active before. unfold identity, replay.
  apply Nat.ltb_lt in before. rewrite before. reflexivity.
Qed.

Lemma missing_identity : forall epoch verified prior active,
  identity None epoch verified prior active = Some prior.
Proof. reflexivity. Qed.

Lemma anchored_identity : forall p epoch prior active,
  (start p <= epoch)%nat ->
  identity (Some p) epoch false prior active = None.
Proof.
  intros p epoch prior active reached. unfold identity, replay.
  apply Nat.ltb_ge in reached. rewrite reached. reflexivity.
Qed.

End Window.

Module Appeals.

Open Scope Z_scope.

Definition sweep (epoch : Z) (deadlines : list Z) :=
  filter (Z.leb epoch) deadlines.

Definition offer epoch deadline deadlines :=
  if epoch <=? deadline then deadline :: sweep epoch deadlines
  else sweep epoch deadlines.

Theorem sweep_live : forall epoch deadlines deadline,
  In deadline (sweep epoch deadlines) -> epoch <= deadline.
Proof.
  intros epoch deadlines deadline kept.
  apply filter_In in kept. destruct kept as [_ kept].
  apply Z.leb_le. exact kept.
Qed.

Theorem offer_live : forall epoch deadline deadlines kept,
  In kept (offer epoch deadline deadlines) -> epoch <= kept.
Proof.
  intros epoch deadline deadlines kept accepted. unfold offer in accepted.
  destruct (epoch <=? deadline) eqn:valid.
  - simpl in accepted. destruct accepted as [same | accepted].
    + subst. apply Z.leb_le. exact valid.
    + eapply sweep_live. exact accepted.
  - eapply sweep_live. exact accepted.
Qed.

Theorem expired_refused : forall epoch deadline deadlines,
  deadline < epoch -> offer epoch deadline deadlines = sweep epoch deadlines.
Proof.
  intros epoch deadline deadlines expired. unfold offer.
  assert (refused : (epoch <=? deadline) = false) by (apply Z.leb_gt; lia).
  rewrite refused. reflexivity.
Qed.

Theorem sweep_size : forall epoch deadlines,
  (length (sweep epoch deadlines) <= length deadlines)%nat.
Proof.
  intros epoch deadlines. unfold sweep.
  induction deadlines as [|deadline rest IH]; simpl; auto.
  destruct (epoch <=? deadline); simpl; lia.
Qed.

Inductive outcome := Marked | Expired | Unread | Capacity.

Definition report (epoch deadline : Z) (receipt : option (Z * bool)) : outcome :=
  match receipt with
  | Some (_, true) => Marked
  | Some (read_epoch, false) =>
    if deadline <? epoch then
      if deadline <? read_epoch then Expired else Unread
    else Capacity
  | None => if deadline <? epoch then Unread else Capacity
  end.

Theorem observed_mark : forall epoch deadline read_epoch,
  report epoch deadline (Some (read_epoch, true)) = Marked.
Proof. reflexivity. Qed.

Theorem open_absence : forall epoch deadline read_epoch,
  read_epoch <= deadline -> deadline < epoch ->
  report epoch deadline (Some (read_epoch, false)) = Unread.
Proof.
  intros epoch deadline read_epoch open_window elapsed. unfold report.
  apply Z.ltb_lt in elapsed. rewrite elapsed.
  assert (not_closed : (deadline <? read_epoch) = false) by (apply Z.ltb_ge; lia).
  rewrite not_closed. reflexivity.
Qed.

Theorem closed_absence : forall epoch deadline read_epoch,
  deadline < epoch -> deadline < read_epoch ->
  report epoch deadline (Some (read_epoch, false)) = Expired.
Proof.
  intros epoch deadline read_epoch elapsed closed. unfold report.
  apply Z.ltb_lt in elapsed. apply Z.ltb_lt in closed.
  rewrite elapsed, closed. reflexivity.
Qed.

Theorem read_unknown : forall epoch deadline,
  deadline < epoch -> report epoch deadline None = Unread.
Proof.
  intros epoch deadline elapsed. unfold report.
  apply Z.ltb_lt in elapsed. rewrite elapsed. reflexivity.
Qed.

Definition ack (epoch : Z) (marked : Z -> bool) (proofs : list (Z * Z)) :=
  filter (fun proof => (epoch <=? snd proof) && negb (marked (fst proof))) proofs.

Theorem ack_unmarked : forall epoch marked proofs proof,
  In proof (ack epoch marked proofs) -> marked (fst proof) = false.
Proof.
  intros epoch marked proofs proof kept. apply filter_In in kept.
  destruct kept as [_ kept]. apply Bool.andb_true_iff in kept.
  destruct kept as [_ kept]. apply Bool.negb_true_iff. exact kept.
Qed.

Theorem ack_live : forall epoch marked proofs proof,
  In proof (ack epoch marked proofs) -> epoch <= snd proof.
Proof.
  intros epoch marked proofs proof kept. apply filter_In in kept.
  destruct kept as [_ kept]. apply Bool.andb_true_iff in kept.
  destruct kept as [kept _]. apply Z.leb_le. exact kept.
Qed.

Theorem ack_keeps : forall epoch marked proofs proof,
  In proof proofs -> epoch <= snd proof -> marked (fst proof) = false ->
  In proof (ack epoch marked proofs).
Proof.
  intros epoch marked proofs proof present live absent. apply filter_In.
  split; [exact present |]. apply Bool.andb_true_iff. split.
  - apply Z.leb_le. exact live.
  - apply Bool.negb_true_iff. exact absent.
Qed.

Theorem ack_repeat : forall epoch marked proofs,
  ack epoch marked (ack epoch marked proofs) = ack epoch marked proofs.
Proof.
  intros epoch marked proofs. unfold ack.
  induction proofs as [|proof rest IH]; simpl; [reflexivity |].
  destruct ((epoch <=? snd proof) && negb (marked (fst proof))) eqn:keep.
  - simpl. rewrite keep, IH. reflexivity.
  - exact IH.
Qed.

Print Assumptions sweep_live.
Print Assumptions offer_live.
Print Assumptions expired_refused.
Print Assumptions sweep_size.
Print Assumptions observed_mark.
Print Assumptions open_absence.
Print Assumptions closed_absence.
Print Assumptions read_unknown.
Print Assumptions ack_unmarked.
Print Assumptions ack_live.
Print Assumptions ack_keeps.
Print Assumptions ack_repeat.

End Appeals.