(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith Bool Lia List.
Import ListNotations.

Definition ready epoch target limit head :=
  (epoch <=? head) && (target <=? head + limit).

Theorem ready_exact : forall epoch target limit head,
  ready epoch target limit head = true <->
  epoch <= head /\ target <= head + limit.
Proof.
  intros. unfold ready. rewrite andb_true_iff, !Nat.leb_le. reflexivity.
Qed.

Theorem ready_progress : forall epoch target limit first next,
  first <= next -> ready epoch target limit first = true ->
  ready epoch target limit next = true.
Proof.
  intros. apply ready_exact. apply ready_exact in H0. lia.
Qed.

Record state := State { marked : bool; restarts : nat }.

Definition attest epoch target limit head checked conflict s :=
  if marked s && checked && negb conflict && ready epoch target limit head
  then State false (S (restarts s)) else s.

Theorem consume_requires_check : forall epoch target limit head checked conflict s,
  marked s = true ->
  marked (attest epoch target limit head checked conflict s) = false ->
  checked = true /\ conflict = false /\ epoch <= head /\ target <= head + limit.
Proof.
  intros epoch target limit head checked conflict [mark count] held consumed.
  simpl in held. subst mark. unfold attest in consumed. simpl in consumed.
  destruct checked, conflict; simpl in consumed; try discriminate.
  destruct (ready epoch target limit head) eqn:ok; simpl in consumed; try discriminate.
  apply ready_exact in ok. tauto.
Qed.

Theorem outside_window_held : forall epoch target limit head checked conflict s,
  head < epoch \/ head + limit < target ->
  attest epoch target limit head checked conflict s = s.
Proof.
  intros epoch target limit head checked conflict s outside.
  assert (ready epoch target limit head = false) as no.
  { destruct (ready epoch target limit head) eqn:yes; [|reflexivity].
    apply ready_exact in yes. lia. }
  unfold attest. rewrite no, andb_false_r. reflexivity.
Qed.

Theorem checked_restarts_once : forall epoch target limit head count,
  epoch <= head -> target <= head + limit ->
  attest epoch target limit head true false (State true count) = State false (S count).
Proof.
  intros. unfold attest. simpl.
  assert (ready epoch target limit head = true) as ok by (apply ready_exact; auto).
  now rewrite ok.
Qed.

Theorem consumed_stays_consumed : forall epoch target limit head checked conflict count,
  attest epoch target limit head checked conflict (State false count) = State false count.
Proof. reflexivity. Qed.

Record boot := Boot { head : nat; guard : state }.

Definition restore_range epoch target limit snapshot checked previous :=
  if checked && ready epoch target limit snapshot
  then Boot snapshot (State true (restarts (guard previous))) else previous.

Definition may_start epoch target limit s := ready epoch target limit (head s).
Definition may_vote s := negb (marked (guard s)).

Theorem restore_keeps_vote_closed : forall epoch target limit snapshot checked previous,
  restore_range epoch target limit snapshot checked previous <> previous ->
  may_vote (restore_range epoch target limit snapshot checked previous) = false.
Proof.
  intros. unfold restore_range in *. destruct (checked && ready epoch target limit snapshot);
    [reflexivity|contradiction].
Qed.

Theorem restored_can_attest : forall epoch target limit snapshot previous,
  epoch <= snapshot -> target <= snapshot + limit ->
  let next := restore_range epoch target limit snapshot true previous in
  may_start epoch target limit next = true /\ may_vote next = false /\
  attest epoch target limit (head next) true false (guard next)
    = State false (S (restarts (guard previous))).
Proof.
  intros epoch target limit snapshot previous above near.
  assert (ready epoch target limit snapshot = true) as ok by (apply ready_exact; auto).
  cbv zeta. unfold restore_range. rewrite ok. simpl. unfold may_start, may_vote. simpl.
  repeat split; auto. apply checked_restarts_once; assumption.
Qed.

Theorem failed_snapshot_keeps_state : forall epoch target limit snapshot previous,
  restore_range epoch target limit snapshot false previous = previous.
Proof. reflexivity. Qed.

Inductive cause := Root | Range | Conflict | Journal | NoneNeeded.

Definition restore kind epoch target limit snapshot checked previous :=
  match kind with
  | Root | Range => restore_range epoch target limit snapshot checked previous
  | _ => previous
  end.

Theorem restore_preserves_cause : forall kind epoch target limit snapshot checked previous,
  restore kind epoch target limit snapshot checked previous <> previous ->
  (kind = Root \/ kind = Range) /\ may_vote
    (restore kind epoch target limit snapshot checked previous) = false.
Proof.
  intros kind epoch target limit snapshot checked previous changed.
  destruct kind; simpl in *; try contradiction; split; auto;
    now apply restore_keeps_vote_closed.
Qed.

Theorem restored_root_waits : forall epoch target limit snapshot previous,
  epoch <= snapshot -> target <= snapshot + limit ->
  may_vote (restore Root epoch target limit snapshot true previous) = false.
Proof.
  intros. simpl. unfold restore_range.
  assert (ready epoch target limit snapshot = true) as ok by (apply ready_exact; lia).
  now rewrite ok.
Qed.

Definition covers old_epoch old_target new_head new_target :=
  (old_epoch <=? new_head) && (old_target <=? Nat.max new_head new_target).

Definition carry old_target new_target := Nat.max old_target new_target.

Theorem carry_keeps_targets : forall old_target new_target,
  old_target <= carry old_target new_target /\ new_target <= carry old_target new_target.
Proof. intros. split; [apply Nat.le_max_l | apply Nat.le_max_r]. Qed.

Theorem carry_keeps_window : forall epoch old_target new_target limit head,
  ready epoch (carry old_target new_target) limit head = true ->
  old_target <= head + limit /\ new_target <= head + limit.
Proof.
  intros. apply ready_exact in H. pose proof (carry_keeps_targets old_target new_target). lia.
Qed.

Theorem replacement_retains_need : forall old_epoch old_target new_head new_target,
  covers old_epoch old_target new_head new_target = true ->
  old_epoch <= new_head /\ old_target <= Nat.max new_head new_target.
Proof.
  intros. unfold covers in H. rewrite andb_true_iff, !Nat.leb_le in H. exact H.
Qed.

Record mark := Mark { kind : cause; epoch : nat; base : nat; target : option nat }.

Definition aim value := match target value with Some goal => goal | None => 0 end.

Definition carry_mark prior next :=
  match target prior, kind next with
  | Some goal, Root =>
    if base next <? goal then
      Mark Range (epoch next) (base next) (Some (Nat.max goal (aim next)))
    else next
  | _, _ => next
  end.

Definition covers_mark prior next :=
  match kind prior with
  | Root | Range => covers (epoch prior) (aim prior) (base next) (aim next)
  | _ => false
  end.

Theorem root_carry_progress : forall prior next,
  (kind prior = Root \/ kind prior = Range) -> kind next = Root ->
  epoch prior <= base next -> covers_mark prior (carry_mark prior next) = true.
Proof.
  intros prior next old root progress.
  unfold carry_mark. rewrite root. destruct (target prior) as [goal|] eqn:targeted.
  - destruct (base next <? goal) eqn:below;
      unfold covers_mark; destruct old as [old|old]; rewrite old;
      unfold covers, aim; rewrite targeted; simpl;
      apply andb_true_iff; split; apply Nat.leb_le; auto.
    + lia.
    + lia.
    + apply Nat.ltb_ge in below. lia.
    + apply Nat.ltb_ge in below. lia.
  - unfold covers_mark. destruct old as [old|old]; rewrite old;
      unfold covers, aim; rewrite targeted; simpl;
      apply andb_true_iff; split; try reflexivity; now apply Nat.leb_le.
Qed.

Theorem protected_mark_kept : forall prior next,
  kind prior = Conflict \/ kind prior = Journal -> covers_mark prior next = false.
Proof. intros prior next [conflict|journal]; unfold covers_mark; now rewrite ?conflict, ?journal. Qed.

Theorem range_lower_kept : forall prior next goal next_goal,
  target prior = Some goal -> target next = Some next_goal -> kind next = Range ->
  base next < goal -> next_goal < goal ->
  covers_mark prior (carry_mark prior next) = false.
Proof.
  intros prior next goal next_goal old fresh range below less.
  unfold carry_mark. rewrite old, range. unfold covers_mark, covers, aim.
  rewrite old, fresh. simpl. destruct (kind prior); try reflexivity;
    assert (goal <=? Nat.max (base next) next_goal = false) as no by (apply Nat.leb_gt; lia);
    now rewrite no, andb_false_r.
Qed.

Theorem carry_mark_range : forall prior next goal,
  target prior = Some goal -> kind next = Root -> base next < goal ->
  kind (carry_mark prior next) = Range /\
  epoch (carry_mark prior next) = epoch next /\ base (carry_mark prior next) = base next /\
  exists kept, target (carry_mark prior next) = Some kept /\
    goal <= kept /\ aim next <= kept.
Proof.
  intros prior next goal old root below. unfold carry_mark. rewrite old, root.
  apply Nat.ltb_lt in below. rewrite below. simpl.
  split; [reflexivity|]. split; [reflexivity|]. split; [reflexivity|].
  exists (Nat.max goal (aim next)). split; [reflexivity|].
  split; [apply Nat.le_max_l|apply Nat.le_max_r].
Qed.

Definition preflight floor network limit snapshot :=
  match snapshot with
  | Some epoch => (floor <=? epoch) && (network <=? epoch + limit)
  | None => false
  end.

Lemma preflight_window_spec : forall floor network limit snapshot,
  preflight floor network limit snapshot = true ->
  exists epoch, snapshot = Some epoch /\ floor <= epoch /\ network <= epoch + limit.
Proof.
  intros floor network limit [epoch|] ok; [|discriminate].
  unfold preflight in ok. rewrite andb_true_iff, !Nat.leb_le in ok. eauto.
Qed.

Definition recover cause floor network limit available fetched checked previous :=
  if preflight floor network limit available && checked && ready floor network limit fetched
  then match cause with
    | Root | Range => Boot fetched (State true (restarts (guard previous)))
    | Conflict | Journal | NoneNeeded => Boot fetched (guard previous)
    end
  else previous.

Theorem recover_install_checked : forall cause floor network limit available fetched checked previous,
  recover cause floor network limit available fetched checked previous <> previous ->
  checked = true /\ floor <= fetched /\ network <= fetched + limit.
Proof.
  intros cause floor network limit available fetched checked previous changed.
  unfold recover in changed.
  destruct (preflight floor network limit available && checked && ready floor network limit fetched) eqn:ok;
    [|contradiction].
  rewrite !andb_true_iff in ok. destruct ok as [[_ verified] within].
  apply ready_exact in within. tauto.
Qed.

Theorem recover_marked_waits : forall cause floor network limit available fetched checked previous,
  cause = Root \/ cause = Range ->
  recover cause floor network limit available fetched checked previous <> previous ->
  may_vote (recover cause floor network limit available fetched checked previous) = false.
Proof.
  intros cause floor network limit available fetched checked previous kind changed.
  unfold recover in *.
  destruct (preflight floor network limit available && checked && ready floor network limit fetched);
    [|contradiction].
  destruct kind as [root|range]; subst cause; reflexivity.
Qed.

Theorem recover_unmarked_kept : forall floor network limit available fetched checked previous,
  guard (recover NoneNeeded floor network limit available fetched checked previous) = guard previous.
Proof.
  intros. unfold recover.
  destruct (preflight floor network limit available && checked && ready floor network limit fetched);
    reflexivity.
Qed.

Theorem recover_then_attest : forall cause floor network limit available fetched previous,
  cause = Root \/ cause = Range ->
  preflight floor network limit available = true ->
  floor <= fetched -> network <= fetched + limit ->
  let next := recover cause floor network limit available fetched true previous in
  head next = fetched /\ may_vote next = false /\
  attest floor network limit (head next) true false (guard next)
    = State false (S (restarts (guard previous))).
Proof.
  intros cause floor network limit available fetched previous kind source above near.
  assert (ready floor network limit fetched = true) as valid by (apply ready_exact; auto).
  cbv zeta. unfold recover. rewrite source, valid. simpl.
  destruct kind as [root|range]; subst cause; simpl;
    repeat split; try reflexivity; now apply checked_restarts_once.
Qed.

Theorem recover_other_kept : forall cause floor network limit available fetched checked previous,
  cause <> Root -> cause <> Range ->
  guard (recover cause floor network limit available fetched checked previous) = guard previous.
Proof.
  intros cause floor network limit available fetched checked previous not_root not_range.
  unfold recover.
  destruct (preflight floor network limit available && checked && ready floor network limit fetched);
    [destruct cause; try contradiction|]; reflexivity.
Qed.

Theorem recover_guard_survives : forall kind floor network limit available fetched checked previous,
  marked (guard previous) = true ->
  may_vote (recover kind floor network limit available fetched checked previous) = false.
Proof.
  intros kind floor network limit available fetched checked previous held.
  unfold recover, may_vote.
  destruct (preflight floor network limit available && checked &&
    ready floor network limit fetched); destruct kind; simpl; try reflexivity;
    now rewrite held.
Qed.

Theorem joined_guard_exact : forall first_epoch first_target next_epoch next_target limit current,
  ready (Nat.max first_epoch next_epoch) (Nat.max first_target next_target) limit current = true <->
  ready first_epoch first_target limit current = true /\
  ready next_epoch next_target limit current = true.
Proof.
  intros. rewrite !ready_exact. lia.
Qed.

Theorem recover_old_preserved : forall cause floor network limit available fetched checked previous,
  fetched < floor \/ fetched + limit < network ->
  recover cause floor network limit available fetched checked previous = previous.
Proof.
  intros. unfold recover.
  assert (ready floor network limit fetched = false) as no.
  { destruct (ready floor network limit fetched) eqn:ok; [|reflexivity].
    apply ready_exact in ok. lia. }
  now rewrite no, andb_false_r.
Qed.

Theorem old_snapshot_cannot_move : forall floor network limit snapshot,
  snapshot < floor -> preflight floor network limit (Some snapshot) = false.
Proof.
  intros. unfold preflight. assert (floor <=? snapshot = false) as no by (apply Nat.leb_gt; lia).
  now rewrite no.
Qed.

Inductive phase := Fresh | Written | Synced | Published | Stopped.
Record disk := Disk { stage : phase; durable : bool; visible : bool }.

Inductive install_step : disk -> disk -> Prop :=
| write_guard : forall saved live,
    install_step (Disk Fresh saved live) (Disk Written saved live)
| flush_guard : forall saved live,
    install_step (Disk Written saved live) (Disk Synced true live)
| publish_copy : forall saved live,
    install_step (Disk Synced saved live) (Disk Published saved true)
| interrupt_copy : forall part saved live,
    install_step (Disk part saved live) (Disk Stopped saved live)
| retry_copy : forall saved,
    install_step (Disk Stopped saved false) (Disk Fresh saved false).

Definition disk_safe s :=
  (stage s = Synced -> durable s = true) /\
  (visible s = true -> durable s = true).

Theorem install_step_safe : forall before after,
  disk_safe before -> install_step before after -> disk_safe after.
Proof.
  intros before after safe step. destruct step;
    unfold disk_safe in *; simpl in *; intuition discriminate.
Qed.

Inductive install_run : disk -> Prop :=
| install_start : install_run (Disk Fresh false false)
| install_next : forall before after,
    install_run before -> install_step before after -> install_run after.

Theorem published_guard_durable : forall s,
  install_run s -> visible s = true -> durable s = true.
Proof.
  assert (forall s, install_run s -> disk_safe s) as safe.
  { intros s run. induction run.
    - unfold disk_safe. simpl. intuition discriminate.
    - eapply install_step_safe; eauto. }
  intros s run. exact (proj2 (safe s run)).
Qed.

Theorem interrupted_guard_kept : forall part saved,
  install_run (Disk part saved true) ->
  durable (Disk Stopped saved true) = true.
Proof.
  intros part saved run.
  exact (published_guard_durable (Disk part saved true) run eq_refl).
Qed.

Definition seen_max (heads : list nat) := fold_right Nat.max 0 heads.

Definition start_floor checkpoint limit heads :=
  match heads with
  | nil => None
  | _ => Some (Nat.max checkpoint (seen_max heads - limit))
  end.

Definition start_load checkpoint limit heads available fetched checked :=
  match start_floor checkpoint limit heads, available with
  | Some floor, Some snapshot =>
    if (floor <=? snapshot) && checked && (floor <=? fetched)
    then Some fetched else None
  | _, _ => None
  end.

Lemma seen_max_member : forall heads peer,
  In peer heads -> peer <= seen_max heads.
Proof.
  intros heads. induction heads as [|first rest IH]; intros peer member.
  - contradiction.
  - simpl in member. unfold seen_max. simpl. destruct member as [same|member].
    + subst peer. apply Nat.le_max_l.
    + specialize (IH peer member). unfold seen_max in IH. lia.
Qed.

Theorem start_floor_exact : forall checkpoint limit heads floor fetched,
  start_floor checkpoint limit heads = Some floor ->
  (floor <= fetched <->
   checkpoint <= fetched /\ forall peer, In peer heads -> peer <= fetched + limit).
Proof.
  intros checkpoint limit heads floor fetched selected.
  destruct heads as [|first rest]; [discriminate|].
  unfold start_floor in selected. inversion selected; subst floor. split.
  - intros accepted. split; [lia|].
    intros peer member. pose proof (seen_max_member (first :: rest) peer member).
    unfold seen_max in *. simpl in *. lia.
  - intros [above near].
    assert (seen_max (first :: rest) <= fetched + limit) as covered.
    { assert (forall entries, (forall peer, In peer entries -> peer <= fetched + limit) ->
          seen_max entries <= fetched + limit) as all.
      { intros entries. induction entries as [|entry remainder IH]; intros valid.
        - simpl. lia.
        - assert (entry <= fetched + limit) by (apply valid; now left).
          assert (seen_max remainder <= fetched + limit) by (apply IH; intros; apply valid; now right).
          unfold seen_max in *. simpl. lia. }
      apply all. exact near. }
    unfold seen_max in *. simpl in *. lia.
Qed.

Theorem start_checked_window : forall checkpoint limit heads available fetched checked,
  start_load checkpoint limit heads available fetched checked = Some fetched ->
  checked = true /\ checkpoint <= fetched /\
    forall peer, In peer heads -> peer <= fetched + limit.
Proof.
  intros checkpoint limit heads available fetched checked accepted.
  unfold start_load in accepted.
  destruct (start_floor checkpoint limit heads) as [floor|] eqn:selected;
    destruct available as [snapshot|]; try discriminate.
  destruct ((floor <=? snapshot) && checked && (floor <=? fetched)) eqn:ok;
    [|discriminate].
  rewrite !andb_true_iff, !Nat.leb_le in ok. destruct ok as [[_ verified] above].
  split; [exact verified|].
  now apply (proj1 (start_floor_exact checkpoint limit heads floor fetched selected)).
Qed.

Theorem start_missing_head : forall checkpoint limit available fetched checked,
  start_load checkpoint limit nil available fetched checked = None.
Proof. reflexivity. Qed.

Theorem start_old_not_loaded : forall checkpoint limit heads snapshot fetched checked peer,
  In peer heads -> snapshot + limit < peer ->
  start_load checkpoint limit heads (Some snapshot) fetched checked = None.
Proof.
  intros checkpoint limit heads snapshot fetched checked peer member old.
  unfold start_load. destruct (start_floor checkpoint limit heads) as [floor|] eqn:selected;
    [|reflexivity].
  assert (floor <=? snapshot = false) as no.
  { apply Nat.leb_gt. destruct (le_dec floor snapshot) as [above|below]; [|lia].
    apply (proj1 (start_floor_exact checkpoint limit heads floor snapshot selected)) in above.
    destruct above as [_ covered]. specialize (covered peer member). lia. }
  now rewrite no.
Qed.

Example start_separate_head :
  start_load 100 4096 (811 :: 5000 :: nil) (Some 760) 760 true = None.
Proof. vm_compute. reflexivity. Qed.

Theorem saved_target_kept : forall checkpoint limit marked planned saved floor fetched,
  start_floor checkpoint limit [marked; planned; saved] = Some floor ->
  floor <= fetched -> saved <= fetched + limit.
Proof.
  intros checkpoint limit marked planned saved floor fetched selected accepted.
  apply (proj1 (start_floor_exact checkpoint limit [marked; planned; saved]
    floor fetched selected)) in accepted.
  destruct accepted as [_ covered]. apply covered. simpl. auto.
Qed.

Module IdentityPublish.

Record state := State {
  prepared : option nat;
  synced : bool;
  visible : option nat;
  published : bool;
  saved : nat
}.

Inductive action := Copy | Sync | Publish | Crash.

Definition step value input :=
  match input with
  | Copy => State (Some (saved value)) false (visible value) (published value) (saved value)
  | Sync => match prepared value with
      | Some _ => State (prepared value) true (visible value) (published value) (saved value)
      | None => value
      end
  | Publish => if synced value
      then State (prepared value) true (prepared value) true (saved value)
      else value
  | Crash => State None false (visible value) (published value) (saved value)
  end.

Definition valid value :=
  (forall identity, prepared value = Some identity -> identity = saved value) /\
  (synced value = true -> prepared value = Some (saved value)) /\
  (published value = true -> visible value = Some (saved value)).

Lemma step_valid : forall value input,
  valid value -> valid (step value input).
Proof.
  intros [prepared synced visible published saved] input safe.
  destruct safe as [copy [sync public]].
  destruct input; unfold valid; simpl in *.
  - repeat split; intros; try discriminate; eauto; congruence.
  - destruct prepared; simpl in *; repeat split; intros; try discriminate; eauto.
  - destruct synced; simpl in *; repeat split; intros; try discriminate; eauto.
  - repeat split; intros; try discriminate; eauto.
Qed.

Fixpoint run value inputs :=
  match inputs with [] => value | input :: rest => run (step value input) rest end.

Theorem all_publications_keep_identity : forall identity inputs,
  published (run (State None false None false identity) inputs) = true ->
  visible (run (State None false None false identity) inputs) = Some identity.
Proof.
  assert (forall value inputs, valid value -> valid (run value inputs)) as invariant.
  { intros value inputs. revert value. induction inputs; intros; simpl; auto.
    apply IHinputs. now apply step_valid. }
  assert (forall value input, saved (step value input) = saved value) as same.
  { intros [prepared synced visible published saved] input.
    destruct input; simpl; auto; destruct prepared; auto; destruct synced; auto. }
  assert (forall inputs value, saved (run value inputs) = saved value) as kept.
  { induction inputs; intros; simpl; auto. rewrite IHinputs. apply same. }
  intros identity inputs installed.
  pose proof (invariant (State None false None false identity) inputs) as safe.
  assert (valid (State None false None false identity)) as initial.
  { repeat split; discriminate. }
  specialize (safe initial). destruct safe as [_ [_ safe]].
  specialize (safe installed). rewrite kept in safe. exact safe.
Qed.

Example copy_before_publish :
  visible (run (State None false None false 7) [Copy; Sync; Publish; Crash]) = Some 7.
Proof. reflexivity. Qed.

Example publish_without_copy :
  published (run (State None false None false 7) [Sync; Publish; Crash]) = false.
Proof. reflexivity. Qed.

End IdentityPublish.

Module SnapshotMargin.

Definition allowed limit := Nat.min 3000 (limit - 1).

Definition load checkpoint limit heads (independent : bool) available fetched checked :=
  if independent then start_load checkpoint (allowed limit) heads available fetched checked
  else None.

Theorem independent_required : forall checkpoint limit heads available fetched checked,
  load checkpoint limit heads false available fetched checked = None.
Proof. reflexivity. Qed.

Theorem retained_capacity : forall limit,
  0 < limit -> allowed limit < limit /\ allowed limit <= 3000.
Proof.
  intros limit positive. unfold allowed.
  pose proof (Nat.le_min_l 3000 (limit - 1)).
  pose proof (Nat.le_min_r 3000 (limit - 1)). lia.
Qed.

Theorem accepted_margin : forall checkpoint limit heads independent available fetched checked peer,
  0 < limit -> In peer heads ->
  load checkpoint limit heads independent available fetched checked = Some fetched ->
  independent = true /\ checked = true /\ checkpoint <= fetched /\
    peer + 1 <= fetched + limit /\ peer <= fetched + 3000.
Proof.
  intros checkpoint limit heads independent available fetched checked peer positive member accepted.
  unfold load in accepted. destruct independent; [|discriminate].
  apply start_checked_window in accepted. destruct accepted as [verified [above near]].
  specialize (near peer member). pose proof (retained_capacity limit positive).
  repeat split; auto; lia.
Qed.

Theorem progress_within_capacity : forall checkpoint limit heads available fetched peer advance,
  0 < limit -> In peer heads ->
  load checkpoint limit heads true available fetched true = Some fetched ->
  advance <= limit - allowed limit -> peer + advance <= fetched + limit.
Proof.
  intros checkpoint limit heads available fetched peer advance positive member accepted progress.
  unfold load in accepted. apply start_checked_window in accepted.
  destruct accepted as [_ [_ near]]. specialize (near peer member).
  pose proof (retained_capacity limit positive). lia.
Qed.

Example full_window_rejected :
  load 100 4096 [5000] true (Some 904) 904 true = None.
Proof. vm_compute. reflexivity. Qed.

Example independent_missing :
  load 100 4096 [5000] false (Some 5000) 5000 true = None.
Proof. vm_compute. reflexivity. Qed.

Example accepted_with_capacity :
  load 100 4096 [5000] true (Some 2000) 2000 true = Some 2000.
Proof. vm_compute. reflexivity. Qed.

End SnapshotMargin.

Module GuardSelection.

Definition eligible value :=
  match kind value with Root | Range => true | _ => false end.

Fixpoint targets (entries : list mark) :=
  match entries with
  | [] => None
  | value :: rest =>
    match target value, targets rest with
    | None, remaining => remaining
    | Some goal, None => Some goal
    | Some goal, Some remaining => Some (Nat.max goal remaining)
    end
  end.

Definition prepare entries :=
  match filter eligible entries with
  | [] => None
  | entries =>
    let current := seen_max (map base entries) in
    Some (match targets entries with
      | None => Mark Root (S current) current None
      | Some goal => Mark Range (S current) current (Some (Nat.max (S current) goal))
      end)
  end.

Lemma targets_member : forall entries value goal,
  In value entries -> target value = Some goal ->
  exists kept, targets entries = Some kept /\ goal <= kept.
Proof.
  intros entries. induction entries as [|first rest IH]; intros value goal member source.
  - contradiction.
  - simpl in member. simpl. destruct member as [same|member].
    + subst value. rewrite source. destruct (targets rest) as [remaining|].
      * exists (Nat.max goal remaining). split; [reflexivity|apply Nat.le_max_l].
      * exists goal. auto.
    + destruct (IH value goal member source) as [kept [selected covered]].
      rewrite selected. destruct (target first) as [initial|].
      * exists (Nat.max initial kept). split; [reflexivity|lia].
      * exists kept. auto.
Qed.

Theorem source_target_kept : forall entries value goal,
  In value entries -> eligible value = true -> target value = Some goal ->
  exists result kept,
    prepare entries = Some result /\ kind result = Range /\
    target result = Some kept /\ goal <= kept /\
    base value <= base result /\ epoch result = S (base result) /\ base result < kept.
Proof.
  intros entries value goal member protected source.
  assert (In value (filter eligible entries)) as selected by
    (apply filter_In; auto).
  destruct (targets_member (filter eligible entries) value goal selected source)
    as [kept [targets_kept covered]].
  assert (base value <= seen_max (map base (filter eligible entries))) as progress.
  { apply seen_max_member. apply in_map. exact selected. }
  unfold prepare. destruct (filter eligible entries) as [|first rest] eqn:nonempty;
    [contradiction|].
  rewrite targets_kept.
  set (current := seen_max (map base (first :: rest))) in *.
  exists (Mark Range (S current) current (Some (Nat.max (S current) kept))).
  exists (Nat.max (S current) kept).
  cbn [kind target base epoch].
  repeat split; auto; lia.
Qed.

Example saved_range_kept :
  prepare [Mark Root 101 100 None; Mark Conflict 6001 6000 None;
    Mark Range 81 80 (Some 9000)] =
  Some (Mark Range 101 100 (Some 9000)).
Proof. reflexivity. Qed.

Example root_without_target :
  prepare [Mark Root 101 100 None; Mark Journal 201 200 None] =
  Some (Mark Root 101 100 None).
Proof. reflexivity. Qed.

Example excluded_target :
  prepare [Mark Conflict 201 200 (Some 9000)] = None.
Proof. reflexivity. Qed.

End GuardSelection.

Module HeadRoutes.

Definition contains route entries := existsb (Nat.eqb route) entries.

Lemma contains_exact : forall route entries,
  contains route entries = true <-> In route entries.
Proof.
  intros. unfold contains. rewrite existsb_exists.
  split.
  - intros [entry [member same]]. apply Nat.eqb_eq in same. now subst.
  - intros member. exists route. split; auto. apply Nat.eqb_refl.
Qed.

Definition outside sources heads :=
  filter (fun route => negb (contains route sources)) heads.

Lemma outside_exact : forall sources heads route,
  In route (outside sources heads) <-> In route heads /\ ~ In route sources.
Proof.
  intros. unfold outside. rewrite filter_In, negb_true_iff.
  assert (contains route sources = false <-> ~ In route sources) as absent.
  { split.
    - intros no yes. apply contains_exact in yes. congruence.
    - intros no. destruct (contains route sources) eqn:found; auto.
      exfalso. apply no. now apply contains_exact. }
  now rewrite absent.
Qed.

Inductive status := Missing | Unavailable | Ready.

Definition classify sources heads responses :=
  match outside sources heads with
  | [] => Missing
  | routes => if existsb (fun route => contains route responses) routes
      then Ready else Unavailable
  end.

Theorem ready_has_other_route : forall sources heads responses,
  classify sources heads responses = Ready ->
  exists route, In route heads /\ ~ In route sources /\ In route responses.
Proof.
  intros sources heads responses accepted. unfold classify in accepted.
  destruct (outside sources heads) as [|first rest] eqn:routes; [discriminate|].
  destruct (existsb (fun route => contains route responses) (first :: rest)) eqn:answered;
    [|discriminate].
  apply existsb_exists in answered. destruct answered as [route [member response]].
  rewrite <- routes in member. apply outside_exact in member.
  apply contains_exact in response. exists route. tauto.
Qed.

Theorem missing_is_configuration : forall sources heads responses,
  classify sources heads responses = Missing <->
  forall route, In route heads -> In route sources.
Proof.
  intros sources heads responses. unfold classify.
  destruct (outside sources heads) as [|first rest] eqn:routes.
  - split; auto. intros _ route member.
    destruct (in_dec Nat.eq_dec route sources); auto.
    assert (In route (outside sources heads)) as other by (apply outside_exact; auto).
    rewrite routes in other. contradiction.
  - split.
    + destruct (existsb (fun route => contains route responses) (first :: rest)); discriminate.
    + intros all. assert (In first (outside sources heads)) as member by (rewrite routes; now left).
      apply outside_exact in member. destruct member as [head absent].
      exfalso. apply absent. now apply all.
Qed.

Example missing_despite_replies : classify [1; 2] [1; 2] [1; 2] = Missing.
Proof. reflexivity. Qed.

Example other_route_unavailable : classify [1] [1; 2] [1] = Unavailable.
Proof. reflexivity. Qed.

Example other_route_ready : classify [1] [1; 2] [2] = Ready.
Proof. reflexivity. Qed.

End HeadRoutes.

Module OperatorResume.

Definition reached kind epoch target limit head :=
  match kind with
  | Root | Range => Nat.max epoch (target - limit) <=? head
  | _ => false
  end.

Theorem reached_exact : forall kind epoch target limit head,
  reached kind epoch target limit head = true <->
  (kind = Root \/ kind = Range) /\ ready epoch target limit head = true.
Proof.
  intros kind epoch target limit head. destruct kind; unfold reached; rewrite ready_exact.
  - rewrite Nat.leb_le. intuition lia.
  - rewrite Nat.leb_le. intuition lia.
  - intuition discriminate.
  - intuition discriminate.
  - intuition discriminate.
Qed.

Theorem joint_window : forall epoch target plan_epoch plan_target limit head,
  ready epoch target limit head = true /\ ready plan_epoch plan_target limit head = true <->
  ready (Nat.max epoch plan_epoch) (Nat.max target plan_target) limit head = true.
Proof.
  intros. rewrite !ready_exact. lia.
Qed.

Theorem too_far_refused : forall kind epoch target limit head,
  head < epoch \/ head + limit < target -> reached kind epoch target limit head = false.
Proof.
  intros kind epoch target limit head far.
  destruct (reached kind epoch target limit head) eqn:ok; [|reflexivity].
  apply reached_exact in ok. destruct ok as [_ ok]. apply ready_exact in ok. lia.
Qed.

Theorem progress_retains_start : forall kind epoch target limit first next,
  first <= next -> reached kind epoch target limit first = true ->
  reached kind epoch target limit next = true.
Proof.
  intros kind epoch target limit first next progress started.
  apply reached_exact in started. apply reached_exact. destruct started as [kind_ok start].
  split; [exact kind_ok|]. eapply ready_progress; eauto.
Qed.

Print Assumptions reached_exact.
Print Assumptions joint_window.
Print Assumptions too_far_refused.
Print Assumptions progress_retains_start.

End OperatorResume.