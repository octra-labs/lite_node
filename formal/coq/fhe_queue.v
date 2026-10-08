(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith Bool List Lia.
Import ListNotations.

Record entry := Entry {
  identity : nat;
  deadline : nat;
  abandoned : bool;
  urgent : bool
}.

Record state := State {
  active : option entry;
  pending : list entry;
  closed : bool
}.

Inductive message :=
  | Submit (work : entry)
  | Complete (id : nat)
  | Cancel (id : nat)
  | Tick
  | Stop.

Definition capacity := 8.
Definition view_limit := 2.
Definition empty := State None [] false.
Definition isolate work := Entry (identity work) (deadline work) true (urgent work).
Definition live now work := now <? deadline work.

Fixpoint enqueue work items :=
  match items with
  | [] => [work]
  | first :: rest =>
    if negb (urgent work) || urgent first then first :: enqueue work rest
    else work :: items
  end.

Definition view_full current work := negb (urgent work) &&
  (view_limit <=? length (filter (fun item => negb (urgent item)) (pending current))).

Definition expire now current :=
  State (option_map (fun work =>
    if live now work then work else isolate work) (active current))
    (filter (live now) (pending current)) (closed current).

Definition same id work := Nat.eqb id (identity work).

Definition present id current :=
  match active current with
  | Some work => same id work
  | None => false
  end || existsb (same id) (pending current).

Definition priority work item :=
  if urgent work && negb (urgent item) then isolate item else item.

Definition handle now current input :=
  match input with
  | Submit work =>
    if closed current || negb (live now work)
        || present (identity work) current
        || (capacity <=? length (pending current)) || view_full current work then current
    else State (option_map (priority work) (active current))
      (enqueue (Entry (identity work) (deadline work) false (urgent work)) (pending current)) false
  | Complete id =>
    let next := match active current with
      | Some work => if same id work then None else Some work
      | None => None
      end in
    State next (pending current) (closed current)
  | Cancel id =>
    let next := option_map (fun work =>
      if same id work then isolate work else work) (active current) in
    State next (filter (fun work => negb (same id work)) (pending current)) (closed current)
  | Tick => current
  | Stop => State (option_map isolate (active current)) [] true
  end.

Definition dispatch current :=
  match active current, pending current, closed current with
  | None, work :: rest, false => State (Some work) rest false
  | _, _, _ => current
  end.

Definition delta current now input := dispatch (handle now (expire now current) input).

Lemma filter_size : forall test (items : list entry),
  length (filter test items) <= length items.
Proof.
  intros test items. induction items; simpl; auto.
  destruct (test a); simpl; lia.
Qed.

Lemma expire_size : forall current now,
  length (pending (expire now current)) <= length (pending current).
Proof. intros. apply filter_size. Qed.

Lemma enqueue_size : forall work items,
  length (enqueue work items) = S (length items).
Proof.
  intros work items. induction items; simpl; auto.
  destruct (negb (urgent work) || urgent a); simpl; lia.
Qed.

Lemma dispatch_size : forall current,
  length (pending (dispatch current)) <= length (pending current).
Proof.
  intros [work items stopped]. destruct work; destruct items; destruct stopped;
    simpl; lia.
Qed.

Theorem finite_queue : forall current now input,
  length (pending current) <= capacity ->
  length (pending (delta current now input)) <= capacity.
Proof.
  intros current now input small.
  unfold delta. eapply Nat.le_trans. apply dispatch_size.
  pose proof (expire_size current now) as size.
  remember (expire now current) as next.
  destruct input; unfold handle.
  - destruct (closed next || negb (live now work) || present (identity work) next
      || (capacity <=? length (pending next)) || view_full next work) eqn:accept.
    + lia.
    + apply orb_false_iff in accept. destruct accept as [accept _].
      apply orb_false_iff in accept. destruct accept as [_ room].
      apply Nat.leb_gt in room. simpl. rewrite enqueue_size. lia.
  - simpl. lia.
  - simpl. eapply Nat.le_trans. apply filter_size. lia.
  - lia.
  - simpl. unfold capacity. lia.
Qed.

Lemma dispatch_active : forall current work,
  active current = Some work -> active (dispatch current) = Some work.
Proof. intros [value items stopped] work eq. simpl in eq. subst. reflexivity. Qed.

Lemma expire_active : forall current work now,
  active current = Some work ->
  exists next, active (expire now current) = Some next /\ identity next = identity work.
Proof.
  intros current work now eq. unfold expire. simpl. rewrite eq. simpl.
  destruct (live now work); simpl; eexists; split; reflexivity.
Qed.

Theorem holds_worker : forall current work now input,
  active current = Some work -> input <> Complete (identity work) ->
  exists next, active (delta current now input) = Some next /\ identity next = identity work.
Proof.
  intros current work now input occupied not_done.
  destruct (expire_active current work now occupied) as [kept [has same_id]].
  unfold delta. remember (expire now current) as ready in *.
  assert (exists next, active (handle now ready input) = Some next
    /\ identity next = identity work) as held.
  { destruct input; unfold handle.
    - destruct (closed ready || negb (live now work0)
        || present (identity work0) ready
        || (capacity <=? length (pending ready)) || view_full ready work0).
      + exists kept. auto.
      + simpl. rewrite has. simpl. exists (priority work0 kept). split; auto.
        unfold priority. destruct (urgent work0 && negb (urgent kept)); simpl; auto.
    - simpl. rewrite has. unfold same.
      destruct (id =? identity kept) eqn:eq.
      + apply Nat.eqb_eq in eq. exfalso. apply not_done. congruence.
      + exists kept. auto.
    - simpl. rewrite has. simpl. destruct (same id kept).
      + exists (isolate kept). simpl. auto.
      + exists kept. auto.
    - exists kept. auto.
    - simpl. rewrite has. simpl. exists (isolate kept). simpl. auto. }
  destruct held as [next [has_next same_next]]. exists next. split; auto.
  apply dispatch_active. assumption.
Qed.

Theorem cancel_holds : forall current work now,
  active current = Some work ->
  exists next, active (delta current now (Cancel (identity work))) = Some next
    /\ identity next = identity work /\ abandoned next = true.
Proof.
  intros current work now occupied.
  destruct (expire_active current work now occupied) as [kept [has same_id]].
  unfold delta. remember (expire now current) as ready in *.
  unfold handle. rewrite has. simpl. unfold same. rewrite same_id, Nat.eqb_refl.
  exists (isolate kept). split.
  - reflexivity.
  - simpl. auto.
Qed.

Fixpoint run current inputs :=
  match inputs with
  | [] => current
  | (now, input) :: rest => run (delta current now input) rest
  end.

Theorem holds_trace : forall inputs current work,
  active current = Some work ->
  Forall (fun input => snd input <> Complete (identity work)) inputs ->
  exists next, active (run current inputs) = Some next /\ identity next = identity work.
Proof.
  induction inputs as [|[now input] rest induction]; intros current work has intact.
  - exists work. simpl. auto.
  - inversion intact as [|pair suffix not_done remaining]; subst. simpl in not_done.
    destruct (holds_worker current work now input has not_done) as [next [held same_id]].
    simpl. destruct (induction (delta current now input) next held) as [last [still last_id]].
    + rewrite same_id. assumption.
    + exists last. split; congruence.
Qed.

Theorem finite_trace : forall inputs current,
  length (pending current) <= capacity ->
  length (pending (run current inputs)) <= capacity.
Proof.
  induction inputs as [|[now input] rest induction]; intros current small; simpl; auto.
  apply induction. apply finite_queue. assumption.
Qed.

Theorem initial_finite : forall inputs,
  length (pending (run empty inputs)) <= capacity.
Proof. intros. apply finite_trace. simpl. unfold capacity. lia. Qed.

Definition views items := length (filter (fun work => negb (urgent work)) items).

Lemma filter_views : forall keep items,
  views (filter keep items) <= views items.
Proof.
  intros keep items. induction items as [|work rest induction]; simpl; auto.
  unfold views in *. simpl.
  destruct (keep work); simpl; destruct (urgent work); simpl in *; lia.
Qed.

Lemma enqueue_views : forall work items,
  views (enqueue work items) = views items + if urgent work then 0 else 1.
Proof.
  intros work items. induction items as [|first rest induction].
  - unfold views. simpl. destruct (urgent work); reflexivity.
  - unfold views in *. simpl.
    destruct (urgent work) eqn:priority; destruct (urgent first) eqn:front;
      simpl in *; rewrite ?priority, ?front; simpl; lia.
Qed.

Lemma dispatch_views : forall current,
  views (pending (dispatch current)) <= views (pending current).
Proof.
  intros [work items stopped]. destruct work; destruct items; destruct stopped;
    unfold views; simpl; try lia.
  destruct (urgent e); simpl; lia.
Qed.

Theorem finite_views : forall current now input,
  views (pending current) <= view_limit ->
  views (pending (delta current now input)) <= view_limit.
Proof.
  intros current now input small.
  unfold delta. eapply Nat.le_trans. apply dispatch_views.
  assert (views (pending (expire now current)) <= views (pending current)) as size.
  { apply filter_views. }
  remember (expire now current) as next.
  destruct input; unfold handle.
  - destruct (closed next || negb (live now work) || present (identity work) next
      || (capacity <=? length (pending next)) || view_full next work) eqn:accept.
    + lia.
    + apply orb_false_iff in accept. destruct accept as [_ room].
      simpl. rewrite enqueue_views. simpl.
      unfold view_full in room. destruct (urgent work) eqn:priority.
      * simpl. lia.
      * change ((view_limit <=? views (pending next)) = false) in room.
        apply Nat.leb_gt in room. unfold views in *. lia.
  - simpl. lia.
  - simpl. eapply Nat.le_trans. apply filter_views. lia.
  - lia.
  - simpl. unfold views, view_limit. simpl. lia.
Qed.

Theorem initial_views : forall inputs,
  views (pending (run empty inputs)) <= view_limit.
Proof.
  assert (forall inputs current, views (pending current) <= view_limit ->
    views (pending (run current inputs)) <= view_limit) as preserves.
  { induction inputs as [|[now input] rest induction]; intros current small; simpl; auto.
    apply induction. apply finite_views. assumption. }
  intros. apply preserves. unfold views, empty, view_limit. simpl. lia.
Qed.

Print Assumptions finite_queue.
Print Assumptions holds_worker.
Print Assumptions cancel_holds.
Print Assumptions holds_trace.
Print Assumptions initial_finite.
Print Assumptions finite_views.
Print Assumptions initial_views.

Theorem urgent_interrupts : forall current work next now,
  active (expire now current) = Some work ->
  urgent work = false -> urgent next = true ->
  closed (expire now current) = false -> live now next = true ->
  present (identity next) (expire now current) = false ->
  length (pending (expire now current)) < capacity ->
  active (delta current now (Submit next)) = Some (isolate work).
Proof.
  intros current work next now has view required open alive absent room.
  unfold delta. remember (expire now current) as ready in *.
  unfold handle. rewrite open, alive, absent.
  apply Nat.leb_gt in room. rewrite room.
  unfold view_full. rewrite required. simpl.
  rewrite has. simpl. unfold priority. rewrite required, view. reflexivity.
Qed.

Print Assumptions urgent_interrupts.

Fixpoint retry (fuel : nat) (replies : list (option nat)) : option nat * nat :=
  match fuel, replies with
  | S rest, Some value :: _ => (Some value, 1)
  | S rest, None :: more =>
    let '(value, calls) := retry rest more in (value, S calls)
  | _, _ => (None, 0)
  end.

Theorem retry_count : forall fuel replies,
  snd (retry fuel replies) <= fuel.
Proof.
  induction fuel; intros replies; simpl; [lia |].
  destruct replies as [|[value|] more]; simpl; try lia.
  specialize (IHfuel more).
  destruct (retry fuel more) as [result calls]. simpl in *. lia.
Qed.

Theorem retry_source : forall fuel replies value,
  fst (retry fuel replies) = Some value -> In (Some value) replies.
Proof.
  induction fuel; intros replies value found; simpl in found; try discriminate.
  destruct replies as [|[first|] more]; simpl in found; try discriminate.
  - inversion found. now left.
  - destruct (retry fuel more) as [result calls] eqn:run.
    simpl in found. right. apply (IHfuel more value).
    rewrite run. exact found.
Qed.

Theorem retry_faults : forall fuel count,
  fst (retry fuel (repeat None count)) = None.
Proof.
  induction fuel; intros count; [reflexivity |].
  destruct count; [reflexivity |]. simpl.
  specialize (IHfuel count).
  destruct (retry fuel (repeat None count)) as [result calls].
  simpl in *. exact IHfuel.
Qed.

Theorem retry_finite : forall replies,
  snd (retry 4 replies) <= 4.
Proof. intros. apply retry_count. Qed.

Print Assumptions retry_count.
Print Assumptions retry_source.
Print Assumptions retry_faults.
Print Assumptions retry_finite.