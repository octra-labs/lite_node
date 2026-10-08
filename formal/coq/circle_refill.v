(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool Arith Lia.
Import ListNotations.

Record tx := {
  sender : nat;
  nonce : nat;
  isolated : bool
}.

Section Refill.
Variable cutoffs : nat -> option nat.

Definition keep (item : tx) : bool :=
  negb (isolated item) &&
  match cutoffs (sender item) with
  | None => true
  | Some first => nonce item <? first
  end.

Definition select (inputs : list tx) : list tx := filter keep inputs.

Theorem selected_input : forall inputs item,
  In item (select inputs) -> In item inputs.
Proof.
  intros inputs item present.
  apply filter_In in present. exact (proj1 present).
Qed.

Theorem selected_not_isolated : forall inputs item,
  In item (select inputs) -> isolated item = false.
Proof.
  intros inputs item present.
  apply filter_In in present. destruct present as [_ kept].
  unfold keep in kept. apply andb_true_iff in kept.
  apply negb_true_iff. exact (proj1 kept).
Qed.

Theorem dependency_excluded : forall inputs item first,
  cutoffs (sender item) = Some first -> first <= nonce item ->
  ~ In item (select inputs).
Proof.
  intros inputs item first cutoff later present.
  apply filter_In in present. destruct present as [_ kept].
  unfold keep in kept. rewrite cutoff in kept.
  apply andb_true_iff in kept. destruct kept as [_ earlier].
  apply Nat.ltb_lt in earlier. lia.
Qed.

Theorem selection_limit : forall inputs,
  length (select inputs) <= length inputs.
Proof.
  unfold select. intros inputs. apply filter_length_le.
Qed.

Theorem selection_order : forall left right,
  select (left ++ right) = select left ++ select right.
Proof.
  unfold select. intros left right. apply filter_app.
Qed.

Theorem no_second_isolation : forall inputs,
  existsb isolated (select inputs) = false.
Proof.
  intros inputs.
  destruct (existsb isolated (select inputs)) eqn:found; [|reflexivity].
  apply existsb_exists in found. destruct found as [item [present yes]].
  pose proof (selected_not_isolated inputs item present). congruence.
Qed.

Definition choose (previous : option bool) (urgent : bool) (inputs : list tx)
    : list tx :=
  match previous, urgent with
  | Some false, false => inputs
  | _, _ => select inputs
  end.

Theorem next_turn_excludes : forall inputs urgent,
  existsb isolated (choose (Some true) urgent inputs) = false.
Proof.
  intros inputs urgent. destruct urgent; apply no_second_isolation.
Qed.

Theorem unknown_turn_excludes : forall inputs urgent,
  existsb isolated (choose None urgent inputs) = false.
Proof.
  intros inputs urgent. destruct urgent; apply no_second_isolation.
Qed.

Theorem urgent_turn_excludes : forall inputs previous,
  existsb isolated (choose previous true inputs) = false.
Proof.
  intros inputs [previous|]; [destruct previous|]; apply no_second_isolation.
Qed.

Theorem ordinary_turn_unchanged : forall inputs,
  choose (Some false) false inputs = inputs.
Proof. reflexivity. Qed.

Theorem turn_input : forall inputs previous urgent item,
  In item (choose previous urgent inputs) -> In item inputs.
Proof.
  intros inputs [previous|] urgent item; destruct urgent;
    try destruct previous; simpl; intros present;
    try exact present; eapply selected_input; exact present.
Qed.

Definition mixed (ordered exclusive : bool) (previous : option bool)
    (urgent : bool) (inputs : list tx) : list tx :=
  if ordered && negb exclusive && negb urgent then inputs
  else choose previous urgent inputs.

Theorem mixed_urgent : forall inputs ordered exclusive previous,
  existsb isolated (mixed ordered exclusive previous true inputs) = false.
Proof.
  intros. unfold mixed. rewrite andb_false_r. apply urgent_turn_excludes.
Qed.

Theorem mixed_exclusive : forall inputs previous urgent,
  mixed true true previous urgent inputs = choose previous urgent inputs.
Proof. reflexivity. Qed.

Theorem mixed_regular : forall inputs previous,
  mixed true false previous false inputs = inputs.
Proof. reflexivity. Qed.

Theorem mixed_prior : forall inputs exclusive previous urgent,
  mixed false exclusive previous urgent inputs = choose previous urgent inputs.
Proof. reflexivity. Qed.

Theorem mixed_input : forall inputs ordered exclusive previous urgent item,
  In item (mixed ordered exclusive previous urgent inputs) -> In item inputs.
Proof.
  intros. unfold mixed in H.
  destruct (ordered && negb exclusive && negb urgent); [exact H|].
  eapply turn_input. exact H.
Qed.
End Refill.

Module Nonces.
Fixpoint first (who : nat) (excluded : list tx) : option nat :=
  match excluded with
  | [] => None
  | item :: rest =>
    let prior := first who rest in
    if Nat.eqb (sender item) who then
      Some (match prior with None => nonce item | Some n => Nat.min (nonce item) n end)
    else prior
  end.

Definition keep (excluded : list tx) (item : tx) : bool :=
  match first (sender item) excluded with
  | None => true
  | Some n => nonce item <? n
  end.

Definition select (excluded inputs : list tx) := filter (keep excluded) inputs.

Lemma keep_step : forall excluded cut item,
  keep (cut :: excluded) item =
  (negb (Nat.eqb (sender cut) (sender item)) || (nonce item <? nonce cut))
    && keep excluded item.
Proof.
  intros excluded cut item. unfold keep. simpl.
  destruct (Nat.eqb (sender cut) (sender item)); simpl;
    destruct (first (sender item) excluded) as [n|]; simpl;
    try rewrite andb_true_r; try reflexivity.
  apply eq_true_iff_eq. rewrite andb_true_iff, !Nat.ltb_lt.
  destruct (Nat.min_spec (nonce cut) n) as [[_ equal]|[_ equal]]; rewrite equal; lia.
Qed.

Theorem kept_exact : forall excluded item,
  keep excluded item = true <->
  forall cut, In cut excluded -> sender cut = sender item -> nonce item < nonce cut.
Proof.
  induction excluded as [|cut rest ih]; intros item.
  - simpl. split; [tauto|reflexivity].
  - rewrite keep_step, andb_true_iff, ih.
    destruct (Nat.eqb (sender cut) (sender item)) eqn:same; simpl.
    + apply Nat.eqb_eq in same. rewrite Nat.ltb_lt.
      split.
      * intros [earlier remaining] other [equal|present] address;
          [subst other; exact earlier|now apply remaining].
      * intros valid. split.
        -- apply valid; [left; reflexivity|exact same].
        -- intros other present address. apply valid; [right; exact present|exact address].
    + apply Nat.eqb_neq in same. split.
      * intros [_ remaining] other [equal|present] address;
          [subst other; contradiction|now apply remaining].
      * intros valid. split; [reflexivity|].
        intros other present address. apply valid; [right; exact present|exact address].
Qed.

Theorem selected_exact : forall excluded inputs item,
  In item (select excluded inputs) <-> In item inputs /\
  forall cut, In cut excluded -> sender cut = sender item -> nonce item < nonce cut.
Proof. intros. unfold select. rewrite filter_In, kept_exact. reflexivity. Qed.

Theorem selection_repeat : forall excluded inputs,
  select excluded (select excluded inputs) = select excluded inputs.
Proof.
  intros excluded inputs. unfold select. induction inputs as [|item rest ih]; simpl;
    [reflexivity|].
  destruct (keep excluded item) eqn:kept; simpl; rewrite ?kept, ?ih; reflexivity.
Qed.

Theorem exclusion_order : forall left right inputs,
  (forall item, In item left <-> In item right) ->
  select left inputs = select right inputs.
Proof.
  intros left right inputs same. unfold select. apply filter_ext. intros item.
  apply eq_true_iff_eq. rewrite !kept_exact.
  split; intros valid cut present address; apply valid; try assumption;
    apply same; assumption.
Qed.

Theorem rejected_removed : forall excluded inputs item,
  In item excluded -> ~ In item (select excluded inputs).
Proof.
  intros excluded inputs item removed present.
  apply selected_exact in present. destruct present as [_ earlier].
  specialize (earlier item removed eq_refl). lia.
Qed.

Theorem last_pass_simple : forall inputs item,
  In item (select (filter isolated inputs) inputs) -> isolated item = false.
Proof.
  intros inputs item kept.
  pose proof (proj1 (selected_exact (filter isolated inputs) inputs item) kept)
    as [present earlier].
  destruct (isolated item) eqn:heavy; [|reflexivity].
  exfalso. eapply (rejected_removed (filter isolated inputs) inputs item).
  - apply filter_In. auto.
  - exact kept.
Qed.

Theorem retry_decreases : forall excluded inputs item,
  In item excluded -> In item inputs ->
  length (select excluded inputs) < length inputs.
Proof.
  intros excluded inputs item removed.
  induction inputs as [|head rest ih]; intros present; [contradiction|].
  unfold select in *. simpl in *.
  destruct present as [equal|present].
  - subst head. assert (keep excluded item = false) as no.
    { destruct (keep excluded item) eqn:kept; [|reflexivity].
      pose proof (proj1 (kept_exact excluded item) kept) as earlier.
      specialize (earlier item removed eq_refl). lia. }
    rewrite no. pose proof (filter_length_le (keep excluded) rest). lia.
  - specialize (ih present). destruct (keep excluded head); simpl; lia.
Qed.
End Nonces.

Module Work.
Inductive trace (limit : nat) : nat -> nat -> nat -> Prop :=
  | empty : trace limit 1 4 0
  | execute : forall attempts credits spent cost,
      trace limit attempts (S credits) spent -> cost <= limit ->
      trace limit attempts credits (spent + cost)
  | rebuild : forall credits spent,
      trace limit 1 credits spent -> 2 <= credits ->
      trace limit 0 credits spent.

Theorem finite_work : forall limit attempts credits spent,
  trace limit attempts credits spent ->
  credits <= 4 /\ spent <= (4 - credits) * limit /\ attempts <= 1.
Proof.
  intros limit attempts credits spent path.
  induction path.
  - lia.
  - destruct IHpath as [remaining [used retries]].
    split; [lia|]. split; [|exact retries].
    assert (4 - credits = S (4 - S credits)) as count by lia.
    rewrite count. rewrite Nat.mul_succ_l. lia.
  - destruct IHpath as [remaining [used retries]]. auto.
Qed.
End Work.

Module Reserve.
Inductive trace (limit : nat) : nat -> nat -> nat -> Prop :=
  | empty : trace limit 4 2 0
  | execute : forall credits slots spent cost,
      trace limit (S credits) slots spent -> cost <= limit ->
      trace limit credits slots (spent + cost)
  | reserve : forall slots spent cost,
      trace limit 0 (S slots) spent -> cost <= limit ->
      trace limit 0 slots (spent + cost).

Theorem finite_work : forall limit credits slots spent,
  trace limit credits slots spent ->
  credits <= 4 /\ slots <= 2 /\
  spent <= (4 - credits) * limit + (2 - slots) * limit.
Proof.
  intros limit credits slots spent path.
  induction path.
  - lia.
  - destruct IHpath as [remaining [reserved used]].
    split; [lia|]. split; [exact reserved|].
    assert (4 - credits = S (4 - S credits)) as count by lia.
    rewrite count, Nat.mul_succ_l. lia.
  - destruct IHpath as [remaining [reserved used]].
    split; [lia|]. split; [lia|].
    assert (2 - slots = S (2 - S slots)) as count by lia.
    rewrite count, Nat.mul_succ_l. lia.
Qed.

Definition select (removed inputs : list tx) := Nonces.select removed inputs.

Theorem independent_kept : forall removed inputs item,
  In item inputs ->
  (forall refused, In refused removed -> sender refused <> sender item) ->
  In item (select removed inputs).
Proof.
  intros removed inputs item present independent.
  unfold select. apply Nonces.selected_exact. split; [exact present|].
  intros refused included equal. exfalso. apply (independent refused included). exact equal.
Qed.

Theorem successors_removed : forall removed inputs cut item,
  In cut removed -> sender cut = sender item -> nonce cut <= nonce item ->
  ~ In item (select removed inputs).
Proof.
  intros removed inputs cut item included same later kept.
  apply Nonces.selected_exact in kept. destruct kept as [_ earlier].
  specialize (earlier cut included same). lia.
Qed.
End Reserve.

Module Ordered.
Section Execution.
Context {State Item Stamp : Type}.
Variable step : State -> Item -> State.
Variable circle : Item -> bool.
Variable view : State -> Stamp.
Variable same : forall left right : Stamp, {left = right} + {left <> right}.

Fixpoint prepare (state : State) (inputs : list Item) : State * list Stamp :=
  match inputs with
  | [] => (state, [])
  | item :: rest =>
    let '(final, receipts) := prepare (step state item) rest in
    (final, if circle item then view state :: receipts else receipts)
  end.

Fixpoint replay (state : State) (inputs : list Item) (receipts : list Stamp)
    : option State :=
  match inputs with
  | [] => match receipts with [] => Some state | _ => None end
  | item :: rest =>
    if circle item then
      match receipts with
      | [] => None
      | receipt :: more =>
        if same (view state) receipt then replay (step state item) rest more
        else None
      end
    else replay (step state item) rest receipts
  end.

Theorem prepared_replays : forall inputs state,
  replay state inputs (snd (prepare state inputs)) = Some (fst (prepare state inputs)).
Proof.
  induction inputs as [|item rest ih]; intros state; simpl; [reflexivity|].
  specialize (ih (step state item)).
  destruct (prepare (step state item) rest) as [final receipts] eqn:computed.
  simpl in *. destruct (circle item); simpl; [|exact ih].
  destruct (same (view state) (view state)); [exact ih|contradiction].
Qed.

Theorem replay_exact : forall inputs state receipts final,
  replay state inputs receipts = Some final -> prepare state inputs = (final, receipts).
Proof.
  induction inputs as [|item rest ih]; intros state receipts final accepted; simpl in *.
  - destruct receipts; inversion accepted; reflexivity.
  - destruct (circle item) eqn:heavy.
    + destruct receipts as [|receipt more]; [discriminate|].
      destruct (same (view state) receipt) as [equal|different]; [|discriminate].
      specialize (ih (step state item) more final accepted).
      rewrite ih. subst receipt. reflexivity.
    + specialize (ih (step state item) receipts final accepted).
      rewrite ih. reflexivity.
Qed.

Theorem prepare_append : forall left right state,
  prepare state (left ++ right) =
  let '(middle, first) := prepare state left in
  let '(final, second) := prepare middle right in (final, first ++ second).
Proof.
  induction left as [|item rest ih]; intros right state; simpl; [|rewrite ih].
  - destruct (prepare state right); reflexivity.
  - destruct (prepare (step state item) rest) as [middle first].
    destruct (prepare middle right) as [final second].
    destruct (circle item); reflexivity.
Qed.

Theorem replay_split : forall left right state receipts final,
  replay state (left ++ right) receipts = Some final <->
  exists middle first second,
    receipts = first ++ second /\
    replay state left first = Some middle /\
    replay middle right second = Some final.
Proof.
  intros left right state receipts final. split.
  - intros accepted. apply replay_exact in accepted.
    rewrite prepare_append in accepted.
    pose proof (prepared_replays left state) as first_ok.
    destruct (prepare state left) as [middle first]. simpl in first_ok.
    pose proof (prepared_replays right middle) as second_ok.
    destruct (prepare middle right) as [last second]. simpl in second_ok.
    inversion accepted. subst. exists middle, first, second. auto.
  - intros [middle [first [second [joined [first_ok second_ok]]]]].
    apply replay_exact in first_ok. apply replay_exact in second_ok.
    pose proof (prepared_replays (left ++ right) state) as accepted.
    rewrite prepare_append, first_ok, second_ok in accepted. simpl in accepted.
    now subst receipts.
Qed.

Theorem receipt_count : forall inputs state,
  length (snd (prepare state inputs)) = length (filter circle inputs).
Proof.
  induction inputs as [|item rest ih]; intros state; simpl; [reflexivity|].
  specialize (ih (step state item)).
  destruct (prepare (step state item) rest) as [final receipts].
  simpl in *. destruct (circle item); simpl; congruence.
Qed.

Theorem changed_receipts_refused : forall inputs state receipts,
  receipts <> snd (prepare state inputs) -> replay state inputs receipts = None.
Proof.
  intros inputs state receipts changed.
  destruct (replay state inputs receipts) as [final|] eqn:checked; [|reflexivity].
  apply replay_exact in checked. rewrite checked in changed. simpl in changed.
  contradiction.
Qed.
End Execution.

Definition add (state : nat) (item : bool * nat) : nat := state + snd item.
Definition trace := prepare add (@fst bool nat) (fun state : nat => state).
Definition check := replay add (@fst bool nat) (fun state : nat => state) Nat.eq_dec.

Example ordinary_write_visible : trace 0 [(false, 3); (true, 4); (true, 5)] = (12, [3; 7]).
Proof. reflexivity. Qed.

Example initial_state_refused : check 0 [(false, 3); (true, 4); (true, 5)] [0; 0] = None.
Proof. reflexivity. Qed.

Example first_state_refused : check 0 [(false, 3); (true, 4); (true, 5)] [3; 3] = None.
Proof. reflexivity. Qed.
End Ordered.