(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia List.
Require Import object_quorum.
Open Scope Z_scope.

Record point := Point { at_round : Z; value : nat }.
Record state := State {
  height : Z;
  round : Z;
  locked : option point;
  valid : option point;
  polc : Z -> nat -> bool
}.

Definition floor_ok s :=
  forall lock, locked s = Some lock ->
  exists known, valid s = Some known /\ at_round lock <= at_round known.

Definition choose_valid lock known :=
  match lock, known with
  | None, _ => known
  | Some p, None => Some p
  | Some p, Some q => if at_round q <? at_round p then Some p else Some q
  end.

Definition reconfigure s known projected :=
  State (height s) (round s) (locked s)
    (choose_valid (locked s) known) projected.

Theorem reconfigure_floor s known projected :
  floor_ok (reconfigure s known projected).
Proof.
  intros lock same. simpl in same. unfold reconfigure, choose_valid; simpl.
  rewrite same. destruct known as [q|].
  - destruct (at_round q <? at_round lock) eqn:order.
    + exists lock. split; [reflexivity|lia].
    + exists q. split; [reflexivity|]. apply Z.ltb_ge in order. lia.
  - exists lock. split; [reflexivity|lia].
Qed.

Definition restart s next_height next_round :=
  if height s =? next_height then
    State next_height (Z.max (round s) next_round)
      (locked s) (valid s) (polc s)
  else State next_height 0 None None (fun _ _ => false).

Theorem restart_floor s next_height next_round :
  floor_ok s -> floor_ok (restart s next_height next_round).
Proof.
  intros before lock same. unfold restart in *.
  destruct (height s =? next_height); simpl in *.
  - apply before. exact same.
  - discriminate.
Qed.

Theorem restart_same_lock s next_round :
  locked (restart s (height s) next_round) = locked s.
Proof. unfold restart. rewrite Z.eqb_refl. reflexivity. Qed.

Theorem restart_same_polc s next_round vr pid :
  polc (restart s (height s) next_round) vr pid = polc s vr pid.
Proof. unfold restart. rewrite Z.eqb_refl. reflexivity. Qed.

Theorem restart_next_polc s next_height next_round vr pid :
  height s <> next_height ->
  polc (restart s next_height next_round) vr pid = false.
Proof.
  intros different. unfold restart.
  apply Z.eqb_neq in different. rewrite different. reflexivity.
Qed.

Definition permits s pid claim :=
  match locked s with
  | None => true
  | Some lock =>
    Nat.eqb pid (value lock) ||
      match claim with
      | None => false
      | Some vr => (at_round lock <? vr) && (vr <? round s) && polc s vr pid
      end
  end.

Definition prevote s proposal claim :=
  match proposal with
  | None => None
  | Some pid => if permits s pid claim then Some pid else None
  end.

Theorem prevote_lock s proposal claim pid lock :
  locked s = Some lock ->
  prevote s proposal claim = Some pid ->
  pid = value lock \/ exists vr, claim = Some vr /\
    at_round lock < vr /\ vr < round s /\ polc s vr pid = true.
Proof.
  intros same sent. unfold prevote in sent.
  destruct proposal as [p|]; [|discriminate].
  destruct (permits s p claim) eqn:allowed; [|discriminate].
  inversion sent; subst p. unfold permits in allowed. rewrite same in allowed.
  apply orb_true_iff in allowed as [equal|proof].
  - left. apply Nat.eqb_eq. exact equal.
  - right. destruct claim as [vr|]; [|discriminate]. exists vr.
    repeat rewrite andb_true_iff in proof.
    destruct proof as [[after before] checked].
    apply Z.ltb_lt in after, before. repeat split; auto.
Qed.

Theorem different_without_polc s pid lock claim :
  locked s = Some lock -> pid <> value lock ->
  (forall vr, polc s vr pid = false) ->
  prevote s (Some pid) claim = None.
Proof.
  intros same different no_polc. unfold prevote, permits. rewrite same.
  apply Nat.eqb_neq in different. rewrite different; simpl.
  destruct claim as [vr|]; [rewrite no_polc, andb_false_r|]; reflexivity.
Qed.

Definition finalize s p :=
  let r := match locked s with
    | None => at_round p
    | Some lock => Z.max (at_round lock) (at_round p)
    end in
  State (height s) (round s) (Some (Point r (value p)))
    (Some (Point r (value p))) (polc s).

Theorem finalize_floor s p : floor_ok (finalize s p).
Proof.
  intros lock same. unfold finalize in *; simpl in *.
  inversion same; subst. eexists. split; [reflexivity|lia].
Qed.

Definition relief_check n q target_q faults by_count total qw target_qw :=
  (faults <? q + target_q - n) &&
  (total - qw <? Z.max by_count (qw + target_qw - total)).

Theorem relief_count n q target_q faults by_count total qw target_qw :
  relief_check n q target_q faults by_count total qw target_qw = true ->
  faults < q + target_q - n.
Proof.
  unfold relief_check. rewrite andb_true_iff. intros [yes _].
  apply Z.ltb_lt. exact yes.
Qed.

Theorem relief_weight weight left right ids n q target_q faults by_count qw target_qw :
  (forall id, In id ids -> 0 <= weight id) ->
  qw <= Stake.score weight left ids ->
  target_qw <= Stake.score weight right ids ->
  by_count <= Stake.score weight (fun id => left id && right id) ids ->
  relief_check n q target_q faults by_count
    (Stake.score weight (fun _ => true) ids) qw target_qw = true ->
  Stake.score weight (fun _ => true) ids - qw <
    Stake.score weight (fun id => left id && right id) ids.
Proof.
  intros positive first second count checked.
  unfold relief_check in checked. apply andb_true_iff in checked as [_ checked].
  apply Z.ltb_lt in checked.
  pose proof (Stake.score_join weight left right ids) as joined.
  pose proof (Stake.score_cap weight (fun id => left id || right id) ids positive) as cap.
  lia.
Qed.

Theorem conflicting_votes s lock proposal claim :
  locked s = Some lock ->
  (forall vr, at_round lock < vr -> vr < round s -> polc s vr proposal = false) ->
  prevote s (Some proposal) claim = Some proposal ->
  proposal = value lock.
Proof.
  intros same no_polc sent.
  destruct (prevote_lock s (Some proposal) claim proposal lock same sent)
    as [equal|[vr [_ [after [before yes]]]]]; [exact equal|].
  rewrite (no_polc vr after before) in yes. discriminate.
Qed.

Lemma score_subset weight left right ids :
  (forall id, In id ids -> 0 <= weight id) ->
  (forall id, In id ids -> left id = true -> right id = true) ->
  Stake.score weight left ids <= Stake.score weight right ids.
Proof.
  induction ids as [|id rest ih]; intros positive subset; simpl; [lia|].
  assert (0 <= weight id) by (apply positive; simpl; auto).
  assert (Stake.score weight left rest <= Stake.score weight right rest).
  { apply ih; intros other present; [apply positive|apply subset]; simpl; auto. }
  specialize (subset id (or_introl eq_refl)).
  destruct (left id), (right id); simpl in *; lia.
Qed.

Lemma honest_overlap weight left right bad ids :
  (forall id, In id ids -> 0 <= weight id) ->
  Stake.score weight bad ids < Stake.score weight (fun id => left id && right id) ids ->
  exists id, In id ids /\ left id = true /\ right id = true /\ bad id = false.
Proof.
  intros positive more.
  destruct (existsb (fun id => left id && right id && negb (bad id)) ids) eqn:found.
  - apply existsb_exists in found as [id [member yes]].
    repeat rewrite andb_true_iff in yes. destruct yes as [[l r] good].
    apply negb_true_iff in good. exists id. auto.
  - assert (Stake.score weight (fun id => left id && right id) ids <= Stake.score weight bad ids).
    { apply score_subset; [exact positive|]. intros id member both.
      destruct (bad id) eqn:bad_id; [reflexivity|].
      assert (existsb (fun id => left id && right id && negb (bad id)) ids = true) as yes.
      { apply existsb_exists. exists id. split; [exact member|]. rewrite both, bad_id. reflexivity. }
      rewrite yes in found. discriminate. }
    lia.
Qed.

Theorem relief_honest_overlap weight left right bad ids n q target_q faults by_count qw target_qw :
  (forall id, In id ids -> 0 <= weight id) ->
  qw <= Stake.score weight left ids ->
  target_qw <= Stake.score weight right ids ->
  by_count <= Stake.score weight (fun id => left id && right id) ids ->
  Stake.score weight bad ids <= Stake.score weight (fun _ => true) ids - qw ->
  relief_check n q target_q faults by_count
    (Stake.score weight (fun _ => true) ids) qw target_qw = true ->
  exists id, In id ids /\ left id = true /\ right id = true /\ bad id = false.
Proof.
  intros positive first second count faults_ok checked.
  pose proof (relief_weight weight left right ids n q target_q faults by_count qw target_qw
    positive first second count checked) as overlap.
  apply (honest_overlap weight left right bad ids); [exact positive|lia].
Qed.

Theorem relief_single_value weight left right bad ids n q target_q faults by_count qw target_qw
    (signed : nat -> nat -> Prop) p s :
  (forall id, In id ids -> 0 <= weight id) ->
  qw <= Stake.score weight left ids -> target_qw <= Stake.score weight right ids ->
  by_count <= Stake.score weight (fun id => left id && right id) ids ->
  Stake.score weight bad ids <= Stake.score weight (fun _ => true) ids - qw ->
  relief_check n q target_q faults by_count
    (Stake.score weight (fun _ => true) ids) qw target_qw = true ->
  (forall id, In id ids -> left id = true -> signed id p) ->
  (forall id, In id ids -> right id = true -> signed id s) ->
  (forall id, bad id = false -> signed id p -> signed id s -> p = s) -> p = s.
Proof.
  intros positive first second count faults_ok checked left_signed right_signed single.
  destruct (relief_honest_overlap weight left right bad ids n q target_q faults by_count qw target_qw
    positive first second count faults_ok checked) as [id [member [l [r good]]]].
  eapply single; eauto.
Qed.

Print Assumptions reconfigure_floor.
Print Assumptions restart_floor.
Print Assumptions restart_same_polc.
Print Assumptions prevote_lock.
Print Assumptions finalize_floor.
Print Assumptions relief_weight.
Print Assumptions conflicting_votes.
Print Assumptions relief_honest_overlap.
Print Assumptions relief_single_value.

Module Durable_polc.

Definition same p q := (at_round p =? at_round q) && Nat.eqb (value p) (value q).

Lemma same_exact p q : same p q = true <-> p = q.
Proof.
  destruct p, q. unfold same; simpl. rewrite andb_true_iff, Z.eqb_eq, Nat.eqb_eq.
  split; intros eq; [destruct eq; subst; reflexivity|inversion eq; auto].
Qed.

Definition consistent p prior := forallb (fun q =>
  negb (at_round p =? at_round q) || Nat.eqb (value p) (value q)) prior.

Definition keep lock p prior :=
  let all := p :: filter (fun q => negb (at_round q =? at_round p)) prior in
  let newest := fold_right (fun q r => Z.max (at_round q) r) (at_round p) all in
  filter (fun q => (at_round q =? newest) ||
    (match lock with None => false | Some v => at_round q =? at_round v end) ||
    (at_round q =? at_round p)) all.

Lemma keep_new lock p prior : In p (keep lock p prior).
Proof.
  unfold keep. apply filter_In. split; [left; reflexivity|].
  rewrite Z.eqb_refl, orb_true_r. reflexivity.
Qed.

Lemma keep_checked checked lock p prior :
  checked p = true -> (forall q, In q prior -> checked q = true) ->
  forall q, In q (keep lock p prior) -> checked q = true.
Proof.
  intros fresh old q member. unfold keep in member.
  apply filter_In in member as [member _]. destruct member as [eq|member].
  - subst. exact fresh.
  - apply filter_In in member as [member _]. auto.
Qed.

Lemma keep_lock p prior q :
  consistent p prior = true -> In q prior -> In q (keep (Some q) p prior).
Proof.
  intros valid member. unfold keep. apply filter_In. split.
  - destruct (Z.eq_dec (at_round q) (at_round p)) as [eq|different].
    + left. unfold consistent in valid. rewrite forallb_forall in valid.
      specialize (valid q member). assert (at_round p =? at_round q = true) as same_r
        by (apply Z.eqb_eq; lia).
      rewrite same_r in valid; simpl in valid. apply Nat.eqb_eq in valid.
      destruct p, q; simpl in *; subst; reflexivity.
    + right. apply filter_In. split; [exact member|].
      apply negb_true_iff. apply Z.eqb_neq. exact different.
  - rewrite Z.eqb_refl, orb_true_r. reflexivity.
Qed.

Record disk := Disk { proofs : list point; vote : option point }.
Inductive event := Save (p : point) | Send (p : point) | Crash | Write_error.

Definition sound (checked : point -> bool) d :=
  (forall p, In p (proofs d) -> checked p = true) /\
  (forall q, vote d = Some q -> In q (proofs d)).

Definition step (checked : point -> bool) d input : disk * bool :=
  match input with
  | Crash | Write_error => (d, false)
  | Save p => if checked p && consistent p (proofs d)
    then (Disk (keep (vote d) p (proofs d)) (vote d), false) else (d, false)
  | Send q => if existsb (same q) (proofs d)
    then (Disk (proofs d) (Some q), true) else (d, false)
  end.

Theorem step_sound checked d input :
  sound checked d -> sound checked (fst (step checked d input)).
Proof.
  intros [valid backed]. destruct d as [prior lock]; destruct input as [p|q| |];
    simpl in *; try (split; assumption).
  - destruct (checked p && consistent p prior) eqn:accepted; [|split; assumption].
    apply andb_true_iff in accepted as [fresh same_round]. simpl. split.
    + apply keep_checked; assumption.
    + intros q eq. simpl in eq. rewrite eq. apply keep_lock; auto.
  - destruct (existsb (same q) prior) eqn:present; [|split; assumption].
    apply existsb_exists in present as [p [member eq]]. apply same_exact in eq. subst.
    simpl. split; [exact valid|]. intros got eq. inversion eq; subst; exact member.
Qed.

Fixpoint run checked d events :=
  match events with nil => d | e :: rest => run checked (fst (step checked d e)) rest end.

Theorem crash_keeps_proof checked events q :
  vote (run checked (Disk nil None) events) = Some q ->
  In q (proofs (fst (step checked (run checked (Disk nil None) events) Crash)))
    /\ checked q = true.
Proof.
  assert (forall d, sound checked d -> sound checked (run checked d events)) as preserve.
  { induction events as [|e rest ih]; intros d valid; simpl; auto.
    apply ih. apply step_sound. exact valid. }
  assert (sound checked (Disk nil None)) as empty.
  { split; simpl; intros p no; [contradiction|discriminate]. }
  destruct (preserve _ empty) as [valid backed]. intros sent.
  pose proof (backed q sent) as member. simpl. split; auto.
Qed.

Theorem no_proof_no_send checked lock q :
  snd (step checked (Disk nil lock) (Send q)) = false.
Proof. reflexivity. Qed.

Print Assumptions keep_lock.
Print Assumptions keep_new.
Print Assumptions step_sound.
Print Assumptions crash_keeps_proof.
Print Assumptions no_proof_no_send.
End Durable_polc.

Definition author_role enabled epoch saved (voters prior : nat -> bool) id :=
  voters id || (enabled && Z.eqb epoch saved && prior id).

Theorem author_only enabled epoch saved voters prior id :
  voters id = false -> author_role enabled epoch saved voters prior id = true ->
  enabled = true /\ epoch = saved /\ prior id = true.
Proof.
  intros absent accepted. unfold author_role in accepted. rewrite absent in accepted.
  simpl in accepted. repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[active same] member]. apply Z.eqb_eq in same. auto.
Qed.

Theorem author_expiry enabled epoch saved voters prior id :
  epoch <> saved -> author_role enabled epoch saved voters prior id = voters id.
Proof.
  intros different. unfold author_role. apply Z.eqb_neq in different.
  rewrite different, andb_false_r. simpl. apply orb_false_r.
Qed.

Theorem locked_reproposal s lock known projected claim :
  locked s = Some lock ->
  prevote (reconfigure s known projected) (Some (value lock)) claim = Some (value lock).
Proof.
  intros same. unfold prevote, permits, reconfigure; simpl. rewrite same.
  rewrite Nat.eqb_refl. reflexivity.
Qed.

Section QuorumTrace.
  Variable voter : Type.
  Variable honest : voter -> Prop.
  Variable quorum : (voter -> Prop) -> Prop.
  Variable voted committed : voter -> nat -> nat -> Prop.

  Definition polc_at r p :=
    exists members, quorum members /\ forall id, members id -> voted id r p.
  Definition final_at r p :=
    exists members, quorum members /\ forall id, members id -> committed id r p.

  Hypothesis intersect : forall left right,
    quorum left -> quorum right -> exists id, left id /\ right id /\ honest id.
  Hypothesis single_vote : forall id r p q,
    honest id -> voted id r p -> voted id r q -> p = q.
  Hypothesis commit_polc : forall id r p,
    honest id -> committed id r p -> polc_at r p.
  Hypothesis lock_trace : forall id r p s q,
    honest id -> committed id r p -> (r < s)%nat -> voted id s q -> p <> q ->
    exists prior value, (r < prior < s)%nat /\ value <> p /\ polc_at prior value.

  Theorem polc_after_final r p s q :
    final_at r p -> (r <= s)%nat -> polc_at s q -> q = p.
  Proof.
    intros final. revert q.
    induction s as [s induction] using lt_wf_ind. intros q after certificate.
    destruct final as [locked_set [locked_quorum locks]].
    destruct certificate as [vote_set [vote_quorum votes]].
    destruct (intersect locked_set vote_set locked_quorum vote_quorum)
      as [id [locked_member [vote_member good]]].
    pose proof (locks id locked_member) as locked_vote.
    pose proof (votes id vote_member) as current_vote.
    destruct (Nat.eq_dec r s) as [same|later].
    - subst s.
      destruct (commit_polc id r p good locked_vote) as [prior [enough proof]].
      destruct (intersect prior vote_set enough vote_quorum)
        as [signer [in_prior [in_votes honest_signer]]].
      symmetry. eapply single_vote; eauto.
    - destruct (Nat.eq_dec p q) as [same|different]; [symmetry; exact same|].
      assert (strict : (r < s)%nat) by lia.
      destruct (lock_trace id r p s q good locked_vote strict current_vote different)
        as [prior [value [[low high] [changed certificate]]]].
      assert (same : value = p).
      { apply (induction prior high value); [lia|exact certificate]. }
      contradiction.
  Qed.

  Theorem unique_final r p s q : final_at r p -> final_at s q -> p = q.
  Proof.
    intros first second.
    assert (to_polc : forall round pid, final_at round pid -> polc_at round pid).
    { intros round pid [members [enough signatures]].
      destruct (intersect members members enough enough) as [id [member [_ good]]].
      eapply commit_polc; eauto. }
    destruct (Nat.le_ge_cases r s) as [forward|backward].
    - symmetry. eapply polc_after_final; eauto.
    - eapply polc_after_final; eauto.
  Qed.
End QuorumTrace.

Print Assumptions author_only.
Print Assumptions locked_reproposal.
Print Assumptions unique_final.