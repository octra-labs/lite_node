(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Lia List Bool.
Require Import quorum_measure.
Import ListNotations.
Open Scope Z_scope.

Ltac Zify.zify_post_hook ::= Z.to_euclidean_division_equations.

Definition mask := nat -> bool.

Fixpoint score (value : seat -> Z) (yes : mask) (weights : list seat) : Z :=
  match weights with
  | [] => 0
  | entry :: rest => (if yes (fst entry) then value entry else 0) + score value yes rest
  end.

Definition unit_value (entry : seat) : Z := 1.
Definition weigh := score snd.
Definition count := score unit_value.
Definition all : mask := fun _ => true.
Definition both (left right : mask) : mask := fun id => left id && right id.
Definition either (left right : mask) : mask := fun id => left id || right id.
Definition outside (yes : mask) : mask := fun id => negb (yes id).
Definition below (k : nat) : mask := fun id => Nat.ltb id k.
Definition only (k : nat) : mask := fun id => Nat.eqb id k.
Definition pair (a b : nat) : mask := either (only a) (only b).

Definition positive (weights : list seat) :=
  forall entry, In entry weights -> 0 < snd entry.

Definition flat (unit : Z) (weights : list seat) :=
  forall entry, In entry weights -> snd entry = unit.

Definition form_of (weights : list seat) : shape :=
  shape_of (Z.of_nat (length weights)) (sum weights).

Definition priced (unit : Z) (weights : list seat) :=
  forall entry, In entry weights -> unit <= snd entry.

Definition accepts (chosen : rule) (stated : bool) (a : ask) (yes : mask)
    (weights : list seat) : bool :=
  meets (ruled chosen stated weights) a (count yes weights) (weigh yes weights).

Definition attests (chosen : rule) (stated : bool) (required : Z) (yes : mask)
    (weights : list seat) : bool :=
  attested (ruled chosen stated weights) required (count yes weights) (weigh yes weights).

Definition apart (yes : mask) (own extra : list seat) : bool :=
  forallb (fun entry => yes (fst entry)) own
  && forallb (fun entry => negb (yes (fst entry))) extra.

Definition mint (start k : nat) (unit : Z) : list seat :=
  map (fun id => (id, unit)) (seq start k).

Lemma score_join value left right weights :
  score value left weights + score value right weights =
  score value (both left right) weights + score value (either left right) weights.
Proof.
  induction weights as [|entry rest ih]; simpl; [lia|].
  unfold both, either in *. destruct (left (fst entry)), (right (fst entry)); simpl; lia.
Qed.

Lemma score_split value yes weights :
  score value yes weights + score value (outside yes) weights = score value all weights.
Proof.
  induction weights as [|entry rest ih]; simpl; [lia|].
  unfold outside, all in *. destruct (yes (fst entry)); simpl; lia.
Qed.

Lemma score_order value left right weights :
  (forall entry, In entry weights -> 0 <= value entry) ->
  (forall entry, In entry weights -> left (fst entry) = true -> right (fst entry) = true) ->
  score value left weights <= score value right weights.
Proof.
  induction weights as [|entry rest ih]; intros sign included; simpl; [lia|].
  assert (0 <= value entry) by (apply sign; simpl; auto).
  assert (score value left rest <= score value right rest).
  { apply ih; intros other present; [apply sign|apply included]; simpl; auto. }
  specialize (included entry (or_introl eq_refl)).
  destruct (left (fst entry)), (right (fst entry)); simpl in *; try lia.
  all: specialize (included eq_refl); discriminate.
Qed.

Lemma score_cap value yes weights :
  (forall entry, In entry weights -> 0 <= value entry) ->
  score value yes weights <= score value all weights.
Proof. intros sign. apply score_order; [exact sign|]. reflexivity. Qed.

Lemma score_sign value yes weights :
  (forall entry, In entry weights -> 0 <= value entry) ->
  0 <= score value yes weights.
Proof.
  induction weights as [|entry rest ih]; intros sign; simpl; [lia|].
  assert (0 <= value entry) by (apply sign; simpl; auto).
  assert (0 <= score value yes rest).
  { apply ih. intros other present. apply sign. simpl. auto. }
  destruct (yes (fst entry)); lia.
Qed.

Lemma score_app value yes left right :
  score value yes (left ++ right) = score value yes left + score value yes right.
Proof. induction left as [|entry rest ih]; simpl; lia. Qed.

Lemma score_kept value yes weights :
  forallb (fun entry => yes (fst entry)) weights = true ->
  score value yes weights = score value all weights.
Proof.
  induction weights as [|entry rest ih]; intros kept; simpl in *; [reflexivity|].
  apply andb_true_iff in kept as [here later]. rewrite here, (ih later). reflexivity.
Qed.

Lemma score_none value yes weights :
  forallb (fun entry => negb (yes (fst entry))) weights = true ->
  score value yes weights = 0.
Proof.
  induction weights as [|entry rest ih]; intros none; simpl in *; [reflexivity|].
  apply andb_true_iff in none as [here later]. apply negb_true_iff in here.
  rewrite here, (ih later). reflexivity.
Qed.

Lemma member value bad yes weights :
  (forall entry, In entry weights -> 0 <= value entry) ->
  score value bad weights < score value yes weights ->
  exists entry, In entry weights /\ yes (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign more.
  destruct (existsb (fun entry => yes (fst entry) && negb (bad (fst entry))) weights) eqn:found.
  - apply existsb_exists in found as [entry [present chosen]].
    apply andb_true_iff in chosen as [signed good]. apply negb_true_iff in good.
    exists entry. auto.
  - assert (score value yes weights <= score value bad weights); [|lia].
    apply score_order; [exact sign|].
    intros entry present signed.
    destruct (bad (fst entry)) eqn:faulty; [reflexivity|].
    assert (existsb (fun entry => yes (fst entry) && negb (bad (fst entry))) weights = true)
      as again.
    { apply existsb_exists. exists entry. split; [exact present|].
      rewrite signed, faulty. reflexivity. }
    rewrite again in found. discriminate.
Qed.

Lemma positive_sign weights :
  positive weights -> forall entry, In entry weights -> 0 <= snd entry.
Proof. intros sign entry present. specialize (sign entry present). lia. Qed.

Lemma unit_sign weights : forall entry : seat, In entry weights -> 0 <= unit_value entry.
Proof. intros entry present. unfold unit_value. lia. Qed.

Lemma positive_check weights :
  forallb (fun entry => 0 <? snd entry) weights = true -> positive weights.
Proof.
  intros checked entry present. rewrite forallb_forall in checked.
  apply Z.ltb_lt. apply checked. exact present.
Qed.

Lemma sum_from start weights :
  fold_left (fun total (entry : seat) => total + snd entry) weights start =
  start + weigh all weights.
Proof.
  revert start. unfold weigh, all.
  induction weights as [|entry rest ih]; intros start; simpl; [lia|].
  rewrite ih. lia.
Qed.

Lemma sum_all weights : sum weights = weigh all weights.
Proof. unfold sum. rewrite sum_from. lia. Qed.

Lemma sum_app left right : sum (left ++ right) = sum left + sum right.
Proof. rewrite !sum_all. unfold weigh. apply score_app. Qed.

Lemma count_all weights : count all weights = Z.of_nat (length weights).
Proof.
  unfold count, all, unit_value.
  induction weights as [|entry rest ih]; [reflexivity|].
  cbn [score length]. rewrite Nat2Z.inj_succ, ih. lia.
Qed.

Lemma sum_positive weights : positive weights -> weights <> [] -> 0 < sum weights.
Proof.
  intros sign present. rewrite sum_all. unfold weigh, all.
  destruct weights as [|entry rest]; [contradiction|]. simpl.
  assert (0 < snd entry) by (apply sign; simpl; auto).
  assert (0 <= score snd (fun _ => true) rest).
  { apply score_sign. intros other later. apply positive_sign with (weights := entry :: rest).
    - exact sign.
    - simpl. auto. }
  lia.
Qed.

Lemma seats_present (weights : list seat) : weights <> [] -> 1 <= Z.of_nat (length weights).
Proof. intros present. destruct weights as [|entry rest]; [contradiction|]. simpl length. lia. Qed.

Lemma form_total weights : total_weight (form_of weights) = weigh all weights.
Proof. unfold form_of. rewrite shape_total. apply sum_all. Qed.

Lemma form_seats weights : n (form_of weights) = count all weights.
Proof. unfold form_of. rewrite shape_seats, count_all. reflexivity. Qed.

Lemma form_heavy weights :
  positive weights -> weights <> [] ->
  quorum_weight (form_of weights) = quota (weigh all weights).
Proof.
  intros sign present. unfold form_of.
  rewrite (shape_heavy _ _ (sum_positive weights sign present)), sum_all. reflexivity.
Qed.

Lemma flat_weigh unit yes weights :
  flat unit weights -> weigh yes weights = unit * count yes weights.
Proof.
  unfold weigh, count, unit_value.
  induction weights as [|entry rest ih]; intros same; simpl; [lia|].
  rewrite ih by (intros other present; apply same; simpl; auto).
  rewrite (same entry (or_introl eq_refl)). destruct (yes (fst entry)); lia.
Qed.

Lemma flat_app unit (left right : list seat) :
  flat unit left -> flat unit right -> flat unit (left ++ right).
Proof.
  intros first second entry present. apply in_app_or in present as [here|there]; auto.
Qed.

Lemma priced_sum unit weights :
  priced unit weights -> Z.of_nat (length weights) * unit <= sum weights.
Proof.
  rewrite sum_all. unfold weigh, all.
  induction weights as [|entry rest ih]; intros least; [simpl; lia|].
  cbn [score length]. rewrite Nat2Z.inj_succ.
  assert (unit <= snd entry) by (apply least; simpl; auto).
  assert (Z.of_nat (length rest) * unit <= score snd (fun _ => true) rest).
  { apply ih. intros other present. apply least. simpl. auto. }
  lia.
Qed.

Lemma apart_tally yes own extra :
  apart yes own extra = true ->
  count yes (own ++ extra) = Z.of_nat (length own) /\ weigh yes (own ++ extra) = sum own.
Proof.
  unfold apart. intros separate. apply andb_true_iff in separate as [kept none].
  unfold count, weigh. rewrite !score_app.
  rewrite (score_kept unit_value yes own kept), (score_none unit_value yes extra none).
  rewrite (score_kept snd yes own kept), (score_none snd yes extra none).
  fold count. fold weigh. rewrite count_all, sum_all. lia.
Qed.

Lemma mint_length start k unit : length (mint start k unit) = k.
Proof. unfold mint. rewrite length_map, length_seq. reflexivity. Qed.

Lemma mint_sum start k unit : sum (mint start k unit) = Z.of_nat k * unit.
Proof.
  rewrite sum_all. unfold weigh, all, mint. revert start.
  induction k as [|k ih]; intros start; [reflexivity|].
  simpl seq. simpl map. simpl score. rewrite ih. lia.
Qed.

Lemma mint_positive start k unit : 0 < unit -> positive (mint start k unit).
Proof.
  intros sign entry present. unfold mint in present.
  apply in_map_iff in present as [id [same _]]. subst entry. exact sign.
Qed.

Theorem quorum_overlap left right weights :
  positive weights ->
  quorum_weight (form_of weights) <= weigh left weights ->
  quorum_weight (form_of weights) <= weigh right weights ->
  2 * quorum_weight (form_of weights) - total_weight (form_of weights) <=
    weigh (both left right) weights.
Proof.
  intros sign first second.
  pose proof (score_join snd left right weights) as joined.
  pose proof (score_cap snd (either left right) weights (positive_sign weights sign)) as cap.
  rewrite form_total. unfold weigh in *. lia.
Qed.

Theorem overlap_margin weights :
  positive weights -> weights <> [] ->
  total_weight (form_of weights) - quorum_weight (form_of weights) <
    2 * quorum_weight (form_of weights) - total_weight (form_of weights) /\
  minority (form_of weights) <=
    2 * quorum_weight (form_of weights) - total_weight (form_of weights).
Proof.
  intros sign present. unfold form_of.
  pose proof (sum_positive weights sign present) as funded.
  rewrite (shape_minority _ _ funded), shape_total, (shape_heavy _ _ funded).
  pose proof (quota_range _ funded). lia.
Qed.

Theorem seatless_shared bad left right stated weights :
  positive weights ->
  weigh bad weights <= total_weight (form_of weights) - quorum_weight (form_of weights) ->
  accepts Seatless stated Quorum left weights = true ->
  accepts Seatless stated Quorum right weights = true ->
  exists entry, In entry weights /\
    left (fst entry) = true /\ right (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign budget first second.
  unfold accepts, ruled in first, second.
  apply seatless_quorum_rows in first as [_ [funded first]].
  apply seatless_quorum_rows in second as [_ [_ second]].
  unfold form_of in budget. rewrite shape_total, (shape_heavy _ _ funded) in budget.
  pose proof (score_join snd left right weights) as joined.
  pose proof (score_cap snd (either left right) weights (positive_sign weights sign)) as cap.
  pose proof (quota_range _ funded) as [strict _].
  pose proof (sum_all weights) as whole.
  destruct (member snd bad (both left right) weights (positive_sign weights sign))
    as [entry [present [shared good]]].
  { unfold weigh in *. lia. }
  unfold both in shared. apply andb_true_iff in shared as [signed_left signed_right].
  exists entry. auto.
Qed.

Theorem shared_wide bad left right stated weights :
  positive weights ->
  weigh bad weights <
    2 * quorum_weight (form_of weights) - total_weight (form_of weights) ->
  accepts Seatless stated Quorum left weights = true ->
  accepts Seatless stated Quorum right weights = true ->
  exists entry, In entry weights /\
    left (fst entry) = true /\ right (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign budget first second.
  unfold accepts, ruled in first, second.
  apply seatless_quorum_rows in first as [_ [funded first]].
  apply seatless_quorum_rows in second as [_ [_ second]].
  unfold form_of in budget. rewrite shape_total, (shape_heavy _ _ funded) in budget.
  pose proof (score_join snd left right weights) as joined.
  pose proof (score_cap snd (either left right) weights (positive_sign weights sign)) as cap.
  pose proof (sum_all weights) as whole.
  destruct (member snd bad (both left right) weights (positive_sign weights sign))
    as [entry [present [shared good]]].
  { unfold weigh in *. lia. }
  unfold both in shared. apply andb_true_iff in shared as [signed_left signed_right].
  exists entry. auto.
Qed.

Theorem shared_sharp total :
  0 < total ->
  exists weights bad left right,
    positive weights /\ sum weights = total /\
    weigh bad weights =
      2 * quorum_weight (form_of weights) - total_weight (form_of weights) /\
    accepts Seatless true Quorum left weights = true /\
    accepts Seatless true Quorum right weights = true /\
    forall entry, In entry weights ->
      left (fst entry) = true -> right (fst entry) = true -> bad (fst entry) = true.
Proof.
  intros funded.
  pose proof (quota_range total funded) as [strict cap].
  destruct (Z.ltb_spec 0 (total - quota total)) as [spare|none].
  - exists [(0%nat, total - quota total); (1%nat, quota total + quota total - total);
      (2%nat, total - quota total)], (only 1), (below 2), (outside (only 0)).
    assert (sum [(0%nat, total - quota total); (1%nat, quota total + quota total - total);
      (2%nat, total - quota total)] = total) as whole by (unfold sum; simpl; lia).
    unfold accepts, ruled, form_of. rewrite !seatless_quorum_rows.
    rewrite whole, shape_total, (shape_heavy _ _ funded).
    unfold weigh, below, outside, only. cbn [score fst snd Nat.ltb Nat.leb Nat.eqb negb].
    split; [|split; [reflexivity|split; [lia|split; [|split]]]].
    + intros entry [same|[same|[same|[]]]]; subst entry; cbn [snd]; lia.
    + split; [reflexivity|]. split; lia.
    + split; [reflexivity|]. split; lia.
    + intros entry [same|[same|[same|[]]]]; subst entry; simpl; intros first second;
        try reflexivity; discriminate.
  - exists [(1%nat, total)], (only 1), all, all.
    assert (sum [(1%nat, total)] = total) as whole by (unfold sum; simpl; lia).
    unfold accepts, ruled, form_of. rewrite !seatless_quorum_rows.
    rewrite whole, shape_total, (shape_heavy _ _ funded).
    unfold weigh, all, only. cbn [score fst snd Nat.eqb].
    split; [|split; [reflexivity|split; [lia|split; [|split]]]].
    + intros entry [same|[]]; subst entry; cbn [snd]; lia.
    + split; [reflexivity|]. split; lia.
    + split; [reflexivity|]. split; lia.
    + intros entry [same|[]]; subst entry; simpl; intros first second; reflexivity.
Qed.

Theorem honest_reach bad weights :
  weigh bad weights <= total_weight (form_of weights) - quorum_weight (form_of weights) ->
  quorum_weight (form_of weights) <= weigh (outside bad) weights.
Proof.
  intros budget. rewrite form_total in budget.
  pose proof (score_split snd bad weights) as whole. unfold weigh in *. lia.
Qed.

Theorem seatless_live bad weights :
  positive weights -> weights <> [] ->
  (accepts Seatless true Quorum (outside bad) weights = true <->
   weigh bad weights <= total_weight (form_of weights) - quorum_weight (form_of weights)).
Proof.
  intros sign present.
  pose proof (sum_positive weights sign present) as funded.
  unfold accepts, ruled. rewrite seatless_quorum_rows.
  unfold form_of. rewrite shape_total, (shape_heavy _ _ funded).
  pose proof (score_split snd bad weights) as whole.
  pose proof (sum_all weights) as same. unfold weigh in *.
  split.
  - intros [_ [_ heavy]]. lia.
  - intros budget. split; [reflexivity|]. split; [exact funded|]. lia.
Qed.

Definition retained (gone : mask) (weights : list seat) : list seat :=
  filter (fun entry => negb (gone (fst entry))) weights.

Lemma retained_present entry gone weights :
  In entry (retained gone weights) <-> In entry weights /\ gone (fst entry) = false.
Proof. unfold retained. rewrite filter_In, negb_true_iff. reflexivity. Qed.

Lemma retained_positive gone weights :
  positive weights -> positive (retained gone weights).
Proof.
  intros sign entry present. apply retained_present in present as [present _].
  exact (sign entry present).
Qed.

Lemma retained_unique gone weights :
  NoDup (map fst weights) -> NoDup (map fst (retained gone weights)).
Proof.
  induction weights as [|entry rest ih]; intros unique; [constructor|].
  cbn [map] in unique. inversion unique as [|head ids absent distinct]; subst.
  change (NoDup (map fst (if negb (gone (fst entry))
    then entry :: retained gone rest else retained gone rest))).
  destruct (gone (fst entry)); cbn [negb map]; [apply ih; exact distinct|].
  constructor; [|apply ih; exact distinct].
  intros present. apply absent. apply in_map_iff in present as [other [same kept]].
  apply retained_present in kept as [present _].
  rewrite <- same. apply in_map. exact present.
Qed.

Lemma score_retained_split value yes gone weights :
  score value yes (retained gone weights) + score value (both yes gone) weights =
  score value yes weights.
Proof.
  unfold retained, both.
  induction weights as [|entry rest ih]; simpl; [lia|].
  destruct (gone (fst entry)) eqn:removed; destruct (yes (fst entry)) eqn:chosen;
    simpl; rewrite ?chosen; lia.
Qed.

Lemma sum_retained gone weights :
  sum (retained gone weights) = sum weights - weigh gone weights.
Proof.
  rewrite !sum_all. unfold weigh, retained, all.
  induction weights as [|entry rest ih]; simpl; [lia|].
  destruct (gone (fst entry)); simpl in *; lia.
Qed.

Lemma weigh_retained_bad bad gone weights :
  (forall entry, In entry weights -> gone (fst entry) = true -> bad (fst entry) = true) ->
  weigh bad (retained gone weights) = weigh bad weights - weigh gone weights.
Proof.
  unfold weigh, retained.
  induction weights as [|entry rest ih]; intros included; simpl; [lia|].
  assert (forall other, In other rest -> gone (fst other) = true -> bad (fst other) = true)
    as later.
  { intros other present. apply included. simpl. auto. }
  specialize (ih later). specialize (included entry (or_introl eq_refl)).
  destruct (gone (fst entry)) eqn:removed; destruct (bad (fst entry)) eqn:faulty;
    simpl in *; rewrite ?faulty; try lia.
  all: specialize (included eq_refl); discriminate.
Qed.

Lemma retained_loss yes gone weights :
  positive weights ->
  weigh yes weights - weigh gone weights <= weigh yes (retained gone weights).
Proof.
  intros sign. pose proof (score_retained_split snd yes gone weights) as partition.
  assert (score snd (both yes gone) weights <= score snd gone weights) as limit.
  { apply score_order; [exact (positive_sign weights sign)|].
    intros entry present chosen. apply andb_true_iff in chosen as [_ removed].
    exact removed. }
  unfold weigh in *. lia.
Qed.

Lemma removal_range bad gone weights :
  positive weights ->
  (forall entry, In entry weights -> gone (fst entry) = true -> bad (fst entry) = true) ->
  0 <= weigh gone weights <= weigh bad weights.
Proof.
  intros sign included. split.
  - apply score_sign. exact (positive_sign weights sign).
  - apply score_order; [exact (positive_sign weights sign)|exact included].
Qed.

Theorem removal_fraction total bad gone :
  0 <= gone <= bad -> 3 * bad < total ->
  0 < total - gone /\ 0 <= bad - gone /\ 3 * (bad - gone) < total - gone.
Proof. intros removed budget. lia. Qed.

Lemma removal_margin total bad gone :
  0 <= gone <= bad -> 3 * bad < total ->
  bad - gone < quota total + quota (total - gone) - total.
Proof.
  intros removed budget.
  pose proof (removal_fraction total bad gone removed budget) as [funded _].
  pose proof (quota_range total ltac:(lia)).
  pose proof (quota_range (total - gone) funded). lia.
Qed.

Theorem removal_state bad gone weights :
  positive weights ->
  (forall entry, In entry weights -> gone (fst entry) = true -> bad (fst entry) = true) ->
  3 * weigh bad weights < sum weights ->
  0 < sum (retained gone weights) /\ 0 <= weigh bad (retained gone weights) /\
  3 * weigh bad (retained gone weights) < sum (retained gone weights).
Proof.
  intros sign included budget.
  rewrite sum_retained, (weigh_retained_bad bad gone weights included).
  apply removal_fraction; [exact (removal_range bad gone weights sign included)|exact budget].
Qed.

Corollary removal_nonempty bad gone weights :
  positive weights ->
  (forall entry, In entry weights -> gone (fst entry) = true -> bad (fst entry) = true) ->
  3 * weigh bad weights < sum weights -> retained gone weights <> [].
Proof.
  intros sign included budget empty.
  pose proof (removal_state bad gone weights sign included budget) as [funded _].
  rewrite empty in funded. change (0 < 0) in funded. lia.
Qed.

Theorem removal_shared bad gone left right stated weights :
  positive weights ->
  (forall entry, In entry weights -> gone (fst entry) = true -> bad (fst entry) = true) ->
  3 * weigh bad weights < sum weights ->
  accepts Seatless stated Quorum left weights = true ->
  accepts Seatless stated Quorum right (retained gone weights) = true ->
  exists entry, In entry (retained gone weights) /\
    left (fst entry) = true /\ right (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign included budget first second.
  pose proof (removal_range bad gone weights sign included) as range.
  pose proof (removal_margin (sum weights) (weigh bad weights) (weigh gone weights)
    range budget) as margin.
  pose proof (retained_positive gone weights sign) as kept.
  pose proof (retained_loss left gone weights sign) as loss.
  pose proof (sum_retained gone weights) as total.
  pose proof (weigh_retained_bad bad gone weights included) as faulty.
  unfold accepts, ruled in first, second.
  apply seatless_quorum_rows in first as [_ [_ first]].
  apply seatless_quorum_rows in second as [_ [_ second]]. rewrite total in second.
  pose proof (score_join snd left right (retained gone weights)) as joined.
  pose proof (score_cap snd (either left right) (retained gone weights)
    (positive_sign _ kept)) as cap.
  pose proof (sum_all (retained gone weights)) as whole.
  destruct (member snd bad (both left right) (retained gone weights) (positive_sign _ kept))
    as [entry [present [shared good]]].
  { unfold weigh in *. lia. }
  apply andb_true_iff in shared as [signed_left signed_right]. exists entry. auto.
Qed.

Definition seat_cap (weights : list seat) : bool :=
  negb (existsb (fun entry => minority (form_of weights) <=? snd entry) weights).

Lemma seat_cap_spec weights :
  seat_cap weights = true <->
  forall entry, In entry weights -> snd entry < minority (form_of weights).
Proof.
  unfold seat_cap. rewrite negb_true_iff, <- not_true_iff_false, existsb_exists.
  split.
  - intros absent entry present.
    destruct (Z_lt_ge_dec (snd entry) (minority (form_of weights))) as [light|heavy].
    + exact light.
    + exfalso. apply absent. exists entry. split; [exact present|].
      apply Z.leb_le. lia.
  - intros cap [entry [present heavy]].
    specialize (cap entry present). apply Z.leb_le in heavy. lia.
Qed.

Definition seat_excess total (entry : seat) := 3 * snd entry - total.

Definition seat_delta before after (entry : seat) :=
  (seat_excess (sum after) entry <? 0) ||
  match find (fun old => Nat.eqb (fst old) (fst entry)) before with
  | None => false
  | Some old => seat_excess (sum after) entry <=? seat_excess (sum before) old
  end.

Definition seat_change before after :=
  if Nat.ltb (length after) 4 then true
  else if Nat.ltb (length before) 4 then
    forallb (fun entry => 3 * snd entry <? sum after) after
  else forallb (seat_delta before after) after.

Lemma found_member (select : seat -> bool) (weights : list seat) (entry : seat) :
  find select weights = Some entry -> In entry weights.
Proof.
  induction weights as [|head rest ih]; simpl; [discriminate|].
  destruct (select head); intros found.
  - inversion found; subst. left. reflexivity.
  - right. apply ih. exact found.
Qed.

Theorem seat_delta_preserves before after :
  (forall entry, In entry before -> 3 * snd entry < sum before) ->
  forallb (seat_delta before after) after = true ->
  forall entry, In entry after -> 3 * snd entry < sum after.
Proof.
  intros previous accepted entry present.
  apply forallb_forall with (x := entry) in accepted; [|exact present].
  unfold seat_delta in accepted. apply orb_true_iff in accepted as [light|prior].
  - apply Z.ltb_lt in light. unfold seat_excess in light. lia.
  - destruct (find (fun old => Nat.eqb (fst old) (fst entry)) before)
      as [old|] eqn:found; [|discriminate].
    apply Z.leb_le in prior.
    specialize (previous old (found_member _ _ _ found)).
    unfold seat_excess in prior. lia.
Qed.

Theorem seat_change_preserves before after :
  ((length before < 4)%nat \/
    (forall entry, In entry before -> 3 * snd entry < sum before)) ->
  (4 <= length after)%nat -> seat_change before after = true ->
  forall entry, In entry after -> 3 * snd entry < sum after.
Proof.
  intros previous size accepted. unfold seat_change in accepted.
  destruct (Nat.ltb (length after) 4) eqn:small_after.
  { apply Nat.ltb_lt in small_after. lia. }
  destruct (Nat.ltb (length before) 4) eqn:small_before.
  - intros entry present. apply forallb_forall with (x := entry) in accepted; [|exact present].
    apply Z.ltb_lt. exact accepted.
  - apply (seat_delta_preserves before after); [|exact accepted].
    destruct previous as [small|cap]; [apply Nat.ltb_ge in small_before; lia|exact cap].
Qed.

Example seat_change_fourth :
  seat_change [(0%nat,100);(1%nat,1);(2%nat,1)]
    [(0%nat,100);(1%nat,1);(2%nat,1);(3%nat,1)] = false.
Proof. vm_compute. reflexivity. Qed.

Lemma seat_ratio total weight :
  0 < total ->
  (3 * weight < total <-> weight < total - quota total + 1).
Proof.
  intros positive. rewrite quota_floor by lia.
  pose proof (Z.div_mod (2 * total) 3 ltac:(lia)).
  pose proof (Z.mod_pos_bound (2 * total) 3 ltac:(lia)). lia.
Qed.

Corollary seat_delta_cap before after :
  positive after -> after <> [] ->
  (forall entry, In entry before -> 3 * snd entry < sum before) ->
  forallb (seat_delta before after) after = true -> seat_cap after = true.
Proof.
  intros sign nonempty previous accepted. apply seat_cap_spec.
  intros entry present.
  pose proof (sum_positive after sign nonempty) as funded.
  pose proof (seat_delta_preserves before after previous accepted entry present) as light.
  apply (proj1 (seat_ratio _ _ funded)) in light.
  unfold minority, form_of. rewrite shape_total, (shape_heavy _ _ funded). exact light.
Qed.

Example seat_delta_exit :
  forallb (seat_delta [(0%nat,3);(1%nat,2);(2%nat,2);(3%nat,2);(4%nat,1)]
    [(0%nat,3);(1%nat,2);(2%nat,2);(3%nat,2)])
    [(0%nat,3);(1%nat,2);(2%nat,2);(3%nat,2)] = false.
Proof. vm_compute. reflexivity. Qed.

Lemma only_missing id weights :
  ~ In id (map fst weights) -> weigh (only id) weights = 0.
Proof.
  intros absent. unfold weigh. apply score_none. apply forallb_forall.
  intros entry present. apply negb_true_iff. unfold only. apply Nat.eqb_neq.
  intros same. apply absent. rewrite <- same. apply in_map. exact present.
Qed.

Lemma only_present id weight weights :
  NoDup (map fst weights) -> In (id, weight) weights ->
  weigh (only id) weights = weight.
Proof.
  induction weights as [|[other value] rest ih]; intros unique present.
  - inversion present.
  - cbn [map fst] in unique. inversion unique as [|head ids absent distinct]; subst.
    cbn [In] in present. destruct present as [same|present].
    + injection same as equal_id equal_weight. subst other value.
      change ((if Nat.eqb id id then weight else 0) + weigh (only id) rest = weight).
      rewrite Nat.eqb_refl, (only_missing id rest absent). lia.
    + assert (other <> id) as different.
      { intros same. subst other. apply absent.
        exact (in_map fst rest (id, weight) present). }
      apply Nat.eqb_neq in different.
      change ((if Nat.eqb other id then value else 0) + weigh (only id) rest = weight).
      rewrite different, (ih distinct present). lia.
Qed.

Theorem seatless_single_exact id weight weights :
  positive weights -> NoDup (map fst weights) -> In (id, weight) weights ->
  (accepts Seatless true Quorum (outside (only id)) weights = true <->
   weight < minority (form_of weights)).
Proof.
  intros sign unique member.
  assert (weights <> []) as present.
  { intros empty. rewrite empty in member. inversion member. }
  rewrite (seatless_live (only id) weights sign present).
  rewrite (only_present id weight weights unique member). unfold minority. lia.
Qed.

Theorem seatless_single id weights :
  positive weights -> weights <> [] -> NoDup (map fst weights) ->
  (forall entry, In entry weights -> snd entry < minority (form_of weights)) ->
  accepts Seatless true Quorum (outside (only id)) weights = true.
Proof.
  intros sign present unique cap.
  destruct (in_dec Nat.eq_dec id (map fst weights)) as [known|absent].
  - apply in_map_iff in known as [[other weight] [same member]].
    cbn [fst] in same. subst other.
    apply (proj2 (seatless_single_exact id weight weights sign unique member)).
    exact (cap (id, weight) member).
  - apply (proj2 (seatless_live (only id) weights sign present)).
    rewrite (only_missing id weights absent).
    pose proof (sum_positive weights sign present) as funded.
    pose proof (quota_within (sum weights) funded) as [_ within].
    unfold form_of. rewrite shape_total, (shape_heavy _ _ funded). lia.
Qed.

Corollary seat_cap_single id weights :
  positive weights -> weights <> [] -> NoDup (map fst weights) ->
  seat_cap weights = true ->
  accepts Seatless true Quorum (outside (only id)) weights = true.
Proof.
  intros sign present unique checked.
  apply (seatless_single id weights sign present unique).
  apply (proj1 (seat_cap_spec weights)). exact checked.
Qed.

Corollary seatless_single_heavy id weight weights :
  positive weights -> NoDup (map fst weights) -> In (id, weight) weights ->
  minority (form_of weights) <= weight ->
  accepts Seatless true Quorum (outside (only id)) weights = false.
Proof.
  intros sign unique member heavy. rewrite <- not_true_iff_false. intros accepted.
  apply (proj1 (seatless_single_exact id weight weights sign unique member)) in accepted.
  lia.
Qed.

Theorem skip_dual yes weights :
  minority (form_of weights) <= weigh yes weights <->
  weigh (outside yes) weights < quorum_weight (form_of weights).
Proof.
  unfold minority. rewrite form_total.
  pose proof (score_split snd yes weights) as whole. unfold weigh in *. lia.
Qed.

Theorem seatless_skip_dual yes weights :
  positive weights -> weights <> [] ->
  accepts Seatless true Skip yes weights =
  negb (accepts Seatless true Quorum (outside yes) weights).
Proof.
  intros sign present.
  pose proof (sum_positive weights sign present) as funded.
  apply eq_true_iff_eq. rewrite negb_true_iff, <- not_true_iff_false.
  unfold accepts, ruled. rewrite seatless_quorum_rows, seatless_skip_rows.
  pose proof (score_split snd yes weights) as whole.
  pose proof (sum_all weights) as same. unfold weigh in *.
  split.
  - intros [_ [_ blocking]] [_ [_ heavy]]. lia.
  - intros refused. split; [reflexivity|]. split; [exact funded|].
    destruct (Z_le_gt_dec (sum weights - quota (sum weights) + 1) (score snd yes weights))
      as [enough|short]; [exact enough|].
    exfalso. apply refused. split; [reflexivity|]. split; [exact funded|]. lia.
Qed.

Theorem skip_honest bad yes weights :
  positive weights ->
  weigh bad weights <= total_weight (form_of weights) - quorum_weight (form_of weights) ->
  minority (form_of weights) <= weigh yes weights ->
  exists entry, In entry weights /\ yes (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign budget blocking.
  apply (member snd bad yes weights (positive_sign weights sign)).
  unfold minority in blocking. unfold weigh in *. lia.
Qed.

Theorem skip_least total level :
  0 < total -> level <= total - quota total ->
  exists weights yes,
    positive weights /\ sum weights = total /\ level <= weigh yes weights /\
    weigh yes weights <= total_weight (form_of weights) - quorum_weight (form_of weights) /\
    quorum_weight (form_of weights) <= weigh (outside yes) weights.
Proof.
  intros funded low.
  pose proof (quota_within total funded) as [least most].
  destruct (Z.ltb_spec 0 (total - quota total)) as [spare|none].
  - exists [(0%nat, total - quota total); (1%nat, quota total)], (only 0).
    assert (sum [(0%nat, total - quota total); (1%nat, quota total)] = total) as whole
      by (unfold sum; simpl; lia).
    unfold form_of. rewrite whole, shape_total, (shape_heavy _ _ funded).
    unfold weigh, outside, only. simpl.
    split; [|split; [reflexivity|split; [lia|split; lia]]].
    intros entry [same|[same|[]]]; subst entry; simpl; lia.
  - exists [(1%nat, quota total)], (only 0).
    assert (sum [(1%nat, quota total)] = total) as whole by (unfold sum; simpl; lia).
    unfold form_of. rewrite whole, shape_total, (shape_heavy _ _ funded).
    unfold weigh, outside, only. simpl.
    split; [|split; [reflexivity|split; [lia|split; lia]]].
    intros entry [same|[]]; subst entry; simpl; lia.
Qed.

Theorem skip_sharp total :
  4 <= total ->
  exists weights yes,
    positive weights /\ sum weights = total /\
    0 < weigh yes weights /\
    weigh yes weights = minority (form_of weights) - 1 /\
    weigh yes weights <= total_weight (form_of weights) - quorum_weight (form_of weights) /\
    accepts Seatless true Skip yes weights = false /\
    accepts Seatless true Quorum (outside yes) weights = true.
Proof.
  intros enough.
  assert (0 < total) as funded by lia.
  pose proof (quota_range total funded) as [strict cap].
  exists [(0%nat, total - quota total); (1%nat, quota total)], (only 0).
  assert (sum [(0%nat, total - quota total); (1%nat, quota total)] = total) as whole
    by (unfold sum; simpl; lia).
  rewrite <- (not_true_iff_false (accepts Seatless true Skip _ _)).
  unfold accepts, ruled, form_of. rewrite seatless_quorum_rows, seatless_skip_rows.
  rewrite whole, shape_total, (shape_minority _ _ funded), (shape_heavy _ _ funded).
  unfold weigh, outside, only. simpl.
  split; [|split; [reflexivity|split; [lia|split; [lia|split; [lia|split]]]]].
  - intros entry [same|[same|[]]]; subst entry; simpl; lia.
  - intros [_ [_ blocking]]. lia.
  - split; [reflexivity|]. split; lia.
Qed.

Theorem weight_only stated a left right first second :
  a <> Start ->
  sum first = sum second -> weigh left first = weigh right second ->
  accepts Seatless stated a left first = accepts Seatless stated a right second.
Proof.
  intros other whole signed. unfold accepts, ruled. rewrite whole, signed.
  apply seatless_blind. exact other.
Qed.

Theorem seatless_closed stated a yes weights :
  a <> Start -> stated = false \/ sum weights <= 0 ->
  accepts Seatless stated a yes weights = false.
Proof. intros other closed. unfold accepts, ruled. apply seatless_guard; assumption. Qed.

Theorem stake_accepted stated yes weights a :
  a = Quorum \/ a = Skip ->
  positive weights -> weights <> [] ->
  accepts Stake stated a yes weights = true ->
  accepts Seatless stated a yes weights = true.
Proof.
  intros chosen sign present. unfold accepts, ruled.
  apply stake_within; [exact chosen|]. apply sum_positive; assumption.
Qed.

Theorem stake_shared bad left right stated weights :
  positive weights -> weights <> [] ->
  weigh bad weights <= total_weight (form_of weights) - quorum_weight (form_of weights) \/
    count bad weights <= f (form_of weights) ->
  accepts Stake stated Quorum left weights = true ->
  accepts Stake stated Quorum right weights = true ->
  exists entry, In entry weights /\
    left (fst entry) = true /\ right (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign present budget first second.
  pose proof (sum_positive weights sign present) as funded.
  unfold accepts, ruled in first, second.
  apply stake_quorum_rows in first as [_ [seats_left heavy_left]].
  apply stake_quorum_rows in second as [_ [seats_right heavy_right]].
  unfold form_of in budget.
  assert (exists entry, In entry weights /\
    both left right (fst entry) = true /\ bad (fst entry) = false) as [entry [here [shared good]]].
  { destruct budget as [by_weight|by_count].
    - rewrite shape_total, (shape_heavy _ _ funded) in by_weight.
      rewrite (shape_heavy _ _ funded) in heavy_left, heavy_right.
      pose proof (score_join snd left right weights) as joined.
      pose proof (score_cap snd (either left right) weights (positive_sign weights sign)) as cap.
      pose proof (quota_range _ funded) as [strict _].
      pose proof (sum_all weights) as whole.
      apply (member snd bad (both left right) weights (positive_sign weights sign)).
      unfold weigh in *. lia.
    - pose proof (score_join unit_value left right weights) as joined.
      pose proof (score_cap unit_value (either left right) weights (unit_sign weights)) as cap.
      pose proof (count_all weights) as whole.
      pose proof (seat_overlap _ (sum weights) (seats_present weights present)) as overlap.
      apply (member unit_value bad (both left right) weights (unit_sign weights)).
      unfold count in *. lia. }
  unfold both in shared. apply andb_true_iff in shared as [signed_left signed_right].
  exists entry. auto.
Qed.

Theorem stake_shared_wide bad left right stated weights :
  positive weights ->
  weigh bad weights <
    2 * quorum_weight (form_of weights) - total_weight (form_of weights) \/
    count bad weights < 2 * quorum (form_of weights) - n (form_of weights) ->
  accepts Stake stated Quorum left weights = true ->
  accepts Stake stated Quorum right weights = true ->
  exists entry, In entry weights /\
    left (fst entry) = true /\ right (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign budget first second.
  unfold accepts, ruled in first, second.
  apply stake_quorum_rows in first as [_ [seats_left heavy_left]].
  apply stake_quorum_rows in second as [_ [seats_right heavy_right]].
  assert (exists entry, In entry weights /\
    both left right (fst entry) = true /\ bad (fst entry) = false) as [entry [here [shared good]]].
  { destruct budget as [by_weight|by_count].
    - rewrite form_total in by_weight. unfold form_of in by_weight.
      pose proof (score_join snd left right weights) as joined.
      pose proof (score_cap snd (either left right) weights (positive_sign weights sign)) as cap.
      apply (member snd bad (both left right) weights (positive_sign weights sign)).
      unfold weigh in *. lia.
    - rewrite form_seats in by_count. unfold form_of in by_count.
      pose proof (score_join unit_value left right weights) as joined.
      pose proof (score_cap unit_value (either left right) weights (unit_sign weights)) as cap.
      apply (member unit_value bad (both left right) weights (unit_sign weights)).
      unfold count in *. lia. }
  unfold both in shared. apply andb_true_iff in shared as [signed_left signed_right].
  exists entry. auto.
Qed.

Theorem stake_skip_honest bad yes stated weights :
  positive weights ->
  weigh bad weights <= total_weight (form_of weights) - quorum_weight (form_of weights) \/
    count bad weights <= f (form_of weights) ->
  accepts Stake stated Skip yes weights = true ->
  exists entry, In entry weights /\ yes (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign budget accepted.
  unfold accepts, ruled in accepted. apply stake_skip_rows in accepted as [_ [seats blocking]].
  destruct budget as [by_weight|by_count].
  - apply (member snd bad yes weights (positive_sign weights sign)).
    unfold form_of in by_weight. unfold minority in blocking. unfold weigh in *. lia.
  - apply (member unit_value bad yes weights (unit_sign weights)).
    unfold form_of in by_count. unfold count in *. lia.
Qed.

Theorem stake_live bad weights :
  positive weights -> weights <> [] ->
  (accepts Stake true Quorum (outside bad) weights = true <->
   count bad weights <= n (form_of weights) - quorum (form_of weights) /\
   weigh bad weights <= total_weight (form_of weights) - quorum_weight (form_of weights)).
Proof.
  intros sign present.
  unfold accepts, ruled. rewrite stake_quorum_rows.
  rewrite form_total, form_seats. unfold form_of.
  pose proof (score_split snd bad weights) as whole.
  pose proof (score_split unit_value bad weights) as seats.
  unfold weigh, count in *.
  split.
  - intros [_ [seated heavy]]. lia.
  - intros [seated heavy]. split; [reflexivity|]. lia.
Qed.

Theorem equal_weights unit yes weights a :
  a = Quorum \/ a = Skip ->
  0 < unit -> flat unit weights -> weights <> [] ->
  accepts Stake true a yes weights = accepts Seatless true a yes weights.
Proof.
  intros chosen sign same present.
  pose proof (seats_present weights present) as seats.
  pose proof (flat_weigh unit yes weights same) as signed.
  pose proof (flat_weigh unit all weights same) as whole.
  rewrite count_all, <- sum_all in whole.
  pose proof (score_sign unit_value yes weights (unit_sign weights)) as low.
  pose proof (score_cap unit_value yes weights (unit_sign weights)) as high.
  fold count in low, high. rewrite count_all in high.
  assert (0 < sum weights) as funded by nia.
  pose proof (quota_range _ funded) as [strict cap].
  pose proof (fault_range _ (sum weights) seats) as [under over].
  apply eq_true_iff_eq. unfold accepts, ruled.
  destruct chosen as [chosen|chosen]; subst a.
  - rewrite stake_quorum_rows, seatless_quorum_rows, (shape_heavy _ _ funded).
    split.
    + intros [flag [_ heavy]]. auto.
    + intros [flag [_ heavy]]. split; [exact flag|]. split; [|exact heavy].
      assert (2 * Z.of_nat (length weights) < 3 * count yes weights) as wide by nia.
      rewrite seat_quorum.
      destruct (Z.leb_spec (Z.of_nat (length weights)) 0); [lia|].
      destruct (Z.ltb_spec (Z.of_nat (length weights)) 4); lia.
  - rewrite stake_skip_rows, seatless_skip_rows, (shape_minority _ _ funded).
    split.
    + intros [flag [_ blocking]]. auto.
    + intros [flag [_ blocking]]. split; [exact flag|]. split; [|exact blocking].
      assert (Z.of_nat (length weights) <= 3 * count yes weights) as wide by nia.
      lia.
Qed.

Theorem ahead_same yes weights :
  positive weights -> weights <> [] ->
  accepts Stake true Ahead yes weights = accepts Seatless true Ahead yes weights.
Proof.
  intros sign present.
  pose proof (sum_positive weights sign present) as funded.
  pose proof (minority_range (Z.of_nat (length weights)) (sum weights) funded) as [low _].
  assert ((0 <? sum weights) = true) as opened by (apply Z.ltb_lt; exact funded).
  assert ((0 <? minority (shape_of (Z.of_nat (length weights)) (sum weights))) = true) as wide
    by (apply Z.ltb_lt; lia).
  unfold accepts, ruled, meets, valid. cbn [rule_of declared form].
  rewrite shape_total, !sign_high, opened, wide. reflexivity.
Qed.

Theorem flat_same unit yes weights a :
  a <> Start -> a <> Promise ->
  0 < unit -> flat unit weights -> weights <> [] ->
  accepts Stake true a yes weights = accepts Seatless true a yes weights.
Proof.
  intros start promise sign same present.
  destruct a; try contradiction.
  - apply (equal_weights unit); auto.
  - apply (equal_weights unit); auto.
  - change (accepts Stake true Skip yes weights = accepts Seatless true Skip yes weights).
    apply (equal_weights unit); auto.
  - change (accepts Stake true Quorum yes weights = accepts Seatless true Quorum yes weights).
    apply (equal_weights unit); auto.
  - apply ahead_same; [|exact present].
    intros entry here. rewrite (same entry here). exact sign.
Qed.

Theorem flat_halt unit yes own extra :
  0 < unit -> flat unit own -> flat unit extra -> own <> [] ->
  accepts Stake true Quorum yes (own ++ extra) =
  accepts Seatless true Quorum yes (own ++ extra).
Proof.
  intros sign first second present.
  apply (equal_weights unit); [left; reflexivity|exact sign| |].
  - apply flat_app; assumption.
  - intros empty. apply app_eq_nil in empty as [none _]. contradiction.
Qed.

Theorem few_halt yes own extra :
  apart yes own extra = true -> extra <> [] ->
  Z.of_nat (length own) + Z.of_nat (length extra) < 4 ->
  accepts Stake true Quorum yes (own ++ extra) = false.
Proof.
  intros separate present small.
  destruct (apart_tally yes own extra separate) as [seats signed].
  pose proof (seats_present extra present) as grown.
  pose proof (seat_halt (Z.of_nat (length own)) (Z.of_nat (length extra))
    (sum (own ++ extra)) ltac:(lia) ltac:(lia)) as by_seats.
  rewrite <- not_true_iff_false. unfold accepts, ruled.
  rewrite stake_quorum_rows, seats, length_app, Nat2Z.inj_add.
  intros [_ [seated _]]. lia.
Qed.

Theorem stake_halt yes own extra :
  positive own -> positive extra -> own <> [] ->
  apart yes own extra = true ->
  4 <= Z.of_nat (length own) + Z.of_nat (length extra) ->
  (accepts Stake true Quorum yes (own ++ extra) = false <->
   Z.of_nat (length own) <= 2 * Z.of_nat (length extra) \/ sum own <= 2 * sum extra).
Proof.
  intros sign_own sign_extra present separate enough.
  destruct (apart_tally yes own extra separate) as [seats signed].
  pose proof (sum_positive own sign_own present) as held.
  assert (0 <= sum extra) as grown.
  { rewrite sum_all. apply score_sign. apply positive_sign. exact sign_extra. }
  rewrite <- not_true_iff_false. unfold accepts, ruled.
  rewrite stake_quorum_rows, seats, signed, length_app, Nat2Z.inj_add, sum_app.
  rewrite (shape_heavy _ (sum own + sum extra) ltac:(lia)).
  pose proof (seat_halt (Z.of_nat (length own)) (Z.of_nat (length extra))
    (sum own + sum extra) ltac:(lia) ltac:(lia)) as by_seats.
  pose proof (weight_keep (sum own) (sum extra) held grown) as by_weight.
  split.
  - intros refused.
    destruct (Z_le_gt_dec (Z.of_nat (length own)) (2 * Z.of_nat (length extra)))
      as [short|seated]; [left; exact short|].
    destruct (Z_le_gt_dec (sum own) (2 * sum extra)) as [light|heavy]; [right; exact light|].
    exfalso. apply refused. split; [reflexivity|]. split; lia.
  - intros [short|light] [_ [seated heavy]]; lia.
Qed.

Theorem seatless_halt yes own extra :
  positive own -> positive extra -> own <> [] ->
  apart yes own extra = true ->
  (accepts Seatless true Quorum yes (own ++ extra) = false <-> sum own <= 2 * sum extra) /\
  (sum own <= 2 * sum extra <-> minority (form_of (own ++ extra)) <= sum extra).
Proof.
  intros sign_own sign_extra present separate.
  destruct (apart_tally yes own extra separate) as [seats signed].
  pose proof (sum_positive own sign_own present) as held.
  assert (0 <= sum extra) as grown.
  { rewrite sum_all. apply score_sign. apply positive_sign. exact sign_extra. }
  pose proof (weight_keep (sum own) (sum extra) held grown) as by_weight.
  split.
  - rewrite <- not_true_iff_false. unfold accepts, ruled.
    rewrite seatless_quorum_rows, signed, sum_app.
    split.
    + intros refused.
      destruct (Z_le_gt_dec (sum own) (2 * sum extra)) as [light|heavy]; [exact light|].
      exfalso. apply refused. split; [reflexivity|]. split; lia.
    + intros light [_ [_ heavy]]. lia.
  - unfold form_of. rewrite sum_app.
    pose proof (weight_block (Z.of_nat (length (own ++ extra))) (sum own) (sum extra) held grown).
    tauto.
Qed.

Theorem stake_cost unit yes own extra :
  positive own -> positive extra -> own <> [] ->
  0 < unit -> priced unit extra ->
  apart yes own extra = true ->
  4 <= Z.of_nat (length own) + Z.of_nat (length extra) ->
  accepts Stake true Quorum yes (own ++ extra) = false ->
  Z.min (Z.of_nat (length own) * unit) (sum own) <= 2 * sum extra.
Proof.
  intros sign_own sign_extra present sign least separate enough refused.
  apply (stake_halt yes own extra sign_own sign_extra present separate enough) in refused.
  pose proof (priced_sum unit extra least) as paid.
  destruct refused as [short|light]; [|lia].
  assert (Z.of_nat (length own) * unit <= 2 * Z.of_nat (length extra) * unit) as scaled
    by (apply Z.mul_le_mono_nonneg_r; lia).
  lia.
Qed.

Theorem cheap_halt yes own start k unit :
  positive own -> 0 < unit ->
  apart yes own (mint start k unit) = true ->
  4 <= Z.of_nat (length own) + Z.of_nat k ->
  Z.of_nat (length own) <= 2 * Z.of_nat k ->
  2 * (Z.of_nat k * unit) < sum own ->
  sum (mint start k unit) = Z.of_nat k * unit /\
  accepts Stake true Quorum yes (own ++ mint start k unit) = false /\
  quorum_weight (form_of (own ++ mint start k unit)) <= weigh yes (own ++ mint start k unit) /\
  accepts Seatless true Quorum yes (own ++ mint start k unit) = true.
Proof.
  intros sign_own sign_unit separate enough short light.
  pose proof (mint_sum start k unit) as cost.
  pose proof (mint_length start k unit) as size.
  pose proof (mint_positive start k unit sign_unit) as sign_extra.
  assert (0 <= Z.of_nat k * unit) as paid by (apply Z.mul_nonneg_nonneg; lia).
  assert (own <> []) as present.
  { intros empty. subst own. change (sum []) with 0 in light. lia. }
  pose proof (sum_positive own sign_own present) as held.
  destruct (apart_tally yes own (mint start k unit) separate) as [seats signed].
  split; [exact cost|]. split; [|split].
  - apply (stake_halt yes own (mint start k unit) sign_own sign_extra present separate).
    + rewrite size. exact enough.
    + left. rewrite size. exact short.
  - unfold form_of. rewrite signed, sum_app, cost.
    rewrite (shape_heavy _ (sum own + Z.of_nat k * unit) ltac:(lia)).
    apply (weight_keep (sum own) (Z.of_nat k * unit) held paid). lia.
  - destruct (accepts Seatless true Quorum yes (own ++ mint start k unit)) eqn:answer;
      [reflexivity|].
    destruct (seatless_halt yes own (mint start k unit) sign_own sign_extra present separate)
      as [route _].
    apply route in answer. rewrite cost in answer. lia.
Qed.

Definition asks : list ask := [Quorum; Skip; Source; Pair; Ahead; Start; Promise].

Definition row (chosen : rule) (yes : mask) (weights : list seat) : list bool :=
  map (fun a => accepts chosen true a yes weights) asks.

Definition picked (code : Z) : mask := fun id => Z.testbit code (Z.of_nat id).

Definition thin : list seat :=
  [(0%nat, 400); (1%nat, 300); (2%nat, 60); (3%nat, 60); (4%nat, 60); (5%nat, 60); (6%nat, 60)].

Definition skew : list seat := [(0%nat, 500); (1%nat, 250); (2%nat, 249); (3%nat, 1)].

Definition even : list seat := mint 0 4 1.

Definition seven : list seat := mint 0 7 1.

Definition five : list seat := [(0%nat, 2); (1%nat, 1); (2%nat, 1); (3%nat, 1)].

Definition exact : list seat := [(0%nat, 333); (1%nat, 334); (2%nat, 333)].

Definition parted : list seat :=
  [(0%nat, 494); (1%nat, 250); (2%nat, 249); (3%nat, 1)] ++ mint 4 6 1.

Definition crowd : mask := fun id => Nat.eqb id 0 || Nat.leb 4 id.

Definition dense : list seat :=
  [(0%nat, 100); (1%nat, 100); (2%nat, 100); (3%nat, 1); (4%nat, 1); (5%nat, 1)].

Definition loose : list seat := [(0%nat, 170); (1%nat, 170); (2%nat, 600); (3%nat, 60)].

Definition lone : list seat := [(0%nat, 700); (1%nat, 100); (2%nat, 100); (3%nat, 100)].

Definition third : list seat := [(0%nat, 334); (1%nat, 222); (2%nat, 222); (3%nat, 222)].

Example thin_case :
  forallb (fun entry => 0 <? snd entry) thin = true /\
  quorum_weight (form_of thin) = 667 /\ minority (form_of thin) = 334 /\
  quorum (form_of thin) = 5 /\ f (form_of thin) = 2 /\
  accepts Seatless true Quorum (below 2) thin = true /\
  accepts Stake true Quorum (below 2) thin = false /\
  accepts Seatless true Skip (only 0) thin = true /\
  accepts Stake true Skip (only 0) thin = false /\
  accepts Seatless true Quorum (outside (only 1)) thin = true /\
  accepts Stake true Quorum (outside (only 1)) thin = true /\
  accepts Seatless true Quorum (outside (only 0)) thin = false /\
  accepts Stake true Quorum (outside (only 0)) thin = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example thin_shared left right :
  accepts Seatless true Quorum left thin = true ->
  accepts Seatless true Quorum right thin = true ->
  exists entry, In entry thin /\
    left (fst entry) = true /\ right (fst entry) = true /\ only 1 (fst entry) = false.
Proof.
  apply seatless_shared.
  - apply positive_check. reflexivity.
  - vm_compute. discriminate.
Qed.

Example thin_rows :
  map (fun yes => (row Seatless yes thin, row Stake yes thin))
    [only 0; below 2; only 1; outside (pair 1 6); outside (below 2)] =
  [([false; true; true; false; true; true; true],
    [false; false; false; false; true; false; false]);
   ([true; true; true; true; true; true; true],
    [false; false; false; false; true; false; false]);
   ([false; false; false; false; false; true; false],
    [false; false; false; false; false; false; false]);
   ([false; true; true; false; true; true; true],
    [false; true; true; false; true; true; true]);
   ([false; false; false; false; false; true; false],
    [false; false; false; false; false; true; true])].
Proof. vm_compute. reflexivity. Qed.

Example thin_attested :
  attests Seatless true 4 (only 0) thin = true /\
  attests Stake true 4 (only 0) thin = false /\
  attests Seatless true 3 (outside (below 2)) thin = false /\
  attests Stake true 3 (outside (below 2)) thin = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example thin_promise :
  accepts Stake true Promise (outside (below 2)) thin = true /\
  accepts Seatless true Promise (outside (below 2)) thin = false.
Proof. vm_compute. split; reflexivity. Qed.

Example skew_case :
  forallb (fun entry => 0 <? snd entry) skew = true /\
  count (only 0) skew = 1 /\ f (form_of skew) = 1 /\
  weigh (only 0) skew = 500 /\
  total_weight (form_of skew) - quorum_weight (form_of skew) = 333 /\
  accepts Seatless true Quorum (pair 0 1) skew = true /\
  accepts Seatless true Quorum (pair 0 2) skew = true /\
  forallb (fun entry =>
    negb (both (pair 0 1) (pair 0 2) (fst entry)) || only 0 (fst entry)) skew = true /\
  accepts Stake true Quorum (pair 0 1) skew = false /\
  accepts Stake true Quorum (pair 0 2) skew = false /\
  accepts Stake true Quorum (below 3) skew = true /\
  accepts Stake true Quorum (either (pair 0 1) (only 3)) skew = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example skew_stake left right :
  accepts Stake true Quorum left skew = true ->
  accepts Stake true Quorum right skew = true ->
  exists entry, In entry skew /\
    left (fst entry) = true /\ right (fst entry) = true /\ only 0 (fst entry) = false.
Proof.
  apply stake_shared.
  - apply positive_check. reflexivity.
  - discriminate.
  - right. vm_compute. discriminate.
Qed.

Example parted_case :
  forallb (fun entry => 0 <? snd entry) parted = true /\
  sum parted = 1000 /\ Z.of_nat (length parted) = 10 /\
  f (form_of parted) = 3 /\ quorum (form_of parted) = 7 /\
  count crowd parted = 7 /\ weigh crowd parted = 500 /\
  accepts Stake true Quorum (either crowd (only 1)) parted = true /\
  accepts Stake true Quorum (either crowd (only 2)) parted = true /\
  forallb (fun entry =>
    negb (both (either crowd (only 1)) (either crowd (only 2)) (fst entry))
    || crowd (fst entry)) parted = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example loose_case :
  count (below 2) loose = 2 /\
  2 * quorum (form_of loose) - n (form_of loose) = 2 /\
  weigh (below 2) loose = 340 /\
  2 * quorum_weight (form_of loose) - total_weight (form_of loose) = 334 /\
  forallb (fun code =>
    negb (accepts Stake true Quorum (picked code) loose) || picked code 2%nat)
    (map Z.of_nat (seq 0 16)) = true /\
  forallb (fun code =>
    negb (accepts Seatless true Quorum (picked code) loose) || picked code 2%nat)
    (map Z.of_nat (seq 0 16)) = true /\
  accepts Stake true Quorum (outside (only 3)) loose = true /\
  accepts Seatless true Quorum (pair 0 2) loose = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example five_case :
  total_weight (form_of five) - quorum_weight (form_of five) = 1 /\
  weigh (only 0) five = 2 /\
  2 * quorum_weight (form_of five) - total_weight (form_of five) = 3.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example five_shared left right :
  accepts Seatless true Quorum left five = true ->
  accepts Seatless true Quorum right five = true ->
  exists entry, In entry five /\
    left (fst entry) = true /\ right (fst entry) = true /\ only 0 (fst entry) = false.
Proof.
  apply shared_wide.
  - apply positive_check. reflexivity.
  - vm_compute. reflexivity.
Qed.

Example exact_case :
  weigh (only 1) exact =
    2 * quorum_weight (form_of exact) - total_weight (form_of exact) /\
  accepts Seatless true Quorum (below 2) exact = true /\
  accepts Seatless true Quorum (outside (only 0)) exact = true /\
  forallb (fun entry =>
    negb (both (below 2) (outside (only 0)) (fst entry)) || only 1 (fst entry)) exact = true /\
  weigh (only 0) exact = minority (form_of exact) - 1 /\
  accepts Seatless true Skip (only 0) exact = false /\
  accepts Seatless true Quorum (outside (only 0)) exact = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example margin_rows :
  map (fun total => 2 * quota total - total - 1 - (total - quota total))
    [1; 2; 3; 4; 5; 6; 7; 1000] = [0; 1; 2; 0; 1; 2; 0; 0].
Proof. vm_compute. reflexivity. Qed.

Example dense_case :
  f (form_of dense) = 1 /\ count (below 3) dense = 3 /\
  2 * quorum (form_of dense) - n (form_of dense) = 4 /\
  weigh (below 3) dense = 300 /\
  2 * quorum_weight (form_of dense) - total_weight (form_of dense) = 103 /\
  accepts Seatless true Quorum (below 3) dense = true /\
  accepts Stake true Quorum (below 3) dense = false /\
  accepts Stake true Quorum (outside (only 5)) dense = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example dense_shared left right :
  accepts Stake true Quorum left dense = true ->
  accepts Stake true Quorum right dense = true ->
  exists entry, In entry dense /\
    left (fst entry) = true /\ right (fst entry) = true /\ below 3 (fst entry) = false.
Proof.
  apply stake_shared_wide.
  - apply positive_check. reflexivity.
  - right. vm_compute. reflexivity.
Qed.

Example skew_skip yes :
  accepts Stake true Skip yes skew = true ->
  exists entry, In entry skew /\ yes (fst entry) = true /\ only 0 (fst entry) = false.
Proof.
  apply stake_skip_honest.
  - apply positive_check. reflexivity.
  - right. vm_compute. discriminate.
Qed.

Example skew_alone :
  accepts Seatless true Skip (only 0) skew = true /\
  accepts Stake true Skip (only 0) skew = false /\
  accepts Stake true Skip (pair 0 3) skew = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example blind_case :
  sum thin = sum lone /\ weigh (below 2) thin = weigh (only 0) lone /\
  count (below 2) thin = 2 /\ count (only 0) lone = 1 /\
  Z.of_nat (length thin) = 7 /\ Z.of_nat (length lone) = 4 /\
  row Seatless (below 2) thin = row Seatless (only 0) lone /\
  weigh (below 2) thin = weigh (outside (only 1)) thin /\
  count (outside (only 1)) thin = 6 /\
  row Seatless (below 2) thin = row Seatless (outside (only 1)) thin /\
  row Stake (below 2) thin = [false; false; false; false; true; false; false] /\
  row Stake (outside (only 1)) thin = [true; true; true; true; true; true; true].
Proof. vm_compute. repeat split; reflexivity. Qed.

Example even_case :
  count (below 2) even = 2 /\ f (form_of even) = 1 /\
  weigh (below 2) even = 2 /\
  total_weight (form_of even) - quorum_weight (form_of even) = 1 /\
  accepts Stake true Quorum (below 3) even = true /\
  accepts Stake true Quorum (either (below 2) (only 3)) even = true /\
  accepts Seatless true Quorum (below 3) even = true /\
  accepts Seatless true Quorum (either (below 2) (only 3)) even = true /\
  forallb (fun entry =>
    negb (both (below 3) (either (below 2) (only 3)) (fst entry)) || below 2 (fst entry))
    even = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example cheap_case :
  apart (below 36) (mint 0 36 1000) (mint 36 18 1) = true /\
  sum (mint 0 36 1000) = 36000 /\ sum (mint 36 18 1) = 18 /\
  f (form_of (mint 0 36 1000)) = 11 /\
  f (form_of (mint 0 36 1000 ++ mint 36 18 1)) = 17 /\
  quorum (form_of (mint 0 36 1000 ++ mint 36 18 1)) = 37 /\
  accepts Stake true Quorum (below 36) (mint 0 36 1000 ++ mint 36 18 1) = false /\
  accepts Seatless true Quorum (below 36) (mint 0 36 1000 ++ mint 36 18 1) = true /\
  accepts Stake true Quorum (below 36) (mint 0 36 1000 ++ mint 36 17 1) = true /\
  accepts Stake true Quorum (below 36) (mint 0 36 1000 ++ mint 36 12 1) = true /\
  accepts Seatless true Quorum (below 36) (mint 0 36 1000 ++ mint 36 1 17999) = true /\
  accepts Seatless true Quorum (below 36) (mint 0 36 1000 ++ mint 36 1 18000) = false /\
  accepts Seatless true Quorum (below 36) (mint 0 36 1000 ++ mint 36 18 1000) = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example level_case :
  accepts Stake true Quorum (below 36) (mint 0 36 1 ++ mint 36 18 1) = false /\
  accepts Seatless true Quorum (below 36) (mint 0 36 1 ++ mint 36 18 1) = false /\
  accepts Stake true Quorum (below 36) (mint 0 36 1 ++ mint 36 17 1) = true /\
  accepts Seatless true Quorum (below 36) (mint 0 36 1 ++ mint 36 17 1) = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example few_case :
  apart (below 2) (mint 0 2 10) (mint 2 1 1) = true /\
  accepts Stake true Quorum (below 2) (mint 0 2 10) = true /\
  accepts Stake true Quorum (below 2) (mint 0 2 10 ++ mint 2 1 1) = false /\
  accepts Seatless true Quorum (below 2) (mint 0 2 10 ++ mint 2 1 1) = true /\
  accepts Stake true Quorum (below 3) (mint 0 3 10 ++ mint 3 1 1) = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example cost_instance :
  Z.min (Z.of_nat (length (mint 0 36 1000)) * 1) (sum (mint 0 36 1000)) <=
    2 * sum (mint 36 18 1) /\
  Z.min (Z.of_nat (length (mint 0 36 1000)) * 1) (sum (mint 0 36 1000)) = 36 /\
  2 * sum (mint 36 18 1) = 36.
Proof.
  split; [|vm_compute; split; reflexivity].
  apply (stake_cost 1 (below 36)).
  - apply mint_positive. lia.
  - apply mint_positive. lia.
  - discriminate.
  - lia.
  - intros entry present. pose proof (mint_positive 36 18 1 ltac:(lia) entry present). lia.
  - vm_compute. reflexivity.
  - vm_compute. discriminate.
  - vm_compute. reflexivity.
Qed.

Example few_instance :
  accepts Stake true Quorum (below 2) (mint 0 2 10 ++ mint 2 1 1) = false.
Proof.
  apply few_halt.
  - vm_compute. reflexivity.
  - discriminate.
  - vm_compute. reflexivity.
Qed.

Example cheap_instance :
  sum (mint 36 18 1) = 18 /\
  accepts Stake true Quorum (below 36) (mint 0 36 1000 ++ mint 36 18 1) = false /\
  quorum_weight (form_of (mint 0 36 1000 ++ mint 36 18 1)) <=
    weigh (below 36) (mint 0 36 1000 ++ mint 36 18 1) /\
  accepts Seatless true Quorum (below 36) (mint 0 36 1000 ++ mint 36 18 1) = true.
Proof.
  apply (cheap_halt (below 36) (mint 0 36 1000) 36 18 1).
  - apply mint_positive. lia.
  - lia.
  - vm_compute. reflexivity.
  - vm_compute. discriminate.
  - vm_compute. discriminate.
  - vm_compute. reflexivity.
Qed.

Example lone_case :
  accepts Seatless true Quorum (only 0) lone = true /\
  accepts Stake true Quorum (only 0) lone = false /\
  accepts Seatless true Quorum (outside (only 0)) lone = false /\
  accepts Stake true Quorum (outside (only 0)) lone = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example third_case :
  minority (form_of third) = 334 /\
  accepts Seatless true Quorum (outside (only 0)) third = false /\
  accepts Stake true Quorum (outside (only 0)) third = false /\
  accepts Seatless true Skip (only 0) third = true /\
  accepts Stake true Skip (only 0) third = false /\
  accepts Seatless true Quorum (only 0) third = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example seat_cap_edge :
  positive third /\ NoDup (map fst third) /\
  forallb (fun entry => snd entry <=? minority (form_of third)) third = true /\
  sum third = 1000 /\ quorum_weight (form_of third) = 667 /\
  minority (form_of third) = 334 /\
  weigh (only 0) third = minority (form_of third) /\
  weigh (outside (only 0)) third = quorum_weight (form_of third) - 1 /\
  seat_cap third = false /\
  accepts Seatless true Quorum (outside (only 0)) third = false.
Proof.
  split; [apply positive_check; reflexivity|]. split.
  - change (NoDup [0%nat; 1%nat; 2%nat; 3%nat]).
    constructor; [simpl; intuition discriminate|].
    constructor; [simpl; intuition discriminate|].
    constructor; [simpl; intuition discriminate|].
    constructor; [simpl; tauto|constructor].
  - vm_compute. repeat split; reflexivity.
Qed.

Example seat_cap_empty :
  positive [] /\ NoDup (map fst ([] : list seat)) /\ seat_cap [] = true /\
  accepts Seatless true Quorum (outside (only 0)) [] = false.
Proof.
  split; [intros entry present; inversion present|].
  split; [constructor|]. split; reflexivity.
Qed.

Definition eviction_weights : list seat :=
  [(0%nat, 3); (1%nat, 3); (2%nat, 2); (3%nat, 1); (4%nat, 1)].

Example eviction_cap :
  positive eviction_weights /\ NoDup (map fst eviction_weights) /\
  retained (only 4) eviction_weights = [(0%nat, 3); (1%nat, 3); (2%nat, 2); (3%nat, 1)] /\
  (3 * weigh (only 4) eviction_weights <? sum eviction_weights) = true /\
  sum eviction_weights = 10 /\ minority (form_of eviction_weights) = 4 /\
  seat_cap eviction_weights = true /\
  sum (retained (only 4) eviction_weights) = 9 /\
  weigh (only 4) (retained (only 4) eviction_weights) = 0 /\
  quorum_weight (form_of (retained (only 4) eviction_weights)) = 7 /\
  minority (form_of (retained (only 4) eviction_weights)) = 3 /\
  weigh (only 0) (retained (only 4) eviction_weights) = 3 /\
  seat_cap (retained (only 4) eviction_weights) = false /\
  accepts Seatless true Quorum (outside (only 4)) (retained (only 4) eviction_weights) = true /\
  accepts Seatless true Quorum (outside (only 0)) (retained (only 4) eviction_weights) = false.
Proof.
  split; [apply positive_check; reflexivity|]. split.
  - change (NoDup [0%nat; 1%nat; 2%nat; 3%nat; 4%nat]).
    constructor; [simpl; intuition discriminate|].
    constructor; [simpl; intuition discriminate|].
    constructor; [simpl; intuition discriminate|].
    constructor; [simpl; intuition discriminate|].
    constructor; [simpl; tauto|constructor].
  - vm_compute. repeat split; reflexivity.
Qed.

Example eviction_subset_guard :
  let bad := only 0 in
  let gone := only 1 in
  let left := either (pair 0 1) (only 3) in
  let right := pair 0 2 in
  (3 * weigh bad eviction_weights <? sum eviction_weights) = true /\
  weigh gone eviction_weights = weigh bad eviction_weights /\
  accepts Seatless true Quorum left eviction_weights = true /\
  accepts Seatless true Quorum right (retained gone eviction_weights) = true /\
  weigh (both (outside bad) (both left right)) (retained gone eviction_weights) = 0 /\
  (3 * weigh bad (retained gone eviction_weights) <? sum (retained gone eviction_weights)) = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example eviction_edges :
  retained (outside all) eviction_weights = eviction_weights /\
  weigh (outside all) eviction_weights = 0 /\
  retained all eviction_weights = [] /\
  retained (only 0) [] = [] /\
  (3 * weigh (only 0) [] <? sum []) = false /\
  accepts Seatless true Quorum all [] = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example flat_rows :
  flat 1 seven /\
  map (fun yes => (row Seatless yes seven, row Stake yes seven))
    [below 1; below 3; below 4; below 5] =
  [([false; false; false; false; false; true; false],
    [false; false; false; false; false; false; false]);
   ([false; true; true; false; true; true; true],
    [false; true; true; false; true; false; false]);
   ([false; true; true; false; true; true; true],
    [false; true; true; false; true; true; true]);
   ([true; true; true; true; true; true; true],
    [true; true; true; true; true; true; true])] /\
  map (fun yes => (row Seatless yes [(0%nat, 5)], row Stake yes [(0%nat, 5)]))
    [all; outside all] =
  [([true; true; true; true; true; true; true],
    [true; true; true; true; true; true; true]);
   ([false; false; false; false; false; false; false],
    [false; false; false; false; false; false; true])].
Proof.
  split.
  - intros entry present. unfold seven, mint in present.
    apply in_map_iff in present as [id [same _]]. subst entry. reflexivity.
  - vm_compute. split; reflexivity.
Qed.

Example single_rows :
  forallb (fun unit =>
    accepts Seatless true Quorum all [(0%nat, unit)]
    && accepts Stake true Quorum all [(0%nat, unit)]
    && accepts Seatless true Skip all [(0%nat, unit)]
    && accepts Stake true Skip all [(0%nat, unit)]
    && negb (accepts Seatless true Quorum (outside all) [(0%nat, unit)])
    && negb (accepts Seatless true Skip (outside all) [(0%nat, unit)])
    && negb (accepts Stake true Quorum (outside all) [(0%nat, unit)]))
    [1; 2; 3; 4; 1000] = true.
Proof. vm_compute. reflexivity. Qed.

Example small_totals :
  map (fun weights =>
    (sum weights, quorum_weight (form_of weights), minority (form_of weights),
     accepts Seatless true Quorum (below 1) weights,
     accepts Seatless true Skip (below 1) weights))
    [[]; [(0%nat, 1)]; [(0%nat, 1); (1%nat, 1)]; [(0%nat, 2); (1%nat, 1)];
     [(0%nat, 3); (1%nat, 1)]; [(0%nat, 2); (1%nat, 2)]] =
  [(0, 0, 1, false, false); (1, 1, 1, true, true); (2, 2, 1, false, true);
   (3, 3, 1, false, true); (4, 3, 2, true, true); (4, 3, 2, false, true)].
Proof. vm_compute. reflexivity. Qed.

Definition eviction_choice gone weights :=
  if quota (sum weights) <=? sum weights - weigh gone weights
  then retained gone weights else weights.

Theorem eviction_wait_exact gone weights :
  sum weights - weigh gone weights < quota (sum weights) ->
  eviction_choice gone weights = weights.
Proof.
  intro short. unfold eviction_choice.
  destruct (quota (sum weights) <=? sum weights - weigh gone weights) eqn:check.
  - apply Z.leb_le in check. lia.
  - reflexivity.
Qed.

Theorem eviction_keeps_entries gone weights entry :
  In entry (eviction_choice gone weights) -> In entry weights.
Proof.
  unfold eviction_choice, retained.
  destruct (quota (sum weights) <=? sum weights - weigh gone weights).
  - intros present. apply filter_In in present. tauto.
  - auto.
Qed.

Theorem eviction_shared bad gone left right stated weights :
  positive weights ->
  (forall entry, In entry weights -> gone (fst entry) = true -> bad (fst entry) = true) ->
  3 * weigh bad weights < sum weights ->
  accepts Seatless stated Quorum left weights = true ->
  accepts Seatless stated Quorum right (eviction_choice gone weights) = true ->
  exists entry, In entry (eviction_choice gone weights) /\
    left (fst entry) = true /\ right (fst entry) = true /\ bad (fst entry) = false.
Proof.
  intros sign included budget first second.
  unfold eviction_choice in *.
  destruct (quota (sum weights) <=? sum weights - weigh gone weights).
  - exact (removal_shared bad gone left right stated weights sign included budget first second).
  - apply (shared_wide bad left right stated weights sign); try assumption.
    assert (0 < sum weights) as funded.
    { pose proof (score_sign snd bad weights (positive_sign weights sign)).
      unfold weigh in *. lia. }
    unfold form_of. rewrite shape_total, (shape_heavy _ _ funded).
    pose proof (quota_range _ funded). lia.
Qed.

Theorem slash_supply supply remaining retired bond reward maximum :
  0 <= bond -> 0 <= reward -> bond + reward <= supply ->
  supply + remaining + retired = maximum ->
  0 <= supply - bond - reward /\
  (supply - bond - reward) + remaining + (retired + bond + reward) = maximum.
Proof. intros. lia. Qed.

Example eviction_wait_edge :
  eviction_choice (below 3) seven = seven /\
  eviction_choice (below 4) seven = seven /\
  eviction_choice (below 2) seven = retained (below 2) seven.
Proof. vm_compute. repeat split; reflexivity. Qed.

Print Assumptions eviction_wait_exact.
Print Assumptions eviction_keeps_entries.
Print Assumptions eviction_shared.
Print Assumptions slash_supply.
Print Assumptions eviction_wait_edge.
Print Assumptions shared_wide.
Print Assumptions shared_sharp.
Print Assumptions skip_sharp.
Print Assumptions weight_only.
Print Assumptions seatless_closed.
Print Assumptions stake_shared_wide.
Print Assumptions stake_skip_honest.
Print Assumptions ahead_same.
Print Assumptions flat_same.
Print Assumptions flat_halt.
Print Assumptions few_halt.
Print Assumptions stake_cost.
Print Assumptions quorum_overlap.
Print Assumptions overlap_margin.
Print Assumptions seatless_shared.
Print Assumptions honest_reach.
Print Assumptions seatless_live.
Print Assumptions removal_fraction.
Print Assumptions removal_state.
Print Assumptions removal_nonempty.
Print Assumptions removal_shared.
Print Assumptions eviction_cap.
Print Assumptions eviction_subset_guard.
Print Assumptions eviction_edges.
Print Assumptions seat_cap_spec.
Print Assumptions seat_delta_preserves.
Print Assumptions seat_change_preserves.
Print Assumptions seat_ratio.
Print Assumptions seat_delta_cap.
Print Assumptions seatless_single_exact.
Print Assumptions seatless_single.
Print Assumptions seat_cap_single.
Print Assumptions seatless_single_heavy.
Print Assumptions seat_cap_edge.
Print Assumptions skip_dual.
Print Assumptions seatless_skip_dual.
Print Assumptions skip_honest.
Print Assumptions skip_least.
Print Assumptions stake_accepted.
Print Assumptions stake_shared.
Print Assumptions stake_live.
Print Assumptions equal_weights.
Print Assumptions stake_halt.
Print Assumptions seatless_halt.
Print Assumptions cheap_halt.