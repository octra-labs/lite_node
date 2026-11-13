(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import Arith Bool Lia List.

Import ListNotations.

Inductive decision := Defer (need : nat) | Measure | Known (need : nat).

Definition select reserve free epoch commit generation saved :=
  if free <? reserve then Defer reserve
  else match saved with
  | Some (saved_epoch, saved_commit, saved_generation, need, walked) =>
    if ((epoch =? saved_epoch) && (commit =? saved_commit))
        && (generation =? saved_generation) then
      if walked || (need <=? free) then Known need else Measure
    else Measure
  | None => Measure
  end.

Theorem reserve_refusal_never_scans : forall reserve free epoch commit generation saved,
  free < reserve -> select reserve free epoch commit generation saved = Defer reserve.
Proof.
  intros; unfold select; rewrite (proj2 (Nat.ltb_lt _ _) H); reflexivity.
Qed.

Theorem same_snapshot_is_not_measured :
  forall reserve free epoch commit generation need walked,
  reserve <= free -> (walked = true \/ need <= free) ->
  select reserve free epoch commit generation
    (Some (epoch, commit, generation, need, walked)) = Known need.
Proof.
  intros reserve free epoch commit generation need walked enough usable.
  unfold select. rewrite (proj2 (Nat.ltb_ge _ _) enough), !Nat.eqb_refl.
  assert (walked || (need <=? free) = true) as ready.
  { apply orb_true_iff. destruct usable as [precise|fits].
    - left. exact precise.
    - right. apply Nat.leb_le. exact fits. }
  rewrite ready. reflexivity.
Qed.

Theorem reused_size_names_same_snapshot :
  forall reserve free epoch commit generation se sc sg size walked need,
  select reserve free epoch commit generation (Some (se, sc, sg, size, walked))
    = Known need ->
  epoch = se /\ commit = sc /\ generation = sg /\ need = size.
Proof.
  intros reserve free epoch commit generation se sc sg size walked need H.
  unfold select in H.
  destruct (free <? reserve); [discriminate|].
  destruct (((epoch =? se) && (commit =? sc)) && (generation =? sg)) eqn:E;
    [|discriminate].
  destruct (walked || (size <=? free)); [|discriminate].
  inversion H; subst; apply andb_true_iff in E.
  destruct E as [pair Hg]. apply andb_true_iff in pair as [He Hc].
  apply Nat.eqb_eq in He, Hc, Hg. auto.
Qed.

Theorem gc_precision_or_fits :
  forall reserve free epoch commit generation se sc sg size walked need,
  select reserve free epoch commit generation (Some (se, sc, sg, size, walked))
    = Known need ->
  walked = true \/ need <= free.
Proof.
  intros reserve free epoch commit generation se sc sg size walked need accepted.
  unfold select in accepted.
  destruct (free <? reserve); [discriminate|].
  destruct (((epoch =? se) && (commit =? sc)) && (generation =? sg));
    [|discriminate].
  destruct (walked || (size <=? free)) eqn:usable; [|discriminate].
  inversion accepted; subst. apply orb_true_iff in usable.
  destruct usable as [precise|fits].
  - left. exact precise.
  - right. apply Nat.leb_le. exact fits.
Qed.

Theorem gc_precise_after_append :
  forall reserve before after epoch commit generation need,
  reserve <= after -> after <= before ->
  select reserve before epoch commit generation
    (Some (epoch, commit, generation, need, true)) = Known need /\
  select reserve after epoch commit generation
    (Some (epoch, commit, generation, need, true)) = Known need.
Proof.
  intros reserve before after epoch commit generation need enough consumed.
  split; apply same_snapshot_is_not_measured; try lia; left; reflexivity.
Qed.

Theorem gc_cheap_fits : forall reserve free epoch commit generation need,
  reserve <= free -> need <= free ->
  select reserve free epoch commit generation
    (Some (epoch, commit, generation, need, false)) = Known need.
Proof.
  intros reserve free epoch commit generation need enough fits.
  apply same_snapshot_is_not_measured; [exact enough|right; exact fits].
Qed.

Theorem gc_cheap_refines : forall reserve free epoch commit generation need,
  reserve <= free -> free < need ->
  select reserve free epoch commit generation
    (Some (epoch, commit, generation, need, false)) = Measure.
Proof.
  intros reserve free epoch commit generation need enough short.
  unfold select. rewrite (proj2 (Nat.ltb_ge _ _) enough), !Nat.eqb_refl.
  rewrite (proj2 (Nat.leb_gt _ _) short). reflexivity.
Qed.

Theorem gc_cheap_after_append :
  forall reserve before after epoch commit generation need,
  reserve <= after -> after < need -> need <= before ->
  select reserve before epoch commit generation
    (Some (epoch, commit, generation, need, false)) = Known need /\
  select reserve after epoch commit generation
    (Some (epoch, commit, generation, need, false)) = Measure.
Proof.
  intros reserve before after epoch commit generation need enough short fits.
  split.
  - apply gc_cheap_fits; lia.
  - apply gc_cheap_refines; assumption.
Qed.

Theorem gc_generation_changed :
  forall reserve free epoch commit generation saved need walked,
  reserve <= free -> generation <> saved ->
  select reserve free epoch commit generation
    (Some (epoch, commit, saved, need, walked)) = Measure.
Proof.
  intros reserve free epoch commit generation saved need walked enough changed.
  unfold select. rewrite (proj2 (Nat.ltb_ge _ _) enough), !Nat.eqb_refl.
  rewrite (proj2 (Nat.eqb_neq _ _) changed). reflexivity.
Qed.

Example gc_cache_precision :
  select 10 40 1 2 3 (Some (1, 2, 3, 50, false)) = Measure /\
  select 10 40 1 2 3 (Some (1, 2, 3, 50, true)) = Known 50 /\
  select 10 50 1 2 3 (Some (1, 2, 3, 50, false)) = Known 50 /\
  select 10 9 1 2 3 (Some (1, 2, 3, 50, true)) = Defer 10.
Proof. vm_compute. repeat split; reflexivity. Qed.

Definition can_start free need := need <=? free.

Theorem recheck_refuses_consumed_space : forall before after need,
  can_start before need = true -> after < need -> can_start after need = false.
Proof. intros; unfold can_start; now apply Nat.leb_gt. Qed.

Theorem space_recovery_allows_retry : forall before after need,
  can_start before need = false -> need <= after -> can_start after need = true.
Proof. intros; unfold can_start; now apply Nat.leb_le. Qed.

Fixpoint gc_bytes (entries : list (nat * nat)) : nat :=
  match entries with
  | [] => 0
  | (_, len) :: rest => len + gc_bytes rest
  end.

Fixpoint gc_coalesce (entries : list (nat * nat)) (off len : nat) :=
  match entries with
  | [] => [(off, len)]
  | (next, size) :: rest =>
    if next + size =? off then gc_coalesce rest next (size + len)
    else (off, len) :: gc_coalesce rest next size
  end.

Definition gc_ranges (entries : list (nat * nat)) :=
  match entries with
  | [] => []
  | (off, len) :: rest => gc_coalesce rest off len
  end.

Fixpoint gc_below (limit : nat) (entries : list (nat * nat)) : Prop :=
  match entries with
  | [] => True
  | (off, len) :: rest =>
    0 < len /\ off + len <= limit /\ gc_below off rest
  end.

Definition gc_ordered (entries : list (nat * nat)) :=
  match entries with
  | [] => True
  | (off, len) :: rest => 0 < len /\ gc_below off rest
  end.

Fixpoint gc_cover (point : nat) (entries : list (nat * nat)) : Prop :=
  match entries with
  | [] => False
  | (off, len) :: rest => off <= point < off + len \/ gc_cover point rest
  end.

Fixpoint gc_mapping (entries : list (nat * nat)) : nat :=
  match entries with
  | [] => 0
  | (_, len) :: rest =>
    if len =? 0 then gc_mapping rest else 3 * 8 + gc_mapping rest
  end.

Definition gc_tagged {A : Type} (entries : list ((nat * nat) * A)) :=
  gc_ranges (map fst entries).

Lemma gc_coalesce_bytes : forall entries off len,
  gc_bytes (gc_coalesce entries off len) = len + gc_bytes entries.
Proof.
  induction entries as [|[next size] rest ih]; intros off len.
  - reflexivity.
  - cbn [gc_coalesce].
    destruct (next + size =? off); cbn [gc_bytes]; rewrite ih;
      cbn [gc_bytes]; lia.
Qed.

Lemma gc_coalesce_count : forall entries off len,
  length (gc_coalesce entries off len) <= S (length entries).
Proof.
  induction entries as [|[next size] rest ih]; intros off len.
  - simpl. lia.
  - cbn [gc_coalesce]. destruct (next + size =? off).
    + specialize (ih next (size + len)). simpl in *. lia.
    + specialize (ih next size). simpl in *. lia.
Qed.

Lemma gc_coalesce_cover : forall entries off len point,
  gc_cover point (gc_coalesce entries off len) <->
  off <= point < off + len \/ gc_cover point entries.
Proof.
  induction entries as [|[next size] rest ih]; intros off len point.
  - simpl. tauto.
  - cbn [gc_coalesce gc_cover].
    destruct (next + size =? off) eqn:adjacent.
    + rewrite ih. apply Nat.eqb_eq in adjacent.
      destruct (le_lt_dec off point); intuition lia.
    + cbn [gc_cover]. rewrite ih. tauto.
Qed.

Lemma gc_coalesce_layout : forall entries off len limit,
  0 < len -> off + len <= limit -> gc_below off entries ->
  gc_below limit (gc_coalesce entries off len).
Proof.
  induction entries as [|[next size] rest ih]; intros off len limit positive fits ordered.
  - simpl. auto.
  - simpl in ordered. destruct ordered as [live [before rest_order]].
    cbn [gc_coalesce]. destruct (next + size =? off) eqn:adjacent.
    + apply Nat.eqb_eq in adjacent. apply ih; try assumption; lia.
    + cbn [gc_below]. split; [exact positive|].
      split; [exact fits|]. apply ih; assumption.
Qed.

Theorem gc_ranges_count : forall entries,
  length (gc_ranges entries) <= length entries.
Proof.
  intros [|[off len] rest].
  - simpl. lia.
  - apply gc_coalesce_count.
Qed.

Theorem gc_prefix_exact : forall entries,
  gc_ordered entries -> gc_bytes (gc_ranges entries) = gc_bytes entries.
Proof.
  intros [|[off len] rest] ordered.
  - reflexivity.
  - apply gc_coalesce_bytes.
Qed.

Theorem gc_ranges_cover : forall entries point,
  gc_cover point (gc_ranges entries) <-> gc_cover point entries.
Proof.
  intros [|[off len] rest] point.
  - simpl. tauto.
  - apply gc_coalesce_cover.
Qed.

Theorem gc_ranges_ordered : forall entries,
  gc_ordered entries -> gc_ordered (gc_ranges entries).
Proof.
  intros [|[off len] rest] ordered.
  - exact I.
  - destruct ordered as [positive rest_order].
    pose proof (gc_coalesce_layout rest off len (off + len)
      positive (Nat.le_refl _) rest_order) as kept.
    unfold gc_ranges, gc_ordered.
    destruct (gc_coalesce rest off len) as [|[start size] output]; simpl in *; tauto.
Qed.

Lemma gc_mapping_count : forall entries limit,
  gc_below limit entries -> gc_mapping entries = 24 * length entries.
Proof.
  induction entries as [|[off len] rest ih]; intros limit ordered.
  - reflexivity.
  - simpl in ordered. destruct ordered as [positive [fits rest_order]].
    cbn [gc_mapping].
    assert (len =? 0 = false) as nonzero by (apply Nat.eqb_neq; lia).
    rewrite nonzero, (ih off rest_order). simpl. lia.
Qed.

Theorem gc_mapping_exact : forall entries,
  gc_ordered entries -> gc_mapping entries = 24 * length entries.
Proof.
  intros [|[off len] rest] ordered.
  - reflexivity.
  - destruct ordered as [positive rest_order].
    apply gc_mapping_count with (limit := off + len).
    simpl. split; [exact positive|]. split; [lia|exact rest_order].
Qed.

Theorem gc_mapping_cap : forall entries,
  gc_ordered entries -> gc_mapping (gc_ranges entries) <= 24 * length entries.
Proof.
  intros entries ordered.
  rewrite (gc_mapping_exact (gc_ranges entries) (gc_ranges_ordered entries ordered)).
  pose proof (gc_ranges_count entries). lia.
Qed.

Theorem gc_total_cap : forall entries,
  gc_ordered entries ->
  gc_bytes (gc_ranges entries) + gc_mapping (gc_ranges entries)
    <= gc_bytes entries + 24 * length entries.
Proof.
  intros entries ordered. rewrite (gc_prefix_exact entries ordered).
  pose proof (gc_mapping_cap entries ordered). lia.
Qed.

Theorem gc_hash_irrelevant : forall (A : Type) entries (hash : (nat * nat) -> A),
  gc_tagged (map (fun entry => (entry, hash entry)) entries) = gc_ranges entries.
Proof.
  intros A entries hash. unfold gc_tagged. rewrite map_map.
  change (gc_ranges (map (fun entry : nat * nat => entry) entries) = gc_ranges entries).
  rewrite map_id. reflexivity.
Qed.

Example gc_empty :
  gc_ranges [] = [] /\ gc_bytes (gc_ranges []) = 0 /\ gc_mapping (gc_ranges []) = 0.
Proof. repeat split; reflexivity. Qed.

Example gc_adjacent :
  gc_ordered [(100, 10); (90, 10)] /\
  gc_ranges [(100, 10); (90, 10)] = [(90, 20)] /\
  gc_bytes (gc_ranges [(100, 10); (90, 10)]) = 20 /\
  gc_mapping (gc_ranges [(100, 10); (90, 10)]) = 24.
Proof. simpl. repeat split; try reflexivity; lia. Qed.

Example gc_gap :
  gc_ordered [(100, 10); (80, 10)] /\
  gc_ranges [(100, 10); (80, 10)] = [(100, 10); (80, 10)] /\
  gc_bytes (gc_ranges [(100, 10); (80, 10)]) = 20 /\
  gc_mapping (gc_ranges [(100, 10); (80, 10)]) = 48.
Proof. simpl. repeat split; try reflexivity; lia. Qed.

Example gc_same_hash :
  let entries := [((100, 10), 7); ((80, 10), 7)] in
  gc_tagged entries = [(100, 10); (80, 10)] /\
  gc_bytes (gc_tagged entries) = 20 /\ gc_mapping (gc_tagged entries) = 48.
Proof. vm_compute. repeat split; reflexivity. Qed.

Print Assumptions reserve_refusal_never_scans.
Print Assumptions same_snapshot_is_not_measured.
Print Assumptions reused_size_names_same_snapshot.
Print Assumptions gc_precision_or_fits.
Print Assumptions gc_precise_after_append.
Print Assumptions gc_cheap_fits.
Print Assumptions gc_cheap_refines.
Print Assumptions gc_cheap_after_append.
Print Assumptions gc_generation_changed.
Print Assumptions gc_cache_precision.
Print Assumptions gc_ranges_count.
Print Assumptions gc_prefix_exact.
Print Assumptions gc_ranges_cover.
Print Assumptions gc_ranges_ordered.
Print Assumptions gc_mapping_exact.
Print Assumptions gc_mapping_cap.
Print Assumptions gc_total_cap.
Print Assumptions gc_hash_irrelevant.
Print Assumptions gc_empty.
Print Assumptions gc_adjacent.
Print Assumptions gc_gap.
Print Assumptions gc_same_hash.