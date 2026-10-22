(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool Arith Lia.
Import ListNotations.

Definition context (key body : list nat) := length key :: key ++ body.

Theorem context_key : forall a b x y,
  context a x = context b y -> a = b.
Proof.
  intros a b x y equal. unfold context in equal.
  injection equal as size words.
  apply (f_equal (firstn (length a))) in words.
  rewrite !firstn_app in words.
  rewrite firstn_all, Nat.sub_diag in words. simpl in words.
  rewrite app_nil_r in words.
  rewrite size, firstn_all, Nat.sub_diag in words. simpl in words.
  rewrite app_nil_r in words. exact words.
Qed.

Fixpoint relation (key weights : list nat) :=
  match key, weights with
  | k :: ks, w :: ws => k * w + relation ks ws
  | _, _ => 0
  end.

Theorem context_relation : forall a b x y weights,
  context a x = context b y -> relation a weights = relation b weights.
Proof.
  intros a b x y weights same. rewrite (context_key a b x y same). reflexivity.
Qed.

Theorem digest_collision : forall (hash : list nat -> nat) a b x y,
  a <> b -> hash (context a x) = hash (context b y) ->
  exists left right, left <> right /\ hash left = hash right.
Proof.
  intros hash a b x y different same.
  exists (context a x), (context b y). split; auto.
  intro equal. apply different. eapply context_key; exact equal.
Qed.

Definition key_words head powers rest :=
  (length head :: head) ++ (length powers :: powers) ++ rest.

Lemma key_words_powers : forall head a b rest,
  key_words head a rest = key_words head b rest -> a = b.
Proof.
  intros head a b rest same. unfold key_words in same.
  apply app_inv_head in same. apply app_inv_tail in same.
  injection same as _ equal. exact equal.
Qed.

Theorem powg_change_needs_collision :
  forall (hash : list nat -> nat) head a b rest left right,
  a <> b ->
  hash (context (key_words head a rest) left) =
    hash (context (key_words head b rest) right) ->
  exists x y, x <> y /\ hash x = hash y.
Proof.
  intros hash head a b rest left right different same.
  eapply digest_collision.
  - intro equal. apply different. eapply key_words_powers. exact equal.
  - exact same.
Qed.

Definition selected activation epoch := activation <=? epoch.

Definition accept activation epoch supplied prior keyed :=
  if selected activation epoch then supplied && keyed else negb supplied && prior.

Theorem active_no_prior : forall activation epoch supplied prior,
  activation <= epoch -> accept activation epoch supplied prior false = false.
Proof.
  intros. unfold accept, selected.
  assert (activation <=? epoch = true) by (apply Nat.leb_le; assumption).
  rewrite H0. apply Bool.andb_false_r.
Qed.

Theorem prior_kept : forall activation epoch supplied prior keyed,
  epoch < activation ->
  accept activation epoch supplied prior keyed = negb supplied && prior.
Proof.
  intros activation epoch supplied prior keyed before.
  unfold accept, selected.
  apply Nat.leb_gt in before. rewrite before. reflexivity.
Qed.

Example missing_key_changes_relation :
  relation [2] [3] <> relation [4] [3].
Proof. simpl. lia. Qed.

Lemma context_rest : forall a b x y,
  context a x = context b y -> x = y.
Proof.
  intros a b x y equal.
  pose proof (context_key a b x y equal) as same. subst b.
  unfold context in equal. injection equal as words.
  apply app_inv_head in words. exact words.
Qed.

Fixpoint fields_code (fields : list (list nat)) :=
  match fields with
  | [] => []
  | field :: rest => context field (fields_code rest)
  end.

Lemma fields_code_inj : forall a b,
  length a = length b -> fields_code a = fields_code b -> a = b.
Proof.
  induction a as [|a rest IH]; intros [|b more] size same;
    simpl in size, same; try discriminate; auto.
  injection size as size.
  pose proof (context_key a b _ _ same) as head.
  pose proof (context_rest a b _ _ same) as suffix.
  rewrite head, (IH more size suffix). reflexivity.
Qed.

Definition key_frame domain fields :=
  context domain (length fields :: fields_code fields).

Theorem key_frame_inj : forall d e a b,
  key_frame d a = key_frame e b -> d = e /\ a = b.
Proof.
  intros d e a b same. unfold key_frame in same.
  split.
  - eapply context_key. exact same.
  - apply context_rest in same. injection same as size fields.
    apply fields_code_inj; assumption.
Qed.

Theorem field_change_needs_collision :
  forall (hash : list nat -> nat) d e a b,
  (d <> e \/ a <> b) -> hash (key_frame d a) = hash (key_frame e b) ->
  exists x y, x <> y /\ hash x = hash y.
Proof.
  intros hash d e a b different same.
  exists (key_frame d a), (key_frame e b). split; auto.
  intro equal. apply key_frame_inj in equal. tauto.
Qed.

Corollary added_field_needs_collision :
  forall (hash : list nat -> nat) d fields field,
  hash (key_frame d fields) = hash (key_frame d (fields ++ [field])) ->
  exists x y, x <> y /\ hash x = hash y.
Proof.
  intros hash d fields field same.
  eapply field_change_needs_collision; [right|exact same].
  intro equal. apply (f_equal (@length (list nat))) in equal.
  rewrite app_length in equal. simpl in equal. lia.
Qed.

Example field_split_distinct :
  key_frame [1] [[2; 3]; [4]] <> key_frame [1] [[2]; [3; 4]].
Proof.
  intro same. apply key_frame_inj in same. destruct same as [_ same].
  discriminate.
Qed.

Print Assumptions context_key.
Print Assumptions context_relation.
Print Assumptions digest_collision.
Print Assumptions key_words_powers.
Print Assumptions powg_change_needs_collision.
Print Assumptions active_no_prior.
Print Assumptions prior_kept.
Print Assumptions context_rest.
Print Assumptions fields_code_inj.
Print Assumptions key_frame_inj.
Print Assumptions field_change_needs_collision.
Print Assumptions added_field_needs_collision.

Fixpoint powers modulus ratio value count :=
  match count with
  | 0 => []
  | S n => value :: powers modulus ratio ((value * ratio) mod modulus) n
  end.

Fixpoint powers_ok modulus ratio value entries :=
  match entries with
  | [] => true
  | entry :: rest => (entry =? value) &&
      powers_ok modulus ratio ((value * ratio) mod modulus) rest
  end.

Theorem powers_ok_exact : forall modulus ratio value entries,
  powers_ok modulus ratio value entries = true <->
  entries = powers modulus ratio value (length entries).
Proof.
  intros modulus ratio value entries. revert value.
  induction entries as [|entry rest IH]; intro value; simpl.
  - tauto.
  - rewrite Bool.andb_true_iff, Nat.eqb_eq, IH.
    split.
    + intros [same more]. subst entry. now rewrite more at 1.
    + intro same. injection same as first more. auto.
Qed.

Definition series_ok modulus count entries :=
  (length entries =? count) &&
  match entries with
  | [] => false
  | [first] => false
  | first :: ratio :: rest =>
      (3 <=? count) && (0 <? ratio) && (ratio <? modulus) && negb (ratio =? 1) &&
      powers_ok modulus ratio 1 (first :: ratio :: rest) &&
      ((ratio ^ count) mod modulus =? 1)
  end.

Theorem series_ok_exact : forall modulus count first ratio rest,
  series_ok modulus count (first :: ratio :: rest) = true ->
  3 <= count /\ 0 < ratio /\ ratio < modulus /\ ratio <> 1 /\
  (ratio ^ count) mod modulus = 1 /\
  first :: ratio :: rest = powers modulus ratio 1 count.
Proof.
  intros modulus count first ratio rest accepted.
  unfold series_ok in accepted.
  apply Bool.andb_true_iff in accepted as [size valid].
  apply Nat.eqb_eq in size.
  repeat rewrite Bool.andb_true_iff in valid.
  destruct valid as [[[[[minimum positive] range] nonunit] valid] closed].
  apply Nat.leb_le in minimum.
  apply Nat.ltb_lt in positive. apply Nat.ltb_lt in range.
  apply Bool.negb_true_iff in nonunit. apply Nat.eqb_neq in nonunit.
  apply Nat.eqb_eq in closed.
  apply powers_ok_exact in valid. rewrite size in valid. repeat split; assumption.
Qed.

Lemma powers_length : forall modulus ratio value count,
  length (powers modulus ratio value count) = count.
Proof.
  intros modulus ratio value count. revert value.
  induction count; intro value; simpl; auto.
Qed.

Theorem series_generated : forall modulus ratio count,
  0 < ratio -> ratio < modulus -> 3 <= count -> ratio <> 1 ->
  (ratio ^ count) mod modulus = 1 ->
  series_ok modulus count (powers modulus ratio 1 count) = true.
Proof.
  intros modulus ratio count positive small size nonunit closed.
  destruct count as [|[|count]]; try lia.
  unfold series_ok. rewrite powers_length, Nat.eqb_refl.
  cbn [powers]. rewrite Nat.mul_1_l, (Nat.mod_small ratio modulus small).
  rewrite Bool.andb_true_l.
  repeat rewrite Bool.andb_true_iff. repeat split.
  - apply Nat.leb_le. exact size.
  - apply Nat.ltb_lt. exact positive.
  - apply Nat.ltb_lt. exact small.
  - apply Bool.negb_true_iff. apply Nat.eqb_neq. exact nonunit.
  - apply powers_ok_exact. cbn [length]. rewrite powers_length.
    cbn [powers]. rewrite Nat.mul_1_l, (Nat.mod_small ratio modulus small).
    reflexivity.
  - apply Nat.eqb_eq. exact closed.
Qed.

Example short_series_refused : series_ok 7 2 [1; 6] = false.
Proof. reflexivity. Qed.

Example unit_ratio_refused : series_ok 7 3 [1; 1; 1] = false.
Proof. reflexivity. Qed.

Example open_cycle_refused : series_ok 7 3 [1; 3; 2] = false.
Proof. reflexivity. Qed.

Example cyclic_series_accepted : series_ok 7 3 [1; 2; 4] = true.
Proof. reflexivity. Qed.

Example divisor_cycle_accepted : series_ok 7 4 [1; 6; 1; 6] = true.
Proof. reflexivity. Qed.

Definition register_key active shape series :=
  shape && (negb active || series).

Theorem register_prior : forall shape series,
  register_key false shape series = shape.
Proof. intros; unfold register_key; simpl; apply Bool.andb_true_r. Qed.

Theorem register_active : forall shape series,
  register_key true shape series = true -> shape = true /\ series = true.
Proof. intros; unfold register_key in H; simpl in H; now apply Bool.andb_true_iff. Qed.

Definition register_at activation epoch shape series :=
  register_key (selected activation epoch) shape series.

Theorem register_before : forall activation epoch shape series,
  epoch < activation -> register_at activation epoch shape series = shape.
Proof.
  intros. unfold register_at, selected.
  assert (activation <=? epoch = false) as before by
    (apply Nat.leb_gt; assumption).
  rewrite before. apply register_prior.
Qed.

Theorem register_after : forall activation epoch shape series,
  activation <= epoch -> register_at activation epoch shape series = true ->
  shape = true /\ series = true.
Proof.
  intros. unfold register_at, selected in H0.
  assert (activation <=? epoch = true) as after by
    (apply Nat.leb_le; assumption).
  rewrite after in H0. now apply register_active in H0.
Qed.

Definition install_key active shape series (before after : list nat) :=
  if register_key active shape series then after else before.

Theorem rejected_key_kept : forall shape before after,
  install_key true shape false before after = before.
Proof. intros; unfold install_key, register_key; destruct shape; reflexivity. Qed.

Theorem switched_proof_key : forall active shape series before after body other rest,
  register_key active shape series = true ->
  context (install_key active shape series before after) body = context other rest ->
  after = other.
Proof.
  intros active shape series before after body other rest accepted same.
  unfold install_key in same. rewrite accepted in same.
  eapply context_key. exact same.
Qed.

Theorem switched_digest_collision :
  forall (hash : list nat -> nat) active shape series before after body other rest,
  register_key active shape series = true -> after <> other ->
  hash (context (install_key active shape series before after) body) =
    hash (context other rest) ->
  exists x y, x <> y /\ hash x = hash y.
Proof.
  intros hash active shape series before after body other rest accepted different same.
  unfold install_key in same. rewrite accepted in same.
  eapply digest_collision; eauto.
Qed.

Definition verify_stored activation epoch shape series proof :=
  register_at activation epoch shape series && proof.

Theorem stored_series_refused : forall activation epoch shape proof,
  activation <= epoch -> verify_stored activation epoch shape false proof = false.
Proof.
  intros activation epoch shape proof after.
  unfold verify_stored, register_at, selected, register_key.
  apply Nat.leb_le in after. rewrite after. simpl.
  destruct shape; reflexivity.
Qed.

Theorem stored_prior_kept : forall activation epoch shape series proof,
  epoch < activation ->
  verify_stored activation epoch shape series proof = shape && proof.
Proof.
  intros activation epoch shape series proof before.
  unfold verify_stored. rewrite (register_before activation epoch shape series before).
  reflexivity.
Qed.

Theorem stored_valid_passes : forall activation epoch,
  verify_stored activation epoch true true true = true.
Proof.
  intros activation epoch. unfold verify_stored, register_at, register_key.
  destruct (selected activation epoch); reflexivity.
Qed.

Definition owner_gate active shape series proof :=
  shape && (negb active || (series && proof)).

Inductive owner_field :=
| OwnerText : nat -> owner_field
| OwnerNull : owner_field
| OwnerOther : owner_field.

Definition owner_value value : option (option nat) :=
  match value with
  | OwnerText word => Some (Some word)
  | OwnerNull => Some None
  | OwnerOther => None
  end.

Definition read_owner (active unique : bool) (fields : list owner_field) :=
  if active then
    match fields with
    | [] => Some None
    | [value] => owner_value value
    | value :: _ :: _ => if unique then None else owner_value value
    end
  else Some None.

Theorem owner_fields_prior : forall unique fields,
  read_owner false unique fields = read_owner false unique [].
Proof. reflexivity. Qed.

Theorem owner_fields_bad_type : forall unique fields,
  read_owner true unique (OwnerOther :: fields) = None.
Proof.
  intros unique [|value fields]; simpl; [reflexivity|].
  destruct unique; reflexivity.
Qed.

Theorem owner_fields_repeat : forall first second rest,
  read_owner true true (first :: second :: rest) = None.
Proof. reflexivity. Qed.

Theorem owner_fields_first : forall first rest,
  read_owner true false (first :: rest) = owner_value first.
Proof. intros first [|second rest]; reflexivity. Qed.

Theorem owner_fields_one : forall fields word,
  read_owner true true fields = Some (Some word) ->
  fields = [OwnerText word].
Proof.
  intros [|first [|second rest]] word accepted; simpl in accepted;
    try discriminate.
  destruct first; simpl in accepted; try discriminate.
  injection accepted as equal. subst. reflexivity.
Qed.

Theorem owner_gate_prior : forall shape series proof,
  owner_gate false shape series proof = shape.
Proof.
  intros. unfold owner_gate. simpl. apply Bool.andb_true_r.
Qed.

Theorem owner_gate_active : forall shape series proof,
  owner_gate true shape series proof = true ->
  shape = true /\ series = true /\ proof = true.
Proof.
  intros shape series proof accepted.
  unfold owner_gate in accepted. simpl in accepted.
  apply Bool.andb_true_iff in accepted as [valid_shape rest].
  apply Bool.andb_true_iff in rest as [valid_series valid_proof].
  repeat split; assumption.
Qed.

Theorem owner_gate_missing : forall shape series,
  owner_gate true shape series false = false.
Proof.
  intros. unfold owner_gate. destruct shape, series; reflexivity.
Qed.

Theorem owner_address_change :
  forall (hash : list nat -> nat) (address other key : list nat),
  address <> other ->
  hash (context address key) = hash (context other key) ->
  exists x y, x <> y /\ hash x = hash y.
Proof.
  intros hash address other key different same.
  eapply digest_collision; eauto.
Qed.

Definition switch_gate old_shape old_proof new_shape new_series new_proof owner same :=
  old_shape && old_proof && new_shape && new_series && new_proof && owner && same.

Theorem switch_requires_proofs : forall old_shape old_proof new_shape new_series
  new_proof owner same,
  switch_gate old_shape old_proof new_shape new_series new_proof owner same = true ->
  old_shape = true /\ old_proof = true /\ new_shape = true /\
  new_series = true /\ new_proof = true /\ owner = true /\ same = true.
Proof.
  intros old_shape old_proof new_shape new_series new_proof owner same accepted.
  unfold switch_gate in accepted.
  repeat rewrite Bool.andb_true_iff in accepted.
  tauto.
Qed.

Theorem switch_stored_key : forall old_series,
  switch_gate true true true true true true true = true /\
  verify_stored 1 1 true old_series true = old_series.
Proof.
  intros old_series. destruct old_series; split; reflexivity.
Qed.

Theorem switch_amount : forall (commit : nat -> nat -> nat)
  old_amount old_blind new_amount new_blind,
  (forall a x b y, commit a x = commit b y -> a = b) ->
  commit old_amount old_blind = commit new_amount new_blind ->
  old_amount = new_amount.
Proof.
  intros commit old_amount old_blind new_amount new_blind binding same.
  eapply binding. exact same.
Qed.

Theorem switch_value : forall (commit : nat -> nat -> nat)
  old_amount old_blind new_amount new_blind shared
  old_shape old_proof new_shape new_series new_proof owner same,
  (forall a x b y, commit a x = commit b y -> a = b) ->
  switch_gate old_shape old_proof new_shape new_series new_proof owner same = true ->
  (old_proof = true -> commit old_amount old_blind = shared) ->
  (new_proof = true -> commit new_amount new_blind = shared) ->
  old_amount = new_amount.
Proof.
  intros commit old_amount old_blind new_amount new_blind shared
    old_shape old_proof new_shape new_series new_proof owner same
    binding accepted old_sound new_sound.
  apply switch_requires_proofs in accepted.
  destruct accepted as [_ [old_ok [_ [_ [new_ok _]]]]].
  eapply switch_amount; [exact binding|].
  rewrite (old_sound old_ok), (new_sound new_ok). reflexivity.
Qed.

Theorem switch_supply : forall public old_amount new_amount,
  old_amount = new_amount ->
  public + old_amount = public + new_amount.
Proof. intros; congruence. Qed.

Print Assumptions powers_ok_exact.
Print Assumptions series_ok_exact.
Print Assumptions powers_length.
Print Assumptions series_generated.
Print Assumptions register_prior.
Print Assumptions register_active.
Print Assumptions register_before.
Print Assumptions register_after.
Print Assumptions rejected_key_kept.
Print Assumptions switched_proof_key.
Print Assumptions switched_digest_collision.
Print Assumptions stored_series_refused.
Print Assumptions stored_prior_kept.
Print Assumptions stored_valid_passes.
Print Assumptions owner_gate_prior.
Print Assumptions owner_gate_active.
Print Assumptions owner_gate_missing.
Print Assumptions owner_address_change.
Print Assumptions switch_requires_proofs.
Print Assumptions switch_stored_key.
Print Assumptions switch_amount.
Print Assumptions switch_value.
Print Assumptions switch_supply.