(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool Arith ZArith Lia.
Import ListNotations.

Section History.
Variable point : Type.

Inductive effect :=
  | Neutral
  | Known (step : point -> option point)
  | Missing
  | Invalid.

Fixpoint total (acc : point) (values : list effect) : option point :=
  match values with
  | [] => Some acc
  | Neutral :: rest => total acc rest
  | Known step :: rest =>
    match step acc with
    | None => None
    | Some next => total next rest
    end
  | Missing :: _ | Invalid :: _ => None
  end.

Theorem missing_refused :
  forall values acc, In Missing values -> total acc values = None.
Proof.
  induction values as [|value rest next]; intros acc present.
  - contradiction.
  - destruct present as [same|later].
    + subst value. reflexivity.
    + destruct value; simpl; try reflexivity; auto.
      destruct (step acc); auto.
Qed.

Theorem invalid_refused :
  forall values acc, In Invalid values -> total acc values = None.
Proof.
  induction values as [|value rest next]; intros acc present.
  - contradiction.
  - destruct present as [same|later].
    + subst value. reflexivity.
    + destruct value; simpl; try reflexivity; auto.
      destruct (step acc); auto.
Qed.

Theorem total_app :
  forall left right acc,
    total acc (left ++ right) =
    match total acc left with
    | None => None
    | Some next => total next right
    end.
Proof.
  induction left as [|value rest next]; intros right acc; simpl.
  - reflexivity.
  - destruct value; simpl; try reflexivity; auto.
    destruct (step acc); auto.
Qed.

Theorem refusal_retained :
  forall left right acc,
    total acc left = None -> total acc (left ++ right) = None.
Proof.
  intros left right acc refused.
  rewrite total_app, refused.
  reflexivity.
Qed.

Theorem success_has_no_gaps :
  forall values acc result,
    total acc values = Some result ->
    ~ In Missing values /\ ~ In Invalid values.
Proof.
  intros values acc result accepted.
  split; intro present.
  - rewrite (missing_refused values acc present) in accepted. discriminate.
  - rewrite (invalid_refused values acc present) in accepted. discriminate.
Qed.

Theorem neutral_unchanged :
  forall values acc, total acc (Neutral :: values) = total acc values.
Proof. reflexivity. Qed.

End History.

Module Links.

Record header := {
  epoch : nat;
  high : nat;
  signed_parent : bool
}.

Section Trace.
Variable linked : header -> header -> bool.

Fixpoint trace (current : header) (prior : list header) : bool :=
  match prior with
  | [] => true
  | next :: rest =>
    signed_parent current && linked current next &&
    (S (epoch next) =? epoch current) && (high next <=? high current) &&
    trace next rest
  end.

Theorem endpoint_allowed : forall current,
  trace current [] = true.
Proof. reflexivity. Qed.

Theorem unsigned_stops : forall current next rest,
  signed_parent current = false -> trace current (next :: rest) = false.
Proof. intros current next rest unsigned. simpl. rewrite unsigned. reflexivity. Qed.

Theorem step_valid : forall current next rest,
  trace current (next :: rest) = true ->
  signed_parent current = true /\ linked current next = true /\
  S (epoch next) = epoch current /\ high next <= high current /\
  trace next rest = true.
Proof.
  intros current next rest accepted.
  change (signed_parent current && linked current next &&
    (S (epoch next) =? epoch current) && (high next <=? high current) &&
    trace next rest = true) in accepted.
  apply andb_true_iff in accepted as [checks tail].
  apply andb_true_iff in checks as [checks ordered].
  apply andb_true_iff in checks as [checks consecutive].
  apply andb_true_iff in checks as [signed link].
  apply Nat.eqb_eq in consecutive. apply Nat.leb_le in ordered. tauto.
Qed.

Theorem depth_finite : forall prior current,
  trace current prior = true -> length prior <= epoch current.
Proof.
  induction prior as [|next rest later]; intros current accepted; simpl; try lia.
  apply step_valid in accepted as [_ [_ [consecutive [_ accepted]]]].
  specialize (later next accepted). lia.
Qed.

Theorem earlier_positions : forall prior current next,
  trace current prior = true -> In next prior ->
  epoch next < epoch current /\ high next <= high current.
Proof.
  induction prior as [|head rest later]; intros current next accepted present.
  - contradiction.
  - apply step_valid in accepted as [_ [_ [consecutive [ordered accepted]]]].
    destruct present as [same|present].
    + subst next. lia.
    + specialize (later head next accepted present). lia.
Qed.

Theorem epochs_unique : forall prior current,
  trace current prior = true -> NoDup (map epoch (current :: prior)).
Proof.
  induction prior as [|next rest later]; intros current accepted; simpl.
  - constructor; [simpl; tauto | constructor].
  - constructor.
    + intro present.
      change (In (epoch current) (map epoch (next :: rest))) in present.
      apply in_map_iff in present as [other [same present]].
      pose proof (earlier_positions (next :: rest) current other accepted present).
      lia.
    + apply later. apply step_valid in accepted as [_ [_ [_ [_ accepted]]]].
      exact accepted.
Qed.

End Trace.
End Links.

Module Account.
Open Scope Z_scope.

Record state := {
  seen : list nat;
  amount : Z;
  blind : Z
}.

Definition public (value : Z) (s : state) : state :=
  {| seen := seen s; amount := amount s + value; blind := blind s |}.

Definition send (value mask : Z) (s : state) : state :=
  {| seen := seen s; amount := amount s - value; blind := blind s - mask |}.

Definition claim (id : nat) (value mask : Z) (s : state) : option state :=
  if in_dec Nat.eq_dec id (seen s) then None
  else Some {| seen := id :: seen s;
    amount := amount s + value; blind := blind s + mask |}.

Theorem repeated_refused : forall id value mask s,
  In id (seen s) -> claim id value mask s = None.
Proof.
  intros id value mask s present. unfold claim.
  destruct (in_dec Nat.eq_dec id (seen s)); tauto.
Qed.

Theorem claim_once : forall id value mask s next,
  claim id value mask s = Some next ->
  claim id value mask next = None.
Proof.
  intros id value mask s next accepted. unfold claim in accepted.
  destruct (in_dec Nat.eq_dec id (seen s)); try discriminate.
  inversion accepted; subst next. apply repeated_refused. simpl; auto.
Qed.

Theorem unique_preserved : forall id value mask s next,
  NoDup (seen s) -> claim id value mask s = Some next -> NoDup (seen next).
Proof.
  intros id value mask s next unique accepted. unfold claim in accepted.
  destruct (in_dec Nat.eq_dec id (seen s)); try discriminate.
  inversion accepted; subst next. simpl. constructor; assumption.
Qed.

Theorem send_claim_cancel : forall id value mask s next,
  claim id value mask (send value mask s) = Some next ->
  amount next = amount s /\ blind next = blind s.
Proof.
  intros id value mask s next accepted. unfold claim in accepted.
  destruct (in_dec Nat.eq_dec id (seen (send value mask s))); try discriminate.
  inversion accepted; subst next. simpl. lia.
Qed.

Theorem claim_conservation : forall id value mask s next,
  claim id value mask s = Some next ->
  amount next = amount s + value /\ blind next = blind s + mask /\
  length (seen next) = S (length (seen s)).
Proof.
  intros id value mask s next accepted. unfold claim in accepted.
  destruct (in_dec Nat.eq_dec id (seen s)); try discriminate.
  inversion accepted; subst next. simpl. auto.
Qed.

Theorem claim_funds_withdraw : forall id value mask paid next,
  0 <= paid <= value ->
  claim id value mask {| seen := []; amount := 0; blind := 0 |} = Some next ->
  amount (public (- paid) next) = value - paid /\
  0 <= amount (public (- paid) next).
Proof.
  intros id value mask paid next range accepted.
  pose proof (claim_conservation id value mask _ next accepted) as [sum _].
  simpl in *. lia.
Qed.

End Account.