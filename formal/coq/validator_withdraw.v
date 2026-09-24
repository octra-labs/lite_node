(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia.

Open Scope Z_scope.

Record custody := {
  liquid : Z;
  escrow : Z;
  fees : Z;
  nonce : Z;
  bond : option Z;
  exited : option Z
}.

Definition epoch_limit := 9223372036854775807.

Definition ready now delay fee amount epoch state :=
  (0 <? amount) && (0 <=? epoch) && (0 <=? delay) &&
  (epoch + delay <=? epoch_limit) && (epoch + delay <=? now) &&
  (0 <=? fee) && (fee <=? liquid state) && (amount <=? escrow state).

Definition pay fee amount state := {|
  liquid := liquid state - fee + amount;
  escrow := escrow state - amount;
  fees := fees state + fee;
  nonce := nonce state + 1;
  bond := None;
  exited := None
|}.

Definition release now delay fee (active : bool) state :=
  if active then None
  else
    match bond state, exited state with
    | Some amount, Some epoch =>
        if ready now delay fee amount epoch state
        then Some (pay fee amount state)
        else None
    | _, _ => None
    end.

Definition total state := liquid state + escrow state + fees state.

Theorem pay_conserves_value :
  forall state fee amount,
    total (pay fee amount state) = total state.
Proof.
  intros state fee amount.
  unfold total, pay.
  simpl.
  lia.
Qed.

Theorem release_requires_evidence :
  forall now delay fee active state next,
    release now delay fee active state = Some next ->
    active = false /\
    exists amount epoch,
      bond state = Some amount /\ exited state = Some epoch /\
      epoch + delay <= epoch_limit /\ epoch + delay <= now.
Proof.
  intros now delay fee active state next applied.
  unfold release in applied.
  destruct active; try discriminate.
  destruct (bond state) as [amount|] eqn:bonded; try discriminate.
  destruct (exited state) as [epoch|] eqn:exit_record; try discriminate.
  destruct (ready now delay fee amount epoch state) eqn:allowed; try discriminate.
  split; [reflexivity|].
  exists amount, epoch.
  unfold ready in allowed.
  repeat rewrite Bool.andb_true_iff in allowed.
  repeat rewrite Z.leb_le in allowed.
  repeat rewrite Z.ltb_lt in allowed.
  repeat split; try assumption; tauto.
Qed.

Theorem release_conserves_value :
  forall now delay fee active state next,
    release now delay fee active state = Some next ->
    total next = total state.
Proof.
  intros now delay fee active state next applied.
  unfold release in applied.
  destruct active; try discriminate.
  destruct (bond state) as [amount|]; try discriminate.
  destruct (exited state) as [epoch|]; try discriminate.
  destruct (ready now delay fee amount epoch state); try discriminate.
  inversion applied; subst.
  apply pay_conserves_value.
Qed.

Theorem release_is_single_use :
  forall now delay fee active state next later later_delay later_fee member,
    release now delay fee active state = Some next ->
    release later later_delay later_fee member next = None.
Proof.
  intros now delay fee active state next later later_delay later_fee member applied.
  unfold release in applied.
  destruct active; try discriminate.
  destruct (bond state) as [amount|]; try discriminate.
  destruct (exited state) as [epoch|]; try discriminate.
  destruct (ready now delay fee amount epoch state); try discriminate.
  inversion applied; subst.
  unfold release, pay.
  destruct member; reflexivity.
Qed.

Theorem release_advances_nonce :
  forall now delay fee active state next,
    release now delay fee active state = Some next ->
    nonce next = nonce state + 1.
Proof.
  intros now delay fee active state next applied.
  unfold release in applied.
  destruct active; try discriminate.
  destruct (bond state) as [amount|]; try discriminate.
  destruct (exited state) as [epoch|]; try discriminate.
  destruct (ready now delay fee amount epoch state); try discriminate.
  inversion applied; reflexivity.
Qed.

Theorem release_nonnegative :
  forall now delay fee active state next,
    0 <= fees state ->
    release now delay fee active state = Some next ->
    0 <= liquid next /\ 0 <= escrow next /\ 0 <= fees next.
Proof.
  intros now delay fee active state next fees_positive applied.
  unfold release in applied.
  destruct active; try discriminate.
  destruct (bond state) as [amount|]; try discriminate.
  destruct (exited state) as [epoch|]; try discriminate.
  destruct (ready now delay fee amount epoch state) eqn:allowed; try discriminate.
  inversion applied; subst.
  unfold ready in allowed.
  repeat rewrite Bool.andb_true_iff in allowed.
  repeat rewrite Z.leb_le in allowed.
  repeat rewrite Z.ltb_lt in allowed.
  unfold pay; simpl.
  intuition lia.
Qed.

Definition receipt_current saved state :=
  match bond state with
  | None => nonce state =? saved
  | Some _ => false
  end.

Theorem receipt_requires_state :
  forall saved state,
    receipt_current saved state = true ->
    bond state = None /\ nonce state = saved.
Proof.
  intros saved state accepted.
  unfold receipt_current in accepted.
  destruct (bond state) eqn:registered; try discriminate.
  apply Z.eqb_eq in accepted.
  auto.
Qed.

Theorem receipt_rejects_progress :
  forall saved state,
    saved < nonce state -> receipt_current saved state = false.
Proof.
  intros saved state advanced.
  unfold receipt_current.
  destruct (bond state); [reflexivity|].
  apply Z.eqb_neq.
  lia.
Qed.

Theorem release_has_receipt :
  forall now delay fee active state next,
    release now delay fee active state = Some next ->
    receipt_current (nonce state + 1) next = true.
Proof.
  intros now delay fee active state next applied.
  unfold release in applied.
  destruct active; try discriminate.
  destruct (bond state) as [amount|]; try discriminate.
  destruct (exited state) as [epoch|]; try discriminate.
  destruct (ready now delay fee amount epoch state); try discriminate.
  inversion applied; subst.
  unfold receipt_current, pay.
  simpl.
  apply Z.eqb_refl.
Qed.

Print Assumptions receipt_requires_state.
Print Assumptions receipt_rejects_progress.
Print Assumptions release_has_receipt.

Definition exit_delay (active : bool) := if active then 8192 else 172800.
Definition exit_span (active : bool) := if active then 4096 else 86400.

Definition epoch_release gate now fee member state :=
  release now (exit_delay (gate <=? now)) fee member state.

Theorem prior_exit : forall gate now fee member state,
  now < gate ->
  epoch_release gate now fee member state = release now 172800 fee member state.
Proof.
  intros gate now fee member state before.
  unfold epoch_release, exit_delay.
  assert (gate <=? now = false) as mode by (apply Z.leb_gt; lia).
  rewrite mode. reflexivity.
Qed.

Theorem active_exit : forall gate now fee member state,
  gate <= now ->
  epoch_release gate now fee member state = release now 8192 fee member state.
Proof.
  intros gate now fee member state after.
  unfold epoch_release, exit_delay.
  assert (gate <=? now = true) as mode by (apply Z.leb_le; lia).
  rewrite mode. reflexivity.
Qed.

Theorem exit_reserve : forall active,
  0 < exit_span active /\ exit_delay active = 2 * exit_span active.
Proof. intros []; unfold exit_span, exit_delay; lia. Qed.

Theorem epoch_exit_conserves : forall gate now fee member state next,
  epoch_release gate now fee member state = Some next -> total next = total state.
Proof.
  intros gate now fee member state next applied.
  unfold epoch_release in applied.
  eapply release_conserves_value; exact applied.
Qed.

Theorem epoch_exit_once : forall gate now fee member state next later cost active,
  epoch_release gate now fee member state = Some next ->
  epoch_release gate later cost active next = None.
Proof.
  intros gate now fee member state next later cost active applied.
  unfold epoch_release in *.
  eapply release_is_single_use; exact applied.
Qed.

Print Assumptions prior_exit.
Print Assumptions active_exit.
Print Assumptions exit_reserve.
Print Assumptions epoch_exit_conserves.
Print Assumptions epoch_exit_once.
Print Assumptions pay_conserves_value.
Print Assumptions release_requires_evidence.
Print Assumptions release_conserves_value.
Print Assumptions release_is_single_use.
Print Assumptions release_advances_nonce.
Print Assumptions release_nonnegative.

Definition fund seq amount fee state :=
  match exited state with
  | Some _ => None
  | None =>
    if (seq =? nonce state + 1) && (0 <? amount) && (0 <=? fee) &&
       (amount + fee <=? liquid state) then
      Some {|
        liquid := liquid state - amount - fee;
        escrow := escrow state + amount;
        fees := fees state + fee;
        nonce := seq;
        bond := Some (amount + match bond state with Some prior => prior | None => 0 end);
        exited := None
      |}
    else None
  end.

Theorem fund_conserves : forall seq amount fee state next,
  fund seq amount fee state = Some next -> total next = total state.
Proof.
  intros seq amount fee state next applied.
  unfold fund in applied.
  destruct (exited state); try discriminate.
  destruct (_ && _ && _ && _)%bool; try discriminate.
  inversion applied; subst. unfold total; simpl. lia.
Qed.

Theorem fund_retry_once : forall seq amount fee state next later cost,
  fund seq amount fee state = Some next -> fund seq later cost next = None.
Proof.
  intros seq amount fee state next later cost applied.
  unfold fund in applied.
  destruct (exited state); try discriminate.
  destruct (_ && _ && _ && _)%bool; try discriminate.
  inversion applied; subst. unfold fund; simpl.
  assert (seq =? seq + 1 = false) as different by (apply Z.eqb_neq; lia).
  rewrite different. reflexivity.
Qed.

Print Assumptions fund_conserves.
Print Assumptions fund_retry_once.