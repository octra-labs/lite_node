(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Bool Lia.
Import ListNotations.

Definition byte_ok (value : nat) := value < 256.
Definition pair_encode (value : nat) := (value / 16, value mod 16).
Definition pair_decode (pair : nat * nat) :=
  let '(high, low) := pair in
  if (high <? 16) && (low <? 16) then Some (16 * high + low) else None.

Fixpoint decode (pairs : list (nat * nat)) : option (list nat) :=
  match pairs with
  | [] => Some []
  | first :: rest =>
    match pair_decode first, decode rest with
    | Some value, Some values => Some (value :: values)
    | _, _ => None
    end
  end.

Definition encode := map pair_encode.

Lemma byte_roundtrip : forall value,
  byte_ok value -> pair_decode (pair_encode value) = Some value.
Proof.
  intros value Hvalue.
  assert (Hhigh : value / 16 < 16).
  { apply Nat.div_lt_upper_bound; unfold byte_ok in Hvalue; lia. }
  assert (Hlow : value mod 16 < 16).
  { apply Nat.mod_upper_bound; lia. }
  apply Nat.ltb_lt in Hhigh.
  apply Nat.ltb_lt in Hlow.
  unfold pair_decode, pair_encode.
  rewrite Hhigh, Hlow.
  change (Some (16 * (value / 16) + value mod 16) = Some value).
  f_equal. symmetry. apply Nat.div_mod. lia.
Qed.

Theorem bytes_roundtrip : forall values,
  Forall byte_ok values -> decode (encode values) = Some values.
Proof.
  intros values Hvalues. induction Hvalues; cbn [encode map decode]; auto.
  rewrite byte_roundtrip by assumption.
  rewrite IHHvalues. reflexivity.
Qed.

Theorem encoding_injective : forall left right,
  Forall byte_ok left -> Forall byte_ok right ->
  encode left = encode right -> left = right.
Proof.
  intros left right Hleft Hright Hequal.
  apply (f_equal decode) in Hequal.
  rewrite bytes_roundtrip in Hequal by assumption.
  rewrite bytes_roundtrip in Hequal by assumption.
  inversion Hequal. reflexivity.
Qed.

Definition check_root (values : list nat) :=
  if (length values =? 32) && forallb (fun value => value <? 256) values
  then Some values else None.

Inductive representation :=
  | Raw (values : list nat)
  | Hex (pairs : list (nat * nat))
  | LedgerHex (pairs : list (nat * nat)).

Definition normalize (input : representation) :=
  match input with
  | Raw values => check_root values
  | Hex pairs => match decode pairs with
                 | Some values => check_root values
                 | None => None
                 end
  | LedgerHex pairs => match decode pairs with
                       | Some values => if length values =? 64
                           then check_root (firstn 32 values) else None
                       | None => None
                       end
  end.

Lemma root_valid : forall values,
  length values = 32 -> Forall byte_ok values -> check_root values = Some values.
Proof.
  intros values Hlength Hvalues.
  assert (Hall : forallb (fun value => value <? 256) values = true).
  { apply forallb_forall. intros value Hin.
    apply Nat.ltb_lt. rewrite Forall_forall in Hvalues. apply Hvalues. exact Hin. }
  unfold check_root. rewrite Hlength, Hall. reflexivity.
Qed.

Theorem representation_agrees : forall values,
  length values = 32 -> Forall byte_ok values ->
  normalize (Raw values) = Some values /\
  normalize (Hex (encode values)) = Some values.
Proof.
  intros values Hlength Hvalues. split; simpl.
  - apply root_valid; assumption.
  - rewrite bytes_roundtrip by assumption. apply root_valid; assumption.
Qed.

Theorem wrong_width_refused : forall values,
  length values <> 32 -> Forall byte_ok values ->
  normalize (Raw values) = None /\ normalize (Hex (encode values)) = None.
Proof.
  intros values Hlength Hvalues.
  assert (Htest : (length values =? 32) = false) by (apply Nat.eqb_neq; assumption).
  split; simpl; unfold check_root; rewrite ?bytes_roundtrip by assumption;
    rewrite Htest; reflexivity.
Qed.

Definition execute {State Result : Type}
    (step : list nat -> State -> Result) (input : representation) (state : State) :=
  match normalize input with Some root => Some (step root state) | None => None end.

Theorem execution_agrees : forall State Result (step : list nat -> State -> Result)
    values state,
  length values = 32 -> Forall byte_ok values ->
  execute step (Raw values) state = execute step (Hex (encode values)) state.
Proof.
  intros State Result step values state Hlength Hvalues.
  destruct (representation_agrees values Hlength Hvalues) as [Hraw Hhex].
  unfold execute. rewrite Hraw, Hhex. reflexivity.
Qed.

Example overwide_digit_refused : pair_decode (16, 0) = None.
Proof. reflexivity. Qed.

Example missing_root_refused : normalize (Raw []) = None.
Proof. reflexivity. Qed.

Theorem ledger_projection_agrees : forall prefix suffix,
  length prefix = 32 -> length suffix = 32 ->
  Forall byte_ok prefix -> Forall byte_ok suffix ->
  normalize (LedgerHex (encode (prefix ++ suffix))) = Some prefix.
Proof.
  intros prefix suffix Hp Hs Vp Vs. simpl.
  rewrite bytes_roundtrip by (apply Forall_app; auto).
  rewrite app_length, Hp, Hs.
  change (check_root (firstn 32 (prefix ++ suffix)) = Some prefix).
  replace 32 with (length prefix) at 1 by lia.
  rewrite firstn_app, firstn_all, Nat.sub_diag. simpl.
  rewrite app_nil_r. apply root_valid; assumption.
Qed.

Theorem ledger_execution_agrees : forall State Result
    (step : list nat -> State -> Result) prefix suffix state,
  length prefix = 32 -> length suffix = 32 ->
  Forall byte_ok prefix -> Forall byte_ok suffix ->
  execute step (Raw prefix) state =
    execute step (LedgerHex (encode (prefix ++ suffix))) state.
Proof.
  intros State Result step prefix suffix state Hp Hs Vp Vs.
  unfold execute. rewrite ledger_projection_agrees by assumption.
  simpl. rewrite root_valid by assumption. reflexivity.
Qed.

Theorem invalid_ledger_digits pairs :
  decode pairs = None -> normalize (LedgerHex pairs) = None.
Proof. intros H. simpl. now rewrite H. Qed.

Print Assumptions ledger_projection_agrees.
Print Assumptions ledger_execution_agrees.