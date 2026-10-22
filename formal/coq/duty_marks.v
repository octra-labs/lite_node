(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool Arith Lia.
Import ListNotations.

Record anchor := { chain : nat; set_id : nat; round : nat; block : nat }.
Record keys := { key_chain : nat; key_set : nat; key : nat }.
Record claim := { epoch : nat; signature : nat }.

Section Marks.
Variable finals : nat -> option anchor.
Variable history : nat -> option keys.
Variable verify : nat -> nat -> nat -> nat -> nat -> nat -> bool.
Variable network now age : nat.

Definition valid (c : claim) : bool :=
  match finals (epoch c), history (epoch c) with
  | Some f, Some k =>
    (epoch c + 2 <=? now) && (now - epoch c <=? age) &&
    Nat.eqb (chain f) network && Nat.eqb (key_chain k) network &&
    Nat.eqb (key_set k) (set_id f) &&
    verify (key k) network (epoch c) (round f) (block f) (signature c)
  | _, _ => false
  end.

Definition sendable (c : claim) (delay : nat) : Prop :=
  epoch c + 2 <= now /\ now + delay <= epoch c + age.

Theorem delivery_time : forall c delay step,
  sendable c delay -> step <= delay ->
  epoch c + 2 <= now + step /\ now + step - epoch c <= age.
Proof.
  intros c delay step [ready open] within. lia.
Qed.

Definition write (cs : list claim) (old : nat -> bool) (e : nat) : bool :=
  existsb (fun c => Nat.eqb (epoch c) e) cs || old e.

Definition apply (cs : list claim) (old : nat -> bool)
    : option (nat -> bool) :=
  if forallb valid cs then Some (write cs old) else None.

Theorem missing_context : forall c,
  finals (epoch c) = None \/ history (epoch c) = None -> valid c = false.
Proof.
  intros c [H | H]; unfold valid; rewrite H; auto.
  destruct (finals (epoch c)); reflexivity.
Qed.

Theorem wrong_set : forall c f k,
  finals (epoch c) = Some f -> history (epoch c) = Some k ->
  key_set k <> set_id f -> valid c = false.
Proof.
  intros c f k F K different. unfold valid. rewrite F, K.
  apply Nat.eqb_neq in different. rewrite different. simpl.
  rewrite andb_false_r. reflexivity.
Qed.

Theorem signature_required : forall c f k,
  finals (epoch c) = Some f -> history (epoch c) = Some k ->
  valid c = true ->
  verify (key k) network (epoch c) (round f) (block f) (signature c) = true.
Proof.
  intros c f k F K accepted. unfold valid in accepted. rewrite F, K in accepted.
  apply andb_true_iff in accepted. tauto.
Qed.

Theorem batch_atomic : forall cs old c,
  In c cs -> valid c = false -> apply cs old = None.
Proof.
  intros cs old c inside bad. unfold apply.
  destruct (forallb valid cs) eqn:checks; auto.
  apply forallb_forall with (x := c) in checks; auto. congruence.
Qed.

Theorem other_epochs : forall cs old next e,
  apply cs old = Some next ->
  (forall c, In c cs -> epoch c <> e) -> next e = old e.
Proof.
  intros cs old next e accepted absent. unfold apply in accepted.
  destruct (forallb valid cs); try discriminate. inversion accepted; subst.
  unfold write. assert (existsb (fun c => Nat.eqb (epoch c) e) cs = false) as no.
  { destruct (existsb (fun c => Nat.eqb (epoch c) e) cs) eqn:found; auto.
    apply existsb_exists in found as [c [inside same]].
    apply Nat.eqb_eq in same. exfalso. apply (absent c inside). exact same. }
  rewrite no. reflexivity.
Qed.

Theorem repeat_stable : forall cs old next again e,
  apply cs old = Some next -> apply cs next = Some again -> again e = next e.
Proof.
  intros cs old next again e accepted repeated. unfold apply in *.
  destruct (forallb valid cs); try discriminate.
  inversion accepted; inversion repeated; subst. unfold write.
  destruct (existsb (fun c => Nat.eqb (epoch c) e) cs); reflexivity.
Qed.
End Marks.

Print Assumptions missing_context.
Print Assumptions delivery_time.
Print Assumptions wrong_set.
Print Assumptions signature_required.
Print Assumptions batch_atomic.
Print Assumptions other_epochs.
Print Assumptions repeat_stable.