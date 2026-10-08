(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith Bool List.
Import ListNotations.

Definition ticket := (nat * nat)%type.
Inductive state := Waiting (id : ticket) (deadline : nat) | Closed.
Inductive reason := Cancelled | Deadline.
Inductive message := Reply (id : ticket) (now : nat) (valid : bool) | Cancel | Expire.
Inductive effect := Deliver (valid : bool) | Stop (why : reason).

Definition same (left right : ticket) :=
  Nat.eqb (fst left) (fst right) && Nat.eqb (snd left) (snd right).

Definition delta (current : state) (input : message) : state * list effect :=
  match current, input with
  | Waiting expected deadline, Reply id now valid =>
    if same expected id then
      if now <? deadline then (Closed, [Deliver valid])
      else (Closed, [Stop Deadline])
    else (current, [])
  | Waiting _ _, Cancel => (Closed, [Stop Cancelled])
  | Waiting _ _, Expire => (Closed, [Stop Deadline])
  | Closed, _ => (Closed, [])
  end.

Lemma same_exact : forall left right, same left right = true <-> left = right.
Proof.
  intros [a b] [c d]. unfold same. simpl.
  rewrite andb_true_iff, !Nat.eqb_eq. split; intros; intuition congruence.
Qed.

Theorem reply_matches : forall current id now valid next value,
  delta current (Reply id now valid) = (next, [Deliver value]) ->
  exists deadline,
    current = Waiting id deadline /\ now < deadline /\ next = Closed /\ value = valid.
Proof.
  intros [expected deadline|] id now valid next value step;
    simpl in step; [|discriminate].
  destruct (same expected id) eqn:eq; [|discriminate].
  destruct (now <? deadline) eqn:before; [|discriminate].
  apply same_exact in eq. apply Nat.ltb_lt in before.
  inversion step. subst. exists deadline. auto.
Qed.

Theorem cancelled_no_reply : forall current id now valid,
  delta (fst (delta current Cancel)) (Reply id now valid) = (Closed, []).
Proof. intros current id now valid. destruct current; reflexivity. Qed.

Theorem expired_no_reply : forall current id now valid,
  delta (fst (delta current Expire)) (Reply id now valid) = (Closed, []).
Proof. intros current id now valid. destruct current; reflexivity. Qed.

Theorem deadline_refused : forall id deadline now valid,
  deadline <= now ->
  delta (Waiting id deadline) (Reply id now valid) = (Closed, [Stop Deadline]).
Proof.
  intros id deadline now valid late. simpl.
  assert (same id id = true) as eq by (apply same_exact; reflexivity).
  rewrite eq. apply Nat.ltb_ge in late. rewrite late. reflexivity.
Qed.

Fixpoint deliver (current : state) (inputs : list message) : list bool :=
  match inputs with
  | [] => []
  | input :: rest =>
    let '(next, effects) := delta current input in
    match effects with
    | [Deliver result] => result :: deliver next rest
    | _ => deliver next rest
    end
  end.

Lemma closed_no_output : forall inputs, deliver Closed inputs = [].
Proof. induction inputs as [|[] rest induction]; simpl; auto. Qed.

Theorem at_most_once : forall inputs current,
  length (deliver current inputs) <= 1.
Proof.
  induction inputs as [|input rest induction]; intros [id deadline|]; simpl; auto.
  - destruct input as [other now valid| |]; simpl.
    + destruct (same id other); simpl.
      * destruct (now <? deadline); simpl; rewrite closed_no_output; simpl; auto.
      * apply induction.
    + rewrite closed_no_output. simpl. auto.
    + rewrite closed_no_output. simpl. auto.
Qed.

Print Assumptions reply_matches.
Print Assumptions cancelled_no_reply.
Print Assumptions expired_no_reply.
Print Assumptions deadline_refused.
Print Assumptions at_most_once.