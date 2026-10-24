(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Lia.
Import ListNotations.

Record window := {
  chain : nat;
  request : nat;
  deadline : nat;
  pending : list nat
}.

Record header := {
  reply_chain : nat;
  reply_request : nat;
  peer : nat;
  now : nat
}.

Definition matches (w : window) (h : header) : Prop :=
  reply_chain h = chain w /\ reply_request h = request w /\
  now h < deadline w /\ In (peer h) (pending w).

Definition consume (w : window) (h : header) : window :=
  {| chain := chain w; request := request w; deadline := deadline w;
     pending := filter (fun p => negb (Nat.eqb (peer h) p)) (pending w) |}.

Inductive step : window -> header -> window -> nat -> Prop :=
  | Ignore : forall w h, ~ matches w h -> step w h w 0
  | Read : forall w h, matches w h -> step w h (consume w h) 1.

Inductive trace : window -> list header -> window -> nat -> Prop :=
  | Stop : forall w, trace w [] w 0
  | Next : forall w h middle rest final read total,
      step w h middle read -> trace middle rest final total ->
      trace w (h :: rest) final (read + total).

Lemma consume_unique : forall w h,
  NoDup (pending w) -> NoDup (pending (consume w h)).
Proof.
  intros w h H. unfold consume; simpl. apply NoDup_filter. exact H.
Qed.

Lemma filter_one : forall xs p,
  NoDup xs -> In p xs ->
  S (length (filter (fun x => negb (Nat.eqb p x)) xs)) = length xs.
Proof.
  intros xs p H. induction H as [|x xs Hnot Hdup IH]; intros Hin.
  - contradiction.
  - simpl in Hin. destruct Hin as [Heq|Hin].
    + subst x. simpl. rewrite Nat.eqb_refl. simpl.
      assert (Hfilter : filter (fun x => negb (Nat.eqb p x)) xs = xs).
      { clear IH Hdup. induction xs as [|x xs IH]; [reflexivity|].
        simpl. destruct (Nat.eqb p x) eqn:Heq.
        - apply Nat.eqb_eq in Heq. subst x. exfalso. apply Hnot. now left.
        - simpl. f_equal. apply IH. intros Hin. apply Hnot. now right. }
      now rewrite Hfilter.
    + simpl. destruct (Nat.eqb p x) eqn:Heq.
      * apply Nat.eqb_eq in Heq. subst x. contradiction.
      * simpl. specialize (IH Hin). lia.
Qed.

Lemma step_size : forall w h next read,
  NoDup (pending w) -> step w h next read ->
  NoDup (pending next) /\ length (pending next) + read = length (pending w).
Proof.
  intros w h next read Hdup Hstep. inversion Hstep; subst.
  - split; [exact Hdup|lia].
  - split; [now apply consume_unique|].
    unfold matches in H. destruct H as [_ [_ [_ Hin]]].
    unfold consume; simpl. pose proof (filter_one _ _ Hdup Hin). lia.
Qed.

Theorem trace_budget : forall w events final reads,
  NoDup (pending w) -> trace w events final reads ->
  length (pending final) + reads = length (pending w).
Proof.
  intros w events final reads Hdup Htrace. revert Hdup.
  induction Htrace; intros Hdup; [lia|].
  destruct (step_size _ _ _ _ Hdup H) as [Hnext Hsize].
  specialize (IHHtrace Hnext). lia.
Qed.

Theorem reads_limited : forall w events final reads,
  NoDup (pending w) -> trace w events final reads -> reads <= length (pending w).
Proof.
  intros. pose proof (trace_budget _ _ _ _ H H0). lia.
Qed.

Theorem header_before_records : forall w h next read,
  step w h next read -> read = 1 -> matches w h.
Proof.
  intros w h next read Hstep Hread. inversion Hstep; subst; [discriminate|assumption].
Qed.

Theorem repeat_does_not_read : forall w h later next read,
  peer later = peer h -> step (consume w h) later next read -> read = 0.
Proof.
  intros w h later next read Heq Hstep. inversion Hstep; subst; [reflexivity|].
  unfold matches in H. destruct H as [_ [_ [_ Hin]]].
  unfold consume in Hin; simpl in Hin. apply filter_In in Hin.
  destruct Hin as [_ Hneq]. rewrite Heq, Nat.eqb_refl in Hneq. discriminate.
Qed.

Theorem take_keeps_deadline : forall w h next read,
  step w h next read -> deadline next = deadline w.
Proof.
  intros. inversion H; reflexivity.
Qed.