(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Record slot := Slot { host : nat; net : nat; known : bool; grant : nat; bootstrap : bool }.

Definition boot row := known row && bootstrap row.
Definition boots rows := length (filter boot rows).

Definition count lane (key : slot -> nat) value rows :=
  length (filter (fun row => Bool.eqb (known row) lane && (key row =? value)) rows).

Definition room row rows :=
  (length rows <? if known row then (if bootstrap row then 128 else 136) else 120)
  && (count (known row) host (host row) rows <? 4)
  && (count (known row) net (net row) rows <? 16)
  && (negb (known row) || (count true grant (grant row) rows <? 4))
  && (negb (boot row) || (boots rows <? 8)).

Definition add row rows := if room row rows then row :: rows else rows.

Definition safe rows :=
  length rows <= 136 /\
  (forall lane h, count lane host h rows <= 4) /\
  (forall lane n, count lane net n rows <= 16) /\
  (forall g, count true grant g rows <= 4).

Lemma count_cons : forall lane key value row rows,
  count lane key value (row :: rows) =
    if Bool.eqb (known row) lane && (key row =? value)
    then S (count lane key value rows) else count lane key value rows.
Proof.
  intros. unfold count. simpl.
  destruct (Bool.eqb (known row) lane && (key row =? value)); reflexivity.
Qed.

Theorem add_safe : forall row rows, safe rows -> safe (add row rows).
Proof.
  intros row rows [total [hosts [nets grants]]]. unfold add.
  destruct (room row rows) eqn:accepted; [|repeat split; assumption].
  unfold room in accepted. repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[[[space one] group] owner] pool].
  apply Nat.ltb_lt in space. apply Nat.ltb_lt in one. apply Nat.ltb_lt in group.
  split; [simpl; destruct (known row), (bootstrap row); lia|split].
  - intros lane h. rewrite count_cons.
    destruct (Bool.eqb (known row) lane && (host row =? h)) eqn:same.
    + apply andb_true_iff in same. destruct same as [a b].
      apply eqb_prop in a. apply Nat.eqb_eq in b. rewrite <- a, <- b. lia.
    + apply hosts.
  - split.
    + intros lane n. rewrite count_cons.
    destruct (Bool.eqb (known row) lane && (net row =? n)) eqn:same.
      * apply andb_true_iff in same. destruct same as [a b].
      apply eqb_prop in a. apply Nat.eqb_eq in b. rewrite <- a, <- b. lia.
      * apply nets.
    + intros g. rewrite count_cons.
      destruct (Bool.eqb (known row) true && (grant row =? g)) eqn:same.
      * apply andb_true_iff in same. destruct same as [a b].
        apply eqb_prop in a. apply Nat.eqb_eq in b.
        rewrite a in owner. simpl in owner. apply Nat.ltb_lt in owner.
        rewrite <- b. lia.
      * apply grants.
Qed.

Lemma count_filter : forall lane key value keep rows,
  count lane key value (filter keep rows) <= count lane key value rows.
Proof.
  intros lane key value keep rows. induction rows as [|row rest IH]; [simpl; lia|].
  simpl. destruct (keep row); rewrite ?count_cons;
    destruct (Bool.eqb (known row) lane && (key row =? value)); simpl in *; lia.
Qed.

Lemma filter_length : forall (keep : slot -> bool) rows,
  length (filter keep rows) <= length rows.
Proof.
  intros keep rows. induction rows as [|row rest IH]; simpl.
  - lia.
  - destruct (keep row); simpl; lia.
Qed.

Theorem release_safe : forall keep rows, safe rows -> safe (filter keep rows).
Proof.
  intros keep rows [total [hosts [nets grants]]]. split.
  - pose proof (filter_length keep rows). lia.
  - split.
    + intros lane value. eapply Nat.le_trans; [apply count_filter|apply hosts].
    + split.
      * intros lane value. eapply Nat.le_trans; [apply count_filter|apply nets].
      * intros value. eapply Nat.le_trans; [apply count_filter|apply grants].
Qed.

Inductive trace : list slot -> Prop :=
| empty : trace []
| join : forall rows row, trace rows -> trace (add row rows)
| leave : forall rows keep, trace rows -> trace (filter keep rows).

Theorem trace_safe : forall rows, trace rows -> safe rows.
Proof.
  intros rows events. induction events.
  - unfold safe, count. simpl. repeat split; intros; lia.
  - apply add_safe. assumption.
  - apply release_safe. assumption.
Qed.

Theorem reserve_kept : forall row rows,
  known row = false -> length rows >= 120 -> add row rows = rows.
Proof.
  intros. unfold add, room. rewrite H.
  assert (length rows <? 120 = false) as no by (apply Nat.ltb_ge; lia).
  now rewrite no.
Qed.

Theorem public_cannot_fill_known : forall key value rows,
  Forall (fun row => known row = false) rows -> count true key value rows = 0.
Proof.
  intros key value rows all. induction all; [reflexivity|].
  rewrite count_cons, H. simpl. assumption.
Qed.

Theorem known_joins_public_full : forall row rows,
  known row = true -> length rows < 128 ->
  Forall (fun other => known other = false) rows -> add row rows = row :: rows.
Proof.
  intros row rows yes space public. unfold add, room. rewrite yes.
  rewrite (public_cannot_fill_known host (host row) rows public).
  rewrite (public_cannot_fill_known net (net row) rows public).
  rewrite (public_cannot_fill_known grant (grant row) rows public).
  assert (boots rows = 0) as empty.
  { unfold boots. clear space. induction public; simpl; auto. unfold boot. rewrite H. simpl. exact IHpublic. }
  unfold boot. rewrite yes, empty. simpl.
  destruct (bootstrap row); simpl;
    assert (length rows <? 128 = true) by (apply Nat.ltb_lt; lia);
    assert (length rows <? 136 = true) by (apply Nat.ltb_lt; lia);
    now rewrite ?H, ?H0.
Qed.

Theorem record_move_cap : forall rows row,
  known row = true -> count true grant (grant row) rows >= 4 ->
  add row rows = rows.
Proof.
  intros rows row yes full. unfold add, room. rewrite yes. simpl.
  assert (count true grant (grant row) rows <? 4 = false) as no
    by (apply Nat.ltb_ge; lia).
  now rewrite no, !andb_false_r.
Qed.

Theorem boot_add_limit : forall row rows,
  boots rows <= 8 -> boots (add row rows) <= 8.
Proof.
  intros row rows cap. unfold add. destruct (room row rows) eqn:ok; [|assumption].
  unfold room in ok. apply andb_true_iff in ok. destruct ok as [_ pool].
  unfold boots.
  change (length (if boot row then row :: filter boot rows else filter boot rows) <= 8).
  destruct (boot row) eqn:yes; [|exact cap].
  simpl in pool. apply Nat.ltb_lt in pool. simpl. unfold boots in pool. lia.
Qed.

Theorem boot_leaves_record_room : forall row rows,
  known row = true -> bootstrap row = false -> length rows = 128 ->
  count true host (host row) rows < 4 -> count true net (net row) rows < 16 ->
  count true grant (grant row) rows < 4 -> add row rows = row :: rows.
Proof.
  intros row rows yes record total hosts nets grants. unfold add, room, boot.
  rewrite yes, record, total. simpl.
  apply Nat.ltb_lt in hosts. apply Nat.ltb_lt in nets. apply Nat.ltb_lt in grants.
  now rewrite hosts, nets, grants.
Qed.