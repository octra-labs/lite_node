(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith.PeanoNat.
From Stdlib Require Import Bool.Bool.
From Stdlib Require Import Lists.List.
From Stdlib Require Import Lia.
From Stdlib Require Import ZArith.
From Stdlib Require Import Sorting.Permutation.

Import ListNotations.

Module Sequence.
Definition state := nat -> option nat.

Definition fresh (disk : state) key number :=
  match disk key with
  | None => true
  | Some previous => Nat.ltb previous number
  end.

Definition write (disk : state) key number : state :=
  fun other => if Nat.eqb other key then Some number else disk other.

Definition reserve disk key number stored :=
  if fresh disk key number && stored
  then (write disk key number, true)
  else (disk, false).

Definition extends (before after : state) :=
  forall key previous, before key = Some previous ->
  exists current, after key = Some current /\ previous <= current.

Theorem accepted_stored : forall disk key number stored,
  snd (reserve disk key number stored) = true ->
  stored = true /\ fst (reserve disk key number stored) key = Some number.
Proof.
  intros disk key number stored accepted. unfold reserve in *.
  destruct (fresh disk key number && stored) eqn:valid; simpl in *;
    try discriminate.
  apply andb_true_iff in valid as [_ persisted]. split; [exact persisted|].
  unfold write. rewrite Nat.eqb_refl. reflexivity.
Qed.

Theorem failed_write : forall disk key number,
  reserve disk key number false = (disk, false).
Proof.
  intros. unfold reserve. rewrite andb_false_r. reflexivity.
Qed.

Theorem repeat_refused : forall disk key number stored other ready,
  snd (reserve disk key number stored) = true -> other <= number ->
  reserve (fst (reserve disk key number stored)) key other ready =
  (fst (reserve disk key number stored), false).
Proof.
  intros disk key number stored other ready accepted older.
  pose proof (accepted_stored disk key number stored accepted) as [_ saved].
  unfold reserve at 1. unfold fresh. rewrite saved.
  apply Nat.ltb_ge in older. rewrite older. reflexivity.
Qed.

Theorem sequence_extends : forall disk key number stored,
  extends disk (fst (reserve disk key number stored)).
Proof.
  intros disk key number stored old previous found.
  unfold reserve. destruct (fresh disk key number && stored) eqn:valid; simpl.
  - unfold write. destruct (Nat.eqb old key) eqn:same.
    + apply Nat.eqb_eq in same. subst old.
      apply andb_true_iff in valid as [newer _].
      unfold fresh in newer. rewrite found in newer.
      apply Nat.ltb_lt in newer. exists number. auto with arith.
    + exists previous. auto.
  - exists previous. auto.
Qed.

Theorem later_refuses_old : forall before after key previous number stored,
  extends before after -> before key = Some previous -> number <= previous ->
  reserve after key number stored = (after, false).
Proof.
  intros before after key previous number stored growth found older.
  destruct (growth key previous found) as [current [saved monotone]].
  unfold reserve, fresh. rewrite saved.
  assert (Nat.ltb current number = false) as refused by (apply Nat.ltb_ge; lia).
  rewrite refused. reflexivity.
Qed.

Fixpoint run (events : list (nat * nat * bool)) disk :=
  match events with
  | [] => disk
  | (key, number, stored) :: rest =>
    run rest (fst (reserve disk key number stored))
  end.

Theorem history_extends : forall events disk, extends disk (run events disk).
Proof.
  induction events as [|[[key number] stored] rest ih]; intros disk old previous found.
  - exists previous. auto.
  - simpl. destruct (sequence_extends disk key number stored old previous found)
      as [next [saved first]].
    destruct (ih (fst (reserve disk key number stored)) old next saved)
      as [last [final second]].
    exists last. split; [exact final|lia].
Qed.

Print Assumptions accepted_stored.
Print Assumptions failed_write.
Print Assumptions repeat_refused.
Print Assumptions sequence_extends.
Print Assumptions later_refuses_old.
Print Assumptions history_extends.
End Sequence.

Module Session.
Record state := { sequences : Sequence.state; closed : nat -> bool }.

Definition reserve disk key number stored :=
  if closed disk key then (disk, false)
  else let '(next, accepted) := Sequence.reserve (sequences disk) key number stored in
    ({| sequences := next; closed := closed disk |}, accepted).

Definition cancel disk key (stored : bool) :=
  if stored then
    ({| sequences := sequences disk;
        closed := fun other => Nat.eqb other key || closed disk other |}, true)
  else (disk, false).

Definition ensure disk key stored :=
  (disk, negb (closed disk key) && stored).

Inductive event :=
  | Work : nat -> nat -> bool -> event
  | Stop : nat -> bool -> event
  | Open : nat -> bool -> event.

Definition step disk event :=
  match event with
  | Work key number stored => fst (reserve disk key number stored)
  | Stop key stored => fst (cancel disk key stored)
  | Open key stored => fst (ensure disk key stored)
  end.

Fixpoint run events disk :=
  match events with
  | [] => disk
  | event :: rest => run rest (step disk event)
  end.

Theorem cancel_saved : forall disk key stored,
  snd (cancel disk key stored) = true ->
  closed (fst (cancel disk key stored)) key = true.
Proof.
  intros disk key []; simpl; try discriminate.
  intros _. rewrite Nat.eqb_refl. reflexivity.
Qed.

Theorem closed_step : forall event disk key,
  closed disk key = true -> closed (step disk event) key = true.
Proof.
  intros [other number stored|other stored|other stored] disk key saved.
  - unfold step, reserve. destruct (closed disk other); simpl; auto.
    destruct (Sequence.reserve (sequences disk) other number stored); exact saved.
  - unfold step, cancel. destruct stored; simpl; auto.
    rewrite saved, orb_true_r. reflexivity.
  - exact saved.
Qed.

Theorem closed_history : forall events disk key,
  closed disk key = true -> closed (run events disk) key = true.
Proof.
  induction events as [|event rest ih]; intros disk key saved; simpl; auto.
  apply ih. apply closed_step. exact saved.
Qed.

Theorem resume_refused : forall events disk key stored number ready,
  snd (cancel disk key stored) = true ->
  let after := run events (fst (cancel disk key stored)) in
  reserve after key number ready = (after, false) /\
  ensure after key ready = (after, false).
Proof.
  intros events disk key stored number ready saved after.
  pose proof (closed_history events (fst (cancel disk key stored)) key
    (cancel_saved disk key stored saved)) as stopped.
  change (closed after key = true) in stopped.
  unfold reserve, ensure. rewrite stopped. simpl. auto.
Qed.

Theorem failed_cancel : forall disk key,
  cancel disk key false = (disk, false).
Proof. reflexivity. Qed.

Definition result_allowed disk key := negb (closed disk key).

Theorem result_refused : forall events disk key stored,
  snd (cancel disk key stored) = true ->
  result_allowed (run events (fst (cancel disk key stored))) key = false.
Proof.
  intros events disk key stored saved. unfold result_allowed.
  rewrite (closed_history events (fst (cancel disk key stored)) key
    (cancel_saved disk key stored saved)). reflexivity.
Qed.

Theorem other_session : forall disk key other stored,
  key <> other ->
  closed (fst (cancel disk key stored)) other = closed disk other.
Proof.
  intros disk key other [] different; simpl; auto.
  assert (Nat.eqb other key = false) as apart by (apply Nat.eqb_neq; congruence).
  rewrite apart. reflexivity.
Qed.

Print Assumptions cancel_saved.
Print Assumptions closed_step.
Print Assumptions closed_history.
Print Assumptions resume_refused.
Print Assumptions failed_cancel.
Print Assumptions result_refused.
Print Assumptions other_session.
End Session.

Module Reply.
Inductive cell :=
  | Empty
  | Running : nat -> nat -> cell
  | Done : nat -> nat -> nat -> cell
  | Closed.

Definition reserve state number request (stored : bool) :=
  if stored then
    match state with
    | Empty => (Running number request, true)
    | Running before _ | Done before _ _ =>
      if Nat.ltb before number then (Running number request, true) else (state, false)
    | Closed => (state, false)
    end
  else (state, false).

Definition finish state number request value (stored : bool) :=
  if stored then
    match state with
    | Running before key =>
      if Nat.eqb before number && Nat.eqb key request
      then (Done number request value, true) else (state, false)
    | Done before key prior =>
      (state, Nat.eqb before number && Nat.eqb key request && Nat.eqb prior value)
    | _ => (state, false)
    end
  else (state, false).

Definition read state number request :=
  match state with
  | Done before key value =>
    if Nat.eqb before number && Nat.eqb key request then Some value else None
  | _ => None
  end.

Theorem finish_state : forall state number request value stored,
  snd (finish state number request value stored) = true ->
  stored = true /\ fst (finish state number request value stored) = Done number request value.
Proof.
  intros state number request value [] accepted; [|simpl in accepted; discriminate].
  split; [reflexivity|].
  destruct state as [|before key|before key prior|]; simpl in *; try discriminate.
  - destruct (Nat.eqb before number && Nat.eqb key request); simpl in *;
      congruence.
  - apply andb_true_iff in accepted as [same equal_value].
    apply andb_true_iff in same as [equal_number equal_key].
    apply Nat.eqb_eq in equal_number, equal_key, equal_value. subst. reflexivity.
Qed.

Theorem result_readable : forall state number request value stored,
  snd (finish state number request value stored) = true ->
  read (fst (finish state number request value stored)) number request = Some value.
Proof.
  intros state number request value stored accepted.
  rewrite (proj2 (finish_state state number request value stored accepted)).
  unfold read. rewrite !Nat.eqb_refl. reflexivity.
Qed.

Theorem distinct_reply : forall state number request value other stored ready,
  snd (finish state number request value stored) = true -> value <> other ->
  finish (fst (finish state number request value stored)) number request other ready =
    (Done number request value, false).
Proof.
  intros state number request value other stored ready accepted different.
  rewrite (proj2 (finish_state state number request value stored accepted)).
  destruct ready; simpl; [|reflexivity].
  rewrite !Nat.eqb_refl.
  apply Nat.eqb_neq in different. rewrite different. reflexivity.
Qed.

Theorem no_repeat : forall state number request value stored next key ready,
  snd (finish state number request value stored) = true -> next <= number ->
  reserve (fst (finish state number request value stored)) next key ready =
    (Done number request value, false).
Proof.
  intros state number request value stored next key ready accepted older.
  rewrite (proj2 (finish_state state number request value stored accepted)).
  destruct ready; simpl; [|reflexivity].
  apply Nat.ltb_ge in older. rewrite older. reflexivity.
Qed.

Theorem failed_finish : forall state number request value,
  finish state number request value false = (state, false).
Proof. reflexivity. Qed.

Theorem closed_refuses : forall number request value stored,
  finish Closed number request value stored = (Closed, false) /\
  reserve Closed number request stored = (Closed, false) /\
  read Closed number request = None.
Proof. intros. destruct stored; repeat split; reflexivity. Qed.

Print Assumptions finish_state.
Print Assumptions result_readable.
Print Assumptions distinct_reply.
Print Assumptions no_repeat.
Print Assumptions failed_finish.
Print Assumptions closed_refuses.
End Reply.

Module Receipt.
Definition book := nat -> option nat.

Definition save (disk : book) key value checked stored :=
  if checked && stored then
    match disk key with
    | None => ((fun other => if Nat.eqb key other then Some value else disk other), true)
    | Some previous => (disk, Nat.eqb previous value)
    end
  else (disk, false).

Example conflict_keeps :
  fst (save (fun key => if Nat.eqb key 3 then Some 7 else None) 3 9 true true) 3 = Some 7.
Proof. vm_compute. reflexivity. Qed.

Theorem acknowledged : forall disk key value checked stored,
  snd (save disk key value checked stored) = true ->
  checked = true /\ stored = true /\ fst (save disk key value checked stored) key = Some value.
Proof.
  intros disk key value checked stored. unfold save.
  destruct checked, stored; simpl; try discriminate.
  destruct (disk key) as [previous|] eqn:found; simpl.
  - intro equal. apply Nat.eqb_eq in equal. subst. auto.
  - intros _. rewrite Nat.eqb_refl. auto.
Qed.

Theorem saved_kept : forall disk key value other output checked stored,
  disk key = Some value -> fst (save disk other output checked stored) key = Some value.
Proof.
  intros disk key value other output checked stored found.
  unfold save. destruct (checked && stored); [|exact found].
  destruct (disk other) eqn:prior; simpl; [exact found|].
  destruct (Nat.eqb other key) eqn:same; [|exact found].
  apply Nat.eqb_eq in same. subst. congruence.
Qed.

Definition event := (nat * nat * bool * bool)%type.

Fixpoint run (events : list event) disk :=
  match events with
  | [] => disk
  | (key, value, checked, stored) :: rest =>
    run rest (fst (save disk key value checked stored))
  end.

Theorem history_keeps : forall events disk key value,
  disk key = Some value -> run events disk key = Some value.
Proof.
  induction events as [|[[[other output] checked] stored] rest ih];
    intros disk key value found; simpl; [exact found|].
  apply ih. apply saved_kept. exact found.
Qed.

Theorem committed_readable : forall events disk key value checked stored,
  snd (save disk key value checked stored) = true ->
  run events (fst (save disk key value checked stored)) key = Some value.
Proof.
  intros events disk key value checked stored accepted.
  apply history_keeps. exact (proj2 (proj2 (acknowledged disk key value checked stored accepted))).
Qed.

Theorem failed_store : forall disk key value checked,
  save disk key value checked false = (disk, false).
Proof. intros. unfold save. rewrite andb_false_r. reflexivity. Qed.

Theorem unchecked_refused : forall disk key value stored,
  save disk key value false stored = (disk, false).
Proof. reflexivity. Qed.

Print Assumptions acknowledged.
Print Assumptions saved_kept.
Print Assumptions history_keeps.
Print Assumptions committed_readable.
Print Assumptions failed_store.
Print Assumptions unchecked_refused.
End Receipt.

Module Pin.
Record state := { saved : Receipt.book; current : nat }.

Inductive event :=
| Observe : nat -> event
| Issue : nat -> bool -> bool -> event
| Restart.

Definition issue disk key checked stored :=
  let '(next, accepted) := Receipt.save (saved disk) key (current disk) checked stored in
  ({| saved := next; current := current disk |}, accepted).

Definition step disk action :=
  match action with
  | Observe root => {| saved := saved disk; current := root |}
  | Issue key checked stored => fst (issue disk key checked stored)
  | Restart => disk
  end.

Fixpoint run events disk :=
  match events with
  | [] => disk
  | action :: rest => run rest (step disk action)
  end.

Definition read_set disk key :=
  match saved disk key with Some root => root | None => current disk end.

Definition matches disk key root := Nat.eqb (read_set disk key) root.

Definition fresh disk root := Nat.eqb (current disk) root.

Theorem accepted_current : forall disk key checked stored,
  snd (issue disk key checked stored) = true ->
  checked = true /\ stored = true /\
  saved (fst (issue disk key checked stored)) key = Some (current disk).
Proof.
  intros disk key checked stored accepted.
  unfold issue in *.
  destruct (Receipt.save (saved disk) key (current disk) checked stored)
    as [next ok] eqn:result. simpl in *.
  pose proof (Receipt.acknowledged (saved disk) key (current disk) checked stored) as saved_ok.
  rewrite result in saved_ok. apply saved_ok. exact accepted.
Qed.

Theorem step_keeps : forall disk action key root,
  saved disk key = Some root -> saved (step disk action) key = Some root.
Proof.
  intros disk action key root found. destruct action; simpl; try exact found.
  unfold issue.
  destruct (Receipt.save (saved disk) n (current disk) b b0) as [next ok] eqn:result.
  simpl. pose proof (Receipt.saved_kept (saved disk) key root n (current disk) b b0 found) as kept.
  rewrite result in kept. exact kept.
Qed.

Theorem history_keeps : forall events disk key root,
  saved disk key = Some root -> saved (run events disk) key = Some root.
Proof.
  induction events as [|action rest ih]; intros disk key root found; simpl.
  - exact found.
  - apply ih. apply step_keeps. exact found.
Qed.

Theorem history_matches : forall events disk key root,
  saved disk key = Some root -> matches (run events disk) key root = true.
Proof.
  intros events disk key root found. unfold matches, read_set.
  rewrite (history_keeps events disk key root found). apply Nat.eqb_refl.
Qed.

Theorem wrong_root : forall events disk key root other,
  saved disk key = Some root -> root <> other ->
  matches (run events disk) key other = false.
Proof.
  intros events disk key root other found different. unfold matches, read_set.
  rewrite (history_keeps events disk key root found). apply Nat.eqb_neq. exact different.
Qed.

Theorem missing_pin : forall disk key root,
  saved disk key = None -> current disk <> root -> matches disk key root = false.
Proof.
  intros disk key root absent different. unfold matches, read_set.
  rewrite absent. apply Nat.eqb_neq. exact different.
Qed.

Theorem no_new_authority : forall disk old newer,
  newer <> old -> fresh (step disk (Observe newer)) old = false.
Proof.
  intros disk old newer different. unfold fresh. simpl.
  apply Nat.eqb_neq. exact different.
Qed.

Example old_root_after_change :
  let initial := {| saved := fun _ => None; current := 7 |} in
  let issued := fst (issue initial 2 true true) in
  let next := run [Observe 9; Issue 2 true true; Restart] issued in
  matches next 2 7 = true /\ matches next 2 9 = false /\ fresh next 7 = false.
Proof. vm_compute. auto. Qed.

Print Assumptions accepted_current.
Print Assumptions history_keeps.
Print Assumptions history_matches.
Print Assumptions wrong_root.
Print Assumptions missing_pin.
Print Assumptions no_new_authority.
End Pin.

Module ReadKey.
Inductive row :=
| Pending : nat -> nat -> nat -> row
| Done : nat -> nat -> nat -> nat -> row.

Record state := { cell : row; current : nat }.

Definition finish disk number call root value checked stored :=
  if checked && stored then
    match cell disk with
    | Pending n c r =>
      if Nat.eqb n number && Nat.eqb c call && Nat.eqb root r
      then ({| cell := Done number call root value; current := current disk |}, true)
      else (disk, false)
    | Done n c r v =>
      (disk, Nat.eqb n number && Nat.eqb c call && Nat.eqb r root && Nat.eqb v value)
    end
  else (disk, false).

Definition read disk number call root (verified : bool) :=
  if verified then
    match cell disk with
    | Pending _ _ _ => None
    | Done n c r value =>
      if Nat.eqb n number && Nat.eqb c call && Nat.eqb r root
      then Some value else None
    end
  else None.

Inductive event :=
| Observe : nat -> event
| Complete : nat -> nat -> nat -> nat -> bool -> bool -> event
| Restart.

Definition step disk action :=
  match action with
  | Observe root => {| cell := cell disk; current := root |}
  | Complete number call root value checked stored =>
    fst (finish disk number call root value checked stored)
  | Restart => disk
  end.

Fixpoint run actions disk :=
  match actions with
  | [] => disk
  | action :: rest => run rest (step disk action)
  end.

Theorem finish_pair : forall disk number call root value checked stored,
  snd (finish disk number call root value checked stored) = true ->
  checked = true /\ stored = true /\
  cell (fst (finish disk number call root value checked stored)) = Done number call root value.
Proof.
  intros [row now] number call root value [] [] accepted; unfold finish in *; cbn in *;
    try discriminate.
  split; [reflexivity|]. split; [reflexivity|].
  destruct row as [n c reserved|n c r v]; simpl in *.
  - destruct (Nat.eqb n number && Nat.eqb c call && Nat.eqb root reserved) eqn:same;
      cbn in *.
    + reflexivity.
    + discriminate.
  - repeat rewrite andb_true_iff in accepted.
    destruct accepted as [[[hn hc] hr] hv].
    apply Nat.eqb_eq in hn, hc, hr, hv. subst. reflexivity.
Qed.

Theorem read_pair : forall disk number call root value verified,
  read disk number call root verified = Some value ->
  verified = true /\ cell disk = Done number call root value.
Proof.
  intros [row now] number call root value [] found; unfold read in *; cbn in *; try discriminate.
  split; [reflexivity|]. destruct row as [n c reserved|n c r v]; cbn in *; try discriminate.
  destruct (Nat.eqb n number && Nat.eqb c call && Nat.eqb r root) eqn:same;
    try discriminate.
  repeat rewrite andb_true_iff in same. destruct same as [[hn hc] hr].
  apply Nat.eqb_eq in hn, hc, hr. inversion found. subst. reflexivity.
Qed.

Theorem step_keeps : forall disk number call root value action,
  cell disk = Done number call root value ->
  cell (step disk action) = Done number call root value.
Proof.
  intros [row now] number call root value action found. simpl in found. subst row.
  destruct action; simpl; try reflexivity.
  unfold finish. destruct (b && b0); reflexivity.
Qed.

Theorem history_keeps : forall actions disk number call root value,
  cell disk = Done number call root value ->
  cell (run actions disk) = Done number call root value.
Proof.
  induction actions as [|action rest ih]; intros disk number call root value found; simpl.
  - exact found.
  - apply ih. apply step_keeps. exact found.
Qed.

Theorem committed_read : forall actions disk number call root value checked stored,
  snd (finish disk number call root value checked stored) = true ->
  read (run actions (fst (finish disk number call root value checked stored)))
    number call root true = Some value.
Proof.
  intros actions disk number call root value checked stored accepted.
  destruct (finish_pair disk number call root value checked stored accepted)
    as [_ [_ found]].
  unfold read. rewrite (history_keeps actions _ number call root value found).
  rewrite !Nat.eqb_refl. reflexivity.
Qed.

Theorem wrong_call : forall actions disk number call root value other,
  cell disk = Done number call root value -> call <> other ->
  read (run actions disk) number other root true = None.
Proof.
  intros actions disk number call root value other found different.
  unfold read. rewrite (history_keeps actions disk number call root value found).
  apply Nat.eqb_neq in different. rewrite different, Nat.eqb_refl. reflexivity.
Qed.

Theorem failed_store : forall disk number call root value checked,
  finish disk number call root value checked false = (disk, false).
Proof. intros. unfold finish. rewrite andb_false_r. reflexivity. Qed.

Example read_after_change :
  let initial := {| cell := Pending 1 2 7; current := 7 |} in
  let changed := run [Observe 9; Restart] initial in
  let saved := fst (finish changed 1 2 7 11 true true) in
  read (run [Observe 9; Complete 1 2 9 13 true true; Restart] saved)
    1 2 7 true = Some 11.
Proof. reflexivity. Qed.

Print Assumptions finish_pair.
Print Assumptions read_pair.
Print Assumptions history_keeps.
Print Assumptions committed_read.
Print Assumptions wrong_call.
Print Assumptions failed_store.
End ReadKey.

Module Read.
Record scope := {
  chain : nat;
  node : nat;
  owner : nat;
  job : nat;
  first : nat;
  last : nat
}.

Definition disk := nat -> nat -> option nat.

Definition permit chain_id node_id now signed request :=
  (first request <=? last request)
  && (last request - first request <=? 16)
  && (chain request =? chain_id)
  && (node request =? node_id)
  && (first request <=? now)
  && (now <=? last request)
  && signed.

Definition observe (data : disk) chain_id node_id now signed request :=
  if permit chain_id node_id now signed request
  then Some (data (owner request) (job request))
  else None.

Theorem interval : forall chain_id node_id now signed request,
  permit chain_id node_id now signed request = true ->
  first request <= now /\ now <= last request
  /\ last request <= first request + 16.
Proof.
  intros chain_id node_id now signed request accepted.
  unfold permit in accepted.
  repeat rewrite andb_true_iff in accepted.
  repeat rewrite Nat.leb_le in accepted.
  intuition lia.
Qed.

Theorem audience : forall chain_id node_id now signed request,
  permit chain_id node_id now signed request = true ->
  chain request = chain_id /\ node request = node_id /\ signed = true.
Proof.
  intros chain_id node_id now signed request accepted.
  unfold permit in accepted.
  repeat rewrite andb_true_iff in accepted.
  repeat rewrite Nat.eqb_eq in accepted.
  intuition.
Qed.

Theorem unsigned_private : forall data chain_id node_id now request,
  observe data chain_id node_id now false request = None.
Proof.
  intros. unfold observe, permit. rewrite andb_false_r. reflexivity.
Qed.

Theorem expired_private : forall data chain_id node_id now signed request,
  last request < now ->
  observe data chain_id node_id now signed request = None.
Proof.
  intros data chain_id node_id now signed request expired.
  unfold observe.
  destruct (permit chain_id node_id now signed request) eqn:accepted; auto.
  apply interval in accepted. lia.
Qed.

Theorem future_private : forall data chain_id node_id now signed request,
  now < first request ->
  observe data chain_id node_id now signed request = None.
Proof.
  intros data chain_id node_id now signed request future.
  unfold observe.
  destruct (permit chain_id node_id now signed request) eqn:accepted; auto.
  apply interval in accepted. lia.
Qed.

Theorem refused_independent : forall left right chain_id node_id now signed request,
  permit chain_id node_id now signed request = false ->
  observe left chain_id node_id now signed request =
  observe right chain_id node_id now signed request.
Proof.
  intros left right chain_id node_id now signed request refused.
  unfold observe. rewrite refused. reflexivity.
Qed.

Theorem same_key : forall left right chain_id node_id now signed request,
  left (owner request) (job request) = right (owner request) (job request) ->
  observe left chain_id node_id now signed request =
  observe right chain_id node_id now signed request.
Proof.
  intros left right chain_id node_id now signed request equal.
  unfold observe. destruct (permit chain_id node_id now signed request); congruence.
Qed.

Example expiry_edge :
  let request := {| chain := 1; node := 2; owner := 3; job := 4;
    first := 100; last := 116 |} in
  permit 1 2 116 true request = true /\ permit 1 2 117 true request = false.
Proof. split; reflexivity. Qed.
End Read.

Module Work.
Inductive state :=
  | Empty
  | Planned (intent : nat)
  | Issued (intent call : nat) (votes : list nat)
  | Done (intent call : nat) (votes : list nat).

Inductive action :=
  | Plan (intent : nat)
  | Issue (call : nat)
  | Vote (node : nat)
  | Finish.

Inductive effect :=
  | Offer (intent : nat)
  | Send (call : nat)
  | Ack (node : nat)
  | Result (call : nat).

Definition step (quorum : nat) (disk : state) (command : action) (stored : bool)
    : state * list effect :=
  if stored then
    match disk, command with
    | Empty, Plan intent => (Planned intent, [Offer intent])
    | Planned intent, Issue call => (Issued intent call [], [Send call])
    | Issued intent call votes, Vote node =>
      if existsb (Nat.eqb node) votes then (disk, [])
      else (Issued intent call (node :: votes), [Ack node])
    | Issued intent call votes, Finish =>
      if (0 <? quorum) && (quorum <=? length votes)
      then (Done intent call votes, [Result call])
      else (disk, [])
    | _, _ => (disk, [])
    end
  else (disk, []).

Definition selected disk :=
  match disk with
  | Issued _ call _ | Done _ call _ => Some call
  | _ => None
  end.

Definition votes disk :=
  match disk with
  | Issued _ _ values | Done _ _ values => values
  | _ => []
  end.

Lemma failed_write : forall quorum disk command,
  step quorum disk command false = (disk, []).
Proof. reflexivity. Qed.

Theorem send_committed : forall quorum disk command stored call,
  In (Send call) (snd (step quorum disk command stored)) ->
  stored = true /\ exists intent,
    fst (step quorum disk command stored) = Issued intent call [].
Proof.
  intros quorum disk command stored call sent.
  destruct stored; [|simpl in sent; contradiction]. split; [reflexivity|].
  destruct disk; destruct command; simpl in *; try contradiction;
    repeat match type of sent with
    | context [if ?test then _ else _] => destruct test eqn:?; simpl in sent
    end; try contradiction;
    destruct sent as [same|[]]; inversion same; subst; eauto.
Qed.

Theorem keeps_call : forall quorum disk command stored call,
  selected disk = Some call ->
  selected (fst (step quorum disk command stored)) = Some call.
Proof.
  intros quorum disk command stored call chosen.
  destruct stored; [|exact chosen].
  destruct disk; try discriminate; destruct command; simpl in *;
    repeat match goal with
    | |- context [if ?test then _ else _] => destruct test
    end; exact chosen.
Qed.

Theorem keeps_votes : forall quorum disk command stored node,
  In node (votes disk) -> In node (votes (fst (step quorum disk command stored))).
Proof.
  intros quorum disk command stored node present.
  destruct stored; [|exact present].
  destruct disk; try contradiction; destruct command; simpl in *;
    repeat match goal with
    | |- context [if ?test then _ else _] => destruct test
    end; simpl; auto.
Qed.

Theorem unique_votes : forall quorum disk command stored,
  NoDup (votes disk) -> NoDup (votes (fst (step quorum disk command stored))).
Proof.
  intros quorum disk command stored unique.
  destruct stored; [|exact unique].
  destruct disk; destruct command; simpl in *; try exact unique;
    try constructor.
  - destruct (existsb (Nat.eqb node) votes0) eqn:seen; [exact unique|].
    simpl. constructor; [|exact unique].
    intro present. assert (existsb (Nat.eqb node) votes0 = true) as found.
    { apply existsb_exists. exists node. split; [exact present|apply Nat.eqb_refl]. }
    congruence.
  - destruct ((0 <? quorum) && (quorum <=? length votes0)); exact unique.
Qed.

Theorem result_quorum : forall quorum disk command stored call,
  In (Result call) (snd (step quorum disk command stored)) ->
  stored = true /\ 0 < quorum /\
  quorum <= length (votes (fst (step quorum disk command stored))).
Proof.
  intros quorum disk command stored call sent.
  destruct stored; [|simpl in sent; contradiction].
  destruct disk; destruct command; simpl in *; try contradiction;
    repeat match type of sent with
    | context [if ?test then _ else _] => destruct test eqn:?; simpl in sent
    end; try contradiction;
    destruct sent as [same|[]]; inversion same; subst.
  match goal with
  | valid : (_ && _) = true |- _ =>
    apply andb_true_iff in valid as [positive enough];
    apply Nat.ltb_lt in positive; apply Nat.leb_le in enough
  end. auto.
Qed.

Fixpoint run quorum disk commands :=
  match commands with
  | [] => disk
  | (command, stored) :: rest => run quorum (fst (step quorum disk command stored)) rest
  end.

Theorem fixed_call : forall commands quorum disk call,
  selected disk = Some call -> selected (run quorum disk commands) = Some call.
Proof.
  induction commands as [|[command stored] rest ih]; intros quorum disk call chosen.
  - exact chosen.
  - apply ih. apply keeps_call. exact chosen.
Qed.

Theorem retained_votes : forall commands quorum disk node,
  In node (votes disk) -> In node (votes (run quorum disk commands)).
Proof.
  induction commands as [|[command stored] rest ih]; intros quorum disk node present.
  - exact present.
  - apply ih. apply keeps_votes. exact present.
Qed.

Definition recover disk :=
  match disk with
  | Planned intent => [Offer intent]
  | Done _ call _ => [Result call]
  | _ => []
  end.

Theorem issued_no_send : forall intent call values,
  recover (Issued intent call values) = [].
Proof. reflexivity. Qed.

Print Assumptions send_committed.
Print Assumptions keeps_call.
Print Assumptions unique_votes.
Print Assumptions result_quorum.
Print Assumptions fixed_call.
Print Assumptions retained_votes.
End Work.

Module Fetch.
Definition store := nat -> option nat.

Definition serve fuel (disk : store) (query : nat * bool) :=
  match fuel, query with
  | S rest, (key, true) => (rest, disk key)
  | _, _ => (fuel, None)
  end.

Theorem only_saved : forall fuel disk key allowed value,
  snd (serve fuel disk (key, allowed)) = Some value ->
  allowed = true /\ disk key = Some value.
Proof.
  intros fuel disk key allowed value found.
  destruct fuel, allowed; simpl in *; try discriminate. auto.
Qed.

Theorem invalid_no_answer : forall fuel disk key,
  serve fuel disk (key, false) = (fuel, None).
Proof. intros. destruct fuel; reflexivity. Qed.

Definition answer (key : nat) (value : option nat) :=
  match value with None => [] | Some result => [(key, result)] end.

Fixpoint run fuel (disk : store) queries : nat * list (nat * nat) :=
  match queries with
  | [] => (fuel, [])
  | (key, allowed) :: rest =>
    let '(next, value) := serve fuel disk (key, allowed) in
    let '(remaining, values) := run next disk rest in
    (remaining, answer key value ++ values)
  end.

Lemma serve_limit : forall fuel disk key allowed,
  fst (serve fuel disk (key, allowed))
    + length (answer key (snd (serve fuel disk (key, allowed)))) <= fuel.
Proof.
  intros fuel disk key allowed. destruct fuel, allowed; simpl; try lia.
  destruct (disk key); simpl; lia.
Qed.

Theorem reply_limit : forall queries fuel disk,
  fst (run fuel disk queries) + length (snd (run fuel disk queries)) <= fuel.
Proof.
  induction queries as [|[key allowed] rest ih]; intros fuel disk; simpl; [lia|].
  pose proof (serve_limit fuel disk key allowed) as step.
  destruct (serve fuel disk (key, allowed)) as [next value] eqn:served.
  specialize (ih next disk). destruct (run next disk rest) as [remaining values].
  simpl in *. rewrite length_app. lia.
Qed.

Theorem replies_saved : forall queries fuel disk key value,
  In (key, value) (snd (run fuel disk queries)) ->
  disk key = Some value /\ In (key, true) queries.
Proof.
  induction queries as [|[head allowed] rest ih]; intros fuel disk key value found.
  - contradiction.
  - simpl in found.
    destruct (serve fuel disk (head, allowed)) as [next result] eqn:served.
    destruct (run next disk rest) as [remaining values] eqn:ran. simpl in found.
    apply in_app_or in found as [first|later].
    + destruct result as [saved|]; simpl in first; [|contradiction].
      destruct first as [same|[]]. inversion same; subst.
      pose proof (only_saved fuel disk key allowed value) as checked.
      rewrite served in checked. simpl in checked.
      destruct (checked eq_refl) as [authorized stored]. subst allowed.
      split; [exact stored|left; reflexivity].
    + specialize (ih next disk key value). rewrite ran in ih. simpl in ih.
      destruct (ih later) as [stored earlier]. split; [exact stored|right; exact earlier].
Qed.

Print Assumptions only_saved.
Print Assumptions invalid_no_answer.
Print Assumptions reply_limit.
Print Assumptions replies_saved.

Record quota := { used : nat; queued : bool; published : nat }.
Inductive command := Reserve : bool -> command | Send | Restart.

Definition safe limit state :=
  published state + (if queued state then 1 else 0) <= used state /\ used state <= limit.

Definition step limit state action :=
  match action with
  | Reserve stored =>
    if stored && negb (queued state) && (used state <? limit)
    then {| used := S (used state); queued := true; published := published state |}
    else state
  | Send =>
    if queued state
    then {| used := used state; queued := false; published := S (published state) |}
    else state
  | Restart => {| used := used state; queued := false; published := published state |}
  end.

Lemma safe_step : forall limit state action,
  safe limit state -> safe limit (step limit state action).
Proof.
  intros limit [used queued published] action valid.
  unfold safe in *. simpl in *.
  destruct action as [stored| |]; destruct queued; try destruct stored;
    simpl in *; try (split; lia).
  destruct (used <? limit) eqn:room; simpl; split; try lia;
    apply Nat.ltb_lt in room; lia.
Qed.

Fixpoint trace limit state commands :=
  match commands with
  | [] => state
  | action :: rest => trace limit (step limit state action) rest
  end.

Theorem safe_trace : forall commands limit state,
  safe limit state -> safe limit (trace limit state commands).
Proof.
  induction commands as [|action rest ih]; intros limit state valid.
  - exact valid.
  - apply ih. apply safe_step. exact valid.
Qed.

Theorem publication_limit : forall commands limit state,
  safe limit state -> published (trace limit state commands) <= limit.
Proof.
  intros commands limit state valid.
  pose proof (safe_trace commands limit state valid) as [accounted finite].
  destruct (queued (trace limit state commands)); simpl in *; lia.
Qed.

Example restart_keeps_usage :
  published (trace 1 {| used := 0; queued := false; published := 0 |}
    [Reserve true; Send; Restart; Reserve true; Send]) = 1.
Proof. reflexivity. Qed.

Example failed_write_no_send :
  published (trace 1 {| used := 0; queued := false; published := 0 |}
    [Reserve false; Send]) = 0.
Proof. reflexivity. Qed.

Print Assumptions safe_trace.
Print Assumptions publication_limit.
End Fetch.

Module Window.
Inductive phase := Planned | Issued | Finished : nat -> phase.
Record state := { limit : option Z; expires : Z; stage : phase }.
Inductive event := Prepare : Z -> Z -> event | Execute : Z -> event
  | Save : nat -> event | Read.
Inductive effect := Work : Z -> effect | Value : nat -> effect.

Definition step current action : state * list effect :=
  match action, stage current, limit current with
  | Prepare now requested, Planned, Some until =>
    let stop := Z.min until requested in
    if Z.leb now stop then
      ({| limit := Some until; expires := stop; stage := Issued |}, [])
    else (current, [])
  | Execute now, Issued, Some until =>
    if Z.leb now (expires current) && Z.leb (expires current) until
    then (current, [Work now]) else (current, [])
  | Save value, Issued, _ =>
    ({| limit := limit current; expires := expires current;
        stage := Finished value |}, [])
  | Read, Finished value, _ => (current, [Value value])
  | _, _, _ => (current, [])
  end.

Example narrowed :
  step {| limit := Some 112%Z; expires := 0%Z; stage := Planned |}
    (Prepare 100%Z 164%Z) =
  ({| limit := Some 112%Z; expires := 112%Z; stage := Issued |}, []).
Proof. reflexivity. Qed.

Lemma limit_kept : forall current action,
  limit (fst (step current action)) = limit current.
Proof.
  intros [scope expiry phase] action. destruct action, phase, scope;
    simpl; try reflexivity.
  all: repeat match goal with
    | |- context [if ?test then _ else _] => destruct test
    end; reflexivity.
Qed.

Lemma work_scope : forall current action now,
  In (Work now) (snd (step current action)) ->
  exists until, limit current = Some until /\ (now <= until)%Z.
Proof.
  intros [scope expiry phase] action now issued.
  destruct action, phase, scope; simpl in issued; try contradiction.
  - destruct (Z.leb z (Z.min z1 z0)); contradiction.
  - destruct (Z.leb z expiry && Z.leb expiry z0) eqn:valid;
      simpl in issued; [|contradiction].
    destruct issued as [same|[]]. inversion same; subst.
    apply andb_true_iff in valid as [before within].
    apply Z.leb_le in before. apply Z.leb_le in within.
    exists z0. simpl. split; [reflexivity|lia].
  - destruct issued as [same|[]]. discriminate.
  - destruct issued as [same|[]]. discriminate.
Qed.

Fixpoint run actions current : state * list effect :=
  match actions with
  | [] => (current, [])
  | action :: rest =>
    let '(next, first) := step current action in
    let '(last, later) := run rest next in
    (last, first ++ later)
  end.

Theorem history_scope : forall actions current now,
  In (Work now) (snd (run actions current)) ->
  exists until, limit current = Some until /\ (now <= until)%Z.
Proof.
  induction actions as [|action rest ih]; intros current now member;
    simpl in member; [contradiction|].
  pose proof (limit_kept current action) as same.
  pose proof (work_scope current action now) as first.
  destruct (step current action) as [next effects] eqn:stepped.
  destruct (run rest next) as [last later] eqn:ran. simpl in *.
  apply in_app_or in member as [member|member]; [apply first; exact member|].
  specialize (ih next now). rewrite ran in ih. simpl in ih.
  destruct (ih member) as [until [scope valid]].
  exists until. rewrite <- same. auto.
Qed.

Theorem historical_no_work : forall actions current now,
  limit current = None -> ~ In (Work now) (snd (run actions current)).
Proof.
  intros actions current now old member.
  destruct (history_scope actions current now member) as [until [scope _]].
  rewrite old in scope. discriminate.
Qed.

Theorem saved_read : forall scope expiry value,
  step {| limit := scope; expires := expiry; stage := Finished value |} Read =
  ({| limit := scope; expires := expiry; stage := Finished value |}, [Value value]).
Proof. intros [until|]; reflexivity. Qed.

Print Assumptions history_scope.
Print Assumptions historical_no_work.
Print Assumptions saved_read.
End Window.

Module Meter.
Record state := { credit : nat; used : nat }.

Definition charge cost current : option state :=
  if cost <=? credit current then
    Some {| credit := credit current - cost; used := used current + cost |}
  else None.

Fixpoint run costs current : state * list nat :=
  match costs with
  | [] => (current, [])
  | cost :: rest =>
    match charge cost current with
    | None => (current, [])
    | Some next =>
      let '(last, executed) := run rest next in
      (last, cost :: executed)
    end
  end.

Lemma charge_sum : forall cost current next,
  charge cost current = Some next ->
  credit next + used next = credit current + used current /\
  used next = used current + cost.
Proof.
  intros cost [available spent] next charged.
  unfold charge in charged. simpl in charged.
  destruct (cost <=? available) eqn:fits; [|discriminate].
  apply Nat.leb_le in fits. inversion charged; subst. simpl. lia.
Qed.

Theorem prefix_budget : forall costs current,
  let '(last, executed) := run costs current in
  credit last + used last = credit current + used current /\
  used last = used current + fold_right Nat.add 0 executed /\
  exists rest, costs = executed ++ rest.
Proof.
  induction costs as [|cost rest ih]; intros current; simpl.
  - split; [reflexivity|]. split; [lia|exists []; reflexivity].
  - destruct (charge cost current) as [next|] eqn:charged.
    + pose proof (charge_sum cost current next charged) as [total spent].
      specialize (ih next). destruct (run rest next) as [last executed].
      destruct ih as [kept [sum [suffix prefix]]]. simpl.
      split; [lia|]. split; [lia|exists suffix; simpl; congruence].
    + simpl. split; [reflexivity|]. split; [lia|exists (cost :: rest); reflexivity].
Qed.

Theorem initial_limit : forall costs budget,
  used (fst (run costs {| credit := budget; used := 0 |})) <= budget.
Proof.
  intros costs budget. pose proof (prefix_budget costs {| credit := budget; used := 0 |}).
  destruct (run costs {| credit := budget; used := 0 |}) as [last executed].
  simpl in *. lia.
Qed.

Theorem refused_unchanged : forall cost current rest,
  credit current < cost -> run (cost :: rest) current = (current, []).
Proof.
  intros cost current rest short. simpl. unfold charge.
  apply Nat.leb_gt in short. rewrite short. reflexivity.
Qed.

Example stops_before_work :
  run [3; 8; 1] {| credit := 10; used := 0 |} =
    ({| credit := 7; used := 3 |}, [3]).
Proof. reflexivity. Qed.

Print Assumptions prefix_budget.
Print Assumptions initial_limit.
Print Assumptions refused_unchanged.
End Meter.

Module Usage.
Record report := { weight : nat; honest : bool; fuel : option nat }.

Definition truth actual value :=
  honest value = true -> fuel value = Some actual.

Fixpoint support limit (claimed : nat) values :=
  match values with
  | [] => 0
  | value :: rest =>
    let count := match fuel value with
      | Some used => if (used <=? limit) && (used =? claimed) then weight value else 0
      | None => 0
      end in
    count + support limit claimed rest
  end.

Fixpoint faults values :=
  match values with
  | [] => 0
  | value :: rest => (if honest value then 0 else weight value) + faults rest
  end.

Example different_not_counted :
  support 100 30 [{| weight := 1; honest := true; fuel := Some 29 |}] = 0.
Proof. reflexivity. Qed.

Example excessive_not_counted :
  support 28 29 [{| weight := 1; honest := true; fuel := Some 29 |}] = 0.
Proof. reflexivity. Qed.

Theorem wrong_weight : forall values actual limit claimed,
  Forall (truth actual) values -> claimed <> actual ->
  support limit claimed values <= faults values.
Proof.
  induction values as [|value rest ih]; intros actual limit claimed truthful different; simpl.
  - lia.
  - inversion truthful as [|first others valid remaining]; subst.
    specialize (ih actual limit claimed remaining different).
    destruct (honest value) eqn:is_honest.
    + unfold truth in valid. specialize (valid is_honest). rewrite valid.
      assert (actual =? claimed = false) as unequal by (apply Nat.eqb_neq; congruence).
      rewrite unequal, andb_false_r. simpl. exact ih.
    + destruct (fuel value) as [used|]; simpl.
      * destruct ((used <=? limit) && (used =? claimed)); lia.
      * lia.
Qed.

Theorem quorum_meter : forall values costs budget claimed quorum,
  Forall (truth (Meter.used (fst (Meter.run costs
    {| Meter.credit := budget; Meter.used := 0 |})))) values ->
  faults values < quorum -> quorum <= support budget claimed values ->
  claimed = Meter.used (fst (Meter.run costs {| Meter.credit := budget; Meter.used := 0 |}))
  /\ claimed <= budget.
Proof.
  intros values costs budget claimed quorum truthful fewer accepted.
  destruct (Nat.eq_dec claimed
    (Meter.used (fst (Meter.run costs {| Meter.credit := budget; Meter.used := 0 |}))))
    as [same|different].
  - split; [exact same|rewrite same; apply Meter.initial_limit].
  - pose proof (wrong_weight values _ budget claimed truthful different). lia.
Qed.

Print Assumptions wrong_weight.
Print Assumptions quorum_meter.

Definition quote price budget used :=
  if used <=? budget
  then Some (price * used, price * (budget - used)) else None.

Theorem quorum_quote : forall values costs budget claimed quorum price cap,
  Forall (truth (Meter.used (fst (Meter.run costs
    {| Meter.credit := budget; Meter.used := 0 |})))) values ->
  faults values < quorum -> quorum <= support budget claimed values ->
  price * budget <= cap ->
  exists unused,
    quote price budget claimed = Some (price * claimed, unused) /\
    price * claimed <= cap /\ price * claimed + unused = price * budget /\
    price * claimed = price * Meter.used (fst (Meter.run costs
      {| Meter.credit := budget; Meter.used := 0 |})).
Proof.
  intros values costs budget claimed quorum price cap truthful fewer accepted fits.
  pose proof (quorum_meter values costs budget claimed quorum truthful fewer accepted)
    as [actual used_limit].
  exists (price * (budget - claimed)).
  unfold quote. assert ((claimed <=? budget) = true) as allowed
    by (apply Nat.leb_le; exact used_limit).
  rewrite allowed. repeat split; nia.
Qed.

Example excess_quote : quote 2 3 4 = None.
Proof. reflexivity. Qed.

Print Assumptions quorum_quote.
End Usage.

Module Expiry.
Section Entries.
Context {A : Type}.
Variable deadline : A -> Z.

Definition retain now (entries : list A) :=
  filter (fun entry => Z.leb now (deadline entry)) entries.

Definition remove now (entries : list A) :=
  filter (fun entry => negb (Z.leb now (deadline entry))) entries.

Theorem retain_exact : forall now entries entry,
  In entry (retain now entries) <->
  In entry entries /\ (now <= deadline entry)%Z.
Proof.
  intros now entries entry. unfold retain.
  rewrite filter_In, Z.leb_le. reflexivity.
Qed.

Theorem expiry_count : forall now entries,
  length (retain now entries) + length (remove now entries) = length entries.
Proof.
  intros now entries. induction entries as [|entry rest ih].
  - reflexivity.
  - unfold retain, remove in *. simpl.
    destruct (Z.leb now (deadline entry)); simpl; lia.
Qed.

Theorem room_after_expiry : forall now entries cap,
  length entries <= cap ->
  (exists entry, In entry entries /\ (deadline entry < now)%Z) ->
  length (retain now entries) < cap.
Proof.
  intros now entries cap fits [entry [member expired]].
  assert (In entry (remove now entries)) as removed.
  { unfold remove. apply filter_In. split; [exact member|].
    apply Bool.negb_true_iff, Z.leb_gt. exact expired. }
  pose proof (expiry_count now entries).
  destruct (remove now entries); simpl in *; [contradiction|lia].
Qed.

Theorem no_live_eviction : forall now entries,
  Forall (fun entry => (now <= deadline entry)%Z) entries ->
  retain now entries = entries.
Proof.
  intros now entries live. induction live as [|entry rest valid live ih].
  - reflexivity.
  - unfold retain in *. simpl.
    apply Z.leb_le in valid. rewrite valid, ih. reflexivity.
Qed.

Theorem retain_advance : forall entries earlier later,
  (earlier <= later)%Z ->
  retain later (retain earlier entries) = retain later entries.
Proof.
  induction entries as [|entry rest ih]; intros earlier later monotone.
  - reflexivity.
  - unfold retain in *. simpl.
    destruct (Z.leb earlier (deadline entry)) eqn:old;
      destruct (Z.leb later (deadline entry)) eqn:current; simpl.
    + rewrite current, ih by exact monotone. reflexivity.
    + rewrite current, ih by exact monotone. reflexivity.
    + apply Z.leb_gt in old. apply Z.leb_le in current. lia.
    + apply ih. exact monotone.
Qed.

Print Assumptions retain_exact.
Print Assumptions expiry_count.
Print Assumptions room_after_expiry.
Print Assumptions no_live_eviction.
Print Assumptions retain_advance.
End Entries.
End Expiry.

Module Cancellation.
Definition entries := list (nat * Z).

Fixpoint contains key (state : entries) :=
  match state with
  | [] => false
  | (current, _) :: rest => Nat.eqb key current || contains key rest
  end.

Fixpoint put key expiry (state : entries) :=
  match state with
  | [] => [(key, expiry)]
  | (current, previous) :: rest =>
    if Nat.eqb key current then (key, expiry) :: rest
    else (current, previous) :: put key expiry rest
  end.

Definition record cap now ttl key state :=
  let live := Expiry.retain snd now state in
  if contains key live || Nat.ltb (length live) cap
  then (put key (now + ttl)%Z live, true)
  else (live, false).

Lemma put_size : forall state key expiry,
  length (put key expiry state) =
    if contains key state then length state else S (length state).
Proof.
  induction state as [|[current previous] rest ih]; intros key expiry.
  - reflexivity.
  - simpl. destruct (Nat.eqb key current); simpl; [reflexivity|].
    rewrite ih. destruct (contains key rest); reflexivity.
Qed.

Lemma put_present : forall state key expiry,
  In (key, expiry) (put key expiry state).
Proof.
  induction state as [|[current previous] rest ih]; intros key expiry.
  - simpl. auto.
  - simpl. destruct (Nat.eqb key current); simpl; auto.
Qed.

Lemma put_other : forall state key expiry old deadline,
  old <> key -> In (old, deadline) state ->
  In (old, deadline) (put key expiry state).
Proof.
  induction state as [|[current previous] rest ih]; intros key expiry old deadline ne member.
  - contradiction.
  - simpl in *. destruct (Nat.eqb key current) eqn:same.
    + apply Nat.eqb_eq in same. simpl.
      destruct member as [equal|member]; [inversion equal; subst; contradiction|auto].
    + simpl. destruct member as [equal|member]; [auto|].
      right. eapply ih; eauto.
Qed.

Theorem capacity_kept : forall cap now ttl key state,
  length state <= cap ->
  length (fst (record cap now ttl key state)) <= cap.
Proof.
  intros cap now ttl key state fits.
  pose proof (Expiry.expiry_count snd now state) as count.
  unfold record. destruct (contains key (Expiry.retain snd now state)) eqn:found;
    destruct (Nat.ltb (length (Expiry.retain snd now state)) cap) eqn:room;
    simpl; try rewrite put_size, found; try lia.
  apply Nat.ltb_lt in room. lia.
Qed.

Theorem acknowledged_present : forall cap now ttl key state,
  snd (record cap now ttl key state) = true ->
  In (key, (now + ttl)%Z) (fst (record cap now ttl key state)).
Proof.
  intros cap now ttl key state accepted. unfold record in *.
  destruct (contains key (Expiry.retain snd now state)
    || Nat.ltb (length (Expiry.retain snd now state)) cap);
    simpl in *; [apply put_present|discriminate].
Qed.

Theorem full_refused : forall cap now ttl key state,
  contains key (Expiry.retain snd now state) = false ->
  cap <= length (Expiry.retain snd now state) ->
  record cap now ttl key state = (Expiry.retain snd now state, false).
Proof.
  intros cap now ttl key state absent full. unfold record.
  rewrite absent. apply Nat.ltb_ge in full. rewrite full. reflexivity.
Qed.

Theorem live_cancel_kept : forall cap now ttl key old deadline state,
  old <> key -> In (old, deadline) state -> (now <= deadline)%Z ->
  In (old, deadline) (fst (record cap now ttl key state)).
Proof.
  intros cap now ttl key old deadline state ne member live.
  assert (In (old, deadline) (Expiry.retain snd now state)) as retained.
  { apply Expiry.retain_exact. simpl. auto. }
  unfold record. destruct (contains key (Expiry.retain snd now state)
    || Nat.ltb (length (Expiry.retain snd now state)) cap); simpl.
  - apply put_other; assumption.
  - exact retained.
Qed.

Theorem flood_preserves_cancel : forall keys cap now ttl old deadline state,
  Forall (fun key => old <> key) keys ->
  In (old, deadline) state -> (now <= deadline)%Z ->
  In (old, deadline)
    (fold_left (fun state key => fst (record cap now ttl key state)) keys state).
Proof.
  induction keys as [|key rest ih]; intros cap now ttl old deadline state distinct member live.
  - exact member.
  - inversion distinct; subst. simpl. apply ih; try assumption.
    apply live_cancel_kept; assumption.
Qed.

Print Assumptions capacity_kept.
Print Assumptions acknowledged_present.
Print Assumptions full_refused.
Print Assumptions live_cancel_kept.
Print Assumptions flood_preserves_cancel.
End Cancellation.

Module Slots.
Definition owned (owner : nat -> nat) caller (slots : list nat) :=
  length (filter (fun key => Nat.eqb (owner key) caller) slots).

Definition claim cap quota owner key slots :=
  if existsb (Nat.eqb key) slots then (slots, true)
  else if Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota
  then (key :: slots, true)
  else (slots, false).

Lemma present : forall key slots,
  existsb (Nat.eqb key) slots = true <-> In key slots.
Proof.
  intros key slots. rewrite existsb_exists. split.
  - intros [value [member equal]]. apply Nat.eqb_eq in equal. subst. exact member.
  - intros member. exists key. split; [exact member|apply Nat.eqb_refl].
Qed.

Theorem quotas_kept : forall cap quota owner key slots,
  length slots <= cap ->
  (forall caller, owned owner caller slots <= quota) ->
  length (fst (claim cap quota owner key slots)) <= cap
  /\ (forall caller, owned owner caller (fst (claim cap quota owner key slots)) <= quota).
Proof.
  intros cap quota owner key slots global local. unfold claim.
  destruct (existsb (Nat.eqb key) slots); simpl; [auto|].
  destruct (Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota) eqn:room; simpl; [|auto].
  apply andb_true_iff in room. destruct room as [space own].
  apply Nat.ltb_lt in space. apply Nat.ltb_lt in own. split; [lia|].
  intros caller. unfold owned. simpl.
  destruct (Nat.eqb (owner key) caller) eqn:same; simpl.
  - apply Nat.eqb_eq in same. subst caller. unfold owned in own. lia.
  - apply local.
Qed.

Theorem reserved_cancel : forall cap quota owner key slots,
  In key slots -> claim cap quota owner key slots = (slots, true).
Proof.
  intros cap quota owner key slots member. unfold claim.
  apply present in member. rewrite member. reflexivity.
Qed.

Theorem acknowledged_present : forall cap quota owner key slots,
  snd (claim cap quota owner key slots) = true ->
  In key (fst (claim cap quota owner key slots)).
Proof.
  intros cap quota owner key slots accepted. unfold claim in *.
  destruct (existsb (Nat.eqb key) slots) eqn:member; simpl; [apply present; exact member|].
  destruct (Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota); simpl in *; [auto|discriminate].
Qed.

Theorem caller_full : forall cap quota owner key slots,
  ~ In key slots -> quota <= owned owner (owner key) slots ->
  claim cap quota owner key slots = (slots, false).
Proof.
  intros cap quota owner key slots absent full. unfold claim.
  destruct (existsb (Nat.eqb key) slots) eqn:member.
  - apply present in member. contradiction.
  - apply Nat.ltb_ge in full. rewrite full, andb_false_r. reflexivity.
Qed.

Theorem distinct_kept : forall cap quota owner key slots,
  NoDup slots -> NoDup (fst (claim cap quota owner key slots)).
Proof.
  intros cap quota owner key slots unique. unfold claim.
  destruct (existsb (Nat.eqb key) slots) eqn:member; simpl; [exact unique|].
  destruct (Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota); simpl; [|exact unique].
  constructor; [|exact unique]. intro found. apply present in found. congruence.
Qed.

Theorem reservation_kept : forall cap quota owner key saved slots,
  In saved slots -> In saved (fst (claim cap quota owner key slots)).
Proof.
  intros cap quota owner key saved slots member. unfold claim.
  destruct (existsb (Nat.eqb key) slots); simpl; [exact member|].
  destruct (Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota); simpl; auto.
Qed.

Theorem flood_preserves_slot : forall attempts cap quota owner saved slots,
  In saved slots ->
  In saved (fold_left (fun state key => fst (claim cap quota owner key state)) attempts slots).
Proof.
  induction attempts as [|key rest ih]; intros cap quota owner saved slots member.
  - exact member.
  - simpl. apply ih. apply reservation_kept. exact member.
Qed.

Print Assumptions quotas_kept.
Print Assumptions reserved_cancel.
Print Assumptions acknowledged_present.
Print Assumptions caller_full.
Print Assumptions distinct_kept.
Print Assumptions reservation_kept.
Print Assumptions flood_preserves_slot.
End Slots.

Fixpoint path_ok (index : nat) (sides : list bool) : bool :=
  match sides with
  | [] => Nat.eqb index 0
  | bit :: rest =>
      andb (Nat.eqb (index mod 2) (if bit then 1 else 0))
        (path_ok (index / 2) rest)
  end.

Theorem path_index_unique :
  forall sides left right,
    path_ok left sides = true ->
    path_ok right sides = true ->
    left = right.
Proof.
  induction sides as [|side rest ih]; intros left right hl hr; simpl in *.
  - apply Nat.eqb_eq in hl. apply Nat.eqb_eq in hr. lia.
  - apply Bool.andb_true_iff in hl as [hl pl].
    apply Bool.andb_true_iff in hr as [hr pr].
    apply Nat.eqb_eq in hl. apply Nat.eqb_eq in hr.
    change (left mod 2 = (if side then 1 else 0)) in hl.
    change (right mod 2 = (if side then 1 else 0)) in hr.
    pose proof (ih (left / 2) (right / 2) pl pr).
    pose proof (Nat.div_mod left 2 ltac:(lia)).
    pose proof (Nat.div_mod right 2 ltac:(lia)).
    lia.
Qed.

Theorem path_relabel_rejected :
  forall sides left right,
    path_ok left sides = true -> left <> right ->
    path_ok right sides = false.
Proof.
  intros sides left right hl ne.
  destruct (path_ok right sides) eqn:hr; [|reflexivity].
  exfalso. apply ne. eapply path_index_unique; eauto.
Qed.

Definition quorum_safe total quorum byzantine :=
  3 * quorum > 2 * total /\ 3 * byzantine < total.

Theorem valid_qc_intersection :
  forall total quorum byzantine,
    quorum_safe total quorum byzantine ->
    quorum + quorum > total + byzantine.
Proof.
  unfold quorum_safe.
  intros total quorum byzantine assumptions.
  destruct assumptions.
  lia.
Qed.

Theorem conflicting_qc_impossible :
  forall total quorum byzantine,
    quorum_safe total quorum byzantine ->
    ~(quorum + quorum <= total + byzantine).
Proof.
  intros total quorum byzantine assumptions contradiction.
  pose proof (valid_qc_intersection total quorum byzantine assumptions).
  lia.
Qed.

Module Shares.
Local Open Scope Z_scope.

Definition quorum weight := 2 * weight / 3 + 1.

Lemma quorum_range : forall weight,
  0 < weight -> 2 * weight < 3 * quorum weight /\ quorum weight <= weight.
Proof.
  intros weight positive. unfold quorum.
  pose proof (Z.div_mod (2 * weight) 3 ltac:(lia)).
  pose proof (Z.mod_pos_bound (2 * weight) 3 ltac:(lia)).
  nia.
Qed.

Theorem quorum_overlap : forall weight faulty,
  0 < weight -> 0 <= faulty -> 3 * faulty < weight ->
  faulty < Z.max 0 (2 * quorum weight - weight).
Proof.
  intros weight faulty positive nonnegative minority.
  pose proof (quorum_range weight positive).
  pose proof (Z.le_max_r 0 (2 * quorum weight - weight)).
  nia.
Qed.

Fixpoint total (values : list Z) : Z :=
  match values with [] => 0 | x :: xs => x + total xs end.

Definition parts budget weight values :=
  map (fun value => budget * value / weight) values.

Lemma part_error : forall budget weight value,
  0 < weight ->
  0 <= budget * value - (budget * value / weight) * weight <= weight - 1.
Proof.
  intros budget weight value positive.
  pose proof (Z.div_mod (budget * value) weight ltac:(lia)).
  pose proof (Z.mod_pos_bound (budget * value) weight positive).
  nia.
Qed.

Lemma total_error : forall values budget weight,
  0 < weight ->
  0 <= budget * total values - total (parts budget weight values) * weight
    <= Z.of_nat (length values) * (weight - 1).
Proof.
  induction values as [|value rest ih]; intros budget weight positive.
  - simpl. unfold parts. simpl. lia.
  - specialize (ih budget weight positive).
    pose proof (part_error budget weight value positive).
    unfold parts in *.
    change (0 <= budget * (value + total rest) -
      (budget * value / weight + total (map (fun x => budget * x / weight) rest)) * weight
      <= Z.of_nat (S (length rest)) * (weight - 1)).
    rewrite Nat2Z.inj_succ. nia.
Qed.

Lemma residue_range : forall values budget,
  0 < total values ->
  let residue := budget - total (parts budget (total values) values) in
  0 <= residue < Z.of_nat (length values).
Proof.
  intros values budget positive.
  pose proof (total_error values budget (total values) positive).
  assert (0 < Z.of_nat (length values)).
  { destruct values; simpl in *; lia. }
  cbn zeta. nia.
Qed.

Lemma base_budget : forall values budget,
  0 < total values ->
  total (parts budget (total values) values) <= budget.
Proof.
  intros values budget positive.
  pose proof (residue_range values budget positive).
  cbn zeta in *. lia.
Qed.

Fixpoint dust count values :=
  match count, values with
  | S rest, value :: suffix => value + 1 :: dust rest suffix
  | _, _ => values
  end.

Lemma dust_sum : forall count values,
  (count <= length values)%nat ->
  total (dust count values) = total values + Z.of_nat count.
Proof.
  induction count as [|count ih]; intros values fits.
  - simpl. lia.
  - destruct values as [|value rest]; simpl in fits; try lia.
    simpl dust. simpl total.
    rewrite ih by lia. rewrite Nat2Z.inj_succ. lia.
Qed.

Theorem payout_sum : forall values budget,
  0 < total values ->
  let base := parts budget (total values) values in
  total (dust (Z.to_nat (budget - total base)) base) = budget.
Proof.
  intros values budget positive. cbn zeta.
  pose proof (residue_range values budget positive) as range.
  cbn zeta in range.
  rewrite dust_sum.
  - rewrite Z2Nat.id by lia. lia.
  - unfold parts in *. rewrite length_map.
    apply Nat2Z.inj_le. rewrite Z2Nat.id by lia. lia.
Qed.

Lemma parts_pos : forall values budget weight,
  Forall (fun x => 0 <= x) values -> 0 <= budget -> 0 < weight ->
  Forall (fun x => 0 <= x) (parts budget weight values).
Proof.
  intros values budget weight positive money units.
  unfold parts. induction positive; simpl; constructor.
  - apply Z.div_pos; [apply Z.mul_nonneg_nonneg; assumption|exact units].
  - assumption.
Qed.

Lemma dust_pos : forall count values,
  Forall (fun x => 0 <= x) values ->
  Forall (fun x => 0 <= x) (dust count values).
Proof.
  induction count as [|count ih]; intros values positive; [exact positive|].
  destruct values as [|value rest]; [constructor|].
  inversion positive; subst. simpl dust. constructor; [lia|].
  apply ih. assumption.
Qed.

Lemma total_pos : forall values,
  Forall (fun x => 0 <= x) values -> 0 <= total values.
Proof.
  intros values positive. induction positive; simpl; lia.
Qed.

Lemma member_total : forall values value,
  Forall (fun x => 0 <= x) values -> In value values -> value <= total values.
Proof.
  induction values as [|head rest ih]; intros value positive member.
  - inversion member.
  - inversion positive; subst. simpl in member. simpl total.
    pose proof (total_pos rest ltac:(assumption)).
    destruct member as [same|member]; [subst; lia|].
    specialize (ih value ltac:(assumption) member). lia.
Qed.

Theorem payout_range : forall values budget,
  Forall (fun x => 0 <= x) values -> 0 < total values ->
  0 <= budget <= 9223372036854775807 ->
  let base := parts budget (total values) values in
  Forall (fun x => 0 <= x <= 9223372036854775807)
    (dust (Z.to_nat (budget - total base)) base).
Proof.
  intros values budget positive units money. cbn zeta.
  pose proof (payout_sum values budget units) as paid. cbn zeta in paid.
  pose proof (parts_pos values budget (total values) positive ltac:(lia) units) as base.
  pose proof (dust_pos (Z.to_nat (budget - total (parts budget (total values) values)))
    (parts budget (total values) values) base) as nonnegative.
  rewrite Forall_forall. intros value member.
  pose proof (member_total _ value nonnegative member).
  rewrite Forall_forall in nonnegative. specialize (nonnegative value member).
  lia.
Qed.

Print Assumptions payout_sum.
Print Assumptions payout_range.
End Shares.

Record attestation := {
  attestation_weight : nat;
  attestation_valid : bool
}.

Definition attestation_influence value :=
  match attestation_valid value with
  | true => attestation_weight value
  | false => 0
  end.

Fixpoint influence values :=
  match values with
  | nil => 0
  | cons head suffix => attestation_influence head + influence suffix
  end.

Theorem attestation_sybil_invariance :
  forall left right rest,
    influence ({| attestation_weight := left + right; attestation_valid := true |} :: rest)
    =
    influence
      ({| attestation_weight := left; attestation_valid := true |}
       :: {| attestation_weight := right; attestation_valid := true |}
       :: rest).
Proof.
  intros left right rest.
  simpl.
  rewrite Nat.add_assoc.
  reflexivity.
Qed.

Fixpoint pow_nat base exponent :=
  match exponent with
  | 0 => 1
  | S previous => base * pow_nat base previous
  end.

Fixpoint binom total selected :=
  match total, selected with
  | _, 0 => 1
  | 0, S _ => 0
  | S total_previous, S selected_previous =>
      binom total_previous selected_previous + binom total_previous (S selected_previous)
  end.

Definition weighted_binomial_term total selected adversarial honest :=
  binom total selected
  * pow_nat adversarial selected
  * pow_nat honest (total - selected).

Fixpoint weighted_threshold_count total selected count adversarial honest :=
  match count with
  | 0 => 0
  | S rest =>
      weighted_binomial_term total selected adversarial honest
      + weighted_threshold_count total (S selected) rest adversarial honest
  end.

Definition capture_threshold committee_size :=
  committee_size / 3 + 1.

Definition weighted_capture_numerator committee_size threshold adversarial honest :=
  if threshold <=? committee_size then
    weighted_threshold_count
      committee_size
      threshold
      (S (committee_size - threshold))
      adversarial
      honest
  else 0.

Definition probability_denominator committee_size adversarial honest :=
  pow_nat (adversarial + honest) committee_size.

Definition rational_limit_holds committee_size threshold adversarial honest limit_num limit_den :=
  weighted_capture_numerator committee_size threshold adversarial honest * limit_den
  <= limit_num * probability_denominator committee_size adversarial honest.

Definition committee_captured committee_size adversarial_selected :=
  3 * adversarial_selected > committee_size.

Theorem capture_zero_above_size :
  forall committee_size threshold adversarial honest,
    threshold > committee_size ->
    weighted_capture_numerator committee_size threshold adversarial honest = 0.
Proof.
  intros committee_size threshold adversarial honest greater.
  unfold weighted_capture_numerator.
  apply Nat.leb_gt in greater.
  rewrite greater.
  reflexivity.
Qed.

Theorem capture_false_below_threshold :
  forall committee_size adversarial_selected,
    3 * adversarial_selected <= committee_size ->
    ~ committee_captured committee_size adversarial_selected.
Proof.
  intros committee_size adversarial_selected within_limit captured.
  unfold committee_captured in captured.
  lia.
Qed.

Example binom_5_2 :
  binom 5 2 = 10.
Proof.
  reflexivity.
Qed.

Example capture_half_5_threshold_2 :
  weighted_capture_numerator 5 2 1 1 = 26.
Proof.
  reflexivity.
Qed.

Example capture_denominator_half_5 :
  probability_denominator 5 1 1 = 32.
Proof.
  reflexivity.
Qed.

Theorem capture_limit_half_5 :
  rational_limit_holds 5 2 1 1 13 16.
Proof.
  unfold rational_limit_holds.
  simpl.
  lia.
Qed.

Theorem capture_limit_fifth_5 :
  rational_limit_holds 5 2 1 4 1 3.
Proof.
  unfold rational_limit_holds.
  simpl.
  lia.
Qed.

Record catchup_state := {
  catchup_verified : bool;
  catchup_roots_match : bool;
  catchup_ranges_complete : bool
}.

Definition ready_to_vote state :=
  catchup_verified state = true
  /\ catchup_roots_match state = true
  /\ catchup_ranges_complete state = true.

Theorem ready_to_vote_requires_verified_catchup :
  forall state,
    ready_to_vote state ->
    catchup_verified state = true.
Proof.
  intros state ready.
  unfold ready_to_vote in ready.
  destruct ready as [verified _].
  exact verified.
Qed.

Module CancelSend.

Record notice := Notice { payload : nat; expiry : Z; used : nat }.
Record state := State { closed : bool; queued : option notice }.

Definition save now deadline body value :=
  if Z.leb now deadline then
    let live := match queued value with
      | Some row => if Z.leb now (expiry row) then Some row else None
      | None => None
      end in
    match live with
    | Some row => if Nat.eqb body (payload row)
        then Some (State true (Some row)) else None
    | None => Some (State true (Some (Notice body deadline 0)))
    end
  else None.

Definition claim limit now value :=
  match queued value with
  | None => (value, None)
  | Some row =>
    if Z.leb now (expiry row) then
      if closed value && Nat.ltb (used row) limit
      then (State true (Some (Notice (payload row) (expiry row) (S (used row)))),
        Some (payload row))
      else (value, None)
    else (State (closed value) None, None)
  end.

Definition credit limit value := match queued value with
  | Some row => if closed value then limit - used row else 0
  | None => 0
  end.

Definition count (output : option nat) := match output with Some _ => 1 | None => 0 end.

Theorem saved_only : forall limit now value next body,
  claim limit now value = (next, Some body) ->
  exists row, queued value = Some row /\ closed value = true /\
    payload row = body /\ (now <= expiry row)%Z /\ used row < limit /\
    queued next = Some (Notice body (expiry row) (S (used row))).
Proof.
  intros limit now value next body issued. unfold claim in issued.
  destruct (queued value) as [row|] eqn:pending; [|discriminate].
  destruct (Z.leb now (expiry row)) eqn:live; [|discriminate].
  destruct (closed value && (used row <? limit)) eqn:room; [|discriminate].
  apply andb_true_iff in room. destruct room as [checked room].
  apply Nat.ltb_lt in room. apply Z.leb_le in live.
  inversion issued; subst. exists row. simpl. repeat split; assumption || reflexivity.
Qed.

Theorem duplicate_keeps : forall now deadline body row value,
  (now <= expiry row)%Z -> payload row = body ->
  save now deadline body (State true (Some row)) = Some value ->
  value = State true (Some row).
Proof.
  intros now deadline body row value live same saved.
  unfold save in saved; simpl in saved.
  destruct (Z.leb now deadline); [|discriminate].
  apply Z.leb_le in live. rewrite live in saved.
  rewrite same, Nat.eqb_refl in saved. inversion saved. reflexivity.
Qed.

Theorem claim_credit : forall limit now value next output,
  claim limit now value = (next, output) ->
  count output + credit limit next <= credit limit value.
Proof.
  intros limit now value next output issued. unfold claim in issued.
  destruct (queued value) as [row|] eqn:pending.
  - destruct (Z.leb now (expiry row)).
    + destruct (closed value && (used row <? limit)) eqn:room.
      * apply andb_true_iff in room. destruct room as [checked room].
        apply Nat.ltb_lt in room.
        inversion issued; subst. unfold credit, count. rewrite pending, checked. simpl. lia.
      * inversion issued; subst. unfold count; simpl. lia.
    + inversion issued; subst. unfold count, credit; simpl. lia.
  - inversion issued; subst. unfold count; simpl. lia.
Qed.

Theorem remains_closed : forall limit now value,
  closed (fst (claim limit now value)) = closed value.
Proof.
  intros limit now value. unfold claim.
  destruct (queued value) as [row|]; [|reflexivity].
  destruct (Z.leb now (expiry row)); [|reflexivity].
  destruct (closed value) eqn:checked; simpl; [|congruence].
  destruct (used row <? limit); simpl; congruence.
Qed.

Inductive event := Tick (now : Z) | Restart.

Definition step limit value event := match event with
  | Tick now => claim limit now value
  | Restart => (value, None)
  end.

Fixpoint run limit events value := match events with
  | [] => (value, 0)
  | event :: rest =>
    let '(next, output) := step limit value event in
    let '(final, total) := run limit rest next in
    (final, count output + total)
  end.

Theorem trace_credit : forall events limit value,
  snd (run limit events value) + credit limit (fst (run limit events value))
    <= credit limit value.
Proof.
  induction events as [|event rest ih]; intros limit value; simpl; [lia|].
  destruct (step limit value event) as [next output] eqn:issued.
  specialize (ih limit next). destruct (run limit rest next) as [final total].
  simpl in *.
  assert (count output + credit limit next <= credit limit value) as paid.
  { destruct event; simpl in issued.
    - eapply claim_credit; eauto.
    - inversion issued; subst. simpl. lia. }
  lia.
Qed.

Corollary finite_sends : forall events limit row,
  used row = 0 ->
  snd (run limit events (State true (Some row))) <= limit.
Proof.
  intros events limit row empty.
  pose proof (trace_credit events limit (State true (Some row))).
  unfold credit in H; simpl in H. rewrite empty in H. lia.
Qed.

Example restart_not_reset :
  snd (run 2 [Tick 1; Restart; Tick 2; Tick 3]
    (State true (Some (Notice 7 64 0)))) = 2.
Proof. reflexivity. Qed.

Example expired_stays_closed :
  fst (claim 64 65 (State true (Some (Notice 7 64 1)))) = State true None.
Proof. reflexivity. Qed.

Print Assumptions saved_only.
Print Assumptions duplicate_keeps.
Print Assumptions trace_credit.
Print Assumptions finite_sends.
End CancelSend.

Module CancelPace.

Definition claim gap next now ready saved :=
  if (next <=? now) && ready && saved
  then (now + gap, true) else (next, false).

Theorem accepted : forall gap next now ready saved,
  snd (claim gap next now ready saved) = true ->
  next <= now /\ ready = true /\ saved = true /\
  fst (claim gap next now ready saved) = now + gap.
Proof.
  intros gap next now ready saved ok. unfold claim in *.
  destruct ((next <=? now) && ready && saved) eqn:valid; simpl in *;
    try discriminate.
  apply andb_true_iff in valid as [valid wrote].
  apply andb_true_iff in valid as [time live].
  apply Nat.leb_le in time. repeat split; auto.
Qed.

Theorem monotone : forall gap next now ready saved,
  next <= fst (claim gap next now ready saved).
Proof.
  intros. unfold claim.
  destruct ((next <=? now) && ready && saved) eqn:valid; simpl; [|lia].
  apply andb_true_iff in valid as [valid _].
  apply andb_true_iff in valid as [time _]. apply Nat.leb_le in time. lia.
Qed.

Inductive event := Attempt (now : nat) (ready saved : bool) | Restart.

Definition step gap next action := match action with
  | Attempt now ready saved => fst (claim gap next now ready saved)
  | Restart => next
  end.

Fixpoint run gap next actions := match actions with
  | [] => next
  | action :: rest => run gap (step gap next action) rest
  end.

Theorem history_monotone : forall actions gap next,
  next <= run gap next actions.
Proof.
  induction actions as [|action rest ih]; intros; simpl; [lia|].
  specialize (ih gap (step gap next action)).
  assert (next <= step gap next action).
  { destruct action; simpl; [apply monotone|lia]. }
  lia.
Qed.

Theorem send_spacing : forall actions gap next now ready saved later live wrote,
  snd (claim gap next now ready saved) = true ->
  snd (claim gap (run gap (fst (claim gap next now ready saved)) actions)
    later live wrote) = true -> now + gap <= later.
Proof.
  intros actions gap next now ready saved later live wrote first second.
  apply accepted in first as [_ [_ [_ first]]]. rewrite first in second.
  apply accepted in second as [second _].
  pose proof (history_monotone actions gap (now + gap)). lia.
Qed.

Example restart_keeps_delay :
  claim 3000 (run 3000 0 [Attempt 1000 true true; Restart]) 3999 true true =
  (4000, false).
Proof. reflexivity. Qed.

Print Assumptions send_spacing.
Print Assumptions history_monotone.
End CancelPace.

Module RelayQuota.

Definition claim limit gap now state :=
  let '(deadline, used) := state in
  let '(deadline, used) := if deadline <=? now then (now + gap, 0) else state in
  if used <? limit then ((deadline, S used), 1) else ((deadline, used), 0).

Inductive event := Attempt (now : nat) | Restart.

Definition before deadline action := match action with
  | Attempt now => now < deadline
  | Restart => True
  end.

Fixpoint run limit gap state actions := match actions with
  | [] => (state, 0)
  | Restart :: rest => run limit gap state rest
  | Attempt now :: rest =>
    let '(next, sent) := claim limit gap now state in
    let '(last, later) := run limit gap next rest in
    (last, sent + later)
  end.

Theorem window_count : forall actions limit gap deadline used last sent,
  used <= limit -> Forall (before deadline) actions ->
  run limit gap (deadline, used) actions = (last, sent) ->
  fst last = deadline /\ snd last = used + sent /\ snd last <= limit.
Proof.
  induction actions as [|action rest ih]; intros limit gap deadline used last sent cap times result.
  - simpl in result. inversion result; subst. simpl. lia.
  - inversion times as [|? ? early later]; subst.
    destruct action as [now|]; simpl in result.
    + unfold claim in result. simpl in early.
      assert ((deadline <=? now) = false) as wait by (apply Nat.leb_gt; lia).
      rewrite wait in result. destruct (used <? limit) eqn:room.
      * apply Nat.ltb_lt in room.
        destruct (run limit gap (deadline, S used) rest) as [finish amount] eqn:replay.
        specialize (ih limit gap deadline (S used) finish amount ltac:(lia) later replay).
        inversion result; subst. simpl in *. lia.
      * apply Nat.ltb_ge in room.
        destruct (run limit gap (deadline, used) rest) as [finish amount] eqn:replay.
        specialize (ih limit gap deadline used finish amount cap later replay).
        inversion result; subst. simpl in *. lia.
    + eapply ih; eauto.
Qed.

Corollary finite_output : forall actions limit gap deadline used last sent,
  used <= limit -> Forall (before deadline) actions ->
  run limit gap (deadline, used) actions = (last, sent) -> sent <= limit - used.
Proof.
  intros. pose proof (window_count actions limit gap deadline used last sent H H0 H1).
  lia.
Qed.

Theorem paired_output : forall work control gap work_end control_end
  work_used control_used work_last control_last work_sent control_sent,
  work_used <= 160 -> control_used <= 32 ->
  Forall (before work_end) work -> Forall (before control_end) control ->
  run 160 gap (work_end, work_used) work = (work_last, work_sent) ->
  run 32 gap (control_end, control_used) control = (control_last, control_sent) ->
  work_sent + control_sent <= 192 /\ control_sent <= 32 - control_used.
Proof.
  intros work control gap work_end control_end work_used control_used
    work_last control_last work_sent control_sent wc cc wt ct wr cr.
  pose proof (finite_output work 160 gap work_end work_used work_last work_sent wc wt wr).
  pose proof (finite_output control 32 gap control_end control_used control_last control_sent cc ct cr).
  lia.
Qed.

Theorem transit_output : forall work control transit gap work_end control_end transit_end
  work_used control_used transit_used work_last control_last transit_last
  work_sent control_sent transit_sent,
  work_used <= 160 -> control_used <= 32 -> transit_used <= 32 ->
  Forall (before work_end) work -> Forall (before control_end) control ->
  Forall (before transit_end) transit ->
  run 160 gap (work_end, work_used) work = (work_last, work_sent) ->
  run 32 gap (control_end, control_used) control = (control_last, control_sent) ->
  run 32 gap (transit_end, transit_used) transit = (transit_last, transit_sent) ->
  work_sent + control_sent + transit_sent <= 224 /\
  work_sent <= 160 - work_used /\ control_sent <= 32 - control_used.
Proof.
  intros work control transit gap work_end control_end transit_end
    work_used control_used transit_used work_last control_last transit_last
    work_sent control_sent transit_sent wc cc tc wt ct tt wr cr tr.
  pose proof (finite_output work 160 gap work_end work_used work_last work_sent wc wt wr).
  pose proof (finite_output control 32 gap control_end control_used control_last control_sent cc ct cr).
  pose proof (finite_output transit 32 gap transit_end transit_used transit_last transit_sent tc tt tr).
  lia.
Qed.

Definition transit_step limit gap now (state : (nat * nat) * (nat * nat) * (nat * nat)) :=
  let '(work, control, transit) := state in
  (work, control, fst (claim limit gap now transit)).

Fixpoint transit_run limit gap state times := match times with
  | [] => state
  | now :: rest => transit_run limit gap (transit_step limit gap now state) rest
  end.

Theorem transit_keeps_work : forall times limit gap work control transit,
  let '(next_work, next_control, _) := transit_run limit gap (work, control, transit) times in
  next_work = work /\ next_control = control.
Proof.
  induction times as [|now rest ih]; intros; simpl; [auto|apply ih].
Qed.

Definition round_claim limit gap now round saved quota :=
  if saved <? round then
    let '(next, sent) := claim limit gap now quota in
    if sent =? 1 then (round, next) else (saved, quota)
  else (saved, quota).

Theorem repeated_round_keeps_quota : forall limit gap now round saved quota,
  round <= saved -> round_claim limit gap now round saved quota = (saved, quota).
Proof.
  intros. unfold round_claim.
  assert ((saved <? round) = false) by (apply Nat.ltb_ge; lia).
  rewrite H0. reflexivity.
Qed.

Theorem full_quota_keeps_round : forall limit gap now round saved deadline used,
  now < deadline -> limit <= used ->
  round_claim limit gap now round saved (deadline, used) = (saved, (deadline, used)).
Proof.
  intros. unfold round_claim, claim.
  assert ((deadline <=? now) = false) by (apply Nat.leb_gt; lia).
  assert ((used <? limit) = false) by (apply Nat.ltb_ge; lia).
  destruct (saved <? round); rewrite ?H1, ?H2; reflexivity.
Qed.

Theorem fresh_round_consumes_one : forall limit gap now round saved deadline used,
  saved < round -> now < deadline -> used < limit ->
  round_claim limit gap now round saved (deadline, used) = (round, (deadline, S used)).
Proof.
  intros. unfold round_claim, claim.
  assert ((saved <? round) = true) by (apply Nat.ltb_lt; lia).
  assert ((deadline <=? now) = false) by (apply Nat.leb_gt; lia).
  assert ((used <? limit) = true) by (apply Nat.ltb_lt; lia).
  rewrite H2, H3, H4. reflexivity.
Qed.

Example mixed_restarts :
  run 2 60000 (60000, 0) [Attempt 100; Restart; Attempt 200; Restart; Attempt 300]
    = ((60000, 2), 2).
Proof. reflexivity. Qed.

Print Assumptions window_count.
Print Assumptions finite_output.
Print Assumptions paired_output.
Print Assumptions transit_output.
Print Assumptions transit_keeps_work.
Print Assumptions repeated_round_keeps_quota.
Print Assumptions full_quota_keeps_round.
Print Assumptions fresh_round_consumes_one.
End RelayQuota.

Module CancelExpiry.
Inductive row := Vacant | Ready (until : nat) | Held | Stopped (until : nat) | Closed.
Inductive event := Submit (until : nat) | Fund | Cancel (now until : nat)
  | Sweep (now : nat) | Restart.

Definition step state action :=
  match action with
  | Submit until => match state with Vacant => Ready until | _ => state end
  | Fund => match state with Ready _ => Held | _ => state end
  | Cancel now until =>
    if Nat.leb now until then
      match state with
      | Ready deadline => if Nat.eqb deadline until then Stopped until else state
      | Held => Closed
      | _ => state
      end
    else state
  | Sweep now =>
    match state with
    | Ready until | Stopped until => if Nat.ltb until now then Vacant else state
    | _ => state
    end
  | Restart => state
  end.

Fixpoint run state events :=
  match events with [] => state | action :: rest => run (step state action) rest end.

Theorem paid_step : forall state action,
  state = Held \/ state = Closed -> step state action = Held \/ step state action = Closed.
Proof.
  intros state action [-> | ->]; destruct action; simpl; auto;
    destruct (Nat.leb now until); auto.
Qed.

Theorem paid_history : forall events state,
  state = Held \/ state = Closed -> run state events = Held \/ run state events = Closed.
Proof.
  induction events as [|action rest IH]; intros state paid; simpl; auto.
  apply IH. apply paid_step. exact paid.
Qed.

Theorem closed_history : forall events, run Closed events = Closed.
Proof.
  induction events as [|action rest IH]; simpl; auto.
  assert (step Closed action = Closed) as fixed.
  { destruct action; simpl; auto. destruct (Nat.leb now until); auto. }
  rewrite fixed. exact IH.
Qed.

Definition before until action := match action with Sweep now => now <= until | _ => True end.

Theorem live_history : forall events until,
  Forall (before until) events -> run (Stopped until) events = Stopped until.
Proof.
  induction events as [|action rest IH]; intros deadline valid; simpl; auto.
  inversion valid as [|? ? current remaining]; subst.
  assert (step (Stopped deadline) action = Stopped deadline) as fixed.
  { destruct action; simpl in *; auto.
    - destruct (Nat.leb now until); auto.
    - assert (Nat.ltb deadline now = false) as not_due by (apply Nat.ltb_ge; lia).
      rewrite not_due. reflexivity. }
  rewrite fixed. apply IH. exact remaining.
Qed.

Theorem expired_cancel : forall state now until,
  until < now -> step state (Cancel now until) = state.
Proof.
  intros. simpl. assert (Nat.leb now until = false) as expired by (apply Nat.leb_gt; lia).
  rewrite expired. reflexivity.
Qed.

Theorem reuse_without_old_cancel : forall until now next,
  until < now ->
  run (Stopped until) [Sweep now; Submit next; Cancel now until] = Ready next.
Proof.
  intros deadline now next expired. simpl.
  assert (Nat.ltb deadline now = true) as due by (apply Nat.ltb_lt; exact expired).
  assert (Nat.leb now deadline = false) as old by (apply Nat.leb_gt; exact expired).
  rewrite due, old. reflexivity.
Qed.

Print Assumptions paid_history.
Print Assumptions closed_history.
Print Assumptions live_history.
Print Assumptions reuse_without_old_cancel.
End CancelExpiry.

Module CancelRelay.

Definition forward (checked : bool) limit deadline now round seen :=
  if checked then
    if Z.leb 0 now && Z.leb now deadline then
      if (seen <? round) && (round <=? limit) then (round, true)
      else (seen, false)
    else (seen, false)
  else (seen, false).

Definition sent (yes : bool) := if yes then 1 else 0.

Theorem step_credit : forall checked limit deadline now round seen next yes,
  forward checked limit deadline now round seen = (next, yes) ->
  sent yes + (limit - next) <= limit - seen.
Proof.
  intros checked limit deadline now round seen next yes issued.
  unfold forward in issued. destruct checked; [|inversion issued; subst; simpl; lia].
  destruct (Z.leb 0 now && Z.leb now deadline); [|inversion issued; subst; simpl; lia].
  destruct ((seen <? round) && (round <=? limit)) eqn:room.
  - apply andb_true_iff in room. destruct room as [more cap].
    apply Nat.ltb_lt in more. apply Nat.leb_le in cap.
    inversion issued; subst. unfold sent. lia.
  - inversion issued; subst. unfold sent. lia.
Qed.

Theorem checked_only : forall checked limit deadline now round seen next,
  forward checked limit deadline now round seen = (next, true) ->
  checked = true /\ (0 <= now <= deadline)%Z /\ seen < round /\ round <= limit /\ next = round.
Proof.
  intros checked limit deadline now round seen next issued.
  unfold forward in issued. destruct checked; [|discriminate].
  destruct (Z.leb 0 now && Z.leb now deadline) eqn:live; [|discriminate].
  destruct ((seen <? round) && (round <=? limit)) eqn:room; [|discriminate].
  apply andb_true_iff in live. destruct live as [start finish].
  apply Z.leb_le in start. apply Z.leb_le in finish.
  apply andb_true_iff in room. destruct room as [more cap].
  apply Nat.ltb_lt in more. apply Nat.leb_le in cap.
  inversion issued; subst. repeat split; auto.
Qed.

Inductive event := Receive (checked : bool) (now : Z) (round : nat) | Restart.

Definition step limit deadline seen event := match event with
  | Receive checked now round => forward checked limit deadline now round seen
  | Restart => (seen, false)
  end.

Fixpoint run limit deadline seen events := match events with
  | [] => (seen, 0)
  | event :: rest =>
    let '(next, yes) := step limit deadline seen event in
    let '(final, total) := run limit deadline next rest in
    (final, sent yes + total)
  end.

Theorem trace_credit : forall events limit deadline seen,
  snd (run limit deadline seen events) + (limit - fst (run limit deadline seen events))
    <= limit - seen.
Proof.
  induction events as [|event rest ih]; intros limit deadline seen; simpl; [lia|].
  destruct (step limit deadline seen event) as [next yes] eqn:issued.
  specialize (ih limit deadline next). destruct (run limit deadline next rest) as [final total].
  simpl in *.
  assert (sent yes + (limit - next) <= limit - seen) as credit.
  { destruct event; simpl in issued.
    - eapply step_credit; eauto.
    - inversion issued; subst. unfold sent. lia. }
  lia.
Qed.

Corollary finite_relays : forall events limit deadline,
  snd (run limit deadline 0 events) <= limit.
Proof.
  intros events limit deadline. pose proof (trace_credit events limit deadline 0). lia.
Qed.

Example restart_keeps_round :
  run 64 10 0 [Receive true 1 2; Restart; Receive true 1 2; Receive true 2 3]
    = (3, 2).
Proof. reflexivity. Qed.

Example expired_no_relay : forward true 64 10 11 4 3 = (3, false).
Proof. reflexivity. Qed.

Example unchecked_no_relay : forward false 64 10 1 4 3 = (3, false).
Proof. reflexivity. Qed.

Print Assumptions checked_only.
Print Assumptions trace_credit.
Print Assumptions finite_relays.
End CancelRelay.

Module CancelReceipt.

Definition receive limit peer (checked stored : bool) (peers : list nat) :=
  if checked && stored then
    if in_dec Nat.eq_dec peer peers then peers
    else if length peers <? limit then peer :: peers else peers
  else peers.

Theorem receipt_source : forall limit peer checked stored peers node,
  In node (receive limit peer checked stored peers) ->
  In node peers \/ (checked = true /\ stored = true /\ node = peer).
Proof.
  intros limit peer checked stored peers node seen.
  unfold receive in seen. destruct checked, stored; simpl in *; auto.
  destruct (in_dec Nat.eq_dec peer peers); auto.
  destruct (length peers <? limit); auto.
  simpl in seen. destruct seen as [same|seen]; auto.
Qed.

Theorem receipt_capacity : forall limit peer checked stored peers,
  NoDup peers -> length peers <= limit ->
  NoDup (receive limit peer checked stored peers) /\
  length (receive limit peer checked stored peers) <= limit.
Proof.
  intros limit peer checked stored peers unique capacity.
  unfold receive. destruct (checked && stored); auto.
  destruct (in_dec Nat.eq_dec peer peers); auto.
  destruct (length peers <? limit) eqn:room; auto.
  apply Nat.ltb_lt in room. split; simpl; [constructor; auto|lia].
Qed.

Inductive event := Confirm (peer : nat) (checked stored : bool) | Restart.

Definition step limit peers event := match event with
  | Confirm peer checked stored => receive limit peer checked stored peers
  | Restart => peers
  end.

Fixpoint run limit peers events := match events with
  | [] => peers
  | event :: rest => run limit (step limit peers event) rest
  end.

Theorem trace_source : forall events limit peers node,
  In node (run limit peers events) ->
  In node peers \/ In (Confirm node true true) events.
Proof.
  induction events as [|event rest ih]; intros limit peers node seen; simpl in *; auto.
  specialize (ih limit (step limit peers event) node seen).
  destruct ih as [now|later]; [|right; auto].
  destruct event as [peer checked stored|]; simpl in now; auto.
  apply receipt_source in now. destruct now as [before|[check [store same]]]; auto.
  subst. right. auto.
Qed.

Theorem trace_capacity : forall events limit peers,
  NoDup peers -> length peers <= limit ->
  NoDup (run limit peers events) /\ length (run limit peers events) <= limit.
Proof.
  induction events as [|event rest ih]; intros limit peers unique capacity; simpl; auto.
  apply ih. all: destruct event; simpl; try assumption;
    apply receipt_capacity; assumption.
Qed.

Corollary authenticated_trace : forall events limit node,
  In node (run limit [] events) -> In (Confirm node true true) events.
Proof.
  intros events limit node seen. apply trace_source in seen.
  destruct seen as [none|found]; [contradiction|exact found].
Qed.

Example failed_commit : receive 256 1 true false [] = [].
Proof. reflexivity. Qed.

Example restart_keeps : run 256 [] [Confirm 1 true true; Restart; Confirm 1 true true] = [1].
Proof. reflexivity. Qed.

Example cap_keeps : receive 1 2 true true [1] = [1].
Proof. reflexivity. Qed.

Print Assumptions receipt_source.
Print Assumptions trace_capacity.
Print Assumptions authenticated_trace.
End CancelReceipt.

Module CancelSelection.

Definition missing (members receipts : list nat) :=
  filter (fun node => negb (existsb (Nat.eqb node) receipts)) members.

Definition complete (members receipts : list nat) :=
  match members with
  | [] => false
  | _ => Nat.eqb (length (missing members receipts)) 0
  end.

Lemma contains : forall node receipts,
  existsb (Nat.eqb node) receipts = true <-> In node receipts.
Proof.
  intros node receipts. rewrite existsb_exists. split.
  - intros [peer [seen same]]. apply Nat.eqb_eq in same. subst. exact seen.
  - intro seen. exists node. split; [exact seen|apply Nat.eqb_refl].
Qed.

Lemma missing_spec : forall node members receipts,
  In node (missing members receipts) <-> In node members /\ ~ In node receipts.
Proof.
  intros node members receipts. unfold missing. rewrite filter_In, negb_true_iff.
  split.
  - intros [chosen absent]. split; [exact chosen|]. intro seen.
    apply contains in seen. congruence.
  - intros [chosen absent]. split; [exact chosen|].
    destruct (existsb (Nat.eqb node) receipts) eqn:seen; [|reflexivity].
    exfalso. apply absent. apply contains. exact seen.
Qed.

Theorem complete_sound : forall members receipts,
  complete members receipts = true ->
  members <> [] /\ forall node, In node members -> In node receipts.
Proof.
  intros members receipts done. destruct members as [|first rest]; [discriminate|].
  split; [discriminate|]. intros node chosen.
  unfold complete in done. apply Nat.eqb_eq in done.
  apply length_zero_iff_nil in done.
  destruct (existsb (Nat.eqb node) receipts) eqn:seen.
  - apply contains. exact seen.
  - assert (In node (missing (first :: rest) receipts)) as absent.
    { apply missing_spec. split; [exact chosen|]. intro found.
      apply contains in found. congruence. }
    rewrite done in absent. contradiction.
Qed.

Theorem missing_shrinks : forall members before after,
  incl before after -> incl (missing members after) (missing members before).
Proof.
  intros members before after grows node absent.
  apply missing_spec in absent. destruct absent as [chosen unseen].
  apply missing_spec. split; [exact chosen|]. intro seen. apply unseen. apply grows. exact seen.
Qed.

Theorem complete_trace : forall events limit members,
  complete members (CancelReceipt.run limit [] events) = true ->
  members <> [] /\ forall node, In node members ->
    In (CancelReceipt.Confirm node true true) events.
Proof.
  intros events limit members done. apply complete_sound in done.
  destruct done as [present received]. split; [exact present|].
  intros node chosen. apply CancelReceipt.authenticated_trace with (limit := limit).
  apply received. exact chosen.
Qed.

Example empty_not_complete : complete [] [] = false.
Proof. reflexivity. Qed.

Example others_not_complete : complete [1; 2] [1; 3; 4; 5] = false.
Proof. reflexivity. Qed.

Example quorum_not_complete : complete [1; 2; 3; 4; 5] [1; 2; 3; 4] = false.
Proof. reflexivity. Qed.

Example all_complete : complete [1; 2] [3; 2; 1] = true.
Proof. reflexivity. Qed.

Print Assumptions complete_sound.
Print Assumptions missing_shrinks.
Print Assumptions complete_trace.
End CancelSelection.

Module CancelScope.

Record scope := { sequence : nat; request : nat; keys : nat }.

Definition scope_eq : forall a b : scope, {a = b} + {a <> b}.
Proof. decide equality; apply Nat.eq_dec. Defined.

Definition save before value (checked committed : bool) :=
  if checked && committed then
    match before with
    | None => (Some value, true)
    | Some previous =>
      if sequence previous <? sequence value then (Some value, true)
      else if scope_eq previous value then (before, true) else (before, false)
    end
  else (before, false).

Lemma save_source : forall before value checked committed after,
  fst (save before value checked committed) = Some after ->
  before = Some after \/ (after = value /\ checked = true /\ committed = true).
Proof.
  intros before value checked committed after result.
  unfold save in result. destruct checked, committed; simpl in *; auto.
  destruct before as [previous|]; simpl in *.
  - destruct (sequence previous <? sequence value); simpl in result.
    + inversion result; auto.
    + destruct (scope_eq previous value); simpl in result; auto.
  - inversion result; auto.
Qed.

Lemma save_atomic : forall before value checked committed,
  snd (save before value checked committed) = false ->
  fst (save before value checked committed) = before.
Proof.
  intros before value checked committed refused.
  unfold save in *. destruct checked, committed; simpl in *; auto.
  destruct before as [previous|]; simpl in *; [|discriminate].
  destruct (sequence previous <? sequence value); simpl in *; [discriminate|].
  destruct (scope_eq previous value); simpl in *; congruence.
Qed.

Lemma save_order : forall previous value checked committed,
  exists after, fst (save (Some previous) value checked committed) = Some after /\
    sequence previous <= sequence after /\
    (sequence previous = sequence after -> previous = after).
Proof.
  intros previous value checked committed. unfold save.
  destruct checked, committed; simpl; try solve [exists previous; auto].
  destruct (sequence previous <? sequence value) eqn:newer; simpl.
  - apply Nat.ltb_lt in newer. exists value. repeat split; auto; lia.
  - destruct (scope_eq previous value); exists previous; auto.
Qed.

Inductive event := Store (value : scope) (checked committed : bool) | Restart | Cancel.

Definition step before event := match event with
  | Store value checked committed => fst (save before value checked committed)
  | Restart | Cancel => before
  end.

Fixpoint run before events := match events with
  | [] => before
  | event :: rest => run (step before event) rest
  end.

Theorem trace_source : forall events before after,
  run before events = Some after ->
  before = Some after \/ In (Store after true true) events.
Proof.
  induction events as [|event rest ih]; intros before after result; simpl in *; auto.
  specialize (ih (step before event) after result).
  destruct ih as [now|later]; [|right; auto].
  destruct event as [value checked committed| |]; simpl in now; auto.
  apply save_source in now. destruct now as [old|[same [check commit]]]; auto.
  subst. right. auto.
Qed.

Theorem trace_order : forall events previous,
  exists after, run (Some previous) events = Some after /\
    sequence previous <= sequence after /\
    (sequence previous = sequence after -> previous = after).
Proof.
  induction events as [|event rest ih]; intro previous; simpl.
  - exists previous. auto.
  - assert (exists next, step (Some previous) event = Some next /\
      sequence previous <= sequence next /\
      (sequence previous = sequence next -> previous = next)) as one.
    { destruct event; simpl; [apply save_order|exists previous; auto|exists previous; auto]. }
    destruct one as [next [saved [ordered equal]]]. rewrite saved.
    destruct (ih next) as [after [done [ordered_after equal_after]]].
    exists after. split; [exact done|]. split; [lia|]. intro same.
    assert (sequence previous = sequence next) by lia.
    assert (sequence next = sequence after) by lia.
    rewrite (equal H), (equal_after H0). reflexivity.
Qed.

Corollary committed_source : forall events after,
  run None events = Some after -> In (Store after true true) events.
Proof.
  intros events after result. apply trace_source in result.
  destruct result as [impossible|present]; [discriminate|exact present].
Qed.

Example failed_write_no_scope : save None {|sequence := 1; request := 2; keys := 3|} true false
  = (None, false).
Proof. reflexivity. Qed.

Example same_sequence_refused :
  save (Some {|sequence := 1; request := 2; keys := 3|})
    {|sequence := 1; request := 2; keys := 4|} true true
  = (Some {|sequence := 1; request := 2; keys := 3|}, false).
Proof. reflexivity. Qed.

Example cancel_keeps_keys :
  run None [Store {|sequence := 1; request := 2; keys := 3|} true true; Cancel; Restart]
  = Some {|sequence := 1; request := 2; keys := 3|}.
Proof. reflexivity. Qed.

Print Assumptions save_atomic.
Print Assumptions trace_source.
Print Assumptions trace_order.
Print Assumptions committed_source.
End CancelScope.

Module ScopeArchive.
Import CancelScope.

Definition keep (rows : list scope) (value : scope) :=
  match find (fun previous => Nat.eqb (sequence previous) (sequence value)) rows with
  | None => Some (value :: rows)
  | Some previous => if scope_eq previous value then Some rows else None
  end.

Record state := { latest : option scope; rows : list scope }.

Definition write before value checked committed :=
  let '(next, accepted) := save (latest before) value checked committed in
  if accepted then
    match keep (rows before) value with
    | Some kept => {|latest := next; rows := kept|}
    | None => before
    end
  else before.

Lemma keep_retains : forall before value after,
  keep before value = Some after -> incl before after.
Proof.
  intros before value after saved. unfold keep in saved.
  destruct (find (fun previous => Nat.eqb (sequence previous) (sequence value)) before).
  - destruct (scope_eq s value); inversion saved; subst. apply incl_refl.
  - inversion saved; subst. intros old present. simpl. auto.
Qed.

Lemma keep_source : forall before value after found,
  keep before value = Some after -> In found after -> In found before \/ found = value.
Proof.
  intros before value after found saved present. unfold keep in saved.
  destruct (find (fun previous => Nat.eqb (sequence previous) (sequence value)) before).
  - destruct (scope_eq s value); inversion saved; subst; auto.
  - inversion saved; subst. destruct present; auto.
Qed.

Definition unique rows := forall first second,
  In first rows -> In second rows -> sequence first = sequence second -> first = second.

Lemma keep_unique : forall before value after,
  unique before -> keep before value = Some after -> unique after.
Proof.
  intros before value after same saved. unfold keep in saved.
  destruct (find (fun previous => Nat.eqb (sequence previous) (sequence value)) before) eqn:found.
  - destruct (scope_eq s value); inversion saved; subst; auto.
  - inversion saved; subst. intros first second one two equal.
    destruct one as [one|one], two as [two|two]; subst; auto.
    + apply find_none with (x := second) in found; [|exact two].
      apply Nat.eqb_neq in found. congruence.
    + apply find_none with (x := first) in found; [|exact one].
      apply Nat.eqb_neq in found. congruence.
Qed.

Lemma write_source : forall before value checked committed found,
  In found (rows (write before value checked committed)) ->
  In found (rows before) \/ (found = value /\ checked = true /\ committed = true).
Proof.
  intros before value checked committed found present.
  unfold write in present.
  destruct (save (latest before) value checked committed) as [next accepted] eqn:saved.
  destruct accepted; [|auto].
  assert (checked = true /\ committed = true) as flags.
  { unfold save in saved. destruct checked, committed; simpl in saved; try discriminate; auto. }
  destruct (keep (rows before) value) eqn:kept; [|auto]. simpl in present.
  apply keep_source with (found := found) in kept; [|exact present]. tauto.
Qed.

Lemma write_retains : forall before value checked committed,
  incl (rows before) (rows (write before value checked committed)).
Proof.
  intros before value checked committed. unfold write.
  destruct (save (latest before) value checked committed) as [next accepted].
  destruct accepted; [|apply incl_refl].
  destruct (keep (rows before) value) eqn:kept; [|apply incl_refl].
  simpl. eapply keep_retains. exact kept.
Qed.

Lemma write_unique : forall before value checked committed,
  unique (rows before) -> unique (rows (write before value checked committed)).
Proof.
  intros before value checked committed same. unfold write.
  destruct (save (latest before) value checked committed) as [next accepted].
  destruct accepted; [|exact same]. destruct (keep (rows before) value) eqn:kept; [|exact same].
  simpl. eapply keep_unique; eauto.
Qed.

Definition step before event := match event with
  | Store value checked committed => write before value checked committed
  | Restart | Cancel => before
  end.

Fixpoint run before events := match events with
  | [] => before
  | event :: rest => run (step before event) rest
  end.

Theorem retained : forall events before,
  incl (rows before) (rows (run before events)).
Proof.
  induction events as [|event rest ih]; intros before; simpl; [apply incl_refl|].
  intros old present. apply ih.
  destruct event; simpl; try exact present. eapply write_retains. exact present.
Qed.

Theorem immutable : forall events before,
  unique (rows before) -> unique (rows (run before events)).
Proof.
  induction events as [|event rest ih]; intros before same; simpl; [exact same|].
  apply ih. destruct event; simpl; try exact same. apply write_unique. exact same.
Qed.

Theorem provenance : forall events before found,
  In found (rows (run before events)) ->
  In found (rows before) \/ In (Store found true true) events.
Proof.
  induction events as [|event rest ih]; intros before found present; simpl in *; [auto|].
  apply ih in present. destruct present as [now|later]; [|right; auto].
  destruct event as [value checked committed| |]; simpl in now; auto.
  apply write_source in now. destruct now as [old|[same [check commit]]]; auto.
  subst. right. auto.
Qed.

Theorem search_complete : forall (rows : list scope) (accept : scope -> bool),
  (exists value, In value rows /\ accept value = true) ->
  exists value, find accept rows = Some value /\ In value rows /\ accept value = true.
Proof.
  intros values. induction values as [|first rest ih];
    intros accept [value [present accepted]]; simpl in *.
  - contradiction.
  - destruct (accept first) eqn:yes.
    + exists first. auto.
    + assert (In value rest) as there by (destruct present; subst; [congruence|assumption]).
      destruct (ih accept) as [found [selected [member checked]]]; [eauto|].
      exists found. auto.
Qed.

Theorem search_sound : forall (rows : list scope) accept value,
  find accept rows = Some value -> In value rows /\ accept value = true.
Proof. intros values accept value selected. eapply find_some. exact selected. Qed.

Corollary old_keys_found : forall events before value accept,
  In value (rows before) -> accept value = true ->
  exists found, find accept (rows (run before events)) = Some found /\
    In found (rows (run before events)) /\ accept found = true.
Proof.
  intros events before value accept present accepted. apply search_complete.
  exists value. split; [eapply retained; exact present|exact accepted].
Qed.

Print Assumptions retained.
Print Assumptions immutable.
Print Assumptions provenance.
Print Assumptions search_complete.
Print Assumptions search_sound.
Print Assumptions old_keys_found.
End ScopeArchive.

Module JournalSpace.

Fixpoint total (values : list nat) := match values with
  | [] => 0
  | value :: rest => value + total rest
  end.

Record state := { work : nat; controls : list nat }.

Definition valid limit slots unit state :=
  work state <= limit /\ length (controls state) <= slots /\
  Forall (fun bytes => bytes <= unit) (controls state).

Definition store_work limit before next committed :=
  if (next + total (controls before) <=? limit) && committed then
    ({|work := next; controls := controls before|}, true)
  else (before, false).

Definition store_control slots unit before bytes committed :=
  if (length (controls before) <? slots) && (bytes <=? unit) && committed then
    ({|work := work before; controls := bytes :: controls before|}, true)
  else (before, false).

Lemma total_limit : forall values unit,
  Forall (fun bytes => bytes <= unit) values -> total values <= length values * unit.
Proof.
  intros values unit checked. induction checked; simpl; lia.
Qed.

Lemma work_safe : forall limit slots unit before next committed,
  valid limit slots unit before ->
  valid limit slots unit (fst (store_work limit before next committed)).
Proof.
  intros limit slots unit before next committed [work_ok [slots_ok bytes_ok]].
  unfold valid, store_work. destruct (next + total (controls before) <=? limit) eqn:space;
    destruct committed; simpl; try (repeat split; assumption).
  apply Nat.leb_le in space. repeat split; auto; lia.
Qed.

Lemma control_safe : forall limit slots unit before bytes committed,
  valid limit slots unit before ->
  valid limit slots unit (fst (store_control slots unit before bytes committed)).
Proof.
  intros limit slots unit before bytes committed [work_ok [slots_ok bytes_ok]].
  unfold valid, store_control. destruct (length (controls before) <? slots) eqn:room;
    destruct (bytes <=? unit) eqn:size; destruct committed; simpl;
    try (repeat split; assumption).
  apply Nat.ltb_lt in room. apply Nat.leb_le in size.
  repeat split; auto; try lia.
Qed.

Theorem control_after_full_work : forall limit slots unit values bytes,
  length values < slots -> bytes <= unit ->
  store_control slots unit {|work := limit; controls := values|} bytes true =
    ({|work := limit; controls := bytes :: values|}, true).
Proof.
  intros limit slots unit values bytes room size. unfold store_control. simpl.
  apply Nat.ltb_lt in room. apply Nat.leb_le in size. rewrite room, size. reflexivity.
Qed.

Theorem refused_work_atomic : forall limit before next committed,
  snd (store_work limit before next committed) = false ->
  fst (store_work limit before next committed) = before.
Proof.
  intros limit before next committed refused. unfold store_work in *.
  destruct (next + total (controls before) <=? limit), committed; simpl in *; congruence.
Qed.

Theorem work_preserves_controls : forall limit before next committed,
  controls (fst (store_work limit before next committed)) = controls before.
Proof.
  intros. unfold store_work.
  destruct (next + total (controls before) <=? limit), committed; reflexivity.
Qed.

Inductive event := Work (next : nat) (committed : bool)
  | Control (bytes : nat) (committed : bool) | Restart.

Definition step limit slots unit before event := match event with
  | Work next committed => fst (store_work limit before next committed)
  | Control bytes committed => fst (store_control slots unit before bytes committed)
  | Restart => before
  end.

Fixpoint run limit slots unit before events := match events with
  | [] => before
  | event :: rest => run limit slots unit (step limit slots unit before event) rest
  end.

Theorem trace_safe : forall events limit slots unit before,
  valid limit slots unit before -> valid limit slots unit (run limit slots unit before events).
Proof.
  induction events as [|event rest ih]; intros limit slots unit before checked; simpl; auto.
  apply ih. destruct event; simpl; auto; [apply work_safe|apply control_safe]; exact checked.
Qed.

Theorem trace_capacity : forall events limit slots unit before,
  valid limit slots unit before ->
  let after := run limit slots unit before events in
  work after + total (controls after) <= limit + slots * unit.
Proof.
  intros events limit slots unit before checked after.
  pose proof (trace_safe events limit slots unit before checked) as safe.
  change (valid limit slots unit after) in safe.
  destruct safe as [work_ok [slots_ok bytes_ok]].
  pose proof (total_limit _ _ bytes_ok). nia.
Qed.

Print Assumptions control_after_full_work.
Print Assumptions refused_work_atomic.
Print Assumptions work_preserves_controls.
Print Assumptions trace_safe.
Print Assumptions trace_capacity.
End JournalSpace.

Module ReplySpace.

Inductive slot := Free | Held (id capacity : nat) | Saved (id value bytes : nat).

Definition size slot := match slot with
  | Free => 0 | Held _ capacity => capacity | Saved _ _ bytes => bytes
  end.

Record state := { other : nat; entry : slot }.

Definition used state := other state + size (entry state).

Definition reserve limit before id bytes committed :=
  if (other before + bytes <=? limit) && committed then
    {|other := other before; entry := Held id bytes|}
  else before.

Definition work limit before bytes committed :=
  if (bytes + size (entry before) <=? limit) && committed then
    {|other := bytes; entry := entry before|}
  else before.

Definition finish before id value bytes checked committed :=
  match entry before with
  | Held key capacity =>
    if (id =? key) && (bytes <=? capacity) && checked && committed then
      ({|other := other before; entry := Saved id value bytes|}, true)
    else (before, false)
  | Saved key output count =>
    (before, (id =? key) && (value =? output) && (bytes =? count) && checked)
  | Free => (before, false)
  end.

Lemma reserve_safe : forall limit before id bytes committed,
  used before <= limit -> used (reserve limit before id bytes committed) <= limit.
Proof.
  intros limit before id bytes committed safe. unfold reserve.
  destruct (other before + bytes <=? limit) eqn:room; destruct committed; try exact safe.
  apply Nat.leb_le in room. exact room.
Qed.

Lemma work_safe : forall limit before bytes committed,
  used before <= limit -> used (work limit before bytes committed) <= limit.
Proof.
  intros limit before bytes committed safe. unfold work.
  destruct (bytes + size (entry before) <=? limit) eqn:room;
    destruct committed; try exact safe.
  apply Nat.leb_le in room. exact room.
Qed.

Theorem finish_shrinks : forall before id value bytes checked committed,
  used (fst (finish before id value bytes checked committed)) <= used before.
Proof.
  intros before id value bytes checked committed. unfold finish.
  destruct (entry before) eqn:slot; simpl; try lia.
  destruct ((id =? id0) && (bytes <=? capacity) && checked && committed) eqn:ready;
    simpl; try lia.
  repeat rewrite andb_true_iff in ready.
  rewrite Nat.leb_le in ready. unfold used. simpl. rewrite slot. simpl. intuition lia.
Qed.

Theorem full_completion : forall limit key value capacity other bytes,
  other + capacity = limit -> bytes <= capacity ->
  finish {|other := other; entry := Held key capacity|} key value bytes true true =
    ({|other := other; entry := Saved key value bytes|}, true).
Proof.
  intros limit key value capacity other bytes full room. unfold finish. simpl.
  rewrite Nat.eqb_refl. apply Nat.leb_le in room. rewrite room. reflexivity.
Qed.

Theorem reply_saved : forall before id value bytes checked committed,
  snd (finish before id value bytes checked committed) = true ->
  entry (fst (finish before id value bytes checked committed)) = Saved id value bytes.
Proof.
  intros before id value bytes checked committed accepted. unfold finish in *.
  destruct (entry before) eqn:slot; simpl in *; try discriminate.
  - destruct ((id =? id0) && (bytes <=? capacity) && checked && committed);
      simpl in *; try discriminate. reflexivity.
  - repeat rewrite andb_true_iff in accepted.
    repeat rewrite Nat.eqb_eq in accepted. intuition subst. exact slot.
Qed.

Theorem refused_atomic : forall before id value bytes checked committed,
  snd (finish before id value bytes checked committed) = false ->
  fst (finish before id value bytes checked committed) = before.
Proof.
  intros before id value bytes checked committed refused. unfold finish in *.
  destruct (entry before); simpl in *; try reflexivity.
  destruct ((id =? id0) && (bytes <=? capacity) && checked && committed);
    simpl in *; congruence.
Qed.

Theorem work_keeps_reply : forall limit before bytes committed,
  entry (work limit before bytes committed) = entry before.
Proof.
  intros. unfold work.
  destruct (bytes + size (entry before) <=? limit), committed; reflexivity.
Qed.

Inductive event := Allocate (id bytes : nat) (committed : bool)
  | Work (bytes : nat) (committed : bool)
  | Finish (id value bytes : nat) (checked committed : bool) | Restart.

Definition step limit before event := match event with
  | Allocate id bytes committed => reserve limit before id bytes committed
  | Work bytes committed => work limit before bytes committed
  | Finish id value bytes checked committed => fst (finish before id value bytes checked committed)
  | Restart => before
  end.

Fixpoint run limit before events := match events with
  | [] => before
  | event :: rest => run limit (step limit before event) rest
  end.

Theorem trace_safe : forall events limit before,
  used before <= limit -> used (run limit before events) <= limit.
Proof.
  induction events as [|event rest ih]; intros limit before safe; simpl; auto.
  apply ih. destruct event; simpl; auto.
  - apply reserve_safe. exact safe.
  - apply work_safe. exact safe.
  - pose proof (finish_shrinks before id value bytes checked committed). lia.
Qed.

Print Assumptions finish_shrinks.
Print Assumptions full_completion.
Print Assumptions reply_saved.
Print Assumptions refused_atomic.
Print Assumptions work_keeps_reply.
Print Assumptions trace_safe.
End ReplySpace.

Module JobSpace.

Record vote := { node : nat; value : nat; bytes : nat }.
Record state := {
  key : nat; capacity : nat; base : nat; votes : list vote;
  output : option (nat * nat)
}.

Definition used state := base state + fold_right (fun vote size => bytes vote + size) 0 (votes state).
Definition footprint state := match output state with
  | None => capacity state | Some (_, size) => size end.
Definition valid state := used state <= capacity state /\ footprint state <= capacity state.

Definition put before id vote checked committed :=
  match output before with
  | Some _ => before
  | None =>
    if existsb (fun saved => node saved =? node vote) (votes before) then before
    else if checked && (id =? key before) && (used before + bytes vote <=? capacity before) && committed
    then {|key := key before; capacity := capacity before; base := base before;
           votes := vote :: votes before; output := None|}
    else before
  end.

Definition finish before id value size checked committed :=
  match output before with
  | Some _ => before
  | None =>
    if checked && (id =? key before) && (size <=? capacity before) && committed
    then {|key := key before; capacity := capacity before; base := base before;
           votes := votes before; output := Some (value, size)|}
    else before
  end.

Theorem put_safe : forall before id vote checked committed,
  valid before -> valid (put before id vote checked committed).
Proof.
  intros before id vote checked committed safe. unfold put.
  destruct (output before); auto.
  destruct (existsb (fun saved => node saved =? node vote) (votes before)); auto.
  destruct (checked && (id =? key before) &&
    (used before + bytes vote <=? capacity before) && committed) eqn:ready; auto.
  repeat rewrite andb_true_iff in ready.
  rewrite Nat.leb_le in ready. unfold valid, used, footprint in *. simpl in *. intuition lia.
Qed.

Theorem finish_safe : forall before id value size checked committed,
  valid before -> valid (finish before id value size checked committed).
Proof.
  intros before id value size checked committed safe. unfold finish.
  destruct (output before); auto.
  destruct (checked && (id =? key before) && (size <=? capacity before) && committed) eqn:ready; auto.
  repeat rewrite andb_true_iff in ready.
  rewrite Nat.leb_le in ready. unfold valid, used, footprint in *. simpl in *. intuition lia.
Qed.

Theorem finish_shrinks : forall before id value size checked committed,
  footprint (finish before id value size checked committed) <= footprint before.
Proof.
  intros before id value size checked committed. unfold finish.
  destruct (output before) eqn:saved; try lia.
  destruct (checked && (id =? key before) && (size <=? capacity before) && committed) eqn:ready; try lia.
  repeat rewrite andb_true_iff in ready. rewrite Nat.leb_le in ready.
  unfold footprint. rewrite saved. simpl. intuition lia.
Qed.

Theorem full_completion : forall before value size,
  output before = None -> used before = capacity before -> size <= capacity before ->
  output (finish before (key before) value size true true) = Some (value, size).
Proof.
  intros before value size pending full fits. unfold finish. rewrite pending.
  simpl. rewrite Nat.eqb_refl. apply Nat.leb_le in fits. rewrite fits. reflexivity.
Qed.

Theorem failed_commit : forall before id vote value size checked,
  put before id vote checked false = before /\ finish before id value size checked false = before.
Proof.
  intros before id vote value size checked. unfold put, finish. destruct (output before); auto.
  repeat rewrite andb_false_r.
  destruct (existsb (fun saved => node saved =? node vote) (votes before)); auto.
Qed.

Theorem closed_keeps_result : forall before result id vote value size checked committed,
  output before = Some result ->
  put before id vote checked committed = before /\ finish before id value size checked committed = before.
Proof. intros. unfold put, finish. rewrite H. auto. Qed.

Lemma absent_node : forall vote prior,
  existsb (fun saved => node saved =? node vote) prior = false ->
  ~ In (node vote) (map node prior).
Proof.
  intros vote prior absent present. apply in_map_iff in present as [saved [same included]].
  assert (existsb (fun item => node item =? node vote) prior = true) as found.
  { apply existsb_exists. exists saved. split; auto. apply Nat.eqb_eq. exact same. }
  congruence.
Qed.

Theorem put_unique : forall before id vote checked committed,
  NoDup (map node (votes before)) ->
  NoDup (map node (votes (put before id vote checked committed))).
Proof.
  intros before id vote checked committed unique. unfold put.
  destruct (output before); auto.
  destruct (existsb (fun saved => node saved =? node vote) (votes before)) eqn:seen; auto.
  destruct (checked && (id =? key before) &&
    (used before + bytes vote <=? capacity before) && committed); simpl; auto.
  constructor; auto. apply absent_node. exact seen.
Qed.

Inductive event := Vote (id : nat) (item : vote) (checked committed : bool)
  | Finish (id value size : nat) (checked committed : bool) | Restart.

Definition step before event := match event with
  | Vote id item checked committed => put before id item checked committed
  | Finish id value size checked committed => finish before id value size checked committed
  | Restart => before
  end.

Fixpoint run before events := match events with
  | [] => before | event :: rest => run (step before event) rest end.

Theorem trace_safe : forall events before,
  valid before -> valid (run before events).
Proof.
  induction events as [|event rest ih]; intros before safe; simpl; auto.
  apply ih. destruct event; simpl; auto using put_safe, finish_safe.
Qed.

Print Assumptions put_safe.
Print Assumptions finish_safe.
Print Assumptions finish_shrinks.
Print Assumptions full_completion.
Print Assumptions failed_commit.
Print Assumptions closed_keeps_result.
Print Assumptions put_unique.
Print Assumptions trace_safe.
End JobSpace.

Module Capacity.
Open Scope Z_scope.

Record limits := Limits { memory : Z; input : Z; output : Z }.
Record offer := Offer { node : nat; size : limits }.

Definition fits supplied needed :=
  (memory needed <=? memory supplied) &&
  (input needed <=? input supplied) && (output needed <=? output supplied).

Definition supports (valid : offer -> bool) who needed offers :=
  existsb (fun item => (node item =? who)%nat && valid item && fits (size item) needed) offers.

Lemma fits_spec : forall supplied needed,
  fits supplied needed = true <->
  memory needed <= memory supplied /\ input needed <= input supplied /\
  output needed <= output supplied.
Proof.
  intros. unfold fits. repeat rewrite Bool.andb_true_iff.
  repeat rewrite Z.leb_le. tauto.
Qed.

Theorem support_witness : forall valid who needed offers,
  supports valid who needed offers = true ->
  exists item, In item offers /\ node item = who /\ valid item = true /\
    memory needed <= memory (size item) /\ input needed <= input (size item) /\
    output needed <= output (size item).
Proof.
  intros valid who needed offers accepted.
  apply existsb_exists in accepted as [item [present checked]].
  repeat rewrite Bool.andb_true_iff in checked.
  destruct checked as [[same trusted] fits]. apply Nat.eqb_eq in same.
  apply fits_spec in fits. exists item. tauto.
Qed.

Theorem support_permutation : forall valid who needed left right,
  Permutation left right ->
  supports valid who needed left = supports valid who needed right.
Proof.
  intros valid who needed left right reordered.
  assert (supports valid who needed left = true <->
          supports valid who needed right = true) as equivalent.
  { unfold supports. repeat rewrite existsb_exists. split.
    - intros [item [present accepted]]. exists item. split; [|exact accepted].
      eapply Permutation_in; eauto.
    - intros [item [present accepted]]. exists item. split; [|exact accepted].
      eapply Permutation_in; [apply Permutation_sym; exact reordered|exact present]. }
  destruct (supports valid who needed left), (supports valid who needed right);
    intuition discriminate.
Qed.

Theorem support_monotone : forall valid who less more offers,
  fits more less = true -> supports valid who more offers = true ->
  supports valid who less offers = true.
Proof.
  intros valid who less more offers smaller accepted.
  apply fits_spec in smaller.
  apply support_witness in accepted as [item [present [same [trusted enough]]]].
  apply existsb_exists. exists item. split; [exact present|].
  repeat rewrite Bool.andb_true_iff. split; [split|].
  - apply Nat.eqb_eq. exact same.
  - exact trusted.
  - apply fits_spec. destruct smaller as [? [? ?]]. destruct enough as [? [? ?]]. lia.
Qed.

Theorem memory_shortage : forall valid who needed offers,
  (forall item, In item offers -> memory (size item) < memory needed) ->
  supports valid who needed offers = false.
Proof.
  intros valid who needed offers short.
  destruct (supports valid who needed offers) eqn:accepted; [|reflexivity].
  apply support_witness in accepted as [item [present [_ [_ [enough _]]]]].
  specialize (short item present). lia.
Qed.

Print Assumptions support_witness.
Print Assumptions support_permutation.
Print Assumptions support_monotone.
Print Assumptions memory_shortage.
End Capacity.

Module ReplayMeter.
Section Machine.
Variables (State Input Output : Type).
Variable step : State -> Input -> State * Output * nat.

Fixpoint run fuel state inputs : option (State * list Output * nat) :=
  match inputs with
  | [] => Some (state, [], 0)
  | input :: rest =>
    let '(next, output, cost) := step state input in
    if (0 <? cost) && (cost <=? fuel) then
      match run (fuel - cost) next rest with
      | None => None
      | Some (last, outputs, spent) => Some (last, output :: outputs, cost + spent)
      end
    else None
  end.

Theorem fuel_and_count : forall inputs fuel state last outputs spent,
  run fuel state inputs = Some (last, outputs, spent) ->
  spent <= fuel /\ length outputs = length inputs /\ length inputs <= spent.
Proof.
  induction inputs as [|input rest ih]; intros fuel state last outputs spent accepted.
  - simpl in accepted. inversion accepted; subst. simpl. lia.
  - simpl in accepted.
    destruct (step state input) as [[next output] cost] eqn:executed.
    destruct ((0 <? cost) && (cost <=? fuel)) eqn:room; [|discriminate].
    apply andb_true_iff in room as [positive enough].
    apply Nat.ltb_lt in positive. apply Nat.leb_le in enough.
    destruct (run (fuel - cost) next rest) as [[[final values] used]|] eqn:replayed;
      [|discriminate].
    inversion accepted; subst.
    specialize (ih _ _ _ _ _ replayed). simpl. lia.
Qed.

Theorem prefix_extension : forall inputs fuel state middle outputs spent,
  run fuel state inputs = Some (middle, outputs, spent) ->
  forall suffix last more extra,
  run (fuel - spent) middle suffix = Some (last, more, extra) ->
  run fuel state (inputs ++ suffix) = Some (last, outputs ++ more, spent + extra).
Proof.
  induction inputs as [|input rest ih]; intros fuel state middle outputs spent accepted
    suffix last more extra extended.
  - simpl in accepted. inversion accepted; subst. simpl.
    rewrite Nat.sub_0_r in extended. exact extended.
  - simpl in accepted.
    destruct (step state input) as [[next output] cost] eqn:executed.
    destruct ((0 <? cost) && (cost <=? fuel)) eqn:room; [|discriminate].
    destruct (run (fuel - cost) next rest) as [[[final values] used]|] eqn:replayed;
      [|discriminate].
    inversion accepted; subst.
    simpl. rewrite executed, room.
    assert ((fuel - cost - used) = (fuel - (cost + used))) as difference by lia.
    rewrite <- difference in extended.
    specialize (ih _ _ _ _ _ replayed _ _ _ _ extended).
    rewrite ih. simpl. rewrite Nat.add_assoc. reflexivity.
Qed.
End Machine.
Print Assumptions fuel_and_count.
Print Assumptions prefix_extension.
End ReplayMeter.

Module SessionOrder.
Fixpoint numbered start values :=
  match values with
  | [] => true
  | value :: rest => Nat.eqb value start && numbered (S start) rest
  end.

Theorem exact_range : forall values start,
  numbered start values = true <-> values = seq start (length values).
Proof.
  induction values as [|value rest ih]; intros start; simpl.
  - split; auto.
  - rewrite andb_true_iff, Nat.eqb_eq, ih.
    split.
    + intros [same ordered]. f_equal; assumption.
    + intros equal. injection equal as same ordered. auto.
Qed.

Theorem unique_steps : forall values start,
  numbered start values = true -> NoDup values.
Proof.
  intros values start accepted. apply exact_range in accepted.
  rewrite accepted. apply seq_NoDup.
Qed.

Theorem complete_prefix : forall values last,
  numbered 1 values = true -> In last values ->
  forall previous, 1 <= previous < last -> In previous values.
Proof.
  intros values last accepted present previous earlier.
  apply exact_range in accepted. rewrite accepted in *.
  apply in_seq in present. apply in_seq. simpl in *. lia.
Qed.

Print Assumptions exact_range.
Print Assumptions unique_steps.
Print Assumptions complete_prefix.
End SessionOrder.

Module SendReserve.
Record quota := { used : nat; forwarded : nat }.

Definition take total limit local state :=
  if (used state <? total) && (local || (forwarded state <? limit)) then
    Some {| used := S (used state);
      forwarded := if local then forwarded state else S (forwarded state) |}
  else None.

Theorem take_limits : forall total limit local state next,
  forwarded state <= limit ->
  take total limit local state = Some next ->
  used next <= total /\ forwarded next <= limit /\ used next = S (used state).
Proof.
  intros total limit local state next prior accepted.
  unfold take in accepted.
  destruct ((used state <? total) && (local || (forwarded state <? limit))) eqn:room;
    [|discriminate].
  apply andb_true_iff in room as [space quota_ok].
  apply Nat.ltb_lt in space.
  inversion accepted; subst; simpl.
  destruct local; simpl in *; try lia.
  apply Nat.ltb_lt in quota_ok. lia.
Qed.

Theorem own_slot : forall total limit state,
  used state < total -> exists next, take total limit true state = Some next.
Proof.
  intros total limit state room. unfold take.
  apply Nat.ltb_lt in room. rewrite room. simpl. eauto.
Qed.

Fixpoint forward count total limit state :=
  match count with
  | 0 => state
  | S rest => match take total limit false state with
    | None => state
    | Some next => forward rest total limit next
    end
  end.

Theorem reserve_survives : forall count total limit state,
  used state = forwarded state -> limit < total -> forwarded state <= limit ->
  used (forward count total limit state) <= limit /\
  exists next, take total limit true (forward count total limit state) = Some next.
Proof.
  induction count as [|count ih]; intros total limit state same less fits.
  - simpl. split; [lia|apply own_slot; lia].
  - simpl. destruct (take total limit false state) as [next|] eqn:accepted.
    + pose proof (take_limits total limit false state next fits accepted) as facts.
      assert (used next = forwarded next) as equal.
      { unfold take in accepted.
        destruct ((used state <? total) && (false || (forwarded state <? limit)));
          inversion accepted; subst; simpl; lia. }
      apply ih; tauto.
    + split; [lia|apply own_slot; lia].
Qed.
Print Assumptions take_limits.
Print Assumptions own_slot.
Print Assumptions reserve_survives.

Definition priority pending := filter (fun own => own) pending ++ filter negb pending.

Lemma own_front : forall pending,
  In true pending -> exists rest, filter (fun own => own) pending = true :: rest.
Proof.
  induction pending as [|own pending ih]; intros present; [contradiction|].
  destruct own; simpl in *.
  - eexists. reflexivity.
  - apply ih. destruct present as [same|present]; [discriminate|assumption].
Qed.

Theorem own_progress : forall pending total limit state,
  In true pending -> used state < total ->
  exists rest next, priority pending = true :: rest /\
    take total limit true state = Some next.
Proof.
  intros pending total limit state present room.
  destruct (own_front pending present) as [rest front].
  destruct (own_slot total limit state room) as [next accepted].
  exists (rest ++ filter negb pending), next.
  unfold priority. rewrite front. simpl. auto.
Qed.
Print Assumptions own_progress.
End SendReserve.