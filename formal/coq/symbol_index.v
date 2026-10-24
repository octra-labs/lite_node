(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Bool Lia.
Import ListNotations.

Fixpoint read key (entries : list (nat * nat)) :=
  match entries with
  | [] => None
  | (name, value) :: rest => if key =? name then Some value else read key rest
  end.

Definition insert name value entries :=
  match read name entries with
  | Some _ => entries
  | None => (name, value) :: entries
  end.

Fixpoint collect entries index :=
  match entries with
  | [] => index
  | (name, value) :: rest => collect rest (insert name value index)
  end.

Lemma insert_read : forall index key name value,
  read key (insert name value index) =
    match read key index with
    | Some value => Some value
    | None => if key =? name then Some value else None
    end.
Proof.
  intros index key name value. unfold insert.
  destruct (read name index) eqn:existing.
  - destruct (read key index) eqn:found; [reflexivity|].
    destruct (key =? name) eqn:same; [|reflexivity].
    apply Nat.eqb_eq in same. subst. rewrite existing in found. discriminate.
  - simpl. destruct (key =? name) eqn:same.
    + apply Nat.eqb_eq in same. subst. now rewrite existing.
    + destruct (read key index); reflexivity.
Qed.

Theorem collect_preserves_first : forall entries index key,
  read key (collect entries index) =
    match read key index with Some value => Some value | None => read key entries end.
Proof.
  induction entries as [|[name value] rest IH]; intros index key; simpl.
  - now destruct (read key index).
  - rewrite IH, insert_read. destruct (read key index); [reflexivity|].
    now destruct (key =? name).
Qed.

Corollary index_equals_list : forall entries key,
  read key (collect entries []) = read key entries.
Proof. intros. now rewrite collect_preserves_first. Qed.

Theorem append_cannot_shadow : forall before after index key value,
  read key before = Some value -> read key index = None ->
  read key (collect (before ++ after) index) = Some value.
Proof.
  intros before after index key value found empty.
  rewrite collect_preserves_first, empty.
  induction before as [|[name item] rest IH]; simpl in *; [discriminate|].
  destruct (key =? name); auto.
Qed.

Print Assumptions collect_preserves_first.
Print Assumptions index_equals_list.
Print Assumptions append_cannot_shadow.

Section AliasPaths.
Variable next : nat -> option nat.

Inductive reaches : nat -> nat -> Prop :=
| at_end : forall key, next key = None -> reaches key key
| along_edge : forall key child target,
    next key = Some child -> reaches child target -> reaches key target.

Definition sound memo := forall key target, read key memo = Some target -> reaches key target.

Fixpoint walk fuel memo key : option (nat * list nat) :=
  match fuel with
  | 0 => None
  | S fuel =>
    match read key memo with
    | Some target => Some (target, [])
    | None =>
      match next key with
      | None => Some (key, [key])
      | Some child =>
        match walk fuel memo child with
        | Some (target, path) => Some (target, key :: path)
        | None => None
        end
      end
    end
  end.

Lemma walk_sound : forall fuel memo key target path,
  sound memo -> walk fuel memo key = Some (target, path) ->
  reaches key target /\ Forall (fun key => reaches key target) path.
Proof.
  induction fuel as [|fuel IH]; intros memo key target path valid found;
    simpl in found; [discriminate|].
  destruct (read key memo) eqn:cached.
  - inversion found; subst. split; [now apply valid|constructor].
  - destruct (next key) eqn:edge.
    + destruct (walk fuel memo n) as [[value steps]|] eqn:child; [|discriminate].
      inversion found; subst. specialize (IH memo n target steps valid child).
      destruct IH as [root rest].
      assert (reaches key target) by (eapply along_edge; eauto).
      split; [assumption|now constructor].
    + inversion found; subst. assert (reaches target target) by (now apply at_end).
      split; [assumption|constructor; [assumption|constructor]].
Qed.

Definition remember (path : list nat) (target : nat) (memo : list (nat * nat)) :=
  map (fun key => (key, target)) path ++ memo.

Lemma remember_sound : forall path target memo,
  sound memo -> Forall (fun key => reaches key target) path ->
  sound (remember path target memo).
Proof.
  induction path as [|name rest IH]; intros target memo valid path_valid.
  - exact valid.
  - inversion path_valid; subst. unfold sound in *.
    intros key value found. simpl in found. destruct (key =? name) eqn:same.
    + apply Nat.eqb_eq in same. subst. inversion found; subst. assumption.
    + eapply IH; eauto.
Qed.

Theorem compression_preserves_lookup : forall fuel memo key target path,
  sound memo -> walk fuel memo key = Some (target, path) ->
  sound (remember path target memo).
Proof.
  intros. apply remember_sound; [assumption|].
  eapply (proj2 (walk_sound fuel memo key target path H H0)).
Qed.

Lemma remembered_read : forall path target memo key,
  In key path -> read key (remember path target memo) = Some target.
Proof.
  induction path as [|name rest IH]; intros target memo key member; [contradiction|].
  simpl in *. destruct (key =? name) eqn:same; [reflexivity|].
  destruct member as [equal|member].
  - subst. now rewrite Nat.eqb_refl in same.
  - now apply IH.
Qed.

Theorem compressed_path_cost : forall path target memo key,
  In key path -> walk 1 (remember path target memo) key = Some (target, []).
Proof. intros. simpl. now rewrite remembered_read. Qed.

Theorem finite_cycle_refused : forall fuel memo key,
  sound memo -> (forall target, ~ reaches key target) -> walk fuel memo key = None.
Proof.
  intros. destruct (walk fuel memo key) as [[target path]|] eqn:found; [|reflexivity].
  exfalso. apply (H0 target). exact (proj1 (walk_sound fuel memo key target path H found)).
Qed.
End AliasPaths.

Print Assumptions compression_preserves_lookup.
Print Assumptions compressed_path_cost.
Print Assumptions finite_cycle_refused.