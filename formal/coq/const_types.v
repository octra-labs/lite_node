(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Bool Lia.
Import ListNotations.

Definition key := (nat * nat)%type.
Definition key_eq (left right : key) : {left = right} + {left <> right}.
Proof. decide equality; apply Nat.eq_dec. Defined.

Fixpoint lookup (k : key) (cache : list (key * nat)) :=
  match cache with
  | [] => None
  | (item, typ) :: rest => if key_eq item k then Some typ else lookup k rest
  end.

Definition valid (meaning : key -> nat) (cache : list (key * nat)) :=
  forall k t, In (k, t) cache -> meaning k = t.

Theorem cache_read_preserves_type : forall meaning cache k t,
  valid meaning cache -> lookup k cache = Some t -> meaning k = t.
Proof.
  intros meaning cache; induction cache as [|[item typ] rest IH]; intros k t Hv Hread;
    simpl in Hread; [discriminate|].
  destruct (key_eq item k) as [Heq | Hneq].
  - inversion Hread; subst; apply Hv; now left.
  - apply IH; auto. intros a b H; apply Hv; now right.
Qed.

Theorem successful_fill_preserves : forall meaning cache k,
  valid meaning cache -> valid meaning ((k, meaning k) :: cache).
Proof.
  intros meaning cache k H a b [Heq | Hin].
  - inversion Heq; reflexivity.
  - now apply H.
Qed.

Theorem different_scope_misses : forall name scope other typ,
  scope <> other -> lookup (name, scope) [((name, other), typ)] = None.
Proof.
  intros name scope other typ H.
  change ((if key_eq (name, other) (name, scope) then Some typ else None) = None).
  destruct (key_eq (name, other) (name, scope)) as [E | E].
  - inversion E; subst; contradiction.
  - reflexivity.
Qed.

Definition reusable active dependencies :=
  negb (existsb (fun name => existsb (Nat.eqb name) active) dependencies).

Theorem active_dependency_is_not_reused : forall active dependencies name,
  In name active -> In name dependencies -> reusable active dependencies = false.
Proof.
  intros active dependencies name Ha Hd; unfold reusable.
  assert (existsb (fun x => existsb (Nat.eqb x) active) dependencies = true) as H.
  { apply existsb_exists; exists name; split; auto.
    apply existsb_exists; exists name; split; auto; apply Nat.eqb_refl. }
  now rewrite H.
Qed.

Fixpoint misses (names : list key) seen :=
  match names with
  | [] => (0, seen)
  | name :: rest =>
    if in_dec key_eq name seen then misses rest seen
    else let '(count, result) := misses rest (name :: seen) in (S count, result)
  end.

Theorem cached_work_is_distinct : forall names seen,
  fst (misses names seen) + length seen = length (snd (misses names seen)).
Proof.
  induction names as [|name rest IH]; intros seen; simpl; [lia|].
  destruct (in_dec key_eq name seen); auto.
  specialize (IH (name :: seen)).
  destruct (misses rest (name :: seen)); simpl in *; lia.
Qed.

Theorem cache_has_no_duplicate_keys : forall names seen,
  NoDup seen -> NoDup (snd (misses names seen)).
Proof.
  induction names as [|name rest IH]; intros seen H; simpl; auto.
  destruct (in_dec key_eq name seen); auto.
  specialize (IH (name :: seen) (NoDup_cons name n H)).
  destruct (misses rest (name :: seen)); exact IH.
Qed.