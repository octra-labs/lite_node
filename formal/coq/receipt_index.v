(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Lia FMapAVL FMapFacts Structures.OrderedTypeEx.
Import ListNotations.
Module Index := FMapAVL.Make(Nat_as_OT).
Module Facts := FMapFacts.WFacts_fun Nat_as_OT Index.

Section Lookup.
Variable Value : Type.

Fixpoint lookup key (rows : list (nat * Value)) :=
  match rows with
  | [] => None
  | (other, value) :: rest => if Nat.eq_dec key other then Some value else lookup key rest
  end.

Definition agree (index : Index.t Value) rows :=
  forall key, Index.find key index = lookup key rows.

Lemma empty_agrees : agree (Index.empty Value) [].
Proof.
  intro key. rewrite Facts.empty_o. reflexivity.
Qed.

Lemma add_agrees : forall index rows key value,
  agree index rows -> agree (Index.add key value index) ((key, value) :: rows).
Proof.
  intros index rows key value same sought. simpl.
  destruct (Nat.eq_dec sought key) as [equal | different].
  - subst. rewrite Facts.add_eq_o; reflexivity.
  - rewrite Facts.add_neq_o; [apply same|congruence].
Qed.

Fixpoint load (rows : list (nat * Value)) index : nat + Index.t Value :=
  match rows with
  | [] => inr index
  | (key, value) :: rest =>
    match Index.find key index with
    | Some _ => inl key
    | None => load rest (Index.add key value index)
    end
  end.

Fixpoint scan (rows seen : list (nat * Value)) : nat + list (nat * Value) :=
  match rows with
  | [] => inr seen
  | (key, value) :: rest =>
    match lookup key seen with
    | Some _ => inl key
    | None => scan rest ((key, value) :: seen)
    end
  end.

Theorem load_preserves_first_error : forall rows index seen,
  agree index seen ->
  match load rows index, scan rows seen with
  | inl a, inl b => a = b
  | inr a, inr b => agree a b
  | _, _ => False
  end.
Proof.
  induction rows as [|[key value] rest IH]; intros index seen same; simpl; auto.
  rewrite same. destruct (lookup key seen); [reflexivity|].
  apply IH, add_agrees, same.
Qed.

Fixpoint inspect read (keys : list nat) : nat + list Value :=
  match keys with
  | [] => inr []
  | key :: rest =>
    match read key with
    | None => inl key
    | Some value =>
      match inspect read rest with
      | inl failed => inl failed
      | inr values => inr (value :: values)
      end
    end
  end.

Theorem receipt_order_preserved : forall keys index rows,
  agree index rows -> inspect (fun key => Index.find key index) keys =
    inspect (fun key => lookup key rows) keys.
Proof.
  induction keys as [|key rest IH]; intros index rows same; simpl; auto.
  rewrite same. destruct (lookup key rows); auto. now rewrite IH with (rows := rows).
Qed.

Theorem duplicate_prefix_preserved : forall prefix key value other suffix index,
  load prefix index = inr other -> Index.find key other <> None ->
  load (prefix ++ (key, value) :: suffix) index = inl key.
Proof.
  induction prefix as [|[first item] rest IH]; intros key value other suffix index loaded present.
  - simpl in loaded. inversion loaded; subst. simpl.
    destruct (Index.find key other); [reflexivity|contradiction].
  - simpl in *. destruct (Index.find first index); [discriminate|].
    eapply IH; eauto.
Qed.
End Lookup.

Section Materialize.
Variable Tx : Type.
Variable hash : Tx -> nat.
Variable bytes : Tx -> nat.

Fixpoint prepare (txs : list Tx) : list (nat * Tx) * nat :=
  match txs with
  | [] => ([], 0)
  | tx :: rest =>
    let '(rows, work) := prepare rest in
    ((hash tx, tx) :: rows, bytes tx + work)
  end.

Theorem one_serialization_pass : forall txs,
  fst (prepare txs) = map (fun tx => (hash tx, tx)) txs /\
  snd (prepare txs) = fold_right (fun tx total => bytes tx + total) 0 txs.
Proof.
  induction txs as [|tx rest IH]; simpl; [auto|].
  destruct (prepare rest) as [rows work]. simpl in *. destruct IH; subst. auto.
Qed.
End Materialize.

Print Assumptions load_preserves_first_error.
Print Assumptions receipt_order_preserved.
Print Assumptions duplicate_prefix_preserved.
Print Assumptions one_serialization_pass.