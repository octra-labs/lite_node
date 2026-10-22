(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool Arith Lia.
Import ListNotations.

Record proof := { round : nat; pid : nat; signers : list nat }.
Record receipt := { context : nat; proofs : list proof }.
Record state := { disk : list proof; saved : option receipt }.

Definition matches r p item := (round item =? r) && (pid item =? p).

Definition hit ctx r p s :=
  match saved s with
  | None => false
  | Some stamp => (context stamp =? ctx) && existsb (matches r p) (proofs stamp)
  end.

Definition synced check s :=
  match saved s with
  | None => True
  | Some stamp => proofs stamp = disk s /\ length (proofs stamp) <= 3 /\
    Forall (fun item => check (context stamp) item = true) (proofs stamp)
  end.

Definition write check ctx rows (sync : bool) s :=
  if forallb (check ctx) rows && (length rows <=? 3) then
    {| disk := rows; saved := if sync then Some {| context := ctx; proofs := rows |} else None |}
  else {| disk := disk s; saved := None |}.

Theorem write_safe : forall check ctx rows sync s,
  synced check (write check ctx rows sync s).
Proof.
  intros. unfold write.
  destruct (forallb (check ctx) rows && (length rows <=? 3)) eqn:checked; [|exact I].
  destruct sync; [|exact I]. unfold synced. simpl.
  apply andb_true_iff in checked. destruct checked as [valid count].
  apply Nat.leb_le in count. split; [reflexivity|]. split; [exact count|].
  apply Forall_forall. intros item found. now apply (proj1 (forallb_forall _ _) valid).
Qed.

Theorem hit_has_synced_proof : forall check ctx r p s,
  synced check s -> hit ctx r p s = true ->
  exists item, In item (disk s) /\ round item = r /\ pid item = p /\ check ctx item = true.
Proof.
  intros check ctx r p s valid found. unfold hit in found.
  destruct (saved s) as [stamp|] eqn:ack; [|discriminate].
  apply andb_true_iff in found. destruct found as [same found].
  apply Nat.eqb_eq in same. apply existsb_exists in found.
  destruct found as [item [present matching]].
  unfold synced in valid. rewrite ack in valid. destruct valid as [kept [_ checked]].
  unfold matches in matching. apply andb_true_iff in matching.
  destruct matching as [r_same p_same]. apply Nat.eqb_eq in r_same, p_same.
  exists item. split; [now rewrite <- kept|]. split; [exact r_same|]. split; [exact p_same|].
  rewrite Forall_forall in checked. rewrite <- same. now apply checked.
Qed.

Theorem failed_sync_has_no_hit : forall check ctx rows s want r p,
  hit want r p (write check ctx rows false s) = false.
Proof.
  intros. unfold write. destruct (forallb (check ctx) rows && (length rows <=? 3)); reflexivity.
Qed.

Definition restart s := {| disk := disk s; saved := None |}.

Theorem restart_needs_sync : forall s ctx r p,
  hit ctx r p (restart s) = false.
Proof. reflexivity. Qed.

Theorem other_context_misses : forall s stamp ctx r p,
  saved s = Some stamp -> context stamp <> ctx -> hit ctx r p s = false.
Proof.
  intros. unfold hit. rewrite H. assert (context stamp =? ctx = false) as different
    by (apply Nat.eqb_neq; assumption). now rewrite different.
Qed.

Definition ensure check ctx rows sync r p s :=
  if hit ctx r p s then (s, 0) else (write check ctx rows sync s, 1).

Theorem repeat_no_write : forall check ctx rows sync r p s,
  hit ctx r p s = true -> ensure check ctx rows sync r p s = (s, 0).
Proof. intros. unfold ensure. now rewrite H. Qed.

Theorem kept_only : forall check ctx rows sync s r p,
  hit ctx r p (write check ctx rows sync s) = true -> exists item,
    In item rows /\ round item = r /\ pid item = p.
Proof.
  intros. unfold write in H.
  destruct (forallb (check ctx) rows && (length rows <=? 3)); [|discriminate].
  destruct sync; [|discriminate]. unfold hit in H. simpl in H.
  rewrite Nat.eqb_refl in H. simpl in H. apply existsb_exists in H.
  destruct H as [item [found match_]]. unfold matches in match_.
  apply andb_true_iff in match_. destruct match_ as [r_same p_same].
  apply Nat.eqb_eq in r_same, p_same. exists item. auto.
Qed.