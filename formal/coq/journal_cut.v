(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith ZArith Lia.
Import ListNotations.

Definition cut_prefix {A : Type} (offset : nat) (bytes : list A) :=
  firstn offset bytes.

Theorem retained_prefix : forall (A : Type) (kept suffix : list A),
  cut_prefix (length kept) (kept ++ suffix) = kept.
Proof.
  intros A kept suffix. unfold cut_prefix.
  rewrite firstn_app, firstn_all, Nat.sub_diag. simpl. apply app_nil_r.
Qed.

Theorem repeat_cut : forall (A : Type) offset (bytes : list A),
  cut_prefix offset (cut_prefix offset bytes) = cut_prefix offset bytes.
Proof.
  intros. unfold cut_prefix. rewrite firstn_firstn, Nat.min_id. reflexivity.
Qed.

Theorem committed_txids : forall (head start txid : Z),
  start = (head + 1)%Z -> (txid <= head)%Z -> ~ (start <= txid)%Z.
Proof. intros. lia. Qed.

Theorem future_txids : forall (head start txid : Z),
  start = (head + 1)%Z -> (start <= txid)%Z -> (head < txid)%Z.
Proof. intros. lia. Qed.

Inductive phase := Inspect | Cut | FileSynced | SuffixRemoved | DirectorySynced | IndexSynced.
Inductive event := Checked | FileAck | RemoveAck | DirectoryAck | IndexAck | Failure.

Definition step (state : phase) (input : event) : phase :=
  match state, input with
  | Inspect, Checked => Cut
  | Cut, FileAck => FileSynced
  | FileSynced, RemoveAck => SuffixRemoved
  | SuffixRemoved, DirectoryAck => DirectorySynced
  | DirectorySynced, IndexAck => IndexSynced
  | _, _ => state
  end.

Definition delete_wal (state : phase) : bool :=
  match state with IndexSynced => true | _ => false end.

Theorem failure_keeps_wal : forall state,
  delete_wal state = false -> delete_wal (step state Failure) = false.
Proof. intros state refused. destruct state; exact refused. Qed.

Theorem index_ack_required : forall state input,
  delete_wal state = false -> delete_wal (step state input) = true ->
  state = DirectorySynced /\ input = IndexAck.
Proof.
  intros state input before after.
  destruct state; destruct input; simpl in *; try discriminate; auto.
Qed.

Theorem directory_ack_required : forall state input,
  state <> DirectorySynced -> step state input = DirectorySynced ->
  state = SuffixRemoved /\ input = DirectoryAck.
Proof.
  intros state input before after.
  destruct state; destruct input; simpl in *; try discriminate; try contradiction; auto.
Qed.

Print Assumptions retained_prefix.
Print Assumptions repeat_cut.
Print Assumptions committed_txids.
Print Assumptions future_txids.
Print Assumptions failure_keeps_wal.
Print Assumptions index_ack_required.
Print Assumptions directory_ack_required.