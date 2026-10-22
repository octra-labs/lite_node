(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith Bool List Lia.
Import ListNotations.

Record certificate := Certificate {
  epoch : nat;
  previous : nat;
  result_root : nat
}.

Record state := State {
  head : nat;
  root : nat;
  writes : nat;
  pending : option certificate
}.

Inductive event := Inspect (c : certificate) | Apply | Restart | Upgrade.

Section Replay.
Variable base initial : nat.
Variable checked : certificate -> bool.
Variable execute : nat -> certificate -> nat.

Definition linked s c :=
  (writes s =? 0) && (head s =? base) && (root s =? initial) &&
  (epoch c =? S base) && (previous c =? root s) && checked c.

Definition step s input :=
  match input with
  | Inspect c =>
    match pending s with
    | None => if linked s c then State (head s) (root s) (writes s) (Some c) else s
    | Some _ => s
    end
  | Apply =>
    match pending s with
    | Some c =>
      if linked s c && (execute (root s) c =? result_root c)
      then State (epoch c) (result_root c) (S (writes s)) None
      else State (head s) (root s) (writes s) None
    | None => s
    end
  | Restart | Upgrade => State (head s) (root s) (writes s) None
  end.

Definition valid s :=
  (writes s = 0 /\ head s = base /\ root s = initial) \/
  (writes s = 1 /\ head s = S base /\
    exists c, checked c = true /\ epoch c = S base /\
      previous c = initial /\ root s = result_root c /\
      execute initial c = result_root c).

Lemma linked_exact s c : linked s c = true ->
  writes s = 0 /\ head s = base /\ root s = initial /\
  epoch c = S base /\ previous c = initial /\ checked c = true.
Proof.
  unfold linked. repeat rewrite andb_true_iff. repeat rewrite Nat.eqb_eq.
  intros [[[[[count height] source] target] parent] proof].
  repeat split; auto. now rewrite source in parent.
Qed.

Theorem step_valid s input : valid s -> valid (step s input).
Proof.
  intros safe. destruct input as [c| | |]; unfold step.
  - destruct (pending s); auto. destruct (linked s c); auto.
  - destruct (pending s) as [c|]; auto.
    destruct (linked s c && (execute (root s) c =? result_root c)) eqn:ok.
    + apply andb_true_iff in ok. destruct ok as [link computation].
      apply linked_exact in link.
      destruct link as [count [height [source [target [parent proof]]]]].
      apply Nat.eqb_eq in computation. unfold valid. simpl. right.
      rewrite count, target. repeat split; auto.
      exists c. repeat split; auto. now rewrite source in computation.
    + exact safe.
  - exact safe.
  - exact safe.
Qed.

Theorem trace_valid inputs s : valid s -> valid (fold_left step inputs s).
Proof.
  revert s. induction inputs as [|input rest ih]; intros s safe; simpl; auto.
  apply ih. now apply step_valid.
Qed.

Theorem at_most_one_write inputs :
  writes (fold_left step inputs (State base initial 0 None)) <= 1.
Proof.
  assert (valid (State base initial 0 None)) as start by (left; auto).
  pose proof (trace_valid inputs _ start) as final.
  destruct final as [[count _]|[count _]]; lia.
Qed.

Theorem committed_execution inputs :
  let s := fold_left step inputs (State base initial 0 None) in
  writes s = 1 -> head s = S base /\
    exists c, checked c = true /\ epoch c = S base /\
      previous c = initial /\ root s = result_root c /\
      execute initial c = result_root c.
Proof.
  simpl. intros committed.
  assert (valid (State base initial 0 None)) as start by (left; auto).
  pose proof (trace_valid inputs _ start) as final.
  destruct final as [[count _]|[_ final]]; [lia|exact final].
Qed.

Theorem changed_execution_keeps_head s c :
  pending s = Some c -> execute (root s) c <> result_root c ->
  head (step s Apply) = head s /\ root (step s Apply) = root s /\
  writes (step s Apply) = writes s.
Proof.
  intros queued different. unfold step. rewrite queued.
  apply Nat.eqb_neq in different. rewrite different, andb_false_r.
  repeat split; reflexivity.
Qed.

Theorem restart_preserves_commit s :
  head (step s Restart) = head s /\ root (step s Restart) = root s /\
  writes (step s Restart) = writes s.
Proof. repeat split; reflexivity. Qed.

Theorem upgrade_requires_inspection s :
  head (step (step s Upgrade) Apply) = head s /\
  root (step (step s Upgrade) Apply) = root s /\
  writes (step (step s Upgrade) Apply) = writes s.
Proof. repeat split; reflexivity. Qed.

Theorem upgrade_then_checked_apply s c :
  linked s c = true -> execute (root s) c = result_root c ->
  let final := fold_left step [Upgrade; Inspect c; Apply] s in
  head final = epoch c /\ root final = result_root c /\
  writes final = 1.
Proof.
  intros Hlink Hroot. simpl. unfold step. simpl.
  assert (Hsame : linked (State (head s) (root s) (writes s) None) c = true).
  { exact Hlink. }
  rewrite Hsame. simpl.
  assert (Hqueued : linked (State (head s) (root s) (writes s) (Some c)) c = true).
  { exact Hlink. }
  rewrite Hqueued, Hroot, Nat.eqb_refl. simpl.
  apply linked_exact in Hlink. destruct Hlink as [count _].
  rewrite count. repeat split; reflexivity.
Qed.
End Replay.

Print Assumptions trace_valid.
Print Assumptions at_most_one_write.
Print Assumptions committed_execution.
Print Assumptions changed_execution_keeps_head.
Print Assumptions restart_preserves_commit.
Print Assumptions upgrade_requires_inspection.
Print Assumptions upgrade_then_checked_apply.