(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import Arith Bool Lia List.
Import ListNotations.

Record state := State {
  persisted : bool;
  owned : bool;
  eligible : bool;
  pending : nat;
  ready : bool;
  uses : nat
}.

Inductive event :=
| Open
| Write
| Finish
| Close
| Changed
| Consume (matching : bool).

Definition initial := State false false false 0 false 0.

Definition revoke s :=
  State (persisted s) false false (pending s) false (uses s).

Definition step s e :=
  match e with
  | Open => if owned s then s else
      State true true (negb (persisted s)) 3 false (uses s)
  | Write => if owned s && negb (ready s) then
      State (persisted s) (owned s) (eligible s) (pred (pending s)) false (uses s)
      else s
  | Finish => if owned s && eligible s && Nat.eqb (pending s) 0 then
      State (persisted s) true false 0 true (uses s) else s
  | Close | Changed => revoke s
  | Consume matching =>
      State (persisted s) (owned s) false (pending s) false
        (uses s + if ready s && owned s && matching then 1 else 0)
  end.

Definition flag (b : bool) := if b then 1 else 0.

Definition safe s :=
  uses s + flag (ready s) + flag (eligible s) <= 1 /\
  (eligible s = true -> persisted s = true /\ owned s = true) /\
  (ready s = true -> persisted s = true /\ owned s = true /\ pending s = 0) /\
  (persisted s = false -> uses s = 0 /\ ready s = false /\ eligible s = false).

Lemma initial_safe : safe initial.
Proof. unfold safe, initial, flag; simpl; intuition (try discriminate; lia). Qed.

Lemma step_safe s e : safe s -> safe (step s e).
Proof.
  destruct s as [disk owner origin writes armed spent].
  unfold safe, step, revoke, flag; simpl.
  destruct disk, owner, origin, armed; destruct e;
    try destruct matching; simpl;
    try destruct (Nat.eqb writes 0) eqn:E; simpl;
    try apply Nat.eqb_eq in E; intuition (try discriminate; lia).
Qed.

Fixpoint run events s :=
  match events with [] => s | e :: rest => run rest (step s e) end.

Theorem run_safe events s : safe s -> safe (run events s).
Proof.
  revert s; induction events as [|e rest IH]; intros s H; simpl; auto.
  apply IH, step_safe, H.
Qed.

Theorem single_use events : uses (run events initial) <= 1.
Proof.
  pose proof (run_safe events initial initial_safe) as H.
  unfold safe in H; destruct H as [H _]; lia.
Qed.

Theorem requires_writes s matching : safe s ->
  uses (step s (Consume matching)) > uses s ->
  pending s = 0 /\ persisted s = true /\ owned s = true /\ matching = true.
Proof.
  destruct s as [disk owner origin writes armed spent].
  unfold safe, step; simpl.
  destruct disk, owner, origin, armed, matching; simpl;
    intuition (try discriminate; lia).
Qed.

Theorem reopen_cannot_renew s : persisted s = true ->
  eligible (step (step s Close) Open) = false /\
  ready (step (step s Close) Open) = false.
Proof. destruct s; unfold step, revoke; simpl; intros H; rewrite H; auto. Qed.

Theorem mismatch_does_not_grant s :
  uses (step s (Consume false)) = uses s /\
  ready (step s (Consume false)) = false.
Proof.
  destruct s; unfold step; simpl; destruct ready0, owned0; simpl; lia.
Qed.

Example fresh_sequence : uses
  (run [Open; Write; Write; Write; Finish; Consume true] initial) = 1.
Proof. reflexivity. Qed.

Example failed_sequence : uses
  (run [Open; Write; Close; Open; Write; Write; Write; Finish; Consume true] initial) = 0.
Proof. reflexivity. Qed.