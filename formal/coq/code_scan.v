(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List.
Import ListNotations.

Fixpoint call_pcs (pc : nat) (code : list bool) : list nat :=
  match code with
  | [] => []
  | call :: rest =>
    if call then pc :: call_pcs (S pc) rest else call_pcs (S pc) rest
  end.

Definition scan_step (state : nat * list nat) (call : bool) :=
  let '(pc, out) := state in
  (S pc, if call then pc :: out else out).

Theorem scan_acc :
  forall code pc out,
    snd (fold_left scan_step code (pc, out)) = rev (call_pcs pc code) ++ out.
Proof.
  induction code as [|call rest IH]; intros pc out.
  - reflexivity.
  - destruct call; simpl; rewrite IH.
    + rewrite <- app_assoc. reflexivity.
    + reflexivity.
Qed.

Theorem scan_order :
  forall code,
    rev (snd (fold_left scan_step code (0, []))) = call_pcs 0 code.
Proof.
  intro code.
  rewrite scan_acc, app_nil_r, rev_involutive.
  reflexivity.
Qed.