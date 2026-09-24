(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import String List Bool.
Import ListNotations.
Open Scope string_scope.

Definition valid (cipher : option string) : bool :=
  match cipher with
  | None => true
  | Some text => String.eqb text "" || prefix "hfhe_v1" text
  end.

Definition can_load (cipher : option string) : bool :=
  match cipher with
  | Some text => String.eqb text "0" || valid cipher
  | None => true
  end.

Definition normalize (cipher : option string) : option string :=
  match cipher with
  | Some text => if String.eqb text "0" then None else cipher
  | None => None
  end.

Definition load (values : list (option string)) :=
  if forallb can_load values then Some (map normalize values) else None.

Theorem unknown_refused :
  forall values cipher,
    In cipher values -> can_load cipher = false -> load values = None.
Proof.
  intros values cipher present refused.
  unfold load.
  destruct (forallb can_load values) eqn:accepted; [|reflexivity].
  rewrite forallb_forall in accepted.
  specialize (accepted cipher present).
  rewrite refused in accepted.
  discriminate.
Qed.

Theorem only_zero_changes :
  forall cipher, normalize cipher <> cipher -> cipher = Some "0".
Proof.
  intros [text|] changed; [|contradiction].
  unfold normalize in changed.
  destruct (String.eqb text "0") eqn:zero; [|contradiction].
  apply String.eqb_eq in zero.
  subst text.
  reflexivity.
Qed.

Theorem nonzero_kept :
  forall cipher, cipher <> Some "0" -> normalize cipher = cipher.
Proof.
  intros [text|] nonzero; [|reflexivity].
  unfold normalize.
  destruct (String.eqb text "0") eqn:zero; [|reflexivity].
  apply String.eqb_eq in zero.
  subst text.
  contradiction.
Qed.

Theorem load_keeps_position :
  forall values result index cipher,
    load values = Some result ->
    nth_error values index = Some cipher ->
    cipher <> Some "0" -> nth_error result index = Some cipher.
Proof.
  intros values result index cipher loaded at_index nonzero.
  unfold load in loaded.
  destruct (forallb can_load values); [|discriminate].
  injection loaded as same.
  subst result.
  rewrite nth_error_map, at_index.
  simpl.
  rewrite nonzero_kept by exact nonzero.
  reflexivity.
Qed.

Theorem zero_compatible : can_load (Some "0") = true
  /\ normalize (Some "0") = None.
Proof. split; reflexivity. Qed.