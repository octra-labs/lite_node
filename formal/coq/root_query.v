(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool.
Import ListNotations.

Record slot := Slot { readers : list nat; replies : list nat }.
Definition pool := nat -> option slot.

Definition read reader value :=
  match value with
  | Some value =>
    if in_dec Nat.eq_dec reader (readers value) then replies value else []
  | None => []
  end.

Definition join reader value :=
  match value with
  | Some value => Some (Slot (reader :: readers value) (replies value))
  | None => Some (Slot [reader] [])
  end.

Definition leave reader value :=
  match value with
  | None => None
  | Some value =>
    match remove Nat.eq_dec reader (readers value) with
    | [] => None
    | retained => Some (Slot retained (replies value))
    end
  end.

Definition release epoch reader (queries : pool) : pool :=
  fun target => if Nat.eq_dec target epoch
    then leave reader (queries target) else queries target.

Theorem join_preserves : forall kept added value,
  In kept (readers value) ->
  read kept (join added (Some value)) = replies value.
Proof.
  intros kept added [members values] present.
  change (In kept members) in present.
  change ((if in_dec Nat.eq_dec kept (added :: members) then values else []) = values).
  destruct (in_dec Nat.eq_dec kept (added :: members)) as [inside|outside]; auto.
  exfalso. apply outside. right. exact present.
Qed.

Theorem join_shares : forall added value,
  read added (join added (Some value)) = replies value.
Proof.
  intros added [members values].
  change ((if in_dec Nat.eq_dec added (added :: members) then values else []) = values).
  destruct (in_dec Nat.eq_dec added (added :: members)) as [inside|outside]; auto.
  exfalso. apply outside. left. reflexivity.
Qed.

Theorem leave_preserves : forall kept removed value,
  kept <> removed -> In kept (readers value) ->
  read kept (leave removed (Some value)) = replies value.
Proof.
  intros kept removed [members values] distinct present.
  change (In kept members) in present.
  pose proof (in_in_remove Nat.eq_dec members distinct present) as retained.
  change (read kept (match remove Nat.eq_dec removed members with
    | [] => None | kept => Some (Slot kept values) end) = values).
  destruct (remove Nat.eq_dec removed members) as [|first rest] eqn:remaining.
  - contradiction.
  - change ((if in_dec Nat.eq_dec kept (first :: rest) then values else []) = values).
    destruct (in_dec Nat.eq_dec kept (first :: rest)); auto. contradiction.
Qed.

Theorem leave_closes : forall removed value,
  read removed (leave removed value) = [].
Proof.
  intros removed [[members values]|]; [|reflexivity].
  pose proof (remove_In Nat.eq_dec members removed) as absent.
  change (read removed (match remove Nat.eq_dec removed members with
    | [] => None | kept => Some (Slot kept values) end) = []).
  destruct (remove Nat.eq_dec removed members) as [|first rest] eqn:remaining; auto.
  change ((if in_dec Nat.eq_dec removed (first :: rest) then values else []) = []).
  destruct (in_dec Nat.eq_dec removed (first :: rest)); auto. contradiction.
Qed.

Theorem last_removes : forall reader values,
  leave reader (Some (Slot [reader] values)) = None.
Proof.
  intros. simpl. destruct (Nat.eq_dec reader reader); congruence.
Qed.

Theorem epoch_separate : forall queries epoch target reader,
  target <> epoch -> release epoch reader queries target = queries target.
Proof.
  intros. unfold release. destruct (Nat.eq_dec target epoch); congruence.
Qed.

Theorem old_release_preserves : forall old fresh value,
  old < fresh ->
  read fresh (leave old (join fresh (Some value))) = replies value.
Proof.
  intros old fresh [members values] later.
  apply (leave_preserves fresh old (Slot (fresh :: members) values)).
  - intro same. subst fresh. exact (Nat.lt_irrefl old later).
  - simpl. auto.
Qed.

Theorem allocation_distinct : forall previous next,
  previous < next -> previous <> S next.
Proof.
  intros previous next before same. subst previous.
  exact (Nat.lt_irrefl next (Nat.lt_trans next (S next) next (Nat.lt_succ_diag_r next) before)).
Qed.

Print Assumptions join_preserves.
Print Assumptions join_shares.
Print Assumptions leave_preserves.
Print Assumptions leave_closes.
Print Assumptions last_removes.
Print Assumptions epoch_separate.
Print Assumptions old_release_preserves.
Print Assumptions allocation_distinct.