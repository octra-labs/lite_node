(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool Arith Lia.
Import ListNotations.

Record program := {
  owner : nat;
  code : list nat;
  storage : list nat;
  balance : nat;
  report : option (list nat)
}.

Definition publish (caller : nat) (expected body data : list nat) (s : program)
  : option program :=
  if Nat.eqb caller (owner s) then
    if list_eq_dec Nat.eq_dec expected (code s) then
      if list_eq_dec Nat.eq_dec body (code s) then
        Some {| owner := owner s; code := code s; storage := storage s;
                balance := balance s; report := Some data |}
      else None
    else None
  else None.

Theorem owner_required : forall caller expected body data s next,
  publish caller expected body data s = Some next -> caller = owner s.
Proof.
  intros caller expected body data s next accepted.
  unfold publish in accepted.
  destruct (Nat.eqb caller (owner s)) eqn:same; try discriminate.
  apply Nat.eqb_eq. exact same.
Qed.

Theorem bytecode_required : forall caller expected body data s next,
  publish caller expected body data s = Some next ->
  expected = code s /\ body = code s.
Proof.
  intros caller expected body data s next accepted.
  unfold publish in accepted.
  destruct (Nat.eqb caller (owner s)); try discriminate.
  destruct (list_eq_dec Nat.eq_dec expected (code s)); try discriminate.
  destruct (list_eq_dec Nat.eq_dec body (code s)); try discriminate.
  auto.
Qed.

Theorem state_preserved : forall caller expected body data s next,
  publish caller expected body data s = Some next ->
  owner next = owner s /\ code next = code s /\
  storage next = storage s /\ balance next = balance s /\ report next = Some data.
Proof.
  intros caller expected body data s next accepted.
  unfold publish in accepted.
  destruct (Nat.eqb caller (owner s)); try discriminate.
  destruct (list_eq_dec Nat.eq_dec expected (code s)); try discriminate.
  destruct (list_eq_dec Nat.eq_dec body (code s)); try discriminate.
  inversion accepted. auto.
Qed.

Theorem repeat_stable : forall caller expected body data s next,
  publish caller expected body data s = Some next ->
  publish caller expected body data next = Some next.
Proof.
  intros caller expected body data s next accepted.
  pose proof (owner_required _ _ _ _ _ _ accepted) as own.
  pose proof (bytecode_required _ _ _ _ _ _ accepted) as [old bytes].
  pose proof (state_preserved _ _ _ _ _ _ accepted) as [o [c [v [b r]]]].
  unfold publish. rewrite o, own, Nat.eqb_refl.
  destruct (list_eq_dec Nat.eq_dec expected (code next)); [|congruence].
  destruct (list_eq_dec Nat.eq_dec body (code next)); [|congruence].
  destruct next; simpl in *. subst. reflexivity.
Qed.

Theorem foreign_refused : forall caller expected body data s,
  caller <> owner s -> publish caller expected body data s = None.
Proof.
  intros caller expected body data s foreign.
  unfold publish. destruct (Nat.eqb caller (owner s)) eqn:same; auto.
  apply Nat.eqb_eq in same. contradiction.
Qed.

Section Compiler.
Variable compile : nat -> list nat -> option (list nat * list nat).
Variable supported : nat -> bool.

Definition check_source active profile paid cost caller expected source s
    : option program * nat :=
  if active && supported profile && (cost <=? paid) then
    match compile profile source with
    | None => (None, 1)
    | Some (body, data) => (publish caller expected body data s, 1)
    end
  else (None, 0).

Theorem one_compilation : forall active profile paid cost caller expected source s,
  snd (check_source active profile paid cost caller expected source s) <= 1.
Proof.
  intros. unfold check_source.
  destruct (active && supported profile && (cost <=? paid)); simpl; try lia.
  destruct (compile profile source) as [[body data]|]; simpl; lia.
Qed.

Theorem paid_before_compile : forall active profile paid cost caller expected source s,
  paid < cost ->
  check_source active profile paid cost caller expected source s = (None, 0).
Proof.
  intros active profile paid cost caller expected source s short.
  unfold check_source.
  assert ((cost <=? paid) = false) as refused by (apply Nat.leb_gt; exact short).
  rewrite refused, andb_false_r. reflexivity.
Qed.

Theorem unknown_profile_refused : forall active profile paid cost caller expected source s,
  supported profile = false ->
  check_source active profile paid cost caller expected source s = (None, 0).
Proof.
  intros active profile paid cost caller expected source s different.
  unfold check_source.
  rewrite different, andb_false_r. reflexivity.
Qed.

Theorem inactive_keeps_program : forall profile paid cost caller expected source s,
  check_source false profile paid cost caller expected source s = (None, 0).
Proof. reflexivity. Qed.

Theorem verified_code_only : forall profile paid cost caller expected source s next work,
  check_source true profile paid cost caller expected source s = (Some next, work) ->
  exists body data, compile profile source = Some (body, data) /\
    body = code s /\ work = 1 /\ storage next = storage s /\ balance next = balance s.
Proof.
  intros profile paid cost caller expected source s next work accepted.
  unfold check_source in accepted.
  destruct (true && supported profile && (cost <=? paid)); try discriminate.
  destruct (compile profile source) as [[body data]|] eqn:compiled; try discriminate.
  inversion accepted as [[published count]].
  pose proof (bytecode_required _ _ _ _ _ _ published) as [_ exact].
  pose proof (state_preserved _ _ _ _ _ _ published) as [_ [_ [stored [funds _]]]].
  exists body, data. auto.
Qed.
End Compiler.

Print Assumptions owner_required.
Print Assumptions bytecode_required.
Print Assumptions state_preserved.
Print Assumptions repeat_stable.
Print Assumptions foreign_refused.
Print Assumptions one_compilation.
Print Assumptions paid_before_compile.
Print Assumptions unknown_profile_refused.
Print Assumptions inactive_keeps_program.
Print Assumptions verified_code_only.