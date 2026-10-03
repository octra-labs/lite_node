(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia.
Import ListNotations.

Section Chain.
Context {Step : Type}.

Definition accept (cap : nat) (xs : list Step) : option (list Step) :=
  if length xs <=? cap then Some xs else None.

Definition choose (cap : nat) (saved base : option (list Step))
  : option (list Step) :=
  let retry := match base with Some xs => accept cap xs | None => None end in
  match saved with
  | Some xs => match accept cap xs with Some ys => Some ys | None => retry end
  | None => retry
  end.

Lemma accept_limit : forall cap xs ys,
  accept cap xs = Some ys -> length ys <= cap /\ xs = ys.
Proof.
  intros cap xs ys result. unfold accept in result.
  destruct (length xs <=? cap) eqn:size; try discriminate.
  inversion result; subst. apply Nat.leb_le in size. auto.
Qed.

Lemma choose_limit : forall cap saved base xs,
  choose cap saved base = Some xs -> length xs <= cap.
Proof.
  intros cap saved base xs result. unfold choose in result.
  destruct saved as [ys |].
  - destruct (accept cap ys) as [zs |] eqn:prior.
    + inversion result; subst. apply accept_limit in prior. tauto.
    + destruct base as [zs |]; try discriminate.
      apply accept_limit in result. tauto.
  - destruct base as [zs |]; try discriminate.
    apply accept_limit in result. tauto.
Qed.

Lemma choose_whole : forall cap saved base xs,
  choose cap saved base = Some xs -> saved = Some xs \/ base = Some xs.
Proof.
  intros cap saved base xs result. unfold choose in result.
  destruct saved as [ys |].
  - destruct (accept cap ys) as [zs |] eqn:prior.
    + inversion result; subst. apply accept_limit in prior as [_ same].
      subst. auto.
    + destruct base as [zs |]; try discriminate.
      apply accept_limit in result as [_ same]. subst. auto.
  - destruct base as [zs |]; try discriminate.
    apply accept_limit in result as [_ same]. subst. auto.
Qed.

Lemma retry_long : forall cap xs base,
  cap < length xs ->
  choose cap (Some xs) base = choose cap None base.
Proof.
  intros cap xs base size. unfold choose, accept.
  assert (length xs <=? cap = false) as denied by (apply Nat.leb_gt; lia).
  rewrite denied. reflexivity.
Qed.

Lemma keep_valid : forall cap xs base,
  length xs <= cap -> choose cap (Some xs) base = Some xs.
Proof.
  intros cap xs base size. unfold choose, accept.
  apply Nat.leb_le in size. rewrite size. reflexivity.
Qed.

Lemma no_chain : forall cap,
  choose cap None None = None.
Proof. reflexivity. Qed.

End Chain.

Record certificate := Certificate {
  cert_epoch : nat;
  cert_set : nat;
  cert_prev : nat;
  cert_root : nat;
  cert_valid : bool
}.

Definition check_cert (trust : nat -> option nat) (head root : nat)
    (cert : certificate) :=
  match trust (cert_epoch cert) with
  | None => false
  | Some set =>
    andb (Nat.eqb (cert_epoch cert) (S head))
      (andb (Nat.eqb (cert_prev cert) root)
        (andb (Nat.eqb (cert_set cert) set) (cert_valid cert)))
  end.

Fixpoint replay (trust : nat -> option nat) (head root : nat)
    (certs : list certificate) : option (nat * nat) :=
  match certs with
  | [] => Some (head, root)
  | cert :: rest =>
    if check_cert trust head root cert then
      replay trust (cert_epoch cert) (cert_root cert) rest
    else None
  end.

Lemma cert_checked : forall trust head root cert,
  check_cert trust head root cert = true ->
  cert_epoch cert = S head /\ cert_prev cert = root /\
  trust (cert_epoch cert) = Some (cert_set cert) /\ cert_valid cert = true.
Proof.
  intros trust head root cert valid. unfold check_cert in valid.
  destruct (trust (cert_epoch cert)) as [set |] eqn:selected; try discriminate.
  apply Bool.andb_true_iff in valid as [height valid].
  apply Bool.andb_true_iff in valid as [previous valid].
  apply Bool.andb_true_iff in valid as [committee proof].
  apply Nat.eqb_eq in height. apply Nat.eqb_eq in previous.
  apply Nat.eqb_eq in committee. subst. auto.
Qed.

Theorem replay_sets : forall certs trust head root result,
  replay trust head root certs = Some result ->
  Forall (fun cert => trust (cert_epoch cert) = Some (cert_set cert)
    /\ cert_valid cert = true) certs.
Proof.
  induction certs as [|cert rest step]; intros trust head root result accepted.
  - constructor.
  - simpl in accepted.
    destruct (check_cert trust head root cert) eqn:checked; try discriminate.
    constructor.
    + apply cert_checked in checked. tauto.
    + eapply step. exact accepted.
Qed.

Theorem replay_heights : forall certs trust head root result,
  replay trust head root certs = Some result ->
  map cert_epoch certs = seq (S head) (length certs).
Proof.
  induction certs as [|cert rest step]; intros trust head root result accepted.
  - reflexivity.
  - simpl in accepted.
    destruct (check_cert trust head root cert) eqn:checked; try discriminate.
    apply cert_checked in checked as [height _].
    specialize (step trust (cert_epoch cert) (cert_root cert) result accepted).
    simpl. rewrite step, height. reflexivity.
Qed.

Theorem replay_unknown : forall certs trust head root result cert,
  In cert certs -> trust (cert_epoch cert) = None ->
  replay trust head root certs <> Some result.
Proof.
  intros certs trust head root result cert present absent accepted.
  apply replay_sets in accepted.
  rewrite Forall_forall in accepted.
  specialize (accepted cert present). destruct accepted as [selected _].
  rewrite absent in selected. discriminate.
Qed.

Theorem replay_contiguous : forall certs trust head root last last_root,
  replay trust head root certs = Some (last, last_root) ->
  last = head + length certs.
Proof.
  induction certs as [|cert rest step]; intros trust head root last last_root accepted.
  - simpl in accepted. inversion accepted. simpl. lia.
  - simpl in accepted.
    destruct (check_cert trust head root cert) eqn:checked; try discriminate.
    apply cert_checked in checked as [height _].
    specialize (step trust (cert_epoch cert) (cert_root cert) last last_root accepted).
    simpl. lia.
Qed.

Print Assumptions replay_sets.
Print Assumptions replay_heights.
Print Assumptions replay_unknown.
Print Assumptions replay_contiguous.