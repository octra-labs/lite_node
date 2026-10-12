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

Module Cert_cache.

Section Cache.
Context {Value : Type}.

Record entry := Entry {
  key : nat;
  trust : nat;
  size : nat;
  value : Value
}.

Fixpoint bytes (xs : list entry) : nat :=
  match xs with
  | [] => 0
  | x :: rest => size x + bytes rest
  end.

Fixpoint trim (count budget : nat) (xs : list entry) : list entry :=
  match xs with
  | [] => []
  | x :: rest =>
    match count with
    | 0 => []
    | S count =>
      if size x <=? budget then
        x :: trim count (budget - size x) rest
      else []
    end
  end.

Lemma trim_caps : forall xs count budget,
  length (trim count budget xs) <= count /\
  bytes (trim count budget xs) <= budget.
Proof.
  induction xs as [|x rest step]; intros [|count] budget; simpl; try (split; lia).
  destruct (size x <=? budget) eqn:fits; simpl; try (split; lia).
  apply Nat.leb_le in fits.
  specialize (step count (budget - size x)). destruct step as [items memory].
  split; lia.
Qed.

Lemma trim_prefix : forall xs count budget,
  exists suffix, xs = trim count budget xs ++ suffix.
Proof.
  induction xs as [|x rest step]; intros [|count] budget; simpl.
  - exists []. reflexivity.
  - exists []. reflexivity.
  - exists (x :: rest). reflexivity.
  - destruct (size x <=? budget) eqn:fits.
    + destruct (step count (budget - size x)) as [suffix kept].
      exists suffix. simpl. now rewrite <- kept.
    + exists (x :: rest). reflexivity.
Qed.

Definition matches (digest trusted : nat) (x : entry) : bool :=
  (key x =? digest) && (trust x =? trusted).

Definition lookup (digest trusted : nat) (xs : list entry) : option entry :=
  find (matches digest trusted) xs.

Definition save (count budget : nat) (x : entry) (xs : list entry) : list entry :=
  if size x <=? budget then
    trim count budget
      (x :: filter (fun y => negb (matches (key x) (trust x) y)) xs)
  else xs.

Theorem save_caps : forall count budget x xs,
  length xs <= count -> bytes xs <= budget ->
  length (save count budget x xs) <= count /\
  bytes (save count budget x xs) <= budget.
Proof.
  intros count budget x xs items memory. unfold save.
  destruct (size x <=? budget).
  - apply trim_caps.
  - auto.
Qed.

Theorem save_origin : forall count budget x xs y,
  In y (save count budget x xs) -> y = x \/ In y xs.
Proof.
  intros count budget x xs y present. unfold save in present.
  destruct (size x <=? budget) eqn:fits; [|auto].
  destruct (trim_prefix
    (x :: filter (fun y => negb (matches (key x) (trust x) y)) xs)
    count budget) as [suffix kept].
  assert (In y (x :: filter (fun y => negb (matches (key x) (trust x) y)) xs)) as origin.
  { rewrite kept. apply in_or_app. auto. }
  simpl in origin. destruct origin as [same | prior]; [left; auto | right].
  apply filter_In in prior. tauto.
Qed.

Theorem lookup_trust : forall digest trusted xs x,
  lookup digest trusted xs = Some x ->
  In x xs /\ key x = digest /\ trust x = trusted.
Proof.
  intros digest trusted xs x found. unfold lookup in found.
  apply find_some in found as [present matched].
  unfold matches in matched. apply Bool.andb_true_iff in matched as [hashed trusted_key].
  apply Nat.eqb_eq in hashed. apply Nat.eqb_eq in trusted_key. auto.
Qed.

Theorem trust_refusal : forall digest trusted xs,
  Forall (fun x => trust x <> trusted) xs -> lookup digest trusted xs = None.
Proof.
  intros digest trusted xs distinct.
  destruct (lookup digest trusted xs) as [x|] eqn:found; [|reflexivity].
  apply lookup_trust in found as [present [_ same]].
  rewrite Forall_forall in distinct. specialize (distinct x present).
  contradiction.
Qed.

End Cache.

Print Assumptions trim_caps.
Print Assumptions trim_prefix.
Print Assumptions save_caps.
Print Assumptions save_origin.
Print Assumptions lookup_trust.
Print Assumptions trust_refusal.

End Cert_cache.

Module Capture_cycle.

Record state := State {
  published : option nat;
  running : option nat;
  seen : option nat
}.

Inductive effect := Capture (epoch : nat) | Retain (count : nat).
Inductive outcome := Published (epoch : nat) | Failed.

Definition target (interval epoch minimum : nat) :=
  Nat.max (epoch - epoch mod interval) minimum.

Definition due (prior : option nat) (epoch : nat) :=
  match prior with None => true | Some old => old <? epoch end.

Definition schedule (interval : nat) (value : state) (epoch minimum : nat) :=
  if minimum <=? epoch then
    let older := match seen value with
      | Some old => epoch <? old
      | None => false
      end in
    if older then Some (value, []) else
      let next := State (published value) (running value) (Some epoch) in
      match running value with
      | Some _ => Some (next, [])
      | None =>
        let wanted := target interval epoch minimum in
        if due (published value) wanted then
          Some (State (published value) (Some wanted) (Some epoch), [Capture wanted])
        else Some (next, [])
      end
  else None.

Definition complete (count : nat) (value : state) (epoch : nat) (result : outcome) :=
  match running value with
  | None => None
  | Some wanted =>
    if wanted =? epoch then
      match result with
      | Failed => Some (State (published value) None (seen value), [])
      | Published actual =>
        if epoch <=? actual then
          let next := match published value with
            | None => actual
            | Some old => Nat.max old actual
            end in
          Some (State (Some next) None (seen value), [Retain count])
        else None
      end
    else None
  end.

Lemma target_limits : forall interval epoch minimum,
  minimum <= epoch ->
  minimum <= target interval epoch minimum <= epoch.
Proof.
  intros interval epoch minimum valid. unfold target.
  split; [apply Nat.le_max_r | apply Nat.max_lub; lia].
Qed.

Lemma zero_minimum : forall interval epoch,
  target interval epoch 0 = epoch - epoch mod interval.
Proof. intros. unfold target. apply Nat.max_0_r. Qed.

Theorem capture_due : forall interval epoch minimum prior,
  prior < minimum -> minimum <= epoch ->
  schedule interval (State (Some prior) None (Some epoch)) epoch minimum =
  Some (State (Some prior) (Some (target interval epoch minimum)) (Some epoch),
    [Capture (target interval epoch minimum)]).
Proof.
  intros interval epoch minimum prior behind valid.
  assert (minimum <=? epoch = true) as allowed by (apply Nat.leb_le; exact valid).
  assert (epoch <? epoch = false) as same by (apply Nat.ltb_ge; lia).
  assert (prior <? target interval epoch minimum = true) as needed.
  { apply Nat.ltb_lt. pose proof (target_limits interval epoch minimum valid). lia. }
  unfold schedule. simpl. rewrite allowed, same. simpl. unfold due.
  rewrite needed. reflexivity.
Qed.

Theorem single_capture : forall interval epoch minimum prior wanted old next effects,
  schedule interval (State prior (Some wanted) old) epoch minimum = Some (next, effects) ->
  effects = [] /\ running next = Some wanted /\ published next = prior.
Proof.
  intros interval epoch minimum prior wanted old next effects accepted.
  unfold schedule in accepted. simpl in accepted.
  destruct (minimum <=? epoch); [|discriminate].
  destruct old as [old|]; simpl in accepted.
  - destruct (epoch <? old); inversion accepted; subst; simpl; auto.
  - inversion accepted; subst; simpl; auto.
Qed.

Theorem publication_minimum : forall count value epoch actual next effects minimum,
  minimum <= epoch ->
  complete count value epoch (Published actual) = Some (next, effects) ->
  exists saved, published next = Some saved /\ minimum <= actual /\ actual <= saved.
Proof.
  intros count [prior active old] epoch actual next effects minimum required accepted.
  unfold complete in accepted. simpl in accepted.
  destruct active as [wanted|]; [|discriminate].
  destruct (wanted =? epoch); [|discriminate].
  destruct (epoch <=? actual) eqn:valid; [|discriminate].
  apply Nat.leb_le in valid. destruct prior as [prior|]; inversion accepted; subst; simpl.
  - exists (Nat.max prior actual). split; [reflexivity|].
    split; [lia|apply Nat.le_max_r].
  - exists actual. split; [reflexivity|]. split; lia.
Qed.

Theorem failed_keeps_image : forall count value epoch next effects,
  complete count value epoch Failed = Some (next, effects) ->
  published next = published value /\ running next = None /\ effects = [].
Proof.
  intros count [prior active old] epoch next effects accepted.
  unfold complete in accepted. simpl in accepted.
  destruct active as [wanted|]; [|discriminate].
  destruct (wanted =? epoch); [|discriminate].
  inversion accepted; subst; simpl; auto.
Qed.

Theorem stable_head : forall interval epoch minimum,
  minimum <= epoch ->
  schedule interval (State (Some epoch) None (Some epoch)) epoch minimum =
  Some (State (Some epoch) None (Some epoch), []).
Proof.
  intros interval epoch minimum valid.
  assert (minimum <=? epoch = true) as allowed by (apply Nat.leb_le; exact valid).
  assert (epoch <? epoch = false) as same by (apply Nat.ltb_ge; lia).
  assert (epoch <? target interval epoch minimum = false) as done.
  { apply Nat.ltb_ge. apply (target_limits interval epoch minimum valid). }
  unfold schedule. simpl. rewrite allowed, same. simpl. unfold due.
  rewrite done. reflexivity.
Qed.

Print Assumptions target_limits.
Print Assumptions zero_minimum.
Print Assumptions capture_due.
Print Assumptions single_capture.
Print Assumptions publication_minimum.
Print Assumptions failed_keeps_image.
Print Assumptions stable_head.

End Capture_cycle.