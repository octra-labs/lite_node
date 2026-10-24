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

Module LateProof.

Record evidence := {
  checked : bool;
  nonnil : bool;
  count : nat;
  weight : nat;
  need_count : nat;
  need_weight : nat;
  at_round : nat;
  header_checked : bool
}.

Record node := {
  phase : nat;
  own_vote : option nat;
  lock : nat;
  valid : nat;
  known : list nat
}.

Definition quorum e := checked e && nonnil e &&
  (need_count e <=? count e) && (need_weight e <=? weight e).

Definition remember r rows := if existsb (Nat.eqb r) rows then rows else r :: rows.

Definition learn e s :=
  if quorum e then
    {| phase := phase s; own_vote := own_vote s; lock := lock s;
       valid := if header_checked e && (Nat.max (lock s) (valid s) <? at_round e)
                then at_round e else valid s;
       known := remember (at_round e) (known s) |}
  else s.

Definition timeout s :=
  {| phase := 2; own_vote := Some 0; lock := lock s;
     valid := valid s; known := known s |}.

Example delayed_quorum :
  known (learn
    {| checked := true; nonnil := true; count := 3; weight := 3;
       need_count := 3; need_weight := 3; at_round := 2; header_checked := true |}
    {| phase := 2; own_vote := Some 0; lock := 0; valid := 0; known := [] |}) = [2].
Proof. reflexivity. Qed.

Theorem quorum_requires_both : forall e,
  quorum e = true -> need_count e <= count e /\ need_weight e <= weight e.
Proof.
  intros e H. unfold quorum in H. repeat rewrite andb_true_iff in H.
  destruct H as [[[C N] K] W]. apply Nat.leb_le in K, W. auto.
Qed.

Theorem remember_repeat : forall r rows,
  remember r (remember r rows) = remember r rows.
Proof.
  intros. unfold remember. destruct (existsb (Nat.eqb r) rows) eqn:E.
  - now rewrite E.
  - simpl. now rewrite Nat.eqb_refl.
Qed.

Theorem learning_keeps_vote : forall e s,
  own_vote (learn e s) = own_vote s /\ lock (learn e s) = lock s /\
  phase (learn e s) = phase s.
Proof. intros. unfold learn. destruct (quorum e); simpl; auto. Qed.

Theorem learning_never_regresses : forall e s,
  valid s <= valid (learn e s).
Proof.
  intros. unfold learn. destruct (quorum e); simpl; [|lia].
  destruct (header_checked e && (Nat.max (lock s) (valid s) <? at_round e)) eqn:H; [|lia].
  apply andb_true_iff in H. destruct H as [_ H]. apply Nat.ltb_lt in H. lia.
Qed.

Theorem learning_after_timeout : forall e s,
  learn e (timeout s) = timeout (learn e s).
Proof. intros. unfold learn, timeout. destruct (quorum e); reflexivity. Qed.

Theorem learning_repeat : forall e s,
  learn e (learn e s) = learn e s.
Proof.
  intros. unfold learn. destruct (quorum e) eqn:Q; [|reflexivity]. simpl.
  rewrite remember_repeat.
  destruct (header_checked e && (Nat.max (lock s) (valid s) <? at_round e)) eqn:H.
  - simpl. assert (Nat.max (lock s) (at_round e) <? at_round e = false) as R
      by (apply Nat.ltb_ge; lia).
    rewrite R, andb_false_r. reflexivity.
  - simpl. rewrite H. reflexivity.
Qed.

Theorem proof_survives_timeout : forall e s,
  quorum e = true -> In (at_round e) (known (learn e (timeout s))).
Proof.
  intros e s Q. unfold learn. rewrite Q. simpl. unfold remember.
  destruct (existsb (Nat.eqb (at_round e)) (known s)) eqn:E; [|now left].
  apply existsb_exists in E. destruct E as [r [R E]]. apply Nat.eqb_eq in E. now subst.
Qed.

End LateProof.

Module ProofSearch.

Definition next (current : nat) (prior : option nat) :=
  match prior with
  | Some target => if S target <? current then S target else 0
  | None => 0
  end.

Definition seek current prior active ready :=
  if active && ready then Some (next current prior) else prior.

Definition retained recent requests target r :=
  recent r || existsb (Nat.eqb r) requests ||
  match target with Some wanted => r =? wanted | None => false end.

Theorem query_is_past : forall current prior,
  0 < current -> next current prior < current.
Proof.
  intros current [target|] H; simpl; [|lia].
  destruct (S target <? current) eqn:E; [apply Nat.ltb_lt in E|]; lia.
Qed.

Theorem scan_advances : forall current target,
  S target < current -> next current (Some target) = S target.
Proof.
  intros. simpl. apply Nat.ltb_lt in H. now rewrite H.
Qed.

Theorem scan_wraps : forall current target,
  current <= S target -> next current (Some target) = 0.
Proof.
  intros. simpl. apply Nat.ltb_ge in H. now rewrite H.
Qed.

Theorem pause_keeps_progress : forall current prior ready,
  seek current prior false ready = prior.
Proof. reflexivity. Qed.

Theorem rate_limit_keeps_progress : forall current prior active,
  seek current prior active false = prior.
Proof. intros. unfold seek. now rewrite andb_false_r. Qed.

Example evicted_round_two :
  retained (fun r => 67 - 64 <=? r) (seq 3 64) (Some 2) 2 = true.
Proof. reflexivity. Qed.

Theorem query_survives_eviction : forall recent requests target,
  retained recent requests (Some target) target = true.
Proof. intros. unfold retained. rewrite Nat.eqb_refl. now rewrite orb_true_r. Qed.

Record stored := { statement : nat; certificate : LateProof.evidence }.

Definition restore record executed s :=
  let e := certificate record in
  if (statement record =? LateProof.at_round e) && executed then
    LateProof.learn
      {| LateProof.checked := LateProof.checked e;
         LateProof.nonnil := LateProof.nonnil e;
         LateProof.count := LateProof.count e;
         LateProof.weight := LateProof.weight e;
         LateProof.need_count := LateProof.need_count e;
         LateProof.need_weight := LateProof.need_weight e;
         LateProof.at_round := LateProof.at_round e;
         LateProof.header_checked := true |} s
  else s.

Theorem restore_needs_execution : forall record s,
  restore record false s = s.
Proof. intros. unfold restore. now rewrite andb_false_r. Qed.

Theorem restore_needs_same_round : forall record s executed,
  statement record <> LateProof.at_round (certificate record) ->
  restore record executed s = s.
Proof.
  intros. unfold restore. apply Nat.eqb_neq in H. now rewrite H.
Qed.

Theorem restore_keeps_signed_state : forall record executed s,
  LateProof.own_vote (restore record executed s) = LateProof.own_vote s /\
  LateProof.lock (restore record executed s) = LateProof.lock s /\
  LateProof.phase (restore record executed s) = LateProof.phase s.
Proof.
  intros. unfold restore.
  destruct ((statement record =? LateProof.at_round (certificate record)) && executed).
  - apply LateProof.learning_keeps_vote.
  - auto.
Qed.

Theorem restore_preserves_newer_value : forall record executed s,
  LateProof.valid s <= LateProof.valid (restore record executed s).
Proof.
  intros. unfold restore.
  destruct ((statement record =? LateProof.at_round (certificate record)) && executed).
  - apply LateProof.learning_never_regresses.
  - lia.
Qed.

End ProofSearch.

Module SearchCredit.

Record query := { local_round : nat; target : nat; attempts : nat; requests : nat }.

Definition counters prior r wanted :=
  match prior with
  | Some q => if local_round q =? r then
      (if target q =? wanted then attempts q else 0, requests q)
    else (0, 0)
  | None => (0, 0)
  end.

Definition send prior r wanted ready :=
  let '(tries, count) := counters prior r wanted in
  if ready && (tries <? 4) && (count <? 8) then
    Some {| local_round := r; target := wanted;
      attempts := S tries; requests := S count |}
  else None.

Theorem finite_queries : forall prior r wanted ready q,
  send prior r wanted ready = Some q -> attempts q <= 4 /\ requests q <= 8.
Proof.
  intros. unfold send in H.
  destruct (counters prior r wanted) as [tries count].
  destruct (ready && (tries <? 4) && (count <? 8)) eqn:E; [|discriminate].
  inversion H; subst. simpl. repeat rewrite andb_true_iff in E.
  destruct E as [[_ T] C]. apply Nat.ltb_lt in T, C. lia.
Qed.

Theorem no_query_before_ready : forall prior r wanted,
  send prior r wanted false = None.
Proof. intros. unfold send. destruct (counters prior r wanted); reflexivity. Qed.

Theorem same_round_no_reset : forall prior r wanted ready q,
  local_round prior = r -> send (Some prior) r wanted ready = Some q ->
  requests q = S (requests prior).
Proof.
  intros prior r wanted ready q R H. unfold send, counters in H.
  rewrite R, Nat.eqb_refl in H.
  destruct (target prior =? wanted);
  destruct (ready && (_ <? 4) && (requests prior <? 8));
  inversion H; reflexivity.
Qed.

End SearchCredit.

Module ResponseCredit.

Definition spent := fold_right Nat.add 0.

Definition reserve cost recent :=
  if (8 <=? cost) && (cost + spent recent <=? 1024)
  then Some (cost :: recent) else None.

Theorem accepted_within_credit : forall cost recent kept,
  reserve cost recent = Some kept -> spent kept <= 1024.
Proof.
  intros. unfold reserve in H.
  destruct ((8 <=? cost) && (cost + spent recent <=? 1024)) eqn:E; [|discriminate].
  inversion H; subst. simpl. apply andb_true_iff in E.
  destruct E as [_ E]. now apply Nat.leb_le in E.
Qed.

Theorem expired_only_refunds : forall keep recent,
  spent (filter keep recent) <= spent recent.
Proof.
  intros keep recent. induction recent; simpl; [lia|].
  destruct (keep a); simpl; lia.
Qed.

Theorem positive_costs : forall cost recent kept,
  Forall (fun value => 8 <= value) recent ->
  reserve cost recent = Some kept -> Forall (fun value => 8 <= value) kept.
Proof.
  intros cost recent kept P H. unfold reserve in H.
  destruct ((8 <=? cost) && (cost + spent recent <=? 1024)) eqn:E; [|discriminate].
  inversion H; subst. apply andb_true_iff in E. destruct E as [E _].
  apply Nat.leb_le in E. now constructor.
Qed.

Theorem finite_history : forall recent,
  Forall (fun value => 8 <= value) recent -> spent recent <= 1024 -> length recent <= 128.
Proof.
  intros recent P H. assert (8 * length recent <= spent recent) as S.
  { clear H. induction P; unfold spent in *; simpl in *; lia. }
  lia.
Qed.

End ResponseCredit.

Module ProposalWork.

Definition select (current past : option nat) :=
  match current with Some value => Some value | None => past end.

Theorem current_first : forall current past, select (Some current) past = Some current.
Proof. reflexivity. Qed.

Theorem past_survives_current : forall past, select None (Some past) = Some past.
Proof. reflexivity. Qed.

Definition clean through (record : option nat) :=
  match record with Some height => if height <=? through then None else record | None => None end.

Theorem finalized_removed : forall height through,
  height <= through -> clean through (Some height) = None.
Proof. intros. unfold clean. apply Nat.leb_le in H. now rewrite H. Qed.

Theorem future_kept : forall height through,
  through < height -> clean through (Some height) = Some height.
Proof. intros. unfold clean. assert (height <=? through = false) by (apply Nat.leb_gt; lia). now rewrite H0. Qed.

End ProposalWork.