(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Bool Lia.
Import ListNotations.

Inductive phase := Building | Writing (bytes : nat).
Record job := Job { ticket : nat; peer : nat; validator : bool; stage : phase }.
Record state := State { serial : nat; jobs : list job }.
Inductive message := Request (who : nat) (lane : bool)
  | Prepared (id bytes : nat) | Written (id : nat) | Closed (id : nat).

Section Capacity.
Variable frame_cap : nat.
Definition cpu job := match stage job with Building => 1 | Writing _ => 0 end.
Definition bytes job := match stage job with Building => frame_cap | Writing n => n end.
Definition lane_sum measure lane values :=
  fold_right (fun job total => (if Bool.eqb (validator job) lane then measure job else 0) + total) 0 values.
Definition count := lane_sum cpu.
Definition retained := lane_sum bytes.
Definition busy who values := existsb (fun job => Nat.eqb (peer job) who) values.
Definition finish id s := State (serial s)
  (filter (fun job => negb (Nat.eqb (ticket job) id)) (jobs s)).
Definition prepare id n job :=
  match stage job with
  | Building => if (ticket job =? id) && (n <=? frame_cap)
    then Job (ticket job) (peer job) (validator job) (Writing n) else job
  | Writing _ => job
  end.
Definition step s input :=
  match input with
  | Request who lane =>
    if negb (busy who (jobs s)) && (count lane (jobs s) <? 2)
       && (retained lane (jobs s) + frame_cap <=? 4 * frame_cap)
    then State (S (serial s)) (Job (serial s) who lane Building :: jobs s)
    else s
  | Prepared id n => State (serial s) (map (prepare id n) (jobs s))
  | Written id | Closed id => finish id s
  end.

Lemma filter_sum : forall measure lane test values,
  lane_sum measure lane (filter test values) <= lane_sum measure lane values.
Proof.
  intros measure lane test values. induction values as [|value rest IH]; simpl; [lia|].
  unfold lane_sum in *. simpl in *. destruct (test value); simpl; lia.
Qed.

Lemma prepare_cpu : forall id n job, cpu (prepare id n job) <= cpu job.
Proof.
  intros id n [key who lane phase]. destruct phase; cbn [prepare cpu stage ticket peer validator]; [|lia].
  destruct ((key =? id) && (n <=? frame_cap)); cbn [cpu stage]; lia.
Qed.

Lemma prepare_bytes : forall id n job, bytes (prepare id n job) <= bytes job.
Proof.
  intros id n [key who lane phase]. destruct phase; cbn [prepare bytes stage ticket peer validator]; [|lia].
  destruct ((key =? id) && (n <=? frame_cap)) eqn:E; cbn [bytes stage]; [|lia].
  apply andb_true_iff in E as [_ size]. apply Nat.leb_le in size; assumption.
Qed.

Lemma prepare_lane : forall id n job, validator (prepare id n job) = validator job.
Proof.
  intros id n [key who lane phase]. destruct phase; cbn [prepare stage ticket peer validator]; auto.
  destruct ((key =? id) && (n <=? frame_cap)); reflexivity.
Qed.

Lemma prepare_sum : forall measure id n lane values,
  (forall job, measure (prepare id n job) <= measure job) ->
  lane_sum measure lane (map (prepare id n) values) <= lane_sum measure lane values.
Proof.
  intros measure id n lane values smaller. induction values as [|value rest IH]; simpl; [lia|].
  unfold lane_sum in *. simpl in *. rewrite prepare_lane.
  specialize (smaller value). destruct (Bool.eqb (validator value) lane); lia.
Qed.

Theorem cpu_capacity : forall s input,
  (forall lane, count lane (jobs s) <= 2) ->
  forall lane, count lane (jobs (step s input)) <= 2.
Proof.
  intros s input capacity lane. destruct input; cbn [step].
  - destruct (negb (busy who (jobs s)) && (count lane0 (jobs s) <? 2)
      && (retained lane0 (jobs s) + frame_cap <=? 4 * frame_cap)) eqn:E; [|apply capacity].
    apply andb_true_iff in E as [E _]. apply andb_true_iff in E as [_ room].
    apply Nat.ltb_lt in room.
    change ((if Bool.eqb lane0 lane then 1 else 0) + count lane (jobs s) <= 2).
    destruct lane0, lane; simpl in *; try apply capacity; lia.
  - eapply Nat.le_trans; [apply prepare_sum, prepare_cpu|apply capacity].
  - eapply Nat.le_trans; [apply filter_sum|apply capacity].
  - eapply Nat.le_trans; [apply filter_sum|apply capacity].
Qed.

Theorem byte_capacity : forall s input,
  (forall lane, retained lane (jobs s) <= 4 * frame_cap) ->
  forall lane, retained lane (jobs (step s input)) <= 4 * frame_cap.
Proof.
  intros s input capacity lane. destruct input; cbn [step].
  - destruct (negb (busy who (jobs s)) && (count lane0 (jobs s) <? 2)
      && (retained lane0 (jobs s) + frame_cap <=? 4 * frame_cap)) eqn:E; [|apply capacity].
    apply andb_true_iff in E as [_ room]. apply Nat.leb_le in room.
    change ((if Bool.eqb lane0 lane then frame_cap else 0) + retained lane (jobs s) <= 4 * frame_cap).
    destruct lane0, lane; simpl in *; try apply capacity; lia.
  - eapply Nat.le_trans; [apply prepare_sum, prepare_bytes|apply capacity].
  - eapply Nat.le_trans; [apply filter_sum|apply capacity].
  - eapply Nat.le_trans; [apply filter_sum|apply capacity].
Qed.

Theorem repeat_peer_refused : forall s who lane,
  busy who (jobs s) = true -> step s (Request who lane) = s.
Proof. intros; unfold step; rewrite H; reflexivity. Qed.

Theorem late_finish_preserves_new : forall s old newer,
  ticket newer <> old -> In newer (jobs s) -> In newer (jobs (finish old s)).
Proof.
  intros s old newer different present. unfold finish; simpl. apply filter_In; split; auto.
  apply Bool.negb_true_iff, Nat.eqb_neq; assumption.
Qed.

Theorem prepare_repeat : forall id n job,
  prepare id n (prepare id n job) = prepare id n job.
Proof.
  intros id n [key who lane phase]. destruct phase; cbn [prepare stage ticket peer validator]; auto.
  destruct ((key =? id) && (n <=? frame_cap)) eqn:E;
    cbn [prepare stage ticket peer validator]; try rewrite E; reflexivity.
Qed.

Theorem prepared_releases_cpu : forall id who lane n,
  n <= frame_cap -> cpu (prepare id n (Job id who lane Building)) = 0.
Proof.
  intros. unfold prepare; simpl. rewrite Nat.eqb_refl.
  assert (n <=? frame_cap = true) by (apply Nat.leb_le; assumption).
  rewrite H0. reflexivity.
Qed.

Theorem writers_leave_cpu : forall lane a b,
  count lane [Job 0 0 lane (Writing a); Job 1 1 lane (Writing b)] = 0 /\
  retained lane [Job 0 0 lane (Writing a); Job 1 1 lane (Writing b)] = a + b.
Proof.
  intros. unfold count, retained, lane_sum, cpu, bytes.
  destruct lane; simpl; split; lia.
Qed.

Theorem third_peer_can_start : forall lane a b,
  a <= frame_cap -> b <= frame_cap ->
  serial (step (State 2 [Job 0 0 lane (Writing a); Job 1 1 lane (Writing b)])
    (Request 2 lane)) = 3.
Proof.
  intros lane a b A B. unfold step. cbn [busy jobs existsb peer Nat.eqb negb].
  destruct (writers_leave_cpu lane a b) as [cpu_room byte_total].
  rewrite cpu_room, byte_total.
  assert (a + b + frame_cap <=? 4 * frame_cap = true) by (apply Nat.leb_le; lia).
  rewrite H. reflexivity.
Qed.

Print Assumptions cpu_capacity.
Print Assumptions byte_capacity.
Print Assumptions prepared_releases_cpu.
Print Assumptions third_peer_can_start.
Print Assumptions late_finish_preserves_new.
End Capacity.

Module Credit.
Record account := Account { balance : nat; spent : nat; earned : nat }.
Definition initial burst := Account burst 0 0.
Definition tick cap rate elapsed cost s :=
  let available := Nat.min cap (balance s + rate * elapsed) in
  if cost <=? available then
    Account (available - cost) (spent s + cost) (earned s + rate * elapsed)
  else Account available (spent s) (earned s + rate * elapsed).

Lemma tick_budget : forall burst cap rate elapsed cost s,
  balance s + spent s <= burst + earned s ->
  balance (tick cap rate elapsed cost s) + spent (tick cap rate elapsed cost s)
    <= burst + earned (tick cap rate elapsed cost s).
Proof.
  intros. unfold tick. pose proof (Nat.le_min_r cap (balance s + rate * elapsed)).
  destruct (cost <=? Nat.min cap (balance s + rate * elapsed)) eqn:E; simpl.
  - apply Nat.leb_le in E. lia.
  - lia.
Qed.

Fixpoint run cap rate events s :=
  match events with
  | [] => s
  | (elapsed, cost) :: rest => run cap rate rest (tick cap rate elapsed cost s)
  end.

Lemma run_budget : forall events burst cap rate s,
  balance s + spent s <= burst + earned s ->
  balance (run cap rate events s) + spent (run cap rate events s)
    <= burst + earned (run cap rate events s).
Proof.
  induction events as [|[elapsed cost] rest IH]; intros; simpl; auto.
  apply IH, tick_budget; assumption.
Qed.

Definition duration (events : list (nat * nat)) := fold_right (fun item total => fst item + total) 0 events.

Lemma run_earned : forall events cap rate s,
  earned (run cap rate events s) = earned s + rate * duration events.
Proof.
  induction events as [|[elapsed cost] rest IH]; intros; simpl; [lia|].
  rewrite IH. unfold tick. destruct (cost <=? Nat.min cap (balance s + rate * elapsed)); simpl; lia.
Qed.

Theorem two_peers_leave_credit : forall first second burst cap rate span cost,
  duration first <= span -> duration second <= span ->
  2 * burst + cost <= 2 * rate * span ->
  spent (run cap rate first (initial burst)) +
  spent (run cap rate second (initial burst)) + cost <= 4 * rate * span.
Proof.
  intros first second burst cap rate span cost D1 D2 room.
  assert (balance (initial burst) + spent (initial burst) <= burst + earned (initial burst)) as init.
  { unfold initial; simpl; lia. }
  pose proof (run_budget first burst cap rate (initial burst) init) as A.
  pose proof (run_budget second burst cap rate (initial burst) init) as B.
  rewrite run_earned in A, B.
  assert (rate * duration first <= rate * span) as M1 by (apply Nat.mul_le_mono_l; assumption).
  assert (rate * duration second <= rate * span) as M2 by (apply Nat.mul_le_mono_l; assumption).
  cbn [initial earned] in A, B. nia.
Qed.

Print Assumptions two_peers_leave_credit.
End Credit.

Module Turns.
Definition reserve who first price funds held :=
  if Nat.eqb who first then price <=? funds else price + held <=? funds.
Definition spend price funds := funds - price.

Theorem reservation_preserved : forall who first price funds held,
  who <> first -> reserve who first price funds held = true ->
  held <= spend price funds.
Proof.
  intros who first price funds held different accepted.
  unfold reserve in accepted.
  assert (Nat.eqb who first = false) as E by (apply Nat.eqb_neq; assumption).
  rewrite E in accepted. apply Nat.leb_le in accepted.
  unfold spend. lia.
Qed.

Theorem head_receives_credit : forall who price funds held,
  price <= funds -> reserve who who price funds held = true.
Proof.
  intros. unfold reserve. rewrite Nat.eqb_refl. apply Nat.leb_le; assumption.
Qed.

Definition live now entries := filter (fun item : nat * nat => now <? snd item) entries.

Theorem absent_head_expires : forall now who until entries,
  until <= now -> live now ((who, until) :: entries) = live now entries.
Proof.
  intros. change ((if now <? until then (who, until) :: live now entries
    else live now entries) = live now entries).
  assert (now <? until = false) as E by (apply Nat.ltb_ge; lia).
  now rewrite E.
Qed.

Definition lane_count lane entries := length (filter (fun item => Bool.eqb item lane) entries).

Theorem other_lane_no_capacity : forall lane entries,
  lane_count lane (negb lane :: entries) = lane_count lane entries.
Proof. intros lane entries. unfold lane_count. destruct lane; reflexivity. Qed.

Print Assumptions reservation_preserved.
Print Assumptions head_receives_credit.
Print Assumptions absent_head_expires.
Print Assumptions other_lane_no_capacity.

Definition protected_tick cap rate held cost funds :=
  let ready := Nat.min cap (funds + rate) in
  if held + cost <=? ready then ready - cost else ready.

Lemma protected_progress : forall cap rate held cost funds,
  held <= cap ->
  Nat.min held (funds + rate) <= protected_tick cap rate held cost funds.
Proof.
  intros. unfold protected_tick.
  destruct (held + cost <=? Nat.min cap (funds + rate)) eqn:E.
  - apply Nat.leb_le in E. pose proof (Nat.le_min_l held (funds + rate)). lia.
  - apply Nat.min_glb; [eapply Nat.le_trans; [apply Nat.le_min_l|assumption]|apply Nat.le_min_r].
Qed.

Fixpoint protected_run cap rate held costs funds :=
  match costs with
  | [] => funds
  | cost :: rest => protected_run cap rate held rest
      (protected_tick cap rate held cost funds)
  end.

Theorem finite_credit_wait : forall costs cap rate held funds,
  held <= cap ->
  Nat.min held (funds + length costs * rate) <= protected_run cap rate held costs funds.
Proof.
  induction costs as [|cost rest IH]; intros; simpl; [rewrite Nat.add_0_r; apply Nat.le_min_r|].
  specialize (IH cap rate held (protected_tick cap rate held cost funds) H).
  pose proof (protected_progress cap rate held cost funds H).
  destruct (le_dec held (funds + rate)).
  - rewrite Nat.min_l in H0 by assumption. rewrite Nat.min_l in IH by lia.
    eapply Nat.le_trans; [apply Nat.le_min_l|exact IH].
  - rewrite Nat.min_r in H0 by lia.
    eapply Nat.le_trans; [|exact IH]. apply Nat.min_le_compat_l. nia.
Qed.

Theorem retry_retains_turn : forall now delay who entries,
  delay <= 10 ->
  live (now + delay) ((who, now + 16) :: entries) =
    (who, now + 16) :: live (now + delay) entries.
Proof.
  intros. unfold live. simpl.
  assert (now + delay <? now + 16 = true) as E by (apply Nat.ltb_lt; lia).
  now rewrite E.
Qed.

Print Assumptions finite_credit_wait.
Print Assumptions retry_retains_turn.
End Turns.