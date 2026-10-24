(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Record emitter := { used : nat; code : list nat }.

Definition emit cap item state :=
  if used state <? cap then
    Some {| used := S (used state); code := item :: code state |}
  else None.

Fixpoint emit_all cap items state :=
  match items with
  | [] => state
  | item :: rest =>
    match emit cap item state with
    | None => state
    | Some next => emit_all cap rest next
    end
  end.

Lemma emit_consistent : forall cap item state next,
  used state = length (code state) -> emit cap item state = Some next ->
  used next = length (code next) /\ used next <= cap.
Proof.
  intros cap item state next consistent accepted.
  unfold emit in accepted. destruct (used state <? cap) eqn:room; try discriminate.
  apply Nat.ltb_lt in room. inversion accepted; subst; cbn. split; lia.
Qed.

Theorem trace_capacity : forall items cap state,
  used state = length (code state) -> used state <= cap ->
  used (emit_all cap items state) = length (code (emit_all cap items state)) /\
  used (emit_all cap items state) <= cap.
Proof.
  induction items as [|item rest IH]; intros cap state consistent safe; cbn; auto.
  destruct (emit cap item state) as [next|] eqn:step; auto.
  apply IH; apply (emit_consistent _ _ _ _ consistent step).
Qed.

Theorem trace_bytes : forall items cap state,
  used state + length items <= cap ->
  code (emit_all cap items state) = rev items ++ code state /\
  used (emit_all cap items state) = used state + length items.
Proof.
  induction items as [|item rest IH]; intros cap state room; cbn in *.
  - split; [reflexivity|lia].
  - unfold emit. destruct (used state <? cap) eqn:test.
    + specialize (IH cap {| used := S (used state); code := item :: code state |}).
      cbn in IH. destruct IH as [bytes count]; [lia|].
      split; [rewrite bytes, <- app_assoc; reflexivity|lia].
    + apply Nat.ltb_ge in test. lia.
Qed.

Theorem full_trace_stops : forall items cap state,
  used state = cap -> emit_all cap items state = state.
Proof.
  intros items cap state full. destruct items; cbn; auto.
  unfold emit. rewrite full, Nat.ltb_irrefl. reflexivity.
Qed.

Inductive read_result := Absent | Host | Key (blob : nat).
Definition registry := nat -> option nat.
Definition disk := nat -> option nat.

Definition read_key (hash : nat -> nat) (index : registry) (files : disk) addr :=
  match index addr with
  | None => Absent
  | Some digest =>
    match files digest with
    | None => Host
    | Some blob => if hash blob =? digest then Key blob else Host
    end
  end.

Lemma read_has_digest : forall hash index files addr blob,
  read_key hash index files addr = Key blob ->
  exists digest, index addr = Some digest /\ files digest = Some blob /\ hash blob = digest.
Proof.
  intros hash index files addr blob accepted. unfold read_key in accepted.
  destruct (index addr) as [digest|] eqn:entry; try discriminate.
  destruct (files digest) as [bytes|] eqn:file; try discriminate.
  destruct (hash bytes =? digest) eqn:equal; try discriminate.
  apply Nat.eqb_eq in equal. inversion accepted; subst.
  eexists. eauto.
Qed.

Theorem successful_reads_agree : forall hash index left right addr first second,
  (forall x y, hash x = hash y -> x = y) ->
  read_key hash index left addr = Key first ->
  read_key hash index right addr = Key second -> first = second.
Proof.
  intros hash index left right addr first second collision a b.
  apply read_has_digest in a as [ha [ia [fa ea]]].
  apply read_has_digest in b as [hb [ib [fb eb]]].
  rewrite ia in ib. inversion ib; subst. apply collision. congruence.
Qed.

Theorem unrelated_files_ignored : forall hash index left right addr,
  (forall digest, index addr = Some digest -> left digest = right digest) ->
  read_key hash index left addr = read_key hash index right addr.
Proof.
  intros hash index left right addr equal. unfold read_key.
  destruct (index addr) as [digest|] eqn:entry; auto. rewrite equal; auto.
Qed.

Theorem missing_file_is_host : forall hash index files addr digest,
  index addr = Some digest -> files digest = None -> read_key hash index files addr = Host.
Proof.
  intros hash index files addr digest entry missing.
  unfold read_key. rewrite entry, missing. reflexivity.
Qed.

Definition select_rule epoch activation (old fresh : bool) :=
  if epoch <? activation then old else fresh.

Theorem prior_epochs_preserved : forall epoch activation old fresh,
  epoch < activation -> @select_rule epoch activation old fresh = old.
Proof.
  intros epoch activation old fresh before. unfold select_rule.
  apply Nat.ltb_lt in before. rewrite before. reflexivity.
Qed.

Print Assumptions trace_capacity.
Print Assumptions trace_bytes.
Print Assumptions full_trace_stops.
Print Assumptions successful_reads_agree.
Print Assumptions unrelated_files_ignored.
Print Assumptions missing_file_is_host.
Print Assumptions prior_epochs_preserved.

Inductive compile_stage :=
  | Parsed | Imports | Emission (count : nat) | Finished (count : nat) | Refused.

Inductive compile_step (only program : bool) : compile_stage -> compile_stage -> Prop :=
  | Accept_main : only = false \/ program = true -> compile_step only program Parsed Imports
  | Refuse_main : only = true -> program = false -> compile_step only program Parsed Refused
  | Read_import : compile_step only program Imports Imports
  | Start_emit : compile_step only program Imports (Emission 0)
  | Add_instruction : forall n, compile_step only program (Emission n) (Emission (S n))
  | Finish_emit : forall n, compile_step only program (Emission n) (Finished n).

Inductive compile_steps (only program : bool) : nat -> compile_stage -> Prop :=
  | Begin_compile : compile_steps only program 0 Parsed
  | Next_compile : forall n before after, compile_steps only program n before ->
      compile_step only program before after -> compile_steps only program (S n) after.

Definition stage_checked only program stage :=
  match stage with
  | Imports | Emission _ | Finished _ => only = false \/ program = true
  | Parsed | Refused => True
  end.

Lemma step_checked : forall only program before after,
  stage_checked only program before -> compile_step only program before after ->
  stage_checked only program after.
Proof.
  intros only program before after safe step.
  destruct step; cbn in *; auto.
Qed.

Theorem reachable_checked : forall only program n stage,
  compile_steps only program n stage -> stage_checked only program stage.
Proof.
  intros only program n stage steps. induction steps; cbn; auto.
  eapply step_checked; eauto.
Qed.

Theorem rejected_main_no_work : forall n stage,
  compile_steps true false n stage -> stage = Parsed \/ stage = Refused.
Proof.
  intros n stage steps. apply reachable_checked in steps.
  destruct stage; cbn in *; intuition discriminate.
Qed.

Print Assumptions reachable_checked.
Print Assumptions rejected_main_no_work.

Definition first_stop (code : list nat) :=
  match rev code with 0 :: _ => true | _ => false end.

Definition remember_first (code : list nat) saved item :=
  match code with [] => Nat.eqb item 0 | _ => saved end.

Lemma first_stop_step : forall code item saved,
  saved = first_stop code ->
  remember_first code saved item = first_stop (item :: code).
Proof.
  intros code item saved same. subst saved. unfold remember_first, first_stop.
  cbn. destruct code as [|head rest].
  - cbn. destruct item; reflexivity.
  - destruct (rev (head :: rest)) as [|old more] eqn:previous.
    + apply (f_equal (@rev nat)) in previous. rewrite rev_involutive in previous. discriminate.
    + cbn. destruct old; reflexivity.
Qed.

Print Assumptions first_stop_step.