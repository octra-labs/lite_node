(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia Bool.
Import ListNotations.

Record budget := Budget { effort : nat; remaining : nat; available : nat }.

Definition plan value limit cost bytes allocated :=
  if (effort value + cost <=? limit) && (bytes <=? remaining value)
    && (allocated <=? available value)
  then Some (Budget (effort value + cost) (remaining value - bytes)
    (available value - allocated))
  else None.

Inductive event := Work (cost bytes allocated : nat) | Rollback | Refused.

Definition step value limit input :=
  match input with
  | Work cost bytes allocated =>
      match plan value limit cost bytes allocated with
      | Some next => next
      | None => value
      end
  | Rollback | Refused => value
  end.

Definition run value limit inputs :=
  fold_left (fun value input => step value limit input) inputs value.

Theorem plan_cost : forall value limit cost bytes allocated next,
  plan value limit cost bytes allocated = Some next ->
  effort next = effort value + cost /\
  remaining next + bytes = remaining value /\
  available next + allocated = available value /\
  effort next <= limit.
Proof.
  intros [used free space] limit cost bytes allocated next accepted.
  unfold plan in accepted. simpl in accepted.
  destruct (((used + cost <=? limit) && (bytes <=? free))
    && (allocated <=? space)) eqn:checks;
    [|discriminate].
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[paid fits] reserved].
  apply Nat.leb_le in reserved.
  apply Nat.leb_le in paid. apply Nat.leb_le in fits.
  inversion accepted; subst. simpl. lia.
Qed.

Module DecodeCache.
Section Cache.
Context {K V : Type}.
Variable equal : forall x y : K, {x = y} + {x <> y}.
Variable decode : K -> option V.
Variable charge : K -> V -> nat.
Variable slots bytes : nat.

Record entry := Entry { key : K; value : V; size : nat }.

Fixpoint used (entries : list entry) :=
  match entries with [] => 0 | item :: rest => size item + used rest end.

Fixpoint take count room entries :=
  match count, entries with
  | S count, item :: rest =>
    if size item <=? room
    then item :: take count (room - size item) rest
    else []
  | _, _ => []
  end.

Definition matches id item := if equal (key item) id then true else false.
Definition keep item entries :=
  take slots bytes (item :: filter (fun old => negb (matches (key item) old)) entries).
Definition valid entries := Forall (fun item => decode (key item) = Some (value item)) entries.

Definition load id entries :=
  match find (matches id) entries with
  | Some item => (keep item entries, Some (value item))
  | None =>
    match decode id with
    | None => (entries, None)
    | Some result => (keep (Entry id result (charge id result)) entries, Some result)
    end
  end.

Lemma take_limits : forall count room entries,
  length (take count room entries) <= count /\ used (take count room entries) <= room.
Proof.
  induction count as [|count induction]; intros room entries; [simpl; lia|].
  destruct entries as [|item rest]; [simpl; lia|].
  simpl. destruct (size item <=? room) eqn:fits; [|simpl; lia].
  apply Nat.leb_le in fits.
  specialize (induction (room - size item) rest). simpl. lia.
Qed.

Lemma take_valid : forall entries count room,
  valid entries -> valid (take count room entries).
Proof.
  intros entries count. revert entries.
  induction count as [|count induction]; intros entries room checked; [constructor|].
  destruct entries as [|item rest]; [constructor|].
  simpl. destruct (size item <=? room); [|constructor].
  inversion checked; subst. constructor; [assumption|].
  apply induction. assumption.
Qed.

Lemma keep_valid : forall item entries,
  decode (key item) = Some (value item) -> valid entries -> valid (keep item entries).
Proof.
  intros item entries checked valid_entries. unfold keep. apply take_valid.
  constructor; [assumption|].
  unfold valid in *. rewrite Forall_forall in *.
  intros old present. apply filter_In in present. apply valid_entries. tauto.
Qed.

Lemma found_valid : forall id entries item,
  valid entries -> find (matches id) entries = Some item ->
  decode id = Some (value item).
Proof.
  intros id entries item checked found.
  apply find_some in found. destruct found as [present same].
  unfold matches in same. destruct (equal (key item) id) as [same_key|]; [|discriminate].
  unfold valid in checked. rewrite Forall_forall in checked.
  rewrite <- same_key. apply checked. assumption.
Qed.

Theorem load_correct : forall id entries,
  valid entries -> snd (load id entries) = decode id /\ valid (fst (load id entries)).
Proof.
  intros id entries checked. unfold load.
  destruct (find (matches id) entries) as [item|] eqn:found.
  - pose proof (found_valid id entries item checked found) as result.
    split; [simpl; symmetry; assumption|].
    simpl. apply keep_valid; [|assumption].
    apply find_some in found. unfold valid in checked. rewrite Forall_forall in checked.
    apply checked. tauto.
  - destruct (decode id) as [result|] eqn:parsed; simpl.
    + split; [reflexivity|]. apply keep_valid; assumption.
    + split; [reflexivity|assumption].
Qed.

Theorem load_limits : forall id entries,
  length entries <= slots -> used entries <= bytes ->
  length (fst (load id entries)) <= slots /\ used (fst (load id entries)) <= bytes.
Proof.
  intros id entries count room. unfold load.
  destruct (find (matches id) entries); simpl.
  - apply take_limits.
  - destruct (decode id); simpl; [apply take_limits|tauto].
Qed.

Fixpoint run keys entries :=
  match keys with
  | [] => entries
  | id :: rest => run rest (fst (load id entries))
  end.

Theorem trace_valid : forall keys entries,
  valid entries -> length entries <= slots -> used entries <= bytes ->
  valid (run keys entries) /\ length (run keys entries) <= slots /\ used (run keys entries) <= bytes.
Proof.
  induction keys as [|id rest induction]; intros entries checked count room.
  - simpl. tauto.
  - simpl. apply induction.
    + apply (proj2 (load_correct id entries checked)).
    + apply (proj1 (load_limits id entries count room)).
    + apply (proj2 (load_limits id entries count room)).
Qed.
End Cache.
End DecodeCache.

Theorem plan_refused : forall value limit cost bytes allocated,
  limit < effort value + cost \/ remaining value < bytes \/ available value < allocated ->
  plan value limit cost bytes allocated = None.
Proof.
  intros [used free space] limit cost bytes allocated invalid.
  unfold plan. simpl in *.
  destruct (((used + cost <=? limit) && (bytes <=? free))
    && (allocated <=? space)) eqn:checks;
    [|reflexivity].
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[paid fits] reserved].
  apply Nat.leb_le in reserved.
  apply Nat.leb_le in paid. apply Nat.leb_le in fits. lia.
Qed.

Theorem step_spent : forall value limit input,
  effort value <= effort (step value limit input) /\
  remaining (step value limit input) <= remaining value /\
  available (step value limit input) <= available value.
Proof.
  intros value limit input. destruct input; simpl; try lia.
  destruct (plan value limit cost bytes allocated) as [next|] eqn:accepted; [|lia].
  apply plan_cost in accepted. destruct accepted as [paid [spent [reserved _]]]. lia.
Qed.

Theorem trace_spent : forall inputs value limit,
  effort value <= effort (run value limit inputs) /\
  remaining (run value limit inputs) <= remaining value /\
  available (run value limit inputs) <= available value.
Proof.
  induction inputs as [|input rest induction]; intros value limit.
  - simpl. lia.
  - change (effort value <= effort (run (step value limit input) limit rest) /\
      remaining (run (step value limit input) limit rest) <= remaining value /\
      available (run (step value limit input) limit rest) <= available value).
    pose proof (step_spent value limit input).
    pose proof (induction (step value limit input) limit). lia.
Qed.

Theorem rollback_keeps_work : forall value limit,
  step value limit Rollback = value.
Proof. reflexivity. Qed.

Theorem replay_cannot_refill : forall inputs value limit cost bytes allocated next,
  plan value limit cost bytes allocated = Some next ->
  remaining (run next limit (Rollback :: inputs)) + bytes <= remaining value /\
  available (run next limit (Rollback :: inputs)) + allocated <= available value.
Proof.
  intros inputs value limit cost bytes allocated next accepted.
  apply plan_cost in accepted. destruct accepted as [_ [spent [reserved _]]].
  pose proof (trace_spent (Rollback :: inputs) next limit). lia.
Qed.

Definition publish {A : Type} value limit cost bytes allocated (writes : list A) :=
  match plan value limit cost bytes allocated with
  | Some next => (next, writes)
  | None => (value, [])
  end.

Theorem refusal_no_writes : forall (A : Type) value limit cost bytes allocated (writes : list A),
  plan value limit cost bytes allocated = None ->
  publish value limit cost bytes allocated writes = (value, []).
Proof. intros. unfold publish. rewrite H. reflexivity. Qed.

Theorem complete_write_plan : forall (A : Type) value limit cost bytes allocated
  (writes : list A) next,
  plan value limit cost bytes allocated = Some next ->
  publish value limit cost bytes allocated writes = (next, writes).
Proof. intros. unfold publish. rewrite H. reflexivity. Qed.

Theorem reserve_opcode : forall value limit parsing opcode bytes allocated checked,
  plan value limit (parsing + opcode) bytes allocated = Some checked ->
  exists reserved,
    plan value limit parsing bytes allocated = Some reserved /\
    plan reserved limit opcode 0 0 = Some checked.
Proof.
  intros [used free space] limit parsing opcode bytes allocated checked accepted.
  unfold plan in accepted. simpl in accepted.
  destruct (((used + (parsing + opcode) <=? limit) && (bytes <=? free))
    && (allocated <=? space)) eqn:checks; [|discriminate].
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[paid fits] memory].
  apply Nat.leb_le in paid.
  assert (first : (used + parsing <=? limit) = true) by
    (apply Nat.leb_le; lia).
  assert (second : (used + parsing + opcode <=? limit) = true) by
    (apply Nat.leb_le; lia).
  inversion accepted; subst.
  exists (Budget (used + parsing) (free - bytes) (space - allocated)).
  split.
  - unfold plan. simpl. rewrite first, fits, memory. reflexivity.
  - unfold plan. simpl. rewrite second. simpl.
    repeat rewrite Nat.sub_0_r.
    replace (used + parsing + opcode) with (used + (parsing + opcode)) by lia.
    reflexivity.
Qed.