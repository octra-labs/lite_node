(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Record state := State { next : nat; active : list nat }.
Inductive message := Start | Finish (id : nat) | Cancel (id : nat).

Definition step s message :=
  match message with
  | Start => if length (active s) <? 2
      then State (S (next s)) (next s :: active s) else s
  | Finish id => State (next s) (filter (fun value => negb (value =? id)) (active s))
  | Cancel _ => s
  end.

Definition valid s := length (active s) <= 2 /\
  Forall (fun id => id < next s) (active s).

Lemma step_valid : forall s message, valid s -> valid (step s message).
Proof.
  intros [fresh tasks] message [capacity ids]. destruct message.
  - change (valid (if length tasks <? 2
      then State (S fresh) (fresh :: tasks) else State fresh tasks)).
    destruct (length tasks <? 2) eqn:room.
    + unfold valid; cbn. split.
      * apply Nat.ltb_lt in room. lia.
      * constructor; [lia|]. eapply Forall_impl; [|exact ids]. cbn. intros; lia.
    + split; assumption.
  - unfold valid; cbn. split.
    + eapply Nat.le_trans; [apply filter_length_le|exact capacity].
    + apply Forall_forall. intros value present. apply filter_In in present as [present _].
      rewrite Forall_forall in ids. auto.
  - split; assumption.
Qed.

Theorem trace_capacity : forall messages,
  valid (fold_left step messages (State 0 [])).
Proof.
  assert (forall messages s, valid s -> valid (fold_left step messages s)) as trace.
  { induction messages; cbn; auto. intros. apply IHmessages, step_valid; auto. }
  intros. apply trace. split; cbn; auto.
Qed.

Theorem reserved_workers : forall messages,
  2 <= 4 - length (active (fold_left step messages (State 0 []))).
Proof.
  intros. pose proof (trace_capacity messages) as [capacity _]. lia.
Qed.

Theorem cancel_cannot_enable_start : forall s id,
  length (active s) = 2 -> step (step s (Cancel id)) Start = s.
Proof.
  intros s id full. cbn [step]. rewrite full. reflexivity.
Qed.

Theorem late_completion_keeps_new : forall s old fresh,
  old <> fresh -> In fresh (active s) -> In fresh (active (step s (Finish old))).
Proof.
  intros s old fresh different present. cbn. apply filter_In. split; auto.
  apply Bool.negb_true_iff, Nat.eqb_neq. auto.
Qed.

Theorem new_slot_is_distinct : forall s,
  valid s -> ~ In (next s) (active s).
Proof.
  intros s [_ ids] present. rewrite Forall_forall in ids.
  specialize (ids _ present). lia.
Qed.

Print Assumptions trace_capacity.
Print Assumptions reserved_workers.
Print Assumptions cancel_cannot_enable_start.
Print Assumptions late_completion_keeps_new.
Print Assumptions new_slot_is_distinct.

Record lanes := Lanes { heavy : state; view_busy : bool }.
Inductive event := Heavy (input : message) | ViewStart | ViewFinish | ViewCancel.

Definition advance s input :=
  match input with
  | Heavy request => Lanes (step (heavy s) request) (view_busy s)
  | ViewStart => Lanes (heavy s) true
  | ViewFinish => Lanes (heavy s) false
  | ViewCancel => s
  end.

Definition used s := length (active (heavy s)) + if view_busy s then 1 else 0.

Lemma advance_valid : forall s input,
  valid (heavy s) -> valid (heavy (advance s input)).
Proof.
  intros [work busy] input checked. destruct input; cbn; auto using step_valid.
Qed.

Theorem lane_capacity : forall events,
  used (fold_left advance events (Lanes (State 0 []) false)) <= 3.
Proof.
  assert (forall events s, valid (heavy s) ->
    valid (heavy (fold_left advance events s))) as checked.
  { induction events; cbn; auto. intros. apply IHevents, advance_valid; auto. }
  intro events. specialize (checked events (Lanes (State 0 []) false)).
  assert (valid (State 0 [])) as initial by (split; cbn; auto).
  specialize (checked initial). destruct checked as [capacity _].
  unfold used. destruct (view_busy _); lia.
Qed.

Theorem epoch_worker_available : forall events,
  1 <= 4 - used (fold_left advance events (Lanes (State 0 []) false)).
Proof. intro events. pose proof (lane_capacity events). lia. Qed.

Theorem heavy_cannot_close_view : forall requests s,
  view_busy (fold_left advance (map Heavy requests) s) = view_busy s.
Proof.
  induction requests; cbn; auto. intro s. rewrite IHrequests. reflexivity.
Qed.

Theorem view_cannot_consume_heavy : forall events s,
  heavy (fold_left advance events s) =
    fold_left step
      (fold_right (fun input rest => match input with
        | Heavy request => request :: rest | _ => rest end) [] events) (heavy s).
Proof.
  induction events as [|input rest next]; cbn; auto. intro s.
  destruct input; cbn; rewrite next; reflexivity.
Qed.

Theorem canceled_view_stays_busy : forall requests s,
  view_busy s = true ->
  view_busy (advance (fold_left advance (map Heavy requests) s) ViewCancel) = true.
Proof. intros requests s busy. cbn. rewrite heavy_cannot_close_view. exact busy. Qed.

Print Assumptions lane_capacity.
Print Assumptions epoch_worker_available.
Print Assumptions heavy_cannot_close_view.
Print Assumptions view_cannot_consume_heavy.
Print Assumptions canceled_view_stays_busy.