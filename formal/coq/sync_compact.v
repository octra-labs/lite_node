(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Section Paths.
Context {SetId Proof : Type}.
Variable same : SetId -> SetId -> bool.
Hypothesis same_exact : forall x y, same x y = true <-> x = y.
Variable qc : SetId -> Proof -> bool.
Variable state : Proof -> SetId -> bool.

Record link := Link {
  height : nat;
  next : SetId;
  evidence : Proof;
  bytes : nat
}.

Definition checked current prior edge :=
  (prior <? height edge) && qc current (evidence edge) && state (evidence edge) (next edge).

Fixpoint run current prior edges : option (SetId * nat) :=
  match edges with
  | [] => Some (current, prior)
  | edge :: rest => if checked current prior edge
      then run (next edge) (height edge) rest else None
  end.

Inductive path : SetId -> nat -> list link -> SetId -> nat -> Prop :=
| path_nil : forall current prior, path current prior [] current prior
| path_cons : forall current prior edge rest last epoch,
    prior < height edge -> qc current (evidence edge) = true ->
    state (evidence edge) (next edge) = true ->
    path (next edge) (height edge) rest last epoch ->
    path current prior (edge :: rest) last epoch.

Lemma checked_parts : forall current prior edge,
  checked current prior edge = true ->
  prior < height edge /\ qc current (evidence edge) = true /\
  state (evidence edge) (next edge) = true.
Proof.
  intros current prior edge valid. unfold checked in valid.
  repeat rewrite andb_true_iff in valid.
  destruct valid as [[order signed] rooted]. apply Nat.ltb_lt in order. auto.
Qed.

Theorem run_path : forall edges current prior last epoch,
  run current prior edges = Some (last, epoch) -> path current prior edges last epoch.
Proof.
  induction edges as [|edge rest IH]; intros current prior last epoch valid.
  - simpl in valid. inversion valid; subst. constructor.
  - simpl in valid. destruct (checked current prior edge) eqn:ok; try discriminate.
    apply checked_parts in ok as [order [signed rooted]].
    econstructor; eauto.
Qed.

Lemma path_run : forall edges current prior last epoch,
  path current prior edges last epoch -> run current prior edges = Some (last, epoch).
Proof.
  intros edges current prior last epoch valid. induction valid.
  - reflexivity.
  - simpl. unfold checked. apply Nat.ltb_lt in H.
    rewrite H, H0, H1. exact IHvalid.
Qed.

Lemma run_app : forall first rest current prior middle epoch,
  run current prior first = Some (middle, epoch) ->
  run current prior (first ++ rest) = run middle epoch rest.
Proof.
  induction first as [|edge xs IH]; intros rest current prior middle epoch valid.
  - simpl in valid. inversion valid; subst. reflexivity.
  - simpl in *. destruct (checked current prior edge); try discriminate. eapply IH; eauto.
Qed.

Theorem replace_prefix : forall trusted start old short rest middle epoch early,
  run trusted start old = Some (middle, epoch) ->
  run trusted start short = Some (middle, early) ->
  forall last finish, run middle early rest = Some (last, finish) ->
  path trusted start (short ++ rest) last finish.
Proof.
  intros trusted start old short rest middle epoch early old_ok short_ok last finish rest_ok.
  apply run_path. rewrite (run_app short rest trusted start middle early short_ok). exact rest_ok.
Qed.

Theorem checked_bridge : forall trusted start prefix current prior edge wanted,
  run trusted start prefix = Some (current, prior) ->
  checked current prior edge = true -> same (next edge) wanted = true ->
  run trusted start (prefix ++ [edge]) = Some (wanted, height edge).
Proof.
  intros trusted start prefix current prior edge wanted prefix_ok edge_ok endpoint.
  apply same_exact in endpoint. rewrite (run_app prefix [edge] trusted start current prior prefix_ok).
  simpl. rewrite edge_ok, endpoint. reflexivity.
Qed.

Theorem no_unsigned_bridge : forall trusted start prefix current prior edge,
  run trusted start prefix = Some (current, prior) ->
  qc current (evidence edge) = false -> run trusted start (prefix ++ [edge]) = None.
Proof.
  intros trusted start prefix current prior edge prefix_ok unsigned.
  rewrite (run_app prefix [edge] trusted start current prior prefix_ok).
  simpl. unfold checked. rewrite unsigned, andb_false_r. reflexivity.
Qed.

Fixpoint cost edges := match edges with [] => 0 | edge :: rest => bytes edge + cost rest end.

Variable view : list link -> SetId -> SetId.

Definition select trusted start wanted old short :=
  match run trusted start old with
  | None => None
  | Some (ending, _) =>
    if same (view old ending) wanted then
      match run trusted start short with
      | Some (ending, _) =>
        if same (view short ending) wanted && (cost short <? cost old) then Some short else Some old
      | None => Some old
      end
    else None
  end.

Theorem select_path : forall trusted start wanted old short chosen,
  select trusted start wanted old short = Some chosen ->
  (exists ending epoch, path trusted start chosen ending epoch /\ view chosen ending = wanted)
  /\ cost chosen <= cost old.
Proof.
  intros trusted start wanted old short chosen valid. unfold select in valid.
  destruct (run trusted start old) as [[last epoch]|] eqn:old_ok; try discriminate.
  destruct (same (view old last) wanted) eqn:old_end; try discriminate.
  apply same_exact in old_end.
  destruct (run trusted start short) as [[small_last small_epoch]|] eqn:short_ok.
  - destruct (same (view short small_last) wanted && (cost short <? cost old)) eqn:better.
    + inversion valid; subst. apply andb_true_iff in better as [ending size].
      apply same_exact in ending. apply Nat.ltb_lt in size.
      split; [exists small_last, small_epoch; split; [eapply run_path; eauto|auto]|lia].
    + inversion valid; subst. split; [exists last, epoch; split; [eapply run_path; eauto|auto]|lia].
  - inversion valid; subst. split; [exists last, epoch; split; [eapply run_path; eauto|auto]|lia].
Qed.

Theorem bad_input_not_hidden : forall trusted start wanted old short,
  run trusted start old = None -> select trusted start wanted old short = None.
Proof. intros. unfold select. rewrite H. reflexivity. Qed.

End Paths.

Section Retained.
Context {Edge : Type}.
Variable eq_edge : forall x y : Edge, {x = y} + {x <> y}.

Definition frontier required edge prefix paths :=
  if eq_edge edge required then [prefix ++ [edge]]
  else (prefix ++ [edge]) :: paths.

Definition carries (required : Edge) paths :=
  Forall (fun path => In required path) paths.

Theorem retain_mark : forall required prefix paths,
  carries required (frontier required required prefix paths).
Proof.
  intros. unfold frontier, carries.
  destruct (eq_edge required required); [|contradiction].
  constructor; [apply in_or_app; right; simpl; auto|constructor].
Qed.

Theorem retain_step : forall required edge prefix paths,
  carries required paths -> In prefix paths ->
  carries required (frontier required edge prefix paths).
Proof.
  intros required edge prefix paths kept member.
  unfold frontier. destruct (eq_edge edge required) as [same|different].
  - subst. unfold carries. constructor.
    + apply in_or_app. right. simpl. auto.
    + constructor.
  - unfold carries in *. constructor; [|exact kept].
    apply in_or_app. left. rewrite Forall_forall in kept. apply kept. exact member.
Qed.

Theorem selected_mark : forall required paths selected,
  carries required paths -> In selected paths -> In required selected.
Proof.
  intros required paths selected kept member.
  unfold carries in kept. rewrite Forall_forall in kept. auto.
Qed.

End Retained.

Section Votes.
Context {Vote : Type}.
Variable member : Vote -> bool.
Variable signed : Vote -> Prop.

Theorem retained_votes_signed : forall votes,
  Forall signed votes -> Forall signed (filter member votes).
Proof.
  intros votes valid. induction valid; simpl; [constructor|].
  destruct (member x); auto.
Qed.

Theorem retained_votes_members : forall votes,
  Forall (fun vote => member vote = true) (filter member votes).
Proof.
  induction votes; simpl; [constructor|].
  destruct (member a) eqn:yes; auto.
Qed.

End Votes.

Fixpoint visits steps := match steps with 0 => 0 | S n => S n + visits n end.
Theorem search_work : forall steps, 2 * visits steps = steps * (steps + 1).
Proof. induction steps; simpl; nia. Qed.

Definition wire_bytes bytes := 4 * ((bytes + 2) / 3).
Theorem wire_monotone : forall short old, short <= old -> wire_bytes short <= wire_bytes old.
Proof.
  intros short old smaller. unfold wire_bytes.
  apply Nat.mul_le_mono_l. apply Nat.div_le_mono; lia.
Qed.

Theorem wire_budget : forall short old limit,
  short <= old -> wire_bytes old <= limit -> wire_bytes short <= limit.
Proof. intros. eapply Nat.le_trans; [apply wire_monotone; eauto|eauto]. Qed.

Print Assumptions select_path.
Print Assumptions checked_bridge.
Print Assumptions no_unsigned_bridge.
Print Assumptions retained_votes_signed.
Print Assumptions search_work.
Print Assumptions wire_budget.
Print Assumptions retain_mark.
Print Assumptions retain_step.
Print Assumptions selected_mark.