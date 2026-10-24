(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Bool Lia.
Import ListNotations.

Inductive node := Leaf (value : nat) | Pair (left right : nat).
Inductive tree := Tip (value : nat) | Fork (left right : tree).
Definition cache := list (nat * tree).

Fixpoint lookup id (saved : cache) :=
  match saved with
  | [] => None
  | (key, value) :: rest => if Nat.eqb id key then Some value else lookup id rest
  end.

Fixpoint evaluate fuel (graph : nat -> node) saved id :=
  match fuel with
  | 0 => None
  | S fuel =>
    match lookup id saved with
    | Some value => Some (value, saved)
    | None =>
      match graph id with
      | Leaf value => Some (Tip value, (id, Tip value) :: saved)
      | Pair a b =>
        match evaluate fuel graph saved a with
        | None => None
        | Some (l, first) =>
          match evaluate fuel graph first b with
          | None => None
          | Some (r, second) => Some (Fork l r, (id, Fork l r) :: second)
          end
        end
      end
    end
  end.

Definition valid meaning saved :=
  forall id value, lookup id saved = Some value -> value = meaning id.

Definition denotes graph meaning := forall id,
  meaning id = match graph id with
    | Leaf value => Tip value
    | Pair a b => Fork (meaning a) (meaning b)
    end.

Lemma store_valid : forall meaning saved id value,
  valid meaning saved -> value = meaning id -> valid meaning ((id, value) :: saved).
Proof.
  intros meaning saved id value H E key item read. simpl in read.
  destruct (Nat.eqb key id) eqn:equal.
  - apply Nat.eqb_eq in equal. inversion read; subst; reflexivity.
  - now apply H.
Qed.

Theorem shared_graph_preserves_tree : forall fuel graph meaning saved id value next,
  denotes graph meaning -> valid meaning saved ->
  evaluate fuel graph saved id = Some (value, next) ->
  value = meaning id /\ valid meaning next.
Proof.
  induction fuel as [|fuel IH]; intros graph meaning saved id value next shape sound run;
    simpl in run; [discriminate|].
  destruct (lookup id saved) as [found|] eqn:read.
  - inversion run; subst. split; auto.
  - destruct (graph id) as [leaf|left right] eqn:part.
    + inversion run; subst. assert (Tip leaf = meaning id) as E.
      { specialize (shape id). now rewrite part in shape. }
      split; auto. now apply store_valid.
    + destruct (evaluate fuel graph saved left) as [[l first]|] eqn:first_run; [|discriminate].
      destruct (evaluate fuel graph first right) as [[r second]|] eqn:second_run; [|discriminate].
      destruct (IH graph meaning saved left l first shape sound first_run) as [L first_valid].
      destruct (IH graph meaning first right r second shape first_valid second_run) as [R second_valid].
      inversion run; subst. assert (Fork (meaning left) (meaning right) = meaning id) as E.
      { specialize (shape id). now rewrite part in shape. }
      split; auto. now apply store_valid.
Qed.

Theorem repeated_edge_is_cached : forall fuel graph saved id value next,
  evaluate fuel graph saved id = Some (value, next) -> lookup id next = Some value.
Proof.
  intros fuel graph saved id value next run.
  destruct fuel; simpl in run; [discriminate|].
  destruct (lookup id saved) eqn:read.
  - inversion run; subst; assumption.
  - destruct (graph id).
    + inversion run; subst; simpl; now rewrite Nat.eqb_refl.
    + destruct (evaluate fuel graph saved left) as [[l first]|]; [|discriminate].
      destruct (evaluate fuel graph first right) as [[r second]|]; [|discriminate].
      inversion run; subst; simpl; now rewrite Nat.eqb_refl.
Qed.

Theorem second_edge_does_no_expansion : forall fuel graph saved id value next,
  evaluate fuel graph saved id = Some (value, next) ->
  evaluate 1 graph next id = Some (value, next).
Proof.
  intros. pose proof (repeated_edge_is_cached _ _ _ _ _ _ H) as read.
  simpl. now rewrite read.
Qed.

Definition pair_relation (meaning : nat -> tree) pair :=
  meaning (fst pair) = meaning (snd pair).

Theorem pair_decomposition : forall meaning la lb ra rb left right,
  meaning left = Fork (meaning la) (meaning lb) ->
  meaning right = Fork (meaning ra) (meaning rb) ->
  pair_relation meaning (left, right) <->
    pair_relation meaning (la, ra) /\ pair_relation meaning (lb, rb).
Proof.
  intros meaning la lb ra rb left right L R.
  unfold pair_relation; simpl. rewrite L, R. split.
  - intros E; inversion E; auto.
  - intros [A B]; now rewrite A, B.
Qed.

Theorem ordered_pair_reuse : forall meaning saved pair,
  Forall (pair_relation meaning) saved -> In pair saved -> pair_relation meaning pair.
Proof. intros meaning saved pair H member. rewrite Forall_forall in H. now apply H. Qed.

Print Assumptions shared_graph_preserves_tree.
Print Assumptions second_edge_does_no_expansion.
Print Assumptions pair_decomposition.

Fixpoint locals fuel graph saved ids :=
  match ids with
  | [] => Some ([], saved)
  | id :: rest =>
    match evaluate fuel graph saved id with
    | None => None
    | Some (value, next) =>
      match locals fuel graph next rest with
      | None => None
      | Some (values, last) => Some (value :: values, last)
      end
    end
  end.

Theorem locals_share_sound_cache : forall ids fuel graph meaning saved values last,
  denotes graph meaning -> valid meaning saved ->
  locals fuel graph saved ids = Some (values, last) ->
  values = map meaning ids /\ valid meaning last.
Proof.
  induction ids as [|id rest IH]; intros fuel graph meaning saved values last shape sound run;
    simpl in run.
  - inversion run; subst; auto.
  - destruct (evaluate fuel graph saved id) as [[value next]|] eqn:first; [|discriminate].
    destruct (locals fuel graph next rest) as [[items final]|] eqn:more; [|discriminate].
    destruct (shared_graph_preserves_tree _ _ _ _ _ _ _ shape sound first) as [equal cache_ok].
    destruct (IH fuel graph meaning next items final shape cache_ok more) as [rest_equal last_ok].
    inversion run; subst. split; [reflexivity|assumption].
Qed.

Theorem copied_local_uses_saved_node : forall fuel graph saved id value next,
  evaluate fuel graph saved id = Some (value, next) ->
  locals 1 graph next [id; id] = Some ([value; value], next).
Proof.
  intros. pose proof (second_edge_does_no_expansion _ _ _ _ _ _ H) as E.
  cbn [locals]. rewrite E. cbn [locals]. rewrite E. reflexivity.
Qed.

Print Assumptions locals_share_sound_cache.
Print Assumptions copied_local_uses_saved_node.

Fixpoint visits fuel graph saved id :=
  match fuel with
  | 0 => 0
  | S fuel =>
    match lookup id saved with
    | Some _ => 0
    | None => match graph id with
      | Leaf _ => 1
      | Pair a b => 1 + visits fuel graph saved a + visits fuel graph saved b
      end
    end
  end.

Theorem saved_branch_is_pruned : forall fuel graph saved id value,
  lookup id saved = Some value -> visits fuel graph saved id = 0.
Proof. intros [|fuel] graph saved id value read; simpl; [reflexivity|now rewrite read]. Qed.

Theorem new_pair_constant_work : forall fuel graph saved id left right l r,
  lookup id saved = None -> graph id = Pair left right ->
  lookup left saved = Some l -> lookup right saved = Some r ->
  visits (S fuel) graph saved id = 1.
Proof.
  intros. simpl. rewrite H, H0.
  rewrite (saved_branch_is_pruned fuel graph saved left l H1).
  rewrite (saved_branch_is_pruned fuel graph saved right r H2). reflexivity.
Qed.

Print Assumptions new_pair_constant_work.

Definition referenced ids (saved : cache) :=
  filter (fun item => existsb (Nat.eqb (fst item)) ids) saved.

Theorem referenced_lookup : forall saved ids id,
  In id ids -> lookup id (referenced ids saved) = lookup id saved.
Proof.
  induction saved as [|[key value] rest IH]; intros ids id present; simpl; [reflexivity|].
  unfold referenced in *. simpl.
  destruct (Nat.eqb id key) eqn:E.
  - apply Nat.eqb_eq in E. subst key.
    assert (existsb (Nat.eqb id) ids = true) as seen.
    { apply existsb_exists. exists id. split; [assumption|apply Nat.eqb_refl]. }
    rewrite seen. simpl. now rewrite Nat.eqb_refl.
  - destruct (existsb (Nat.eqb key) ids); simpl; [rewrite E|]; now apply IH.
Qed.

Theorem referenced_results : forall saved ids,
  map (fun id => lookup id (referenced ids saved)) ids = map (fun id => lookup id saved) ids.
Proof.
  intros. apply map_ext_in. intros id present. now apply referenced_lookup.
Qed.

Print Assumptions referenced_results.

Module ShortCircuit.

Inductive tests := Result (ok : bool) | Both (left right : tests).

Fixpoint eager (value : tests) : bool * nat :=
  match value with
  | Result ok => (ok, 1)
  | Both lhs rhs =>
    let '(a, x) := eager lhs in
    let '(b, y) := eager rhs in (a && b, 1 + x + y)
  end.

Fixpoint ordered (value : tests) : bool * nat :=
  match value with
  | Result ok => (ok, 1)
  | Both lhs rhs =>
    let '(a, x) := ordered lhs in
    if a then let '(b, y) := ordered rhs in (b, 1 + x + y)
    else (false, 1 + x)
  end.

Theorem same_decision_less_work : forall value,
  fst (ordered value) = fst (eager value) /\ snd (ordered value) <= snd (eager value).
Proof.
  induction value as [ok|left [L LC] right [R RC]]; simpl; [auto|].
  destruct (ordered left) as [a x], (eager left) as [c z].
  destruct (ordered right) as [b y], (eager right) as [d w].
  simpl in *. subst. destruct c; simpl; split; auto; lia.
Qed.

Theorem first_refusal_prunes : forall left right work,
  ordered left = (false, work) -> ordered (Both left right) = (false, S work).
Proof. intros. simpl. now rewrite H. Qed.

Theorem first_refusal_saved_work : forall left right work,
  ordered left = (false, work) ->
  snd (ordered (Both left right)) + snd (eager right) <= snd (eager (Both left right)).
Proof.
  intros left right work failed.
  pose proof (same_decision_less_work left) as [_ cost].
  rewrite failed in cost. rewrite (first_refusal_prunes left right work failed).
  simpl. destruct (eager left), (eager right). simpl in *. lia.
Qed.

End ShortCircuit.

Print Assumptions ShortCircuit.same_decision_less_work.
Print Assumptions ShortCircuit.first_refusal_saved_work.