(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Definition env := list (nat * nat).
Definition memory := nat -> nat.

Fixpoint lookup (name : nat) (locals : env) : option nat :=
  match locals with
  | [] => None
  | (key, reg) :: rest => if name =? key then Some reg else lookup name rest
  end.

Definition placed base (locals : env) := Forall (fun item => snd item < base) locals.

Definition assign name value locals (cells : memory) : option memory :=
  match lookup name locals with
  | None => None
  | Some reg => Some (fun index => if index =? reg then value else cells index)
  end.

Lemma lookup_placed : forall locals base name reg,
  placed base locals -> lookup name locals = Some reg -> reg < base.
Proof.
  induction locals as [|[key slot] rest IH]; intros base name reg valid found.
  - discriminate.
  - inversion valid as [|item more first remaining]; subst.
    cbn in found. destruct (name =? key).
    + inversion found; subst. exact first.
    + eapply IH; eauto.
Qed.

Theorem outer_write_preserves_fresh : forall locals base name value cells next index,
  placed base locals -> base <= index ->
  assign name value locals cells = Some next -> next index = cells index.
Proof.
  intros locals base name value cells next index valid fresh written.
  unfold assign in written.
  destruct (lookup name locals) as [reg|] eqn:found; try discriminate.
  pose proof (lookup_placed _ _ _ _ valid found) as lower.
  inversion written; subst. cbn.
  destruct (index =? reg) eqn:same; auto.
  apply Nat.eqb_eq in same. lia.
Qed.

Fixpoint leave_scopes (saved : list env) (locals : env) : env :=
  match saved with
  | [] => locals
  | frame :: rest => leave_scopes rest frame
  end.

Lemma outer_frame_restored : forall frames outer inner,
  leave_scopes (frames ++ [outer]) inner = outer.
Proof.
  induction frames as [|frame rest IH]; intros outer inner; cbn; auto.
Qed.

Theorem nested_scope_isolation : forall frames outer inner base name value cells next index,
  placed base outer -> base <= index ->
  assign name value (leave_scopes (frames ++ [outer]) inner) cells = Some next ->
  next index = cells index.
Proof.
  intros frames outer inner base name value cells next index valid fresh written.
  rewrite outer_frame_restored in written.
  eapply outer_write_preserves_fresh; eauto.
Qed.

Example escaped_name_overwrites_local :
  match assign 1 9 [(1, 3); (1, 0)] (fun _ => 7) with
  | Some cells => cells 3
  | None => 0
  end = 9.
Proof. reflexivity. Qed.

Example restored_name_preserves_local :
  match assign 1 9 (leave_scopes [[(1, 0)]] [(1, 3); (1, 0)]) (fun _ => 7) with
  | Some cells => cells 3
  | None => 0
  end = 7.
Proof. reflexivity. Qed.

Definition code := list nat.

Definition eligible (scoped : list code) (image : code) :=
  existsb (fun checked => if list_eq_dec Nat.eq_dec image checked then true else false) scoped.

Lemma eligible_image : forall scoped image,
  eligible scoped image = true -> In image scoped.
Proof.
  intros scoped image accepted.
  apply existsb_exists in accepted as [checked [member equal]].
  destruct (list_eq_dec Nat.eq_dec image checked); subst; auto; discriminate.
Qed.

Theorem mismatched_images_refused : forall scoped image,
  (forall checked, In checked scoped -> image <> checked) -> eligible scoped image = false.
Proof.
  intros scoped image distinct.
  destruct (eligible scoped image) eqn:accepted; auto.
  apply eligible_image in accepted. exfalso. eapply distinct; eauto.
Qed.

Print Assumptions lookup_placed.
Print Assumptions outer_write_preserves_fresh.
Print Assumptions nested_scope_isolation.
Print Assumptions mismatched_images_refused.

Definition write_cell (cells : memory) (item : nat * nat) : memory :=
  fun index => if index =? fst item then snd item else cells index.

Fixpoint write_cells (items : list (nat * nat)) (cells : memory) : memory :=
  match items with
  | [] => cells
  | item :: rest => write_cells rest (write_cell cells item)
  end.

Lemma reserved_cell_preserved : forall items base index cells,
  index < base -> Forall (fun item => base <= fst item) items ->
  write_cells items cells index = cells index.
Proof.
  induction items as [|[slot value] rest IH]; intros base index cells reserved valid; cbn; auto.
  inversion valid; subst. rewrite (IH base index); auto.
  unfold write_cell; cbn. destruct (index =? slot) eqn:same; auto.
  apply Nat.eqb_eq in same. cbn in *. lia.
Qed.

Fixpoint dynamic_loop limit bodies cursor cells :=
  match bodies with
  | [] => cursor
  | body :: rest =>
    if cursor <? cells limit
    then dynamic_loop limit rest (S cursor) (write_cells body cells)
    else cursor
  end.

Fixpoint fixed_loop limit (bodies : list (list (nat * nat))) cursor :=
  match bodies with
  | [] => cursor
  | _ :: rest =>
    if cursor <? limit then fixed_loop limit rest (S cursor) else cursor
  end.

Theorem reserved_limit_execution : forall bodies base limit cursor cells,
  limit < base -> Forall (Forall (fun item => base <= fst item)) bodies ->
  dynamic_loop limit bodies cursor cells = fixed_loop (cells limit) bodies cursor.
Proof.
  induction bodies as [|body rest IH]; intros base limit cursor cells reserved valid; [reflexivity|].
  inversion valid; subst.
  change ((if cursor <? cells limit then dynamic_loop limit rest (S cursor) (write_cells body cells) else cursor) =
    (if cursor <? cells limit then fixed_loop (cells limit) rest (S cursor) else cursor)).
  destruct (cursor <? cells limit); auto.
  rewrite (IH base limit); auto.
  rewrite (reserved_cell_preserved body base limit); auto.
Qed.

Inductive statement := Plain | Range | Branch | Each (body : statement)
  | Sequence (left right : statement).

Fixpoint needs_scope source :=
  match source with
  | Plain => false
  | Range | Branch => true
  | Each body => needs_scope body
  | Sequence lhs rhs => needs_scope lhs || needs_scope rhs
  end.

Inductive contains_range : statement -> Prop :=
  | Range_here : contains_range Range
  | Range_each : forall body, contains_range body -> contains_range (Each body)
  | Range_left : forall left right, contains_range left -> contains_range (Sequence left right)
  | Range_right : forall left right, contains_range right -> contains_range (Sequence left right).

Theorem unchanged_has_no_range : forall source,
  needs_scope source = false -> ~ contains_range source.
Proof.
  intros source unchanged found. induction found; cbn in unchanged.
  - discriminate.
  - auto.
  - apply Bool.orb_false_iff in unchanged as [left_ok right_ok]. auto.
  - apply Bool.orb_false_iff in unchanged as [left_ok right_ok]. auto.
Qed.

Example reused_limit_changes_execution :
  dynamic_loop 1 [[(1, 100)]; []; []; []] 0 (fun _ => 3) = 4 /\
  fixed_loop 3 [[(1, 100)]; []; []; []] 0 = 3.
Proof. split; reflexivity. Qed.

Print Assumptions reserved_limit_execution.
Print Assumptions unchanged_has_no_range.