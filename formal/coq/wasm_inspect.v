(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Bool Arith.
Import ListNotations.

Module Inspect.

Inductive host := Stable | Session | Float | Compute.

Definition denied h :=
  match h with Stable => false | _ => true end.

Inductive guest :=
| Return : nat -> guest
| Pure : nat -> (nat -> guest) -> guest
| Read : host -> nat -> (nat -> guest) -> guest
| Write : host -> nat -> nat -> guest -> guest.

Inductive answer := Ready : nat -> answer | Refused | Exhausted.

Definition cache := nat -> nat.

Definition put (c : cache) k v : cache :=
  fun key => if key =? k then v else c key.

Fixpoint run fuel strict (g : guest) (c : cache) : answer * cache :=
  match fuel with
  | 0 => (Exhausted, c)
  | S rest =>
      match g with
      | Return v => (Ready v, c)
      | Pure v next => run rest strict (next v) c
      | Read h key next =>
          if strict && denied h then (Refused, c)
          else run rest strict (next (if denied h then c key else key)) c
      | Write h key v next =>
          if strict && denied h then (Refused, c)
          else run rest strict next (if denied h then put c key v else c)
      end
  end.

Theorem no_effect : forall fuel g c,
  snd (run fuel true g c) = c.
Proof.
  induction fuel as [|fuel ih]; intros g c; [reflexivity|].
  destruct g as [v|v next|h key next|h key v next]; cbn [run].
  - reflexivity.
  - apply ih.
  - destruct h; cbn [denied]; try reflexivity; apply ih.
  - destruct h; cbn [denied]; try reflexivity; apply ih.
Qed.

Theorem no_observation : forall fuel g a b,
  fst (run fuel true g a) = fst (run fuel true g b).
Proof.
  induction fuel as [|fuel ih]; intros g a b; [reflexivity|].
  destruct g as [v|v next|h key next|h key v next]; cbn [run].
  - reflexivity.
  - apply ih.
  - destruct h; cbn [denied]; try reflexivity; apply ih.
  - destruct h; cbn [denied]; try reflexivity; apply ih.
Qed.

Theorem read_refused : forall fuel h key next c,
  denied h = true -> run (S fuel) true (Read h key next) c = (Refused, c).
Proof. intros. cbn [run]. rewrite H. reflexivity. Qed.

Theorem write_refused : forall fuel h key v next c,
  denied h = true -> run (S fuel) true (Write h key v next) c = (Refused, c).
Proof. intros. cbn [run]. rewrite H. reflexivity. Qed.

Definition precheck strict update guarded imports :=
  negb (strict && update &&
    existsb (fun h => match h with
      | Stable => false
      | Compute => true
      | _ => guarded
      end) imports).

Definition start fuel strict update guarded imports g c :=
  if precheck strict update guarded imports
  then run fuel strict g c else (Refused, c).

Theorem before_start : forall fuel imports g c,
  existsb denied imports = true ->
  start fuel true true true imports g c = (Refused, c).
Proof.
  intros fuel imports g c h. unfold start, precheck.
  assert (e : existsb (fun h => match h with
    | Stable => false | Compute => true | _ => true end) imports = true).
  { change (existsb denied imports = true). exact h. }
  rewrite e. reflexivity.
Qed.

Theorem prior_preserved : forall fuel update guarded imports g c,
  start fuel false update guarded imports g c = run fuel false g c.
Proof. reflexivity. Qed.

Theorem ordinary_read : forall key next c,
  run 2 false (Read Session key next) c = run 1 false (next (c key)) c.
Proof. reflexivity. Qed.

Theorem ordinary_write : forall key v next c,
  run 2 false (Write Session key v next) c = run 1 false next (put c key v).
Proof. reflexivity. Qed.

Example warm_changes :
  fst (run 2 false (Read Session 0 Return) (fun _ => 3)) <>
  fst (run 2 false (Read Session 0 Return) (fun _ => 7)).
Proof. cbn [run denied]. discriminate. Qed.

Record entry := Entry {
  code : nat;
  policy : bool;
  output : answer
}.

Fixpoint lookup strict key entries : option answer :=
  match entries with
  | [] => None
  | e :: rest =>
      if (code e =? key) && Bool.eqb (policy e) strict
      then Some (output e) else lookup strict key rest
  end.

Example prior_not_reused :
  lookup true 7 [Entry 7 false (Ready 3)] = None.
Proof. reflexivity. Qed.

Example prior_cache_read :
  lookup false 7 [Entry 7 false (Ready 3)] = Some (Ready 3).
Proof. reflexivity. Qed.

Theorem lookup_origin : forall entries strict key value,
  lookup strict key entries = Some value ->
  exists e, In e entries /\ code e = key /\ policy e = strict /\ output e = value.
Proof.
  induction entries as [|e rest ih]; intros strict key value h; [discriminate|].
  cbn [lookup] in h.
  destruct ((code e =? key) && Bool.eqb (policy e) strict) eqn:check.
  - apply andb_true_iff in check as [identity mode].
    apply Nat.eqb_eq in identity.
    assert (policy e = strict) as same.
    { destruct (policy e), strict; cbn in mode; congruence. }
    inversion h; subst value. exists e.
    split; [left; reflexivity|]. split; [exact identity|].
    split; [exact same|reflexivity].
  - destruct (ih strict key value h) as [found [member facts]].
    exists found. split; [right; exact member|exact facts].
Qed.

Definition valid fuel g key entries :=
  forall e, In e entries -> code e = key -> policy e = true ->
    output e = fst (run fuel true g (fun _ => 0)).

Definition cached fuel strict g key entries ambient :=
  match lookup strict key entries with
  | Some value => value
  | None => fst (run fuel strict g ambient)
  end.

Theorem checked_cache_origin : forall fuel g key entries ambient,
  valid fuel g key entries ->
  cached fuel true g key entries ambient = fst (run fuel true g ambient).
Proof.
  intros fuel g key entries ambient good. unfold cached.
  destruct (lookup true key entries) as [value|] eqn:read; [|reflexivity].
  destruct (lookup_origin entries true key value read)
    as [e [member [identity [mode result]]]].
  rewrite <- result, (good e member identity mode).
  apply no_observation.
Qed.

Theorem cached_no_observation : forall fuel g key entries a b,
  valid fuel g key entries ->
  cached fuel true g key entries a = cached fuel true g key entries b.
Proof.
  intros. rewrite !checked_cache_origin by assumption. apply no_observation.
Qed.

End Inspect.