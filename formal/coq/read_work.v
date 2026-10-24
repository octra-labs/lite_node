(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Coq Require Import List Arith Bool Lia PArith.
Import ListNotations.

Fixpoint encode (value : positive) : list bool :=
  match value with
  | xH => [false; false]
  | xO rest => false :: true :: encode rest
  | xI rest => true :: false :: encode rest
  end.

Fixpoint decode (input : list bool) : option (positive * list bool) :=
  match input with
  | false :: false :: rest => Some (xH, rest)
  | false :: true :: rest =>
    match decode rest with Some (value, suffix) => Some (xO value, suffix) | None => None end
  | true :: false :: rest =>
    match decode rest with Some (value, suffix) => Some (xI value, suffix) | None => None end
  | _ => None
  end.

Theorem integer_prefix_exact : forall value suffix,
  decode (encode value ++ suffix) = Some (value, suffix).
Proof.
  induction value; intros suffix; simpl; try rewrite IHvalue; reflexivity.
Qed.

Fixpoint digits (value : positive) : nat :=
  match value with xH => 1 | xO rest | xI rest => S (digits rest) end.

Theorem integer_wire_work : forall value,
  length (encode value) = 2 * digits value.
Proof. induction value; simpl; lia. Qed.

Fixpoint number (bits : list bool) : nat :=
  match bits with [] => 0 | bit :: rest => (if bit then 1 else 0) + 2 * number rest end.

Theorem byte_chunk_composition : forall left right,
  number (left ++ right) = number left + 2 ^ length left * number right.
Proof.
  induction left as [|bit rest IH]; intros right; simpl; [lia|].
  rewrite IH. destruct bit; simpl; nia.
Qed.

Section Cache.
  Variable K V : Type.
  Variable equal : forall a b : K, {a = b} + {a <> b}.
  Variable verify : K -> option V.

  Record entry := Entry { key : K; value : V; bytes : nat }.

  Fixpoint trim slots budget (entries : list entry) :=
    match slots, entries with
    | S remaining, entry :: rest =>
      if bytes entry <=? budget then entry :: trim remaining (budget - bytes entry) rest else []
    | _, _ => []
    end.

  Definition memory entries := fold_right (fun e total => bytes e + total) 0 entries.
  Definition sound entries := Forall (fun e => verify (key e) = Some (value e)) entries.

  Theorem cache_capacity : forall slots budget entries,
    length (trim slots budget entries) <= slots /\ memory (trim slots budget entries) <= budget.
  Proof.
    induction slots; intros budget entries; destruct entries; simpl; try (split; lia).
    destruct (bytes e <=? budget) eqn:fit; simpl; [|split; lia].
    apply Nat.leb_le in fit. specialize (IHslots (budget - bytes e) entries).
    destruct IHslots as [count space]. unfold memory in *. simpl in *. split; lia.
  Qed.

  Lemma trim_sound : forall slots budget entries,
    sound entries -> sound (trim slots budget entries).
  Proof.
    induction slots; intros budget entries H.
    - constructor.
    - destruct entries as [|e rest]; [constructor|].
      simpl. inversion H; subst.
      destruct (bytes e <=? budget); [constructor; [assumption|apply IHslots; assumption]|constructor].
  Qed.

  Definition keep slots budget item entries :=
    trim slots budget (item :: filter (fun e => if equal (key e) (key item) then false else true) entries).

  Theorem checked_insert_sound : forall slots budget item entries,
    sound entries -> verify (key item) = Some (value item) ->
    sound (keep slots budget item entries).
  Proof.
    intros slots budget item entries H checked. apply trim_sound. constructor; auto.
    unfold sound in *. rewrite Forall_forall in *. intros e present.
    apply filter_In in present. apply H. tauto.
  Qed.

  Fixpoint lookup sought entries :=
    match entries with
    | [] => None
    | e :: rest => if equal sought (key e) then Some (value e) else lookup sought rest
    end.

  Theorem cache_hit_preserves_check : forall entries sought result,
    sound entries -> lookup sought entries = Some result -> verify sought = Some result.
  Proof.
    intros entries sought result H. induction H; simpl; [discriminate|].
    destruct (equal sought (key x)); intro found.
    - inversion found; subst; assumption.
    - now apply IHForall.
  Qed.

  Theorem rejected_key_not_reused : forall entries allowed denied result,
    sound entries -> verify denied = None -> lookup allowed entries = Some result ->
    lookup denied entries = None.
  Proof.
    intros entries allowed denied result H rejected accepted.
    destruct (lookup denied entries) eqn:found; auto.
    pose proof (cache_hit_preserves_check _ _ _ H found). congruence.
  Qed.
End Cache.

Section Receipt.
  Variable K D M : Type.
  Variable equal : forall a b : K, {a = b} + {a <> b}.
  Variable read : K -> option D.
  Variable check : D -> option M.

  Definition verified k :=
    match read k with
    | None => None
    | Some data => match check data with
      | None => None | Some meta => Some (data, meta) end
    end.

  Definition receipt k := option_map (@snd D M) (verified k).

  Definition restore k (meta : M) :=
    match read k with None => None | Some data => Some (data, meta) end.

  Theorem receipt_reconstructs_check : forall entries k meta,
    sound K M receipt entries ->
    lookup K M equal k entries = Some meta ->
    restore k meta = verified k.
  Proof.
    intros entries k meta consistent found.
    pose proof (cache_hit_preserves_check K M equal receipt entries k meta consistent found) as accepted.
    unfold receipt, verified in accepted. unfold restore, verified.
    destruct (read k) as [data|]; [|discriminate].
    destruct (check data); simpl in accepted; inversion accepted; reflexivity.
  Qed.
End Receipt.

Fixpoint served quota (requests : list bool) :=
  match requests with
  | [] => 0
  | false :: rest => served quota rest
  | true :: rest => match quota with 0 => 0 | S remaining => S (served remaining rest) end
  end.

Theorem abi_batch_capacity : forall requests quota, served quota requests <= quota.
Proof.
  induction requests; intros quota; simpl; [lia|].
  destruct a; [destruct quota; simpl; [lia|specialize (IHrequests quota); lia]|auto].
Qed.