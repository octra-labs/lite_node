(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Definition field (bytes : list nat) := length bytes :: bytes.
Definition read_field (input : list nat) :=
  match input with
  | [] => None
  | size :: rest =>
    if size <=? length rest then Some (firstn size rest, skipn size rest) else None
  end.

Lemma field_roundtrip : forall bytes rest,
  read_field (field bytes ++ rest) = Some (bytes, rest).
Proof.
  intros bytes rest. unfold read_field, field; cbn.
  rewrite length_app.
  assert (room : length bytes <=? length bytes + length rest = true) by (apply Nat.leb_le; lia).
  rewrite room.
  rewrite firstn_app, firstn_all, Nat.sub_diag, firstn_O, app_nil_r.
  rewrite skipn_app, skipn_all, Nat.sub_diag, skipn_O. reflexivity.
Qed.

Definition frame chain body := field [111; 99; 116; 114; 97] ++ field chain ++ field body.
Definition read_frame input :=
  match read_field input with
  | Some (tag, rest) =>
    match read_field rest with
    | Some (chain, rest) =>
      match read_field rest with
      | Some (body, []) => Some (tag, chain, body)
      | _ => None
      end
    | None => None
    end
  | None => None
  end.

Lemma frame_roundtrip : forall chain body,
  read_frame (frame chain body) = Some ([111; 99; 116; 114; 97], chain, body).
Proof.
  intros chain body. unfold read_frame, frame.
  rewrite field_roundtrip, field_roundtrip.
  replace (field body) with (field body ++ []) by apply app_nil_r.
  rewrite field_roundtrip. reflexivity.
Qed.

Theorem network_separation : forall left right a b,
  frame left a = frame right b -> left = right /\ a = b.
Proof.
  intros left right a b same.
  pose proof (f_equal read_frame same) as decoded.
  rewrite !frame_roundtrip in decoded. inversion decoded; auto.
Qed.

Inductive mode := Historical | Network.
Definition select epoch activation :=
  match activation with
  | None => Historical
  | Some first => if epoch <? first then Historical else Network
  end.

Theorem exact_transition : forall first,
  select first (Some first) = Network /\
  (forall epoch, epoch < first -> select epoch (Some first) = Historical) /\
  (forall epoch, first <= epoch -> select epoch (Some first) = Network).
Proof.
  intro first. unfold select. rewrite Nat.ltb_irrefl.
  split; [reflexivity|]. split; intros epoch order.
  - apply Nat.ltb_lt in order. now rewrite order.
  - apply Nat.ltb_ge in order. now rewrite order.
Qed.

Print Assumptions network_separation.
Print Assumptions exact_transition.