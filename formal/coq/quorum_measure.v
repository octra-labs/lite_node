(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Lia List Bool.
Import ListNotations.
Open Scope Z_scope.

Ltac Zify.zify_post_hook ::= Z.to_euclidean_division_equations.

Inductive rule := Prior | Capped | Stake | Seatless.

Inductive ask := Quorum | Skip | Source | Pair | Ahead | Start | Promise.

Record shape := Shape {
  n : Z;
  f : Z;
  quorum : Z;
  total_weight : Z;
  quorum_weight : Z
}.

Record space := Space {
  rule_of : rule;
  declared : bool;
  form : shape
}.

Definition seat := (nat * Z)%type.

Definition quota (total : Z) : Z := Z.quot (total * 2) 3 + 1.

Definition shape_of (seats total : Z) : shape :=
  let faults := Z.quot (seats - 1) 3 in
  let seated :=
    if seats <=? 0 then 0
    else if seats <? 4 then seats
    else seats - faults in
  let heavy := if Z.sgn total <=? 0 then 0 else quota total in
  Shape seats faults seated total heavy.

Definition live (total lost : Z) : bool := quota total <=? total - lost.

Definition sum (weights : list seat) : Z :=
  fold_left (fun total entry => total + snd entry) weights 0.

Definition ruled (chosen : rule) (stated : bool) (weights : list seat) : space :=
  Space chosen stated (shape_of (Z.of_nat (length weights)) (sum weights)).

Definition valid (s : space) : bool :=
  match rule_of s with
  | Prior | Capped => true
  | Stake | Seatless => declared s
  end.

Definition minority (s : shape) : Z := total_weight s - quorum_weight s + 1.

Definition start_seats (s : space) : Z :=
  match rule_of s with
  | Prior => 1
  | Capped => Z.max 1 (quorum (form s) - 1)
  | Stake => Z.max 1 (quorum (form s) - 1)
  | Seatless => 1
  end.

Definition promise_seats (s : space) : Z := Z.max 0 (quorum (form s) - 1).

Definition meets (s : space) (a : ask) (seats weight : Z) : bool :=
  let seated := quorum (form s) <=? seats in
  let present := f (form s) + 1 <=? seats in
  let heavy := quorum_weight (form s) <=? weight in
  let blocking := minority (form s) <=? weight in
  let open_minority := 0 <? Z.sgn (minority (form s)) in
  let funded := valid s && (0 <? Z.sgn (total_weight (form s))) in
  match a, rule_of s with
  | Quorum, Prior => heavy
  | Quorum, Capped => seated && heavy
  | Quorum, Stake => valid s && seated && heavy
  | Quorum, Seatless => funded && heavy
  | Skip, Prior => blocking
  | Skip, Capped => present && blocking
  | Skip, Stake => valid s && present && blocking
  | Skip, Seatless => funded && blocking
  | Source, Prior => blocking
  | Source, Capped => present && blocking
  | Source, Stake => present && blocking
  | Source, Seatless => funded && blocking
  | Pair, Prior => seated && heavy
  | Pair, Capped => seated && heavy
  | Pair, Stake => seated && heavy
  | Pair, Seatless => funded && heavy
  | Ahead, Prior => open_minority && blocking
  | Ahead, Capped => open_minority && blocking
  | Ahead, Stake => open_minority && blocking
  | Ahead, Seatless => funded && blocking
  | Start, Prior => start_seats s <=? seats
  | Start, Capped => start_seats s <=? seats
  | Start, Stake => start_seats s <=? seats
  | Start, Seatless => start_seats s <=? seats
  | Promise, Prior => promise_seats s <=? seats
  | Promise, Capped => promise_seats s <=? seats
  | Promise, Stake => promise_seats s <=? seats
  | Promise, Seatless => funded && blocking
  end.

Definition attested (s : space) (required seats weight : Z) : bool :=
  match rule_of s with
  | Prior | Capped | Stake => required <=? seats
  | Seatless =>
    valid s && (0 <? Z.sgn (total_weight (form s))) && (minority (form s) <=? weight)
  end.

Lemma sign_low total : (Z.sgn total <=? 0) = (total <=? 0).
Proof. destruct total; reflexivity. Qed.

Lemma sign_high total : (0 <? Z.sgn total) = (0 <? total).
Proof. destruct total; reflexivity. Qed.

Lemma quota_range total :
  0 < total -> 2 * total < 3 * quota total /\ 3 * quota total <= 2 * total + 3.
Proof. intros funded. unfold quota. lia. Qed.

Lemma quota_within total : 0 < total -> 1 <= quota total /\ quota total <= total.
Proof. intros funded. pose proof (quota_range total funded). lia. Qed.

Lemma quota_least total level : 0 < total -> 2 * total < 3 * level -> quota total <= level.
Proof. intros funded wide. pose proof (quota_range total funded). lia. Qed.

Lemma margin_class total :
  0 < total ->
  2 * quota total - total - 1 - (total - quota total) =
  match total mod 3 with 0 => 2 | 1 => 0 | _ => 1 end.
Proof.
  intros funded.
  assert (total mod 3 = 0 \/ total mod 3 = 1 \/ total mod 3 = 2) as [class|[class|class]]
    by lia.
  all: rewrite class; cbv beta iota; unfold quota; lia.
Qed.

Lemma quota_floor total : 0 <= total -> quota total = 2 * total / 3 + 1.
Proof. intros sign. unfold quota. lia. Qed.

Lemma fault_level total faulty :
  0 < total -> (faulty <= total - quota total <-> 3 * faulty < total).
Proof. intros funded. pose proof (quota_range total funded). lia. Qed.

Lemma live_exact total lost :
  0 < total -> (live total lost = true <-> 3 * lost < total).
Proof.
  intros funded. unfold live. rewrite Z.leb_le.
  pose proof (fault_level total lost funded). lia.
Qed.

Lemma shape_seats seats total : n (shape_of seats total) = seats.
Proof. reflexivity. Qed.

Lemma shape_total seats total : total_weight (shape_of seats total) = total.
Proof. reflexivity. Qed.

Lemma shape_heavy seats total :
  0 < total -> quorum_weight (shape_of seats total) = quota total.
Proof.
  intros funded. unfold shape_of. cbn [quorum_weight]. rewrite sign_low.
  destruct (Z.leb_spec total 0); [lia|reflexivity].
Qed.

Lemma shape_unfunded seats total :
  total <= 0 -> quorum_weight (shape_of seats total) = 0.
Proof.
  intros none. unfold shape_of. cbn [quorum_weight]. rewrite sign_low.
  destruct (Z.leb_spec total 0); [reflexivity|lia].
Qed.

Lemma shape_minority seats total :
  0 < total -> minority (shape_of seats total) = total - quota total + 1.
Proof.
  intros funded. unfold minority. rewrite shape_total, (shape_heavy seats total funded).
  reflexivity.
Qed.

Lemma minority_range seats total :
  0 < total ->
  total <= 3 * minority (shape_of seats total) /\
  3 * minority (shape_of seats total) < total + 3 /\
  minority (shape_of seats total) <= quorum_weight (shape_of seats total).
Proof.
  intros funded. rewrite (shape_minority seats total funded), (shape_heavy seats total funded).
  pose proof (quota_range total funded). lia.
Qed.

Lemma fault_range seats total :
  1 <= seats ->
  3 * f (shape_of seats total) <= seats - 1 /\ seats - 1 < 3 * f (shape_of seats total) + 3.
Proof. intros present. unfold shape_of. cbn [f]. lia. Qed.

Lemma seat_quorum seats total :
  quorum (shape_of seats total) =
  if seats <=? 0 then 0
  else if seats <? 4 then seats
  else seats - f (shape_of seats total).
Proof. reflexivity. Qed.

Lemma seat_overlap seats total :
  1 <= seats -> f (shape_of seats total) < 2 * quorum (shape_of seats total) - seats.
Proof.
  intros present. rewrite seat_quorum.
  pose proof (fault_range seats total present).
  destruct (Z.leb_spec seats 0); [lia|].
  destruct (Z.ltb_spec seats 4); lia.
Qed.

Lemma seat_spare seats total :
  0 <= seats ->
  seats - quorum (shape_of seats total) =
  if seats <? 4 then 0 else f (shape_of seats total).
Proof.
  intros sign. rewrite seat_quorum.
  destruct (Z.leb_spec seats 0); destruct (Z.ltb_spec seats 4); lia.
Qed.

Lemma seat_halt own added total :
  0 <= own -> 0 <= added ->
  (own < quorum (shape_of (own + added) total) <->
   (own + added < 4 /\ 0 < added) \/ (4 <= own + added /\ own <= 2 * added)).
Proof.
  intros held grown. rewrite seat_quorum. unfold shape_of. cbn [f].
  destruct (Z.leb_spec (own + added) 0); destruct (Z.ltb_spec (own + added) 4); lia.
Qed.

Lemma seat_fault own added total :
  0 <= own -> 4 <= own + added ->
  (own < quorum (shape_of (own + added) total) <->
   f (shape_of (own + added) total) < added).
Proof.
  intros held enough. rewrite seat_quorum.
  destruct (Z.leb_spec (own + added) 0); destruct (Z.ltb_spec (own + added) 4); lia.
Qed.

Lemma seat_least own added total :
  0 <= own -> 4 <= own + added -> own <= 2 * added -> 2 * added <= own + 1 ->
  added = f (shape_of (own + added) total) + 1.
Proof. intros held enough reach least. unfold shape_of. cbn [f]. lia. Qed.

Lemma weight_keep own added :
  0 < own -> 0 <= added -> (quota (own + added) <= own <-> 2 * added < own).
Proof.
  intros held grown.
  pose proof (quota_range (own + added) ltac:(lia)). lia.
Qed.

Lemma weight_block seats own added :
  0 < own -> 0 <= added ->
  (minority (shape_of seats (own + added)) <= added <-> own <= 2 * added).
Proof.
  intros held grown. rewrite shape_minority by lia.
  pose proof (quota_range (own + added) ltac:(lia)). lia.
Qed.

Lemma stake_quorum_rows stated seats total signed weight :
  meets (Space Stake stated (shape_of seats total)) Quorum signed weight = true <->
  stated = true /\
  quorum (shape_of seats total) <= signed /\
  quorum_weight (shape_of seats total) <= weight.
Proof.
  unfold meets, valid. cbn [rule_of declared form].
  rewrite !andb_true_iff, !Z.leb_le. tauto.
Qed.

Lemma stake_skip_rows stated seats total signed weight :
  meets (Space Stake stated (shape_of seats total)) Skip signed weight = true <->
  stated = true /\
  f (shape_of seats total) + 1 <= signed /\
  minority (shape_of seats total) <= weight.
Proof.
  unfold meets, valid. cbn [rule_of declared form].
  rewrite !andb_true_iff, !Z.leb_le. tauto.
Qed.

Lemma seatless_quorum_rows stated seats total signed weight :
  meets (Space Seatless stated (shape_of seats total)) Quorum signed weight = true <->
  stated = true /\ 0 < total /\ quota total <= weight.
Proof.
  unfold meets, valid. cbn [rule_of declared form].
  rewrite shape_total, sign_high, !andb_true_iff, Z.ltb_lt, Z.leb_le.
  split.
  - intros [[flag funded] heavy]. rewrite (shape_heavy seats total funded) in heavy. auto.
  - intros [flag [funded heavy]]. rewrite (shape_heavy seats total funded). auto.
Qed.

Lemma seatless_skip_rows stated seats total signed weight :
  meets (Space Seatless stated (shape_of seats total)) Skip signed weight = true <->
  stated = true /\ 0 < total /\ total - quota total + 1 <= weight.
Proof.
  unfold meets, valid. cbn [rule_of declared form].
  rewrite shape_total, sign_high, !andb_true_iff, Z.ltb_lt, Z.leb_le.
  split.
  - intros [[flag funded] blocking]. rewrite (shape_minority seats total funded) in blocking. auto.
  - intros [flag [funded blocking]]. rewrite (shape_minority seats total funded). auto.
Qed.

Theorem seatless_blind stated first second total a left right weight :
  a <> Start ->
  meets (Space Seatless stated (shape_of first total)) a left weight =
  meets (Space Seatless stated (shape_of second total)) a right weight.
Proof.
  intros other. destruct a; try contradiction; unfold meets, valid, minority;
    cbn [rule_of declared form total_weight quorum_weight shape_of]; reflexivity.
Qed.

Theorem seatless_levels stated seats total signed weight :
  meets (Space Seatless stated (shape_of seats total)) Pair signed weight =
    meets (Space Seatless stated (shape_of seats total)) Quorum signed weight /\
  meets (Space Seatless stated (shape_of seats total)) Source signed weight =
    meets (Space Seatless stated (shape_of seats total)) Skip signed weight /\
  meets (Space Seatless stated (shape_of seats total)) Ahead signed weight =
    meets (Space Seatless stated (shape_of seats total)) Skip signed weight /\
  meets (Space Seatless stated (shape_of seats total)) Promise signed weight =
    meets (Space Seatless stated (shape_of seats total)) Skip signed weight /\
  (forall required,
    attested (Space Seatless stated (shape_of seats total)) required signed weight =
    meets (Space Seatless stated (shape_of seats total)) Skip signed weight).
Proof. repeat split; reflexivity. Qed.

Theorem seatless_start stated seats total signed weight :
  meets (Space Seatless stated (shape_of seats total)) Start signed weight = (1 <=? signed).
Proof. reflexivity. Qed.

Theorem seatless_guard stated seats total a signed weight :
  a <> Start -> stated = false \/ total <= 0 ->
  meets (Space Seatless stated (shape_of seats total)) a signed weight = false.
Proof.
  intros other closed.
  assert (valid (Space Seatless stated (shape_of seats total))
    && (0 <? Z.sgn (total_weight (form (Space Seatless stated (shape_of seats total)))))
    = false) as shut.
  { unfold valid. cbn [rule_of declared form]. rewrite shape_total, sign_high.
    destruct closed as [flag|none]; [subst stated; reflexivity|].
    destruct (Z.ltb_spec 0 total); [lia|]. apply andb_false_r. }
  destruct a; try contradiction; unfold meets; cbv zeta; rewrite shut; reflexivity.
Qed.

Theorem stake_within stated seats total signed weight a :
  a = Quorum \/ a = Skip ->
  0 < total ->
  meets (Space Stake stated (shape_of seats total)) a signed weight = true ->
  meets (Space Seatless stated (shape_of seats total)) a signed weight = true.
Proof.
  intros [chosen|chosen] funded accepted; subst a.
  - apply stake_quorum_rows in accepted as [flag [_ heavy]].
    apply seatless_quorum_rows. rewrite (shape_heavy seats total funded) in heavy. auto.
  - apply stake_skip_rows in accepted as [flag [_ blocking]].
    apply seatless_skip_rows. rewrite (shape_minority seats total funded) in blocking. auto.
Qed.

Theorem stake_within_stated seats total signed weight a :
  a <> Promise ->
  0 < total ->
  meets (Space Stake true (shape_of seats total)) a signed weight = true ->
  meets (Space Seatless true (shape_of seats total)) a signed weight = true.
Proof.
  intros other funded.
  assert ((0 <? total) = true) as opened by (apply Z.ltb_lt; exact funded).
  destruct a; unfold meets, valid, start_seats; cbn [rule_of declared form];
    rewrite ?shape_total, ?sign_high, ?opened; cbn [andb].
  - rewrite andb_true_iff. tauto.
  - rewrite andb_true_iff. tauto.
  - rewrite andb_true_iff. tauto.
  - rewrite andb_true_iff. tauto.
  - rewrite andb_true_iff. tauto.
  - rewrite !Z.leb_le. lia.
  - contradiction.
Qed.

Example weight_limits :
  map (fun total =>
    (quota total, quorum_weight (shape_of 1 total), minority (shape_of 1 total)))
    [-1; 0; 1; 2; 3; 4; 5; 6; 7; 1000] =
  [(1, 0, 0); (1, 0, 1); (1, 1, 1); (2, 2, 1); (3, 3, 1); (3, 3, 2); (4, 4, 2);
   (5, 5, 2); (5, 5, 3); (667, 667, 334)].
Proof. vm_compute. reflexivity. Qed.

Example seat_limits :
  map (fun seats => (f (shape_of seats 1), quorum (shape_of seats 1)))
    [-1; 0; 1; 2; 3; 4; 5; 6; 7; 10; 36; 48; 53; 54] =
  [(0, 0); (0, 0); (0, 1); (0, 2); (0, 3); (1, 3); (1, 4); (1, 5); (2, 5); (3, 7);
   (11, 25); (15, 33); (17, 36); (17, 37)].
Proof. vm_compute. reflexivity. Qed.

Example live_limits :
  map (fun pair => live (fst pair) (snd pair))
    [(0, 0); (1, 0); (1, 1); (3, 0); (3, 1); (4, 1); (4, 2); (1000, 333); (1000, 334)] =
  [false; true; false; true; false; true; false; true; false].
Proof. vm_compute. reflexivity. Qed.

Print Assumptions seatless_blind.
Print Assumptions seatless_levels.
Print Assumptions seatless_guard.
Print Assumptions margin_class.
Print Assumptions stake_within.
Print Assumptions stake_within_stated.
Print Assumptions seat_halt.
Print Assumptions seat_least.
Print Assumptions weight_keep.
Print Assumptions weight_block.