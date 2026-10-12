(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Lia Bool List.
Open Scope Z_scope.

Module PaidSelection.
Inductive outcome := Accepted | Charged | Refused.
Definition row := (outcome * Z)%type.

Definition executes (item : row) :=
  match fst item with Refused => false | _ => true end.

Definition fee (item : row) :=
  if executes item then snd item else 0.

Fixpoint fees (items : list row) :=
  match items with nil => 0 | cons item rest => fee item + fees rest end.

Theorem selection_preserves_fees : forall items,
  fees (filter executes items) = fees items.
Proof.
  induction items as [|[result amount] rest ih]; simpl; [reflexivity|].
  destruct result; simpl in *; rewrite ih; lia.
Qed.

Theorem charged_is_executed : forall prefix suffix amount,
  In (Charged, amount) (filter executes (prefix ++ (Charged, amount) :: suffix)).
Proof.
  intros prefix suffix amount. apply filter_In. split.
  - apply in_or_app. right. simpl. auto.
  - reflexivity.
Qed.

Theorem lost_fee_if_omitted : forall amount,
  0 < amount -> fees ((Charged, amount) :: nil) <> fees nil.
Proof. intros amount positive. cbv [fees fee executes]. simpl. lia. Qed.

Print Assumptions selection_preserves_fees.
Print Assumptions charged_is_executed.
Print Assumptions lost_fee_if_omitted.
End PaidSelection.

Definition clip floor ceiling price := Z.min ceiling (Z.max floor price).

Definition change price target speed used :=
  price * Z.abs (used - target) / (target * speed).

Definition step price target speed used :=
  if target <? used
  then price + Z.max 1 (change price target speed used)
  else price - change price target speed used.

Definition advance floor ceiling price target speed used :=
  clip floor ceiling (step price target speed used).

Theorem clip_range : forall floor ceiling price,
  floor <= ceiling -> floor <= clip floor ceiling price <= ceiling.
Proof.
  intros. unfold clip.
  pose proof (Z.le_max_l floor price).
  destruct (Z.min_spec ceiling (Z.max floor price)) as [[? ->]|[? ->]]; lia.
Qed.

Theorem clip_same : forall floor ceiling price,
  floor <= price <= ceiling -> clip floor ceiling price = price.
Proof.
  intros. unfold clip. rewrite Z.max_r, Z.min_r; lia.
Qed.

Theorem clip_order : forall floor ceiling left right,
  left <= right -> clip floor ceiling left <= clip floor ceiling right.
Proof.
  intros. unfold clip. apply Z.min_le_compat; [lia|].
  apply Z.max_le_compat; lia.
Qed.

Theorem change_positive : forall price target speed used,
  0 <= price -> 0 < target -> 0 < speed ->
  0 <= change price target speed used.
Proof.
  intros. unfold change. apply Z.div_pos.
  - pose proof (Z.abs_nonneg (used - target)). nia.
  - nia.
Qed.

Theorem target_same : forall floor ceiling price target speed,
  floor <= price <= ceiling -> 0 < target -> 0 < speed ->
  advance floor ceiling price target speed target = price.
Proof.
  intros. unfold advance, step, change. rewrite Z.ltb_irrefl.
  replace (target - target) with 0 by lia. simpl.
  rewrite Z.mul_0_r, Z.div_0_l by nia.
  rewrite Z.sub_0_r. apply clip_same. assumption.
Qed.

Theorem step_order : forall price target speed left right,
  0 <= price -> 0 < target -> 0 < speed -> left <= right ->
  step price target speed left <= step price target speed right.
Proof.
  intros price target speed left right positive width rate ordered.
  pose proof (change_positive price target speed left positive width rate) as low.
  pose proof (change_positive price target speed right positive width rate) as high.
  unfold step.
  destruct (target <? left) eqn:first; destruct (target <? right) eqn:second;
    try apply Z.ltb_lt in first; try apply Z.ltb_ge in first;
    try apply Z.ltb_lt in second; try apply Z.ltb_ge in second.
  - assert (cost : change price target speed left <= change price target speed right).
    { unfold change. rewrite !Z.abs_eq by lia. apply Z.div_le_mono; nia. }
    pose proof (Z.max_le_compat 1 1 _ _ (Z.le_refl 1) cost). lia.
  - lia.
  - pose proof (Z.le_max_l 1 (change price target speed right)). lia.
  - assert (cost : change price target speed right <= change price target speed left).
    { unfold change. rewrite !Z.abs_neq by lia. apply Z.div_le_mono; nia. }
    lia.
Qed.

Theorem price_order : forall floor ceiling price target speed left right,
  0 <= price -> 0 < target -> 0 < speed -> left <= right ->
  advance floor ceiling price target speed left <=
    advance floor ceiling price target speed right.
Proof. intros. apply clip_order. apply step_order; assumption. Qed.

Theorem price_direction : forall floor ceiling price target speed used,
  0 <= price -> 0 < target -> 0 < speed -> floor <= price <= ceiling ->
  (used <= target -> advance floor ceiling price target speed used <= price) /\
  (target <= used -> price <= advance floor ceiling price target speed used).
Proof.
  intros. pose proof (target_same floor ceiling price target speed H2 H0 H1) as same.
  split; intro load.
  - rewrite <- same at 2. apply price_order; assumption.
  - rewrite <- same at 1. apply price_order; assumption.
Qed.

Theorem overload_increases : forall floor ceiling price target speed used,
  floor <= price -> price < ceiling -> target < used ->
  price < advance floor ceiling price target speed used.
Proof.
  intros. unfold advance, step.
  assert (load : (target <? used) = true) by (apply Z.ltb_lt; lia).
  rewrite load. unfold clip.
  pose proof (Z.le_max_l 1 (change price target speed used)).
  pose proof (Z.le_max_r floor (price + Z.max 1 (change price target speed used))).
  destruct (Z.min_spec ceiling
    (Z.max floor (price + Z.max 1 (change price target speed used))))
    as [[? ->]|[? ->]]; lia.
Qed.

Definition reserve price work cap :=
  if (0 <? price) && (0 <=? work) && (0 <=? cap) && (price * work <=? cap)
  then Some (price * work) else None.

Definition settle price work used :=
  if (0 <=? used) && (used <=? work)
  then Some (price * used, price * work - price * used) else None.

Theorem reserve_cap : forall price work cap held,
  reserve price work cap = Some held -> 0 <= held <= cap /\ held = price * work.
Proof.
  intros. unfold reserve in H.
  destruct (((0 <? price) && (0 <=? work) && (0 <=? cap)) && (price * work <=? cap))
    eqn:accepted; [|discriminate].
  repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[[positive work_ok] cap_ok] fits].
  apply Z.ltb_lt in positive. apply Z.leb_le in work_ok.
  apply Z.leb_le in cap_ok. apply Z.leb_le in fits.
  inversion H; subst. nia.
Qed.

Theorem payment_conservation : forall price work cap held used charged refund,
  reserve price work cap = Some held ->
  settle price work used = Some (charged, refund) ->
  0 <= charged <= cap /\ 0 <= refund /\ charged + refund = held /\ used <= work.
Proof.
  intros price work cap held used charged refund offer payment.
  pose proof (reserve_cap price work cap held offer) as [fits amount].
  unfold reserve in offer.
  destruct (((0 <? price) && (0 <=? work) && (0 <=? cap)) && (price * work <=? cap))
    eqn:accepted; [|discriminate].
  repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[[positive work_ok] cap_ok] reserved].
  apply Z.ltb_lt in positive.
  unfold settle in payment.
  destruct ((0 <=? used) && (used <=? work)) eqn:ran; [|discriminate].
  rewrite andb_true_iff in ran. destruct ran as [used_ok complete].
  apply Z.leb_le in used_ok. apply Z.leb_le in complete.
  inversion payment; subst. nia.
Qed.

Theorem work_not_price : forall price work used,
  work < used -> settle price work used = None.
Proof.
  intros. unfold settle.
  assert (excess : (used <=? work) = false) by (apply Z.leb_gt; assumption).
  rewrite excess, andb_false_r. reflexivity.
Qed.

Theorem partial_refund : forall price work used,
  0 <= used <= work ->
  settle price work used = Some (price * used, price * (work - used)).
Proof.
  intros. unfold settle.
  assert (positive : (0 <=? used) = true) by (apply Z.leb_le; lia).
  assert (fits : (used <=? work) = true) by (apply Z.leb_le; lia).
  rewrite positive, fits. simpl. f_equal. f_equal. ring.
Qed.

Module Payment.

Inductive status := Open | Paid | Returned.
Inductive outcome := Complete | Stopped (code : Z) | Exhausted.

Record state := {
  payer : Z;
  escrow : Z;
  workers : Z;
  phase : status
}.

Definition total s := payer s + escrow s + workers s.

Definition pay s cap price fuel used deadline epoch (result : outcome) :=
  match phase s with
  | Open =>
    let stop_ok := match result with Complete | Exhausted => true | Stopped code => 0 <? code end in
    if stop_ok && (0 <? price) && (0 <? used) && (used <=? fuel)
       && (price * fuel <=? cap) && (cap <=? escrow s) && (epoch <=? deadline)
    then Some {| payer := payer s + cap - price * used;
                 escrow := escrow s - cap;
                 workers := workers s + price * used;
                 phase := Paid |}
    else None
  | _ => None
  end.

Definition refund s cap deadline epoch :=
  match phase s with
  | Open =>
    if (0 <=? cap) && (cap <=? escrow s) && (deadline <? epoch)
    then Some {| payer := payer s + cap; escrow := escrow s - cap;
                 workers := workers s; phase := Returned |}
    else None
  | _ => None
  end.

Theorem pay_conserves : forall s next cap price fuel used deadline epoch result,
  pay s cap price fuel used deadline epoch result = Some next ->
  total next = total s /\ phase next = Paid /\
  payer s <= payer next /\ workers s < workers next /\ 0 <= escrow next.
Proof.
  intros s next cap price fuel used deadline epoch result H.
  unfold pay in H. destruct (phase s); try discriminate.
  destruct (match result with Complete | Exhausted => true | Stopped code => 0 <? code end
    && (0 <? price) && (0 <? used) && (used <=? fuel)
    && (price * fuel <=? cap) && (cap <=? escrow s) && (epoch <=? deadline)) eqn:checks;
    try discriminate.
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[[[[[stop positive] work] fits] limit] funds] time].
  apply Z.ltb_lt in positive, work. apply Z.leb_le in fits, limit, funds.
  inversion H; subst. unfold total; simpl. repeat split; try reflexivity; nia.
Qed.

Theorem pay_once : forall s next cap price fuel used deadline epoch result,
  pay s cap price fuel used deadline epoch result = Some next ->
  forall cap' price' fuel' used' deadline' epoch' result',
  pay next cap' price' fuel' used' deadline' epoch' result' = None /\
  refund next cap' deadline' epoch' = None.
Proof.
  intros. pose proof (pay_conserves _ _ _ _ _ _ _ _ _ H) as [_ [paid _]].
  unfold pay, refund. rewrite paid. split; reflexivity.
Qed.

Theorem refund_once : forall s next cap deadline epoch,
  refund s cap deadline epoch = Some next ->
  total next = total s /\
  forall cap' price fuel used deadline' epoch' result,
  pay next cap' price fuel used deadline' epoch' result = None /\
  refund next cap' deadline' epoch' = None.
Proof.
  intros s next cap deadline epoch H.
  unfold refund in H. destruct (phase s); try discriminate.
  destruct ((0 <=? cap) && (cap <=? escrow s) && (deadline <? epoch)); try discriminate.
  inversion H; subst. split.
  - unfold total; simpl; ring.
  - intros; split; reflexivity.
Qed.

Theorem stopped_work_paid : forall s cap price fuel used deadline epoch code,
  0 < code ->
  pay s cap price fuel used deadline epoch (Stopped code) =
  pay s cap price fuel used deadline epoch Complete.
Proof.
  intros. unfold pay. destruct (phase s); try reflexivity.
  assert ((0 <? code) = true) by (apply Z.ltb_lt; assumption).
  rewrite H0. reflexivity.
Qed.

Theorem closed_after_deadline : forall s cap price fuel used deadline epoch result,
  deadline < epoch -> pay s cap price fuel used deadline epoch result = None.
Proof.
  intros. unfold pay. destruct (phase s); try reflexivity.
  assert ((epoch <=? deadline) = false) by (apply Z.leb_gt; assumption).
  rewrite H0, andb_false_r. reflexivity.
Qed.

Theorem split_exact : forall charged count,
  0 <= charged -> 0 < count ->
  let share := charged / count in
  let extra := charged mod count in
  0 <= share /\ 0 <= extra < count /\
  extra * (share + 1) + (count - extra) * share = charged.
Proof.
  intros charged count positive nonempty share extra.
  assert (quota : 0 <= share) by (unfold share; apply Z.div_pos; lia).
  assert (remainder : 0 <= extra < count) by
    (unfold extra; apply Z.mod_pos_bound; lia).
  pose proof (Z.div_mod charged count ltac:(lia)) as equation.
  unfold share, extra in *. repeat split; nia.
Qed.

End Payment.

Module Deposit.

Import ListNotations.

Inductive phase := Empty | Held (amount deadline : Z) | Returned.

Record state := {
  wallet : Z;
  vault : Z;
  nonce : Z;
  status : phase
}.

Inductive action :=
  | Fund (amount deadline sequence epoch : Z)
  | Refund (epoch : Z).

Definition step (s : state) (a : action) : option state :=
  match s.(status), a with
  | Empty, Fund amount deadline sequence epoch =>
    if (0 <? amount) && (amount <=? s.(wallet))
       && (sequence =? s.(nonce) + 1) && (0 <=? epoch) && (epoch <=? deadline)
    then Some {| wallet := s.(wallet) - amount; vault := s.(vault) + amount;
                 nonce := sequence; status := Held amount deadline |}
    else None
  | Held amount deadline, Refund epoch =>
    if (deadline <? epoch) && (amount <=? s.(vault))
    then Some {| wallet := s.(wallet) + amount; vault := s.(vault) - amount;
                 nonce := s.(nonce); status := Returned |}
    else None
  | _, _ => None
  end.

Definition valid (s : state) :=
  0 <= s.(wallet) /\ 0 <= s.(vault) /\ 0 <= s.(nonce) /\
  match s.(status) with Held amount _ => 0 < amount <= s.(vault) | _ => True end.

Theorem step_total : forall s a next,
  step s a = Some next -> wallet next + vault next = wallet s + vault s.
Proof.
  intros [cash locked sequence phase] a next accepted.
  destruct phase; destruct a; unfold step in accepted; cbn in accepted; try discriminate;
    match type of accepted with
    | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
    end;
    inversion accepted; subst; simpl; ring.
Qed.

Theorem step_valid : forall s a next,
  valid s -> step s a = Some next -> valid next.
Proof.
  intros [cash locked sequence phase] a next initial accepted.
  destruct phase; destruct a; unfold step in accepted; cbn in accepted; try discriminate.
  - match type of accepted with
    | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
    end.
    repeat rewrite andb_true_iff in checks.
    destruct checks as [[[[positive fits] number] now] expires].
    apply Z.ltb_lt in positive. apply Z.leb_le in fits.
    apply Z.eqb_eq in number. apply Z.leb_le in now. apply Z.leb_le in expires.
    inversion accepted; subst. unfold valid in *; simpl in *. lia.
  - match type of accepted with
    | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
    end.
    rewrite andb_true_iff in checks. destruct checks as [expired fits].
    apply Z.ltb_lt in expired. apply Z.leb_le in fits.
    inversion accepted; subst. unfold valid in *; simpl in *. lia.
Qed.

Definition commit (written : bool) (s : state) (a : action) :=
  match step s a with Some next => if written then next else s | None => s end.

Fixpoint trace (events : list (bool * action)) (s : state) :=
  match events with
  | nil => s
  | (written, a) :: rest => trace rest (commit written s a)
  end.

Theorem trace_total : forall events s,
  wallet (trace events s) + vault (trace events s) = wallet s + vault s.
Proof.
  induction events as [|[written a] rest induction]; intros s; simpl; [reflexivity|].
  rewrite induction. unfold commit.
  destruct (step s a) eqn:accepted; [destruct written|]; try reflexivity.
  now apply (step_total s a).
Qed.

Theorem trace_valid : forall events s,
  valid s -> valid (trace events s).
Proof.
  induction events as [|[written a] rest induction]; intros s initial; simpl; [exact initial|].
  apply induction. unfold commit.
  destruct (step s a) eqn:accepted; [destruct written|]; try exact initial.
  now apply (step_valid s a).
Qed.

Theorem refund_once : forall s epoch next a,
  step s (Refund epoch) = Some next -> step next a = None.
Proof.
  intros [cash locked sequence phase] epoch next a accepted.
  destruct phase; unfold step in accepted; cbn in accepted; try discriminate.
  match type of accepted with
  | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
  end.
  inversion accepted; subst. now destruct a.
Qed.

Theorem refund_after_deadline : forall s epoch next amount deadline,
  s.(status) = Held amount deadline ->
  step s (Refund epoch) = Some next -> deadline < epoch.
Proof.
  intros s epoch next amount deadline held accepted.
  unfold step in accepted. rewrite held in accepted.
  match type of accepted with
  | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
  end.
  apply andb_true_iff in checks. destruct checks as [expired _].
  now apply Z.ltb_lt.
Qed.

Theorem fund_nonce : forall s amount deadline sequence epoch next,
  step s (Fund amount deadline sequence epoch) = Some next ->
  sequence = s.(nonce) + 1 /\ next.(nonce) = sequence.
Proof.
  intros [cash locked previous phase] amount deadline sequence epoch next accepted.
  destruct phase; unfold step in accepted; cbn in accepted; try discriminate.
  match type of accepted with
  | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
  end.
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[[[_ _] number] _] _].
  apply Z.eqb_eq in number. inversion accepted; subst. simpl. auto.
Qed.

Definition transact s a fee sequence :=
  if (0 <=? fee) && (fee <=? wallet s) && (sequence =? nonce s + 1)
  then
    let charged := {| wallet := wallet s - fee; vault := vault s;
                      nonce := nonce s; status := status s |} in
    match step charged a with
    | Some next => Some {| wallet := wallet next; vault := vault next;
                          nonce := sequence; status := status next |}
    | None => None
    end
  else None.

Theorem transact_total : forall s a fee sequence next,
  transact s a fee sequence = Some next ->
  wallet next + vault next + fee = wallet s + vault s /\
  nonce next = nonce s + 1.
Proof.
  intros s a fee sequence next accepted. unfold transact in accepted.
  destruct ((0 <=? fee) && (fee <=? wallet s) && (sequence =? nonce s + 1))
    eqn:checks; try discriminate.
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[positive funds] number]. apply Z.eqb_eq in number.
  destruct (step {| wallet := wallet s - fee; vault := vault s;
                   nonce := nonce s; status := status s |} a) as [done|] eqn:executed;
    try discriminate.
  pose proof (step_total _ _ _ executed) as sum.
  inversion accepted; subst; simpl in *. split; lia.
Qed.

Theorem transact_valid : forall s a fee sequence next,
  valid s -> transact s a fee sequence = Some next -> valid next.
Proof.
  intros s a fee sequence next initial accepted. unfold transact in accepted.
  destruct ((0 <=? fee) && (fee <=? wallet s) && (sequence =? nonce s + 1))
    eqn:checks; try discriminate.
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[positive funds] number].
  apply Z.leb_le in positive, funds. apply Z.eqb_eq in number.
  assert (charged : valid {| wallet := wallet s - fee; vault := vault s;
    nonce := nonce s; status := status s |}).
  { unfold valid in *. simpl. intuition lia. }
  destruct (step {| wallet := wallet s - fee; vault := vault s;
                   nonce := nonce s; status := status s |} a) as [done|] eqn:executed;
    try discriminate.
  pose proof (step_valid _ _ _ charged executed) as good.
  inversion accepted; subst. unfold valid in *; simpl in *. intuition lia.
Qed.

Theorem transact_no_repeat : forall s a fee sequence next other cost,
  transact s a fee sequence = Some next ->
  transact next other cost sequence = None.
Proof.
  intros s a fee sequence next other cost accepted.
  unfold transact in accepted.
  destruct ((0 <=? fee) && (fee <=? wallet s) && (sequence =? nonce s + 1));
    try discriminate.
  destruct (step {| wallet := wallet s - fee; vault := vault s;
                   nonce := nonce s; status := status s |} a); try discriminate.
  inversion accepted; subst. unfold transact; simpl.
  assert ((sequence =? sequence + 1) = false) as different by (apply Z.eqb_neq; lia).
  rewrite different, andb_false_r. reflexivity.
Qed.

Definition visible (observed current : Z) (s : state) :=
  (observed =? current) &&
  match s.(status) with
  | Held _ deadline => (0 <=? current) && (current <=? deadline)
  | _ => false
  end.

Theorem visible_funds : forall s observed current,
  valid s -> visible observed current s = true ->
  exists amount deadline, s.(status) = Held amount deadline /\
    observed = current /\ 0 <= current <= deadline /\ 0 < amount <= s.(vault).
Proof.
  intros [cash locked sequence phase] observed current initial view.
  destruct phase; unfold visible in view; cbn in view;
    try (rewrite andb_false_r in view; discriminate).
  repeat rewrite andb_true_iff in view.
  destruct view as [same [nonnegative timely]].
  apply Z.eqb_eq in same. apply Z.leb_le in nonnegative. apply Z.leb_le in timely.
  unfold valid in initial; cbn in initial.
  exists amount, deadline. cbn. tauto.
Qed.

Theorem visible_no_refund : forall s observed current,
  valid s -> visible observed current s = true -> step s (Refund current) = None.
Proof.
  intros s observed current initial view.
  destruct (visible_funds s observed current initial view)
    as [amount [deadline [held [_ [[_ timely] _]]]]].
  unfold step. rewrite held.
  assert ((deadline <? current) = false) as early by (apply Z.ltb_ge; lia).
  rewrite early. reflexivity.
Qed.

Theorem old_read_refused : forall s observed current,
  observed <> current -> visible observed current s = false.
Proof.
  intros s observed current different. unfold visible.
  assert ((observed =? current) = false) as same by (apply Z.eqb_neq; exact different).
  rewrite same. reflexivity.
Qed.

Theorem returned_not_visible : forall s epoch next events observed current,
  step s (Refund epoch) = Some next ->
  visible observed current (trace events next) = false.
Proof.
  intros [cash locked sequence phase] epoch next events observed current accepted.
  destruct phase; unfold step in accepted; cbn in accepted; try discriminate.
  match type of accepted with
  | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
  end.
  inversion accepted; subst; clear accepted.
  induction events as [|[written action] rest induction]; cbn.
  - change ((observed =? current) && false = false). apply andb_false_r.
  - unfold commit. destruct action; cbn; exact induction.
Qed.

Print Assumptions visible_funds.
Print Assumptions visible_no_refund.
Print Assumptions old_read_refused.
Print Assumptions returned_not_visible.

Definition fund_fee (s : state) amount deadline sequence epoch fee :=
  if 0 <=? fee then
    let paid := {| wallet := s.(wallet) - fee; vault := s.(vault);
                   nonce := s.(nonce); status := s.(status) |} in
    match step paid (Fund amount deadline sequence epoch) with
    | Some next => Some (next, fee)
    | None => None
    end
  else None.

Theorem fee_fund : forall s amount deadline sequence epoch fee next charged,
  valid s -> fund_fee s amount deadline sequence epoch fee = Some (next, charged) ->
  valid next /\ next.(status) = Held amount deadline /\
  next.(nonce) = s.(nonce) + 1 /\ next.(vault) = s.(vault) + amount /\
  next.(wallet) + next.(vault) + charged = s.(wallet) + s.(vault) /\
  0 <= charged /\ charged = fee.
Proof.
  intros [cash locked previous phase] amount deadline sequence epoch fee next charged initial accepted.
  unfold fund_fee in accepted; cbn in accepted.
  destruct (0 <=? fee) eqn:paid; try discriminate.
  apply Z.leb_le in paid.
  destruct phase; unfold step in accepted; cbn in accepted; try discriminate.
  match type of accepted with
  | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
  end.
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[[[positive fits] number] now] expires].
  apply Z.ltb_lt in positive. apply Z.leb_le in fits.
  apply Z.eqb_eq in number. apply Z.leb_le in now. apply Z.leb_le in expires.
  inversion accepted; subst. unfold valid in *; cbn in *.
  repeat split; try reflexivity; lia.
Qed.

Theorem fee_refund : forall s amount deadline sequence epoch fee next charged later final,
  valid s -> fund_fee s amount deadline sequence epoch fee = Some (next, charged) ->
  step next (Refund later) = Some final ->
  final.(wallet) = s.(wallet) - fee /\ final.(vault) = s.(vault) /\
  final.(nonce) = s.(nonce) + 1.
Proof.
  intros s amount deadline sequence epoch fee next charged later final initial funded returned.
  destruct (fee_fund s amount deadline sequence epoch fee next charged initial funded)
    as [checked [held [number [locked [total [_ amount_paid]]]]]].
  unfold step in returned. rewrite held in returned.
  match type of returned with
  | context [if ?check then _ else _] => destruct check eqn:checks; try discriminate
  end.
  inversion returned; subst. cbn. repeat split; lia.
Qed.

Print Assumptions fee_fund.
Print Assumptions fee_refund.

End Deposit.

Module Dispatch.

Inductive phase := Planned | Funding | Issued | Stored | Cancelled.

Record job := {
  status : phase;
  starts : nat
}.

Inductive event :=
  | Wait
  | Confirm (observed current source target : Z) (funds : Deposit.state)
  | Finish
  | Cancel
  | Restart.

Definition step (s : job) action :=
  match action, s.(status) with
  | Wait, Planned => {| status := Funding; starts := s.(starts) |}
  | Confirm observed current source target funds, Funding =>
    if Z.eqb source target && Deposit.visible observed current funds
    then {| status := Issued; starts := S s.(starts) |}
    else s
  | Finish, Issued => {| status := Stored; starts := s.(starts) |}
  | Cancel, _ => {| status := Cancelled; starts := s.(starts) |}
  | _, _ => s
  end.

Definition valid s :=
  match s.(status) with
  | Planned | Funding => s.(starts) = 0%nat
  | Issued | Stored => s.(starts) = 1%nat
  | Cancelled => (s.(starts) <= 1)%nat
  end.

Theorem step_valid : forall s action,
  valid s -> valid (step s action).
Proof.
  intros [phase count] action checked.
  destruct action; destruct phase; cbn in *; try lia; try assumption.
  destruct (Z.eqb source target && Deposit.visible observed current funds); cbn; lia.
Qed.

Fixpoint run actions s :=
  match actions with
  | nil => s
  | cons action rest => run rest (step s action)
  end.

Theorem run_valid : forall actions s,
  valid s -> valid (run actions s).
Proof.
  induction actions; intros s checked; cbn; auto using step_valid.
Qed.

Theorem one_start : forall actions,
  (starts (run actions {| status := Planned; starts := 0 |}) <= 1)%nat.
Proof.
  intros actions.
  pose proof (run_valid actions {| status := Planned; starts := 0 |} eq_refl) as checked.
  unfold valid in checked. destruct (status (run actions {| status := Planned; starts := 0 |})); lia.
Qed.

Theorem restart_same : forall s,
  step s Restart = s.
Proof.
  intros [phase count]. destruct phase; reflexivity.
Qed.

Theorem release_no_refund : forall count observed current source target funds,
  Deposit.valid funds ->
  status (step {| status := Funding; starts := count |}
    (Confirm observed current source target funds)) = Issued ->
  Deposit.step funds (Deposit.Refund current) = None.
Proof.
  intros count observed current source target funds checked issued.
  cbn in issued.
  destruct (Z.eqb source target && Deposit.visible observed current funds) eqn:visible;
    try discriminate.
  apply andb_true_iff in visible. destruct visible as [_ visible].
  eapply Deposit.visible_no_refund; eauto.
Qed.

Theorem root_change : forall s observed current source target funds,
  source <> target ->
  step s (Confirm observed current source target funds) = s.
Proof.
  intros [phase count] observed current source target funds different.
  destruct phase; cbn; try reflexivity.
  apply Z.eqb_neq in different. rewrite different. reflexivity.
Qed.

Theorem cancelled_stays : forall actions count,
  run actions {| status := Cancelled; starts := count |} =
  {| status := Cancelled; starts := count |}.
Proof.
  induction actions; intros count; cbn; auto.
  destruct a; cbn; apply IHactions.
Qed.

Print Assumptions one_start.
Print Assumptions restart_same.
Print Assumptions release_no_refund.
Print Assumptions cancelled_stays.
Print Assumptions root_change.

End Dispatch.

Module ComputeSettle.

Import ListNotations.

Record bill := Bill {
  key : nat;
  root : nat;
  refund : Z;
  shares : list Z
}.

Definition work b := fold_right Z.add 0 (shares b).
Definition amount b := refund b + work b.

Definition valid b held :=
  (0 <? held) && (0 <=? refund b) &&
  forallb (fun value => 0 <=? value) (shares b) && (amount b =? held).

Inductive cell := Empty | Held (value : Z) | Paid (value : Z) (hash : nat) | Returned.
Definition book := nat -> cell.

Fixpoint unique (keys : list nat) :=
  match keys with
  | [] => true
  | id :: rest => if in_dec Nat.eq_dec id rest then false else unique rest
  end.

Fixpoint pick (disk : book) (bills : list bill) : option (list bill) :=
  match bills with
  | [] => Some []
  | b :: rest =>
    match disk (key b) with
    | Held value =>
      if valid b value then
        match pick disk rest with Some more => Some (b :: more) | None => None end
      else None
    | Paid _ hash => if Nat.eqb hash (root b) then pick disk rest else None
    | _ => None
    end
  end.

Definition total bills := fold_right (fun b sum => amount b + sum) 0 bills.
Definition refunds bills := fold_right (fun b sum => refund b + sum) 0 bills.
Definition wages bills := fold_right (fun b sum => work b + sum) 0 bills.

Fixpoint mark (disk : book) (bills : list bill) : book :=
  match bills with
  | [] => disk
  | b :: rest =>
    fun id => if Nat.eqb id (key b) then Paid (amount b) (root b) else mark disk rest id
  end.

Record state := State {
  saved : book;
  escrow : Z;
  payer : Z;
  workers : Z
}.

Definition wealth s := escrow s + payer s + workers s.

Definition apply s bills :=
  State (mark (saved s) bills) (escrow s - total bills)
    (payer s + refunds bills) (workers s + wages bills).

Definition plan s bills :=
  if unique (map key bills) then
    match pick (saved s) bills with
    | Some (b :: rest) =>
      if total (b :: rest) <=? escrow s then Some (b :: rest) else None
    | _ => None
    end
  else None.

Definition settle s bills checked stored :=
  if checked && stored then
    match plan s bills with
    | Some unpaid => Some (apply s unpaid, unpaid)
    | None => None
    end
  else None.

Definition commit s bills checked stored :=
  match settle s bills checked stored with Some (next, _) => next | None => s end.

Definition event := (list bill * bool * bool)%type.

Fixpoint run s (events : list event) :=
  match events with
  | [] => s
  | (bills, checked, stored) :: rest => run (commit s bills checked stored) rest
  end.

Example suffix_only :
  let first := Bill 7 11 3 [2; 5] in
  let second := Bill 8 12 4 [6] in
  let disk id := if Nat.eqb id 7 then Paid 10 11
    else if Nat.eqb id 8 then Held 10 else Empty in
  match settle (State disk 10 0 0) [first; second] true true with
  | Some (next, unpaid) => unpaid = [second] /\ escrow next = 0 /\
    payer next = 4 /\ workers next = 6 /\ saved next 7%nat = Paid 10 11 /\
    saved next 8%nat = Paid 10 12
  | None => False
  end.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example all_paid_refused :
  let b := Bill 7 11 3 [2; 5] in
  settle (State (fun _ => Paid 10 11) 10 0 0) [b] true true = None.
Proof. vm_compute. reflexivity. Qed.

Lemma unique_spec : forall keys, unique keys = true <-> NoDup keys.
Proof.
  induction keys as [|id rest ih]; simpl; [split; auto using NoDup_nil|].
  destruct (in_dec Nat.eq_dec id rest) as [present|absent].
  - split; [discriminate|]. intro once. inversion once. contradiction.
  - rewrite ih. split; [constructor; assumption|]. intro once. inversion once; assumption.
Qed.

Lemma sum_nonnegative : forall values,
  forallb (fun value => 0 <=? value) values = true ->
  0 <= fold_right Z.add 0 values.
Proof.
  induction values as [|value rest ih]; simpl; [lia|].
  rewrite andb_true_iff, Z.leb_le. intros [positive checked].
  specialize (ih checked). lia.
Qed.

Lemma valid_amount : forall b held,
  valid b held = true ->
  0 < held /\ 0 <= refund b /\ 0 <= work b /\ amount b = held.
Proof.
  intros b held checked. unfold valid in checked.
  repeat rewrite andb_true_iff in checked.
  destruct checked as [[[positive back] parts] same].
  apply Z.ltb_lt in positive. apply Z.leb_le in back. apply Z.eqb_eq in same.
  pose proof (sum_nonnegative _ parts). unfold work. tauto.
Qed.

Lemma pick_source : forall bills disk unpaid,
  pick disk bills = Some unpaid ->
  forall b, In b unpaid -> In b bills /\
    exists held, disk (key b) = Held held /\ valid b held = true.
Proof.
  induction bills as [|b rest ih]; intros disk unpaid accepted item present; simpl in accepted.
  - inversion accepted; subst. contradiction.
  - destruct (disk (key b)) as [|value|value hash|] eqn:found; try discriminate.
    + destruct (valid b value) eqn:checked; try discriminate.
      destruct (pick disk rest) as [more|] eqn:next; try discriminate.
      inversion accepted; subst. destruct present as [same|present].
      * subst item. split; [left; reflexivity|]. exists value. auto.
      * destruct (ih _ _ next _ present) as [included held].
        split; [right; exact included|exact held].
    + destruct (Nat.eqb hash (root b)); try discriminate.
      destruct (ih _ _ accepted _ present) as [included held].
      split; [right; exact included|exact held].
Qed.

Lemma pick_permits : forall bills disk unpaid,
  pick disk bills = Some unpaid ->
  forall b, In b bills ->
    (exists held, disk (key b) = Held held /\ valid b held = true) \/
    (exists held, disk (key b) = Paid held (root b)).
Proof.
  induction bills as [|b rest ih]; intros disk unpaid accepted item present; simpl in accepted.
  - contradiction.
  - destruct (disk (key b)) as [|value|value hash|] eqn:found; try discriminate.
    + destruct (valid b value) eqn:checked; try discriminate.
      destruct (pick disk rest) as [more|] eqn:next; try discriminate.
      destruct present as [same|present].
      * subst item. left. exists value. auto.
      * eapply ih; eauto.
    + destruct (Nat.eqb hash (root b)) eqn:same; try discriminate.
      apply Nat.eqb_eq in same. subst hash.
      destruct present as [equal|present].
      * subst item. right. exists value. exact found.
      * eapply ih; eauto.
Qed.

Lemma pick_app : forall first last disk,
  pick disk (first ++ last) =
  match pick disk first with
  | Some front =>
    match pick disk last with Some back => Some (front ++ back) | None => None end
  | None => None
  end.
Proof.
  induction first as [|b rest ih]; intros last disk; simpl.
  - destruct (pick disk last); reflexivity.
  - destruct (disk (key b)); try reflexivity.
    + destruct (valid b value); try reflexivity. rewrite ih.
      destruct (pick disk rest); [destruct (pick disk last)|]; reflexivity.
    + destruct (Nat.eqb hash (root b)); [apply ih|reflexivity].
Qed.

Lemma pick_closed : forall bills disk,
  Forall (fun b => exists held, disk (key b) = Paid held (root b)) bills ->
  pick disk bills = Some [].
Proof.
  intros bills disk checked. induction checked as [|b rest [held found] checked ih]; simpl.
  - reflexivity.
  - rewrite found, Nat.eqb_refl. exact ih.
Qed.

Lemma pick_open : forall bills disk,
  Forall (fun b => exists held, disk (key b) = Held held /\ valid b held = true) bills ->
  pick disk bills = Some bills.
Proof.
  intros bills disk checked. induction checked as [|b rest [held [found valid]] checked ih]; simpl.
  - reflexivity.
  - rewrite found, valid, ih. reflexivity.
Qed.

Lemma mark_other : forall bills disk id,
  ~ In id (map key bills) -> mark disk bills id = disk id.
Proof.
  induction bills as [|b rest ih]; intros disk id absent; simpl in *; [reflexivity|].
  destruct (Nat.eqb id (key b)) eqn:same.
  - apply Nat.eqb_eq in same. exfalso. apply absent. auto.
  - apply ih. intro present. apply absent. auto.
Qed.

Lemma mark_member : forall bills disk b,
  NoDup (map key bills) -> In b bills ->
  mark disk bills (key b) = Paid (amount b) (root b).
Proof.
  induction bills as [|item rest ih]; intros disk b once present; [contradiction|].
  inversion once as [|id keys absent unique]; subst. simpl in present.
  simpl. destruct present as [same|present].
  - subst item. rewrite Nat.eqb_refl. reflexivity.
  - destruct (Nat.eqb (key b) (key item)) eqn:same.
    + apply Nat.eqb_eq in same. exfalso. apply absent. rewrite <- same. apply in_map. exact present.
    + apply ih; assumption.
Qed.

Lemma pick_unique : forall bills disk unpaid,
  NoDup (map key bills) -> pick disk bills = Some unpaid -> NoDup (map key unpaid).
Proof.
  induction bills as [|b rest ih]; intros disk unpaid once accepted; simpl in accepted.
  - inversion accepted; constructor.
  - inversion once as [|id keys absent distinct]; subst.
    destruct (disk (key b)); try discriminate.
    + destruct (valid b value); try discriminate.
      destruct (pick disk rest) as [more|] eqn:next; try discriminate.
      inversion accepted; subst. simpl. constructor.
      * intro present. apply in_map_iff in present as [item [same included]].
        apply absent. rewrite <- same. apply in_map.
        exact (proj1 (pick_source _ _ _ next item included)).
      * eapply ih; eauto.
    + destruct (Nat.eqb hash (root b)); try discriminate. eapply ih; eauto.
Qed.

Lemma plan_source : forall s bills unpaid,
  plan s bills = Some unpaid ->
  unpaid <> [] /\ NoDup (map key unpaid) /\ total unpaid <= escrow s /\
  pick (saved s) bills = Some unpaid.
Proof.
  intros s bills unpaid accepted. unfold plan in accepted.
  destruct (unique (map key bills)) eqn:once; try discriminate.
  apply unique_spec in once.
  destruct (pick (saved s) bills) as [[|b rest]|] eqn:picked; try discriminate.
  destruct (total (b :: rest) <=? escrow s) eqn:enough; try discriminate.
  apply Z.leb_le in enough. inversion accepted; subst. repeat split; try assumption; try discriminate.
  eapply pick_unique; eauto.
Qed.

Lemma settle_source : forall s bills checked stored next unpaid,
  settle s bills checked stored = Some (next, unpaid) ->
  checked = true /\ stored = true /\ next = apply s unpaid /\ plan s bills = Some unpaid.
Proof.
  intros s bills checked stored next unpaid accepted. unfold settle in accepted.
  destruct (checked && stored) eqn:ready; try discriminate.
  apply andb_true_iff in ready as [verified written].
  destruct (plan s bills) as [values|] eqn:planned; try discriminate.
  inversion accepted; subst. repeat split; reflexivity.
Qed.

Lemma total_split : forall bills, total bills = refunds bills + wages bills.
Proof.
  induction bills as [|b rest ih]; simpl; [reflexivity|]. unfold amount. lia.
Qed.

Theorem apply_conserves : forall s bills, wealth (apply s bills) = wealth s.
Proof.
  intros. unfold wealth, apply. simpl. rewrite total_split. ring.
Qed.

Theorem settle_conserves : forall s bills checked stored next unpaid,
  settle s bills checked stored = Some (next, unpaid) -> wealth next = wealth s.
Proof.
  intros s bills checked stored next unpaid accepted.
  apply settle_source in accepted as [_ [_ [same _]]]. subst. apply apply_conserves.
Qed.

Lemma pick_money : forall bills disk unpaid,
  pick disk bills = Some unpaid ->
  0 <= refunds unpaid /\ 0 <= wages unpaid /\ 0 <= total unpaid.
Proof.
  intros bills disk unpaid accepted.
  assert (Forall (fun b => 0 <= refund b /\ 0 <= work b) unpaid) as positive.
  { apply Forall_forall. intros b present.
    destruct (pick_source _ _ _ accepted b present) as [_ [held [_ checked]]].
    destruct (valid_amount _ _ checked) as [_ [back [parts _]]]. auto. }
  clear accepted. induction positive as [|b rest [back parts] all ih].
  - cbv [refunds wages total fold_right]. repeat split; lia.
  - change (0 <= refund b + refunds rest /\ 0 <= work b + wages rest /\
      0 <= amount b + total rest).
    unfold amount. intuition lia.
Qed.

Theorem settle_nonnegative : forall s bills checked stored next unpaid,
  0 <= escrow s -> 0 <= payer s -> 0 <= workers s ->
  settle s bills checked stored = Some (next, unpaid) ->
  0 <= escrow next /\ 0 <= payer next /\ 0 <= workers next.
Proof.
  intros s bills checked stored next unpaid funds cash income accepted.
  destruct (settle_source _ _ _ _ _ _ accepted) as [_ [_ [same planned]]].
  destruct (plan_source _ _ _ planned) as [_ [_ [enough picked]]].
  pose proof (pick_money _ _ _ picked) as money.
  subst next. unfold apply. simpl. intuition lia.
Qed.

Theorem payment_source : forall s bills checked stored next unpaid b,
  settle s bills checked stored = Some (next, unpaid) -> In b unpaid ->
  saved s (key b) = Held (amount b) /\ 0 < amount b /\
  0 <= refund b /\ 0 <= work b.
Proof.
  intros s bills checked stored next unpaid b accepted present.
  destruct (settle_source _ _ _ _ _ _ accepted) as [_ [_ [_ planned]]].
  destruct (plan_source _ _ _ planned) as [_ [_ [_ picked]]].
  destruct (pick_source _ _ _ picked b present) as [_ [held [found valid]]].
  destruct (valid_amount _ _ valid) as [positive [back [parts same]]].
  rewrite same. auto.
Qed.

Theorem payment_totals : forall s bills checked stored next unpaid,
  settle s bills checked stored = Some (next, unpaid) ->
  escrow s - escrow next = total unpaid /\
  payer next - payer s = refunds unpaid /\
  workers next - workers s = wages unpaid /\
  total unpaid = refunds unpaid + wages unpaid.
Proof.
  intros s bills checked stored next unpaid accepted.
  destruct (settle_source _ _ _ _ _ _ accepted) as [_ [_ [same _]]].
  subst next. simpl. rewrite total_split. repeat split; ring.
Qed.

Lemma mark_closed : forall bills disk id value hash,
  (forall b, In b bills -> exists held, disk (key b) = Held held) ->
  disk id = Paid value hash -> mark disk bills id = disk id.
Proof.
  intros bills disk id value hash open paid. apply mark_other.
  intro present. apply in_map_iff in present as [b [same included]].
  destruct (open b included) as [held found]. rewrite same, paid in found. discriminate.
Qed.

Theorem paid_kept : forall s bills checked stored next unpaid id value hash,
  settle s bills checked stored = Some (next, unpaid) ->
  saved s id = Paid value hash -> saved next id = Paid value hash.
Proof.
  intros s bills checked stored next unpaid id value hash accepted paid.
  destruct (settle_source _ _ _ _ _ _ accepted) as [_ [_ [same planned]]].
  destruct (plan_source _ _ _ planned) as [_ [_ [_ picked]]].
  subst next. simpl. rewrite (mark_closed unpaid (saved s) id value hash); auto.
  intros b present. destruct (pick_source _ _ _ picked b present) as [_ [held [found _]]].
  eauto.
Qed.

Theorem paid_history : forall events s id value hash,
  saved s id = Paid value hash -> saved (run s events) id = Paid value hash.
Proof.
  induction events as [|[[bills checked] stored] rest ih]; intros s id value hash paid; simpl.
  - exact paid.
  - apply ih. unfold commit.
    destruct (settle s bills checked stored) as [[next unpaid]|] eqn:accepted; [|exact paid].
    eapply paid_kept; eauto.
Qed.

Theorem payment_marked : forall s bills checked stored next unpaid b,
  settle s bills checked stored = Some (next, unpaid) -> In b unpaid ->
  saved next (key b) = Paid (amount b) (root b).
Proof.
  intros s bills checked stored next unpaid b accepted present.
  destruct (settle_source _ _ _ _ _ _ accepted) as [_ [_ [same planned]]].
  destruct (plan_source _ _ _ planned) as [_ [once _]].
  subst next. simpl. apply mark_member; assumption.
Qed.

Theorem payment_once : forall s bills checked stored next unpaid b events
  more verified written final paid,
  settle s bills checked stored = Some (next, unpaid) -> In b unpaid ->
  settle (run next events) more verified written = Some (final, paid) ->
  ~ In (key b) (map key paid).
Proof.
  intros s bills checked stored next unpaid b events more verified written final paid
    first present second repeated.
  pose proof (payment_marked _ _ _ _ _ _ _ first present) as marked.
  pose proof (paid_history events next _ _ _ marked) as kept.
  destruct (settle_source _ _ _ _ _ _ second) as [_ [_ [_ planned]]].
  destruct (plan_source _ _ _ planned) as [_ [_ [_ picked]]].
  apply in_map_iff in repeated as [item [same included]].
  destruct (pick_source _ _ _ picked item included) as [_ [held [found _]]].
  rewrite same, kept in found. discriminate.
Qed.

Theorem trace_conserves : forall events s, wealth (run s events) = wealth s.
Proof.
  induction events as [|[[bills checked] stored] rest ih]; intros s; simpl; [reflexivity|].
  rewrite ih. unfold commit.
  destruct (settle s bills checked stored) as [[next unpaid]|] eqn:accepted; [|reflexivity].
  eapply settle_conserves; eauto.
Qed.

Theorem unpaid_suffix : forall s prefix suffix,
  Forall (fun b => exists held, saved s (key b) = Paid held (root b)) prefix ->
  Forall (fun b => exists held, saved s (key b) = Held held /\ valid b held = true) suffix ->
  NoDup (map key (prefix ++ suffix)) -> suffix <> [] -> total suffix <= escrow s ->
  settle s (prefix ++ suffix) true true = Some (apply s suffix, suffix).
Proof.
  intros s prefix suffix closed open once nonempty enough.
  assert (unique (map key (prefix ++ suffix)) = true) as distinct by (apply unique_spec; exact once).
  assert (plan s (prefix ++ suffix) = Some suffix) as planned.
  { unfold plan. rewrite distinct, pick_app.
    rewrite (pick_closed _ _ closed), (pick_open _ _ open).
    destruct suffix as [|b rest]; [contradiction|].
    change ((if total (b :: rest) <=? escrow s then Some (b :: rest) else None) = Some (b :: rest)).
    apply Z.leb_le in enough. rewrite enough. reflexivity. }
  unfold settle. rewrite planned. reflexivity.
Qed.

Theorem prefix_kept : forall s prefix suffix next unpaid,
  Forall (fun b => exists held, saved s (key b) = Paid held (root b)) prefix ->
  settle s (prefix ++ suffix) true true = Some (next, unpaid) ->
  forall b, In b prefix -> saved next (key b) = saved s (key b).
Proof.
  intros s prefix suffix next unpaid closed accepted b present.
  rewrite Forall_forall in closed. destruct (closed b present) as [held paid].
  rewrite paid. eapply paid_kept; eauto.
Qed.

Theorem repeat_refused : forall s bills checked stored next unpaid,
  settle s bills checked stored = Some (next, unpaid) ->
  forall verified written, settle next unpaid verified written = None.
Proof.
  intros s bills checked stored next unpaid accepted verified written.
  assert (Forall (fun b => exists held, saved next (key b) = Paid held (root b)) unpaid) as closed.
  { apply Forall_forall. intros b present. exists (amount b). eapply payment_marked; eauto. }
  unfold settle, plan. destruct (verified && written); [|reflexivity].
  destruct (unique (map key unpaid)); [|reflexivity].
  rewrite (pick_closed _ _ closed). reflexivity.
Qed.

Theorem wrong_root_refused : forall s bills b value hash checked stored,
  In b bills -> saved s (key b) = Paid value hash -> hash <> root b ->
  settle s bills checked stored = None.
Proof.
  intros s bills b value hash checked stored present paid different.
  destruct (settle s bills checked stored) as [[next unpaid]|] eqn:accepted; [|reflexivity].
  destruct (settle_source _ _ _ _ _ _ accepted) as [_ [_ [_ planned]]].
  destruct (plan_source _ _ _ planned) as [_ [_ [_ picked]]].
  destruct (pick_permits _ _ _ picked b present) as [[held [found _]]|[held found]].
  - rewrite paid in found. discriminate.
  - rewrite paid in found. inversion found. contradiction.
Qed.

Theorem duplicate_refused : forall s bills checked stored,
  ~ NoDup (map key bills) -> settle s bills checked stored = None.
Proof.
  intros s bills checked stored repeated. unfold settle, plan.
  destruct (checked && stored); [|reflexivity].
  destruct (unique (map key bills)) eqn:once; [|reflexivity].
  apply unique_spec in once. contradiction.
Qed.

Theorem failed_write : forall s bills checked, commit s bills checked false = s.
Proof.
  intros. unfold commit, settle. rewrite andb_false_r. reflexivity.
Qed.

Theorem unchecked_refused : forall s bills stored, commit s bills false stored = s.
Proof. reflexivity. Qed.

Example wrong_sum_refused :
  let b := Bill 7 11 3 [2; 5] in
  settle (State (fun _ => Held 11) 11 0 0) [b] true true = None.
Proof. vm_compute. reflexivity. Qed.

Example negative_share_refused :
  let b := Bill 7 11 3 [-2; 9] in
  settle (State (fun _ => Held 10) 10 0 0) [b] true true = None.
Proof. vm_compute. reflexivity. Qed.

Example conflict_refused :
  let first := Bill 7 11 3 [2; 5] in
  let second := Bill 8 12 4 [6] in
  let disk id := if Nat.eqb id 7 then Paid 10 13 else Held 10 in
  settle (State disk 10 0 0) [first; second] true true = None.
Proof. vm_compute. reflexivity. Qed.

Example repeat_key_refused :
  let b := Bill 7 11 3 [2; 5] in
  settle (State (fun _ => Held 10) 20 0 0) [b; b] true true = None.
Proof. vm_compute. reflexivity. Qed.

Example short_escrow_refused :
  let b := Bill 7 11 3 [2; 5] in
  settle (State (fun _ => Held 10) 9 0 0) [b] true true = None.
Proof. vm_compute. reflexivity. Qed.

Print Assumptions settle_conserves.
Print Assumptions settle_nonnegative.
Print Assumptions payment_source.
Print Assumptions payment_totals.
Print Assumptions paid_history.
Print Assumptions payment_marked.
Print Assumptions payment_once.
Print Assumptions trace_conserves.
Print Assumptions unpaid_suffix.
Print Assumptions prefix_kept.
Print Assumptions repeat_refused.
Print Assumptions wrong_root_refused.
Print Assumptions duplicate_refused.
Print Assumptions failed_write.
Print Assumptions unchecked_refused.

End ComputeSettle.

Module PayInput.

Import ListNotations.

Definition data := list Z.
Definition table := nat -> option data.

Definition put (rows : table) (key : nat) (value : data) : table :=
  fun index => if Nat.eqb index key then Some value else rows index.

Definition retain (history saved : table) (epoch key : nat) : option table :=
  match saved key, history epoch with
  | None, Some value => Some (put saved key value)
  | _, _ => None
  end.

Theorem retained_exact : forall history saved epoch key next,
  retain history saved epoch key = Some next ->
  exists value, history epoch = Some value /\ next key = Some value.
Proof.
  intros history saved epoch key next accepted. unfold retain in accepted.
  destruct (saved key); [discriminate|].
  destruct (history epoch) as [value|] eqn:found; [|discriminate].
  inversion accepted; subst. exists value. split; [reflexivity|].
  unfold put. rewrite Nat.eqb_refl. reflexivity.
Qed.

Theorem retained_immutable : forall history saved epoch key next index value,
  retain history saved epoch key = Some next -> saved index = Some value ->
  next index = Some value.
Proof.
  intros history saved epoch key next index value accepted present.
  unfold retain in accepted. destruct (saved key) eqn:empty; [discriminate|].
  destruct (history epoch); [|discriminate]. inversion accepted; subst.
  unfold put. destruct (Nat.eqb index key) eqn:same; [|exact present].
  apply Nat.eqb_eq in same. subst. rewrite empty in present. discriminate.
Qed.

Theorem missing_refused : forall history saved epoch key,
  history epoch = None -> retain history saved epoch key = None.
Proof.
  intros. unfold retain. rewrite H. destruct (saved key); reflexivity.
Qed.

Definition disk := (table * table)%type.
Definition prune (s : disk) (epoch : nat) : disk :=
  ((fun index => if Nat.eqb index epoch then None else fst s index), snd s).

Fixpoint trim (epochs : list nat) (s : disk) : disk :=
  match epochs with [] => s | epoch :: rest => trim rest (prune s epoch) end.

Theorem prune_keeps_inputs : forall epochs s key value,
  snd s key = Some value -> snd (trim epochs s) key = Some value.
Proof.
  induction epochs as [|epoch rest ih]; intros s key value found; simpl; auto.
Qed.

Record account := Account { funds : Z; fees : Z; nonce : nat }.
Inductive verdict := Invalid | Unavailable.

Definition charge (s : account) (n : nat) (fee : Z) (v : verdict) : account :=
  if (Nat.eqb n (S (nonce s)) && (0 <=? fee) && (fee <=? funds s))%bool then
    match v with
    | Invalid => Account (funds s - fee) (fees s + fee) n
    | Unavailable => s
    end
  else s.

Theorem fee_conserves : forall s n fee v,
  funds (charge s n fee v) + fees (charge s n fee v) = funds s + fees s.
Proof.
  intros. unfold charge. destruct (_ && _ && _)%bool; [destruct v|]; simpl; lia.
Qed.

Theorem fee_nonnegative : forall s n fee v,
  0 <= funds s -> 0 <= funds (charge s n fee v).
Proof.
  intros s n fee v enough. unfold charge.
  destruct (Nat.eqb n (S (nonce s)) && (0 <=? fee) && (fee <=? funds s))%bool eqn:valid.
  - apply andb_true_iff in valid as [_ valid]. apply Z.leb_le in valid.
    destruct v; simpl; lia.
  - exact enough.
Qed.

Theorem refusal_consumes_nonce : forall s n fee,
  n = S (nonce s) -> 0 <= fee <= funds s ->
  charge s n fee Invalid = Account (funds s - fee) (fees s + fee) n.
Proof.
  intros s n fee same [positive enough]. unfold charge.
  rewrite same, Nat.eqb_refl.
  apply Z.leb_le in positive. apply Z.leb_le in enough.
  rewrite positive, enough. reflexivity.
Qed.

Theorem refusal_cannot_repeat : forall s n fee,
  n = S (nonce s) -> 0 <= fee <= funds s ->
  charge (charge s n fee Invalid) n fee Invalid = charge s n fee Invalid.
Proof.
  intros s n fee same enough.
  rewrite (refusal_consumes_nonce s n fee same enough).
  unfold charge. simpl. destruct (Nat.eqb n (S n)) eqn:equal; [|reflexivity].
  apply Nat.eqb_eq in equal. lia.
Qed.

Theorem host_failure_unchanged : forall s n fee,
  charge s n fee Unavailable = s.
Proof.
  intros. unfold charge. destruct (_ && _ && _)%bool; reflexivity.
Qed.

Print Assumptions retained_exact.
Print Assumptions retained_immutable.
Print Assumptions missing_refused.
Print Assumptions prune_keeps_inputs.
Print Assumptions fee_conserves.
Print Assumptions fee_nonnegative.
Print Assumptions refusal_consumes_nonce.
Print Assumptions refusal_cannot_repeat.
Print Assumptions host_failure_unchanged.

End PayInput.