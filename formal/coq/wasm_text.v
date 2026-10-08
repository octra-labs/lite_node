(* SPDX-License-Identifier: BSD-3-Clause *)

From Stdlib Require Import List Arith Lia Bool.
Import ListNotations.

Definition span lo hi x := (lo <=? x) && (x <=? hi).

Definition test xs :=
  match xs with
  | [a] => a <=? 127
  | [a; b] => span 194 223 a && span 128 191 b
  | [a; b; c] =>
      (((a =? 224) && span 160 191 b) ||
       (span 225 236 a && span 128 191 b) ||
       ((a =? 237) && span 128 159 b) ||
       (span 238 239 a && span 128 191 b)) && span 128 191 c
  | [a; b; c; d] =>
      (((a =? 240) && span 144 191 b) ||
       (span 241 243 a && span 128 191 b) ||
       ((a =? 244) && span 128 143 b)) &&
      span 128 191 c && span 128 191 d
  | _ => false
  end.

Definition scalar xs :=
  match xs with
  | [a] => a <= 127
  | [a; b] => 194 <= a <= 223 /\ 128 <= b <= 191
  | [a; b; c] =>
      (a = 224 /\ 160 <= b <= 191 \/
       225 <= a <= 236 /\ 128 <= b <= 191 \/
       a = 237 /\ 128 <= b <= 159 \/
       238 <= a <= 239 /\ 128 <= b <= 191) /\ 128 <= c <= 191
  | [a; b; c; d] =>
      (a = 240 /\ 144 <= b <= 191 \/
       241 <= a <= 243 /\ 128 <= b <= 191 \/
       a = 244 /\ 128 <= b <= 143) /\
      128 <= c <= 191 /\ 128 <= d <= 191
  | _ => False
  end.

Inductive utf8 : list nat -> Prop :=
| utf8_nil : utf8 []
| utf8_cons : forall s xs, scalar s -> utf8 xs -> utf8 (s ++ xs).

Lemma test_ok : forall xs, test xs = true <-> scalar xs.
Proof.
  intros [|a [|b [|c [|d [|e xs]]]]]; unfold test, scalar, span;
    repeat rewrite andb_true_iff;
    repeat rewrite orb_true_iff;
    repeat rewrite andb_true_iff;
    repeat rewrite Nat.leb_le;
    repeat rewrite Nat.eqb_eq; intuition discriminate.
Qed.

Lemma scalar_size : forall s, scalar s -> 1 <= length s <= 4.
Proof.
  intros [|a [|b [|c [|d [|e s]]]]]; cbn [scalar length]; intuition lia.
Qed.

Definition decode xs :=
  match xs with
  | [] => None
  | a :: ys =>
      if test [a] then Some ([a], ys)
      else match ys with
      | [] => None
      | b :: zs =>
          if test [a; b] then Some ([a; b], zs)
          else match zs with
          | [] => None
          | c :: ws =>
              if test [a; b; c] then Some ([a; b; c], ws)
              else match ws with
              | [] => None
              | d :: vs =>
                  if test [a; b; c; d] then Some ([a; b; c; d], vs)
                  else None
              end
          end
      end
  end.

Lemma decode_ok : forall xs s ys,
  decode xs = Some (s, ys) -> scalar s /\ xs = s ++ ys.
Proof.
  intros [|a xs] s ys h; [discriminate|].
  unfold decode in h.
  destruct (test [a]) eqn:p.
  - inversion h; subst. split; [apply test_ok; exact p|reflexivity].
  - destruct xs as [|b xs]; [discriminate|].
    destruct (test [a; b]) eqn:q.
    + inversion h; subst. split; [apply test_ok; exact q|reflexivity].
    + destruct xs as [|c xs]; [discriminate|].
      destruct (test [a; b; c]) eqn:r.
      * inversion h; subst. split; [apply test_ok; exact r|reflexivity].
      * destruct xs as [|d xs]; [discriminate|].
        destruct (test [a; b; c; d]) eqn:t; [|discriminate].
        inversion h; subst. split; [apply test_ok; exact t|reflexivity].
Qed.

Lemma decode_scalar : forall s xs,
  scalar s -> decode (s ++ xs) = Some (s, xs).
Proof.
  intros s xs h. pose proof (proj2 (test_ok s) h) as p.
  destruct s as [|a [|b [|c [|d [|e s]]]]];
    cbn [scalar] in h; try contradiction.
  - cbn [app decode]. rewrite p. reflexivity.
  - assert (q : test [a] = false).
    { unfold test. apply Nat.leb_gt. lia. }
    cbn [app decode]. rewrite q, p. reflexivity.
  - assert (q : test [a] = false).
    { unfold test. apply Nat.leb_gt. intuition lia. }
    assert (r : test [a; b] = false).
    { unfold test, span. apply andb_false_iff. left.
      apply andb_false_iff. right. apply Nat.leb_gt. intuition lia. }
    cbn [app decode]. rewrite q, r, p. reflexivity.
  - assert (q : test [a] = false).
    { unfold test. apply Nat.leb_gt. intuition lia. }
    assert (r : test [a; b] = false).
    { unfold test, span. apply andb_false_iff. left.
      apply andb_false_iff. right. apply Nat.leb_gt. intuition lia. }
    assert (t : test [a; b; c] = false).
    { apply not_true_is_false. intro t. apply test_ok in t.
      cbn [scalar] in t. intuition lia. }
    cbn [app decode]. rewrite q, r, t, p. reflexivity.
Qed.

Theorem decode_none : forall xs,
  decode xs = None <-> ~ exists s ys, scalar s /\ xs = s ++ ys.
Proof.
  intro xs. split.
  - intros h [s [ys [p q]]]. subst xs.
    rewrite (decode_scalar s ys p) in h. discriminate.
  - intro h. destruct (decode xs) as [[s ys]|] eqn:p; [|reflexivity].
    exfalso. apply h. exists s, ys. apply decode_ok. exact p.
Qed.

Fixpoint scan fuel cap xs :=
  match fuel, cap, xs with
  | S n, S room, a :: ys =>
      match decode (a :: ys) with
      | Some (s, zs) =>
          if length s <=? S room
          then s ++ scan n (S room - length s) zs
          else []
      | None => 63 :: scan n room ys
      end
  | _, _, _ => []
  end.

Definition clean cap xs := scan cap cap xs.
Definition text xs := clean 256 xs.

Lemma scan_valid : forall fuel cap xs, utf8 (scan fuel cap xs).
Proof.
  induction fuel as [|n ih]; intros [|room] [|a xs];
    cbn [scan]; try constructor.
  destruct (decode (a :: xs)) as [[s ys]|] eqn:p.
  - destruct (length s <=? S room); [|constructor].
    apply utf8_cons; [apply (proj1 (decode_ok _ _ _ p))|apply ih].
  - change (utf8 ([63] ++ scan n room xs)).
    apply utf8_cons; [unfold scalar; lia|apply ih].
Qed.

Lemma scan_cap : forall fuel cap xs, length (scan fuel cap xs) <= cap.
Proof.
  induction fuel as [|n ih]; intros [|room] [|a xs];
    cbn [scan length]; try lia.
  destruct (decode (a :: xs)) as [[s ys]|] eqn:p.
  - destruct (length s <=? S room) eqn:q; [|cbn; lia].
    rewrite length_app. apply Nat.leb_le in q.
    specialize (ih (S room - length s) ys). lia.
  - cbn [length]. specialize (ih room xs). lia.
Qed.

Lemma scan_agree : forall n m cap xs,
  cap <= n -> cap <= m -> scan n cap xs = scan m cap xs.
Proof.
  induction n as [|n ih]; intros m cap xs hn hm.
  - assert (cap = 0) by lia. subst. destruct m; reflexivity.
  - destruct cap as [|room]; [destruct m; reflexivity|].
    destruct m as [|m]; [lia|]. destruct xs as [|a xs]; [reflexivity|].
    cbn [scan]. destruct (decode (a :: xs)) as [[s ys]|] eqn:p.
    + destruct (length s <=? S room); [|reflexivity].
      pose proof (scalar_size s (proj1 (decode_ok _ _ _ p))) as q.
      f_equal. apply ih; lia.
    + f_equal. apply ih; lia.
Qed.

Theorem clean_step : forall room a xs,
  clean (S room) (a :: xs) =
  match decode (a :: xs) with
  | Some (s, ys) =>
      if length s <=? S room
      then s ++ clean (S room - length s) ys
      else []
  | None => 63 :: clean room xs
  end.
Proof.
  intros room a xs. unfold clean. cbn [scan].
  destruct (decode (a :: xs)) as [[s ys]|] eqn:p; [|reflexivity].
  destruct (length s <=? S room); [|reflexivity].
  pose proof (scalar_size s (proj1 (decode_ok _ _ _ p))) as q.
  f_equal. apply scan_agree; lia.
Qed.

Theorem clean_bad : forall room a xs,
  decode (a :: xs) = None ->
  clean (S room) (a :: xs) = 63 :: clean room xs.
Proof. intros room a xs h. rewrite clean_step, h. reflexivity. Qed.

Theorem clean_cut : forall cap xs s ys,
  decode xs = Some (s, ys) -> cap < length s -> clean cap xs = [].
Proof.
  intros [|room] [|a xs] s ys h p; try discriminate; try reflexivity.
  rewrite clean_step, h.
  assert (q : (length s <=? S room) = false) by (apply Nat.leb_gt; lia).
  rewrite q. reflexivity.
Qed.

Lemma scan_id : forall xs, utf8 xs -> forall fuel cap,
  length xs <= fuel -> length xs <= cap -> scan fuel cap xs = xs.
Proof.
  intros xs h. induction h as [|s xs hs hx ih]; intros fuel cap hf hc.
  - destruct fuel, cap; reflexivity.
  - pose proof (scalar_size s hs) as p.
    rewrite length_app in hf, hc.
    destruct fuel as [|n]; [lia|]. destruct cap as [|room]; [lia|].
    destruct s as [|a s]; [cbn in p; lia|].
    pose proof (decode_scalar (a :: s) xs hs) as d.
    cbn [app] in d. cbn [app scan]. rewrite d.
    assert (q : (length (a :: s) <=? S room) = true).
    { apply Nat.leb_le. lia. }
    rewrite q. rewrite ih; [reflexivity|lia|lia].
Qed.

Theorem clean_valid : forall cap xs, utf8 (clean cap xs).
Proof. intros. apply scan_valid. Qed.

Theorem clean_cap : forall cap xs, length (clean cap xs) <= cap.
Proof. intros. apply scan_cap. Qed.

Theorem clean_id : forall cap xs,
  utf8 xs -> length xs <= cap -> clean cap xs = xs.
Proof. intros cap xs h p. apply scan_id; assumption. Qed.

Theorem clean_repeat : forall cap xs,
  clean cap (clean cap xs) = clean cap xs.
Proof. intros. apply clean_id; [apply clean_valid|apply clean_cap]. Qed.

Theorem text_valid : forall xs, utf8 (text xs).
Proof. intros. apply clean_valid. Qed.

Theorem text_cap : forall xs, length (text xs) <= 256.
Proof. intros. apply clean_cap. Qed.

Theorem text_id : forall xs,
  utf8 xs -> length xs <= 256 -> text xs = xs.
Proof. intros xs h p. apply clean_id; assumption. Qed.

Example scalar_edges :
  map test [[0]; [127]; [194; 128]; [223; 191];
    [224; 160; 128]; [237; 159; 191]; [238; 128; 128]; [239; 191; 191];
    [240; 144; 128; 128]; [244; 143; 191; 191]] = repeat true 10.
Proof. vm_compute. reflexivity. Qed.

Example scalar_reject :
  map test [[128]; [192; 128]; [193; 191]; [224; 128; 128];
    [237; 160; 128]; [240; 128; 128; 128]; [244; 144; 128; 128];
    [245; 128; 128; 128]; [256]; [194]; [226; 130];
    [240; 144; 128]; [194; 256]] = repeat false 13.
Proof. vm_compute. reflexivity. Qed.

Example valid_copy :
  text [65; 194; 162; 226; 130; 172; 240; 159; 152; 128] =
    [65; 194; 162; 226; 130; 172; 240; 159; 152; 128].
Proof. vm_compute. reflexivity. Qed.

Example bad_stride : text [226; 130; 65] = [63; 63; 65].
Proof. vm_compute. reflexivity. Qed.

Example bad_resume : text [240; 144; 128; 194; 162] = [63; 63; 63; 194; 162].
Proof. vm_compute. reflexivity. Qed.

Example bad_bytes : text [128; 255; 256; 0; 194] = [63; 63; 63; 0; 63].
Proof. vm_compute. reflexivity. Qed.

Example bad_surrogate : text [237; 160; 128] = [63; 63; 63].
Proof. vm_compute. reflexivity. Qed.

Example cut_stop : clean 2 [226; 130; 172; 65] = [].
Proof. vm_compute. reflexivity. Qed.

Example cut_prefix : clean 2 [65; 226; 130; 172; 66] = [65].
Proof. vm_compute. reflexivity. Qed.

Example cut_exact : clean 3 [226; 130; 172; 65] = [226; 130; 172].
Proof. vm_compute. reflexivity. Qed.

Example cut_256 : text (repeat 65 255 ++ [194; 162]) = repeat 65 255.
Proof. vm_compute. reflexivity. Qed.

Example fill_256 :
  text (repeat 65 254 ++ [194; 162; 66]) = repeat 65 254 ++ [194; 162].
Proof. vm_compute. reflexivity. Qed.

Example bad_256 :
  text (repeat 65 255 ++ [255; 65]) = repeat 65 255 ++ [63].
Proof. vm_compute. reflexivity. Qed.