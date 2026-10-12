(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Lia Bool.
Import ListNotations.

Record budget := Budget { effort : nat; remaining : nat; available : nat }.

Definition plan value limit cost bytes allocated :=
  if (effort value + cost <=? limit) && (bytes <=? remaining value)
    && (allocated <=? available value)
  then Some (Budget (effort value + cost) (remaining value - bytes)
    (available value - allocated))
  else None.

Inductive event := Work (cost bytes allocated : nat) | Rollback | Refused.

Definition step value limit input :=
  match input with
  | Work cost bytes allocated =>
      match plan value limit cost bytes allocated with
      | Some next => next
      | None => value
      end
  | Rollback | Refused => value
  end.

Definition run value limit inputs :=
  fold_left (fun value input => step value limit input) inputs value.

Theorem plan_cost : forall value limit cost bytes allocated next,
  plan value limit cost bytes allocated = Some next ->
  effort next = effort value + cost /\
  remaining next + bytes = remaining value /\
  available next + allocated = available value /\
  effort next <= limit.
Proof.
  intros [used free space] limit cost bytes allocated next accepted.
  unfold plan in accepted. simpl in accepted.
  destruct (((used + cost <=? limit) && (bytes <=? free))
    && (allocated <=? space)) eqn:checks;
    [|discriminate].
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[paid fits] reserved].
  apply Nat.leb_le in reserved.
  apply Nat.leb_le in paid. apply Nat.leb_le in fits.
  inversion accepted; subst. simpl. lia.
Qed.

Module Matrix.

Definition slot columns row column := row * columns + column.

Definition offset columns row column := 8 * slot columns row column.

Theorem cell_limit : forall rows columns row column,
  row < rows -> column < columns ->
  offset columns row column + 8 <= 8 * (rows * columns).
Proof. intros. unfold offset, slot. nia. Qed.

Theorem slot_unique : forall columns row col other next,
  col < columns -> next < columns ->
  slot columns row col = slot columns other next ->
  row = other /\ col = next.
Proof.
  intros columns row col other next first second same.
  unfold slot in same. assert (row = other) by nia.
  subst. split; nia.
Qed.

Theorem cell_cover : forall rows columns index,
  0 < columns -> index < rows * columns ->
  index / columns < rows /\ index mod columns < columns /\
  slot columns (index / columns) (index mod columns) = index.
Proof.
  intros rows columns index positive fits.
  pose proof (Nat.div_mod index columns ltac:(lia)).
  pose proof (Nat.mod_upper_bound index columns ltac:(lia)).
  unfold slot. repeat split; nia.
Qed.

Theorem byte_cover : forall rows columns index,
  0 < columns -> index < 8 * (rows * columns) ->
  exists row col byte,
    row < rows /\ col < columns /\ byte < 8 /\
    offset columns row col + byte = index.
Proof.
  intros rows columns index positive fits.
  pose proof (Nat.div_mod index 8 ltac:(lia)) as divided.
  pose proof (Nat.mod_upper_bound index 8 ltac:(lia)) as remainder.
  assert (cell : index / 8 < rows * columns) by nia.
  destruct (cell_cover rows columns (index / 8) positive cell)
    as [row [col position]].
  exists ((index / 8) / columns), ((index / 8) mod columns), (index mod 8).
  repeat split; try assumption. unfold offset. rewrite position. nia.
Qed.

Theorem byte_unique : forall columns row col byte other next rest,
  col < columns -> next < columns -> byte < 8 -> rest < 8 ->
  offset columns row col + byte = offset columns other next + rest ->
  row = other /\ col = next /\ byte = rest.
Proof.
  intros columns row col byte other next rest first second left right same.
  unfold offset in same.
  assert (position : slot columns row col = slot columns other next) by nia.
  destruct (slot_unique columns row col other next first second position).
  subst. repeat split; nia.
Qed.

End Matrix.

Module I8Table.
Section Values.
Context {A : Type}.
Variable eval : nat -> option A.

Definition put (table : nat -> option A) key value :=
  fun index => if Nat.eqb index key then Some value else table index.

Definition sound table := forall key value,
  table key = Some value -> eval key = Some value.

Fixpoint fill keys table :=
  match keys with
  | [] => Some table
  | key :: rest =>
    match table key with
    | Some _ => fill rest table
    | None =>
      match eval key with
      | None => None
      | Some value => fill rest (put table key value)
      end
    end
  end.

Fixpoint decode (table : nat -> option A) keys :=
  match keys with
  | [] => Some []
  | key :: rest =>
    match table key, decode table rest with
    | Some value, Some values => Some (value :: values)
    | _, _ => None
    end
  end.

Lemma put_sound : forall table key value,
  sound table -> eval key = Some value -> sound (put table key value).
Proof.
  intros table key value valid equal index found lookup.
  unfold put in lookup. destruct (Nat.eqb index key) eqn:same.
  - apply Nat.eqb_eq in same. subst. inversion lookup; subst. exact equal.
  - exact (valid index found lookup).
Qed.

Lemma fill_sound : forall keys table next,
  sound table -> fill keys table = Some next -> sound next.
Proof.
  induction keys as [|key rest ih]; intros table next valid accepted; simpl in accepted.
  - inversion accepted; subst. exact valid.
  - destruct (table key) eqn:found.
    + eapply ih; eauto.
    + destruct (eval key) eqn:value; [|discriminate].
      eapply ih; [eapply put_sound; eauto|exact accepted].
Qed.

Lemma fill_keeps : forall keys table next key value,
  fill keys table = Some next -> table key = Some value -> next key = Some value.
Proof.
  induction keys as [|index rest ih]; intros table next key value accepted found;
    simpl in accepted.
  - inversion accepted; subst. exact found.
  - destruct (table index) eqn:lookup.
    + eapply ih; eauto.
    + destruct (eval index) eqn:result; [|discriminate].
      eapply ih; [exact accepted|]. unfold put.
      destruct (Nat.eqb key index) eqn:same; [|exact found].
      apply Nat.eqb_eq in same. subst. rewrite lookup in found. discriminate.
Qed.

Lemma fill_has : forall keys table next,
  fill keys table = Some next ->
  forall key, In key keys -> exists value, next key = Some value.
Proof.
  induction keys as [|index rest ih]; intros table next accepted key member;
    simpl in accepted, member; [contradiction|].
  destruct (table index) eqn:found.
  - destruct member as [same|later].
    + subst. eexists. eapply fill_keeps; eauto.
    + eapply ih; eauto.
  - destruct (eval index) eqn:value; [|discriminate].
    destruct member as [same|later].
    + subst. eexists. eapply fill_keeps; [exact accepted|].
      unfold put. now rewrite Nat.eqb_refl.
    + eapply ih; eauto.
Qed.

Lemma decode_total : forall keys table,
  (forall key, In key keys -> exists value, table key = Some value) ->
  exists values, decode table keys = Some values.
Proof.
  induction keys as [|key rest ih]; intros table present.
  - exists []. reflexivity.
  - destruct (present key (or_introl eq_refl)) as [value found].
    destruct (ih table) as [values decoded].
    { intros index member. apply present. now right. }
    exists (value :: values). simpl. now rewrite found, decoded.
Qed.

Lemma decode_sound : forall keys table values,
  sound table -> decode table keys = Some values -> decode eval keys = Some values.
Proof.
  induction keys as [|key rest ih]; intros table values valid decoded; simpl in *.
  - exact decoded.
  - destruct (table key) eqn:found; [|discriminate].
    destruct (decode table rest) eqn:rest_read; [|discriminate].
    rewrite (valid key a found), (ih table l valid rest_read). exact decoded.
Qed.

Theorem fill_exact : forall keys table next,
  sound table -> fill keys table = Some next ->
  exists values, decode eval keys = Some values /\ decode next keys = Some values.
Proof.
  intros keys table next valid accepted.
  destruct (decode_total keys next (fill_has keys table next accepted))
    as [values decoded].
  exists values. split; [|exact decoded].
  eapply decode_sound; [eapply fill_sound; eauto|exact decoded].
Qed.

End Values.

Example only_used :
  fill (fun key => if Nat.eqb key 1 then None else Some key) [0; 0] (fun _ => None) =
    Some (put (fun _ => None) 0 0).
Proof. reflexivity. Qed.

Example used_refused :
  fill (fun key => if Nat.eqb key 1 then None else Some key) [0; 1] (fun _ => None) = None.
Proof. reflexivity. Qed.

Print Assumptions fill_exact.
End I8Table.

Module LinearTiles.
Section Ordered.
Context {E A H : Type}.
Variable push : H -> A -> H.

Fixpoint scan (values : list (E + A)) (state : H) : E + H :=
  match values with
  | [] => inr state
  | inl error :: _ => inl error
  | inr value :: rest => scan rest (push state value)
  end.

Fixpoint tiles (rows : list (list (E + A))) (state : H) : E + H :=
  match rows with
  | [] => inr state
  | row :: rest =>
    match scan row state with
    | inl error => inl error
    | inr next => tiles rest next
    end
  end.

Lemma scan_app : forall first rest state,
  scan (first ++ rest) state =
  match scan first state with
  | inl error => inl error
  | inr next => scan rest next
  end.
Proof.
  induction first as [|[error|value] rest ih]; intros; simpl; auto.
Qed.

Theorem tiles_exact : forall rows state,
  tiles rows state = scan (concat rows) state.
Proof.
  induction rows as [|row rest ih]; intros state; simpl; [reflexivity|].
  rewrite scan_app. destruct (scan row state); auto.
Qed.

Theorem partition_exact : forall rows other state,
  concat rows = concat other -> tiles rows state = tiles other state.
Proof.
  intros rows other state equal. repeat rewrite tiles_exact. now rewrite equal.
Qed.

Theorem first_error : forall first rest state next error,
  scan first state = inr next ->
  scan (first ++ inl error :: rest) state = inl error.
Proof.
  intros first rest state next error complete.
  rewrite scan_app, complete. reflexivity.
Qed.

End Ordered.

Example order_case :
  tiles (fun state value => 10 * state + value)
    [[inr 1; inr 2]; []; [inr 3; inl 7; inr 9]] 0 = inl 7.
Proof. reflexivity. Qed.

Print Assumptions tiles_exact.
Print Assumptions partition_exact.
Print Assumptions first_error.
End LinearTiles.

Module JsonSize.

Definition width byte :=
  if existsb (Nat.eqb byte) [34; 92; 8; 12; 10; 13; 9] then 2
  else if (byte <? 32) || (byte =? 127) then 6 else 1.

Fixpoint size bytes :=
  match bytes with [] => 0 | byte :: suffix => width byte + size suffix end.

Definition quoted bytes := 2 + size bytes.

Fixpoint scan room bytes :=
  match bytes with
  | [] => Some room
  | byte :: suffix =>
    if width byte <=? room then scan (room - width byte) suffix else None
  end.

Definition measure limit bytes :=
  if (2 <=? limit) && (length bytes <=? limit - 2) then
    option_map (fun left => limit - left) (scan (limit - 2) bytes)
  else None.

Lemma width_range : forall byte, 1 <= width byte <= 6.
Proof.
  intros byte. unfold width.
  destruct (existsb (Nat.eqb byte) [34; 92; 8; 12; 10; 13; 9]); [lia|].
  destruct ((byte <? 32) || (byte =? 127)); lia.
Qed.

Theorem size_range : forall bytes,
  length bytes <= size bytes <= 6 * length bytes.
Proof.
  induction bytes as [|byte suffix induction]; simpl; [lia|].
  pose proof (width_range byte). lia.
Qed.

Lemma scan_exact : forall bytes room left,
  scan room bytes = Some left -> left + size bytes = room.
Proof.
  induction bytes as [|byte suffix induction]; intros room left accepted.
  - inversion accepted. simpl. lia.
  - simpl in accepted. destruct (width byte <=? room) eqn:fits; [|discriminate].
    apply Nat.leb_le in fits. apply induction in accepted. simpl. lia.
Qed.

Lemma scan_complete : forall bytes room,
  size bytes <= room -> scan room bytes = Some (room - size bytes).
Proof.
  induction bytes as [|byte suffix induction]; intros room fits.
  - simpl. f_equal. lia.
  - simpl in *. assert (first : (width byte <=? room) = true) by
      (apply Nat.leb_le; lia).
    rewrite first, induction; [f_equal; lia|lia].
Qed.

Theorem measure_exact : forall bytes limit result,
  measure limit bytes = Some result ->
  result = quoted bytes /\ result <= limit.
Proof.
  intros bytes limit result accepted. unfold measure in accepted.
  destruct ((2 <=? limit) && (length bytes <=? limit - 2)) eqn:fits; [|discriminate].
  apply andb_true_iff in fits. destruct fits as [room _]. apply Nat.leb_le in room.
  destruct (scan (limit - 2) bytes) as [left|] eqn:read; [|discriminate].
  apply scan_exact in read. inversion accepted; subst. unfold quoted. lia.
Qed.

Theorem measure_complete : forall bytes limit,
  quoted bytes <= limit -> measure limit bytes = Some (quoted bytes).
Proof.
  intros bytes limit fits. pose proof (size_range bytes) as [minimum _].
  unfold quoted in fits. unfold measure.
  assert (room : (2 <=? limit) = true) by (apply Nat.leb_le; lia).
  assert (raw : (length bytes <=? limit - 2) = true) by (apply Nat.leb_le; lia).
  rewrite room, raw. simpl. rewrite scan_complete; [|lia].
  simpl. unfold quoted. f_equal. lia.
Qed.

Theorem measure_refused : forall bytes limit,
  measure limit bytes = None <-> limit < quoted bytes.
Proof.
  intros bytes limit. split; intros refused.
  - destruct (le_dec (quoted bytes) limit) as [fits|exceeds]; [|lia].
    rewrite (measure_complete bytes limit fits) in refused. discriminate.
  - destruct (measure limit bytes) as [result|] eqn:read; [|reflexivity].
    apply measure_exact in read. lia.
Qed.

End JsonSize.

Module StoreRead.
Section Read.
Context {A : Type}.
Variable size : A -> nat.

Fixpoint bytes (rows : list A) :=
  match rows with [] => 0 | row :: rest => size row + bytes rest end.

Fixpoint read room rows : option (list A) * nat :=
  match rows with
  | [] => (Some [], 0)
  | row :: rest =>
    if size row <=? room then
      let '(result, reads) := read (room - size row) rest in
      (option_map (cons row) result, S reads)
    else (None, 1)
  end.

Definition scan count room rows :=
  if length rows <=? count then read room rows else (None, 0).

Example repeated_row : forall row,
  size row = 2 -> read 3 [row; row] = (None, 2).
Proof. intros row measured. simpl. rewrite measured. reflexivity. Qed.

Theorem read_complete : forall rows room,
  bytes rows <= room -> read room rows = (Some rows, length rows).
Proof.
  induction rows as [|row rest induction]; intros room fits; [reflexivity|].
  simpl in *. assert (first : (size row <=? room) = true) by
    (apply Nat.leb_le; lia).
  rewrite first, induction; [reflexivity|lia].
Qed.

Theorem read_exact : forall rows room result reads,
  read room rows = (Some result, reads) ->
  result = rows /\ reads = length rows /\ bytes rows <= room.
Proof.
  induction rows as [|row rest induction]; intros room result reads accepted.
  - simpl in accepted. inversion accepted; subst. simpl. repeat split; try reflexivity; lia.
  - simpl in accepted. destruct (size row <=? room) eqn:fits; [|discriminate].
    destruct (read (room - size row) rest) as [[values|] work] eqn:next;
      [|discriminate].
    inversion accepted; subst. specialize (induction _ _ _ next).
    apply Nat.leb_le in fits. destruct induction as [same [count cost]].
    subst. simpl. repeat split; try reflexivity; lia.
Qed.

Theorem read_refused : forall rows room reads,
  read room rows = (None, reads) ->
  0 < reads /\ reads <= length rows /\ room < bytes (firstn reads rows).
Proof.
  induction rows as [|row rest induction]; intros room reads refused.
  - discriminate.
  - simpl in refused. destruct (size row <=? room) eqn:fits.
    + destruct (read (room - size row) rest) as [[values|] work] eqn:next;
        [discriminate|].
      inversion refused; subst. specialize (induction _ _ next).
      apply Nat.leb_le in fits. simpl. lia.
    + inversion refused; subst. apply Nat.leb_gt in fits. simpl. lia.
Qed.

Theorem count_refused : forall rows count room,
  count < length rows -> scan count room rows = (None, 0).
Proof.
  intros rows count room exceeds. unfold scan.
  assert (checked : (length rows <=? count) = false) by
    (apply Nat.leb_gt; lia).
  rewrite checked. reflexivity.
Qed.

Theorem scan_complete : forall rows count room,
  length rows <= count -> bytes rows <= room ->
  scan count room rows = (Some rows, length rows).
Proof.
  intros rows count room fits space. unfold scan.
  apply Nat.leb_le in fits. rewrite fits. apply read_complete. assumption.
Qed.

Theorem scan_exact : forall rows count room result reads,
  scan count room rows = (Some result, reads) ->
  result = rows /\ reads = length rows /\ length rows <= count /\ bytes rows <= room.
Proof.
  intros rows count room result reads accepted. unfold scan in accepted.
  destruct (length rows <=? count) eqn:fits; [|discriminate].
  apply Nat.leb_le in fits. apply read_exact in accepted. tauto.
Qed.

Theorem scan_work : forall rows count room result reads,
  scan count room rows = (result, reads) -> reads <= count.
Proof.
  intros rows count room result reads scanned. unfold scan in scanned.
  destruct (length rows <=? count) eqn:fits.
  - apply Nat.leb_le in fits. destruct result as [values|].
    + apply read_exact in scanned. lia.
    + apply read_refused in scanned. lia.
  - inversion scanned. lia.
Qed.
End Read.

Section Encoded.
Context {A : Type}.
Variable size wire : A -> nat.
Variable extra : nat.

Fixpoint wire_bytes rows :=
  match rows with [] => 0 | row :: rest => wire row + wire_bytes rest end.

Fixpoint fetch room rows : nat * nat :=
  match rows with
  | [] => (0, 0)
  | row :: rest =>
    if wire row <=? 6 * room + extra then
      if (size row <=? room) && (wire row <=? 6 * size row + extra) then
        let '(reads, cost) := fetch (room - size row) rest in
        (S reads, wire row + cost)
      else (1, wire row)
    else (0, 0)
  end.

Theorem fetch_cost : forall rows room reads cost,
  fetch room rows = (reads, cost) ->
  reads <= length rows /\ cost <= 6 * room + extra * reads.
Proof.
  induction rows as [|row rest induction]; intros room reads cost fetched.
  - inversion fetched; subst. simpl. lia.
  - change ((if wire row <=? 6 * room + extra then
      if (size row <=? room) && (wire row <=? 6 * size row + extra) then
        let '(next, used) := fetch (room - size row) rest in
        (S next, wire row + used)
      else (1, wire row)
      else (0, 0)) = (reads, cost)) in fetched.
    destruct (wire row <=? 6 * room + extra) eqn:encoded.
    + apply Nat.leb_le in encoded.
      destruct ((size row <=? room) && (wire row <=? 6 * size row + extra)) eqn:fits.
      * apply andb_true_iff in fits. destruct fits as [space shape].
        apply Nat.leb_le in space. apply Nat.leb_le in shape.
        destruct (fetch (room - size row) rest) as [next used] eqn:scanned.
        inversion fetched; subst. specialize (induction _ _ _ scanned).
        simpl length. rewrite Nat.mul_succ_r. lia.
      * inversion fetched; subst. simpl. lia.
    + inversion fetched; subst. simpl. lia.
Qed.

Theorem fetch_complete : forall rows room,
  Forall (fun row => wire row <= 6 * size row + extra) rows ->
  bytes size rows <= room -> fetch room rows = (length rows, wire_bytes rows).
Proof.
  induction rows as [|row rest induction]; intros room encoded space; [reflexivity|].
  inversion encoded; subst. simpl in space.
  assert (first : (wire row <=? 6 * room + extra) = true) by
    (apply Nat.leb_le; lia).
  assert (fits : (size row <=? room) = true) by (apply Nat.leb_le; lia).
  assert (shape : (wire row <=? 6 * size row + extra) = true) by
    (apply Nat.leb_le; assumption).
  change ((if wire row <=? 6 * room + extra then
    if (size row <=? room) && (wire row <=? 6 * size row + extra) then
      let '(next, used) := fetch (room - size row) rest in
      (S next, wire row + used)
    else (1, wire row)
    else (0, 0)) = (length (row :: rest), wire_bytes (row :: rest))).
  rewrite first, fits, shape. simpl andb.
  rewrite induction; [reflexivity|assumption|lia].
Qed.
End Encoded.

Example wire_stop : fetch (fun n => n) (fun n => n) 1 2 [16; 1] = (0, 0).
Proof. reflexivity. Qed.

Example entry_stop : scan (fun n => n) 2 100 [1; 1; 1] = (None, 0).
Proof. reflexivity. Qed.

Example byte_stop : scan (fun n => n) 4 3 [2; 2; 1; 1] = (None, 2).
Proof. reflexivity. Qed.

Example exact_fit : scan (fun n => n) 2 3 [2; 1] = (Some [2; 1], 2).
Proof. reflexivity. Qed.
End StoreRead.

Module DecodeCache.
Section Cache.
Context {K V : Type}.
Variable equal : forall x y : K, {x = y} + {x <> y}.
Variable decode : K -> option V.
Variable charge : K -> V -> nat.
Variable slots bytes : nat.

Record entry := Entry { key : K; value : V; size : nat }.

Fixpoint used (entries : list entry) :=
  match entries with [] => 0 | item :: rest => size item + used rest end.

Fixpoint take count room entries :=
  match count, entries with
  | S count, item :: rest =>
    if size item <=? room
    then item :: take count (room - size item) rest
    else []
  | _, _ => []
  end.

Definition matches id item := if equal (key item) id then true else false.
Definition keep item entries :=
  take slots bytes (item :: filter (fun old => negb (matches (key item) old)) entries).
Definition valid entries := Forall (fun item => decode (key item) = Some (value item)) entries.

Definition load id entries :=
  match find (matches id) entries with
  | Some item => (keep item entries, Some (value item))
  | None =>
    match decode id with
    | None => (entries, None)
    | Some result => (keep (Entry id result (charge id result)) entries, Some result)
    end
  end.

Lemma take_limits : forall count room entries,
  length (take count room entries) <= count /\ used (take count room entries) <= room.
Proof.
  induction count as [|count induction]; intros room entries; [simpl; lia|].
  destruct entries as [|item rest]; [simpl; lia|].
  simpl. destruct (size item <=? room) eqn:fits; [|simpl; lia].
  apply Nat.leb_le in fits.
  specialize (induction (room - size item) rest). simpl. lia.
Qed.

Lemma take_valid : forall entries count room,
  valid entries -> valid (take count room entries).
Proof.
  intros entries count. revert entries.
  induction count as [|count induction]; intros entries room checked; [constructor|].
  destruct entries as [|item rest]; [constructor|].
  simpl. destruct (size item <=? room); [|constructor].
  inversion checked; subst. constructor; [assumption|].
  apply induction. assumption.
Qed.

Lemma keep_valid : forall item entries,
  decode (key item) = Some (value item) -> valid entries -> valid (keep item entries).
Proof.
  intros item entries checked valid_entries. unfold keep. apply take_valid.
  constructor; [assumption|].
  unfold valid in *. rewrite Forall_forall in *.
  intros old present. apply filter_In in present. apply valid_entries. tauto.
Qed.

Lemma found_valid : forall id entries item,
  valid entries -> find (matches id) entries = Some item ->
  decode id = Some (value item).
Proof.
  intros id entries item checked found.
  apply find_some in found. destruct found as [present same].
  unfold matches in same. destruct (equal (key item) id) as [same_key|]; [|discriminate].
  unfold valid in checked. rewrite Forall_forall in checked.
  rewrite <- same_key. apply checked. assumption.
Qed.

Theorem load_correct : forall id entries,
  valid entries -> snd (load id entries) = decode id /\ valid (fst (load id entries)).
Proof.
  intros id entries checked. unfold load.
  destruct (find (matches id) entries) as [item|] eqn:found.
  - pose proof (found_valid id entries item checked found) as result.
    split; [simpl; symmetry; assumption|].
    simpl. apply keep_valid; [|assumption].
    apply find_some in found. unfold valid in checked. rewrite Forall_forall in checked.
    apply checked. tauto.
  - destruct (decode id) as [result|] eqn:parsed; simpl.
    + split; [reflexivity|]. apply keep_valid; assumption.
    + split; [reflexivity|assumption].
Qed.

Theorem load_limits : forall id entries,
  length entries <= slots -> used entries <= bytes ->
  length (fst (load id entries)) <= slots /\ used (fst (load id entries)) <= bytes.
Proof.
  intros id entries count room. unfold load.
  destruct (find (matches id) entries); simpl.
  - apply take_limits.
  - destruct (decode id); simpl; [apply take_limits|tauto].
Qed.

Fixpoint run keys entries :=
  match keys with
  | [] => entries
  | id :: rest => run rest (fst (load id entries))
  end.

Theorem trace_valid : forall keys entries,
  valid entries -> length entries <= slots -> used entries <= bytes ->
  valid (run keys entries) /\ length (run keys entries) <= slots /\ used (run keys entries) <= bytes.
Proof.
  induction keys as [|id rest induction]; intros entries checked count room.
  - simpl. tauto.
  - simpl. apply induction.
    + apply (proj2 (load_correct id entries checked)).
    + apply (proj1 (load_limits id entries count room)).
    + apply (proj2 (load_limits id entries count room)).
Qed.
End Cache.
End DecodeCache.

Module ModelLease.
Record token := Token { id : nat; generation : nat; key : nat }.

Definition same left right :=
  Nat.eqb (id left) (id right) && Nat.eqb (generation left) (generation right)
    && Nat.eqb (key left) (key right).

Definition leased leases value :=
  existsb (fun lease => Nat.eqb (key lease) value) leases.

Definition victim models leases :=
  find (fun model => negb (leased leases model)) (rev models).

Definition remove value models :=
  filter (fun model => negb (Nat.eqb model value)) models.

Definition release value leases :=
  filter (fun lease => negb (same lease value)) leases.

Definition acquire limit retiring value leases :=
  if Nat.ltb (length leases) limit
    && negb (Nat.eqb (key value) retiring)
  then value :: leases else leases.

Lemma same_exact : forall left right,
  same left right = true <-> left = right.
Proof.
  intros [a b c] [d e f]. unfold same. simpl.
  repeat rewrite andb_true_iff. repeat rewrite Nat.eqb_eq.
  split; [intros [[? ?] ?]; subst; reflexivity|intros equal; inversion equal; auto].
Qed.

Lemma leased_exact : forall leases value,
  leased leases value = true <-> exists lease, In lease leases /\ key lease = value.
Proof.
  intros leases value. unfold leased. rewrite existsb_exists.
  split; intros [lease [member equal]]; exists lease; split; [exact member| |exact member|];
    [apply Nat.eqb_eq in equal|apply Nat.eqb_eq]; exact equal.
Qed.

Theorem victim_idle : forall models leases selected,
  victim models leases = Some selected -> leased leases selected = false.
Proof.
  intros models leases selected chosen. unfold victim in chosen.
  apply find_some in chosen. destruct chosen as [_ idle].
  apply negb_true_iff in idle. exact idle.
Qed.

Theorem pinned_model_kept : forall models leases selected lease,
  victim models leases = Some selected ->
  In lease leases -> In (key lease) models ->
  In (key lease) (remove selected models).
Proof.
  intros models leases selected lease chosen active present.
  pose proof (victim_idle _ _ _ chosen) as idle.
  unfold remove. apply filter_In. split; [exact present|].
  apply negb_true_iff, Nat.eqb_neq. intro equal.
  assert (leased leases selected = true) as occupied.
  { apply leased_exact. exists lease. auto. }
  rewrite occupied in idle. discriminate.
Qed.

Theorem all_busy : forall models leases,
  Forall (fun model => leased leases model = true) models ->
  victim models leases = None.
Proof.
  intros models leases busy. unfold victim.
  assert (Forall (fun model => leased leases model = true) (rev models)) as reversed.
  { rewrite Forall_forall in *. intros model member. apply busy. apply in_rev. exact member. }
  induction reversed as [|model rest occupied active ih]; [reflexivity|].
  simpl. rewrite occupied. simpl. exact ih.
Qed.

Theorem release_exact : forall value leases lease,
  In lease (release value leases) <-> In lease leases /\ lease <> value.
Proof.
  intros value leases lease. unfold release. rewrite filter_In, negb_true_iff.
  assert (same lease value = false <-> lease <> value) as exact.
  { split.
    - intros different equal. subst. assert (same value value = true) by
        (apply same_exact; reflexivity). congruence.
    - intros different. destruct (same lease value) eqn:equal; [|reflexivity].
      apply same_exact in equal. contradiction. }
  rewrite exact. reflexivity.
Qed.

Theorem release_old_generation : forall leases old current,
  generation old <> generation current -> In current leases ->
  In current (release old leases).
Proof.
  intros leases old current different member. apply release_exact.
  split; [exact member|]. intro equal. subst. contradiction.
Qed.

Theorem release_twice : forall leases value,
  release value (release value leases) = release value leases.
Proof.
  intros leases value. induction leases as [|lease rest ih]; [reflexivity|].
  unfold release in *. simpl.
  destruct (negb (same lease value)) eqn:kept; simpl;
    rewrite ?kept, ih; reflexivity.
Qed.

Theorem capacity_kept : forall leases limit retiring value,
  length leases <= limit -> length (acquire limit retiring value leases) <= limit.
Proof.
  intros leases limit retiring value fits. unfold acquire.
  destruct (Nat.ltb (length leases) limit && negb (Nat.eqb (key value) retiring)) eqn:room;
    [|exact fits].
  apply andb_true_iff in room. destruct room as [room _].
  apply Nat.ltb_lt in room. simpl. lia.
Qed.

Theorem reservation_kept : forall leases limit retiring value,
  leased leases retiring = false ->
  leased (acquire limit retiring value leases) retiring = false.
Proof.
  intros leases limit retiring value idle. unfold acquire.
  destruct (Nat.ltb (length leases) limit && negb (Nat.eqb (key value) retiring)) eqn:room;
    [|exact idle].
  apply andb_true_iff in room. destruct room as [_ different].
  apply negb_true_iff in different. unfold leased in *. simpl.
  rewrite different, idle. reflexivity.
Qed.

Example shared_key_kept :
  remove 2 [1; 1; 2] = [1; 1].
Proof. reflexivity. Qed.

Example active_key_not_selected :
  victim [2; 1] [Token 4 7 1] = Some 2.
Proof. reflexivity. Qed.

Print Assumptions victim_idle.
Print Assumptions pinned_model_kept.
Print Assumptions all_busy.
Print Assumptions release_exact.
Print Assumptions release_old_generation.
Print Assumptions release_twice.
Print Assumptions capacity_kept.
Print Assumptions reservation_kept.
End ModelLease.

Module SharedCache.
Definition models := list (nat * nat).

Fixpoint read (values : models) key :=
  match values with
  | [] => None
  | (stored, root) :: rest => if Nat.eqb stored key then Some root else read rest key
  end.

Definition remove (values : models) key :=
  filter (fun value => negb (Nat.eqb (fst value) key)) values.

Definition write values key root := (key, root) :: remove values key.

Definition drop values leases key :=
  if ModelLease.leased leases key then values else remove values key.

Definition seal (values : models) leases key root :=
  if ModelLease.leased leases key then values
  else match read values key with
  | Some _ => write values key root
  | None =>
    if Nat.ltb (length values) 2 then write values key root
    else match ModelLease.victim (map fst values) leases with
    | None => values
    | Some selected => write (remove values selected) key root
    end
  end.

Example peer_drop_kept :
  read (drop [(1, 42); (2, 9)] [ModelLease.Token 7 3 1] 1) 1 = Some 42.
Proof. reflexivity. Qed.

Example third_model_kept :
  read (seal [(1, 42); (2, 9)] [ModelLease.Token 7 3 1] 3 11) 1 = Some 42.
Proof. reflexivity. Qed.

Lemma remove_read : forall values removed key,
  key <> removed -> read (remove values removed) key = read values key.
Proof.
  intros values removed key different. induction values as [|[stored root] rest ih];
    [reflexivity|].
  unfold remove in *. simpl.
  destruct (Nat.eqb stored removed) eqn:equal; simpl.
  - apply Nat.eqb_eq in equal. subst stored.
    assert (Nat.eqb removed key = false) as unequal by (apply Nat.eqb_neq; congruence).
    rewrite unequal. exact ih.
  - destruct (Nat.eqb stored key); [reflexivity|exact ih].
Qed.

Lemma write_read : forall values written root key,
  key <> written -> read (write values written root) key = read values key.
Proof.
  intros values written root key different. unfold write. simpl.
  assert (Nat.eqb written key = false) as unequal by (apply Nat.eqb_neq; congruence).
  rewrite unequal. apply remove_read. exact different.
Qed.

Theorem drop_keeps_active : forall values leases removed key,
  ModelLease.leased leases key = true ->
  read (drop values leases removed) key = read values key.
Proof.
  intros values leases removed key active. unfold drop.
  destruct (ModelLease.leased leases removed) eqn:held; [reflexivity|].
  apply remove_read. intro equal. subst removed. congruence.
Qed.

Theorem seal_keeps_active : forall values leases written root key,
  ModelLease.leased leases key = true ->
  read (seal values leases written root) key = read values key.
Proof.
  intros values leases written root key active. unfold seal.
  destruct (ModelLease.leased leases written) eqn:held; [reflexivity|].
  assert (key <> written) as different by (intro equal; subst; congruence).
  destruct (read values written); [apply write_read; exact different|].
  destruct (Nat.ltb (length values) 2); [apply write_read; exact different|].
  destruct (ModelLease.victim (map fst values) leases) as [chosen|] eqn:selected;
    [|reflexivity].
  rewrite write_read by exact different.
  apply remove_read. intro equal. subst chosen.
  pose proof (ModelLease.victim_idle _ _ _ selected). congruence.
Qed.

Print Assumptions drop_keeps_active.
Print Assumptions seal_keeps_active.
End SharedCache.

Module ModelBuild.
Inductive phase := Writing | Sealing | Closing.
Record slot := Slot { generation : nat; stage : phase }.

Definition matches id value := Nat.eqb id (generation value).

Definition move (value : option slot) id next :=
  match value with
  | Some current =>
    if matches id current then Some (Slot id next) else value
  | None => None
  end.

Definition cancel value id := move value id Closing.

Definition finish (value : option slot) id :=
  match value with
  | Some current =>
    if matches id current then
      match stage current with Writing => value | _ => None end
    else value
  | None => None
  end.

Definition publish (value : option slot) id (root : nat) ready :=
  match value with
  | Some current =>
    if matches id current then
      match stage current with Sealing => Some root | _ => ready end
    else ready
  | None => ready
  end.

Definition begin_load (value : option slot) id :=
  match value with None => Some (Slot id Writing) | Some _ => value end.

Theorem cancel_no_publish : forall value id root ready,
  publish (cancel value id) id root ready = ready.
Proof.
  intros [current|] id root ready; [|reflexivity].
  unfold cancel, move, publish, matches.
  destruct (Nat.eqb id (generation current)) eqn:equal; simpl.
  - rewrite Nat.eqb_refl. reflexivity.
  - rewrite equal. reflexivity.
Qed.

Theorem foreign_cancel_kept : forall current id,
  id <> generation current -> cancel (Some current) id = Some current.
Proof.
  intros current id different. unfold cancel, move, matches.
  apply Nat.eqb_neq in different. rewrite different. reflexivity.
Qed.

Theorem foreign_finish_kept : forall current id,
  id <> generation current -> finish (Some current) id = Some current.
Proof.
  intros current id different. unfold finish, matches.
  apply Nat.eqb_neq in different. rewrite different. reflexivity.
Qed.

Theorem old_reply_refused : forall current id root ready,
  id <> generation current -> publish (Some current) id root ready = ready.
Proof.
  intros current id root ready different. unfold publish, matches.
  apply Nat.eqb_neq in different. rewrite different. reflexivity.
Qed.

Theorem begin_keeps_owner : forall current id,
  begin_load (Some current) id = Some current.
Proof. reflexivity. Qed.

Theorem detached_keeps_slot : forall id,
  cancel (Some (Slot id Sealing)) id = Some (Slot id Closing).
Proof. intros. unfold cancel, move, matches. simpl. rewrite Nat.eqb_refl. reflexivity. Qed.

Inductive message := Cancel (id : nat) | Finish (id : nat) | Publish (id root : nat).

Definition step (value : option slot * option nat) input :=
  let '(current, ready) := value in
  match input with
  | Cancel id => (cancel current id, ready)
  | Finish id => (finish current id, ready)
  | Publish id root => (current, publish current id root ready)
  end.

Fixpoint run value inputs :=
  match inputs with [] => value | input :: rest => run (step value input) rest end.

Definition foreign generation input :=
  match input with Cancel id | Finish id | Publish id _ => id <> generation end.

Theorem foreign_trace_kept : forall inputs current ready,
  Forall (foreign (generation current)) inputs ->
  run (Some current, ready) inputs = (Some current, ready).
Proof.
  induction inputs as [|input rest ih]; intros current ready checked; [reflexivity|].
  inversion checked as [|value values first remaining]; subst.
  destruct input; cbn [run step]; simpl in first.
  - rewrite foreign_cancel_kept by exact first. apply ih. exact remaining.
  - rewrite foreign_finish_kept by exact first. apply ih. exact remaining.
  - rewrite old_reply_refused by exact first. apply ih. exact remaining.
Qed.

Example cancelled_seal :
  run (Some (Slot 7 Sealing), Some 42) [Cancel 7; Publish 7 9] =
  (Some (Slot 7 Closing), Some 42).
Proof. reflexivity. Qed.

Example replaced_owner :
  run (Some (Slot 8 Writing), Some 42) [Cancel 7; Finish 7; Publish 7 9] =
  (Some (Slot 8 Writing), Some 42).
Proof. reflexivity. Qed.

Print Assumptions cancel_no_publish.
Print Assumptions foreign_trace_kept.
Print Assumptions detached_keeps_slot.
End ModelBuild.

Module StorageEdit.
Inductive command := Get (key : nat) | Put (key value : nat) | Delete (key : nat) | Refuse.
Record image := Image { values : SharedCache.models; shared : bool; copies : nat }.

Definition needs_copy values input :=
  match input with
  | Get _ | Refuse => false
  | Put _ _ => true
  | Delete key => match SharedCache.read values key with Some _ => true | None => false end
  end.

Definition fork needed value :=
  if needed && shared value then Image (values value) false (S (copies value)) else value.

Definition step value input :=
  let selected := fork (needs_copy (values value) input) value in
  let next := match input with
    | Get _ | Refuse => values selected
    | Put key root => SharedCache.write (values selected) key root
    | Delete key => SharedCache.remove (values selected) key
    end in
  Image next (shared selected) (copies selected).

Definition answer value input :=
  match input with Get key => SharedCache.read (values value) key | _ => None end.

Fixpoint exec value inputs :=
  match inputs with [] => value | input :: rest => exec (step value input) rest end.

Fixpoint answers value inputs :=
  match inputs with
  | [] => []
  | input :: rest => answer value input :: answers (step value input) rest
  end.

Definition readonly input := match input with Get _ | Refuse => True | _ => False end.
Definition budget value := copies value + if shared value then 1 else 0.

Lemma fork_values : forall needed value, values (fork needed value) = values value.
Proof. intros [] [contents [] count]; reflexivity. Qed.

Lemma step_values : forall left right input,
  values left = values right -> values (step left input) = values (step right input).
Proof.
  intros left right input equal. unfold step. simpl.
  destruct input; simpl; repeat rewrite fork_values; rewrite equal; reflexivity.
Qed.

Lemma readonly_step : forall value input, readonly input -> step value input = value.
Proof.
  intros [contents sharing count] input checked. destruct input; simpl in checked;
    try contradiction; reflexivity.
Qed.

Lemma remove_absent : forall contents key,
  SharedCache.read contents key = None -> SharedCache.remove contents key = contents.
Proof.
  induction contents as [|[stored root] rest ih]; intros key empty; [reflexivity|].
  simpl in empty. destruct (Nat.eqb stored key) eqn:equal; [discriminate|].
  unfold SharedCache.remove in *. simpl. rewrite equal. simpl.
  f_equal. apply ih. exact empty.
Qed.

Theorem absent_delete : forall value key,
  SharedCache.read (values value) key = None -> step value (Delete key) = value.
Proof.
  intros [contents sharing count] key empty. unfold step. simpl in *.
  rewrite empty. simpl. rewrite remove_absent by exact empty. reflexivity.
Qed.

Theorem readonly_trace : forall inputs value,
  Forall readonly inputs -> exec value inputs = value.
Proof.
  induction inputs as [|input rest ih]; intros value checked; [reflexivity|].
  inversion checked as [|first remaining accepted checked_rest]; subst.
  simpl. rewrite readonly_step by exact accepted. apply ih. exact checked_rest.
Qed.

Theorem trace_values : forall inputs left right,
  values left = values right -> values (exec left inputs) = values (exec right inputs).
Proof.
  induction inputs as [|input rest ih]; intros left right equal; [exact equal|].
  simpl. apply ih. apply step_values. exact equal.
Qed.

Theorem trace_answers : forall inputs left right,
  values left = values right -> answers left inputs = answers right inputs.
Proof.
  induction inputs as [|input rest ih]; intros left right equal; [reflexivity|].
  simpl. f_equal.
  - destruct input; simpl; try reflexivity. rewrite equal. reflexivity.
  - apply ih. apply step_values. exact equal.
Qed.

Lemma fork_budget : forall needed value,
  budget value = 1 -> budget (fork needed value) = 1.
Proof.
  intros [] [contents [] count] checked; unfold budget, fork in *; simpl in *; lia.
Qed.

Lemma step_budget : forall value input,
  budget value = 1 -> budget (step value input) = 1.
Proof.
  intros value input checked. unfold step, budget. destruct input; simpl;
    apply fork_budget; exact checked.
Qed.

Theorem copies_limit : forall inputs value,
  budget value = 1 -> copies (exec value inputs) <= 1.
Proof.
  assert (forall inputs value, budget value = 1 -> budget (exec value inputs) = 1) as preserved.
  { induction inputs as [|input rest ih]; intros value checked; [exact checked|].
    simpl. apply ih. apply step_budget. exact checked. }
  intros inputs value checked. specialize (preserved inputs value checked).
  unfold budget in preserved. destruct (shared (exec value inputs)); lia.
Qed.

Example first_write_once :
  copies (exec (Image [(1, 42)] true 0) [Get 1; Refuse; Put 1 7; Get 1; Put 2 9]) = 1.
Proof. reflexivity. Qed.

Print Assumptions readonly_trace.
Print Assumptions absent_delete.
Print Assumptions trace_values.
Print Assumptions trace_answers.
Print Assumptions copies_limit.
End StorageEdit.

Theorem plan_refused : forall value limit cost bytes allocated,
  limit < effort value + cost \/ remaining value < bytes \/ available value < allocated ->
  plan value limit cost bytes allocated = None.
Proof.
  intros [used free space] limit cost bytes allocated invalid.
  unfold plan. simpl in *.
  destruct (((used + cost <=? limit) && (bytes <=? free))
    && (allocated <=? space)) eqn:checks;
    [|reflexivity].
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[paid fits] reserved].
  apply Nat.leb_le in reserved.
  apply Nat.leb_le in paid. apply Nat.leb_le in fits. lia.
Qed.

Theorem step_spent : forall value limit input,
  effort value <= effort (step value limit input) /\
  remaining (step value limit input) <= remaining value /\
  available (step value limit input) <= available value.
Proof.
  intros value limit input. destruct input; simpl; try lia.
  destruct (plan value limit cost bytes allocated) as [next|] eqn:accepted; [|lia].
  apply plan_cost in accepted. destruct accepted as [paid [spent [reserved _]]]. lia.
Qed.

Theorem trace_spent : forall inputs value limit,
  effort value <= effort (run value limit inputs) /\
  remaining (run value limit inputs) <= remaining value /\
  available (run value limit inputs) <= available value.
Proof.
  induction inputs as [|input rest induction]; intros value limit.
  - simpl. lia.
  - change (effort value <= effort (run (step value limit input) limit rest) /\
      remaining (run (step value limit input) limit rest) <= remaining value /\
      available (run (step value limit input) limit rest) <= available value).
    pose proof (step_spent value limit input).
    pose proof (induction (step value limit input) limit). lia.
Qed.

Theorem rollback_keeps_work : forall value limit,
  step value limit Rollback = value.
Proof. reflexivity. Qed.

Theorem replay_cannot_refill : forall inputs value limit cost bytes allocated next,
  plan value limit cost bytes allocated = Some next ->
  remaining (run next limit (Rollback :: inputs)) + bytes <= remaining value /\
  available (run next limit (Rollback :: inputs)) + allocated <= available value.
Proof.
  intros inputs value limit cost bytes allocated next accepted.
  apply plan_cost in accepted. destruct accepted as [_ [spent [reserved _]]].
  pose proof (trace_spent (Rollback :: inputs) next limit). lia.
Qed.

Definition publish {A : Type} value limit cost bytes allocated (writes : list A) :=
  match plan value limit cost bytes allocated with
  | Some next => (next, writes)
  | None => (value, [])
  end.

Theorem refusal_no_writes : forall (A : Type) value limit cost bytes allocated (writes : list A),
  plan value limit cost bytes allocated = None ->
  publish value limit cost bytes allocated writes = (value, []).
Proof. intros. unfold publish. rewrite H. reflexivity. Qed.

Theorem complete_write_plan : forall (A : Type) value limit cost bytes allocated
  (writes : list A) next,
  plan value limit cost bytes allocated = Some next ->
  publish value limit cost bytes allocated writes = (next, writes).
Proof. intros. unfold publish. rewrite H. reflexivity. Qed.

Theorem reserve_opcode : forall value limit parsing opcode bytes allocated checked,
  plan value limit (parsing + opcode) bytes allocated = Some checked ->
  exists reserved,
    plan value limit parsing bytes allocated = Some reserved /\
    plan reserved limit opcode 0 0 = Some checked.
Proof.
  intros [used free space] limit parsing opcode bytes allocated checked accepted.
  unfold plan in accepted. simpl in accepted.
  destruct (((used + (parsing + opcode) <=? limit) && (bytes <=? free))
    && (allocated <=? space)) eqn:checks; [|discriminate].
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[paid fits] memory].
  apply Nat.leb_le in paid.
  assert (first : (used + parsing <=? limit) = true) by
    (apply Nat.leb_le; lia).
  assert (second : (used + parsing + opcode <=? limit) = true) by
    (apply Nat.leb_le; lia).
  inversion accepted; subst.
  exists (Budget (used + parsing) (free - bytes) (space - allocated)).
  split.
  - unfold plan. simpl. rewrite first, fits, memory. reflexivity.
  - unfold plan. simpl. rewrite second. simpl.
    repeat rewrite Nat.sub_0_r.
    replace (used + parsing + opcode) with (used + (parsing + opcode)) by lia.
    reflexivity.
Qed.

Theorem quote_covers : forall value limit parsing quoted actual bytes allocated checked,
  plan value limit (parsing + quoted) bytes allocated = Some checked ->
  actual <= quoted ->
  exists reserved next,
    plan value limit parsing bytes allocated = Some reserved /\
    plan reserved limit actual 0 0 = Some next /\
    effort next <= effort checked /\
    remaining next = remaining checked /\ available next = available checked.
Proof.
  intros value limit parsing quoted actual bytes allocated checked accepted fits.
  destruct (reserve_opcode value limit parsing quoted bytes allocated checked accepted)
    as [reserved [loaded priced]].
  destruct (plan_cost reserved limit quoted 0 0 checked priced)
    as [used [written [memory paid]]].
  destruct reserved as [before free space]. simpl in *.
  exists (Budget before free space), (Budget (before + actual) free space).
  split; [exact loaded|]. split.
  - unfold plan. simpl.
    assert (enough : (before + actual <=? limit) = true) by
      (apply Nat.leb_le; lia).
    rewrite enough. simpl. repeat rewrite Nat.sub_0_r. reflexivity.
  - simpl. repeat split; lia.
Qed.

Example omitted_work :
  plan (Budget 30 0 1000) 35 4 0 88 = Some (Budget 34 0 912) /\
  plan (Budget 34 0 912) 35 2 0 0 = None.
Proof. split; reflexivity. Qed.

Print Assumptions quote_covers.

Module CopyWork.

Definition price unit size := (size + unit - 1) / unit.

Definition charge value limit unit cap left right :=
  if (0 <? unit) && (left + right <=? cap) then
    plan value limit (3 + price unit (left + right)) 0 (left + right)
  else None.

Theorem price_covers : forall unit size,
  0 < unit -> size <= unit * price unit size.
Proof.
  intros unit size positive. unfold price.
  pose proof (Nat.div_mod (size + unit - 1) unit ltac:(lia)) as exact.
  pose proof (Nat.mod_upper_bound (size + unit - 1) unit ltac:(lia)) as rest.
  nia.
Qed.

Theorem charge_paid : forall value limit unit cap left right next,
  charge value limit unit cap left right = Some next ->
  left + right <= cap /\
  available next + left + right = available value /\
  effort value <= effort next /\ effort next <= limit /\
  left + right <= unit * (effort next - effort value).
Proof.
  intros value limit unit cap left right next accepted.
  unfold charge in accepted.
  destruct ((0 <? unit) && (left + right <=? cap)) eqn:checks; [|discriminate].
  apply andb_true_iff in checks. destruct checks as [positive fits].
  apply Nat.ltb_lt in positive. apply Nat.leb_le in fits.
  apply plan_cost in accepted. destruct accepted as [paid [_ [space limited]]].
  pose proof (price_covers unit (left + right) positive). repeat split; nia.
Qed.

Fixpoint trace value limit unit cap pairs :=
  match pairs with
  | [] => Some value
  | (first, second) :: rest =>
    match charge value limit unit cap first second with
    | Some next => trace next limit unit cap rest
    | None => None
    end
  end.

Fixpoint outputs (pairs : list (nat * nat)) :=
  match pairs with
  | [] => 0
  | (first, second) :: rest => first + second + outputs rest
  end.

Theorem trace_paid : forall pairs value limit unit cap next,
  trace value limit unit cap pairs = Some next ->
  outputs pairs + available next = available value /\
  outputs pairs <= unit * (effort next - effort value) /\
  effort value <= effort next.
Proof.
  induction pairs as [|[left right] rest induction];
    intros value limit unit cap next accepted.
  - inversion accepted; subst. simpl. rewrite Nat.sub_diag. lia.
  - simpl in accepted.
    destruct (charge value limit unit cap left right) as [first|] eqn:paid; [|discriminate].
    specialize (induction _ _ _ _ _ accepted).
    apply charge_paid in paid. simpl outputs. nia.
Qed.

Theorem oversized_refused : forall value limit unit cap left right,
  cap < left + right -> charge value limit unit cap left right = None.
Proof.
  intros value limit unit cap left right exceeds. unfold charge.
  assert (checked : (left + right <=? cap) = false) by (apply Nat.leb_gt; lia).
  rewrite checked, andb_false_r. reflexivity.
Qed.

Fixpoint execute (inputs : list (option (nat * nat))) value limit unit cap :=
  match inputs with
  | [] => (value, 0)
  | None :: rest => execute rest value limit unit cap
  | Some (first, second) :: rest =>
    match charge value limit unit cap first second with
    | None => execute rest value limit unit cap
    | Some next =>
      let '(last, total) := execute rest next limit unit cap in
      (last, first + second + total)
    end
  end.

Theorem execute_total : forall inputs value limit unit cap last total,
  execute inputs value limit unit cap = (last, total) ->
  available last + total = available value /\
  effort value <= effort last /\
  total <= unit * (effort last - effort value).
Proof.
  induction inputs as [|input rest ih];
    intros value limit unit cap last total executed.
  - simpl in executed. inversion executed; subst. repeat split; nia.
  - destruct input as [[left right]|]; cbn [execute] in executed.
    + destruct (charge value limit unit cap left right) as [next|] eqn:copied.
      * destruct (execute rest next limit unit cap) as [result count] eqn:rest_run.
        inversion executed; subst.
        pose proof (ih _ _ _ _ _ _ rest_run).
        pose proof (charge_paid _ _ _ _ _ _ _ copied).
        repeat split; nia.
      * exact (ih _ _ _ _ _ _ executed).
    + exact (ih _ _ _ _ _ _ executed).
Qed.

Theorem execute_limit : forall inputs value limit unit cap last total,
  effort value <= limit ->
  execute inputs value limit unit cap = (last, total) -> effort last <= limit.
Proof.
  induction inputs as [|input rest ih];
    intros value limit unit cap last total initial executed.
  - simpl in executed. inversion executed; subst. exact initial.
  - destruct input as [[left right]|]; cbn [execute] in executed.
    + destruct (charge value limit unit cap left right) as [next|] eqn:copied.
      * destruct (execute rest next limit unit cap) as [result count] eqn:rest_run.
        inversion executed; subst.
        destruct (charge_paid _ _ _ _ _ _ _ copied) as [_ [_ [_ [fits _]]]].
        exact (ih _ _ _ _ _ _ fits rest_run).
      * exact (ih _ _ _ _ _ _ initial executed).
    + exact (ih _ _ _ _ _ _ initial executed).
Qed.

Theorem aggregate_limit : forall inputs value limit unit cap last total,
  effort value <= limit ->
  execute inputs value limit unit cap = (last, total) ->
  total <= available value /\ total <= unit * (limit - effort value).
Proof.
  intros inputs value limit unit cap last total initial executed.
  pose proof (execute_total _ _ _ _ _ _ _ executed).
  pose proof (execute_limit _ _ _ _ _ _ _ initial executed).
  split; nia.
Qed.

Print Assumptions execute_total.
Print Assumptions execute_limit.
Print Assumptions aggregate_limit.
End CopyWork.

Module EventWork.

Record quota := Quota { bytes : nat; count : nat }.

Definition emit value size :=
  if (size <=? bytes value) && (0 <? count value)
  then Some (Quota (bytes value - size) (count value - 1))
  else None.

Fixpoint publish value sizes :=
  match sizes with
  | [] => Some value
  | size :: rest =>
    match emit value size with
    | Some next => publish next rest
    | None => None
    end
  end.

Theorem emit_exact : forall value size next,
  emit value size = Some next ->
  bytes next + size = bytes value /\ count next + 1 = count value.
Proof.
  intros [space slots] size next accepted.
  unfold emit in accepted. simpl in accepted.
  destruct ((size <=? space) && (0 <? slots)) eqn:checked; [|discriminate].
  apply andb_true_iff in checked. destruct checked as [fits positive].
  apply Nat.leb_le in fits. apply Nat.ltb_lt in positive.
  inversion accepted; subst. simpl. lia.
Qed.

Theorem publish_exact : forall sizes value next,
  publish value sizes = Some next ->
  bytes next + fold_right Nat.add 0 sizes = bytes value /\
  count next + length sizes = count value.
Proof.
  induction sizes as [|size rest ih]; intros value next accepted.
  - simpl in accepted. inversion accepted; subst. simpl. lia.
  - simpl in accepted.
    destruct (emit value size) as [first|] eqn:charged; [|discriminate].
    pose proof (emit_exact _ _ _ charged).
    specialize (ih _ _ accepted). simpl. lia.
Qed.

Lemma sum_join : forall first second,
  fold_right Nat.add 0 (first ++ second) =
  fold_right Nat.add 0 first + fold_right Nat.add 0 second.
Proof.
  induction first as [|value rest ih]; intros second; simpl; [lia|].
  rewrite ih. lia.
Qed.

Theorem shared_limit : forall first second value next,
  publish value (first ++ second) = Some next ->
  fold_right Nat.add 0 first + fold_right Nat.add 0 second <= bytes value /\
  length first + length second <= count value.
Proof.
  intros first second value next accepted.
  apply publish_exact in accepted.
  rewrite sum_join, length_app in accepted. lia.
Qed.

Print Assumptions shared_limit.

Definition output value size :=
  if size <=? bytes value
  then Some (Quota (bytes value - size) (count value))
  else None.

Theorem output_exact : forall value size next,
  output value size = Some next ->
  bytes next + size = bytes value /\ count next = count value.
Proof.
  intros [space slots] size next accepted.
  unfold output in accepted. simpl in accepted.
  destruct (size <=? space) eqn:fits; [|discriminate].
  apply Nat.leb_le in fits.
  inversion accepted; subst. simpl. lia.
Qed.

Inductive piece := Log (size : nat) | Reply (size : nat).

Definition size item := match item with Log n | Reply n => n end.
Definition slots item := match item with Log _ => 1 | Reply _ => 0 end.
Definition write value item :=
  match item with Log n => emit value n | Reply n => output value n end.

Fixpoint collect value items :=
  match items with
  | [] => Some value
  | item :: rest =>
    match write value item with
    | Some next => collect next rest
    | None => None
    end
  end.

Theorem collect_exact : forall items value next,
  collect value items = Some next ->
  bytes next + fold_right (fun item total => size item + total) 0 items = bytes value /\
  count next + fold_right (fun item total => slots item + total) 0 items = count value.
Proof.
  induction items as [|item rest ih]; intros value next accepted.
  - simpl in accepted. inversion accepted; subst. simpl. lia.
  - simpl in accepted.
    destruct (write value item) as [first|] eqn:charged; [|discriminate].
    specialize (ih _ _ accepted).
    destruct item; simpl in charged |- *.
    + apply emit_exact in charged. lia.
    + apply output_exact in charged. lia.
Qed.

Theorem receipt_limit : forall items value next,
  collect value items = Some next ->
  fold_right (fun item total => size item + total) 0 items <= bytes value /\
  fold_right (fun item total => slots item + total) 0 items <= count value.
Proof.
  intros items value next accepted. apply collect_exact in accepted. lia.
Qed.

Print Assumptions receipt_limit.
End EventWork.

Module B64Size.

Definition size wire padding :=
  3 * (wire / 4) + (wire mod 4 - 1) - padding.

Definition buffer wire :=
  3 * (wire / 4 + if wire mod 4 =? 0 then 0 else 1).

Definition wire groups rest :=
  4 * (groups + if rest =? 0 then 0 else 1).

Definition padding rest :=
  match rest with 0 => 0 | 1 => 2 | _ => 1 end.

Definition allocate encoded pad limit :=
  if size encoded pad <=? limit then Some (buffer encoded) else None.

Lemma groups_size : forall groups pad,
  size (4 * groups) pad = 3 * groups - pad.
Proof.
  intros groups pad. unfold size.
  replace (4 * groups) with (groups * 4) by lia.
  rewrite Nat.div_mul, Nat.Div0.mod_mul by lia. simpl. lia.
Qed.

Lemma groups_buffer : forall groups,
  buffer (4 * groups) = 3 * groups.
Proof.
  intros groups. unfold buffer.
  replace (4 * groups) with (groups * 4) by lia.
  rewrite Nat.div_mul, Nat.Div0.mod_mul by lia. simpl. lia.
Qed.

Theorem exact_size : forall groups rest,
  rest < 3 -> size (wire groups rest) (padding rest) = 3 * groups + rest.
Proof.
  intros groups rest valid.
  destruct rest as [|[|[|rest]]]; try lia;
    unfold wire, padding; rewrite groups_size; simpl; lia.
Qed.

Theorem valid_gate : forall groups rest limit,
  rest < 3 ->
  (allocate (wire groups rest) (padding rest) limit =
    Some (buffer (wire groups rest)) <-> 3 * groups + rest <= limit).
Proof.
  intros groups rest limit valid. unfold allocate.
  rewrite exact_size by assumption.
  destruct (3 * groups + rest <=? limit) eqn:fits.
  - apply Nat.leb_le in fits. split; auto.
  - apply Nat.leb_gt in fits. split; intros; [discriminate|lia].
Qed.

Theorem valid_refusal : forall groups rest limit,
  rest < 3 -> limit < 3 * groups + rest ->
  allocate (wire groups rest) (padding rest) limit = None.
Proof.
  intros groups rest limit valid large. unfold allocate.
  rewrite exact_size by assumption.
  assert (fits : (3 * groups + rest <=? limit) = false) by
    (apply Nat.leb_gt; lia).
  rewrite fits. reflexivity.
Qed.

Theorem valid_capacity : forall groups rest limit allocated,
  rest < 3 ->
  allocate (wire groups rest) (padding rest) limit = Some allocated ->
  allocated <= limit + 2.
Proof.
  intros groups rest limit allocated valid accepted.
  unfold allocate in accepted. rewrite exact_size in accepted by assumption.
  destruct (3 * groups + rest <=? limit) eqn:fits; [|discriminate].
  apply Nat.leb_le in fits. inversion accepted; subst allocated.
  unfold wire. rewrite groups_buffer.
  destruct rest as [|[|[|rest]]]; simpl; lia.
Qed.

Theorem size_fits : forall encoded maximum,
  encoded <= maximum ->
  3 * (encoded / 4) <= maximum /\
  3 * (encoded / 4) + (encoded mod 4 - 1) <= maximum.
Proof.
  intros encoded maximum fits.
  pose proof (Nat.div_mod encoded 4 ltac:(lia)) as parts.
  pose proof (Nat.mod_upper_bound encoded 4 ltac:(lia)) as rest.
  lia.
Qed.

Theorem all_capacity : forall encoded pad limit allocated,
  pad <= 2 -> allocate encoded pad limit = Some allocated ->
  allocated <= limit + 5.
Proof.
  intros encoded pad limit allocated pads accepted.
  unfold allocate in accepted.
  destruct (size encoded pad <=? limit) eqn:fits; [|discriminate].
  apply Nat.leb_le in fits. inversion accepted; subst allocated.
  assert (capacity : buffer encoded <= 3 * (encoded / 4) + 3).
  { unfold buffer. destruct (encoded mod 4 =? 0); lia. }
  assert (floor : 3 * (encoded / 4) <= size encoded pad + pad).
  { unfold size. lia. }
  lia.
Qed.

End B64Size.

Module PutCheck.

Record image := Image { entries : nat; bytes : nat }.

Definition fits value entry_cap byte_cap word_cap :=
  entries value <= entry_cap /\ bytes value <= byte_cap /\ bytes value <= word_cap.

Definition plan value prior size (entry_cap byte_cap word_cap : nat) :=
  match prior with
  | Some length => if length =? size then Some value else None
  | None =>
    if ((entries value <? entry_cap) && (bytes value + size <=? word_cap))
       && (bytes value + size <=? byte_cap)
    then Some (Image (S (entries value)) (bytes value + size))
    else None
  end.

Example entry_limit_case :
  plan (Image 2 0) None 1 2 8 16 = None.
Proof. reflexivity. Qed.

Theorem plan_preserves : forall value prior size entry_cap byte_cap word_cap next,
  fits value entry_cap byte_cap word_cap ->
  plan value prior size entry_cap byte_cap word_cap = Some next ->
  fits next entry_cap byte_cap word_cap.
Proof.
  intros [count weight] prior size entry_cap byte_cap word_cap next safe accepted.
  unfold plan in accepted. destruct prior as [length|].
  - destruct (length =? size); inversion accepted; subst. exact safe.
  - simpl in accepted.
    destruct (((count <? entry_cap) && (weight + size <=? word_cap))
      && (weight + size <=? byte_cap)) eqn:checks; [|discriminate].
    repeat rewrite andb_true_iff in checks.
    destruct checks as [[slots words] total].
    apply Nat.ltb_lt in slots. apply Nat.leb_le in words. apply Nat.leb_le in total.
    inversion accepted; subst. unfold fits. simpl. lia.
Qed.

Theorem duplicate_kept : forall value size entry_cap byte_cap word_cap,
  plan value (Some size) size entry_cap byte_cap word_cap = Some value.
Proof. intros. unfold plan. rewrite Nat.eqb_refl. reflexivity. Qed.

Theorem full_entries_refused : forall value size entry_cap byte_cap word_cap,
  entry_cap <= entries value ->
  plan value None size entry_cap byte_cap word_cap = None.
Proof.
  intros value size entry_cap byte_cap word_cap full. unfold plan.
  assert (slots : (entries value <? entry_cap) = false) by (apply Nat.ltb_ge; lia).
  rewrite slots. reflexivity.
Qed.

Theorem full_bytes_refused : forall value size entry_cap byte_cap word_cap,
  byte_cap < bytes value + size ->
  plan value None size entry_cap byte_cap word_cap = None.
Proof.
  intros value size entry_cap byte_cap word_cap full. unfold plan.
  assert (total : (bytes value + size <=? byte_cap) = false) by (apply Nat.leb_gt; lia).
  rewrite total, andb_false_r. reflexivity.
Qed.

Theorem overflow_refused : forall value size entry_cap byte_cap word_cap,
  word_cap < bytes value + size ->
  plan value None size entry_cap byte_cap word_cap = None.
Proof.
  intros value size entry_cap byte_cap word_cap full. unfold plan.
  assert (words : (bytes value + size <=? word_cap) = false) by (apply Nat.leb_gt; lia).
  rewrite words, andb_false_r. reflexivity.
Qed.

Definition commit value prior size entry_cap byte_cap word_cap expected actual writing :=
  if (expected =? actual) && writing
  then plan value prior size entry_cap byte_cap word_cap
  else None.

Theorem foreign_put_refused : forall value prior size entry_cap byte_cap word_cap expected actual writing,
  expected <> actual ->
  commit value prior size entry_cap byte_cap word_cap expected actual writing = None.
Proof.
  intros value prior size entry_cap byte_cap word_cap expected actual writing different.
  unfold commit. apply Nat.eqb_neq in different. rewrite different. reflexivity.
Qed.

Theorem commit_preserves : forall value prior size entry_cap byte_cap word_cap expected actual writing next,
  fits value entry_cap byte_cap word_cap ->
  commit value prior size entry_cap byte_cap word_cap expected actual writing = Some next ->
  fits next entry_cap byte_cap word_cap.
Proof.
  intros value prior size entry_cap byte_cap word_cap expected actual writing next safe accepted.
  unfold commit in accepted. destruct ((expected =? actual) && writing); [|discriminate].
  eapply plan_preserves; eauto.
Qed.

Definition step value prior size entry_cap byte_cap word_cap expected actual writing :=
  match commit value prior size entry_cap byte_cap word_cap expected actual writing with
  | Some next => next
  | None => value
  end.

Definition run value inputs entry_cap byte_cap word_cap expected actual writing :=
  fold_left (fun value input =>
    let '(prior, size) := input in
    step value prior size entry_cap byte_cap word_cap expected actual writing) inputs value.

Theorem trace_preserves : forall inputs value entry_cap byte_cap word_cap expected actual writing,
  fits value entry_cap byte_cap word_cap ->
  fits (run value inputs entry_cap byte_cap word_cap expected actual writing)
    entry_cap byte_cap word_cap.
Proof.
  induction inputs as [|[prior size] rest ih]; intros value entry_cap byte_cap word_cap expected actual writing safe.
  - exact safe.
  - unfold run. simpl. fold (run (step value prior size entry_cap byte_cap word_cap expected actual writing)
      rest entry_cap byte_cap word_cap expected actual writing).
    apply ih. unfold step.
    destruct (commit value prior size entry_cap byte_cap word_cap expected actual writing) eqn:accepted;
      [eapply commit_preserves; eauto|exact safe].
Qed.

Example changed_capacity :
  plan (Image 0 0) None 1 1 8 8 = Some (Image 1 1) /\
  commit (Image 1 1) None 1 1 8 8 7 7 true = None.
Proof. split; reflexivity. Qed.

Print Assumptions plan_preserves.
Print Assumptions duplicate_kept.
Print Assumptions full_entries_refused.
Print Assumptions full_bytes_refused.
Print Assumptions overflow_refused.
Print Assumptions foreign_put_refused.
Print Assumptions commit_preserves.
Print Assumptions trace_preserves.
End PutCheck.

Module Memory.

Record block := Block { bytes : nat; refs : nat }.

Fixpoint total rows :=
  match rows with [] => 0 | row :: rest => bytes row + total rest end.

Definition reserve cap size rows :=
  if total rows + size <=? cap then Some (Block size 1 :: rows) else None.

Fixpoint retain index rows :=
  match rows, index with
  | [], _ => []
  | row :: rest, 0 => Block (bytes row) (S (refs row)) :: rest
  | row :: rest, S index => row :: retain index rest
  end.

Fixpoint release index rows :=
  match rows, index with
  | [], _ => []
  | row :: rest, 0 =>
    if refs row <=? 1 then rest else Block (bytes row) (pred (refs row)) :: rest
  | row :: rest, S index => row :: release index rest
  end.

Theorem reserve_limit : forall cap size rows next,
  reserve cap size rows = Some next -> total next <= cap.
Proof.
  intros cap size rows next accepted. unfold reserve in accepted.
  destruct (total rows + size <=? cap) eqn:fits; [|discriminate].
  apply Nat.leb_le in fits. inversion accepted; subst. simpl. lia.
Qed.

Theorem retain_total : forall rows index,
  total (retain index rows) = total rows.
Proof.
  induction rows as [|row rest ih]; intros [|index]; simpl; auto.
Qed.

Theorem release_limit : forall rows index,
  total (release index rows) <= total rows.
Proof.
  induction rows as [|row rest ih]; intros [|index]; simpl; auto.
  - destruct (refs row <=? 1); simpl; lia.
  - specialize (ih index). lia.
Qed.

Theorem release_shared : forall rows index row,
  nth_error rows index = Some row -> 1 < refs row ->
  total (release index rows) = total rows.
Proof.
  induction rows as [|head rest ih]; intros [|index] row found shared;
    simpl in found; try discriminate.
  - inversion found; subst. simpl.
    assert (many : (refs row <=? 1) = false) by (apply Nat.leb_gt; lia).
    rewrite many. reflexivity.
  - simpl. rewrite (ih index row found shared). reflexivity.
Qed.

Theorem release_last : forall rows index row,
  nth_error rows index = Some row -> refs row = 1 ->
  total (release index rows) + bytes row = total rows.
Proof.
  induction rows as [|head rest ih]; intros [|index] row found last;
    simpl in found; try discriminate.
  - inversion found; subst. simpl. rewrite last. simpl. lia.
  - simpl. specialize (ih index row found last). lia.
Qed.

Theorem retain_release : forall rows index row,
  nth_error rows index = Some row -> 0 < refs row ->
  release index (retain index rows) = rows.
Proof.
  induction rows as [|head rest ih]; intros [|index] row found live;
    simpl in found; try discriminate.
  - inversion found; subst. destruct row as [size [|count]];
      [simpl in live; lia|reflexivity].
  - simpl. rewrite (ih index row found live). reflexivity.
Qed.

Theorem shared_replace_refused : forall rows index row cap size,
  nth_error rows index = Some row -> 1 < refs row ->
  cap < total rows + size -> reserve cap size (release index rows) = None.
Proof.
  intros rows index row cap size found shared exceeds.
  unfold reserve. rewrite (release_shared rows index row found shared).
  assert (full : (total rows + size <=? cap) = false) by
    (apply Nat.leb_gt; lia).
  rewrite full. reflexivity.
Qed.

Theorem last_replace_size : forall rows index row cap size next,
  nth_error rows index = Some row -> refs row = 1 ->
  reserve cap size (release index rows) = Some next ->
  total next + bytes row = total rows + size /\ total next <= cap.
Proof.
  intros rows index row cap size next found last accepted.
  pose proof (release_last rows index row found last) as freed.
  pose proof (reserve_limit cap size (release index rows) next accepted) as fits.
  unfold reserve in accepted.
  destruct (total (release index rows) + size <=? cap); [|discriminate].
  inversion accepted; subst. simpl in *. split; lia.
Qed.

Definition replace cap index size rows :=
  match reserve cap size rows with
  | Some next => release (S index) next
  | None => rows
  end.

Theorem replace_refused : forall rows index cap size,
  cap < total rows + size -> replace cap index size rows = rows.
Proof.
  intros rows index cap size exceeds. unfold replace, reserve.
  assert (full : (total rows + size <=? cap) = false) by
    (apply Nat.leb_gt; exact exceeds).
  rewrite full. reflexivity.
Qed.

Theorem replace_limit : forall rows index cap size,
  total rows <= cap -> total (replace cap index size rows) <= cap.
Proof.
  intros rows index cap size fits. unfold replace.
  destruct (reserve cap size rows) as [next|] eqn:accepted; [|exact fits].
  eapply Nat.le_trans; [apply release_limit|eapply reserve_limit; eauto].
Qed.

Theorem replace_peak : forall rows cap size next,
  reserve cap size rows = Some next ->
  total next = total rows + size /\ total next <= cap.
Proof.
  intros rows cap size next accepted.
  pose proof (reserve_limit cap size rows next accepted) as fits.
  unfold reserve in accepted.
  destruct (total rows + size <=? cap); [|discriminate].
  inversion accepted; subst. simpl in *. split; lia.
Qed.

Theorem replace_owned : forall rows index row cap size,
  nth_error rows index = Some row -> refs row = 1 ->
  total rows + size <= cap ->
  total (replace cap index size rows) + bytes row = total rows + size.
Proof.
  intros rows index row cap size found last fits. unfold replace, reserve.
  assert (room : (total rows + size <=? cap) = true) by
    (apply Nat.leb_le; exact fits).
  rewrite room. simpl.
  pose proof (release_last rows index row found last) as freed. lia.
Qed.

Theorem replace_shared : forall rows index row cap size,
  nth_error rows index = Some row -> 1 < refs row ->
  total rows + size <= cap ->
  total (replace cap index size rows) = total rows + size.
Proof.
  intros rows index row cap size found shared fits. unfold replace, reserve.
  assert (room : (total rows + size <=? cap) = true) by
    (apply Nat.leb_le; exact fits).
  rewrite room. simpl.
  rewrite (release_shared rows index row found shared). lia.
Qed.

Example replace_overlap :
  replace 12 0 8 [Block 8 1] = [Block 8 1] /\
  reserve 12 8 (release 0 [Block 8 1]) = Some [Block 8 1].
Proof. split; reflexivity. Qed.

Fixpoint set_size index size rows :=
  match rows, index with
  | [], _ => []
  | row :: rest, 0 => Block size (refs row) :: rest
  | row :: rest, S index => row :: set_size index size rest
  end.

Definition resize cap index size rows :=
  match nth_error rows index with
  | Some row =>
    if Nat.eqb (refs row) 1 && (total rows - bytes row + size <=? cap)
    then Some (set_size index size rows) else None
  | None => None
  end.

Theorem set_size_total : forall rows index row size,
  nth_error rows index = Some row ->
  total (set_size index size rows) + bytes row = total rows + size.
Proof.
  induction rows as [|head rest ih]; intros [|index] row size found;
    simpl in found; try discriminate.
  - inversion found; subst. simpl. lia.
  - simpl. specialize (ih index row size found). lia.
Qed.

Theorem resize_exact : forall rows index row cap size next,
  nth_error rows index = Some row -> resize cap index size rows = Some next ->
  total next + bytes row = total rows + size /\ total next <= cap.
Proof.
  intros rows index row cap size next found accepted.
  unfold resize in accepted. rewrite found in accepted.
  destruct (Nat.eqb (refs row) 1 && (total rows - bytes row + size <=? cap))
    eqn:room; [|discriminate].
  apply andb_true_iff in room as [_ room]. apply Nat.leb_le in room.
  inversion accepted; subst.
  pose proof (set_size_total rows index row size found) as exact.
  pose proof (set_size_total rows index row 0 found) as present.
  split; lia.
Qed.

Theorem resize_undo : forall rows index row size,
  nth_error rows index = Some row ->
  set_size index (bytes row) (set_size index size rows) = rows.
Proof.
  induction rows as [|head rest ih]; intros [|index] row size found;
    simpl in found; try discriminate.
  - inversion found; subst. destruct row. reflexivity.
  - simpl. rewrite (ih index row size found). reflexivity.
Qed.

Theorem resize_other : forall rows index other size,
  index <> other ->
  nth_error (set_size index size rows) other = nth_error rows other.
Proof.
  induction rows as [|row rest ih]; intros [|index] [|other] size different;
    simpl; try reflexivity; try contradiction.
  apply ih. lia.
Qed.

Example resize_full :
  resize 12 0 9 [Block 2 1; Block 4 1] = None /\
  resize 12 0 8 [Block 2 1; Block 4 1] = Some [Block 8 1; Block 4 1].
Proof. split; reflexivity. Qed.

Definition reserve_cells cap width count rows := reserve cap (width * count) rows.

Theorem cells_exact : forall cap width count rows next,
  reserve_cells cap width count rows = Some next ->
  total next = total rows + width * count /\
  total next <= cap /\ release 0 next = rows.
Proof.
  intros cap width count rows next accepted.
  unfold reserve_cells in accepted.
  pose proof (reserve_limit cap (width * count) rows next accepted) as fits.
  unfold reserve in accepted.
  destruct (total rows + width * count <=? cap); [|discriminate].
  inversion accepted; subst. simpl. repeat split; auto; lia.
Qed.

Theorem cells_pair : forall cap width count scratch rows output both,
  reserve_cells cap width count rows = Some output ->
  reserve_cells cap width scratch output = Some both ->
  total both = total rows + width * (count + scratch) /\
  total both <= cap /\ release 0 (release 0 both) = rows.
Proof.
  intros cap width count scratch rows output both first second.
  destruct (cells_exact cap width count rows output first)
    as [out_total [_ out_release]].
  destruct (cells_exact cap width scratch output both second)
    as [both_total [fits scratch_release]].
  split; [nia|]. split; [exact fits|].
  now rewrite scratch_release, out_release.
Qed.

Theorem cells_refused : forall cap width count scratch rows output,
  reserve_cells cap width count rows = Some output ->
  reserve_cells cap width scratch output = None ->
  cap < total rows + width * (count + scratch) /\ release 0 output = rows.
Proof.
  intros cap width count scratch rows output first second.
  destruct (cells_exact cap width count rows output first)
    as [out_total [_ out_release]].
  unfold reserve_cells, reserve in second.
  destruct (total output + width * scratch <=? cap) eqn:room; [discriminate|].
  apply Nat.leb_gt in room. split; [nia|exact out_release].
Qed.

Fixpoint reserve_many cap sizes rows :=
  match sizes with
  | [] => Some rows
  | size :: rest =>
    match reserve cap size rows with
    | None => None
    | Some next => reserve_many cap rest next
    end
  end.

Fixpoint release_many count rows :=
  match count with
  | 0 => rows
  | S rest => release_many rest (release 0 rows)
  end.

Lemma release_many_step : forall count rows,
  release_many (S count) rows = release 0 (release_many count rows).
Proof.
  induction count as [|count ih]; intros rows; simpl; [reflexivity|].
  apply ih.
Qed.

Theorem many_exact : forall sizes cap rows next,
  reserve_many cap sizes rows = Some next ->
  total next = total rows + fold_right Nat.add 0 sizes /\
  release_many (length sizes) next = rows.
Proof.
  induction sizes as [|size rest ih]; intros cap rows next accepted; simpl in accepted.
  - inversion accepted; subst. simpl. split; [lia|reflexivity].
  - unfold reserve in accepted.
    destruct (total rows + size <=? cap); [|discriminate].
    specialize (ih cap (Block size 1 :: rows) next accepted).
    destruct ih as [amount freed]. split; [simpl in *; lia|].
    change (release_many (S (length rest)) next = rows).
    rewrite release_many_step. rewrite freed. reflexivity.
Qed.

Theorem many_limit : forall sizes cap rows next,
  total rows <= cap -> reserve_many cap sizes rows = Some next -> total next <= cap.
Proof.
  induction sizes as [|size rest ih]; intros cap rows next fits accepted;
    simpl in accepted.
  - inversion accepted; subst. exact fits.
  - destruct (reserve cap size rows) as [loaded|] eqn:reserved; [|discriminate].
    eapply ih; [eapply reserve_limit; eauto|exact accepted].
Qed.

Theorem many_complete : forall sizes cap rows,
  total rows + fold_right Nat.add 0 sizes <= cap ->
  exists next, reserve_many cap sizes rows = Some next.
Proof.
  induction sizes as [|size rest ih]; intros cap rows fits; simpl in *.
  - exists rows. reflexivity.
  - assert (total rows + size <=? cap = true) as first by (apply Nat.leb_le; lia).
    unfold reserve. rewrite first. apply ih. simpl. lia.
Qed.

Definition join rows :=
  match rows with
  | Block first 1 :: Block second 1 :: rest => Some (Block (first + second) 1 :: rest)
  | _ => None
  end.

Theorem join_total : forall rows next,
  join rows = Some next -> total next = total rows.
Proof.
  intros [|[first [|[|count]]] [|[second [|[|other]]] rest]] next accepted;
    simpl in accepted; try discriminate.
  inversion accepted; subst. simpl. lia.
Qed.

Theorem join_release : forall rows next,
  join rows = Some next -> release 0 next = release 0 (release 0 rows).
Proof.
  intros [|[first [|[|count]]] [|[second [|[|other]]] rest]] next accepted;
    simpl in accepted; try discriminate.
  inversion accepted; subst. reflexivity.
Qed.

Theorem join_reserved : forall cap first second rows left right next,
  reserve cap first rows = Some left -> reserve cap second left = Some right ->
  join right = Some next ->
  total next = total rows + first + second /\ total next <= cap /\ release 0 next = rows.
Proof.
  intros cap first second rows left right next one two joined.
  pose proof (join_total right next joined) as same.
  pose proof (reserve_limit cap second left right two) as fits.
  unfold reserve in one, two.
  destruct (total rows + first <=? cap); [|discriminate].
  inversion one; subst. simpl in two.
  destruct (first + total rows + second <=? cap); [|discriminate].
  inversion two; subst. simpl in joined. inversion joined; subst.
  simpl in *. repeat split; lia.
Qed.

Inductive event := Allocate (size : nat) | Share (index : nat) | Destroy (index : nat).

Definition step cap rows input :=
  match input with
  | Allocate size => match reserve cap size rows with Some next => next | None => rows end
  | Share index => retain index rows
  | Destroy index => release index rows
  end.

Theorem trace_limit : forall inputs rows cap,
  total rows <= cap ->
  total (fold_left (step cap) inputs rows) <= cap.
Proof.
  induction inputs as [|input rest ih]; intros rows cap fits; simpl; [exact fits|].
  apply ih. destruct input; simpl.
  - destruct (reserve cap size rows) eqn:accepted; [eapply reserve_limit; eauto|exact fits].
  - rewrite retain_total. exact fits.
  - eapply Nat.le_trans; [apply release_limit|exact fits].
Qed.

Example shared_image : release 0 (retain 0 [Block 8 1]) = [Block 8 1].
Proof. reflexivity. Qed.

Example last_image : release 0 [Block 8 1] = [].
Proof. reflexivity. Qed.

Definition resize_guest cap quota index size rows :=
  if size <=? quota then resize cap index size rows else None.

Theorem guest_resize_limits : forall cap quota index size rows row next,
  nth_error rows index = Some row ->
  resize_guest cap quota index size rows = Some next ->
  size <= quota /\ total next <= cap /\
  total next + bytes row = total rows + size.
Proof.
  intros cap quota index size rows row next found accepted.
  unfold resize_guest in accepted.
  destruct (size <=? quota) eqn:fits; [|discriminate].
  apply Nat.leb_le in fits.
  pose proof (resize_exact rows index row cap size next found accepted) as [exact room].
  repeat split; assumption.
Qed.

Theorem guest_resize_other : forall cap quota index other size rows next,
  index <> other -> resize_guest cap quota index size rows = Some next ->
  nth_error next other = nth_error rows other.
Proof.
  intros cap quota index other size rows next different accepted.
  unfold resize_guest, resize in accepted.
  destruct (size <=? quota); [|discriminate].
  destruct (nth_error rows index) as [row|]; [|discriminate].
  destruct (Nat.eqb (refs row) 1 && (total rows - bytes row + size <=? cap));
    [|discriminate].
  inversion accepted; subst. apply resize_other; exact different.
Qed.

Theorem guest_resize_undo : forall cap quota index size rows row next,
  nth_error rows index = Some row ->
  resize_guest cap quota index size rows = Some next ->
  set_size index (bytes row) next = rows.
Proof.
  intros cap quota index size rows row next found accepted.
  unfold resize_guest, resize in accepted.
  destruct (size <=? quota); [|discriminate].
  rewrite found in accepted.
  destruct (Nat.eqb (refs row) 1 && (total rows - bytes row + size <=? cap));
    [|discriminate].
  inversion accepted; subst. eapply resize_undo; exact found.
Qed.

Theorem guest_resize_refused : forall cap quota index size rows,
  quota < size -> resize_guest cap quota index size rows = None.
Proof.
  intros cap quota index size rows denied. unfold resize_guest.
  apply Nat.leb_gt in denied. rewrite denied. reflexivity.
Qed.

Print Assumptions reserve_limit.
Print Assumptions release_shared.
Print Assumptions release_last.
Print Assumptions retain_release.
Print Assumptions shared_replace_refused.
Print Assumptions last_replace_size.
Print Assumptions replace_refused.
Print Assumptions replace_limit.
Print Assumptions replace_peak.
Print Assumptions replace_owned.
Print Assumptions replace_shared.
Print Assumptions resize_exact.
Print Assumptions resize_undo.
Print Assumptions resize_other.
Print Assumptions cells_exact.
Print Assumptions cells_pair.
Print Assumptions cells_refused.
Print Assumptions many_exact.
Print Assumptions many_limit.
Print Assumptions many_complete.
Print Assumptions join_total.
Print Assumptions join_release.
Print Assumptions join_reserved.
Print Assumptions trace_limit.
Print Assumptions guest_resize_limits.
Print Assumptions guest_resize_other.
Print Assumptions guest_resize_undo.
Print Assumptions guest_resize_refused.
End Memory.

Module Response.

Definition grow_size current needed limit :=
  Nat.min limit (Nat.max needed (Nat.max 256 (2 * current))).

Fixpoint copy_work current limit sizes :=
  match sizes with
  | [] => 0
  | size :: rest =>
    if size <=? current then copy_work current limit rest
    else current + copy_work (grow_size current size limit) limit rest
  end.

Example doubled_copy : copy_work 257 1024 [514; 515] = 771.
Proof. reflexivity. Qed.

Example capped_copy : copy_work 514 1024 [515; 1024] = 514.
Proof. reflexivity. Qed.

Theorem grow_limits : forall current needed limit,
  needed <= limit ->
  needed <= grow_size current needed limit /\ grow_size current needed limit <= limit.
Proof.
  intros current needed limit fits. unfold grow_size.
  pose proof (Nat.le_max_l needed (Nat.max 256 (2 * current))).
  split; [apply Nat.min_glb; assumption|apply Nat.le_min_l].
Qed.

Theorem copy_full : forall sizes limit,
  Forall (fun size => size <= limit) sizes -> copy_work limit limit sizes = 0.
Proof.
  induction sizes as [|size rest ih]; intros limit fits; simpl; [reflexivity|].
  inversion fits; subst.
  assert (size <=? limit = true) as allowed by (apply Nat.leb_le; assumption).
  rewrite allowed. apply ih. assumption.
Qed.

Theorem copy_linear : forall sizes current limit,
  current <= limit -> Forall (fun size => size <= limit) sizes ->
  copy_work current limit sizes + current <= 2 * limit.
Proof.
  induction sizes as [|size rest ih]; intros current limit room fits; simpl; [lia|].
  inversion fits as [|? ? size_fits rest_fit]; subst.
  destruct (size <=? current) eqn:reuse.
  - apply ih; assumption.
  - pose proof (grow_limits current size limit size_fits) as [size_next next_fit].
    destruct (le_dec (2 * current) limit) as [doubled|capped].
    + assert (2 * current <= grow_size current size limit) as growth.
      { unfold grow_size. apply Nat.min_glb; [exact doubled|].
        eapply Nat.le_trans; [apply Nat.le_max_r|apply Nat.le_max_r]. }
      pose proof (ih (grow_size current size limit) limit next_fit rest_fit). lia.
    + assert (grow_size current size limit = limit) as full.
      { unfold grow_size. apply Nat.min_l.
        pose proof (Nat.le_max_r 256 (2 * current)).
        pose proof (Nat.le_max_r size (Nat.max 256 (2 * current))). lia. }
      rewrite full, copy_full by assumption. lia.
Qed.

Print Assumptions grow_limits.
Print Assumptions copy_full.
Print Assumptions copy_linear.
End Response.

Module Reply.

Definition handoff cap header word rows :=
  match rows with
  | Memory.Block size 1 :: rest =>
    Memory.resize cap 0 (2 * size + header + 2 * word) rows
  | _ => None
  end.

Example handoff_edge :
  handoff 20 2 1 [Memory.Block 8 1] = Some [Memory.Block 20 1] /\
  handoff 19 2 1 [Memory.Block 8 1] = None.
Proof. split; reflexivity. Qed.

Theorem handoff_exact : forall cap header word size rest next,
  handoff cap header word (Memory.Block size 1 :: rest) = Some next ->
  next = Memory.Block (2 * size + header + 2 * word) 1 :: rest /\
  Memory.total next <= cap /\ Memory.release 0 next = rest.
Proof.
  intros cap header word size rest next accepted.
  change ((if size + Memory.total rest - size + (2 * size + header + 2 * word) <=? cap
    then Some (Memory.Block (2 * size + header + 2 * word) 1 :: rest)
    else None) = Some next) in accepted.
  replace (size + Memory.total rest - size) with (Memory.total rest)
    in accepted by lia.
  destruct (Memory.total rest + (2 * size + header + 2 * word) <=? cap)
    eqn:fits; [|discriminate].
  apply Nat.leb_le in fits. inversion accepted; subst.
  split; [reflexivity|]. split; [|reflexivity].
  cbn [Memory.total Memory.bytes]. lia.
Qed.

Theorem handoff_full : forall cap header word size rest,
  cap < Memory.total rest + 2 * size + header + 2 * word ->
  handoff cap header word (Memory.Block size 1 :: rest) = None.
Proof.
  intros cap header word size rest full.
  change ((if size + Memory.total rest - size + (2 * size + header + 2 * word) <=? cap
    then Some (Memory.Block (2 * size + header + 2 * word) 1 :: rest)
    else None) = None).
  replace (size + Memory.total rest - size) with (Memory.total rest) by lia.
  assert ((Memory.total rest + (2 * size + header + 2 * word) <=? cap) = false)
    as refused by (apply Nat.leb_gt; lia).
  rewrite refused. reflexivity.
Qed.

Example early_release :
  Memory.reserve 19 12 (Memory.release 0 [Memory.Block 8 1]) =
    Some [Memory.Block 12 1] /\ 19 < 8 + 12.
Proof. split; [reflexivity|lia]. Qed.

Print Assumptions handoff_exact.
Print Assumptions handoff_full.
End Reply.

Module ShardRead.
Section Read.
Context {A : Type}.

Fixpoint read count (segments : list (list A)) :=
  match segments with
  | [] => ([], [], count)
  | segment :: rest =>
    if count <=? length segment then
      (firstn count segment, skipn count segment :: rest, 0)
    else
      let '(value, suffix, missing) := read (count - length segment) rest in
      (segment ++ value, suffix, missing)
  end.

Theorem read_accounted : forall segments count value suffix missing,
  read count segments = (value, suffix, missing) ->
  value ++ concat suffix = concat segments /\ length value + missing = count.
Proof.
  induction segments as [|segment rest ih]; intros count value suffix missing got.
  - simpl in got. inversion got; subst. simpl. auto.
  - cbn [read] in got. destruct (count <=? length segment) eqn:fits.
    + inversion got; subst. simpl. rewrite app_assoc, firstn_skipn.
      rewrite length_firstn. apply Nat.leb_le in fits.
      rewrite Nat.min_l by exact fits. auto.
    + destruct (read (count - length segment) rest)
        as [[part remainder] shortage] eqn:next.
      inversion got; subst.
      destruct (ih _ _ _ _ next) as [ordered counted].
      simpl. rewrite <- app_assoc, ordered, length_app.
      apply Nat.leb_gt in fits. split; [reflexivity|lia].
Qed.

Theorem read_complete : forall segments count value suffix missing,
  count <= length (concat segments) ->
  read count segments = (value, suffix, missing) -> missing = 0.
Proof.
  induction segments as [|segment rest ih]; intros count value suffix missing fits got.
  - simpl in *. inversion got; subst. lia.
  - cbn [read] in got. destruct (count <=? length segment) eqn:local.
    + inversion got; reflexivity.
    + destruct (read (count - length segment) rest)
        as [[part remainder] shortage] eqn:next.
      inversion got; subst. eapply ih; [|exact next].
      simpl in fits. rewrite length_app in fits. lia.
Qed.

Fixpoint drain sizes segments :=
  match sizes with
  | [] => Some ([], segments)
  | count :: rest =>
    let '(value, suffix, missing) := read count segments in
    if missing =? 0 then
      match drain rest suffix with
      | Some (values, remainder) => Some (value :: values, remainder)
      | None => None
      end
    else None
  end.

Theorem drain_accounted : forall sizes segments values remainder,
  drain sizes segments = Some (values, remainder) ->
  concat values ++ concat remainder = concat segments /\
  map (@length A) values = sizes.
Proof.
  induction sizes as [|count rest ih]; intros segments values remainder got.
  - simpl in got. inversion got; subst. simpl. auto.
  - cbn [drain] in got.
    destruct (read count segments) as [[value suffix] missing] eqn:next.
    destruct (missing =? 0) eqn:full; [|discriminate].
    apply Nat.eqb_eq in full. subst missing.
    destruct (drain rest suffix) as [[parts remaining]|] eqn:loaded;
      [|discriminate].
    inversion got; subst.
    destruct (read_accounted _ _ _ _ _ next) as [ordered counted].
    destruct (ih _ _ _ loaded) as [rest_ordered rest_sizes].
    simpl. rewrite <- app_assoc, rest_ordered, ordered, rest_sizes.
    split; [reflexivity|f_equal; lia].
Qed.

Theorem finished_exact : forall sizes segments values remainder,
  drain sizes segments = Some (values, remainder) ->
  concat remainder = [] -> concat values = concat segments.
Proof.
  intros sizes segments values remainder got empty.
  destruct (drain_accounted _ _ _ _ got) as [ordered _].
  rewrite empty, app_nil_r in ordered. exact ordered.
Qed.

End Read.

Example uneven_segments :
  drain [3; 1; 4] [[1; 2]; [3; 4; 5]; [6; 7; 8]] =
  Some ([[1; 2; 3]; [4]; [5; 6; 7; 8]], [[]]).
Proof. reflexivity. Qed.

Example short_stream : drain [3; 6] [[1; 2]; [3; 4; 5]; [6; 7; 8]] = None.
Proof. reflexivity. Qed.

Print Assumptions read_accounted.
Print Assumptions read_complete.
Print Assumptions drain_accounted.
Print Assumptions finished_exact.
End ShardRead.

Module Parse.

Record vector := {
  count : nat;
  capacity : nat;
  peak : nat
}.

Definition push v :=
  if count v <? capacity v then
    {| count := S (count v); capacity := capacity v; peak := peak v |}
  else
    let next := if capacity v =? 0 then 4 else 2 * capacity v in
    {| count := S (count v); capacity := next;
       peak := Nat.max (peak v) (capacity v + next) |}.

Definition valid v :=
  count v <= capacity v /\ capacity v <= 4 * count v /\ peak v <= 8 * count v.

Lemma push_valid : forall v, valid v -> valid (push v).
Proof.
  intros [n c p] [fits [cap seen]]. unfold push, valid; simpl in *.
  destruct (n <? c) eqn:room; simpl.
  - apply Nat.ltb_lt in room. lia.
  - apply Nat.ltb_ge in room. assert (c = n) by lia. subst c.
    destruct (n =? 0) eqn:empty; simpl.
    + apply Nat.eqb_eq in empty. subst n. simpl in *. lia.
    + apply Nat.eqb_neq in empty.
      split; [lia|]. split; [lia|]. apply Nat.max_lub; lia.
Qed.

Fixpoint fill n :=
  match n with
  | 0 => {| count := 0; capacity := 0; peak := 0 |}
  | S rest => push (fill rest)
  end.

Lemma fill_valid : forall n, valid (fill n) /\ count (fill n) = n.
Proof.
  induction n as [|n [v count_n]].
  - simpl. unfold valid; simpl. lia.
  - split.
    + apply push_valid. exact v.
    + unfold fill; fold fill. unfold push.
      destruct (count (fill n) <? capacity (fill n)); simpl;
        try destruct (capacity (fill n) =? 0); simpl; lia.
Qed.

Definition total := fold_right Nat.add 0.

Theorem arrays_accounted : forall lengths,
  total (map (fun n => peak (fill n)) lengths) <= 8 * total lengths.
Proof.
  induction lengths as [|n rest ih]; simpl; [lia|].
  destruct (fill_valid n) as [[_ [_ seen]] count_n].
  rewrite count_n in seen. lia.
Qed.

Definition reserve bytes slots unit := 4 * bytes + 8 * slots * unit + 3 * (6 * bytes + unit) + unit.

Theorem payload_accounted : forall bytes slots unit strings lengths diagnostic,
  strings <= 4 * bytes -> total lengths <= slots ->
  diagnostic <= 6 * bytes + unit ->
  strings + total (map (fun n => peak (fill n)) lengths) * unit + 3 * diagnostic <=
    reserve bytes slots unit.
Proof.
  intros bytes slots unit strings lengths diagnostic text nodes message.
  pose proof (arrays_accounted lengths) as arrays.
  unfold reserve. nia.
Qed.

Example overlap_edge : peak (fill 5) * 24 <= reserve 0 5 24.
Proof. apply Nat.leb_le. reflexivity. Qed.

Print Assumptions push_valid.
Print Assumptions arrays_accounted.
Print Assumptions payload_accounted.
End Parse.

Module PoolTree.

Record pool := Pool { used : nat; cap : nat }.

Fixpoint take size path :=
  match path with
  | [] => Some []
  | value :: rest =>
    if used value + size <=? cap value then
      match take size rest with
      | Some next => Some (Pool (used value + size) (cap value) :: next)
      | None => None
      end
    else None
  end.

Fixpoint release size path :=
  match path with
  | [] => Some []
  | value :: rest =>
    if size <=? used value then
      match release size rest with
      | Some next => Some (Pool (used value - size) (cap value) :: next)
      | None => None
      end
    else None
  end.

Definition valid path := Forall (fun value => used value <= cap value) path.

Theorem take_exact : forall path size next,
  take size path = Some next ->
  Forall2 (fun old new => used new = used old + size /\ cap new = cap old) path next.
Proof.
  induction path as [|value rest ih]; intros size next accepted; simpl in accepted.
  - inversion accepted; constructor.
  - destruct (used value + size <=? cap value); [|discriminate].
    destruct (take size rest) as [more|] eqn:done; [|discriminate].
    inversion accepted; subst. constructor; [simpl; auto|eapply ih; eauto].
Qed.

Theorem take_limits : forall path size next,
  take size path = Some next -> valid next.
Proof.
  induction path as [|value rest ih]; intros size next accepted; simpl in accepted.
  - inversion accepted; constructor.
  - destruct (used value + size <=? cap value) eqn:fits; [|discriminate].
    destruct (take size rest) as [more|] eqn:done; [|discriminate].
    inversion accepted; subst. constructor; [simpl; apply Nat.leb_le; exact fits|].
    eapply ih; eauto.
Qed.

Theorem take_undo : forall path size next,
  take size path = Some next -> release size next = Some path.
Proof.
  induction path as [|value rest ih]; intros size next accepted; simpl in accepted.
  - inversion accepted; reflexivity.
  - destruct (used value + size <=? cap value); [|discriminate].
    destruct (take size rest) as [more|] eqn:done; [|discriminate].
    inversion accepted; subst. simpl.
    assert (enough : (size <=? used value + size) = true) by (apply Nat.leb_le; lia).
    rewrite enough, (ih size more done).
    replace (used value + size - size) with (used value) by lia.
    destruct value; reflexivity.
Qed.

Theorem parent_refusal : forall prefix parent rest size,
  cap parent < used parent + size -> take size (prefix ++ parent :: rest) = None.
Proof.
  induction prefix as [|value prefix ih]; intros parent rest size full; simpl.
  - assert (fails : (used parent + size <=? cap parent) = false)
      by (apply Nat.leb_gt; lia).
    rewrite fails. reflexivity.
  - destruct (used value + size <=? cap value); [rewrite (ih parent rest size full)|]; reflexivity.
Qed.

Theorem child_refusal : forall parent rest size,
  cap parent < used parent + size -> take size (parent :: rest) = None.
Proof.
  intros. apply (parent_refusal [] parent rest size). assumption.
Qed.

Theorem release_exact : forall path size next,
  release size path = Some next ->
  Forall2 (fun old new => used new + size = used old /\ cap new = cap old) path next.
Proof.
  induction path as [|value rest ih]; intros size next accepted; simpl in accepted.
  - inversion accepted; constructor.
  - destruct (size <=? used value) eqn:fits; [|discriminate].
    destruct (release size rest) as [more|] eqn:done; [|discriminate].
    inversion accepted; subst. constructor.
    + apply Nat.leb_le in fits. simpl. split; [lia|reflexivity].
    + eapply ih; eauto.
Qed.

Theorem release_limits : forall path size next,
  valid path -> release size path = Some next -> valid next.
Proof.
  induction path as [|value rest ih]; intros size next valid_path accepted; simpl in accepted.
  - inversion accepted; constructor.
  - inversion valid_path; subst.
    destruct (size <=? used value); [|discriminate].
    destruct (release size rest) as [more|] eqn:done; [|discriminate].
    inversion accepted; subst. constructor; [simpl; lia|eapply ih; eauto].
Qed.

Inductive action := Take (size : nat) | Release (size : nat).

Definition step path event :=
  match match event with Take size => take size path | Release size => release size path end with
  | Some next => next
  | None => path
  end.

Theorem trace_limits : forall events path,
  valid path -> valid (fold_left step events path).
Proof.
  induction events as [|event rest ih]; intros path valid_path; [exact valid_path|].
  simpl. apply ih. unfold step. destruct event as [size|size].
  - destruct (take size path) eqn:done; [eapply take_limits; eauto|exact valid_path].
  - destruct (release size path) eqn:done; [eapply release_limits; eauto|exact valid_path].
Qed.

Example parent_full : take 2 [Pool 0 8; Pool 9 10] = None.
Proof. reflexivity. Qed.

Example exact_path : take 2 [Pool 0 8; Pool 8 10] = Some [Pool 2 8; Pool 10 10].
Proof. reflexivity. Qed.

Print Assumptions take_exact.
Print Assumptions take_undo.
Print Assumptions parent_refusal.
Print Assumptions trace_limits.

Record ledger := Ledger { physical : list pool; quota : pool }.

Definition path (fresh : bool) value :=
  quota value :: if fresh then physical value else [].

Definition merge (fresh : bool) value next :=
  match next with
  | [] => None
  | q :: rest => Some (Ledger (if fresh then rest else physical value) q)
  end.

Definition reserve fresh size value :=
  match take size (path fresh value) with
  | Some next => merge fresh value next
  | None => None
  end.

Definition restore fresh size value :=
  match release size (path fresh value) with
  | Some next => merge fresh value next
  | None => None
  end.

Definition safe value := valid (physical value) /\ used (quota value) <= cap (quota value).

Theorem reserve_safe : forall fresh size value next,
  safe value -> reserve fresh size value = Some next -> safe next.
Proof.
  intros fresh size [p q] next [hp hq] accepted.
  change (match take size (q :: if fresh then p else []) with
    | Some result => merge fresh (Ledger p q) result | None => None end = Some next) in accepted.
  destruct (take size (q :: if fresh then p else [])) as [result|] eqn:done;
    [|discriminate].
  pose proof (take_limits _ _ _ done) as limits.
  destruct result as [|r rest]; [discriminate|].
  unfold merge in accepted; simpl in accepted.
  inversion accepted; subst. inversion limits; subst.
  unfold safe; simpl. destruct fresh; simpl; auto.
Qed.

Theorem borrow_exact : forall size value next,
  reserve false size value = Some next ->
  physical next = physical value /\ used (quota next) = used (quota value) + size.
Proof.
  intros size [p q] next accepted. unfold reserve, path in accepted; simpl in accepted.
  destruct (used q + size <=? cap q) eqn:fits; [|discriminate].
  inversion accepted; subst; simpl; auto.
Qed.

Theorem reserve_undo : forall fresh size value next,
  reserve fresh size value = Some next -> restore fresh size next = Some value.
Proof.
  intros fresh size [p q] next accepted.
  change (match take size (q :: if fresh then p else []) with
    | Some result => merge fresh (Ledger p q) result | None => None end = Some next) in accepted.
  destruct (take size (q :: if fresh then p else [])) as [result|] eqn:done;
    [|discriminate].
  pose proof (take_undo _ _ _ done) as undo.
  destruct result as [|r rest]; [discriminate|].
  unfold merge in accepted; simpl in accepted. inversion accepted; subst.
  destruct fresh; simpl in *.
  - unfold restore, path; simpl. rewrite undo. reflexivity.
  - simpl in done. destruct (used q + size <=? cap q); [|discriminate].
    inversion done; subst.
    unfold restore, path; simpl.
    assert (enough : (size <=? used q + size) = true) by (apply Nat.leb_le; lia).
    rewrite enough. simpl.
    replace (used q + size - size) with (used q) by lia.
    destruct q; reflexivity.
Qed.

Theorem restore_safe : forall fresh size value next,
  safe value -> restore fresh size value = Some next -> safe next.
Proof.
  intros fresh size [p q] next [hp hq] accepted.
  change (match release size (q :: if fresh then p else []) with
    | Some result => merge fresh (Ledger p q) result | None => None end = Some next) in accepted.
  destruct (release size (q :: if fresh then p else [])) as [result|] eqn:done;
    [|discriminate].
  assert (old : valid (q :: if fresh then p else [])).
  { constructor; [assumption|destruct fresh; simpl; auto; constructor]. }
  pose proof (release_limits _ _ _ old done) as limits.
  destruct result as [|r rest]; [discriminate|].
  unfold merge in accepted; simpl in accepted. inversion accepted; subst.
  inversion limits; subst. unfold safe; simpl. destruct fresh; simpl; auto.
Qed.

Definition advance value event :=
  let '(fresh, input) := event in
  match match input with
    | Take size => reserve fresh size value
    | Release size => restore fresh size value
    end with
  | Some next => next
  | None => value
  end.

Theorem job_trace_safe : forall events value,
  safe value -> safe (fold_left advance events value).
Proof.
  induction events as [|[fresh event] rest ih]; intros value valid_value; [exact valid_value|].
  simpl. apply ih. unfold advance. destruct event as [size|size].
  - destruct (reserve fresh size value) eqn:done; [eapply reserve_safe; eauto|exact valid_value].
  - destruct (restore fresh size value) eqn:done; [eapply restore_safe; eauto|exact valid_value].
Qed.

Example cache_not_free : reserve false 4 (Ledger [Pool 4 8] (Pool 0 3)) = None.
Proof. reflexivity. Qed.

Example cache_not_duplicated : reserve false 4 (Ledger [Pool 8 8] (Pool 0 4)) =
  Some (Ledger [Pool 8 8] (Pool 4 4)).
Proof. reflexivity. Qed.

Print Assumptions borrow_exact.
Print Assumptions reserve_undo.
Print Assumptions job_trace_safe.

Lemma reserve_cap : forall fresh size value next,
  reserve fresh size value = Some next -> cap (quota next) = cap (quota value).
Proof.
  intros fresh size [p q] next accepted.
  change (match take size (q :: if fresh then p else []) with
    | Some result => merge fresh (Ledger p q) result | None => None end = Some next) in accepted.
  destruct (take size (q :: if fresh then p else [])) as [result|] eqn:done;
    [|discriminate].
  pose proof (take_exact _ _ _ done) as exact.
  destruct result as [|r rest]; [discriminate|].
  unfold merge in accepted; simpl in accepted. inversion accepted; subst.
  inversion exact; subst. simpl. tauto.
Qed.

Lemma restore_cap : forall fresh size value next,
  restore fresh size value = Some next -> cap (quota next) = cap (quota value).
Proof.
  intros fresh size [p q] next accepted.
  change (match release size (q :: if fresh then p else []) with
    | Some result => merge fresh (Ledger p q) result | None => None end = Some next) in accepted.
  destruct (release size (q :: if fresh then p else [])) as [result|] eqn:done;
    [|discriminate].
  pose proof (release_exact _ _ _ done) as exact.
  destruct result as [|r rest]; [discriminate|].
  unfold merge in accepted; simpl in accepted. inversion accepted; subst.
  inversion exact; subst. simpl. tauto.
Qed.

Theorem trace_cap : forall events value,
  cap (quota (fold_left advance events value)) = cap (quota value).
Proof.
  induction events as [|[fresh event] rest ih]; intros value; [reflexivity|].
  simpl. rewrite ih. unfold advance. destruct event as [size|size].
  - destruct (reserve fresh size value) eqn:done;
      [eapply reserve_cap; eauto|reflexivity].
  - destruct (restore fresh size value) eqn:done;
      [eapply restore_cap; eauto|reflexivity].
Qed.

Definition launch agreed proposed p :=
  match agreed with
  | None => None
  | Some limit =>
    if (0 <? limit) && (limit =? proposed)
    then Some (Ledger p (Pool 0 proposed)) else None
  end.

Theorem signed_trace : forall limit proposed p value events,
  launch (Some limit) proposed p = Some value -> valid p ->
  used (quota (fold_left advance events value)) <= limit /\
  cap (quota (fold_left advance events value)) = limit.
Proof.
  intros limit proposed p value events accepted valid_p.
  unfold launch in accepted.
  destruct ((0 <? limit) && (limit =? proposed)) eqn:same; [|discriminate].
  apply andb_true_iff in same. destruct same as [positive same].
  apply Nat.eqb_eq in same. subst proposed. inversion accepted; subst value.
  assert (safe_start : safe (Ledger p (Pool 0 limit))).
  { unfold safe; simpl; auto with arith. }
  pose proof (job_trace_safe events _ safe_start) as limits.
  pose proof (trace_cap events (Ledger p (Pool 0 limit))) as fixed.
  unfold safe in limits. simpl in fixed. split; [lia|exact fixed].
Qed.

Example unsigned_refused : launch None 4 [Pool 0 8] = None.
Proof. reflexivity. Qed.

Example enlarged_refused : launch (Some 3) 4 [Pool 0 8] = None.
Proof. reflexivity. Qed.

Print Assumptions signed_trace.
End PoolTree.

Module AssetRead.

Definition encoded size := 4 * ((size + 2) / 3).
Definition allocated size := 3 * ((size + 3) / 4).
Definition accepts limit declared raw :=
  (declared <=? limit) && (raw <=? encoded declared).

Lemma encoded_mono : forall first second,
  first <= second -> encoded first <= encoded second.
Proof.
  intros first second ordered. unfold encoded.
  pose proof (Nat.div_le_mono (first + 2) (second + 2) 3 ltac:(lia) ltac:(lia)).
  lia.
Qed.

Theorem decode_space : forall declared raw,
  raw <= encoded declared -> allocated raw <= declared + 2.
Proof.
  intros declared raw fits. unfold encoded in fits. unfold allocated.
  pose proof (Nat.div_mod (declared + 2) 3 ltac:(lia)) as source.
  pose proof (Nat.div_mod (raw + 3) 4 ltac:(lia)) as target.
  assert ((raw + 3) / 4 <= (declared + 2) / 3) by nia.
  nia.
Qed.

Theorem read_peak : forall limit declared raw,
  accepts limit declared raw = true ->
  allocated raw <= limit + 2 /\
  raw + 2 * allocated raw <= encoded limit + 2 * (limit + 2).
Proof.
  intros limit declared raw accepted. unfold accepts in accepted.
  apply andb_true_iff in accepted. destruct accepted as [declared_ok raw_ok].
  apply Nat.leb_le in declared_ok. apply Nat.leb_le in raw_ok.
  pose proof (decode_space declared raw raw_ok).
  pose proof (encoded_mono declared limit declared_ok).
  split; lia.
Qed.

Theorem oversized_refused : forall limit declared raw,
  limit < declared \/ encoded declared < raw ->
  accepts limit declared raw = false.
Proof.
  intros limit declared raw oversized.
  destruct (accepts limit declared raw) eqn:accepted; [|reflexivity].
  unfold accepts in accepted. apply andb_true_iff in accepted.
  destruct accepted as [declared_ok raw_ok].
  apply Nat.leb_le in declared_ok. apply Nat.leb_le in raw_ok. lia.
Qed.

Example exact_read : accepts 5 5 8 = true /\ allocated 8 = 6.
Proof. split; reflexivity. Qed.

Example large_read_refused : accepts 1 1 4096 = false.
Proof. reflexivity. Qed.

Print Assumptions decode_space.
Print Assumptions read_peak.
Print Assumptions oversized_refused.
End AssetRead.