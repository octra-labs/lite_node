(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool.
Import ListNotations.

Record slot := Slot { readers : list nat; replies : list nat }.
Definition pool := nat -> option slot.

Definition read reader value :=
  match value with
  | Some value =>
    if in_dec Nat.eq_dec reader (readers value) then replies value else []
  | None => []
  end.

Definition join reader value :=
  match value with
  | Some value => Some (Slot (reader :: readers value) (replies value))
  | None => Some (Slot [reader] [])
  end.

Definition leave reader value :=
  match value with
  | None => None
  | Some value =>
    match remove Nat.eq_dec reader (readers value) with
    | [] => None
    | retained => Some (Slot retained (replies value))
    end
  end.

Definition release epoch reader (queries : pool) : pool :=
  fun target => if Nat.eq_dec target epoch
    then leave reader (queries target) else queries target.

Theorem join_preserves : forall kept added value,
  In kept (readers value) ->
  read kept (join added (Some value)) = replies value.
Proof.
  intros kept added [members values] present.
  change (In kept members) in present.
  change ((if in_dec Nat.eq_dec kept (added :: members) then values else []) = values).
  destruct (in_dec Nat.eq_dec kept (added :: members)) as [inside|outside]; auto.
  exfalso. apply outside. right. exact present.
Qed.

Theorem join_shares : forall added value,
  read added (join added (Some value)) = replies value.
Proof.
  intros added [members values].
  change ((if in_dec Nat.eq_dec added (added :: members) then values else []) = values).
  destruct (in_dec Nat.eq_dec added (added :: members)) as [inside|outside]; auto.
  exfalso. apply outside. left. reflexivity.
Qed.

Theorem leave_preserves : forall kept removed value,
  kept <> removed -> In kept (readers value) ->
  read kept (leave removed (Some value)) = replies value.
Proof.
  intros kept removed [members values] distinct present.
  change (In kept members) in present.
  pose proof (in_in_remove Nat.eq_dec members distinct present) as retained.
  change (read kept (match remove Nat.eq_dec removed members with
    | [] => None | kept => Some (Slot kept values) end) = values).
  destruct (remove Nat.eq_dec removed members) as [|first rest] eqn:remaining.
  - contradiction.
  - change ((if in_dec Nat.eq_dec kept (first :: rest) then values else []) = values).
    destruct (in_dec Nat.eq_dec kept (first :: rest)); auto. contradiction.
Qed.

Theorem leave_closes : forall removed value,
  read removed (leave removed value) = [].
Proof.
  intros removed [[members values]|]; [|reflexivity].
  pose proof (remove_In Nat.eq_dec members removed) as absent.
  change (read removed (match remove Nat.eq_dec removed members with
    | [] => None | kept => Some (Slot kept values) end) = []).
  destruct (remove Nat.eq_dec removed members) as [|first rest] eqn:remaining; auto.
  change ((if in_dec Nat.eq_dec removed (first :: rest) then values else []) = []).
  destruct (in_dec Nat.eq_dec removed (first :: rest)); auto. contradiction.
Qed.

Theorem last_removes : forall reader values,
  leave reader (Some (Slot [reader] values)) = None.
Proof.
  intros. simpl. destruct (Nat.eq_dec reader reader); congruence.
Qed.

Theorem epoch_separate : forall queries epoch target reader,
  target <> epoch -> release epoch reader queries target = queries target.
Proof.
  intros. unfold release. destruct (Nat.eq_dec target epoch); congruence.
Qed.

Theorem old_release_preserves : forall old fresh value,
  old < fresh ->
  read fresh (leave old (join fresh (Some value))) = replies value.
Proof.
  intros old fresh [members values] later.
  apply (leave_preserves fresh old (Slot (fresh :: members) values)).
  - intro same. subst fresh. exact (Nat.lt_irrefl old later).
  - simpl. auto.
Qed.

Theorem allocation_distinct : forall previous next,
  previous < next -> previous <> S next.
Proof.
  intros previous next before same. subst previous.
  exact (Nat.lt_irrefl next (Nat.lt_trans next (S next) next (Nat.lt_succ_diag_r next) before)).
Qed.

Print Assumptions join_preserves.
Print Assumptions join_shares.
Print Assumptions leave_preserves.
Print Assumptions leave_closes.
Print Assumptions last_removes.
Print Assumptions epoch_separate.
Print Assumptions old_release_preserves.
Print Assumptions allocation_distinct.

Module View_root.

Definition root := nat -> nat.

Inductive query :=
| Done : nat -> query
| Fetch : nat -> (nat -> query) -> query
| Move : root -> query -> query
| Nested : query -> (option nat -> query) -> query
| Guard : nat -> nat -> query -> query.

Record reply := Reply {
  value : option nat;
  reads : list (nat * nat);
  live : root
}.

Fixpoint run (fuel : nat) (fixed : bool) (saved current : root) (q : query) : reply :=
  match fuel with
  | 0 => Reply None [] current
  | S rest =>
      match q with
      | Done v => Reply (Some v) [] current
      | Fetch key next =>
          let v := (if fixed then saved else current) key in
          let result := run rest fixed saved current (next v) in
          Reply (value result) ((key, v) :: reads result) (live result)
      | Move head next => run rest fixed saved head next
      | Nested child next =>
          let first := run rest fixed saved current child in
          let second := run rest fixed saved (live first) (next (value first)) in
          Reply (value second) (reads first ++ reads second) (live second)
      | Guard key sender next =>
          let owner := (if fixed then saved else current) key in
          let result := if Nat.eqb sender owner then run rest fixed saved current next
            else Reply None [] current in
          Reply (value result) ((key, owner) :: reads result) (live result)
      end
  end.

Definition observed result := (value result, reads result).

Theorem read_origin : forall fuel saved current q,
  Forall (fun pair => snd pair = saved (fst pair))
    (reads (run fuel true saved current q)).
Proof.
  induction fuel as [|fuel ih]; intros saved current q; [constructor|].
  destruct q as [v|key next|head next|child next|key sender next]; cbn [run].
  - constructor.
  - constructor; [reflexivity|apply ih].
  - apply ih.
  - apply Forall_app. split; apply ih.
  - constructor; [reflexivity|].
    destruct (Nat.eqb sender (saved key)); cbn [reads]; [apply ih|constructor].
Qed.

Theorem live_independent : forall fuel saved a b q,
  observed (run fuel true saved a q) = observed (run fuel true saved b q).
Proof.
  induction fuel as [|fuel ih]; intros saved a b q; [reflexivity|].
  destruct q as [v|key next|head next|child next|key sender next]; cbn [run].
  - reflexivity.
  - pose proof (ih saved a b (next (saved key))) as same.
    destruct (run fuel true saved a (next (saved key))) as [va ra ha].
    destruct (run fuel true saved b (next (saved key))) as [vb rb hb].
    cbn [observed] in same |- *. inversion same. reflexivity.
  - reflexivity.
  - pose proof (ih saved a b child) as first.
    destruct (run fuel true saved a child) as [va ra ha].
    destruct (run fuel true saved b child) as [vb rb hb].
    cbn [observed] in first. inversion first. subst vb rb.
    cbn [live value reads].
    pose proof (ih saved ha hb (next va)) as second.
    destruct (run fuel true saved ha (next va)) as [vc rc hc].
    destruct (run fuel true saved hb (next va)) as [vd rd hd].
    cbn [observed] in second |- *. inversion second. reflexivity.
  - destruct (Nat.eqb sender (saved key)); [|reflexivity].
    pose proof (ih saved a b next) as same.
    destruct (run fuel true saved a next) as [va ra ha].
    destruct (run fuel true saved b next) as [vb rb hb].
    cbn [observed] in same |- *. inversion same. reflexivity.
Qed.

Theorem nested_origin : forall fuel saved current child next,
  Forall (fun pair => snd pair = saved (fst pair))
    (reads (run (S fuel) true saved current (Nested child next))).
Proof. intros. apply read_origin. Qed.

Theorem owner_refusal : forall fuel saved current key sender next,
  sender <> saved key ->
  run (S fuel) true saved current (Guard key sender next) =
    Reply None [(key, saved key)] current.
Proof.
  intros fuel saved current key sender next different.
  cbn [run]. apply Nat.eqb_neq in different. rewrite different. reflexivity.
Qed.

Theorem owner_reads : forall fuel saved current key sender next,
  sender = saved key ->
  reads (run (S fuel) true saved current (Guard key sender next)) =
    (key, saved key) :: reads (run fuel true saved current next).
Proof.
  intros fuel saved current key sender next same.
  cbn [run]. subst sender. rewrite Nat.eqb_refl. reflexivity.
Qed.

Definition overlap :=
  Fetch 0 (fun first => Move (fun _ => 2)
    (Fetch 0 (fun second => Done (first + second)))).

Example fixed_read :
  observed (run 8 true (fun _ => 1) (fun _ => 7) overlap) =
    (Some 2, [(0, 1); (0, 1)]).
Proof. reflexivity. Qed.

Definition owned := Guard 0 2 (Fetch 1 (fun data => Done data)).

Example pending_owner_refused :
  observed (run 8 true (fun _ => 1) (fun _ => 2) owned) =
    (None, [(0, 1)]).
Proof. reflexivity. Qed.

Example live_owner_reads :
  observed (run 8 false (fun _ => 1) (fun _ => 2) owned) =
    (Some 2, [(0, 2); (1, 2)]).
Proof. reflexivity. Qed.

Example live_read_mixes :
  observed (run 8 false (fun _ => 1) (fun _ => 7) overlap) =
    (Some 9, [(0, 7); (0, 2)]).
Proof. reflexivity. Qed.

Print Assumptions read_origin.
Print Assumptions live_independent.
Print Assumptions nested_origin.
Print Assumptions owner_refusal.
Print Assumptions owner_reads.

Definition linked_query owner sender key fresh :=
  Guard owner sender (Fetch key (fun link =>
    Move fresh (Fetch link (fun data => Done data)))).

Theorem linked_reply : forall fuel saved current fresh owner sender key,
  sender = saved owner ->
  observed (run (S (S (S (S (S fuel))))) true saved current
    (linked_query owner sender key fresh)) =
  (Some (saved (saved key)),
    [(owner, saved owner); (key, saved key); (saved key, saved (saved key))]).
Proof.
  intros fuel saved current fresh owner sender key same.
  subst sender. cbn [linked_query run]. rewrite Nat.eqb_refl. reflexivity.
Qed.

Definition object_tree key :=
  match key with
  | 0 => 2
  | 1 => 7
  | _ => 50
  end.

Example linked_saved_reply :
  observed (run 5 true object_tree (fun _ => 80)
    (linked_query 0 2 1 (fun _ => 900))) =
  (Some 50, [(0, 2); (1, 7); (7, 50)]).
Proof. reflexivity. Qed.

Example linked_live_reply :
  observed (run 5 false object_tree object_tree
    (linked_query 0 2 1 (fun _ => 900))) =
  (Some 900, [(0, 2); (1, 7); (7, 900)]).
Proof. reflexivity. Qed.

Print Assumptions linked_reply.

Definition timed_query owner sender clock start stop fresh :=
  Guard owner sender (Fetch clock (fun epoch =>
    Move fresh (Fetch start (fun first => Fetch stop (fun last =>
      Done (if Nat.leb first (S epoch) && Nat.ltb (S epoch) last then 1 else 0)))))).

Theorem timed_reply : forall fuel saved current fresh owner sender clock start stop,
  sender = saved owner ->
  observed (run (S (S (S (S (S (S fuel)))))) true saved current
    (timed_query owner sender clock start stop fresh)) =
  (Some (if Nat.leb (saved start) (S (saved clock)) &&
    Nat.ltb (S (saved clock)) (saved stop) then 1 else 0),
    [(owner, saved owner); (clock, saved clock); (start, saved start); (stop, saved stop)]).
Proof.
  intros fuel saved current fresh owner sender clock start stop same.
  subst sender. cbn [timed_query run]. rewrite Nat.eqb_refl. reflexivity.
Qed.

Theorem timed_live : forall fuel saved current fresh owner sender clock start stop,
  sender = saved owner ->
  value (run (S (S (S (S (S (S fuel)))))) true saved current
    (timed_query owner sender clock start stop fresh)) = Some 1 <->
  saved start <= S (saved clock) /\ S (saved clock) < saved stop.
Proof.
  intros fuel saved current fresh owner sender clock start stop same.
  subst sender. cbn [timed_query run]. rewrite Nat.eqb_refl. cbn [value].
  destruct (Nat.leb (saved start) (S (saved clock)) &&
    Nat.ltb (S (saved clock)) (saved stop)) eqn:inside.
  - apply andb_true_iff in inside as [first last].
    apply Nat.leb_le in first. apply Nat.ltb_lt in last. split; auto.
  - split; [discriminate|]. intros [first last].
    apply Nat.leb_le in first. apply Nat.ltb_lt in last.
    rewrite first, last in inside. discriminate.
Qed.

Definition policy_tree key :=
  match key with
  | 0 => 2
  | 1 => 51
  | 2 => 52
  | _ => 70
  end.

Example timed_saved_live :
  observed (run 6 true policy_tree (fun _ => 1000)
    (timed_query 0 2 1 2 3 (fun _ => 0))) =
  (Some 1, [(0, 2); (1, 51); (2, 52); (3, 70)]).
Proof. reflexivity. Qed.

Example timed_live_mixes :
  observed (run 6 false policy_tree policy_tree
    (timed_query 0 2 1 2 3 (fun _ => 0))) =
  (Some 0, [(0, 2); (1, 51); (2, 0); (3, 0)]).
Proof. reflexivity. Qed.

Print Assumptions timed_reply.
Print Assumptions timed_live.

Definition cell_query owner sender clock index fresh :=
  Guard owner sender (Fetch clock (fun epoch => Move fresh
    (Fetch index (fun link => Fetch (S link) (fun first =>
      Fetch (S (S link)) (fun last =>
        if Nat.leb first (S epoch) && Nat.ltb (S epoch) last then
          Fetch link (fun data => Done data)
        else Done 0)))))).

Definition cell_open (saved : root) clock index :=
  Nat.leb (saved (S (saved index))) (S (saved clock)) &&
  Nat.ltb (S (saved clock)) (saved (S (S (saved index)))).

Theorem cell_reply : forall fuel saved current fresh owner sender clock index,
  sender = saved owner ->
  observed (run (8 + fuel) true saved current
    (cell_query owner sender clock index fresh)) =
  (Some (if cell_open saved clock index then saved (saved index) else 0),
    [(owner, saved owner); (clock, saved clock); (index, saved index);
      (S (saved index), saved (S (saved index)));
      (S (S (saved index)), saved (S (S (saved index))))] ++
      if cell_open saved clock index then [(saved index, saved (saved index))] else []).
Proof.
  intros fuel saved current fresh owner sender clock index same.
  subst sender. cbn [Nat.add cell_query run]. rewrite Nat.eqb_refl.
  unfold cell_open.
  destruct (Nat.leb (saved (S (saved index))) (S (saved clock)) &&
    Nat.ltb (S (saved clock)) (saved (S (S (saved index))))); reflexivity.
Qed.

Theorem cell_closed : forall fuel saved current fresh owner sender clock index,
  sender = saved owner ->
  (S (saved clock) < saved (S (saved index)) \/
    saved (S (S (saved index))) <= S (saved clock)) ->
  observed (run (8 + fuel) true saved current
    (cell_query owner sender clock index fresh)) =
  (Some 0, [(owner, saved owner); (clock, saved clock); (index, saved index);
    (S (saved index), saved (S (saved index)));
    (S (S (saved index)), saved (S (S (saved index))))]).
Proof.
  intros fuel saved current fresh owner sender clock index same outside.
  rewrite (cell_reply fuel saved current fresh owner sender clock index same).
  assert (cell_open saved clock index = false) as closed.
  { unfold cell_open. destruct outside as [early|late].
    - apply Nat.leb_gt in early. rewrite early. reflexivity.
    - apply Nat.ltb_ge in late. rewrite late. apply andb_false_r. }
  rewrite closed. cbn. reflexivity.
Qed.

Definition cell_tree key :=
  match key with
  | 0 => 2
  | 1 => 51
  | 2 => 7
  | 7 => 999
  | 8 => 50
  | _ => 52
  end.

Example expired_cell_not_read :
  observed (run 8 true cell_tree cell_tree
    (cell_query 0 2 1 2 (fun _ => 1000))) =
  (Some 0, [(0, 2); (1, 51); (2, 7); (8, 50); (9, 52)]).
Proof. reflexivity. Qed.

Example live_cell_policy_mixes :
  observed (run 8 false cell_tree cell_tree
    (cell_query 0 2 1 2 (fun key => if Nat.eqb key 1001 then 0 else 1000))) =
  (Some 1000, [(0, 2); (1, 51); (2, 1000); (1001, 0); (1002, 1000); (1000, 1000)]).
Proof. reflexivity. Qed.

Print Assumptions cell_reply.
Print Assumptions cell_closed.

Definition binding_query owner sender index version fresh :=
  Guard owner sender (Fetch index (fun link => Move fresh
    (Fetch version (fun revision => if Nat.eqb link 0 then Done revision else
      Fetch link (fun data => Done (revision + data)))))).

Theorem binding_reply : forall fuel saved current fresh owner sender index version,
  sender = saved owner ->
  observed (run (6 + fuel) true saved current
    (binding_query owner sender index version fresh)) =
  (Some (if Nat.eqb (saved index) 0 then saved version else
      saved version + saved (saved index)),
    [(owner, saved owner); (index, saved index); (version, saved version)] ++
      if Nat.eqb (saved index) 0 then [] else [(saved index, saved (saved index))]).
Proof.
  intros fuel saved current fresh owner sender index version same.
  subst sender. cbn [Nat.add binding_query run]. rewrite Nat.eqb_refl.
  destruct (Nat.eqb (saved index) 0); reflexivity.
Qed.

Theorem binding_absent : forall fuel saved current fresh owner sender index version,
  sender = saved owner -> saved index = 0 ->
  observed (run (6 + fuel) true saved current
    (binding_query owner sender index version fresh)) =
  (Some (saved version), [(owner, saved owner); (index, 0); (version, saved version)]).
Proof.
  intros fuel saved current fresh owner sender index version same missing.
  rewrite (binding_reply fuel saved current fresh owner sender index version same).
  rewrite missing. reflexivity.
Qed.

Definition binding_tree key :=
  match key with
  | 0 => 2
  | 1 => 7
  | 2 => 50
  | _ => 100
  end.

Example saved_binding_revision :
  observed (run 6 true binding_tree binding_tree
    (binding_query 0 2 1 2 (fun _ => 999))) =
  (Some 150, [(0, 2); (1, 7); (2, 50); (7, 100)]).
Proof. reflexivity. Qed.

Example live_binding_revision_mixes :
  observed (run 6 false binding_tree binding_tree
    (binding_query 0 2 1 2 (fun _ => 999))) =
  (Some 1998, [(0, 2); (1, 7); (2, 999); (7, 999)]).
Proof. reflexivity. Qed.

Print Assumptions binding_reply.
Print Assumptions binding_absent.

Definition lease_live start stop closed epoch :=
  Nat.leb start epoch && Nat.ltb epoch stop && Nat.eqb closed 0.

Definition epoch_option encoded :=
  match encoded with
  | 0 => None
  | S epoch => Some epoch
  end.

Definition key_live (start stop : option nat) revoked erased epoch :=
  (match start with None => true | Some active => Nat.leb active epoch end) &&
    (match stop with None => true | Some expiry => Nat.ltb epoch expiry end) &&
    Nat.eqb revoked 0 && Nat.eqb erased 0.

Theorem lease_exact : forall start stop closed epoch,
  lease_live start stop closed epoch = true <->
  start <= epoch /\ epoch < stop /\ closed = 0.
Proof.
  intros. unfold lease_live.
  rewrite !andb_true_iff, Nat.leb_le, Nat.ltb_lt, Nat.eqb_eq. tauto.
Qed.

Theorem key_exact : forall start stop revoked erased epoch,
  key_live start stop revoked erased epoch = true <->
  (match start with None => True | Some active => active <= epoch end) /\
    (match stop with None => True | Some expiry => epoch < expiry end) /\
    revoked = 0 /\ erased = 0.
Proof.
  intros start stop revoked erased epoch.
  destruct start as [start|]; destruct stop as [stop|]; unfold key_live;
    rewrite !andb_true_iff;
    repeat rewrite Nat.leb_le;
    repeat rewrite Nat.ltb_lt;
    repeat rewrite Nat.eqb_eq;
    intuition.
Qed.

Definition transport_query owner sender clock start stop closed key_start key_stop revoked erased fresh :=
  Guard owner sender (Fetch clock (fun prior => Move fresh
    (Fetch start (fun begin => Fetch stop (fun finish => Fetch closed (fun cancelled =>
      Fetch key_start (fun active => Fetch key_stop (fun expiry => Fetch revoked (fun removed =>
        Fetch erased (fun destroyed => Done (if lease_live begin finish cancelled (S prior) &&
          key_live (epoch_option active) (epoch_option expiry) removed destroyed (S prior)
            then 1 else 0))))))))))).

Theorem transport_reply : forall fuel saved current fresh owner sender clock start stop closed key_start key_stop revoked erased,
  sender = saved owner ->
  run (11 + fuel) true saved current
    (transport_query owner sender clock start stop closed key_start key_stop revoked erased fresh) =
  Reply (Some (if lease_live (saved start) (saved stop) (saved closed) (S (saved clock)) &&
      key_live (epoch_option (saved key_start)) (epoch_option (saved key_stop))
        (saved revoked) (saved erased) (S (saved clock)) then 1 else 0))
    [(owner, saved owner); (clock, saved clock); (start, saved start); (stop, saved stop);
      (closed, saved closed); (key_start, saved key_start); (key_stop, saved key_stop);
      (revoked, saved revoked); (erased, saved erased)] fresh.
Proof.
  intros fuel saved current fresh owner sender clock start stop closed key_start key_stop revoked erased same.
  subst sender. cbn [Nat.add transport_query run]. rewrite Nat.eqb_refl. reflexivity.
Qed.

Lemma selected_one : forall (flag : bool),
  Some (if flag then 1 else 0) = Some 1 <-> flag = true.
Proof. destruct flag; split; intros; try reflexivity; discriminate. Qed.

Theorem transport_eligible : forall fuel saved current fresh owner sender clock start stop closed key_start key_stop revoked erased,
  sender = saved owner ->
  value (run (11 + fuel) true saved current
    (transport_query owner sender clock start stop closed key_start key_stop revoked erased fresh)) = Some 1 <->
  saved start <= S (saved clock) /\ S (saved clock) < saved stop /\ saved closed = 0 /\
    (match epoch_option (saved key_start) with None => True
      | Some active => active <= S (saved clock) end) /\
    (match epoch_option (saved key_stop) with None => True
      | Some expiry => S (saved clock) < expiry end) /\ saved revoked = 0 /\ saved erased = 0.
Proof.
  intros fuel saved current fresh owner sender clock start stop closed key_start key_stop revoked erased same.
  rewrite (transport_reply fuel saved current fresh owner sender clock start stop closed key_start key_stop revoked erased same).
  cbn [value]. rewrite selected_one, andb_true_iff, lease_exact, key_exact. tauto.
Qed.

Theorem transport_denied : forall fuel saved current fresh owner sender clock start stop closed key_start key_stop revoked erased,
  sender <> saved owner ->
  reads (run (S fuel) true saved current
    (transport_query owner sender clock start stop closed key_start key_stop revoked erased fresh)) =
    [(owner, saved owner)].
Proof. intros. unfold transport_query. rewrite owner_refusal by assumption. reflexivity. Qed.

Definition transport_tree key :=
  match key with
  | 0 => 2
  | 1 => 50
  | 2 => 51
  | 3 => 70
  | 4 => 0
  | 5 => 52
  | 6 => 111
  | _ => 0
  end.

Example saved_transport_remains_live :
  observed (run 11 true transport_tree transport_tree
    (transport_query 0 2 1 2 3 4 5 6 7 8 (fun _ => 999))) =
  (Some 1, [(0, 2); (1, 50); (2, 51); (3, 70); (4, 0); (5, 52); (6, 111); (7, 0); (8, 0)]).
Proof. reflexivity. Qed.

Example live_transport_mixes_commit :
  value (run 11 false transport_tree transport_tree
    (transport_query 0 2 1 2 3 4 5 6 7 8 (fun _ => 999))) = Some 0.
Proof. reflexivity. Qed.

Example key_absent_expiry_live : key_live None None 0 0 51 = true.
Proof. reflexivity. Qed.

Example key_zero_expiry_closed : key_live None (Some 0) 0 0 51 = false.
Proof. reflexivity. Qed.

Example key_erased_closed : key_live None None 0 1 51 = false.
Proof. reflexivity. Qed.

Print Assumptions lease_exact.
Print Assumptions key_exact.
Print Assumptions transport_reply.
Print Assumptions transport_eligible.
Print Assumptions transport_denied.

Record epoch_tree := Epoch_tree {
  height : nat;
  tree_id : nat;
  index_id : nat;
  tree : root
}.

Definition checked (commit : nat -> nat -> nat) epoch declared point fuel current q :=
  if Nat.eqb (height point) epoch &&
     Nat.eqb (commit (tree_id point) (index_id point)) declared then
    Some (run fuel true (tree point) current q)
  else None.

Example replaced_tree_refused :
  checked Nat.add 7 3 (Epoch_tree 7 2 2 (fun _ => 9)) 8 (fun _ => 1) overlap = None.
Proof. reflexivity. Qed.

Theorem checked_origin : forall commit epoch declared point fuel current q result,
  checked commit epoch declared point fuel current q = Some result ->
  height point = epoch /\
  commit (tree_id point) (index_id point) = declared /\
  Forall (fun pair => snd pair = tree point (fst pair)) (reads result).
Proof.
  intros commit epoch declared point fuel current q result accepted.
  unfold checked in accepted.
  destruct (Nat.eqb (height point) epoch &&
    Nat.eqb (commit (tree_id point) (index_id point)) declared) eqn:matches;
    [|discriminate].
  apply andb_true_iff in matches as [epoch_match root_match].
  apply Nat.eqb_eq in epoch_match. apply Nat.eqb_eq in root_match.
  inversion accepted. subst result. repeat split; try assumption.
  apply read_origin.
Qed.

Theorem checked_refusal : forall commit epoch declared point fuel current q,
  height point <> epoch \/ commit (tree_id point) (index_id point) <> declared ->
  checked commit epoch declared point fuel current q = None.
Proof.
  intros commit epoch declared point fuel current q different.
  unfold checked.
  destruct different as [epoch_diff|root_diff].
  - apply Nat.eqb_neq in epoch_diff. rewrite epoch_diff. reflexivity.
  - apply Nat.eqb_neq in root_diff. rewrite root_diff.
    rewrite andb_false_r. reflexivity.
Qed.

Definition observation (value : option reply) :=
  match value with
  | Some result => Some (observed result)
  | None => None
  end.

Theorem checked_live : forall commit epoch declared point fuel a b q,
  observation (checked commit epoch declared point fuel a q) =
  observation (checked commit epoch declared point fuel b q).
Proof.
  intros. unfold checked.
  destruct (Nat.eqb (height point) epoch &&
    Nat.eqb (commit (tree_id point) (index_id point)) declared); [|reflexivity].
  cbn [observation]. f_equal. apply live_independent.
Qed.

Print Assumptions checked_origin.
Print Assumptions checked_refusal.
Print Assumptions checked_live.

End View_root.