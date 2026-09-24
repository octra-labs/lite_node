(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Octra_consensus.Resource_attestations

let check name value =
  if not value then failwith ("resource path = " ^ name)

let claim root =
  R.unsigned_attestation ~chain_id:"resource-path" ~epoch_id:1L ~node_id:"node"
    ~kind:R.PoStorage ~commitment:root ~proof_hash:(String.make 32 '\000')
    ~weight:1L

let verify challenge att proof =
  let att = { att with R.proof_hash = R.storage_evidence_hash proof } in
  R.verify_storage_attestation ~challenge ~commitment:att.commitment
    ~max_weight:1L ~leaf_count:2L att proof

let check_leaf_swap () =
  let left = "left" and right = "right" in
  let root = R.storage_parent_hash (R.storage_leaf_hash left) (R.storage_leaf_hash right) in
  let att = claim root in
  for seed = 0 to 255 do
    let challenge = String.make 32 (Char.chr seed) in
    let index = Option.get (R.storage_challenge_index ~challenge ~leaf_count:2L att) in
    let proof =
      if index = 0L then
        R.{ leaf_index = index; leaf_count = 2L; chunk = left;
            path = [{ side = Right; sibling_hash = R.storage_leaf_hash right }] }
      else
        R.{ leaf_index = index; leaf_count = 2L; chunk = right;
            path = [{ side = Left; sibling_hash = R.storage_leaf_hash left }] }
    in
    let other =
      if index = 0L then
        R.{ proof with chunk = right;
            path = [{ side = Left; sibling_hash = R.storage_leaf_hash left }] }
      else
        R.{ proof with chunk = left;
            path = [{ side = Right; sibling_hash = R.storage_leaf_hash right }] }
    in
    check "valid leaf" (verify challenge att proof);
    check "same root" (R.merkle_root_from_evidence other = root);
    check "other leaf refused" (not (verify challenge att other))
  done

let check_shape () =
  let hash = String.make 32 '\001' in
  let leaf = R.{ leaf_index = 0L; leaf_count = 1L; chunk = "one"; path = [] } in
  let step = R.{ side = Right; sibling_hash = hash } in
  check "single leaf" (R.storage_path leaf);
  check "extra path" (not (R.storage_path { leaf with path = [step] }));
  check "missing path" (not (R.storage_path { leaf with leaf_count = 2L }));
  check "negative index" (not (R.storage_path { leaf with leaf_index = -1L }));
  check "outside index" (not (R.storage_path { leaf with leaf_index = 1L }));
  check "negative count" (not (R.storage_path { leaf with leaf_count = -1L }));
  check "invalid sibling"
    (not (R.storage_path { leaf with leaf_count = 2L;
      path = [{ step with sibling_hash = "short" }] }));
  let rec path index count =
    if count = 1L then []
    else
      let side = if Int64.rem index 2L = 1L then R.Left else R.Right in
      R.{ side; sibling_hash = hash }
      :: path (Int64.div index 2L)
           (Int64.add (Int64.div count 2L) (Int64.rem count 2L))
  in
  for count = 1 to 64 do
    for index = 0 to count - 1 do
      let proof = R.{ leaf with leaf_index = Int64.of_int index;
        leaf_count = Int64.of_int count;
        path = path (Int64.of_int index) (Int64.of_int count) } in
      check "tree position" (R.storage_path proof);
      for other = 0 to count - 1 do
        if other <> index then
          check "position unique"
            (not (R.storage_path { proof with leaf_index = Int64.of_int other }))
      done
    done
  done;
  let top = Int64.max_int in
  check "largest count" (R.storage_path { leaf with leaf_index = Int64.pred top;
    leaf_count = top; path = path (Int64.pred top) top });
  check "bad challenge"
    (R.storage_challenge_index ~challenge:"short" ~leaf_count:1L (claim hash) = None)

let () =
  check_leaf_swap ();
  check_shape ();
  Printf.printf "status = pass test = resource_path\n"