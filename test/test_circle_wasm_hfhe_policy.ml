(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Policy = Octra_core.Circle_wasm_hfhe_policy
module Backend = Octra_core.Circle_wasm_hfhe_backend
module Worker = Octra_core.Pvac_verify_worker
module Pool = Octra_core.Compute_pool

let check name condition =
  if not condition then failwith name

let shape
    ?(slots = 1)
    ?(layers = 2)
    ?(edges = 128)
    ?(c0 = 1)
    ?(base_layers = 2)
    () =
  Pvac_ffi.{ slots; layers; edges; c0; base_layers }

let test_busy_class () =
  let pubkey, _ = Pvac_ffi.keygen (Pvac_ffi.default_params ()) in
  let payload =
    `Assoc [
      "action", `String "verify_zero";
      "pubkey_b64",
      `String
        (Base64.encode_exn
           (Bytes.to_string (Pvac_ffi.serialize_pubkey pubkey)));
      "ciphertext", `String "ciphertext";
      "proof", `String "proof";
      "cap", `Bool true;
      "strict", `Bool false;
    ]
    |> Yojson.Safe.to_string
  in
  Mutex.lock Backend.verifier_lane;
  let response =
    Fun.protect
      ~finally:(fun () -> Mutex.unlock Backend.verifier_lane)
      (fun () -> Backend.call_json payload |> Yojson.Safe.from_string)
  in
  check
    "busy response class"
    (Yojson.Safe.Util.member "class" response = `String "unavailable")

let with_full_worker_pool f =
  let active = Atomic.make 0 in
  let release = Atomic.make false in
  let hold () =
    ignore (Atomic.fetch_and_add active 1);
    while not (Atomic.get release) do
      Unix.sleepf 0.001
    done
  in
  let workers =
    List.init Worker.capacity (fun _ ->
      Thread.create
        (fun () ->
          ignore
            (Pool.run_sync
               Worker.proof_channel
               Pool.Required
               hold))
        ())
  in
  let rec await attempts =
    if Atomic.get active = Worker.capacity then ()
    else if attempts = 0 then failwith "proof pool did not fill"
    else begin
      Unix.sleepf 0.001;
      await (attempts - 1)
    end
  in
  Fun.protect
    ~finally:(fun () ->
      Atomic.set release true;
      List.iter Thread.join workers)
    (fun () ->
      await 5_000;
      f ())

let test_worker_queue_is_not_held () =
  let busy = function
    | Error Worker.Worker_busy -> true
    | _ -> false
  in
  let calls = [|
    (fun () ->
      Backend.verify_zero_worker ~math:false
        ~pubkey:"pubkey"
        ~cipher:"cipher"
        ~proof:"proof");
    (fun () ->
      Backend.verify_range_worker ~math:false
        ~strict:false
        ~pubkey:"pubkey"
        ~cipher:"cipher"
        ~proof:"proof"
        ~commitment:"commitment");
    (fun () ->
      Backend.verify_bound_worker ~math:false
        ~strict:false
        ~pubkey:"pubkey"
        ~cipher:"cipher"
        ~proof:"proof"
        ~commitment:"commitment");
  |] in
  let completed = Atomic.make 0 in
  let results = Array.make (Array.length calls) None in
  let fast, queue_empty, threads =
    with_full_worker_pool (fun () ->
      let threads =
        Array.to_list
          (Array.mapi
             (fun index call ->
               Thread.create
                 (fun () ->
                   results.(index) <- Some (call ());
                   ignore (Atomic.fetch_and_add completed 1))
                 ())
             calls)
      in
      let rec await attempts =
        if Atomic.get completed = Array.length calls then true
        else if attempts = 0 then false
        else begin
          Unix.sleepf 0.001;
          await (attempts - 1)
        end
      in
      let fast = await 1_000 in
      let queue_empty =
        (Pool.stats Worker.proof_channel).speculative_waiting = 0
      in
      fast, queue_empty, threads)
  in
  List.iter Thread.join threads;
  check "view verifier waited for proof pool" fast;
  Array.iteri
    (fun index result ->
      check
        (Printf.sprintf "view verifier %d did not report busy" index)
        (Option.fold ~none:false ~some:busy result))
    results;
  check "view verifier entered speculative queue" queue_empty

let test_math () =
  let module F = Pvac_ffi in
  let module B = Octra_core.Crypto.FheBalance in
  let seed = Bytes.make 32 '\011' in
  let pk, sk = F.keygen_from_seed (F.default_params ()) seed in
  let cipher = F.enc_value_seeded pk sk 1L seed in
  let fields action field value = [
    "action", `String action;
    "cap", `Bool true;
    "pubkey_b64", `String (F.serialize_pubkey pk |> Bytes.to_string |> Base64.encode_exn);
    "ciphertext", `String (B.encode_cipher cipher);
    field, `String (Int64.to_string value);
  ] in
  let result fields =
    match Backend.run_action fields with
    | `Assoc fields when List.assoc_opt "ok" fields = Some (`Bool true) ->
      List.assoc "value" fields
    | _ -> failwith "circle arithmetic refused"
  in
  List.iter (fun value ->
    List.iter (fun math ->
      let apply action field expected =
        let input = fields action field value in
        let actual = result (("math", `Bool math) :: input) in
        check "circle arithmetic result" (actual = `String (B.encode_cipher expected));
        if not math then check "circle legacy arithmetic" (actual = result input)
      in
      apply "cipher_scale" "factor" (F.ct_scale ~math pk cipher value);
      let lo, hi = if math && value < 0L then Int64.pred value, Int64.max_int else value, 0L in
      apply "cipher_add_const" "amount" (F.ct_add_const ~math pk cipher lo hi);
      let sub = if math && value < 0L then F.ct_add_const ~math pk cipher (Int64.neg value) 0L
        else F.ct_sub_const ~math pk cipher value in
      apply "cipher_sub_const" "amount" sub) [false; true])
    [-1L; Int64.min_int; 0L; 1L; Int64.max_int];
  let input = fields "cipher_scale" "factor" 1L in
  List.iter (fun extra ->
    let result = Backend.run_action (extra @ input) in
    check "invalid circle arithmetic mode accepted"
      (Yojson.Safe.Util.member "ok" result = `Bool false))
    [["math", `String "true"]; ["math", `Bool true; "math", `Bool false]]

let () =
  check
    "request limit"
    (Policy.request_allowed (String.make Policy.max_request_bytes 'a'));
  check
    "request overflow"
    (not
       (Policy.request_allowed
          (String.make (Policy.max_request_bytes + 1) 'a')));
  check
    "ciphertext limit"
    (Policy.ciphertext_allowed
       (String.make Policy.max_ciphertext_encoded_bytes 'a'));
  check
    "ciphertext overflow"
    (not
       (Policy.ciphertext_allowed
          (String.make (Policy.max_ciphertext_encoded_bytes + 1) 'a')));
  check
    "verifier ciphertext limit"
    (Policy.verifier_ciphertext_allowed
       (String.make Policy.max_verifier_ciphertext_encoded_bytes 'a'));
  check
    "verifier ciphertext overflow"
    (not
       (Policy.verifier_ciphertext_allowed
          (String.make
             (Policy.max_verifier_ciphertext_encoded_bytes + 1)
             'a')));
  check
    "proof limit"
    (Policy.proof_allowed (String.make Policy.max_proof_encoded_bytes 'a'));
  check
    "proof overflow"
    (not
       (Policy.proof_allowed
          (String.make (Policy.max_proof_encoded_bytes + 1) 'a')));
  check
    "commitment limit"
    (Policy.commitment_allowed
       (String.make Policy.max_commitment_encoded_bytes 'a'));
  check
    "commitment overflow"
    (not
       (Policy.commitment_allowed
          (String.make (Policy.max_commitment_encoded_bytes + 1) 'a')));
  check "empty proof rejected" (not (Policy.proof_allowed ""));
  check "empty commitment rejected" (not (Policy.commitment_allowed ""));
  check "valid shape" (Policy.verifier_shape_allowed (shape ()));
  check
    "slots rejected"
    (not (Policy.verifier_shape_allowed (shape ~slots:2 ())));
  check
    "c0 rejected"
    (not (Policy.verifier_shape_allowed (shape ~c0:0 ())));
  check
    "base layers rejected"
    (not (Policy.verifier_shape_allowed (shape ~base_layers:3 ())));
  check
    "layers rejected"
    (not
       (Policy.verifier_shape_allowed
          (shape ~layers:(Policy.max_layers + 1) ())));
  check
    "edges rejected"
    (not
       (Policy.verifier_shape_allowed
          (shape ~edges:(Policy.max_edges + 1) ())));
  check
    "proof rejection is a verdict"
    (Backend.verification_value (Error (Worker.Proof_rejected "invalid"))
     = Ok false);
  check
    "worker busy is unavailable"
    (match Backend.verification_value (Error Worker.Worker_busy) with
     | Error _ -> true
     | Ok _ -> false);
  test_busy_class ();
  test_worker_queue_is_not_held ();
  test_math ();
  Printf.printf "status = pass test = circle_wasm_hfhe_policy\n%!"