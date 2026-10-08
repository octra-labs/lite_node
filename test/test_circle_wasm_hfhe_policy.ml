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

let test_keys () =
  let module F = Pvac_ffi in
  let module B = Octra_core.Crypto.FheBalance in
  let one = Hfhe_case.cipher () in
  let constant = Hfhe_case.cipher ~edges:false () in
  List.iter (fun budget ->
    let raw = Hfhe_case.key ~budget () in
    let pk = F.deserialize_pubkey raw in
    List.iter (fun bytes ->
      let pubkey = bytes |> Bytes.to_string |> Base64.encode_exn in
      let call mode cap action lhs rhs =
        let fields = [
          "action", `String action;
          "cap", `Bool cap;
          "pubkey_b64", `String pubkey;
          "lhs_ciphertext", `String (B.encode_cipher lhs);
          "rhs_ciphertext", `String (B.encode_cipher rhs);
        ] in
        Backend.call_json (Yojson.Safe.to_string (`Assoc (mode @ fields)))
        |> Yojson.Safe.from_string
      in
      List.iter (fun cap ->
        List.iter (fun (action, operation) ->
          List.iter (fun wrong ->
            List.iter (fun (lhs, rhs) ->
              let prior = call [] cap action lhs rhs in
              if budget < 2 then
                check "prior key error changed"
                  (prior = Backend.unavailable_json "hfhe backend exception")
              else
                check "prior key result changed"
                  (Yojson.Safe.Util.member "ok" prior = `Bool true);
              check "disabled key mode changed"
                (call ["hfhe_pairs", `Bool false] cap action lhs rhs = prior);
              let active = call ["hfhe_pairs", `Bool true] cap action lhs rhs in
              check "incompatible key became technical retry"
                (active = Backend.error_json "hfhe key shape mismatch"))
              [one, wrong; wrong, one])
            [Hfhe_case.cipher ~index:2 (); Hfhe_case.cipher ~width:2 ()];
          let slots = call ["hfhe_pairs", `Bool true] cap action one
            (Hfhe_case.cipher ~width:2 ~slots:2 ()) in
          check "slot refusal precedence changed"
            (slots = Backend.error_json "hfhe slot count mismatch");
          List.iter (fun lhs ->
            let prior = call [] cap action lhs one in
            let active = call ["hfhe_pairs", `Bool true] cap action lhs one in
            check "compatible key result changed" (active = prior);
            check "compatible key bytes differ"
              (active = Backend.value_json (`String (B.encode_cipher (operation pk lhs one)))))
            [one; constant; Hfhe_case.cipher ~c0:false (); Hfhe_case.cipher ~index:1 ();
             F.ct_add pk one one])
          ["cipher_add", F.ct_add; "cipher_sub", F.ct_sub]) [false; true])
      [raw; F.serialize_pubkey pk]) [0; 1; 2; 4]

let test_pairs () =
  test_keys ();
  let module F = Pvac_ffi in
  let module B = Octra_core.Crypto.FheBalance in
  let seed = Bytes.make 32 '\012' in
  let pk, sk = F.keygen_from_seed (F.default_params ()) seed in
  let pubkey = F.serialize_pubkey pk |> Bytes.to_string |> Base64.encode_exn in
  let one = F.enc_values_seeded pk sk [|3L|] seed in
  let two = F.enc_values_seeded pk sk [|5L; 7L|] seed in
  let call mode cap action lhs rhs =
    let fields = [
      "action", `String action;
      "cap", `Bool cap;
      "pubkey_b64", `String pubkey;
      "lhs_ciphertext", `String (B.encode_cipher lhs);
      "rhs_ciphertext", `String (B.encode_cipher rhs);
    ] in
    Backend.call_json (Yojson.Safe.to_string (`Assoc (mode @ fields)))
    |> Yojson.Safe.from_string
  in
  List.iter (fun cap ->
    List.iter (fun (action, operation) ->
      List.iter (fun (lhs, rhs) ->
        let prior = call [] cap action lhs rhs in
        check "prior pair class changed"
          (Yojson.Safe.Util.member "class" prior = `String "unavailable");
        check "prior pair message changed"
          (Yojson.Safe.Util.member "error" prior = `String "hfhe backend exception");
        check "disabled pair mode changed"
          (call ["hfhe_pairs", `Bool false] cap action lhs rhs = prior);
        let active = call ["hfhe_pairs", `Bool true] cap action lhs rhs in
        check "unequal slots became technical retry"
          (Yojson.Safe.Util.member "class" active = `String "rejected");
        check "pair refusal differs"
          (Yojson.Safe.Util.member "error" active = `String "hfhe slot count mismatch"))
        [one, two; two, one];
      List.iter (fun cipher ->
        let prior = call [] cap action cipher cipher in
        let active = call ["hfhe_pairs", `Bool true] cap action cipher cipher in
        check "valid pair result changed" (active = prior);
        check "valid pair refused" (Yojson.Safe.Util.member "ok" active = `Bool true);
        check "valid pair bytes differ"
          (Yojson.Safe.Util.member "value" active
           = `String (B.encode_cipher (operation pk cipher cipher)))) [one; two])
      ["cipher_add", F.ct_add; "cipher_sub", F.ct_sub]) [false; true];
  List.iter (fun pairs ->
    List.iter (fun fault ->
      let result = try
        ignore (Backend.cipher_pair ~pairs (fun _ _ _ -> raise fault) pk one one);
        None
      with error -> Some error in
      check "pair check converted a host fault" (result = Some fault))
      [Out_of_memory; Stack_overflow; Failure "host failure"]) [false; true];
  List.iter (fun mode ->
    let result = call mode true "cipher_add" one one in
    check "invalid pair mode accepted"
      (Yojson.Safe.Util.member "error" result = `String "invalid hfhe_pairs"))
    [["hfhe_pairs", `String "true"];
     ["hfhe_pairs", `Bool true; "hfhe_pairs", `Bool false]]

let run () =
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
  test_pairs ();
  Printf.printf "status = pass test = circle_wasm_hfhe_policy\n%!"

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--pairs" then begin
    test_pairs ();
    Printf.printf "status = pass test = hfhe_pairs\n%!"
  end
  else run ()