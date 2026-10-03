(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module V = Octra_vm.Contract_vm
module P = Octra_vm.Program_package
module R = Octra_core.Rule_graph
module C = Octra_circle_runtime.Circle_runtime_storage

let expect name value = if not value then failwith name

let sources = [
  "scalar", "program Scalar { public pure fn get(): int { return 7 } }",
    "f76c761072c09cc9332d4935bce94b4cd2cd28349720e7ace5aabd02adf85d6d";
  "state", "program State { state { total: int } public fn add(amount: int): int { self.total = self.total + amount return self.total } }",
    "9cf0394c513818d7b858ce48c5609599b6fdc77cb7cece487f7d897d68cb10cd";
  "loop", "program Loop { private pure fn inner(limit: int): int { let sum = 0 for i in 0..limit { sum = sum + 1 } return sum } public pure fn get(): int { let sum = 0 for j in 0..4 { sum = sum + inner(j) } return sum } }",
    "f61febbe7400b257c4c072bc32c9b79162a84c120cafe4c53ed2095fe74a4ab6";
  "parameter", "program Param { public fn get(value: int): int { return value } }",
    "bad706074727781dbd1f9e6871a7bf2bfe47aec19431f790c71a6651fb7a4d3e";
  "syntax", "program Syntax { public fn get(): int { return }",
    "179deb97b95346e6f365e0e4be5b7f4f73576c2d890f399899c3a758e947c82e";
  "comment", "program Comment { /*",
    "179deb97b95346e6f365e0e4be5b7f4f73576c2d890f399899c3a758e947c82e";
]

let digest value = Digestif.SHA256.(digest_string value |> to_hex)

let compiler_files () =
  let files = [
    "prior_compile.ml", "a087065e9c7adbd47578d4005a442fcbf9866d4774f14e7030591505a086213e";
    "prior_gen.ml", "b0c5d166fb4b0a7615d28c6e65d01aae6fb89d4897f8079673c002da3cdbe631";
    "prior_lang.ml", "8b57b70ca6ef70f759545aa6b9b874afa26992ea70ee9e6e09adc07f1a2635fb";
    "prior_lex.ml", "ccd669e0f5b5ad3b853b5552c2f5db6ca06c63707078319cb31d6a02b64488fc";
    "prior_parse.ml", "8a3f43a05e9df9defa4b42cf92268a7adbab6a97101ec1ae9e5c6eabe5cafc88";
    "prior_verify.ml", "1517d5e46f6850413fe8d730e8a2812fccd9c724daaa30b229df19258c14a18e";
  ] in
  let root = if Sys.file_exists "lib/vm/compiler/prior" then "." else ".." in
  List.iter (fun (name, expected) ->
    let path = Filename.concat root ("lib/vm/compiler/prior/" ^ name) in
    let channel = open_in_bin path in
    let body = Fun.protect ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel)) in
    let body =
      match String.split_on_char '\n' body with
      | "(* SPDX-License-Identifier: BSD-3-Clause *)" ::
        "(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)" :: "" :: rows ->
        String.concat "\n" rows
      | _ -> body
    in
    let rec end_at n =
      if n > 0 && (body.[n - 1] = '\n' || body.[n - 1] = '\r') then end_at (n - 1)
      else n
    in
    let body = String.sub body 0 (end_at (String.length body)) in
    expect (name ^ " differs from frozen compiler") (digest body = expected)
  ) files

let constant_header () =
  let bytes = Bytes.of_string (Octra_vm.Bytecode.encode [|V.STOP|]) in
  Bytes.set bytes 6 '\001';
  let raw = Bytes.sub_string bytes 0 12 in
  List.iter (fun active ->
    expect "constant header error differs"
      (Octra_vm.Bytecode.decode ~active raw = Error "OCTB truncated constant header")
  ) [false; true]

let constant_count () =
  let module B = Octra_vm.Bytecode in
  let count = B.max_consts + 1 in
  let code = Array.init count (fun value -> V.LDI (0, V.VInt (Z.of_int value))) in
  let refused =
    try ignore (B.encode code); false
    with Invalid_argument reason -> reason = "constant count exceeds capacity"
  in
  expect "current encoder constant limit" refused;
  let prior = B.encode ~active:false code in
  let reason = Printf.sprintf "OCTB too many constants: %d" count in
  expect "prior encoder changed decoder outcome"
    (B.decode ~active:false prior = Error reason)

let packages () =
  List.iter (fun (name, body, reference) ->
    let resolver path = if path = "main.aml" then Some body else None in
    let prior = Octra_vm.Prior_compile.compile_program_multi_first resolver "main.aml" in
    let compiled = P.compile_for ~point_ops:false ~main:"main.aml"
      ~sources:[P.{ path = "main.aml"; body }]
    in
    match prior.error, prior.program_envelope, compiled with
    | None, Some expected, Ok compiled ->
      expect (name ^ " envelope differs") (compiled.envelope = expected);
      expect (name ^ " published hash differs") (digest expected = reference);
      let encoded = Base64.encode_exn compiled.package in
      begin match P.admit_base64 ~point_ops:false encoded with
      | Ok admitted -> expect (name ^ " admission differs") (admitted.envelope = expected)
      | Error error -> failwith (name ^ ": " ^ P.error_message error)
      end;
      Printf.printf "event = prior_package case = %s hash = %s reference = c54167b\n"
        name (digest expected)
    | Some error, _, Error actual ->
      expect (name ^ " reason differs")
        (P.error_message actual = "Program compile failed: " ^ error);
      let error_hash = digest (P.error_message actual) in
      expect (name ^ " published error hash differs") (error_hash = reference);
      Printf.printf
        "event = prior_package case = %s error_hash = %s reference = c54167b\n"
        name error_hash
    | _ -> failwith (name ^ " compilation outcome differs")
  ) sources;
  let body = "program Mixed { form add [many x: int] (many y: int) ->[many] int marks {} = x + y public fn get(): int { return add(2, 3) } }" in
  match P.compile ~main:"main.aml" ~sources:[P.{ path = "main.aml"; body }] with
  | Error error -> failwith (P.error_message error)
  | Ok compiled ->
    let encoded = Base64.encode_exn compiled.package in
    expect "active package rejected" (Result.is_ok (P.admit_base64 ~point_ops:true encoded));
    expect "prior compiler accepted new form" (Result.is_error (P.admit_base64 ~point_ops:false encoded))

let transaction point_ops nested =
  let storage = Hashtbl.create 4 in
  Hashtbl.add storage "total" "2";
  let ctx = { V.default_ctx with point_ops } in
  let state = V.create_state ~ctx ~storage ~caller:"caller" ~origin:"caller"
    ~address:"program" ~value:Z.zero () in
  let code = Array.of_list (
    [V.CHECKPOINT]
    @ (if nested then [V.CHECKPOINT] else [])
    @ [V.LDI (0, V.VInt (Z.of_int 5)); V.SSTORE ("total", 0);
       V.COMMIT; V.ROLLBACK; V.STOP])
  in
  expect "checkpoint run reverted" (V.run state code);
  Hashtbl.find storage "total", state.effort_used

let checkpoints () =
  let prior, prior_cost = transaction false true in
  let active, active_cost = transaction true true in
  expect "prior nested commit changed" (prior = "5");
  expect "active nested commit changed" (active = "2");
  expect "checkpoint price changed" (prior_cost = active_cost);
  expect "prior single commit changed" (fst (transaction false false) = "5");
  expect "active single commit changed" (fst (transaction true false) = "5")

let cipher_image () =
  let image = Bytes.make 95 '\000' in
  Bytes.blit_string "PVAC" 0 image 0 4;
  Bytes.set image 4 '\001';
  Bytes.set_int64_le image 6 65537L;
  Bytes.set_int64_le image 14 1L;
  image

let deserialize_cipher ~point_ops ~is_view =
  let ctx = { V.default_ctx with point_ops } in
  let state = V.create_state
    ~ctx
    ~is_view
    ~storage:(Hashtbl.create 1)
    ~caller:"caller"
    ~origin:"caller"
    ~address:"program"
    ~value:Z.zero
    ()
  in
  let encoded = Base64.encode_exn (Bytes.to_string (cipher_image ())) in
  V.run state [|V.LDI (0, V.VString encoded); V.FHE_DESER (1, 0); V.STOP|]

let cipher_caps () =
  expect "prior cipher decode changed"
    (deserialize_cipher ~point_ops:false ~is_view:false);
  expect "active cipher cap missing"
    (not (deserialize_cipher ~point_ops:true ~is_view:false));
  expect "view cipher cap missing"
    (not (deserialize_cipher ~point_ops:false ~is_view:true))

let storage_order () =
  let before = Hashtbl.create 64 in
  let after = Hashtbl.create 64 in
  List.iter (fun key -> Hashtbl.replace before key "old")
    ["mailbox:c"; "mailbox:a"; "mailbox:b"];
  Hashtbl.replace after "mailbox:d" "new";
  let seen = Hashtbl.create 64 in
  let expected = ref (Ok ()) in
  let visit key _ =
    match !expected with
    | Error _ -> ()
    | Ok () when Hashtbl.mem seen key -> ()
    | Ok () ->
      Hashtbl.replace seen key ();
      expected := (match Hashtbl.find_opt after key with
        | Some value -> C.validate_runtime_key_write key value
        | None -> C.validate_runtime_key_delete key)
  in
  Hashtbl.iter visit before;
  Hashtbl.iter visit after;
  expect "prior storage order changed"
    (C.validate_runtime_storage_delta ~proof_mode:R.Prior before after = !expected);
  expect "active storage order differs"
    (C.validate_runtime_storage_delta ~proof_mode:R.Active before after
     = Error ("circle_runtime_reserved_storage_key", "mailbox:a", "mailbox:"))

let () =
  compiler_files ();
  constant_header ();
  constant_count ();
  packages ();
  checkpoints ();
  cipher_caps ();
  storage_order ();
  Printf.printf "event = prior_checks status = pass packages = 7 checkpoints = 4\n"