(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let require condition message =
  if not condition then failwith message

let hash value =
  Digestif.SHA256.(digest_string value |> to_hex)

type expected =
  | Int of int
  | Bytes of string
  | Data of string

type sample = {
  name : string;
  raw : string;
  digest : string;
  expected : expected;
  effort : int;
}

let sample name raw digest expected effort = {
  name;
  raw = Base64.decode_exn raw;
  digest;
  expected;
  effort;
}

let samples = [
  sample
    "arithmetic"
    "T0NUQgEABwARAAAAAAEAAAA3AAIAAAAxMwABAAAAMwACAAAAMTcAAQAAADUAAQAAADQAAgAAAC0yCwEAAAsCAQAAAQECBQEBBgEBCwICAAMBAQILAgMACwMEAAQCAgMLAwUAAgICAwABAQILAgYAAQEBAgwAARc="
    "f67e425c5c5acafc2fd2605ff95cc05b79ee4e806908768db4fa83785e2bd638"
    (Int 16)
    49;
  sample
    "branch"
    "T0NUQgEABQAVAAAAAAEAAAAyAAEAAAAzAAEAAAA1AAEAAAA5AAEAAAA3CwEAAAsCAQAAAQECCwICAAwDAgwEAQEDAwQMBAMGBAQMBQMHBAQFFQQPAAAACwEDAAwEARQSAAAAFg8AAAALAQQADAQBFhIAAAAMAAQX"
    "76080bd0295b62b6674b6e12a1a4826e9d607ad076438689aa6d14c7bc407a46"
    (Int 7)
    34;
  sample
    "bytes"
    "T0NUQgEABAAPAAAAAwIAAAABAgMCAAAAAwQAAQAAADAAAQAAADILAQAACwIBACgBAQIMAgELAwIACwQDAEMCAgMEDAQBCwMDACwFBAEFBQNDBAQDBSgCAgQMAAIX"
    "dae2e9a1e164cc9c5f710f8662812f84881472d615b6ac9a6fae2fa1853eca11"
    (Bytes "\x01\x02\x03\x04")
    32;
  sample
    "structure"
    "T0NUQgEABQALAAAAAAEAAAAzAAEAAAA1AAEAAAA4AAIAAAAxMwACAAAAMjELAQAACwIBAAwDAQwEAgADAwQLBAIACwUDAAsGBAAAAwMFDAADFw=="
    "8176572efadc92792c6bbd50948130234be55f51a8b376e4bd0a04d49e96f9df"
    (Int 21)
    17;
  sample
    "complete"
    "T0NUQgEAAQADAAAAAAIAAAAxNwsBAAAMAAEX"
    "06e89666a62f12aacb0aa754a73749971b43109f98d406ff3f262dc9df34b707"
    (Int 17)
    3;
  sample
    "result"
    "T0NUQgEAAQADAAAAAsIAAABBUjEKMTExMDAxMTAxMTEwMTAxMTAwMDExMDExMTAxMDEwMTAwMTEwMDAwMTEwMDAxMTAxMTEwMTAxMDAxMDEwMTEwMTExMDEwMDEwMTAxMTAxMTEwMTAxMTAwMDExMDExMTAxMDEwMTAwMTEwMTExMDEwMTAwMDEwMTEwMDAxMTAxMTEwMTAxMDAwMTAxMTAwMTAwMTEwMTExMDEwMTAwMDEwMTAxMDEwMTAwMTAxMTAxMTEwMTAwMDAwMTAwMTAxMAsBAAAMAAEX"
    "580a1f4480ca8c6cd2454a3a08e603ccf055759be4a286e2ff537eb6c3b5459c"
    (Data "952041ac006091a1cba814809793e7a8754001f84deece89037c7ebf9716b824")
    3;
]

let raw =
  Base64.decode_exn
    "T0NUQgEABAAPAAAAAQEAAAAxAwAAAAAAAQAAADMAAQAAADQLAQAACwIAAAsDAQALBAIACwUDAAwGBAwHBQwIBgwJBwwKCQwLCAwMCgALCwwMAAsX"

let profile =
  match Octra_vm.Aml_profile.decode raw with
  | Ok value -> value
  | Error error -> failwith (Octra_vm.Aml_profile.error_text error)

let state code =
  let state =
    Octra_vm.Contract_vm.create_state
      ~strict_values:true
      ~caller:""
      ~origin:""
      ~address:""
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  require (Octra_vm.Contract_vm.run state code) "base VM refused AML code";
  state

let require_int expected = function
  | Octra_vm.Contract_vm.VInt value ->
    require (Z.equal value (Z.of_int expected)) "AML result differs"
  | _ -> failwith "AML result type differs"

let matches expected actual =
  match expected, actual with
  | Int expected, Octra_vm.Contract_vm.VInt actual ->
    Z.equal actual (Z.of_int expected)
  | Bytes expected, Octra_vm.Contract_vm.VBytes actual ->
    String.equal expected actual
  | Data expected, Octra_vm.Contract_vm.VString actual ->
    String.equal expected (hash actual)
  | _ -> false

let check_sample sample =
  let profile =
    match Octra_vm.Aml_profile.decode sample.raw with
    | Ok value -> value
    | Error error ->
      failwith
        (sample.name ^ ": " ^ Octra_vm.Aml_profile.error_text error)
  in
  require (String.equal profile.digest sample.digest)
    (sample.name ^ " digest differs");
  require
    (String.equal sample.raw (Octra_vm.Bytecode.encode profile.code))
    (sample.name ^ " bytes differ");
  let outcome =
    match Octra_vm.Aml_profile.run profile with
    | Ok value -> value
    | Error error ->
      failwith
        (sample.name ^ ": " ^ Octra_vm.Aml_profile.error_text error)
  in
  require (matches sample.expected outcome.value)
    (sample.name ^ " result differs");
  require (outcome.effort = sample.effort)
    (sample.name ^ " effort differs")

let sample_named name =
  List.find (fun sample -> String.equal sample.name name) samples

let require_refused name raw =
  match Octra_vm.Aml_profile.decode raw with
  | Error _ -> ()
  | Ok _ -> failwith (name ^ " accepted")

let require_run_refused name limit profile =
  match Octra_vm.Aml_profile.run ~limit profile with
  | Error Octra_vm.Aml_profile.Refused -> ()
  | Error error -> failwith (name ^ " " ^ Octra_vm.Aml_profile.error_text error)
  | Ok _ -> failwith (name ^ " accepted")

let refuse_code name code =
  require_refused name (Octra_vm.Bytecode.encode code)

let read_path path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let check_path path =
  let profile =
    match Octra_vm.Aml_profile.decode (read_path path) with
    | Ok value -> value
    | Error error ->
      failwith (path ^ ": " ^ Octra_vm.Aml_profile.error_text error)
  in
  match Octra_vm.Aml_profile.run profile with
  | Ok _ -> ()
  | Error error ->
    failwith (path ^ ": " ^ Octra_vm.Aml_profile.error_text error)

let () =
  require
    (String.equal
      profile.digest
      "891c85054f7f46dfe235b33d75262307d3f8c8abe64e14fb61d3c15d18bd9614")
    "AMLC artifact hash differs";
  require
    (String.equal raw (Octra_vm.Bytecode.encode profile.code))
    "AMLC artifact bytes differ";
  let base = state profile.code in
  require_int 7 base.regs.(0);
  require (base.effort_used = 17) "base VM effort changed";
  let bytes_sample = sample_named "bytes" in
  let bytes_profile =
    match Octra_vm.Aml_profile.decode bytes_sample.raw with
    | Ok value -> value
    | Error error -> failwith (Octra_vm.Aml_profile.error_text error)
  in
  let base_bytes = state bytes_profile.code in
  require
    (match base_bytes.regs.(0) with
     | Octra_vm.Contract_vm.VString value ->
       String.equal value "\x01\x02\x03\x04"
     | _ -> false)
    "base VM bytes result changed";
  require (base_bytes.effort_used = 31) "base VM bytes effort changed";
  let active =
    match Octra_vm.Aml_profile.run profile with
    | Ok value -> value
    | Error error -> failwith (Octra_vm.Aml_profile.error_text error)
  in
  require_int 7 active.value;
  require (active.effort = 18) "AML profile effort differs";
  let wide = Z.shift_left Z.one 128 in
  let wide_raw =
    Octra_vm.Bytecode.encode
      [|Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VInt wide);
        Octra_vm.Contract_vm.LDI (2, Octra_vm.Contract_vm.VInt wide);
        Octra_vm.Contract_vm.MUL (0, 1, 2);
        Octra_vm.Contract_vm.STOP|]
  in
  let wide_profile =
    match Octra_vm.Aml_profile.decode wide_raw with
    | Ok value -> value
    | Error error -> failwith (Octra_vm.Aml_profile.error_text error)
  in
  require_run_refused "wide work" 14 wide_profile;
  let wide_out =
    match Octra_vm.Aml_profile.run ~limit:15 wide_profile with
    | Ok value -> value
    | Error error -> failwith (Octra_vm.Aml_profile.error_text error)
  in
  require (wide_out.effort = 15) "wide integer effort differs";
  begin
    match wide_out.value with
    | Octra_vm.Contract_vm.VInt value ->
      require (Z.equal value (Z.mul wide wide)) "wide integer result differs"
    | _ -> failwith "wide integer result type differs"
  end;
  require_refused
    "host opcode"
    (Octra_vm.Bytecode.encode
      [|Octra_vm.Contract_vm.EPOCH 0; Octra_vm.Contract_vm.STOP|]);
  require_refused
    "storage opcode"
    (Octra_vm.Bytecode.encode
      [|Octra_vm.Contract_vm.SLOAD (0, "value");
        Octra_vm.Contract_vm.STOP|]);
  require_refused
    "FHE opcode"
    (Octra_vm.Bytecode.encode
      [|Octra_vm.Contract_vm.FHE_ADD (0, 1, 2, 3);
        Octra_vm.Contract_vm.STOP|]);
  require_refused
    "mark"
    (Octra_vm.Bytecode.encode
      [|Octra_vm.Contract_vm.JDEST 1; Octra_vm.Contract_vm.STOP|]);
  require_refused
    "literal"
    (Octra_vm.Bytecode.encode
      [|Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VAddr "x");
        Octra_vm.Contract_vm.STOP|]);
  refuse_code
    "empty register"
    [|Octra_vm.Contract_vm.MOV (0, 1);
      Octra_vm.Contract_vm.STOP|];
  refuse_code
    "arithmetic kind"
    [|Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VBool true);
      Octra_vm.Contract_vm.LDI (2, Octra_vm.Contract_vm.VInt Z.one);
      Octra_vm.Contract_vm.ADD (0, 1, 2);
      Octra_vm.Contract_vm.STOP|];
  refuse_code
    "byte kind"
    [|Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VString "AR1\n1");
      Octra_vm.Contract_vm.STRLEN (0, 1);
      Octra_vm.Contract_vm.STOP|];
  refuse_code
    "data marker"
    [|Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VString "value");
      Octra_vm.Contract_vm.STOP|];
  refuse_code
    "data operation"
    [|Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VString "AR1\n1");
      Octra_vm.Contract_vm.LDI (2, Octra_vm.Contract_vm.VString "AR1\n1");
      Octra_vm.Contract_vm.EQ (0, 1, 2);
      Octra_vm.Contract_vm.STOP|];
  refuse_code
    "guard kind"
    [|Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt Z.one);
      Octra_vm.Contract_vm.JIF (0, 2);
      Octra_vm.Contract_vm.JDEST 2;
      Octra_vm.Contract_vm.STOP|];
  refuse_code
    "cycle"
    [|Octra_vm.Contract_vm.JDEST 0;
      Octra_vm.Contract_vm.JMP 0|];
  refuse_code
    "fallthrough"
    [|Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt Z.one)|];
  refuse_code
    "dead code"
    [|Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt Z.one);
      Octra_vm.Contract_vm.STOP;
      Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VInt Z.zero);
      Octra_vm.Contract_vm.STOP|];
  refuse_code
    "branch join"
    [|Octra_vm.Contract_vm.LDI (0, Octra_vm.Contract_vm.VBool true);
      Octra_vm.Contract_vm.JIF (0, 5);
      Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VInt Z.one);
      Octra_vm.Contract_vm.JMP 7;
      Octra_vm.Contract_vm.NOP;
      Octra_vm.Contract_vm.JDEST 5;
      Octra_vm.Contract_vm.LDI (1, Octra_vm.Contract_vm.VBool true);
      Octra_vm.Contract_vm.JDEST 7;
      Octra_vm.Contract_vm.MOV (0, 1);
      Octra_vm.Contract_vm.STOP|];
  List.iter check_sample samples;
  let paths = Array.to_list Sys.argv |> List.tl in
  List.iter check_path paths;
  Printf.printf
    "aml_profile = pass artifacts = 7 refusals = 15 files = %d\n%!"
    (List.length paths)