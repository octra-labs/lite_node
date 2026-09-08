(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  octb : string;
  code : Contract_vm.instr array;
}

let finish octb =
  match Bytecode.decode_image octb with
  | Error reason -> Error reason
  | Ok image ->
    begin
      match Contract_vm.Verifier.verify image.code with
      | Ok () -> Ok { octb; code = image.code }
      | Error _ -> Error "OCTB verification refused"
    end

let compile source =
  if Aml_source.owns source then
    match Aml_source.compile source with
    | Ok value -> finish value.octb
    | Error reason -> Error reason
  else
    match C_octb.compile source with
    | Ok value -> finish value.octb
    | Error error -> Error (C_octb.text error)