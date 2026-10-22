(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type outcome = Checked | Different | Pending | Failed

type input = {
  stored : string option;
  record : Octra_core.Store_chaindata.program_record;
}

type state = {
  generation : int;
  active : string list;
  cache : (string * outcome) list;
  closed : bool;
}

type message = Ask of string | Finish of int * string * outcome | Stop
type effect = Reply of outcome | Check of int * string

let capacity = 2
let cache_limit = 128
let initial = {generation = 0; active = []; cache = []; closed = false}

let rec take count = function
  | _ when count <= 0 -> []
  | [] -> []
  | first :: rest -> first :: take (count - 1) rest

let step state = function
  | Stop -> {state with closed = true; generation = state.generation + 1; cache = []}, Reply Pending
  | Ask _ when state.closed -> state, Reply Pending
  | Ask key ->
    begin match List.assoc_opt key state.cache with
    | Some result ->
      {state with cache = (key, result) :: List.remove_assoc key state.cache}, Reply result
    | None when List.mem key state.active || List.length state.active >= capacity -> state, Reply Pending
    | None -> {state with active = key :: state.active}, Check (state.generation, key)
    end
  | Finish (generation, key, result) ->
    if generation <> state.generation || not (List.mem key state.active) then state, Reply Pending
    else
      let cache = match result with
        | Checked | Different ->
          (key, result) :: take (cache_limit - 1) (List.remove_assoc key state.cache)
        | Pending | Failed -> state.cache in
      {state with active = List.filter ((<>) key) state.active; cache}, Reply result

let identity input =
  let open Digestif.SHA256 in
  init () |> fun hash -> feed_string hash Program_record.consensus_id
    |> fun hash -> feed_string hash (Octra_core.Store_chaindata.program_record_json input.record)
    |> fun hash -> feed_string hash (Option.value input.stored ~default:"")
    |> get |> to_hex

let check input =
  Lwt.catch (fun () -> Octra_core.Exec_resource.detach (fun () ->
    match Option.bind input.stored (fun value -> Result.to_option (Base64.decode value)) with
    | None -> Failed
    | Some code when Digestif.SHA256.(digest_string code |> to_hex) <> input.record.code_hash -> Failed
    | Some code ->
      match Program_record.record_scope ~stored:code input.record with
      | Ok () -> Checked
      | Error "program source scope differs" -> Different
      | Error _ -> Failed) ())
    (fun _ -> Lwt.return Failed)

type t = {mutable state : state; check : input -> outcome Lwt.t}

let create ?(check = check) () = {state = initial; check}

let send reader message =
  let state, effect = step reader.state message in
  reader.state <- state;
  effect

let query reader input =
  match send reader (Ask (identity input)) with
  | Reply result -> Lwt.return result
  | Check (generation, key) ->
    let job = Octra_core.Rpc_work.run ~busy:(fun () -> Lwt.return Pending)
      (fun () -> Lwt.catch (fun () -> reader.check input) (fun _ -> Lwt.return Failed)) in
    let finished =
      let open Lwt.Syntax in
      let+ result = job in
      match send reader (Finish (generation, key, result)) with
      | Reply result -> result
      | Check _ -> Failed in
    let wait =
      let open Lwt.Syntax in
      let+ () = Lwt_unix.sleep 2. in Pending in
    Lwt.pick [Lwt.protected finished; wait]

let shutdown reader = ignore (send reader Stop)