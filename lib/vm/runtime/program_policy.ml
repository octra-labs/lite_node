(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type node = {
  target : int;
  start : int;
  stop : int;
}

let add name names =
  if List.mem name names then names else names @ [name]

let add_all names values =
  List.fold_left (fun names value -> add value names) names values

let union left right =
  add_all left right

let normalize names =
  List.sort_uniq String.compare names

let capability_effects = function
  | Program_type_flow.View -> []
  | Program_type_flow.Storage_read -> ["storage_read"]
  | Program_type_flow.Storage_write -> ["storage_write"]
  | Program_type_flow.Transfer -> ["transfer"]
  | Program_type_flow.Deploy -> ["deploy"]
  | Program_type_flow.Fhe -> ["fhe"]

let nodes code (facts : Program_type_flow.facts) =
  let targets =
    0 :: List.map (fun (entry : Program_type_flow.entry) -> entry.target) facts.entries
    |> List.sort_uniq compare
  in
  let stop target =
    match List.find_opt (fun value -> value > target) targets with
    | Some next -> next
    | None -> Array.length code
  in
  List.map (fun target -> { target; start = target; stop = stop target }) targets

let xcall_pcs code =
  Array.to_list code
  |> List.mapi (fun pc instr ->
    match instr with
    | Contract_vm.XCALL _ -> Some pc
    | _ -> None)
  |> List.filter_map (fun value -> value)

let xcall_at (facts : Program_type_flow.facts) pc =
  List.find_opt (fun (call : Program_type_flow.xcall) -> call.pc = pc) facts.xcalls

let missing_xcall_abi code (facts : Program_type_flow.facts) =
  xcall_pcs code
  |> List.filter (fun pc -> Option.is_none (xcall_at facts pc))

let direct code (facts : Program_type_flow.facts) node =
  let width = node.stop - node.start in
  let slice = Array.sub code node.start width in
  let names = Program_effects.names (Program_effects.scan slice) in
  let rec add_xcalls pc names =
    if pc = node.stop then Ok names
    else
      match code.(pc) with
      | Contract_vm.XCALL _ ->
        (match xcall_at facts pc with
         | None -> Error (Printf.sprintf "external XCALL at pc %d has no checked ABI" pc)
         | Some call ->
           let effects = List.concat_map capability_effects call.capabilities in
           add_xcalls (pc + 1) (add_all names effects))
      | _ -> add_xcalls (pc + 1) names
  in
  add_xcalls node.start names

let calls_from (facts : Program_type_flow.facts) target =
  facts.calls
  |> List.filter_map (fun (call : Program_type_flow.call) ->
    if call.owner = target then Some call.target else None)

let compute code (facts : Program_type_flow.facts) =
  let graph = nodes code facts in
  let rec summary stack target =
    if List.mem target stack then
      Error (Printf.sprintf "effect graph cycle at entry %d" target)
    else
      match List.find_opt (fun node -> node.target = target) graph with
      | None -> Error (Printf.sprintf "effect graph target %d is missing" target)
      | Some node ->
        (match direct code facts node with
         | Error error -> Error error
         | Ok names ->
           let rec inherit_effects current = function
             | [] -> Ok current
             | child :: rest ->
               (match summary (target :: stack) child with
                | Error error -> Error error
                | Ok inherited -> inherit_effects (union current inherited) rest)
           in
           (match inherit_effects names (calls_from facts target) with
            | Error error -> Error error
            | Ok effects -> Ok effects))
  in
  let rec collect summaries = function
    | [] -> Ok summaries
    | node :: rest ->
      (match summary [] node.target with
       | Error error -> Error error
       | Ok effects -> collect ((node.target, effects) :: summaries) rest)
  in
  match missing_xcall_abi code facts with
  | pc :: _ -> Error (Printf.sprintf "external XCALL at pc %d has no checked ABI" pc)
  | [] ->
    (match collect [] graph with
     | Error error -> Error error
     | Ok summaries ->
       let effects =
         List.fold_left (fun names (_, values) -> union names values) [] summaries
       in
       Ok (normalize effects, summaries))

let complete code (facts : Program_type_flow.facts) =
  match compute code facts with
  | Error error -> Error error
  | Ok (_, summaries) ->
    let entries =
      List.map (fun (entry : Program_type_flow.entry) ->
        match List.assoc_opt entry.target summaries with
        | Some effects -> { entry with effects = normalize effects }
        | None -> entry)
        facts.entries
    in
    Ok { facts with entries }

let effects code (facts : Program_type_flow.facts) =
  match compute code facts with
  | Ok (effects, _) -> Ok effects
  | Error error -> Error error

let verify code (facts : Program_type_flow.facts) declared =
  match compute code facts with
  | Error error -> Error error
  | Ok (effective, summaries) ->
    let declared = normalize declared in
    if effective <> declared then
      Error "program effect policy mismatch"
    else
      let rec check_entries = function
        | [] -> Ok ()
        | (entry : Program_type_flow.entry) :: rest ->
          let expected =
            match List.assoc_opt entry.target summaries with
            | Some effects -> normalize effects
            | None -> []
          in
          if normalize entry.effects <> expected then
            Error (Printf.sprintf "effect summary mismatch at entry %d" entry.target)
          else check_entries rest
      in
      check_entries facts.entries