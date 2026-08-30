(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type policy = {
  interval : int64;
  retain : int;
}

type outcome =
  | Published
  | Failed

type event =
  | Finalized of int64
  | Completed of {
      epoch : int64;
      outcome : outcome;
    }

type effect =
  | Capture of int64
  | Retain of int

type t = {
  published : int64 option;
  running : int64 option;
  seen : int64 option;
}

let policy ~interval ~retain =
  if Int64.compare interval 1L < 0 then
    Error "sync interval must be positive"
  else if retain < 2 then
    Error "sync retention must be at least two"
  else
    Ok { interval; retain }

let default =
  match policy ~interval:360L ~retain:2 with
  | Ok value -> value
  | Error reason -> invalid_arg reason

let interval policy = policy.interval
let retention policy = policy.retain

let init ~published = {
  published;
  running = None;
  seen = published;
}

let published t = t.published
let running t = t.running

let target policy epoch =
  Int64.sub epoch (Int64.rem epoch policy.interval)

let due published epoch =
  match published with
  | None -> true
  | Some prior -> Int64.compare epoch prior > 0

let finalized policy t epoch =
  if Int64.compare epoch 0L < 0 then
    Error "finalized epoch is negative"
  else
    match t.seen with
    | Some seen when Int64.compare epoch seen < 0 -> Ok (t, [])
    | _ ->
        let next = { t with seen = Some epoch } in
        let target = target policy epoch in
        begin
          match t.running with
          | Some _ -> Ok (next, [])
          | None when due t.published target ->
              Ok ({ next with running = Some target }, [Capture target])
          | None -> Ok (next, [])
        end

let completed policy t epoch outcome =
  match t.running with
  | None -> Error "sync completion has no active capture"
  | Some running when running <> epoch ->
      Error "sync completion epoch mismatch"
  | Some _ ->
      begin
        match outcome with
        | Failed -> Ok ({ t with running = None }, [])
        | Published ->
            let published =
              match t.published with
              | None -> epoch
              | Some prior -> Int64.max prior epoch
            in
            Ok ({ t with published = Some published; running = None }, [Retain policy.retain])
      end

let step policy t = function
  | Finalized epoch -> finalized policy t epoch
  | Completed { epoch; outcome } -> completed policy t epoch outcome