(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Store_scope = Octra_core.Store_scope

let expect message value = if not value then failwith message

let failed action =
  match action () with
  | () -> failwith "failed cleanup was accepted"
  | exception Store_scope.Close_failed _ -> ()

let run () =
  let events = ref [] in
  let record name = events := !events @ [name] in
  let resource scope name fail =
    Store_scope.acquire scope (fun () -> name) (fun value ->
      record value;
      if fail then failwith ("close failed: " ^ name)) in
  let cases = [
    "close_order", (fun () ->
      let scope = Store_scope.create ~release:(fun () -> record "release") in
      ignore (resource scope "first" false);
      ignore (resource scope "second" false);
      Store_scope.close scope;
      Store_scope.close scope;
      expect "resources closed twice or out of order" (!events = ["second"; "first"; "release"]));
    "close_failure", (fun () ->
      let scope = Store_scope.create ~release:(fun () -> record "release") in
      ignore (resource scope "first" false);
      ignore (resource scope "second" true);
      ignore (resource scope "third" false);
      failed (fun () -> Store_scope.close scope);
      failed (fun () -> Store_scope.close scope);
      expect "failed close released ownership or repeated callbacks"
        (!events = ["third"; "second"; "first"]));
    "open_failure", (fun () ->
      let scope = Store_scope.create ~release:(fun () -> record "release") in
      (match Store_scope.guard scope (fun () ->
        ignore (resource scope "first" false);
        Store_scope.acquire scope (fun () -> raise Not_found) (fun () -> record "unopened")) with
      | () -> failwith "failed open was accepted"
      | exception Not_found -> ());
      expect "open failure leaked resources" (!events = ["first"; "release"]));
    "nested_failure", (fun () ->
      let outer = Store_scope.create ~release:(fun () -> record "outer_release") in
      failed (fun () -> Store_scope.guard outer (fun () ->
        ignore (resource outer "outer" false);
        ignore (Store_scope.acquire outer (fun () ->
          let inner = Store_scope.create ~release:(fun () -> record "inner_release") in
          Store_scope.guard inner (fun () ->
            ignore (resource inner "inner" true);
            raise Not_found)) Store_scope.close)));
      expect "nested failure released ownership" (!events = ["inner"; "outer"]));
    "body_failure", (fun () ->
      let scope = Store_scope.create ~release:(fun () -> record "release") in
      (match Store_scope.guard scope (fun () ->
        ignore (resource scope "first" false);
        raise Not_found) with
      | () -> failwith "body failure was accepted"
      | exception Not_found -> ());
      expect "body failure leaked resources" (!events = ["first"; "release"]));
    "closed_acquire", (fun () ->
      let scope = Store_scope.create ~release:(fun () -> record "release") in
      Store_scope.close scope;
      (match Store_scope.acquire scope (fun () -> record "opened") (fun () -> record "closed") with
      | () -> failwith "closed scope acquired a resource"
      | exception Invalid_argument _ -> ());
      expect "closed scope ran constructor" (!events = ["release"]));
    "release_failure", (fun () ->
      let scope = Store_scope.create ~release:(fun () -> record "release"; raise Not_found) in
      ignore (resource scope "first" false);
      failed (fun () -> Store_scope.close scope);
      failed (fun () -> Store_scope.close scope);
      expect "failed release was repeated" (!events = ["first"; "release"]));
    "protect_success", (fun () ->
      let value = Store_scope.protect ~close:(fun () -> record "closed") (fun () -> 7) in
      expect "successful constructor closed its resource" (value = 7 && !events = []));
    "protect_failure", (fun () ->
      (match Store_scope.protect ~close:(fun () -> record "closed") (fun () -> raise Not_found) with
      | () -> failwith "constructor failure was accepted"
      | exception Not_found -> ());
      expect "failed constructor leaked its resource" (!events = ["closed"]));
    "protect_nested", (fun () ->
      let scope = Store_scope.create ~release:(fun () -> record "release") in
      failed (fun () -> Store_scope.guard scope (fun () ->
        Store_scope.protect ~close:(fun () -> record "closed"; failwith "close error")
          (fun () -> raise Not_found)));
      expect "constructor cleanup failure released ownership" (!events = ["closed"]))] in
  let errors = List.filter_map (fun (name, action) ->
    events := [];
    match action () with
    | () -> Printf.printf "event = passed case = %s\n%!" name; None
    | exception error ->
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string error);
      Some name) cases in
  if errors <> [] then exit 1

let () = run ()