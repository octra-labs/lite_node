(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module S = Octra_core.Store_irmin
module T = S.Store.Tree

let expect name value = if not value then failwith name

let shared count =
  let* branch = T.add (T.empty ()) ["value"] (String.make 1_048_576 'x') in
  let* tree = Lwt_list.fold_left_s (fun tree index ->
    T.add_tree tree ["copies"; string_of_int index] branch) (T.empty ()) (List.init count Fun.id) in
  Lwt.return tree

let stored_key tree path =
  let* branch = T.find_tree tree path in
  let key = match Option.bind branch T.key with
    | Some (`Node key) | Some (`Contents (key, _)) -> key
    | None -> failwith "GC sample key missing" in
  match Irmin_pack_unix.Pack_key.to_offset key with
  | None -> failwith "GC sample offset missing"
  | Some offset -> Lwt.return (Irmin_pack_unix.Pack_key.to_hash key, offset)

let test_offsets path =
  let count = 16 in
  let value = String.make 1_048_576 'x' in
  let paths = List.init count (fun index -> ["copies"; string_of_int index]) in
  let sizes () =
    let size name = (Unix.LargeFile.stat (Filename.concat path name)).Unix.LargeFile.st_size in
    size "store.1.prefix", size "store.1.mapping" in
  let* store = S.open_store ~fresh:true path in
  let* expected, target_hash, need, prefix, mapping = Lwt.finalize (fun () ->
    let* () = Lwt_list.iter_s (fun path ->
      let* branch = T.add (T.empty ()) ["value"] value in
      S.Store.set_tree_exn store.store ~info:(fun () -> S.Store.Info.empty) path branch) paths in
    let* () = S.write store ["height"] "two" in
    let* () = S.tag_epoch store 2 in
    let* target = S.Store.Head.get store.store in
    let tree = S.Store.Commit.tree target in
    let* nodes = Lwt_list.map_s (stored_key tree) paths in
    let* values = Lwt_list.map_s (fun path -> stored_key tree (path @ ["value"])) paths in
    let check name keys =
      let hash, _ = List.hd keys in
      expect (name ^ " hashes differ") (List.for_all (fun (other, _) -> other = hash) keys);
      expect (name ^ " offsets reused")
        (List.length (List.sort_uniq Stdlib.compare (List.map snd keys)) = count) in
    check "GC subtree" nodes;
    check "GC value" values;
    let* split = S.collect_pack_at store ~keep:1 2 in
    expect "physical copy split missing" (split = S.Gc_split 2);
    let* () = S.write store ["height"] "three" in
    let* () = S.tag_epoch store 3 in
    let* before = S.Store.Head.get store.store in
    let* decision = S.collect_pack_at ~free:S.gc_reserve store ~keep:1 3 in
    let need = match decision with
      | S.Gc_space {need; _} -> need
      | _ -> failwith "physical copy estimate did not defer" in
    expect "GC output exists before first collection"
      (not (Sys.file_exists (Filename.concat path "store.1.prefix"))
       && not (Sys.file_exists (Filename.concat path "store.1.mapping")));
    let* started = S.collect_pack_at ~free:Int64.max_int store ~keep:1 3 in
    expect "physical copy GC did not start"
      (match started with S.Gc_started {floor = 2; _} -> true | _ -> false);
    let* result = S.wait_pack_gc store in
    expect "physical copy GC failed" (result = Ok ());
    let* after = S.Store.Head.get store.store in
    expect "physical copy GC changed head" (S.Store.Commit.hash before = S.Store.Commit.hash after);
    let prefix, mapping = sizes () in
    expect "GC prefix omitted physical copies"
      (prefix >= Int64.of_int (count * String.length value));
    expect "GC mapping missing" (mapping > 0L);
    Lwt.return (S.Store.Commit.hash after, S.Store.Commit.hash target, need, prefix, mapping)
  ) (fun () -> S.close store) in
  let* store = S.open_store path in
  let* () = Lwt.finalize (fun () ->
    let* current = S.Store.Head.get store.store in
    expect "physical copy GC reopen changed head" (S.Store.Commit.hash current = expected);
    let* retained = S.Store.Branch.find store.repo "epoch_2" in
    expect "physical copy GC changed retained commit"
      (Option.fold ~none:false ~some:(fun commit -> S.Store.Commit.hash commit = target_hash) retained);
    let* () = Lwt_list.iter_s (fun path ->
      let* current = S.read store (path @ ["value"]) in
      let* retained = S.read_at_epoch store 2 (path @ ["value"]) in
      expect "GC changed physical copy values" (current = Some value && retained = Some value);
      Lwt.return_unit) paths in
    let* current = S.read store ["height"] in
    let* retained = S.read_at_epoch store 2 ["height"] in
    expect "GC changed current or retained height" (current = Some "three" && retained = Some "two");
    expect "GC reopen changed output sizes" (sizes () = (prefix, mapping));
    Lwt.return_unit
  ) (fun () -> S.close store) in
  let estimate = Int64.sub need S.gc_reserve in
  let actual = Int64.add prefix mapping in
  Printf.printf
    "event = gc_offsets copies = %d estimate_bytes = %Ld prefix_bytes = %Ld mapping_bytes = %Ld\n%!"
    count estimate prefix mapping;
  expect "GC estimate below physical output" (estimate >= actual);
  Lwt.return_unit

type pack_case = Shared | Copies | Small | Large | Dense

type pack_sample = {
  size : S.gc_measure;
  ample : S.gc_measure;
  ample_walked : bool;
  extent : int64;
  logical : int64 option;
  head : S.Store.Hash.t;
  target : S.Store.Hash.t;
}

let pack_name = function
  | Shared -> "shared"
  | Copies -> "copies"
  | Small -> "small"
  | Large -> "large"
  | Dense -> "dense"

let pack_paths shape =
  List.init (if shape = Copies then 16 else if shape = Dense then 32_768 else 64)
    (fun index -> ["copies"; string_of_int index])

let pack_value shape revision index =
  match shape with
  | Shared | Copies -> String.make 1_048_576 'x'
  | Small | Large | Dense ->
    let revision = if index mod 2 = 0 then revision else 0 in
    Printf.sprintf "%d:%d:" revision index
    ^ String.make (if shape = Large then 65_536 else 120) (Char.chr (65 + revision))

let write_pack store shape revision =
  match shape with
  | Shared when revision = 0 ->
    let* tree = shared 64 in
    S.Store.set_tree_exn store.S.store ~info:(fun () -> S.Store.Info.empty) [] tree
  | Shared | Copies when revision > 0 -> Lwt.return_unit
  | Shared | Copies | Small | Large | Dense ->
    Lwt_list.iteri_s (fun index path ->
      if revision > 0 && index mod 2 <> 0 then Lwt.return_unit
      else
        let value = pack_value shape revision index in
        if shape = Copies then
          let* branch = T.add (T.empty ()) ["value"] value in
          S.Store.set_tree_exn store.S.store ~info:(fun () -> S.Store.Info.empty) path branch
        else S.write store (path @ ["value"]) value
    ) (pack_paths shape)

let pack_epoch store shape epoch =
  let* () = write_pack store shape (epoch - 1) in
  let* () = S.write store ["height"] (string_of_int epoch) in
  S.tag_epoch store epoch

let pack_layout shape commit =
  let tree = S.Store.Commit.tree commit in
  let paths = pack_paths shape in
  let* entries = T.list tree ["copies"] in
  expect "GC entry count differs" (List.length entries = List.length paths);
  if shape <> Copies then
    expect "GC inode split not exercised" (List.length entries > S.Conf.entries);
  let check name keys =
    let hash, _ = List.hd keys in
    let offsets = List.sort_uniq Stdlib.compare (List.map snd keys) in
    expect (name ^ " hashes differ") (List.for_all (fun (other, _) -> other = hash) keys);
    expect (name ^ " offsets differ")
      (List.length offsets = if shape = Shared then 1 else List.length paths) in
  if shape = Shared || shape = Copies then begin
    let* nodes = Lwt_list.map_s (stored_key tree) paths in
    let* values = Lwt_list.map_s (fun path -> stored_key tree (path @ ["value"])) paths in
    check "GC subtree" nodes;
    check "GC contents" values;
    Lwt.return_unit
  end else Lwt.return_unit

let pack_file path generation name =
  Filename.concat path (Printf.sprintf "store.%d.%s" generation name)

let file_size path = (Unix.LargeFile.stat path).Unix.LargeFile.st_size

let pack_extent store commit =
  let fm = S.Store.Internal.file_manager store.S.repo in
  let generation = S.Store.Internal.File_manager.generation fm in
  let dispatcher = S.Store.Internal.dispatcher store.repo in
  let start = S.Store.Internal.Dispatcher.suffix_start_offset dispatcher in
  let offset, length = match Irmin_pack_unix.Pack_key.inspect (S.Store.Commit.key commit) with
    | Direct {offset; length; _} -> offset, length
    | Indexed hash ->
      begin match S.Store.Internal.Index.find (S.Store.Internal.File_manager.index fm) hash with
      | Some (offset, length, _) -> offset, length
      | None -> failwith "GC extent commit missing"
      end in
  let prefix = if generation = 0 then 0L
    else file_size (pack_file store.store_path generation "prefix") in
  let finish = Int64.add (Optint.Int63.to_int64 offset) (Int64.of_int length) in
  Int64.add prefix (Int64.max 0L (Int64.sub finish (Optint.Int63.to_int64 start)))

let pack_target store floor =
  let* commit = S.Store.Branch.find store.S.repo (Printf.sprintf "pack_split_%d" floor) in
  match commit with
  | Some commit -> Lwt.return commit
  | None -> Lwt.fail_with "GC split commit missing"

let pack_sample store shape target =
  let* size, walked = S.measure_pack ~free:S.gc_reserve store target in
  expect "GC precise walk missing" walked;
  expect "GC precise measure empty" (size.records > 0L && size.bytes > 0L);
  expect "GC space formula differs"
    (S.gc_space_need size = Int64.add S.gc_reserve
      (Int64.add size.bytes (Int64.mul 24L size.records)));
  let pending = S.measure_pack ~free:Int64.max_int store target in
  let immediate = not (Lwt.is_sleeping pending) in
  let* ample, ample_walked = pending in
  expect "GC fast estimate walked" (not ample_walked);
  expect "GC fast estimate yielded" immediate;
  expect "GC ample estimate too small" (ample.bytes >= size.bytes);
  let* () = Lwt_list.iter_s (fun (free, expected) ->
    let hash = S.Store.Commit.hash target in
    let key = Irmin_pack_unix.Pack_key.v_indexed hash in
    let indexed = S.Store.of_backend_commit store.repo key (S.Store.to_backend_commit target) in
    expect "GC indexed target is direct" (match Irmin_pack_unix.Pack_key.inspect
      (S.Store.Commit.key indexed) with Indexed actual -> actual = hash | Direct _ -> false);
    let* actual = S.measure_pack ~free store indexed in
    expect "GC indexed measure differs" (actual = expected);
    Lwt.return_unit
  ) [S.gc_reserve, (size, true); Int64.max_int, (ample, false)] in
  let* logical = if shape <> Copies then Lwt.return_none else
    let* old = S.measure_tree (S.Store.Commit.tree target) in
    Lwt.return_some (Int64.add (Int64.mul 2L old.bytes) (Int64.mul 512L old.records)) in
  let* head = S.Store.Head.get store.store in
  Lwt.return {size; ample; ample_walked; extent = pack_extent store target; logical;
    head = S.Store.Commit.hash head; target = S.Store.Commit.hash target}

let pack_refused ?measure store epoch free need =
  let* decision = S.collect_pack_at ?measure ~free store ~keep:1 epoch in
  expect "GC space refusal differs" (decision = S.Gc_space {free; need});
  expect "GC refusal retained planning" (not store.S.gc_planning);
  expect "GC refusal started worker" (S.Store.Gc.is_finished store.repo);
  Lwt.return_unit

let pack_cache store epoch sample =
  let need = S.gc_space_need sample.size in
  let calls = ref 0 in
  expect "GC file estimate is not larger" (S.gc_space_need sample.ample > need);
  let measure ~free store commit =
    incr calls;
    S.measure_pack ~free:(if !calls = 1 then Int64.max_int else free) store commit in
  let* () = pack_refused ~measure store epoch (Int64.pred need) (S.gc_space_need sample.ample) in
  expect "GC file cache not recorded" (match store.S.gc_need with
    | Some size -> not size.walked
    | None -> false);
  let* () = pack_refused ~measure store epoch (Int64.pred need) need in
  expect "GC file cache not remeasured" (!calls = 2);
  expect "GC precise cache not recorded" (match store.gc_need with
    | Some size -> size.walked
    | None -> false);
  Lwt.return_unit

let pack_values store shape floor =
  let* () = Lwt_list.iteri_s (fun index path ->
    let* current = S.read store (path @ ["value"]) in
    let* retained = S.read_at_epoch store floor (path @ ["value"]) in
    expect "GC current value differs" (current = Some (pack_value shape floor index));
    expect "GC retained value differs" (retained = Some (pack_value shape (floor - 1) index));
    Lwt.return_unit
  ) (pack_paths shape) in
  let* current = S.read store ["height"] in
  let* retained = S.read_at_epoch store floor ["height"] in
  expect "GC heights differ"
    (current = Some (string_of_int (floor + 1)) && retained = Some (string_of_int floor));
  Lwt.return_unit

let pack_preserved store shape floor sample =
  let* head = S.Store.Head.get store.S.store in
  let* retained = S.Store.Branch.find store.repo (Printf.sprintf "epoch_%d" floor) in
  expect "GC changed current commit" (S.Store.Commit.hash head = sample.head);
  expect "GC changed retained commit" (Option.fold ~none:false
    ~some:(fun commit -> S.Store.Commit.hash commit = sample.target) retained);
  pack_values store shape floor

let pack_output path shape generation sample =
  let prefix = file_size (pack_file path generation "prefix") in
  let mapping = file_size (pack_file path generation "mapping") in
  Printf.printf
    "event = gc_matrix case = %s source_generation = %d generation = %d walk_bytes = %Ld prefix_bytes = %Ld records = %Ld mapping_bytes = %Ld extent_bytes = %Ld ample_walked = %b\n%!"
    (pack_name shape) (generation - 1) generation sample.size.bytes prefix
    sample.size.records mapping sample.extent sample.ample_walked;
  expect "GC prefix differs from walk" (prefix = sample.size.bytes);
  expect "GC mapping exceeds records"
    (mapping > 0L && mapping <= Int64.mul 24L sample.size.records);
  expect "GC file extent below output" (sample.extent >= prefix);
  expect "GC ample estimate below output" (sample.ample.bytes >= prefix);
  expect "GC ample space below output"
    (S.gc_space_need sample.ample >= Int64.add S.gc_reserve (Int64.add prefix mapping));
  Option.iter (fun logical ->
    Printf.printf
      "event = gc_logical generation = %d estimate_bytes = %Ld actual_bytes = %Ld\n%!"
      generation logical (Int64.add prefix mapping);
    expect "GC logical control missed copies" (logical < Int64.add prefix mapping)) sample.logical;
  prefix, mapping

let with_pack ?(fresh = false) path action =
  let* store = S.open_store ~fresh path in
  Lwt.finalize (fun () -> action store) (fun () -> S.close store)

let pack_prepare path shape generation =
  with_pack path (fun store ->
    let floor = generation + 1 in
    let fm = S.Store.Internal.file_manager store.S.repo in
    expect "GC input generation differs"
      (S.Store.Internal.File_manager.generation fm = generation - 1);
    expect "GC reopen retained estimate" (store.gc_need = None);
    let* target = pack_target store floor in
    let* () = pack_layout shape target in
    let* () = pack_epoch store shape (floor + 1) in
    let* sample = pack_sample store shape target in
    expect "GC target equals head" (sample.target <> sample.head);
    let called = ref false in
    let measure ~free:_ _ _ =
      called := true;
      Lwt.fail_with "GC reserve invoked measure" in
    let free = Int64.pred S.gc_reserve in
    let* refused = S.collect_pack_at ~measure ~free store ~keep:1 (floor + 1) in
    expect "GC reserve invoked measure" (not !called);
    expect "GC reserve refusal differs" (refused = S.Gc_space {free; need = S.gc_reserve});
    expect "GC reserve cached estimate" (store.gc_need = None);
    let pending = S.measure_pack ~free store target in
    let immediate = not (Lwt.is_sleeping pending) in
    let* refused = Lwt.try_bind (fun () -> pending)
      (fun _ -> Lwt.return_false)
      (function Failure reason -> Lwt.return (reason = "GC free space is below reserve")
        | error -> Lwt.fail error) in
    expect "GC measure reserve refusal differs" (immediate && refused);
    let* () = pack_cache store (floor + 1) sample in
    expect "GC refusal advanced split" (store.split_epoch = Some floor);
    expect "GC refusal wrote output"
      (not (Sys.file_exists (pack_file path generation "prefix"))
       && not (Sys.file_exists (pack_file path generation "mapping")));
    let* () = pack_preserved store shape floor sample in
    Lwt.return sample)

let pack_collect path shape generation sample =
  with_pack path (fun store ->
    let floor = generation + 1 in
    let need = S.gc_space_need sample.size in
    expect "GC restart retained cache" (store.S.gc_need = None);
    let* target = pack_target store floor in
    let* size, walked = S.measure_pack ~free:S.gc_reserve store target in
    expect "GC restart estimate differs" (walked && size = sample.size);
    let* () = pack_refused store (floor + 1) (Int64.pred need) need in
    let free = if generation = 1 then Int64.succ need else need in
    let* started = S.collect_pack_at ~free store ~keep:1 (floor + 1) in
    expect "GC sufficient space refused" (match started with
      | S.Gc_started {floor = actual; _} -> actual = floor
      | _ -> false);
    let* result = S.wait_pack_gc store in
    expect "GC matrix worker failed" (result = Ok ());
    let* () = pack_preserved store shape floor sample in
    Lwt.return (pack_output path shape generation sample))

let test_pack_case path shape =
  let* () = with_pack ~fresh:true path (fun store ->
    let* () = pack_epoch store shape 1 in
    let* () = pack_epoch store shape 2 in
    let* split = S.collect_pack_at store ~keep:1 2 in
    expect "GC matrix split missing" (split = S.Gc_split 2);
    Lwt.return_unit) in
  Lwt_list.iter_s (fun generation ->
    let* sample = pack_prepare path shape generation in
    let* sizes = pack_collect path shape generation sample in
    with_pack path (fun store ->
      let fm = S.Store.Internal.file_manager store.S.repo in
      expect "GC reopen generation differs"
        (S.Store.Internal.File_manager.generation fm = generation);
      expect "GC reopen output differs"
        ((file_size (pack_file path generation "prefix"),
            file_size (pack_file path generation "mapping")) = sizes);
      pack_preserved store shape (generation + 1) sample)
  ) [1; 2]

let forward_prepare store =
  let* () = S.write store ["value"] "forward-kept" in
  let* () = S.write store ["height"] "two" in
  let* seed = S.Store.Head.get store.S.store in
  let tree = S.Store.Commit.tree seed in
  let node = S.Store.Backend.Commit.Val.node (S.Store.to_backend_commit seed) in
  let info = S.Store.Info.v ~message:(String.make 1_048_576 'p') 1L in
  let value = S.Store.Backend.Commit.Val.v ~info ~node ~parents:[] in
  let parent_hash = S.Store.Backend.Commit.Hash.hash value in
  let fm = S.Store.Internal.file_manager store.repo in
  let index = S.Store.Internal.File_manager.index fm in
  expect "GC forward parent already indexed" (S.Store.Internal.Index.find index parent_hash = None);
  let* child = S.Store.Commit.v store.repo ~info:(S.Store.Info.v ~message:"forward-child" 2L)
    ~parents:[Irmin_pack_unix.Pack_key.v_indexed parent_hash] tree in
  expect "GC child indexed absent parent" (S.Store.Internal.Index.find index parent_hash = None);
  let* parent = S.Store.Commit.v store.repo ~info ~parents:[] tree in
  expect "GC forward parent hash differs" (S.Store.Commit.hash parent = parent_hash);
  S.Store.flush store.repo;
  let* () = S.Store.Head.set store.store child in
  let* () = S.tag_epoch store 2 in
  let* split = S.collect_pack_at store ~keep:1 2 in
  let* () = match split with
    | S.Gc_split 2 -> Lwt.return_unit
    | S.Gc_error reason -> Lwt.fail_with reason
    | _ -> Lwt.fail_with "GC forward split missing" in
  let* target = pack_target store 2 in
  expect "GC forward split selected parent" (S.Store.Commit.hash target = S.Store.Commit.hash child);
  let* () = S.write store ["height"] "three" in
  let* () = S.tag_epoch store 3 in
  let* head = S.Store.Head.get store.store in
  Lwt.return (S.Store.Commit.hash child, parent_hash, S.Store.Commit.hash head)

let forward_target repo child_hash parent_hash =
  let* target = S.Store.Commit.of_hash repo child_hash in
  let target = match target with Some target -> target | None -> failwith "GC forward child missing" in
  expect "GC forward target changed" (S.Store.Commit.hash target = child_hash);
  expect "GC cold parent is not indexed" (match S.Store.Commit.parents target with
    | [key] -> (match Irmin_pack_unix.Pack_key.inspect key with
      | Indexed hash -> hash = parent_hash
      | Direct _ -> false)
    | _ -> false);
  let* parent = S.Store.Commit.of_hash repo parent_hash in
  let parent = match parent with Some parent -> parent | None -> failwith "GC forward parent missing" in
  let position commit = match Irmin_pack_unix.Pack_key.inspect (S.Store.Commit.key commit) with
    | Direct {offset; length; _} -> Optint.Int63.to_int64 offset, Int64.of_int length
    | Indexed _ -> failwith "GC forward position missing" in
  let offset, length = position target in
  let parent_offset, parent_length = position parent in
  expect "GC parent does not follow child"
    (parent_offset > offset && parent_offset >= Int64.add offset length);
  expect "GC forward parent is too small" (parent_length >= 1_048_576L);
  Printf.printf
    "event = gc_forward phase = decoded child_offset = %Ld parent_offset = %Ld parent_bytes = %Ld\n%!"
    offset parent_offset parent_length;
  Lwt.return_unit

let forward_preserved store child_hash head_hash =
  let* head = S.Store.Head.get store.S.store in
  let* retained = S.Store.Branch.find store.repo "epoch_2" in
  expect "GC forward head changed" (S.Store.Commit.hash head = head_hash);
  expect "GC forward retained commit changed" (Option.fold ~none:false
    ~some:(fun commit -> S.Store.Commit.hash commit = child_hash) retained);
  Lwt_list.iter_s (fun (path, current, prior) ->
    let* actual = S.read store path in
    let* kept = S.read_at_epoch store 2 path in
    expect "GC forward contents differ" (actual = Some current && kept = Some prior);
    Lwt.return_unit
  ) [["value"], "forward-kept", "forward-kept"; ["height"], "three", "two"]

let forward_collect store child_hash head_hash =
  let* target = pack_target store 2 in
  expect "GC forward split target changed" (S.Store.Commit.hash target = child_hash);
  expect "GC forward head equals target" (head_hash <> child_hash);
  let extent = pack_extent store target in
  let* size, walked = S.measure_pack ~free:S.gc_reserve store target in
  expect "GC forward precise walk missing" walked;
  let* ample, walked = S.measure_pack ~free:Int64.max_int store target in
  expect "GC forward fast estimate walked" (not walked);
  let* started = S.collect_pack_at ~free:(S.gc_space_need size) store ~keep:1 3 in
  let* () = match started with
    | S.Gc_started {floor = 2; _} -> Lwt.return_unit
    | S.Gc_error reason -> Lwt.fail_with reason
    | _ -> Lwt.fail_with "GC forward start differs" in
  let* result = S.wait_pack_gc store in
  let* () = match result with Ok () -> Lwt.return_unit | Error reason -> Lwt.fail_with reason in
  let prefix = file_size (pack_file store.store_path 1 "prefix") in
  let mapping = file_size (pack_file store.store_path 1 "mapping") in
  Printf.printf
    "event = gc_forward phase = collected walk_bytes = %Ld prefix_bytes = %Ld mapping_bytes = %Ld target_extent_bytes = %Ld\n%!"
    size.bytes prefix mapping extent;
  expect "GC forward prefix differs" (size.bytes = prefix);
  expect "GC forward mapping exceeds records" (mapping > 0L && mapping <= Int64.mul 24L size.records);
  expect "GC forward target extent control failed" (extent < prefix);
  expect "GC forward fast space too small"
    (ample.bytes >= prefix
     && S.gc_space_need ample >= Int64.add S.gc_reserve (Int64.add prefix mapping));
  let* () = forward_preserved store child_hash head_hash in
  Lwt.return (prefix, mapping)

let test_forward_parent path =
  let* child_hash, parent_hash, head_hash = with_pack ~fresh:true path forward_prepare in
  let config = Irmin_pack.Conf.init ~readonly:true
    ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal path in
  let* repo = S.Store.Repo.v config in
  let* () = Lwt.finalize (fun () -> forward_target repo child_hash parent_hash)
    (fun () -> S.Store.Repo.close repo) in
  let* sizes = with_pack path (fun store -> forward_collect store child_hash head_hash) in
  with_pack path (fun store ->
    expect "GC forward reopen output differs"
      ((file_size (pack_file path 1 "prefix"), file_size (pack_file path 1 "mapping")) = sizes);
    forward_preserved store child_hash head_hash)

let test_pack_matrix () =
  expect "GC reserve changed" (S.gc_reserve = Int64.shift_left 1L 33);
  expect "GC inode threshold changed" (S.Conf.entries = 32);
  List.iter (fun (bytes, records) ->
    match S.gc_space_need S.{bytes; records} with
    | exception Invalid_argument reason when reason = "GC measure is negative" -> ()
    | _ -> failwith "GC negative measure accepted"
  ) [-1L, 0L; 0L, -1L; -1L, -1L; Int64.min_int, 1L; 1L, Int64.min_int];
  List.iter (fun (bytes, records) ->
    expect "GC space saturation differs"
      (S.gc_space_need S.{bytes; records} = Int64.max_int)
  ) [Int64.max_int, 0L; 0L, Int64.max_int; Int64.max_int, Int64.max_int;
     Int64.sub Int64.max_int S.gc_reserve, 1L];
  List.iter (fun (bytes, records, extra) ->
    expect "GC exact space need differs"
      (S.gc_space_need S.{bytes; records} = Int64.add S.gc_reserve extra)
  ) [0L, 0L, 0L; 1L, 0L, 1L; 0L, 1L, 24L; 19L, 2L, 67L];
  List.iter (fun shape ->
    Printf.printf "event = gc_matrix case = %s status = started\n%!" (pack_name shape);
    Test_workspace.with_dir ("gc-" ^ pack_name shape)
      (fun path -> Lwt_main.run (test_pack_case path shape))
  ) [Shared; Copies; Small; Large];
  Printf.printf "event = gc_matrix case = forward status = started\n%!";
  Test_workspace.with_dir "gc-forward" (fun path -> Lwt_main.run (test_forward_parent path))

let test_historical path record =
  let input = open_in record in
  let count, generation, first_hash, target_hash, head_hash, tree_hash =
    Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      let count = int_of_string (input_line input) in
      let generation = int_of_string (input_line input) in
      let first_hash = input_line input in
      let target_hash = input_line input in
      let head_hash = input_line input in
      let tree_hash = input_line input in
      expect "History record has extra lines" (try ignore (input_line input); false with End_of_file -> true);
      expect "History count invalid" (count > 0 && count <= 512);
      expect "History generation invalid" (generation = 0 || generation = 1);
      count, generation, first_hash, target_hash, head_hash, tree_hash) in
  let floor = generation + 2 in
  let hash value = Irmin.Type.to_string S.Store.Hash.t value in
  let check store =
    let* head = S.Store.Head.get store.S.store in
    let* target = S.Store.Branch.find store.repo (Printf.sprintf "epoch_%d" floor) in
    let target = match target with Some value -> value | None -> failwith "History retained epoch missing" in
    expect "History HEAD differs" (hash (S.Store.Commit.hash head) = head_hash);
    expect "History target differs" (hash (S.Store.Commit.hash target) = target_hash);
    expect "History tree differs" (hash (T.hash (S.Store.Commit.tree head)) = tree_hash);
    let* () = Lwt_list.iter_s (fun index ->
      let key = ["history"; string_of_int index] in
      let value = Printf.sprintf "%08d:" index
        ^ String.make (1_048_576 - 9) (Char.chr (33 + index mod 80)) in
      let* current = S.read store key in
      let* retained = S.read_at_epoch store floor key in
      expect "History value differs" (current = Some value && retained = Some value);
      Lwt.return_unit) (List.init count Fun.id) in
    let* current = S.read store ["height"] in
    let* retained = S.read_at_epoch store floor ["height"] in
    expect "History heights differ"
      (current = Some (if generation = 0 then "three" else "four")
       && retained = Some (if generation = 0 then "two" else "three"));
    Lwt.return_unit in
  let* () = with_pack path (fun store ->
    let fm = S.Store.Internal.file_manager store.S.repo in
    expect "History input generation differs"
      (S.Store.Internal.File_manager.generation fm = generation);
    expect "History split differs" (store.split_epoch = Some floor);
    let* () = check store in
    if generation = 0 then begin
      let* first = S.Store.Branch.find store.repo "epoch_1" in
      expect "History oldest commit differs" (Option.fold ~none:false
        ~some:(fun commit -> hash (S.Store.Commit.hash commit) = first_hash) first);
      let* oldest = S.read_at_epoch store 1 ["history"; "0"] in
      expect "History old contents missing" (Option.is_some oldest);
      Lwt.return_unit
    end else Lwt.return_unit) in
  let* () = with_pack path (fun store ->
    let* target = pack_target store floor in
    let start = Mtime_clock.elapsed () in
    let* size, walked = S.measure_pack ~free:S.gc_reserve store target in
    let seconds = Mtime.Span.to_float_ns (Mtime.Span.abs_diff start (Mtime_clock.elapsed ())) /. 1e9 in
    expect "History physical walk missing" walked;
    expect "History physical bytes missing" (size.bytes >= Int64.of_int (count * 1_048_576));
    expect "History disk reserve insufficient" (S.disk_free path >= S.gc_space_need size);
    let* started = S.collect_pack_at store ~keep:1 (floor + 1) in
    expect "History GC did not start"
      (match started with S.Gc_started {floor = actual; _} -> actual = floor | _ -> false);
    let* finished = S.wait_pack_gc store in
    expect "History GC failed" (finished = Ok ());
    let prefix = file_size (pack_file path (generation + 1) "prefix") in
    let mapping = file_size (pack_file path (generation + 1) "mapping") in
    expect "History prefix differs from measurement" (prefix = size.bytes);
    expect "History mapping exceeds estimate" (mapping <= Int64.mul 24L size.records);
    let* gone = S.Store.Branch.find store.repo "epoch_1" in
    expect "History expired tag remains" (gone = None);
    let* () = check store in
    Printf.printf "event = history_gc values = %d input_generation = %d bytes = %Ld records = %Ld mapping = %Ld measure_seconds = %.6f\n%!"
      count generation prefix size.records mapping seconds;
    Lwt.return_unit) in
  let* () = with_pack path (fun store ->
    expect "History output generation differs"
      (S.Store.Internal.File_manager.generation (S.Store.Internal.file_manager store.S.repo) = generation + 1);
    check store) in
  Printf.printf "event = history_reopen status = passed values = %d\n%!" count;
  Lwt.return_unit

let checkpoint_fields record =
  let json = Yojson.Safe.from_file record in
  let text name = Yojson.Safe.Util.(json |> member name |> to_string) in
  let epoch = Yojson.Safe.Util.(json |> member "epoch_id" |> to_int) in
  expect "Checkpoint epoch invalid" (epoch > 0 && epoch <= max_int - 3);
  epoch, text "irmin_commit", text "ledger_state_root", text "epoch_index_root", text "state_root"

let checkpoint_read store fields =
  let epoch, commit, ledger, index, state = fields in
  let hash value = Irmin.Type.to_string S.Store.Hash.t value in
  let* head = S.Store.Head.get store.S.store in
  expect "Checkpoint HEAD differs" (hash (S.Store.Commit.hash head) = commit);
  let tree = S.Store.Commit.tree head in
  expect "Checkpoint ledger root differs" (hash (T.hash tree) = ledger);
  expect "Checkpoint folded root differs" (S.snapshot_matches ~index_root:index ~state_root:state ledger = Ok ());
  let* height = T.find tree ["meta"; "last_epoch"] in
  expect "Checkpoint epoch differs" (height = Some (string_of_int epoch));
  let add context value = Digestif.SHA256.feed_string context (string_of_int (String.length value) ^ ":" ^ value) in
  let* count, bytes, context = T.fold ~force:`True ~cache:false ~uniq:`False ~order:`Sorted
    ~contents:(fun path value (count, bytes, context) ->
      let context = if path = ["meta"; "last_epoch"] then context else
        let context = add context (string_of_int (List.length path)) in
        List.fold_left add context path |> fun context -> add context value in
      let count = Int64.succ count in
      let bytes = Int64.add bytes (Int64.of_int (String.length value)) in
      let* () = if Int64.rem count 4096L = 0L then Lwt.pause () else Lwt.return_unit in
      Lwt.return (count, bytes, context)) tree (0L, 0L, Digestif.SHA256.empty) in
  expect "Checkpoint contents missing" (count > 0L && bytes > 0L);
  let digest = Digestif.SHA256.(to_hex (get context)) in
  Printf.printf "event = checkpoint_read epoch = %d values = %Ld bytes = %Ld payload_digest = %s\n%!"
    epoch count bytes digest;
  Lwt.return (Int64.pred count, Int64.sub bytes (Int64.of_int (String.length (string_of_int epoch))), digest)

let test_checkpoint path record =
  let fields = checkpoint_fields record in
  let epoch, _, _, _, _ = fields in
  let* original =
    let* store = S.open_store ~readonly:true path in
    Lwt.finalize (fun () -> checkpoint_read store fields) (fun () -> S.close store) in
  let floor = epoch + 1 in
  let retained_read store fields =
    let* commit = S.Store.Branch.find store.S.repo (Printf.sprintf "epoch_%d" floor) in
    match commit with
    | None -> Lwt.fail_with "Checkpoint retained epoch missing"
    | Some commit ->
      let* view = S.Store.of_commit commit in
      checkpoint_read {store with S.store = view} fields in
  let* retained_fields, next_fields = with_pack path (fun store ->
    let hash value = Irmin.Type.to_string S.Store.Hash.t value in
    let _, _, _, index, _ = fields in
    let* before = S.Store.Head.get store.store in
    let advance height =
      let* tree = T.add (S.Store.Commit.tree before) ["meta"; "last_epoch"] (string_of_int height) in
      let* () = S.write store ["meta"; "last_epoch"] (string_of_int height) in
      let* () = S.tag_epoch store height in
      let* head = S.Store.Head.get store.store in
      let ledger = hash (T.hash (S.Store.Commit.tree head)) in
      expect "Checkpoint advance changed other values" (ledger = hash (T.hash tree));
      let state = Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:ledger ~epoch_index_root:index in
      Lwt.return (head, (height, hash (S.Store.Commit.hash head), ledger, index, state)) in
    let* head, retained_fields = advance floor in
    let* _ = advance (floor + 1) in
    let* () = S.set_split store floor head in
    let* _, next_fields = advance (floor + 2) in
    let generation = S.Store.Internal.File_manager.generation (S.Store.Internal.file_manager store.repo) in
    let started = Mtime_clock.counter () in
    let* size, walked = S.measure_pack ~free:S.gc_reserve store head in
    let seconds = Mtime.Span.to_float_ns (Mtime_clock.count started) /. 1e9 in
    expect "Checkpoint physical walk missing" walked;
    expect "Checkpoint measured bytes missing" (size.bytes > 0L && size.records > 0L);
    expect "Checkpoint disk reserve insufficient" (S.disk_free path >= S.gc_space_need size);
    Printf.printf "event = checkpoint_measure bytes = %Ld records = %Ld seconds = %.6f\n%!"
      size.bytes size.records seconds;
    let* decision = S.collect_pack_at store ~keep:2 (floor + 2) in
    begin match decision with
    | S.Gc_error reason -> failwith ("Checkpoint GC refused: " ^ reason)
    | _ -> ()
    end;
    expect "Checkpoint GC did not start"
      (match decision with S.Gc_started {floor = actual; _} -> actual = floor | _ -> false);
    let* finished = S.wait_pack_gc store in
    expect "Checkpoint GC failed" (finished = Ok ());
    let prefix = file_size (pack_file path (generation + 1) "prefix") in
    let mapping = file_size (pack_file path (generation + 1) "mapping") in
    expect "Checkpoint prefix differs from measurement" (prefix = size.bytes);
    expect "Checkpoint mapping exceeds estimate" (mapping <= Int64.mul 24L size.records);
    let* retained = S.Store.Branch.find store.repo (Printf.sprintf "epoch_%d" floor) in
    expect "Checkpoint retained commit differs" (Option.fold ~none:false
      ~some:(fun value -> S.Store.Commit.hash value = S.Store.Commit.hash head) retained);
    let* after = retained_read store retained_fields in
    expect "Checkpoint retained contents changed after GC" (after = original);
    let* after = checkpoint_read store next_fields in
    expect "Checkpoint current contents changed after GC" (after = original);
    Printf.printf "event = checkpoint_gc generation = %d bytes = %Ld mapping = %Ld\n%!"
      (generation + 1) prefix mapping;
    Lwt.return (retained_fields, next_fields)) in
  let* () = with_pack path (fun store ->
    let* after = retained_read store retained_fields in
    expect "Checkpoint retained contents changed after reopen" (after = original);
    let* after = checkpoint_read store next_fields in
    expect "Checkpoint current contents changed after reopen" (after = original);
    Lwt.return_unit) in
  Printf.printf "event = checkpoint status = passed epoch = %d\n%!" epoch;
  Lwt.return_unit

let checkpoint_case path =
  expect "Checkpoint case output exists" (not (Sys.file_exists path));
  Unix.mkdir path 0o700;
  let* store = S.open_store ~fresh:true (Filename.concat path "irmin_store") in
  Lwt.finalize (fun () ->
    let* () = S.write store ["meta"; "last_epoch"] "42" in
    let* () = S.write store ["before"; "value"] (String.make 1_048_576 'a') in
    let* () = S.write store ["after"; "value"] (String.make 1_048_576 'b') in
    let* () = S.tag_epoch store 42 in
    let* head = S.Store.Head.get store.store in
    let hash value = Irmin.Type.to_string S.Store.Hash.t value in
    let ledger = hash (T.hash (S.Store.Commit.tree head)) in
    let index = String.make 64 '0' in
    let state = Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:ledger ~epoch_index_root:index in
    S.Store.split store.repo;
    let* () = S.write store ["meta"; "last_epoch"] "43" in
    S.Store.flush store.repo;
    let* started = S.Store.Gc.run store.repo (S.Store.Commit.key head) in
    expect "Checkpoint seed GC did not start" (started = Ok true);
    let* finished = S.Store.Gc.wait store.repo in
    expect "Checkpoint seed GC failed" (Result.is_ok finished);
    let* () = S.Store.Head.set store.store head in
    let json = `Assoc ["epoch_id", `Int 42; "irmin_commit", `String (hash (S.Store.Commit.hash head));
      "ledger_state_root", `String ledger; "epoch_index_root", `String index; "state_root", `String state] in
    let output = open_out_gen [Open_wronly; Open_creat; Open_excl] 0o600 (Filename.concat path "HEAD.json") in
    Fun.protect ~finally:(fun () -> close_out output) (fun () -> Yojson.Safe.to_channel output json);
    Lwt.return_unit) (fun () -> S.close store)

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--physical" then begin
    try
      Test_workspace.with_dir "gc-measure" (fun path -> Lwt_main.run (test_offsets path));
      print_endline "event = gc_measure mode = physical status = passed"
    with error ->
      Printf.eprintf "event = gc_measure mode = physical status = failed reason = %S\n%!"
        (Printexc.to_string error);
      exit 1
  end else if Array.length Sys.argv = 4 && Sys.argv.(1) = "--historical" then begin
    Lwt_main.run (test_historical Sys.argv.(2) Sys.argv.(3))
  end else if Array.length Sys.argv = 4 && Sys.argv.(1) = "--checkpoint" then begin
    Lwt_main.run (test_checkpoint Sys.argv.(2) Sys.argv.(3))
  end else if Array.length Sys.argv = 3 && Sys.argv.(1) = "--checkpoint-case" then begin
    Lwt_main.run (checkpoint_case Sys.argv.(2))
  end else if Array.length Sys.argv = 3 && Sys.argv.(1) = "--dense" then begin
    expect "GC dense output exists" (not (Sys.file_exists Sys.argv.(2)));
    expect "GC dense disk reserve insufficient"
      (S.disk_free (Filename.dirname Sys.argv.(2)) >= Int64.add S.gc_reserve 2_147_483_648L);
    Lwt_main.run (test_pack_case Sys.argv.(2) Dense)
  end else if Array.length Sys.argv = 1 then begin
    test_pack_matrix ();
    print_endline "event = gc_measure status = passed"
  end else begin
    Printf.eprintf "event = usage command = test_gc_measure argument = %s\n%!" "--physical";
    exit 1
  end