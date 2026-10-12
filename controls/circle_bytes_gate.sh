# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
if [ "${1:-}" = "--float" ]; then
  cargo test --locked --offline \
    --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
    --target-dir "$ROOT/runtime_data/circle_bytes_cargo" float_cases -- --test-threads=1 --nocapture
  for name in load_values linear_nan top1_executor attention_result_quota tensor_session_float
  do
    cargo test --locked --offline \
      --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
      --target-dir "$ROOT/runtime_data/circle_bytes_cargo" "tests::$name" -- --exact --test-threads=1
  done
  printf 'event = gate name = circle_bytes scope = float status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--relay" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j2 \
    test/test_compute_history.exe test/test_resource_compute_service.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_history.exe" --expiry
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --relay
  sh "$ROOT/controls/proof_gate.sh" formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = relay status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--replay" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  cargo test --locked --offline \
    --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
    --target-dir "$ROOT/runtime_data/circle_bytes_cargo" linear_ -- --test-threads=1 --nocapture
  cargo test --locked --offline \
    --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
    --target-dir "$ROOT/runtime_data/circle_bytes_cargo" replay_ -- --test-threads=1
  cargo test --locked --offline \
    --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
    --target-dir "$ROOT/runtime_data/circle_bytes_cargo" top1_executor -- --test-threads=1
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j2 \
    test/test_compute_engine.exe test/test_sync_chain.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_engine.exe" --payment
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_sync_chain.exe" --origin
  sh "$ROOT/controls/proof_gate.sh" formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = replay status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--transfers" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j2 \
    test/test_rest_admit.exe test/test_tx_rule.exe \
    test/test_compute_history.exe test/test_compute_deposit.exe \
    test/test_resource_compute_service.exe test/test_consensus_profile_golden.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_rest_admit.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_tx_rule.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_history.exe" --expiry
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_deposit.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --cancel-traffic
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --funding
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --funding-cancel
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = transfers status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--payment" ] || [ "${1:-}" = "--settle" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_compute_deposit.exe test/test_compute_engine.exe \
    test/test_resource_compute_service.exe test/test_compute_history.exe test/test_consensus_profile_golden.exe \
    test/test_rest_admit.exe test/test_tx_rule.exe test/test_proposal.exe test/test_sync_chain.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_rest_admit.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_tx_rule.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_proposal.exe" --compute
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_history.exe" --expiry
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_deposit.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_engine.exe" --payment
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_engine.exe" --graph
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_engine.exe" --cache
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_sync_chain.exe" --origin
  if [ "${1:-}" = "--payment" ]; then
    opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --stopped
    opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --cancel-slots
  fi
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/fee_market.v
  sh "$ROOT/controls/proof_gate.sh" formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = payment status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--funding" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_compute_funding.exe test/test_compute_deposit.exe test/test_compute_history.exe \
    test/test_fee_market.exe test/test_consensus_profile_golden.exe test/test_resource_compute_service.exe \
    test/test_sync_chain.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_sync_chain.exe" --origin
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_funding.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_deposit.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_history.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --restart
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --funding
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --result-restart
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_fee_market.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/fee_market.v formal/coq/sync_chain.v \
    formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = funding status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--cancel-send" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_compute_history.exe test/test_resource_compute_service.exe test/test_consensus_profile_golden.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_history.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --cancel-send
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = cancel-send status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--read-set" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_compute_history.exe test/test_resource_compute_service.exe test/test_consensus_profile_golden.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_history.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --read-set
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = read-set status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--set" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_compute_history.exe test/test_resource_compute_service.exe test/test_consensus_profile_golden.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_history.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --set
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = set status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--fetch" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_compute_history.exe test/test_resource_compute_service.exe test/test_consensus_profile_golden.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_history.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_resource_compute_service.exe" --fetch
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = fetch status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--terms" ] || [ "${1:-}" = "--fees" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    @compute_work test/test_consensus_profile_golden.exe test/test_fee_market.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_fee_market.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/byte_work.v
  sh "$ROOT/controls/proof_gate.sh" formal/coq/fee_market.v formal/coq/resource_attestations_model.v
  printf 'event = gate name = circle_bytes scope = %s status = passed\n' "${1#--}"
  exit 0
fi
if [ "${1:-}" = "--consensus" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_byte_work.exe test/test_vm_effects.exe test/test_vm_transition.exe \
    test/test_rule_graph.exe test/test_consensus_profile_golden.exe \
    test/test_contract_rpc_guard.exe
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_circle_program.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_byte_work.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_vm_effects.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_vm_transition.exe" --bytes
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_contract_rpc_guard.exe" --bytes
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_rule_graph.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_circle_program.exe" --inspect
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_circle_program.exe" --events
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/byte_work.v
  printf 'event = gate name = circle_bytes scope = consensus status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--input" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  tests=$(cargo test --locked --offline \
    --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
    --target-dir "$ROOT/runtime_data/circle_bytes_cargo" -- --list)
  for name in local_input_quota local_input_alloc code_input_shapes code_input_copy \
    request_input_quota request_input_lifetime request_input_errors request_input_modes \
    request_input_ffi request_input_values
  do
    printf '%s\n' "$tests" | grep -Fqx "tests::$name: test"
    cargo test --locked --offline \
      --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
      --target-dir "$ROOT/runtime_data/circle_bytes_cargo" "tests::$name" -- --exact --test-threads=1
  done
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_build_input.exe test/test_circle_program.exe \
    test/test_consensus_profile_golden.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_build_input.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_circle_program.exe" --wasm-inputs
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/byte_work.v
  printf 'event = gate name = circle_bytes scope = input status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--json" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_json_tree.exe test/test_rpc_size.exe test/test_circle_program.exe \
    test/test_consensus_profile_golden.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_json_tree.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_rpc_size.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_circle_program.exe" --wasm-inputs
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  sh "$ROOT/controls/proof_gate.sh" formal/coq/byte_work.v
  printf 'event = gate name = circle_bytes scope = json status = passed\n'
  exit 0
fi
if [ "${1:-}" = "--model" ]; then
  BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
  opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
    test/test_compute_engine.exe test/test_build_input.exe
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_build_input.exe"
  opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_engine.exe" --cache
  sh "$ROOT/controls/proof_gate.sh" formal/coq/byte_work.v
  printf 'event = gate name = circle_bytes scope = model status = passed\n'
  exit 0
fi
OUT="$ROOT/runtime_data/circle_bytes"
mkdir -p "$OUT"
cargo tree --locked --offline \
  --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
  --prefix none -e features -i wasmparser@0.221.3 > "$OUT/parser-features.txt"
sed -n 's/^wasmparser feature "\([^"]*\)".*/\1/p' "$OUT/parser-features.txt" \
  | sort -u > "$OUT/parser-features.actual"
printf 'features\nstd\nvalidate\n' > "$OUT/parser-features.expected"
diff -u "$OUT/parser-features.expected" "$OUT/parser-features.actual"
rustc --edition=2021 --test "$ROOT/lib/core/circle_wasm_native/src/native_bytes.rs" \
  -o "$OUT/check"
"$OUT/check" --test-threads=1
if [ "${1:-}" = "--write" ] || [ "${1:-}" = "--memory" ] \
  || [ "${1:-}" = "--session" ] || [ "${1:-}" = "--tensor" ] \
  || [ "${1:-}" = "--matrix" ] || [ "${1:-}" = "--linear" ] \
  || [ "${1:-}" = "--read" ] || [ "${1:-}" = "--q24" ] \
  || [ "${1:-}" = "--top1" ] || [ "${1:-}" = "--attention" ] \
  || [ "${1:-}" = "--tensor-cache" ] || [ "${1:-}" = "--load" ] \
  || [ "${1:-}" = "--pages" ] || [ "${1:-}" = "--numeric" ] \
  || [ "${1:-}" = "--result" ] || [ "${1:-}" = "--storage" ] \
  || [ "${1:-}" = "--response" ] || [ "${1:-}" = "--reply" ] \
  || [ "${1:-}" = "--parse" ] || [ "${1:-}" = "--job" ]; then
  if [ "${1:-}" = "--write" ]; then
    selected='tests::compute_write_copy tests::compute_write_refused
      tests::compute_read_probe tests::kv_write_sharing tests::kv_read_memory
      call_step::tests::resume_once'
    scope=write
  elif [ "${1:-}" = "--memory" ]; then
    selected='call_step::tests::retained_session
      tests::compute_input_owner tests::compute_input_refused
      tests::compute_input_shapes tests::compute_input_copy
      tests::storage_input_copy tests::public_input_copy
      tests::compute_memory_race tests::compute_memory_images tests::compute_memory_close
      tests::compute_memory_publish tests::compute_build_repeat
      tests::compute_build_late tests::compute_cache_capture'
    scope=memory
  elif [ "${1:-}" = "--job" ]; then
    selected='tests::job_order tests::job_layout tests::job_pages tests::job_request tests::job_response
      tests::job_numeric tests::job_storage
      tests::job_session tests::job_tensor reply::tests::job_reply
      tests::compute_input_shapes
      tests::request_input_modes tests::request_input_values tests::request_input_ffi
      tests::guest_pages_growth tests::tensor_result_quota tests::response_modes
      tests::response_ffi tests::session_shared tests::tensor_cache_calls
      call_step::tests::resume_once tests::top1_executor'
    scope=job
  elif [ "${1:-}" = "--session" ]; then
    selected='tests::session_shared tests::session_quota tests::session_lifetime tests::session_keys
      tests::session_isolation tests::session_parallel tests::session_errors tests::session_start
      tests::session_read_memory tests::session_read_results tests::session_read_snapshot
      tests::inspect_sessions tests::inspect_update_start tests::inspect_controls
      tests::compute_session_cleanup tests::compute_session_expiry tests::top1_executor'
    scope=session
  elif [ "${1:-}" = "--tensor" ]; then
    selected='tests::tensor_io_refusal tests::tensor_io_copy tests::tensor_io_bits
      tests::tensor_io_errors tests::tensor_io_shapes'
    scope=tensor
  elif [ "${1:-}" = "--load" ]; then
    selected='tests::load_memory tests::load_values tests::load_errors'
    scope=load
  elif [ "${1:-}" = "--pages" ]; then
    selected='tests::guest_limit tests::guest_pages_ffi tests::guest_pages_quota tests::guest_pages_growth
      tests::guest_pages_lifetime tests::guest_pages_modes tests::guest_pages_parallel
      tests::top1_executor'
    scope=pages
  elif [ "${1:-}" = "--numeric" ]; then
    selected='tests::tensor_input_quota tests::tensor_input_failure
      tests::tensor_input_lifetime tests::tensor_input_fp32
      tests::tensor_io_copy tests::tensor_io_bits
      tests::tensor_io_errors tests::tensor_io_shapes tests::linear_bits
      tests::linear_values tests::top1_executor'
    scope=numeric
  elif [ "${1:-}" = "--storage" ]; then
    selected='tests::storage_manifest_quota tests::storage_value_quota tests::storage_key_quota
      tests::storage_cache_modes tests::storage_decode_errors tests::storage_copy_quota
      tests::storage_alloc_failure tests::storage_lifetime tests::storage_runtime_modes
      tests::storage_ffi_modes
      tests::storage_write_quota tests::storage_write_modes
      tests::storage_write_values tests::storage_write_alloc
      tests::storage_copy_failure tests::storage_write_free tests::storage_write_compute
      tests::compute_key_quota tests::compute_key_owner tests::compute_key_lifetime
      tests::compute_memory_race tests::compute_memory_images tests::compute_memory_close
      tests::compute_memory_publish
      tests::storage_absent_delete tests::storage_expiry_compute
      tests::storage_input_copy tests::public_input_copy tests::compute_write_copy
      tests::kv_write_sharing tests::top1_executor'
    scope=storage
  elif [ "${1:-}" = "--response" ]; then
    selected='tests::response_quota tests::response_growth tests::response_owner
      tests::response_alloc tests::response_modes tests::response_errors
      tests::response_ffi tests::top1_executor'
    scope=response
  elif [ "${1:-}" = "--parse" ]; then
    selected='tests::parse_quota tests::parse_lifetime tests::parse_errors tests::parse_vectors
      tests::parse_diagnostics
      parse_memory::tests::quoted_slots parse_memory::tests::checked_sizes
      tests::compute_input_copy tests::compute_input_shapes
      tests::compute_memory_publish tests::compute_memory_images tests::compute_memory_close
      tests::compute_memory_race tests::compute_build_repeat tests::compute_build_late
      tests::top1_executor'
    scope=parse
  elif [ "${1:-}" = "--reply" ]; then
    selected='tests::reply_lifetime tests::reply_bytes tests::reply_modes tests::reply_describe
      reply::tests::quota reply::tests::allocation reply::tests::copy_quota
      reply::tests::encoding reply::tests::rows reply::tests::changed_size
      reply::tests::lifetime reply::tests::large_encoding
      tests::response_ffi tests::guest_pages_ffi tests::request_input_ffi
      tests::top1_executor'
    scope=reply
  elif [ "${1:-}" = "--result" ]; then
    selected='tests::tensor_result_quota tests::tensor_result_lifetime
      tests::attention_result_quota tests::q24_linear_memory
      tests::q24_linear_errors tests::attention_memory tests::attention_errors
      deterministic_tensor::tests::owned_results_match
      deterministic_tensor::tests::output_shape_checked
      tests::top1_executor'
    scope=result
  elif [ "${1:-}" = "--matrix" ]; then
    selected='tests::matrix_memory tests::matrix_refusal tests::matrix_bits
      tests::matrix_values tests::matrix_errors'
    scope=matrix
  elif [ "${1:-}" = "--linear" ]; then
    selected='tests::linear_memory tests::linear_refusal tests::linear_bits
      tests::linear_values tests::linear_errors tests::linear_nan'
    scope=linear
  elif [ "${1:-}" = "--top1" ]; then
    selected='tests::top1_refusal tests::top1_values tests::top1_policies
      tests::top1_description tests::top1_errors tests::top1_executor'
    scope=top1
  elif [ "${1:-}" = "--tensor-cache" ]; then
    selected='tests::tensor_cache_quota tests::tensor_cache_lifetime
      tests::tensor_cache_alloc tests::tensor_cache_count tests::tensor_cache_standard
      tests::tensor_cache_expiry tests::tensor_cache_start
      tests::tensor_cache_isolation tests::tensor_cache_calls
      tests::compute_memory_images tests::compute_input_owner tests::top1_executor'
    scope=tensor-cache
  elif [ "${1:-}" = "--attention" ]; then
    selected='tests::attention_memory tests::attention_errors
      deterministic_tensor::tests::attention_values
      deterministic_tensor::tests::attention_errors
      deterministic_tensor::tests::rope_attention_outputs tests::top1_executor'
    scope=attention
  elif [ "${1:-}" = "--q24" ]; then
    selected='tests::q24_linear_memory tests::q24_linear_errors tests::q24_linear_alias
      deterministic_tensor::tests::linear_tiles deterministic_tensor::tests::linear_error_order
      deterministic_tensor::tests::parallel_linear_top1
      deterministic_tensor::tests::thread_partitions_are_identical'
    scope=q24
  else
    selected='tests::guest_read_memory tests::guest_read_values tests::guest_read_errors
      tests::guest_read_modes tests::tensor_io_refusal tests::tensor_io_shapes
      tests::invoke_request_memory'
    scope=read
  fi
  mode=
  target="$ROOT/runtime_data/circle_bytes_cargo"
  if [ "$scope" = linear ] || [ "$scope" = read ] || [ "$scope" = q24 ] \
    || [ "$scope" = top1 ] || [ "$scope" = attention ] \
    || [ "$scope" = tensor-cache ] || [ "$scope" = load ] || [ "$scope" = pages ] \
    || [ "$scope" = numeric ] || [ "$scope" = result ] || [ "$scope" = storage ] \
    || [ "$scope" = response ] || [ "$scope" = reply ] || [ "$scope" = parse ] \
    || [ "$scope" = session ]; then
    mode=--release
    target="$ROOT/_cargo_target/circle_wasm_native"
  fi
  tests=$(cargo test --locked --offline $mode \
    --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
    --target-dir "$target" -- --list)
  for name in $selected
  do
    if ! printf '%s\n' "$tests" | grep -Fqx "$name: test"; then
      printf 'status = fail test = %s reason = missing\n' "$name" >&2
      exit 1
    fi
    cargo test --locked --offline $mode \
      --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
      --target-dir "$target" "$name" -- --exact --test-threads=1
  done
  if [ "$scope" = top1 ]; then
    BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
    opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
      test/test_compute_engine.exe
    opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_engine.exe"
  fi
  if [ "$scope" = reply ] || [ "$scope" = parse ]; then
    BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
    opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
      test/test_build_input.exe
    opam exec -- "$ROOT/$BUILD_DIR/default/test/test_build_input.exe"
  fi
  if [ "$scope" = parse ]; then
    opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
      test/test_compute_engine.exe test/test_consensus_profile_golden.exe
    opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_engine.exe" --cache
    opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  fi
  if [ "$scope" = session ] || [ "$scope" = job ]; then
    BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
    opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
      test/test_compute_engine.exe test/test_consensus_profile_golden.exe
    opam exec -- "$ROOT/$BUILD_DIR/default/test/test_compute_engine.exe" --cache
    opam exec -- "$ROOT/$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
  fi
  sh "$ROOT/controls/proof_gate.sh" formal/coq/byte_work.v
  printf 'event = gate name = circle_bytes scope = %s status = passed\n' "$scope"
  exit 0
fi
cargo test --locked --offline \
  --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
  --target-dir "$ROOT/runtime_data/circle_bytes_cargo" -- --test-threads=1
printf 'event = gate name = circle_bytes status = passed\n'