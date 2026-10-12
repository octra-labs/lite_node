// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/custom.h>
#include <caml/signals.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern int octra_circle_wasm_host_run_json(
  const uint8_t* input_ptr,
  size_t input_len,
  uint8_t** out_ptr,
  size_t* out_len,
  uint8_t** err_ptr,
  size_t* err_len,
  void** owner
);

extern void octra_circle_wasm_host_free_bytes(uint8_t* ptr, size_t len);
extern void* octra_compute_lease_new(void);
extern void octra_compute_lease_drop(void* port);
extern int octra_compute_lease_hold(void* port, const uint8_t* input, size_t length);
extern void* octra_compute_build_new(void);
extern void octra_circle_call_drop(void* port);
extern void* octra_compute_build_retain(void* port);
extern void octra_compute_build_release(void* port);
extern void octra_compute_build_close(void* port);
extern int octra_compute_build_begin(void* port, const uint8_t* input, size_t length);
extern int octra_compute_input_new(
  const uint8_t* input, size_t length, void** owner, const uint8_t** bytes
);
extern void octra_compute_input_drop(void* owner);
extern int octra_circle_input_take(size_t size, void** owner);
extern void octra_circle_input_drop(void* owner);
extern int octra_compute_build_step(
  void* port, const uint8_t* input, size_t length, uint8_t** output, size_t* size
);

static void build_release(value build) {
  void** port = (void**)Data_custom_val(build);
  octra_compute_build_close(*port);
  octra_compute_build_release(*port);
  *port = NULL;
}

static struct custom_operations build_ops = {
  "octra.compute.build",
  build_release,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_octra_compute_build_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(build);
  build = caml_alloc_custom(&build_ops, sizeof(void*), 0, 1);
  *((void**)Data_custom_val(build)) = NULL;
  *((void**)Data_custom_val(build)) = octra_compute_build_new();
  CAMLreturn(build);
}

CAMLprim value caml_octra_compute_build_begin(value build, value key) {
  CAMLparam2(build, key);
  int code = octra_compute_build_begin(
    *((void**)Data_custom_val(build)), (const uint8_t*)String_val(key),
    caml_string_length(key)
  );
  CAMLreturn(Val_int(code));
}

CAMLprim value caml_octra_compute_build_close(value build) {
  CAMLparam1(build);
  build_release(build);
  CAMLreturn(Val_unit);
}

typedef struct {
  uint8_t* bytes;
  size_t size;
  uint8_t* error;
  size_t error_size;
  uint8_t* copied;
  void* memory;
  void* input;
  void* port;
  void* call;
  void* reply;
} native_reply;

static void native_reply_release(value output) {
  native_reply* result = (native_reply*)Data_custom_val(output);
  octra_compute_input_drop(result->input);
  octra_compute_build_release(result->port);
  octra_circle_call_drop(result->call);
  free(result->copied);
  octra_circle_input_drop(result->memory);
  if (result->reply != NULL) {
    octra_compute_input_drop(result->reply);
  } else {
    octra_circle_wasm_host_free_bytes(result->bytes, result->size);
  }
  octra_circle_wasm_host_free_bytes(result->error, result->error_size);
  *result = (native_reply){0};
}

static struct custom_operations native_reply_ops = {
  "octra.compute.output",
  native_reply_release,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value native_reply_new(void) {
  CAMLparam0();
  CAMLlocal1(output);
  output = caml_alloc_custom(&native_reply_ops, sizeof(native_reply), 0, 1);
  *((native_reply*)Data_custom_val(output)) = (native_reply){0};
  CAMLreturn(output);
}

static void lease_release(value lease) {
  void** port = (void**)Data_custom_val(lease);
  octra_compute_lease_drop(*port);
  *port = NULL;
}

static struct custom_operations lease_ops = {
  "octra.compute.lease",
  lease_release,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_octra_compute_lease_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(lease);
  lease = caml_alloc_custom(&lease_ops, sizeof(void*), 0, 1);
  *((void**)Data_custom_val(lease)) = NULL;
  *((void**)Data_custom_val(lease)) = octra_compute_lease_new();
  CAMLreturn(lease);
}

CAMLprim value caml_octra_compute_lease_hold(value lease, value input) {
  CAMLparam2(lease, input);
  int code = octra_compute_lease_hold(
    *((void**)Data_custom_val(lease)), (const uint8_t*)String_val(input),
    caml_string_length(input)
  );
  CAMLreturn(Val_int(code));
}

CAMLprim value caml_octra_compute_lease_close(value lease) {
  CAMLparam1(lease);
  lease_release(lease);
  CAMLreturn(Val_unit);
}

extern void* octra_circle_call_new(void);
extern void* octra_circle_call_retain(void* port);
extern int octra_circle_call_valid(const void* port, const uint8_t* input, size_t length);
extern int octra_circle_call_step(
  void* port,
  const uint8_t* input,
  size_t length,
  uint8_t** output,
  size_t* size
);

static void call_release(value session) {
  void** port = (void**)Data_custom_val(session);
  octra_circle_call_drop(*port);
  *port = NULL;
}

static struct custom_operations call_ops = {
  "octra.circle.call",
  call_release,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_octra_circle_call_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(session);
  session = caml_alloc_custom(&call_ops, sizeof(void*), 0, 1);
  *((void**)Data_custom_val(session)) = octra_circle_call_new();
  CAMLreturn(session);
}

CAMLprim value caml_octra_circle_call_close(value session) {
  CAMLparam1(session);
  call_release(session);
  CAMLreturn(Val_unit);
}

static void copy_to_malloc_string(value v_string, uint8_t** out_ptr, size_t* out_len) {
  size_t len = caml_string_length(v_string);
  uint8_t* buf = NULL;
  if (len > 0) {
    buf = malloc(len);
    if (buf == NULL) {
      caml_failwith("octra_circle_wasm_host: alloc failed");
    }
    memcpy(buf, String_val(v_string), len);
  }
  if (out_ptr != NULL) {
    *out_ptr = buf;
  }
  if (out_len != NULL) {
    *out_len = len;
  }
}

CAMLprim value caml_octra_compute_build_step(value build, value input) {
  CAMLparam2(build, input);
  CAMLlocal3(body, result, owned);
  result = caml_alloc_tuple(2);
  owned = native_reply_new();
  native_reply* saved = (native_reply*)Data_custom_val(owned);
  const uint8_t* bytes = NULL;
  size_t length = caml_string_length(input);
  uint8_t* output = NULL;
  size_t size = 0;
  int status = octra_compute_input_new(
    (const uint8_t*)String_val(input), length, &saved->input, &bytes
  );
  if (status != 0) {
    if (status == 4) {
      caml_failwith("octra_circle_wasm_host: alloc failed");
    }
    const char* reason = status == 1 ? "compute storage input refused"
      : status == 2 ? "compute memory busy" : "compute memory owner failed";
    body = caml_copy_string(reason);
    Store_field(result, 0, Val_int(status == 3 ? 2 : 1));
    Store_field(result, 1, body);
    CAMLreturn(result);
  }
  saved->port = octra_compute_build_retain(*((void**)Data_custom_val(build)));
  void* port = saved->port;
  caml_release_runtime_system();
  int code = octra_compute_build_step(port, bytes, length, &output, &size);
  caml_acquire_runtime_system();
  saved = (native_reply*)Data_custom_val(owned);
  saved->bytes = output;
  saved->size = size;
  octra_compute_input_drop(saved->input);
  octra_compute_build_release(saved->port);
  saved->input = NULL;
  saved->port = NULL;
  body = caml_alloc_string(size);
  if (size > 0 && output != NULL) {
    memcpy(Bytes_val(body), output, size);
  }
  native_reply_release(owned);
  Store_field(result, 0, Val_int(code));
  Store_field(result, 1, body);
  CAMLreturn(result);
}

CAMLprim value caml_octra_circle_call_step(value session, value input) {
  CAMLparam2(session, input);
  CAMLlocal3(body, result, owned);
  result = caml_alloc_tuple(2);
  owned = native_reply_new();
  native_reply* saved = (native_reply*)Data_custom_val(owned);
  size_t length = caml_string_length(input);
  uint8_t* output = NULL;
  size_t size = 0;
  saved->call = octra_circle_call_retain(*((void**)Data_custom_val(session)));
  void* port = saved->call;
  const uint8_t* input_bytes = length == 0 ? NULL : (const uint8_t*)String_val(input);
  int code;
  if (octra_circle_call_valid(port, input_bytes, length)) {
    copy_to_malloc_string(input, &saved->copied, &length);
    uint8_t* bytes = saved->copied;
    caml_release_runtime_system();
    code = octra_circle_call_step(port, bytes, length, &output, &size);
    caml_acquire_runtime_system();
  } else {
    code = octra_circle_call_step(port, NULL, length, &output, &size);
  }
  saved = (native_reply*)Data_custom_val(owned);
  saved->bytes = output;
  saved->size = size;
  free(saved->copied);
  saved->copied = NULL;
  body = caml_alloc_string(size);
  if (size > 0 && output != NULL) {
    memcpy(Bytes_val(body), output, size);
  }
  native_reply_release(owned);
  Store_field(result, 0, Val_int(code));
  Store_field(result, 1, body);
  CAMLreturn(result);
}

CAMLprim value caml_octra_circle_wasm_host_run_json(value optional, value v_input) {
  CAMLparam2(optional, v_input);
  CAMLlocal3(v_payload, v_result, owned);
  v_result = caml_alloc_tuple(2);
  owned = native_reply_new();
  native_reply* saved = (native_reply*)Data_custom_val(owned);

  size_t input_len = 0;
  uint8_t* out_ptr = NULL;
  size_t out_len = 0;
  uint8_t* err_ptr = NULL;
  size_t err_len = 0;
  void* reply = NULL;

  if (Bool_val(optional)) {
    int status = octra_circle_input_take(caml_string_length(v_input), &saved->memory);
    if (status != 0) {
      if (status == 4) caml_raise_out_of_memory();
      v_payload = caml_copy_string(status == 2
        ? "native input busy" : "native input owner failed");
      Store_field(v_result, 0, Val_int(2));
      Store_field(v_result, 1, v_payload);
      CAMLreturn(v_result);
    }
  }
  copy_to_malloc_string(v_input, &saved->copied, &input_len);
  if (saved->copied == NULL) {
    saved->copied = malloc(1);
    if (saved->copied == NULL) {
      caml_failwith("octra_circle_wasm_host: alloc failed");
    }
  }
  uint8_t* input_ptr = saved->copied;
  caml_release_runtime_system();
  int rc =
    octra_circle_wasm_host_run_json(
      input_ptr,
      input_len,
      &out_ptr,
      &out_len,
      &err_ptr,
      &err_len,
      &reply
    );
  caml_acquire_runtime_system();
  saved = (native_reply*)Data_custom_val(owned);
  saved->bytes = out_ptr;
  saved->size = out_len;
  saved->error = err_ptr;
  saved->error_size = err_len;
  saved->reply = reply;
  free(saved->copied);
  saved->copied = NULL;
  octra_circle_input_drop(saved->memory);
  saved->memory = NULL;

  if (rc == 0) {
    v_payload = caml_alloc_string(out_len);
    if (out_len > 0 && out_ptr != NULL) {
      memcpy(Bytes_val(v_payload), out_ptr, out_len);
    }
  } else {
    v_payload = caml_alloc_string(err_len);
    if (err_len > 0 && err_ptr != NULL) {
      memcpy(Bytes_val(v_payload), err_ptr, err_len);
    }
  }

  native_reply_release(owned);
  Store_field(v_result, 0, Val_int(rc));
  Store_field(v_result, 1, v_payload);
  CAMLreturn(v_result);
}

typedef struct {
  const uint8_t* input;
  size_t length;
  uint8_t* output;
  size_t output_size;
  uint8_t* error;
  size_t error_size;
  int code;
} hfhe_scope;

static _Thread_local hfhe_scope* active_hfhe = NULL;

static int hfhe_call_json_locked(
  const uint8_t* input_ptr,
  size_t input_len,
  uint8_t** out_ptr,
  size_t* out_len,
  uint8_t** err_ptr,
  size_t* err_len
) {
  static const value* registered_cb = NULL;
  CAMLparam0();
  CAMLlocal2(v_input, v_output);

  if (registered_cb == NULL) {
    registered_cb = caml_named_value("octra_circle_wasm_host_hfhe_call_json");
  }
  if (registered_cb == NULL) {
    const char* message = "octra_circle_wasm_host: hfhe callback not registered";
    size_t len = strlen(message);
    uint8_t* buf = malloc(len);
    if (buf != NULL) {
      memcpy(buf, message, len);
    }
    if (err_ptr != NULL) {
      *err_ptr = buf;
    }
    if (err_len != NULL) {
      *err_len = len;
    }
    if (out_ptr != NULL) {
      *out_ptr = NULL;
    }
    if (out_len != NULL) {
      *out_len = 0;
    }
    CAMLreturnT(int, 1);
  }

  v_input = caml_alloc_string(input_len);
  if (input_len > 0 && input_ptr != NULL) {
    memcpy(Bytes_val(v_input), input_ptr, input_len);
  }

  v_output = caml_callback_exn(*registered_cb, v_input);
  if (Is_exception_result(v_output)) {
    const char* message = "octra_circle_wasm_host: hfhe callback exception";
    size_t len = strlen(message);
    uint8_t* buf = malloc(len);
    if (buf != NULL) {
      memcpy(buf, message, len);
    }
    if (err_ptr != NULL) {
      *err_ptr = buf;
    }
    if (err_len != NULL) {
      *err_len = len;
    }
    if (out_ptr != NULL) {
      *out_ptr = NULL;
    }
    if (out_len != NULL) {
      *out_len = 0;
    }
    CAMLreturnT(int, 1);
  }

  copy_to_malloc_string(v_output, out_ptr, out_len);
  if (err_ptr != NULL) {
    *err_ptr = NULL;
  }
  if (err_len != NULL) {
    *err_len = 0;
  }
  CAMLreturnT(int, 0);
}

CAMLprim value caml_octra_circle_hfhe_bridge(value unit) {
  CAMLparam1(unit);
  if (active_hfhe == NULL) {
    caml_failwith("hfhe callback context missing");
  }
  active_hfhe->code = hfhe_call_json_locked(
    active_hfhe->input, active_hfhe->length,
    &active_hfhe->output, &active_hfhe->output_size,
    &active_hfhe->error, &active_hfhe->error_size
  );
  CAMLreturn(Val_unit);
}

int octra_circle_wasm_host_hfhe_call_json(
  const uint8_t* input_ptr,
  size_t input_len,
  uint8_t** out_ptr,
  size_t* out_len,
  uint8_t** err_ptr,
  size_t* err_len
) {
  caml_acquire_runtime_system();
  hfhe_scope scope = {.input = input_ptr, .length = input_len, .code = 1};
  hfhe_scope* previous = active_hfhe;
  active_hfhe = &scope;
  const value* bridge = caml_named_value("octra_circle_wasm_host_hfhe_bridge");
  const char* failure = "octra_circle_wasm_host: hfhe callback not registered";
  if (bridge != NULL) {
    value result = caml_callback_exn(*bridge, Val_unit);
    if (Is_exception_result(result)) {
      failure = "octra_circle_wasm_host: hfhe callback exception";
    } else {
      failure = NULL;
    }
  }
  active_hfhe = previous;
  if (failure != NULL) {
    free(scope.output);
    free(scope.error);
    scope.output = NULL;
    scope.output_size = 0;
    scope.error_size = strlen(failure);
    scope.error = malloc(scope.error_size);
    if (scope.error != NULL) memcpy(scope.error, failure, scope.error_size);
    scope.code = 1;
  }
  if (out_ptr != NULL) *out_ptr = scope.output;
  else free(scope.output);
  if (out_len != NULL) *out_len = scope.output_size;
  if (err_ptr != NULL) *err_ptr = scope.error;
  else free(scope.error);
  if (err_len != NULL) *err_len = scope.error_size;
  caml_enter_blocking_section_no_pending();
  return scope.code;
}