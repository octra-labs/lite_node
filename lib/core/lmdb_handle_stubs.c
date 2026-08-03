// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static void octra_lmdb_map_check(value map)
{
  if (!Is_block(map) || Tag_val(map) != 0 || Wosize_val(map) != 5) {
    caml_invalid_argument("lmdb map representation changed");
  }
}

CAMLprim value octra_lmdb_map_env(value map)
{
  CAMLparam1(map);
  octra_lmdb_map_check(map);
  CAMLreturn(Field(map, 0));
}

CAMLprim value octra_lmdb_map_dbi(value map)
{
  CAMLparam1(map);
  octra_lmdb_map_check(map);
  CAMLreturn(Field(map, 1));
}

CAMLprim value octra_lmdb_map_invalidate(value map)
{
  CAMLparam1(map);
  octra_lmdb_map_check(map);
  Store_field(map, 1, Val_long(-1));
  CAMLreturn(Val_unit);
}