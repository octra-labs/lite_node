// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct { value key; size_t offset; size_t fields; intnat saved; } type_node;
typedef struct {
  type_node *nodes;
  size_t *slots;
  size_t count;
  size_t capacity;
  size_t words;
} type_index;

static size_t type_hash(value key) {
  uint64_t n = (uint64_t)key;
  n ^= n >> 30;
  n *= UINT64_C(0xbf58476d1ce4e5b9);
  n ^= n >> 27;
  n *= UINT64_C(0x94d049bb133111eb);
  return (size_t)(n ^ (n >> 31));
}

static void type_free(type_index *index) {
  free(index->nodes);
  free(index->slots);
}

static int type_grow(type_index *index) {
  size_t capacity = index->capacity ? index->capacity * 2 : 64;
  if (capacity < index->capacity || capacity > SIZE_MAX / sizeof(type_node)
      || capacity > SIZE_MAX / (2 * sizeof(size_t))) return 0;
  type_node *nodes = realloc(index->nodes, capacity * sizeof(type_node));
  if (!nodes) return 0;
  index->nodes = nodes;
  size_t *slots = calloc(capacity * 2, sizeof(size_t));
  if (!slots) return 0;
  for (size_t i = 0; i < index->count; i++) {
    size_t slot = type_hash(nodes[i].key) & (capacity * 2 - 1);
    while (slots[slot]) slot = (slot + 1) & (capacity * 2 - 1);
    slots[slot] = i + 1;
  }
  free(index->slots);
  index->slots = slots;
  index->capacity = capacity;
  return 1;
}

static size_t type_slot(type_index *index, value key) {
  size_t slot = type_hash(key) & (index->capacity * 2 - 1);
  while (index->slots[slot]
      && index->nodes[index->slots[slot] - 1].key != key)
    slot = (slot + 1) & (index->capacity * 2 - 1);
  return slot;
}

static int type_add(type_index *index, type_index *seeds, value key) {
  if (index->count == index->capacity && !type_grow(index)) return 0;
  size_t slot = type_slot(index, key);
  if (index->slots[slot]) return 1;
  intnat saved = -1;
  if (seeds && seeds->count) {
    size_t found = seeds->slots[type_slot(seeds, key)];
    if (found) saved = seeds->nodes[found - 1].saved;
  }
  size_t fields = saved < 0 && Is_block(key) && Tag_val(key) <= 6 ? Wosize_val(key) : 0;
  if (fields > Max_wosize - 2 || index->words > Max_wosize - fields - 2)
    return 0;
  index->nodes[index->count] = (type_node){key, index->words, fields, saved};
  index->words += fields + 2;
  index->slots[slot] = ++index->count;
  return 1;
}

static int type_build(type_index *index, value root, value values) {
  type_index seeds = {0};
  for (size_t i = 0; i < Wosize_val(values); i++) {
    value key = Field(values, i);
    if (!type_add(&seeds, NULL, key)) { type_free(&seeds); return 0; }
    size_t found = seeds.slots[type_slot(&seeds, key)] - 1;
    seeds.nodes[found].saved = i;
  }
  if (!type_add(index, &seeds, root)) { type_free(&seeds); return 0; }
  for (size_t i = 0; i < index->count; i++) {
    value key = index->nodes[i].key;
    size_t fields = index->nodes[i].fields;
    for (size_t j = 0; j < fields; j++)
      if (!type_add(index, &seeds, Field(key, j))) { type_free(&seeds); return 0; }
  }
  type_free(&seeds);
  return 1;
}

CAMLprim value octra_type_index(value root, value seeds) {
  CAMLparam2(root, seeds);
  CAMLlocal1(result);
  type_index before = {0};
  if (!type_build(&before, root, seeds)) {
    type_free(&before);
    caml_raise_out_of_memory();
  }
  size_t words = before.words;
  type_free(&before);
  result = caml_alloc(words, 0);
  for (size_t i = 0; i < words; i++) Field(result, i) = Val_unit;
  type_index after = {0};
  if (!type_build(&after, root, seeds)) {
    type_free(&after);
    caml_raise_out_of_memory();
  }
  if (after.words != words) {
    type_free(&after);
    caml_failwith("type graph changed during indexing");
  }
  for (size_t i = 0; i < after.count; i++) {
    type_node node = after.nodes[i];
    caml_initialize(&Field(result, node.offset), node.key);
    Field(result, node.offset + 1) = Val_long(node.saved < 0 ? (intnat)node.fields : -1 - node.saved);
    for (size_t j = 0; j < node.fields; j++) {
      size_t child = after.slots[type_slot(&after, Field(node.key, j))] - 1;
      Field(result, node.offset + 2 + j) = Val_long(after.nodes[child].offset);
    }
  }
  type_free(&after);
  CAMLreturn(result);
}