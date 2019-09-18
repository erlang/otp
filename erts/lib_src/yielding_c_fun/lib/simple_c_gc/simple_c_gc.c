/*
 * %CopyrightBegin%
 *
 * Copyright 2019 Kjell Winblad (kjellwinblad@gmail.com, http://winsh.me).
 * All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/*
 *
 * Author: Kjell Winblad
 *
 */

#include "simple_c_gc.h"
#include "chained_hash_set.h"

#include <setjmp.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static bool scgc_print_gc_info = false;
static ChainedHashSet *scgc_objects;
static void *scgc_stack_top;

#define SCGC_MIN_ALLOCS_UNTIL_FREE 100
static int scgc_allocs_until_gc = SCGC_MIN_ALLOCS_UNTIL_FREE;
static void *(*scgc_user_malloc)(size_t size);
static void (*scgc_user_free)(void *ptr);

typedef enum { scgc_blank_state, scgc_marked } scgc_object_state;

typedef struct {
  unsigned long magic_number;
  size_t size;
  scgc_object_state state;
  void *data[1];
} scgc_object;

typedef struct {
  void *address;
  scgc_object *base;
  unsigned long magic_number;
} scgc_object_ref;

static void *scgc_malloc(size_t size) {
  void *(*my_malloc)(size_t size);
  if (scgc_user_malloc == NULL) {
    my_malloc = malloc;
  } else {
    my_malloc = scgc_user_malloc;
  }
  void *res = my_malloc(size);
  if (res == NULL) {
    printf("GCGC: Allocator returned NULL.\n");
    exit(1);
  }
  return res;
}

static void scgc_free(void *ptr) {
  void (*my_free)(void *ptr);
  if (scgc_user_free == NULL) {
    my_free = free;
  } else {
    my_free = scgc_user_free;
  }
  my_free(ptr);
}

static void *scgc_extract_key(void *v, int keyPos) {
  (void)keyPos;
  return v;
}

static unsigned int scgc_hash_key(void *keyPtr) {
  /* From
   * https://lemire.me/blog/2018/08/15/fast-strongly-universal-64-bit-hashing-everywhere*/
  int64_t x = (long)*((void **)keyPtr);
  int64_t a = 2348923;
  int64_t b = 3292;
  int64_t c = 9893487421;
  int32_t low = (int)x;
  int32_t high = (int)(x >> 32);
  return (unsigned int)((a * low + b * high + c) >> 32);
}

static bool scgc_are_equal(void *v1p, void *v2p) {
  scgc_object_ref *v1 = ((scgc_object_ref *)v1p);
  scgc_object_ref *v2 = ((scgc_object_ref *)v2p);
  return v1->address == v2->address;
}

static char *scgc_to_string(void *vp) {
  scgc_object_ref *v = ((scgc_object_ref *)vp);
  char *str = scgc_malloc(200);
  sprintf(str, "{.address=%p, .base_address=%p, magic_number=%lu}", v->address,
          (void *)v->base, v->magic_number);
  return str;
}

static void scgc_initialize_global_state() {
  scgc_objects =
      ch_set_create(0, scgc_extract_key, scgc_hash_key, scgc_are_equal,
                    scgc_to_string, scgc_malloc, scgc_free);
  srand((int)(long)scgc_objects * 2654435761);
}

static void scgc_do_gc(bool no_stack);

static void scgc_destroy_global_state() {
  scgc_do_gc(true);
  ch_set_free(scgc_objects);
}

static int scgc_start_gced_code_2(int (*main)(int, char *[]), int argc,
                                  char **argv[]) {
  volatile int noinline = 1;
  volatile char **my_argv = (volatile char **)*argv;
  scgc_stack_top = (void *)my_argv;
  int (*next)(int, char *[]) = noinline ? main : (int (*)(int, char *[]))(NULL);
  {
    int to_return;
    scgc_initialize_global_state();
    to_return = next(argc, (char **)my_argv);
    scgc_destroy_global_state();
    return to_return;
  }
}

static void scgc_global_set_put(scgc_object_ref ref) {
  ch_set_insert(scgc_objects, &ref, sizeof(scgc_object_ref));
}

static void scgc_global_set_del(void *key) {
  ch_set_delete(scgc_objects, &key, sizeof(void *));
}

static scgc_object_ref *scgc_global_set_get(void *key) {
  scgc_object_ref *ret = ch_set_lookup(scgc_objects, &key);
  return ret;
}

static void scgc_mark_reachable_objects_in_region(void *start, void *end);

static void *scgc_min(void *a, void *b) { return a <= b ? a : b; }

static void *scgc_max(void *a, void *b) { return a > b ? a : b; }

static void ycf_find_stack_bottom_and_mark_conservative_helper(void) {
  volatile void *p = NULL;
  volatile intptr_t stack_bottom = (intptr_t)&p;
  scgc_mark_reachable_objects_in_region(
      scgc_min(scgc_stack_top, (void *)&stack_bottom),
      scgc_max((void *)&stack_bottom, scgc_stack_top));
}

static void scgc_get_stack_bottom_and_mark() {
  jmp_buf env;
  setjmp(env);

  volatile int noinline = 1;

  void (*bottom)(void) =
      noinline ? ycf_find_stack_bottom_and_mark_conservative_helper
               : (void (*)(void))(NULL);

  bottom();
}

static void scgc_mark_reachable_objects_in_region(void *start, void *end) {
  void **iter = start;
  void **iter_end = end;
  scgc_object *object;
  while (iter <= iter_end) {
    scgc_object_ref *ref = scgc_global_set_get(*iter);
    if (ref != NULL && ref->base->data == *iter &&
        ref->magic_number == ref->base->magic_number &&
        ref->base->state == scgc_blank_state) {
      object = ref->base;
      object->state = scgc_marked;
      scgc_mark_reachable_objects_in_region(
          &object->data[0], &((char *)object->data)[object->size - 1]);
    }
    iter = iter + 1;
  }
}

static void scgc_mark_reachable_objects(bool no_stack) {
  void *tmp = NULL;
  if (no_stack) {
    scgc_mark_reachable_objects_in_region(&tmp, &tmp);
  } else {
    scgc_get_stack_bottom_and_mark();
  }
}

static void scgc_collect_unmarked_traverser(size_t index, void *v,
                                            void *context) {
  void **objects_to_remove = context;
  scgc_object_ref *ref = v;
  if (ref->base->state == scgc_blank_state) {
    scgc_free(ref->base);
    objects_to_remove[index] = ref->address;
  } else {
    objects_to_remove[index] = NULL;
  }
}

static void scgc_remove_unmarked_objects() {
  size_t nr_of_candidates = scgc_objects->size;
  void **objects_to_remove = scgc_malloc(sizeof(void *) * nr_of_candidates);
  for (size_t i = 0; i < nr_of_candidates; i++) {
    objects_to_remove[i] = NULL;
  }
  ch_set_traverse(scgc_objects, scgc_collect_unmarked_traverser,
                  objects_to_remove);
  for (size_t i = 0; i < nr_of_candidates; i++) {
    if (objects_to_remove[i] != NULL) {
      scgc_global_set_del(objects_to_remove[i]);
    }
  }
  scgc_free(objects_to_remove);
}

static void scgc_unmark_traverser(size_t index, void *v, void *context) {
  scgc_object_ref *ref = v;
  ref->base->state = scgc_blank_state;
}

static void scgc_unmark_objects() {
  ch_set_traverse(scgc_objects, scgc_unmark_traverser, NULL);
}

static void scgc_do_gc(bool no_stack) {
  unsigned int objects_before = scgc_objects->size;
  scgc_mark_reachable_objects(no_stack);
  scgc_remove_unmarked_objects();
  scgc_unmark_objects();
  if (scgc_print_gc_info) {
    unsigned int objects_after = scgc_objects->size;
    unsigned int objects_removed = objects_before - objects_after;
    fprintf(stderr, "GC: before=%u, after=%u, removed=%u\n", objects_before,
            objects_after, objects_removed);
  }
}

static void scgc_gc() {
  scgc_allocs_until_gc--;
  if (scgc_allocs_until_gc <= 0) {
    scgc_do_gc(false);
    unsigned int objects_after = scgc_objects->size;
    scgc_allocs_until_gc = objects_after * 2;
    if (scgc_allocs_until_gc < SCGC_MIN_ALLOCS_UNTIL_FREE) {
      scgc_allocs_until_gc = SCGC_MIN_ALLOCS_UNTIL_FREE;
    }
  }
}

/* Public interface */

int scgc_start_gced_code(int (*main)(int, char *[]), int argc, char *argv[],
                         void *(*my_malloc)(size_t), void (*my_free)(void *)) {
  volatile int noinline = 1;
  int (*next)(int (*)(int, char *[]), int, char **[]) =
      (noinline ? scgc_start_gced_code_2
                : (int (*)(int (*)(int, char *[]), int, char **[]))(NULL));
  volatile char **my_argv = (volatile char **)argv;
  int res;
  scgc_user_malloc = my_malloc;
  scgc_user_free = my_free;
  if (my_argv == NULL) {
    fprintf(stderr,
            "scgc_start_gced_code: the argv parameter should not be NULL!");
    exit(1);
  }
  res = next(main, argc, (char ***)&my_argv);
  return res;
}

void *scgc_new(size_t size) {
  scgc_gc();
  scgc_object *new = scgc_malloc(size + sizeof(scgc_object));
  scgc_object_ref new_ref;
  unsigned long magic_number = (unsigned long)rand();
  new->state = scgc_blank_state;
  new->magic_number = magic_number;
  new->size = size;
  new_ref.address = new->data;
  new_ref.base = new;
  new_ref.magic_number = magic_number;
  scgc_global_set_put(new_ref);
  return new->data;
}

void scgc_enable_print_gc_info() { scgc_print_gc_info = true; }
