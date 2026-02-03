/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_global_literals.h"
#include "erl_mmap.h"


#define GLOBAL_LITERAL_INITIAL_SIZE (1<<16)
#define GLOBAL_LITERAL_EXPAND_SIZE 512


/*
 * Global Constant Literals
 */
Eterm ERTS_WRITE_UNLIKELY(ERTS_GLOBAL_LIT_OS_TYPE);
Eterm ERTS_WRITE_UNLIKELY(ERTS_GLOBAL_LIT_OS_VERSION);
Eterm ERTS_WRITE_UNLIKELY(ERTS_GLOBAL_LIT_DFLAGS_RECORD);
Eterm ERTS_WRITE_UNLIKELY(ERTS_GLOBAL_LIT_ERL_FILE_SUFFIX);
Eterm ERTS_WRITE_UNLIKELY(ERTS_GLOBAL_LIT_EMPTY_TUPLE);

/* This lock is taken in the beginning of erts_global_literal_allocate,
 * released at the end of erts_global_literal_register. It protects the 
 * allocated literal chunk, and the heap pointer from concurrent access until 
 * the literal tag is set.
 */
erts_mtx_t global_literal_lock;

/* Bump allocator for global literal chunks, allocating them in
 * reasonably large chunks to simplify crash dumping and avoid fragmenting the
 * literal heap too much.
 *
 * This is protected by the global literal lock. */
struct global_literal_chunk {
    struct global_literal_chunk *next;
    Eterm *chunk_end;

    ErtsLiteralArea area;
} *global_literal_chunk = NULL;

/* The size of the global literal term that is being built */
Uint global_literal_build_size;


ErtsLiteralArea *erts_global_literal_iterate_area(ErtsLiteralArea *prev)
{
    struct global_literal_chunk *next;

    ASSERT(ERTS_IS_CRASH_DUMPING);

    if (prev != NULL) {
        struct global_literal_chunk *chunk = ErtsContainerStruct(prev,
                                                    struct global_literal_chunk,
                                                    area);
        next = chunk->next;

        if (next == NULL) {
            return NULL;
        }
    } else {
        next = global_literal_chunk;
    }

    return &next->area;
}

static void expand_shared_global_literal_area(Uint heap_size)
{
    const size_t size = (offsetof(struct global_literal_chunk, area)
                         + ERTS_LITERAL_AREA_ALLOC_SIZE(heap_size));
    struct global_literal_chunk *chunk;
                        
#ifndef DEBUG 
    chunk = (struct global_literal_chunk *) erts_alloc(ERTS_ALC_T_LITERAL, size); 
#else
    /* erts_mem_guard requires the memory area to be page aligned. Overallocate
     * and align the address to ensure that is the case. */
    UWord address;
    address = (UWord) erts_alloc(ERTS_ALC_T_LITERAL, size + sys_page_size * 2);
    address = (address + (sys_page_size - 1)) & ~(sys_page_size - 1);
    chunk = (struct global_literal_chunk *) address;

    for (int i = 0; i < heap_size; i++) {
        chunk->area.start[i] = ERTS_HOLE_MARKER;
    }
#endif

    chunk->area.end = &(chunk->area.start[0]);
    chunk->chunk_end = &(chunk->area.start[heap_size]);
    chunk->area.off_heap = NULL;
    chunk->next = global_literal_chunk;

    global_literal_chunk = chunk;
}

Eterm *erts_global_literal_allocate(Uint heap_size, struct erl_off_heap_header ***ohp)
{
    erts_mtx_lock(&global_literal_lock);

    ASSERT(global_literal_chunk->area.end <= global_literal_chunk->chunk_end &&
           global_literal_chunk->area.end >= global_literal_chunk->area.start);
    if (global_literal_chunk->chunk_end - global_literal_chunk->area.end < heap_size) {
        expand_shared_global_literal_area(heap_size + GLOBAL_LITERAL_EXPAND_SIZE);
    }

    *ohp = &global_literal_chunk->area.off_heap;

#ifdef DEBUG
    {
        struct global_literal_chunk *chunk = global_literal_chunk;
        erts_mem_guard(chunk,
                       (byte*)(chunk->area.end + heap_size) - (byte*)chunk,
                       1, 
                       1);
    }
#endif

    global_literal_build_size = heap_size;

    return global_literal_chunk->area.end;
}

void erts_global_literal_register(Eterm *variable) {
    struct global_literal_chunk *chunk = global_literal_chunk;

    ASSERT(ptr_val(*variable) >= chunk->area.end &&
           ptr_val(*variable) < (chunk->area.end + global_literal_build_size));

    erts_set_literal_tag(variable,
                         chunk->area.end,
                         global_literal_build_size);
    
    chunk->area.end += global_literal_build_size;

    ASSERT(chunk->area.end <= chunk->chunk_end &&
           chunk->area.end >= chunk->area.start);
    ASSERT(chunk->area.end == chunk->chunk_end ||
           chunk->area.end[0] == ERTS_HOLE_MARKER);

#ifdef DEBUG
    erts_mem_guard(chunk,
                   (byte*)chunk->chunk_end - (byte*)chunk,
                   1,
                   0);
#endif

    erts_mtx_unlock(&global_literal_lock);
}

static void init_empty_tuple(void) {
    struct erl_off_heap_header **ohp;
    Eterm* hp = erts_global_literal_allocate(2, &ohp);
    Eterm tuple;
    hp[0] = make_arityval_zero();
    hp[1] = make_arityval_zero();
    tuple = make_tuple(hp);
    erts_global_literal_register(&tuple);
    ERTS_GLOBAL_LIT_EMPTY_TUPLE = tuple;
}

void
init_global_literals(void)
{
    erts_mtx_init(&global_literal_lock, "global_literals", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    
    expand_shared_global_literal_area(GLOBAL_LITERAL_INITIAL_SIZE);
    init_empty_tuple();
}