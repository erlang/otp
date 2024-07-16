/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2024. All Rights Reserved.
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
    Eterm *hp;

    ErtsLiteralArea area;
} *global_literal_chunk = NULL;



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

    next->area.end = next->hp;
    return &next->area;
}

static void expand_shared_global_literal_area(Uint heap_size)
{
    const size_t size = sizeof(struct global_literal_chunk) +
                        (heap_size - 1) * sizeof(Eterm);
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
#endif

    chunk->hp = &chunk->area.start[0];
    chunk->area.end = &chunk->hp[heap_size];
    chunk->area.off_heap = NULL;
    chunk->next = global_literal_chunk;

    global_literal_chunk = chunk;
}

Eterm *erts_global_literal_allocate(Uint heap_size, struct erl_off_heap_header ***ohp)
{
    Eterm *hp;

    erts_mtx_lock(&global_literal_lock);

    ASSERT((global_literal_chunk->hp <= global_literal_chunk->area.end &&
            global_literal_chunk->hp >= global_literal_chunk->area.start) );
    if (global_literal_chunk->area.end - global_literal_chunk->hp <= heap_size) {
        expand_shared_global_literal_area(heap_size + GLOBAL_LITERAL_EXPAND_SIZE);
    }

    *ohp = &global_literal_chunk->area.off_heap;
    hp = global_literal_chunk->hp;
    global_literal_chunk->hp += heap_size;

#ifdef DEBUG
    {
        struct global_literal_chunk *chunk = global_literal_chunk;
        erts_mem_guard(&chunk->area.start[0], 
                       (chunk->area.end - &chunk->area.start[0]) * sizeof(Eterm), 
                       1, 
                       1);
    }
#endif

    return hp;
}

void erts_global_literal_register(Eterm *variable, Eterm *hp, Uint heap_size) {
    erts_set_literal_tag(variable, hp, heap_size);
    
#ifdef DEBUG
    {
        struct global_literal_chunk *chunk = global_literal_chunk;
        erts_mem_guard(&chunk->area.start[0], 
                       (chunk->area.end - &chunk->area.start[0]) * sizeof(Eterm), 
                       1, 
                       0);
    }
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
    erts_global_literal_register(&tuple, hp, 2);
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