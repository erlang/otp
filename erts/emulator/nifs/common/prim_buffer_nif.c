/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson 2017. All Rights Reserved.
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

#define STATIC_ERLANG_NIF 1

#include "erl_nif.h"
#include "config.h"
#include "sys.h"

#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#endif

#define ACCUMULATOR_SIZE (2 << 10)

#define FIND_NIF_RESCHEDULE_SIZE (1 << 20)

/* NIF interface declarations */
static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv *env, void* priv_data);

static ErlNifResourceType *rtype_buffer;

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;

static ERL_NIF_TERM am_lock_order_violation;

static ERL_NIF_TERM am_acquired;
static ERL_NIF_TERM am_busy;

static ERL_NIF_TERM am_continue;

static ERL_NIF_TERM am_out_of_memory;
static ERL_NIF_TERM am_not_found;

typedef struct {
#ifdef DEBUG
    erts_atomic32_t concurrent_users;
#endif

    ErlNifBinary accumulator;
    size_t accumulated_bytes;
    int accumulator_present;

    ErlNifIOQueue *queue;

    erts_atomic32_t external_lock;
} buffer_data_t;

static ERL_NIF_TERM new_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM peek_head_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM skip_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM size_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM write_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM copying_read_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM find_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM trylock_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM unlock_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"new", 0, new_nif},
    {"size", 1, size_nif},
    {"peek_head", 1, peek_head_nif},
    {"copying_read", 2, copying_read_nif},
    {"write", 2, write_nif},
    {"skip", 2, skip_nif},
    {"find_byte_index", 2, find_nif},
    {"try_lock", 1, trylock_nif},
    {"unlock", 1, unlock_nif},
};

ERL_NIF_INIT(prim_buffer, nif_funcs, load, NULL, upgrade, unload)

static void gc_buffer(ErlNifEnv *env, void* data);

static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");

    am_lock_order_violation = enif_make_atom(env, "lock_order_violation");
    am_acquired = enif_make_atom(env, "acquired");
    am_busy = enif_make_atom(env, "busy");

    am_continue = enif_make_atom(env, "continue");

    am_out_of_memory = enif_make_atom(env, "out_of_memory");
    am_not_found = enif_make_atom(env, "not_found");

    rtype_buffer = enif_open_resource_type(env, NULL, "gc_buffer", gc_buffer,
        ERL_NIF_RT_CREATE, NULL);

    *priv_data = NULL;

    return 0;
}

static void unload(ErlNifEnv *env, void* priv_data)
{

}

static int upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    if(*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }

    if(*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }

    if(load(env, priv_data, load_info)) {
        return -1;
    }

    return 0;
}

static void gc_buffer(ErlNifEnv *env, void* data) {
    buffer_data_t *buffer = (buffer_data_t*)data;

    if(buffer->accumulator_present) {
        enif_release_binary(&buffer->accumulator);
    }

    enif_ioq_destroy(buffer->queue);
}

static int get_buffer_data(ErlNifEnv *env, ERL_NIF_TERM opaque, buffer_data_t **buffer) {
    return enif_get_resource(env, opaque, rtype_buffer, (void **)buffer);
}

/* Copies a number of bytes from the head of the iovec, skipping "vec_skip"
 * vector elements followed by "byte_skip" bytes on the target vector. */
static void copy_from_iovec(SysIOVec *iovec, int vec_len, int vec_skip,
        size_t byte_skip, size_t size, char *data) {

    size_t bytes_copied, skip_offset;
    int vec_index;

    skip_offset = byte_skip;
    vec_index = vec_skip;
    bytes_copied = 0;

    while(bytes_copied < size) {
        size_t block_size, copy_size;
        char *block_start;

        ASSERT(vec_index < vec_len);

        block_start = (char*)iovec[vec_index].iov_base;
        block_size = iovec[vec_index].iov_len;

        copy_size = MIN(size - bytes_copied, block_size - skip_offset);
        sys_memcpy(&data[bytes_copied], &block_start[skip_offset], copy_size);

        bytes_copied += copy_size;
        skip_offset = 0;

        vec_index++;
    }
}

/* Convenience function for copy_from_iovec over queues. */
static void copy_from_queue(ErlNifIOQueue *queue, int queue_skip,
    size_t byte_skip, size_t size, char *data) {

    SysIOVec *queued_data;
    int queue_length;

    queued_data = enif_ioq_peek(queue, &queue_length);
    ASSERT(queue_skip < queue_length);

    copy_from_iovec(queued_data, queue_length, queue_skip, byte_skip, size, data);
}

static int enqueue_write_accumulator(buffer_data_t *buffer) {
    ASSERT(!buffer->accumulator_present ^ (buffer->accumulated_bytes > 0));

    if(buffer->accumulator_present && buffer->accumulated_bytes > 0) {
        if(!enif_realloc_binary(&buffer->accumulator, buffer->accumulated_bytes)) {
            return 0;
        } else if(!enif_ioq_enq_binary(buffer->queue, &buffer->accumulator, 0)) {
            return 0;
        }

        /* The queue owns the accumulator now. */
        buffer->accumulator_present = 0;
        buffer->accumulated_bytes = 0;
    }

    return 1;
}

static int combine_small_writes(buffer_data_t *buffer, ErlNifIOVec *iovec) {
    ASSERT(!buffer->accumulator_present ^ (buffer->accumulated_bytes > 0));

    if(buffer->accumulated_bytes + iovec->size >= ACCUMULATOR_SIZE) {
        if(iovec->size >= (ACCUMULATOR_SIZE / 2)) {
            return 0;
        }

        if(!enqueue_write_accumulator(buffer)) {
            return 0;
        }
    }

    if(!buffer->accumulator_present) {
        if(!enif_alloc_binary(ACCUMULATOR_SIZE, &buffer->accumulator)) {
            return 0;
        }

        buffer->accumulator_present = 1;
    }

    copy_from_iovec(iovec->iov, iovec->iovcnt, 0, 0, iovec->size,
            (char*)&buffer->accumulator.data[buffer->accumulated_bytes]);
    buffer->accumulated_bytes += iovec->size;

    return 1;
}

/* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** */

static ERL_NIF_TERM new_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;
    ERL_NIF_TERM result;

    buffer = (buffer_data_t*)enif_alloc_resource(rtype_buffer, sizeof(buffer_data_t));
    buffer->queue = enif_ioq_create(ERL_NIF_IOQ_NORMAL);

    if(buffer->queue != NULL) {
#ifdef DEBUG
        erts_atomic32_init_nob(&buffer->concurrent_users, 0);
#endif
        erts_atomic32_init_nob(&buffer->external_lock, 0);

        buffer->accumulator_present = 0;
        buffer->accumulated_bytes = 0;

        result = enif_make_resource(env, buffer);
    } else {
        result = enif_raise_exception(env, am_out_of_memory);
    }

    enif_release_resource(buffer);

    return result;
}

static ERL_NIF_TERM size_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;

    size_t total_size;

    if(argc != 1 || !get_buffer_data(env, argv[0], &buffer)) {
        return enif_make_badarg(env);
    }

    ASSERT(erts_atomic32_inc_read_acqb(&buffer->concurrent_users) == 1);

    total_size = enif_ioq_size(buffer->queue);

    if(buffer->accumulator_present) {
        total_size += buffer->accumulated_bytes;
    } else {
        ASSERT(buffer->accumulated_bytes == 0);
    }

    ASSERT(erts_atomic32_dec_read_relb(&buffer->concurrent_users) == 0);

    return enif_make_uint64(env, total_size);
}

static ERL_NIF_TERM copying_read_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;

    ERL_NIF_TERM result;
    unsigned char *data;
    Uint64 block_size;

    if(argc != 2 || !get_buffer_data(env, argv[0], &buffer)
                 || !enif_get_uint64(env, argv[1], &block_size)) {
        return enif_make_badarg(env);
    }

    ASSERT(erts_atomic32_inc_read_acqb(&buffer->concurrent_users) == 1);

    if(!enqueue_write_accumulator(buffer)) {
        return enif_raise_exception(env, am_out_of_memory);
    }

    if(enif_ioq_size(buffer->queue) < block_size) {
        return enif_make_badarg(env);
    }

    data = enif_make_new_binary(env, block_size, &result);

    if(block_size > 0) {
        copy_from_queue(buffer->queue, 0, 0, block_size, (char*)data);
        enif_ioq_deq(buffer->queue, block_size, NULL);
    }

    ASSERT(erts_atomic32_dec_read_relb(&buffer->concurrent_users) == 0);

    return result;
}

static ERL_NIF_TERM write_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;

    ErlNifIOVec vec, *iovec = &vec;
    ERL_NIF_TERM tail;

    if(argc != 2 || !get_buffer_data(env, argv[0], &buffer)
                 || !enif_inspect_iovec(env, 64, argv[1], &tail, &iovec)) {
        return enif_make_badarg(env);
    }

    ASSERT(erts_atomic32_inc_read_acqb(&buffer->concurrent_users) == 1);

    if(!combine_small_writes(buffer, iovec)) {
        if(!enqueue_write_accumulator(buffer) || !enif_ioq_enqv(buffer->queue, iovec, 0)) {
            return enif_raise_exception(env, am_out_of_memory);
        }
    }

    ASSERT(erts_atomic32_dec_read_relb(&buffer->concurrent_users) == 0);

    if(!enif_is_empty_list(env, tail)) {
        const ERL_NIF_TERM new_argv[2] = {argv[0], tail};

        return enif_schedule_nif(env, "write", 0, &write_nif, argc, new_argv);
    }

    return am_ok;
}

static ERL_NIF_TERM peek_head_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;

    ERL_NIF_TERM result;

    if(argc != 1 || !get_buffer_data(env, argv[0], &buffer)) {
        return enif_make_badarg(env);
    }

    ASSERT(erts_atomic32_inc_read_acqb(&buffer->concurrent_users) == 1);

    if(!enqueue_write_accumulator(buffer)) {
        return enif_raise_exception(env, am_out_of_memory);
    }

    if(!enif_ioq_peek_head(env, buffer->queue, NULL, &result)) {
        return enif_make_badarg(env);
    }

    ASSERT(erts_atomic32_dec_read_relb(&buffer->concurrent_users) == 0);

    return result;
}

static ERL_NIF_TERM skip_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;

    Uint64 block_size;

    if(argc != 2 || !get_buffer_data(env, argv[0], &buffer)
                 || !enif_get_uint64(env, argv[1], &block_size)) {
        return enif_make_badarg(env);
    }

    ASSERT(erts_atomic32_inc_read_acqb(&buffer->concurrent_users) == 1);

    if(!enqueue_write_accumulator(buffer)) {
        return enif_raise_exception(env, am_out_of_memory);
    } else if(enif_ioq_size(buffer->queue) < block_size) {
        return enif_make_badarg(env);
    }

    enif_ioq_deq(buffer->queue, block_size, NULL);

    ASSERT(erts_atomic32_dec_read_relb(&buffer->concurrent_users) == 0);

    return am_ok;
}

static ERL_NIF_TERM find_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;

    int queue_length, queue_index;
    SysIOVec *queued_data;
    size_t queue_size;

    size_t search_offset;
    int needle;

    if(argc != 2 || !get_buffer_data(env, argv[0], &buffer)
                 || !enif_get_int(env, argv[1], &needle)) {
        return enif_make_badarg(env);
    }

    ASSERT(erts_atomic32_inc_read_acqb(&buffer->concurrent_users) == 1);

    if(!enqueue_write_accumulator(buffer)) {
        return enif_raise_exception(env, am_out_of_memory);
    } else if(needle < 0 || needle > 255) {
        return enif_make_badarg(env);
    }

    queued_data = enif_ioq_peek(buffer->queue, &queue_length);
    queue_size = enif_ioq_size(buffer->queue);
    queue_index = 0;

    search_offset = 0;

    if(queue_size > (FIND_NIF_RESCHEDULE_SIZE / 100)) {
        if(enif_thread_type() == ERL_NIF_THR_NORMAL_SCHEDULER) {
            int timeslice_percent;

            if(queue_size >= FIND_NIF_RESCHEDULE_SIZE) {
                ASSERT(erts_atomic32_dec_read_relb(&buffer->concurrent_users) == 0);

                return enif_schedule_nif(env, "find",
                    ERL_NIF_DIRTY_JOB_CPU_BOUND, &find_nif, argc, argv);
            }

            timeslice_percent = (queue_size * 100) / FIND_NIF_RESCHEDULE_SIZE;
            enif_consume_timeslice(env, timeslice_percent);
        }
    }

    while(queue_index < queue_length) {
        char *needle_address;
        char *block_start;
        size_t block_size;

        block_start = queued_data[queue_index].iov_base;
        block_size = queued_data[queue_index].iov_len;

        needle_address = memchr(block_start, needle, block_size);

        if(needle_address != NULL) {
            size_t result = search_offset + (needle_address - block_start);

            ASSERT(erts_atomic32_dec_read_relb(&buffer->concurrent_users) == 0);

            return enif_make_tuple2(env, am_ok, enif_make_uint64(env, result));
        }

        search_offset += block_size;
        queue_index++;
    }

    ASSERT(erts_atomic32_dec_read_relb(&buffer->concurrent_users) == 0);

    return am_not_found;
}

/* */

static ERL_NIF_TERM trylock_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;

    if(argc != 1 || !get_buffer_data(env, argv[0], &buffer)) {
        return enif_make_badarg(env);
    }

    if(erts_atomic32_cmpxchg_acqb(&buffer->external_lock, 1, 0) == 0) {
        return am_acquired;
    }

    return am_busy;
}

static ERL_NIF_TERM unlock_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    buffer_data_t *buffer;

    if(argc != 1 || !get_buffer_data(env, argv[0], &buffer)) {
        return enif_make_badarg(env);
    }

    if(erts_atomic32_cmpxchg_relb(&buffer->external_lock, 0, 1) == 0) {
        return enif_raise_exception(env, am_lock_order_violation);
    }

    return am_ok;
}
