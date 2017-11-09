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

#include <stdio.h>
#include <zlib.h>

#include "erl_nif.h"
#include "config.h"
#include "sys.h"

#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#endif

#define INFL_DICT_SZ    (32768)

/* NIF interface declarations */
static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv *env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv *env, void* priv_data);

static ErlNifResourceType *rtype_zlib;

static ERL_NIF_TERM am_not_on_controlling_process;

static ERL_NIF_TERM am_not_initialized;
static ERL_NIF_TERM am_already_initialized;

static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_error;

static ERL_NIF_TERM am_continue;
static ERL_NIF_TERM am_finished;

static ERL_NIF_TERM am_not_supported;
static ERL_NIF_TERM am_need_dictionary;

static ERL_NIF_TERM am_empty;

static ERL_NIF_TERM am_stream_end;
static ERL_NIF_TERM am_stream_error;
static ERL_NIF_TERM am_data_error;
static ERL_NIF_TERM am_mem_error;
static ERL_NIF_TERM am_buf_error;
static ERL_NIF_TERM am_version_error;
static ERL_NIF_TERM am_unknown_error;

typedef enum {
    ST_NONE    = 0,
    ST_DEFLATE = 1,
    ST_INFLATE = 2,
    ST_CLOSED = 3
} zlib_state_t;

/* Controls what to do when the user attempts to decompress more data after
 * Z_STREAM_END has been returned:
 *
 * - 'cut' wipes all further input and returns empty results until reset by
 * the user. This is the default behavior, matching that of the old driver.
 * - 'reset' resets the state without discarding any input, making it possible
 * to decompress blindly concatenated streams.
 * - 'error' crashes with a data error. */
typedef enum {
    EOS_BEHAVIOR_ERROR = 0,
    EOS_BEHAVIOR_RESET = 1,
    EOS_BEHAVIOR_CUT = 2
} zlib_eos_behavior_t;

typedef struct {
    z_stream s;
    zlib_state_t state;

    zlib_eos_behavior_t eos_behavior;

    /* These refer to the plaintext CRC, and are only needed for zlib:crc32/1
     * which is deprecated. */
    uLong input_crc;
    uLong output_crc;
    int want_input_crc;
    int want_output_crc;

    int is_raw_stream;

    int eos_seen;

    /* DEPRECATED */
    int inflateChunk_buffer_size;

    ErlNifPid controlling_process;
    ErlNifMutex *controller_lock;

    ErlNifIOQueue *input_queue;

    ErlNifEnv *stash_env;
    ERL_NIF_TERM stash_term;
} zlib_data_t;

/* The NIFs: */

static ERL_NIF_TERM zlib_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_set_controller(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM zlib_deflateInit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_deflateSetDictionary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_deflateReset(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_deflateEnd(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_deflateParams(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_deflate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM zlib_inflateInit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_inflateSetDictionary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_inflateGetDictionary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_inflateReset(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_inflateEnd(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_inflate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM zlib_crc32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM zlib_clearStash(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_setStash(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_getStash(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM zlib_getBufSize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM zlib_setBufSize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM zlib_enqueue_input(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"close_nif", 1, zlib_close},
    {"open_nif", 0, zlib_open},

    {"set_controller_nif", 2, zlib_set_controller},

    /* deflate */
    {"deflateInit_nif", 6, zlib_deflateInit},
    {"deflateSetDictionary_nif", 2, zlib_deflateSetDictionary},
    {"deflateReset_nif", 1, zlib_deflateReset},
    {"deflateEnd_nif", 1, zlib_deflateEnd},
    {"deflateParams_nif", 3, zlib_deflateParams},
    {"deflate_nif", 4, zlib_deflate},

    /* inflate */
    {"inflateInit_nif", 3, zlib_inflateInit},
    {"inflateSetDictionary_nif", 2, zlib_inflateSetDictionary},
    {"inflateGetDictionary_nif", 1, zlib_inflateGetDictionary},
    {"inflateReset_nif", 1, zlib_inflateReset},
    {"inflateEnd_nif", 1, zlib_inflateEnd},
    {"inflate_nif", 4, zlib_inflate},

    /* running checksum */
    {"crc32_nif", 1, zlib_crc32},

    /* The stash keeps a single term alive across calls, and is used in
     * exception_on_need_dict/1 to retain the old error behavior, and for
     * saving data flushed through deflateParams/3. */
    {"getStash_nif", 1, zlib_getStash},
    {"clearStash_nif", 1, zlib_clearStash},
    {"setStash_nif", 2, zlib_setStash},

    /* DEPRECATED: buffer size for inflateChunk */
    {"getBufSize_nif", 1, zlib_getBufSize},
    {"setBufSize_nif", 2, zlib_setBufSize},

    {"enqueue_nif", 2, zlib_enqueue_input},
};

ERL_NIF_INIT(zlib, nif_funcs, load, NULL, upgrade, unload)

static void gc_zlib(ErlNifEnv *env, void* data);

static int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
    am_not_on_controlling_process =
        enif_make_atom(env, "not_on_controlling_process");

    am_not_initialized = enif_make_atom(env, "not_initialized");
    am_already_initialized = enif_make_atom(env, "already_initialized");

    am_ok = enif_make_atom(env, "ok");
    am_error = enif_make_atom(env, "error");

    am_continue = enif_make_atom(env, "continue");
    am_finished = enif_make_atom(env, "finished");

    am_not_supported = enif_make_atom(env, "not_supported");
    am_need_dictionary = enif_make_atom(env, "need_dictionary");

    am_empty = enif_make_atom(env, "empty");

    am_stream_end = enif_make_atom(env, "stream_end");
    am_stream_error = enif_make_atom(env, "stream_error");
    am_data_error = enif_make_atom(env, "data_error");
    am_mem_error = enif_make_atom(env, "mem_error");
    am_buf_error = enif_make_atom(env, "buf_error");
    am_version_error = enif_make_atom(env, "version_error");
    am_unknown_error = enif_make_atom(env, "unknown_error");

    rtype_zlib = enif_open_resource_type(env, NULL,
            "gc_zlib", gc_zlib, ERL_NIF_RT_CREATE, NULL);
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

static void* zlib_alloc(void* data, unsigned int items, unsigned int size)
{
    return (void*) enif_alloc(items * size);
}

static void zlib_free(void* data, void* addr)
{
    enif_free(addr);
}

static ERL_NIF_TERM zlib_return(ErlNifEnv *env, int code) {
    ERL_NIF_TERM reason;
    switch(code) {
    case Z_OK:
        reason = am_ok;
        break;
    case Z_STREAM_END:
        reason = am_stream_end;
        break;
    case Z_ERRNO:
        reason = enif_make_int(env, errno);
        break;
    case Z_STREAM_ERROR:
        reason = enif_raise_exception(env, am_stream_error);
        break;
    case Z_DATA_ERROR:
        reason = enif_raise_exception(env, am_data_error);
        break;
    case Z_MEM_ERROR:
        reason = am_mem_error;
        break;
    case Z_BUF_ERROR:
        reason = am_buf_error;
        break;
    case Z_VERSION_ERROR:
        reason = am_version_error;
        break;
    default:
        reason = am_unknown_error;
        break;
    }
    return reason;
}

static void zlib_internal_close(zlib_data_t *d) {
    if(d->state == ST_DEFLATE) {
        deflateEnd(&d->s);
    } else if(d->state == ST_INFLATE) {
        inflateEnd(&d->s);
    }

    if(d->state != ST_CLOSED) {
        if(d->stash_env != NULL) {
            enif_free_env(d->stash_env);
        }

        d->state = ST_CLOSED;
    }
}

static void gc_zlib(ErlNifEnv *env, void* data) {
    zlib_data_t *d = (zlib_data_t*)data;

    enif_mutex_destroy(d->controller_lock);
    enif_ioq_destroy(d->input_queue);

    zlib_internal_close(d);

    (void)env;
}

static int get_zlib_data(ErlNifEnv *env, ERL_NIF_TERM opaque, zlib_data_t **d) {
    return enif_get_resource(env, opaque, rtype_zlib, (void **)d);
}

static int zlib_process_check(ErlNifEnv *env, zlib_data_t *d) {
    int is_controlling_process;
    ErlNifPid current_process;

    enif_self(env, &current_process);

    enif_mutex_lock(d->controller_lock);

    is_controlling_process = enif_is_identical(
        enif_make_pid(env, &current_process),
        enif_make_pid(env, &d->controlling_process));

    enif_mutex_unlock(d->controller_lock);

    return is_controlling_process;
}

static void zlib_reset_input(zlib_data_t *d) {
    enif_ioq_destroy(d->input_queue);
    d->input_queue = enif_ioq_create(ERL_NIF_IOQ_NORMAL);

    if(d->stash_env != NULL) {
        enif_free_env(d->stash_env);
        d->stash_env = NULL;
        d->stash_term = NIL;
    }
}

static int zlib_flush_queue(int (*codec)(z_stream*, int), ErlNifEnv *env,
        zlib_data_t *d, size_t input_limit, ErlNifBinary *output_buffer, int flush,
        size_t *bytes_produced, size_t *bytes_consumed, size_t *bytes_remaining) {

    int vec_len, vec_idx;
    SysIOVec *input_vec;
    int res;

    input_vec = enif_ioq_peek(d->input_queue, &vec_len);
    vec_idx = 0;
    res = Z_OK;

    *bytes_produced = 0;
    *bytes_consumed = 0;

    d->s.avail_out = output_buffer->size;
    d->s.next_out = output_buffer->data;

    while(res == Z_OK && vec_idx < vec_len && *bytes_consumed < input_limit) {
        size_t timeslice_percent, block_consumed, block_size;

        block_size = MIN(input_vec[vec_idx].iov_len, input_limit);

        d->s.next_in = input_vec[vec_idx].iov_base;
        d->s.avail_in = block_size;

        res = codec(&d->s, Z_NO_FLUSH);

        ASSERT(d->s.avail_in == 0 || d->s.avail_out == 0 || res != Z_OK);

        block_consumed = block_size - d->s.avail_in;
        *bytes_consumed += block_consumed;

        if(d->want_input_crc) {
            d->input_crc =
                crc32(d->input_crc, input_vec[vec_idx].iov_base, block_consumed);
        }

        timeslice_percent = (100 * block_consumed) / input_limit;
        if(enif_consume_timeslice(env, MAX(1, timeslice_percent))) {
            break;
        }

        vec_idx++;
    }

    if(!enif_ioq_deq(d->input_queue, *bytes_consumed, bytes_remaining)) {
        *bytes_remaining = 0;
        res = Z_BUF_ERROR;
    }

    if(res == Z_OK && flush != Z_NO_FLUSH && (*bytes_remaining == 0)) {
        d->s.next_in = NULL;
        d->s.avail_in = 0;

        res = codec(&d->s, flush);
    }

    *bytes_produced = output_buffer->size - d->s.avail_out;

    return res;
}

static ERL_NIF_TERM zlib_codec(int (*codec)(z_stream*, int),
                               ErlNifEnv *env, zlib_data_t *d,
                               int input_chunk_size,
                               int output_chunk_size,
                               int flush) {

    size_t bytes_produced, bytes_consumed, bytes_remaining;
    ErlNifBinary output_buffer;
    int res;

    if(!enif_alloc_binary(output_chunk_size, &output_buffer)) {
        return zlib_return(env, Z_MEM_ERROR);
    }

    res = zlib_flush_queue(codec, env, d, input_chunk_size, &output_buffer,
        flush, &bytes_produced, &bytes_consumed, &bytes_remaining);

    if(res < 0 && res != Z_BUF_ERROR) {
        enif_release_binary(&output_buffer);
        return zlib_return(env, res);
    }

    if(res == Z_STREAM_END) {
        d->eos_seen = 1;
    }

    if(d->want_output_crc) {
        d->output_crc =
            crc32(d->output_crc, output_buffer.data, bytes_produced);
    }

    if(bytes_consumed == 0 && bytes_produced == 0 && bytes_remaining != 0) {
        /* Die if we've made zero progress; this should not happen on
         * well-formed input. */

        enif_release_binary(&output_buffer);
        return zlib_return(env, Z_DATA_ERROR);
    } else {
        ERL_NIF_TERM flushed_output;

        if(bytes_produced > 0) {
            if(bytes_produced < output_buffer.size) {
                enif_realloc_binary(&output_buffer, bytes_produced);
            }

            flushed_output =
                enif_make_list1(env, enif_make_binary(env, &output_buffer));
        } else {
            enif_release_binary(&output_buffer);
            flushed_output = enif_make_list(env, 0);
        }

        if(bytes_remaining == 0 && bytes_produced < output_chunk_size) {
            return enif_make_tuple2(env, am_finished, flushed_output);
        } else if(res != Z_NEED_DICT) {
            return enif_make_tuple2(env, am_continue, flushed_output);
        }

        return enif_make_tuple3(env, am_need_dictionary,
            enif_make_int(env, d->s.adler), flushed_output);
    }
}

/* zlib nifs */

static ERL_NIF_TERM zlib_getStash(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    }

    if(d->stash_env == NULL) {
        return am_empty;
    }

    return enif_make_tuple2(env, am_ok, enif_make_copy(env, d->stash_term));
}

static ERL_NIF_TERM zlib_clearStash(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->stash_env == NULL) {
        return enif_raise_exception(env, am_error);
    }

    enif_free_env(d->stash_env);
    d->stash_env = NULL;
    d->stash_term = NIL;

    return am_ok;
}

static ERL_NIF_TERM zlib_setStash(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    if(argc != 2 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->stash_env != NULL) {
        return enif_raise_exception(env, am_error);
    }

    d->stash_env = enif_alloc_env();
    d->stash_term = enif_make_copy(d->stash_env, argv[1]);

    return am_ok;
}

static ERL_NIF_TERM zlib_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;
    ERL_NIF_TERM result;

    d = (zlib_data_t *) enif_alloc_resource(rtype_zlib, sizeof(zlib_data_t));

    memset(&d->s, 0, sizeof(z_stream));

    enif_self(env, &d->controlling_process);

    d->input_queue = enif_ioq_create(ERL_NIF_IOQ_NORMAL);

    d->controller_lock = enif_mutex_create("zlib_controller_lock");

    d->s.zalloc = zlib_alloc;
    d->s.zfree  = zlib_free;
    d->s.opaque = d;
    d->s.data_type = Z_BINARY;

    d->eos_behavior = EOS_BEHAVIOR_CUT;
    d->eos_seen = 0;

    d->state = ST_NONE;

    d->want_output_crc = 0;
    d->want_input_crc = 0;
    d->is_raw_stream = 0;

    d->output_crc = crc32(0L, Z_NULL, 0);
    d->input_crc = crc32(0L, Z_NULL, 0);

    d->stash_env = NULL;
    d->stash_term = NIL;

    d->inflateChunk_buffer_size = 4000;

    result = enif_make_resource(env, d);
    enif_release_resource(d);

    return result;
}

static ERL_NIF_TERM zlib_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    /* strictly speaking not needed since the gc will handle this */
    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state == ST_CLOSED) {
        return enif_raise_exception(env, am_not_initialized);
    }

    zlib_internal_close(d);

    return am_ok;
}

static ERL_NIF_TERM zlib_set_controller(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    ErlNifPid new_owner;

    if(argc != 2 || !get_zlib_data(env, argv[0], &d)
                 || !enif_get_local_pid(env, argv[1], &new_owner)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    }

    enif_mutex_lock(d->controller_lock);

    d->controlling_process = new_owner;

    enif_mutex_unlock(d->controller_lock);

    return am_ok;
}

/* deflate */

static ERL_NIF_TERM zlib_deflateInit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;
    int level, method, windowBits, memLevel, strategy, res;

    if(argc != 6 || !get_zlib_data(env, argv[0], &d) 
                 || !enif_get_int(env, argv[1], &level)
                 || !enif_get_int(env, argv[2], &method)
                 || !enif_get_int(env, argv[3], &windowBits)
                 || !enif_get_int(env, argv[4], &memLevel)
                 || !enif_get_int(env, argv[5], &strategy)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_NONE) {
        return enif_raise_exception(env, am_already_initialized);
    }

    res = deflateInit2(&d->s, level, method, windowBits, memLevel, strategy);

    if(res == Z_OK) {
        d->state = ST_DEFLATE;
        d->eos_seen = 0;

        d->is_raw_stream = (windowBits < 0);

        d->want_output_crc = 0;
        d->want_input_crc = d->is_raw_stream;

        d->output_crc = crc32(0L, Z_NULL, 0);
        d->input_crc = crc32(0L, Z_NULL, 0);
    }

    return zlib_return(env, res);
}

static ERL_NIF_TERM zlib_deflateSetDictionary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;
    ErlNifBinary bin;
    int res;

    if(argc != 2 || !get_zlib_data(env, argv[0], &d)
                 || !enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_DEFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    if((res = deflateSetDictionary(&d->s, bin.data, bin.size)) == Z_OK) {
        uLong checksum = d->s.adler;

        /* d->s.adler is not updated in raw deflate mode, so we'll calculate it
         * ourselves in case the user wants to rely on that behavior. */
        if(d->is_raw_stream) {
            checksum = adler32(0, bin.data, bin.size);
        }

        return enif_make_int(env, checksum);
    }

    return zlib_return(env, res);
}

static ERL_NIF_TERM zlib_deflateReset(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;
    int res;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_DEFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    res = deflateReset(&d->s);

    d->input_crc = crc32(0L, Z_NULL, 0);
    d->eos_seen = 0;

    zlib_reset_input(d);

    return zlib_return(env, res);
}

static ERL_NIF_TERM zlib_deflateEnd(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;
    int res;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_DEFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    res = deflateEnd(&d->s);

    if(res == Z_OK && enif_ioq_size(d->input_queue) > 0) {
        res = Z_DATA_ERROR;
    }

    zlib_reset_input(d);
    d->state = ST_NONE;

    return zlib_return(env, res);
}

static ERL_NIF_TERM zlib_deflateParams(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    int res, level, strategy;
    Bytef dummy_buffer;

    if(argc != 3 || !get_zlib_data(env, argv[0], &d)
                 || !enif_get_int(env, argv[1], &level)
                 || !enif_get_int(env, argv[2], &strategy)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_DEFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    /* This is a bit of a hack; deflateParams flushes with Z_BLOCK which won't
     * stop at a byte boundary, so we can't split this operation up, and we
     * can't allocate a buffer large enough to fit it in one go since we have
     * to support zlib versions that lack deflatePending.
     *
     * We therefore flush everything prior to this call to ensure that we are
     * stopped on a byte boundary and have no pending data. We then hand it a
     * dummy buffer to detect when this assumption doesn't hold (Hopefully
     * never), and to smooth over an issue with zlib 1.2.11 which always
     * returns Z_BUF_ERROR when d->s.avail_out is 0, regardless of whether
     * there's any pending data or not. */

    d->s.next_out = &dummy_buffer;
    d->s.avail_out = 1;

    res = deflateParams(&d->s, level, strategy);

    if(d->s.avail_out == 0) {
        return zlib_return(env, Z_STREAM_ERROR);
    }

    return zlib_return(env, res);
}

static ERL_NIF_TERM zlib_deflate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    int input_chunk_size, output_chunk_size, flush;

    if(argc != 4 || !get_zlib_data(env, argv[0], &d)
                 || !enif_get_int(env, argv[1], &input_chunk_size)
                 || !enif_get_int(env, argv[2], &output_chunk_size)
                 || !enif_get_int(env, argv[3], &flush)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_DEFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    return zlib_codec(&deflate, env, d, input_chunk_size, output_chunk_size, flush);
}

/* inflate */

static ERL_NIF_TERM zlib_inflateInit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    int windowBits, eosBehavior, res;

    if(argc != 3 || !get_zlib_data(env, argv[0], &d)
                 || !enif_get_int(env, argv[1], &windowBits)
                 || !enif_get_int(env, argv[2], &eosBehavior)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_NONE) {
        return enif_raise_exception(env, am_already_initialized);
    }

    res = inflateInit2(&d->s, windowBits);

    if(res == Z_OK) {
        d->state = ST_INFLATE;

        d->eos_behavior = eosBehavior;
        d->eos_seen = 0;

        d->is_raw_stream = (windowBits < 0);

        d->want_output_crc = d->is_raw_stream;
        d->want_input_crc = 0;

        d->output_crc = crc32(0L, Z_NULL, 0);
        d->input_crc = crc32(0L, Z_NULL, 0);
    }

    return zlib_return(env, res);
}

static ERL_NIF_TERM zlib_inflateSetDictionary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;
    ErlNifBinary bin;
    int res;

    if(argc != 2 || !get_zlib_data(env, argv[0], &d)
                 || !enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_INFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    res = inflateSetDictionary(&d->s, bin.data, bin.size);

    return zlib_return(env, res);
}

#ifdef HAVE_ZLIB_INFLATEGETDICTIONARY
/* Work around broken build system with runtime version test */
static int zlib_supports_inflateGetDictionary(void) {
    static int supportsGetDictionary = -1;

#if defined(__APPLE__) && defined(__MACH__)
    if(supportsGetDictionary < 0) {
        unsigned int v[4] = {0, 0, 0, 0};
        unsigned hexver;

        sscanf(zlibVersion(), "%u.%u.%u.%u", &v[0], &v[1], &v[2], &v[3]);

        hexver = (v[0] << (8*3)) | (v[1] << (8*2)) | (v[2] << (8)) | v[3];
        supportsGetDictionary = (hexver >= 0x1020701); /* 1.2.7.1 */
    }
#endif

    return supportsGetDictionary;
}
#endif

static ERL_NIF_TERM zlib_inflateGetDictionary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_INFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

#ifdef HAVE_ZLIB_INFLATEGETDICTIONARY
    if(zlib_supports_inflateGetDictionary()) {
        ErlNifBinary obin;
        uInt len;
        int res;

        enif_alloc_binary(INFL_DICT_SZ, &obin);
        len = 0;

        if((res = inflateGetDictionary(&d->s, obin.data, &len)) < 0) {
            enif_release_binary(&obin);
            return zlib_return(env, res);
        }

        enif_realloc_binary(&obin, (size_t)len);
        return enif_make_binary(env, &obin);
    }
#endif

    return enif_raise_exception(env, am_not_supported);
}

static ERL_NIF_TERM zlib_inflateReset(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;
    int res;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_INFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    res = inflateReset(&d->s);

    d->output_crc = crc32(0L, Z_NULL, 0);
    d->eos_seen = 0;

    zlib_reset_input(d);

    return zlib_return(env, res);
}

static ERL_NIF_TERM zlib_inflateEnd(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;
    int res;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_INFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    res = inflateEnd(&d->s);

    if(res == Z_OK && (!d->eos_seen || enif_ioq_size(d->input_queue) > 0)) {
        res = Z_DATA_ERROR;
    }

    zlib_reset_input(d);
    d->state = ST_NONE;

    return zlib_return(env, res);
}

static ERL_NIF_TERM zlib_inflate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    int input_chunk_size, output_chunk_size, flush;

    if(argc != 4 || !get_zlib_data(env, argv[0], &d)
                 || !enif_get_int(env, argv[1], &input_chunk_size)
                 || !enif_get_int(env, argv[2], &output_chunk_size)
                 || !enif_get_int(env, argv[3], &flush)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_INFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    if(d->eos_seen && enif_ioq_size(d->input_queue) > 0) {
        int res;

        switch(d->eos_behavior) {
        case EOS_BEHAVIOR_ERROR:
            return zlib_return(env, Z_DATA_ERROR);
        case EOS_BEHAVIOR_RESET:
            res = inflateReset(&d->s);

            if(res != Z_OK) {
                return zlib_return(env, res);
            }

            d->eos_seen = 0;

            break;
        case EOS_BEHAVIOR_CUT:
            zlib_reset_input(d);
        }
    }

    return zlib_codec(&inflate, env, d, input_chunk_size, output_chunk_size, flush);
}

static ERL_NIF_TERM zlib_crc32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    }

    if(d->state == ST_DEFLATE) {
        return enif_make_ulong(env, d->input_crc);
    } else if(d->state == ST_INFLATE) {
        return enif_make_ulong(env, d->output_crc);
    }

    return enif_raise_exception(env, am_not_initialized);
}

static ERL_NIF_TERM zlib_getBufSize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    if(argc != 1 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    }

    return enif_make_int(env, d->inflateChunk_buffer_size);
}

static ERL_NIF_TERM zlib_setBufSize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    if(argc != 2 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    }

    if(!enif_get_int(env, argv[1], &d->inflateChunk_buffer_size)) {
        return enif_make_badarg(env);
    }

    return am_ok;
}

static ERL_NIF_TERM zlib_enqueue_input(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    zlib_data_t *d;

    ErlNifIOVec prealloc, *iovec = &prealloc;
    ERL_NIF_TERM tail;

    if(argc != 2 || !get_zlib_data(env, argv[0], &d)) {
        return enif_make_badarg(env);
    } else if(!zlib_process_check(env, d)) {
        return enif_raise_exception(env, am_not_on_controlling_process);
    } else if(d->state != ST_DEFLATE && d->state != ST_INFLATE) {
        return enif_raise_exception(env, am_not_initialized);
    }

    if(!enif_inspect_iovec(env, 256, argv[1], &tail, &iovec)) {
        return enif_make_badarg(env);
    } else if(!enif_ioq_enqv(d->input_queue, iovec, 0)) {
        return enif_make_badarg(env);
    }

    if(!enif_is_empty_list(env, tail)) {
        return enif_make_tuple2(env, am_continue, tail);
    }

    return am_ok;
}
