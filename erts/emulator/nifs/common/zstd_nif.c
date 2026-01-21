/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson 2023-2025. All Rights Reserved.
 * Copyright Ericsson AB 2023-2025. All Rights Reserved.
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
 * To update the Zstandard version, run erts/emulator/zstd/update.sh
 */

#define STATIC_ERLANG_NIF 1

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_nif.h"
#define ZSTD_STATIC_LINKING_ONLY
#include "erl_zstd.h"
#include "erl_zdict.h"

static ErlNifResourceType *compress_type;
static ErlNifResourceType *decompress_type;

static ErlNifResourceType *compress_dict_type;
static ErlNifResourceType *decompress_dict_type;

static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_undefined;

static ERL_NIF_TERM am_badarg;

static ERL_NIF_TERM am_continue;
static ERL_NIF_TERM am_flush;
static ERL_NIF_TERM am_done;

static ERL_NIF_TERM am_true;
static ERL_NIF_TERM am_false;

typedef struct zstd_ctx {
    union {
        ZSTD_CCtx *c;
        ZSTD_DCtx *d;
    } handle;
    // Environment keeping the ZstdDict
    ErlNifEnv *dict;
    ErlNifPid owner;
    // Set when the context is compressing/decompressing
    unsigned char in_use;
} ZstdCtx;

typedef struct zstd_dict {
    union {
        ZSTD_CDict *c;
        ZSTD_DDict *d;
    };
    // Environment keeping binary for dictionary
    ErlNifEnv *env;
} ZstdDict;

static void *ztsd_alloc(void *state, size_t size) {
    void *ptr = enif_alloc(size);
    // printf("alloc %p(%ld)\r\n", ptr, size);
    return ptr;
}

static void zstd_free(void *state, void *ptr) {
    // printf("free %p\r\n", ptr);
    enif_free(ptr);
}

static ZSTD_customMem zstd_customMem = { .customAlloc = ztsd_alloc, .customFree = zstd_free, .opaque = NULL };

static ERL_NIF_TERM make_zstd_error(ErlNifEnv *env, size_t code) {
    ERL_NIF_TERM result;

    const char *name = ZSTD_getErrorName(code);
    size_t length = strlen(name);
    unsigned char *data = enif_make_new_binary(env, length, &result);
    memcpy(data, name, length);

    return enif_raise_exception(env, enif_make_tuple2(env,
        enif_make_atom(env, "zstd_error"), result));
}

static void close_compress_context(ZstdCtx *ctx) {
    if (ctx->handle.c) {
        ZSTD_freeCCtx(ctx->handle.c);
        ctx->handle.c = NULL;
        if (ctx->dict)
            enif_free_env(ctx->dict);
        ctx->dict = NULL;
    }
}

static void close_decompress_context(ZstdCtx *ctx) {
    if (ctx->handle.d) {
        ZSTD_freeDCtx(ctx->handle.d);
        ctx->handle.d = NULL;
        if (ctx->dict)
            enif_free_env(ctx->dict);
        ctx->dict = NULL;
    }
}

static void reset_compress_context(ZstdCtx *ctx) {
    if (ctx->handle.c) {
        ZSTD_CCtx_reset(ctx->handle.c, ZSTD_reset_session_only);
        ctx->in_use = 0;
    }
}

static void reset_decompress_context(ZstdCtx *ctx) {
    if (ctx->handle.d) {
        ZSTD_DCtx_reset(ctx->handle.d, ZSTD_reset_session_only);
        ctx->in_use = 0;
    }
}

static ERL_NIF_TERM init_compress_nif(ErlNifEnv *env,
                                      int argc,
                                      const ERL_NIF_TERM argv[]) {
    ZstdCtx *context;
    ERL_NIF_TERM result;

    context = enif_alloc_resource(compress_type, sizeof(ZstdCtx));

    context->handle.c = ZSTD_createCCtx_advanced(zstd_customMem);

    (void)enif_self(env, &context->owner);
    context->dict = NULL;
    context->in_use = 0;

    result = enif_make_resource(env, (void *)context);
    enif_release_resource((void *)context);
    return result;
}

static ERL_NIF_TERM init_decompress_nif(ErlNifEnv *env,
                                        int argc,
                                        const ERL_NIF_TERM argv[]) {
    ZstdCtx *context;
    ERL_NIF_TERM result;

    context = enif_alloc_resource(decompress_type, sizeof(ZstdCtx));

    context->handle.d = ZSTD_createDCtx_advanced(zstd_customMem);

    (void)enif_self(env, &context->owner);
    context->dict = NULL;
    context->in_use = 0;

    result = enif_make_resource(env, (void *)context);
    enif_release_resource((void *)context);
    return result;
}

static ZstdCtx *get_zstd_context(ErlNifEnv *env, ERL_NIF_TERM ctx_term,
        ErlNifResourceType *type) {
    ErlNifPid self;
    ZstdCtx *ctx;

    enif_self(env, &self);

    if (!enif_get_resource(env, ctx_term, type, (void **)&ctx) || ctx->handle.c == NULL) {
        enif_make_badarg(env);
        return NULL;
    }

    if (enif_compare_pids(&self, &ctx->owner) != 0) {
        enif_raise_exception(env,
            enif_make_atom(env, "not_on_controlling_process"));
        return NULL;
    }

    return ctx;

}

static ERL_NIF_TERM make_badarg(ErlNifEnv *env) {
    ERL_NIF_TERM reason;

    if (enif_has_pending_exception(env, &reason))
        return enif_raise_exception(env, reason);

    return enif_make_badarg(env);
}

static ERL_NIF_TERM load_compress_dictionary_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], compress_type);
    ErlNifBinary bin;
    size_t res;

    if (!ctx || !enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        return make_badarg(env);
    }

    if (ctx->dict) {
        enif_free_env(ctx->dict);
        ctx->dict = NULL;
    }

    res = ZSTD_CCtx_loadDictionary(ctx->handle.c, bin.data, bin.size);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    return am_ok;
}

static ERL_NIF_TERM load_decompress_dictionary_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], decompress_type);
    ErlNifBinary bin;
    size_t res;

    if (!ctx || !enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        return make_badarg(env);
    }

    if (ctx->dict) {
        enif_free_env(ctx->dict);
        ctx->dict = NULL;
    }

    res = ZSTD_DCtx_loadDictionary(ctx->handle.d, bin.data, bin.size);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    return am_ok;

}

static ERL_NIF_TERM ref_compress_dictionary_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], compress_type);
    ZstdDict *dict;
    size_t res;

    if (!ctx || !enif_get_resource(env, argv[1], compress_dict_type, (void**)&dict)) {
        return make_badarg(env);
    }

    res = ZSTD_CCtx_refCDict(ctx->handle.c, dict->c);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    if (ctx->dict) {
        enif_clear_env(ctx->dict);
    } else {
        ctx->dict = enif_alloc_env();
    }

    enif_make_copy(ctx->dict, argv[1]);

    return am_ok;
}

static ERL_NIF_TERM ref_decompress_dictionary_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], decompress_type);
    ZstdDict *dict;
    size_t res;

    if (!ctx || !enif_get_resource(env, argv[1], decompress_dict_type, (void**)&dict)) {
        return make_badarg(env);
    }

    res = ZSTD_DCtx_refDDict(ctx->handle.d, dict->d);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    if (ctx->dict) {
        enif_clear_env(ctx->dict);
    } else {
        ctx->dict = enif_alloc_env();
    }

    enif_make_copy(ctx->dict, argv[1]);

    return am_ok;
}

static ZSTD_cParameter get_cparam(ErlNifEnv *env, ERL_NIF_TERM key) {
    if (enif_is_identical(key, enif_make_atom(env, "compressionLevel"))) {
        return ZSTD_c_compressionLevel;
    } else if (enif_is_identical(key, enif_make_atom(env, "windowLog"))) {
        return ZSTD_c_windowLog;
    } else if (enif_is_identical(key, enif_make_atom(env, "hashLog"))) {
        return ZSTD_c_hashLog;
    } else if (enif_is_identical(key, enif_make_atom(env, "chainLog"))) {
        return ZSTD_c_chainLog;
    } else if (enif_is_identical(key, enif_make_atom(env, "searchLog"))) {
        return ZSTD_c_searchLog;
    } else if (enif_is_identical(key, enif_make_atom(env, "minMatch"))) {
        return ZSTD_c_minMatch;
    } else if (enif_is_identical(key, enif_make_atom(env, "targetLength"))) {
        return ZSTD_c_targetLength;
    } else if (enif_is_identical(key, enif_make_atom(env, "strategy"))) {
        return ZSTD_c_strategy;
    } else if (enif_is_identical(key, enif_make_atom(env, "targetCBlockSize"))) {
        return ZSTD_c_targetCBlockSize;
    } else if (enif_is_identical(key, enif_make_atom(env, "enableLongDistanceMatching"))) {
        return ZSTD_c_enableLongDistanceMatching;
    } else if (enif_is_identical(key, enif_make_atom(env, "ldmHashLog"))) {
        return ZSTD_c_ldmHashLog;
    } else if (enif_is_identical(key, enif_make_atom(env, "ldmMinMatch"))) {
        return ZSTD_c_ldmMinMatch;
    } else if (enif_is_identical(key, enif_make_atom(env, "ldmBucketSizeLog"))) {
        return ZSTD_c_ldmBucketSizeLog;
    } else if (enif_is_identical(key, enif_make_atom(env, "ldmHashRateLog"))) {
        return ZSTD_c_ldmHashRateLog;
    } else if (enif_is_identical(key, enif_make_atom(env, "contentSizeFlag"))) {
        return ZSTD_c_contentSizeFlag;
    } else if (enif_is_identical(key, enif_make_atom(env, "checksumFlag"))) {
        return ZSTD_c_checksumFlag;
    } else if (enif_is_identical(key, enif_make_atom(env, "dictIDFlag"))) {
        return ZSTD_c_dictIDFlag;
    } else {
        return 0;
    }
}

static ERL_NIF_TERM set_compress_parameter_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], compress_type);
    ZSTD_cParameter param;
    int value;
    size_t res;

    if (!ctx) {
        return make_badarg(env);
    }
        
    param = get_cparam(env, argv[1]);

    if (!param) {
        return enif_raise_exception(env,
                enif_make_tuple2(env, am_badarg,
                    enif_make_tuple2(env, enif_make_atom(env, "invalid_key"),
                    argv[1]))
                );
    }

    switch (param) {
        case ZSTD_c_strategy:
            if (enif_is_identical(argv[2], enif_make_atom(env, "default"))) {
                value = 0;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "fast"))) {
                value = ZSTD_fast;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "dfast"))) {
                value = ZSTD_dfast;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "greedy"))) {
                value = ZSTD_greedy;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "lazy"))) {
                value = ZSTD_lazy;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "lazy2"))) {
                value = ZSTD_lazy2;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "btlazy2"))) {
                value = ZSTD_btlazy2;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "btopt"))) {
                value = ZSTD_btopt;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "btultra"))) {
                value = ZSTD_btultra;
            } else if (enif_is_identical(argv[2], enif_make_atom(env, "btultra2"))) {
                value = ZSTD_btultra2;
            } else {
                return enif_raise_exception(env,
                    enif_make_tuple2(env, am_badarg,
                        enif_make_tuple2(env, enif_make_atom(env, "invalid_value"),
                        argv[2]))
                );
            }
            break;
        case ZSTD_c_enableLongDistanceMatching:
        case ZSTD_c_contentSizeFlag:
        case ZSTD_c_checksumFlag:
        case ZSTD_c_dictIDFlag:
            if (enif_is_identical(argv[2], am_true)) {
                value = 1;
            } else if (enif_is_identical(argv[2], am_false)) {
                value = 0;
            } else {
                return enif_raise_exception(env,
                    enif_make_tuple2(env, am_badarg,
                    enif_make_tuple2(env, enif_make_atom(env, "invalid_value"),
                        argv[2]))
                );
            }
            break;
        default:
            if (!enif_get_int(env, argv[2], &value)) {
                return enif_raise_exception(env,
                    enif_make_tuple2(env, am_badarg,
                    enif_make_tuple2(env, enif_make_atom(env, "invalid_value"),
                        argv[2]))
                );
            }
    }

    res = ZSTD_CCtx_setParameter(ctx->handle.c, param, value);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    return am_ok;
}

static ERL_NIF_TERM get_compress_parameter_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], compress_type);
    ZSTD_cParameter param;
    int value;
    size_t res;

    if (!ctx) {
        return make_badarg(env);
    }

    param = get_cparam(env, argv[1]);

    if (!param) {
        return enif_raise_exception(env,
                enif_make_tuple2(env, am_badarg,
                    enif_make_tuple2(env, enif_make_atom(env, "invalid_key"),
                    argv[1]))
                );
    }

    res = ZSTD_CCtx_getParameter(ctx->handle.c, param, &value);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    switch (param) {
        case ZSTD_c_enableLongDistanceMatching:
        case ZSTD_c_contentSizeFlag:
        case ZSTD_c_checksumFlag:
        case ZSTD_c_dictIDFlag:
            return res ? am_true : am_false;
        case ZSTD_c_strategy:
            if (value == 0) {
                return enif_make_atom(env, "default");
            }
            switch ((ZSTD_strategy)value) {
                case ZSTD_fast: return enif_make_atom(env, "fast");
                case ZSTD_dfast: return enif_make_atom(env, "dfast");
                case ZSTD_greedy: return enif_make_atom(env, "greedy");
                case ZSTD_lazy: return enif_make_atom(env, "lazy");
                case ZSTD_lazy2: return enif_make_atom(env, "lazy2");
                case ZSTD_btlazy2: return enif_make_atom(env, "btlazy2");
                case ZSTD_btopt: return enif_make_atom(env, "btopt");
                case ZSTD_btultra: return enif_make_atom(env, "btultra");
                case ZSTD_btultra2: return enif_make_atom(env, "btultra2");
                default:
                    return enif_make_badarg(env);
            }
        default:
            return enif_make_int(env, value);
    }
}

static ERL_NIF_TERM set_pledged_src_size_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], compress_type);
    ErlNifUInt64 value;
    size_t res;

    if (!ctx || !enif_get_uint64(env, argv[1], &value)) {
        return make_badarg(env);
    }

    res = ZSTD_CCtx_setPledgedSrcSize(ctx->handle.c, value);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    return am_ok;
}

static ZSTD_dParameter get_dparam(ErlNifEnv *env, ERL_NIF_TERM key) {
    if (enif_is_identical(key, enif_make_atom(env, "windowLogMax"))) {
        return ZSTD_d_windowLogMax;
    } else {
        return 0;
    }
}

static ERL_NIF_TERM set_decompress_parameter_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], decompress_type);
    ZSTD_dParameter param;
    int value;
    size_t res;

    if (!ctx || !enif_get_int(env, argv[2], &value)) {
        return make_badarg(env);
    }

    param = get_dparam(env, argv[1]);

    if (!param) {
        return enif_raise_exception(env,
                enif_make_tuple2(env, am_badarg,
                    enif_make_tuple2(env, enif_make_atom(env, "invalid_key"),
                    argv[1]))
                );
    }

    res = ZSTD_DCtx_setParameter(ctx->handle.d, param, value);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    return am_ok;
}

static ERL_NIF_TERM get_decompress_parameter_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], decompress_type);
    ZSTD_dParameter param;
    int value;
    size_t res;

    if (!ctx) {
        return make_badarg(env);
    }

    param = get_dparam(env, argv[1]);

    if (!param) {
        return enif_raise_exception(env,
                enif_make_tuple2(env, am_badarg,
                    enif_make_tuple2(env, enif_make_atom(env, "invalid_key"),
                    argv[1]))
                );
    }

    res = ZSTD_DCtx_getParameter(ctx->handle.d, param, &value);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    return enif_make_int(env, value);
}

static ERL_NIF_TERM codec_nif(ZstdCtx* ctx,
                              size_t (*codec)(ZstdCtx *, ZSTD_outBuffer *, ZSTD_inBuffer *, int),
                              ErlNifEnv *env,
                              int argc,
                              const ERL_NIF_TERM argv[],
                              size_t chunk_size) {
    ErlNifBinary output;
    ErlNifBinary input;
    ZSTD_outBuffer out_buffer;
    ZSTD_inBuffer in_buffer;
    size_t remaining;
    int flush;

    if (!enif_get_int(env, argv[1], &flush)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    if (!enif_alloc_binary(chunk_size, &output)) {
        return enif_make_badarg(env);
    }

    out_buffer.dst = output.data;
    out_buffer.size = output.size;
    out_buffer.pos = 0;
    in_buffer.src = input.data;
    in_buffer.size = input.size;
    in_buffer.pos = 0;

    do {
        remaining = codec(ctx, &out_buffer, &in_buffer, flush);

        if (ZSTD_isError(remaining)) {
            enif_release_binary(&output);
            return make_zstd_error(env, remaining);
        }
    } while ((in_buffer.pos < in_buffer.size) &&
             (out_buffer.pos < out_buffer.size));

    if (!enif_realloc_binary(&output, out_buffer.pos)) {
        enif_release_binary(&output);
        return enif_raise_exception(env, am_error);
    }


#ifndef MAX
#define MAX(A, B) ((A) > (B) ? (A) : (B))
#endif

#ifndef MIN
#define MIN(A, B) ((A) < (B) ? (A) : (B))
#endif

#define BYTES_PER_REDUCTION (1024)

    enif_consume_timeslice(env, MIN(100, MAX(1, in_buffer.pos / BYTES_PER_REDUCTION)));

    if (flush && in_buffer.pos == in_buffer.size) {
        /* If our output buffer is full and we've got remaining data, signal
         * that we need to continue. */
        ERL_NIF_TERM tag =
                ((out_buffer.pos == out_buffer.size) && remaining > 0)
                        ? am_flush
                        : am_done;

        return enif_make_tuple(env, 2, tag, enif_make_binary(env, &output));
    }

    if (in_buffer.pos < in_buffer.size) {
        return enif_make_tuple3(
                env,
                am_continue,
                enif_make_sub_binary(env,
                                     argv[0],
                                     in_buffer.pos,
                                     in_buffer.size - in_buffer.pos),
                enif_make_binary(env, &output));
    } else {
        return enif_make_tuple(env,
                               2,
                               am_continue,
                               enif_make_binary(env, &output));
    }
}

static size_t compress_stream_callback(
    ZstdCtx *ctx,
    ZSTD_outBuffer *out_buffer,
    ZSTD_inBuffer *in_buffer,
    int flush) {
    return ZSTD_compressStream2(ctx->handle.c,
                                out_buffer,
                                in_buffer,
                                (ZSTD_EndDirective)flush);
}

static ERL_NIF_TERM compress_stream_nif(ErlNifEnv *env,
                                       int argc,
                                       const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], compress_type);
#ifdef DEBUG
    const size_t COMPRESS_CHUNK_SIZE = 5;
#else
    const size_t COMPRESS_CHUNK_SIZE = ZSTD_CStreamOutSize();
#endif

    if (ctx) {

        ERL_NIF_TERM result = codec_nif(ctx, compress_stream_callback,
                        env,
                        argc - 1,
                        argv + 1,
                        COMPRESS_CHUNK_SIZE);
        if (enif_is_exception(env, result)) {
            ZSTD_CCtx_reset(ctx->handle.c, ZSTD_reset_session_only);
        }

        return result;
    }

    return make_badarg(env);
}

static size_t decompress_stream_callback(
    ZstdCtx *ctx,
    ZSTD_outBuffer *out_buffer,
    ZSTD_inBuffer *in_buffer,
    int flush) {

    return ZSTD_decompressStream(ctx->handle.d,
                                 out_buffer,
                                 in_buffer);
}

static ERL_NIF_TERM decompress_stream_nif(ErlNifEnv *env,
                                         int argc,
                                         const ERL_NIF_TERM argv[]) {
    size_t DECOMPRESS_CHUNK_SIZE = ZSTD_DStreamOutSize();
    ZstdCtx *ctx = get_zstd_context(env, argv[0], decompress_type);

#ifdef DEBUG
    DECOMPRESS_CHUNK_SIZE = 5;
#endif

    if (ctx) {
        ERL_NIF_TERM result;

        /* When starting to decompress a new stream we check if we
           can find the total decompressed size and if so use it
           as the decompress chunk size. */
        if (!ctx->in_use) {
            unsigned long long decompressedSize;
            ErlNifBinary bin;

            if (!enif_inspect_binary(env, argv[1], &bin)) {
                return enif_make_badarg(env);
            }

            /* If the context is not in use and we are trying to flush
               with an empty binary, we just return done as otherwise the
               context will be started making it impossible to set any parameters.
             */
            if (bin.size == 0 && enif_is_identical(argv[2], am_true)) {
                return enif_make_tuple2(env, am_done, enif_make_binary(env, &bin));
            }

            decompressedSize = ZSTD_findDecompressedSize(bin.data, bin.size);

            if (decompressedSize != ZSTD_CONTENTSIZE_UNKNOWN &&
                decompressedSize != ZSTD_CONTENTSIZE_ERROR &&
                decompressedSize < DECOMPRESS_CHUNK_SIZE) {
                DECOMPRESS_CHUNK_SIZE = decompressedSize;
#ifdef DEBUG
                DECOMPRESS_CHUNK_SIZE = DECOMPRESS_CHUNK_SIZE / 2;
#endif
            }
            ctx->in_use = 1;
        }

        result = codec_nif(ctx,
            decompress_stream_callback,
            env,
            argc - 1,
            argv + 1,
            DECOMPRESS_CHUNK_SIZE);
        
        if (enif_is_exception(env, result)) {
            reset_decompress_context(ctx);
        }

        return result;
    }

    return make_badarg(env);
}

static ERL_NIF_TERM compress_reset_nif(ErlNifEnv *env,
                                      int argc,
                                      const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], compress_type);

    if (ctx) {
        reset_compress_context(ctx);
    }
    return am_ok;
}

static ERL_NIF_TERM decompress_reset_nif(ErlNifEnv *env,
                                      int argc,
                                      const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], decompress_type);

    if (ctx) {
        reset_decompress_context(ctx);
    }
    return am_ok;
}

static ERL_NIF_TERM compress_close_nif(ErlNifEnv *env,
                                      int argc,
                                      const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], compress_type);

    if (ctx) {
        close_compress_context(ctx);
    }
    return am_ok;
}

static ERL_NIF_TERM decompress_close_nif(ErlNifEnv *env,
                                      int argc,
                                      const ERL_NIF_TERM argv[]) {
    ZstdCtx *ctx = get_zstd_context(env, argv[0], decompress_type);

    if (ctx) {
        close_decompress_context(ctx);
    }
    return am_ok;
}

static void compress_context_dtor(ErlNifEnv *env, void *ctx) {
    close_compress_context(ctx);
}

static void decompress_context_dtor(ErlNifEnv *env, void *ctx) {
    close_decompress_context(ctx);
}

static ERL_NIF_TERM create_cdict_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    int level;

    if (!enif_is_binary(env, argv[0]) || !enif_get_int(env, argv[1], &level)) {
        return enif_make_badarg(env);
    } else {
        ERL_NIF_TERM result, binary_copy;
        ZstdDict *dictp, dict;

        dict.env = enif_alloc_env();
        binary_copy = enif_make_copy(dict.env, argv[0]);

        (void)enif_inspect_binary(dict.env, binary_copy, &bin);
        
        dict.c = ZSTD_createCDict_advanced(bin.data, bin.size,
                    ZSTD_dlm_byRef, ZSTD_dct_auto,
                    ZSTD_getCParams(level, 0, bin.size),
                    zstd_customMem);

        if (!dict.c) {
            enif_free_env(dict.env);
            return enif_make_tuple2(env, am_error,
                enif_make_atom(env, "invalid_compress_dict"));
        }

        dictp = enif_alloc_resource(compress_dict_type, sizeof(ZstdDict));

        *dictp = dict;

        result = enif_make_resource(env, (void *)dictp);
        enif_release_resource((void *)dictp);
        return result;
    }
}

static void compress_dict_dtor(ErlNifEnv *env, void *ctx) {
    ZstdDict *dict = ctx;
    ZSTD_freeCDict(dict->c);
    enif_free_env(dict->env);
}

static ERL_NIF_TERM create_ddict_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;

    if (!enif_is_binary(env, argv[0])) {
        return enif_make_badarg(env);
    } else {
        ERL_NIF_TERM result, binary_copy;
        ZstdDict *dictp, dict;
        
        dict.env = enif_alloc_env();
        binary_copy = enif_make_copy(dict.env, argv[0]);

        (void)enif_inspect_binary(dict.env, binary_copy, &bin);
        
        dict.d = ZSTD_createDDict_advanced(bin.data, bin.size,
                    ZSTD_dlm_byRef, ZSTD_dct_auto, zstd_customMem);

        if (!dict.d) {
            enif_free_env(dict.env);
            return enif_make_tuple2(env, am_error,
                enif_make_atom(env, "invalid_decompress_dict"));
        }

        dictp = enif_alloc_resource(decompress_dict_type, sizeof(ZstdDict));

        *dictp = dict;

        result = enif_make_resource(env, (void *)dictp);
        enif_release_resource((void *)dictp);
        return result;
    }
}

static void decompress_dict_dtor(ErlNifEnv *env, void *ctx) {
    ZstdDict *dict = ctx;
    ZSTD_freeDDict(dict->d);
    enif_free_env(dict->env);
}

static ERL_NIF_TERM getDictId_fromCDict_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdDict *dict;

    if (!enif_get_resource(env, argv[0], compress_dict_type, (void **)&dict)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, ZSTD_getDictID_fromCDict(dict->c));
}

static ERL_NIF_TERM getDictId_fromDDict_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ZstdDict *dict;

    if (!enif_get_resource(env, argv[0], decompress_dict_type, (void **)&dict)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, ZSTD_getDictID_fromDDict(dict->d));
}

static ERL_NIF_TERM getDictId_fromDict_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, ZSTD_getDictID_fromDict(bin.data, bin.size));
}

static ERL_NIF_TERM getDictId_fromFrame_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint(env, ZSTD_getDictID_fromFrame(bin.data, bin.size));
}

static ERL_NIF_TERM get_frame_header_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    ZSTD_frameHeader hdr;
    ERL_NIF_TERM map;
    ERL_NIF_TERM keys[] = {
        enif_make_atom(env, "frameContentSize"),
        enif_make_atom(env, "windowSize"),
        enif_make_atom(env, "blockSizeMax"),
        enif_make_atom(env, "frameType"),
        enif_make_atom(env, "headerSize"),
        enif_make_atom(env, "dictID"),
        enif_make_atom(env, "checksumFlag")
    };
    ERL_NIF_TERM values[sizeof(keys) / sizeof(*keys)];
    size_t res;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    res = ZSTD_getFrameHeader(&hdr, bin.data, bin.size);

    if (ZSTD_isError(res)) {
        return make_zstd_error(env, res);
    }

    values[0] = hdr.frameContentSize == ZSTD_CONTENTSIZE_UNKNOWN ? am_undefined
        : enif_make_ulong(env, hdr.frameContentSize);
    values[1] = enif_make_ulong(env, hdr.windowSize);
    values[2] = enif_make_uint(env, hdr.blockSizeMax);
    values[3] = hdr.frameType == ZSTD_frame ? enif_make_atom(env, "ZSTD_frame") :
        enif_make_atom(env, "ZSTD_skippableFrame");
    values[4] = enif_make_uint(env, hdr.headerSize);
    values[5] = enif_make_uint(env, hdr.dictID);
    values[6] = hdr.checksumFlag ? am_true : am_false;

    if (!enif_make_map_from_arrays(env, keys, values, sizeof(keys) / sizeof(*keys), &map)) {
        return enif_make_badarg(env);
    }

    return enif_make_tuple2(env, am_ok, map);
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg) {
    ErlNifResourceTypeInit callbacks;

    memset(&callbacks, 0, sizeof(callbacks));

    callbacks.members = 3;

    callbacks.dtor = compress_context_dtor;

    compress_type = enif_init_resource_type(env,
                                            "ZstdCompressContext",
                                            &callbacks,
                                            ERL_NIF_RT_CREATE,
                                            NULL);

    callbacks.dtor = decompress_context_dtor;

    decompress_type = enif_init_resource_type(env,
                                              "ZstdDecompressContext",
                                              &callbacks,
                                              ERL_NIF_RT_CREATE,
                                              NULL);

    callbacks.dtor = compress_dict_dtor;

    compress_dict_type = enif_init_resource_type(env,
                                                 "ZstdCompressDict",
                                                 &callbacks,
                                                 ERL_NIF_RT_CREATE,
                                                 NULL);

    callbacks.dtor = decompress_dict_dtor;

    decompress_dict_type = enif_init_resource_type(env,
                                                   "ZstdCompressDict",
                                                   &callbacks,
                                                   ERL_NIF_RT_CREATE,
                                                   NULL);

    am_true = enif_make_atom(env, "true");
    am_false = enif_make_atom(env, "false");
    am_undefined = enif_make_atom(env, "undefined");

    am_badarg = enif_make_atom(env, "badarg");
    am_error = enif_make_atom(env, "error");
    am_ok = enif_make_atom(env, "ok");

    am_continue = enif_make_atom(env, "continue");
    am_flush = enif_make_atom(env, "flush");
    am_done = enif_make_atom(env, "done");

    *priv_data = NULL;

    (void)arg;

    /* Compile-time checks to ensure ZSTD_EndDirective values
     * match Erlang defines. */
    ERTS_CT_ASSERT(ZSTD_e_continue == 0);
    ERTS_CT_ASSERT(ZSTD_e_flush == 1);
    ERTS_CT_ASSERT(ZSTD_e_end == 2);

    return 0;
}

static void unload(ErlNifEnv *env, void *priv_data) {
    (void)priv_data;
    (void)env;
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data,
    void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static ErlNifFunc nif_funcs[] = {
        {"init_compress_nif", 0, init_compress_nif, 0},
        {"init_decompress_nif", 0, init_decompress_nif, 0},

        {"load_compress_dictionary_nif", 2, load_compress_dictionary_nif,
            ERL_NIF_DIRTY_JOB_CPU_BOUND},
        {"ref_compress_dictionary_nif", 2, ref_compress_dictionary_nif, 0},
        {"set_compress_parameter_nif", 3, set_compress_parameter_nif, 0},
        {"get_compress_parameter_nif", 2, get_compress_parameter_nif, 0},
        {"set_pledged_src_size_nif", 2, set_pledged_src_size_nif, 0},

        {"load_decompress_dictionary_nif", 2, load_decompress_dictionary_nif,
            ERL_NIF_DIRTY_JOB_CPU_BOUND},
        {"ref_decompress_dictionary_nif", 2, ref_decompress_dictionary_nif, 0},
        {"set_decompress_parameter_nif", 3, set_decompress_parameter_nif, 0},
        {"get_decompress_parameter_nif", 2, get_decompress_parameter_nif, 0},

        {"compress_stream_nif",  3, compress_stream_nif, 0},
        {"decompress_stream_nif", 3, decompress_stream_nif, 0},

        {"compress_reset_nif", 1, compress_reset_nif, 0},
        {"decompress_reset_nif", 1, decompress_reset_nif, 0},

        {"compress_close_nif", 1, compress_close_nif, 0},
        {"decompress_close_nif", 1, decompress_close_nif, 0},

        {"create_cdict_nif", 2, create_cdict_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
        {"create_ddict_nif", 1, create_ddict_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
        {"getDictId_fromCDict_nif", 1, getDictId_fromCDict_nif, 0},
        {"getDictId_fromDDict_nif", 1, getDictId_fromDDict_nif, 0},
        {"getDictId_fromDict_nif", 1, getDictId_fromDict_nif, 0},
        {"getDictId_fromFrame_nif", 1, getDictId_fromFrame_nif, 0},

        {"get_frame_header_nif", 1, get_frame_header_nif, 0}
};

ERL_NIF_INIT(zstd, nif_funcs, load, NULL, upgrade, unload)
