/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2024. All Rights Reserved.
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

#include "hash.h"
#include "digest.h"
#include "info.h"

#ifdef HAVE_MD5
#  define MD5_CTX_LEN       (sizeof(MD5_CTX))
#endif
#ifdef HAVE_MD4
#  define MD4_CTX_LEN       (sizeof(MD4_CTX))
#endif
#ifdef HAVE_RIPEMD160
#  define RIPEMD160_CTX_LEN (sizeof(RIPEMD160_CTX))
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
struct evp_md_ctx {
    EVP_MD_CTX* ctx;
};

/* Define resource types for OpenSSL context structures. */
static ErlNifResourceType* evp_md_ctx_rtype;

static void evp_md_ctx_dtor(ErlNifEnv* env, struct evp_md_ctx *ctx) {
    if (ctx == NULL)
        return;

    if (ctx->ctx)
        EVP_MD_CTX_free(ctx->ctx);
}
#endif

int init_hash_ctx(ErlNifEnv* env, ErlNifBinary* rt_buf) {
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
    evp_md_ctx_rtype = enif_open_resource_type(env, NULL,
                                               resource_name("EVP_MD_CTX", rt_buf),
                                               (ErlNifResourceDtor*) evp_md_ctx_dtor,
                                               ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                               NULL);
    if (evp_md_ctx_rtype == NULL)
        goto err;
#endif

    return 1;

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_MD_CTX'");
    return 0;
#endif
}

ERL_NIF_TERM hash_info_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    struct digest_type_t *digp = NULL;
    const EVP_MD         *md;
    ERL_NIF_TERM keys[3] = { atom_type, atom_size, atom_block_size };
    ERL_NIF_TERM values[3];
    ERL_NIF_TERM ret;
    int ok;

    ASSERT(argc == 1);

    if ((digp = get_digest_type(argv[0])) == NULL)
        return enif_make_badarg(env);
    if (DIGEST_FORBIDDEN_IN_FIPS(digp))
        return RAISE_NOTSUP(env);

    if ((md = digp->md.p) == NULL)
        return RAISE_NOTSUP(env);

    values[0] = enif_make_int(env, EVP_MD_type(md));
    values[1] = enif_make_int(env, EVP_MD_size(md));
    values[2] = enif_make_int(env, EVP_MD_block_size(md));
    ok = enif_make_map_from_arrays(env, keys, values, 3, &ret);
    ASSERT(ok); (void)ok;
    return ret;
}

ERL_NIF_TERM hash_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Data) */
    struct digest_type_t *digp = NULL;
    const EVP_MD         *md;
    ErlNifBinary         data;
    ERL_NIF_TERM         ret;
    unsigned             ret_size;
    unsigned char        *outp;

    if ((digp = get_digest_type(argv[0])) == NULL)
        return EXCP_BADARG_N(env, 0, "Bad digest type");
    if (DIGEST_FORBIDDEN_IN_FIPS(digp))
        return EXCP_NOTSUP_N(env, 0, "Bad digest type in FIPS");
    if ((md = digp->md.p) == NULL)
        return EXCP_NOTSUP_N(env, 0, "Digest type not supported in this cryptolib");

    if (!enif_inspect_iolist_as_binary(env, argv[1], &data))
        return EXCP_BADARG_N(env, 1, "Not iolist");


    ret_size = (unsigned)EVP_MD_size(md);
    ASSERT(0 < ret_size && ret_size <= EVP_MAX_MD_SIZE);

    if ((outp = enif_make_new_binary(env, ret_size, &ret)) == NULL)
        return EXCP_ERROR(env, "Can't allocate binary");

    if (EVP_Digest(data.data, data.size, outp, &ret_size, md, NULL) != 1)
        return EXCP_ERROR(env, "Low-level call failed");

    ASSERT(ret_size == (unsigned)EVP_MD_size(md));

    CONSUME_REDS(env, data);
    return ret;
}

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)

ERL_NIF_TERM hash_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    struct digest_type_t *digp = NULL;
    struct evp_md_ctx    *ctx = NULL;
    ERL_NIF_TERM         ret;

    if ((digp = get_digest_type(argv[0])) == NULL)
        return EXCP_BADARG_N(env, 0, "Bad digest type");

    if (DIGEST_FORBIDDEN_IN_FIPS(digp))
        return EXCP_NOTSUP_N(env, 0, "Digest type not supported in FIPS");
    if (digp->md.p == NULL)
        return EXCP_NOTSUP_N(env, 0, "Unsupported digest type");

    if ((ctx = enif_alloc_resource(evp_md_ctx_rtype, sizeof(struct evp_md_ctx))) == NULL)
        return EXCP_ERROR(env, "Can't allocate nif resource");
    if ((ctx->ctx = EVP_MD_CTX_new()) == NULL)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_MD_CTX_new failed"));
    if (EVP_DigestInit(ctx->ctx, digp->md.p) != 1)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_DigestInit failed"));

    ret = enif_make_resource(env, ctx);

 done:
    if (ctx)
        enif_release_resource(ctx);
    return ret;
}

ERL_NIF_TERM hash_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    struct evp_md_ctx   *ctx, *new_ctx = NULL;
    ErlNifBinary data;
    ERL_NIF_TERM ret;

    if (!enif_get_resource(env, argv[0], evp_md_ctx_rtype, (void**)&ctx))
        return EXCP_BADARG_N(env, 0, "Bad state");

    if (!enif_inspect_iolist_as_binary(env, argv[1], &data))
        return EXCP_BADARG_N(env, 1, "Not iolist");

    if ((new_ctx = enif_alloc_resource(evp_md_ctx_rtype, sizeof(struct evp_md_ctx))) == NULL)
        return EXCP_ERROR(env, "Can't allocate nif resource");
    if ((new_ctx->ctx = EVP_MD_CTX_new()) == NULL)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_MD_CTX_new failed"));
    if (EVP_MD_CTX_copy(new_ctx->ctx, ctx->ctx) != 1)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_MD_CTX_copy failed"));
    if (EVP_DigestUpdate(new_ctx->ctx, data.data, data.size) != 1)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_DigestUpdate failed"));

    ret = enif_make_resource(env, new_ctx);
    CONSUME_REDS(env, data);

 done:
    if (new_ctx)
        enif_release_resource(new_ctx);
    return ret;
}

ERL_NIF_TERM hash_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
    struct evp_md_ctx *ctx;
    EVP_MD_CTX        *new_ctx;
    ERL_NIF_TERM  ret;
    unsigned      ret_size;
    unsigned char     *outp;

    ASSERT(argc == 1);

    if (!enif_get_resource(env, argv[0], evp_md_ctx_rtype, (void**)&ctx))
        return EXCP_BADARG_N(env, 0, "Bad state");

    ret_size = (unsigned)EVP_MD_CTX_size(ctx->ctx);
    ASSERT(0 < ret_size && ret_size <= EVP_MAX_MD_SIZE);

    if ((new_ctx = EVP_MD_CTX_new()) == NULL)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_MD_CTX_new failed"));
    if (EVP_MD_CTX_copy(new_ctx, ctx->ctx) != 1)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_MD_CTX_copy failed"));
    if ((outp = enif_make_new_binary(env, ret_size, &ret)) == NULL)
        assign_goto(ret, done, EXCP_ERROR(env, "Can't make a new binary"));
    if (EVP_DigestFinal(new_ctx, outp, &ret_size) != 1)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_DigestFinal failed"));

    ASSERT(ret_size == (unsigned)EVP_MD_CTX_size(ctx->ctx));

 done:
    if (new_ctx)
        EVP_MD_CTX_free(new_ctx);
    return ret;
}

#else /* if OPENSSL_VERSION_NUMBER < 1.0 */

ERL_NIF_TERM hash_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    typedef int (*init_fun)(unsigned char*);
    struct digest_type_t *digp = NULL;
    ERL_NIF_TERM         ctx;
    size_t               ctx_size = 0;
    init_fun             ctx_init = 0;
    unsigned char        *outp;

    ASSERT(argc == 1);

    if ((digp = get_digest_type(argv[0])) == NULL)
        return EXCP_BADARG_N(env, 0, "Bad digest type");
    
    if (DIGEST_FORBIDDEN_IN_FIPS(digp))
        return EXCP_NOTSUP_N(env, 0, "Digest type not supported in FIPS");
    if (digp->md.p == NULL)
        return EXCP_NOTSUP_N(env, 0, "Unsupported digest type");

    switch (EVP_MD_type(digp->md.p))
    {
#ifdef HAVE_MD4
    case NID_md4:
        ctx_size = MD4_CTX_LEN;
        ctx_init = (init_fun)(&MD4_Init);
        break;
#endif
#ifdef HAVE_MD5
    case NID_md5:
        ctx_size = MD5_CTX_LEN;
        ctx_init = (init_fun)(&MD5_Init);
        break;
#endif
#ifdef HAVE_RIPEMD160
    case NID_ripemd160:
        ctx_size = RIPEMD160_CTX_LEN;
        ctx_init = (init_fun)(&RIPEMD160_Init);
        break;
#endif
    case NID_sha1:
        ctx_size = sizeof(SHA_CTX);
        ctx_init = (init_fun)(&SHA1_Init);
        break;
#ifdef HAVE_SHA224
    case NID_sha224:
        ctx_size = sizeof(SHA256_CTX);
        ctx_init = (init_fun)(&SHA224_Init);
        break;
#endif
#ifdef HAVE_SHA256
    case NID_sha256:
        ctx_size = sizeof(SHA256_CTX);
        ctx_init = (init_fun)(&SHA256_Init);
        break;
#endif
#ifdef HAVE_SHA384
    case NID_sha384:
        ctx_size = sizeof(SHA512_CTX);
        ctx_init = (init_fun)(&SHA384_Init);
        break;
#endif
#ifdef HAVE_SHA512
    case NID_sha512:
        ctx_size = sizeof(SHA512_CTX);
        ctx_init = (init_fun)(&SHA512_Init);
        break;
#endif
    default:
        return EXCP_NOTSUP_N(env, 0, "Unsupported digest type");
    }
    ASSERT(ctx_size);
    ASSERT(ctx_init);

    if ((outp = enif_make_new_binary(env, ctx_size, &ctx)) == NULL)
        return EXCP_ERROR(env, "Can't allocate binary");

    if (ctx_init(outp) != 1)
        return EXCP_ERROR(env, "Can't init ctx");

    return enif_make_tuple2(env, argv[0], ctx);
}

ERL_NIF_TERM hash_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* ({Type, Context}, Data) */
    typedef int (*update_fun)(unsigned char*, const unsigned char*, size_t);
    ERL_NIF_TERM         new_ctx;
    ErlNifBinary         ctx, data;
    const ERL_NIF_TERM   *tuple;
    int                  arity;
    struct digest_type_t *digp = NULL;
    unsigned char        *ctx_buff;
    size_t               ctx_size   = 0;
    update_fun           ctx_update = 0;

    if (!enif_get_tuple(env, argv[0], &arity, &tuple))
        return EXCP_BADARG_N(env, 0, "Bad state");
    if (arity != 2)
        return EXCP_BADARG_N(env, 0, "Bad state");
    if ((digp = get_digest_type(tuple[0])) == NULL)
        return EXCP_BADARG_N(env, 0, "Bad state");
    if (DIGEST_FORBIDDEN_IN_FIPS(digp))
        return EXCP_BADARG_N(env, 0, "Bad state");
    if (digp->md.p == NULL)
        return EXCP_BADARG_N(env, 0, "Bad state");
    if (!enif_inspect_binary(env, tuple[1], &ctx))
        return EXCP_BADARG_N(env, 0, "Bad state");
    
    if (!enif_inspect_iolist_as_binary(env, argv[1], &data))
        return EXCP_BADARG_N(env, 0, "Bad data");

    switch (EVP_MD_type(digp->md.p))
    {
#ifdef HAVE_MD4
    case NID_md4:
        ctx_size   = MD4_CTX_LEN;
        ctx_update = (update_fun)(&MD4_Update);
        break;
#endif
#ifdef HAVE_MD5
    case NID_md5:
        ctx_size   = MD5_CTX_LEN;
        ctx_update = (update_fun)(&MD5_Update);
        break;
#endif
#ifdef HAVE_RIPEMD160
    case NID_ripemd160:
        ctx_size   = RIPEMD160_CTX_LEN;
        ctx_update = (update_fun)(&RIPEMD160_Update);
        break;
#endif
    case NID_sha1:
        ctx_size   = sizeof(SHA_CTX);
        ctx_update = (update_fun)(&SHA1_Update);
        break;
#ifdef HAVE_SHA224
    case NID_sha224:
        ctx_size   = sizeof(SHA256_CTX);
        ctx_update = (update_fun)(&SHA224_Update);
        break;
#endif
#ifdef HAVE_SHA256
    case NID_sha256:
        ctx_size   = sizeof(SHA256_CTX);
        ctx_update = (update_fun)(&SHA256_Update);
        break;
#endif
#ifdef HAVE_SHA384
    case NID_sha384:
        ctx_size   = sizeof(SHA512_CTX);
        ctx_update = (update_fun)(&SHA384_Update);
        break;
#endif
#ifdef HAVE_SHA512
    case NID_sha512:
        ctx_size   = sizeof(SHA512_CTX);
        ctx_update = (update_fun)(&SHA512_Update);
        break;
#endif
    default:
        return EXCP_BADARG_N(env, 0, "Bad state");
    }
    ASSERT(ctx_size);
    ASSERT(ctx_update);

    if (ctx.size != ctx_size)
        return EXCP_BADARG_N(env, 0, "Bad state");

    if ((ctx_buff = enif_make_new_binary(env, ctx_size, &new_ctx)) == NULL)
        return EXCP_ERROR(env, "Can't allocate binary");

    memcpy(ctx_buff, ctx.data, ctx_size);

    if (ctx_update(ctx_buff, data.data, data.size) != 1)
        return EXCP_ERROR(env, "Can't update");

    CONSUME_REDS(env, data);
    return enif_make_tuple2(env, tuple[0], new_ctx);
}

ERL_NIF_TERM hash_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* ({Type, Context}) */
    typedef int (*final_fun)(unsigned char*, void*);
    ERL_NIF_TERM         ret;
    ErlNifBinary         ctx;
    const ERL_NIF_TERM   *tuple;
    int                  arity;
    struct digest_type_t *digp = NULL;
    const EVP_MD         *md;
    void                 *new_ctx = NULL;
    size_t               ctx_size  = 0;
    final_fun            ctx_final = 0;
    unsigned char        *outp;

    if (!enif_get_tuple(env, argv[0], &arity, &tuple))
        return EXCP_BADARG_N(env, 0, "Bad state");
    if (arity != 2)
        return EXCP_BADARG_N(env, 0, "Bad state");
    if ((digp = get_digest_type(tuple[0])) == NULL)
        return EXCP_BADARG_N(env, 0, "Bad state");
    if (DIGEST_FORBIDDEN_IN_FIPS(digp))
        return EXCP_BADARG_N(env, 0, "Bad state");
    if ((md = digp->md.p) == NULL)
        return EXCP_BADARG_N(env, 0, "Bad state");

    if (!enif_inspect_binary(env, tuple[1], &ctx))
        return EXCP_BADARG_N(env, 0, "Bad data");

    switch (EVP_MD_type(md))
    {
#ifdef HAVE_MD4
    case NID_md4:
        ctx_size  = MD4_CTX_LEN;
        ctx_final = (final_fun)(&MD4_Final);
        break;
#endif
#ifdef HAVE_MD5
    case NID_md5:
        ctx_size  = MD5_CTX_LEN;
        ctx_final = (final_fun)(&MD5_Final);
        break;
#endif
#ifdef HAVE_RIPEMD160
   case NID_ripemd160:
        ctx_size  = RIPEMD160_CTX_LEN;
        ctx_final = (final_fun)(&RIPEMD160_Final);
        break;
#endif
    case NID_sha1:
        ctx_size  = sizeof(SHA_CTX);
        ctx_final = (final_fun)(&SHA1_Final);
        break;
#ifdef HAVE_SHA224
    case NID_sha224:
        ctx_size  = sizeof(SHA256_CTX);
        ctx_final = (final_fun)(&SHA224_Final);
        break;
#endif
#ifdef HAVE_SHA256
    case NID_sha256:
        ctx_size  = sizeof(SHA256_CTX);
        ctx_final = (final_fun)(&SHA256_Final);
        break;
#endif
#ifdef HAVE_SHA384
    case NID_sha384:
        ctx_size  = sizeof(SHA512_CTX);
        ctx_final = (final_fun)(&SHA384_Final);
        break;
#endif
#ifdef HAVE_SHA512
    case NID_sha512:
        ctx_size  = sizeof(SHA512_CTX);
        ctx_final = (final_fun)(&SHA512_Final);
        break;
#endif
    default:
        return EXCP_BADARG_N(env, 0, "Bad state");
    }
    ASSERT(ctx_size);
    ASSERT(ctx_final);

    if (ctx.size != ctx_size)
        return EXCP_BADARG_N(env, 0, "Bad state");

    if ((new_ctx = enif_alloc(ctx_size)) == NULL)
        return EXCP_ERROR(env, "Can't allocate");

    memcpy(new_ctx, ctx.data, ctx_size);

    if ((outp = enif_make_new_binary(env, (size_t)EVP_MD_size(md), &ret)) == NULL)
        assign_goto(ret, done, EXCP_ERROR(env, "Can't allocate binary"));

    if (ctx_final(outp, new_ctx) != 1)
        assign_goto(ret, done, EXCP_ERROR(env, "Can't do final"));

 done:
    if (new_ctx)
        enif_free(new_ctx);
    return ret;
}

#endif  /* OPENSSL_VERSION_NUMBER < 1.0 */

#if defined(HAVE_SHAKE128) || defined(HAVE_SHAKE256)
ERL_NIF_TERM hash_final_xof_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
    struct evp_md_ctx   *ctx;
    EVP_MD_CTX          *new_ctx;
    ERL_NIF_TERM        ret;
    unsigned char       *outp;
    unsigned int        len;

    ASSERT(argc == 2);
    if (!enif_get_resource(env, argv[0], evp_md_ctx_rtype, (void**)&ctx))
        return EXCP_BADARG_N(env, 0, "Bad state");
    if (!enif_get_uint(env, argv[1], &len))
        return EXCP_BADARG_N(env, 1, "Bad len");
    ASSERT(0 < len);

    if ((new_ctx = EVP_MD_CTX_new()) == NULL)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_MD_CTX_new failed"));
    if (EVP_MD_CTX_copy(new_ctx, ctx->ctx) != 1)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_MD_CTX_copy failed"));
    if ((outp = enif_make_new_binary(env, len>>3, &ret)) == NULL)
        assign_goto(ret, done, EXCP_ERROR(env, "Can't make a new binary"));
    if (EVP_DigestFinalXOF(new_ctx, outp, len>>3) != 1)
        assign_goto(ret, done, EXCP_ERROR(env, "Low-level call EVP_DigestFinalXOF failed"));

 done:
    if (new_ctx)
        EVP_MD_CTX_free(new_ctx);
    return ret;
}
#else
ERL_NIF_TERM hash_final_xof_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return EXCP_NOTSUP(env, "Low-level EVP_DigestFinalXOF function is not supported in this cryptolib");
}
#endif /* defined(HAVE_SHAKE128) || defined(HAVE_SHAKE256) */
