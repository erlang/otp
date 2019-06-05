/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2018. All Rights Reserved.
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


/*****************************************************************
 *
 * This file has functions for compatibility with cryptolibs
 * lacking the EVP_Digest API.
 *
 * See mac.c for the implementation using the EVP interface.
 *
 ****************************************************************/

#ifndef HAS_EVP_PKEY_CTX

#include "hmac.h"
#include "digest.h"

struct hmac_context
{
    ErlNifMutex* mtx;
    int alive;
    HMAC_CTX* ctx;
};

static ErlNifResourceType* hmac_context_rtype;

static void hmac_context_dtor(ErlNifEnv* env, struct hmac_context*);

int init_hmac_ctx(ErlNifEnv *env) {
    hmac_context_rtype = enif_open_resource_type(env, NULL, "hmac_context",
						 (ErlNifResourceDtor*) hmac_context_dtor,
						 ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
						 NULL);
    if (hmac_context_rtype == NULL)
        goto err;

    return 1;

 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'hmac_context'");
    return 0;
}

static void hmac_context_dtor(ErlNifEnv* env, struct hmac_context *obj)
{
    if (obj == NULL)
        return;

    if (obj->alive) {
        if (obj->ctx)
            HMAC_CTX_free(obj->ctx);
	obj->alive = 0;
    }

    if (obj->mtx != NULL)
        enif_mutex_destroy(obj->mtx);
}

ERL_NIF_TERM hmac_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (hmac, Type, Key) */
    struct digest_type_t *digp = NULL;
    ErlNifBinary         key;
    ERL_NIF_TERM         ret;
    struct hmac_context  *obj = NULL;

    ASSERT(argc == 3);

    if ((digp = get_digest_type(argv[1])) == NULL)
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[2], &key))
        goto bad_arg;
    if (key.size > INT_MAX)
        goto bad_arg;

    if (digp->md.p == NULL)
        goto err;

    if ((obj = enif_alloc_resource(hmac_context_rtype, sizeof(struct hmac_context))) == NULL)
        goto err;
    obj->ctx = NULL;
    obj->mtx = NULL;
    obj->alive = 0;

    if ((obj->ctx = HMAC_CTX_new()) == NULL)
        goto err;
    obj->alive = 1;
    if ((obj->mtx = enif_mutex_create("crypto.hmac")) == NULL)
        goto err;

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
    // Check the return value of HMAC_Init: it may fail in FIPS mode
    // for disabled algorithms
    if (!HMAC_Init_ex(obj->ctx, key.data, (int)key.size, digp->md.p, NULL))
        goto err;
#else
    // In ancient versions of OpenSSL, this was a void function.
    HMAC_Init_ex(obj->ctx, key.data, (int)key.size, digp->md.p, NULL);
#endif

    ret = enif_make_resource(env, obj);
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    ret = atom_notsup;

 done:
    if (obj)
        enif_release_resource(obj);
    return ret;
}

ERL_NIF_TERM hmac_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    ERL_NIF_TERM ret;
    ErlNifBinary data;
    struct hmac_context *obj = NULL;

    ASSERT(argc == 2);

    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)hmac_context_rtype, (void**)&obj))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &data))
        goto bad_arg;

    enif_mutex_lock(obj->mtx);
    if (!obj->alive)
        goto err;

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
    if (!HMAC_Update(obj->ctx, data.data, data.size))
        goto err;
#else
    // In ancient versions of OpenSSL, this was a void function.
    HMAC_Update(obj->ctx, data.data, data.size);
#endif

    CONSUME_REDS(env,data);
    ret = argv[0];
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    ret = enif_make_badarg(env);

 done:
    enif_mutex_unlock(obj->mtx);
    return ret;
}

ERL_NIF_TERM hmac_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) or (Context, HashLen) */
    ERL_NIF_TERM ret;
    struct hmac_context* obj;
    unsigned char mac_buf[EVP_MAX_MD_SIZE];
    unsigned char * mac_bin;
    unsigned int req_len = 0;
    unsigned int mac_len;

    ASSERT(argc == 1 || argc == 2);

    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)hmac_context_rtype, (void**)&obj))
        goto bad_arg;
    if (argc == 2) {
        if (!enif_get_uint(env, argv[1], &req_len))
            goto bad_arg;
    }

    enif_mutex_lock(obj->mtx);
    if (!obj->alive)
        goto err;

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
    if (!HMAC_Final(obj->ctx, mac_buf, &mac_len))
        goto err;
#else
    // In ancient versions of OpenSSL, this was a void function.
    HMAC_Final(obj->ctx, mac_buf, &mac_len);
#endif

    if (obj->ctx)
        HMAC_CTX_free(obj->ctx);
    obj->alive = 0;

    if (argc == 2 && req_len < mac_len) {
        /* Only truncate to req_len bytes if asked. */
        mac_len = req_len;
    }
    if ((mac_bin = enif_make_new_binary(env, mac_len, &ret)) == NULL)
        goto err;

    memcpy(mac_bin, mac_buf, mac_len);
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    ret = enif_make_badarg(env);

 done:
    enif_mutex_unlock(obj->mtx);
    return ret;
}



int hmac_low_level(ErlNifEnv* env, const EVP_MD *md,
                   ErlNifBinary key_bin, ErlNifBinary text,
                   ErlNifBinary *ret_bin, int *ret_bin_alloc, ERL_NIF_TERM *return_term)
{
    unsigned int size_int;
    size_t size;

    /* Find the needed space */
    if (HMAC(md,
             key_bin.data, (int)key_bin.size,
             text.data, text.size,
             NULL, &size_int) == NULL)
        {
            *return_term = EXCP_ERROR(env, "Get HMAC size failed");
            return 0;
        }

    size = (size_t)size_int; /* Otherwise "size" is unused in 0.9.8.... */
    if (!enif_alloc_binary(size, ret_bin))
        {
            *return_term = EXCP_ERROR(env, "Alloc binary");
            return 0;
        }
    *ret_bin_alloc = 1;

    /* And do the real HMAC calc */
    if (HMAC(md,
             key_bin.data, (int)key_bin.size,
             text.data, text.size,
             ret_bin->data, &size_int) == NULL)
        {
            *return_term = EXCP_ERROR(env, "HMAC sign failed");
            return 0;
        }
                    
    return 1;
}

#endif
