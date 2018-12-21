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
    if (hmac_context_rtype == NULL) {
        PRINTF_ERR0("CRYPTO: Could not open resource type 'hmac_context'");
        return 0;
    }
    return 1;
}

ERL_NIF_TERM hmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key, Data) or (Type, Key, Data, MacSize) */
    struct digest_type_t *digp = NULL;
    ErlNifBinary         key, data;
    unsigned char        buff[EVP_MAX_MD_SIZE];
    unsigned             size = 0, req_size = 0;
    ERL_NIF_TERM         ret;

    digp = get_digest_type(argv[0]);
    if (!digp ||
        !enif_inspect_iolist_as_binary(env, argv[1], &key) ||
        !enif_inspect_iolist_as_binary(env, argv[2], &data) ||
        (argc == 4 && !enif_get_uint(env, argv[3], &req_size))) {
        return enif_make_badarg(env);
    }

    if (!digp->md.p ||
        !HMAC(digp->md.p,
              key.data, key.size,
              data.data, data.size,
              buff, &size)) {
        return atom_notsup;
    }
    ASSERT(0 < size && size <= EVP_MAX_MD_SIZE);
    CONSUME_REDS(env, data);

    if (argc == 4) {
        if (req_size <= size) {
            size = req_size;
        }
        else {
            return enif_make_badarg(env);
        }
    }
    memcpy(enif_make_new_binary(env, size, &ret), buff, size);
    return ret;
}

static void hmac_context_dtor(ErlNifEnv* env, struct hmac_context *obj)
{
    if (obj->alive) {
	HMAC_CTX_free(obj->ctx);
	obj->alive = 0;
    }
    enif_mutex_destroy(obj->mtx);
}

ERL_NIF_TERM hmac_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key) */
    struct digest_type_t *digp = NULL;
    ErlNifBinary         key;
    ERL_NIF_TERM         ret;
    struct hmac_context  *obj;

    digp = get_digest_type(argv[0]);
    if (!digp ||
        !enif_inspect_iolist_as_binary(env, argv[1], &key)) {
        return enif_make_badarg(env);
    }
    if (!digp->md.p) {
        return atom_notsup;
    }

    obj = enif_alloc_resource(hmac_context_rtype, sizeof(struct hmac_context));
    obj->mtx = enif_mutex_create("crypto.hmac");
    obj->alive = 1;
    obj->ctx = HMAC_CTX_new();
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
    // Check the return value of HMAC_Init: it may fail in FIPS mode
    // for disabled algorithms
    if (!HMAC_Init_ex(obj->ctx, key.data, key.size, digp->md.p, NULL)) {
        enif_release_resource(obj);
        return atom_notsup;
    }
#else
    HMAC_Init_ex(obj->ctx, key.data, key.size, digp->md.p, NULL);
#endif

    ret = enif_make_resource(env, obj);
    enif_release_resource(obj);
    return ret;
}

ERL_NIF_TERM hmac_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    ErlNifBinary data;
    struct hmac_context* obj;

    if (!enif_get_resource(env, argv[0], hmac_context_rtype, (void**)&obj)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
	return enif_make_badarg(env);
    }
    enif_mutex_lock(obj->mtx);
    if (!obj->alive) {
	enif_mutex_unlock(obj->mtx);
	return enif_make_badarg(env);
    }
    HMAC_Update(obj->ctx, data.data, data.size);
    enif_mutex_unlock(obj->mtx);

    CONSUME_REDS(env,data);
    return argv[0];
}

ERL_NIF_TERM hmac_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) or (Context, HashLen) */
    ERL_NIF_TERM ret;
    struct hmac_context* obj;
    unsigned char mac_buf[EVP_MAX_MD_SIZE];
    unsigned char * mac_bin;
    unsigned int req_len = 0;
    unsigned int mac_len;

    if (!enif_get_resource(env,argv[0],hmac_context_rtype, (void**)&obj)
	|| (argc == 2 && !enif_get_uint(env, argv[1], &req_len))) {
	return enif_make_badarg(env);
    }

    enif_mutex_lock(obj->mtx);
    if (!obj->alive) {
	enif_mutex_unlock(obj->mtx);
	return enif_make_badarg(env);
    }

    HMAC_Final(obj->ctx, mac_buf, &mac_len);
    HMAC_CTX_free(obj->ctx);
    obj->alive = 0;
    enif_mutex_unlock(obj->mtx);

    if (argc == 2 && req_len < mac_len) {
        /* Only truncate to req_len bytes if asked. */
        mac_len = req_len;
    }
    mac_bin = enif_make_new_binary(env, mac_len, &ret);
    memcpy(mac_bin, mac_buf, mac_len);

    return ret;
}

