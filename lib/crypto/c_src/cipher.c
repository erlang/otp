/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

#include "cipher.h"
#include "info.h"
#include "evp.h"
#include "algorithms_cipher.h"

ErlNifResourceType* evp_cipher_ctx_rtype;

static void evp_cipher_ctx_dtor(ErlNifEnv* env, struct evp_cipher_ctx* ctx) {
    if (ctx == NULL)
        return;

    if (ctx->ctx)
        EVP_CIPHER_CTX_free(ctx->ctx);

#if !defined(HAVE_EVP_AES_CTR)
    if (ctx->env)
        enif_free_env(ctx->env);
#endif
}

int init_cipher_ctx(ErlNifEnv *env, ErlNifBinary* rt_buf) {
    evp_cipher_ctx_rtype = enif_open_resource_type(env, NULL,
                                                   resource_name("EVP_CIPHER_CTX", rt_buf),
                                                   (ErlNifResourceDtor*) evp_cipher_ctx_dtor,
                                                   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                   NULL);
    if (evp_cipher_ctx_rtype == NULL)
        goto err;

    return 1;

 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_CIPHER_CTX'");
    return 0;
}

ERL_NIF_TERM cipher_info_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    const struct cipher_type_t *cipherp;
    const EVP_CIPHER     *cipher;
    ERL_NIF_TERM         ret, ret_mode;
    unsigned             type;
    unsigned long        mode;
    ERL_NIF_TERM keys[6];
    ERL_NIF_TERM vals[6];
    int ok;

    if ((cipherp = get_cipher_type_no_key(env, argv[0])) == NULL)
        return enif_make_badarg(env);

    if (is_cipher_forbidden_in_fips(cipherp))
        return enif_raise_exception(env, atom_notsup);
    if ((cipher = get_cipher_type_resource(cipherp)) == NULL)
        return enif_raise_exception(env, atom_notsup);

    type = EVP_CIPHER_type(cipher);

    keys[0] = atom_type;
    vals[0] = (type == NID_undef ? atom_undefined : enif_make_int(env, type));
    keys[1] = atom_key_length;
    vals[1] = enif_make_int(env, EVP_CIPHER_key_length(cipher));
    keys[2] = atom_iv_length;
    vals[2] = enif_make_int(env, EVP_CIPHER_iv_length(cipher));
    keys[3] = atom_block_size;
    vals[3] = enif_make_int(env, EVP_CIPHER_block_size(cipher));
    keys[4] = atom_prop_aead;
#if defined(HAVE_AEAD)
    vals[4] = (((EVP_CIPHER_flags(cipher) & EVP_CIPH_FLAG_AEAD_CIPHER) != 0) ? atom_true : atom_false);
#else
    vals[4] = atom_false;
#endif

    mode = EVP_CIPHER_mode(cipher);
    switch (mode) {
        case EVP_CIPH_ECB_MODE:
            ret_mode = atom_ecb_mode;
            break;

        case EVP_CIPH_CBC_MODE:
            ret_mode = atom_cbc_mode;
            break;

        case EVP_CIPH_CFB_MODE:
            ret_mode = atom_cfb_mode;
            break;

        case EVP_CIPH_OFB_MODE:
            ret_mode = atom_ofb_mode;
            break;

#ifdef EVP_CIPH_CTR_MODE
        case EVP_CIPH_CTR_MODE:
            ret_mode = atom_ctr_mode;
            break;
#endif

#ifdef EVP_CIPH_GCM_MODE
        case EVP_CIPH_GCM_MODE:
            ret_mode = atom_gcm_mode;
            break;
#endif

#ifdef EVP_CIPH_CCM_MODE
        case EVP_CIPH_CCM_MODE:
            ret_mode = atom_ccm_mode;
            break;
#endif

#ifdef EVP_CIPH_XTS_MODE
        case EVP_CIPH_XTS_MODE:
            ret_mode = atom_xts_mode;
            break;
#endif

#ifdef EVP_CIPH_WRAP_MODE
        case EVP_CIPH_WRAP_MODE:
            ret_mode = atom_wrap_mode;
            break;
#endif

#ifdef EVP_CIPH_OCB_MODE
        case EVP_CIPH_OCB_MODE:
            ret_mode = atom_ocb_mode;
            break;
#endif

        case EVP_CIPH_STREAM_CIPHER:
            ret_mode = atom_stream_cipher;
            break;

        default:
            ret_mode = atom_undefined;
            break;
    }
    keys[5] = atom_mode;
    vals[5] = ret_mode;

    ok = enif_make_map_from_arrays(env, keys, vals, 6, &ret);
    ASSERT(ok); (void)ok;

    return ret;
}
