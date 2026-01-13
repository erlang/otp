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

#include "aead.h"
#include "aes.h"
#include "cipher.h"
#include "info.h"


ErlNifResourceType* aead_cipher_ctx_rtype;

struct aead_cipher_ctx {
    const struct cipher_type_t *cipherp;
    EVP_CIPHER_CTX *ctx;

    ERL_NIF_TERM key;

    int encflg;
    unsigned int tag_len;
    ErlNifEnv *env;
};

static void aead_cipher_ctx_dtor(ErlNifEnv* env, struct aead_cipher_ctx* ctx) {
    enif_free_env(ctx->env);
    if (ctx->ctx)
        EVP_CIPHER_CTX_free(ctx->ctx);

    return;
}

int init_aead_cipher_ctx(ErlNifEnv *env, ErlNifBinary* rt_buf) {
    aead_cipher_ctx_rtype = enif_open_resource_type(env, NULL,
                                                    resource_name("AEAD_CIPHER_CTX", rt_buf),
                                                    (ErlNifResourceDtor*) aead_cipher_ctx_dtor,
                                                    ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                    NULL);
    if (aead_cipher_ctx_rtype == NULL)
        goto err;

    return 1;

 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'AEAD_CIPHER_CTX'");
    return 0;
}

ERL_NIF_TERM aead_cipher_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/*
   (Type,Key,TagLen,DoEncode)
 */
#if defined(HAVE_AEAD)
    struct aead_cipher_ctx *ctx_res = NULL;
    ERL_NIF_TERM ret, encflg_arg, type;
    ErlNifBinary key;

    if ((ctx_res = enif_alloc_resource(aead_cipher_ctx_rtype, sizeof(struct aead_cipher_ctx))) == NULL)
        return EXCP_ERROR(env, "Can't allocate resource");

    ctx_res->env = enif_alloc_env();
    encflg_arg = argv[3];

    /* Fetch the flag telling if we are going to encrypt (=true) or decrypt (=false) */
    if (encflg_arg == atom_true)
        ctx_res->encflg = 1;
    else if (encflg_arg == atom_false)
        ctx_res->encflg = 0;
    else
        {
            ret = EXCP_BADARG_N(env, 3, "Bad enc flag");
            goto done;
        }

    type = argv[0];

    if (!enif_is_atom(env, type))
        {ret = EXCP_BADARG_N(env, 0, "non-atom cipher type"); goto done;}

    if (!enif_inspect_binary(env, argv[1], &key))
        {ret = EXCP_BADARG_N(env, 1, "non-binary key"); goto done;}
    ctx_res->key = enif_make_copy(ctx_res->env, argv[1]);

    if (!enif_get_uint(env, argv[2], &ctx_res->tag_len))
        {ret = EXCP_BADARG_N(env, 2, "Bad Tag length"); goto done;}

    if (ctx_res->tag_len > INT_MAX
        || key.size > INT_MAX)
        {ret = EXCP_BADARG_N(env, 1, "key or tag too long"); goto done;}

    if ((ctx_res->cipherp = get_cipher_type(type, key.size)) == NULL)
        {ret = EXCP_BADARG_N(env, 0, "Unknown cipher or invalid key size"); goto done;}
    if (ctx_res->cipherp->flags & NON_EVP_CIPHER)
        {ret = EXCP_BADARG_N(env, 0, "Bad cipher"); goto done;}
    if (! (ctx_res->cipherp->flags & AEAD_CIPHER) )
        {ret = EXCP_BADARG_N(env, 0, "Not aead cipher"); goto done;}
    if (CIPHER_FORBIDDEN_IN_FIPS(ctx_res->cipherp))
        {ret = EXCP_NOTSUP_N(env, 0, "Forbidden in FIPS"); goto done;}

#if defined(HAVE_GCM_EVP_DECRYPT_BUG)
    if ( !ctx_res->encflg && (ctx_res->cipherp->flags & GCM_MODE)) {
        {ret = EXCP_NOTSUP_N(env, 0, "HAVE_GCM_EVP_DECRYPT_BUG with init aead not supported, update ssl version"); goto done;}
    }
#endif

    if (ctx_res->cipherp->cipher.p == NULL)
        {ret = EXCP_NOTSUP_N(env, 0, "The cipher is not supported in this libcrypto version"); goto done;}

    if ((ctx_res->ctx = EVP_CIPHER_CTX_new()) == NULL)
        {ret = EXCP_ERROR(env, "Can't allocate ctx"); goto done;}
    if (EVP_CipherInit_ex(ctx_res->ctx, ctx_res->cipherp->cipher.p, NULL, NULL, NULL, ctx_res->encflg) != 1)
        {ret = EXCP_ERROR(env, "CipherInit failed"); goto done;}

    ret = enif_make_resource(env, ctx_res);

done:
    if(ctx_res) enif_release_resource(ctx_res);
    return ret;
#else
    return EXCP_NOTSUP_N(env, 0, "Unsupported Cipher");
#endif
}

ERL_NIF_TERM aead_cipher_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#if defined(HAVE_AEAD)
    const struct cipher_type_t *cipherp;
    EVP_CIPHER_CTX *ctx = NULL;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in, tag;
    unsigned int tag_len;
    unsigned char *outp, *tagp, *tag_data, *in_data;
    ERL_NIF_TERM type, out, out_tag, ret, encflg_arg;
    int len, encflg, in_len;

    if(argc == 7) {
        /*
          (Type,Key,Iv,In,AAD,TagLen,true)
          (Type,Key,Iv,In,AAD,Tag,false)
        */
        encflg_arg = argv[6];

        /* Fetch the flag telling if we are going to encrypt (=true) or decrypt (=false) */
        if (encflg_arg == atom_true)
            encflg = 1;
        else if (encflg_arg == atom_false)
            encflg = 0;
        else if (encflg_arg == atom_undefined)
            /* For compat funcs in crypto.erl */
            encflg = -1;
        else
            {
                ret = EXCP_BADARG_N(env, 6, "Bad enc flag");
                goto done;
            }

        type = argv[0];

        if (!enif_is_atom(env, type))
            {ret = EXCP_BADARG_N(env, 0, "non-atom cipher type"); goto done;}
        if (!enif_inspect_iolist_as_binary(env, argv[1], &key))
            {ret = EXCP_BADARG_N(env, 1, "non-binary key"); goto done;}
        if (!enif_inspect_iolist_as_binary(env, argv[2], &iv))
            {ret = EXCP_BADARG_N(env, 2, "non-binary iv"); goto done;}
        if (!enif_inspect_iolist_as_binary(env, argv[3], &in))
            {ret = EXCP_BADARG_N(env, 3, "non-binary text"); goto done;}
        in_data = in.data;
        in_len = in.size;
        if (!enif_inspect_iolist_as_binary(env, argv[4], &aad))
            {ret = EXCP_BADARG_N(env, 4, "non-binary AAD"); goto done;}

        if (encflg) {
            if (!enif_get_uint(env, argv[5], &tag_len))
                {ret = EXCP_BADARG_N(env, 5, "Bad Tag length"); goto done;}
            tag_data = NULL;
        } else {
            if (!enif_inspect_iolist_as_binary(env, argv[5], &tag))
                {ret = EXCP_BADARG_N(env, 5, "non-binary Tag"); goto done;}
            tag_len = tag.size;
            tag_data = tag.data;
        }

        if (tag_len > INT_MAX
            || key.size > INT_MAX
            || iv.size > INT_MAX
            || in.size > INT_MAX
            || aad.size > INT_MAX)
            {ret = EXCP_BADARG_N(env, 5, "binary too long"); goto done;}

        if ((cipherp = get_cipher_type(type, key.size)) == NULL)
            {ret = EXCP_BADARG_N(env, 0, "Unknown cipher or invalid key size"); goto done;}
        if (cipherp->flags & NON_EVP_CIPHER)
            {ret = EXCP_BADARG_N(env, 0, "Bad cipher"); goto done;}
        if (! (cipherp->flags & AEAD_CIPHER) )
            {ret = EXCP_BADARG_N(env, 0, "Not aead cipher"); goto done;}
        if (CIPHER_FORBIDDEN_IN_FIPS(cipherp))
            {ret = EXCP_NOTSUP_N(env, 0, "Forbidden in FIPS"); goto done;}

#if defined(HAVE_GCM_EVP_DECRYPT_BUG)
        if ( !encflg && (cipherp->flags & GCM_MODE)) {
            return aes_gcm_decrypt_NO_EVP(env, argc, argv);
        }
#endif
        if ((cipher = cipherp->cipher.p) == NULL)
            {ret = EXCP_NOTSUP_N(env, 0, "The cipher is not supported in this libcrypto version"); goto done;}

        if ((ctx = EVP_CIPHER_CTX_new()) == NULL)
            {ret = EXCP_ERROR(env, "Can't allocate ctx"); goto done;}
        if (EVP_CipherInit_ex(ctx, cipher, NULL, NULL, NULL, encflg) != 1)
            {ret = EXCP_ERROR(env, "CipherInit failed"); goto done;}

    } else {
        /* argc = 4  {state, IV, InData, AAD }  */
        struct aead_cipher_ctx *ctx_res = NULL;
        if (!enif_get_resource(env, argv[0], aead_cipher_ctx_rtype, (void**)&ctx_res))
            {ret = EXCP_BADARG_N(env, 0, "Bad State"); goto done;}
        if (!enif_inspect_iolist_as_binary(env, argv[1], &iv))
            {ret = EXCP_BADARG_N(env, 1, "non-binary iv"); goto done;}
        if (!enif_inspect_iolist_as_binary(env, argv[2], &in))
            {ret = EXCP_BADARG_N(env, 2, "non-binary text"); goto done;}
        in_data = in.data;
        in_len = in.size;

        if (!enif_inspect_iolist_as_binary(env, argv[3], &aad))
            {ret = EXCP_BADARG_N(env, 3, "non-binary AAD"); goto done;}

        if (!enif_inspect_binary(env, ctx_res->key, &key))
            {ret = EXCP_BADARG_N(env, 0, "Bad State key"); goto done;}

        encflg = ctx_res->encflg;

        if(ctx_res->encflg) {
            tag_len = ctx_res->tag_len;
            tag_data = NULL;
        } else {
            tag_len  = ctx_res->tag_len;
            in_len   = in_len - tag_len;
            if (in_len < 0)
                {ret = EXCP_ERROR(env, "Bad in data"); goto done;}
            tag_data = in_data + in_len;
        }

        cipherp = ctx_res->cipherp;
        cipher = cipherp->cipher.p;
        ctx = ctx_res->ctx;
    }
    /* Init done */

    if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_set_ivlen, (int)iv.size, NULL) != 1)
        {ret = EXCP_BADARG_N(env, 2, "Bad IV length"); goto done;}

#if defined(HAVE_CCM)
    if (cipherp->flags & CCM_MODE) {
        if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_set_tag, (int)tag_len, tag_data) != 1)
            {ret = EXCP_BADARG_N(env, 5, "Can't set tag"); goto done;}
        if (EVP_CipherInit_ex(ctx, NULL, NULL, key.data, iv.data, -1) != 1)
            {ret = EXCP_ERROR(env, "Can't set key or iv"); goto done;}
        if (EVP_CipherUpdate(ctx, NULL, &len, NULL, (int)in_len) != 1)
            {ret = EXCP_ERROR(env, "Can't set text size"); goto done;}
    } else
#endif
        { /* GCM_MODE or CHACHA20_POLY1305 */
            /* Set key and iv */
            if (EVP_CipherInit_ex(ctx, NULL, NULL, key.data, iv.data, -1) != 1)
                {ret = EXCP_ERROR(env, "Can't set key and iv"); goto done;}
        }

    /* Set the AAD */
    if (EVP_CipherUpdate(ctx, NULL, &len, aad.data, (int)aad.size) != 1)
        {ret = EXCP_BADARG_N(env, 4, "Can't set AAD"); goto done;}

    /* For GCM-SIV decryption, set tag BEFORE decrypting */
    if (!encflg && (cipherp->flags & GCM_SIV_MODE)) {
        if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_set_tag, (int)tag_len, tag_data) != 1)
            {ret = atom_error; goto done;}
    }

    /* Set the plain text and get the crypto text (or vice versa :) ) */
    if (encflg && argc == 4)
        len = in_len+tag_len;
    else
        len = in_len;

    if ((outp = enif_make_new_binary(env, len, &out)) == NULL)
        {ret = EXCP_ERROR(env, "Can't make 'Out' binary"); goto done;}

    if (EVP_CipherUpdate(ctx, outp, &len, in_data, in_len) != 1)
        {
            if (encflg)
                ret = EXCP_BADARG_N(env, 3, "Can't set in-text");
            else
                /* Decrypt error */
                ret = atom_error;
            goto done;
        }
    if (encflg)
        {
            if (argc == 7) {
                /* Finalize the encrypted text */
                if (EVP_CipherFinal_ex(ctx, outp, &len) != 1)
                    {ret = EXCP_ERROR(env, "Encrypt error"); goto done;}

                /* Get the tag */
                if ((tagp = enif_make_new_binary(env, tag_len, &out_tag)) == NULL)
                    {ret = EXCP_ERROR(env, "Can't make 'Out' binary"); goto done;}
                if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_get_tag, (int)tag_len, tagp) != 1)
                    {ret = EXCP_ERROR(env, "Can't get Tag"); goto done;}

                /* Make the return value (the tuple with binary crypto text and the tag) */
                ret = enif_make_tuple2(env, out, out_tag);
            } else {
                if (EVP_CipherFinal_ex(ctx, outp, &len) != 1)
                    {ret = EXCP_ERROR(env, "Encrypt error"); goto done;}
                /* Add tag to output end */
                tagp = outp + in_len;
                if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_get_tag, (int)tag_len, tagp) != 1)
                    {ret = EXCP_ERROR(env, "Can't get Tag"); goto done;}
                ret = out;
            }
        }
    else /* Decrypting. The plain text is already pointed to by 'out' */
        {
#if defined(HAVE_GCM) || defined(HAVE_CHACHA20_POLY1305)
            /* Check the Tag before returning. CCM_MODE does this previously. */
            if (!(cipherp->flags & CCM_MODE)) { /* That is, CHACHA20_POLY1305 or GCM_MODE */
                /* For GCM-SIV, tag was already set before decryption */
                if (!(cipherp->flags & GCM_SIV_MODE)) {
                    if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_set_tag, (int)tag_len, tag_data) != 1)
                        /* Decrypt error */
                        {ret = atom_error; goto done;}
                }
                /* CCM dislikes EVP_DecryptFinal_ex on decrypting for pre 1.1.1, so we do it only here */
                if (EVP_DecryptFinal_ex(ctx, outp+len, &len) != 1)
                    /* Decrypt error */
                    {ret = atom_error; goto done;}
            }
#endif
            /* Make the return value, that is, the plain text */
            ret = out;
        }

    CONSUME_REDS(env, in);

done:
    if (ctx && argc == 7 )
        EVP_CIPHER_CTX_free(ctx);
    return ret;

#else
    return EXCP_NOTSUP_N(env, 0, "Unsupported Cipher");
#endif
}

