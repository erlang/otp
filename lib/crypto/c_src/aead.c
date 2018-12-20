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

#include "aead.h"
#include "aes.h"

ERL_NIF_TERM aead_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type,Key,Iv,AAD,In) */
#if defined(HAVE_AEAD)
    EVP_CIPHER_CTX *ctx;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in;
    unsigned int tag_len;
    unsigned char *outp, *tagp;
    ERL_NIF_TERM type, out, out_tag;
    int len, ctx_ctrl_set_ivlen, ctx_ctrl_get_tag;

    type = argv[0];

    if (!enif_is_atom(env, type)
        || !enif_inspect_iolist_as_binary(env, argv[1], &key)
	|| !enif_inspect_binary(env, argv[2], &iv)
	|| !enif_inspect_iolist_as_binary(env, argv[3], &aad)
	|| !enif_inspect_iolist_as_binary(env, argv[4], &in)
	|| !enif_get_uint(env, argv[5], &tag_len)) {
	return enif_make_badarg(env);
    }

    /* Use cipher_type some day.  Must check block_encrypt|decrypt first */
#if defined(HAVE_GCM)
    if (type == atom_aes_gcm) {
        if ((iv.size > 0)
            && (1 <= tag_len && tag_len <= 16)) {
            ctx_ctrl_set_ivlen = EVP_CTRL_GCM_SET_IVLEN;
            ctx_ctrl_get_tag = EVP_CTRL_GCM_GET_TAG;
            if (key.size == 16)      cipher = EVP_aes_128_gcm();
            else if (key.size == 24) cipher = EVP_aes_192_gcm();
            else if (key.size == 32) cipher = EVP_aes_256_gcm();
            else enif_make_badarg(env);
        } else
            enif_make_badarg(env);
    } else
#endif
#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if ((7 <= iv.size && iv.size <= 13)
            && (4 <= tag_len && tag_len <= 16)
            && ((tag_len & 1) == 0)
            ) {
            ctx_ctrl_set_ivlen = EVP_CTRL_CCM_SET_IVLEN;
            ctx_ctrl_get_tag = EVP_CTRL_CCM_GET_TAG;
            if (key.size == 16)      cipher = EVP_aes_128_ccm();
            else if (key.size == 24) cipher = EVP_aes_192_ccm();
            else if (key.size == 32) cipher = EVP_aes_256_ccm();
            else enif_make_badarg(env);
        } else
            enif_make_badarg(env);
    } else
#endif
#if defined(HAVE_CHACHA20_POLY1305)
    if (type == atom_chacha20_poly1305) {
        if ((key.size == 32)
            && (1 <= iv.size && iv.size <= 16)
            && (tag_len == 16)
            ) {
            ctx_ctrl_set_ivlen = EVP_CTRL_AEAD_SET_IVLEN;
            ctx_ctrl_get_tag = EVP_CTRL_AEAD_GET_TAG,
                cipher = EVP_chacha20_poly1305();
        } else enif_make_badarg(env);
    } else
#endif
        return enif_raise_exception(env, atom_notsup);
 
    ctx = EVP_CIPHER_CTX_new();
    if (EVP_EncryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1) goto out_err;
    if (EVP_CIPHER_CTX_ctrl(ctx, ctx_ctrl_set_ivlen, iv.size, NULL) != 1) goto out_err;

#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_SET_TAG, tag_len, NULL) != 1) goto out_err;
        if (EVP_EncryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1) goto out_err;
        if (EVP_EncryptUpdate(ctx, NULL, &len, NULL, in.size) != 1) goto out_err;
    } else
#endif
        if (EVP_EncryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1) goto out_err;

    if (EVP_EncryptUpdate(ctx, NULL, &len, aad.data, aad.size) != 1) goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    if (EVP_EncryptUpdate(ctx, outp, &len, in.data, in.size) != 1) goto out_err;
    if (EVP_EncryptFinal_ex(ctx, outp/*+len*/, &len) != 1) goto out_err;

    tagp = enif_make_new_binary(env, tag_len, &out_tag);

    if (EVP_CIPHER_CTX_ctrl(ctx, ctx_ctrl_get_tag, tag_len, tagp) != 1) goto out_err;

    EVP_CIPHER_CTX_free(ctx);
    CONSUME_REDS(env, in);
    return enif_make_tuple2(env, out, out_tag);

out_err: 
    EVP_CIPHER_CTX_free(ctx);
    return atom_error;

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

ERL_NIF_TERM aead_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type,Key,Iv,AAD,In,Tag) */
#if defined(HAVE_AEAD)
    EVP_CIPHER_CTX *ctx;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in, tag;
    unsigned char *outp;
    ERL_NIF_TERM type, out;
    int len, ctx_ctrl_set_ivlen, ctx_ctrl_set_tag;

    type = argv[0];
#if defined(HAVE_GCM_EVP_DECRYPT_BUG)
    if (type == atom_aes_gcm)
        return aes_gcm_decrypt_NO_EVP(env, argc, argv);
#endif

    if (!enif_is_atom(env, type)
        || !enif_inspect_iolist_as_binary(env, argv[1], &key)
	|| !enif_inspect_binary(env, argv[2], &iv)
	|| !enif_inspect_iolist_as_binary(env, argv[3], &aad)
	|| !enif_inspect_iolist_as_binary(env, argv[4], &in)
	|| !enif_inspect_iolist_as_binary(env, argv[5], &tag)) {
	return enif_make_badarg(env);
    }

    /* Use cipher_type some day.  Must check block_encrypt|decrypt first */
#if defined(HAVE_GCM)
    if (type == atom_aes_gcm) {
        if (iv.size > 0) {
            ctx_ctrl_set_ivlen = EVP_CTRL_GCM_SET_IVLEN;
            ctx_ctrl_set_tag = EVP_CTRL_GCM_SET_TAG;
            if (key.size == 16)      cipher = EVP_aes_128_gcm();
            else if (key.size == 24) cipher = EVP_aes_192_gcm();
            else if (key.size == 32) cipher = EVP_aes_256_gcm();
            else enif_make_badarg(env);
        } else
            enif_make_badarg(env);
    } else
#endif
#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (iv.size > 0) {
            ctx_ctrl_set_ivlen = EVP_CTRL_CCM_SET_IVLEN;
            if (key.size == 16)      cipher = EVP_aes_128_ccm();
            else if (key.size == 24) cipher = EVP_aes_192_ccm();
            else if (key.size == 32) cipher = EVP_aes_256_ccm();
            else enif_make_badarg(env);
        } else
            enif_make_badarg(env);
    } else
#endif
#if defined(HAVE_CHACHA20_POLY1305)
    if (type == atom_chacha20_poly1305) {
        if ((key.size == 32)
            && (1 <= iv.size && iv.size <= 16)
            && tag.size == 16
            ) {
            ctx_ctrl_set_ivlen = EVP_CTRL_AEAD_SET_IVLEN;
            ctx_ctrl_set_tag = EVP_CTRL_AEAD_SET_TAG;
            cipher = EVP_chacha20_poly1305();
        } else enif_make_badarg(env);
    } else
#endif
        return enif_raise_exception(env, atom_notsup);

    outp = enif_make_new_binary(env, in.size, &out);

    ctx = EVP_CIPHER_CTX_new();
    if (EVP_DecryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1) goto out_err;
    if (EVP_CIPHER_CTX_ctrl(ctx,  ctx_ctrl_set_ivlen, iv.size, NULL) != 1) goto out_err;

#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_SET_TAG, tag.size, tag.data) != 1) goto out_err;
    }
#endif

    if (EVP_DecryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1) goto out_err;

#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (1 != EVP_DecryptUpdate(ctx, NULL, &len, NULL, in.size)) goto out_err;
    }
#endif

    if (EVP_DecryptUpdate(ctx, NULL, &len, aad.data, aad.size) != 1) goto out_err;
    if (EVP_DecryptUpdate(ctx, outp, &len, in.data, in.size) != 1) goto out_err;

#if defined(HAVE_GCM) || defined(HAVE_CHACHA20_POLY1305)
    if (type == atom_aes_gcm) {
         if (EVP_CIPHER_CTX_ctrl(ctx, ctx_ctrl_set_tag, tag.size, tag.data) != 1) goto out_err;
         if (EVP_DecryptFinal_ex(ctx, outp+len, &len) != 1) goto out_err;
    }
#endif
    EVP_CIPHER_CTX_free(ctx);

    CONSUME_REDS(env, in);
    return out;

out_err:
    EVP_CIPHER_CTX_free(ctx);
    return atom_error;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}
