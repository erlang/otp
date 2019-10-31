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
#include "cipher.h"



ERL_NIF_TERM aead_cipher(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/*
   (Type,Key,Iv,AAD,In,TagLen,true)
   (Type,Key,Iv,AAD,In,Tag,false)
 */
#if defined(HAVE_AEAD)
    const struct cipher_type_t *cipherp;
    EVP_CIPHER_CTX *ctx = NULL;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in, tag;
    unsigned int tag_len;
    unsigned char *outp, *tagp, *tag_data;
    ERL_NIF_TERM type, out, out_tag, ret, encflg_arg;
    int len, encflg;

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
            ret = EXCP_BADARG(env, "Bad enc flag");
            goto done;
        }

    type = argv[0];

    if (!enif_is_atom(env, type))
        {ret = EXCP_BADARG(env, "non-atom cipher type"); goto done;}
    if (!enif_inspect_iolist_as_binary(env, argv[1], &key))
        {ret = EXCP_BADARG(env, "non-binary key"); goto done;}
    if (!enif_inspect_iolist_as_binary(env, argv[2], &iv))
        {ret = EXCP_BADARG(env, "non-binary iv"); goto done;}
    if (!enif_inspect_iolist_as_binary(env, argv[3], &in))
        {ret = EXCP_BADARG(env, "non-binary text"); goto done;}
    if (!enif_inspect_iolist_as_binary(env, argv[4], &aad))
        {ret = EXCP_BADARG(env, "non-binary AAD"); goto done;}

    if (encflg) {
        if (!enif_get_uint(env, argv[5], &tag_len))
            {ret = EXCP_BADARG(env, "Bad Tag length"); goto done;}
        tag_data = NULL;
    } else {
        if (!enif_inspect_iolist_as_binary(env, argv[5], &tag))
            {ret = EXCP_BADARG(env, "non-binary Tag"); goto done;}
        tag_len = tag.size;
        tag_data = tag.data;
    }

    if (tag_len > INT_MAX
        || key.size > INT_MAX
        || iv.size > INT_MAX
        || in.size > INT_MAX
        || aad.size > INT_MAX)
        {ret = EXCP_BADARG(env, "binary too long"); goto done;}

    if ((cipherp = get_cipher_type(type, key.size)) == NULL)
        {ret = EXCP_BADARG(env, "Unknown cipher"); goto done;}
    if (cipherp->flags & NON_EVP_CIPHER)
        {ret = EXCP_BADARG(env, "Bad cipher"); goto done;}
    if (! (cipherp->flags & AEAD_CIPHER) )
        {ret = EXCP_BADARG(env, "Not aead cipher"); goto done;}
    if ((cipher = cipherp->cipher.p) == NULL)
        {ret = EXCP_NOTSUP(env, "Cipher not supported in this libcrypto version"); goto done;}

#if defined(HAVE_GCM_EVP_DECRYPT_BUG)
    if ( !encflg && (cipherp->flags & GCM_MODE))
        return aes_gcm_decrypt_NO_EVP(env, argc, argv);
#endif

    if ((ctx = EVP_CIPHER_CTX_new()) == NULL)
        {ret = EXCP_ERROR(env, "Can't allocate ctx"); goto done;}

    if (EVP_CipherInit_ex(ctx, cipher, NULL, NULL, NULL, encflg) != 1)
        {ret = EXCP_ERROR(env, "CipherInit failed"); goto done;}
    if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_set_ivlen, (int)iv.size, NULL) != 1)
        {ret = EXCP_BADARG(env, "Bad IV length"); goto done;}

#if defined(HAVE_CCM)
    if (cipherp->flags & CCM_MODE) {
        if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_set_tag, (int)tag_len, tag_data) != 1)
            {ret = EXCP_BADARG(env, "Can't set tag"); goto done;}
        if (EVP_CipherInit_ex(ctx, NULL, NULL, key.data, iv.data, -1) != 1)
            {ret = EXCP_BADARG(env, "Can't set key or iv"); goto done;}
        if (EVP_CipherUpdate(ctx, NULL, &len, NULL, (int)in.size) != 1)
            {ret = EXCP_BADARG(env, "Can't set text size"); goto done;}
    } else
#endif
        { /* GCM_MODE or CHACHA20_POLY1305 */
            /* Set key and iv */
            if (EVP_CipherInit_ex(ctx, NULL, NULL, key.data, iv.data, -1) != 1)
                {ret = EXCP_BADARG(env, "Can't set key or iv"); goto done;}
        }

    /* Set the AAD */
    if (EVP_CipherUpdate(ctx, NULL, &len, aad.data, (int)aad.size) != 1)
        {ret = EXCP_BADARG(env, "Can't set AAD"); goto done;}

    /* Set the plain text and get the crypto text (or vice versa :) ) */
    if ((outp = enif_make_new_binary(env, in.size, &out)) == NULL)
        {ret = EXCP_ERROR(env, "Can't make 'Out' binary"); goto done;}
    if (EVP_CipherUpdate(ctx, outp, &len, in.data, (int)in.size) != 1)
        {
            if (encflg)
                ret = EXCP_BADARG(env, "Can't set in-text");
            else
                /* Decrypt error */
                ret = atom_error;
            goto done;
        }

    if (encflg)
        {
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
        }
    else /* Decrypting. The plain text is already pointed to by 'out' */
        {
#if defined(HAVE_GCM) || defined(HAVE_CHACHA20_POLY1305)
            /* Check the Tag before returning. CCM_MODE does this previously. */
            if (!(cipherp->flags & CCM_MODE)) { /* That is, CHACHA20_POLY1305 or GCM_MODE */ 
                if (EVP_CIPHER_CTX_ctrl(ctx, cipherp->extra.aead.ctx_ctrl_set_tag, (int)tag_len, tag.data) != 1)
                    /* Decrypt error */
                    {ret = atom_error; goto done;}
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
    if (ctx)
        EVP_CIPHER_CTX_free(ctx);
    return ret;

#else
    return EXCP_NOTSUP(env, "Unsupported Cipher");
#endif
}


