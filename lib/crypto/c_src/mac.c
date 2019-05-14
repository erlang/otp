/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2019. All Rights Reserved.
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

#include "common.h"
#include "cipher.h"
#include "digest.h"
#include "mac.h"

#ifdef HAS_EVP_PKEY_CTX
struct mac_type_t {
    union {
	const char*  str;        /* before init, NULL for end-of-table */
	ERL_NIF_TERM atom;       /* after init, 'false' for end-of-table */
    }type;
    union {
        const int type;
    }alg;
};


static struct mac_type_t mac_types[] =
{
    {{"poly1305"},
#ifdef HAVE_POLY1305
     {EVP_PKEY_POLY1305}
#else
     {EVP_PKEY_NONE}
#endif
    },

    {{"hmac"},
#ifdef HAVE_PKEY_HMAC
     {EVP_PKEY_HMAC}
#else
     {EVP_PKEY_NONE}
#endif
    },

    {{"cmac"},
#ifdef HAVE_CMAC
     {EVP_PKEY_CMAC}
#else
     {EVP_PKEY_NONE}
#endif
    },
    /*==== End of list ==== */
    {{NULL},{0}}
};

#endif /* HAS_EVP_PKEY_CTX */


void init_mac_types(ErlNifEnv* env)
{
#ifdef HAS_EVP_PKEY_CTX
    struct mac_type_t* p = mac_types;

    for (p = mac_types; p->type.str; p++) {
	p->type.atom = enif_make_atom(env, p->type.str);
    }
    p->type.atom = atom_false;  /* end marker */
#endif    
}


ERL_NIF_TERM mac_types_as_list(ErlNifEnv* env)
{
#ifdef HAS_EVP_PKEY_CTX
    struct mac_type_t* p;
    ERL_NIF_TERM prev, hd;

    hd = enif_make_list(env, 0);
    prev = atom_undefined;

    for (p = mac_types; (p->type.atom & (p->type.atom != atom_false)); p++) {
        if (prev == p->type.atom)
            continue;

        if (p->alg.type != EVP_PKEY_NONE)
            {
                hd = enif_make_list_cell(env, p->type.atom, hd);
            }
    }

    return hd;
#else
    return enif_make_list1(env, atom_hmac);
#endif
}

     
#ifdef HAS_EVP_PKEY_CTX
struct mac_type_t* get_mac_type(ERL_NIF_TERM type);

struct mac_type_t* get_mac_type(ERL_NIF_TERM type)
{
    struct mac_type_t* p = NULL;
    for (p = mac_types; p->type.atom != atom_false; p++) {
	if (type == p->type.atom) {
	    return p;
	}
    }
    return NULL;
}
#endif




ERL_NIF_TERM mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (MacType, SubType, Key, Text) */

    ErlNifBinary key_bin, text;
    int ret_bin_alloc = 0;
    size_t size;
    ERL_NIF_TERM return_term;
    const EVP_MD *md = NULL;
    ErlNifBinary ret_bin;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY *pkey = NULL;
    EVP_MD_CTX *mctx = NULL;
    EVP_PKEY_CTX *pctx = NULL;
#endif

    if (!enif_inspect_iolist_as_binary(env, argv[2], &key_bin))
        {
            return_term = EXCP_BADARG(env, "Bad key");
            goto err;
        }

    if (!enif_inspect_iolist_as_binary(env, argv[3], &text))
        {
            return_term = EXCP_BADARG(env, "Bad text");
            goto err;
        }
    
    if (argv[0] == atom_hmac)
        {
            struct digest_type_t *digp;
            
            if ((digp = get_digest_type(argv[1])) == NULL)
                {
                    return_term = EXCP_BADARG(env, "Bad digest algorithm for HMAC");
                    goto err;
                }
            if (digp->md.p == NULL)
                {
                    return_term = EXCP_NOTSUP(env, "Unsupported digest algorithm");
                    goto err;
                }

            md = digp->md.p;

#ifndef HAS_EVP_PKEY_CTX
            /* Old cryptolib: use low level functions */
            {
                unsigned int size_int;

                /* Find the needed space */
                if (HMAC(md,
                         key_bin.data, (int)key_bin.size,
                         text.data, text.size,
                         NULL, &size_int) == NULL)
                    {
                        return_term = EXCP_ERROR(env, "Get HMAC size failed");
                        goto err;
                    }

                size = (size_t)size_int; /* Otherwise "size" is unused in 0.9.8.... */
                if (!enif_alloc_binary(size, &ret_bin))
                    {
                        return_term = EXCP_ERROR(env, "Alloc binary");
                        goto err;
                    }
                ret_bin_alloc = 1;

                /* And do the real HMAC calc */
                if (HMAC(md,
                         key_bin.data, (int)key_bin.size,
                         text.data, text.size,
                         ret_bin.data, &size_int) == NULL)
                    {
                        return_term = EXCP_ERROR(env, "HMAC sign failed");
                        goto err;
                    }
            }
#else
/* HAS_EVP_PKEY_CTX and HMAC is the type. Produce a PKEY for later use */

# ifdef HAVE_PKEY_new_raw_private_key
            /* Prefered for new applications according to EVP_PKEY_new_mac_key(3) */
            pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
# else
            /* Available in older versions */
            pkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
# endif
        }

    else if (argv[0] == atom_cmac)
        {
#ifdef HAVE_CMAC
            const struct cipher_type_t *cipherp;
            if (!(cipherp = get_cipher_type(argv[1], key_bin.size)))
                { /* Something went wrong. Find out what by retrying in another way. */
                    if (!get_cipher_type_no_key(argv[1]))
                        return_term = EXCP_BADARG(env, "Unknown cipher");
                    else
                        /* Cipher exists, so it must be the key size that is wrong */
                        return_term = EXCP_BADARG(env, "Bad key size");
                    goto err;
                }
            
            if (FORBIDDEN_IN_FIPS(cipherp))
                {
                    return_term = EXCP_NOTSUP(env, "Cipher algorithm not supported in FIPS");
                    goto err;
                }

            if (cipherp->cipher.p == NULL)
                {
                    return_term = EXCP_NOTSUP(env, "Unsupported cipher algorithm");
                    goto err;
                }

# ifdef HAVE_EVP_PKEY_new_CMAC_key
            pkey = EVP_PKEY_new_CMAC_key(/*engine*/ NULL, key_bin.data,  key_bin.size, cipherp->cipher.p);
# else
            /* Compatibility with < 1.1.1 that doesn't have EVP_PKEY_new_CMAC_key
               It is a complicated flow so just do some goto to get out of it.
             */
            {
                CMAC_CTX *ctx = NULL;

                if ((ctx = CMAC_CTX_new()) == NULL)
                    goto local_err;

                if (!CMAC_Init(ctx, key_bin.data, key_bin.size, cipherp->cipher.p, NULL))
                    goto local_err;

                if (!CMAC_Update(ctx, text.data, text.size))
                    goto local_err;

                if ((size = (size_t)EVP_CIPHER_block_size(cipherp->cipher.p)) < 0)
                    goto local_err;

                if (!enif_alloc_binary(size, &ret_bin))
                    goto local_err;
                ret_bin_alloc = 1;
                
                if (!CMAC_Final(ctx, ret_bin.data, &ret_bin.size))
                    goto local_err;

                CONSUME_REDS(env, text);

                return_term = enif_make_binary(env, &ret_bin);
                ret_bin_alloc = 0;
                goto done;

            local_err:
                 if (ctx)
                     CMAC_CTX_free(ctx);

                 return_term=EXCP_ERROR(env,"Compat cmac");
                 goto err;
            }
# endif
#else
            return_term = EXCP_NOTSUP(env, "Unsupported mac type");
            goto err;  
#endif /* HAVE_CMAC */
        }

    else if (argv[0] == atom_poly1305)
        {
#ifdef HAVE_POLY1305
            if (key_bin.size != 32)
                {
                    return_term = EXCP_BADARG(env, "Bad key size, != 32 bytes");
                    goto err;
                }
            /* poly1305 implies that EVP_PKEY_new_raw_private_key exists */
            pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_POLY1305, /*engine*/ NULL, key_bin.data,  key_bin.size);
#else
            return_term = EXCP_NOTSUP(env, "Unsupported mac type");
            goto err;  
#endif /* HAVE_POLY1305 */
#endif /* HAS_EVP_PKEY_CTX */

        }
    else
        {
            return_term = EXCP_BADARG(env, "Bad mac type");
            goto err;
        }

#ifdef HAS_EVP_PKEY_CTX
    if (!pkey)
        {
            return_term = EXCP_ERROR(env, "EVP_PKEY_key creation");
            goto err;
        }

    
    if ((mctx = EVP_MD_CTX_new()) == NULL)
        {
            return_term = EXCP_ERROR(env, "EVP_MD_CTX_new");
            goto err;
        }

    if (EVP_DigestSignInit(mctx, &pctx, md, /*engine*/ NULL, pkey) != 1)
        {
            return_term = EXCP_ERROR(env, "EVP_DigestSign");
            goto err;
        }

# ifdef HAVE_DigestSign_as_single_op
    if (EVP_DigestSign(mctx, NULL, &size, text.data, text.size) != 1)
# else
    if (EVP_DigestSignUpdate(mctx, text.data, text.size) != 1)
        {
            return_term = EXCP_ERROR(env, "EVP_DigestSignUpdate");
            goto err;
        }

    if (EVP_DigestSignFinal(mctx, NULL, &size) != 1)
# endif
        {
            return_term = EXCP_ERROR(env, "Can't get sign size");
            goto err;
        }
   
    if (!enif_alloc_binary(size, &ret_bin))
        {
            return_term = EXCP_ERROR(env, "Alloc binary");
            goto err;
        }
    ret_bin_alloc = 1;

# ifdef HAVE_DigestSign_as_single_op
    if (EVP_DigestSign(mctx, ret_bin.data, &size, text.data, text.size) != 1)
# else    
    if (EVP_DigestSignFinal(mctx, ret_bin.data, &size) != 1)
# endif
        {
            return_term = EXCP_ERROR(env, "Signing");
            goto err;
        }
    
#endif /* ifdef HAS_EVP_PKEY_CTX */

    CONSUME_REDS(env, text);

    return_term = enif_make_binary(env, &ret_bin);
    ret_bin_alloc = 0;
    goto done;

err:
    if (ret_bin_alloc)
        enif_release_binary(&ret_bin);

done:
#ifdef HAS_EVP_PKEY_CTX
    if (pkey)
        EVP_PKEY_free(pkey);
#endif
    return return_term;
}





