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
#include "cmac.h"
#include "hmac.h"
#include "mac.h"

/***************************
     MAC type declaration
***************************/

struct mac_type_t {
    union {
	const char*  str;        /* before init, NULL for end-of-table */
	ERL_NIF_TERM atom;       /* after init, 'false' for end-of-table */
    }name;
    unsigned flags;
    union {
        const int pkey_type;
    }alg;
    int type;
    size_t key_len;      /* != 0 to also match on key_len */
};

/* masks in the flags field if mac_type_t */
#define NO_FIPS_MAC 1

#define NO_mac 0
#define HMAC_mac 1
#define CMAC_mac 2
#define POLY1305_mac 3

static struct mac_type_t mac_types[] =
{
    {{"poly1305"}, NO_FIPS_MAC,
#ifdef HAVE_POLY1305
     /* If we have POLY then we have EVP_PKEY */
     {EVP_PKEY_POLY1305}, POLY1305_mac, 32
#else
     {EVP_PKEY_NONE}, NO_mac, 0
#endif
    },

    {{"hmac"}, 0,
#ifdef HAS_EVP_PKEY_CTX
     {EVP_PKEY_HMAC}, HMAC_mac, 0
#else
     /* HMAC is always supported, but possibly with low-level routines */
     {EVP_PKEY_NONE}, HMAC_mac, 0
#endif
    },

    {{"cmac"}, 0,
#ifdef HAVE_CMAC
     /* If we have CMAC then we have EVP_PKEY */
     {EVP_PKEY_CMAC}, CMAC_mac, 0
#else
     {EVP_PKEY_NONE}, NO_mac, 0
#endif
    },

    /*==== End of list ==== */
    {{NULL}, 0,
     {0}, NO_mac, 0
    }
};


#ifdef FIPS_SUPPORT
/* May have FIPS support, must check dynamically if it is enabled */
# define MAC_FORBIDDEN_IN_FIPS(P) (((P)->flags & NO_FIPS_MAC) && FIPS_mode())
#else
/* No FIPS support since the symbol FIPS_SUPPORT is undefined */
# define MAC_FORBIDDEN_IN_FIPS(P) 0
#endif


/***************************
 Mandatory prototypes
***************************/

struct mac_type_t* get_mac_type(ERL_NIF_TERM type, size_t key_len);
struct mac_type_t* get_mac_type_no_key(ERL_NIF_TERM type);

ERL_NIF_TERM mac_one_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM mac_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


/********************************
 Support functions for type array
*********************************/

void init_mac_types(ErlNifEnv* env)
{
    struct mac_type_t* p = mac_types;

    for (p = mac_types; p->name.str; p++) {
	p->name.atom = enif_make_atom(env, p->name.str);
    }
    p->name.atom = atom_false;  /* end marker */
}


ERL_NIF_TERM mac_types_as_list(ErlNifEnv* env)
{
    struct mac_type_t* p;
    ERL_NIF_TERM prev, hd;

    hd = enif_make_list(env, 0);
    prev = atom_undefined;

    for (p = mac_types; (p->name.atom & (p->name.atom != atom_false)); p++) {
        if (prev == p->name.atom)
            continue;

        if (p->type != NO_mac)
            {
                hd = enif_make_list_cell(env, p->name.atom, hd);
            }
    }

    return hd;
}

struct mac_type_t* get_mac_type(ERL_NIF_TERM type, size_t key_len)
{
    struct mac_type_t* p = NULL;
    for (p = mac_types; p->name.atom != atom_false; p++) {
	if (type == p->name.atom) {
            if ((p->key_len == 0) || (p->key_len == key_len))
                return p;
	}
    }
    return NULL;
}

struct mac_type_t* get_mac_type_no_key(ERL_NIF_TERM type)
{
    struct mac_type_t* p = NULL;
    for (p = mac_types; p->name.atom != atom_false; p++) {
	if (type == p->name.atom) {
	    return p;
	}
    }
    return NULL;
}

/*******************************************************************
 *
 * Mac nif
 *
 ******************************************************************/
ERL_NIF_TERM mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (MacType, SubType, Key, Text) */
    ErlNifBinary  text;

    if (!enif_inspect_iolist_as_binary(env, argv[3], &text))
        return EXCP_BADARG(env, "Bad text");

    if (text.size > INT_MAX)
        return EXCP_BADARG(env, "Too long text");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (text.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "mac_one_time",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 mac_one_time, argc, argv);
    }

    return mac_one_time(env, argc, argv);
}



ERL_NIF_TERM mac_one_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (MacType, SubType, Key, Text) */

    struct mac_type_t *macp;
    ErlNifBinary key_bin, text;
    int ret_bin_alloc = 0;
    ERL_NIF_TERM return_term;
    const EVP_MD *md = NULL;
    ErlNifBinary ret_bin;
#ifdef HAS_EVP_PKEY_CTX
    size_t size;
    EVP_PKEY *pkey = NULL;
    EVP_MD_CTX *mctx = NULL;
#endif

    /*---------------------------------
      Get common indata and validate it
    */
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

    if (!(macp = get_mac_type(argv[0], key_bin.size)))
        {
            if (!get_mac_type_no_key(argv[0]))
                return_term = EXCP_BADARG(env, "Unknown mac algorithm");
            else
                return_term = EXCP_BADARG(env, "Bad key length");
            goto err;
        }

    if (MAC_FORBIDDEN_IN_FIPS(macp))
        {
            return_term = EXCP_NOTSUP(env, "MAC algorithm forbidden in FIPS");
            goto err;
        }

    /*--------------------------------------------------
      Algorithm dependent indata checking and computation.
      If EVP_PKEY is available, only set the pkey variable
      and do the computation after the switch statement.
      If not available, do the low-level calls in the 
      corresponding case part
    */
    switch (macp->type) {

        /********
         * HMAC *
         ********/
    case HMAC_mac:
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
            if (DIGEST_FORBIDDEN_IN_FIPS(digp))
                {
                    return_term = EXCP_NOTSUP(env, "Digest algorithm for HMAC forbidden in FIPS");
                    goto err;
                }
            md = digp->md.p;

#ifdef HAS_EVP_PKEY_CTX
# ifdef HAVE_PKEY_new_raw_private_key
            /* Prefered for new applications according to EVP_PKEY_new_mac_key(3) */
            pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
# else
            /* Available in older versions */
            pkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
# endif

#else
            if (!hmac_low_level(env, md, key_bin, text, &ret_bin, &ret_bin_alloc, &return_term))
                goto err;
            else
                goto success;
#endif
        }
        break;


        /********
         * CMAC *
         ********/
#ifdef HAVE_CMAC
    case CMAC_mac:
        {
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
            
            if (CIPHER_FORBIDDEN_IN_FIPS(cipherp))
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
            if (!cmac_low_level(env, key_bin, cipherp->cipher.p, text, &ret_bin, &ret_bin_alloc, &return_term))
                goto err;
            else
                goto success;
# endif
        }
        break;
#endif /* HAVE_CMAC */


        /************
         * POLY1305 *
         ************/
#ifdef HAVE_POLY1305
    case POLY1305_mac:
        /* poly1305 implies that EVP_PKEY_new_raw_private_key exists */
        pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_POLY1305, /*engine*/ NULL, key_bin.data,  key_bin.size);
        break;
#endif


        /***************
         * Unknown MAC *
         ***************/
    case NO_mac:
    default:
        /* We know that this mac is supported with some version(s) of cryptolib */
        return_term = EXCP_NOTSUP(env, "Unsupported mac algorithm");
        goto err;
    }

    /*-----------------------------------------
      Common computations when we have EVP_PKEY
    */
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

    if (EVP_DigestSignInit(mctx, /*&pctx*/ NULL, md, /*engine*/ NULL, pkey) != 1)
        {
            return_term = EXCP_ERROR(env, "EVP_DigestSign");
            goto err;
        }

# ifdef HAVE_DigestSign_as_single_op
    if (EVP_DigestSign(mctx, NULL, &size, text.data, text.size) != 1)
        {
            return_term = EXCP_ERROR(env, "Can't get sign size");
            goto err;
        }
# else
    if (EVP_DigestSignUpdate(mctx, text.data, text.size) != 1)
        {
            return_term = EXCP_ERROR(env, "EVP_DigestSignUpdate");
            goto err;
        }

    if (EVP_DigestSignFinal(mctx, NULL, &size) != 1)
        {
            return_term = EXCP_ERROR(env, "Can't get sign size");
            goto err;
        }
# endif
   
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

    goto success; /* The label "success:" could be left without any "goto success"
                     in some combination of flags. This prevents a compiler warning
                  */
#endif /* ifdef HAS_EVP_PKEY_CTX */


    /****************************
     Exit when we got a signature
    *****************************/
 success:
    CONSUME_REDS(env, text);

    return_term = enif_make_binary(env, &ret_bin);
    ret_bin_alloc = 0;

 err:

#ifdef HAS_EVP_PKEY_CTX
    if (pkey)
        EVP_PKEY_free(pkey);
    if (mctx)
        EVP_MD_CTX_free(mctx);
#endif

    if (ret_bin_alloc)
        enif_release_binary(&ret_bin);

    return return_term;
}


/*******************************************************************
 *
 * Mac ctx
 *
 ******************************************************************/

int init_mac_ctx(ErlNifEnv *env);

struct mac_context
{
    EVP_MD_CTX *ctx;
};

static ErlNifResourceType* mac_context_rtype;

static void mac_context_dtor(ErlNifEnv* env, struct mac_context*);

int init_mac_ctx(ErlNifEnv *env) {
    mac_context_rtype = enif_open_resource_type(env, NULL, "mac_context",
                                                (ErlNifResourceDtor*) mac_context_dtor,
                                                ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                NULL);
    if (mac_context_rtype == NULL)
        goto err;

    return 1;

 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'mac_context'");
    return 0;
}


static void mac_context_dtor(ErlNifEnv* env, struct mac_context *obj)
{
    if (obj == NULL)
        return;

    if (obj->ctx)
        EVP_MD_CTX_free(obj->ctx);
}

/*******************************************************************
 *
 * mac_init, mac_update, mac_final nifs
 *
 ******************************************************************/

ERL_NIF_TERM mac_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (MacType, SubType, Key) */
#ifdef HAS_EVP_PKEY_CTX
    struct mac_context  *obj = NULL;
    struct mac_type_t *macp;
    ErlNifBinary key_bin;
    ERL_NIF_TERM return_term;
    const EVP_MD *md = NULL;
    EVP_PKEY *pkey = NULL;

    /*---------------------------------
      Get common indata and validate it
    */
    if (!enif_inspect_iolist_as_binary(env, argv[2], &key_bin))
        {
            return_term = EXCP_BADARG(env, "Bad key");
            goto err;
        }

    if (!(macp = get_mac_type(argv[0], key_bin.size)))
        {
            if (!get_mac_type_no_key(argv[0]))
                return_term = EXCP_BADARG(env, "Unknown mac algorithm");
            else
                return_term = EXCP_BADARG(env, "Bad key length");
            goto err;
        }

    if (MAC_FORBIDDEN_IN_FIPS(macp))
        {
            return_term = EXCP_NOTSUP(env, "MAC algorithm forbidden in FIPS");
            goto err;
        }

    /*--------------------------------------------------
      Algorithm dependent indata checking and computation.
      If EVP_PKEY is available, only set the pkey variable
      and do the computation after the switch statement.
      If not available, do the low-level calls in the 
      corresponding case part
    */
    switch (macp->type) {

        /********
         * HMAC *
         ********/
    case HMAC_mac:
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
            if (DIGEST_FORBIDDEN_IN_FIPS(digp))
                {
                    return_term = EXCP_NOTSUP(env, "Digest algorithm for HMAC forbidden in FIPS");
                    goto err;
                }
            md = digp->md.p;

# ifdef HAVE_PKEY_new_raw_private_key
            /* Prefered for new applications according to EVP_PKEY_new_mac_key(3) */
            pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
# else
            /* Available in older versions */
            pkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
# endif
        }
        break;


        /********
         * CMAC *
         ********/
#if defined(HAVE_CMAC) && defined(HAVE_EVP_PKEY_new_CMAC_key)
    case CMAC_mac:
        {
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
            
            if (CIPHER_FORBIDDEN_IN_FIPS(cipherp))
                {
                    return_term = EXCP_NOTSUP(env, "Cipher algorithm not supported in FIPS");
                    goto err;
                }

            if (cipherp->cipher.p == NULL)
                {
                    return_term = EXCP_NOTSUP(env, "Unsupported cipher algorithm");
                    goto err;
                }

            pkey = EVP_PKEY_new_CMAC_key(/*engine*/ NULL, key_bin.data,  key_bin.size, cipherp->cipher.p);
        }
        break;
#endif /* HAVE_CMAC && HAVE_EVP_PKEY_new_CMAC_key */


        /************
         * POLY1305 *
         ************/
#ifdef HAVE_POLY1305
    case POLY1305_mac:
        /* poly1305 implies that EVP_PKEY_new_raw_private_key exists */
        pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_POLY1305, /*engine*/ NULL, key_bin.data,  key_bin.size);
        break;
#endif


        /***************
         * Unknown MAC *
         ***************/
    case NO_mac:
    default:
        /* We know that this mac is supported with some version(s) of cryptolib */
        return_term = EXCP_NOTSUP(env, "Unsupported mac algorithm");
        goto err;
    }

    /*-----------------------------------------
      Common computations
    */
    if (!pkey)
        {
            return_term = EXCP_ERROR(env, "EVP_PKEY_key creation");
            goto err;
        }
    
    if ((obj = enif_alloc_resource(mac_context_rtype, sizeof(struct mac_context))) == NULL)
        {
            return_term = EXCP_ERROR(env, "Can't allocate mac_context_rtype");
            goto err;
        }

    if ((obj->ctx = EVP_MD_CTX_new()) == NULL)
        {
            return_term = EXCP_ERROR(env, "EVP_MD_CTX_new");
            goto err;
        }

    if (EVP_DigestSignInit(obj->ctx, /*&pctx*/ NULL, md, /*engine*/ NULL, pkey) != 1)
        {
            return_term = EXCP_ERROR(env, "EVP_DigestSign");
            goto err;
        }

    return_term = enif_make_resource(env, obj);

 err:

    if (obj)
        enif_release_resource(obj);

    if (pkey)
        EVP_PKEY_free(pkey);

    return return_term;

#else
    if (argv[0] != atom_hmac)
        return EXCP_NOTSUP(env, "Unsupported mac algorithm");

    return hmac_init_nif(env, argc, argv);
#endif
}



ERL_NIF_TERM mac_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Ref, Text) */
    ErlNifBinary  text;

    if (!enif_inspect_iolist_as_binary(env, argv[1], &text))
        return EXCP_BADARG(env, "Bad text");

    if (text.size > INT_MAX)
        return EXCP_BADARG(env, "Too long text");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (text.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "mac_update",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 mac_update, argc, argv);
    }

    return mac_update(env, argc, argv);
}


ERL_NIF_TERM mac_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Ref, Text) */
#ifdef HAS_EVP_PKEY_CTX
    struct mac_context *obj = NULL;
    ErlNifBinary text;

    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)mac_context_rtype, (void**)&obj))
        return EXCP_BADARG(env, "Bad ref");

    if (!enif_inspect_iolist_as_binary(env, argv[1], &text))
        return EXCP_BADARG(env, "Bad text");

    if (EVP_DigestSignUpdate(obj->ctx, text.data, text.size) != 1)
        return EXCP_ERROR(env, "EVP_DigestSignUpdate");

    CONSUME_REDS(env, text);
    return argv[0];

#else
    return hmac_update_nif(env, argc, argv);
#endif
}



ERL_NIF_TERM mac_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Ref) */
#ifdef HAS_EVP_PKEY_CTX
    struct mac_context *obj;
    size_t size;
    ErlNifBinary ret_bin;
    
    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)mac_context_rtype, (void**)&obj))
        return EXCP_BADARG(env, "Bad ref");

    if (EVP_DigestSignFinal(obj->ctx, NULL, &size) != 1)
        return EXCP_ERROR(env, "Can't get sign size");
   
    if (!enif_alloc_binary(size, &ret_bin))
        return EXCP_ERROR(env, "Alloc binary");

    if (EVP_DigestSignFinal(obj->ctx, ret_bin.data, &size) != 1)
        {
            enif_release_binary(&ret_bin);
            return EXCP_ERROR(env, "Signing");
        }
 
    return enif_make_binary(env, &ret_bin);

#else
    return hmac_final_nif(env, argc, argv);
#endif
}

