/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2023. All Rights Reserved.
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

#include "digest.h"

static struct digest_type_t digest_types[] =
{
    {"md4", "MD4", 0, NO_FIPS_DIGEST,
#ifdef HAVE_MD4
     {&EVP_md4,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"md5", "MD5", 0, NO_FIPS_DIGEST,
#ifdef HAVE_MD5
     {&EVP_md5,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"ripemd160", "RIPEMD160", 0, NO_FIPS_DIGEST,
#ifdef HAVE_RIPEMD160
     {&EVP_ripemd160,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"sha", "SHA1", 0, PBKDF2_ELIGIBLE_DIGEST,
     {&EVP_sha1,NULL}
    },
    
    {"sha224", "SHA2-224", 0, PBKDF2_ELIGIBLE_DIGEST,
#ifdef HAVE_SHA224
     {&EVP_sha224,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"sha256", "SHA2-256", 0, PBKDF2_ELIGIBLE_DIGEST,
#ifdef HAVE_SHA256
     {&EVP_sha256,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"sha384", "SHA2-384", 0, PBKDF2_ELIGIBLE_DIGEST,
#ifdef HAVE_SHA384
     {&EVP_sha384,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"sha512", "SHA2-512", 0, PBKDF2_ELIGIBLE_DIGEST,
#ifdef HAVE_SHA512
     {&EVP_sha512,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"sha3_224", "SHA3-224", 0, 0,
#ifdef HAVE_SHA3_224
     {&EVP_sha3_224,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"sha3_256", "SHA3-256", 0, 0,
#ifdef HAVE_SHA3_256
     {&EVP_sha3_256,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"sha3_384", "SHA3-384", 0, 0,
#ifdef HAVE_SHA3_384
     {&EVP_sha3_384,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"sha3_512", "SHA3-512", 0, 0,
#ifdef HAVE_SHA3_512
     {&EVP_sha3_512,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"shake128", "SHAKE-128", 0, 0,
#ifdef HAVE_SHAKE128
    {&EVP_shake128, NULL}
#else
    {NULL,NULL}
#endif
    },

    {"shake256", "SHAKE-256", 0, 0,
#ifdef HAVE_SHAKE256
    {&EVP_shake256, NULL}
#else
    {NULL,NULL}
#endif
    },

    {"sm3", "SM3", 0, 0,
#ifdef HAVE_SM3
    {&EVP_sm3, NULL}
#else
    {NULL,NULL}
#endif
    },

    {"blake2b", "BLAKE2b512", 0, 0,
#ifdef HAVE_BLAKE2
     {&EVP_blake2b512,NULL}
#else
     {NULL,NULL}
#endif
    },

    {"blake2s", "BLAKE2s256", 0, 0,
#ifdef HAVE_BLAKE2
     {&EVP_blake2s256,NULL}
#else
     {NULL,NULL}
#endif
    },

    /*==== End of list ==== */
    {NULL,  NULL, 0, 0, {NULL,NULL}}
};

void init_digest_types(ErlNifEnv* env)
{
    struct digest_type_t* p = digest_types;

    for (p = digest_types; p->str; p++) {
#ifdef HAS_3_0_API
        if (p->str_v3) {
            p->md.p = EVP_MD_fetch(NULL, p->str_v3, "");
# ifdef FIPS_SUPPORT
            /* Try if valid in FIPS */
            {
                EVP_MD *tmp = EVP_MD_fetch(NULL, p->str_v3, "fips=yes");

                if (tmp) {
                    EVP_MD_free(tmp);
                    p->flags &= ~NO_FIPS_DIGEST;
                } else
                    p->flags |= NO_FIPS_DIGEST;
            }
# endif /* FIPS_SUPPORT and >=3.0.0 */
        }
#else
        if (p->md.funcp)
	    p->md.p = p->md.funcp();
#endif
        p->atom = enif_make_atom(env, p->str);
    }

    p->atom = atom_false;  /* end marker */
}

struct digest_type_t* get_digest_type(ERL_NIF_TERM type)
{
    struct digest_type_t* p = NULL;
    for (p = digest_types; p->atom != atom_false; p++) {
	if (type == p->atom) {
	    return p;
	}
    }

    return NULL;
}


#ifdef HAS_3_0_API
ERL_NIF_TERM digest_types_as_list(ErlNifEnv* env)
{
    struct digest_type_t* p;
    ERL_NIF_TERM hd;

    hd = enif_make_list(env, 0);

    for (p = digest_types; (p->atom & (p->atom != atom_false)); p++) {
        if (DIGEST_FORBIDDEN_IN_FIPS(p))
            continue;

        if (p->md.p != NULL)
            hd = enif_make_list_cell(env, p->atom, hd);
    }

    return hd;
}
#endif
