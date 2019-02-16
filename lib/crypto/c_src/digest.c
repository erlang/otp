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

#include "digest.h"

static struct digest_type_t digest_types[] =
{
    {{"md4"}, {&EVP_md4}},
    {{"md5"}, {&EVP_md5}},
    {{"ripemd160"}, {&EVP_ripemd160}},
    {{"sha"}, {&EVP_sha1}},
    {{"sha224"},
#ifdef HAVE_SHA224
     {&EVP_sha224}
#else
     {NULL}
#endif
    },
    {{"sha256"},
#ifdef HAVE_SHA256
     {&EVP_sha256}
#else
     {NULL}
#endif
    },
    {{"sha384"},
#ifdef HAVE_SHA384
     {&EVP_sha384}
#else
     {NULL}
#endif
    },
    {{"sha512"},
#ifdef HAVE_SHA512
     {&EVP_sha512}
#else
     {NULL}
#endif
    },
    {{"sha3_224"},
#ifdef HAVE_SHA3_224
     {&EVP_sha3_224}
#else
     {NULL}
#endif
    },
    {{"sha3_256"},
#ifdef HAVE_SHA3_256
     {&EVP_sha3_256}
#else
     {NULL}
#endif
    },
    {{"sha3_384"},
#ifdef HAVE_SHA3_384
     {&EVP_sha3_384}
#else
     {NULL}
#endif
    },
    {{"sha3_512"},
#ifdef HAVE_SHA3_512
     {&EVP_sha3_512}
#else
     {NULL}
#endif
    },
    {{"blake2b"},
#ifdef HAVE_BLAKE2
     {&EVP_blake2b512}
#else
     {NULL}
#endif
    },
    {{"blake2s"},
#ifdef HAVE_BLAKE2
     {&EVP_blake2s256}
#else
     {NULL}
#endif
    },

    {{NULL}, {NULL}}
};

void init_digest_types(ErlNifEnv* env)
{
    struct digest_type_t* p = digest_types;

    for (p = digest_types; p->type.str; p++) {
	p->type.atom = enif_make_atom(env, p->type.str);
	if (p->md.funcp)
	    p->md.p = p->md.funcp();
    }
    p->type.atom = atom_false;  /* end marker */
}

struct digest_type_t* get_digest_type(ERL_NIF_TERM type)
{
    struct digest_type_t* p = NULL;
    for (p = digest_types; p->type.atom != atom_false; p++) {
	if (type == p->type.atom) {
	    return p;
	}
    }
    return NULL;
}

