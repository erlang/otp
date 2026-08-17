/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright 2026 John Downey <jdowney@gmail.com>
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

#ifndef E_KDF_H__
#define E_KDF_H__ 1

#include "common.h"

#ifdef HAVE_KDF

enum kdf_kind {
    NO_kdf = 0,
    ARGON2_kdf,
    HKDF_kdf,
    PBKDF2_kdf,
    SCRYPT_kdf,
    SSKDF_kdf
};

struct kdf_type_t {
    const char *str;          /* before init, NULL for end-of-table */
    const char *ossl_name;    /* the KDF name as in OpenSSL 3.x */
    ERL_NIF_TERM atom;        /* after init */
    const char *input_name;   /* OSSL_KDF_PARAM_* of the key material */
    int kind;                 /* enum kdf_kind */
    unsigned flags;
    EVP_KDF *kdf;             /* after init, NULL if notsup */
};

/* masks in the flags field of kdf_type_t */
#define NO_FIPS_KDF 1

#ifdef FIPS_SUPPORT
# define KDF_FORBIDDEN_IN_FIPS(P) (((P)->flags & NO_FIPS_KDF) && FIPS_MODE())
#else
# define KDF_FORBIDDEN_IN_FIPS(P) 0
#endif

#endif /* HAVE_KDF */

void init_kdf_types(ErlNifEnv* env);

ERL_NIF_TERM kdf_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM kdf_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_KDF_H__ */
