/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2022. All Rights Reserved.
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
#include "pbkdf2_hmac.h"
#include "digest.h"

ERL_NIF_TERM pbkdf2_hmac_nif(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[])
{
#ifdef HAS_PKCS5_PBKDF2_HMAC
    ErlNifBinary pass, salt, out;
    ErlNifUInt64 iter, keylen;
    struct digest_type_t* digp = NULL;

    if ((digp = get_digest_type(argv[0])) == NULL)
        return EXCP_BADARG_N(env, 0, "Bad digest type");
    if (digp->md.p == NULL)
        return EXCP_BADARG_N(env, 0, "md.p is not NULL");
    if ((digp->flags & PBKDF2_ELIGIBLE_DIGEST) == 0)
        return EXCP_BADARG_N(env, 0, "Not eligible digest type");

    if (!enif_inspect_binary(env, argv[1], &pass))
        return EXCP_BADARG_N(env, 1, "Not binary");

    if (!enif_inspect_binary(env, argv[2], &salt))
        return EXCP_BADARG_N(env, 2, "Not binary");

    if (!enif_get_uint64(env, argv[3], &iter))
        return EXCP_BADARG_N(env, 3, "Not integer");
    if (iter < 1)
        return EXCP_BADARG_N(env, 3, "Must be > 0");

    if (!enif_get_uint64(env, argv[4], &keylen))
        return EXCP_BADARG_N(env, 4, "Not integer");
    if (keylen < 1)
        return EXCP_BADARG_N(env, 4, "Must be > 0");

    if (!enif_alloc_binary(keylen, &out))
        return EXCP_ERROR(env, "Can't allocate binary");

    if (!PKCS5_PBKDF2_HMAC((const char *)pass.data, pass.size,
                           salt.data, salt.size, iter,
                           digp->md.p,
                           keylen, out.data)) {
        enif_release_binary(&out);
        return EXCP_ERROR(env, "Low-level call failed");
    }

    return enif_make_binary(env, &out);
#else
    return EXCP_NOTSUP(env, "Unsupported CRYPTO_PKCS5_PBKDF2_HMAC");
#endif
}
