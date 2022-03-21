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

#include "ecdh.h"
#include "ec.h"

/*
  (_OthersPublicKey, _MyPrivateKey)
  (_OthersPublicKey, _MyEC_Point)
*/
ERL_NIF_TERM ecdh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
/* (OtherPublicKey, Curve, My) */
{
#if defined(HAVE_EC)
    ERL_NIF_TERM ret = atom_undefined;
    unsigned char *p;
    EC_KEY* key = NULL;
    int degree;
    size_t field_size;
    EC_GROUP *group = NULL;
    const BIGNUM *priv_key;
    EC_POINT *my_ecpoint = NULL;
    EC_KEY *other_ecdh = NULL;

    if (!get_ec_key_sz(env, argv[1], argv[2], atom_undefined, &key, NULL)) // my priv key
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Couldn't get local key"));
    
    if ((group = EC_GROUP_dup(EC_KEY_get0_group(key))) == NULL)
         assign_goto(ret, err, EXCP_ERROR(env, "Couldn't duplicate EC key"));

    priv_key = EC_KEY_get0_private_key(key);

    if (!term2point(env, argv[0], group, &my_ecpoint))
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Couldn't get ecpoint"));

    if ((other_ecdh = EC_KEY_new()) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't allocate EC_KEY"));
    
    if (!EC_KEY_set_group(other_ecdh, group))
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't set group"));

    if (!EC_KEY_set_private_key(other_ecdh, priv_key))
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't set private key"));

    if ((degree = EC_GROUP_get_degree(group)) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't get degree"));

    field_size = (size_t)degree;
    if ((p = enif_make_new_binary(env, (field_size+7)/8, &ret)) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't allocate binary"));

    if (ECDH_compute_key(p, (field_size+7)/8, my_ecpoint, other_ecdh, NULL) < 1)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't compute key"));

 err:
    if (group)
        EC_GROUP_free(group);
    if (my_ecpoint)
        EC_POINT_free(my_ecpoint);
    if (other_ecdh)
        EC_KEY_free(other_ecdh);
    if (key)
        EC_KEY_free(key);

    return ret;

#else
    return EXCP_NOTSUP_N(env, 0, "EC not supported");
#endif
}
