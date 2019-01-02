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

#include "dss.h"
#include "bn.h"

int get_dss_private_key(ErlNifEnv* env, ERL_NIF_TERM key, DSA *dsa)
{
    /* key=[P,Q,G,KEY] */
    ERL_NIF_TERM head, tail;
    BIGNUM *dsa_p = NULL, *dsa_q = NULL, *dsa_g = NULL;
    BIGNUM *dummy_pub_key, *priv_key = NULL;

    if (!enif_get_list_cell(env, key, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa_p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa_q)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa_g)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &priv_key)
	|| !enif_is_empty_list(env,tail)) {
	if (dsa_p) BN_free(dsa_p);
	if (dsa_q) BN_free(dsa_q);
	if (dsa_g) BN_free(dsa_g);
	if (priv_key) BN_free(priv_key);
	return 0;
    }

    /* Note: DSA_set0_key() does not allow setting only the
     * private key, although DSA_sign() does not use the
     * public key. Work around this limitation by setting
     * the public key to a copy of the private key.
     */
    dummy_pub_key = BN_dup(priv_key);

    DSA_set0_pqg(dsa, dsa_p, dsa_q, dsa_g);
    DSA_set0_key(dsa, dummy_pub_key, priv_key);
    return 1;
}


int get_dss_public_key(ErlNifEnv* env, ERL_NIF_TERM key, DSA *dsa)
{
    /* key=[P, Q, G, Y] */
    ERL_NIF_TERM head, tail;
    BIGNUM *dsa_p = NULL, *dsa_q = NULL, *dsa_g = NULL, *dsa_y = NULL;

    if (!enif_get_list_cell(env, key, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa_p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa_q)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa_g)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa_y)
	|| !enif_is_empty_list(env,tail)) {
	if (dsa_p) BN_free(dsa_p);
	if (dsa_q) BN_free(dsa_q);
	if (dsa_g) BN_free(dsa_g);
	if (dsa_y) BN_free(dsa_y);
	return 0;
    }

    DSA_set0_pqg(dsa, dsa_p, dsa_q, dsa_g);
    DSA_set0_key(dsa, dsa_y, NULL);
    return 1;
}
