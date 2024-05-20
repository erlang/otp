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

#include "common.h"
#include "algorithms.h"
#include "cipher.h"
#include "mac.h"
#ifdef HAS_3_0_API
#include "digest.h"
#endif

#ifdef HAS_3_0_API
#else
static unsigned int algo_hash_cnt, algo_hash_fips_cnt;
static ERL_NIF_TERM algo_hash[17];   /* increase when extending the list */
void init_hash_types(ErlNifEnv* env);
#endif

static unsigned int algo_pubkey_cnt, algo_pubkey_fips_cnt;
static ERL_NIF_TERM algo_pubkey[12]; /* increase when extending the list */
void init_pubkey_types(ErlNifEnv* env);

static ERL_NIF_TERM algo_curve[2][89]; /* increase when extending the list */
static ErlNifMutex* mtx_init_curve_types;
void init_curve_types(ErlNifEnv* env);
int get_curve_cnt(ErlNifEnv* env, int fips);

static unsigned int algo_rsa_opts_cnt, algo_rsa_opts_fips_cnt;
static ERL_NIF_TERM algo_rsa_opts[11]; /* increase when extending the list */
void init_rsa_opts_types(ErlNifEnv* env);




void init_algorithms_types(ErlNifEnv* env)
{
#ifdef HAS_3_0_API
#else
    init_hash_types(env);
#endif
    init_pubkey_types(env);
    init_curve_types(env);
    init_rsa_opts_types(env);
    /* ciphers and macs are initiated statically */
}


int create_curve_mutex(void)
{
    if (!mtx_init_curve_types) {
        mtx_init_curve_types =  enif_mutex_create("init_curve_types");
    }
    return !!mtx_init_curve_types;
}

void destroy_curve_mutex(void)
{
    if (mtx_init_curve_types) {
        enif_mutex_destroy(mtx_init_curve_types);
        mtx_init_curve_types = NULL;
    }
}

/*================================================================
  Hash algorithms
*/

ERL_NIF_TERM hash_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAS_3_0_API
    return digest_types_as_list(env);
#else
    unsigned int cnt  =
        FIPS_MODE() ? algo_hash_fips_cnt : algo_hash_cnt;

    return enif_make_list_from_array(env, algo_hash, cnt);
#endif
}

#ifdef HAS_3_0_API
#else
void init_hash_types(ErlNifEnv* env) {
    // Validated algorithms first
    algo_hash_cnt = 0;
    algo_hash[algo_hash_cnt++] = atom_sha;
#ifdef HAVE_SHA224
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha224");
#endif
#ifdef HAVE_SHA256
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha256");
#endif
#ifdef HAVE_SHA384
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha384");
#endif
#ifdef HAVE_SHA512
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha512");
#endif
#ifdef HAVE_SHA3_224
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_224");
#endif
#ifdef HAVE_SHA3_256
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_256");
#endif
#ifdef HAVE_SHA3_384
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_384");
#endif
#ifdef HAVE_SHA3_512
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sha3_512");
#endif
#ifdef HAVE_SHAKE128
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "shake128");
#endif
#ifdef HAVE_SHAKE256
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "shake256");
#endif
#ifdef HAVE_SM3
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "sm3");
#endif
#ifdef HAVE_BLAKE2
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "blake2b");
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "blake2s");
#endif

    // Non-validated algorithms follow
    algo_hash_fips_cnt = algo_hash_cnt;
#ifdef HAVE_MD4
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "md4");
#endif
#ifdef HAVE_MD5
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "md5");
#endif
#ifdef HAVE_RIPEMD160
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "ripemd160");
#endif

    ASSERT(algo_hash_cnt <= sizeof(algo_hash)/sizeof(ERL_NIF_TERM));
}
#endif

/*================================================================
  Public key algorithms
*/

ERL_NIF_TERM pubkey_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int cnt  =
        FIPS_MODE() ? algo_pubkey_fips_cnt : algo_pubkey_cnt;

    return enif_make_list_from_array(env, algo_pubkey, cnt);
}

void init_pubkey_types(ErlNifEnv* env) {
    // Validated algorithms first
    algo_pubkey_cnt = 0;
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "rsa");
#ifdef HAVE_DSA
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "dss");
#endif
#ifdef HAVE_DH
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "dh");
#endif
#if defined(HAVE_EC)
#if !defined(OPENSSL_NO_EC2M)
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "ec_gf2m");
#endif
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "ecdsa");
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "ecdh");
#endif
    // Non-validated algorithms follow
    algo_pubkey_fips_cnt = algo_pubkey_cnt;
    // Don't know if Edward curves are fips validated
#if defined(HAVE_EDDSA)
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "eddsa");
#endif
#if defined(HAVE_EDDH)
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "eddh");
#endif
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "srp");

    ASSERT(algo_pubkey_cnt <= sizeof(algo_pubkey)/sizeof(ERL_NIF_TERM));
}


/*================================================================
  Cipher key algorithms
*/

ERL_NIF_TERM cipher_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return cipher_types_as_list(env); /* Exclude old api ciphers */
}


/*================================================================
  MAC key algorithms
*/

ERL_NIF_TERM mac_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return mac_types_as_list(env);
}


/*================================================================
  Curves
*/

ERL_NIF_TERM curve_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fips_mode;
    int algo_curve_cnt;

    fips_mode = FIPS_MODE();
    algo_curve_cnt = get_curve_cnt(env, fips_mode);

    return enif_make_list_from_array(env, algo_curve[fips_mode], algo_curve_cnt);
}

int init_curves(ErlNifEnv* env, int fips);
#if defined(HAVE_EC)
int valid_curve(int nid);
#endif

int get_curve_cnt(ErlNifEnv* env, int fips) {
    static int algo_curve_cnt = -1;
    static int algo_curve_fips_cnt = -1;
    int cnt = 0;
    if (0 == fips && algo_curve_cnt >= 0) {
        return algo_curve_cnt;
    }

    if (1 == fips && algo_curve_fips_cnt >= 0) {
        return algo_curve_fips_cnt;
    }

    enif_mutex_lock(mtx_init_curve_types);
    if (1 == fips) {
        if (algo_curve_fips_cnt >= 0) {
            return algo_curve_fips_cnt;
        }
        algo_curve_fips_cnt = init_curves(env, 1);
        cnt = algo_curve_fips_cnt;
    } else {
        if (algo_curve_cnt >= 0) {
            return algo_curve_cnt;
        }
        algo_curve_cnt = init_curves(env, 0);
        cnt = algo_curve_cnt;
    }
    enif_mutex_unlock(mtx_init_curve_types);

    return cnt;
}

void init_curve_types(ErlNifEnv* env) {
    /* Initialize the curve counters and curve's lists
       by calling get_curve_cnt
    */
#ifdef FIPS_SUPPORT
    if (FIPS_MODE()) {
        // FIPS enabled
        get_curve_cnt(env, 1);
        FIPS_mode_set(0); // disable
        get_curve_cnt(env, 0);
        FIPS_mode_set(1); // re-enable
    } else {
        // FIPS disabled but available
        get_curve_cnt(env, 0);
        FIPS_mode_set(1); // enable
        get_curve_cnt(env, 1);
        FIPS_mode_set(0); // re-disable
    }
#else
    // FIPS mode is not available
    get_curve_cnt(env, 0);
#endif

# ifdef DEBUG
    {
        int curve_cnt = get_curve_cnt(env, 0);
        ASSERT(curve_cnt <= sizeof(algo_curve[0])/sizeof(ERL_NIF_TERM));
    }
# endif 
}


int init_curves(ErlNifEnv* env, int fips) {
#if defined(HAVE_EC)
    int cnt = 0;

#ifdef NID_secp160k1
    if (valid_curve(NID_secp160k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp160k1"); 
#else
#endif
#ifdef NID_secp160r1
    if (valid_curve(NID_secp160r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp160r1"); 
#else
#endif
#ifdef NID_secp160r2
    if (valid_curve(NID_secp160r2)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp160r2"); 
#else
#endif
#ifdef NID_secp192k1
    if (valid_curve(NID_secp192k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp192k1"); 
#else
#endif
#ifdef NID_secp224k1
    if (valid_curve(NID_secp224k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp224k1"); 
#else
#endif
#ifdef NID_secp224r1
    if (valid_curve(NID_secp224r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp224r1"); 
#else
#endif
#ifdef NID_secp256k1
    if (valid_curve(NID_secp256k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp256k1"); 
#else
#endif
#ifdef NID_secp384r1
    if (valid_curve(NID_secp384r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp384r1"); 
#else
#endif
#ifdef NID_secp521r1
    if (valid_curve(NID_secp521r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp521r1"); 
#else
#endif
#ifdef NID_X9_62_prime192v1
    if (valid_curve(NID_X9_62_prime192v1)) {
        algo_curve[fips][cnt++] = enif_make_atom(env,"secp192r1");
        algo_curve[fips][cnt++] = enif_make_atom(env,"prime192v1");
    }
#else
#endif
#ifdef NID_X9_62_prime192v2
    if (valid_curve(NID_X9_62_prime192v2)) algo_curve[fips][cnt++] = enif_make_atom(env,"prime192v2");
#else
#endif
#ifdef NID_X9_62_prime192v3
    if (valid_curve(NID_X9_62_prime192v3)) algo_curve[fips][cnt++] = enif_make_atom(env,"prime192v3");
#else
#endif
#ifdef NID_X9_62_prime239v1
    if (valid_curve(NID_X9_62_prime239v1)) algo_curve[fips][cnt++] = enif_make_atom(env,"prime239v1");
#else
#endif
#ifdef NID_X9_62_prime239v2
    if (valid_curve(NID_X9_62_prime239v2)) algo_curve[fips][cnt++] = enif_make_atom(env,"prime239v2");
#else
#endif
#ifdef NID_X9_62_prime239v3
    if (valid_curve(NID_X9_62_prime239v3)) algo_curve[fips][cnt++] = enif_make_atom(env,"prime239v3");
#else
#endif
#ifdef NID_X9_62_prime256v1
    if (valid_curve(NID_X9_62_prime256v1)) {
        algo_curve[fips][cnt++] = enif_make_atom(env,"secp256r1");
        algo_curve[fips][cnt++] = enif_make_atom(env,"prime256v1");
    }
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls7
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls7)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls7");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls9
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls9)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls9");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls12
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls12)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls12");
#else
#endif
#ifdef NID_brainpoolP160r1
    if (valid_curve(NID_brainpoolP160r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP160r1");
#else
#endif
#ifdef NID_brainpoolP160t1
    if (valid_curve(NID_brainpoolP160t1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP160t1");
#else
#endif
#ifdef NID_brainpoolP192r1
    if (valid_curve(NID_brainpoolP192r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP192r1");
#else
#endif
#ifdef NID_brainpoolP192t1
    if (valid_curve(NID_brainpoolP192t1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP192t1");
#else
#endif
#ifdef NID_brainpoolP224r1
    if (valid_curve(NID_brainpoolP224r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP224r1");
#else
#endif
#ifdef NID_brainpoolP224t1
    if (valid_curve(NID_brainpoolP224t1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP224t1");
#else
#endif
#ifdef NID_brainpoolP256r1
    if (valid_curve(NID_brainpoolP256r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP256r1");
#else
#endif
#ifdef NID_brainpoolP256t1
    if (valid_curve(NID_brainpoolP256t1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP256t1");
#else
#endif
#ifdef NID_brainpoolP320r1
    if (valid_curve(NID_brainpoolP320r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP320r1");
#else
#endif
#ifdef NID_brainpoolP320t1
    if (valid_curve(NID_brainpoolP320t1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP320t1");
#else
#endif
#ifdef NID_brainpoolP384r1
    if (valid_curve(NID_brainpoolP384r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP384r1");
#else
#endif
#ifdef NID_brainpoolP384t1
    if (valid_curve(NID_brainpoolP384t1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP384t1");
#else
#endif
#ifdef NID_brainpoolP512r1
    if (valid_curve(NID_brainpoolP512r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP512r1");
#else
#endif
#ifdef NID_brainpoolP512t1
    if (valid_curve(NID_brainpoolP512t1)) algo_curve[fips][cnt++] = enif_make_atom(env,"brainpoolP512t1");
#else
#endif
    //#if !defined(OPENSSL_NO_EC2M)        
#ifdef NID_sect163k1
    if (valid_curve(NID_sect163k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect163k1"); 
#else
#endif
#ifdef NID_sect163r1
    if (valid_curve(NID_sect163r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect163r1"); 
#else
#endif
#ifdef NID_sect163r2
    if (valid_curve(NID_sect163r2)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect163r2"); 
#else
#endif
#ifdef NID_sect193r1
    if (valid_curve(NID_sect193r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect193r1"); 
#else
#endif
#ifdef NID_sect193r2
    if (valid_curve(NID_sect193r2)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect193r2"); 
#else
#endif
#ifdef NID_sect233k1
    if (valid_curve(NID_sect233k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect233k1"); 
#else
#endif
#ifdef NID_sect233r1
    if (valid_curve(NID_sect233r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect233r1"); 
#else
#endif
#ifdef NID_sect239k1
    if (valid_curve(NID_sect239k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect239k1"); 
#else
#endif
#ifdef NID_sect283k1
    if (valid_curve(NID_sect283k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect283k1"); 
#else
#endif
#ifdef NID_sect283r1
    if (valid_curve(NID_sect283r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect283r1"); 
#else
#endif
#ifdef NID_sect409k1
    if (valid_curve(NID_sect409k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect409k1"); 
#else
#endif
#ifdef NID_sect409r1
    if (valid_curve(NID_sect409r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect409r1"); 
#else
#endif
#ifdef NID_sect571k1
    if (valid_curve(NID_sect571k1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect571k1"); 
#else
#endif
#ifdef NID_sect571r1
    if (valid_curve(NID_sect571r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect571r1"); 
#else
#endif
#ifdef NID_X9_62_c2pnb163v1
    if (valid_curve(NID_X9_62_c2pnb163v1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2pnb163v1");
#else
#endif
#ifdef NID_X9_62_c2pnb163v2
    if (valid_curve(NID_X9_62_c2pnb163v2)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2pnb163v2");
#else
#endif
#ifdef NID_X9_62_c2pnb163v3
    if (valid_curve(NID_X9_62_c2pnb163v3)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2pnb163v3");
#else
#endif
#ifdef NID_X9_62_c2pnb176v1
    if (valid_curve(NID_X9_62_c2pnb176v1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2pnb176v1");
#else
#endif
#ifdef NID_X9_62_c2tnb191v1
    if (valid_curve(NID_X9_62_c2tnb191v1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2tnb191v1");
#else
#endif
#ifdef NID_X9_62_c2tnb191v2
    if (valid_curve(NID_X9_62_c2tnb191v2)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2tnb191v2");
#else
#endif
#ifdef NID_X9_62_c2tnb191v3
    if (valid_curve(NID_X9_62_c2tnb191v3)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2tnb191v3");
#else
#endif
#ifdef NID_X9_62_c2pnb208w1
    if (valid_curve(NID_X9_62_c2pnb208w1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2pnb208w1");
#else
#endif
#ifdef NID_X9_62_c2tnb239v1
    if (valid_curve(NID_X9_62_c2tnb239v1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2tnb239v1");
#else
#endif
#ifdef NID_X9_62_c2tnb239v2
    if (valid_curve(NID_X9_62_c2tnb239v2)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2tnb239v2");
#else
#endif
#ifdef NID_X9_62_c2tnb239v3
    if (valid_curve(NID_X9_62_c2tnb239v3)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2tnb239v3");
#else
#endif
#ifdef NID_X9_62_c2pnb272w1
    if (valid_curve(NID_X9_62_c2pnb272w1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2pnb272w1");
#else
#endif
#ifdef NID_X9_62_c2pnb304w1
    if (valid_curve(NID_X9_62_c2pnb304w1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2pnb304w1");
#else
#endif
#ifdef NID_X9_62_c2tnb359v1
    if (valid_curve(NID_X9_62_c2tnb359v1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2tnb359v1");
#else
#endif
#ifdef NID_X9_62_c2pnb368w1
    if (valid_curve(NID_X9_62_c2pnb368w1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2pnb368w1");
#else
#endif
#ifdef NID_X9_62_c2tnb431r1
    if (valid_curve(NID_X9_62_c2tnb431r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"c2tnb431r1");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls3
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls3)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls3");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls5
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls5)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls5");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls10
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls10)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls10");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls11
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls11)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls11");
#else
#endif
    // Non-validated algorithms follow
#ifdef NID_secp112r1
    if (valid_curve(NID_secp112r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp112r1"); 
#else
#endif
#ifdef NID_secp112r2
    if (valid_curve(NID_secp112r2)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp112r2"); 
#else
#endif
#ifdef NID_secp128r1
    if (valid_curve(NID_secp128r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp128r1"); 
#else
#endif
#ifdef NID_secp128r2
    if (valid_curve(NID_secp128r2)) algo_curve[fips][cnt++] = enif_make_atom(env,"secp128r2"); 
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls6
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls6)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls6");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls8
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls8)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls8");
#else
#endif
    //#if !defined(OPENSSL_NO_EC2M)
#ifdef NID_sect113r1
    if (valid_curve(NID_sect113r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect113r1"); 
#else
#endif
#ifdef NID_sect113r2
    if (valid_curve(NID_sect113r2)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect113r2"); 
#else
#endif
#ifdef NID_sect131r1
    if (valid_curve(NID_sect131r1)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect131r1"); 
#else
#endif
#ifdef NID_sect131r2
    if (valid_curve(NID_sect131r2)) algo_curve[fips][cnt++] = enif_make_atom(env,"sect131r2"); 
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls1
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls1)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls1");
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls4
    if (valid_curve(NID_wap_wsg_idm_ecid_wtls4)) algo_curve[fips][cnt++] = enif_make_atom(env,"wtls4");
#else
#endif
#ifdef NID_ipsec3
    if (valid_curve(NID_ipsec3)) algo_curve[fips][cnt++] = enif_make_atom(env,"ipsec3");
#else
#endif
#ifdef NID_ipsec4
    if (valid_curve(NID_ipsec4)) algo_curve[fips][cnt++] = enif_make_atom(env,"ipsec4");
#else
#endif

    if (!fips) {
#ifdef HAVE_EDDSA
        algo_curve[fips][cnt++] = enif_make_atom(env,"ed25519");
        algo_curve[fips][cnt++] = enif_make_atom(env,"ed448");
#endif
#ifdef HAVE_EDDH
        algo_curve[fips][cnt++] = enif_make_atom(env,"x25519");
        algo_curve[fips][cnt++] = enif_make_atom(env,"x448");
#endif
    }

    return cnt;
#else /* if not HAVE_EC */
    return 0;
#endif
}

#if defined(HAVE_EC)

/* Check if the curve in nid is supported by the
   current cryptolib and current FIPS state.
*/

int valid_curve(int nid) {
    int ret = 0;

#if defined(HAVE_DH)
# if defined(HAS_EVP_PKEY_CTX) && (! DISABLE_EVP_DH)
    EVP_PKEY_CTX *pctx = NULL, *kctx = NULL;
    EVP_PKEY *pkey = NULL, *params = NULL;
    
    if (NULL == (pctx = EVP_PKEY_CTX_new_id(EVP_PKEY_EC, NULL)))
        goto out;
    
    if (1 != EVP_PKEY_paramgen_init(pctx))
        goto out;

    if (1 != EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx, nid))
        goto out;

    if (!EVP_PKEY_paramgen(pctx, &params))
        goto out;

    if (NULL == (kctx = EVP_PKEY_CTX_new(params, NULL)))
        goto out;

    if(1 != EVP_PKEY_keygen_init(kctx))
        goto out;
    if (1 != EVP_PKEY_keygen(kctx, &pkey))
        goto out;
    ret = 1;
 out:
    if (pkey) EVP_PKEY_free(pkey);
    if (kctx) EVP_PKEY_CTX_free(kctx);
    if (params) EVP_PKEY_free(params);
    if (pctx) EVP_PKEY_CTX_free(pctx);

# else
    EC_KEY *key;

    if (NULL == (key = EC_KEY_new_by_curve_name(nid)))
        goto out;

    if(1 != EC_KEY_generate_key(key))
        goto out;

    ret = 1;
 out:
    if (key) EC_KEY_free(key);
# endif
#endif /* HAVE_DH etc */
    
    return ret;
}
#endif /* HAVE_EC */

/*================================================================
  RSA Options
*/

ERL_NIF_TERM rsa_opts_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int cnt  =
        FIPS_MODE() ? algo_rsa_opts_fips_cnt : algo_rsa_opts_cnt;

    return enif_make_list_from_array(env, algo_rsa_opts, cnt);
}

void init_rsa_opts_types(ErlNifEnv* env) {
    // Validated algorithms first
    algo_rsa_opts_cnt = 0;
#ifdef HAS_EVP_PKEY_CTX
# ifdef HAVE_RSA_PKCS1_PSS_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_pss_padding");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pss_saltlen");
# endif
# ifdef HAVE_RSA_MGF1_MD
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_mgf1_md");
# endif
# ifdef HAVE_RSA_OAEP_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_oaep_padding");
# endif
# ifdef HAVE_RSA_OAEP_MD
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_oaep_label");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_oaep_md");
# endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"signature_md");
#endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_pkcs1_padding");
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_x931_padding");
#ifdef HAVE_RSA_SSLV23_PADDING
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_sslv23_padding");
#endif
    algo_rsa_opts[algo_rsa_opts_cnt++] = enif_make_atom(env,"rsa_no_padding");
    algo_rsa_opts_fips_cnt = algo_rsa_opts_cnt;

    ASSERT(algo_rsa_opts_cnt <= sizeof(algo_rsa_opts)/sizeof(ERL_NIF_TERM));
}

