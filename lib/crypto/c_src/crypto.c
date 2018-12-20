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

/*
 * Purpose:  Dynamically loadable NIF library for cryptography.
 * Based on OpenSSL.
 */

#include "common.h"

#include "aead.h"
#include "aes.h"
#include "block.h"
#include "bn.h"
#include "chacha20.h"
#include "cipher.h"
#include "cmac.h"
#include "dh.h"
#include "digest.h"
#include "dss.h"
#include "ec.h"
#include "ecdh.h"
#include "eddsa.h"
#include "engine.h"
#include "evp.h"
#include "fips.h"
#include "hash.h"
#include "hmac.h"
#include "info.h"
#include "math.h"
#include "pkey.h"
#include "poly1305.h"
#include "rand.h"
#include "rc4.h"
#include "rsa.h"
#include "srp.h"

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* helpers */
static void init_algorithms_types(ErlNifEnv*);

static int library_refc = 0; /* number of users of this dynamic library */
static int library_initialized = 0;

static ErlNifFunc nif_funcs[] = {
    {"info_lib", 0, info_lib},
    {"info_fips", 0, info_fips},
    {"enable_fips_mode", 1, enable_fips_mode},
    {"algorithms", 0, algorithms},
    {"hash_nif", 2, hash_nif},
    {"hash_init_nif", 1, hash_init_nif},
    {"hash_update_nif", 2, hash_update_nif},
    {"hash_final_nif", 1, hash_final_nif},
    {"hmac_nif", 3, hmac_nif},
    {"hmac_nif", 4, hmac_nif},
    {"hmac_init_nif", 2, hmac_init_nif},
    {"hmac_update_nif", 2, hmac_update_nif},
    {"hmac_final_nif", 1, hmac_final_nif},
    {"hmac_final_nif", 2, hmac_final_nif},
    {"cmac_nif", 3, cmac_nif},
    {"block_crypt_nif", 5, block_crypt_nif},
    {"block_crypt_nif", 4, block_crypt_nif},
    {"aes_ige_crypt_nif", 4, aes_ige_crypt_nif},
    {"aes_ctr_stream_init", 2, aes_ctr_stream_init},
    {"aes_ctr_stream_encrypt", 2, aes_ctr_stream_encrypt},
    {"aes_ctr_stream_decrypt", 2, aes_ctr_stream_encrypt},
    {"strong_rand_bytes_nif", 1, strong_rand_bytes_nif},
    {"strong_rand_range_nif", 1, strong_rand_range_nif},
    {"rand_uniform_nif", 2, rand_uniform_nif},
    {"mod_exp_nif", 4, mod_exp_nif},
    {"do_exor", 2, do_exor},
    {"rc4_set_key", 1, rc4_set_key},
    {"rc4_encrypt_with_state", 2, rc4_encrypt_with_state},
    {"pkey_sign_nif", 5, pkey_sign_nif},
    {"pkey_verify_nif", 6, pkey_verify_nif},
    {"pkey_crypt_nif", 6, pkey_crypt_nif},
    {"rsa_generate_key_nif", 2, rsa_generate_key_nif},
    {"dh_generate_key_nif", 4, dh_generate_key_nif},
    {"dh_compute_key_nif", 3, dh_compute_key_nif},
    {"evp_compute_key_nif", 3, evp_compute_key_nif},
    {"evp_generate_key_nif", 1, evp_generate_key_nif},
    {"privkey_to_pubkey_nif", 2, privkey_to_pubkey_nif},
    {"srp_value_B_nif", 5, srp_value_B_nif},
    {"srp_user_secret_nif", 7, srp_user_secret_nif},
    {"srp_host_secret_nif", 5, srp_host_secret_nif},

    {"ec_key_generate", 2, ec_key_generate},
    {"ecdh_compute_key_nif", 3, ecdh_compute_key_nif},

    {"rand_seed_nif", 1, rand_seed_nif},

    {"aead_encrypt", 6, aead_encrypt},
    {"aead_decrypt", 6, aead_decrypt},

    {"chacha20_stream_init",    2, chacha20_stream_init},
    {"chacha20_stream_encrypt", 2, chacha20_stream_crypt},
    {"chacha20_stream_decrypt", 2, chacha20_stream_crypt},

    {"poly1305_nif", 2, poly1305_nif},

    {"engine_by_id_nif", 1, engine_by_id_nif},
    {"engine_init_nif", 1, engine_init_nif},
    {"engine_finish_nif", 1, engine_finish_nif},
    {"engine_free_nif", 1, engine_free_nif},
    {"engine_load_dynamic_nif", 0, engine_load_dynamic_nif},
    {"engine_ctrl_cmd_strings_nif", 3, engine_ctrl_cmd_strings_nif},
    {"engine_register_nif", 2, engine_register_nif},
    {"engine_unregister_nif", 2, engine_unregister_nif},
    {"engine_add_nif", 1, engine_add_nif},
    {"engine_remove_nif", 1, engine_remove_nif},
    {"engine_get_first_nif", 0, engine_get_first_nif},
    {"engine_get_next_nif", 1, engine_get_next_nif},
    {"engine_get_id_nif", 1, engine_get_id_nif},
    {"engine_get_name_nif", 1, engine_get_name_nif},
    {"engine_get_all_methods_nif", 0, engine_get_all_methods_nif}

};

ERL_NIF_INIT(crypto,nif_funcs,load,NULL,upgrade,unload)


static int verify_lib_version(void)
{
    const unsigned long libv = SSLeay();
    const unsigned long hdrv = OPENSSL_VERSION_NUMBER;

#   define MAJOR_VER(V) ((unsigned long)(V) >> (7*4))

    if (MAJOR_VER(libv) != MAJOR_VER(hdrv)) {
	PRINTF_ERR2("CRYPTO: INCOMPATIBLE SSL VERSION"
		    " lib=%lx header=%lx\n", libv, hdrv);
	return 0;
    }
    return 1;
}

static int initialize(ErlNifEnv* env, ERL_NIF_TERM load_info)
{
#ifdef OPENSSL_THREADS
    ErlNifSysInfo sys_info;
#endif
    get_crypto_callbacks_t* funcp;
    struct crypto_callbacks* ccb;
    int nlocks = 0;
    int tpl_arity;
    const ERL_NIF_TERM* tpl_array;
    int vernum;
    ErlNifBinary lib_bin;
    char lib_buf[1000];

    if (!verify_lib_version())
	return __LINE__;

    /* load_info: {302, <<"/full/path/of/this/library">>,true|false} */
    if (!enif_get_tuple(env, load_info, &tpl_arity, &tpl_array)
	|| tpl_arity != 3
	|| !enif_get_int(env, tpl_array[0], &vernum)
	|| vernum != 302
	|| !enif_inspect_binary(env, tpl_array[1], &lib_bin)) {

	PRINTF_ERR1("CRYPTO: Invalid load_info '%T'", load_info);
	return __LINE__;
    }

    hmac_context_rtype = enif_open_resource_type(env, NULL, "hmac_context",
						 (ErlNifResourceDtor*) hmac_context_dtor,
						 ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
						 NULL);
    if (!hmac_context_rtype) {
	PRINTF_ERR0("CRYPTO: Could not open resource type 'hmac_context'");
	return __LINE__;
    }
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
    evp_md_ctx_rtype = enif_open_resource_type(env, NULL, "EVP_MD_CTX",
                                               (ErlNifResourceDtor*) evp_md_ctx_dtor,
                                               ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                               NULL);
    if (!evp_md_ctx_rtype) {
        PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_MD_CTX'");
        return __LINE__;
    }
#endif
#ifdef HAVE_EVP_AES_CTR
    evp_cipher_ctx_rtype = enif_open_resource_type(env, NULL, "EVP_CIPHER_CTX",
                                                   (ErlNifResourceDtor*) evp_cipher_ctx_dtor,
                                                   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                   NULL);
    if (!evp_cipher_ctx_rtype) {
        PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_CIPHER_CTX'");
        return __LINE__;
    }
#endif
#ifdef HAS_ENGINE_SUPPORT
    engine_ctx_rtype = enif_open_resource_type(env, NULL, "ENGINE_CTX",
                                                   (ErlNifResourceDtor*) engine_ctx_dtor,
                                                   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                   NULL);
    if (!engine_ctx_rtype) {
        PRINTF_ERR0("CRYPTO: Could not open resource type 'ENGINE_CTX'");
        return __LINE__;
    }
#endif

    if (library_initialized) {
	/* Repeated loading of this library (module upgrade).
	 * Atoms and callbacks are already set, we are done.
	 */
	return 0;
    }

    if (!init_atoms(env, tpl_array[2], load_info)) {
        return 0;
    }

#ifdef HAVE_DYNAMIC_CRYPTO_LIB
    {
	void* handle;
	if (!change_basename(&lib_bin, lib_buf, sizeof(lib_buf), crypto_callback_name)) {
	    return __LINE__;
	}
	if (!(handle = enif_dlopen(lib_buf, &error_handler, NULL))) {
	    return __LINE__;
	}
	if (!(funcp = (get_crypto_callbacks_t*) enif_dlsym(handle, "get_crypto_callbacks",
							   &error_handler, NULL))) {
	    return __LINE__;
	}
    }
#else /* !HAVE_DYNAMIC_CRYPTO_LIB */
    funcp = &get_crypto_callbacks;
#endif

#ifdef OPENSSL_THREADS
    enif_system_info(&sys_info, sizeof(sys_info));
    if (sys_info.scheduler_threads > 1) {
	nlocks = CRYPTO_num_locks();
    }
    /* else no need for locks */
#endif

    ccb = (*funcp)(nlocks);

    if (!ccb || ccb->sizeof_me != sizeof(*ccb)) {
	PRINTF_ERR0("Invalid 'crypto_callbacks'");
	return __LINE__;
    }

    CRYPTO_set_mem_functions(ccb->crypto_alloc, ccb->crypto_realloc, ccb->crypto_free);

#ifdef OPENSSL_THREADS
    if (nlocks > 0) {
	CRYPTO_set_locking_callback(ccb->locking_function);
	CRYPTO_set_id_callback(ccb->id_function);
	CRYPTO_set_dynlock_create_callback(ccb->dyn_create_function);
	CRYPTO_set_dynlock_lock_callback(ccb->dyn_lock_function);
	CRYPTO_set_dynlock_destroy_callback(ccb->dyn_destroy_function);
    }
#endif /* OPENSSL_THREADS */

    init_digest_types(env);
    init_cipher_types(env);
    init_algorithms_types(env);

    library_initialized = 1;
    return 0;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    int errline = initialize(env, load_info);
    if (errline) {
	return errline;
    }

    *priv_data = NULL;
    library_refc++;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    int errline;
    if (*old_priv_data != NULL) {
	return __LINE__; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
	return __LINE__; /* Don't know how to do that */
    }
    errline = initialize(env, load_info);
    if (errline) {
	return errline;
    }
    library_refc++;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    --library_refc;
}

static int algo_hash_cnt, algo_hash_fips_cnt;
static ERL_NIF_TERM algo_hash[12];   /* increase when extending the list */
static int algo_pubkey_cnt, algo_pubkey_fips_cnt;
static ERL_NIF_TERM algo_pubkey[12]; /* increase when extending the list */
static int algo_cipher_cnt, algo_cipher_fips_cnt;
static ERL_NIF_TERM algo_cipher[25]; /* increase when extending the list */
static int algo_mac_cnt, algo_mac_fips_cnt;
static ERL_NIF_TERM algo_mac[3]; /* increase when extending the list */
static int algo_curve_cnt, algo_curve_fips_cnt;
static ERL_NIF_TERM algo_curve[89]; /* increase when extending the list */
static int algo_rsa_opts_cnt, algo_rsa_opts_fips_cnt;
static ERL_NIF_TERM algo_rsa_opts[11]; /* increase when extending the list */

static void init_algorithms_types(ErlNifEnv* env)
{
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
    // Non-validated algorithms follow
    algo_hash_fips_cnt = algo_hash_cnt;
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "md4");
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "md5");
    algo_hash[algo_hash_cnt++] = enif_make_atom(env, "ripemd160");

    algo_pubkey_cnt = 0;
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "rsa");
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "dss");
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "dh");
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
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "srp");

    // Validated algorithms first
    algo_cipher_cnt = 0;
#ifndef OPENSSL_NO_DES
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des3_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des_ede3");
#ifdef HAVE_DES_ede3_cfb_encrypt
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des3_cbf");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des3_cfb");
#endif
#endif
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc128");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cfb8");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cfb128");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc256");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ctr");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ecb");
#if defined(HAVE_GCM)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"aes_gcm");
#endif
#if defined(HAVE_CCM)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"aes_ccm");
#endif
    // Non-validated algorithms follow
    algo_cipher_fips_cnt = algo_cipher_cnt;
#ifdef HAVE_AES_IGE
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"aes_ige256");
#endif
#ifndef OPENSSL_NO_DES
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"des_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"des_cfb");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"des_ecb");
#endif
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_cfb64");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_ofb64");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_ecb");
#ifndef OPENSSL_NO_RC2
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"rc2_cbc");
#endif
#ifndef OPENSSL_NO_RC4
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"rc4");
#endif
#if defined(HAVE_CHACHA20_POLY1305)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"chacha20_poly1305");
#endif
#if defined(HAVE_CHACHA20)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"chacha20");
#endif
 
    // Validated algorithms first
    algo_mac_cnt = 0;
    algo_mac[algo_mac_cnt++] = enif_make_atom(env,"hmac");
#ifdef HAVE_CMAC
    algo_mac[algo_mac_cnt++] = enif_make_atom(env,"cmac");
#endif
#ifdef HAVE_POLY1305
    algo_mac[algo_mac_cnt++] = enif_make_atom(env,"poly1305");
#endif
    // Non-validated algorithms follow
    algo_mac_fips_cnt = algo_mac_cnt;

    // Validated algorithms first
    algo_curve_cnt = 0;
#if defined(HAVE_EC)
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp160k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp160r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp160r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp192r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp192k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp224k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp224r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp256k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp256r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp384r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp521r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime192v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime192v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime192v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime239v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime239v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime239v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"prime256v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls7");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls9");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls12");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP160r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP160t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP192r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP192t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP224r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP224t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP256r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP256t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP320r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP320t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP384r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP384t1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP512r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"brainpoolP512t1");
#if !defined(OPENSSL_NO_EC2M)
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect163k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect163r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect163r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect193r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect193r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect233k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect233r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect239k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect283k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect283r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect409k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect409r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect571k1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect571r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb163v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb163v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb163v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb176v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb191v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb191v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb191v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb208w1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb239v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb239v2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb239v3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb272w1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb304w1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb359v1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2pnb368w1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"c2tnb431r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls5");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls10");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls11");
#endif
#endif
    // Non-validated algorithms follow
    algo_curve_fips_cnt = algo_curve_cnt;
#if defined(HAVE_EC)
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp112r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp112r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp128r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"secp128r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls6");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls8");
#if !defined(OPENSSL_NO_EC2M)
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect113r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect113r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect131r1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"sect131r2");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls1");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"wtls4");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"ipsec3");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"ipsec4");
#endif
#endif
    //--
#ifdef HAVE_EDDSA
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"ed25519");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"ed448");
#endif
#ifdef HAVE_ED_CURVE_DH
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"x25519");
    algo_curve[algo_curve_cnt++] = enif_make_atom(env,"x448");
#endif

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


    // Check that the max number of algos is updated
    ASSERT(algo_hash_cnt <= sizeof(algo_hash)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_pubkey_cnt <= sizeof(algo_pubkey)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_cipher_cnt <= sizeof(algo_cipher)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_mac_cnt <= sizeof(algo_mac)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_curve_cnt <= sizeof(algo_curve)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_rsa_opts_cnt <= sizeof(algo_rsa_opts)/sizeof(ERL_NIF_TERM));
}

static ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef FIPS_SUPPORT
    int fips_mode  = FIPS_mode();
    int hash_cnt   = fips_mode ? algo_hash_fips_cnt   : algo_hash_cnt;
    int pubkey_cnt = fips_mode ? algo_pubkey_fips_cnt : algo_pubkey_cnt;
    int cipher_cnt = fips_mode ? algo_cipher_fips_cnt : algo_cipher_cnt;
    int mac_cnt    = fips_mode ? algo_mac_fips_cnt    : algo_mac_cnt;
    int curve_cnt    = fips_mode ? algo_curve_fips_cnt    : algo_curve_cnt;
    int rsa_opts_cnt = fips_mode ? algo_rsa_opts_fips_cnt : algo_rsa_opts_cnt;
#else
    int hash_cnt   = algo_hash_cnt;
    int pubkey_cnt = algo_pubkey_cnt;
    int cipher_cnt = algo_cipher_cnt;
    int mac_cnt    = algo_mac_cnt;
    int curve_cnt    = algo_curve_cnt;
    int rsa_opts_cnt = algo_rsa_opts_cnt;
#endif
    return enif_make_tuple6(env,
                            enif_make_list_from_array(env, algo_hash,   hash_cnt),
			    enif_make_list_from_array(env, algo_pubkey, pubkey_cnt),
			    enif_make_list_from_array(env, algo_cipher, cipher_cnt),
                            enif_make_list_from_array(env, algo_mac,    mac_cnt),
			    enif_make_list_from_array(env, algo_curve,  curve_cnt),
			    enif_make_list_from_array(env, algo_rsa_opts, rsa_opts_cnt)
                            );
}
