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
#include "hash.h"
#include "hmac.h"
#include "info.h"
#include "math.h"
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
static ERL_NIF_TERM info_fips(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enable_fips_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pkey_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pkey_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pkey_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM privkey_to_pubkey_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

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

static ERL_NIF_TERM info_fips(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef FIPS_SUPPORT
    return FIPS_mode() ? atom_enabled : atom_not_enabled;
#else
    return atom_not_supported;
#endif
}

static ERL_NIF_TERM enable_fips_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Boolean) */
    if (argv[0] == atom_true) {
#ifdef FIPS_SUPPORT
        if (FIPS_mode_set(1)) {
            return atom_true;
        }
#endif
        PRINTF_ERR0("CRYPTO: Could not setup FIPS mode");
        return atom_false;
    } else if (argv[0] == atom_false) {
#ifdef FIPS_SUPPORT
        if (!FIPS_mode_set(0)) {
            return atom_false;
        }
#endif
        return atom_true;
    } else {
        return enif_make_badarg(env);
    }
}

/*================================================================*/
#define PKEY_BADARG -1
#define PKEY_NOTSUP 0
#define PKEY_OK 1

typedef struct PKeyCryptOptions {
    const EVP_MD *rsa_mgf1_md;
    ErlNifBinary rsa_oaep_label;
    const EVP_MD *rsa_oaep_md;
    int rsa_padding;
    const EVP_MD *signature_md;
} PKeyCryptOptions;

typedef struct PKeySignOptions {
    const EVP_MD *rsa_mgf1_md;
    int rsa_padding;
    int rsa_pss_saltlen;
} PKeySignOptions;

static int get_pkey_digest_type(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM type,
				const EVP_MD **md)
{
    struct digest_type_t *digp = NULL;
    *md = NULL;

    if (type == atom_none && algorithm == atom_rsa) return PKEY_OK;
#ifdef HAVE_EDDSA
    if (algorithm == atom_eddsa) return PKEY_OK;
#endif
    digp = get_digest_type(type);
    if (!digp) return PKEY_BADARG;
    if (!digp->md.p) return PKEY_NOTSUP;

    *md = digp->md.p;
    return PKEY_OK;
}


static int get_pkey_sign_digest(ErlNifEnv *env, ERL_NIF_TERM algorithm,
				ERL_NIF_TERM type, ERL_NIF_TERM data,
				unsigned char *md_value, const EVP_MD **mdp,
				unsigned char **tbsp, size_t *tbslenp)
{
    int i;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    ErlNifBinary tbs_bin;
    EVP_MD_CTX *mdctx;
    const EVP_MD *md = *mdp;
    unsigned char *tbs = *tbsp;
    size_t tbslen = *tbslenp;
    unsigned int tbsleni;

    if ((i = get_pkey_digest_type(env, algorithm, type, &md)) != PKEY_OK) {
	return i;
    }
    if (enif_get_tuple(env, data, &tpl_arity, &tpl_terms)) {
	if (tpl_arity != 2 || tpl_terms[0] != atom_digest
	    || !enif_inspect_binary(env, tpl_terms[1], &tbs_bin)
	    || (md != NULL && tbs_bin.size != EVP_MD_size(md))) {
	    return PKEY_BADARG;
	}
        /* We have a digest (= hashed text) in tbs_bin */
	tbs = tbs_bin.data;
	tbslen = tbs_bin.size;
    } else if (md == NULL) {
	if (!enif_inspect_binary(env, data, &tbs_bin)) {
	    return PKEY_BADARG;
	}
        /* md == NULL, that is no hashing because DigestType argument was atom_none */
	tbs = tbs_bin.data;
	tbslen = tbs_bin.size;
    } else {
	if (!enif_inspect_binary(env, data, &tbs_bin)) {
	    return PKEY_BADARG;
	}
        /* We have the cleartext in tbs_bin and the hash algo info in md */
	tbs = md_value;
	mdctx = EVP_MD_CTX_create();
	if (!mdctx) {
	    return PKEY_BADARG;
	}
        /* Looks well, now hash the plain text into a digest according to md */
	if (EVP_DigestInit_ex(mdctx, md, NULL) <= 0) {
	    EVP_MD_CTX_destroy(mdctx);
	    return PKEY_BADARG;
	}
	if (EVP_DigestUpdate(mdctx, tbs_bin.data, tbs_bin.size) <= 0) {
	    EVP_MD_CTX_destroy(mdctx);
	    return PKEY_BADARG;
	}
	if (EVP_DigestFinal_ex(mdctx, tbs, &tbsleni) <= 0) {
	    EVP_MD_CTX_destroy(mdctx);
	    return PKEY_BADARG;
	}
	tbslen = (size_t)(tbsleni);
	EVP_MD_CTX_destroy(mdctx);
    }

    *mdp = md;
    *tbsp = tbs;
    *tbslenp = tbslen;

    return PKEY_OK;
}


static int get_pkey_sign_options(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM options,
                                 const EVP_MD *md, PKeySignOptions *opt)
{
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    const EVP_MD *opt_md;
    int i;

    if (!enif_is_list(env, options)) {
	return PKEY_BADARG;
    }

    /* defaults */
    if (algorithm == atom_rsa) {
	opt->rsa_mgf1_md = NULL;
	opt->rsa_padding = RSA_PKCS1_PADDING;
	opt->rsa_pss_saltlen = -2;
    }

    if (enif_is_empty_list(env, options)) {
	return PKEY_OK;
    }

    if (algorithm == atom_rsa) {
	tail = options;
	while (enif_get_list_cell(env, tail, &head, &tail)) {
	    if (enif_get_tuple(env, head, &tpl_arity, &tpl_terms) && tpl_arity == 2) {
		if (tpl_terms[0] == atom_rsa_mgf1_md && enif_is_atom(env, tpl_terms[1])) {
		    i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
		    if (i != PKEY_OK) {
			return i;
		    }
		    opt->rsa_mgf1_md = opt_md;
		} else if (tpl_terms[0] == atom_rsa_padding) {
		    if (tpl_terms[1] == atom_rsa_pkcs1_padding) {
			opt->rsa_padding = RSA_PKCS1_PADDING;
                    } else if (tpl_terms[1] == atom_rsa_pkcs1_pss_padding) {
#ifdef HAVE_RSA_PKCS1_PSS_PADDING
                        opt->rsa_padding = RSA_PKCS1_PSS_PADDING;
                        if (opt->rsa_mgf1_md == NULL) {
                            opt->rsa_mgf1_md = md;
                        }
#else
                        return PKEY_NOTSUP;
#endif
		    } else if (tpl_terms[1] == atom_rsa_x931_padding) {
			opt->rsa_padding = RSA_X931_PADDING;
		    } else if (tpl_terms[1] == atom_rsa_no_padding) {
			opt->rsa_padding = RSA_NO_PADDING;
		    } else {
			return PKEY_BADARG;
		    }
		} else if (tpl_terms[0] == atom_rsa_pss_saltlen) {
		    if (!enif_get_int(env, tpl_terms[1], &(opt->rsa_pss_saltlen))
			|| opt->rsa_pss_saltlen < -2) {
			return PKEY_BADARG;
		    }
		} else {
		    return PKEY_BADARG;
		}
	    } else {
		return PKEY_BADARG;
	    }
	}
    } else {
	return PKEY_BADARG;
    }

    return PKEY_OK;
}


static int get_pkey_private_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    if (enif_is_map(env, key)) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;
        char *id = NULL;
        char *password;

        if (!get_engine_and_key_id(env, key, &id, &e))
            return PKEY_BADARG;
        password = get_key_password(env, key);
        *pkey = ENGINE_load_private_key(e, id, NULL, password);
        if (password) enif_free(password);
        enif_free(id);
        if (!*pkey)
            return PKEY_BADARG;
#else
        return PKEY_BADARG;
#endif
    }
    else if (algorithm == atom_rsa) {
	RSA *rsa = RSA_new();

	if (!get_rsa_private_key(env, key, rsa)) {
	    RSA_free(rsa);
	    return PKEY_BADARG;
	}

	*pkey = EVP_PKEY_new();
	if (!EVP_PKEY_assign_RSA(*pkey, rsa)) {
	    EVP_PKEY_free(*pkey);
	    RSA_free(rsa);
	    return PKEY_BADARG;
	}
    } else if (algorithm == atom_ecdsa) {
#if defined(HAVE_EC)
	EC_KEY *ec = NULL;
	const ERL_NIF_TERM *tpl_terms;
	int tpl_arity;

	if (enif_get_tuple(env, key, &tpl_arity, &tpl_terms) && tpl_arity == 2
	    && enif_is_tuple(env, tpl_terms[0]) && enif_is_binary(env, tpl_terms[1])
	    && get_ec_key(env, tpl_terms[0], tpl_terms[1], atom_undefined, &ec)) {

	    *pkey = EVP_PKEY_new();
	    if (!EVP_PKEY_assign_EC_KEY(*pkey, ec)) {
		EVP_PKEY_free(*pkey);
		EC_KEY_free(ec);
		return PKEY_BADARG;
	    }
	} else {
	    return PKEY_BADARG;
	}
#else
	return PKEY_NOTSUP;
#endif
    } else if (algorithm == atom_eddsa) {
#if defined(HAVE_EDDSA)
        if (!get_eddsa_key(env, 0, key, pkey)) {
            return PKEY_BADARG;
        }
#else
     return PKEY_NOTSUP;  
#endif
    } else if (algorithm == atom_dss) {
	DSA *dsa = DSA_new();

	if (!get_dss_private_key(env, key, dsa)) {
	    DSA_free(dsa);
            return PKEY_BADARG;
        }

	*pkey = EVP_PKEY_new();
	if (!EVP_PKEY_assign_DSA(*pkey, dsa)) {
	    EVP_PKEY_free(*pkey);
	    DSA_free(dsa);
	    return PKEY_BADARG;
	}
    } else {
	return PKEY_BADARG;
    }

    return PKEY_OK;
}


static int get_pkey_public_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key,
			       EVP_PKEY **pkey)
{
    if (enif_is_map(env, key)) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;
        char *id = NULL;
        char *password;

        if (!get_engine_and_key_id(env, key, &id, &e))
            return PKEY_BADARG;
        password = get_key_password(env, key);
        *pkey = ENGINE_load_public_key(e, id, NULL, password);
        if (password) enif_free(password);
        enif_free(id);
        if (!pkey)
            return PKEY_BADARG;
#else
        return PKEY_BADARG;
#endif
    } else  if (algorithm == atom_rsa) {
	RSA *rsa = RSA_new();

	if (!get_rsa_public_key(env, key, rsa)) {
	    RSA_free(rsa);
	    return PKEY_BADARG;
	}

	*pkey = EVP_PKEY_new();
	if (!EVP_PKEY_assign_RSA(*pkey, rsa)) {
	    EVP_PKEY_free(*pkey);
	    RSA_free(rsa);
	    return PKEY_BADARG;
	}
    } else if (algorithm == atom_ecdsa) {
#if defined(HAVE_EC)
	EC_KEY *ec = NULL;
	const ERL_NIF_TERM *tpl_terms;
	int tpl_arity;

	if (enif_get_tuple(env, key, &tpl_arity, &tpl_terms) && tpl_arity == 2
	    && enif_is_tuple(env, tpl_terms[0]) && enif_is_binary(env, tpl_terms[1])
	    && get_ec_key(env, tpl_terms[0], atom_undefined, tpl_terms[1], &ec)) {

	    *pkey = EVP_PKEY_new();
	    if (!EVP_PKEY_assign_EC_KEY(*pkey, ec)) {
		EVP_PKEY_free(*pkey);
		EC_KEY_free(ec);
		return PKEY_BADARG;
	    }
	} else {
	    return PKEY_BADARG;
	}
#else
	return PKEY_NOTSUP;
#endif
    } else if (algorithm == atom_eddsa) {
#if defined(HAVE_EDDSA)
        if (!get_eddsa_key(env, 1, key, pkey)) {
            return PKEY_BADARG;
        }
#else
     return PKEY_NOTSUP;  
#endif
    } else if (algorithm == atom_dss) {
	DSA *dsa = DSA_new();

	if (!get_dss_public_key(env, key, dsa)) {
	    DSA_free(dsa);
	    return PKEY_BADARG;
	}

	*pkey = EVP_PKEY_new();
	if (!EVP_PKEY_assign_DSA(*pkey, dsa)) {
	    EVP_PKEY_free(*pkey);
	    DSA_free(dsa);
	    return PKEY_BADARG;
	}
    } else {
	return PKEY_BADARG;
    }

    return PKEY_OK;
}

static ERL_NIF_TERM pkey_sign_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Type, Data|{digest,Digest}, Key|#{}, Options) */
    int i;
    const EVP_MD *md = NULL;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    EVP_PKEY *pkey;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx;
    size_t siglen;
#else
    unsigned len, siglen;
#endif
    PKeySignOptions sig_opt;
    ErlNifBinary sig_bin; /* signature */
    unsigned char *tbs; /* data to be signed */
    size_t tbslen;
/*char buf[1024];
enif_get_atom(env,argv[0],buf,1024,ERL_NIF_LATIN1); printf("algo=%s ",buf);
enif_get_atom(env,argv[1],buf,1024,ERL_NIF_LATIN1); printf("hash=%s ",buf);
printf("\r\n");
*/

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[3])) {
        return atom_notsup;
    }
#endif

    i = get_pkey_sign_digest(env, argv[0], argv[1], argv[2], md_value, &md, &tbs, &tbslen);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    i = get_pkey_sign_options(env, argv[0], argv[4], md, &sig_opt);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    if (get_pkey_private_key(env, argv[0], argv[3], &pkey) != PKEY_OK) {
	return enif_make_badarg(env);
    }

#ifdef HAS_EVP_PKEY_CTX
    ctx = EVP_PKEY_CTX_new(pkey, NULL);
    if (!ctx) goto badarg;

    if (argv[0] != atom_eddsa) {
        if (EVP_PKEY_sign_init(ctx) <= 0) goto badarg;
        if (md != NULL && EVP_PKEY_CTX_set_signature_md(ctx, md) <= 0) goto badarg;
    }

    if (argv[0] == atom_rsa) {
	if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) <= 0) goto badarg;
# ifdef HAVE_RSA_PKCS1_PSS_PADDING
	if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
            if (sig_opt.rsa_mgf1_md != NULL) {
# ifdef HAVE_RSA_MGF1_MD
		if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) <= 0) goto badarg;
# else
                EVP_PKEY_CTX_free(ctx);
                EVP_PKEY_free(pkey);
                return atom_notsup;
# endif
            }
	    if (sig_opt.rsa_pss_saltlen > -2
		&& EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) <= 0)
		goto badarg;
	}
#endif
    }

    if (argv[0] == atom_eddsa) {
#ifdef HAVE_EDDSA
        EVP_MD_CTX* mdctx = EVP_MD_CTX_new();
        if (!EVP_DigestSignInit(mdctx, NULL, NULL, NULL, pkey)) {
            if (mdctx) EVP_MD_CTX_free(mdctx);
            goto badarg;
        }

        if (!EVP_DigestSign(mdctx, NULL, &siglen, tbs, tbslen)) {
            EVP_MD_CTX_free(mdctx);
            goto badarg;
        }
        enif_alloc_binary(siglen, &sig_bin);

        if (!EVP_DigestSign(mdctx, sig_bin.data, &siglen, tbs, tbslen)) {
            EVP_MD_CTX_free(mdctx);
            goto badarg;
        }
        EVP_MD_CTX_free(mdctx);
#else
        goto badarg;    
#endif
    }
    else
    {
        if (EVP_PKEY_sign(ctx, NULL, &siglen, tbs, tbslen) <= 0) goto badarg;
        enif_alloc_binary(siglen, &sig_bin);

        if (md != NULL) {
            ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
        }
        i = EVP_PKEY_sign(ctx, sig_bin.data, &siglen, tbs, tbslen);
    }
        
    EVP_PKEY_CTX_free(ctx);
#else
/*printf("Old interface\r\n");
 */
    if (argv[0] == atom_rsa) {
       RSA *rsa = EVP_PKEY_get1_RSA(pkey);
       enif_alloc_binary(RSA_size(rsa), &sig_bin);
       len = EVP_MD_size(md);
       ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);
       i = RSA_sign(md->type, tbs, len, sig_bin.data, &siglen, rsa);
       RSA_free(rsa);
    } else if (argv[0] == atom_dss) {
       DSA *dsa = EVP_PKEY_get1_DSA(pkey);
       enif_alloc_binary(DSA_size(dsa), &sig_bin);
       len = EVP_MD_size(md);
       ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);
       i = DSA_sign(md->type, tbs, len, sig_bin.data, &siglen, dsa);
       DSA_free(dsa);
    } else if (argv[0] == atom_ecdsa) {
#if defined(HAVE_EC)
       EC_KEY *ec = EVP_PKEY_get1_EC_KEY(pkey);
       enif_alloc_binary(ECDSA_size(ec), &sig_bin);
       len = EVP_MD_size(md);
       ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, len);
       i = ECDSA_sign(md->type, tbs, len, sig_bin.data, &siglen, ec);
       EC_KEY_free(ec);
#else
       EVP_PKEY_free(pkey);
       return atom_notsup;
#endif
    } else {
	goto badarg;
    }
#endif

    EVP_PKEY_free(pkey);
    if (i == 1) {
	ERL_VALGRIND_MAKE_MEM_DEFINED(sig_bin.data, siglen);
	if (siglen != sig_bin.size) {
	    enif_realloc_binary(&sig_bin, siglen);
	    ERL_VALGRIND_ASSERT_MEM_DEFINED(sig_bin.data, siglen);
	}
	return enif_make_binary(env, &sig_bin);
    } else {
	enif_release_binary(&sig_bin);
	return atom_error;
    }

 badarg:
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX_free(ctx);
#endif
    EVP_PKEY_free(pkey);
    return enif_make_badarg(env);
}


static ERL_NIF_TERM pkey_verify_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Type, Data|{digest,Digest}, Signature, Key, Options) */
    int i;
    const EVP_MD *md = NULL;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    EVP_PKEY *pkey;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx;
#else
#endif
    PKeySignOptions sig_opt;
    ErlNifBinary sig_bin; /* signature */
    unsigned char *tbs; /* data to be signed */
    size_t tbslen;

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[4])) {
        return atom_notsup;
    }
#endif

    if (!enif_inspect_binary(env, argv[3], &sig_bin)) {
	return enif_make_badarg(env);
    }

    i = get_pkey_sign_digest(env, argv[0], argv[1], argv[2], md_value, &md, &tbs, &tbslen);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    i = get_pkey_sign_options(env, argv[0], argv[5], md, &sig_opt);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    if (get_pkey_public_key(env, argv[0], argv[4], &pkey) != PKEY_OK) {
	return enif_make_badarg(env);
    }

#ifdef HAS_EVP_PKEY_CTX
/* printf("EVP interface\r\n");
 */
    ctx = EVP_PKEY_CTX_new(pkey, NULL);
    if (!ctx) goto badarg;

    if (argv[0] != atom_eddsa) {
        if (EVP_PKEY_verify_init(ctx) <= 0) goto badarg;
        if (md != NULL && EVP_PKEY_CTX_set_signature_md(ctx, md) <= 0) goto badarg;
    }

    if (argv[0] == atom_rsa) {
	if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) <= 0) goto badarg;
	if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
            if (sig_opt.rsa_mgf1_md != NULL) {
# ifdef HAVE_RSA_MGF1_MD
		if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) <= 0) goto badarg;
# else
                EVP_PKEY_CTX_free(ctx);
                EVP_PKEY_free(pkey);
                return atom_notsup;
# endif
            }
	    if (sig_opt.rsa_pss_saltlen > -2
		&& EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) <= 0)
		goto badarg;
	}
    }

        if (argv[0] == atom_eddsa) {
#ifdef HAVE_EDDSA
        EVP_MD_CTX* mdctx = EVP_MD_CTX_create();
        
        if (!EVP_DigestVerifyInit(mdctx, NULL, NULL, NULL, pkey)) {
            if (mdctx) EVP_MD_CTX_destroy(mdctx);
            goto badarg;
        }

        i = EVP_DigestVerify(mdctx, sig_bin.data, sig_bin.size, tbs, tbslen);
        EVP_MD_CTX_destroy(mdctx);
#else
        goto badarg;    
#endif
        }
    else
        {
            if (md != NULL) {
                ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
            }
            i = EVP_PKEY_verify(ctx, sig_bin.data, sig_bin.size, tbs, tbslen);
        }

    EVP_PKEY_CTX_free(ctx);
#else
/*printf("Old interface\r\n");
*/
    if (argv[0] == atom_rsa) {
        RSA *rsa = EVP_PKEY_get1_RSA(pkey);
        i = RSA_verify(md->type, tbs, tbslen, sig_bin.data, sig_bin.size, rsa);
        RSA_free(rsa);
    } else if (argv[0] == atom_dss) {
        DSA *dsa = EVP_PKEY_get1_DSA(pkey);
        i = DSA_verify(0, tbs, tbslen, sig_bin.data, sig_bin.size, dsa);
        DSA_free(dsa);
    } else if (argv[0] == atom_ecdsa) {
#if defined(HAVE_EC)
        EC_KEY *ec = EVP_PKEY_get1_EC_KEY(pkey);
        i = ECDSA_verify(EVP_MD_type(md), tbs, tbslen, sig_bin.data, sig_bin.size, ec);
        EC_KEY_free(ec);
#else
        EVP_PKEY_free(pkey);
        return atom_notsup;
#endif
    } else {
	goto badarg;
    }
#endif

    EVP_PKEY_free(pkey);
    if (i == 1) {
	return atom_true;
    } else {
	return atom_false;
    }

 badarg:
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX_free(ctx);
#endif
    EVP_PKEY_free(pkey);
    return enif_make_badarg(env);
}


/*--------------------------------*/

static int get_pkey_crypt_options(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM options,
				  PKeyCryptOptions *opt)
{
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    const EVP_MD *opt_md;
    int i;

    if (!enif_is_list(env, options)) {
	return PKEY_BADARG;
    }

    /* defaults */
    if (algorithm == atom_rsa) {
	opt->rsa_mgf1_md = NULL;
	opt->rsa_oaep_label.data = NULL;
	opt->rsa_oaep_label.size = 0;
	opt->rsa_oaep_md = NULL;
	opt->rsa_padding = RSA_PKCS1_PADDING;
	opt->signature_md = NULL;
    }

    if (enif_is_empty_list(env, options)) {
	return PKEY_OK;
    }

    if (algorithm == atom_rsa) {
	tail = options;
	while (enif_get_list_cell(env, tail, &head, &tail)) {
	    if (enif_get_tuple(env, head, &tpl_arity, &tpl_terms) && tpl_arity == 2) {
		if (tpl_terms[0] == atom_rsa_padding
                    || tpl_terms[0] == atom_rsa_pad /* Compatibility */
                    ) {
		    if (tpl_terms[1] == atom_rsa_pkcs1_padding) {
			opt->rsa_padding = RSA_PKCS1_PADDING;
#ifdef HAVE_RSA_OAEP_PADDING
		    } else if (tpl_terms[1] == atom_rsa_pkcs1_oaep_padding) {
			opt->rsa_padding = RSA_PKCS1_OAEP_PADDING;
#endif
#ifdef HAVE_RSA_SSLV23_PADDING
		    } else if (tpl_terms[1] == atom_rsa_sslv23_padding) {
			opt->rsa_padding = RSA_SSLV23_PADDING;
#endif
		    } else if (tpl_terms[1] == atom_rsa_x931_padding) {
			opt->rsa_padding = RSA_X931_PADDING;
		    } else if (tpl_terms[1] == atom_rsa_no_padding) {
			opt->rsa_padding = RSA_NO_PADDING;
		    } else {
			return PKEY_BADARG;
		    }
		} else if (tpl_terms[0] == atom_signature_md && enif_is_atom(env, tpl_terms[1])) {
		    i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
		    if (i != PKEY_OK) {
			return i;
		    }
		    opt->signature_md = opt_md;
		} else if (tpl_terms[0] == atom_rsa_mgf1_md && enif_is_atom(env, tpl_terms[1])) {
#ifndef HAVE_RSA_MGF1_MD
		    if (tpl_terms[1] != atom_sha)
			return PKEY_NOTSUP;
#endif
		    i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
		    if (i != PKEY_OK) {
			return i;
		    }
		    opt->rsa_mgf1_md = opt_md;
		} else if (tpl_terms[0] == atom_rsa_oaep_label
			   && enif_inspect_binary(env, tpl_terms[1], &(opt->rsa_oaep_label))) {
#ifdef HAVE_RSA_OAEP_MD
		    continue;
#else
		    return PKEY_NOTSUP;
#endif
		} else if (tpl_terms[0] == atom_rsa_oaep_md && enif_is_atom(env, tpl_terms[1])) {
#ifndef HAVE_RSA_OAEP_MD
		    if (tpl_terms[1] != atom_sha)
			return PKEY_NOTSUP;
#endif
		    i = get_pkey_digest_type(env, algorithm, tpl_terms[1], &opt_md);
		    if (i != PKEY_OK) {
			return i;
		    }
		    opt->rsa_oaep_md = opt_md;
		} else {
		    return PKEY_BADARG;
		}
	    } else {
		return PKEY_BADARG;
	    }
	}
    } else {
	return PKEY_BADARG;
    }

    return PKEY_OK;
}

static size_t size_of_RSA(EVP_PKEY *pkey) {
    size_t tmplen;
    RSA *rsa = EVP_PKEY_get1_RSA(pkey);
    if (rsa == NULL) return 0;
    tmplen = RSA_size(rsa);
    RSA_free(rsa);
    return tmplen;
}

static ERL_NIF_TERM pkey_crypt_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* (Algorithm, Data, PublKey=[E,N]|[E,N,D]|[E,N,D,P1,P2,E1,E2,C], Options, IsPrivate, IsEncrypt) */
    int i;
    EVP_PKEY *pkey;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx;
#else
    RSA *rsa;
#endif
    PKeyCryptOptions crypt_opt;
    ErlNifBinary in_bin, out_bin, tmp_bin;
    size_t outlen;
#ifdef HAVE_RSA_SSLV23_PADDING
    size_t tmplen;
#endif
    int is_private = (argv[4] == atom_true),
        is_encrypt = (argv[5] == atom_true);
    int algo_init = 0;

/* char algo[1024]; */

#ifndef HAS_ENGINE_SUPPORT
    if (enif_is_map(env, argv[2])) {
        return atom_notsup;
    }
#endif

    if (!enif_inspect_binary(env, argv[1], &in_bin)) {
	return enif_make_badarg(env);
    }

    i = get_pkey_crypt_options(env, argv[0], argv[3], &crypt_opt);
    if (i != PKEY_OK) {
	if (i == PKEY_NOTSUP)
	    return atom_notsup;
	else
	    return enif_make_badarg(env);
    }

    if (is_private) {
	if (get_pkey_private_key(env, argv[0], argv[2], &pkey) != PKEY_OK) {
	    return enif_make_badarg(env);
	}
    } else {
	if (get_pkey_public_key(env, argv[0], argv[2], &pkey) != PKEY_OK) {
	    return enif_make_badarg(env);
	}
    }

    out_bin.data = NULL;
    out_bin.size = 0;
    tmp_bin.data = NULL;
    tmp_bin.size = 0;

#ifdef HAS_EVP_PKEY_CTX
    ctx = EVP_PKEY_CTX_new(pkey, NULL);
    if (!ctx) goto badarg;

/* enif_get_atom(env,argv[0],algo,1024,ERL_NIF_LATIN1);  */

    if (is_private) {
        if (is_encrypt) {
            /* private encrypt */
            if ((algo_init=EVP_PKEY_sign_init(ctx)) <= 0) {
                /* fprintf(stderr,"BADARG %s private encrypt algo_init=%d %s:%d\r\n", algo, algo_init, __FILE__, __LINE__); */
                goto badarg;
            }
        } else {
            /* private decrypt */
            if ((algo_init=EVP_PKEY_decrypt_init(ctx)) <= 0) {
                /* fprintf(stderr,"BADARG %s private decrypt algo_init=%d %s:%d\r\n", algo, algo_init, __FILE__, __LINE__); */
                goto badarg;
            }
        }
    } else {
        if (is_encrypt) {
            /* public encrypt */
            if ((algo_init=EVP_PKEY_encrypt_init(ctx)) <= 0) {
                /* fprintf(stderr,"BADARG %s public encrypt algo_init=%d %s:%d\r\n", algo,algo_init,__FILE__, __LINE__); */
                goto badarg;
            }
        } else {
            /* public decrypt */
            if ((algo_init=EVP_PKEY_verify_recover_init(ctx)) <= 0) {
                /* fprintf(stderr,"BADARG %s public decrypt algo_init=%d %s:%d\r\n", algo,algo_init,__FILE__, __LINE__); */
                goto badarg;
            }
        }
    }

    if (argv[0] == atom_rsa) {
	if (crypt_opt.signature_md != NULL
	    && EVP_PKEY_CTX_set_signature_md(ctx, crypt_opt.signature_md) <= 0)
		goto badarg;
#ifdef HAVE_RSA_SSLV23_PADDING
	if (crypt_opt.rsa_padding == RSA_SSLV23_PADDING) {
	    if (is_encrypt) {
                tmplen = size_of_RSA(pkey);
                if (tmplen == 0) goto badarg;
		if (!enif_alloc_binary(tmplen, &tmp_bin)) goto badarg;
		if (RSA_padding_add_SSLv23(tmp_bin.data, tmplen, in_bin.data, in_bin.size) <= 0)
		    goto badarg;
		in_bin = tmp_bin;
	    }
	    if (EVP_PKEY_CTX_set_rsa_padding(ctx, RSA_NO_PADDING) <= 0) goto badarg;
	} else
#endif
                {
	    if (EVP_PKEY_CTX_set_rsa_padding(ctx, crypt_opt.rsa_padding) <= 0) goto badarg;
        }
#ifdef HAVE_RSA_OAEP_MD
	if (crypt_opt.rsa_padding == RSA_PKCS1_OAEP_PADDING) {
	    if (crypt_opt.rsa_oaep_md != NULL
		&& EVP_PKEY_CTX_set_rsa_oaep_md(ctx, crypt_opt.rsa_oaep_md) <= 0)
		goto badarg;
	    if (crypt_opt.rsa_mgf1_md != NULL
		&& EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, crypt_opt.rsa_mgf1_md) <= 0) goto badarg;
	    if (crypt_opt.rsa_oaep_label.data != NULL && crypt_opt.rsa_oaep_label.size > 0) {
		unsigned char *label_copy = NULL;
		label_copy = OPENSSL_malloc(crypt_opt.rsa_oaep_label.size);
		if (label_copy == NULL) goto badarg;
		memcpy((void *)(label_copy), (const void *)(crypt_opt.rsa_oaep_label.data),
		       crypt_opt.rsa_oaep_label.size);
		if (EVP_PKEY_CTX_set0_rsa_oaep_label(ctx, label_copy,
						     crypt_opt.rsa_oaep_label.size) <= 0) {
		    OPENSSL_free(label_copy);
		    label_copy = NULL;
		    goto badarg;
		}
	    }
	}
#endif
    }

    if (is_private) {
	if (is_encrypt) {
	    /* private_encrypt */
	    i = EVP_PKEY_sign(ctx, NULL, &outlen, in_bin.data, in_bin.size);
	} else {
	    /* private_decrypt */
	    i = EVP_PKEY_decrypt(ctx, NULL, &outlen, in_bin.data, in_bin.size);
	}
    } else {
	if (is_encrypt) {
	    /* public_encrypt */
	    i = EVP_PKEY_encrypt(ctx, NULL, &outlen, in_bin.data, in_bin.size);
	} else {
	    /* public_decrypt */
	    i = EVP_PKEY_verify_recover(ctx, NULL, &outlen, in_bin.data, in_bin.size);
	}
    }
    /* fprintf(stderr,"i = %d %s:%d\r\n", i, __FILE__, __LINE__); */

    if (i != 1) goto badarg;

    enif_alloc_binary(outlen, &out_bin);

    if (is_private) {
	if (is_encrypt) {
	    /* private_encrypt */
	    i = EVP_PKEY_sign(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
	} else {
	    /* private_decrypt */
	    i = EVP_PKEY_decrypt(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
	}
    } else {
	if (is_encrypt) {
	    /* public_encrypt */
	    i = EVP_PKEY_encrypt(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
	} else {
	    /* public_decrypt */
	    i = EVP_PKEY_verify_recover(ctx, out_bin.data, &outlen, in_bin.data, in_bin.size);
	}
    }

#else
    /* Non-EVP cryptolib. Only support RSA */

    if (argv[0] != atom_rsa) {
        algo_init = -2;         /* exitcode: notsup */
        goto badarg;
    }
    rsa = EVP_PKEY_get1_RSA(pkey);
    enif_alloc_binary(RSA_size(rsa), &out_bin);

    if (is_private) {
        if (is_encrypt) {
            /* non-evp rsa private encrypt */
            ERL_VALGRIND_ASSERT_MEM_DEFINED(in_bin.data,in_bin.size);
            i = RSA_private_encrypt(in_bin.size, in_bin.data,
                                    out_bin.data, rsa, crypt_opt.rsa_padding);
            if (i > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, i);
            }
        } else {
            /* non-evp rsa private decrypt */
            i = RSA_private_decrypt(in_bin.size, in_bin.data,
                                    out_bin.data, rsa, crypt_opt.rsa_padding);
            if (i > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, i);
                enif_realloc_binary(&out_bin, i);
            }
        }
    } else {
        if (is_encrypt) {
            /* non-evp rsa public encrypt */
            ERL_VALGRIND_ASSERT_MEM_DEFINED(in_bin.data,in_bin.size);
            i = RSA_public_encrypt(in_bin.size, in_bin.data,
                                   out_bin.data, rsa, crypt_opt.rsa_padding);
            if (i > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, i);
	}
        } else {
            /* non-evp rsa public decrypt */
            i = RSA_public_decrypt(in_bin.size, in_bin.data,
                                   out_bin.data, rsa, crypt_opt.rsa_padding);
            if (i > 0) {
                ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, i);
                enif_realloc_binary(&out_bin, i);
            }
        }
    }

    outlen = i;
    RSA_free(rsa);
#endif

    if ((i > 0) && argv[0] == atom_rsa && !is_encrypt) {
#ifdef HAVE_RSA_SSLV23_PADDING
	if (crypt_opt.rsa_padding == RSA_SSLV23_PADDING) {
	    unsigned char *p;
            tmplen = size_of_RSA(pkey);
	    if (tmplen == 0) goto badarg;
	    if (!enif_alloc_binary(tmplen, &tmp_bin))
                goto badarg;
	    p = out_bin.data;
	    p++;
	    i = RSA_padding_check_SSLv23(tmp_bin.data, tmplen, p, out_bin.size - 1, tmplen);
	    if (i >= 0) {
		outlen = i;
		in_bin = out_bin;
		out_bin = tmp_bin;
		tmp_bin = in_bin;
		i = 1;
	    }
	}
#endif
    }

    if (tmp_bin.data != NULL) {
	enif_release_binary(&tmp_bin);
    }

#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX_free(ctx);
#else
#endif
    EVP_PKEY_free(pkey);
    if (i > 0) {
	ERL_VALGRIND_MAKE_MEM_DEFINED(out_bin.data, outlen);
	if (outlen != out_bin.size) {
	    enif_realloc_binary(&out_bin, outlen);
	    ERL_VALGRIND_ASSERT_MEM_DEFINED(out_bin.data, outlen);
	}
	return enif_make_binary(env, &out_bin);
    } else {
	enif_release_binary(&out_bin);
	return atom_error;
    }

 badarg:
    if (out_bin.data != NULL) {
	enif_release_binary(&out_bin);
    }
    if (tmp_bin.data != NULL) {
	enif_release_binary(&tmp_bin);
    }
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX_free(ctx);
#else
#endif
    EVP_PKEY_free(pkey);
    if (algo_init == -2)
        return atom_notsup;
    else
        return enif_make_badarg(env);
}



/*--------------------------------*/
static ERL_NIF_TERM privkey_to_pubkey_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ /* (Algorithm, PrivKey | KeyMap) */
    EVP_PKEY *pkey;
    ERL_NIF_TERM alg = argv[0];
    ERL_NIF_TERM result[8];
    if (get_pkey_private_key(env, alg, argv[1], &pkey) != PKEY_OK) {
	return enif_make_badarg(env);
    }

    if (alg == atom_rsa) {
        const BIGNUM *n = NULL, *e = NULL, *d = NULL;
        RSA *rsa = EVP_PKEY_get1_RSA(pkey);
        if (rsa) {
            RSA_get0_key(rsa, &n, &e, &d);
            result[0] = bin_from_bn(env, e);  // Exponent E
            result[1] = bin_from_bn(env, n);  // Modulus N = p*q
            RSA_free(rsa);
            EVP_PKEY_free(pkey);
            return enif_make_list_from_array(env, result, 2);
        }

    } else if (argv[0] == atom_dss) {
        const BIGNUM *p = NULL, *q = NULL, *g = NULL, *pub_key = NULL;
        DSA *dsa = EVP_PKEY_get1_DSA(pkey);
        if (dsa) {
            DSA_get0_pqg(dsa, &p, &q, &g);
            DSA_get0_key(dsa, &pub_key, NULL);
            result[0] = bin_from_bn(env, p);
            result[1] = bin_from_bn(env, q);
            result[2] = bin_from_bn(env, g);
            result[3] = bin_from_bn(env, pub_key);
	    DSA_free(dsa);
            EVP_PKEY_free(pkey);
            return enif_make_list_from_array(env, result, 4);
        }

    } else if (argv[0] == atom_ecdsa) {
#if defined(HAVE_EC)
        /* not yet implemented
          EC_KEY *ec = EVP_PKEY_get1_EC_KEY(pkey);
          if (ec) {
          / * Example of result:
               {
                 Curve =  {Field, Prime, Point, Order, CoFactor} =
                    {
                      Field =  {prime_field,<<255,...,255>>},
                      Prime = {<<255,...,252>>,
                               <<90,...,75>>,
                               <<196,...,144>>
                              },
                      Point =    <<4,...,245>>,
                      Order =    <<255,...,81>>,
                      CoFactor = <<1>>
                    },
                Key = <<151,...,62>>
                }
              or
              {
                Curve =
                    {characteristic_two_field,
                     M,
                     Basis = {tpbasis, _}
                           | {ppbasis, k1, k2, k3}
                    },
                Key
               }
        * /
            EVP_PKEY_free(pkey);
            return enif_make_list_from_array(env, ..., ...);
        */
#endif
    }

    if (pkey) EVP_PKEY_free(pkey);
    return enif_make_badarg(env);
}
