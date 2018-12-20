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
#include "algorithms.h"
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
