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
#include "api_ng.h"
#include "bn.h"
#include "cipher.h"
#include "mac.h"
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
#include "rand.h"
#include "rsa.h"
#include "srp.h"

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

static int library_refc = 0; /* number of users of this dynamic library */
static int library_initialized = 0;

static ErlNifFunc nif_funcs[] = {
    {"info_lib", 0, info_lib, 0},
    {"info_fips", 0, info_fips, 0},
    {"enable_fips_mode", 1, enable_fips_mode, 0},
    {"hash_algorithms", 0, hash_algorithms, 0},
    {"pubkey_algorithms", 0, pubkey_algorithms, 0},
    {"cipher_algorithms", 0, cipher_algorithms, 0},
    {"mac_algorithms", 0, mac_algorithms, 0},
    {"curve_algorithms", 0, curve_algorithms, 0},
    {"rsa_opts_algorithms", 0, rsa_opts_algorithms, 0},
    {"hash_info", 1, hash_info_nif, 0},
    {"hash_nif", 2, hash_nif, 0},
    {"hash_init_nif", 1, hash_init_nif, 0},
    {"hash_update_nif", 2, hash_update_nif, 0},
    {"hash_final_nif", 1, hash_final_nif, 0},
    {"mac_nif", 4, mac_nif, 0},
    {"mac_init_nif", 3, mac_init_nif, 0},
    {"mac_update_nif", 2, mac_update_nif, 0},
    {"mac_final_nif", 1, mac_final_nif, 0},
    {"cipher_info_nif", 1, cipher_info_nif, 0},
    {"aes_ige_crypt_nif", 4, aes_ige_crypt_nif, 0},
    {"ng_crypto_init_nif", 4, ng_crypto_init_nif, 0},
    {"ng_crypto_update_nif", 2, ng_crypto_update_nif, 0},
    {"ng_crypto_update_nif", 3, ng_crypto_update_nif, 0},
    {"ng_crypto_one_time_nif", 5, ng_crypto_one_time_nif, 0},
    {"strong_rand_bytes_nif", 1, strong_rand_bytes_nif, 0},
    {"strong_rand_range_nif", 1, strong_rand_range_nif, 0},
    {"rand_uniform_nif", 2, rand_uniform_nif, 0},
    {"mod_exp_nif", 4, mod_exp_nif, 0},
    {"do_exor", 2, do_exor, 0},
    {"pkey_sign_nif", 5, pkey_sign_nif, 0},
    {"pkey_verify_nif", 6, pkey_verify_nif, 0},
    {"pkey_crypt_nif", 6, pkey_crypt_nif, 0},
    {"rsa_generate_key_nif", 2, rsa_generate_key_nif, 0},
    {"dh_generate_key_nif", 4, dh_generate_key_nif, 0},
    {"dh_compute_key_nif", 3, dh_compute_key_nif, 0},
    {"evp_compute_key_nif", 3, evp_compute_key_nif, 0},
    {"evp_generate_key_nif", 1, evp_generate_key_nif, 0},
    {"privkey_to_pubkey_nif", 2, privkey_to_pubkey_nif, 0},
    {"srp_value_B_nif", 5, srp_value_B_nif, 0},
    {"srp_user_secret_nif", 7, srp_user_secret_nif, 0},
    {"srp_host_secret_nif", 5, srp_host_secret_nif, 0},

    {"ec_key_generate", 2, ec_key_generate, 0},
    {"ecdh_compute_key_nif", 3, ecdh_compute_key_nif, 0},

    {"rand_seed_nif", 1, rand_seed_nif, 0},

    {"aead_cipher", 7, aead_cipher, 0},

    {"engine_by_id_nif", 1, engine_by_id_nif, 0},
    {"engine_init_nif", 1, engine_init_nif, 0},
    {"engine_finish_nif", 1, engine_finish_nif, 0},
    {"engine_free_nif", 1, engine_free_nif, 0},
    {"engine_load_dynamic_nif", 0, engine_load_dynamic_nif, 0},
    {"engine_ctrl_cmd_strings_nif", 3, engine_ctrl_cmd_strings_nif, 0},
    {"engine_register_nif", 2, engine_register_nif, 0},
    {"engine_unregister_nif", 2, engine_unregister_nif, 0},
    {"engine_add_nif", 1, engine_add_nif, 0},
    {"engine_remove_nif", 1, engine_remove_nif, 0},
    {"engine_get_first_nif", 0, engine_get_first_nif, 0},
    {"engine_get_next_nif", 1, engine_get_next_nif, 0},
    {"engine_get_id_nif", 1, engine_get_id_nif, 0},
    {"engine_get_name_nif", 1, engine_get_name_nif, 0},
    {"engine_get_all_methods_nif", 0, engine_get_all_methods_nif, 0}
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
#ifdef HAVE_DYNAMIC_CRYPTO_LIB
    void *handle;
#endif

    if (!verify_lib_version())
	return __LINE__;

    /* load_info: {302, <<"/full/path/of/this/library">>,true|false} */
    if (!enif_get_tuple(env, load_info, &tpl_arity, &tpl_array))
        return __LINE__;
    if (tpl_arity != 3)
        return __LINE__;
    if (!enif_get_int(env, tpl_array[0], &vernum))
        return __LINE__;
    if (vernum != 302)
        return __LINE__;
    if (!enif_inspect_binary(env, tpl_array[1], &lib_bin))
        return __LINE__;

#ifdef HAS_EVP_PKEY_CTX
    if (!init_mac_ctx(env)) {
	return __LINE__;
    }
#else
    if (!init_hmac_ctx(env)) {
	return __LINE__;
    }
#endif
    if (!init_hash_ctx(env)) {
        return __LINE__;
    }
    if (!init_cipher_ctx(env)) {
        return __LINE__;
    }
    if (!init_engine_ctx(env)) {
        return __LINE__;
    }

    if (library_initialized) {
	/* Repeated loading of this library (module upgrade).
	 * Atoms and callbacks are already set, we are done.
	 */
	return 0;
    }

    if (!init_atoms(env, tpl_array[2], load_info)) {
        return __LINE__;
    }

#ifdef HAVE_DYNAMIC_CRYPTO_LIB
    if (!change_basename(&lib_bin, lib_buf, sizeof(lib_buf), crypto_callback_name))
        return __LINE__;
    if ((handle = enif_dlopen(lib_buf, &error_handler, NULL)) == NULL)
        return __LINE__;
    if ((funcp = (get_crypto_callbacks_t*) enif_dlsym(handle, "get_crypto_callbacks",
                                                       &error_handler, NULL)) == NULL)
        return __LINE__;
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

#ifdef HAS_CRYPTO_MEM_FUNCTIONS
    if (!CRYPTO_set_mem_functions(ccb->crypto_alloc, ccb->crypto_realloc, ccb->crypto_free))
        return __LINE__;
#endif

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
    init_mac_types(env);
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
