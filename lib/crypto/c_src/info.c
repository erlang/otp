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

#include "info.h"

#if defined(DEBUG)
#  define CB_NAME "crypto_callback.debug"
#  define COMPILE_TYPE "debug"

#elif defined(VALGRIND)
#  define CB_NAME "crypto_callback.valgrind"
#  define COMPILE_TYPE "valgrind"

#elif defined(ADDRESS_SANITIZER)
#  define CB_NAME "crypto_callback.asan"
#  define COMPILE_TYPE "asan"

#else
#  define CB_NAME "crypto_callback"
#  define COMPILE_TYPE "normal"

#endif


#if defined(HAVE_DYNAMIC_CRYPTO_LIB)
#  define LINK_TYPE "dynamic"
#else
#  define LINK_TYPE "static"
#endif


#if OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION_PLAIN(1,1,0)
#define OPENSSL_VERSION	SSLEAY_VERSION
#define OpenSSL_version	SSLeay_version
#endif

#ifdef HAVE_DYNAMIC_CRYPTO_LIB

char *crypto_callback_name = CB_NAME;

int change_basename(ErlNifBinary* bin, char* buf, size_t bufsz, const char* newfile)
{
    size_t i;
    size_t newlen;

    for (i = bin->size; i > 0; i--) {
	if (bin->data[i-1] == '/')
	    break;
    }

    newlen = strlen(newfile);
    if (i > SIZE_MAX - newlen)
        goto err;

    if (i + newlen >= bufsz)
        goto err;

    memcpy(buf, bin->data, i);
    strcpy(buf+i, newfile);

    return 1;

 err:
    return 0;
}

void error_handler(void* null, const char* errstr)
{
    PRINTF_ERR1("CRYPTO LOADING ERROR: '%s'", errstr);
}
#endif /* HAVE_DYNAMIC_CRYPTO_LIB */


ERL_NIF_TERM info_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* () */
    ERL_NIF_TERM  ret;

    ret = enif_make_new_map(env);
    
    enif_make_map_put(env, ret,
                      enif_make_atom(env,"compile_type"),
                      enif_make_atom(env, COMPILE_TYPE),
                      &ret);

    enif_make_map_put(env, ret,
                      enif_make_atom(env, "link_type"),
                      enif_make_atom(env, LINK_TYPE),
                      &ret);

    enif_make_map_put(env, ret,
                      enif_make_atom(env, "cryptolib_version_compiled"),
#ifdef OPENSSL_VERSION_TEXT
                      enif_make_string(env, OPENSSL_VERSION_TEXT, ERL_NIF_LATIN1),
#else
                      /* Just to be really safe for versions/clones unknown to me lacking this macro */
                      atom_undefined,
#endif
                      &ret);

    enif_make_map_put(env, ret,
                      enif_make_atom(env, "cryptolib_version_linked"),
                      enif_make_string(env, OpenSSL_version(OPENSSL_VERSION), ERL_NIF_LATIN1),
                      &ret);

#ifdef HAS_3_0_API
    enif_make_map_put(env, ret,
                      enif_make_atom(env, "fips_provider_available"),
                      OSSL_PROVIDER_available(NULL, "fips") ? atom_true : atom_false,
                      &ret);
#endif

    return ret;
}


ERL_NIF_TERM info_lib(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{/* () */
    /* [{<<"OpenSSL">>,9470143,<<"OpenSSL 0.9.8k 25 Mar 2009">>}] */

    ERL_NIF_TERM name_term, ver_term;
    static const char libname[] = "OpenSSL";
    size_t name_sz;
    const char* ver;
    size_t ver_sz;
    int ver_num;
    unsigned char *out_name, *out_ver;

    ASSERT(argc == 0);

    name_sz = strlen(libname);
    ver = OpenSSL_version(OPENSSL_VERSION);
    ver_sz = strlen(ver);
    ver_num = OPENSSL_VERSION_NUMBER;

    /* R16:
     * Ignore library version number from SSLeay() and instead show header
     * version. Otherwise user might try to call a function that is implemented
     * by a newer library but not supported by the headers used at compile time.
     * Example: DES_ede3_cfb_encrypt in 0.9.7i but not in 0.9.7d.
     *
     * Version string is still from library though.
     */

    if ((out_name = enif_make_new_binary(env, name_sz, &name_term)) == NULL)
        goto err;
    if ((out_ver = enif_make_new_binary(env, ver_sz, &ver_term)) == NULL)
        goto err;

    memcpy(out_name, libname, name_sz);
    memcpy(out_ver, ver, ver_sz);

    return enif_make_list1(env, enif_make_tuple3(env, name_term,
						 enif_make_int(env, ver_num),
						 ver_term));

 err:
    return enif_make_badarg(env);
}
