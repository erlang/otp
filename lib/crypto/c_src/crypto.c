/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

#ifdef __WIN32__
    #include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <erl_nif.h>

#define OPENSSL_THREAD_DEFINES
#include <openssl/opensslconf.h>

#include <openssl/crypto.h>
#ifndef OPENSSL_NO_DES
#include <openssl/des.h>
#endif /* #ifndef OPENSSL_NO_DES */
/* #include <openssl/idea.h> This is not supported on the openssl OTP requires */
#include <openssl/dsa.h>
#include <openssl/rsa.h>
#include <openssl/aes.h>
#include <openssl/md5.h>
#include <openssl/md4.h>
#include <openssl/sha.h>
#include <openssl/ripemd.h>
#include <openssl/bn.h>
#include <openssl/objects.h>
#ifndef OPENSSL_NO_RC4
    #include <openssl/rc4.h>
#endif /* OPENSSL_NO_RC4 */
#ifndef OPENSSL_NO_RC2
    #include <openssl/rc2.h>
#endif
#include <openssl/blowfish.h>
#include <openssl/rand.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/engine.h>
#include <openssl/err.h>

/* Helper macro to construct a OPENSSL_VERSION_NUMBER.
 * See openssl/opensslv.h
 */
#define PACKED_OPENSSL_VERSION(MAJ, MIN, FIX, P)	\
    ((((((((MAJ << 8) | MIN) << 8 ) | FIX) << 8) | (P-'a'+1)) << 4) | 0xf)

#define PACKED_OPENSSL_VERSION_PLAIN(MAJ, MIN, FIX) \
    PACKED_OPENSSL_VERSION(MAJ,MIN,FIX,('a'-1))


/* LibreSSL was cloned from OpenSSL 1.0.1g and claims to be API and BPI compatible
 * with 1.0.1.
 *
 * LibreSSL has the same names on include files and symbols as OpenSSL, but defines
 * the OPENSSL_VERSION_NUMBER to be >= 2.0.0
 *
 * Therefor works tests like this as intendend:
 *     OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
 * (The test is for example "2.4.2" >= "1.0.0" although the test
 *  with the cloned OpenSSL test would be "1.0.1" >= "1.0.0")
 *
 * But tests like this gives wrong result:
 *     OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION_PLAIN(1,1,0)
 * (The test is false since "2.4.2" < "1.1.0".  It should have been
 *  true because the LibreSSL API version is "1.0.1")
 *
 */

#ifdef LIBRESSL_VERSION_NUMBER
/* A macro to test on in this file */
#define HAS_LIBRESSL
#endif

#ifdef HAS_LIBRESSL
/* LibreSSL dislikes FIPS */
# ifdef FIPS_SUPPORT
#  undef FIPS_SUPPORT
# endif

/* LibreSSL wants the 1.0.1 API */
# define NEED_EVP_COMPATIBILITY_FUNCTIONS
#endif


#if OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION_PLAIN(1,1,0)
# define NEED_EVP_COMPATIBILITY_FUNCTIONS
#endif


#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
# define HAS_EVP_PKEY_CTX
#endif


#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
#include <openssl/modes.h>
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(0,9,8,'h')
#define HAS_ENGINE_SUPPORT
#endif

#include "crypto_callback.h"

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(0,9,8)	\
    && !defined(OPENSSL_NO_SHA224) && defined(NID_sha224) \
    && !defined(OPENSSL_NO_SHA256) /* disabled like this in my sha.h (?) */
# define HAVE_SHA224
#endif
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(0,9,8)	\
    && !defined(OPENSSL_NO_SHA256) && defined(NID_sha256)
# define HAVE_SHA256
#endif
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(0,9,8)	\
    && !defined(OPENSSL_NO_SHA384) && defined(NID_sha384)\
    && !defined(OPENSSL_NO_SHA512) /* disabled like this in my sha.h (?) */
# define HAVE_SHA384
#endif
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(0,9,8)	\
    && !defined(OPENSSL_NO_SHA512) && defined(NID_sha512)
# define HAVE_SHA512
#endif
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(0,9,7,'e')
# define HAVE_DES_ede3_cfb_encrypt
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(0,9,8,'o') \
	&& !defined(OPENSSL_NO_EC) \
	&& !defined(OPENSSL_NO_ECDH) \
	&& !defined(OPENSSL_NO_ECDSA)
# define HAVE_EC
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(0,9,8,'c')
# define HAVE_AES_IGE
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,1)
# define HAVE_EVP_AES_CTR
# define HAVE_GCM
# define HAVE_CMAC
# if OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION(1,0,1,'d')
#  define HAVE_GCM_EVP_DECRYPT_BUG
# endif
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,1,0)
# ifndef HAS_LIBRESSL
#  define HAVE_CHACHA20_POLY1305
# endif
#endif

#if OPENSSL_VERSION_NUMBER <= PACKED_OPENSSL_VERSION(0,9,8,'l')
# define HAVE_ECB_IVEC_BUG
#endif

#define HAVE_RSA_SSLV23_PADDING
#if defined(HAS_LIBRESSL)                                             \
    && LIBRESSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(2,6,1)
# undef HAVE_RSA_SSLV23_PADDING
#endif

#if defined(HAVE_CMAC)
#include <openssl/cmac.h>
#endif

#if defined(HAVE_EC)
#include <openssl/ec.h>
#include <openssl/ecdh.h>
#include <openssl/ecdsa.h>
#endif

#ifdef VALGRIND
    #  include <valgrind/memcheck.h>

/* libcrypto mixes supplied buffer contents into its entropy pool,
   which makes valgrind complain about the use of uninitialized data.
   We use this valgrind "request" to make sure that no such seemingly
   undefined data is returned.
*/
    #  define ERL_VALGRIND_MAKE_MEM_DEFINED(ptr,size) \
    VALGRIND_MAKE_MEM_DEFINED(ptr,size)

    #   define ERL_VALGRIND_ASSERT_MEM_DEFINED(Ptr,Size)			\
          do {								\
              int __erl_valgrind_mem_defined = VALGRIND_CHECK_MEM_IS_DEFINED((Ptr),(Size));	\
              if (__erl_valgrind_mem_defined != 0) {			\
	          fprintf(stderr,"\r\n####### VALGRIND_ASSSERT(%p,%ld) failed at %s:%d\r\n", \
		          (Ptr),(long)(Size), __FILE__, __LINE__);	\
	          abort();						\
              }								\
          } while (0)

#else
    #  define ERL_VALGRIND_MAKE_MEM_DEFINED(ptr,size)
    #  define ERL_VALGRIND_ASSERT_MEM_DEFINED(ptr,size)
#endif

#ifdef DEBUG
    #  define ASSERT(e) \
    ((void) ((e) ? 1 : (fprintf(stderr,"Assert '%s' failed at %s:%d\n",\
				#e, __FILE__, __LINE__), abort(), 0)))
#else
    #  define ASSERT(e) ((void) 1)
#endif

#ifdef __GNUC__
    #  define INLINE __inline__
#elif defined(__WIN32__)
    #  define INLINE __forceinline
#else
    #  define INLINE
#endif


#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(s,i) \
{ (s)[0] = (char)(((i) >> 24) & 0xff);\
  (s)[1] = (char)(((i) >> 16) & 0xff);\
  (s)[2] = (char)(((i) >> 8) & 0xff);\
  (s)[3] = (char)((i) & 0xff);\
}

/* This shall correspond to the similar macro in crypto.erl */
/* Current value is: erlang:system_info(context_reductions) * 10 */
#define MAX_BYTES_TO_NIF 20000

#define CONSUME_REDS(NifEnv, Ibin)			\
do {							\
    int _cost = ((Ibin).size  * 100) / MAX_BYTES_TO_NIF;\
    if (_cost) {                                        \
        (void) enif_consume_timeslice((NifEnv),		\
	          (_cost > 100) ? 100 : _cost);		\
    }                                                   \
 } while (0)


#ifdef NEED_EVP_COMPATIBILITY_FUNCTIONS
/*
 * In OpenSSL 1.1.0, most structs are opaque. That means that
 * the structs cannot be allocated as automatic variables on the
 * C stack (because the size is unknown) and that it is necessary
 * to use access functions.
 *
 * For backward compatibility to previous versions of OpenSSL, define
 * on our versions of the new functions defined in 1.1.0 here, so that
 * we don't have to sprinkle ifdefs throughout the code.
 */

static HMAC_CTX *HMAC_CTX_new(void);
static void HMAC_CTX_free(HMAC_CTX *ctx);

static HMAC_CTX *HMAC_CTX_new()
{
    HMAC_CTX *ctx = CRYPTO_malloc(sizeof(HMAC_CTX), __FILE__, __LINE__);
    HMAC_CTX_init(ctx);
    return ctx;
}

static void HMAC_CTX_free(HMAC_CTX *ctx)
{
    HMAC_CTX_cleanup(ctx);
    CRYPTO_free(ctx);
}

#define EVP_MD_CTX_new() EVP_MD_CTX_create()
#define EVP_MD_CTX_free(ctx) EVP_MD_CTX_destroy(ctx)

static INLINE void *BN_GENCB_get_arg(BN_GENCB *cb);

static INLINE void *BN_GENCB_get_arg(BN_GENCB *cb)
{
    return cb->arg;
}

static INLINE int RSA_set0_key(RSA *r, BIGNUM *n, BIGNUM *e, BIGNUM *d);
static INLINE void RSA_get0_key(const RSA *r, const BIGNUM **n, const BIGNUM **e, const BIGNUM **d);
static INLINE int RSA_set0_factors(RSA *r, BIGNUM *p, BIGNUM *q);
static INLINE void RSA_get0_factors(const RSA *r, const BIGNUM **p, const BIGNUM **q);
static INLINE int RSA_set0_crt_params(RSA *r, BIGNUM *dmp1, BIGNUM *dmq1, BIGNUM *iqmp);
static INLINE void RSA_get0_crt_params(const RSA *r, const BIGNUM **dmp1, const BIGNUM **dmq1, const BIGNUM **iqmp);

static INLINE int RSA_set0_key(RSA *r, BIGNUM *n, BIGNUM *e, BIGNUM *d)
{
    r->n = n;
    r->e = e;
    r->d = d;
    return 1;
}

static INLINE void RSA_get0_key(const RSA *r, const BIGNUM **n, const BIGNUM **e, const BIGNUM **d)
{
    *n = r->n;
    *e = r->e;
    *d = r->d;
}

static INLINE int RSA_set0_factors(RSA *r, BIGNUM *p, BIGNUM *q)
{
    r->p = p;
    r->q = q;
    return 1;
}

static INLINE void RSA_get0_factors(const RSA *r, const BIGNUM **p, const BIGNUM **q)
{
    *p = r->p;
    *q = r->q;
}

static INLINE int RSA_set0_crt_params(RSA *r, BIGNUM *dmp1, BIGNUM *dmq1, BIGNUM *iqmp)
{
    r->dmp1 = dmp1;
    r->dmq1 = dmq1;
    r->iqmp = iqmp;
    return 1;
}

static INLINE void RSA_get0_crt_params(const RSA *r, const BIGNUM **dmp1, const BIGNUM **dmq1, const BIGNUM **iqmp)
{
    *dmp1 = r->dmp1;
    *dmq1 = r->dmq1;
    *iqmp = r->iqmp;
}

static INLINE int DSA_set0_key(DSA *d, BIGNUM *pub_key, BIGNUM *priv_key);
static INLINE int DSA_set0_pqg(DSA *d, BIGNUM *p, BIGNUM *q, BIGNUM *g);
static INLINE void DSA_get0_pqg(const DSA *dsa,
			       const BIGNUM **p, const BIGNUM **q, const BIGNUM **g);
static INLINE void DSA_get0_key(const DSA *dsa,
			       const BIGNUM **pub_key, const BIGNUM **priv_key);

static INLINE int DSA_set0_key(DSA *d, BIGNUM *pub_key, BIGNUM *priv_key)
{
    d->pub_key = pub_key;
    d->priv_key = priv_key;
    return 1;
}

static INLINE int DSA_set0_pqg(DSA *d, BIGNUM *p, BIGNUM *q, BIGNUM *g)
{
    d->p = p;
    d->q = q;
    d->g = g;
    return 1;
}

static INLINE void
DSA_get0_pqg(const DSA *dsa, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g)
{
    *p = dsa->p;
    *q = dsa->q;
    *g = dsa->g;
}

static INLINE void
DSA_get0_key(const DSA *dsa, const BIGNUM **pub_key, const BIGNUM **priv_key)
{
    if (pub_key) *pub_key = dsa->pub_key;
    if (priv_key) *priv_key = dsa->priv_key;
}



static INLINE int DH_set0_key(DH *dh, BIGNUM *pub_key, BIGNUM *priv_key);
static INLINE int DH_set0_pqg(DH *dh, BIGNUM *p, BIGNUM *q, BIGNUM *g);
static INLINE int DH_set_length(DH *dh, long length);
static INLINE void DH_get0_pqg(const DH *dh,
			       const BIGNUM **p, const BIGNUM **q, const BIGNUM **g);
static INLINE void DH_get0_key(const DH *dh,
			       const BIGNUM **pub_key, const BIGNUM **priv_key);

static INLINE int DH_set0_key(DH *dh, BIGNUM *pub_key, BIGNUM *priv_key)
{
    dh->pub_key = pub_key;
    dh->priv_key = priv_key;
    return 1;
}

static INLINE int DH_set0_pqg(DH *dh, BIGNUM *p, BIGNUM *q, BIGNUM *g)
{
    dh->p = p;
    dh->q = q;
    dh->g = g;
    return 1;
}

static INLINE int DH_set_length(DH *dh, long length)
{
    dh->length = length;
    return 1;
}



static INLINE void
DH_get0_pqg(const DH *dh, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g)
{
    *p = dh->p;
    *q = dh->q;
    *g = dh->g;
}

static INLINE void
DH_get0_key(const DH *dh, const BIGNUM **pub_key, const BIGNUM **priv_key)
{
    if (pub_key) *pub_key = dh->pub_key;
    if (priv_key) *priv_key = dh->priv_key;
}

#else /* End of compatibility definitions. */

#define HAVE_OPAQUE_BN_GENCB

#endif

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM info_lib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM info_fips(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enable_fips_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hash_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hash_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hash_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hash_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM cmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM block_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_cfb_8_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_cfb_128_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ige_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ctr_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM strong_rand_bytes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM strong_rand_range_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rand_uniform_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mod_exp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM do_exor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc4_set_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc4_encrypt_with_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pkey_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pkey_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM pkey_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM privkey_to_pubkey_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM srp_value_B_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM srp_user_secret_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM srp_host_secret_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM ec_key_generate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ecdh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM rand_seed_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM aes_gcm_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_gcm_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#ifdef HAVE_GCM_EVP_DECRYPT_BUG
static ERL_NIF_TERM aes_gcm_decrypt_NO_EVP(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#endif

static ERL_NIF_TERM chacha20_poly1305_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM chacha20_poly1305_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static int get_engine_load_cmd_list(ErlNifEnv* env, const ERL_NIF_TERM term, char **cmds, int i);
static ERL_NIF_TERM engine_by_id_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_finish_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_free_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_load_dynamic_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_ctrl_cmd_strings_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_register_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_unregister_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_add_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_remove_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_get_first_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_get_next_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_get_id_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM engine_get_all_methods_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* helpers */
static void init_algorithms_types(ErlNifEnv*);
static void init_digest_types(ErlNifEnv* env);
static void init_cipher_types(ErlNifEnv* env);
#ifdef HAVE_EC
static EC_KEY* ec_key_new(ErlNifEnv* env, ERL_NIF_TERM curve_arg);
static int term2point(ErlNifEnv* env, ERL_NIF_TERM term,
		      EC_GROUP *group, EC_POINT **pptr);
#endif
static ERL_NIF_TERM bin_from_bn(ErlNifEnv* env, const BIGNUM *bn);

#ifdef HAS_ENGINE_SUPPORT
static int zero_terminate(ErlNifBinary bin, char **buf);
#endif

static int library_refc = 0; /* number of users of this dynamic library */

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
    {"privkey_to_pubkey_nif", 2, privkey_to_pubkey_nif},
    {"srp_value_B_nif", 5, srp_value_B_nif},
    {"srp_user_secret_nif", 7, srp_user_secret_nif},
    {"srp_host_secret_nif", 5, srp_host_secret_nif},

    {"ec_key_generate", 2, ec_key_generate},
    {"ecdh_compute_key_nif", 3, ecdh_compute_key_nif},

    {"rand_seed_nif", 1, rand_seed_nif},

    {"aes_gcm_encrypt", 5, aes_gcm_encrypt},
    {"aes_gcm_decrypt", 5, aes_gcm_decrypt},

    {"chacha20_poly1305_encrypt", 4, chacha20_poly1305_encrypt},
    {"chacha20_poly1305_decrypt", 5, chacha20_poly1305_decrypt},

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
    {"engine_get_all_methods_nif", 0, engine_get_all_methods_nif}

};

ERL_NIF_INIT(crypto,nif_funcs,load,NULL,upgrade,unload)

#define MD5_CTX_LEN       (sizeof(MD5_CTX))
#define MD4_CTX_LEN       (sizeof(MD4_CTX))
#define RIPEMD160_CTX_LEN (sizeof(RIPEMD160_CTX))


static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_sha;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_rsa_pkcs1_padding;
static ERL_NIF_TERM atom_rsa_pkcs1_oaep_padding;
static ERL_NIF_TERM atom_rsa_no_padding;
static ERL_NIF_TERM atom_signature_md;
static ERL_NIF_TERM atom_undefined;

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_not_prime;
static ERL_NIF_TERM atom_not_strong_prime;
static ERL_NIF_TERM atom_unable_to_check_generator;
static ERL_NIF_TERM atom_not_suitable_generator;
static ERL_NIF_TERM atom_check_failed;
static ERL_NIF_TERM atom_unknown;
static ERL_NIF_TERM atom_none;
static ERL_NIF_TERM atom_notsup;
static ERL_NIF_TERM atom_digest;
#ifdef FIPS_SUPPORT
static ERL_NIF_TERM atom_enabled;
static ERL_NIF_TERM atom_not_enabled;
#else
static ERL_NIF_TERM atom_not_supported;
#endif

#if defined(HAVE_EC)
static ERL_NIF_TERM atom_ec;
static ERL_NIF_TERM atom_prime_field;
static ERL_NIF_TERM atom_characteristic_two_field;
static ERL_NIF_TERM atom_tpbasis;
static ERL_NIF_TERM atom_ppbasis;
static ERL_NIF_TERM atom_onbasis;
#endif

static ERL_NIF_TERM atom_aes_cfb8;
static ERL_NIF_TERM atom_aes_cfb128;
#ifdef HAVE_ECB_IVEC_BUG
static ERL_NIF_TERM atom_aes_ecb;
static ERL_NIF_TERM atom_des_ecb;
static ERL_NIF_TERM atom_blowfish_ecb;
#endif

static ERL_NIF_TERM atom_rsa;
static ERL_NIF_TERM atom_dss;
static ERL_NIF_TERM atom_ecdsa;
static ERL_NIF_TERM atom_rsa_mgf1_md;
static ERL_NIF_TERM atom_rsa_oaep_label;
static ERL_NIF_TERM atom_rsa_oaep_md;
static ERL_NIF_TERM atom_rsa_pad; /* backwards compatibility */
static ERL_NIF_TERM atom_rsa_padding;
static ERL_NIF_TERM atom_rsa_pkcs1_pss_padding;
#ifdef HAVE_RSA_SSLV23_PADDING
static ERL_NIF_TERM atom_rsa_sslv23_padding;
#endif
static ERL_NIF_TERM atom_rsa_x931_padding;
static ERL_NIF_TERM atom_rsa_pss_saltlen;
static ERL_NIF_TERM atom_sha224;
static ERL_NIF_TERM atom_sha256;
static ERL_NIF_TERM atom_sha384;
static ERL_NIF_TERM atom_sha512;
static ERL_NIF_TERM atom_md5;
static ERL_NIF_TERM atom_ripemd160;

#ifdef HAS_ENGINE_SUPPORT
static ERL_NIF_TERM atom_bad_engine_method;
static ERL_NIF_TERM atom_bad_engine_id;
static ERL_NIF_TERM atom_ctrl_cmd_failed;
static ERL_NIF_TERM atom_engine_init_failed;
static ERL_NIF_TERM atom_register_engine_failed;
static ERL_NIF_TERM atom_add_engine_failed;
static ERL_NIF_TERM atom_remove_engine_failed;
static ERL_NIF_TERM atom_engine_method_not_supported;

static ERL_NIF_TERM atom_engine_method_rsa;
static ERL_NIF_TERM atom_engine_method_dsa;
static ERL_NIF_TERM atom_engine_method_dh;
static ERL_NIF_TERM atom_engine_method_rand;
static ERL_NIF_TERM atom_engine_method_ecdh;
static ERL_NIF_TERM atom_engine_method_ecdsa;
static ERL_NIF_TERM atom_engine_method_ciphers;
static ERL_NIF_TERM atom_engine_method_digests;
static ERL_NIF_TERM atom_engine_method_store;
static ERL_NIF_TERM atom_engine_method_pkey_meths;
static ERL_NIF_TERM atom_engine_method_pkey_asn1_meths;
static ERL_NIF_TERM atom_engine_method_ec;

static ERL_NIF_TERM atom_engine;
static ERL_NIF_TERM atom_key_id;
static ERL_NIF_TERM atom_password;
#endif

static ErlNifResourceType* hmac_context_rtype;
struct hmac_context
{
    ErlNifMutex* mtx;
    int alive;
    HMAC_CTX* ctx;
};
static void hmac_context_dtor(ErlNifEnv* env, struct hmac_context*);

struct digest_type_t {
    union {
	const char*  str;        /* before init, NULL for end-of-table */
	ERL_NIF_TERM atom;       /* after init, 'false' for end-of-table */
    }type;
    union {
	const EVP_MD* (*funcp)(void);  /* before init, NULL if notsup */
	const EVP_MD* p;               /* after init, NULL if notsup */
    }md;
};

static struct digest_type_t digest_types[] =
{
    {{"md4"}, {&EVP_md4}},
    {{"md5"}, {&EVP_md5}},
    {{"ripemd160"}, {&EVP_ripemd160}},
    {{"sha"}, {&EVP_sha1}},
    {{"sha224"},
#ifdef HAVE_SHA224
     {&EVP_sha224}
#else
     {NULL}
#endif
    },
    {{"sha256"},
#ifdef HAVE_SHA256
     {&EVP_sha256}
#else
     {NULL}
#endif
    },
    {{"sha384"},
#ifdef HAVE_SHA384
     {&EVP_sha384}
#else
     {NULL}
#endif
    },
    {{"sha512"},
#ifdef HAVE_SHA512
     {&EVP_sha512}
#else
     {NULL}
#endif
    },
    {{NULL}}
};

static struct digest_type_t* get_digest_type(ERL_NIF_TERM type);

struct cipher_type_t {
    union {
	const char* str;    /* before init */
	ERL_NIF_TERM atom;  /* after init */
    }type;
    union {
	const EVP_CIPHER* (*funcp)(void); /* before init, NULL if notsup */
	const EVP_CIPHER* p;              /* after init, NULL if notsup */
    }cipher;
    const size_t key_len;      /* != 0 to also match on key_len */
};

#ifdef OPENSSL_NO_DES
#define COND_NO_DES_PTR(Ptr) (NULL)
#else
#define COND_NO_DES_PTR(Ptr) (Ptr)
#endif

static struct cipher_type_t cipher_types[] =
{
    {{"rc2_cbc"},
#ifndef OPENSSL_NO_RC2
     {&EVP_rc2_cbc}
#else
     {NULL}
#endif
    },
    {{"des_cbc"}, {COND_NO_DES_PTR(&EVP_des_cbc)}},
    {{"des_cfb"}, {COND_NO_DES_PTR(&EVP_des_cfb8)}},
    {{"des_ecb"}, {COND_NO_DES_PTR(&EVP_des_ecb)}},
    {{"des_ede3_cbc"}, {COND_NO_DES_PTR(&EVP_des_ede3_cbc)}},
    {{"des_ede3_cbf"}, /* Misspelled, retained */
#ifdef HAVE_DES_ede3_cfb_encrypt
     {COND_NO_DES_PTR(&EVP_des_ede3_cfb8)}
#else
     {NULL}
#endif
    },
    {{"des_ede3_cfb"},
#ifdef HAVE_DES_ede3_cfb_encrypt
     {COND_NO_DES_PTR(&EVP_des_ede3_cfb8)}
#else
     {NULL}
#endif
    },
    {{"blowfish_cbc"}, {&EVP_bf_cbc}},
    {{"blowfish_cfb64"}, {&EVP_bf_cfb64}},
    {{"blowfish_ofb64"}, {&EVP_bf_ofb}},
    {{"blowfish_ecb"}, {&EVP_bf_ecb}},
    {{"aes_cbc"}, {&EVP_aes_128_cbc}, 16},
    {{"aes_cbc"}, {&EVP_aes_192_cbc}, 24},
    {{"aes_cbc"}, {&EVP_aes_256_cbc}, 32},
    {{"aes_cbc128"}, {&EVP_aes_128_cbc}},
    {{"aes_cbc256"}, {&EVP_aes_256_cbc}},
    {{"aes_cfb8"}, {&EVP_aes_128_cfb8}},
    {{"aes_cfb128"}, {&EVP_aes_128_cfb128}},
    {{"aes_ecb"}, {&EVP_aes_128_ecb}, 16},
    {{"aes_ecb"}, {&EVP_aes_192_ecb}, 24},
    {{"aes_ecb"}, {&EVP_aes_256_ecb}, 32},
    {{NULL}}
};

static struct cipher_type_t* get_cipher_type(ERL_NIF_TERM type, size_t key_len);


/*
#define PRINTF_ERR0(FMT) enif_fprintf(stderr, FMT "\n")
#define PRINTF_ERR1(FMT, A1) enif_fprintf(stderr, FMT "\n", A1)
#define PRINTF_ERR2(FMT, A1, A2) enif_fprintf(stderr, FMT "\n", A1, A2)
*/

#define PRINTF_ERR0(FMT)
#define PRINTF_ERR1(FMT,A1)
#define PRINTF_ERR2(FMT,A1,A2)

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
/* Define resource types for OpenSSL context structures. */
static ErlNifResourceType* evp_md_ctx_rtype;
struct evp_md_ctx {
    EVP_MD_CTX* ctx;
};
static void evp_md_ctx_dtor(ErlNifEnv* env, struct evp_md_ctx *ctx) {
    EVP_MD_CTX_free(ctx->ctx);
}
#endif

#ifdef HAVE_EVP_AES_CTR
static ErlNifResourceType* evp_cipher_ctx_rtype;
struct evp_cipher_ctx {
    EVP_CIPHER_CTX* ctx;
};
static void evp_cipher_ctx_dtor(ErlNifEnv* env, struct evp_cipher_ctx* ctx) {
    EVP_CIPHER_CTX_free(ctx->ctx);
}
#endif

// Engine
#ifdef HAS_ENGINE_SUPPORT
static ErlNifResourceType* engine_ctx_rtype;
struct engine_ctx {
    ENGINE *engine;
    char *id;
};
static void engine_ctx_dtor(ErlNifEnv* env, struct engine_ctx* ctx) {
    PRINTF_ERR0("engine_ctx_dtor");
    if(ctx->id) {
        PRINTF_ERR1("  non empty ctx->id=%s", ctx->id);
        enif_free(ctx->id);
    } else
         PRINTF_ERR0("  empty ctx->id=NULL");
}
#endif

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

#ifdef FIPS_SUPPORT
/* In FIPS mode non-FIPS algorithms are disabled and return badarg. */
#define CHECK_NO_FIPS_MODE() { if (FIPS_mode()) return atom_notsup; }
#else
#define CHECK_NO_FIPS_MODE()
#endif

#ifdef HAVE_DYNAMIC_CRYPTO_LIB

# if defined(DEBUG)
static char crypto_callback_name[] = "crypto_callback.debug";
# elif defined(VALGRIND)
static char crypto_callback_name[] = "crypto_callback.valgrind";
# else
static char crypto_callback_name[] = "crypto_callback";
# endif

static int change_basename(ErlNifBinary* bin, char* buf, int bufsz, const char* newfile)
{
    int i;

    for (i = bin->size; i > 0; i--) {
	if (bin->data[i-1] == '/')
	    break;
    }
    if (i + strlen(newfile) >= bufsz) {
	PRINTF_ERR0("CRYPTO: lib name too long");
	return 0;
    }
    memcpy(buf, bin->data, i);
    strcpy(buf+i, newfile);
    return 1;
}

static void error_handler(void* null, const char* errstr)
{
    PRINTF_ERR1("CRYPTO LOADING ERROR: '%s'", errstr);
}
#endif /* HAVE_DYNAMIC_CRYPTO_LIB */

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

    if (library_refc > 0) {
	/* Repeated loading of this library (module upgrade).
	 * Atoms and callbacks are already set, we are done.
	 */
	return 0;
    }
#endif 

    atom_true  = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    /* Enter FIPS mode */
    if (tpl_array[2] == atom_true) {
#ifdef FIPS_SUPPORT
        if (!FIPS_mode_set(1)) {
#else
        {
#endif
            PRINTF_ERR0("CRYPTO: Could not setup FIPS mode");
            return 0;
        }
    } else if (tpl_array[2] != atom_false) {
        PRINTF_ERR1("CRYPTO: Invalid load_info '%T'", load_info);
        return 0;
    }

    atom_sha = enif_make_atom(env,"sha");
    atom_error = enif_make_atom(env,"error");
    atom_rsa_pkcs1_padding = enif_make_atom(env,"rsa_pkcs1_padding");
    atom_rsa_pkcs1_oaep_padding = enif_make_atom(env,"rsa_pkcs1_oaep_padding");
    atom_rsa_no_padding = enif_make_atom(env,"rsa_no_padding");
    atom_signature_md = enif_make_atom(env,"signature_md");
    atom_undefined = enif_make_atom(env,"undefined");
    atom_ok = enif_make_atom(env,"ok");
    atom_not_prime = enif_make_atom(env,"not_prime");
    atom_not_strong_prime = enif_make_atom(env,"not_strong_prime");
    atom_unable_to_check_generator = enif_make_atom(env,"unable_to_check_generator");
    atom_not_suitable_generator = enif_make_atom(env,"not_suitable_generator");
    atom_check_failed = enif_make_atom(env,"check_failed");
    atom_unknown = enif_make_atom(env,"unknown");
    atom_none = enif_make_atom(env,"none");
    atom_notsup = enif_make_atom(env,"notsup");
    atom_digest = enif_make_atom(env,"digest");

#if defined(HAVE_EC)
    atom_ec = enif_make_atom(env,"ec");
    atom_prime_field = enif_make_atom(env,"prime_field");
    atom_characteristic_two_field = enif_make_atom(env,"characteristic_two_field");
    atom_tpbasis = enif_make_atom(env,"tpbasis");
    atom_ppbasis = enif_make_atom(env,"ppbasis");
    atom_onbasis = enif_make_atom(env,"onbasis");
#endif
    atom_aes_cfb8 = enif_make_atom(env, "aes_cfb8");
    atom_aes_cfb128 = enif_make_atom(env, "aes_cfb128");
#ifdef HAVE_ECB_IVEC_BUG
    atom_aes_ecb = enif_make_atom(env, "aes_ecb");
    atom_des_ecb = enif_make_atom(env, "des_ecb");
    atom_blowfish_ecb = enif_make_atom(env, "blowfish_ecb");
#endif

#ifdef FIPS_SUPPORT
    atom_enabled = enif_make_atom(env,"enabled");
    atom_not_enabled = enif_make_atom(env,"not_enabled");
#else
    atom_not_supported = enif_make_atom(env,"not_supported");
#endif
    atom_rsa = enif_make_atom(env,"rsa");
    atom_dss = enif_make_atom(env,"dss");
    atom_ecdsa = enif_make_atom(env,"ecdsa");
    atom_rsa_mgf1_md = enif_make_atom(env,"rsa_mgf1_md");
    atom_rsa_oaep_label = enif_make_atom(env,"rsa_oaep_label");
    atom_rsa_oaep_md = enif_make_atom(env,"rsa_oaep_md");
    atom_rsa_pad = enif_make_atom(env,"rsa_pad"); /* backwards compatibility */
    atom_rsa_padding = enif_make_atom(env,"rsa_padding");
    atom_rsa_pkcs1_pss_padding = enif_make_atom(env,"rsa_pkcs1_pss_padding");
#ifdef HAVE_RSA_SSLV23_PADDING
    atom_rsa_sslv23_padding = enif_make_atom(env,"rsa_sslv23_padding");
#endif
    atom_rsa_x931_padding = enif_make_atom(env,"rsa_x931_padding");
    atom_rsa_pss_saltlen = enif_make_atom(env,"rsa_pss_saltlen");
    atom_sha224 = enif_make_atom(env,"sha224");
    atom_sha256 = enif_make_atom(env,"sha256");
    atom_sha384 = enif_make_atom(env,"sha384");
    atom_sha512 = enif_make_atom(env,"sha512");
    atom_md5 = enif_make_atom(env,"md5");
    atom_ripemd160 = enif_make_atom(env,"ripemd160");

#ifdef HAS_ENGINE_SUPPORT
    atom_bad_engine_method = enif_make_atom(env,"bad_engine_method");
    atom_bad_engine_id = enif_make_atom(env,"bad_engine_id");
    atom_ctrl_cmd_failed = enif_make_atom(env,"ctrl_cmd_failed");
    atom_engine_init_failed = enif_make_atom(env,"engine_init_failed");
    atom_engine_method_not_supported = enif_make_atom(env,"engine_method_not_supported");
    atom_add_engine_failed = enif_make_atom(env,"add_engine_failed");
    atom_remove_engine_failed = enif_make_atom(env,"remove_engine_failed");

    atom_engine_method_rsa = enif_make_atom(env,"engine_method_rsa");
    atom_engine_method_dsa = enif_make_atom(env,"engine_method_dsa");
    atom_engine_method_dh = enif_make_atom(env,"engine_method_dh");
    atom_engine_method_rand = enif_make_atom(env,"engine_method_rand");
    atom_engine_method_ecdh = enif_make_atom(env,"engine_method_ecdh");
    atom_engine_method_ecdsa = enif_make_atom(env,"engine_method_ecdsa");
    atom_engine_method_store = enif_make_atom(env,"engine_method_store");
    atom_engine_method_ciphers = enif_make_atom(env,"engine_method_ciphers");
    atom_engine_method_digests = enif_make_atom(env,"engine_method_digests");
    atom_engine_method_pkey_meths = enif_make_atom(env,"engine_method_pkey_meths");
    atom_engine_method_pkey_asn1_meths = enif_make_atom(env,"engine_method_pkey_asn1_meths");
    atom_engine_method_ec = enif_make_atom(env,"engine_method_ec");

    atom_engine = enif_make_atom(env,"engine");
    atom_key_id = enif_make_atom(env,"key_id");
    atom_password = enif_make_atom(env,"password");
#endif

    init_digest_types(env);
    init_cipher_types(env);
    init_algorithms_types(env);

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
static ERL_NIF_TERM algo_hash[8];   /* increase when extending the list */
static int algo_pubkey_cnt, algo_pubkey_fips_cnt;
static ERL_NIF_TERM algo_pubkey[7]; /* increase when extending the list */
static int algo_cipher_cnt, algo_cipher_fips_cnt;
static ERL_NIF_TERM algo_cipher[24]; /* increase when extending the list */
static int algo_mac_cnt, algo_mac_fips_cnt;
static ERL_NIF_TERM algo_mac[2]; /* increase when extending the list */

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

    // Validated algorithms first
    algo_mac_cnt = 0;
    algo_mac[algo_mac_cnt++] = enif_make_atom(env,"hmac");
#ifdef HAVE_CMAC
    algo_mac[algo_mac_cnt++] = enif_make_atom(env,"cmac");
#endif
    // Non-validated algorithms follow
    algo_mac_fips_cnt = algo_mac_cnt;

    ASSERT(algo_hash_cnt <= sizeof(algo_hash)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_pubkey_cnt <= sizeof(algo_pubkey)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_cipher_cnt <= sizeof(algo_cipher)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_mac_cnt <= sizeof(algo_mac)/sizeof(ERL_NIF_TERM));
}

static ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef FIPS_SUPPORT
    int fips_mode  = FIPS_mode();
    int hash_cnt   = fips_mode ? algo_hash_fips_cnt   : algo_hash_cnt;
    int pubkey_cnt = fips_mode ? algo_pubkey_fips_cnt : algo_pubkey_cnt;
    int cipher_cnt = fips_mode ? algo_cipher_fips_cnt : algo_cipher_cnt;
    int mac_cnt    = fips_mode ? algo_mac_fips_cnt    : algo_mac_cnt;
#else
    int hash_cnt   = algo_hash_cnt;
    int pubkey_cnt = algo_pubkey_cnt;
    int cipher_cnt = algo_cipher_cnt;
    int mac_cnt    = algo_mac_cnt;
#endif
    return enif_make_tuple4(env,
			    enif_make_list_from_array(env, algo_hash,   hash_cnt),
			    enif_make_list_from_array(env, algo_pubkey, pubkey_cnt),
			    enif_make_list_from_array(env, algo_cipher, cipher_cnt),
			    enif_make_list_from_array(env, algo_mac,    mac_cnt)
                            );
}

static ERL_NIF_TERM info_lib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* [{<<"OpenSSL">>,9470143,<<"OpenSSL 0.9.8k 25 Mar 2009">>}] */

    static const char libname[] = "OpenSSL";
    unsigned name_sz = strlen(libname);
    const char* ver = SSLeay_version(SSLEAY_VERSION);
    unsigned ver_sz = strlen(ver);
    ERL_NIF_TERM name_term, ver_term;
    int ver_num = OPENSSL_VERSION_NUMBER;
    /* R16:
     * Ignore library version number from SSLeay() and instead show header
     * version. Otherwise user might try to call a function that is implemented
     * by a newer library but not supported by the headers used at compile time.
     * Example: DES_ede3_cfb_encrypt in 0.9.7i but not in 0.9.7d.
     *
     * Version string is still from library though.
     */

    memcpy(enif_make_new_binary(env, name_sz, &name_term), libname, name_sz);
    memcpy(enif_make_new_binary(env, ver_sz, &ver_term), ver, ver_sz);

    return enif_make_list1(env, enif_make_tuple3(env, name_term,
						 enif_make_int(env, ver_num),
						 ver_term));
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


#if defined(HAVE_EC)
static ERL_NIF_TERM make_badarg_maybe(ErlNifEnv* env)
{
    ERL_NIF_TERM reason;
    if (enif_has_pending_exception(env, &reason))
	return reason; /* dummy return value ignored */
    else
	return enif_make_badarg(env);
}
#endif

static ERL_NIF_TERM hash_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Data) */
    struct digest_type_t *digp = NULL;
    const EVP_MD         *md;
    ErlNifBinary         data;
    ERL_NIF_TERM         ret;
    unsigned             ret_size;

    digp = get_digest_type(argv[0]);
    if (!digp ||
        !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
	return enif_make_badarg(env);
    }
    md = digp->md.p;
    if (!md) {
	return atom_notsup;
    }

    ret_size = (unsigned)EVP_MD_size(md);
    ASSERT(0 < ret_size && ret_size <= EVP_MAX_MD_SIZE);
    if (!EVP_Digest(data.data, data.size,
                    enif_make_new_binary(env, ret_size, &ret), &ret_size,
                    md, NULL)) {
        return atom_notsup;
    }
    ASSERT(ret_size == (unsigned)EVP_MD_size(md));

    CONSUME_REDS(env, data);
    return ret;
}

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)

static ERL_NIF_TERM hash_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    struct digest_type_t *digp = NULL;
    struct evp_md_ctx    *ctx;
    ERL_NIF_TERM         ret;

    digp = get_digest_type(argv[0]);
    if (!digp) {
	return enif_make_badarg(env);
    }
    if (!digp->md.p) {
	return atom_notsup;
    }

    ctx = enif_alloc_resource(evp_md_ctx_rtype, sizeof(struct evp_md_ctx));
    ctx->ctx = EVP_MD_CTX_new();
    if (!EVP_DigestInit(ctx->ctx, digp->md.p)) {
        enif_release_resource(ctx);
        return atom_notsup;
    }
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}
static ERL_NIF_TERM hash_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    struct evp_md_ctx   *ctx, *new_ctx;
    ErlNifBinary data;
    ERL_NIF_TERM ret;

    if (!enif_get_resource(env, argv[0], evp_md_ctx_rtype, (void**)&ctx) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
        return enif_make_badarg(env);
    }

    new_ctx = enif_alloc_resource(evp_md_ctx_rtype, sizeof(struct evp_md_ctx));
    new_ctx->ctx = EVP_MD_CTX_new();
    if (!EVP_MD_CTX_copy(new_ctx->ctx, ctx->ctx) ||
        !EVP_DigestUpdate(new_ctx->ctx, data.data, data.size)) {
        enif_release_resource(new_ctx);
        return atom_notsup;
    }

    ret = enif_make_resource(env, new_ctx);
    enif_release_resource(new_ctx);
    CONSUME_REDS(env, data);
    return ret;
}
static ERL_NIF_TERM hash_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
    struct evp_md_ctx *ctx;
    EVP_MD_CTX        *new_ctx;
    ERL_NIF_TERM  ret;
    unsigned      ret_size;

    if (!enif_get_resource(env, argv[0], evp_md_ctx_rtype, (void**)&ctx)) {
        return enif_make_badarg(env);
    }

    ret_size = (unsigned)EVP_MD_CTX_size(ctx->ctx);
    ASSERT(0 < ret_size && ret_size <= EVP_MAX_MD_SIZE);

    new_ctx = EVP_MD_CTX_new();
    if (!EVP_MD_CTX_copy(new_ctx, ctx->ctx) ||
        !EVP_DigestFinal(new_ctx,
                         enif_make_new_binary(env, ret_size, &ret),
                         &ret_size)) {
	EVP_MD_CTX_free(new_ctx);
        return atom_notsup;
    }
    EVP_MD_CTX_free(new_ctx);
    ASSERT(ret_size == (unsigned)EVP_MD_CTX_size(ctx->ctx));

    return ret;
}

#else /* if OPENSSL_VERSION_NUMBER < 1.0 */

static ERL_NIF_TERM hash_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    typedef int (*init_fun)(unsigned char*);
    struct digest_type_t *digp = NULL;
    ERL_NIF_TERM         ctx;
    size_t               ctx_size = 0;
    init_fun             ctx_init = 0;

    digp = get_digest_type(argv[0]);
    if (!digp) {
	return enif_make_badarg(env);
    }
    if (!digp->md.p) {
	return atom_notsup;
    }

    switch (EVP_MD_type(digp->md.p))
    {
    case NID_md4:
        ctx_size = MD4_CTX_LEN;
        ctx_init = (init_fun)(&MD4_Init);
        break;
    case NID_md5:
        ctx_size = MD5_CTX_LEN;
        ctx_init = (init_fun)(&MD5_Init);
        break;
    case NID_ripemd160:
        ctx_size = RIPEMD160_CTX_LEN;
        ctx_init = (init_fun)(&RIPEMD160_Init);
        break;
    case NID_sha1:
        ctx_size = sizeof(SHA_CTX);
        ctx_init = (init_fun)(&SHA1_Init);
        break;
#ifdef HAVE_SHA224
    case NID_sha224:
        ctx_size = sizeof(SHA256_CTX);
        ctx_init = (init_fun)(&SHA224_Init);
        break;
#endif
#ifdef HAVE_SHA256
    case NID_sha256:
        ctx_size = sizeof(SHA256_CTX);
        ctx_init = (init_fun)(&SHA256_Init);
        break;
#endif
#ifdef HAVE_SHA384
    case NID_sha384:
        ctx_size = sizeof(SHA512_CTX);
        ctx_init = (init_fun)(&SHA384_Init);
        break;
#endif
#ifdef HAVE_SHA512
    case NID_sha512:
        ctx_size = sizeof(SHA512_CTX);
        ctx_init = (init_fun)(&SHA512_Init);
        break;
#endif
    default:
        return atom_notsup;
    }
    ASSERT(ctx_size);
    ASSERT(ctx_init);

    ctx_init(enif_make_new_binary(env, ctx_size, &ctx));
    return enif_make_tuple2(env, argv[0], ctx);
}
static ERL_NIF_TERM hash_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* ({Type, Context}, Data) */
    typedef int (*update_fun)(unsigned char*, const unsigned char*, size_t);
    ERL_NIF_TERM         new_ctx;
    ErlNifBinary         ctx, data;
    const ERL_NIF_TERM   *tuple;
    int                  arity;
    struct digest_type_t *digp = NULL;
    unsigned char        *ctx_buff;
    size_t               ctx_size   = 0;
    update_fun           ctx_update = 0;

    if (!enif_get_tuple(env, argv[0], &arity, &tuple) ||
        arity != 2 ||
        !(digp = get_digest_type(tuple[0])) ||
        !enif_inspect_binary(env, tuple[1], &ctx) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
        return enif_make_badarg(env);
    }
    if (!digp->md.p) {
	return atom_notsup;
    }

    switch (EVP_MD_type(digp->md.p))
    {
    case NID_md4:
        ctx_size   = MD4_CTX_LEN;
        ctx_update = (update_fun)(&MD4_Update);
        break;
    case NID_md5:
        ctx_size   = MD5_CTX_LEN;
        ctx_update = (update_fun)(&MD5_Update);
        break;
    case NID_ripemd160:
        ctx_size   = RIPEMD160_CTX_LEN;
        ctx_update = (update_fun)(&RIPEMD160_Update);
        break;
    case NID_sha1:
        ctx_size   = sizeof(SHA_CTX);
        ctx_update = (update_fun)(&SHA1_Update);
        break;
#ifdef HAVE_SHA224
    case NID_sha224:
        ctx_size   = sizeof(SHA256_CTX);
        ctx_update = (update_fun)(&SHA224_Update);
        break;
#endif
#ifdef HAVE_SHA256
    case NID_sha256:
        ctx_size   = sizeof(SHA256_CTX);
        ctx_update = (update_fun)(&SHA256_Update);
        break;
#endif
#ifdef HAVE_SHA384
    case NID_sha384:
        ctx_size   = sizeof(SHA512_CTX);
        ctx_update = (update_fun)(&SHA384_Update);
        break;
#endif
#ifdef HAVE_SHA512
    case NID_sha512:
        ctx_size   = sizeof(SHA512_CTX);
        ctx_update = (update_fun)(&SHA512_Update);
        break;
#endif
    default:
        return atom_notsup;
    }
    ASSERT(ctx_size);
    ASSERT(ctx_update);

    if (ctx.size != ctx_size) {
        return enif_make_badarg(env);
    }

    ctx_buff = enif_make_new_binary(env, ctx_size, &new_ctx);
    memcpy(ctx_buff, ctx.data, ctx_size);
    ctx_update(ctx_buff, data.data, data.size);

    CONSUME_REDS(env, data);
    return enif_make_tuple2(env, tuple[0], new_ctx);
}
static ERL_NIF_TERM hash_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* ({Type, Context}) */
    typedef int (*final_fun)(unsigned char*, void*);
    ERL_NIF_TERM         ret;
    ErlNifBinary         ctx;
    const ERL_NIF_TERM   *tuple;
    int                  arity;
    struct digest_type_t *digp = NULL;
    const EVP_MD         *md;
    void                 *new_ctx;
    size_t               ctx_size  = 0;
    final_fun            ctx_final = 0;

    if (!enif_get_tuple(env, argv[0], &arity, &tuple) ||
        arity != 2 ||
        !(digp = get_digest_type(tuple[0])) ||
        !enif_inspect_binary(env, tuple[1], &ctx)) {
        return enif_make_badarg(env);
    }
    md = digp->md.p;
    if (!md) {
	return atom_notsup;
    }


    switch (EVP_MD_type(md))
    {
    case NID_md4:
        ctx_size  = MD4_CTX_LEN;
        ctx_final = (final_fun)(&MD4_Final);
        break;
    case NID_md5:
        ctx_size  = MD5_CTX_LEN;
        ctx_final = (final_fun)(&MD5_Final);
        break;
    case NID_ripemd160:
        ctx_size  = RIPEMD160_CTX_LEN;
        ctx_final = (final_fun)(&RIPEMD160_Final);
        break;
    case NID_sha1:
        ctx_size  = sizeof(SHA_CTX);
        ctx_final = (final_fun)(&SHA1_Final);
        break;
#ifdef HAVE_SHA224
    case NID_sha224:
        ctx_size  = sizeof(SHA256_CTX);
        ctx_final = (final_fun)(&SHA224_Final);
        break;
#endif
#ifdef HAVE_SHA256
    case NID_sha256:
        ctx_size  = sizeof(SHA256_CTX);
        ctx_final = (final_fun)(&SHA256_Final);
        break;
#endif
#ifdef HAVE_SHA384
    case NID_sha384:
        ctx_size  = sizeof(SHA512_CTX);
        ctx_final = (final_fun)(&SHA384_Final);
        break;
#endif
#ifdef HAVE_SHA512
    case NID_sha512:
        ctx_size  = sizeof(SHA512_CTX);
        ctx_final = (final_fun)(&SHA512_Final);
        break;
#endif
    default:
        return atom_notsup;
    }
    ASSERT(ctx_size);
    ASSERT(ctx_final);

    if (ctx.size != ctx_size) {
        return enif_make_badarg(env);
    }

    new_ctx = enif_alloc(ctx_size);
    memcpy(new_ctx, ctx.data, ctx_size);
    ctx_final(enif_make_new_binary(env, (size_t)EVP_MD_size(md), &ret),
              new_ctx);
    enif_free(new_ctx);

    return ret;
}
#endif  /* OPENSSL_VERSION_NUMBER < 1.0 */

static ERL_NIF_TERM hmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key, Data) or (Type, Key, Data, MacSize) */
    struct digest_type_t *digp = NULL;
    ErlNifBinary         key, data;
    unsigned char        buff[EVP_MAX_MD_SIZE];
    unsigned             size = 0, req_size = 0;
    ERL_NIF_TERM         ret;

    digp = get_digest_type(argv[0]);
    if (!digp ||
        !enif_inspect_iolist_as_binary(env, argv[1], &key) ||
        !enif_inspect_iolist_as_binary(env, argv[2], &data) ||
        (argc == 4 && !enif_get_uint(env, argv[3], &req_size))) {
        return enif_make_badarg(env);
    }

    if (!digp->md.p ||
        !HMAC(digp->md.p,
              key.data, key.size,
              data.data, data.size,
              buff, &size)) {
        return atom_notsup;
    }
    ASSERT(0 < size && size <= EVP_MAX_MD_SIZE);
    CONSUME_REDS(env, data);

    if (argc == 4) {
        if (req_size <= size) {
            size = req_size;
        }
        else {
            return enif_make_badarg(env);
        }
    }
    memcpy(enif_make_new_binary(env, size, &ret), buff, size);
    return ret;
}

static void hmac_context_dtor(ErlNifEnv* env, struct hmac_context *obj)
{
    if (obj->alive) {
	HMAC_CTX_free(obj->ctx);
	obj->alive = 0;
    }
    enif_mutex_destroy(obj->mtx);
}

static ERL_NIF_TERM hmac_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key) */
    struct digest_type_t *digp = NULL;
    ErlNifBinary         key;
    ERL_NIF_TERM         ret;
    struct hmac_context  *obj;

    digp = get_digest_type(argv[0]);
    if (!digp ||
        !enif_inspect_iolist_as_binary(env, argv[1], &key)) {
        return enif_make_badarg(env);
    }
    if (!digp->md.p) {
        return atom_notsup;
    }

    obj = enif_alloc_resource(hmac_context_rtype, sizeof(struct hmac_context));
    obj->mtx = enif_mutex_create("crypto.hmac");
    obj->alive = 1;
    obj->ctx = HMAC_CTX_new();
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
    // Check the return value of HMAC_Init: it may fail in FIPS mode
    // for disabled algorithms
    if (!HMAC_Init_ex(obj->ctx, key.data, key.size, digp->md.p, NULL)) {
        enif_release_resource(obj);
        return atom_notsup;
    }
#else
    HMAC_Init_ex(obj->ctx, key.data, key.size, digp->md.p, NULL);
#endif

    ret = enif_make_resource(env, obj);
    enif_release_resource(obj);
    return ret;
}

static ERL_NIF_TERM hmac_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    ErlNifBinary data;
    struct hmac_context* obj;

    if (!enif_get_resource(env, argv[0], hmac_context_rtype, (void**)&obj)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
	return enif_make_badarg(env);
    }
    enif_mutex_lock(obj->mtx);
    if (!obj->alive) {
	enif_mutex_unlock(obj->mtx);
	return enif_make_badarg(env);
    }
    HMAC_Update(obj->ctx, data.data, data.size);
    enif_mutex_unlock(obj->mtx);

    CONSUME_REDS(env,data);
    return argv[0];
}

static ERL_NIF_TERM hmac_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) or (Context, HashLen) */
    ERL_NIF_TERM ret;
    struct hmac_context* obj;
    unsigned char mac_buf[EVP_MAX_MD_SIZE];
    unsigned char * mac_bin;
    unsigned int req_len = 0;
    unsigned int mac_len;

    if (!enif_get_resource(env,argv[0],hmac_context_rtype, (void**)&obj)
	|| (argc == 2 && !enif_get_uint(env, argv[1], &req_len))) {
	return enif_make_badarg(env);
    }

    enif_mutex_lock(obj->mtx);
    if (!obj->alive) {
	enif_mutex_unlock(obj->mtx);
	return enif_make_badarg(env);
    }

    HMAC_Final(obj->ctx, mac_buf, &mac_len);
    HMAC_CTX_free(obj->ctx);
    obj->alive = 0;
    enif_mutex_unlock(obj->mtx);

    if (argc == 2 && req_len < mac_len) {
        /* Only truncate to req_len bytes if asked. */
        mac_len = req_len;
    }
    mac_bin = enif_make_new_binary(env, mac_len, &ret);
    memcpy(mac_bin, mac_buf, mac_len);

    return ret;
}

static ERL_NIF_TERM cmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key, Data) */
#if defined(HAVE_CMAC)
    struct cipher_type_t *cipherp = NULL;
    const EVP_CIPHER     *cipher;
    CMAC_CTX             *ctx;
    ErlNifBinary         key;
    ErlNifBinary         data;
    ERL_NIF_TERM         ret;
    size_t               ret_size;

    if (!enif_inspect_iolist_as_binary(env, argv[1], &key)
        || !(cipherp = get_cipher_type(argv[0], key.size))
        || !enif_inspect_iolist_as_binary(env, argv[2], &data)) {
        return enif_make_badarg(env);
    }
    cipher = cipherp->cipher.p;
    if (!cipher) {
        return enif_raise_exception(env, atom_notsup);
    }

    ctx = CMAC_CTX_new();
    if (!CMAC_Init(ctx, key.data, key.size, cipher, NULL)) {
        CMAC_CTX_free(ctx);
        return atom_notsup;
    }

    if (!CMAC_Update(ctx, data.data, data.size) ||
        !CMAC_Final(ctx,
                    enif_make_new_binary(env, EVP_CIPHER_block_size(cipher), &ret),
                    &ret_size)) {
        CMAC_CTX_free(ctx);
        return atom_notsup;
    }
    ASSERT(ret_size == (unsigned)EVP_CIPHER_block_size(cipher));

    CMAC_CTX_free(ctx);
    CONSUME_REDS(env, data);
    return ret;
#else
    /* The CMAC functionality was introduced in OpenSSL 1.0.1
     * Although OTP requires at least version 0.9.8, the versions 0.9.8 and 1.0.0 are
     * no longer maintained. */
    return atom_notsup;
#endif
}

static ERL_NIF_TERM block_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key, Ivec, Text, IsEncrypt) or (Type, Key, Text, IsEncrypt) */
    struct cipher_type_t *cipherp = NULL;
    const EVP_CIPHER     *cipher;
    ErlNifBinary         key, ivec, text;
    EVP_CIPHER_CTX*      ctx;
    ERL_NIF_TERM         ret;
    unsigned char        *out;
    int                  ivec_size, out_size = 0;

    if (!enif_inspect_iolist_as_binary(env, argv[1], &key)
        || !(cipherp = get_cipher_type(argv[0], key.size))
        || !enif_inspect_iolist_as_binary(env, argv[argc - 2], &text)) {
        return enif_make_badarg(env);
    }
    cipher = cipherp->cipher.p;
    if (!cipher) {
        return enif_raise_exception(env, atom_notsup);
    }

    if (argv[0] == atom_aes_cfb8
        && (key.size == 24 || key.size == 32)) {
        /* Why do EVP_CIPHER_CTX_set_key_length() fail on these key sizes?
         * Fall back on low level API
         */
        return aes_cfb_8_crypt(env, argc-1, argv+1);
    }
    else if (argv[0] == atom_aes_cfb128
        && (key.size == 24 || key.size == 32)) {
        /* Why do EVP_CIPHER_CTX_set_key_length() fail on these key sizes?
         * Fall back on low level API
         */
        return aes_cfb_128_crypt_nif(env, argc-1, argv+1);
   }

    ivec_size  = EVP_CIPHER_iv_length(cipher);

#ifdef HAVE_ECB_IVEC_BUG
    if (argv[0] == atom_aes_ecb || argv[0] == atom_blowfish_ecb ||
	argv[0] == atom_des_ecb)
	ivec_size = 0; /* 0.9.8l returns faulty ivec_size */
#endif

    if (text.size % EVP_CIPHER_block_size(cipher) != 0 ||
        (ivec_size == 0 ? argc != 4
                      : (argc != 5 ||
                         !enif_inspect_iolist_as_binary(env, argv[2], &ivec) ||
                         ivec.size != ivec_size))) {
        return enif_make_badarg(env);
    }

    out = enif_make_new_binary(env, text.size, &ret);

    ctx = EVP_CIPHER_CTX_new();
    if (!EVP_CipherInit_ex(ctx, cipher, NULL, NULL, NULL,
                           (argv[argc - 1] == atom_true)) ||
        !EVP_CIPHER_CTX_set_key_length(ctx, key.size) ||
        !(EVP_CIPHER_type(cipher) != NID_rc2_cbc ||
          EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_SET_RC2_KEY_BITS, key.size * 8, NULL)) ||
        !EVP_CipherInit_ex(ctx, NULL, NULL,
                           key.data, ivec_size ? ivec.data : NULL, -1) ||
        !EVP_CIPHER_CTX_set_padding(ctx, 0)) {

        EVP_CIPHER_CTX_free(ctx);
        return enif_raise_exception(env, atom_notsup);
    }

    if (text.size > 0 && /* OpenSSL 0.9.8h asserts text.size > 0 */
        (!EVP_CipherUpdate(ctx, out, &out_size, text.data, text.size)
         || (ASSERT(out_size == text.size), 0)
         || !EVP_CipherFinal_ex(ctx, out + out_size, &out_size))) {

        EVP_CIPHER_CTX_free(ctx);
        return enif_raise_exception(env, atom_notsup);
    }
    ASSERT(out_size == 0);
    EVP_CIPHER_CTX_free(ctx);
    CONSUME_REDS(env, text);

    return ret;
}

static ERL_NIF_TERM aes_cfb_8_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */
     ErlNifBinary key, ivec, text;
     AES_KEY aes_key;
     unsigned char ivec_clone[16]; /* writable copy */
     int new_ivlen = 0;
     ERL_NIF_TERM ret;

     CHECK_NO_FIPS_MODE();

     if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
         || !(key.size == 16 || key.size == 24 || key.size == 32)
         || !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 16
         || !enif_inspect_iolist_as_binary(env, argv[2], &text)) {
         return enif_make_badarg(env);
     }

     memcpy(ivec_clone, ivec.data, 16);
     AES_set_encrypt_key(key.data, key.size * 8, &aes_key);
     AES_cfb8_encrypt((unsigned char *) text.data,
                      enif_make_new_binary(env, text.size, &ret),
                      text.size, &aes_key, ivec_clone, &new_ivlen,
                      (argv[3] == atom_true));
     CONSUME_REDS(env,text);
     return ret;
}

static ERL_NIF_TERM aes_cfb_128_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */
    ErlNifBinary key, ivec, text;
    AES_KEY aes_key;
    unsigned char ivec_clone[16]; /* writable copy */
    int new_ivlen = 0;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
        || !(key.size == 16 || key.size == 24 || key.size == 32)
        || !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 16
        || !enif_inspect_iolist_as_binary(env, argv[2], &text)) {
        return enif_make_badarg(env);
    }

    memcpy(ivec_clone, ivec.data, 16);
    AES_set_encrypt_key(key.data, key.size * 8, &aes_key);
    AES_cfb128_encrypt((unsigned char *) text.data,
                       enif_make_new_binary(env, text.size, &ret),
                       text.size, &aes_key, ivec_clone, &new_ivlen,
                       (argv[3] == atom_true));
    CONSUME_REDS(env,text);
    return ret;
}

static ERL_NIF_TERM aes_ige_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */
#ifdef HAVE_AES_IGE
    ErlNifBinary key_bin, ivec_bin, data_bin;
    AES_KEY aes_key;
    unsigned char ivec[32];
    int i;
    unsigned char* ret_ptr;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
       || (key_bin.size != 16 && key_bin.size != 32)
       || !enif_inspect_binary(env, argv[1], &ivec_bin)
       || ivec_bin.size != 32
       || !enif_inspect_iolist_as_binary(env, argv[2], &data_bin)
       || data_bin.size % 16 != 0) {

       return enif_make_badarg(env);
    }

    if (argv[3] == atom_true) {
       i = AES_ENCRYPT;
       AES_set_encrypt_key(key_bin.data, key_bin.size*8, &aes_key);
    }
    else {
       i = AES_DECRYPT;
       AES_set_decrypt_key(key_bin.data, key_bin.size*8, &aes_key);
    }

    ret_ptr = enif_make_new_binary(env, data_bin.size, &ret);
    memcpy(ivec, ivec_bin.data, 32); /* writable copy */
    AES_ige_encrypt(data_bin.data, ret_ptr, data_bin.size, &aes_key, ivec, i);
    CONSUME_REDS(env,data_bin);
    return ret;
#else
    return atom_notsup;
#endif
}


/* Initializes state for ctr streaming (de)encryption
*/
#ifdef HAVE_EVP_AES_CTR
static ERL_NIF_TERM aes_ctr_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec) */
    ErlNifBinary     key_bin, ivec_bin;
    struct evp_cipher_ctx *ctx;
    const EVP_CIPHER *cipher;
    ERL_NIF_TERM     ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
        || !enif_inspect_binary(env, argv[1], &ivec_bin)
        || ivec_bin.size != 16) {
        return enif_make_badarg(env);
    }

    switch (key_bin.size)
    {
    case 16: cipher = EVP_aes_128_ctr(); break;
    case 24: cipher = EVP_aes_192_ctr(); break;
    case 32: cipher = EVP_aes_256_ctr(); break;
    default: return enif_make_badarg(env);
    }

    ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx));
    ctx->ctx = EVP_CIPHER_CTX_new();
    EVP_CipherInit_ex(ctx->ctx, cipher, NULL,
                      key_bin.data, ivec_bin.data, 1);
    EVP_CIPHER_CTX_set_padding(ctx->ctx, 0);
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}
static ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    struct evp_cipher_ctx *ctx, *new_ctx;
    ErlNifBinary   data_bin;
    ERL_NIF_TERM   ret, cipher_term;
    unsigned char  *out;
    int            outl = 0;

    if (!enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx)
        || !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
        return enif_make_badarg(env);
    }
    new_ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx));
    new_ctx->ctx = EVP_CIPHER_CTX_new();
    EVP_CIPHER_CTX_copy(new_ctx->ctx, ctx->ctx);
    out = enif_make_new_binary(env, data_bin.size, &cipher_term);
    EVP_CipherUpdate(new_ctx->ctx, out, &outl, data_bin.data, data_bin.size);
    ASSERT(outl == data_bin.size);

    ret = enif_make_tuple2(env, enif_make_resource(env, new_ctx), cipher_term);
    enif_release_resource(new_ctx);
    CONSUME_REDS(env,data_bin);
    return ret;
}

#else /* if not HAVE_EVP_AES_CTR */

static ERL_NIF_TERM aes_ctr_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec) */
    ErlNifBinary key_bin, ivec_bin;
    ERL_NIF_TERM ecount_bin;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
        || !enif_inspect_binary(env, argv[1], &ivec_bin)
        || !(key_bin.size == 16 || key_bin.size == 24 || key_bin.size ==32)
        || ivec_bin.size != 16) {
        return enif_make_badarg(env);
    }

    memset(enif_make_new_binary(env, AES_BLOCK_SIZE, &ecount_bin),
           0, AES_BLOCK_SIZE);
    return enif_make_tuple4(env, argv[0], argv[1], ecount_bin, enif_make_int(env, 0));
}

static ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* ({Key, IVec, ECount, Num}, Data) */
    ErlNifBinary key_bin, ivec_bin, text_bin, ecount_bin;
    AES_KEY aes_key;
    unsigned int num;
    ERL_NIF_TERM ret, num2_term, cipher_term, ivec2_term, ecount2_term, new_state_term;
    int state_arity;
    const ERL_NIF_TERM *state_term;
    unsigned char * ivec2_buf;
    unsigned char * ecount2_buf;

    if (!enif_get_tuple(env, argv[0], &state_arity, &state_term)
        || state_arity != 4
        || !enif_inspect_iolist_as_binary(env, state_term[0], &key_bin)
        || AES_set_encrypt_key(key_bin.data, key_bin.size*8, &aes_key) != 0
        || !enif_inspect_binary(env, state_term[1], &ivec_bin) || ivec_bin.size != 16
        || !enif_inspect_binary(env, state_term[2], &ecount_bin) || ecount_bin.size != AES_BLOCK_SIZE
        || !enif_get_uint(env, state_term[3], &num)
        || !enif_inspect_iolist_as_binary(env, argv[1], &text_bin)) {
        return enif_make_badarg(env);
    }

    ivec2_buf = enif_make_new_binary(env, ivec_bin.size, &ivec2_term);
    ecount2_buf = enif_make_new_binary(env, ecount_bin.size, &ecount2_term);

    memcpy(ivec2_buf, ivec_bin.data, 16);
    memcpy(ecount2_buf, ecount_bin.data, ecount_bin.size);

    AES_ctr128_encrypt((unsigned char *) text_bin.data,
		       enif_make_new_binary(env, text_bin.size, &cipher_term),
		       text_bin.size, &aes_key, ivec2_buf, ecount2_buf, &num);

    num2_term = enif_make_uint(env, num);
    new_state_term = enif_make_tuple4(env, state_term[0], ivec2_term, ecount2_term, num2_term);
    ret = enif_make_tuple2(env, new_state_term, cipher_term);
    CONSUME_REDS(env,text_bin);
    return ret;
}
#endif /* !HAVE_EVP_AES_CTR */

static ERL_NIF_TERM aes_gcm_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key,Iv,AAD,In) */
#if defined(HAVE_GCM)
    EVP_CIPHER_CTX *ctx;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in;
    unsigned int tag_len;
    unsigned char *outp, *tagp;
    ERL_NIF_TERM out, out_tag;
    int len;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| (key.size != 16 && key.size != 24 && key.size != 32)
	|| !enif_inspect_binary(env, argv[1], &iv) || iv.size == 0
	|| !enif_inspect_iolist_as_binary(env, argv[2], &aad)
	|| !enif_inspect_iolist_as_binary(env, argv[3], &in)
	|| !enif_get_uint(env, argv[4], &tag_len) || tag_len < 1 || tag_len > 16) {
	return enif_make_badarg(env);
    }

    if (key.size == 16)
        cipher = EVP_aes_128_gcm();
    else if (key.size == 24)
        cipher = EVP_aes_192_gcm();
    else if (key.size == 32)
        cipher = EVP_aes_256_gcm();

    ctx = EVP_CIPHER_CTX_new();

    if (EVP_EncryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1)
        goto out_err;

    EVP_CIPHER_CTX_set_padding(ctx, 0);

    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, iv.size, NULL) != 1)
        goto out_err;
    if (EVP_EncryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1)
        goto out_err;
    if (EVP_EncryptUpdate(ctx, NULL, &len, aad.data, aad.size) != 1)
        goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    if (EVP_EncryptUpdate(ctx, outp, &len, in.data, in.size) != 1)
        goto out_err;
    if (EVP_EncryptFinal_ex(ctx, outp+len, &len) != 1)
        goto out_err;

    tagp = enif_make_new_binary(env, tag_len, &out_tag);

    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, tag_len, tagp) != 1)
        goto out_err;

    EVP_CIPHER_CTX_free(ctx);

    CONSUME_REDS(env, in);

    return enif_make_tuple2(env, out, out_tag);

out_err:
    EVP_CIPHER_CTX_free(ctx);
    return atom_error;

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

static ERL_NIF_TERM aes_gcm_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key,Iv,AAD,In,Tag) */
#if defined(HAVE_GCM_EVP_DECRYPT_BUG)
    return aes_gcm_decrypt_NO_EVP(env, argc, argv);
#elif defined(HAVE_GCM)
    EVP_CIPHER_CTX *ctx;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in, tag;
    unsigned char *outp;
    ERL_NIF_TERM out;
    int len;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| (key.size != 16 && key.size != 24 && key.size != 32)
	|| !enif_inspect_binary(env, argv[1], &iv) || iv.size == 0
	|| !enif_inspect_iolist_as_binary(env, argv[2], &aad)
	|| !enif_inspect_iolist_as_binary(env, argv[3], &in)
	|| !enif_inspect_iolist_as_binary(env, argv[4], &tag)) {
	return enif_make_badarg(env);
    }

    if (key.size == 16)
        cipher = EVP_aes_128_gcm();
    else if (key.size == 24)
        cipher = EVP_aes_192_gcm();
    else if (key.size == 32)
        cipher = EVP_aes_256_gcm();

    ctx = EVP_CIPHER_CTX_new();

    if (EVP_DecryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1)
        goto out_err;
    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, iv.size, NULL) != 1)
        goto out_err;
    if (EVP_DecryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1)
        goto out_err;
    if (EVP_DecryptUpdate(ctx, NULL, &len, aad.data, aad.size) != 1)
        goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    if (EVP_DecryptUpdate(ctx, outp, &len, in.data, in.size) != 1)
        goto out_err;
    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_TAG, tag.size, tag.data) != 1)
        goto out_err;
    if (EVP_DecryptFinal_ex(ctx, outp+len, &len) != 1)
        goto out_err;

    EVP_CIPHER_CTX_free(ctx);

    CONSUME_REDS(env, in);

    return out;

out_err:
    EVP_CIPHER_CTX_free(ctx);
    return atom_error;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

#ifdef HAVE_GCM_EVP_DECRYPT_BUG
static ERL_NIF_TERM aes_gcm_decrypt_NO_EVP(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GCM128_CONTEXT *ctx;
    ErlNifBinary key, iv, aad, in, tag;
    AES_KEY aes_key;
    unsigned char *outp;
    ERL_NIF_TERM out;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
        || AES_set_encrypt_key(key.data, key.size*8, &aes_key) != 0
        || !enif_inspect_binary(env, argv[1], &iv) || iv.size == 0
        || !enif_inspect_iolist_as_binary(env, argv[2], &aad)
        || !enif_inspect_iolist_as_binary(env, argv[3], &in)
        || !enif_inspect_iolist_as_binary(env, argv[4], &tag)) {
        return enif_make_badarg(env);
    }

    if (!(ctx = CRYPTO_gcm128_new(&aes_key, (block128_f)AES_encrypt)))
        return atom_error;

    CRYPTO_gcm128_setiv(ctx, iv.data, iv.size);

    if (CRYPTO_gcm128_aad(ctx, aad.data, aad.size))
        goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    /* decrypt */
    if (CRYPTO_gcm128_decrypt(ctx, in.data, outp, in.size))
            goto out_err;

    /* calculate and check the tag */
    if (CRYPTO_gcm128_finish(ctx, tag.data, tag.size))
            goto out_err;

    CRYPTO_gcm128_release(ctx);
    CONSUME_REDS(env, in);

    return out;

out_err:
    CRYPTO_gcm128_release(ctx);
    return atom_error;
}
#endif /* HAVE_GCM_EVP_DECRYPT_BUG */


static ERL_NIF_TERM chacha20_poly1305_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key,Iv,AAD,In) */
#if defined(HAVE_CHACHA20_POLY1305)
    EVP_CIPHER_CTX *ctx;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in;
    unsigned char *outp, *tagp;
    ERL_NIF_TERM out, out_tag;
    int len;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key) || key.size != 32
	|| !enif_inspect_binary(env, argv[1], &iv) || iv.size == 0 || iv.size > 16
	|| !enif_inspect_iolist_as_binary(env, argv[2], &aad)
	|| !enif_inspect_iolist_as_binary(env, argv[3], &in)) {
	return enif_make_badarg(env);
    }

    cipher = EVP_chacha20_poly1305();

    ctx = EVP_CIPHER_CTX_new();

    if (EVP_EncryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1)
        goto out_err;

    EVP_CIPHER_CTX_set_padding(ctx, 0);

    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_IVLEN, iv.size, NULL) != 1)
        goto out_err;
    if (EVP_EncryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1)
        goto out_err;
    if (EVP_EncryptUpdate(ctx, NULL, &len, aad.data, aad.size) != 1)
        goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    if (EVP_EncryptUpdate(ctx, outp, &len, in.data, in.size) != 1)
        goto out_err;
    if (EVP_EncryptFinal_ex(ctx, outp+len, &len) != 1)
        goto out_err;

    tagp = enif_make_new_binary(env, 16, &out_tag);

    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_GET_TAG, 16, tagp) != 1)
        goto out_err;

    EVP_CIPHER_CTX_free(ctx);

    CONSUME_REDS(env, in);

    return enif_make_tuple2(env, out, out_tag);

out_err:
    EVP_CIPHER_CTX_free(ctx);
    return atom_error;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

static ERL_NIF_TERM chacha20_poly1305_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key,Iv,AAD,In,Tag) */
#if defined(HAVE_CHACHA20_POLY1305)
    EVP_CIPHER_CTX *ctx;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in, tag;
    unsigned char *outp;
    ERL_NIF_TERM out;
    int len;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key) || key.size != 32
	|| !enif_inspect_binary(env, argv[1], &iv) || iv.size == 0 || iv.size > 16
	|| !enif_inspect_iolist_as_binary(env, argv[2], &aad)
	|| !enif_inspect_iolist_as_binary(env, argv[3], &in)
	|| !enif_inspect_iolist_as_binary(env, argv[4], &tag) || tag.size != 16) {
	return enif_make_badarg(env);
    }

    cipher = EVP_chacha20_poly1305();

    ctx = EVP_CIPHER_CTX_new();

    if (EVP_DecryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1)
        goto out_err;
    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_IVLEN, iv.size, NULL) != 1)
        goto out_err;
    if (EVP_DecryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1)
        goto out_err;
    if (EVP_DecryptUpdate(ctx, NULL, &len, aad.data, aad.size) != 1)
        goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    if (EVP_DecryptUpdate(ctx, outp, &len, in.data, in.size) != 1)
        goto out_err;
    if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_TAG, tag.size, tag.data) != 1)
        goto out_err;
    if (EVP_DecryptFinal_ex(ctx, outp+len, &len) != 1)
        goto out_err;

    EVP_CIPHER_CTX_free(ctx);

    CONSUME_REDS(env, in);

    return out;

out_err:
    EVP_CIPHER_CTX_free(ctx);
    return atom_error;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

static ERL_NIF_TERM strong_rand_bytes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Bytes) */
    unsigned bytes;
    unsigned char* data;
    ERL_NIF_TERM ret;

    if (!enif_get_uint(env, argv[0], &bytes)) {
	return enif_make_badarg(env);
    }
    data = enif_make_new_binary(env, bytes, &ret);
    if ( RAND_bytes(data, bytes) != 1) {
        return atom_false;
    }
    ERL_VALGRIND_MAKE_MEM_DEFINED(data, bytes);
    return ret;
}


static int get_bn_from_mpint(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp)
{
    ErlNifBinary bin;
    int sz;
    if (!enif_inspect_binary(env,term,&bin)) {
	return 0;
    }
    ERL_VALGRIND_ASSERT_MEM_DEFINED(bin.data, bin.size);
    sz = bin.size - 4;
    if (sz < 0 || get_int32(bin.data) != sz) {
	return 0;
    }
    *bnp = BN_bin2bn(bin.data+4, sz, NULL);
    return 1;
}

static int get_bn_from_bin(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp)
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env,term,&bin)) {
	return 0;
    }
    ERL_VALGRIND_ASSERT_MEM_DEFINED(bin.data, bin.size);
    *bnp = BN_bin2bn(bin.data, bin.size, NULL);
    return 1;
}

static ERL_NIF_TERM bin_from_bn(ErlNifEnv* env, const BIGNUM *bn)
{
    int bn_len;
    unsigned char *bin_ptr;
    ERL_NIF_TERM term;

    /* Copy the bignum into an erlang binary. */
    bn_len = BN_num_bytes(bn);
    bin_ptr = enif_make_new_binary(env, bn_len, &term);
    BN_bn2bin(bn, bin_ptr);

    return term;
}

static ERL_NIF_TERM strong_rand_range_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Range) */
    BIGNUM *bn_range, *bn_rand;
    ERL_NIF_TERM ret;

    if(!get_bn_from_bin(env, argv[0], &bn_range)) {
        return enif_make_badarg(env);
    }

    bn_rand = BN_new();
    if (BN_rand_range(bn_rand, bn_range) != 1) {
        ret = atom_false;
    }
    else {
        ret = bin_from_bn(env, bn_rand);
    }
    BN_free(bn_rand);
    BN_free(bn_range);
    return ret;
}

static ERL_NIF_TERM rand_uniform_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Lo,Hi) */
    BIGNUM *bn_from = NULL, *bn_to, *bn_rand;
    unsigned char* data;
    unsigned dlen;
    ERL_NIF_TERM ret;

    if (!get_bn_from_mpint(env, argv[0], &bn_from)
	|| !get_bn_from_mpint(env, argv[1], &bn_rand)) {
	if (bn_from) BN_free(bn_from);
	return enif_make_badarg(env);
    }

    bn_to = BN_new();
    BN_sub(bn_to, bn_rand, bn_from);
    BN_pseudo_rand_range(bn_rand, bn_to);
    BN_add(bn_rand, bn_rand, bn_from);
    dlen = BN_num_bytes(bn_rand);
    data = enif_make_new_binary(env, dlen+4, &ret);
    put_int32(data, dlen);
    BN_bn2bin(bn_rand, data+4);
    ERL_VALGRIND_MAKE_MEM_DEFINED(data+4, dlen);
    BN_free(bn_rand);
    BN_free(bn_from);
    BN_free(bn_to);
    return ret;
}

static ERL_NIF_TERM mod_exp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Base,Exponent,Modulo,bin_hdr) */
    BIGNUM *bn_base=NULL, *bn_exponent=NULL, *bn_modulo=NULL, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;
    unsigned bin_hdr; /* return type: 0=plain binary, 4: mpint */
    unsigned extra_byte;
    ERL_NIF_TERM ret;

    if (!get_bn_from_bin(env, argv[0], &bn_base)
	|| !get_bn_from_bin(env, argv[1], &bn_exponent)
	|| !get_bn_from_bin(env, argv[2], &bn_modulo)
	|| !enif_get_uint(env,argv[3],&bin_hdr) || (bin_hdr & ~4)) {

	if (bn_base) BN_free(bn_base);
	if (bn_exponent) BN_free(bn_exponent);
	if (bn_modulo) BN_free(bn_modulo);
	return enif_make_badarg(env);
    }
    bn_result = BN_new();
    bn_ctx = BN_CTX_new();
    BN_mod_exp(bn_result, bn_base, bn_exponent, bn_modulo, bn_ctx);
    dlen = BN_num_bytes(bn_result);
    extra_byte = bin_hdr && BN_is_bit_set(bn_result, dlen*8-1);
    ptr = enif_make_new_binary(env, bin_hdr+extra_byte+dlen, &ret);
    if (bin_hdr) {
	put_int32(ptr, extra_byte+dlen);
	ptr[4] = 0; /* extra zeroed byte to ensure a positive mpint */
	ptr += bin_hdr + extra_byte;
    }
    BN_bn2bin(bn_result, ptr);
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);
    BN_free(bn_modulo);
    BN_free(bn_exponent);
    BN_free(bn_base);
    return ret;
}

static void init_digest_types(ErlNifEnv* env)
{
    struct digest_type_t* p = digest_types;

    for (p = digest_types; p->type.str; p++) {
	p->type.atom = enif_make_atom(env, p->type.str);
	if (p->md.funcp)
	    p->md.p = p->md.funcp();
    }
    p->type.atom = atom_false;  /* end marker */
}

static void init_cipher_types(ErlNifEnv* env)
{
    struct cipher_type_t* p = cipher_types;

    for (p = cipher_types; p->type.str; p++) {
	p->type.atom = enif_make_atom(env, p->type.str);
	if (p->cipher.funcp)
	    p->cipher.p = p->cipher.funcp();
    }
    p->type.atom = atom_false; /* end marker */
}

static struct digest_type_t* get_digest_type(ERL_NIF_TERM type)
{
    struct digest_type_t* p = NULL;
    for (p = digest_types; p->type.atom != atom_false; p++) {
	if (type == p->type.atom) {
	    return p;
	}
    }
    return NULL;
}

static struct cipher_type_t* get_cipher_type(ERL_NIF_TERM type, size_t key_len)
{
    struct cipher_type_t* p = NULL;
    for (p = cipher_types; p->type.atom != atom_false; p++) {
	if (type == p->type.atom && (!p->key_len || key_len == p->key_len)) {
	    return p;
	}
    }
    return NULL;
}


static ERL_NIF_TERM do_exor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data1, Data2) */
    ErlNifBinary d1, d2;
    unsigned char* ret_ptr;
    int i;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env,argv[0], &d1)
	|| !enif_inspect_iolist_as_binary(env,argv[1], &d2)
	|| d1.size != d2.size) {
	return enif_make_badarg(env);
    }
    ret_ptr = enif_make_new_binary(env, d1.size, &ret);

    for (i=0; i<d1.size; i++) {
	ret_ptr[i] = d1.data[i] ^ d2.data[i];
    }
    CONSUME_REDS(env,d1);
    return ret;
}

static ERL_NIF_TERM rc4_set_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key) */
#ifndef OPENSSL_NO_RC4
    ErlNifBinary key;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!enif_inspect_iolist_as_binary(env,argv[0], &key)) {
	return enif_make_badarg(env);
    }
    RC4_set_key((RC4_KEY*)enif_make_new_binary(env, sizeof(RC4_KEY), &ret),
		key.size, key.data);
    return ret;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

static ERL_NIF_TERM rc4_encrypt_with_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (State, Data) */
#ifndef OPENSSL_NO_RC4
    ErlNifBinary state, data;
    RC4_KEY* rc4_key;
    ERL_NIF_TERM new_state, new_data;

    CHECK_NO_FIPS_MODE();

    if (!enif_inspect_iolist_as_binary(env,argv[0], &state)
	|| state.size != sizeof(RC4_KEY)
	|| !enif_inspect_iolist_as_binary(env,argv[1], &data)) {
	return enif_make_badarg(env);
    }
    rc4_key = (RC4_KEY*)enif_make_new_binary(env, sizeof(RC4_KEY), &new_state);
    memcpy(rc4_key, state.data, sizeof(RC4_KEY));
    RC4(rc4_key, data.size, data.data,
	enif_make_new_binary(env, data.size, &new_data));
    CONSUME_REDS(env,data);
    return enif_make_tuple2(env,new_state,new_data);
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

static int get_rsa_private_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa)
{
    /* key=[E,N,D]|[E,N,D,P1,P2,E1,E2,C] */
    ERL_NIF_TERM head, tail;
    BIGNUM *e, *n, *d;
    BIGNUM *p, *q;
    BIGNUM *dmp1, *dmq1, *iqmp;

    if (!enif_get_list_cell(env, key, &head, &tail)
	|| !get_bn_from_bin(env, head, &e)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &n)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &d)) {
	return 0;
    }
    (void) RSA_set0_key(rsa, n, e, d);
    if (enif_is_empty_list(env, tail)) {
	return 1;
    }
    if (!enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &q)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dmp1)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dmq1)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &iqmp)
	|| !enif_is_empty_list(env, tail)) {
	return 0;
    }
    (void) RSA_set0_factors(rsa, p, q);
    (void) RSA_set0_crt_params(rsa, dmp1, dmq1, iqmp);
    return 1;
}


static int get_rsa_public_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa)
{
    /* key=[E,N] */
    ERL_NIF_TERM head, tail;
    BIGNUM *e, *n;

    if (!enif_get_list_cell(env, key, &head, &tail)
	|| !get_bn_from_bin(env, head, &e)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &n)
        || !enif_is_empty_list(env, tail)) {
	return 0;
    }

    (void) RSA_set0_key(rsa, n, e, NULL);
    return 1;
}

static int get_dss_private_key(ErlNifEnv* env, ERL_NIF_TERM key, DSA *dsa)
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


static int get_dss_public_key(ErlNifEnv* env, ERL_NIF_TERM key, DSA *dsa)
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

/* Creates a term which can be parsed by get_rsa_private_key(). This is a list of plain integer binaries (not mpints). */
static ERL_NIF_TERM put_rsa_private_key(ErlNifEnv* env, const RSA *rsa)
{
    ERL_NIF_TERM result[8];
    const BIGNUM *n, *e, *d, *p, *q, *dmp1, *dmq1, *iqmp;

    /* Return at least [E,N,D] */
    n = NULL; e = NULL; d = NULL;
    RSA_get0_key(rsa, &n, &e, &d);

    result[0] = bin_from_bn(env, e);  // Exponent E
    result[1] = bin_from_bn(env, n);  // Modulus N = p*q
    result[2] = bin_from_bn(env, d);  // Exponent D

    /* Check whether the optional additional parameters are available */
    p = NULL; q = NULL;
    RSA_get0_factors(rsa, &p, &q);
    dmp1 = NULL; dmq1 = NULL; iqmp = NULL;
    RSA_get0_crt_params(rsa, &dmp1, &dmq1, &iqmp);

    if (p && q && dmp1 && dmq1 && iqmp) {
	result[3] = bin_from_bn(env, p);     // Factor p
	result[4] = bin_from_bn(env, q);     // Factor q
	result[5] = bin_from_bn(env, dmp1);  // D mod (p-1)
	result[6] = bin_from_bn(env, dmq1);  // D mod (q-1)
	result[7] = bin_from_bn(env, iqmp);  // (1/q) mod p

	return enif_make_list_from_array(env, result, 8);
    } else {
	return enif_make_list_from_array(env, result, 3);
    }
}

static int check_erlang_interrupt(int maj, int min, BN_GENCB *ctxt)
{
    ErlNifEnv *env = BN_GENCB_get_arg(ctxt);

    if (!enif_is_current_process_alive(env)) {
	return 0;
    } else {
	return 1;
    }
}

static ERL_NIF_TERM rsa_generate_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (ModulusSize, PublicExponent) */
    int modulus_bits;
    BIGNUM *pub_exp, *three;
    RSA *rsa;
    int success;
    ERL_NIF_TERM result;
    BN_GENCB *intr_cb;
#ifndef HAVE_OPAQUE_BN_GENCB
    BN_GENCB intr_cb_buf;
#endif

    if (!enif_get_int(env, argv[0], &modulus_bits) || modulus_bits < 256) {
	return enif_make_badarg(env);
    }

    if (!get_bn_from_bin(env, argv[1], &pub_exp)) {
	return enif_make_badarg(env);
    }

    /* Make sure the public exponent is large enough (at least 3).
     * Without this, RSA_generate_key_ex() can run forever. */
    three = BN_new();
    BN_set_word(three, 3);
    success = BN_cmp(pub_exp, three);
    BN_free(three);
    if (success < 0) {
	BN_free(pub_exp);
	return enif_make_badarg(env);
    }

    /* For large keys, prime generation can take many seconds. Set up
     * the callback which we use to test whether the process has been
     * interrupted. */
#ifdef HAVE_OPAQUE_BN_GENCB
    intr_cb = BN_GENCB_new();
#else
    intr_cb = &intr_cb_buf;
#endif
    BN_GENCB_set(intr_cb, check_erlang_interrupt, env);

    rsa = RSA_new();
    success = RSA_generate_key_ex(rsa, modulus_bits, pub_exp, intr_cb);
    BN_free(pub_exp);

#ifdef HAVE_OPAQUE_BN_GENCB
    BN_GENCB_free(intr_cb);
#endif

    if (!success) {
        RSA_free(rsa);
	return atom_error;
    }

    result = put_rsa_private_key(env, rsa);
    RSA_free(rsa);

    return result;
}

static ERL_NIF_TERM rsa_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* RSA key generation can take a long time (>1 sec for a large
     * modulus), so schedule it as a CPU-bound operation. */
    return enif_schedule_nif(env, "rsa_generate_key",
			     ERL_NIF_DIRTY_JOB_CPU_BOUND,
			     rsa_generate_key, argc, argv);
}

static ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (PrivKey|undefined, DHParams=[P,G], Mpint, Len|0) */
    DH* dh_params;
    int pub_len, prv_len;
    unsigned char *pub_ptr, *prv_ptr;
    ERL_NIF_TERM ret, ret_pub, ret_prv, head, tail;
    int mpint; /* 0 or 4 */
    BIGNUM *priv_key = NULL;
    BIGNUM *dh_p = NULL, *dh_g = NULL;
    unsigned long len = 0;

    if (!(get_bn_from_bin(env, argv[0], &priv_key)
	  || argv[0] == atom_undefined)
	|| !enif_get_list_cell(env, argv[1], &head, &tail)
	|| !get_bn_from_bin(env, head, &dh_p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dh_g)
	|| !enif_is_empty_list(env, tail)
	|| !enif_get_int(env, argv[2], &mpint) || (mpint & ~4)
	|| !enif_get_ulong(env, argv[3], &len)  ) {

        if (priv_key) BN_free(priv_key);
	if (dh_p) BN_free(dh_p);
	if (dh_g) BN_free(dh_g);
	return enif_make_badarg(env);
    }

    dh_params = DH_new();
    DH_set0_key(dh_params, NULL, priv_key);
    DH_set0_pqg(dh_params, dh_p, NULL, dh_g);

    if (len) {
        if (len < BN_num_bits(dh_p))
            DH_set_length(dh_params, len);
        else {
            DH_free(dh_params);
            return enif_make_badarg(env);
        }
    }

    if (DH_generate_key(dh_params)) {
	const BIGNUM *pub_key, *priv_key;
	DH_get0_key(dh_params, &pub_key, &priv_key);
	pub_len = BN_num_bytes(pub_key);
	prv_len = BN_num_bytes(priv_key);
	pub_ptr = enif_make_new_binary(env, pub_len+mpint, &ret_pub);
	prv_ptr = enif_make_new_binary(env, prv_len+mpint, &ret_prv);
	if (mpint) {
	    put_int32(pub_ptr, pub_len); pub_ptr += 4;
	    put_int32(prv_ptr, prv_len); prv_ptr += 4;
	}
	BN_bn2bin(pub_key, pub_ptr);
	BN_bn2bin(priv_key, prv_ptr);
	ERL_VALGRIND_MAKE_MEM_DEFINED(pub_ptr, pub_len);
	ERL_VALGRIND_MAKE_MEM_DEFINED(prv_ptr, prv_len);
	ret = enif_make_tuple2(env, ret_pub, ret_prv);
    }
    else {
	ret = atom_error;
    }
    DH_free(dh_params);
    return ret;
}

static ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (OthersPublicKey, MyPrivateKey, DHParams=[P,G]) */
    DH* dh_params;
    BIGNUM *dummy_pub_key = NULL, *priv_key = NULL;
    BIGNUM *other_pub_key;
    BIGNUM *dh_p = NULL, *dh_g = NULL;
    int i;
    ErlNifBinary ret_bin;
    ERL_NIF_TERM ret, head, tail;

    dh_params = DH_new();

    if (!get_bn_from_bin(env, argv[0], &other_pub_key)
	|| !get_bn_from_bin(env, argv[1], &priv_key)
	|| !enif_get_list_cell(env, argv[2], &head, &tail)
	|| !get_bn_from_bin(env, head, &dh_p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dh_g)
	|| !enif_is_empty_list(env, tail)) {
	if (dh_p) BN_free(dh_p);
	if (dh_g) BN_free(dh_g);
	ret = enif_make_badarg(env);
    }
    else {
	/* Note: DH_set0_key() does not allow setting only the
	 * private key, although DH_compute_key() does not use the
	 * public key. Work around this limitation by setting
	 * the public key to a copy of the private key.
	 */
        dummy_pub_key = BN_dup(priv_key);
	DH_set0_key(dh_params, dummy_pub_key, priv_key);
	DH_set0_pqg(dh_params, dh_p, NULL, dh_g);
	enif_alloc_binary(DH_size(dh_params), &ret_bin);
	i = DH_compute_key(ret_bin.data, other_pub_key, dh_params);
	if (i > 0) {
	    if (i != ret_bin.size) {
		enif_realloc_binary(&ret_bin, i);
	    }
	    ret = enif_make_binary(env, &ret_bin);
	}
	else {
            enif_release_binary(&ret_bin);
	    ret = atom_error;
	}
    }
    if (other_pub_key) BN_free(other_pub_key);
    DH_free(dh_params);
    return ret;
}

static ERL_NIF_TERM srp_value_B_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Multiplier, Verifier, Generator, Exponent, Prime) */
    BIGNUM *bn_verifier = NULL;
    BIGNUM *bn_exponent = NULL, *bn_generator = NULL, *bn_prime = NULL, *bn_multiplier = NULL, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!get_bn_from_bin(env, argv[0], &bn_multiplier)
	|| !get_bn_from_bin(env, argv[1], &bn_verifier)
	|| !get_bn_from_bin(env, argv[2], &bn_generator)
	|| !get_bn_from_bin(env, argv[3], &bn_exponent)
	|| !get_bn_from_bin(env, argv[4], &bn_prime)) {
	if (bn_multiplier) BN_free(bn_multiplier);
	if (bn_verifier) BN_free(bn_verifier);
	if (bn_generator) BN_free(bn_generator);
	if (bn_exponent) BN_free(bn_exponent);
	if (bn_prime) BN_free(bn_prime);
	return enif_make_badarg(env);
    }

    bn_result = BN_new();
    bn_ctx = BN_CTX_new();

    /* B = k*v + g^b % N */

    /* k * v */
    BN_mod_mul(bn_multiplier, bn_multiplier, bn_verifier, bn_prime, bn_ctx);

    /* g^b % N */
    BN_mod_exp(bn_result, bn_generator, bn_exponent, bn_prime, bn_ctx);

    /* k*v + g^b % N */
    BN_mod_add(bn_result, bn_result, bn_multiplier, bn_prime, bn_ctx);

    /* check that B % N != 0, reuse bn_multiplier */
    BN_nnmod(bn_multiplier, bn_result, bn_prime, bn_ctx);
    if (BN_is_zero(bn_multiplier)) {
	ret = atom_error;
    } else {
	dlen = BN_num_bytes(bn_result);
	ptr = enif_make_new_binary(env, dlen, &ret);
	BN_bn2bin(bn_result, ptr);
    }
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);
    BN_free(bn_prime);
    BN_free(bn_generator);
    BN_free(bn_multiplier);
    BN_free(bn_exponent);
    BN_free(bn_verifier);
    return ret;
}

static ERL_NIF_TERM srp_user_secret_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (a, u, B, Multiplier, Prime, Exponent, Generator) */
/*
        <premaster secret> = (B - (k * g^x)) ^ (a + (u * x)) % N
*/
    BIGNUM *bn_exponent = NULL, *bn_a = NULL;
    BIGNUM *bn_u = NULL, *bn_multiplier = NULL, *bn_exp2,
        *bn_base, *bn_prime = NULL, *bn_generator = NULL,
        *bn_B = NULL, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!get_bn_from_bin(env, argv[0], &bn_a)
	|| !get_bn_from_bin(env, argv[1], &bn_u)
	|| !get_bn_from_bin(env, argv[2], &bn_B)
	|| !get_bn_from_bin(env, argv[3], &bn_multiplier)
	|| !get_bn_from_bin(env, argv[4], &bn_generator)
	|| !get_bn_from_bin(env, argv[5], &bn_exponent)
	|| !get_bn_from_bin(env, argv[6], &bn_prime))
    {
	if (bn_exponent) BN_free(bn_exponent);
	if (bn_a) BN_free(bn_a);
	if (bn_u) BN_free(bn_u);
	if (bn_B) BN_free(bn_B);
	if (bn_multiplier) BN_free(bn_multiplier);
	if (bn_generator) BN_free(bn_generator);
	if (bn_prime) BN_free(bn_prime);
	return enif_make_badarg(env);
    }

    bn_ctx = BN_CTX_new();
    bn_result = BN_new();

    /* check that B % N != 0 */
    BN_nnmod(bn_result, bn_B, bn_prime, bn_ctx);
    if (BN_is_zero(bn_result)) {
	BN_free(bn_exponent);
	BN_free(bn_a);
	BN_free(bn_generator);
	BN_free(bn_prime);
	BN_free(bn_u);
	BN_free(bn_B);
	BN_CTX_free(bn_ctx);

	return atom_error;
    }

    /* (B - (k * g^x)) */
    bn_base = BN_new();
    BN_mod_exp(bn_result, bn_generator, bn_exponent, bn_prime, bn_ctx);
    BN_mod_mul(bn_result, bn_multiplier, bn_result, bn_prime, bn_ctx);
    BN_mod_sub(bn_base, bn_B, bn_result, bn_prime, bn_ctx);

    /* a + (u * x) */
    bn_exp2 = BN_new();
    BN_mul(bn_result, bn_u, bn_exponent, bn_ctx);
    BN_add(bn_exp2, bn_a, bn_result);

    /* (B - (k * g^x)) ^ (a + (u * x)) % N */
    BN_mod_exp(bn_result, bn_base, bn_exp2, bn_prime, bn_ctx);

    dlen = BN_num_bytes(bn_result);
    ptr = enif_make_new_binary(env, dlen, &ret);
    BN_bn2bin(bn_result, ptr);
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);

    BN_free(bn_multiplier);
    BN_free(bn_exp2);
    BN_free(bn_u);
    BN_free(bn_exponent);
    BN_free(bn_a);
    BN_free(bn_B);
    BN_free(bn_base);
    BN_free(bn_generator);
    BN_free(bn_prime);
    return ret;
}

static ERL_NIF_TERM srp_host_secret_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Verifier, b, u, A, Prime) */
/*
        <premaster secret> = (A * v^u) ^ b % N
*/
    BIGNUM *bn_b = NULL, *bn_verifier = NULL;
    BIGNUM *bn_prime = NULL, *bn_A = NULL, *bn_u = NULL, *bn_base, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!get_bn_from_bin(env, argv[0], &bn_verifier)
	|| !get_bn_from_bin(env, argv[1], &bn_b)
	|| !get_bn_from_bin(env, argv[2], &bn_u)
	|| !get_bn_from_bin(env, argv[3], &bn_A)
	|| !get_bn_from_bin(env, argv[4], &bn_prime))
    {
	if (bn_verifier) BN_free(bn_verifier);
	if (bn_b) BN_free(bn_b);
	if (bn_u) BN_free(bn_u);
	if (bn_A) BN_free(bn_A);
	if (bn_prime) BN_free(bn_prime);
	return enif_make_badarg(env);
    }

    bn_ctx = BN_CTX_new();
    bn_result = BN_new();

    /* check that A % N != 0 */
    BN_nnmod(bn_result, bn_A, bn_prime, bn_ctx);
    if (BN_is_zero(bn_result)) {
	BN_free(bn_b);
	BN_free(bn_verifier);
	BN_free(bn_prime);
	BN_free(bn_A);
	BN_CTX_free(bn_ctx);

	return atom_error;
    }

    /* (A * v^u) */
    bn_base = BN_new();
    BN_mod_exp(bn_base, bn_verifier, bn_u, bn_prime, bn_ctx);
    BN_mod_mul(bn_base, bn_A, bn_base, bn_prime, bn_ctx);

    /* (A * v^u) ^ b % N */
    BN_mod_exp(bn_result, bn_base, bn_b, bn_prime, bn_ctx);

    dlen = BN_num_bytes(bn_result);
    ptr = enif_make_new_binary(env, dlen, &ret);
    BN_bn2bin(bn_result, ptr);
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);

    BN_free(bn_u);
    BN_free(bn_base);
    BN_free(bn_verifier);
    BN_free(bn_prime);
    BN_free(bn_A);
    BN_free(bn_b);
    return ret;
}

#if defined(HAVE_EC)
static EC_KEY* ec_key_new(ErlNifEnv* env, ERL_NIF_TERM curve_arg)
{
    EC_KEY *key = NULL;
    int c_arity = -1;
    const ERL_NIF_TERM* curve;
    ErlNifBinary seed;
    BIGNUM *p = NULL;
    BIGNUM *a = NULL;
    BIGNUM *b = NULL;
    BIGNUM *bn_order = NULL;
    BIGNUM *cofactor = NULL;
    EC_GROUP *group = NULL;
    EC_POINT *point = NULL;

    /* {Field, Prime, Point, Order, CoFactor} = Curve */
    if (enif_get_tuple(env,curve_arg,&c_arity,&curve)
	&& c_arity == 5
	&& get_bn_from_bin(env, curve[3], &bn_order)
	&& (curve[4] != atom_none && get_bn_from_bin(env, curve[4], &cofactor))) {

	int f_arity = -1;
	const ERL_NIF_TERM* field;
	int p_arity = -1;
	const ERL_NIF_TERM* prime;

	long field_bits;

	/* {A, B, Seed} = Prime */
	if (!enif_get_tuple(env,curve[1],&p_arity,&prime)
	    || !get_bn_from_bin(env, prime[0], &a)
	    || !get_bn_from_bin(env, prime[1], &b))
	    goto out_err;

	if (!enif_get_tuple(env,curve[0],&f_arity,&field))
	    goto out_err;

	if (f_arity == 2 && field[0] == atom_prime_field) {
	    /* {prime_field, Prime} */

	    if (!get_bn_from_bin(env, field[1], &p))
		goto out_err;

	    if (BN_is_negative(p) || BN_is_zero(p))
		goto out_err;

	    field_bits = BN_num_bits(p);
	    if (field_bits > OPENSSL_ECC_MAX_FIELD_BITS)
		goto out_err;

	    /* create the EC_GROUP structure */
	    group = EC_GROUP_new_curve_GFp(p, a, b, NULL);

	} else if (f_arity == 3 && field[0] == atom_characteristic_two_field) {
#if defined(OPENSSL_NO_EC2M)
	    enif_raise_exception(env, atom_notsup);
	    goto out_err;
#else
	    /* {characteristic_two_field, M, Basis} */

	    int b_arity = -1;
	    const ERL_NIF_TERM* basis;
	    unsigned int k1, k2, k3;

	    if ((p = BN_new()) == NULL)
		goto out_err;

	    if (!enif_get_long(env, field[1], &field_bits)
		|| field_bits > OPENSSL_ECC_MAX_FIELD_BITS)
		goto out_err;

	    if (enif_get_tuple(env,field[2],&b_arity,&basis)) {
		if (b_arity == 2
		    && basis[0] == atom_tpbasis
		    && enif_get_uint(env, basis[1], &k1)) {
		    /* {tpbasis, k} = Basis */

		    if (!(field_bits > k1 && k1 > 0))
			goto out_err;

		    /* create the polynomial */
		    if (!BN_set_bit(p, (int)field_bits)
                        || !BN_set_bit(p, (int)k1)
                        || !BN_set_bit(p, 0))
                                goto out_err;

		} else if (b_arity == 4
		    && basis[0] == atom_ppbasis
		    && enif_get_uint(env, basis[1], &k1)
		    && enif_get_uint(env, basis[2], &k2)
		    && enif_get_uint(env, basis[3], &k3)) {
		    /* {ppbasis, k1, k2, k3} = Basis */

		    if (!(field_bits > k3 && k3 > k2 && k2 > k1 && k1 > 0))
			goto out_err;

		    /* create the polynomial */
		    if (!BN_set_bit(p, (int)field_bits)
			|| !BN_set_bit(p, (int)k1)
			|| !BN_set_bit(p, (int)k2)
			|| !BN_set_bit(p, (int)k3)
			|| !BN_set_bit(p, 0))
			goto out_err;

		} else
		    goto out_err;
	    } else if (field[2] == atom_onbasis) {
		/* onbasis = Basis */
		/* no parameters */
		goto out_err;

	    } else
		goto out_err;

	    group = EC_GROUP_new_curve_GF2m(p, a, b, NULL);
#endif
	} else
	    goto out_err;

        if (!group)
            goto out_err;

	if (enif_inspect_binary(env, prime[2], &seed)) {
	    EC_GROUP_set_seed(group, seed.data, seed.size);
	}

	if (!term2point(env, curve[2], group, &point))
	    goto out_err;

	if (BN_is_negative(bn_order)
	    || BN_is_zero(bn_order)
	    || BN_num_bits(bn_order) > (int)field_bits + 1)
	    goto out_err;

	if (!EC_GROUP_set_generator(group, point, bn_order, cofactor))
	    goto out_err;

	EC_GROUP_set_asn1_flag(group, 0x0);

	key = EC_KEY_new();
	if (!key)
	    goto out_err;
	EC_KEY_set_group(key, group);
    }
    else {
	goto out_err;
    }


    goto out;

out_err:
    if (key) EC_KEY_free(key);
    key = NULL;

out:
    /* some OpenSSL structures are mem-dup'ed into the key,
       so we have to free our copies here */
    if (p) BN_free(p);
    if (a) BN_free(a);
    if (b) BN_free(b);
    if (bn_order) BN_free(bn_order);
    if (cofactor) BN_free(cofactor);
    if (group) EC_GROUP_free(group);
    if (point) EC_POINT_free(point);

    return key;
}


static ERL_NIF_TERM bn2term(ErlNifEnv* env, const BIGNUM *bn)
{
    unsigned dlen;
    unsigned char* ptr;
    ERL_NIF_TERM ret;

    if (!bn)
	    return atom_undefined;

    dlen = BN_num_bytes(bn);
    ptr = enif_make_new_binary(env, dlen, &ret);
    BN_bn2bin(bn, ptr);
    ERL_VALGRIND_MAKE_MEM_DEFINED(ptr, dlen);
    return ret;
}

static ERL_NIF_TERM point2term(ErlNifEnv* env,
			       const EC_GROUP *group,
			       const EC_POINT *point,
			       point_conversion_form_t form)
{
    unsigned dlen;
    ErlNifBinary bin;

    dlen = EC_POINT_point2oct(group, point, form, NULL, 0, NULL);
    if (dlen == 0)
	return atom_undefined;

    if (!enif_alloc_binary(dlen, &bin))
	return enif_make_badarg(env);

    if (!EC_POINT_point2oct(group, point, form, bin.data, bin.size, NULL)) {
	enif_release_binary(&bin);
	return enif_make_badarg(env);
    }
    ERL_VALGRIND_MAKE_MEM_DEFINED(bin.data, bin.size);
    return enif_make_binary(env, &bin);
}

static int term2point(ErlNifEnv* env, ERL_NIF_TERM term,
		      EC_GROUP *group, EC_POINT **pptr)
{
    int ret = 0;
    ErlNifBinary bin;
    EC_POINT *point;

    if (!enif_inspect_binary(env,term,&bin)) {
        return 0;
    }

    if ((*pptr = point = EC_POINT_new(group)) == NULL) {
	return 0;
    }

    /* set the point conversion form */
    EC_GROUP_set_point_conversion_form(group, (point_conversion_form_t)(bin.data[0] & ~0x01));

    /* extract the ec point */
    if (!EC_POINT_oct2point(group, point, bin.data, bin.size, NULL)) {
	EC_POINT_free(point);
	*pptr = NULL;
    } else
	ret = 1;

    return ret;
}

static int get_ec_key(ErlNifEnv* env,
		      ERL_NIF_TERM curve, ERL_NIF_TERM priv, ERL_NIF_TERM pub,
		      EC_KEY** res)
{
    EC_KEY *key = NULL;
    BIGNUM *priv_key = NULL;
    EC_POINT *pub_key = NULL;
    EC_GROUP *group = NULL;

    if (!(priv == atom_undefined || get_bn_from_bin(env, priv, &priv_key))
	|| !(pub == atom_undefined || enif_is_binary(env, pub))) {
	goto out_err;
    }

    key = ec_key_new(env, curve);

    if (!key) {
	goto out_err;
    }

    if (!group)
	group = EC_GROUP_dup(EC_KEY_get0_group(key));

    if (term2point(env, pub, group, &pub_key)) {
	    if (!EC_KEY_set_public_key(key, pub_key)) {
		    goto out_err;
	    }
    }
    if (priv != atom_undefined
	&& !BN_is_zero(priv_key)) {
	    if (!EC_KEY_set_private_key(key, priv_key))
		    goto out_err;

	    /* calculate public key (if necessary) */
	    if (EC_KEY_get0_public_key(key) == NULL)
	    {
		    /* the public key was not included in the SEC1 private
		     * key => calculate the public key */
		    pub_key = EC_POINT_new(group);
		    if (pub_key == NULL
			|| !EC_POINT_copy(pub_key, EC_GROUP_get0_generator(group))
			|| !EC_POINT_mul(group, pub_key, priv_key, NULL, NULL, NULL)
			|| !EC_KEY_set_public_key(key, pub_key))
			    goto out_err;
	    }
    }

    goto out;

out_err:
    if (key) EC_KEY_free(key);
    key = NULL;

out:
    /* some OpenSSL structures are mem-dup'ed into the key,
       so we have to free our copies here */
    if (priv_key) BN_clear_free(priv_key);
    if (pub_key) EC_POINT_free(pub_key);
    if (group) EC_GROUP_free(group);
    if (!key)
	return 0;
    *res = key;
    return 1;
}
#endif /* HAVE_EC */

static ERL_NIF_TERM ec_key_generate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#if defined(HAVE_EC)
    EC_KEY *key = NULL;
    const EC_GROUP *group;
    const EC_POINT *public_key;
    ERL_NIF_TERM priv_key;
    ERL_NIF_TERM pub_key = atom_undefined;

    if (!get_ec_key(env, argv[0], argv[1], atom_undefined, &key))
	goto badarg;

    if (argv[1] == atom_undefined) {
	if (!EC_KEY_generate_key(key))
	    goto badarg;
    }

    group = EC_KEY_get0_group(key);
    public_key = EC_KEY_get0_public_key(key);

    if (group && public_key) {
	pub_key = point2term(env, group, public_key,
			     EC_KEY_get_conv_form(key));
    }
    priv_key = bn2term(env, EC_KEY_get0_private_key(key));
    EC_KEY_free(key);
    return enif_make_tuple2(env, pub_key, priv_key);

badarg:
    if (key)
	EC_KEY_free(key);
    return make_badarg_maybe(env);
#else
    return atom_notsup;
#endif
}

/*
  (_OthersPublicKey, _MyPrivateKey)
  (_OthersPublicKey, _MyEC_Point)
*/
static ERL_NIF_TERM ecdh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
/* (OtherPublicKey, Curve, My) */
{
#if defined(HAVE_EC)
    ERL_NIF_TERM ret;
    unsigned char *p;
    EC_KEY* key = NULL;
    int field_size = 0;
    int i;
    EC_GROUP *group;
    const BIGNUM *priv_key;
    EC_POINT *my_ecpoint;
    EC_KEY *other_ecdh = NULL;

    if (!get_ec_key(env, argv[1], argv[2], atom_undefined, &key))
	return make_badarg_maybe(env);

    group    = EC_GROUP_dup(EC_KEY_get0_group(key));
    priv_key = EC_KEY_get0_private_key(key);

    if (!term2point(env, argv[0], group, &my_ecpoint)) {
	goto out_err;
    }

    if ((other_ecdh = EC_KEY_new()) == NULL
	|| !EC_KEY_set_group(other_ecdh, group)
	|| !EC_KEY_set_private_key(other_ecdh, priv_key))
	goto out_err;

    field_size = EC_GROUP_get_degree(group);
    if (field_size <= 0)
	goto out_err;

    p = enif_make_new_binary(env, (field_size+7)/8, &ret);
    i = ECDH_compute_key(p, (field_size+7)/8, my_ecpoint, other_ecdh, NULL);

    if (i < 0)
	    goto out_err;
out:
    if (group) EC_GROUP_free(group);
    if (my_ecpoint) EC_POINT_free(my_ecpoint);
    if (other_ecdh) EC_KEY_free(other_ecdh);
    if (key) EC_KEY_free(key);

    return ret;

out_err:
    ret = enif_make_badarg(env);
    goto out;
#else
    return atom_notsup;
#endif
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
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
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


#ifdef HAS_ENGINE_SUPPORT
static int get_engine_and_key_id(ErlNifEnv *env, ERL_NIF_TERM key, char ** id, ENGINE **e)
{
    ERL_NIF_TERM engine_res, key_id_term;
    struct engine_ctx *ctx;
    ErlNifBinary key_id_bin;

    if (!enif_get_map_value(env, key, atom_engine, &engine_res) ||
        !enif_get_resource(env, engine_res, engine_ctx_rtype, (void**)&ctx) ||
        !enif_get_map_value(env, key, atom_key_id, &key_id_term) ||
        !enif_inspect_binary(env, key_id_term, &key_id_bin)) {
        return 0;
    }
    else {
        *e = ctx->engine;
        return zero_terminate(key_id_bin, id);
    }
}


static char *get_key_password(ErlNifEnv *env, ERL_NIF_TERM key) {
    ERL_NIF_TERM tmp_term;
    ErlNifBinary pwd_bin;
    char *pwd;
    if (enif_get_map_value(env, key, atom_password, &tmp_term) &&
        enif_inspect_binary(env, tmp_term, &pwd_bin) &&
        zero_terminate(pwd_bin, &pwd)
        ) return pwd;

    return NULL;
}

static int zero_terminate(ErlNifBinary bin, char **buf) {
    *buf = enif_alloc(bin.size+1);
    if (!*buf)
        return 0;
    memcpy(*buf, bin.data, bin.size);
    *(*buf+bin.size) = 0;
    return 1;
}
#endif

static int get_pkey_private_key(ErlNifEnv *env, ERL_NIF_TERM algorithm, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    if (enif_is_map(env, key)) {
#ifdef HAS_ENGINE_SUPPORT
        /* Use key stored in engine */
        ENGINE *e;
        char *id;
        char *password;

        if (!get_engine_and_key_id(env, key, &id, &e))
            return PKEY_BADARG;
        password = get_key_password(env, key);
        *pkey = ENGINE_load_private_key(e, id, NULL, password);
        if (!*pkey)
            return PKEY_BADARG;
        enif_free(id);
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
        char *id;
        char *password;

        if (!get_engine_and_key_id(env, key, &id, &e))
            return PKEY_BADARG;
        password = get_key_password(env, key);
        *pkey = ENGINE_load_public_key(e, id, NULL, password);
        if (!pkey)
            return PKEY_BADARG;
        enif_free(id);
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

    if (EVP_PKEY_sign_init(ctx) <= 0) goto badarg;
    if (md != NULL && EVP_PKEY_CTX_set_signature_md(ctx, md) <= 0) goto badarg;

    if (argv[0] == atom_rsa) {
	if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) <= 0) goto badarg;
	if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
            if (sig_opt.rsa_mgf1_md != NULL) {
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,1)
		if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) <= 0) goto badarg;
#else
                EVP_PKEY_CTX_free(ctx);
                EVP_PKEY_free(pkey);
                return atom_notsup;
#endif
            }
	    if (sig_opt.rsa_pss_saltlen > -2
		&& EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) <= 0)
		goto badarg;
	}
    }

    if (EVP_PKEY_sign(ctx, NULL, &siglen, tbs, tbslen) <= 0) goto badarg;
    enif_alloc_binary(siglen, &sig_bin);

    if (md != NULL) {
	ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
    }
    i = EVP_PKEY_sign(ctx, sig_bin.data, &siglen, tbs, tbslen);

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
    if (EVP_PKEY_verify_init(ctx) <= 0) goto badarg;
    if (md != NULL && EVP_PKEY_CTX_set_signature_md(ctx, md) <= 0) goto badarg;

    if (argv[0] == atom_rsa) {
	if (EVP_PKEY_CTX_set_rsa_padding(ctx, sig_opt.rsa_padding) <= 0) goto badarg;
	if (sig_opt.rsa_padding == RSA_PKCS1_PSS_PADDING) {
            if (sig_opt.rsa_mgf1_md != NULL) {
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,1)
		if (EVP_PKEY_CTX_set_rsa_mgf1_md(ctx, sig_opt.rsa_mgf1_md) <= 0) goto badarg;
#else
                EVP_PKEY_CTX_free(ctx);
                EVP_PKEY_free(pkey);
                return atom_notsup;
#endif
            }
	    if (sig_opt.rsa_pss_saltlen > -2
		&& EVP_PKEY_CTX_set_rsa_pss_saltlen(ctx, sig_opt.rsa_pss_saltlen) <= 0)
		goto badarg;
	}
    }

    if (md != NULL) {
	ERL_VALGRIND_ASSERT_MEM_DEFINED(tbs, EVP_MD_size(md));
    }
    i = EVP_PKEY_verify(ctx, sig_bin.data, sig_bin.size, tbs, tbslen);

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
		    } else if (tpl_terms[1] == atom_rsa_pkcs1_oaep_padding) {
			opt->rsa_padding = RSA_PKCS1_OAEP_PADDING;
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
#ifndef HAVE_RSA_OAEP_MD
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
		RSA *rsa = EVP_PKEY_get1_RSA(pkey);
		if (rsa == NULL) goto badarg;
		tmplen = RSA_size(rsa);
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
		unsigned char *label_copy;
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
	    RSA *rsa = EVP_PKEY_get1_RSA(pkey);
	    unsigned char *p;
	    if (rsa == NULL) goto badarg;
	    tmplen = RSA_size(rsa);
	    if (!enif_alloc_binary(tmplen, &tmp_bin)) goto badarg;
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

/*================================================================*/

static ERL_NIF_TERM rand_seed_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary seed_bin;

    if (!enif_inspect_binary(env, argv[0], &seed_bin))
        return enif_make_badarg(env);
    RAND_seed(seed_bin.data,seed_bin.size);
    return atom_ok;
}

/*================================================================*/
/* Engine */
/*================================================================*/
static ERL_NIF_TERM engine_by_id_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (EngineId) */
#ifdef HAS_ENGINE_SUPPORT
    ERL_NIF_TERM ret;
    ErlNifBinary engine_id_bin;
    char *engine_id;
    ENGINE *engine;
    struct engine_ctx *ctx;

    // Get Engine Id
    if(!enif_inspect_binary(env, argv[0], &engine_id_bin)) {
        PRINTF_ERR0("engine_by_id_nif Leaved: badarg");
        return enif_make_badarg(env);
    } else {
        engine_id = enif_alloc(engine_id_bin.size+1);
        (void) memcpy(engine_id, engine_id_bin.data, engine_id_bin.size);
        engine_id[engine_id_bin.size] = '\0';
    }

    engine = ENGINE_by_id(engine_id);
    if(!engine) {
        enif_free(engine_id);
        PRINTF_ERR0("engine_by_id_nif Leaved: {error, bad_engine_id}");
        return enif_make_tuple2(env, atom_error, atom_bad_engine_id);
    }

    ctx = enif_alloc_resource(engine_ctx_rtype, sizeof(struct engine_ctx));
    ctx->engine = engine;
    ctx->id = engine_id;

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, atom_ok, ret);
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine) */
#ifdef HAS_ENGINE_SUPPORT
    ERL_NIF_TERM ret = atom_ok;
    struct engine_ctx *ctx;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_init_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }
    if (!ENGINE_init(ctx->engine)) {
        //ERR_print_errors_fp(stderr);
        PRINTF_ERR0("engine_init_nif Leaved: {error, engine_init_failed}");
        return enif_make_tuple2(env, atom_error, atom_engine_init_failed);
    }

    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_free_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine) */
#ifdef HAS_ENGINE_SUPPORT
    struct engine_ctx *ctx;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_free_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }

    ENGINE_free(ctx->engine);
    return atom_ok;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_finish_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine) */
#ifdef HAS_ENGINE_SUPPORT
    struct engine_ctx *ctx;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_finish_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }

    ENGINE_finish(ctx->engine);
    return atom_ok;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_load_dynamic_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */
#ifdef HAS_ENGINE_SUPPORT
    ENGINE_load_dynamic();
    return atom_ok;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_ctrl_cmd_strings_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine, Commands) */
#ifdef HAS_ENGINE_SUPPORT
    ERL_NIF_TERM ret = atom_ok;
    unsigned int cmds_len = 0;
    char **cmds = NULL;
    struct engine_ctx *ctx;
    int i, optional = 0;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_ctrl_cmd_strings_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }

    PRINTF_ERR1("Engine Id:  %s\r\n", ENGINE_get_id(ctx->engine));

    // Get Command List
    if(!enif_get_list_length(env, argv[1], &cmds_len)) {
        PRINTF_ERR0("engine_ctrl_cmd_strings_nif Leaved: Bad Command List");
        return enif_make_badarg(env);
    } else {
        cmds_len *= 2; // Key-Value list from erlang
        cmds = enif_alloc((cmds_len+1)*sizeof(char*));
        if(get_engine_load_cmd_list(env, argv[1], cmds, 0)) {
            PRINTF_ERR0("engine_ctrl_cmd_strings_nif Leaved: Couldn't read Command List");
            ret = enif_make_badarg(env);
            goto error;
        }
    }

    if(!enif_get_int(env, argv[2], &optional)) {
        PRINTF_ERR0("engine_ctrl_cmd_strings_nif Leaved: Parameter optional not an integer");
        return enif_make_badarg(env);
    }

    for(i = 0; i < cmds_len; i+=2) {
        PRINTF_ERR2("Cmd:  %s:%s\r\n",
                   cmds[i] ? cmds[i] : "(NULL)",
                   cmds[i+1] ? cmds[i+1] : "(NULL)");
        if(!ENGINE_ctrl_cmd_string(ctx->engine, cmds[i], cmds[i+1], optional)) {
            PRINTF_ERR2("Command failed:  %s:%s\r\n",
                        cmds[i] ? cmds[i] : "(NULL)",
                        cmds[i+1] ? cmds[i+1] : "(NULL)");
            //ENGINE_free(ctx->engine);
            ret = enif_make_tuple2(env, atom_error, atom_ctrl_cmd_failed);
            PRINTF_ERR0("engine_ctrl_cmd_strings_nif Leaved: {error, ctrl_cmd_failed}");
            goto error;
        }
    }

 error:
    for(i = 0; cmds != NULL && cmds[i] != NULL; i++)
        enif_free(cmds[i]);    
    enif_free(cmds);
    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_add_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine) */
#ifdef HAS_ENGINE_SUPPORT
    struct engine_ctx *ctx;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_add_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }

    if (!ENGINE_add(ctx->engine)) {
        PRINTF_ERR0("engine_add_nif Leaved: {error, add_engine_failed}");
        return enif_make_tuple2(env, atom_error, atom_add_engine_failed);
    }
    return atom_ok;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_remove_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine) */
#ifdef HAS_ENGINE_SUPPORT
    struct engine_ctx *ctx;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_remove_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }

    if (!ENGINE_remove(ctx->engine)) {
        PRINTF_ERR0("engine_remove_nif Leaved: {error, remove_engine_failed}");
        return enif_make_tuple2(env, atom_error, atom_remove_engine_failed);
    }
    return atom_ok;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_register_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine, EngineMethod) */
#ifdef HAS_ENGINE_SUPPORT
    struct engine_ctx *ctx;
    unsigned int method;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_register_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }
    // Get Method
    if (!enif_get_uint(env, argv[1], &method)) {
        PRINTF_ERR0("engine_register_nif Leaved: Parameter Method not an uint");
        return enif_make_badarg(env);
    }

    switch(method)
    {
#ifdef ENGINE_METHOD_RSA
    case ENGINE_METHOD_RSA:
        if (!ENGINE_register_RSA(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_DSA
    case ENGINE_METHOD_DSA:
        if (!ENGINE_register_DSA(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_DH
    case ENGINE_METHOD_DH:
        if (!ENGINE_register_DH(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_RAND
    case ENGINE_METHOD_RAND:
        if (!ENGINE_register_RAND(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_ECDH
    case ENGINE_METHOD_ECDH:
        if (!ENGINE_register_ECDH(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_ECDSA
    case ENGINE_METHOD_ECDSA:
        if (!ENGINE_register_ECDSA(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_STORE
    case ENGINE_METHOD_STORE:
        if (!ENGINE_register_STORE(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_CIPHERS
    case ENGINE_METHOD_CIPHERS:
        if (!ENGINE_register_ciphers(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_DIGESTS
    case ENGINE_METHOD_DIGESTS:
        if (!ENGINE_register_digests(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_PKEY_METHS
    case ENGINE_METHOD_PKEY_METHS:
        if (!ENGINE_register_pkey_meths(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_PKEY_ASN1_METHS
    case ENGINE_METHOD_PKEY_ASN1_METHS:
        if (!ENGINE_register_pkey_asn1_meths(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
#ifdef ENGINE_METHOD_EC
    case ENGINE_METHOD_EC:
        if (!ENGINE_register_EC(ctx->engine))
            return enif_make_tuple2(env, atom_error, atom_register_engine_failed);
        break;
#endif
    default:
        return  enif_make_tuple2(env, atom_error, atom_engine_method_not_supported);
        break;
    }
    return atom_ok;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_unregister_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine, EngineMethod) */
#ifdef HAS_ENGINE_SUPPORT
    struct engine_ctx *ctx;
    unsigned int method;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_unregister_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }
    // Get Method
    if (!enif_get_uint(env, argv[1], &method)) {
        PRINTF_ERR0("engine_unregister_nif Leaved: Parameter Method not an uint");
        return enif_make_badarg(env);
    }

    switch(method)
    {
#ifdef ENGINE_METHOD_RSA
    case ENGINE_METHOD_RSA:
        ENGINE_unregister_RSA(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_DSA
    case ENGINE_METHOD_DSA:
        ENGINE_unregister_DSA(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_DH
    case ENGINE_METHOD_DH:
        ENGINE_unregister_DH(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_RAND
    case ENGINE_METHOD_RAND:
        ENGINE_unregister_RAND(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_ECDH
    case ENGINE_METHOD_ECDH:
        ENGINE_unregister_ECDH(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_ECDSA
    case ENGINE_METHOD_ECDSA:
        ENGINE_unregister_ECDSA(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_STORE
    case ENGINE_METHOD_STORE:
        ENGINE_unregister_STORE(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_CIPHERS
    case ENGINE_METHOD_CIPHERS:
        ENGINE_unregister_ciphers(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_DIGESTS
    case ENGINE_METHOD_DIGESTS:
        ENGINE_unregister_digests(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_PKEY_METHS
    case ENGINE_METHOD_PKEY_METHS:
        ENGINE_unregister_pkey_meths(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_PKEY_ASN1_METHS
    case ENGINE_METHOD_PKEY_ASN1_METHS:
        ENGINE_unregister_pkey_asn1_meths(ctx->engine);
        break;
#endif
#ifdef ENGINE_METHOD_EC
    case ENGINE_METHOD_EC:
        ENGINE_unregister_EC(ctx->engine);
        break;
#endif
    default:
        break;
    }
    return atom_ok;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_get_first_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine) */
#ifdef HAS_ENGINE_SUPPORT
    ERL_NIF_TERM ret;
    ENGINE *engine;
    ErlNifBinary engine_bin;
    struct engine_ctx *ctx;

    engine = ENGINE_get_first();
    if(!engine) {
        enif_alloc_binary(0, &engine_bin);
        engine_bin.size = 0;
        return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &engine_bin));
    }

    ctx = enif_alloc_resource(engine_ctx_rtype, sizeof(struct engine_ctx));
    ctx->engine = engine;
    ctx->id = NULL;

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, atom_ok, ret);
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_get_next_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine) */
#ifdef HAS_ENGINE_SUPPORT
    ERL_NIF_TERM ret;
    ENGINE *engine;
    ErlNifBinary engine_bin;
    struct engine_ctx *ctx, *next_ctx;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_get_next_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }
    engine = ENGINE_get_next(ctx->engine);
    if (!engine) {
        enif_alloc_binary(0, &engine_bin);
        engine_bin.size = 0;
        return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &engine_bin));
    }

    next_ctx = enif_alloc_resource(engine_ctx_rtype, sizeof(struct engine_ctx));
    next_ctx->engine = engine;
    next_ctx->id = NULL;

    ret = enif_make_resource(env, next_ctx);
    enif_release_resource(next_ctx);

    return enif_make_tuple2(env, atom_ok, ret);
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_get_id_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Engine) */
#ifdef HAS_ENGINE_SUPPORT
    ErlNifBinary engine_id_bin;
    const char *engine_id;
    int size;
    struct engine_ctx *ctx;

    // Get Engine
    if (!enif_get_resource(env, argv[0], engine_ctx_rtype, (void**)&ctx)) {
        PRINTF_ERR0("engine_get_id_nif Leaved: Parameter not an engine resource object");
        return enif_make_badarg(env);
    }

    engine_id = ENGINE_get_id(ctx->engine);
    if (!engine_id) {
        enif_alloc_binary(0, &engine_id_bin);
        engine_id_bin.size = 0;
        return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &engine_id_bin));
    }

    size = strlen(engine_id);
    enif_alloc_binary(size, &engine_id_bin);
    engine_id_bin.size = size;
    memcpy(engine_id_bin.data, engine_id, size);

    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &engine_id_bin));
#else
    return atom_notsup;
#endif
}

static int get_engine_load_cmd_list(ErlNifEnv* env, const ERL_NIF_TERM term, char **cmds, int i)
{
#ifdef HAS_ENGINE_SUPPORT
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM *tmp_tuple;
    ErlNifBinary tmpbin;
    int arity;
    char* tmpstr;

    if(!enif_is_empty_list(env, term)) {
        if(!enif_get_list_cell(env, term, &head, &tail)) {
            cmds[i] = NULL;
            return -1;
        } else {
            if(!enif_get_tuple(env, head, &arity, &tmp_tuple)  || arity != 2) {
                cmds[i] = NULL;
                return -1;
            } else {
                if(!enif_inspect_binary(env, tmp_tuple[0], &tmpbin)) {
                    cmds[i] = NULL;
                    return -1;
                } else {
                    tmpstr = enif_alloc(tmpbin.size+1);
                    (void) memcpy(tmpstr, tmpbin.data, tmpbin.size);
                    tmpstr[tmpbin.size] = '\0';
                    cmds[i++] = tmpstr;
                }
                if(!enif_inspect_binary(env, tmp_tuple[1], &tmpbin)) {
                    cmds[i] = NULL;
                    return -1;
                } else {
                    if(tmpbin.size == 0)
                        cmds[i++] = NULL;
                    else {
                        tmpstr = enif_alloc(tmpbin.size+1);
                        (void) memcpy(tmpstr, tmpbin.data, tmpbin.size);
                        tmpstr[tmpbin.size] = '\0';
                        cmds[i++] = tmpstr;
                    }
                }
                return get_engine_load_cmd_list(env, tail, cmds, i);
            }
        }
    } else {
        cmds[i] = NULL;
        return 0;
    }
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM engine_get_all_methods_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */
#ifdef HAS_ENGINE_SUPPORT
    ERL_NIF_TERM method_array[12];
    int i = 0;

#ifdef ENGINE_METHOD_RSA
    method_array[i++] = atom_engine_method_rsa;
#endif
#ifdef ENGINE_METHOD_DSA
    method_array[i++] = atom_engine_method_dsa;
#endif
#ifdef ENGINE_METHOD_DH
    method_array[i++] = atom_engine_method_dh;
#endif
#ifdef ENGINE_METHOD_RAND
    method_array[i++] = atom_engine_method_rand;
#endif
#ifdef ENGINE_METHOD_ECDH
    method_array[i++] = atom_engine_method_ecdh;
#endif
#ifdef ENGINE_METHOD_ECDSA
    method_array[i++] = atom_engine_method_ecdsa;
#endif
#ifdef ENGINE_METHOD_STORE
    method_array[i++] = atom_engine_method_store;
#endif
#ifdef ENGINE_METHOD_CIPHERS
    method_array[i++] = atom_engine_method_ciphers;
#endif
#ifdef ENGINE_METHOD_DIGESTS
    method_array[i++] = atom_engine_method_digests;
#endif
#ifdef ENGINE_METHOD_PKEY_METHS
    method_array[i++] = atom_engine_method_pkey_meths;
#endif
#ifdef ENGINE_METHOD_PKEY_ASN1_METHS
    method_array[i++] = atom_engine_method_pkey_asn1_meths;
#endif
#ifdef ENGINE_METHOD_EC
    method_array[i++] = atom_engine_method_ec;
#endif

    return enif_make_list_from_array(env, method_array, i);
#else
    return atom_notsup;
#endif
}
