/* 
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

#include "erl_nif.h"

#define OPENSSL_THREAD_DEFINES
#include <openssl/opensslconf.h>

#include <openssl/crypto.h>
#include <openssl/des.h>
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
#include <openssl/rc4.h>
#include <openssl/rc2.h>
#include <openssl/blowfish.h>
#include <openssl/rand.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>


/* Helper macro to construct a OPENSSL_VERSION_NUMBER.
 * See openssl/opensslv.h
 */
#define OpenSSL_version(MAJ, MIN, FIX, P)	\
    ((((((((MAJ << 8) | MIN) << 8 ) | FIX) << 8) | (P-'a'+1)) << 4) | 0xf)

#define OpenSSL_version_plain(MAJ, MIN, FIX) \
    OpenSSL_version(MAJ,MIN,FIX,('a'-1))


#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)
#include <openssl/modes.h>
#endif

#include "crypto_callback.h"

#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(0,9,8)	\
    && !defined(OPENSSL_NO_SHA224) && defined(NID_sha224) \
    && !defined(OPENSSL_NO_SHA256) /* disabled like this in my sha.h (?) */
# define HAVE_SHA224
#endif
#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(0,9,8)	\
    && !defined(OPENSSL_NO_SHA256) && defined(NID_sha256)
# define HAVE_SHA256
#endif
#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(0,9,8)	\
    && !defined(OPENSSL_NO_SHA384) && defined(NID_sha384)\
    && !defined(OPENSSL_NO_SHA512) /* disabled like this in my sha.h (?) */
# define HAVE_SHA384
#endif
#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(0,9,8)	\
    && !defined(OPENSSL_NO_SHA512) && defined(NID_sha512)
# define HAVE_SHA512
#endif
#if OPENSSL_VERSION_NUMBER >= OpenSSL_version(0,9,7,'e')
# define HAVE_DES_ede3_cfb_encrypt
#endif

#if OPENSSL_VERSION_NUMBER >= OpenSSL_version(0,9,8,'o') \
	&& !defined(OPENSSL_NO_EC) \
	&& !defined(OPENSSL_NO_ECDH) \
	&& !defined(OPENSSL_NO_ECDSA)
# define HAVE_EC
#endif

#if OPENSSL_VERSION_NUMBER >= OpenSSL_version(0,9,8,'c')
# define HAVE_AES_IGE
#endif

#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,1)
# define HAVE_EVP_AES_CTR
# define HAVE_GCM
# if OPENSSL_VERSION_NUMBER < OpenSSL_version(1,0,1,'d')
#  define HAVE_GCM_EVP_DECRYPT_BUG
# endif
#endif

#if defined(NID_chacha20) && !defined(OPENSSL_NO_CHACHA) && !defined(OPENSSL_NO_POLY1305)
# define HAVE_CHACHA20_POLY1305
#endif

#if OPENSSL_VERSION_NUMBER <= OpenSSL_version(0,9,8,'l')
# define HAVE_ECB_IVEC_BUG
#endif

#if defined(HAVE_EC)
#include <openssl/ec.h>
#include <openssl/ecdh.h>
#include <openssl/ecdsa.h>
#endif

#if defined(HAVE_CHACHA20_POLY1305)
#include <openssl/chacha.h>
#include <openssl/poly1305.h>

#if !defined(CHACHA20_NONCE_LEN)
# define CHACHA20_NONCE_LEN 8
#endif
#if !defined(POLY1305_TAG_LEN)
# define POLY1305_TAG_LEN 16
#endif

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

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM info_lib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hash_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hash_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hash_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hash_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM block_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_cfb_8_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ige_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ctr_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ctr_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rand_bytes_1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM strong_rand_bytes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM strong_rand_mpint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rand_uniform_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mod_exp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dss_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM do_exor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc4_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc4_set_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc4_encrypt_with_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dss_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_public_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_private_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_generate_parameters_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_check(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM srp_value_B_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM srp_user_secret_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM srp_host_secret_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM ec_key_generate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ecdsa_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ecdsa_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ecdh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM rand_seed_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM aes_gcm_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_gcm_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#ifdef HAVE_GCM_EVP_DECRYPT_BUG
static ERL_NIF_TERM aes_gcm_decrypt_NO_EVP(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#endif

static ERL_NIF_TERM chacha20_poly1305_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM chacha20_poly1305_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* helpers */
static void init_algorithms_types(ErlNifEnv*);
static void init_digest_types(ErlNifEnv* env);
static void init_cipher_types(ErlNifEnv* env);
#ifdef HAVE_EC
static EC_KEY* ec_key_new(ErlNifEnv* env, ERL_NIF_TERM curve_arg);
static int term2point(ErlNifEnv* env, ERL_NIF_TERM term,
		      EC_GROUP *group, EC_POINT **pptr);
#endif

static int library_refc = 0; /* number of users of this dynamic library */

static ErlNifFunc nif_funcs[] = {
    {"info_lib", 0, info_lib},
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
    {"block_crypt_nif", 5, block_crypt_nif},
    {"block_crypt_nif", 4, block_crypt_nif},
    {"aes_ige_crypt_nif", 4, aes_ige_crypt_nif},

    {"aes_ctr_encrypt", 3, aes_ctr_encrypt},
    {"aes_ctr_decrypt", 3, aes_ctr_encrypt},
    {"aes_ctr_stream_init", 2, aes_ctr_stream_init},
    {"aes_ctr_stream_encrypt", 2, aes_ctr_stream_encrypt},
    {"aes_ctr_stream_decrypt", 2, aes_ctr_stream_encrypt},
    {"rand_bytes", 1, rand_bytes_1},
    {"strong_rand_bytes_nif", 1, strong_rand_bytes_nif},
    {"strong_rand_mpint_nif", 3, strong_rand_mpint_nif},
    {"rand_uniform_nif", 2, rand_uniform_nif},
    {"mod_exp_nif", 4, mod_exp_nif},
    {"dss_verify_nif", 4, dss_verify_nif},
    {"rsa_verify_nif", 4, rsa_verify_nif},
    {"do_exor", 2, do_exor},
    {"rc4_encrypt", 2, rc4_encrypt},
    {"rc4_set_key", 1, rc4_set_key},
    {"rc4_encrypt_with_state", 2, rc4_encrypt_with_state},
    {"rsa_sign_nif", 3, rsa_sign_nif},
    {"dss_sign_nif", 3, dss_sign_nif},
    {"rsa_public_crypt", 4, rsa_public_crypt},
    {"rsa_private_crypt", 4, rsa_private_crypt},
    {"dh_generate_parameters_nif", 2, dh_generate_parameters_nif},
    {"dh_check", 1, dh_check},
    {"dh_generate_key_nif", 3, dh_generate_key_nif},
    {"dh_compute_key_nif", 3, dh_compute_key_nif},
    {"srp_value_B_nif", 5, srp_value_B_nif},
    {"srp_user_secret_nif", 7, srp_user_secret_nif},
    {"srp_host_secret_nif", 5, srp_host_secret_nif},

    {"ec_key_generate", 2, ec_key_generate},
    {"ecdsa_sign_nif", 4, ecdsa_sign_nif},
    {"ecdsa_verify_nif", 5, ecdsa_verify_nif},
    {"ecdh_compute_key_nif", 3, ecdh_compute_key_nif},

    {"rand_seed_nif", 1, rand_seed_nif},

    {"aes_gcm_encrypt", 5, aes_gcm_encrypt},
    {"aes_gcm_decrypt", 5, aes_gcm_decrypt},

    {"chacha20_poly1305_encrypt", 4, chacha20_poly1305_encrypt},
    {"chacha20_poly1305_decrypt", 5, chacha20_poly1305_decrypt}


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

static ErlNifResourceType* hmac_context_rtype;
struct hmac_context
{
    ErlNifMutex* mtx;
    int alive;
    HMAC_CTX ctx;
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

struct digest_type_t digest_types[] =
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

struct cipher_type_t cipher_types[] =
{
    {{"rc2_cbc"}, {&EVP_rc2_cbc}},
    {{"des_cbc"}, {&EVP_des_cbc}},
    {{"des_cfb"}, {&EVP_des_cfb8}},
    {{"des_ecb"}, {&EVP_des_ecb}},
    {{"des_ede3_cbc"}, {&EVP_des_ede3_cbc}},
    {{"des_ede3_cbf"},
#ifdef HAVE_DES_ede3_cfb_encrypt
     {&EVP_des_ede3_cfb8}
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

#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)
/* Define resource types for OpenSSL context structures. */
static ErlNifResourceType* evp_md_ctx_rtype;
static void evp_md_ctx_dtor(ErlNifEnv* env, EVP_MD_CTX* ctx) {
    EVP_MD_CTX_cleanup(ctx);
}
#endif

#ifdef HAVE_EVP_AES_CTR
static ErlNifResourceType* evp_cipher_ctx_rtype;
static void evp_cipher_ctx_dtor(ErlNifEnv* env, EVP_CIPHER_CTX* ctx) {
    EVP_CIPHER_CTX_cleanup(ctx);
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

static int init(ErlNifEnv* env, ERL_NIF_TERM load_info)
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
	return 0;

    /* load_info: {301, <<"/full/path/of/this/library">>} */
    if (!enif_get_tuple(env, load_info, &tpl_arity, &tpl_array)
	|| tpl_arity != 2
	|| !enif_get_int(env, tpl_array[0], &vernum)
	|| vernum != 301
	|| !enif_inspect_binary(env, tpl_array[1], &lib_bin)) {

	PRINTF_ERR1("CRYPTO: Invalid load_info '%T'", load_info);
	return 0;
    }

    hmac_context_rtype = enif_open_resource_type(env, NULL, "hmac_context",
						 (ErlNifResourceDtor*) hmac_context_dtor,
						 ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
						 NULL);
    if (!hmac_context_rtype) {
	PRINTF_ERR0("CRYPTO: Could not open resource type 'hmac_context'");
	return 0;
    }
#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)
    evp_md_ctx_rtype = enif_open_resource_type(env, NULL, "EVP_MD_CTX",
                                               (ErlNifResourceDtor*) evp_md_ctx_dtor,
                                               ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                               NULL);
    if (!evp_md_ctx_rtype) {
        PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_MD_CTX'");
        return 0;
    }
#endif
#ifdef HAVE_EVP_AES_CTR
    evp_cipher_ctx_rtype = enif_open_resource_type(env, NULL, "EVP_CIPHER_CTX",
                                                   (ErlNifResourceDtor*) evp_cipher_ctx_dtor,
                                                   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                   NULL);
    if (!evp_cipher_ctx_rtype) {
        PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_CIPHER_CTX'");
        return 0;
    }
#endif
    if (library_refc > 0) {
	/* Repeated loading of this library (module upgrade).
	 * Atoms and callbacks are already set, we are done.
	 */
	return 1;
    }

    atom_true  = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    atom_sha = enif_make_atom(env,"sha");
    atom_error = enif_make_atom(env,"error");
    atom_rsa_pkcs1_padding = enif_make_atom(env,"rsa_pkcs1_padding");
    atom_rsa_pkcs1_oaep_padding = enif_make_atom(env,"rsa_pkcs1_oaep_padding");
    atom_rsa_no_padding = enif_make_atom(env,"rsa_no_padding");
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

    init_digest_types(env);
    init_cipher_types(env);
    init_algorithms_types(env);

#ifdef HAVE_DYNAMIC_CRYPTO_LIB
    {
	void* handle;
	if (!change_basename(&lib_bin, lib_buf, sizeof(lib_buf), crypto_callback_name)) {
	    return 0;
	}
	if (!(handle = enif_dlopen(lib_buf, &error_handler, NULL))) {
	    return 0;
	}
	if (!(funcp = (get_crypto_callbacks_t*) enif_dlsym(handle, "get_crypto_callbacks",
							   &error_handler, NULL))) {
	    return 0;
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
	return 0;
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
    return 1;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    if (!init(env, load_info)) {
	return -1;
    }

    *priv_data = NULL;
    library_refc++;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
	return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
	return -1; /* Don't know how to do that */
    }
    if (!init(env, load_info)) {
	return -1;
    }
    library_refc++;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    --library_refc;
}

static int algo_hash_cnt;
static ERL_NIF_TERM algo_hash[8];   /* increase when extending the list */
static int algo_pubkey_cnt;
static ERL_NIF_TERM algo_pubkey[7]; /* increase when extending the list */
static int algo_cipher_cnt;
static ERL_NIF_TERM algo_cipher[21]; /* increase when extending the list */

static void init_algorithms_types(ErlNifEnv* env)
{
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
    algo_pubkey[algo_pubkey_cnt++] = enif_make_atom(env, "srp");

    algo_cipher_cnt = 0;
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des3_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des_ede3");
#ifdef HAVE_DES_ede3_cfb_encrypt
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "des3_cbf");
#endif
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc128");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cfb8");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cfb128");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_cbc256");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ctr");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env, "aes_ecb");
#ifdef HAVE_AES_IGE
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"aes_ige256");
#endif
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"des_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"des_cfb");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_cfb64");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_ofb64");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"blowfish_ecb");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"rc2_cbc");
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"rc4");
#if defined(HAVE_GCM)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"aes_gcm");
#endif
#if defined(HAVE_CHACHA20_POLY1305)
    algo_cipher[algo_cipher_cnt++] = enif_make_atom(env,"chacha20_poly1305");
#endif

    ASSERT(algo_hash_cnt <= sizeof(algo_hash)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_pubkey_cnt <= sizeof(algo_pubkey)/sizeof(ERL_NIF_TERM));
    ASSERT(algo_cipher_cnt <= sizeof(algo_cipher)/sizeof(ERL_NIF_TERM));
}

static ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int hash_cnt   = algo_hash_cnt;
    int pubkey_cnt = algo_pubkey_cnt;
    int cipher_cnt = algo_cipher_cnt;
    return enif_make_tuple3(env,
			    enif_make_list_from_array(env, algo_hash,   hash_cnt),
			    enif_make_list_from_array(env, algo_pubkey, pubkey_cnt),
			    enif_make_list_from_array(env, algo_cipher, cipher_cnt));
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

static ERL_NIF_TERM make_badarg_maybe(ErlNifEnv* env)
{
    ERL_NIF_TERM reason;
    if (enif_has_pending_exception(env, &reason))
	return reason; /* dummy return value ignored */
    else
	return enif_make_badarg(env);
}

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

#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)

static ERL_NIF_TERM hash_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    struct digest_type_t *digp = NULL;
    EVP_MD_CTX           *ctx;
    ERL_NIF_TERM         ret;

    digp = get_digest_type(argv[0]);
    if (!digp) {
	return enif_make_badarg(env);
    }
    if (!digp->md.p) {
	return atom_notsup;
    }

    ctx = enif_alloc_resource(evp_md_ctx_rtype, sizeof(EVP_MD_CTX));
    if (!EVP_DigestInit(ctx, digp->md.p)) {
        enif_release_resource(ctx);
        return atom_notsup;
    }
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}
static ERL_NIF_TERM hash_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    EVP_MD_CTX   *ctx, *new_ctx;
    ErlNifBinary data;
    ERL_NIF_TERM ret;

    if (!enif_get_resource(env, argv[0], evp_md_ctx_rtype, (void**)&ctx) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
        return enif_make_badarg(env);
    }

    new_ctx = enif_alloc_resource(evp_md_ctx_rtype, sizeof(EVP_MD_CTX));
    if (!EVP_MD_CTX_copy(new_ctx, ctx) ||
        !EVP_DigestUpdate(new_ctx, data.data, data.size)) {
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
    EVP_MD_CTX    *ctx, new_ctx;
    ERL_NIF_TERM  ret;
    unsigned      ret_size;

    if (!enif_get_resource(env, argv[0], evp_md_ctx_rtype, (void**)&ctx)) {
        return enif_make_badarg(env);
    }

    ret_size = (unsigned)EVP_MD_CTX_size(ctx);
    ASSERT(0 < ret_size && ret_size <= EVP_MAX_MD_SIZE);

    if (!EVP_MD_CTX_copy(&new_ctx, ctx) ||
        !EVP_DigestFinal(&new_ctx,
                         enif_make_new_binary(env, ret_size, &ret),
                         &ret_size)) {
        return atom_notsup;
    }
    ASSERT(ret_size == (unsigned)EVP_MD_CTX_size(ctx));

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
	HMAC_CTX_cleanup(&obj->ctx);
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
#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)
    // Check the return value of HMAC_Init: it may fail in FIPS mode
    // for disabled algorithms
    if (!HMAC_Init(&obj->ctx, key.data, key.size, digp->md.p)) {
        enif_release_resource(obj);
        return atom_notsup;
    }
#else
    HMAC_Init(&obj->ctx, key.data, key.size, digp->md.p);
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
    HMAC_Update(&obj->ctx, data.data, data.size);
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
    
    HMAC_Final(&obj->ctx, mac_buf, &mac_len);
    HMAC_CTX_cleanup(&obj->ctx);
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

static ERL_NIF_TERM block_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key, Ivec, Text, IsEncrypt) or (Type, Key, Text, IsEncrypt) */
    struct cipher_type_t *cipherp = NULL;
    const EVP_CIPHER     *cipher;
    ErlNifBinary         key, ivec, text;
    EVP_CIPHER_CTX       ctx;
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

    if ((argv[0] == atom_aes_cfb8 || argv[0] == atom_aes_cfb128)
        && (key.size == 24 || key.size == 32)) {
        /* Why do EVP_CIPHER_CTX_set_key_length() fail on these key sizes?
         * Fall back on low level API
         */
        return aes_cfb_8_crypt(env, argc-1, argv+1);
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

    EVP_CIPHER_CTX_init(&ctx);
    if (!EVP_CipherInit_ex(&ctx, cipher, NULL, NULL, NULL,
                           (argv[argc - 1] == atom_true)) ||
        !EVP_CIPHER_CTX_set_key_length(&ctx, key.size) ||
        !(EVP_CIPHER_type(cipher) != NID_rc2_cbc ||
          EVP_CIPHER_CTX_ctrl(&ctx, EVP_CTRL_SET_RC2_KEY_BITS, key.size * 8, NULL)) ||
        !EVP_CipherInit_ex(&ctx, NULL, NULL,
                           key.data, ivec_size ? ivec.data : NULL, -1) ||
        !EVP_CIPHER_CTX_set_padding(&ctx, 0)) {

        EVP_CIPHER_CTX_cleanup(&ctx);
        return enif_raise_exception(env, atom_notsup);
    }

    if (text.size > 0 && /* OpenSSL 0.9.8h asserts text.size > 0 */
        (!EVP_CipherUpdate(&ctx, out, &out_size, text.data, text.size)
         || (ASSERT(out_size == text.size), 0)
         || !EVP_CipherFinal_ex(&ctx, out + out_size, &out_size))) {

        EVP_CIPHER_CTX_cleanup(&ctx);
        return enif_raise_exception(env, atom_notsup);
    }
    ASSERT(out_size == 0);
    EVP_CIPHER_CTX_cleanup(&ctx);
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

static ERL_NIF_TERM aes_ige_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */
#ifdef HAVE_AES_IGE
    ErlNifBinary key_bin, ivec_bin, data_bin;
    AES_KEY aes_key;
    unsigned char ivec[32];
    int i;
    unsigned char* ret_ptr;
    ERL_NIF_TERM ret;

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

/* Common for both encrypt and decrypt
*/
static ERL_NIF_TERM aes_ctr_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data) */    
    ErlNifBinary     key, ivec, text;
#ifdef HAVE_EVP_AES_CTR
    const EVP_CIPHER *cipher;
    EVP_CIPHER_CTX   ctx;
    unsigned char    *out;
    int              outl = 0;
#else
    AES_KEY          aes_key;
    unsigned char    ivec_clone[16]; /* writable copy */
    unsigned char    ecount_buf[AES_BLOCK_SIZE];
    unsigned int     num = 0;
#endif
    ERL_NIF_TERM     ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
#ifndef HAVE_EVP_AES_CTR
	|| AES_set_encrypt_key(key.data, key.size*8, &aes_key) != 0
#endif
	|| !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 16
	|| !enif_inspect_iolist_as_binary(env, argv[2], &text)) {
	return enif_make_badarg(env);
    }
#ifdef HAVE_EVP_AES_CTR
    switch (key.size)
    {
    case 16: cipher = EVP_aes_128_ctr(); break;
    case 24: cipher = EVP_aes_192_ctr(); break;
    case 32: cipher = EVP_aes_256_ctr(); break;
    default: return enif_make_badarg(env);
    }

    out = enif_make_new_binary(env,text.size,&ret);
    EVP_CIPHER_CTX_init(&ctx);
    EVP_CipherInit_ex(&ctx, cipher, NULL,
                      key.data, ivec.data, (argv[3] == atom_true));
    EVP_CIPHER_CTX_set_padding(&ctx, 0);
    EVP_CipherUpdate(&ctx, out, &outl, text.data, text.size);
    ASSERT(outl == text.size);
    EVP_CipherFinal_ex(&ctx, out + outl, &outl);
    ASSERT(outl == 0);
    EVP_CIPHER_CTX_cleanup(&ctx);
#else
    memcpy(ivec_clone, ivec.data, 16);    
    memset(ecount_buf, 0, sizeof(ecount_buf));
    AES_ctr128_encrypt((unsigned char *) text.data,
		       enif_make_new_binary(env, text.size, &ret), 
		       text.size, &aes_key, ivec_clone, ecount_buf, &num);
#endif
    CONSUME_REDS(env,text);

    /* To do an incremental {en|de}cryption, the state to to keep between calls
	must include ivec_clone, ecount_buf and num. */
    return ret;
}

/* Initializes state for ctr streaming (de)encryption
*/
#ifdef HAVE_EVP_AES_CTR
static ERL_NIF_TERM aes_ctr_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec) */
    ErlNifBinary     key_bin, ivec_bin;
    EVP_CIPHER_CTX   *ctx;
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

    ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(EVP_CIPHER_CTX));
    EVP_CIPHER_CTX_init(ctx);
    EVP_CipherInit_ex(ctx, cipher, NULL,
                      key_bin.data, ivec_bin.data, 1);
    EVP_CIPHER_CTX_set_padding(ctx, 0);
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}
static ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    EVP_CIPHER_CTX *ctx, *new_ctx;
    ErlNifBinary   data_bin;
    ERL_NIF_TERM   ret, cipher_term;
    unsigned char  *out;
    int            outl = 0;

    if (!enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx)
        || !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
        return enif_make_badarg(env);
    }
    new_ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(EVP_CIPHER_CTX));
    EVP_CIPHER_CTX_init(new_ctx);
    EVP_CIPHER_CTX_copy(new_ctx, ctx);
    out = enif_make_new_binary(env, data_bin.size, &cipher_term);
    EVP_CipherUpdate(new_ctx, out, &outl, data_bin.data, data_bin.size);
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
    EVP_CIPHER_CTX ctx;
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

    EVP_CIPHER_CTX_init(&ctx);

    if (EVP_EncryptInit_ex(&ctx, cipher, NULL, NULL, NULL) != 1)
        goto out_err;

    EVP_CIPHER_CTX_set_padding(&ctx, 0);

    if (EVP_CIPHER_CTX_ctrl(&ctx, EVP_CTRL_GCM_SET_IVLEN, iv.size, NULL) != 1)
        goto out_err;
    if (EVP_EncryptInit_ex(&ctx, NULL, NULL, key.data, iv.data) != 1)
        goto out_err;
    if (EVP_EncryptUpdate(&ctx, NULL, &len, aad.data, aad.size) != 1)
        goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    if (EVP_EncryptUpdate(&ctx, outp, &len, in.data, in.size) != 1)
        goto out_err;
    if (EVP_EncryptFinal_ex(&ctx, outp+len, &len) != 1)
        goto out_err;

    tagp = enif_make_new_binary(env, tag_len, &out_tag);

    if (EVP_CIPHER_CTX_ctrl(&ctx, EVP_CTRL_GCM_GET_TAG, tag_len, tagp) != 1)
        goto out_err;

    EVP_CIPHER_CTX_cleanup(&ctx);

    CONSUME_REDS(env, in);

    return enif_make_tuple2(env, out, out_tag);

out_err:
    EVP_CIPHER_CTX_cleanup(&ctx);
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
    EVP_CIPHER_CTX ctx;
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

    EVP_CIPHER_CTX_init(&ctx);

    if (EVP_DecryptInit_ex(&ctx, cipher, NULL, NULL, NULL) != 1)
        goto out_err;
    if (EVP_CIPHER_CTX_ctrl(&ctx, EVP_CTRL_GCM_SET_IVLEN, iv.size, NULL) != 1)
        goto out_err;
    if (EVP_DecryptInit_ex(&ctx, NULL, NULL, key.data, iv.data) != 1)
        goto out_err;
    if (EVP_DecryptUpdate(&ctx, NULL, &len, aad.data, aad.size) != 1)
        goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    if (EVP_DecryptUpdate(&ctx, outp, &len, in.data, in.size) != 1)
        goto out_err;
    if (EVP_CIPHER_CTX_ctrl(&ctx, EVP_CTRL_GCM_SET_TAG, tag.size, tag.data) != 1)
        goto out_err;
    if (EVP_DecryptFinal_ex(&ctx, outp+len, &len) != 1)
        goto out_err;

    EVP_CIPHER_CTX_cleanup(&ctx);

    CONSUME_REDS(env, in);

    return out;

out_err:
    EVP_CIPHER_CTX_cleanup(&ctx);
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

#if defined(HAVE_CHACHA20_POLY1305)
static void
poly1305_update_with_length(poly1305_state *poly1305,
			    const unsigned char *data, size_t data_len)
{
        size_t j = data_len;
        unsigned char length_bytes[8];
        unsigned i;

        for (i = 0; i < sizeof(length_bytes); i++) {
                length_bytes[i] = j;
                j >>= 8;
        }

        CRYPTO_poly1305_update(poly1305, data, data_len);
        CRYPTO_poly1305_update(poly1305, length_bytes, sizeof(length_bytes));
}
#endif

static ERL_NIF_TERM chacha20_poly1305_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key,Iv,AAD,In) */
#if defined(HAVE_CHACHA20_POLY1305)
    ErlNifBinary key, iv, aad, in;
    unsigned char *outp;
    ERL_NIF_TERM out, out_tag;
    ErlNifUInt64 in_len_64;
    unsigned char poly1305_key[32];
    poly1305_state poly1305;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key) || key.size != 32
	|| !enif_inspect_binary(env, argv[1], &iv) || iv.size != CHACHA20_NONCE_LEN
	|| !enif_inspect_iolist_as_binary(env, argv[2], &aad)
	|| !enif_inspect_iolist_as_binary(env, argv[3], &in)) {
	return enif_make_badarg(env);
    }

    /* Take from OpenSSL patch set/LibreSSL:
     *
     * The underlying ChaCha implementation may not overflow the block
     * counter into the second counter word. Therefore we disallow
     * individual operations that work on more than 2TB at a time.
     * in_len_64 is needed because, on 32-bit platforms, size_t is only
     * 32-bits and this produces a warning because it's always false.
     * Casting to uint64_t inside the conditional is not sufficient to stop
     * the warning. */
    in_len_64 = in.size;
    if (in_len_64 >= (1ULL << 32) * 64 - 64)
	return enif_make_badarg(env);

    memset(poly1305_key, 0, sizeof(poly1305_key));
    CRYPTO_chacha_20(poly1305_key, poly1305_key, sizeof(poly1305_key), key.data, iv.data, 0);

    outp = enif_make_new_binary(env, in.size, &out);

    CRYPTO_poly1305_init(&poly1305, poly1305_key);
    poly1305_update_with_length(&poly1305, aad.data, aad.size);
    CRYPTO_chacha_20(outp, in.data, in.size, key.data, iv.data, 1);
    poly1305_update_with_length(&poly1305, outp, in.size);

    CRYPTO_poly1305_finish(&poly1305, enif_make_new_binary(env, POLY1305_TAG_LEN, &out_tag));

    CONSUME_REDS(env, in);

    return enif_make_tuple2(env, out, out_tag);

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

static ERL_NIF_TERM chacha20_poly1305_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key,Iv,AAD,In,Tag) */
#if defined(HAVE_CHACHA20_POLY1305)
    ErlNifBinary key, iv, aad, in, tag;
    unsigned char *outp;
    ERL_NIF_TERM out;
    ErlNifUInt64 in_len_64;
    unsigned char poly1305_key[32];
    unsigned char mac[POLY1305_TAG_LEN];
    poly1305_state poly1305;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key) || key.size != 32
	|| !enif_inspect_binary(env, argv[1], &iv) || iv.size != CHACHA20_NONCE_LEN
	|| !enif_inspect_iolist_as_binary(env, argv[2], &aad)
	|| !enif_inspect_iolist_as_binary(env, argv[3], &in)
	|| !enif_inspect_iolist_as_binary(env, argv[4], &tag) || tag.size != POLY1305_TAG_LEN) {
	return enif_make_badarg(env);
    }

    /* Take from OpenSSL patch set/LibreSSL:
     *
     * The underlying ChaCha implementation may not overflow the block
     * counter into the second counter word. Therefore we disallow
     * individual operations that work on more than 2TB at a time.
     * in_len_64 is needed because, on 32-bit platforms, size_t is only
     * 32-bits and this produces a warning because it's always false.
     * Casting to uint64_t inside the conditional is not sufficient to stop
     * the warning. */
    in_len_64 = in.size;
    if (in_len_64 >= (1ULL << 32) * 64 - 64)
	return enif_make_badarg(env);

    memset(poly1305_key, 0, sizeof(poly1305_key));
    CRYPTO_chacha_20(poly1305_key, poly1305_key, sizeof(poly1305_key), key.data, iv.data, 0);

    CRYPTO_poly1305_init(&poly1305, poly1305_key);
    poly1305_update_with_length(&poly1305, aad.data, aad.size);
    poly1305_update_with_length(&poly1305, in.data, in.size);
    CRYPTO_poly1305_finish(&poly1305, mac);

    if (memcmp(mac, tag.data, POLY1305_TAG_LEN) != 0)
	return atom_error;

    outp = enif_make_new_binary(env, in.size, &out);

    CRYPTO_chacha_20(outp, in.data, in.size, key.data, iv.data, 1);

    CONSUME_REDS(env, in);

    return out;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

static ERL_NIF_TERM rand_bytes_1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Bytes) */     
    unsigned bytes;
    unsigned char* data;
    ERL_NIF_TERM ret;

    if (!enif_get_uint(env, argv[0], &bytes)) {
	return enif_make_badarg(env);
    }
    data = enif_make_new_binary(env, bytes, &ret);
    RAND_pseudo_bytes(data, bytes);
    ERL_VALGRIND_MAKE_MEM_DEFINED(data, bytes);
    return ret;
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


static ERL_NIF_TERM strong_rand_mpint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Bytes, TopMask, BottomMask) */    
    unsigned bits;
    BIGNUM *bn_rand;
    int top, bottom;
    unsigned char* data;
    unsigned dlen;
    ERL_NIF_TERM ret;

    if (!enif_get_uint(env, argv[0], &bits)
	|| !enif_get_int(env, argv[1], &top)
	|| !enif_get_int(env, argv[2], &bottom)) {
	return enif_make_badarg(env);
    }
    if (! (top == -1 || top == 0 || top == 1) ) {
        return enif_make_badarg(env);
    }
    if (! (bottom == 0 || bottom == 1) ) {
        return enif_make_badarg(env);
    }

    bn_rand = BN_new();
    if (! bn_rand ) {
        return enif_make_badarg(env);
    }

    /* Get a (bits) bit random number */
    if (!BN_rand(bn_rand, bits, top, bottom)) {
        ret = atom_false;
    }
    else {
	/* Copy the bignum into an erlang mpint binary. */
	dlen = BN_num_bytes(bn_rand);
	data = enif_make_new_binary(env, dlen+4, &ret);
	put_int32(data, dlen);
	BN_bn2bin(bn_rand, data+4);
	ERL_VALGRIND_MAKE_MEM_DEFINED(data+4, dlen);
    }
    BN_free(bn_rand);

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

static ERL_NIF_TERM dss_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (sha, Digest, Signature,Key=[P, Q, G, Y]) */
    ErlNifBinary digest_bin, sign_bin;
    BIGNUM *dsa_p = NULL, *dsa_q = NULL, *dsa_g = NULL, *dsa_y = NULL;
    ERL_NIF_TERM head, tail;
    DSA *dsa;
    int i;

    if (!argv[0] == atom_sha
        || !enif_inspect_binary(env, argv[1], &digest_bin)
        || digest_bin.size != SHA_DIGEST_LENGTH
        || !enif_inspect_binary(env, argv[2], &sign_bin)
	|| !enif_get_list_cell(env, argv[3], &head, &tail)
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
	return enif_make_badarg(env);
    }

    dsa = DSA_new();
    dsa->p = dsa_p;
    dsa->q = dsa_q;
    dsa->g = dsa_g;
    dsa->priv_key = NULL;
    dsa->pub_key = dsa_y;
    i =  DSA_verify(0, digest_bin.data, SHA_DIGEST_LENGTH,
		    sign_bin.data, sign_bin.size, dsa);
    DSA_free(dsa);
    return(i > 0) ? atom_true : atom_false;
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

static ERL_NIF_TERM rsa_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Digest, Signature, Key=[E,N]) */
    ErlNifBinary         digest_bin, sign_bin;
    ERL_NIF_TERM         head, tail, ret;
    int                  i;
    RSA                  *rsa;
#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)
    EVP_PKEY             *pkey;
    EVP_PKEY_CTX         *ctx;
#endif
    const EVP_MD         *md;
    const ERL_NIF_TERM   type = argv[0];
    struct digest_type_t *digp = NULL;

    digp = get_digest_type(type);
    if (!digp) {
	return enif_make_badarg(env);
    }
    md = digp->md.p;
    if (!md) {
	return atom_notsup;
    }

    rsa = RSA_new();

    if (!enif_inspect_binary(env, argv[1], &digest_bin)
        || digest_bin.size != EVP_MD_size(md)
        || !enif_inspect_binary(env, argv[2], &sign_bin)
	|| !enif_get_list_cell(env, argv[3], &head, &tail)
	|| !get_bn_from_bin(env, head, &rsa->e)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &rsa->n)
	|| !enif_is_empty_list(env, tail)) {
	
	ret = enif_make_badarg(env);
	goto done;
    }

#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)
    pkey = EVP_PKEY_new();
    EVP_PKEY_set1_RSA(pkey, rsa);

    ctx = EVP_PKEY_CTX_new(pkey, NULL);
    EVP_PKEY_verify_init(ctx);
    EVP_PKEY_CTX_set_rsa_padding(ctx, RSA_PKCS1_PADDING);
    EVP_PKEY_CTX_set_signature_md(ctx, md);

    i = EVP_PKEY_verify(ctx, sign_bin.data, sign_bin.size,
                        digest_bin.data, digest_bin.size);
    EVP_PKEY_CTX_free(ctx);
    EVP_PKEY_free(pkey);
#else
    i = RSA_verify(md->type, digest_bin.data, EVP_MD_size(md),
		   sign_bin.data, sign_bin.size, rsa);
#endif

    ret = (i==1 ? atom_true : atom_false);

done:
    RSA_free(rsa);
    return ret;
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

static ERL_NIF_TERM rc4_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Data) */
    ErlNifBinary key, data;
    RC4_KEY rc4_key;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env,argv[0], &key)
	|| !enif_inspect_iolist_as_binary(env,argv[1], &data)) {
	return enif_make_badarg(env);
    }
    RC4_set_key(&rc4_key, key.size, key.data);
    RC4(&rc4_key, data.size, data.data,
	enif_make_new_binary(env, data.size, &ret));
    CONSUME_REDS(env,data);
    return ret;
}   

static ERL_NIF_TERM rc4_set_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key) */
    ErlNifBinary key;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env,argv[0], &key)) {
	return enif_make_badarg(env);
    }
    RC4_set_key((RC4_KEY*)enif_make_new_binary(env, sizeof(RC4_KEY), &ret),
		key.size, key.data);        
    return ret;
}

static ERL_NIF_TERM rc4_encrypt_with_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (State, Data) */

    ErlNifBinary state, data;
    RC4_KEY* rc4_key;
    ERL_NIF_TERM new_state, new_data;

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
}   

static int get_rsa_private_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa)
{
    /* key=[E,N,D]|[E,N,D,P1,P2,E1,E2,C] */
    ERL_NIF_TERM head, tail;

    if (!enif_get_list_cell(env, key, &head, &tail)
	|| !get_bn_from_bin(env, head, &rsa->e)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &rsa->n)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &rsa->d)
	|| (!enif_is_empty_list(env, tail) &&
	    (!enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_bin(env, head, &rsa->p)
	     || !enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_bin(env, head, &rsa->q)
	     || !enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_bin(env, head, &rsa->dmp1)
	     || !enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_bin(env, head, &rsa->dmq1)
	     || !enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_bin(env, head, &rsa->iqmp)
	     || !enif_is_empty_list(env, tail)))) {
	return 0;
    }
    return 1;
}

static ERL_NIF_TERM rsa_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Digest, Key=[E,N,D]|[E,N,D,P1,P2,E1,E2,C]) */
    ErlNifBinary         digest_bin, ret_bin;
#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)
    EVP_PKEY             *pkey;
    EVP_PKEY_CTX         *ctx;
    size_t               rsa_s_len;
#else
    unsigned             rsa_s_len, len;
#endif
    RSA                  *rsa;
    int                  i;
    struct digest_type_t *digp;
    const EVP_MD         *md;

    digp = get_digest_type(argv[0]);
    if (!digp) {
	return enif_make_badarg(env);
    }
    md = digp->md.p;
    if (!md) {
	return atom_notsup;
    }

    if (!enif_inspect_binary(env,argv[1],&digest_bin)
        || digest_bin.size != EVP_MD_size(md)) {
        return enif_make_badarg(env);
    }

    rsa = RSA_new();
    if (!get_rsa_private_key(env, argv[2], rsa)) {
	RSA_free(rsa);
	return enif_make_badarg(env);
    }


#if OPENSSL_VERSION_NUMBER >= OpenSSL_version_plain(1,0,0)
    pkey = EVP_PKEY_new();
    EVP_PKEY_set1_RSA(pkey, rsa);
    rsa_s_len=(size_t)EVP_PKEY_size(pkey);
    enif_alloc_binary(rsa_s_len, &ret_bin);

    ctx = EVP_PKEY_CTX_new(pkey, NULL);
    EVP_PKEY_sign_init(ctx);
    EVP_PKEY_CTX_set_rsa_padding(ctx, RSA_PKCS1_PADDING);
    EVP_PKEY_CTX_set_signature_md(ctx, md);

    i = EVP_PKEY_sign(ctx, ret_bin.data, &rsa_s_len,
                      digest_bin.data, digest_bin.size);
    ASSERT(i<=0 || rsa_s_len <= ret_bin.size);
    EVP_PKEY_CTX_free(ctx);
    EVP_PKEY_free(pkey);
#else
    enif_alloc_binary(RSA_size(rsa), &ret_bin);
    len = EVP_MD_size(md);

    ERL_VALGRIND_ASSERT_MEM_DEFINED(digest_bin.data, len);
    i =  RSA_sign(md->type, digest_bin.data, len,
		  ret_bin.data, &rsa_s_len, rsa);
#endif

    RSA_free(rsa);
    if (i > 0) {
	ERL_VALGRIND_MAKE_MEM_DEFINED(ret_bin.data, rsa_s_len);
	if (rsa_s_len != ret_bin.size) {
	    enif_realloc_binary(&ret_bin, rsa_s_len);
	    ERL_VALGRIND_ASSERT_MEM_DEFINED(ret_bin.data, rsa_s_len);
	}
	return enif_make_binary(env,&ret_bin);
    }
    else {
	enif_release_binary(&ret_bin);
	return atom_error;
    }
}


static ERL_NIF_TERM dss_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (sha, Digest, Key=[P,Q,G,PrivKey]) */
    ErlNifBinary digest_bin, ret_bin;
    ERL_NIF_TERM head, tail;
    unsigned int dsa_s_len;
    DSA* dsa;
    int i;

    if (!argv[0] == atom_sha
        || !enif_inspect_binary(env, argv[1], &digest_bin)
        || digest_bin.size != SHA_DIGEST_LENGTH) {
	return enif_make_badarg(env);
    }

    dsa = DSA_new();

    dsa->pub_key  = NULL;
    if (!enif_get_list_cell(env, argv[2], &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa->p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa->q)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa->g)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dsa->priv_key)
	|| !enif_is_empty_list(env,tail)) {
	DSA_free(dsa);
	return enif_make_badarg(env);
    }

    enif_alloc_binary(DSA_size(dsa), &ret_bin);
    i =  DSA_sign(NID_sha1, digest_bin.data, SHA_DIGEST_LENGTH,
		  ret_bin.data, &dsa_s_len, dsa);
    DSA_free(dsa);
    if (i) {
	if (dsa_s_len != ret_bin.size) {
	    enif_realloc_binary(&ret_bin, dsa_s_len);
	}
	return enif_make_binary(env, &ret_bin);
    }
    else {
	enif_release_binary(&ret_bin);
	return atom_error;
    }
}


static int rsa_pad(ERL_NIF_TERM term, int* padding)
{
    if (term == atom_rsa_pkcs1_padding) {
	*padding = RSA_PKCS1_PADDING;
    }
    else if (term == atom_rsa_pkcs1_oaep_padding) {
	*padding = RSA_PKCS1_OAEP_PADDING;
    }
    else if (term == atom_rsa_no_padding) {
	*padding = RSA_NO_PADDING;
    }
    else {
	return 0;
    }
    return 1;
}

static ERL_NIF_TERM rsa_public_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data, PublKey=[E,N], Padding, IsEncrypt) */
    ErlNifBinary data_bin, ret_bin;
    ERL_NIF_TERM head, tail;
    int padding, i;
    RSA* rsa;

    rsa = RSA_new();

    if (!enif_inspect_binary(env, argv[0], &data_bin)
	|| !enif_get_list_cell(env, argv[1], &head, &tail)
	|| !get_bn_from_bin(env, head, &rsa->e)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &rsa->n)
	|| !enif_is_empty_list(env,tail)
	|| !rsa_pad(argv[2], &padding)) {

	RSA_free(rsa);
	return enif_make_badarg(env);
    }

    enif_alloc_binary(RSA_size(rsa), &ret_bin); 

    if (argv[3] == atom_true) {
	ERL_VALGRIND_ASSERT_MEM_DEFINED(data_bin.data,data_bin.size);
	i = RSA_public_encrypt(data_bin.size, data_bin.data,
			       ret_bin.data, rsa, padding);
	if (i > 0) {
	    ERL_VALGRIND_MAKE_MEM_DEFINED(ret_bin.data, i);
	}
    }
    else {
	i = RSA_public_decrypt(data_bin.size, data_bin.data,
			       ret_bin.data, rsa, padding);    
	if (i > 0) {
	    ERL_VALGRIND_MAKE_MEM_DEFINED(ret_bin.data, i);
	    enif_realloc_binary(&ret_bin, i);
	}
    }
    RSA_free(rsa);
    if (i > 0) {
	return enif_make_binary(env,&ret_bin);
    }
    else {
	enif_release_binary(&ret_bin);
	return atom_error;
    }
}

static ERL_NIF_TERM rsa_private_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data, Key=[E,N,D]|[E,N,D,P1,P2,E1,E2,C], Padding, IsEncrypt) */
    ErlNifBinary data_bin, ret_bin;
    int padding, i;
    RSA* rsa;

    rsa = RSA_new();

    if (!enif_inspect_binary(env, argv[0], &data_bin)
	|| !get_rsa_private_key(env, argv[1], rsa)
	|| !rsa_pad(argv[2], &padding)) {

	RSA_free(rsa);
	return enif_make_badarg(env);
    }

    enif_alloc_binary(RSA_size(rsa), &ret_bin); 

    if (argv[3] == atom_true) {
	ERL_VALGRIND_ASSERT_MEM_DEFINED(data_bin.data,data_bin.size);
	i = RSA_private_encrypt(data_bin.size, data_bin.data,
				ret_bin.data, rsa, padding);
	if (i > 0) {
	    ERL_VALGRIND_MAKE_MEM_DEFINED(ret_bin.data, i);
	}
    }
    else {
	i = RSA_private_decrypt(data_bin.size, data_bin.data,
				ret_bin.data, rsa, padding);       
	if (i > 0) {
	    ERL_VALGRIND_MAKE_MEM_DEFINED(ret_bin.data, i);
	    enif_realloc_binary(&ret_bin, i);
	}
    }
    RSA_free(rsa);
    if (i > 0) {
	return enif_make_binary(env,&ret_bin);
    }
    else {
	enif_release_binary(&ret_bin);
	return atom_error;
    }
}

static ERL_NIF_TERM dh_generate_parameters_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (PrimeLen, Generator) */
    int prime_len, generator;
    DH* dh_params;
    int p_len, g_len;
    unsigned char *p_ptr, *g_ptr;
    ERL_NIF_TERM ret_p, ret_g;

    if (!enif_get_int(env, argv[0], &prime_len)
	|| !enif_get_int(env, argv[1], &generator)) {

	return enif_make_badarg(env);
    }
    dh_params = DH_generate_parameters(prime_len, generator, NULL, NULL);
    if (dh_params == NULL) {
	return atom_error;
    }
    p_len = BN_num_bytes(dh_params->p);
    g_len = BN_num_bytes(dh_params->g);
    p_ptr = enif_make_new_binary(env, p_len, &ret_p);
    g_ptr = enif_make_new_binary(env, g_len, &ret_g);
    BN_bn2bin(dh_params->p, p_ptr);
    BN_bn2bin(dh_params->g, g_ptr);
    ERL_VALGRIND_MAKE_MEM_DEFINED(p_ptr, p_len);
    ERL_VALGRIND_MAKE_MEM_DEFINED(g_ptr, g_len);
    DH_free(dh_params);
    return enif_make_list2(env, ret_p, ret_g);    
}

static ERL_NIF_TERM dh_check(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* ([PrimeLen, Generator]) */
    DH* dh_params;
    int i;
    ERL_NIF_TERM ret, head, tail;

    dh_params = DH_new();

    if (!enif_get_list_cell(env, argv[0], &head, &tail)   
	|| !get_bn_from_bin(env, head, &dh_params->p)
	|| !enif_get_list_cell(env, tail, &head, &tail)   
	|| !get_bn_from_bin(env, head, &dh_params->g)
	|| !enif_is_empty_list(env,tail)) {

	DH_free(dh_params);
	return enif_make_badarg(env);
    }
    if (DH_check(dh_params, &i)) {
	if (i == 0) ret = atom_ok;
	else if (i & DH_CHECK_P_NOT_PRIME) ret = atom_not_prime;
	else if (i & DH_CHECK_P_NOT_SAFE_PRIME)	ret = atom_not_strong_prime;
	else if (i & DH_UNABLE_TO_CHECK_GENERATOR) ret = atom_unable_to_check_generator;
	else if (i & DH_NOT_SUITABLE_GENERATOR)	ret = atom_not_suitable_generator;
	else ret = enif_make_tuple2(env, atom_unknown, enif_make_uint(env, i));
    }
    else { /* Check Failed */       
	ret = enif_make_tuple2(env, atom_error, atom_check_failed);
    }
    DH_free(dh_params);
    return ret;
}   

static ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (PrivKey, DHParams=[P,G], Mpint) */
    DH* dh_params;
    int pub_len, prv_len;
    unsigned char *pub_ptr, *prv_ptr;
    ERL_NIF_TERM ret, ret_pub, ret_prv, head, tail;
    int mpint; /* 0 or 4 */

    dh_params = DH_new();

    if (!(get_bn_from_bin(env, argv[0], &dh_params->priv_key)
	  || argv[0] == atom_undefined)
	|| !enif_get_list_cell(env, argv[1], &head, &tail)
	|| !get_bn_from_bin(env, head, &dh_params->p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dh_params->g)
	|| !enif_is_empty_list(env, tail)
	|| !enif_get_int(env, argv[2], &mpint) || (mpint & ~4)) {
	DH_free(dh_params);
	return enif_make_badarg(env);
    }

    if (DH_generate_key(dh_params)) {
	pub_len = BN_num_bytes(dh_params->pub_key);
	prv_len = BN_num_bytes(dh_params->priv_key);    
	pub_ptr = enif_make_new_binary(env, pub_len+mpint, &ret_pub);
	prv_ptr = enif_make_new_binary(env, prv_len+mpint, &ret_prv);
	if (mpint) {
	    put_int32(pub_ptr, pub_len); pub_ptr += 4;
	    put_int32(prv_ptr, prv_len); prv_ptr += 4;
	}
	BN_bn2bin(dh_params->pub_key, pub_ptr);
	BN_bn2bin(dh_params->priv_key, prv_ptr);
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
    BIGNUM* pubkey = NULL;
    int i;
    ErlNifBinary ret_bin;
    ERL_NIF_TERM ret, head, tail;

    dh_params = DH_new();

    if (!get_bn_from_bin(env, argv[0], &pubkey)
	|| !get_bn_from_bin(env, argv[1], &dh_params->priv_key)
	|| !enif_get_list_cell(env, argv[2], &head, &tail)
	|| !get_bn_from_bin(env, head, &dh_params->p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_bin(env, head, &dh_params->g)
	|| !enif_is_empty_list(env, tail)) {

	ret = enif_make_badarg(env);
    }
    else {
	enif_alloc_binary(DH_size(dh_params), &ret_bin);
	i = DH_compute_key(ret_bin.data, pubkey, dh_params);
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
    if (pubkey) BN_free(pubkey);
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

static ERL_NIF_TERM ecdsa_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Digest, Curve, Key) */
#if defined(HAVE_EC)
    ErlNifBinary digest_bin, ret_bin;
    unsigned int dsa_s_len;
    EC_KEY* key = NULL;
    int i, len;
    struct digest_type_t *digp;
    const EVP_MD         *md;

    digp = get_digest_type(argv[0]);
    if (!digp) {
	return enif_make_badarg(env);
    }
    md = digp->md.p;
    if (!md) {
	return atom_notsup;
    }
    len = EVP_MD_size(md);

    if (!enif_inspect_binary(env,argv[1],&digest_bin)
        || digest_bin.size != len
        || !get_ec_key(env, argv[2], argv[3], atom_undefined, &key))
      goto badarg;

    enif_alloc_binary(ECDSA_size(key), &ret_bin);

    i = ECDSA_sign(md->type, digest_bin.data, len,
		   ret_bin.data, &dsa_s_len, key);

    EC_KEY_free(key);
    if (i) {
	if (dsa_s_len != ret_bin.size) {
	    enif_realloc_binary(&ret_bin, dsa_s_len);
	}
	return enif_make_binary(env, &ret_bin);
    }
    else {
	enif_release_binary(&ret_bin);
	return atom_error;
    }

badarg:
    if (key)
	EC_KEY_free(key);
    return make_badarg_maybe(env);
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM ecdsa_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Digest, Signature, Curve, Key) */
#if defined(HAVE_EC)
    ErlNifBinary digest_bin, sign_bin;
    int i, len;
    EC_KEY* key = NULL;
    const ERL_NIF_TERM type = argv[0];
    struct digest_type_t *digp = NULL;
    const EVP_MD         *md;

    digp = get_digest_type(type);
    if (!digp) {
	return enif_make_badarg(env);
    }
    md = digp->md.p;
    if (!md) {
	return atom_notsup;
    }
    len = EVP_MD_size(md);

    if (!enif_inspect_binary(env, argv[1], &digest_bin)
        || digest_bin.size != len
        || !enif_inspect_binary(env, argv[2], &sign_bin)
	|| !get_ec_key(env, argv[3], atom_undefined, argv[4], &key))
	goto badarg;

    i = ECDSA_verify(md->type, digest_bin.data, len,
		     sign_bin.data, sign_bin.size, key);

    EC_KEY_free(key);

    return (i==1 ? atom_true : atom_false);

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

static ERL_NIF_TERM rand_seed_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary seed_bin;

    if (!enif_inspect_binary(env, argv[0], &seed_bin))
        return enif_make_badarg(env);
    RAND_seed(seed_bin.data,seed_bin.size);
    return atom_ok;
}
