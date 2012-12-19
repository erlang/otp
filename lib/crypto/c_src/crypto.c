/* 
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2012. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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

#include "crypto_callback.h"

#if OPENSSL_VERSION_NUMBER >= 0x00908000L && !defined(OPENSSL_NO_SHA224) && defined(NID_sha224)\
         && !defined(OPENSSL_NO_SHA256) /* disabled like this in my sha.h (?) */
# define HAVE_SHA224
#endif
#if OPENSSL_VERSION_NUMBER >= 0x00908000L && !defined(OPENSSL_NO_SHA256) && defined(NID_sha256)
# define HAVE_SHA256
#endif
#if OPENSSL_VERSION_NUMBER >= 0x00908000L && !defined(OPENSSL_NO_SHA384) && defined(NID_sha384)\
         && !defined(OPENSSL_NO_SHA512) /* disabled like this in my sha.h (?) */
# define HAVE_SHA384
#endif
#if OPENSSL_VERSION_NUMBER >= 0x00908000L && !defined(OPENSSL_NO_SHA512) && defined(NID_sha512)
# define HAVE_SHA512
#endif
#if OPENSSL_VERSION_NUMBER >= 0x0090705FL
# define HAVE_DES_ede3_cfb_encrypt
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

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM info_lib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md5(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md5_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md5_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md5_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ripemd160(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ripemd160_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ripemd160_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ripemd160_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha224_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha224_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha224_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha224_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha256_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha256_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha256_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha256_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha384_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha384_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha384_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha384_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha512_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha512_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha512_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha512_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md4_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md4_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md4_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM md5_mac_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha_mac_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha224_mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha256_mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha384_mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sha512_mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM hmac_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM des_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM des_cfb_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM des_ecb_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM des_ede3_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM des_ede3_cfb_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_cfb_128_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ctr_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rand_bytes_1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM strong_rand_bytes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rand_bytes_3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM strong_rand_mpint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rand_uniform_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM mod_exp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dss_verify(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM aes_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM exor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc4_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc4_set_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc4_encrypt_with_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rc2_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dss_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_public_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM rsa_private_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_generate_parameters_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_check(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM bf_cfb64_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM bf_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM bf_ecb_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM blowfish_ofb64_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);



/* helpers */
static void init_digest_types(ErlNifEnv* env);
static void hmac_md5(unsigned char *key, int klen,
		     unsigned char *dbuf, int dlen, 
		     unsigned char *hmacbuf);
static void hmac_sha1(unsigned char *key, int klen,
		      unsigned char *dbuf, int dlen, 
		      unsigned char *hmacbuf);
#ifdef HAVE_SHA224
static void hmac_sha224(unsigned char *key, int klen,
			unsigned char *dbuf, int dlen,
			unsigned char *hmacbuf);
#endif
#ifdef HAVE_SHA256
static void hmac_sha256(unsigned char *key, int klen,
			unsigned char *dbuf, int dlen,
			unsigned char *hmacbuf);
#endif
#ifdef HAVE_SHA384
static void hmac_sha384(unsigned char *key, int klen,
			unsigned char *dbuf, int dlen,
			unsigned char *hmacbuf);
#endif
#ifdef HAVE_SHA512
static void hmac_sha512(unsigned char *key, int klen,
			unsigned char *dbuf, int dlen,
			unsigned char *hmacbuf);
#endif

static int library_refc = 0; /* number of users of this dynamic library */

static ErlNifFunc nif_funcs[] = {
    {"info_lib", 0, info_lib},
    {"md5", 1, md5},
    {"md5_init", 0, md5_init},
    {"md5_update", 2, md5_update},
    {"md5_final", 1, md5_final},
    {"ripemd160", 1, ripemd160},
    {"ripemd160_init", 0, ripemd160_init},
    {"ripemd160_update", 2, ripemd160_update},
    {"ripemd160_final", 1, ripemd160_final},
    {"sha", 1, sha},
    {"sha_init", 0, sha_init},
    {"sha_update", 2, sha_update},
    {"sha_final", 1, sha_final},
    {"sha224_nif", 1, sha224_nif},
    {"sha224_init_nif", 0, sha224_init_nif},
    {"sha224_update_nif", 2, sha224_update_nif},
    {"sha224_final_nif", 1, sha224_final_nif},
    {"sha256_nif", 1, sha256_nif},
    {"sha256_init_nif", 0, sha256_init_nif},
    {"sha256_update_nif", 2, sha256_update_nif},
    {"sha256_final_nif", 1, sha256_final_nif},
    {"sha384_nif", 1, sha384_nif},
    {"sha384_init_nif", 0, sha384_init_nif},
    {"sha384_update_nif", 2, sha384_update_nif},
    {"sha384_final_nif", 1, sha384_final_nif},
    {"sha512_nif", 1, sha512_nif},
    {"sha512_init_nif", 0, sha512_init_nif},
    {"sha512_update_nif", 2, sha512_update_nif},
    {"sha512_final_nif", 1, sha512_final_nif},
    {"md4", 1, md4},
    {"md4_init", 0, md4_init},
    {"md4_update", 2, md4_update},
    {"md4_final", 1, md4_final},
    {"md5_mac_n", 3, md5_mac_n},
    {"sha_mac_n", 3, sha_mac_n},
    {"sha224_mac_nif", 3, sha224_mac_nif},
    {"sha256_mac_nif", 3, sha256_mac_nif},
    {"sha384_mac_nif", 3, sha384_mac_nif},
    {"sha512_mac_nif", 3, sha512_mac_nif},
    {"hmac_init", 2, hmac_init},
    {"hmac_update", 2, hmac_update},
    {"hmac_final", 1, hmac_final},
    {"hmac_final_n", 2, hmac_final},
    {"des_cbc_crypt", 4, des_cbc_crypt},
    {"des_cfb_crypt", 4, des_cfb_crypt},
    {"des_ecb_crypt", 3, des_ecb_crypt},
    {"des_ede3_cbc_crypt", 6, des_ede3_cbc_crypt},
    {"des_ede3_cfb_crypt_nif", 6, des_ede3_cfb_crypt_nif},
    {"aes_cfb_128_crypt", 4, aes_cfb_128_crypt},
    {"aes_ctr_encrypt", 3, aes_ctr_encrypt},
    {"aes_ctr_decrypt", 3, aes_ctr_encrypt},
    {"aes_ctr_stream_encrypt", 2, aes_ctr_stream_encrypt},
    {"aes_ctr_stream_decrypt", 2, aes_ctr_stream_encrypt},
    {"rand_bytes", 1, rand_bytes_1},
    {"strong_rand_bytes_nif", 1, strong_rand_bytes_nif},
    {"rand_bytes", 3, rand_bytes_3},
    {"strong_rand_mpint_nif", 3, strong_rand_mpint_nif},
    {"rand_uniform_nif", 2, rand_uniform_nif},
    {"mod_exp_nif", 3, mod_exp_nif},
    {"dss_verify", 4, dss_verify},
    {"rsa_verify_nif", 4, rsa_verify_nif},
    {"aes_cbc_crypt", 4, aes_cbc_crypt},
    {"exor", 2, exor},
    {"rc4_encrypt", 2, rc4_encrypt},
    {"rc4_set_key", 1, rc4_set_key},
    {"rc4_encrypt_with_state", 2, rc4_encrypt_with_state},
    {"rc2_cbc_crypt", 4, rc2_cbc_crypt},
    {"rsa_sign_nif", 3, rsa_sign_nif},
    {"dss_sign_nif", 3, dss_sign_nif},
    {"rsa_public_crypt", 4, rsa_public_crypt},
    {"rsa_private_crypt", 4, rsa_private_crypt},
    {"dh_generate_parameters_nif", 2, dh_generate_parameters_nif},
    {"dh_check", 1, dh_check},
    {"dh_generate_key_nif", 2, dh_generate_key_nif},
    {"dh_compute_key_nif", 3, dh_compute_key_nif},
    {"bf_cfb64_crypt", 4, bf_cfb64_crypt},
    {"bf_cbc_crypt", 4, bf_cbc_crypt},
    {"bf_ecb_crypt", 3, bf_ecb_crypt},
    {"blowfish_ofb64_encrypt", 3, blowfish_ofb64_encrypt}
};

ERL_NIF_INIT(crypto,nif_funcs,load,NULL,upgrade,unload)


#define MD5_CTX_LEN     (sizeof(MD5_CTX))
#define MD5_LEN         16
#define MD5_LEN_96      12
#define MD4_CTX_LEN     (sizeof(MD4_CTX))
#define MD4_LEN         16
#define RIPEMD160_CTX_LEN (sizeof(RIPEMD160_CTX))
#define RIPEMD160_LEN   20
#define SHA_CTX_LEN     (sizeof(SHA_CTX))
#define SHA_LEN         20
#define SHA_LEN_96      12
#define SHA224_LEN	(224/8)
#define SHA256_LEN	(256/8)
#define SHA384_LEN	(384/8)
#define SHA512_LEN	(512/8)
#define HMAC_INT_LEN    64
#define HMAC_INT2_LEN   128

#define HMAC_IPAD       0x36
#define HMAC_OPAD       0x5c


static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_sha;
static ERL_NIF_TERM atom_sha224;
static ERL_NIF_TERM atom_sha256;
static ERL_NIF_TERM atom_sha384;
static ERL_NIF_TERM atom_sha512;
static ERL_NIF_TERM atom_md5;
static ERL_NIF_TERM atom_ripemd160;
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

/*
#define PRINTF_ERR0(FMT) enif_fprintf(stderr, FMT "\n")
#define PRINTF_ERR1(FMT, A1) enif_fprintf(stderr, FMT "\n", A1)
*/
#define PRINTF_ERR0(FMT)
#define PRINTF_ERR1(FMT,A1)

#ifdef HAVE_DYNAMIC_CRYPTO_LIB
static int change_basename(char* buf, int bufsz, const char* newfile)
{
    char* p = strrchr(buf, '/');
    p = (p == NULL) ? buf : p + 1;
    
    if ((p - buf) + strlen(newfile) >= bufsz) {
	PRINTF_ERR0("CRYPTO: lib name too long");
	return 0;
    }
    strcpy(p, newfile);
    return 1;
}

static void error_handler(void* null, const char* errstr)
{
    PRINTF_ERR1("CRYPTO LOADING ERROR: '%s'", errstr);
}
#endif /* HAVE_DYNAMIC_CRYPTO_LIB */

static int init(ErlNifEnv* env, ERL_NIF_TERM load_info)
{
    ErlNifSysInfo sys_info;
    get_crypto_callbacks_t* funcp;
    struct crypto_callbacks* ccb;
    int nlocks = 0;
    int tpl_arity;
    const ERL_NIF_TERM* tpl_array;
    int vernum;
    char lib_buf[1000];

    /* load_info: {201, "/full/path/of/this/library"} */
    if (!enif_get_tuple(env, load_info, &tpl_arity, &tpl_array)
	|| tpl_arity != 2
	|| !enif_get_int(env, tpl_array[0], &vernum)
	|| vernum != 201
	|| enif_get_string(env, tpl_array[1], lib_buf, sizeof(lib_buf), ERL_NIF_LATIN1) <= 0) {

	PRINTF_ERR1("CRYPTO: Invalid load_info '%T'", load_info);
	return 0;
    }
    if (library_refc > 0) {
	/* Repeated loading of this library (module upgrade).
	 * Atoms and callbacks are already set, we are done.
	 */
	return 1;
    }

    atom_true = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    atom_sha = enif_make_atom(env,"sha");
    atom_sha224 = enif_make_atom(env,"sha224");
    atom_sha256 = enif_make_atom(env,"sha256");
    atom_sha384 = enif_make_atom(env,"sha384");
    atom_sha512 = enif_make_atom(env,"sha512");
    atom_md5 = enif_make_atom(env,"md5");
    atom_ripemd160 = enif_make_atom(env,"ripemd160");
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

    init_digest_types(env);

#ifdef HAVE_DYNAMIC_CRYPTO_LIB
    {
	void* handle;
	if (!change_basename(lib_buf, sizeof(lib_buf), "crypto_callback")) {
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

static ERL_NIF_TERM md5(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data) */
    ErlNifBinary ibin;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &ibin)) {
	return enif_make_badarg(env);
    }
    MD5((unsigned char *) ibin.data, ibin.size,
	enif_make_new_binary(env,MD5_LEN, &ret));
    return ret;
}
static ERL_NIF_TERM md5_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */   
    ERL_NIF_TERM ret;
    MD5_Init((MD5_CTX *) enif_make_new_binary(env, MD5_CTX_LEN, &ret));
    return ret;
}
static ERL_NIF_TERM md5_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    MD5_CTX* new_ctx;
    ErlNifBinary ctx_bin, data_bin;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin)
	|| ctx_bin.size != MD5_CTX_LEN
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
	return enif_make_badarg(env);
    }
    new_ctx = (MD5_CTX*) enif_make_new_binary(env,MD5_CTX_LEN, &ret);
    memcpy(new_ctx, ctx_bin.data, MD5_CTX_LEN);
    MD5_Update(new_ctx, data_bin.data, data_bin.size);
    return ret;
}
static ERL_NIF_TERM md5_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
    ErlNifBinary ctx_bin;
    MD5_CTX ctx_clone; 
    ERL_NIF_TERM ret;    
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != MD5_CTX_LEN) {
	return enif_make_badarg(env);
    }
    memcpy(&ctx_clone, ctx_bin.data, MD5_CTX_LEN); /* writable */
    MD5_Final(enif_make_new_binary(env, MD5_LEN, &ret), &ctx_clone);
    return ret;
}

static ERL_NIF_TERM ripemd160(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data) */
    ErlNifBinary ibin;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &ibin)) {
  return enif_make_badarg(env);
    }
    RIPEMD160((unsigned char *) ibin.data, ibin.size,
  enif_make_new_binary(env,RIPEMD160_LEN, &ret));
    return ret;
}
static ERL_NIF_TERM ripemd160_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */
    ERL_NIF_TERM ret;
    RIPEMD160_Init((RIPEMD160_CTX *) enif_make_new_binary(env, RIPEMD160_CTX_LEN, &ret));
    return ret;
}
static ERL_NIF_TERM ripemd160_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    RIPEMD160_CTX* new_ctx;
    ErlNifBinary ctx_bin, data_bin;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin)
  || ctx_bin.size != RIPEMD160_CTX_LEN
  || !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
  return enif_make_badarg(env);
    }
    new_ctx = (RIPEMD160_CTX*) enif_make_new_binary(env,RIPEMD160_CTX_LEN, &ret);
    memcpy(new_ctx, ctx_bin.data, RIPEMD160_CTX_LEN);
    RIPEMD160_Update(new_ctx, data_bin.data, data_bin.size);
    return ret;
}
static ERL_NIF_TERM ripemd160_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
    ErlNifBinary ctx_bin;
    RIPEMD160_CTX ctx_clone;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != RIPEMD160_CTX_LEN) {
  return enif_make_badarg(env);
    }
    memcpy(&ctx_clone, ctx_bin.data, RIPEMD160_CTX_LEN); /* writable */
    RIPEMD160_Final(enif_make_new_binary(env, RIPEMD160_LEN, &ret), &ctx_clone);
    return ret;
}


static ERL_NIF_TERM sha(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data) */    
    ErlNifBinary ibin;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &ibin)) {
	return enif_make_badarg(env);
    }
    SHA1((unsigned char *) ibin.data, ibin.size,
	 enif_make_new_binary(env,SHA_LEN, &ret));
    return ret;
}
static ERL_NIF_TERM sha_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */   
    ERL_NIF_TERM ret;
    SHA1_Init((SHA_CTX *) enif_make_new_binary(env, SHA_CTX_LEN, &ret));
    return ret;
}
static ERL_NIF_TERM sha_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    SHA_CTX* new_ctx;
    ErlNifBinary ctx_bin, data_bin;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != SHA_CTX_LEN
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
	return enif_make_badarg(env);
    }
    new_ctx = (SHA_CTX*) enif_make_new_binary(env,SHA_CTX_LEN, &ret);
    memcpy(new_ctx, ctx_bin.data, SHA_CTX_LEN);
    SHA1_Update(new_ctx, data_bin.data, data_bin.size);
    return ret;
}
static ERL_NIF_TERM sha_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
    ErlNifBinary ctx_bin;
    SHA_CTX ctx_clone;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != SHA_CTX_LEN) {
	return enif_make_badarg(env);
    }
    memcpy(&ctx_clone, ctx_bin.data, SHA_CTX_LEN); /* writable */
    SHA1_Final(enif_make_new_binary(env, SHA_LEN, &ret), &ctx_clone);    
    return ret;
}

static ERL_NIF_TERM sha224_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data) */
#ifdef HAVE_SHA224
    ErlNifBinary ibin;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &ibin)) {
	return enif_make_badarg(env);
    }
    SHA224((unsigned char *) ibin.data, ibin.size,
	 enif_make_new_binary(env,SHA224_LEN, &ret));
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha224_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */
#ifdef HAVE_SHA224
    ERL_NIF_TERM ret;
    SHA224_Init((SHA256_CTX *) enif_make_new_binary(env, sizeof(SHA256_CTX), &ret));
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha224_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
#ifdef HAVE_SHA224
    SHA256_CTX* new_ctx;
    ErlNifBinary ctx_bin, data_bin;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != sizeof(SHA256_CTX)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
	return enif_make_badarg(env);
    }
    new_ctx = (SHA256_CTX*) enif_make_new_binary(env,sizeof(SHA256_CTX), &ret);
    memcpy(new_ctx, ctx_bin.data, sizeof(SHA256_CTX));
    SHA224_Update(new_ctx, data_bin.data, data_bin.size);
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha224_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
#ifdef HAVE_SHA224
    ErlNifBinary ctx_bin;
    SHA256_CTX ctx_clone;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != sizeof(SHA256_CTX)) {
	return enif_make_badarg(env);
    }
    memcpy(&ctx_clone, ctx_bin.data, sizeof(SHA256_CTX)); /* writable */
    SHA224_Final(enif_make_new_binary(env, SHA224_LEN, &ret), &ctx_clone);
    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM sha256_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data) */
#ifdef HAVE_SHA256
    ErlNifBinary ibin;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &ibin)) {
	return enif_make_badarg(env);
    }
    SHA256((unsigned char *) ibin.data, ibin.size,
	 enif_make_new_binary(env,SHA256_LEN, &ret));
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha256_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */   
#ifdef HAVE_SHA256
    ERL_NIF_TERM ret;
    SHA256_Init((SHA256_CTX *) enif_make_new_binary(env, sizeof(SHA256_CTX), &ret));
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha256_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
#ifdef HAVE_SHA256
    SHA256_CTX* new_ctx;
    ErlNifBinary ctx_bin, data_bin;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != sizeof(SHA256_CTX)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
	return enif_make_badarg(env);
    }
    new_ctx = (SHA256_CTX*) enif_make_new_binary(env,sizeof(SHA256_CTX), &ret);
    memcpy(new_ctx, ctx_bin.data, sizeof(SHA256_CTX));
    SHA256_Update(new_ctx, data_bin.data, data_bin.size);
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha256_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
#ifdef HAVE_SHA256
    ErlNifBinary ctx_bin;
    SHA256_CTX ctx_clone;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != sizeof(SHA256_CTX)) {
	return enif_make_badarg(env);
    }
    memcpy(&ctx_clone, ctx_bin.data, sizeof(SHA256_CTX)); /* writable */
    SHA256_Final(enif_make_new_binary(env, SHA256_LEN, &ret), &ctx_clone);    
    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM sha384_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data) */
#ifdef HAVE_SHA384
    ErlNifBinary ibin;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &ibin)) {
	return enif_make_badarg(env);
    }
    SHA384((unsigned char *) ibin.data, ibin.size,
	 enif_make_new_binary(env,SHA384_LEN, &ret));
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha384_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */
#ifdef HAVE_SHA384
    ERL_NIF_TERM ret;
    SHA384_Init((SHA512_CTX *) enif_make_new_binary(env, sizeof(SHA512_CTX), &ret));
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha384_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
#ifdef HAVE_SHA384
    SHA512_CTX* new_ctx;
    ErlNifBinary ctx_bin, data_bin;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != sizeof(SHA512_CTX)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
	return enif_make_badarg(env);
    }
    new_ctx = (SHA512_CTX*) enif_make_new_binary(env,sizeof(SHA512_CTX), &ret);
    memcpy(new_ctx, ctx_bin.data, sizeof(SHA512_CTX));
    SHA384_Update(new_ctx, data_bin.data, data_bin.size);
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha384_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
#ifdef HAVE_SHA384
    ErlNifBinary ctx_bin;
    SHA512_CTX ctx_clone;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != sizeof(SHA512_CTX)) {
	return enif_make_badarg(env);
    }
    memcpy(&ctx_clone, ctx_bin.data, sizeof(SHA512_CTX)); /* writable */
    SHA384_Final(enif_make_new_binary(env, SHA384_LEN, &ret), &ctx_clone);
    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM sha512_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data) */
#ifdef HAVE_SHA512
    ErlNifBinary ibin;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &ibin)) {
	return enif_make_badarg(env);
    }
    SHA512((unsigned char *) ibin.data, ibin.size,
	 enif_make_new_binary(env,SHA512_LEN, &ret));
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha512_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */   
#ifdef HAVE_SHA512
    ERL_NIF_TERM ret;
    SHA512_Init((SHA512_CTX *) enif_make_new_binary(env, sizeof(SHA512_CTX), &ret));
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha512_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
#ifdef HAVE_SHA512
    SHA512_CTX* new_ctx;
    ErlNifBinary ctx_bin, data_bin;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != sizeof(SHA512_CTX)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
	return enif_make_badarg(env);
    }
    new_ctx = (SHA512_CTX*) enif_make_new_binary(env,sizeof(SHA512_CTX), &ret);
    memcpy(new_ctx, ctx_bin.data, sizeof(SHA512_CTX));
    SHA512_Update(new_ctx, data_bin.data, data_bin.size);
    return ret;
#else
    return atom_notsup;
#endif
}
static ERL_NIF_TERM sha512_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
#ifdef HAVE_SHA512
    ErlNifBinary ctx_bin;
    SHA512_CTX ctx_clone;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != sizeof(SHA512_CTX)) {
	return enif_make_badarg(env);
    }
    memcpy(&ctx_clone, ctx_bin.data, sizeof(SHA512_CTX)); /* writable */
    SHA512_Final(enif_make_new_binary(env, SHA512_LEN, &ret), &ctx_clone);    
    return ret;
#else
    return atom_notsup;
#endif
}


static ERL_NIF_TERM md4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data) */    
    ErlNifBinary ibin;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &ibin)) {
	return enif_make_badarg(env);
    }
    MD4((unsigned char *) ibin.data, ibin.size,
	enif_make_new_binary(env,MD4_LEN, &ret));
    return ret;
}
static ERL_NIF_TERM md4_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */   
    ERL_NIF_TERM ret;
    MD4_Init((MD4_CTX *) enif_make_new_binary(env, MD4_CTX_LEN, &ret));
    return ret;
}
static ERL_NIF_TERM md4_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    MD4_CTX* new_ctx;
    ErlNifBinary ctx_bin, data_bin;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != MD4_CTX_LEN
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
	return enif_make_badarg(env);
    }
    new_ctx = (MD4_CTX*) enif_make_new_binary(env,MD4_CTX_LEN, &ret);
    memcpy(new_ctx, ctx_bin.data, MD4_CTX_LEN);
    MD4_Update(new_ctx, data_bin.data, data_bin.size);
    return ret;
}
static ERL_NIF_TERM md4_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
    ErlNifBinary ctx_bin;
    MD4_CTX ctx_clone;
    ERL_NIF_TERM ret;    
    if (!enif_inspect_binary(env, argv[0], &ctx_bin) || ctx_bin.size != MD4_CTX_LEN) {
	return enif_make_badarg(env);
    }
    memcpy(&ctx_clone, ctx_bin.data, MD4_CTX_LEN); /* writable */
    MD4_Final(enif_make_new_binary(env, MD4_LEN, &ret), &ctx_clone);    
    return ret;
}

static ERL_NIF_TERM md5_mac_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Data, MacSize) */
    unsigned char hmacbuf[SHA_DIGEST_LENGTH];
    ErlNifBinary key, data;
    unsigned mac_sz;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data)
	|| !enif_get_uint(env,argv[2],&mac_sz) || mac_sz > MD5_LEN) {
	return enif_make_badarg(env);
    }
    hmac_md5(key.data, key.size, data.data, data.size, hmacbuf);
    memcpy(enif_make_new_binary(env, mac_sz, &ret), hmacbuf, mac_sz);
    return ret;
}

static ERL_NIF_TERM sha_mac_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Data, MacSize) */
    unsigned char hmacbuf[SHA_DIGEST_LENGTH];
    ErlNifBinary key, data;
    unsigned mac_sz;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data)
	|| !enif_get_uint(env,argv[2],&mac_sz) || mac_sz > SHA_LEN) {
	return enif_make_badarg(env);
    }
    hmac_sha1(key.data, key.size, data.data, data.size, hmacbuf);
    memcpy(enif_make_new_binary(env, mac_sz, &ret),
	   hmacbuf, mac_sz);
    return ret;
}

static ERL_NIF_TERM sha224_mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Data, MacSize) */
#ifdef HAVE_SHA224
    unsigned char hmacbuf[SHA224_DIGEST_LENGTH];
    ErlNifBinary key, data;
    unsigned mac_sz;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data)
	|| !enif_get_uint(env,argv[2],&mac_sz) || mac_sz > SHA224_DIGEST_LENGTH) {
	return enif_make_badarg(env);
    }
    hmac_sha224(key.data, key.size, data.data, data.size, hmacbuf);
    memcpy(enif_make_new_binary(env, mac_sz, &ret),
	   hmacbuf, mac_sz);
    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM sha256_mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Data, MacSize) */
#ifdef HAVE_SHA256
    unsigned char hmacbuf[SHA256_DIGEST_LENGTH];
    ErlNifBinary key, data;
    unsigned mac_sz;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data)
	|| !enif_get_uint(env,argv[2],&mac_sz) || mac_sz > SHA256_DIGEST_LENGTH) {
	return enif_make_badarg(env);
    }
    hmac_sha256(key.data, key.size, data.data, data.size, hmacbuf);
    memcpy(enif_make_new_binary(env, mac_sz, &ret),
	   hmacbuf, mac_sz);
    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM sha384_mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Data, MacSize) */
#ifdef HAVE_SHA384
    unsigned char hmacbuf[SHA384_DIGEST_LENGTH];
    ErlNifBinary key, data;
    unsigned mac_sz;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data)
	|| !enif_get_uint(env,argv[2],&mac_sz) || mac_sz > SHA384_DIGEST_LENGTH) {
	return enif_make_badarg(env);
    }
    hmac_sha384(key.data, key.size, data.data, data.size, hmacbuf);
    memcpy(enif_make_new_binary(env, mac_sz, &ret),
	   hmacbuf, mac_sz);
    return ret;
#else
    return atom_notsup;
#endif
}


static ERL_NIF_TERM sha512_mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Data, MacSize) */
#ifdef HAVE_SHA512
    unsigned char hmacbuf[SHA512_DIGEST_LENGTH];
    ErlNifBinary key, data;
    unsigned mac_sz;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data)
	|| !enif_get_uint(env,argv[2],&mac_sz) || mac_sz > SHA512_DIGEST_LENGTH) {
	return enif_make_badarg(env);
    }
    hmac_sha512(key.data, key.size, data.data, data.size, hmacbuf);
    memcpy(enif_make_new_binary(env, mac_sz, &ret),
	   hmacbuf, mac_sz);
    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM hmac_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key) */
    ErlNifBinary key;
    ERL_NIF_TERM ret;
    unsigned char * ctx_buf;
    const EVP_MD *md;
    
    if (argv[0] == atom_sha) md = EVP_sha1();
#ifdef HAVE_SHA224
    else if (argv[0] == atom_sha224) md = EVP_sha224();
#endif
#ifdef HAVE_SHA256
    else if (argv[0] == atom_sha256) md = EVP_sha256();
#endif
#ifdef HAVE_SHA384
    else if (argv[0] == atom_sha384) md = EVP_sha384();
#endif
#ifdef HAVE_SHA512
    else if (argv[0] == atom_sha512) md = EVP_sha512();
#endif
    else if (argv[0] == atom_md5) md = EVP_md5();
    else if (argv[0] == atom_ripemd160) md = EVP_ripemd160();
    else goto badarg;
    
    if (!enif_inspect_iolist_as_binary(env, argv[1], &key)) {
    badarg:
	return enif_make_badarg(env);
    }

    ctx_buf = enif_make_new_binary(env, sizeof(HMAC_CTX), &ret);
    HMAC_CTX_init((HMAC_CTX *) ctx_buf);
    HMAC_Init((HMAC_CTX *) ctx_buf, key.data, key.size, md);

    return ret;
}

static ERL_NIF_TERM hmac_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    ErlNifBinary context, data;
    ERL_NIF_TERM ret;
    unsigned char * ctx_buf;
    
    if (!enif_inspect_binary(env, argv[0], &context)
        || !enif_inspect_iolist_as_binary(env, argv[1], &data)
        || context.size != sizeof(HMAC_CTX)) {
	return enif_make_badarg(env);
    }

    ctx_buf = enif_make_new_binary(env, sizeof(HMAC_CTX), &ret);
    memcpy(ctx_buf, context.data, context.size);
    HMAC_Update((HMAC_CTX *)ctx_buf, data.data, data.size);

    return ret;
}

static ERL_NIF_TERM hmac_final(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) or (Context, HashLen) */
    ErlNifBinary context;
    ERL_NIF_TERM ret;
    HMAC_CTX ctx;
    unsigned char mac_buf[EVP_MAX_MD_SIZE];
    unsigned char * mac_bin;
    unsigned int req_len = 0;
    unsigned int mac_len;
    
    if (!enif_inspect_binary(env, argv[0], &context)) {
	return enif_make_badarg(env);
    }
    if (argc == 2 && !enif_get_uint(env, argv[1], &req_len)) {
	return enif_make_badarg(env);
    }

    if (context.size != sizeof(ctx)) {
        return enif_make_badarg(env);
    }
    memcpy(&ctx, context.data, context.size);
    
    HMAC_Final(&ctx, mac_buf, &mac_len);
    HMAC_CTX_cleanup(&ctx);

    if (argc == 2 && req_len < mac_len) { 
        // Only truncate to req_len bytes if asked.
        mac_len = req_len;
    }
    mac_bin = enif_make_new_binary(env, mac_len, &ret);
    memcpy(mac_bin, mac_buf, mac_len);

    return ret;
}

static ERL_NIF_TERM des_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Ivec, Text, IsEncrypt) */    
    ErlNifBinary key, ivec, text;
    DES_key_schedule schedule;
    DES_cblock ivec_clone; /* writable copy */
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key) || key.size != 8
	|| !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[2], &text)
	|| text.size % 8 != 0) {
	return enif_make_badarg(env);
    }
    memcpy(&ivec_clone, ivec.data, 8);
    DES_set_key((const_DES_cblock*)key.data, &schedule);
    DES_ncbc_encrypt(text.data, enif_make_new_binary(env, text.size, &ret),
		     text.size, &schedule, &ivec_clone, (argv[3] == atom_true));
    return ret;
}

static ERL_NIF_TERM des_cfb_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Ivec, Text, IsEncrypt) */
    ErlNifBinary key, ivec, text;
    DES_key_schedule schedule;
    DES_cblock ivec_clone; /* writable copy */
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key) || key.size != 8
	|| !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[2], &text)) {
	return enif_make_badarg(env);
    }
    memcpy(&ivec_clone, ivec.data, 8);
    DES_set_key((const_DES_cblock*)key.data, &schedule);
    DES_cfb_encrypt(text.data, enif_make_new_binary(env, text.size, &ret),
		     8, text.size, &schedule, &ivec_clone, (argv[3] == atom_true));
    return ret;
}

static ERL_NIF_TERM des_ecb_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Text/Cipher, IsEncrypt) */
    ErlNifBinary key, text;
    DES_key_schedule schedule;
    ERL_NIF_TERM ret;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &key) || key.size != 8 ||
	!enif_inspect_iolist_as_binary(env, argv[1], &text) || text.size != 8) {
	return enif_make_badarg(env);
    }
    DES_set_key((const_DES_cblock*)key.data, &schedule);
    DES_ecb_encrypt((const_DES_cblock*)text.data,
		    (DES_cblock*)enif_make_new_binary(env, 8, &ret),
		    &schedule, (argv[2] == atom_true));
    return ret;
}

static ERL_NIF_TERM des_ede3_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key1, Key2, Key3, IVec, Text/Cipher, IsEncrypt) */    
    ErlNifBinary key1, key2, key3, ivec, text;
    DES_key_schedule schedule1, schedule2, schedule3;
    DES_cblock ivec_clone; /* writable copy */
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key1) || key1.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[1], &key2) || key2.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[2], &key3) || key3.size != 8
	|| !enif_inspect_binary(env, argv[3], &ivec) || ivec.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[4], &text)
	|| text.size % 8 != 0) {
	return enif_make_badarg(env);
    }

    memcpy(&ivec_clone, ivec.data, 8);
    DES_set_key((const_DES_cblock*)key1.data, &schedule1);
    DES_set_key((const_DES_cblock*)key2.data, &schedule2);
    DES_set_key((const_DES_cblock*)key3.data, &schedule3);
    DES_ede3_cbc_encrypt(text.data, enif_make_new_binary(env,text.size,&ret), 
			 text.size, &schedule1, &schedule2, &schedule3,
			 &ivec_clone, (argv[5] == atom_true));
    return ret;
}

static ERL_NIF_TERM des_ede3_cfb_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key1, Key2, Key3, IVec, Text/Cipher, IsEncrypt) */
#ifdef HAVE_DES_ede3_cfb_encrypt
    ErlNifBinary key1, key2, key3, ivec, text;
    DES_key_schedule schedule1, schedule2, schedule3;
    DES_cblock ivec_clone; /* writable copy */
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key1) || key1.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[1], &key2) || key2.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[2], &key3) || key3.size != 8
	|| !enif_inspect_binary(env, argv[3], &ivec) || ivec.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[4], &text)) {
	return enif_make_badarg(env);
    }

    memcpy(&ivec_clone, ivec.data, 8);
    DES_set_key((const_DES_cblock*)key1.data, &schedule1);
    DES_set_key((const_DES_cblock*)key2.data, &schedule2);
    DES_set_key((const_DES_cblock*)key3.data, &schedule3);
    DES_ede3_cfb_encrypt(text.data, enif_make_new_binary(env,text.size,&ret),
			 8, text.size, &schedule1, &schedule2, &schedule3,
			 &ivec_clone, (argv[5] == atom_true));
    return ret;
#else
    return atom_notsup;
#endif
}

static ERL_NIF_TERM aes_cfb_128_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */    
    ErlNifBinary key, ivec, text;
    AES_KEY aes_key;
    unsigned char ivec_clone[16]; /* writable copy */
    int new_ivlen = 0;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key) || key.size != 16
	|| !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 16
	|| !enif_inspect_iolist_as_binary(env, argv[2], &text)) {
	return enif_make_badarg(env);
    }

    memcpy(ivec_clone, ivec.data, 16);
    AES_set_encrypt_key(key.data, 128, &aes_key);
    AES_cfb128_encrypt((unsigned char *) text.data,
		       enif_make_new_binary(env, text.size, &ret), 
		       text.size, &aes_key, ivec_clone, &new_ivlen,
		       (argv[3] == atom_true));
    return ret;
}

/* Common for both encrypt and decrypt
*/
static ERL_NIF_TERM aes_ctr_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data) */    
    ErlNifBinary key, ivec, text;
    AES_KEY aes_key;
    unsigned char ivec_clone[16]; /* writable copy */
    unsigned char ecount_buf[AES_BLOCK_SIZE];
    unsigned int num = 0;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
	|| AES_set_encrypt_key(key.data, key.size*8, &aes_key) != 0
	|| !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 16
	|| !enif_inspect_iolist_as_binary(env, argv[2], &text)) {
	return enif_make_badarg(env);
    }
    memcpy(ivec_clone, ivec.data, 16);    
    memset(ecount_buf, 0, sizeof(ecount_buf));
    AES_ctr128_encrypt((unsigned char *) text.data,
		       enif_make_new_binary(env, text.size, &ret), 
		       text.size, &aes_key, ivec_clone, ecount_buf, &num);

    /* To do an incremental {en|de}cryption, the state to to keep between calls
	must include ivec_clone, ecount_buf and num. */
    return ret;
}

/* Initializes state for ctr streaming (de)encryption
*/
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
    return ret;
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

static ERL_NIF_TERM rand_bytes_3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Bytes, TopMask, BottomMask) */    
    unsigned bytes;
    unsigned char* data;
    unsigned top_mask, bot_mask;
    ERL_NIF_TERM ret;
    if (!enif_get_uint(env, argv[0], &bytes)
	|| !enif_get_uint(env, argv[1], &top_mask)
	|| !enif_get_uint(env, argv[2], &bot_mask)) {
	return enif_make_badarg(env);
    }
    data = enif_make_new_binary(env, bytes, &ret);
    RAND_pseudo_bytes(data, bytes);
    ERL_VALGRIND_MAKE_MEM_DEFINED(data, bytes);
    if (bytes > 0) {
	data[bytes-1] |= top_mask;
	data[0] |= bot_mask;
    }
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
{/* (Base,Exponent,Modulo) */
    BIGNUM *bn_base=NULL, *bn_exponent=NULL, *bn_modulo, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;      
    ERL_NIF_TERM ret;

    if (!get_bn_from_mpint(env, argv[0], &bn_base)
	|| !get_bn_from_mpint(env, argv[1], &bn_exponent)
	|| !get_bn_from_mpint(env, argv[2], &bn_modulo)) {

	if (bn_base) BN_free(bn_base);
	if (bn_exponent) BN_free(bn_exponent);
	return enif_make_badarg(env);
    }
    bn_result = BN_new();
    bn_ctx = BN_CTX_new();
    BN_mod_exp(bn_result, bn_base, bn_exponent, bn_modulo, bn_ctx);
    dlen = BN_num_bytes(bn_result);
    ptr = enif_make_new_binary(env, dlen+4, &ret);
    put_int32(ptr, dlen);
    BN_bn2bin(bn_result, ptr+4);
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);
    BN_free(bn_modulo);
    BN_free(bn_exponent);
    BN_free(bn_base);
    return ret;
}

static int inspect_mpint(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifBinary* bin)
{    
    return enif_inspect_binary(env, term, bin) &&
    bin->size >= 4 && get_int32(bin->data) == bin->size-4;
}

static ERL_NIF_TERM dss_verify(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (DigestType|none, Data|{digest,Digest}, Signature,Key=[P, Q, G, Y]) */
    ErlNifBinary data_bin, sign_bin;
    BIGNUM *dsa_p = NULL, *dsa_q = NULL, *dsa_g = NULL, *dsa_y = NULL;
    unsigned char hmacbuf[SHA_DIGEST_LENGTH];
    unsigned char* digest;
    ERL_NIF_TERM head, tail;
    const ERL_NIF_TERM* tpl_terms;
    int tpl_arity;
    DSA *dsa;
    int i;

    if (argv[0] == atom_sha) {
	if (enif_get_tuple(env, argv[1], &tpl_arity, &tpl_terms)) {
	    if (tpl_arity != 2 || tpl_terms[0] != atom_digest
		|| !enif_inspect_binary(env, tpl_terms[1], &data_bin)
		|| data_bin.size != SHA_DIGEST_LENGTH) {

		return enif_make_badarg(env);
	    }
	    digest = data_bin.data;
	}
	else {
	    if (!inspect_mpint(env, argv[1], &data_bin)) {
		return enif_make_badarg(env);
	    }
	    SHA1(data_bin.data+4, data_bin.size-4, hmacbuf);
	    digest = hmacbuf;
	}
    }
    else if (argv[0] == atom_none && enif_inspect_binary(env, argv[1], &data_bin)
	     && data_bin.size == SHA_DIGEST_LENGTH) {
	digest = data_bin.data;
    }
    else {
	return enif_make_badarg(env);
    }

    if (!inspect_mpint(env, argv[2], &sign_bin)
	|| !enif_get_list_cell(env, argv[3], &head, &tail)
	|| !get_bn_from_mpint(env, head, &dsa_p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &dsa_q)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &dsa_g)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &dsa_y)
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
    i =  DSA_verify(0, digest, SHA_DIGEST_LENGTH,
		    sign_bin.data+4, sign_bin.size-4, dsa);
    DSA_free(dsa);
    return(i > 0) ? atom_true : atom_false;
}


static void md5_digest(unsigned char* in, unsigned int in_len, unsigned char* out)
{
    MD5(in, in_len, out);
}
static void sha1_digest(unsigned char* in, unsigned int in_len, unsigned char* out)
{
    SHA1(in, in_len, out);
}
#ifdef HAVE_SHA224
static void sha224_digest(unsigned char* in, unsigned int in_len, unsigned char* out)
{
    SHA224(in, in_len, out);
}
#endif
#ifdef HAVE_SHA256
static void sha256_digest(unsigned char* in, unsigned int in_len, unsigned char* out)
{
    SHA256(in, in_len, out);
}
#endif
#ifdef HAVE_SHA384
static void sha384_digest(unsigned char* in, unsigned int in_len, unsigned char* out)
{
    SHA384(in, in_len, out);
}
#endif
#ifdef HAVE_SHA512
static void sha512_digest(unsigned char* in, unsigned int in_len, unsigned char* out)
{
    SHA512(in, in_len, out);
}
#endif

struct digest_type_t {
    const char* type_str;
    unsigned len; /* 0 if notsup */
    int NID_type;
    void (*funcp)(unsigned char* in, unsigned int in_len, unsigned char* out);
    ERL_NIF_TERM type_atom;
};

struct digest_type_t digest_types[] =
{
    {"md5", MD5_DIGEST_LENGTH, NID_md5, md5_digest},
    {"sha", SHA_DIGEST_LENGTH, NID_sha1, sha1_digest},
    {"sha224",
#ifdef HAVE_SHA224
     SHA224_LEN, NID_sha224, sha224_digest
#else
      0
#endif
    },
    {"sha256",
#ifdef HAVE_SHA256
     SHA256_LEN, NID_sha256, sha256_digest
#else
      0
#endif
    },
    {"sha384",
#ifdef HAVE_SHA384
     SHA384_LEN, NID_sha384, sha384_digest
#else
     0
#endif
    },
    {"sha512",
#ifdef HAVE_SHA512
     SHA512_LEN, NID_sha512, sha512_digest
#else
     0
#endif
    },
    {NULL}
};

static void init_digest_types(ErlNifEnv* env)
{
    struct digest_type_t* p = digest_types;

    for (p = digest_types; p->type_str; p++) {
	p->type_atom = enif_make_atom(env, p->type_str);
    }

}

static struct digest_type_t* get_digest_type(ERL_NIF_TERM type)
{
    struct digest_type_t* p = NULL;
    for (p = digest_types; p->type_str; p++) {
	if (type == p->type_atom) {
	    return p;
	}
    }
    return NULL;
}

static ERL_NIF_TERM rsa_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Data|{digest,Digest}, Signature, Key=[E,N]) */
    ErlNifBinary data_bin, sign_bin;
    unsigned char hmacbuf[SHA512_LEN];
    ERL_NIF_TERM head, tail, ret;
    int i;
    RSA* rsa;
    const ERL_NIF_TERM type = argv[0];
    const ERL_NIF_TERM* tpl_terms;
    int tpl_arity;
    struct digest_type_t* digp = NULL;
    unsigned char* digest = NULL;

    digp = get_digest_type(type);
    if (!digp) {
	return enif_make_badarg(env);
    }
    if (!digp->len) {
	return atom_notsup;
    }

    rsa = RSA_new();

    if (!inspect_mpint(env, argv[2], &sign_bin)
	|| !enif_get_list_cell(env, argv[3], &head, &tail)
	|| !get_bn_from_mpint(env, head, &rsa->e)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &rsa->n)
	|| !enif_is_empty_list(env, tail)) {
	
	ret = enif_make_badarg(env);
	goto done;
    }
    if (enif_get_tuple(env, argv[1], &tpl_arity, &tpl_terms)) {
	if (tpl_arity != 2 || tpl_terms[0] != atom_digest
	    || !enif_inspect_binary(env, tpl_terms[1], &data_bin)
	    || data_bin.size != digp->len) {

	    ret = enif_make_badarg(env);
	    goto done;
	}
	digest = data_bin.data;
    }
    else if (inspect_mpint(env, argv[1], &data_bin)) {
	digest = hmacbuf;
	digp->funcp(data_bin.data+4, data_bin.size-4, digest);
    }
    else {
	ret = enif_make_badarg(env);
	goto done;
    }

    i = RSA_verify(digp->NID_type, digest, digp->len,
		   sign_bin.data+4, sign_bin.size-4, rsa);

    ret = (i==1 ? atom_true : atom_false);

done:
    RSA_free(rsa);
    return ret;
}


static ERL_NIF_TERM aes_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */
    ErlNifBinary key_bin, ivec_bin, data_bin;
    AES_KEY aes_key;
    unsigned char ivec[16];
    int i;
    unsigned char* ret_ptr;
    ERL_NIF_TERM ret;    

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
	|| (key_bin.size != 16 && key_bin.size != 32)
	|| !enif_inspect_binary(env, argv[1], &ivec_bin)
	|| ivec_bin.size != 16
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
    memcpy(ivec, ivec_bin.data, 16); /* writable copy */
    AES_cbc_encrypt(data_bin.data, ret_ptr, data_bin.size, &aes_key, ivec, i);
    return ret;
}

static ERL_NIF_TERM exor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

    return enif_make_tuple2(env,new_state,new_data);
}   

static ERL_NIF_TERM rc2_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key,IVec,Data,IsEncrypt) */
    ErlNifBinary key_bin, ivec_bin, data_bin;
    RC2_KEY rc2_key;
    ERL_NIF_TERM ret;
    unsigned char iv_copy[8];
    
    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
	|| (key_bin.size != 5 && key_bin.size != 8 && key_bin.size != 16)
	|| !enif_inspect_binary(env, argv[1], &ivec_bin)
	|| ivec_bin.size != 8
	|| !enif_inspect_iolist_as_binary(env, argv[2], &data_bin)
	|| data_bin.size % 8 != 0) {
	return enif_make_badarg(env);
    }
    
    RC2_set_key(&rc2_key, key_bin.size, key_bin.data, key_bin.size*8);
    memcpy(iv_copy, ivec_bin.data, 8);
    RC2_cbc_encrypt(data_bin.data,
		    enif_make_new_binary(env, data_bin.size, &ret),
		    data_bin.size, &rc2_key,
		    iv_copy,
		    (argv[3] == atom_true));
    return ret;
}

static int get_rsa_private_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa)
{
    /* key=[E,N,D]|[E,N,D,P1,P2,E1,E2,C] */
    ERL_NIF_TERM head, tail;

    if (!enif_get_list_cell(env, key, &head, &tail)
	|| !get_bn_from_mpint(env, head, &rsa->e)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &rsa->n)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &rsa->d)
	|| (!enif_is_empty_list(env, tail) &&
	    (!enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_mpint(env, head, &rsa->p)
	     || !enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_mpint(env, head, &rsa->q)
	     || !enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_mpint(env, head, &rsa->dmp1)
	     || !enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_mpint(env, head, &rsa->dmq1)
	     || !enif_get_list_cell(env, tail, &head, &tail)
	     || !get_bn_from_mpint(env, head, &rsa->iqmp)
	     || !enif_is_empty_list(env, tail)))) {
	return 0;
    }
    return 1;
}

static ERL_NIF_TERM rsa_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Data|{digest,Digest}, Key=[E,N,D]|[E,N,D,P1,P2,E1,E2,C]) */
    ErlNifBinary data_bin, ret_bin;
    unsigned char hmacbuf[SHA512_LEN];
    unsigned rsa_s_len;
    RSA* rsa;
    int i;
    const ERL_NIF_TERM* tpl_terms;
    int tpl_arity;
    struct digest_type_t *digp;
    unsigned char* digest;

    digp = get_digest_type(argv[0]);
    if (!digp) {
	return enif_make_badarg(env);
    }
    if (!digp->len) {
	return atom_notsup;
    }

    if (enif_get_tuple(env, argv[1], &tpl_arity, &tpl_terms)) {
	if (tpl_arity != 2 || tpl_terms[0] != atom_digest
	    || !enif_inspect_binary(env, tpl_terms[1], &data_bin)
	    || data_bin.size != digp->len) {

	    return enif_make_badarg(env);
	}
	digest = data_bin.data;
    }
    else {
	if (!inspect_mpint(env,argv[1],&data_bin)) {
	    return enif_make_badarg(env);
	}
	digest = hmacbuf;
	digp->funcp(data_bin.data+4, data_bin.size-4, digest);
    }

    rsa = RSA_new();
    if (!get_rsa_private_key(env, argv[2], rsa)) {
	RSA_free(rsa);
	return enif_make_badarg(env);
    }


    enif_alloc_binary(RSA_size(rsa), &ret_bin);

    ERL_VALGRIND_ASSERT_MEM_DEFINED(digest, digp->len);
    i =  RSA_sign(digp->NID_type, digest, digp->len,
		  ret_bin.data, &rsa_s_len, rsa);

    RSA_free(rsa);
    if (i) {
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
{/* (DigesType|none, Data|{digest,Digest}, Key=[P,Q,G,PrivKey]) */
    ErlNifBinary data_bin, ret_bin;
    ERL_NIF_TERM head, tail;
    unsigned char hmacbuf[SHA_DIGEST_LENGTH];
    unsigned int dsa_s_len;
    const ERL_NIF_TERM* tpl_terms;
    int tpl_arity;
    unsigned char* digest = NULL;
    DSA* dsa;
    int i;

    if (argv[0] == atom_sha) {
	if (enif_get_tuple(env, argv[1], &tpl_arity, &tpl_terms)) {
	    if (tpl_arity != 2 || tpl_terms[0] != atom_digest
		|| !enif_inspect_binary(env, tpl_terms[1], &data_bin)
		|| data_bin.size != SHA_DIGEST_LENGTH) {

		return enif_make_badarg(env);
	    }
	    digest = data_bin.data;
	}
	else {
	    if (!inspect_mpint(env,argv[1],&data_bin)) {
		return enif_make_badarg(env);
	    }
	    SHA1(data_bin.data+4, data_bin.size-4, hmacbuf);
	    digest = hmacbuf;
	}
    }
    else if (argv[0] == atom_none
	     && enif_inspect_binary(env,argv[1],&data_bin)
	     && data_bin.size == SHA_DIGEST_LENGTH) {

	digest = data_bin.data;
    }
    else {
	return enif_make_badarg(env);
    }

    dsa = DSA_new();

    dsa->pub_key  = NULL;
    if (!enif_get_list_cell(env, argv[2], &head, &tail)
	|| !get_bn_from_mpint(env, head, &dsa->p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &dsa->q)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &dsa->g)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &dsa->priv_key)
	|| !enif_is_empty_list(env,tail)) {
	DSA_free(dsa);
	return enif_make_badarg(env);
    }

    enif_alloc_binary(DSA_size(dsa), &ret_bin);
    i =  DSA_sign(NID_sha1, digest, SHA_DIGEST_LENGTH,
		  ret_bin.data, &dsa_s_len, dsa);
    DSA_free(dsa);
    if (i) {
	if (dsa_s_len != ret_bin.size) {
	    enif_realloc_binary(&ret_bin, dsa_s_len);
	}
	return enif_make_binary(env, &ret_bin);
    }
    else {
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
    RSA* rsa = RSA_new();

    if (!enif_inspect_binary(env, argv[0], &data_bin)
	|| !enif_get_list_cell(env, argv[1], &head, &tail)
	|| !get_bn_from_mpint(env, head, &rsa->e)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &rsa->n)
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
    RSA* rsa = RSA_new();

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
    p_ptr = enif_make_new_binary(env, p_len+4, &ret_p);
    g_ptr = enif_make_new_binary(env, g_len+4, &ret_g);
    put_int32(p_ptr, p_len);
    put_int32(g_ptr, g_len);
    BN_bn2bin(dh_params->p, p_ptr+4);
    BN_bn2bin(dh_params->g, g_ptr+4);
    ERL_VALGRIND_MAKE_MEM_DEFINED(p_ptr+4, p_len);                
    ERL_VALGRIND_MAKE_MEM_DEFINED(g_ptr+4, g_len);
    DH_free(dh_params);
    return enif_make_list2(env, ret_p, ret_g);    
}

static ERL_NIF_TERM dh_check(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* ([PrimeLen, Generator]) */
    DH* dh_params = DH_new();
    int i;
    ERL_NIF_TERM ret, head, tail;

    if (!enif_get_list_cell(env, argv[0], &head, &tail)   
	|| !get_bn_from_mpint(env, head, &dh_params->p)
	|| !enif_get_list_cell(env, tail, &head, &tail)   
	|| !get_bn_from_mpint(env, head, &dh_params->g)
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
{/* (PrivKey, DHParams=[P,G]) */
    DH* dh_params = DH_new();
    int pub_len, prv_len;
    unsigned char *pub_ptr, *prv_ptr;
    ERL_NIF_TERM ret, ret_pub, ret_prv, head, tail;

    if (!(get_bn_from_mpint(env, argv[0], &dh_params->priv_key)
	  || argv[0] == atom_undefined)
	|| !enif_get_list_cell(env, argv[1], &head, &tail)
	|| !get_bn_from_mpint(env, head, &dh_params->p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &dh_params->g)
	|| !enif_is_empty_list(env, tail)) {
	DH_free(dh_params);
	return enif_make_badarg(env);
    }

    if (DH_generate_key(dh_params)) {
	pub_len = BN_num_bytes(dh_params->pub_key);
	prv_len = BN_num_bytes(dh_params->priv_key);    
	pub_ptr = enif_make_new_binary(env, pub_len+4, &ret_pub);
	prv_ptr = enif_make_new_binary(env, prv_len+4, &ret_prv);
	put_int32(pub_ptr, pub_len);
	put_int32(prv_ptr, prv_len);
	BN_bn2bin(dh_params->pub_key, pub_ptr+4);
	BN_bn2bin(dh_params->priv_key, prv_ptr+4);
	ERL_VALGRIND_MAKE_MEM_DEFINED(pub_ptr+4, pub_len);    
	ERL_VALGRIND_MAKE_MEM_DEFINED(prv_ptr+4, prv_len);
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
    DH* dh_params = DH_new();
    BIGNUM* pubkey = NULL;
    int i;
    ErlNifBinary ret_bin;
    ERL_NIF_TERM ret, head, tail;

    if (!get_bn_from_mpint(env, argv[0], &pubkey)
	|| !get_bn_from_mpint(env, argv[1], &dh_params->priv_key)       
	|| !enif_get_list_cell(env, argv[2], &head, &tail)
	|| !get_bn_from_mpint(env, head, &dh_params->p)
	|| !enif_get_list_cell(env, tail, &head, &tail)
	|| !get_bn_from_mpint(env, head, &dh_params->g)
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
	    ret = atom_error;
	}
    }
    if (pubkey) BN_free(pubkey);
    DH_free(dh_params);
    return ret;
}

static ERL_NIF_TERM bf_cfb64_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])    
{/* (Key, Ivec, Data, IsEncrypt) */
    ErlNifBinary key_bin, ivec_bin, data_bin;
    BF_KEY bf_key; /* blowfish key 8 */
    unsigned char bf_tkey[8]; /* blowfish ivec */    
    int bf_n = 0; /* blowfish ivec pos */
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
	|| !enif_inspect_binary(env, argv[1], &ivec_bin)
	|| ivec_bin.size != 8 
	|| !enif_inspect_iolist_as_binary(env, argv[2], &data_bin)) {
	return enif_make_badarg(env);
    }

    BF_set_key(&bf_key, key_bin.size, key_bin.data);
    memcpy(bf_tkey, ivec_bin.data, 8);
    BF_cfb64_encrypt(data_bin.data, enif_make_new_binary(env,data_bin.size,&ret),
		     data_bin.size, &bf_key, bf_tkey, &bf_n,
		     (argv[3] == atom_true ? BF_ENCRYPT : BF_DECRYPT));
    return ret;
}

static ERL_NIF_TERM bf_cbc_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Ivec, Data, IsEncrypt) */
    ErlNifBinary key_bin, ivec_bin, data_bin;
    BF_KEY bf_key; /* blowfish key 8 */
    unsigned char bf_tkey[8]; /* blowfish ivec */    
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
	|| !enif_inspect_binary(env, argv[1], &ivec_bin)
	|| ivec_bin.size != 8 
	|| !enif_inspect_iolist_as_binary(env, argv[2], &data_bin)
	|| data_bin.size % 8 != 0) {
	return enif_make_badarg(env);
    }

    BF_set_key(&bf_key, key_bin.size, key_bin.data);
    memcpy(bf_tkey, ivec_bin.data, 8);
    BF_cbc_encrypt(data_bin.data, enif_make_new_binary(env,data_bin.size,&ret),
		   data_bin.size, &bf_key, bf_tkey,
		   (argv[3] == atom_true ? BF_ENCRYPT : BF_DECRYPT));
    return ret;
}

static ERL_NIF_TERM bf_ecb_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])    
{/* (Key, Data, IsEncrypt) */
    ErlNifBinary key_bin, data_bin;
    BF_KEY bf_key; /* blowfish key 8 */
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
	|| !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)
	|| data_bin.size < 8) {
	return enif_make_badarg(env);
    }
    BF_set_key(&bf_key, key_bin.size, key_bin.data);
    BF_ecb_encrypt(data_bin.data, enif_make_new_binary(env,data_bin.size,&ret),
		   &bf_key, (argv[2] == atom_true ? BF_ENCRYPT : BF_DECRYPT));
    return ret;
}

static ERL_NIF_TERM blowfish_ofb64_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])    
{/* (Key, IVec, Data) */
    ErlNifBinary key_bin, ivec_bin, data_bin;
    BF_KEY bf_key; /* blowfish key 8 */
    unsigned char bf_tkey[8]; /* blowfish ivec */    
    int bf_n = 0; /* blowfish ivec pos */
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
	|| !enif_inspect_binary(env, argv[1], &ivec_bin)
	|| ivec_bin.size != 8 
	|| !enif_inspect_iolist_as_binary(env, argv[2], &data_bin)) {
	return enif_make_badarg(env);
    }

    BF_set_key(&bf_key, key_bin.size, key_bin.data);
    memcpy(bf_tkey, ivec_bin.data, 8);
    BF_ofb64_encrypt(data_bin.data, enif_make_new_binary(env,data_bin.size,&ret),
		     data_bin.size, &bf_key, bf_tkey, &bf_n);
    return ret;
}




/* HMAC */

static void hmac_md5(unsigned char *key, int klen, unsigned char *dbuf, int dlen,
		     unsigned char *hmacbuf)
{
    MD5_CTX ctx;
    char ipad[HMAC_INT_LEN];
    char opad[HMAC_INT_LEN];
    unsigned char nkey[MD5_LEN];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT_LEN) {
	MD5(key, klen, nkey);
	key = nkey;
	klen = MD5_LEN;
    }

    memset(ipad, '\0', sizeof(ipad));
    memset(opad, '\0', sizeof(opad));
    memcpy(ipad, key, klen);
    memcpy(opad, key, klen);

    for (i = 0; i < HMAC_INT_LEN; i++) {
	ipad[i] ^= HMAC_IPAD;
	opad[i] ^= HMAC_OPAD;
    }

    /* inner MD5 */
    MD5_Init(&ctx);
    MD5_Update(&ctx, ipad, HMAC_INT_LEN);
    MD5_Update(&ctx, dbuf, dlen);
    MD5_Final((unsigned char *) hmacbuf, &ctx);
    /* outer MD5 */
    MD5_Init(&ctx);
    MD5_Update(&ctx, opad, HMAC_INT_LEN);
    MD5_Update(&ctx, hmacbuf, MD5_LEN);
    MD5_Final((unsigned char *) hmacbuf, &ctx);
}

static void hmac_sha1(unsigned char *key, int klen,
		      unsigned char *dbuf, int dlen, 
		      unsigned char *hmacbuf)
{
    SHA_CTX ctx;
    char ipad[HMAC_INT_LEN];
    char opad[HMAC_INT_LEN];
    unsigned char nkey[SHA_LEN];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT_LEN) {
	SHA1(key, klen, nkey);
	key = nkey;
	klen = SHA_LEN;
    }

    memset(ipad, '\0', sizeof(ipad));
    memset(opad, '\0', sizeof(opad));
    memcpy(ipad, key, klen);
    memcpy(opad, key, klen);

    for (i = 0; i < HMAC_INT_LEN; i++) {
	ipad[i] ^= HMAC_IPAD;
	opad[i] ^= HMAC_OPAD;
    }

    /* inner SHA */
    SHA1_Init(&ctx);
    SHA1_Update(&ctx, ipad, HMAC_INT_LEN);
    SHA1_Update(&ctx, dbuf, dlen);
    SHA1_Final((unsigned char *) hmacbuf, &ctx);
    /* outer SHA */
    SHA1_Init(&ctx);
    SHA1_Update(&ctx, opad, HMAC_INT_LEN);
    SHA1_Update(&ctx, hmacbuf, SHA_LEN);
    SHA1_Final((unsigned char *) hmacbuf, &ctx);
}

#ifdef HAVE_SHA224
static void hmac_sha224(unsigned char *key, int klen,
			unsigned char *dbuf, int dlen,
			unsigned char *hmacbuf)
{
    SHA256_CTX ctx;
    char ipad[HMAC_INT_LEN];
    char opad[HMAC_INT_LEN];
    unsigned char nkey[SHA224_DIGEST_LENGTH];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT_LEN) {
	SHA224(key, klen, nkey);
	key = nkey;
	klen = SHA224_DIGEST_LENGTH;
    }

    memset(ipad, '\0', sizeof(ipad));
    memset(opad, '\0', sizeof(opad));
    memcpy(ipad, key, klen);
    memcpy(opad, key, klen);

    for (i = 0; i < HMAC_INT_LEN; i++) {
	ipad[i] ^= HMAC_IPAD;
	opad[i] ^= HMAC_OPAD;
    }

    /* inner SHA */
    SHA224_Init(&ctx);
    SHA224_Update(&ctx, ipad, HMAC_INT_LEN);
    SHA224_Update(&ctx, dbuf, dlen);
    SHA224_Final((unsigned char *) hmacbuf, &ctx);
    /* outer SHA */
    SHA224_Init(&ctx);
    SHA224_Update(&ctx, opad, HMAC_INT_LEN);
    SHA224_Update(&ctx, hmacbuf, SHA224_DIGEST_LENGTH);
    SHA224_Final((unsigned char *) hmacbuf, &ctx);
}
#endif

#ifdef HAVE_SHA256
static void hmac_sha256(unsigned char *key, int klen,
			unsigned char *dbuf, int dlen,
			unsigned char *hmacbuf)
{
    SHA256_CTX ctx;
    char ipad[HMAC_INT_LEN];
    char opad[HMAC_INT_LEN];
    unsigned char nkey[SHA256_DIGEST_LENGTH];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT_LEN) {
	SHA256(key, klen, nkey);
	key = nkey;
	klen = SHA256_DIGEST_LENGTH;
    }

    memset(ipad, '\0', sizeof(ipad));
    memset(opad, '\0', sizeof(opad));
    memcpy(ipad, key, klen);
    memcpy(opad, key, klen);

    for (i = 0; i < HMAC_INT_LEN; i++) {
	ipad[i] ^= HMAC_IPAD;
	opad[i] ^= HMAC_OPAD;
    }

    /* inner SHA */
    SHA256_Init(&ctx);
    SHA256_Update(&ctx, ipad, HMAC_INT_LEN);
    SHA256_Update(&ctx, dbuf, dlen);
    SHA256_Final((unsigned char *) hmacbuf, &ctx);
    /* outer SHA */
    SHA256_Init(&ctx);
    SHA256_Update(&ctx, opad, HMAC_INT_LEN);
    SHA256_Update(&ctx, hmacbuf, SHA256_DIGEST_LENGTH);
    SHA256_Final((unsigned char *) hmacbuf, &ctx);
}
#endif

#ifdef HAVE_SHA384
static void hmac_sha384(unsigned char *key, int klen,
			unsigned char *dbuf, int dlen,
			unsigned char *hmacbuf)
{
    SHA512_CTX ctx;
    char ipad[HMAC_INT2_LEN];
    char opad[HMAC_INT2_LEN];
    unsigned char nkey[SHA384_DIGEST_LENGTH];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT2_LEN) {
	SHA384(key, klen, nkey);
	key = nkey;
	klen = SHA384_DIGEST_LENGTH;
    }

    memset(ipad, '\0', sizeof(ipad));
    memset(opad, '\0', sizeof(opad));
    memcpy(ipad, key, klen);
    memcpy(opad, key, klen);

    for (i = 0; i < HMAC_INT2_LEN; i++) {
	ipad[i] ^= HMAC_IPAD;
	opad[i] ^= HMAC_OPAD;
    }

    /* inner SHA */
    SHA384_Init(&ctx);
    SHA384_Update(&ctx, ipad, HMAC_INT2_LEN);
    SHA384_Update(&ctx, dbuf, dlen);
    SHA384_Final((unsigned char *) hmacbuf, &ctx);
    /* outer SHA */
    SHA384_Init(&ctx);
    SHA384_Update(&ctx, opad, HMAC_INT2_LEN);
    SHA384_Update(&ctx, hmacbuf, SHA384_DIGEST_LENGTH);
    SHA384_Final((unsigned char *) hmacbuf, &ctx);
}
#endif

#ifdef HAVE_SHA512
static void hmac_sha512(unsigned char *key, int klen,
			unsigned char *dbuf, int dlen,
			unsigned char *hmacbuf)
{
    SHA512_CTX ctx;
    char ipad[HMAC_INT2_LEN];
    char opad[HMAC_INT2_LEN];
    unsigned char nkey[SHA512_DIGEST_LENGTH];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT2_LEN) {
	SHA512(key, klen, nkey);
	key = nkey;
	klen = SHA512_DIGEST_LENGTH;
    }

    memset(ipad, '\0', sizeof(ipad));
    memset(opad, '\0', sizeof(opad));
    memcpy(ipad, key, klen);
    memcpy(opad, key, klen);

    for (i = 0; i < HMAC_INT2_LEN; i++) {
	ipad[i] ^= HMAC_IPAD;
	opad[i] ^= HMAC_OPAD;
    }

    /* inner SHA */
    SHA512_Init(&ctx);
    SHA512_Update(&ctx, ipad, HMAC_INT2_LEN);
    SHA512_Update(&ctx, dbuf, dlen);
    SHA512_Final((unsigned char *) hmacbuf, &ctx);
    /* outer SHA */
    SHA512_Init(&ctx);
    SHA512_Update(&ctx, opad, HMAC_INT2_LEN);
    SHA512_Update(&ctx, hmacbuf, SHA512_DIGEST_LENGTH);
    SHA512_Final((unsigned char *) hmacbuf, &ctx);
}
#endif
