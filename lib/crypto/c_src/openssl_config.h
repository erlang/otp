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

#ifndef E_OPENSSL_CONFIG_H__
#define E_OPENSSL_CONFIG_H__ 1

#define OPENSSL_THREAD_DEFINES
#include <openssl/opensslconf.h>
#include "openssl_version.h"

#include <openssl/crypto.h>
#include <openssl/des.h>

/* #include <openssl/idea.h> This is not supported on the openssl OTP requires */
#include <openssl/dh.h>
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
#include <openssl/err.h>

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(3,0,0)
# define HAS_3_0_API
# include <openssl/provider.h>
#endif

/* LibreSSL was cloned from OpenSSL 1.0.1g and claims to be API and BPI compatible
 * with 1.0.1.
 *
 * LibreSSL has the same names on include files and symbols as OpenSSL, but defines
 * the OPENSSL_VERSION_NUMBER to be >= 2.0.0
 *
 * Therefore works tests like this as intendend:
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

#ifdef HAS_LIBRESSL
/* LibreSSL dislikes FIPS */
#  undef FIPS_SUPPORT

# if LIBRESSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION_PLAIN(2,7,0)
/* LibreSSL wants the 1.0.1 API */
# define NEED_EVP_COMPATIBILITY_FUNCTIONS
# endif
#endif


/* LibreSSL has never supported the custom mem functions */
#ifndef HAS_LIBRESSL
/* Since f46401d46f9ed331ff2a09bb6a99376707083c96 this macro can NEVER have been enabled
 * because its inside an #ifdef HAS_LIBRESSL
 *
 * If I enable HAS_CRYPTO_MEM_FUNCTIONS, there are two lab machines that fails:
 *    SunOS mallor 5.11 illumos-2d990ab13b i86pc i386 i86pc  OpenSSL 1.0.2u  20 Dec 2019
 *    SunOS fingon 5.11 11.4.0.15.0 i86pc i386 i86pc         OpenSSL 1.0.2o  27 Mar 2018
 *
 * Therefore I don't want to enable this until further investigated
# define HAS_CRYPTO_MEM_FUNCTIONS
 */
#endif

#if OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION_PLAIN(1,1,0)
# define NEED_EVP_COMPATIBILITY_FUNCTIONS
#endif

#ifndef HAS_LIBRESSL
# if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,1,0)
#  define HAS_BN_bn2binpad
# endif
#endif

#ifndef HAS_LIBRESSL
# if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
#  define HAS_EVP_PKEY_CTX
#  define HAVE_EVP_CIPHER_CTX_COPY
# endif

# if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,1,1)
#   define HAVE_PKEY_new_raw_private_key
#   define HAVE_EVP_PKEY_new_CMAC_key
#   define HAVE_DigestSign_as_single_op
# endif
#endif

#ifdef HAS_LIBRESSL
# if LIBRESSL_VERSION_NUMBER >= 0x3050000fL
#  define HAS_EVP_PKEY_CTX
#  define HAVE_EVP_CIPHER_CTX_COPY
# endif
# if LIBRESSL_VERSION_NUMBER >= 0x3070200fL
#   define HAVE_PKEY_new_raw_private_key
# endif
# if LIBRESSL_VERSION_NUMBER >= 0x3030300fL
#   define HAVE_EVP_PKEY_new_CMAC_key
# endif
# if LIBRESSL_VERSION_NUMBER >= 0x3040100fL
#   define HAVE_DigestSign_as_single_op
# endif
#endif

#if defined(HAS_EVP_PKEY_CTX)                                           \
    && OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION_PLAIN(1,1,0)
     /* EVP is slow on antique crypto libs.
      * DISABLE_EVP_* is 0 or 1 from the configure script
      */
# undef  DISABLE_EVP_DH
# define DISABLE_EVP_DH 1
# undef  DISABLE_EVP_HMAC
# define DISABLE_EVP_HMAC 1
#endif

#ifdef HAS_3_0_API
/* Do not use the deprecated interface */
# undef  DISABLE_EVP_HMAC
# define DISABLE_EVP_HMAC 0
#endif


#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
#include <openssl/modes.h>
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

// SHA3:
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,1,1)
// An error in beta releases of 1.1.1 fixed in production release
# ifdef NID_sha3_224
#  define HAVE_SHA3_224
# endif
# ifdef NID_sha3_256
#  define HAVE_SHA3_256
# endif
# ifdef NID_shake128
#  define HAVE_SHAKE128
# endif
# ifdef NID_shake256
#  define HAVE_SHAKE256
# endif
#endif

# ifdef NID_sha3_384
#  define HAVE_SHA3_384
# endif
# ifdef NID_sha3_512
#  define HAVE_SHA3_512
# endif

// BLAKE2:
#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,1,1) \
    && !defined(HAS_LIBRESSL) \
    && !defined(OPENSSL_NO_BLAKE2)
# define HAVE_BLAKE2
#endif

#ifndef OPENSSL_NO_BF
# define HAVE_BF
#endif

#ifndef OPENSSL_NO_DES
# define HAVE_DES
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(0,9,7,'e')
# define HAVE_DES_ede3_cfb
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(0,9,7,'e')
# define HAVE_DES_ede3_cbc
#endif

#ifndef OPENSSL_NO_DH
# define HAVE_DH
#endif

#ifndef OPENSSL_NO_DSA
# define HAVE_DSA
#endif

#ifndef OPENSSL_NO_MD4
# define HAVE_MD4
#endif

#ifndef OPENSSL_NO_MD5
# define HAVE_MD5
#endif

#ifndef OPENSSL_NO_RC2
# define HAVE_RC2
#endif

#ifndef OPENSSL_NO_RC4
# define HAVE_RC4
#endif

#if !defined(OPENSSL_NO_RMD160) && \
    !defined(OPENSSL_NO_RIPEMD160) && \
    !defined(OPENSSL_NO_RIPEMD)
/* Note RMD160 vs RIPEMD160 */
# define HAVE_RIPEMD160
#endif


#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(0,9,8,'o') \
	&& !defined(OPENSSL_NO_EC) \
	&& !defined(OPENSSL_NO_ECDH) \
	&& !defined(OPENSSL_NO_ECDSA)
# define HAVE_EC
#endif

// (test for >= 1.1.1pre8)
#if OPENSSL_VERSION_NUMBER >= (PACKED_OPENSSL_VERSION_PLAIN(1,1,1) -7) \
    && !defined(HAS_LIBRESSL) \
    && defined(HAVE_EC)
# ifdef HAVE_DH
#   define HAVE_EDDH
# endif
# if OPENSSL_VERSION_NUMBER >= (PACKED_OPENSSL_VERSION_PLAIN(1,1,1))
#   define HAVE_EDDSA
# endif
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,1)
# define HAVE_EVP_AES_CTR
# define HAVE_AEAD
# define HAVE_GCM
# define HAVE_CCM
# ifndef OPENSSL_NO_CMAC
#   define HAVE_CMAC
# endif
# if defined(RSA_PKCS1_OAEP_PADDING)
#   define HAVE_RSA_OAEP_PADDING
# endif
# define HAVE_RSA_MGF1_MD
# if OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION(1,0,1,'d')
#  define HAVE_GCM_EVP_DECRYPT_BUG
# endif
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,1,0)
# ifndef HAS_LIBRESSL
#  if !defined(OPENSSL_NO_CHACHA) && !defined(OPENSSL_NO_POLY1305)
#    define HAVE_CHACHA20_POLY1305
#  endif
#  define HAVE_RSA_OAEP_MD
# endif
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(1,1,0,'d')
# ifndef HAS_LIBRESSL
#  ifndef OPENSSL_NO_CHACHA
#    define HAVE_CHACHA20
#  endif
# endif
#endif

// OPENSSL_VERSION_NUMBER >= 1.1.1-pre8
#if OPENSSL_VERSION_NUMBER >= (PACKED_OPENSSL_VERSION_PLAIN(1,1,1)-7)
# ifndef HAS_LIBRESSL
#  if !defined(OPENSSL_NO_POLY1305)
#    define HAVE_POLY1305
#  endif
# endif
#endif

#if OPENSSL_VERSION_NUMBER <= PACKED_OPENSSL_VERSION(0,9,8,'l')
# define HAVE_ECB_IVEC_BUG
# define HAVE_UPDATE_EMPTY_DATA_BUG
#endif

#ifndef HAS_LIBRESSL
# ifdef RSA_SSLV23_PADDING
#  define HAVE_RSA_SSLV23_PADDING
# endif
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
# ifdef RSA_PKCS1_PSS_PADDING
#  define HAVE_RSA_PKCS1_PSS_PADDING
# endif
# define HAS_PKCS5_PBKDF2_HMAC
#endif

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION(0,9,8,'h') \
    && defined(HAVE_EC)
/* If OPENSSL_NO_EC is set, there will be an error in ec.h included from engine.h
   So if EC is disabled, you can't use Engine either....
*/
#if !defined(OPENSSL_NO_ENGINE) && \
    !defined(HAS_3_0_API)
/* Disable FIPS for 3.0 temporaryly until the support is added (might core dump) */
# define HAS_ENGINE_SUPPORT
#endif
#endif


#if defined(HAS_ENGINE_SUPPORT)
# include <openssl/engine.h>
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

#define put_uint32(s,i) \
{ (s)[0] = (unsigned char)(((i) >> 24) & 0xff);\
  (s)[1] = (unsigned char)(((i) >> 16) & 0xff);\
  (s)[2] = (unsigned char)(((i) >> 8) & 0xff);\
  (s)[3] = (unsigned char)((i) & 0xff);\
}

/* This shall correspond to the similar macro in crypto.erl */
/* Current value is: erlang:system_info(context_reductions) * 10 */
#define MAX_BYTES_TO_NIF 20000

#ifdef HAS_3_0_API
# define MAX_NUM_PROVIDERS 10
#endif

#define CONSUME_REDS(NifEnv, Ibin)			\
do {                                                    \
    size_t _cost = (Ibin).size;                         \
    if (_cost > SIZE_MAX / 100)                         \
        _cost = 100;                                    \
    else                                                \
        _cost = (_cost * 100) / MAX_BYTES_TO_NIF;       \
                                                        \
    if (_cost) {                                        \
        (void) enif_consume_timeslice((NifEnv),		\
                (_cost > 100) ? 100 : (int)_cost);      \
    }                                                   \
 } while (0)

#ifdef NEED_EVP_COMPATIBILITY_FUNCTIONS
#  include "evp_compat.h"
#else
#  define HAVE_OPAQUE_BN_GENCB
#endif

#if 0
#  define PRINTF_ERR0(FMT)         enif_fprintf(stderr, FMT "\n")
#  define PRINTF_ERR1(FMT, A1)     enif_fprintf(stderr, FMT "\n", A1)
#  define PRINTF_ERR2(FMT, A1, A2) enif_fprintf(stderr, FMT "\n", A1, A2)
#else
#  define PRINTF_ERR0(FMT)
#  define PRINTF_ERR1(FMT,A1)
#  define PRINTF_ERR2(FMT,A1,A2)
#endif

#if defined(FIPS_SUPPORT) \
    && OPENSSL_VERSION_NUMBER  < PACKED_OPENSSL_VERSION_PLAIN(1,0,1)
/* FIPS is not supported for versions < 1.0.1.  If FIPS_SUPPORT is enabled
   there are some warnings/errors for thoose
*/
# undef FIPS_SUPPORT
#endif

/* Disable FIPS for 3.0 temporaryly until the support is added */
#if defined(FIPS_SUPPORT) &&                                            \
    defined(HAS_3_0_API)
# undef FIPS_SUPPORT
#endif

#if defined(FIPS_SUPPORT) && \
    defined(HAS_3_0_API)
# define FIPS_mode() EVP_default_properties_is_fips_enabled(NULL)
# define FIPS_mode_set(enable) EVP_default_properties_enable_fips(NULL, enable)
#endif


#if defined(FIPS_SUPPORT)
#  define FIPS_MODE() (FIPS_mode() ? 1 : 0)
#else
# define FIPS_MODE() 0
#endif

#ifdef HAS_3_0_API
/* Set CRYPTO_DEVELOP_ERRORS to make error messages more verbose,
   that is, include the error msg from cryptolib.
   Example:
      {error,{"api_ng.c",750},"Can't copy ctx_res"}
   becomes
      {error,{"api_ng.c",750},"Can't copy ctx_res: error:030000BE:digital envelope routines::not able to copy ctx"}
   which enables the developer to locate more in detail where in the cryptolib code a test failed.
*/

//# define CRYPTO_DEVELOP_ERRORS
#endif

#endif /* E_OPENSSL_CONFIG_H__ */
