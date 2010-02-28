/* 
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
 * Purpose:  Dynamically loadable driver for cryptography libraries. 
 * Based on OpenSSL. 
 */

#ifdef __WIN32__
#include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

#define OPENSSL_THREAD_DEFINES
#include <openssl/opensslconf.h>
#ifndef OPENSSL_THREADS
#  ifdef __GNUC__
#    warning No thread support by openssl. Driver will use coarse grain locking.
#  endif 
#endif

#include <openssl/crypto.h>
#include <openssl/des.h>
/* #include <openssl/idea.h> This is not supported on the openssl OTP requires */
#include <openssl/dsa.h>
#include <openssl/rsa.h>
#include <openssl/aes.h>
#include <openssl/md5.h>
#include <openssl/md4.h>
#include <openssl/sha.h>
#include <openssl/bn.h>
#include <openssl/objects.h>
#include <openssl/rc4.h>
#include <openssl/rc2.h>
#include <openssl/blowfish.h>
#include <openssl/rand.h>

#ifdef VALGRIND
#  include <valgrind/memcheck.h>

/* libcrypto mixes supplied buffer contents into its entropy pool,
   which makes valgrind complain about the use of uninitialized data.
   We use this valgrind "request" to make sure that no such seemingly
   undefined data escapes the driver.
*/
#  define ERL_VALGRIND_MAKE_MEM_DEFINED(ptr,size) \
    VALGRIND_MAKE_MEM_DEFINED(ptr,size)

#  define ERL_VALGRIND_ASSERT_MEM_DEFINED(ptr,size) \
    ((void) ((VALGRIND_CHECK_MEM_IS_DEFINED(ptr,size) == 0) ? 1 : \
    (fprintf(stderr,"\r\n####### VALGRIND_ASSSERT(%p,%d) failed at %s:%d\r\n",\
	(ptr),(size), __FILE__, __LINE__), abort(), 0)))
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

/* Driver interface declarations */
static int init(void);
static void finish(void);
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int crypto_control(ErlDrvData drv_data, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen); 

/* openssl callbacks */
#ifdef OPENSSL_THREADS
static void locking_function(int mode, int n, const char *file, int line);
static unsigned long id_function(void);
static struct CRYPTO_dynlock_value* dyn_create_function(const char *file,
							int line);
static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value* ptr,
			      const char *file, int line);
static void dyn_destroy_function(struct CRYPTO_dynlock_value *ptr,
				 const char *file, int line);
#endif /* OPENSSL_THREADS */

/* helpers */
static void hmac_md5(char *key, int klen, char *dbuf, int dlen, 
                     char *hmacbuf);
static void hmac_sha1(char *key, int klen, char *dbuf, int dlen, 
                      char *hmacbuf);

static ErlDrvEntry crypto_driver_entry = {
    init,
    start, 
    stop, 
    NULL,                       /* output */
    NULL,                       /* ready_input */
    NULL,                       /* ready_output */ 
    "crypto_drv", 
    finish,
    NULL,                       /* handle */
    crypto_control, 
    NULL,                       /* timeout */
    NULL,                       /* outputv */

    NULL,                       /* ready_async */
    NULL,                       /* flush */
    NULL,                       /* call */
    NULL,                       /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
#ifdef OPENSSL_THREADS
    ERL_DRV_FLAG_USE_PORT_LOCKING,
#else
    0,
#endif
    NULL,                       /* handle2 */
    NULL                        /* process_exit */
};


/* Keep the following definitions in alignment with the FUNC_LIST
 * in crypto.erl. 
 */

#define DRV_INFO                0
#define DRV_MD5                 1
#define DRV_MD5_INIT            2
#define DRV_MD5_UPDATE          3
#define DRV_MD5_FINAL           4
#define DRV_SHA                 5
#define DRV_SHA_INIT            6
#define DRV_SHA_UPDATE          7
#define DRV_SHA_FINAL           8
#define DRV_MD5_MAC             9
#define DRV_MD5_MAC_96          10
#define DRV_SHA_MAC             11
#define DRV_SHA_MAC_96          12
#define DRV_CBC_DES_ENCRYPT     13
#define DRV_CBC_DES_DECRYPT     14
#define DRV_EDE3_CBC_DES_ENCRYPT 15
#define DRV_EDE3_CBC_DES_DECRYPT 16
#define DRV_AES_CFB_128_ENCRYPT 17
#define DRV_AES_CFB_128_DECRYPT 18
#define DRV_RAND_BYTES          19
#define DRV_RAND_UNIFORM        20
#define DRV_MOD_EXP             21
#define DRV_DSS_VERIFY          22
#define DRV_RSA_VERIFY_SHA      23
/* #define DRV_RSA_VERIFY_MD5      35 */ 
#define DRV_CBC_AES128_ENCRYPT  24
#define DRV_CBC_AES128_DECRYPT  25
#define DRV_XOR                 26
#define DRV_RC4_ENCRYPT         27 /* no decrypt needed; symmetric */
#define DRV_RC4_SETKEY          28
#define DRV_RC4_ENCRYPT_WITH_STATE 29
#define DRV_CBC_RC2_40_ENCRYPT     30
#define DRV_CBC_RC2_40_DECRYPT     31
#define DRV_CBC_AES256_ENCRYPT  32
#define DRV_CBC_AES256_DECRYPT  33
#define DRV_INFO_LIB            34
/* #define DRV_RSA_VERIFY_SHA      23 */
#define DRV_RSA_VERIFY_MD5      35 
#define DRV_RSA_SIGN_SHA        36
#define DRV_RSA_SIGN_MD5        37
#define DRV_DSS_SIGN            38
#define DRV_RSA_PUBLIC_ENCRYPT  39
#define DRV_RSA_PRIVATE_DECRYPT 40
#define DRV_RSA_PRIVATE_ENCRYPT 41
#define DRV_RSA_PUBLIC_DECRYPT  42
#define DRV_DH_GENERATE_PARAMS  43
#define DRV_DH_CHECK            44
#define DRV_DH_GENERATE_KEY     45
#define DRV_DH_COMPUTE_KEY      46
#define DRV_MD4                 47
#define DRV_MD4_INIT            48
#define DRV_MD4_UPDATE          49
#define DRV_MD4_FINAL           50

#define SSL_VERSION_0_9_8 0
#if SSL_VERSION_0_9_8
#define DRV_SHA256              51
#define DRV_SHA256_INIT         52
#define DRV_SHA256_UPDATE       53
#define DRV_SHA256_FINAL        54
#define DRV_SHA512              55
#define DRV_SHA512_INIT         56
#define DRV_SHA512_UPDATE       57
#define DRV_SHA512_FINAL        58
#endif

#define DRV_BF_CFB64_ENCRYPT     59
#define DRV_BF_CFB64_DECRYPT     60
#define DRV_BF_ECB_ENCRYPT       61
#define DRV_BF_ECB_DECRYPT       62
#define DRV_BF_OFB64_ENCRYPT     63
#define DRV_BF_CBC_ENCRYPT       64
#define DRV_BF_CBC_DECRYPT       65

#define DRV_ECB_DES_ENCRYPT     66
#define DRV_ECB_DES_DECRYPT     67

/* #define DRV_CBC_IDEA_ENCRYPT    34 */
/* #define DRV_CBC_IDEA_DECRYPT    35 */

/* Not DRV_DH_GENERATE_PARAMS DRV_DH_CHECK
 * Calc RSA_VERIFY_*  and RSA_SIGN once */
#define NUM_CRYPTO_FUNCS        48

#define MD5_CTX_LEN     (sizeof(MD5_CTX))
#define MD5_LEN         16
#define MD5_LEN_96      12
#define MD4_CTX_LEN     (sizeof(MD4_CTX))
#define MD4_LEN         16
#define SHA_CTX_LEN     (sizeof(SHA_CTX))
#define SHA_LEN         20
#define SHA_LEN_96      12
#define HMAC_INT_LEN    64

#define HMAC_IPAD       0x36
#define HMAC_OPAD       0x5c

#if SSL_VERSION_0_9_8
#define SHA256_CTX_LEN (sizeof(SHA256_CTX))
#define SHA256_LEN     32

#define SHA512_CTX_LEN (sizeof(SHA512_CTX))
#define SHA512_LEN     64
#endif

/* INITIALIZATION AFTER LOADING */

/* 
 * This is the init function called after this driver has been loaded.
 * It must *not* be declared static. Must return the address to 
 * the driver entry.
 */

#if !defined(__WIN32__)
DRIVER_INIT(crypto_drv);
#endif

DRIVER_INIT(crypto_drv)
{
    return &crypto_driver_entry;
}

static ErlDrvRWLock** lock_vec = NULL; /* Static locks used by openssl */

/* DRIVER INTERFACE */

static int init(void)
{
    ErlDrvSysInfo sys_info;
    int i;

    CRYPTO_set_mem_functions(driver_alloc, driver_realloc, driver_free);

#ifdef OPENSSL_THREADS
    driver_system_info(&sys_info, sizeof(sys_info));

    if(sys_info.scheduler_threads > 1) {
	lock_vec = driver_alloc(CRYPTO_num_locks()*sizeof(*lock_vec));
	if (lock_vec==NULL) return -1;
	memset(lock_vec,0,CRYPTO_num_locks()*sizeof(*lock_vec));
    
	for(i=CRYPTO_num_locks()-1; i>=0; --i) {
	    lock_vec[i] = erl_drv_rwlock_create("crypto_drv_stat");
	    if (lock_vec[i]==NULL) return -1;
	}
	CRYPTO_set_locking_callback(locking_function);    
	CRYPTO_set_id_callback(id_function);
	CRYPTO_set_dynlock_create_callback(dyn_create_function);
	CRYPTO_set_dynlock_lock_callback(dyn_lock_function);
	CRYPTO_set_dynlock_destroy_callback(dyn_destroy_function);
    }
    /* else no need for locks */
#endif /* OPENSSL_THREADS */

    return 0;
}

static void finish(void)
{
    /* Moved here from crypto_control() as it's not thread safe */
    CRYPTO_cleanup_all_ex_data();

    if(lock_vec != NULL) {
	int i;
	for(i=CRYPTO_num_locks()-1; i>=0; --i) {
	    if (lock_vec[i] != NULL) {
		erl_drv_rwlock_destroy(lock_vec[i]);
	    }
	}
	driver_free(lock_vec);
    }
}

static ErlDrvData start(ErlDrvPort port, char *command)
{ 
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return 0; /* not used */
}

static void stop(ErlDrvData drv_data)
{
    return;
}

/* Helper functions for 'crypto_control'
*/
static INLINE unsigned char* return_binary(char **rbuf, int rlen, int len)
{
    if (len <= rlen) {
	return (unsigned char *) *rbuf;
    }
    else {
	ErlDrvBinary* bin;
	*rbuf = (char*) (bin = driver_alloc_binary(len));	
	return (bin==NULL) ? NULL : (unsigned char *) bin->orig_bytes;
    }
}

static INLINE unsigned char* return_binary_shrink(char **rbuf, int rlen, unsigned char* data, int len)
{
    if ((char *) data == *rbuf) { /* default buffer */
	ASSERT(len <= rlen);
	return (unsigned char *) data;
    }
    else {
	ErlDrvBinary* bin = (ErlDrvBinary*) *rbuf;
	*rbuf = (char*) (bin=driver_realloc_binary(bin, len));
	return (bin==NULL) ? NULL : (unsigned char *) bin->orig_bytes;
    }
}

/* Nowadays (R13) it does matter what value control returns
 * as it may return data in default buffer.
 */
static int crypto_control(ErlDrvData drv_data, unsigned int command, char *buf, 
			  int len, char **rbuf, int rlen)
{
    int klen, dlen, macsize, from_len, to_len, i;
    int base_len, exponent_len, modulo_len;
    int data_len, dsa_p_len, dsa_q_len;
    int dsa_s_len, dsa_g_len, dsa_y_len;
    int rsa_e_len, rsa_n_len, rsa_d_len, padding;
    int or_mask;
    int prime_len, generator;
    int privkey_len, pubkey_len, dh_p_len, dh_g_len;
    unsigned int rsa_s_len, j;
    char *key, *key2, *dbuf;
    unsigned char *p;
    const_DES_cblock *des_key, *des_key2, *des_key3;
    const unsigned char *des_dbuf;
    BIGNUM *bn_from, *bn_to, *bn_rand, *bn_result;
    BIGNUM *bn_base, *bn_exponent, *bn_modulo;
    BIGNUM *dsa_p, *dsa_q, *dsa_g, *dsa_y;
    BIGNUM *rsa_n, *rsa_e, *rsa_d;
    BIGNUM *dh_p, *dh_g, *privkey, *pubkey;
    DES_cblock *des_ivec;
    unsigned char* bin;
    DES_key_schedule schedule, schedule2, schedule3;
    DH *dh_params;
/*     IDEA_KEY_SCHEDULE idea, idea2; */
    char hmacbuf[SHA_DIGEST_LENGTH];
    unsigned char *rsa_s, *dsa_s;   
    /* char hmacbuf[SHA_LEN]; */
#if SSL_VERSION_0_9_8
    SHA256_CTX sha256_ctx;
    SHA512_CTX sha512_ctx;
#endif
    MD5_CTX md5_ctx;
    MD4_CTX md4_ctx;
    SHA_CTX sha_ctx;
    int new_ivlen = 0;
    BN_CTX *bn_ctx;
    DSA *dsa;
    RSA *rsa;
    AES_KEY aes_key;
    RC4_KEY rc4_key;
    RC2_KEY rc2_key;

    switch(command) {

    case DRV_INFO:
	bin = return_binary(rbuf,rlen,NUM_CRYPTO_FUNCS);
	if (bin==NULL) return -1;

        for (i = 0; i < NUM_CRYPTO_FUNCS; i++) {
	    bin[i] = i + 1;
        }
        return NUM_CRYPTO_FUNCS;

    case DRV_MD5:
	bin = return_binary(rbuf,rlen,MD5_LEN);	
       if (bin==NULL) return -1;
	MD5((unsigned char *) buf, len, bin);
        return MD5_LEN;

    case DRV_MD5_INIT:
	bin = return_binary(rbuf,rlen,MD5_CTX_LEN);
	if (bin==NULL) return -1;
        MD5_Init((MD5_CTX *)bin);
        return MD5_CTX_LEN;             

    case DRV_MD5_UPDATE:
        if (len < MD5_CTX_LEN)
            return -1;
        bin = return_binary(rbuf,rlen,MD5_CTX_LEN);
	if (bin==NULL) return -1;
        memcpy(bin, buf, MD5_CTX_LEN);
        MD5_Update((MD5_CTX *)bin, buf + MD5_CTX_LEN, 
                   len - MD5_CTX_LEN);
        return MD5_CTX_LEN;             

    case DRV_MD5_FINAL:
        if (len != MD5_CTX_LEN)
            return -1;
        memcpy(&md5_ctx, buf, MD5_CTX_LEN); /* XXX Use buf only? */
        bin = return_binary(rbuf,rlen,MD5_LEN);
	if (bin==NULL) return -1;	
        MD5_Final(bin, &md5_ctx);
        return MD5_LEN;         

    case DRV_SHA:
        bin = return_binary(rbuf,rlen,SHA_LEN);
	if (bin==NULL) return -1;
        SHA1((unsigned char *) buf, len, bin);
        return SHA_LEN;

    case DRV_SHA_INIT:
        bin = return_binary(rbuf,rlen,SHA_CTX_LEN);
	if (bin==NULL) return -1;
        SHA1_Init((SHA_CTX*)bin);
        return SHA_CTX_LEN;             

    case DRV_SHA_UPDATE:
        if (len < SHA_CTX_LEN)
            return -1;
        bin = return_binary(rbuf,rlen,SHA_CTX_LEN);
	if (bin==NULL) return -1;
        memcpy(bin, buf, SHA_CTX_LEN);
        SHA1_Update((SHA_CTX*)bin, buf + SHA_CTX_LEN, len - SHA_CTX_LEN);
        return SHA_CTX_LEN;             

    case DRV_SHA_FINAL:
        if (len != SHA_CTX_LEN)
            return -1;
        memcpy(&sha_ctx, buf, SHA_CTX_LEN); /* XXX Use buf only? */
        bin = return_binary(rbuf,rlen,SHA_LEN);
	if (bin==NULL) return -1;
        SHA1_Final(bin, &sha_ctx);
        return SHA_LEN;         

    case DRV_MD5_MAC:
    case DRV_MD5_MAC_96:
        /* buf = klen[4] key data */
        klen = get_int32(buf);
        key = buf + 4;
        dlen = len - klen - 4;
        dbuf = key + klen;
        hmac_md5(key, klen, dbuf, dlen, hmacbuf);
        macsize = (command == DRV_MD5_MAC) ? MD5_LEN : MD5_LEN_96;
        bin = return_binary(rbuf,rlen,macsize);
	if (bin==NULL) return -1;
        memcpy(bin, hmacbuf, macsize);
        return macsize;

    case DRV_SHA_MAC:
    case DRV_SHA_MAC_96:
        /* buf = klen[4] key data */
        klen = get_int32(buf);
        key = buf + 4;
        dlen = len - klen - 4;
        dbuf = key + klen;
        hmac_sha1(key, klen, dbuf, dlen, hmacbuf);
        macsize = (command == DRV_SHA_MAC) ? SHA_LEN : SHA_LEN_96;
        bin = return_binary(rbuf,rlen,macsize);
	if (bin==NULL) return -1;
        memcpy(bin, (unsigned char *) hmacbuf, macsize);
        return macsize;

    case DRV_CBC_DES_ENCRYPT:
    case DRV_CBC_DES_DECRYPT:
        /* buf = key[8] ivec[8] data */
        dlen = len - 16;
        if (dlen < 0)
            return -1;
	if (dlen % 8 != 0)
	    return -1;
        des_key = (const_DES_cblock*) buf; 
        des_ivec = (DES_cblock*)(buf + 8); 
        des_dbuf = (unsigned char *) (buf + 16);
        bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
        DES_set_key(des_key, &schedule);
        DES_ncbc_encrypt(des_dbuf, bin, dlen, &schedule, des_ivec, 
                         (command == DRV_CBC_DES_ENCRYPT));
        return dlen;

    case DRV_ECB_DES_ENCRYPT:
    case DRV_ECB_DES_DECRYPT:
        /* buf = key[8] data */
        dlen = len - 8;
        if (dlen != 8)
            return -1;
        des_key = (const_DES_cblock*) buf;
        des_dbuf = (unsigned char *) (buf + 8);
        bin = return_binary(rbuf,rlen,dlen);
        if (bin==NULL) return -1;
        DES_set_key(des_key, &schedule);
        DES_ecb_encrypt((const_DES_cblock*) des_dbuf, (DES_cblock*) bin, &schedule,
                        (command == DRV_ECB_DES_ENCRYPT));
        return dlen;

    case DRV_BF_ECB_ENCRYPT:
    case DRV_BF_ECB_DECRYPT:
    {
	/* buf = klen[4] key data */
	int bf_direction;
	const unsigned char *ukey;
	const unsigned char *bf_dbuf; /* blowfish input data */
	BF_KEY bf_key; /* blowfish key 8 */

	klen = get_int32(buf);
	ukey = (unsigned char *) buf + 4;
	bf_dbuf = ukey + klen;
	dlen = len - 4 - klen;
	if (dlen < 0) return -1;
	BF_set_key(&bf_key, klen, ukey);
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
	bf_direction = command == DRV_BF_ECB_ENCRYPT ? BF_ENCRYPT : BF_DECRYPT;
	BF_ecb_encrypt(bf_dbuf, bin, &bf_key, bf_direction);
	return dlen;
    }

    case DRV_BF_CBC_ENCRYPT:
    case DRV_BF_CBC_DECRYPT:
    {
	/* buf = klen[4] key ivec[8] data */
	unsigned char *ukey;
	unsigned char* ivec;
	unsigned char bf_tkey[8]; /* blowfish ivec */
	int bf_direction;
	const unsigned char *bf_dbuf; /* blowfish input data */
	BF_KEY bf_key; /* blowfish key 8 */

	klen = get_int32(buf);
	ukey = (unsigned char *)buf + 4;
	ivec = ukey + klen;
	bf_dbuf = ivec + 8;
	dlen = len - 4 - klen - 8;
	if (dlen < 0) return -1;
	BF_set_key(&bf_key, klen, ukey);
	memcpy(bf_tkey, ivec, 8);
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
	bf_direction = command == DRV_BF_CBC_ENCRYPT ? BF_ENCRYPT : BF_DECRYPT;
	BF_cbc_encrypt(bf_dbuf, bin, dlen, &bf_key, bf_tkey, bf_direction);
	return dlen;
    }

    case DRV_BF_OFB64_ENCRYPT:
    {
	/* buf = klen[4] key ivec[8] data */
	unsigned char *ukey;
	unsigned char* ivec;
	unsigned char bf_tkey[8]; /* blowfish ivec */
	int bf_n; /* blowfish ivec pos */
	const unsigned char *bf_dbuf; /* blowfish input data */
	BF_KEY bf_key; /* blowfish key 8 */

	klen = get_int32(buf);
	ukey = (unsigned char *)buf + 4;
	ivec = ukey + klen;
	bf_dbuf = ivec + 8;
	dlen = len - 4 - klen - 8;
	if (dlen < 0) return -1;
	BF_set_key(&bf_key, klen, ukey);
	memcpy(bf_tkey, ivec, 8);
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
	bf_n = 0;
	BF_ofb64_encrypt(bf_dbuf, bin, dlen, &bf_key, bf_tkey, &bf_n);
	return dlen;
    }

    case DRV_BF_CFB64_ENCRYPT:
    case DRV_BF_CFB64_DECRYPT:
    {
	/* buf = klen[4] key ivec[8] data */
	unsigned char* ivec;
	unsigned char bf_tkey[8]; /* blowfish ivec */    
	int bf_n; /* blowfish ivec pos */    
	int bf_direction;
	const unsigned char *bf_dbuf; /* blowfish input data */   
	BF_KEY bf_key; /* blowfish key 8 */
	
	klen = get_int32(buf);
	key = buf + 4;
	ivec = (unsigned char *) (key + klen);
	bf_dbuf = ivec + 8;
	dlen = len - 4 - klen - 8;
	if (dlen < 0) return -1;
	BF_set_key(&bf_key, klen, (unsigned char *) key);
	memcpy(bf_tkey, ivec, 8);
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
	bf_direction = command == DRV_BF_CFB64_ENCRYPT ? BF_ENCRYPT : BF_DECRYPT;
	bf_n = 0;
	BF_cfb64_encrypt(bf_dbuf, bin, dlen, &bf_key, bf_tkey, &bf_n, bf_direction);
	return dlen;
    }

/*     case DRV_CBC_IDEA_ENCRYPT: */
/*     case DRV_CBC_IDEA_DECRYPT: */
         /* buf = key[16] ivec[8] data */
/*         dlen = len - 24; */
/*         if (dlen < 0) */
/*             return -1; */
/* 	if (dlen % 8 != 0) */
/* 	    return -1; */
/*         bin = return_binary(rbuf,rlen,dlen); */
/*         idea_set_encrypt_key(buf, &idea); */
/*  	if (command == DRV_CBC_IDEA_DECRYPT) { */
/*  	    idea_set_decrypt_key(&idea, &idea2); */
/* 	    memcpy(&idea, &idea2, sizeof(idea));  */
/* 	} */
/*         idea_cbc_encrypt(buf + 24, bin, dlen, &idea, buf + 8, */
/*                          (command == DRV_CBC_IDEA_ENCRYPT)); */
/*         return dlen; */

    case DRV_CBC_RC2_40_ENCRYPT:
    case DRV_CBC_RC2_40_DECRYPT:
        /* buf = key[5] ivec[8] data */
        dlen = len - 13;
        if (dlen < 0)
            return -1;
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
        RC2_set_key(&rc2_key, 5, (unsigned char *) buf, 40);
        RC2_cbc_encrypt((unsigned char *) (buf + 13), bin, dlen, &rc2_key, 
			(unsigned char *) (buf + 5),
			(command == DRV_CBC_RC2_40_ENCRYPT));
        return dlen;

    case DRV_EDE3_CBC_DES_ENCRYPT:
    case DRV_EDE3_CBC_DES_DECRYPT:
        dlen = len - 32;
        if (dlen < 0)
            return -1;
        des_key = (const_DES_cblock*) buf; 
        des_key2 = (const_DES_cblock*) (buf + 8); 
        des_key3 = (const_DES_cblock*) (buf + 16);
        des_ivec = (DES_cblock*) (buf + 24); 
        des_dbuf = (unsigned char *) (buf + 32);
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
        DES_set_key(des_key, &schedule);
        DES_set_key(des_key2, &schedule2);
        DES_set_key(des_key3, &schedule3);
        DES_ede3_cbc_encrypt(des_dbuf, bin, dlen, &schedule,
                             &schedule2, &schedule3, des_ivec, 
                             (command == DRV_EDE3_CBC_DES_ENCRYPT));
        return dlen;

    case DRV_AES_CFB_128_ENCRYPT:
    case DRV_AES_CFB_128_DECRYPT:
        /* buf = key[16] ivec[16] data */
        dlen = len - 32;
        if (dlen < 0)
            return -1;
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
        AES_set_encrypt_key((unsigned char *) buf, 128, &aes_key);
        AES_cfb128_encrypt((unsigned char *) (buf+32), bin, dlen, &aes_key,
                           (unsigned char *) (buf+16), &new_ivlen,
                           (command == DRV_AES_CFB_128_ENCRYPT));
        return dlen;

    case DRV_RC4_ENCRYPT:
        /* buf = klen[4] key data */
        klen = get_int32(buf);
        key = buf + 4;
        dlen = len - klen - 4;
        dbuf = key + klen;
        bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
        RC4_set_key(&rc4_key, klen, (unsigned char *) key);
        RC4(&rc4_key, dlen, (unsigned char *) dbuf, bin);
        return dlen;

    case DRV_RC4_SETKEY:
        /* buf = key */
        dlen = sizeof(rc4_key);
        bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
        RC4_set_key(&rc4_key, len, (unsigned char *) buf);
        memcpy(bin, &rc4_key, dlen);
        return dlen;

    case DRV_RC4_ENCRYPT_WITH_STATE:
        /* buf = statelength[4] state data, return statelength[4] state data */
        klen = get_int32(buf);
        key = buf + 4;
        dlen = len - klen - 4;
        dbuf = key + klen;
        bin = return_binary(rbuf,rlen,len);
	if (bin==NULL) return -1;
        memcpy(&rc4_key, key, klen);
        RC4(&rc4_key, dlen, (unsigned char *) dbuf, bin + klen + 4);
        memcpy(bin, buf, 4);
        memcpy(bin + 4, &rc4_key, klen);
        return len;

    case DRV_RAND_BYTES:
        /* buf = <<rlen:32/integer,topmask:8/integer,bottommask:8/integer>> */

        if (len != 6)
            return -1;
        dlen = get_int32(buf);
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
	RAND_pseudo_bytes(bin,dlen);
	ERL_VALGRIND_MAKE_MEM_DEFINED(bin, dlen);
        or_mask = ((unsigned char*)buf)[4];
        bin[dlen-1] |= or_mask; /* topmask */
        or_mask = ((unsigned char*)buf)[5];
        bin[0] |= or_mask; /* bottommask */
        return dlen;
      
    case DRV_RAND_UNIFORM:
      /* buf = <<from_len:32/integer,bn_from:from_len/binary,   *
       *         to_len:32/integer,bn_to:to_len/binary>>        */
      if (len < 8)
        return -1;
      from_len = get_int32(buf);
      if (len < (8 + from_len))
        return -1;
      to_len = get_int32(buf + 4 + from_len);
      if (len != (8 + from_len + to_len))
        return -1;
      ERL_VALGRIND_ASSERT_MEM_DEFINED(buf, 4 + from_len + 4 + to_len);
      bn_from = BN_new();
      BN_bin2bn((unsigned char *)(buf + 4), from_len, bn_from);
      bn_rand = BN_new();
      BN_bin2bn((unsigned char *)(buf + 8 + from_len), to_len, bn_rand);
      bn_to = BN_new();
      BN_sub(bn_to, bn_rand, bn_from);
      BN_pseudo_rand_range(bn_rand, bn_to);      
      BN_add(bn_rand, bn_rand, bn_from);
      dlen = BN_num_bytes(bn_rand);
      bin = return_binary(rbuf,rlen,dlen + 4);
      if (bin==NULL) return -1;
      put_int32(bin, dlen);
      BN_bn2bin(bn_rand,(unsigned char*)(bin + 4));
      ERL_VALGRIND_MAKE_MEM_DEFINED(bin+4, dlen);
      BN_free(bn_rand);
      BN_free(bn_from);
      BN_free(bn_to);
      return dlen + 4;
      
    case DRV_MOD_EXP:
      /* buf = <<base_len:32/integer,base/binary,          *
       *         exponent_len:32/integer,exponent/binary,  *
       *         modulo_len:32/integer, modulo/binary>>    */
      if (len < 12)
        return -1;
      base_len = get_int32(buf);
      if (len < (12 + base_len))
        return -1;
      exponent_len = get_int32(buf + 4 + base_len);
      if (len < (12 + base_len + exponent_len))
        return -1;
      modulo_len = get_int32(buf + 8 + base_len + exponent_len);
      if (len != (12 + base_len + exponent_len + modulo_len))
        return -1;
      bn_base = BN_new();
      BN_bin2bn((unsigned char *)(buf + 4),
                base_len, bn_base);
      bn_exponent = BN_new();
      BN_bin2bn((unsigned char *)(buf + 8 + base_len),
                exponent_len, bn_exponent);
      bn_modulo = BN_new();
      BN_bin2bn((unsigned char *)(buf + 12 + base_len + exponent_len),
                modulo_len, bn_modulo);
      bn_result = BN_new();
      bn_ctx = BN_CTX_new();
      BN_mod_exp(bn_result, bn_base, bn_exponent,
                 bn_modulo, bn_ctx);
      dlen = BN_num_bytes(bn_result);
      bin = return_binary(rbuf,rlen,dlen + 4);
      if (bin==NULL) return -1;
      put_int32(bin, dlen);
      BN_bn2bin(bn_result,(unsigned char*)(bin + 4));
      BN_free(bn_result);
      BN_free(bn_modulo);
      BN_free(bn_exponent);
      BN_free(bn_base);
      BN_CTX_free(bn_ctx);
      return dlen + 4;

    case DRV_DSS_VERIFY:
      /* buf = <<data_len:32/integer, data:data_len/binary,
       *         dsa_s_len:32/integer, dsa_s:dsa_s_len/binary,
       *         dsa_p_len:32/integer, dsa_p:dsa_p_len/binary,
       *         dsa_q_len:32/integer, dsa_q:dsa_q_len/binary,
       *         dsa_g_len:32/integer, dsa_g:dsa_g_len/binary,
       *         dsa_y_len:32/integer, dsa_y:dsa_y_len/binary>> */
      i = 0;
      j = 0;
      if (len < 24)
	return -1;
      data_len = get_int32(buf + i + j);
      j += data_len; i += 4;
      if (len < (24 + j))
	return -1;
      dsa_s_len = get_int32(buf + i + j);
      j += dsa_s_len; i += 4;
      if (len < (24 + j))
	 return -1;      
      dsa_p_len = get_int32(buf + i + j);
      j += dsa_p_len; i += 4;
      if (len < (24 + j))
        return -1;
      dsa_q_len = get_int32(buf + i + j);
      j += dsa_q_len; i += 4;
      if (len < (24 + j))
        return -1;
      dsa_g_len = get_int32(buf + i + j);
      j += dsa_g_len; i += 4;
      if (len < (24 + j))
        return -1;
      dsa_y_len = get_int32(buf + i + j);
      j += dsa_y_len;
      if (len != (24 + j))
        return -1;
      i = 4;
      SHA1((unsigned char *) (buf + i), data_len, (unsigned char *) hmacbuf);
      i += data_len + 4;
      dsa_s = (unsigned char *)(buf + i);
      i += (dsa_s_len + 4);
      dsa_p = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_p_len, dsa_p);
      i += (dsa_p_len + 4);
      dsa_q = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_q_len, dsa_q);
      i += (dsa_q_len + 4);
      dsa_g = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_g_len, dsa_g);
      i += (dsa_g_len + 4);
      dsa_y = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_y_len, dsa_y);
      dsa = DSA_new();
      dsa->p = dsa_p;
      dsa->q = dsa_q;
      dsa->g = dsa_g;
      dsa->priv_key = NULL;
      dsa->pub_key = dsa_y;
      i =  DSA_verify(0, (unsigned char *) hmacbuf, SHA_DIGEST_LENGTH, 
		      dsa_s, dsa_s_len, dsa);
      bin = return_binary(rbuf,rlen,1);
      if (bin==NULL) return -1;

      DSA_free(dsa);
      bin[0] = (i > 0) ? 1 : 0;
      return 1;
      
    case DRV_DSS_SIGN:
      /* buf = <<data_len:32/integer, data:data_len/binary,
       *         dsa_p_len:32/integer, dsa_p:dsa_p_len/binary,
       *         dsa_q_len:32/integer, dsa_q:dsa_q_len/binary,
       *         dsa_g_len:32/integer, dsa_g:dsa_g_len/binary,
       *         dsa_y_len:32/integer, dsa_y:dsa_y_len/binary,
       *         dsa_x_len:32/integer, dsa_s:dsa_x_len/binary>> */
      i = 0;
      j = 0;
      if (len < 20)
	 return -1;
      data_len = get_int32(buf + i + j);
      j += data_len; i += 4;
      if (len < (20 + j))
	  return -1;
      dsa_p_len = get_int32(buf + i + j);
      j += dsa_p_len; i += 4;
      if (len < (20 + j))
	 return -1;
      dsa_q_len = get_int32(buf + i + j);
      j += dsa_q_len; i += 4;
      if (len < (20 + j))
	 return -1;
      dsa_g_len = get_int32(buf + i + j);
      j += dsa_g_len; i += 4;
      if (len < (20 + j))
	 return -1;
      dsa_y_len = get_int32(buf + i + j);
      j += dsa_y_len;
      if (len < (20 + j))
	 return -1;
      if (len != (20 + j))
	 return -1;

      i = 4;
      SHA1((unsigned char *) (buf + i), data_len, (unsigned char *) hmacbuf);
      i += data_len + 4;
      dsa_p = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_p_len, dsa_p);
      i += (dsa_p_len + 4);
      dsa_q = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_q_len, dsa_q);
      i += (dsa_q_len + 4);
      dsa_g = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_g_len, dsa_g);
      i += (dsa_g_len + 4);
      dsa_y = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_y_len, dsa_y);
      /* i += (dsa_y_len + 4); */

      dsa = DSA_new();
      dsa->p = dsa_p;
      dsa->q = dsa_q;
      dsa->g = dsa_g;
      dsa->priv_key = dsa_y;
      dsa->pub_key  = NULL;
      dlen = DSA_size(dsa);
      bin = return_binary(rbuf,rlen, dlen+1);
      if (bin==NULL) return -1;
      i =  DSA_sign(NID_sha1,
		    (unsigned char *) hmacbuf,SHA_DIGEST_LENGTH,
		    (unsigned char *) &bin[1],
		    (unsigned int *) &dsa_s_len, dsa);
      DSA_free(dsa);
      if (i) {
	  if (dsa_s_len != dlen) {
	      bin = return_binary_shrink(rbuf,rlen,bin,dsa_s_len+1);
	  }
	  bin[0] = 1;
	  return dsa_s_len + 1;
      }
      else {
	  bin[0] = 0;
	  return 1;
      }

    case DRV_RSA_VERIFY_MD5:
    case DRV_RSA_VERIFY_SHA:
      /* buf = <<data_len:32/integer, data:data_len/binary,
       *         rsa_s_len:32/integer, rsa_s:rsa_s_len/binary,
       *         rsa_e_len:32/integer, rsa_e:rsa_e_len/binary,
       *         rsa_n_len:32/integer, rsa_n:rsa_n_len/binary>> */
      i = 0;
      j = 0;
      if (len < 16)
	 return -1;
      data_len = get_int32(buf + i + j);
      j += data_len; i += 4;
      if (len < (16 + j))
        return -1;
      rsa_s_len = get_int32(buf + i + j);
      j += rsa_s_len; i += 4;
      if (len < (16 + j))
        return -1;
      rsa_e_len = get_int32(buf + i + j);
      j += rsa_e_len; i += 4;
      if (len < (16 + j))
        return -1;
      rsa_n_len = get_int32(buf + i + j);
      j += rsa_n_len; i += 4;
      if (len != (16 + j))
	 return -1;
      i = 4;
      i += (data_len + 4);
      rsa_s = (unsigned char *)(buf + i);
      i += (rsa_s_len + 4);
      rsa_e = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), rsa_e_len, rsa_e);
      i += (rsa_e_len + 4);
      rsa_n = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), rsa_n_len, rsa_n);
      rsa = RSA_new();
      rsa->n = rsa_n;
      rsa->e = rsa_e;
      i = 4;
      if(command == DRV_RSA_VERIFY_SHA) {
	 SHA1((unsigned char *) (buf + i), data_len, 
	      (unsigned char *) hmacbuf);
	 i = RSA_verify(NID_sha1, (unsigned char *) hmacbuf, SHA_DIGEST_LENGTH,
			rsa_s, rsa_s_len, rsa);
      } else {
	 MD5((unsigned char *) (buf + i), data_len, (unsigned char *) hmacbuf);
	 i =  RSA_verify(NID_md5, (unsigned char *) hmacbuf, MD5_DIGEST_LENGTH,
			 rsa_s, rsa_s_len, rsa);
      }

      bin = return_binary(rbuf,rlen,1);
      if (bin==NULL) return -1;
      bin[0] = (char)(i & 0xff);
      RSA_free(rsa);
      return 1;

    case DRV_RSA_SIGN_MD5:
    case DRV_RSA_SIGN_SHA:        
      /* buf = <<data_len:32/integer, data:data_len/binary,
       *         rsa_e_len:32/integer, rsa_e:rsa_e_len/binary,       
       *         rsa_n_len:32/integer, rsa_n:rsa_n_len/binary,
       *         rsa_d_len:32/integer, rsa_d:rsa_d_len/binary>> */ 

      ERL_VALGRIND_ASSERT_MEM_DEFINED(buf,len);

      i = 0;
      j = 0;
      
      if (len < 16)
	 return -1;
      data_len = get_int32(buf + i + j);
      j += data_len; i += 4;
      if (len < (16 + j))
	 return -1;
      rsa_e_len = get_int32(buf + i + j);
      j += rsa_e_len; i += 4;
      if (len < (16 + j))
	 return -1;
      rsa_n_len = get_int32(buf + i + j);
      j += rsa_n_len; i += 4;
      if (len < (16 + j))
	 return -1;
      rsa_d_len = get_int32(buf + i + j);
      j += rsa_d_len; i += 4;
      if (len != (16 + j))
	 return -1;

      i = 4;
      i += (data_len + 4);
      rsa_e = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), rsa_e_len, rsa_e);
      i += (rsa_e_len + 4);
      rsa_n = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), rsa_n_len, rsa_n);
      i += (rsa_n_len + 4);
      rsa_d = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), rsa_d_len, rsa_d);
      i += (rsa_d_len + 4);
  
      rsa = RSA_new();
      rsa->e = rsa_e;
      rsa->n = rsa_n;
      rsa->d = rsa_d;
      
      dlen = RSA_size(rsa);
      bin = return_binary(rbuf,rlen,dlen+1);
      if (bin==NULL) return -1;
      i = 4;
      if (command == DRV_RSA_SIGN_MD5) {
	 MD5((unsigned char *) (buf + i), data_len, (unsigned char *) hmacbuf);
	 ERL_VALGRIND_ASSERT_MEM_DEFINED(hmacbuf, MD5_DIGEST_LENGTH);
	 i = RSA_sign(NID_md5,
		      (unsigned char *) hmacbuf,MD5_DIGEST_LENGTH,
		      (unsigned char *) &bin[1],
		      &rsa_s_len, rsa);	 
      } else {
	 SHA1((unsigned char *) (buf + i), data_len, 
	      (unsigned char *) hmacbuf);
	 ERL_VALGRIND_ASSERT_MEM_DEFINED(hmacbuf, SHA_DIGEST_LENGTH);
	 i =  RSA_sign(NID_sha1,
		       (unsigned char *) hmacbuf,SHA_DIGEST_LENGTH,
		       (unsigned char *) &bin[1],
		       &rsa_s_len, rsa);
      }
      RSA_free(rsa);
      if (i) {
	  ERL_VALGRIND_MAKE_MEM_DEFINED(bin+1, rsa_s_len);
	  if (rsa_s_len != dlen) {
	      bin = return_binary_shrink(rbuf,rlen,bin,rsa_s_len+1);
	      ERL_VALGRIND_ASSERT_MEM_DEFINED(bin+1, rsa_s_len);
	  }
	  bin[0] = 1;
	  return rsa_s_len + 1;
      }
      else {
	  bin[0] = 0;
	  return 1;
      }

    case DRV_RSA_PRIVATE_DECRYPT:        
    case DRV_RSA_PRIVATE_ENCRYPT:        
       /* buf = <<data_len:32/integer, data:data_len/binary,
	*         rsa_e_len:32/integer, rsa_e:rsa_e_len/binary,       
	*         rsa_n_len:32/integer, rsa_n:rsa_n_len/binary,
	*         rsa_d_len:32/integer, rsa_d:rsa_d_len/binary,
	*         pad:8/integer >> */ 

       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf,len);
       i = 0;
       j = 0;
      
       if (len < 17)
	  return -1;
       data_len = get_int32(buf + i + j);
       j += data_len; i += 4;
       if (len < (17 + j))
	  return -1;
       rsa_e_len = get_int32(buf + i + j);
       j += rsa_e_len; i += 4;
       if (len < (17 + j))
	  return -1;
       rsa_n_len = get_int32(buf + i + j);
       j += rsa_n_len; i += 4;
       if (len < (17 + j))
	  return -1;
       rsa_d_len = get_int32(buf + i + j);
       j += rsa_d_len; i += 4;
       padding = *(unsigned char *) (buf+i+j); 
       if (len != (17 + j))
	  return -1;

       i = 4;
       i += (data_len + 4);
       rsa_e = BN_new();
       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf+i,rsa_e_len);
       BN_bin2bn((unsigned char *)(buf + i), rsa_e_len, rsa_e);
       i += (rsa_e_len + 4);
       rsa_n = BN_new();
       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf+i,rsa_n_len);
       BN_bin2bn((unsigned char *)(buf + i), rsa_n_len, rsa_n);
       i += (rsa_n_len + 4);
       rsa_d = BN_new();
       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf+i,rsa_d_len);
       BN_bin2bn((unsigned char *)(buf + i), rsa_d_len, rsa_d);
       i += (rsa_d_len + 4);
       
       switch(padding) {
       case 0:
	  padding = RSA_NO_PADDING;
	  break;
       case 1:
	  padding = RSA_PKCS1_PADDING;
	  break;
       case 2:
	  padding = RSA_PKCS1_OAEP_PADDING;
	  break;
       case 3:
	  padding = RSA_SSLV23_PADDING;
	  break;
       default:
	  return -1;
       }
       
       rsa = RSA_new();
       rsa->e = rsa_e;
       rsa->n = rsa_n;
       rsa->d = rsa_d;
      
       dlen = RSA_size(rsa) + 1;
       bin = return_binary(rbuf,rlen,dlen);
       if (bin==NULL) return -1;
       i = 4;
       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf+i,data_len);
       if(command == DRV_RSA_PRIVATE_DECRYPT) {
	   i = RSA_private_decrypt(data_len, (unsigned char *) (buf+i),
				   (unsigned char *) &bin[1],
				   rsa, padding);	   
	   if(i > 0) {
	       ERL_VALGRIND_MAKE_MEM_DEFINED(&bin[1],i);
	       bin = return_binary_shrink(rbuf,rlen, bin, i+1);
	       if (bin==NULL) return -1;
	   }
       } else {
	   i = RSA_private_encrypt(data_len, (unsigned char *) (buf+i),
				   (unsigned char *) &bin[1],
				   rsa, padding);
	   if(i > 0) {
	       ERL_VALGRIND_MAKE_MEM_DEFINED(&bin[1],i);
	   }
       }
       RSA_free(rsa);
       if(i > 0) {
	   bin[0] = 1;
	   return i + 1;
       } else {
	   bin[0] = 0;
	   return 1;
       }
       break;

    case DRV_RSA_PUBLIC_ENCRYPT:
    case DRV_RSA_PUBLIC_DECRYPT:
       /* buf = <<data_len:32/integer, data:data_len/binary,
	*         rsa_e_len:32/integer, rsa_e:rsa_e_len/binary,       
	*         rsa_n_len:32/integer, rsa_n:rsa_n_len/binary,
	*         pad:8/integer >> */ 

       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf,len);
       i = 0;
       j = 0;
      
       if (len < 13)
	  return -1;
       data_len = get_int32(buf + i + j);
       j += data_len; i += 4;
       if (len < (13 + j))
	  return -1;
       rsa_e_len = get_int32(buf + i + j);
       j += rsa_e_len; i += 4;
       if (len < (13 + j))
	  return -1;
       rsa_n_len = get_int32(buf + i + j);
       j += rsa_n_len; i += 4;
       if (len < (13 + j))
	  return -1;
       padding = *(unsigned char *) (buf + i + j);
       if (len != (13 + j))
	  return -1;

       i = 4;
       i += (data_len + 4);
       rsa_e = BN_new();
       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf+i,rsa_e_len);
       BN_bin2bn((unsigned char *)(buf + i), rsa_e_len, rsa_e);
       i += (rsa_e_len + 4);
       rsa_n = BN_new();
       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf+i,rsa_n_len);
       BN_bin2bn((unsigned char *)(buf + i), rsa_n_len, rsa_n);
       i += (rsa_n_len + 4);
       
       switch(padding) {
       case 0:
	  padding = RSA_NO_PADDING;
	  break;
       case 1:
	  padding = RSA_PKCS1_PADDING;
	  break;
       case 2:
	  padding = RSA_PKCS1_OAEP_PADDING;
	  break;
       case 3:
	  padding = RSA_SSLV23_PADDING;
	  break;
       default:
	  return -1;
       }
       
       rsa = RSA_new();
       rsa->e = rsa_e;
       rsa->n = rsa_n;
      
       dlen = RSA_size(rsa) + 1;
       bin = return_binary(rbuf,rlen,dlen);
       if (bin==NULL) return -1;
       i = 4;
       if(command == DRV_RSA_PUBLIC_ENCRYPT) {
	   ERL_VALGRIND_ASSERT_MEM_DEFINED(buf+i,data_len);
	   i = RSA_public_encrypt(data_len, (unsigned char *) (buf+i),
				  (unsigned char *) &bin[1],
				  rsa, padding);
	   if (i > 0) {
	       ERL_VALGRIND_MAKE_MEM_DEFINED(bin+1, i);
	   }
       } else {
	   i = RSA_public_decrypt(data_len, (unsigned char *) (buf+i),
				  (unsigned char *) &bin[1],
				  rsa, padding);	   
	   if(i > 0) {
	       ERL_VALGRIND_MAKE_MEM_DEFINED(bin+1, i);
	       bin = return_binary_shrink(rbuf,rlen,bin, i+1);
	       if (bin==NULL) return -1;
	   }
       }
       
       RSA_free(rsa);
       if(i > 0) {
	   bin[0] = 1;
	   return i + 1;
       } else {
/* 	  ERR_load_crypto_strings(); */
/* 	  fprintf(stderr, "%d: %s \r\n", __LINE__, ERR_reason_error_string(ERR_get_error())); */
	   bin[0] = 0;
	   return 1;
       }
       break;

    case DRV_CBC_AES128_ENCRYPT:
    case DRV_CBC_AES256_ENCRYPT:
    case DRV_CBC_AES128_DECRYPT:
    case DRV_CBC_AES256_DECRYPT:
	/* buf = key[klen] ivec[klen] data */
	if (command == DRV_CBC_AES256_ENCRYPT || command == DRV_CBC_AES256_DECRYPT)
	    klen = 32;
	else 
	    klen = 16;
	dlen = len - klen - 16;
	if (dlen < 0)
	    return -1;
	if (dlen % 16 != 0)
	    return -1;
	if (command == DRV_CBC_AES128_ENCRYPT || command == DRV_CBC_AES256_ENCRYPT) {
	    i = AES_ENCRYPT;
	    AES_set_encrypt_key((unsigned char *) buf, klen*8, &aes_key);
	} else {
	    i = AES_DECRYPT;
	    AES_set_decrypt_key((unsigned char *) buf, klen*8, &aes_key);
	}
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
	AES_cbc_encrypt((unsigned char *) (buf + klen+16),
			(unsigned char *) bin,
			dlen,
			&aes_key, 
			(unsigned char *) (buf + klen),
			i);
	return dlen;
	
/*     case DRV_CBC_AES128_DECRYPT: */
/*     case DRV_CBC_AES256_DECRYPT: */
/* 	/\* buf = key[klen] ivec[16] data *\/ */
/* 	if (command == DRV_CBC_AES256_DECRYPT) */
/* 	    klen = 32; */
/* 	else  */
/* 	    klen = 16; */
/* 	dlen = len - klen - 16; */
/* 	if (dlen < 0) */
/* 	    return -1; */
/* 	*rbuf = (char *)(bin = driver_alloc_binary(dlen)); */
/* 	AES_set_decrypt_key((unsigned char *) buf, klen*8, &aes_key); */
/* 	AES_cbc_encrypt((unsigned char *) (buf + klen+16), */
/* 			(unsigned char *) bin->orig_bytes, */
/* 			dlen, */
/* 			&aes_key,  */
/* 			(unsigned char *) (buf + klen), */
/* 			AES_DECRYPT); */
/* 	return dlen; */
/* 	break; */

    case DRV_XOR:
        /* buf = data1, data2 with same size */
        dlen = len / 2;
        if (len != dlen * 2)
            return -1;
	bin = return_binary(rbuf,rlen,dlen);
	if (bin==NULL) return -1;
        p = bin,
        dbuf = buf + dlen;
        for (key = buf, key2 = dbuf; key != dbuf; ++key, ++key2, ++p)
            *p = *key ^ *key2;
        return dlen;
	
    case DRV_DH_GENERATE_PARAMS:
       /* buf = <<PrimeLen:32 Generator:32>> */
       if (len != 8) 
	  return -1;
       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf,len);
       prime_len = get_int32(buf);
       generator = get_int32(buf+4);
       dh_params = DH_generate_parameters(prime_len, generator, NULL, NULL);

       if(dh_params) {
	  dh_p_len = BN_num_bytes(dh_params->p);
	  dh_g_len = BN_num_bytes(dh_params->g);
	  dlen = 1 + 4 + 4 + dh_g_len + dh_p_len;
	  bin = return_binary(rbuf,rlen,dlen);
	  if (bin==NULL) return -1;
	  bin[0] = 1;
	  put_int32(bin+1, dh_p_len);
	  BN_bn2bin(dh_params->p, bin+5);
	  ERL_VALGRIND_MAKE_MEM_DEFINED(bin+5,dh_p_len);
	  put_int32(bin+5+dh_p_len, dh_g_len);
	  BN_bn2bin(dh_params->g, bin+5+dh_p_len+4);
	  ERL_VALGRIND_MAKE_MEM_DEFINED(bin+5+dh_p_len+4,dh_g_len);	  
       } else {
	   dlen = 1;
	   bin = return_binary(rbuf,rlen,dlen);
	   if (bin==NULL) return -1;
	   bin[0] = 0;
       }
       DH_free(dh_params);
       return dlen;

    case DRV_DH_CHECK:
       /* buf = <<dh_p_len:32/integer, dh_p:dh_p_len/binary,
	*         dh_g_len:32/integer, dh_g:dh_g_len/binary>> */
       i = 0;
       j = 0;
       if(len < 8)        return -1;
       dh_p_len = get_int32(buf + i + j);
       j += dh_p_len; i += 4;
       if (len < (8 + j)) return -1;
       dh_g_len = get_int32(buf + i + j);
       j += dh_g_len; i += 4;
       if(len != (8+j))   return -1;
       i=4;
       dh_p = BN_new();
       BN_bin2bn((unsigned char *)(buf + i), dh_p_len, dh_p);
       i += (dh_p_len + 4);
       dh_g = BN_new();
       BN_bin2bn((unsigned char *)(buf + i), dh_g_len, dh_g);
       /* i += (dsa_g_len + 4); */

       dh_params = DH_new();
       dh_params->p = dh_p;
       dh_params->g = dh_g;
       
       i=0;
       bin = return_binary(rbuf,rlen,4);
       if (bin==NULL) return -1;
       if(DH_check(dh_params, &i)) {
	  put_int32(bin, i);
       } else {
	  /* Check Failed */
	  put_int32(bin, -1);
       }
       DH_free(dh_params);
       return 4;

    case DRV_DH_GENERATE_KEY:
       /* buf = <<key_len:32,  key:key_len/binary,            *
	*         dh_p_len:32/integer, dh_p:dh_p_len/binary,  *
	*         dh_g_len:32/integer, dh_g:dh_g_len/binary>> */
       ERL_VALGRIND_ASSERT_MEM_DEFINED(buf,len);
       i = 0;
       j = 0;
       if(len < 12)        return -1;
       base_len = get_int32(buf + i + j);
       j += base_len; i += 4;
       if (len < (12 + j)) return -1;
       dh_p_len = get_int32(buf + i + j);
       j += dh_p_len; i += 4;
       if (len < (12 + j)) return -1;
       dh_g_len = get_int32(buf + i + j);
       j += dh_g_len; i += 4;
       if(len != (12 + j))   return -1;
       i=4;
       i += (base_len + 4);
       dh_p = BN_new();
       BN_bin2bn((unsigned char *)(buf + i), dh_p_len, dh_p);
       i += (dh_p_len + 4);
       dh_g = BN_new();
       BN_bin2bn((unsigned char *)(buf + i), dh_g_len, dh_g);
       /* i += (dsa_g_len + 4); */

       dh_params = DH_new();
       dh_params->p = dh_p;
       dh_params->g = dh_g;
       if(base_len > 0) {
	   dh_params->priv_key = BN_new();
	   BN_bin2bn((unsigned char *)(buf + i), base_len, 
		     dh_params->priv_key);
       }
       i=0;
       if(DH_generate_key(dh_params)) {
	  privkey_len = BN_num_bytes(dh_params->priv_key);
	  pubkey_len = BN_num_bytes(dh_params->pub_key);
	  dlen = 1 + 4 + 4 + pubkey_len + privkey_len;
	  bin = return_binary(rbuf,rlen, dlen);
	  if (bin==NULL) return -1;
	  bin[0] = 1;
	  put_int32(bin+1, pubkey_len);
	  BN_bn2bin(dh_params->pub_key, bin+5);
	  ERL_VALGRIND_MAKE_MEM_DEFINED(bin+5, pubkey_len);	  
	  put_int32(bin+5+pubkey_len, privkey_len);
	  BN_bn2bin(dh_params->priv_key, bin+5+pubkey_len+4);
	  ERL_VALGRIND_MAKE_MEM_DEFINED(bin+5+pubkey_len+4, privkey_len);	  
       } else {
	   dlen = 1;
	   bin = return_binary(rbuf,rlen,dlen);
	   if (bin==NULL) return -1;
	   bin[0] = 0;
       }
       DH_free(dh_params);
       return dlen;

    case DRV_DH_COMPUTE_KEY:
       /* buf = <<pubkey_len:32,  pubkey:pubkey_len/binary,   *
	*         privkey_len:32, privkey:privkey_len/binary, *
	*         dh_p_len:32/integer, dh_p:dh_p_len/binary,  *
	*         dh_g_len:32/integer, dh_g:dh_g_len/binary>> */
       i = 0;
       j = 0;
       if(len < 16)        return -1;
       pubkey_len = get_int32(buf + i + j);
       j += pubkey_len; i += 4;
       if (len < (16 + j)) return -1;
       privkey_len = get_int32(buf + i + j);
       j += privkey_len; i += 4;
       if (len < (16 + j)) return -1;
       dh_p_len = get_int32(buf + i + j);
       j += dh_p_len; i += 4;
       if (len < (16 + j)) return -1;
       dh_g_len = get_int32(buf + i + j);
       j += dh_g_len; i += 4;
       if(len != (16 + j))   return -1;
       i=4;
       pubkey = BN_new();
       BN_bin2bn((unsigned char *)(buf + i), pubkey_len, pubkey);
       i += (pubkey_len + 4);
       privkey = BN_new();
       BN_bin2bn((unsigned char *)(buf + i), privkey_len, privkey);
       i += (privkey_len + 4);
       dh_p = BN_new();
       BN_bin2bn((unsigned char *)(buf + i), dh_p_len, dh_p);
       i += (dh_p_len + 4);
       dh_g = BN_new();
       BN_bin2bn((unsigned char *)(buf + i), dh_g_len, dh_g);
       /* i += (dsa_g_len + 4); */

       dh_params = DH_new();
       dh_params->p = dh_p;
       dh_params->g = dh_g;
       dh_params->priv_key = privkey;
       
       klen = DH_size(dh_params);
       bin = return_binary(rbuf,rlen,1+klen);
       if (bin==NULL) return -1;
       i = DH_compute_key(&bin[1], pubkey, dh_params);
       DH_free(dh_params);
       if (i > 0) {
	   if (i != klen) {
	       bin = return_binary_shrink(rbuf,rlen,bin,1+i);
	   }       
	   bin[0] = 1;
	   return i + 1;
       }
       else {
	   bin[0] = 0;
	   return 1;
       }

    case DRV_MD4:
	bin = return_binary(rbuf,rlen,MD4_LEN);
	MD4((unsigned char *)buf, len, (unsigned char *)bin);
       return MD4_LEN;
       
    case DRV_MD4_INIT:
	bin = return_binary(rbuf,rlen,MD4_CTX_LEN);
	MD4_Init((MD4_CTX *) bin);
       return MD4_CTX_LEN;
       
    case DRV_MD4_UPDATE:
       if (len < MD4_CTX_LEN)
	  return -1;
       bin = return_binary(rbuf,rlen,MD4_CTX_LEN);
       memcpy(bin, buf, MD4_CTX_LEN);
       MD4_Update((MD4_CTX *) bin, buf + MD4_CTX_LEN, len - MD4_CTX_LEN);
       return MD4_CTX_LEN;
       
    case DRV_MD4_FINAL:
       if (len != MD4_CTX_LEN)
	  return -1;
       memcpy(&md4_ctx, buf, MD4_CTX_LEN); /* XXX Use buf only? */
       bin = return_binary(rbuf,rlen,MD4_LEN);
       MD4_Final((unsigned char *)bin, &md4_ctx);
       return MD4_LEN;

#if SSL_VERSION_0_9_8
    case DRV_SHA256:
       bin = return_binary(rbuf,rlen,SHA256_LEN);
       SHA256(buf, len, bin);
       return SHA256_LEN;
       
    case DRV_SHA256_INIT:
	bin = return_binary(rbuf,rlen,SHA256_CTX_LEN);
	SHA256_Init((SHA256_CTX *)bin);
       return SHA256_CTX_LEN;		

    case DRV_SHA256_UPDATE:
       if (len < SHA256_CTX_LEN)
	  return -1;
       bin = return_binary(rbuf,rlen,SHA256_CTX_LEN);
       memcpy(bin, buf, SHA256_CTX_LEN);
       SHA256_Update((SHA256_CTX *)bin, buf + SHA256_CTX_LEN, 
		     len - SHA256_CTX_LEN);
       return SHA256_CTX_LEN;		

    case DRV_SHA256_FINAL:
       if (len != SHA256_CTX_LEN)
	  return -1;
       memcpy(&sha256_ctx, buf, SHA256_CTX_LEN); /* XXX Use buf only? */
       bin = return_binary(rbuf,rlen,SHA256_LEN);
       SHA256_Final(bin, &sha256_ctx);
       return SHA256_LEN;

    case DRV_SHA512:
	bin = return_binary(rbuf,rlen,SHA512_LEN);
	SHA512(buf, len, bin);
       return SHA512_LEN;
       
    case DRV_SHA512_INIT:
	bin = return_binary(rbuf,rlen,SHA512_CTX_LEN);
	SHA512_Init((SHA512_CTX *)bin);
       return SHA512_CTX_LEN;		
       
    case DRV_SHA512_UPDATE:
       if (len < SHA512_CTX_LEN)
	  return -1;
	bin = return_binary(rbuf,rlen,SHA512_CTX_LEN);
	memcpy(bin, buf, SHA512_CTX_LEN);
	SHA512_Update((SHA512_CTX *)bin, buf + SHA512_CTX_LEN, 
		     len - SHA512_CTX_LEN);
       return SHA512_CTX_LEN;		
       
    case DRV_SHA512_FINAL:
       if (len != SHA512_CTX_LEN)
	  return -1;
       memcpy(&sha512_ctx, buf, SHA512_CTX_LEN); /* XXX Use buf only? */
       bin = return_binary(rbuf,rlen,SHA512_LEN));
       SHA512_Final(bin, &sha512_ctx);
       return SHA512_LEN;		
#endif
       
    case DRV_INFO_LIB:	
       {/* <<DrvVer:8, NameSize:8, Name:NameSize/binary, VerNum:32, VerStr/binary>> */
	  static const char libname[] = "OpenSSL";
	  unsigned name_sz = strlen(libname);
	  const char* ver = SSLeay_version(SSLEAY_VERSION);
	  unsigned ver_sz = strlen(ver);
	  dlen = 1+1+name_sz+4+ver_sz;
	  bin = return_binary(rbuf, rlen, dlen);
	  if (bin==NULL) return -1;
	  p = bin;
	  *p++ = 0; /* "driver version" for future use */
	  *p++ = name_sz;
	  memcpy(p, libname, name_sz);
	  p += name_sz;
	  put_int32(p,SSLeay()); /* OPENSSL_VERSION_NUMBER */
	  p += 4;
	  memcpy(p, ver, ver_sz);
       }
       return dlen;

    default:
       break;
    }
    return -1;
}


#ifdef OPENSSL_THREADS /* vvvvvvvvvvvvvvv OPENSSL_THREADS vvvvvvvvvvvvvvvv */

static INLINE void locking(int mode, ErlDrvRWLock* lock)
{
    switch(mode) {
    case CRYPTO_LOCK|CRYPTO_READ:
	erl_drv_rwlock_rlock(lock);
	break;
    case CRYPTO_LOCK|CRYPTO_WRITE:
	erl_drv_rwlock_rwlock(lock);
	break;
    case CRYPTO_UNLOCK|CRYPTO_READ:
	erl_drv_rwlock_runlock(lock);
	break;
    case CRYPTO_UNLOCK|CRYPTO_WRITE:
	erl_drv_rwlock_rwunlock(lock);
	break;
    default:
	ASSERT(!"Invalid lock mode");
    }
}

/* Callback from openssl for static locking
 */
static void locking_function(int mode, int n, const char *file, int line)
{
    ASSERT(n>=0 && n<CRYPTO_num_locks());

    locking(mode, lock_vec[n]);
}

/* Callback from openssl for thread id
 */
static unsigned long id_function(void)
{
    return (unsigned long) erl_drv_thread_self();
}

/* Callbacks for dynamic locking, not used by current openssl version (0.9.8)
 */
static struct CRYPTO_dynlock_value* dyn_create_function(const char *file, int line)
{
    return (struct CRYPTO_dynlock_value*) erl_drv_rwlock_create("crypto_drv_dyn");
}
static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value* ptr,const char *file, int line)
{
    locking(mode, (ErlDrvRWLock*)ptr);
}
static void dyn_destroy_function(struct CRYPTO_dynlock_value *ptr, const char *file, int line)
{
    erl_drv_rwlock_destroy((ErlDrvRWLock*)ptr);
}

#endif /* ^^^^^^^^^^^^^^^^^^^^^^ OPENSSL_THREADS ^^^^^^^^^^^^^^^^^^^^^^ */
    
/* HMAC */

static void hmac_md5(char *key, int klen, char *dbuf, int dlen, char *hmacbuf)
{
    MD5_CTX ctx;
    char ipad[HMAC_INT_LEN];
    char opad[HMAC_INT_LEN];
    unsigned char nkey[MD5_LEN];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT_LEN) {
        MD5_CTX kctx;

        MD5_Init(&kctx);
        MD5_Update(&kctx, key, klen);
        MD5_Final(nkey, &kctx);
        key = (char *) nkey;
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

static void hmac_sha1(char *key, int klen, char *dbuf, int dlen, 
                      char *hmacbuf)
{
    SHA_CTX ctx;
    char ipad[HMAC_INT_LEN];
    char opad[HMAC_INT_LEN];
    unsigned char nkey[SHA_LEN];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT_LEN) {
        SHA_CTX kctx;

        SHA1_Init(&kctx);
        SHA1_Update(&kctx, key, klen);
        SHA1_Final(nkey, &kctx);
        key = (char *) nkey;
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
