/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017-2017. All Rights Reserved.
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

#ifdef _WIN32
#define OPENSSL_OPT_WINDLL
#endif
#include <stdio.h>
#include <string.h>

#include <openssl/engine.h>
#include <openssl/md5.h>
#include <openssl/rsa.h>
#include <openssl/pem.h>

#define PACKED_OPENSSL_VERSION(MAJ, MIN, FIX, P)	\
    ((((((((MAJ << 8) | MIN) << 8 ) | FIX) << 8) | (P-'a'+1)) << 4) | 0xf)

#define PACKED_OPENSSL_VERSION_PLAIN(MAJ, MIN, FIX) \
    PACKED_OPENSSL_VERSION(MAJ,MIN,FIX,('a'-1))

#if OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION_PLAIN(1,1,0) \
    || defined(LIBRESSL_VERSION_NUMBER)
#define OLD
#endif

static const char *test_engine_id = "MD5";
static const char *test_engine_name = "MD5 test engine";

/* The callback that does the job of fetching keys on demand by the Engine */
EVP_PKEY* test_key_load(ENGINE *er, const char *id, UI_METHOD *ui_method, void *callback_data);


static int test_init(ENGINE *e) {
    printf("OTP Test Engine Initializatzion!\r\n");
    
    /* Load all digest and cipher algorithms. Needed for password protected private keys */
    OpenSSL_add_all_algorithms();

    return 111;
}

static void add_test_data(unsigned char *md, unsigned int len)
{
    unsigned int i;

    for (i=0; i<len; i++) {
        md[i] = (unsigned char)(i & 0xff);
    }
}

/* MD5 part */
#undef data
#ifdef OLD
#define data(ctx) ((MD5_CTX *)ctx->md_data)
#endif

static int test_engine_md5_init(EVP_MD_CTX *ctx) {
    fprintf(stderr, "MD5 initialized\r\n");
#ifdef OLD
    return MD5_Init(data(ctx));
#else
    return 1;
#endif
}

static int test_engine_md5_update(EVP_MD_CTX *ctx,const void *data, size_t count)
{
    fprintf(stderr, "MD5 update\r\n");
#ifdef OLD
    return MD5_Update(data(ctx), data, (size_t)count);
#else
    return 1;
#endif
}

static int test_engine_md5_final(EVP_MD_CTX *ctx,unsigned char *md) {
#ifdef OLD
    int ret;

    fprintf(stderr, "MD5 final size of EVP_MD: %lu\r\n", sizeof(EVP_MD));
    ret = MD5_Final(md, data(ctx));

    if (ret > 0) {
         add_test_data(md, MD5_DIGEST_LENGTH);
    }
    return ret;
#else
    fprintf(stderr, "MD5 final\r\n");
    add_test_data(md, MD5_DIGEST_LENGTH);
    return 1;
#endif
}

#ifdef OLD
static EVP_MD test_engine_md5_method=  {
        NID_md5,                      /* The name ID for MD5 */
        NID_undef,                    /* IGNORED: MD5 with private key encryption NID */
        MD5_DIGEST_LENGTH,            /* Size of MD5 result, in bytes */
        0,                            /* Flags */
        test_engine_md5_init,         /* digest init */
        test_engine_md5_update,       /* digest update */
        test_engine_md5_final,        /* digest final */
        NULL,                         /* digest copy */
        NULL,                         /* digest cleanup */
        EVP_PKEY_NULL_method,         /* IGNORED: pkey methods */
        MD5_CBLOCK,                   /* Internal blocksize, see rfc1321/md5.h */
        sizeof(EVP_MD *) + sizeof(MD5_CTX),
        NULL,                          /* IGNORED: control function */
};
#endif

static int test_digest_ids[] = {NID_md5};

static int test_engine_digest_selector(ENGINE *e, const EVP_MD **digest,
        const int **nids, int nid) {
    int ok = 1;
    if (!digest) {
        *nids = test_digest_ids;
        fprintf(stderr, "Digest is empty! Nid:%d\r\n", nid);
        return 2;
    }
    fprintf(stderr, "Digest no %d requested\r\n",nid);
    if (nid == NID_md5) {
#ifdef OLD
        *digest = &test_engine_md5_method;
#else
        EVP_MD *md = EVP_MD_meth_new(NID_md5, NID_undef);
        if (!md ||
            !EVP_MD_meth_set_result_size(md, MD5_DIGEST_LENGTH) ||
            !EVP_MD_meth_set_flags(md, 0) ||
            !EVP_MD_meth_set_init(md,  test_engine_md5_init) ||
            !EVP_MD_meth_set_update(md,  test_engine_md5_update) ||
            !EVP_MD_meth_set_final(md,  test_engine_md5_final) ||
            !EVP_MD_meth_set_copy(md,  NULL) ||
            !EVP_MD_meth_set_cleanup(md,  NULL) ||
            !EVP_MD_meth_set_input_blocksize(md, MD5_CBLOCK) ||
            !EVP_MD_meth_set_app_datasize(md, sizeof(EVP_MD *) + sizeof(MD5_CTX)) ||
            !EVP_MD_meth_set_ctrl(md, NULL))
            {
                ok = 0;
                *digest = NULL;
            } else
            {
                *digest = md;
            }
#endif
    }
    else {
        ok = 0;
        *digest = NULL;
    }
    
    return ok;
}


static int bind_helper(ENGINE * e, const char *id)
{
    if (!ENGINE_set_id(e, test_engine_id) ||
        !ENGINE_set_name(e, test_engine_name) ||
        !ENGINE_set_init_function(e, test_init) ||
        !ENGINE_set_digests(e, &test_engine_digest_selector) ||
        /* For testing of key storage in an Engine: */
        !ENGINE_set_load_privkey_function(e, &test_key_load) ||
        !ENGINE_set_load_pubkey_function(e, &test_key_load)
    )
        return 0;

    return 1;
}

IMPLEMENT_DYNAMIC_CHECK_FN();

IMPLEMENT_DYNAMIC_BIND_FN(bind_helper);

/********************************************************
 *
 * Engine storage simulation
 *
 */
int pem_passwd_cb_fun(char *buf, int size, int rwflag, void *password);

EVP_PKEY* test_key_load(ENGINE *er, const char *id, UI_METHOD *ui_method, void *callback_data)
{
    EVP_PKEY *pkey = NULL;
    FILE *f = fopen(id, "r");

    if (!f) {
            fprintf(stderr, "%s:%d fopen(%s) failed\r\n", __FILE__,__LINE__,id);
            return NULL;
    }

    /* First try to read as a private key. If that fails, try to read as a public key: */
    pkey = PEM_read_PrivateKey(f, NULL, pem_passwd_cb_fun, callback_data);
    if (!pkey) {
        /* ERR_print_errors_fp (stderr); */
        fclose(f);
        f = fopen(id, "r");
        pkey = PEM_read_PUBKEY(f, NULL, NULL, NULL);
    }
    fclose(f);
    
    if (!pkey) {
        fprintf(stderr, "%s:%d Key read from file %s failed.\r\n", __FILE__,__LINE__,id);
        if (callback_data) 
            fprintf(stderr, "Pwd = \"%s\".\r\n", (char *)callback_data);
        fprintf(stderr, "Contents of file \"%s\":\r\n",id);
        f = fopen(id, "r");
        { /* Print the contents of the key file */
            char c;
            while (!feof(f)) {
                switch (c=fgetc(f)) {
                case '\n':
                case '\r': putc('\r',stderr); putc('\n',stderr); break;
                default: putc(c, stderr);
                }
            }
        }
        fprintf(stderr, "File contents printed.\r\n");
        fclose(f);
        return NULL;
    }
    
    return pkey;
}


int pem_passwd_cb_fun(char *buf, int size, int rwflag, void *password) 
{ 
    int i;

    fprintf(stderr, "In pem_passwd_cb_fun\r\n");
    if (!password)
        return 0;

    i = strlen(password);
    if (i < size) {
        /* whole pwd (incl terminating 0) fits */
        fprintf(stderr, "Got FULL pwd %d(%d) chars\r\n", i, size);
        memcpy(buf, (char*)password, i+1);
        return i+1;
    } else {
        fprintf(stderr, "Got TO LONG pwd %d(%d) chars\r\n", i, size);
        /* meaningless with a truncated password */
        return 0;
    }
}
