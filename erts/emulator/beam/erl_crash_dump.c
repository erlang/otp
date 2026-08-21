/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996-2026. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_crash_dump.h"

#include "erl_process.h"
#include "erl_db.h"
#include "bif.h"

typedef struct {
    fmtfn_t to;
    void *to_arg;

    void (*close)(void *to_arg);
} LayeredWriter;

#ifdef ENCRYPTED_CRASH_DUMPS
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/rsa.h>
#include <openssl/store.h>
#include <openssl/ui.h>
#include <openssl/x509.h>

#include "erl_printf.h"

#if OPENSSL_VERSION_NUMBER >= 0x30500000L
#  ifndef OPENSSL_NO_ML_KEM
#    define HAVE_ML_KEM
#  endif
#endif

#ifdef HAVE_ML_KEM
#  define ECD_ERROR_MESSAGE_OR_ML_KEM "or ML-KEM "
#else
#  define ECD_ERROR_MESSAGE_OR_ML_KEM
#endif

#define ECD_ERROR_MESSAGE_HEADER \
    "The emulator has been built with crash dump encryption enabled, and " \
    "will therefore refuse to start without a valid public RSA " \
    ECD_ERROR_MESSAGE_OR_ML_KEM \
    "key.\n\n"

#define AES_256_CBC_BLOCK_SIZE 16
#define AES_256_CBC_IV_SIZE AES_256_CBC_BLOCK_SIZE
#define AES_256_KEY_SIZE 32

static EVP_CIPHER_CTX *cde_ctx;
static unsigned char *cde_header;
static size_t cde_header_length;

static EVP_PKEY *crash_dump_get_public_key(const char **kem_op) {
    char key_path[PATH_MAX];
    size_t key_path_length = PATH_MAX;

    if (erts_sys_explicit_8bit_getenv("ERL_CRASH_DUMP_PUBLIC_KEY",
                                      key_path,
                                      &key_path_length) == 1) {
        OSSL_STORE_CTX *store = OSSL_STORE_open(key_path,
                                                UI_get_default_method(),
                                                NULL,
                                                NULL,
                                                NULL);

        if (store) {
            int too_weak = 0;

            while (!OSSL_STORE_eof(store)) {
                OSSL_STORE_INFO *item = OSSL_STORE_load(store);

                if (item) {
                    EVP_PKEY *public_key = NULL;

                    switch (OSSL_STORE_INFO_get_type(item)) {
                    case OSSL_STORE_INFO_CERT:
                        public_key =
                            X509_get_pubkey(OSSL_STORE_INFO_get0_CERT(item));
                        break;
                    case OSSL_STORE_INFO_PUBKEY:
                        public_key = OSSL_STORE_INFO_get1_PUBKEY(item);
                        break;
                    case OSSL_STORE_INFO_PKEY:
                        erts_fprintf(stderr,
                                     ECD_ERROR_MESSAGE_HEADER
                                     "The given file contains a private key, "
                                     "it should only contain a public RSA "
                                     ECD_ERROR_MESSAGE_OR_ML_KEM
                                     "key.\r\n");
                        exit(1);
                    default:
                        break;
                    }

                    if (public_key != NULL) {
                        if (EVP_PKEY_is_a(public_key, "RSA")) {
                            int key_size = EVP_PKEY_get_size(public_key);

                            if (key_size >= 256) {
                                OSSL_STORE_INFO_free(item);
                                OSSL_STORE_close(store);
                                *kem_op = "RSASVE";
                                return public_key;
                            }

                            EVP_PKEY_free(public_key);
                            too_weak = 1;
                        }
#ifdef HAVE_ML_KEM
                          else if (EVP_PKEY_is_a(public_key, "ML-KEM-512") ||
                                   EVP_PKEY_is_a(public_key, "ML-KEM-768") ||
                                   EVP_PKEY_is_a(public_key, "ML-KEM-1024")) {
                            OSSL_STORE_INFO_free(item);
                            OSSL_STORE_close(store);
                            *kem_op = "ML-KEM";
                            return public_key;
                        }
#endif
                    }

                    OSSL_STORE_INFO_free(item);
                }
            }

            if (too_weak) {
                erts_fprintf(stderr,
                            ECD_ERROR_MESSAGE_HEADER
                            "The given key is too weak (must be at least 2048 "
                            "bits for RSA).\r\n");
                exit(1);
            }
        }

        erts_fprintf(stderr,
                    ECD_ERROR_MESSAGE_HEADER
                    "The given file does not exist, or does not contain a "
                    "public RSA "
                    ECD_ERROR_MESSAGE_OR_ML_KEM
                    "key.\r\n");
        exit(1);
    }

    erts_fprintf(stderr,
                 ECD_ERROR_MESSAGE_HEADER
                 "Make sure that the ERL_CRASH_DUMP_PUBLIC_KEY environment "
                 "variable points at a file containing a public RSA "
                 ECD_ERROR_MESSAGE_OR_ML_KEM
                 "key.\r\n");
    exit(1);
}

static void
erl_crash_dump_init_envelope(EVP_PKEY *public_key,
                             const char *kem_op,
                             unsigned char **secret,
                             size_t *secret_size,
                             unsigned char iv[AES_256_CBC_IV_SIZE]) {
    const EVP_CIPHER *cipher;
    EVP_PKEY_CTX *public_ctx;
    unsigned char *key;
    size_t key_size;

    /* From here on we assume two things: the first is that we support all the
     * ciphers used (AES-256-CBC, RSA, ML-KEM), and the second is that that any
     * errors are straight-up bugs in our usage. We dump core if either
     * assumption turns out to be false at any point, including during dumping
     * itself.
     *
     * Note that we use malloc(2) here as this runs prior to setting up our own
     * allocators. */

    public_ctx = EVP_PKEY_CTX_new_from_pkey(NULL, public_key, NULL);
    ERTS_ASSERT(public_ctx);

    ERTS_ASSERT(EVP_PKEY_encapsulate_init(public_ctx, NULL) == 1);
    ERTS_ASSERT(EVP_PKEY_CTX_set_kem_op(public_ctx, kem_op) == 1);

    ERTS_ASSERT(EVP_PKEY_encapsulate(public_ctx,
                                     NULL,
                                     secret_size,
                                     NULL,
                                     &key_size) == 1);

    ERTS_ASSERT(key_size >= AES_256_KEY_SIZE);
    key = malloc(key_size);
    ERTS_ASSERT(key);

    ERTS_ASSERT(*secret_size >= AES_256_KEY_SIZE && *secret_size <= 65535);
    *secret = malloc(*secret_size);
    ERTS_ASSERT(*secret);

    ERTS_ASSERT(EVP_PKEY_encapsulate(public_ctx,
                                     *secret,
                                     secret_size,
                                     key,
                                     &key_size) == 1);
    EVP_PKEY_CTX_free(public_ctx);

    cipher = EVP_aes_256_cbc();
    ERTS_ASSERT(cipher);

    ERTS_ASSERT(EVP_CIPHER_get_key_length(cipher) == AES_256_KEY_SIZE);
    ERTS_ASSERT(EVP_CIPHER_get_iv_length(cipher) == AES_256_CBC_IV_SIZE);

    ERTS_ASSERT(RAND_bytes(iv, AES_256_CBC_IV_SIZE));

    cde_ctx = EVP_CIPHER_CTX_new();
    ERTS_ASSERT(cde_ctx);
    ERTS_ASSERT(EVP_EncryptInit_ex(cde_ctx,
                                   cipher,
                                   NULL,
                                   key,
                                   iv) == 1);
    OPENSSL_cleanse(key, key_size);
    free(key);
}

void erl_crash_dump_init() {
    unsigned char iv[AES_256_CBC_IV_SIZE];
    unsigned char *secret;
    size_t secret_size;
    EVP_PKEY *public_key;
    const char *kem_op;

    /* This is a dead-simple hybrid encryption scheme, where we encapsulate
     * the key used to encrypt the data (symmetrically) in an asymmetrically
     * encrypted header.
     *
     * See https://en.wikipedia.org/wiki/Hybrid_cryptosystem for more
     * details. */

    public_key = crash_dump_get_public_key(&kem_op);
    erl_crash_dump_init_envelope(public_key, kem_op, &secret, &secret_size, iv);
    EVP_PKEY_free(public_key);

    /* Build the crash-dump encryption header, which is necessary for
     * decryption.
     *
     * The first four bytes identify this is as an encrypted crash dump
     * (version 0), the next 16 bytes contain the IV, the two bytes after that
     * contain the length of the (encrypted) secret in little-endian, followed
     * by the secret. The (decrypted) secret contains a 256-bit AES key.
     *
     * Needless to say this is a bespoke format. While it would have been very
     * convenient to use a format compatible with OpenSSL's command line tool,
     * the latter only operates on raw files, and the logistics of splitting
     * this into separate files make doing so a non-starter. */
    {
        unsigned char *out;

        cde_header_length = 4 + AES_256_CBC_IV_SIZE + (2 + secret_size);
        cde_header = malloc(cde_header_length);
        ERTS_ASSERT(cde_header);
        out = cde_header;

        sys_memcpy(out, "ENC0", 4);
        out += 4;

        sys_memcpy(out, iv, AES_256_CBC_IV_SIZE);
        out += AES_256_CBC_IV_SIZE;

        ERTS_ASSERT(secret_size >= AES_256_KEY_SIZE && secret_size <= 65535);
        out[0] = (secret_size >> 0) & 0xFFu;
        out[1] = (secret_size >> 8) & 0xFFu;
        out += 2;

        sys_memcpy(out, secret, secret_size);
        out += secret_size;

        ERTS_ASSERT(out == &cde_header[cde_header_length]);
    }

    free(secret);
}

static int
crash_dump_encrypted_write(void *vfdp, char *buf, size_t len)
{
    LayeredWriter *inner = (LayeredWriter*)vfdp;
    size_t processed = 0;

    while (processed < len) {
        unsigned char cipher_buffer[128 + AES_256_CBC_BLOCK_SIZE];
        int chunk_size = MIN(len - processed, 128);
        int cipher_length;
        int res;

        ERTS_ASSERT(EVP_EncryptUpdate(cde_ctx,
                                      cipher_buffer,
                                      &cipher_length,
                                      (unsigned char*)&buf[processed],
                                      chunk_size) == 1);
        processed += chunk_size;

        res = inner->to(inner->to_arg, (char*)cipher_buffer, cipher_length);
        if (res < 0) {
            return res;
        }
    }

    return len;
}

static void
crash_dump_encrypted_close(void *to_arg) {
    LayeredWriter *inner = (LayeredWriter *) to_arg;
    unsigned char cipher_buffer[AES_256_CBC_BLOCK_SIZE];
    int cipher_length;

    ERTS_ASSERT(EVP_EncryptFinal(cde_ctx,
                                 cipher_buffer,
                                 &cipher_length) == 1);

    inner->to(inner->to_arg, (char*)cipher_buffer, cipher_length);
    inner->close(inner->to_arg);
}

#else /* !defined(ENCRYPTED_CRASH_DUMPS) */
void erl_crash_dump_init() {
    /* Nothing to do. */
}
#endif

static Sint64 crash_dump_limit = ERTS_SINT64_MAX;
static Sint64 crash_dump_written = 0;

static int
crash_dump_limited_write(void* vfdp, char* buf, size_t len)
{

    const char stop_msg[] = "\n=abort:CRASH DUMP SIZE LIMIT REACHED\n";
    LayeredWriter *inner = (LayeredWriter *) vfdp;

    crash_dump_written += len;
    if (crash_dump_written <= crash_dump_limit) {
        return inner->to(inner->to_arg, buf, len);
    }

    len -= (crash_dump_written - crash_dump_limit);
    inner->to(inner->to_arg, buf, len);
    inner->to(inner->to_arg, (char*)stop_msg, sizeof(stop_msg)-1);

    inner->close(inner->to_arg);

    /* We assume that crash dump was called from erts_exit_vv() */
    erts_exit_epilogue(0);
}

static void
crash_dump_limited_close(void *to_arg) {
    LayeredWriter *inner = (LayeredWriter *) to_arg;
    inner->close(inner->to_arg);
}

static void close_fd(void *to_arg) {
    close(*((int *)to_arg));
}

static void close_fp(void *to_arg) {
    fclose((FILE *) to_arg);
}

static int write_layered(void *to_arg, char *buf, size_t len) {
    LayeredWriter *inner = (LayeredWriter *)to_arg;
    return inner->to(inner->to_arg, buf, len);
}

void
erl_crash_dump_v(char *file, int line, const char* fmt, va_list args)
{
    const size_t WRITE_BUFFER_SIZE = 64 << 10;

    ErtsThrPrgrData tpd_buf; /* in case we aren't a managed thread... */
    int fd;
    size_t envsz;
    time_t now;
    char env[21]; /* enough to hold any 64-bit integer */
    size_t dumpnamebufsize = MAXPATHLEN;
    char dumpnamebuf[MAXPATHLEN];
    char* dumpname;
    int secs;
    int env_erl_crash_dump_seconds_set = 1;
    int i;
    FILE* fp = 0;
    LayeredWriter *outer;
    LayeredWriter base;
    LayeredWriter lwi;
#ifdef ENCRYPTED_CRASH_DUMPS
    LayeredWriter ewi;
#endif
    fmtfn_t to;
    void *to_arg;
    static char* write_buffer;  /* 'static' to avoid a leak warning in valgrind */

    /* Order all managed threads to block, this has to be done
       first to guarantee that this is the only thread to generate
       crash dump. */
    erts_thr_progress_fatal_error_block(&tpd_buf);

    /* Allow us to pass certain places without locking... */
    erts_atomic32_set_mb(&erts_writing_erl_crash_dump, 1);
    erts_tsd_set(erts_is_crash_dumping_key, (void *) 1);

    envsz = sizeof(env);
    /* ERL_CRASH_DUMP_SECONDS not set
     * if we have a heart port, break immediately
     * otherwise dump crash indefinitely (until crash is complete)
     * same as ERL_CRASH_DUMP_SECONDS = 0
     * - do not write dump
     * - do not set an alarm
     * - break immediately
     *
     * ERL_CRASH_DUMP_SECONDS = 0
     * - do not write dump
     * - do not set an alarm
     * - break immediately
     *
     * ERL_CRASH_DUMP_SECONDS < 0
     * - do not set alarm
     * - write dump until done
     *
     * ERL_CRASH_DUMP_SECONDS = S (and S positive)
     * - Don't dump file forever
     * - set alarm (set in sys)
     * - write dump until alarm or file is written completely
     */
        
    if (erts_sys_explicit_8bit_getenv("ERL_CRASH_DUMP_SECONDS", env, &envsz) == 1) {
        env_erl_crash_dump_seconds_set = 1;
        secs = atoi(env);
    } else {
        env_erl_crash_dump_seconds_set = 0;
        secs = -1;
    }

    if (secs == 0) {
        return;
    }

    /* erts_sys_prepare_crash_dump returns 1 if heart port is found, otherwise 0
     * If we don't find heart (0) and we don't have ERL_CRASH_DUMP_SECONDS set
     * we should continue writing a dump
     *
     * beware: secs -1 means no alarm
     */

    if (erts_sys_prepare_crash_dump(secs) && !env_erl_crash_dump_seconds_set ) {
        return;
    }

    crash_dump_limit = ERTS_SINT64_MAX;
    envsz = sizeof(env);

    if (erts_sys_explicit_8bit_getenv("ERL_CRASH_DUMP_BYTES",
                                      env,
                                      &envsz) == 1) {
        Sint64 limit;
        char* endptr;
        errno = 0;

        limit = ErtsStrToSint64(env, &endptr, 10);

        if (errno == 0 && limit >= 0 && endptr != env && *endptr == 0) {
            if (limit == 0) {
                return;
            }

#ifdef ENCRYPTED_CRASH_DUMPS
            if (limit <= cde_header_length) {
                return;
            }
#endif

            crash_dump_limit = limit;
        }
    }

    if (erts_sys_explicit_8bit_getenv("ERL_CRASH_DUMP",
                                      &dumpnamebuf[0],
                                      &dumpnamebufsize) != 1) {
        dumpname = "erl_crash.dump";
    } else {
        dumpname = &dumpnamebuf[0];
    }

    erts_fprintf(stderr,"\nCrash dump is being written to: %s...", dumpname);

    fd = open(dumpname,O_WRONLY | O_CREAT | O_TRUNC,0640);
    if (fd < 0) {
        return; /* Can't create the crash dump, skip it */
    }

#ifdef ENCRYPTED_CRASH_DUMPS
    /* The encryption header must be written verbatim without any other
     * processing. */
    if (erts_write_fd(&fd, (char*)cde_header, cde_header_length) < 0) {
        close(fd);
        return;
    }

    crash_dump_written += cde_header_length;
#endif

    /*
     * Wrap into a FILE* so that we can use buffered output. Set an
     * explicit buffer to make sure the first write does not fail because
     * of a failure to allocate a buffer.
     */
    write_buffer = (char *) erts_alloc_fnf(ERTS_ALC_T_TMP, WRITE_BUFFER_SIZE);
    if (write_buffer && (fp = fdopen(fd, "w")) != NULL) {
        setvbuf(fp, write_buffer, _IOFBF, WRITE_BUFFER_SIZE);
        base.to = &erts_write_fp;
        base.to_arg = (void*)fp;
        base.close = &close_fp;
    } else {
        base.to = &erts_write_fd;
        base.to_arg = (void*)&fd;
        base.close = &close_fd;
    }

    outer = &base;

#ifdef ENCRYPTED_CRASH_DUMPS
    ewi.to = &crash_dump_encrypted_write;
    ewi.to_arg = outer;
    ewi.close = &crash_dump_encrypted_close;

    outer = &ewi;
#endif

    if (crash_dump_limit < ERTS_SINT64_MAX) {
        lwi.to = &crash_dump_limited_write;
        lwi.to_arg = outer;
        lwi.close = &crash_dump_limited_close;

        outer = &lwi;
    }

    to = write_layered;
    to_arg = outer;

    time(&now);
    erts_cbprintf(to, to_arg, "=erl_crash_dump:0.5\n%s", ctime(&now));

#ifdef ERTS_SYS_SUSPEND_SIGNAL
    /*
     * We suspend all scheduler threads so that we can dump some
     * data about the currently running processes and scheduler data.
     * We have to be very very careful when doing this as the schedulers
     * could be anywhere.
     * It may happen that scheduler thread is suspended while holding
     * malloc lock. Therefore code running in this thread must not use
     * it, or it will deadlock. ctime and fdopen calls both use malloc
     * internally and must be executed prior to.
     */
    sys_init_suspend_handler();

    for (i = 0; i < erts_no_schedulers; i++) {
        erts_tid_t tid = ERTS_SCHEDULER_IX(i)->tid;
        if (!erts_equal_tids(tid,erts_thr_self()))
            sys_thr_suspend(tid);
    }

#endif

    if (file != NULL)
       erts_cbprintf(to, to_arg, "The error occurred in file %s, line %d\n", file, line);

    if (fmt != NULL && *fmt != '\0') {
        erts_cbprintf(to, to_arg, "Slogan: ");
        erts_vcbprintf(to, to_arg, fmt, args);
    }

    erts_cbprintf(to, to_arg, "System version: ");
    erts_print_system_version(to, to_arg, NULL);

    erts_cbprintf(to, to_arg, "Taints: ");
    erts_print_nif_taints(to, to_arg);
    erts_cbprintf(to, to_arg, "Atoms: %d\n", atom_table_size());

    /* We want to note which thread it was that called erts_exit */
    if (erts_get_scheduler_data()) {
        erts_cbprintf(to, to_arg, "Calling Thread: scheduler:%d\n",
                      erts_get_scheduler_data()->no);
    } else {
        if (!erts_thr_getname(erts_thr_self(), dumpnamebuf, MAXPATHLEN)) {
            erts_cbprintf(to, to_arg, "Calling Thread: %s\n", dumpnamebuf);
        } else {
            erts_cbprintf(to, to_arg, "Calling Thread: %p\n", erts_thr_self());
        }
    }

#if defined(ERTS_HAVE_TRY_CATCH)

    /*
     * erts_print_scheduler_info is not guaranteed to be safe to call
     * here for all schedulers as we may have suspended a scheduler
     * in the middle of updating the STACK_TOP and STACK_START
     * variables and thus when scanning the stack we could get
     * segmentation faults. We protect against this very unlikely
     * scenario by using the ERTS_SYS_TRY_CATCH.
     */
    for (i = 0; i < erts_no_schedulers; i++) {
        ERTS_SYS_TRY_CATCH(
            erts_print_scheduler_info(to, to_arg, ERTS_SCHEDULER_IX(i)),
            erts_cbprintf(to, to_arg, "** crashed **\n"));
    }
    for (i = 0; i < erts_no_dirty_cpu_schedulers; i++) {
        ERTS_SYS_TRY_CATCH(
            erts_print_scheduler_info(to, to_arg, ERTS_DIRTY_CPU_SCHEDULER_IX(i)),
            erts_cbprintf(to, to_arg, "** crashed **\n"));
    }
    erts_cbprintf(to, to_arg, "=dirty_cpu_run_queue\n");
    erts_print_run_queue_info(to, to_arg, ERTS_DIRTY_CPU_RUNQ);

    for (i = 0; i < erts_no_dirty_io_schedulers; i++) {
        ERTS_SYS_TRY_CATCH(
            erts_print_scheduler_info(to, to_arg, ERTS_DIRTY_IO_SCHEDULER_IX(i)),
            erts_cbprintf(to, to_arg, "** crashed **\n"));
    }
    erts_cbprintf(to, to_arg, "=dirty_io_run_queue\n");
    erts_print_run_queue_info(to, to_arg, ERTS_DIRTY_IO_RUNQ);
#endif


#ifdef ERTS_SYS_SUSPEND_SIGNAL

    /* We resume all schedulers so that we are in a known safe state
       when we write the rest of the crash dump */
    for (i = 0; i < erts_no_schedulers; i++) {
        erts_tid_t tid = ERTS_SCHEDULER_IX(i)->tid;
        if (!erts_equal_tids(tid,erts_thr_self())) {
            sys_thr_resume(tid);
        }
    }
#endif

    /*
     * Wait for all managed threads to block. If all threads haven't blocked
     * after a minute, we go anyway and hope for the best...
     *
     * We do not release system again. We expect an exit() or abort() after
     * dump has been written.
     */
    erts_thr_progress_fatal_error_wait(60000);
    /* Either worked or not... */

#ifndef ERTS_HAVE_TRY_CATCH
    /* This is safe to call here, as all schedulers are blocked */
    for (i = 0; i < erts_no_schedulers; i++) {
        erts_print_scheduler_info(to, to_arg, ERTS_SCHEDULER_IX(i));
    }
#endif
    
    info(to, to_arg); /* General system info */
    if (erts_ptab_initialized(&erts_proc)) {
        process_info(to, to_arg); /* Info about each process and port */
    }

    db_info(to, to_arg, false);
    erts_print_bif_timer_info(to, to_arg);
    distribution_info(to, to_arg);
    erts_cbprintf(to, to_arg, "=loaded_modules\n");
    loaded(to, to_arg);
    erts_dump_fun_entries(to, to_arg);
    erts_deep_process_dump(to, to_arg);
    erts_cbprintf(to, to_arg, "=atoms\n");
    dump_atoms(to, to_arg);

    erts_cbprintf(to, to_arg, "=end\n");

    outer->close(outer->to_arg);

    erts_fprintf(stderr,"done\n");
}
