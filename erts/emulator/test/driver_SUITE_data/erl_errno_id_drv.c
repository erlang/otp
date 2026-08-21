/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
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

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "erl_driver.h"

static ErlDrvData start(ErlDrvPort port, char *buf);
static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);

static ErlDrvEntry erl_errno_id_entry = {
    NULL /* init */,
    start,
    NULL /* stop */,
    output,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "erl_errno_id_drv",
    NULL /* finish */,
    NULL /* handle */,
    NULL /* control */,
    NULL /* timeout */,
    NULL /* outputv */,
    NULL /* ready_async */,
    NULL /* flush */,
    NULL /* call */,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    NULL /* handle2 */,
    NULL /* handle_monitor */
};

DRIVER_INIT(erl_errno_id_drv)
{
    return &erl_errno_id_entry;
}

static ErlDrvData start(ErlDrvPort port, char *buf)
{
    return (ErlDrvData)port;
}

char *to_lower_str(char *str)
{
    size_t sz = strlen(str) + 1;
    int i = 0;
    char *res = malloc(sz);
    if (!res) {
        return NULL;
    }
    do {
        res[i] = (char) tolower(str[i]);
    } while (str[i++]);
    return res;
}

static void
send_ok(ErlDrvData drv_data)
{
    int res;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, driver_mk_atom("ok"),
        ERL_DRV_PORT, driver_mk_port((ErlDrvPort) drv_data),
        ERL_DRV_TUPLE,	(ErlDrvTermData) 2
    };
    res = erl_drv_output_term(driver_mk_port((ErlDrvPort) drv_data),
                              spec,
                              sizeof(spec)/sizeof(ErlDrvTermData));
    if (res <= 0) {
        driver_failure_atom((ErlDrvPort) drv_data,
                            "erl_drv_output_term() failed");
    }
}

static void
send_error(ErlDrvData drv_data,
           char *errno_str,
           char *exp,
           char *got)
{
    int res;
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, driver_mk_atom("error"),
        ERL_DRV_PORT, driver_mk_port((ErlDrvPort) drv_data),
        ERL_DRV_STRING, (ErlDrvTermData) errno_str, (ErlDrvTermData) strlen(errno_str),
        ERL_DRV_STRING, (ErlDrvTermData) exp, (ErlDrvTermData) strlen(exp),
        ERL_DRV_STRING, (ErlDrvTermData) got, (ErlDrvTermData) strlen(got),
        ERL_DRV_TUPLE,	(ErlDrvTermData) 5
    };
    res = erl_drv_output_term(driver_mk_port((ErlDrvPort) drv_data),
                              spec,
                              sizeof(spec)/sizeof(ErlDrvTermData));
    if (res <= 0) {
        driver_failure_atom((ErlDrvPort) drv_data,
                            "erl_drv_output_term() failed");
    }
}

#if 0
#define DEBUG_PRINT(ENO, ENOSTR, STR) \
    fprintf(stderr, "%d | %s | %s\r\n", ENO, ENOSTR, STR)
#else
#define DEBUG_PRINT(ENO, ENOSTR, STR)
#endif

#define TEST_ERRNO_IMPL(Eno, EnoStr, Alt1, Alt2, Alt3)                  \
    do {                                                                \
        char *res = erl_errno_id(Eno);                                  \
        char *exp = to_lower_str(EnoStr);                               \
        DEBUG_PRINT(Eno, EnoStr, res);                                  \
        if (!exp) {                                                     \
            driver_failure_posix((ErlDrvPort) drv_data, ENOMEM);        \
            return;                                                     \
        }                                                               \
        if (strcmp(res, exp) != 0) {                                    \
            char *alts[] = {Alt1, Alt2, Alt3, NULL};                    \
            int i = 0;                                                  \
            while (alts[i]) {                                           \
                if (strcmp(res, alts[i]) == 0) {                        \
                    break;                                              \
                }                                                       \
                i++;                                                    \
            }                                                           \
            if (!alts[i]) {                                             \
                send_error(drv_data, EnoStr, exp, res);                 \
                free(exp);                                              \
                return;                                                 \
            }                                                           \
        }                                                               \
        free(exp);                                                      \
    } while(0)

#define TEST_ERRNO(Eno) TEST_ERRNO_IMPL(Eno, #Eno, NULL, NULL, NULL)
#define TEST_ERRNO_ALT1(Eno, Alt1) TEST_ERRNO_IMPL(Eno, #Eno, Alt1, NULL, NULL)
#define TEST_ERRNO_ALT2(Eno, Alt1, Alt2) TEST_ERRNO_IMPL(Eno, #Eno, Alt1, Alt2, NULL)
#define TEST_ERRNO_ALT3(Eno, Alt1, Alt2, Alt3) TEST_ERRNO_IMPL(Eno, #Eno, Alt1, Alt2, Alt3)

#define TEST_ERRNO_UNKNOWN(RES, INT)                            \
    do {                                                        \
        RES = erl_errno_id(INT);                                \
        DEBUG_PRINT(INT, #INT, RES);                            \
        if (strcmp(RES, "errno_" #INT) != 0) {                  \
            send_error(drv_data, #INT, "errno_" #INT, RES);     \
            return;                                             \
        }                                                       \
    } while (0)

static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    char *neg_4711_res, *res;
    /* Test all POSIX.1-2017 errno names... */
#ifdef E2BIG
    TEST_ERRNO(E2BIG);
#endif
#ifdef EACCES
    TEST_ERRNO(EACCES);
#endif
#ifdef EADDRINUSE
    TEST_ERRNO(EADDRINUSE);
#endif
#ifdef EADDRNOTAVAIL
    TEST_ERRNO(EADDRNOTAVAIL);
#endif
#ifdef EAFNOSUPPORT
    TEST_ERRNO(EAFNOSUPPORT);
#endif
#ifdef EAGAIN
    TEST_ERRNO(EAGAIN);
#endif
#ifdef EALREADY
    TEST_ERRNO_ALT1(EALREADY, "ebusy");
#endif
#ifdef EBADF
    TEST_ERRNO(EBADF);
#endif
#ifdef EBADMSG
    TEST_ERRNO(EBADMSG);
#endif
#ifdef EBUSY
    TEST_ERRNO(EBUSY);
#endif
#ifdef ECANCELED
    TEST_ERRNO(ECANCELED);
#endif
#ifdef ECHILD
    TEST_ERRNO(ECHILD);
#endif
#ifdef ECONNABORTED
    TEST_ERRNO(ECONNABORTED);
#endif
#ifdef ECONNREFUSED
    TEST_ERRNO(ECONNREFUSED);
#endif
#ifdef ECONNRESET
    TEST_ERRNO(ECONNRESET);
#endif
#ifdef EDEADLK
    TEST_ERRNO_ALT2(EDEADLK, "ewouldblock", "eagain");
#endif
#ifdef EDESTADDRREQ
    TEST_ERRNO(EDESTADDRREQ);
#endif
#ifdef EDOM
    TEST_ERRNO(EDOM);
#endif
#ifdef EDQUOT
    TEST_ERRNO(EDQUOT);
#endif
#ifdef EEXIST
    TEST_ERRNO(EEXIST);
#endif
#ifdef EFAULT
    TEST_ERRNO(EFAULT);
#endif
#ifdef EFBIG
    TEST_ERRNO(EFBIG);
#endif
#ifdef EHOSTUNREACH
    TEST_ERRNO(EHOSTUNREACH);
#endif
#ifdef EIDRM
    TEST_ERRNO_ALT1(EIDRM, "einprogress");
#endif
#ifdef EILSEQ
    TEST_ERRNO(EILSEQ);
#endif
#ifdef EINPROGRESS
    TEST_ERRNO(EINPROGRESS);
#endif
#ifdef EINTR
    TEST_ERRNO(EINTR);
#endif
#ifdef EINVAL
    TEST_ERRNO(EINVAL);
#endif
#ifdef EIO
    TEST_ERRNO(EIO);
#endif
#ifdef EISCONN
    TEST_ERRNO(EISCONN);
#endif
#ifdef EISDIR
    TEST_ERRNO(EISDIR);
#endif
#ifdef ELOOP
    TEST_ERRNO_ALT1(ELOOP, "enoent");
#endif
#ifdef EMFILE
    TEST_ERRNO(EMFILE);
#endif
#ifdef EMLINK
    TEST_ERRNO(EMLINK);
#endif
#ifdef EMSGSIZE
    TEST_ERRNO(EMSGSIZE);
#endif
#ifdef EMULTIHOP
    TEST_ERRNO(EMULTIHOP);
#endif
#ifdef ENAMETOOLONG
    TEST_ERRNO(ENAMETOOLONG);
#endif
#ifdef ENETDOWN
    TEST_ERRNO(ENETDOWN);
#endif
#ifdef ENETRESET
    TEST_ERRNO(ENETRESET);
#endif
#ifdef ENETUNREACH
    TEST_ERRNO(ENETUNREACH);
#endif
#ifdef ENFILE
    TEST_ERRNO(ENFILE);
#endif
#ifdef ENOBUFS
    TEST_ERRNO_ALT2(ENOBUFS, "enosr", "enametoolong");
#endif
#ifdef ENODATA
    TEST_ERRNO_ALT1(ENODATA, "econnrefused");
#endif
#ifdef ENODEV
    TEST_ERRNO(ENODEV);
#endif
#ifdef ENOENT
    TEST_ERRNO(ENOENT);
#endif
#ifdef ENOEXEC
    TEST_ERRNO(ENOEXEC);
#endif
#ifdef ENOLCK
    TEST_ERRNO(ENOLCK);
#endif
#ifdef ENOLINK
    TEST_ERRNO(ENOLINK);
#endif
#ifdef ENOMEM
    TEST_ERRNO(ENOMEM);
#endif
#ifdef ENOMSG
    TEST_ERRNO(ENOMSG);
#endif
#ifdef ENOPROTOOPT
    TEST_ERRNO(ENOPROTOOPT);
#endif
#ifdef ENOSPC
    TEST_ERRNO(ENOSPC);
#endif
#ifdef ENOSR
    TEST_ERRNO_ALT1(ENOSR, "enametoolong");
#endif
#ifdef ENOSTR
    TEST_ERRNO_ALT1(ENOSTR, "enotty");
#endif
#ifdef ENOSYS
    TEST_ERRNO(ENOSYS);
#endif
#ifdef ENOTCONN
    TEST_ERRNO(ENOTCONN);
#endif
#ifdef ENOTDIR
    TEST_ERRNO(ENOTDIR);
#endif
#ifdef ENOTEMPTY
    TEST_ERRNO_ALT1(ENOTEMPTY, "eexist");
#endif
#ifdef ENOTRECOVERABLE
    TEST_ERRNO(ENOTRECOVERABLE);
#endif
#ifdef ENOTSOCK
    TEST_ERRNO(ENOTSOCK);
#endif
#ifdef ENOTSUP
    TEST_ERRNO(ENOTSUP);
#endif
#ifdef ENOTTY
    TEST_ERRNO(ENOTTY);
#endif
#ifdef ENXIO
    TEST_ERRNO(ENXIO);
#endif
#ifdef EOPNOTSUPP
    TEST_ERRNO_ALT1(EOPNOTSUPP, "enotsup");
#endif
#ifdef EOVERFLOW
    TEST_ERRNO(EOVERFLOW);
#endif
#ifdef EOWNERDEAD
    TEST_ERRNO(EOWNERDEAD);
#endif
#ifdef EPERM
    TEST_ERRNO(EPERM);
#endif
#ifdef EPIPE
    TEST_ERRNO(EPIPE);
#endif
#ifdef EPROTO
    TEST_ERRNO(EPROTO);
#endif
#ifdef EPROTONOSUPPORT
    TEST_ERRNO(EPROTONOSUPPORT);
#endif
#ifdef EPROTOTYPE
    TEST_ERRNO(EPROTOTYPE);
#endif
#ifdef ERANGE
    TEST_ERRNO(ERANGE);
#endif
#ifdef EROFS
    TEST_ERRNO(EROFS);
#endif
#ifdef ESPIPE
    TEST_ERRNO(ESPIPE);
#endif
#ifdef ESRCH
    TEST_ERRNO(ESRCH);
#endif
#ifdef ESTALE
    TEST_ERRNO(ESTALE);
#endif
#ifdef ETIME
    TEST_ERRNO_ALT2(ETIME, "eloop", "enoent");
#endif
#ifdef ETIMEDOUT
    TEST_ERRNO_ALT3(ETIMEDOUT, "enostr", "enotty", "eagain");
#endif
#ifdef ETXTBSY
    TEST_ERRNO(ETXTBSY);
#endif
#ifdef EWOULDBLOCK
    TEST_ERRNO_ALT1(EWOULDBLOCK, "eagain");
#endif
#ifdef EXDEV
    TEST_ERRNO(EXDEV);
#endif

    TEST_ERRNO_UNKNOWN(res, -4711);
    neg_4711_res = res;
    TEST_ERRNO_UNKNOWN(res, -1);
    TEST_ERRNO_UNKNOWN(res, 0);
    TEST_ERRNO_UNKNOWN(res, 4711);

    if (neg_4711_res != erl_errno_id(-4711)) {
        driver_failure_atom((ErlDrvPort) drv_data,
                            "result for -4711 moved");
        return;
    }

    send_ok(drv_data);
}
