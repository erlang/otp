/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef __WIN32__
#ifndef WINDOWS_H_INCLUDES_WINSOCK2_H
#include <winsock2.h>
#endif
#include <windows.h>
#endif

#include "erl_errno.h"
#include "sys.h"

#include <string.h>
#include "hash.h"
#include "erl_driver.h"
#include "erl_alloc.h"

#if defined(HAVE_STRERRORNAME_NP)
#  define ERTS_HAVE_ERRNO_NAME_LOOKUP_FUNC
#endif

#define ERTS_UNKNOWN_ERRNO_FORMAT "errno_%d"
#define ERTS_UNKNOWN_ERRNO_BUF_SZ 30 /* Big enough even if errno should be 64-bit integer */

static char **errno_map;
static int max_errno = 0;
static erts_rwmtx_t errno_rwmtx;
static int hash_initialized = 0;
static Hash *errno_table = NULL;
static bool used_before_init = false;
static bool init_done = false;
static bool late_init_done = false;

#ifdef DEBUG
#  define ERTS_MAX_CACHED_ERRNO 5
#else
#  define ERTS_MAX_CACHED_ERRNO 1000
#endif

#define ERTS_DEFAULT_ERRNO_STRING_CHARS 4

typedef struct {
    HashBucket bkt;
    int eno;
    char str[ERTS_DEFAULT_ERRNO_STRING_CHARS];
} ErtsErrnoEntry;

static void *
alloc(ErtsAlcType_t type, Uint size)
{
    if (used_before_init) {
        return malloc(size);
    }
    else {
        return erts_alloc(type, size);
    }
}

static void *
permanent_alloc(ErtsAlcType_t type, Uint size)
{
    if (used_before_init) {
        return malloc(size);
    }
    else {
        return erts_alloc_permanent_cache_aligned(type, size);
    }
}

static void
dealloc(ErtsAlcType_t type, void *ptr)
{
    if (used_before_init) {
        free(ptr);
    }
    else {
        erts_free(type, ptr);
    }
}

static char *
tolower_copy_estring(char *dst, char *end, const char *src)
{
    char *ptr = dst;
    char c;
    int i = 0;
    do {
        if (ptr > end) {
            erts_exit(ERTS_ABORT_EXIT, "Errno map overrun!\n");
        }
        c = src[i++];
        if ('A' <= c && c <= 'Z') {
            c += ('a' - 'A');
        }
        *(ptr++) = c;
    } while (c != '\0');

    return ptr;
}

static ErtsErrnoEntry *
make_errno_entry(int eno, char *str)
{
    ErtsErrnoEntry *entry;
    size_t len, sz;

    sz = sizeof(ErtsErrnoEntry);
    len = strlen(str) + 1;
    if (len > ERTS_DEFAULT_ERRNO_STRING_CHARS) {
        sz += len - ERTS_DEFAULT_ERRNO_STRING_CHARS;
    }
    entry = (ErtsErrnoEntry *) alloc(ERTS_ALC_T_ENO_ENTRY, sz);
    entry->eno = eno;
    (void) tolower_copy_estring(&entry->str[0], &entry->str[0] + len, str);
    return entry;
}

static int errno_cmp(void *ve1, void *ve2)
{
    ErtsErrnoEntry *e1 = ve1;
    ErtsErrnoEntry *e2 = ve2;
    return e1->eno != e2->eno;
}

static HashValue errno_hash(void *ve)
{
    ErtsErrnoEntry *e = ve;
    return ((HashValue) e->eno) * ((HashValue) 268437017);
}

static void *errno_alloc(void *ve)
{
    /*
     * We allocate the element before hash_put() and pass it as
     * template which we get as input...
     */
    return ve;
}

static void errno_free(void *ve)
{
    /*
     * We *never* remove any elements that has been inserted...
     */
}

static void *errno_meta_alloc(int i, size_t size)
{
    return alloc(ERTS_ALC_T_ENO_TAB_BKTS, size);
}

static void errno_meta_free(int i, void *ptr)
{
    dealloc(ERTS_ALC_T_ENO_TAB_BKTS, ptr);
}

static void
init_hash_table(void)
{
    if (!hash_initialized) {
        HashFunctions hash_funcs;

        errno_table = permanent_alloc(ERTS_ALC_T_ENO_TAB, sizeof(Hash));
        hash_funcs.hash = errno_hash;
        hash_funcs.cmp = errno_cmp;

        hash_funcs.alloc = errno_alloc;
        hash_funcs.free = errno_free;
        hash_funcs.meta_alloc = errno_meta_alloc;
        hash_funcs.meta_free = errno_meta_free;
        hash_funcs.meta_print = erts_print;
        hash_init(0, errno_table, "errno_table", 1, hash_funcs);
        hash_initialized = !0;
    }
}

static void
write_unknown_errno(char *buf, size_t buf_sz, int eno)
{
    erts_snprintf(&buf[0], buf_sz, ERTS_UNKNOWN_ERRNO_FORMAT, eno);
}

#ifdef __WIN32__

static char *
wsa_error_names(int wsa_err)
{
    switch (wsa_err) {
#ifdef WSAEINTR
    case WSAEINTR: return "eintr";
#endif
#ifdef WSAEBADF
    case WSAEBADF: return "ebadf";
#endif
#ifdef WSAEACCES
    case WSAEACCES: return "eacces";
#endif
#ifdef WSAEFAULT
    case WSAEFAULT: return "efault";
#endif
#ifdef WSAEINVAL
    case WSAEINVAL: return "einval";
#endif
#ifdef WSAEMFILE
    case WSAEMFILE: return "emfile";
#endif
#ifdef WSAEWOULDBLOCK
    case WSAEWOULDBLOCK: return "ewouldblock";
#endif
#ifdef WSAEINPROGRESS
    case WSAEINPROGRESS: return "einprogress";
#endif
#ifdef WSAEALREADY
    case WSAEALREADY: return "ealready";
#endif
#ifdef WSAENOTSOCK
    case WSAENOTSOCK: return "enotsock";
#endif
#ifdef WSAEDESTADDRREQ
    case WSAEDESTADDRREQ: return "edestaddrreq";
#endif
#ifdef WSAEMSGSIZE
    case WSAEMSGSIZE: return "emsgsize";
#endif
#ifdef WSAEPROTOTYPE
    case WSAEPROTOTYPE: return "eprototype";
#endif
#ifdef WSAENOPROTOOPT
    case WSAENOPROTOOPT: return "enoprotoopt";
#endif
#ifdef WSAEPROTONOSUPPORT
    case WSAEPROTONOSUPPORT: return "eprotonosupport";
#endif
#ifdef WSAESOCKTNOSUPPORT
    case WSAESOCKTNOSUPPORT: return "esocktnosupport";
#endif
#ifdef WSAEOPNOTSUPP
    case WSAEOPNOTSUPP: return "eopnotsupp";
#endif
#ifdef WSAEPFNOSUPPORT
    case WSAEPFNOSUPPORT: return "epfnosupport";
#endif
#ifdef WSAEAFNOSUPPORT
    case WSAEAFNOSUPPORT: return "eafnosupport";
#endif
#ifdef WSAEADDRINUSE
    case WSAEADDRINUSE: return "eaddrinuse";
#endif
#ifdef WSAEADDRNOTAVAIL
    case WSAEADDRNOTAVAIL: return "eaddrnotavail";
#endif
#ifdef WSAENETDOWN
    case WSAENETDOWN: return "enetdown";
#endif
#ifdef WSAENETUNREACH
    case WSAENETUNREACH: return "enetunreach";
#endif
#ifdef WSAENETRESET
    case WSAENETRESET: return "enetreset";
#endif
#ifdef WSAECONNABORTED
    case WSAECONNABORTED: return "econnaborted";
#endif
#ifdef WSAECONNRESET
    case WSAECONNRESET: return "econnreset";
#endif
#ifdef WSAENOBUFS
    case WSAENOBUFS: return "enobufs";
#endif
#ifdef WSAEISCONN
    case WSAEISCONN: return "eisconn";
#endif
#ifdef WSAENOTCONN
    case WSAENOTCONN: return "enotconn";
#endif
#ifdef WSAESHUTDOWN
    case WSAESHUTDOWN: return "eshutdown";
#endif
#ifdef WSAETOOMANYREFS
    case WSAETOOMANYREFS: return "etoomanyrefs";
#endif
#ifdef WSAETIMEDOUT
    case WSAETIMEDOUT: return "etimedout";
#endif
#ifdef WSAECONNREFUSED
    case WSAECONNREFUSED: return "econnrefused";
#endif
#ifdef WSAELOOP
    case WSAELOOP: return "eloop";
#endif
#ifdef WSAENAMETOOLONG
    case WSAENAMETOOLONG: return "enametoolong";
#endif
#ifdef WSAEHOSTDOWN
    case WSAEHOSTDOWN: return "ehostdown";
#endif
#ifdef WSAEHOSTUNREACH
    case WSAEHOSTUNREACH: return "ehostunreach";
#endif
#ifdef WSAENOTEMPTY
    case WSAENOTEMPTY: return "enotempty";
#endif
#ifdef WSAEPROCLIM
    case WSAEPROCLIM: return "eproclim";
#endif
#ifdef WSAEUSERS
    case WSAEUSERS: return "eusers";
#endif
#ifdef WSAEDQUOT
    case WSAEDQUOT: return "edquot";
#endif
#ifdef WSAESTALE
    case WSAESTALE: return "estale";
#endif
#ifdef WSAEREMOTE
    case WSAEREMOTE: return "eremote";
#endif
#ifdef WSASYSNOTREADY
    case WSASYSNOTREADY: return "sysnotready";
#endif
#ifdef WSAVERNOTSUPPORTED
    case WSAVERNOTSUPPORTED: return "vernotsupported";
#endif
#ifdef WSANOTINITIALISED
    case WSANOTINITIALISED: return "notinitialised";
#endif
#ifdef WSAEDISCON
    case WSAEDISCON: return "ediscon";
#endif
#ifdef WSAENOMORE
    case WSAENOMORE: return "enomore";
#endif
#ifdef WSAECANCELLED
    case WSAECANCELLED: return "ecancelled";
#endif
#ifdef WSAEINVALIDPROCTABLE
    case WSAEINVALIDPROCTABLE: return "einvalidproctable";
#endif
#ifdef WSAEINVALIDPROVIDER
    case WSAEINVALIDPROVIDER: return "einvalidprovider";
#endif
#ifdef WSAEPROVIDERFAILEDINIT
      /* You could get this if SYSTEMROOT env variable is set incorrectly */
    case WSAEPROVIDERFAILEDINIT: return "eproviderfailedinit";
#endif
#ifdef WSASYSCALLFAILURE
    case WSASYSCALLFAILURE: return "syscallfailure";
#endif
#ifdef WSASERVICE_NOT_FOUND
    case WSASERVICE_NOT_FOUND: return "service_not_found";
#endif
#ifdef WSATYPE_NOT_FOUND
    case WSATYPE_NOT_FOUND: return "type_not_found";
#endif
#ifdef WSA_E_NO_MORE
    case WSA_E_NO_MORE: return "e_no_more";
#endif
#ifdef WSA_E_CANCELLED
    case WSA_E_CANCELLED: return "e_cancelled";
#endif
    default:
        return NULL;
    }
}

#endif /* __WIN32__ */

#define ERTS_ERRNO_CASE(ENO) case ENO: return #ENO

#if !defined(ERTS_HAVE_ERRNO_NAME_LOOKUP_FUNC)

static const char *errno_name_fallback(int eno)
{
    /*
     * This function is used when we have no function on the system mapping
     * errno integers to errno names (such as strerrorname_np()).
     *
     * This functions contains mappings for all POSIX.1-2017 errno names (not
     * already mapped by the errno_name() function due to conflict resolution)
     * plus (currently) mappings for a few BSDs, illumos and Linux.
     *
     * OSes not providing a mapping should get by fairly well on the POSIX
     * mapping below alone. If not, add further OS specific errno mappings at
     * the end of the switch below.
     *
     * Newer Linuxes and OpenIndiana/illumos using glibc 2.32 (released Aug
     * 2020) or newer provide the strerrorname_np() function and thus do not
     * need this function.
     */
    switch (eno) {

        /*
         * All POSIX.1-2017 errno names. Conflict resolutions that are
         * already present in errno_name() have been commented out.
         */
#ifdef E2BIG
        ERTS_ERRNO_CASE(E2BIG);
#endif
#ifdef EACCES
        ERTS_ERRNO_CASE(EACCES);
#endif
#ifdef EADDRINUSE
        ERTS_ERRNO_CASE(EADDRINUSE);
#endif
#ifdef EADDRNOTAVAIL
        ERTS_ERRNO_CASE(EADDRNOTAVAIL);
#endif
#ifdef EAFNOSUPPORT
        ERTS_ERRNO_CASE(EAFNOSUPPORT);
#endif
        /*
#ifdef EAGAIN
    ERTS_ERRNO_CASE(EAGAIN);
#endif
        */
        /*
#ifdef EALREADY
    ERTS_ERRNO_CASE(EALREADY);
#endif
        */
#ifdef EBADF
        ERTS_ERRNO_CASE(EBADF);
#endif
#ifdef EBADMSG
        ERTS_ERRNO_CASE(EBADMSG);
#endif
        /*
#ifdef EBUSY
        ERTS_ERRNO_CASE(EBUSY);
#endif
        */
#ifdef ECANCELED
        ERTS_ERRNO_CASE(ECANCELED);
#endif
#ifdef ECHILD
        ERTS_ERRNO_CASE(ECHILD);
#endif
#ifdef ECONNABORTED
        ERTS_ERRNO_CASE(ECONNABORTED);
#endif
        /*
#ifdef ECONNREFUSED
        ERTS_ERRNO_CASE(ECONNREFUSED);
#endif
        */
#ifdef ECONNRESET
        ERTS_ERRNO_CASE(ECONNRESET);
#endif
        /*
#ifdef EDEADLK
        ERTS_ERRNO_CASE(EDEADLK);
#endif
        */
#ifdef EDESTADDRREQ
        ERTS_ERRNO_CASE(EDESTADDRREQ);
#endif
#ifdef EDOM
        ERTS_ERRNO_CASE(EDOM);
#endif
#ifdef EDQUOT
        ERTS_ERRNO_CASE(EDQUOT);
#endif
        /*
#ifdef EEXIST
        ERTS_ERRNO_CASE(EEXIST);
#endif
        */
#ifdef EFAULT
        ERTS_ERRNO_CASE(EFAULT);
#endif
#ifdef EFBIG
        ERTS_ERRNO_CASE(EFBIG);
#endif
#ifdef EHOSTUNREACH
        ERTS_ERRNO_CASE(EHOSTUNREACH);
#endif
        /*
#ifdef EIDRM
        ERTS_ERRNO_CASE(EIDRM);
#endif
        */
#ifdef EILSEQ
        ERTS_ERRNO_CASE(EILSEQ);
#endif
        /*
#ifdef EINPROGRESS
        ERTS_ERRNO_CASE(EINPROGRESS);
#endif
        */
#ifdef EINTR
        ERTS_ERRNO_CASE(EINTR);
#endif
#ifdef EINVAL
        ERTS_ERRNO_CASE(EINVAL);
#endif
#ifdef EIO
        ERTS_ERRNO_CASE(EIO);
#endif
#ifdef EISCONN
        ERTS_ERRNO_CASE(EISCONN);
#endif
#ifdef EISDIR
        ERTS_ERRNO_CASE(EISDIR);
#endif
        /*
#ifdef ELOOP
        ERTS_ERRNO_CASE(ELOOP);
#endif
        */
#ifdef EMFILE
        ERTS_ERRNO_CASE(EMFILE);
#endif
#ifdef EMLINK
        ERTS_ERRNO_CASE(EMLINK);
#endif
#ifdef EMSGSIZE
        ERTS_ERRNO_CASE(EMSGSIZE);
#endif
#ifdef EMULTIHOP
        ERTS_ERRNO_CASE(EMULTIHOP);
#endif
        /*
#ifdef ENAMETOOLONG
        ERTS_ERRNO_CASE(ENAMETOOLONG);
#endif
        */
#ifdef ENETDOWN
        ERTS_ERRNO_CASE(ENETDOWN);
#endif
#ifdef ENETRESET
        ERTS_ERRNO_CASE(ENETRESET);
#endif
#ifdef ENETUNREACH
        ERTS_ERRNO_CASE(ENETUNREACH);
#endif
#ifdef ENFILE
        ERTS_ERRNO_CASE(ENFILE);
#endif
        /*
#ifdef ENOBUFS
        ERTS_ERRNO_CASE(ENOBUFS);
#endif
        */
        /*
#ifdef ENODATA
        ERTS_ERRNO_CASE(ENODATA);
#endif
        */
#ifdef ENODEV
        ERTS_ERRNO_CASE(ENODEV);
#endif
        /*
#ifdef ENOENT
        ERTS_ERRNO_CASE(ENOENT);
#endif
        */
#ifdef ENOEXEC
        ERTS_ERRNO_CASE(ENOEXEC);
#endif
        /*
#ifdef ENOLCK
        ERTS_ERRNO_CASE(ENOLCK);
#endif
        */
#ifdef ENOLINK
        ERTS_ERRNO_CASE(ENOLINK);
#endif
#ifdef ENOMEM
        ERTS_ERRNO_CASE(ENOMEM);
#endif
#ifdef ENOMSG
        ERTS_ERRNO_CASE(ENOMSG);
#endif
#ifdef ENOPROTOOPT
        ERTS_ERRNO_CASE(ENOPROTOOPT);
#endif
#ifdef ENOSPC
        ERTS_ERRNO_CASE(ENOSPC);
#endif
        /*
#ifdef ENOSR
        ERTS_ERRNO_CASE(ENOSR);
#endif
        */
        /*
#ifdef ENOSTR
        ERTS_ERRNO_CASE(ENOSTR);
#endif
        */
#ifdef ENOSYS
        ERTS_ERRNO_CASE(ENOSYS);
#endif
#ifdef ENOTCONN
        ERTS_ERRNO_CASE(ENOTCONN);
#endif
#ifdef ENOTDIR
        ERTS_ERRNO_CASE(ENOTDIR);
#endif
        /*
#ifdef ENOTEMPTY
        ERTS_ERRNO_CASE(ENOTEMPTY);
#endif
        */
#ifdef ENOTRECOVERABLE
        ERTS_ERRNO_CASE(ENOTRECOVERABLE);
#endif
#ifdef ENOTSOCK
        ERTS_ERRNO_CASE(ENOTSOCK);
#endif
        /*
#ifdef ENOTSUP
        ERTS_ERRNO_CASE(ENOTSUP);
#endif
        */
        /*
#ifdef ENOTTY
        ERTS_ERRNO_CASE(ENOTTY);
#endif
        */
#ifdef ENXIO
        ERTS_ERRNO_CASE(ENXIO);
#endif
        /*
#ifdef EOPNOTSUPP
        ERTS_ERRNO_CASE(EOPNOTSUPP);
#endif
        */
#ifdef EOVERFLOW
        ERTS_ERRNO_CASE(EOVERFLOW);
#endif
#ifdef EOWNERDEAD
        ERTS_ERRNO_CASE(EOWNERDEAD);
#endif
#ifdef EPERM
        ERTS_ERRNO_CASE(EPERM);
#endif
#ifdef EPIPE
        ERTS_ERRNO_CASE(EPIPE);
#endif
#ifdef EPROTO
        ERTS_ERRNO_CASE(EPROTO);
#endif
#ifdef EPROTONOSUPPORT
        ERTS_ERRNO_CASE(EPROTONOSUPPORT);
#endif
#ifdef EPROTOTYPE
        ERTS_ERRNO_CASE(EPROTOTYPE);
#endif
#ifdef ERANGE
        ERTS_ERRNO_CASE(ERANGE);
#endif
#ifdef EROFS
        ERTS_ERRNO_CASE(EROFS);
#endif
#ifdef ESPIPE
        ERTS_ERRNO_CASE(ESPIPE);
#endif
#ifdef ESRCH
        ERTS_ERRNO_CASE(ESRCH);
#endif
#ifdef ESTALE
        ERTS_ERRNO_CASE(ESTALE);
#endif
        /*
#ifdef ETIME
        ERTS_ERRNO_CASE(ETIME);
#endif
        */
        /*
#ifdef ETIMEDOUT
        ERTS_ERRNO_CASE(ETIMEDOUT);
#endif
        */
#ifdef ETXTBSY
        ERTS_ERRNO_CASE(ETXTBSY);
#endif
        /*
#ifdef EWOULDBLOCK
        ERTS_ERRNO_CASE(EWOULDBLOCK);
#endif
        */
#ifdef EXDEV
        ERTS_ERRNO_CASE(EXDEV);
#endif

        /*
         * OS specific follow:
         */

        /* On FreeBSD (and not above) */
#ifdef ENOTBLK
        ERTS_ERRNO_CASE(ENOTBLK);
#endif
#ifdef ESOCKTNOSUPPORT
        ERTS_ERRNO_CASE(ESOCKTNOSUPPORT);
#endif
#ifdef EPFNOSUPPORT
        ERTS_ERRNO_CASE(EPFNOSUPPORT);
#endif
#ifdef ESHUTDOWN
        ERTS_ERRNO_CASE(ESHUTDOWN);
#endif
#ifdef ETOOMANYREFS
        ERTS_ERRNO_CASE(ETOOMANYREFS);
#endif
#ifdef EHOSTDOWN
        ERTS_ERRNO_CASE(EHOSTDOWN);
#endif
#ifdef EPROCLIM
        ERTS_ERRNO_CASE(EPROCLIM);
#endif
#ifdef EUSERS
        ERTS_ERRNO_CASE(EUSERS);
#endif
#ifdef EREMOTE
        ERTS_ERRNO_CASE(EREMOTE);
#endif
#ifdef EBADRPC
        ERTS_ERRNO_CASE(EBADRPC);
#endif
#ifdef ERPCMISMATCH
        ERTS_ERRNO_CASE(ERPCMISMATCH);
#endif
#ifdef EPROGUNAVAIL
        ERTS_ERRNO_CASE(EPROGUNAVAIL);
#endif
#ifdef EPROGMISMATCH
        ERTS_ERRNO_CASE(EPROGMISMATCH);
#endif
#ifdef EPROCUNAVAIL
        ERTS_ERRNO_CASE(EPROCUNAVAIL);
#endif
#ifdef EFTYPE
        ERTS_ERRNO_CASE(EFTYPE);
#endif
#ifdef EAUTH
        ERTS_ERRNO_CASE(EAUTH);
#endif
#ifdef ENEEDAUTH
        ERTS_ERRNO_CASE(ENEEDAUTH);
#endif
#ifdef ENOATTR
        ERTS_ERRNO_CASE(ENOATTR);
#endif
#ifdef EDOOFUS
        ERTS_ERRNO_CASE(EDOOFUS);
#endif
#ifdef ENOTCAPABLE
        ERTS_ERRNO_CASE(ENOTCAPABLE);
#endif
#ifdef ECAPMODE
        ERTS_ERRNO_CASE(ECAPMODE);
#endif
#ifdef EINTEGRITY
        ERTS_ERRNO_CASE(EINTEGRITY);
#endif

        /*
         * On OpenBsd (and not above)
         */
#ifdef EIPSEC
        ERTS_ERRNO_CASE(EIPSEC);
#endif
#ifdef ENOMEDIUM
        ERTS_ERRNO_CASE(ENOMEDIUM);
#endif
#ifdef EMEDIUMTYPE
        ERTS_ERRNO_CASE(EMEDIUMTYPE);
#endif

        /*
         * On Darwin (and not above)
         */
#ifdef EPWROFF
        ERTS_ERRNO_CASE(EPWROFF);
#endif
#ifdef EDEVERR
        ERTS_ERRNO_CASE(EDEVERR);
#endif
#ifdef EBADEXEC
        ERTS_ERRNO_CASE(EBADEXEC);
#endif
#ifdef EBADARCH
        ERTS_ERRNO_CASE(EBADARCH);
#endif
#ifdef ESHLIBVERS
        ERTS_ERRNO_CASE(ESHLIBVERS);
#endif
#ifdef EBADMACHO
        ERTS_ERRNO_CASE(EBADMACHO);
#endif
#ifdef ENOPOLICY
        ERTS_ERRNO_CASE(ENOPOLICY);
#endif
#ifdef EQFULL
        ERTS_ERRNO_CASE(EQFULL);
#endif

        /*
         * On illumos (and not above)
         */
#ifdef ECHRNG
        ERTS_ERRNO_CASE(ECHRNG);
#endif
#ifdef EL2NSYNC
        ERTS_ERRNO_CASE(EL2NSYNC);
#endif
#ifdef EL3HLT
        ERTS_ERRNO_CASE(EL3HLT);
#endif
#ifdef EL3RST
        ERTS_ERRNO_CASE(EL3RST);
#endif
#ifdef ELNRNG
        ERTS_ERRNO_CASE(ELNRNG);
#endif
#ifdef EUNATCH
        ERTS_ERRNO_CASE(EUNATCH);
#endif
#ifdef ENOCSI
        ERTS_ERRNO_CASE(ENOCSI);
#endif
#ifdef EL2HLT
        ERTS_ERRNO_CASE(EL2HLT);
#endif
#ifdef EBADE
        ERTS_ERRNO_CASE(EBADE);
#endif
#ifdef EBADR
        ERTS_ERRNO_CASE(EBADR);
#endif
#ifdef EXFULL
        ERTS_ERRNO_CASE(EXFULL);
#endif
#ifdef ENOANO
        ERTS_ERRNO_CASE(ENOANO);
#endif
#ifdef EBADRQC
        ERTS_ERRNO_CASE(EBADRQC);
#endif
#ifdef EBADSLT
        ERTS_ERRNO_CASE(EBADSLT);
#endif
#ifdef EDEADLOCK
        ERTS_ERRNO_CASE(EDEADLOCK);
#endif
#ifdef EBFONT
        ERTS_ERRNO_CASE(EBFONT);
#endif
#ifdef ENONET
        ERTS_ERRNO_CASE(ENONET);
#endif
#ifdef ENOPKG
        ERTS_ERRNO_CASE(ENOPKG);
#endif
#ifdef EADV
        ERTS_ERRNO_CASE(EADV);
#endif
#ifdef ESRMNT
        ERTS_ERRNO_CASE(ESRMNT);
#endif
#ifdef ECOMM
        ERTS_ERRNO_CASE(ECOMM);
#endif
#ifdef ELOCKUNMAPPED
        ERTS_ERRNO_CASE(ELOCKUNMAPPED);
#endif
#ifdef ENOTACTIVE
        ERTS_ERRNO_CASE(ENOTACTIVE);
#endif
#ifdef ENOTUNIQ
        ERTS_ERRNO_CASE(ENOTUNIQ);
#endif
#ifdef EBADFD
        ERTS_ERRNO_CASE(EBADFD);
#endif
#ifdef EREMCHG
        ERTS_ERRNO_CASE(EREMCHG);
#endif
#ifdef ELIBACC
        ERTS_ERRNO_CASE(ELIBACC);
#endif
#ifdef ELIBBAD
        ERTS_ERRNO_CASE(ELIBBAD);
#endif
#ifdef ELIBSCN
        ERTS_ERRNO_CASE(ELIBSCN);
#endif
#ifdef ELIBMAX
        ERTS_ERRNO_CASE(ELIBMAX);
#endif
#ifdef ELIBEXEC
        ERTS_ERRNO_CASE(ELIBEXEC);
#endif
#ifdef ERESTART
        ERTS_ERRNO_CASE(ERESTART);
#endif
#ifdef ESTRPIPE
        ERTS_ERRNO_CASE(ESTRPIPE);
#endif

        /*
         * On Linux (and not above)
         */
#ifdef EDOTDOT
        ERTS_ERRNO_CASE(EDOTDOT);
#endif
#ifdef EUCLEAN
        ERTS_ERRNO_CASE(EUCLEAN);
#endif
#ifdef ENOTNAM
        ERTS_ERRNO_CASE(ENOTNAM);
#endif
#ifdef ENAVAIL
        ERTS_ERRNO_CASE(ENAVAIL);
#endif
#ifdef EISNAM
        ERTS_ERRNO_CASE(EISNAM);
#endif
#ifdef EREMOTEIO
        ERTS_ERRNO_CASE(EREMOTEIO);
#endif
#ifdef ENOKEY
        ERTS_ERRNO_CASE(ENOKEY);
#endif
#ifdef EKEYEXPIRED
        ERTS_ERRNO_CASE(EKEYEXPIRED);
#endif
#ifdef EKEYREVOKED
        ERTS_ERRNO_CASE(EKEYREVOKED);
#endif
#ifdef EKEYREJECTED
        ERTS_ERRNO_CASE(EKEYREJECTED);
#endif
#ifdef ERFKILL
        ERTS_ERRNO_CASE(ERFKILL);
#endif
#ifdef EHWPOISON
        ERTS_ERRNO_CASE(EHWPOISON);
#endif

        /*
         * Continue with other OSes here...
         */

    default:
        return NULL;
    }

}

#endif /* !defined(ERTS_HAVE_ERRNO_NAME_LOOKUP_FUNC) */

static const char *errno_name(int eno)
{

    /*
     * Some errno names might have the same integer value. Make sure to return
     * the same name that we've always returned for such collisions.
     */
    switch (eno) {

#ifdef EAGAIN
        ERTS_ERRNO_CASE(EAGAIN);
#endif

#ifdef EALREADY
#if !defined(EBUSY) || EBUSY != EALREADY
        ERTS_ERRNO_CASE(EALREADY);
#endif
#endif

#ifdef EBUSY
        ERTS_ERRNO_CASE(EBUSY);
#endif

#ifdef ECONNREFUSED
        ERTS_ERRNO_CASE(ECONNREFUSED);
#endif

#ifdef EDEADLK
#if !defined(EWOULDBLOCK) || EWOULDBLOCK != EDEADLK
        ERTS_ERRNO_CASE(EDEADLK);
#endif
#endif

#ifdef EEXIST
        ERTS_ERRNO_CASE(EEXIST);
#endif

#ifdef EIDRM
#if !defined(EINPROGRESS) || EINPROGRESS != EIDRM
        ERTS_ERRNO_CASE(EIDRM);
#endif
#endif

#ifdef EINPROGRESS
        ERTS_ERRNO_CASE(EINPROGRESS);
#endif

#ifdef ELOOP
#if !defined(ENOENT) || ENOENT != ELOOP
        ERTS_ERRNO_CASE(ELOOP);
#endif
#endif

#ifdef ENAMETOOLONG
        ERTS_ERRNO_CASE(ENAMETOOLONG);
#endif

#ifdef ENOBUFS
#if !defined(ENOSR) || ENOSR != ENOBUFS
        ERTS_ERRNO_CASE(ENOBUFS);
#endif
#endif

#ifdef ENOSTR
#if !defined(ENOTTY) || ENOTTY != ENOSTR
        ERTS_ERRNO_CASE(ENOSTR);
#endif
#endif

#ifdef ENODATA
#if !defined(ECONNREFUSED) || ECONNREFUSED != ENODATA
        ERTS_ERRNO_CASE(ENODATA);
#endif
#endif

#ifdef ENOENT
        ERTS_ERRNO_CASE(ENOENT);
#endif

#ifdef ENOLCK
        ERTS_ERRNO_CASE(ENOLCK);
#endif

#ifdef ENOSR
#if !defined(ENAMETOOLONG) || ENAMETOOLONG != ENOSR
        ERTS_ERRNO_CASE(ENOSR);
#endif
#endif

#ifdef ENOTEMPTY
#if !defined(EEXIST) || EEXIST != ENOTEMPTY
        ERTS_ERRNO_CASE(ENOTEMPTY);
#endif
#endif

#ifdef ENOTTY
        ERTS_ERRNO_CASE(ENOTTY);
#endif

#ifdef ENOTSUP
        ERTS_ERRNO_CASE(ENOTSUP);
#endif

#ifdef EOPNOTSUPP
#if !defined(ENOTSUP) || ENOTSUP != EOPNOTSUPP
        ERTS_ERRNO_CASE(EOPNOTSUPP);
#endif
#endif

#ifdef ETIME
#if !defined(ELOOP) || ELOOP != ETIME
        ERTS_ERRNO_CASE(ETIME);
#endif
#endif

#ifdef ETIMEDOUT
#if ((!defined(ENOSTR) || ENOSTR != ETIMEDOUT)          \
     && (!defined(EAGAIN) || EAGAIN != ETIMEDOUT))
        ERTS_ERRNO_CASE(ETIMEDOUT);
#endif
#endif

#ifdef EWOULDBLOCK
#if !defined(EAGAIN) || EAGAIN != EWOULDBLOCK
        ERTS_ERRNO_CASE(EWOULDBLOCK);
#endif
#endif

    default:

#if !defined(ERTS_HAVE_ERRNO_NAME_LOOKUP_FUNC)
        return errno_name_fallback(eno);
#elif defined(HAVE_STRERRORNAME_NP)
        return strerrorname_np(eno);
#else
#error Missing primitive mapping errno integer values to errno names
#endif

    }

}

#undef ERTS_ERRNO_CASE

void
erts_errno_init(void)
{
    /* Allocators have been initialized... */
    int eno;
    size_t tot_sz = 0, unknown_sz, arr_sz;
    char *ptr, *end_ptr;
    char unknown_buf[ERTS_UNKNOWN_ERRNO_BUF_SZ];

    if (init_done) {
        return;
    }

    write_unknown_errno(&unknown_buf[0], sizeof(unknown_buf), 0);
    unknown_sz = strlen(&unknown_buf[0]) + 1;

    for (eno = 1; eno < ERTS_MAX_CACHED_ERRNO; eno++) {
        const char *str = errno_name(eno);
        if (str) {
            max_errno = eno;
            tot_sz += unknown_sz + strlen(str) + 1;
            unknown_sz = 0;
        }
        else {
            write_unknown_errno(&unknown_buf[0], sizeof(unknown_buf), eno);
            unknown_sz += strlen(&unknown_buf[0]) + 1;
        }
    }

    arr_sz = sizeof(char *)*(max_errno + 1);
    tot_sz += arr_sz;
    ptr = permanent_alloc(ERTS_ALC_T_ENO_MAP, tot_sz);
    end_ptr = ptr + tot_sz;

    errno_map = (char **) ptr;

    ptr += arr_sz;

    errno_map[0] = ptr;
    write_unknown_errno(ptr, end_ptr - ptr, 0);
    ptr += strlen(ptr) + 1;

    for (eno = 1; eno <= max_errno; eno++) {
        const char *str = errno_name(eno);
        errno_map[eno] = ptr;
        if (str) {
            ptr = tolower_copy_estring(ptr, end_ptr, str);
        }
        else {
            write_unknown_errno(ptr, end_ptr - ptr, eno);
            ptr += strlen(ptr) + 1;
        }
    }

#if 0
    for (eno = 0; eno <= max_errno; eno++)
        fprintf(stderr, "%d - %s\r\n", eno, errno_map[eno]);
#endif

    init_done = true;
}

void
erts_errno_late_init(void)
{
    /*
     * We are still single threaded and now the thread lib has been
     * initialized...
     */
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_RWMTX_TYPE_NORMAL;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;
    erts_rwmtx_init_opt(&errno_rwmtx, &rwmtx_opt, "errno_table", NIL,
                        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    late_init_done = true;
}

static void rlock(void)
{
    if (late_init_done) {
        erts_rwmtx_rlock(&errno_rwmtx);
    }
}

static void runlock(void)
{
    if (late_init_done) {
        erts_rwmtx_runlock(&errno_rwmtx);
    }
}

static void rwlock(void)
{
    if (late_init_done) {
        erts_rwmtx_rwlock(&errno_rwmtx);
    }
}

static void rwunlock(void)
{
    if (late_init_done) {
        erts_rwmtx_rwunlock(&errno_rwmtx);
    }
}

char *
erl_errno_id(int eno)
{
    char *res;
    ErtsErrnoEntry *hash_res;
    ErtsErrnoEntry tmpl;
    /*
     * Note! Returned strings must not move or be deallocated
     * during the runtime systems lifetime.
     */
    if (!init_done) {
        /*
         * Alloc-util allocators have not yet been initialized so we will go
         * for malloc/free instead of erts_alloc/erts_free from now on.
         *
         * This scenario is very unlikely, but might happen on a fatal error
         * during initialization of the emulator.
         */
        used_before_init = true;
        erts_errno_init();
    }

#ifdef __WIN32__
    res = wsa_error_names(eno);
    if (res) {
        return res;
    }
#endif

    if (0 <= eno && eno <= max_errno) {
        return errno_map[eno];
    }

    rlock();

    if (!hash_initialized) {
        runlock();
        rwlock();
        init_hash_table();
        rwunlock();
        rlock();
    }

    tmpl.eno = eno;
    tmpl.str[0] = '\0';

    hash_res = hash_get(errno_table, (void *) &tmpl);

    runlock();

    if (hash_res) {
        res = &hash_res->str[0];
    }
    else {
        ErtsErrnoEntry *entry;
        char buf[ERTS_UNKNOWN_ERRNO_BUF_SZ];
        char *str = (char *) errno_name(eno);

        if (!str) {
            write_unknown_errno(&buf[0], sizeof(buf), eno);
            str = &buf[0];
        }

        entry = make_errno_entry(eno, str);

        rwlock();
        hash_res = hash_put(errno_table, entry);
        if (hash_res != entry) {
            /*
             * Insert raced with an insert by another thread and the other
             * thread won...
             */
            dealloc(ERTS_ALC_T_ENO_ENTRY, entry);
        }
        rwunlock();
        res = &hash_res->str[0];
    }

    ASSERT(res && res[0]);

    return res;
}
