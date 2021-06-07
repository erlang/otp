/* 
 * Original: tclPosixStr.c --
 *
 *	This file contains procedures that generate strings
 *	corresponding to various POSIX-related codes, such
 *	as errno and signals.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclPosixStr.c 1.32 96/10/10 10:09:42
 */

/* %ExternalCopyright% */
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
#include "erl_driver.h"

/*
 *----------------------------------------------------------------------
 *
 * erl_errno_id --
 *
 *	Return a textual identifier for the given errno value.
 *
 * Results:
 *	This procedure returns a machine-readable textual identifier
 *	that corresponds to the current errno value (e.g. "eperm").
 *	The identifier is the same as the #define name in errno.h,
 *	except that it is in lowercase.
 *
 *----------------------------------------------------------------------
 */

char *
erl_errno_id(error)
    int error;			/* Posix error number (as from errno). */
{
    switch (error) {
#ifdef E2BIG
    case E2BIG: return "e2big";
#endif
#ifdef EACCES
    case EACCES: return "eacces";
#endif
#ifdef EADDRINUSE
    case EADDRINUSE: return "eaddrinuse";
#endif
#ifdef EADDRNOTAVAIL
    case EADDRNOTAVAIL: return "eaddrnotavail";
#endif
#ifdef EADV
    case EADV: return "eadv";
#endif
#ifdef EAFNOSUPPORT
    case EAFNOSUPPORT: return "eafnosupport";
#endif
#ifdef EAGAIN
    case EAGAIN: return "eagain";
#endif
#ifdef EALIGN
    case EALIGN: return "ealign";
#endif
#if defined(EALREADY) && (!defined(EBUSY) || (EALREADY != EBUSY ))
    case EALREADY: return "ealready";
#endif
#ifdef EBADE
    case EBADE: return "ebade";
#endif
#ifdef EBADF
    case EBADF: return "ebadf";
#endif
#ifdef EBADFD
    case EBADFD: return "ebadfd";
#endif
#ifdef EBADMSG
    case EBADMSG: return "ebadmsg";
#endif
#ifdef EBADR
    case EBADR: return "ebadr";
#endif
#ifdef EBADRPC
    case EBADRPC: return "ebadrpc";
#endif
#ifdef EBADRQC
    case EBADRQC: return "ebadrqc";
#endif
#ifdef EBADSLT
    case EBADSLT: return "ebadslt";
#endif
#ifdef EBFONT
    case EBFONT: return "ebfont";
#endif
#ifdef EBUSY
    case EBUSY: return "ebusy";
#endif
#ifdef ECHILD
    case ECHILD: return "echild";
#endif
#ifdef ECHRNG
    case ECHRNG: return "echrng";
#endif
#ifdef ECOMM
    case ECOMM: return "ecomm";
#endif
#ifdef ECONNABORTED
    case ECONNABORTED: return "econnaborted";
#endif
#ifdef ECONNREFUSED
    case ECONNREFUSED: return "econnrefused";
#endif
#ifdef ECONNRESET
    case ECONNRESET: return "econnreset";
#endif
#if defined(EDEADLK) && (!defined(EWOULDBLOCK) || (EDEADLK != EWOULDBLOCK))
    case EDEADLK: return "edeadlk";
#endif
#if defined(EDEADLOCK) && (!defined(EDEADLK) || (EDEADLOCK != EDEADLK))
    case EDEADLOCK: return "edeadlock";
#endif
#ifdef EDESTADDRREQ
    case EDESTADDRREQ: return "edestaddrreq";
#endif
#ifdef EDIRTY
    case EDIRTY: return "edirty";
#endif
#ifdef EDOM
    case EDOM: return "edom";
#endif
#ifdef EDOTDOT
    case EDOTDOT: return "edotdot";
#endif
#ifdef EDQUOT
    case EDQUOT: return "edquot";
#endif
#ifdef EDUPPKG
    case EDUPPKG: return "eduppkg";
#endif
#ifdef EEXIST
    case EEXIST: return "eexist";
#endif
#ifdef EFAULT
    case EFAULT: return "efault";
#endif
#ifdef EFTYPE
    case EFTYPE: return "eftype";
#endif
#ifdef EFBIG
    case EFBIG: return "efbig";
#endif
#ifdef EHOSTDOWN
    case EHOSTDOWN: return "ehostdown";
#endif
#ifdef EHOSTUNREACH
    case EHOSTUNREACH: return "ehostunreach";
#endif
#if defined(EIDRM) && (!defined(EINPROGRESS) || (EIDRM != EINPROGRESS))
    case EIDRM: return "eidrm";
#endif
#ifdef EILSEQ
    case EILSEQ: return "eilseq";
#endif
#ifdef EINIT
    case EINIT: return "einit";
#endif
#ifdef EINPROGRESS
    case EINPROGRESS: return "einprogress";
#endif
#ifdef EINTR
    case EINTR: return "eintr";
#endif
#ifdef EINVAL
    case EINVAL: return "einval";
#endif
#ifdef EIO
    case EIO: return "eio";
#endif
#ifdef EISCONN
    case EISCONN: return "eisconn";
#endif
#ifdef EISDIR
    case EISDIR: return "eisdir";
#endif
#ifdef EISNAME
    case EISNAM: return "eisnam";
#endif
#ifdef ELBIN
    case ELBIN: return "elbin";
#endif
#ifdef EL2HLT
    case EL2HLT: return "el2hlt";
#endif
#ifdef EL2NSYNC
    case EL2NSYNC: return "el2nsync";
#endif
#ifdef EL3HLT
    case EL3HLT: return "el3hlt";
#endif
#ifdef EL3RST
    case EL3RST: return "el3rst";
#endif
#ifdef ELIBACC
    case ELIBACC: return "elibacc";
#endif
#ifdef ELIBBAD
    case ELIBBAD: return "elibbad";
#endif
#ifdef ELIBEXEC
    case ELIBEXEC: return "elibexec";
#endif
#ifdef ELIBMAX
    case ELIBMAX: return "elibmax";
#endif
#ifdef ELIBSCN
    case ELIBSCN: return "elibscn";
#endif
#ifdef ELNRNG
    case ELNRNG: return "elnrng";
#endif
#if defined(ELOOP) && (!defined(ENOENT) || (ELOOP != ENOENT))
    case ELOOP: return "eloop";
#endif
#ifdef EMFILE
    case EMFILE: return "emfile";
#endif
#ifdef EMLINK
    case EMLINK: return "emlink";
#endif
#ifdef EMSGSIZE
    case EMSGSIZE: return "emsgsize";
#endif
#ifdef EMULTIHOP
    case EMULTIHOP: return "emultihop";
#endif
#ifdef ENAMETOOLONG
    case ENAMETOOLONG: return "enametoolong";
#endif
#ifdef ENAVAIL
    case ENAVAIL: return "enavail";
#endif
#ifdef ENET
    case ENET: return "enet";
#endif
#ifdef ENETDOWN
    case ENETDOWN: return "enetdown";
#endif
#ifdef ENETRESET
    case ENETRESET: return "enetreset";
#endif
#ifdef ENETUNREACH
    case ENETUNREACH: return "enetunreach";
#endif
#ifdef ENFILE
    case ENFILE: return "enfile";
#endif
#ifdef ENOANO
    case ENOANO: return "enoano";
#endif
#if defined(ENOBUFS) && (!defined(ENOSR) || (ENOBUFS != ENOSR))
    case ENOBUFS: return "enobufs";
#endif
#ifdef ENOCSI
    case ENOCSI: return "enocsi";
#endif
#if defined(ENODATA) && (!defined(ECONNREFUSED) || (ENODATA != ECONNREFUSED))
    case ENODATA: return "enodata";
#endif
#ifdef ENODEV
    case ENODEV: return "enodev";
#endif
#ifdef ENOENT
    case ENOENT: return "enoent";
#endif
#ifdef ENOEXEC
    case ENOEXEC: return "enoexec";
#endif
#ifdef ENOLCK
    case ENOLCK: return "enolck";
#endif
#ifdef ENOLINK
    case ENOLINK: return "enolink";
#endif
#ifdef ENOMEM
    case ENOMEM: return "enomem";
#endif
#ifdef ENOMSG
    case ENOMSG: return "enomsg";
#endif
#ifdef ENONET
    case ENONET: return "enonet";
#endif
#ifdef ENOPKG
    case ENOPKG: return "enopkg";
#endif
#ifdef ENOPROTOOPT
    case ENOPROTOOPT: return "enoprotoopt";
#endif
#ifdef ENOSPC
    case ENOSPC: return "enospc";
#endif
#if defined(ENOSR) && (!defined(ENAMETOOLONG) || (ENAMETOOLONG != ENOSR))
    case ENOSR: return "enosr";
#endif
#if defined(ENOSTR) && (!defined(ENOTTY) || (ENOTTY != ENOSTR))
    case ENOSTR: return "enostr";
#endif
#ifdef ENOSYM
    case ENOSYM: return "enosym";
#endif
#ifdef ENOSYS
    case ENOSYS: return "enosys";
#endif
#ifdef ENOTBLK
    case ENOTBLK: return "enotblk";
#endif
#ifdef ENOTCONN
    case ENOTCONN: return "enotconn";
#endif
#ifdef ENOTDIR
    case ENOTDIR: return "enotdir";
#endif
#if defined(ENOTEMPTY) && (!defined(EEXIST) || (ENOTEMPTY != EEXIST))
    case ENOTEMPTY: return "enotempty";
#endif
#ifdef ENOTNAM
    case ENOTNAM: return "enotnam";
#endif
#ifdef ENOTSOCK
    case ENOTSOCK: return "enotsock";
#endif
#ifdef ENOTSUP
    case ENOTSUP: return "enotsup";
#endif
#ifdef ENOTTY
    case ENOTTY: return "enotty";
#endif
#ifdef ENOTUNIQ
    case ENOTUNIQ: return "enotuniq";
#endif
#ifdef ENXIO
    case ENXIO: return "enxio";
#endif
#if defined(EOPNOTSUPP) && (!defined(ENOTSUP) || (EOPNOTSUPP != ENOTSUP))
    case EOPNOTSUPP: return "eopnotsupp";
#endif
#ifdef EOVERFLOW
    case EOVERFLOW: return "eoverflow";
#endif
#ifdef EPERM
    case EPERM: return "eperm";
#endif
#if defined(EPFNOSUPPORT) && (!defined(ENOLCK) || (ENOLCK != EPFNOSUPPORT))
    case EPFNOSUPPORT: return "epfnosupport";
#endif
#ifdef EPIPE
    case EPIPE: return "epipe";
#endif
#ifdef EPROCLIM
    case EPROCLIM: return "eproclim";
#endif
#ifdef EPROCUNAVAIL
    case EPROCUNAVAIL: return "eprocunavail";
#endif
#ifdef EPROGMISMATCH
    case EPROGMISMATCH: return "eprogmismatch";
#endif
#ifdef EPROGUNAVAIL
    case EPROGUNAVAIL: return "eprogunavail";
#endif
#ifdef EPROTO
    case EPROTO: return "eproto";
#endif
#ifdef EPROTONOSUPPORT
    case EPROTONOSUPPORT: return "eprotonosupport";
#endif
#ifdef EPROTOTYPE
    case EPROTOTYPE: return "eprototype";
#endif
#ifdef ERANGE
    case ERANGE: return "erange";
#endif
#if defined(EREFUSED) && (!defined(ECONNREFUSED) || (EREFUSED != ECONNREFUSED))
    case EREFUSED: return "erefused";
#endif
#ifdef EREMCHG
    case EREMCHG: return "eremchg";
#endif
#ifdef EREMDEV
    case EREMDEV: return "eremdev";
#endif
#ifdef EREMOTE
    case EREMOTE: return "eremote";
#endif
#ifdef EREMOTEIO
    case EREMOTEIO: return "eremoteio";
#endif
#ifdef EREMOTERELEASE
    case EREMOTERELEASE: return "eremoterelease";
#endif
#ifdef EROFS
    case EROFS: return "erofs";
#endif
#ifdef ERPCMISMATCH
    case ERPCMISMATCH: return "erpcmismatch";
#endif
#ifdef ERREMOTE
    case ERREMOTE: return "erremote";
#endif
#ifdef ESHUTDOWN
    case ESHUTDOWN: return "eshutdown";
#endif
#ifdef ESOCKTNOSUPPORT
    case ESOCKTNOSUPPORT: return "esocktnosupport";
#endif
#ifdef ESPIPE
    case ESPIPE: return "espipe";
#endif
#ifdef ESRCH
    case ESRCH: return "esrch";
#endif
#ifdef ESRMNT
    case ESRMNT: return "esrmnt";
#endif
#ifdef ESTALE
    case ESTALE: return "estale";
#endif
#ifdef ESUCCESS
    case ESUCCESS: return "esuccess";
#endif
#if defined(ETIME) && (!defined(ELOOP) || (ETIME != ELOOP))
    case ETIME: return "etime";
#endif
#if defined(ETIMEDOUT) && (!defined(ENOSTR) || (ETIMEDOUT != ENOSTR)) && (!defined(EAGAIN) || (ETIMEDOUT != EAGAIN)) && (!defined(WSAETIMEDOUT) || (ETIMEDOUT != WSAETIMEDOUT))
    case ETIMEDOUT: return "etimedout";
#endif
#ifdef ETOOMANYREFS
    case ETOOMANYREFS: return "etoomanyrefs";
#endif
#ifdef ETXTBSY
    case ETXTBSY: return "etxtbsy";
#endif
#ifdef EUCLEAN
    case EUCLEAN: return "euclean";
#endif
#ifdef EUNATCH
    case EUNATCH: return "eunatch";
#endif
#ifdef EUSERS
    case EUSERS: return "eusers";
#endif
#ifdef EVERSION
    case EVERSION: return "eversion";
#endif
#if defined(EWOULDBLOCK) && (!defined(EAGAIN) || (EWOULDBLOCK != EAGAIN)) && (!defined(WSAEWOULDBLOCK) || (EWOULDBLOCK != WSAEWOULDBLOCK))
    case EWOULDBLOCK: return "ewouldblock";
#endif
#ifdef EXDEV
    case EXDEV: return "exdev";
#endif
#ifdef EXFULL
    case EXFULL: return "exfull";
#endif
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
    }
    return "unknown";
}
