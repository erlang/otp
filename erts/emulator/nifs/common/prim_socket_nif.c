/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2021. All Rights Reserved.
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
 *
 * ----------------------------------------------------------------------
 * Purpose : The NIF (C) part of the socket interface
 *
 * All of the nif-functions which are part of the API has two parts.
 * The first function is called 'nif_<something>', e.g. nif_open.
 * This does the initial validation and argument processing and then 
 * calls the function that does the actual work. This is called
 * 'esock_<something>'.
 * ----------------------------------------------------------------------
 *
 *
 * This is just a code snippet in case there is need of extra debugging
 *
 * esock_dbg_printf("DEMONP", "[%d] %s: %T\r\n",
 *                  descP->sock, slogan,
 *                  esock_make_monitor_term(env, &mon));
 *
 */

#define STATIC_ERLANG_NIF 1


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/* If we HAVE_SCTP_H and Solaris, we need to define the following in
 * order to get SCTP working:
 */
#if (defined(HAVE_SCTP_H) && defined(__sun) && defined(__SVR4))
#define SOLARIS10    1
/* WARNING: This is not quite correct, it may also be Solaris 11! */
#define _XPG4_2
#define __EXTENSIONS__
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>
#include <stdint.h>
#include <limits.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#ifdef HAVE_NET_IF_DL_H
#include <net/if_dl.h>
#endif

#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#ifdef HAVE_NETPACKET_PACKET_H
#include <netpacket/packet.h>
#endif

#ifdef HAVE_SENDFILE
#if defined(__linux__) || (defined(__sun) && defined(__SVR4))
    #include <sys/sendfile.h>
#elif defined(__FreeBSD__) || defined(__DragonFly__)
    /* Need to define __BSD_VISIBLE in order to expose prototype
     * of sendfile in sys/socket.h
     */
    #define __BSD_VISIBLE 1
#endif
#endif

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif


#ifdef __WIN32__
/* ---------------------------------------------------------------------- *
 *                                                                        *
 * Start of __WIN32__ section                                             *
 *                                                                        *
 * vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv */


#define STRNCASECMP               strncasecmp
#define INCL_WINSOCK_API_TYPEDEFS 1

#ifndef WINDOWS_H_INCLUDES_WINSOCK2_H
#include <winsock2.h>
#endif
#include <windows.h>
#include <Ws2tcpip.h>

/* Visual studio 2008+: NTDDI_VERSION needs to be set for iphlpapi.h
 * to define the right structures. It needs to be set to WINXP (or LONGHORN)
 * for IPV6 to work and it's set lower by default, so we need to change it.
 */
#ifdef HAVE_SDKDDKVER_H
#  include <sdkddkver.h>
#  ifdef NTDDI_VERSION
#    undef NTDDI_VERSION
#  endif
#  define NTDDI_VERSION NTDDI_WINXP
#endif
#include <iphlpapi.h>

#undef WANT_NONBLOCKING
#include "sys.h"


/* AND HERE WE MAY HAVE A BUNCH OF DEFINES....SEE INET DRIVER.... */


/* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *
 *                                                                        *
 * End of __WIN32__ section                                               *
 *                                                                        *
 * ---------------------------------------------------------------------- */
#else /* #ifdef __WIN32__ */
/* ---------------------------------------------------------------------- *
 *                                                                        *
 * Start of non-__WIN32__ section a.k.a UNIX section                      *
 *                                                                        *
 * vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv */


#include <sys/time.h>
#ifdef NETDB_H_NEEDS_IN_H
#include <netinet/in.h>
#endif
#include <netdb.h>

#include <sys/socket.h>
#include <netinet/in.h>

#ifdef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H
#include <rpc/types.h>
#endif

#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>
#include <arpa/inet.h>

#include <sys/param.h>
#ifdef HAVE_ARPA_NAMESER_H
#include <arpa/nameser.h>
#endif

#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#include <net/if.h>

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#ifdef HAVE_SETNS_H
#include <setns.h>
#endif
#ifdef HAVE_LINUX_ERRQUEUE_H
#include <linux/types.h>     /* On some (I assume) "old" linux,    *
                              * for example SLES 10 SP1, this is   *
                              * not (explicitly) included by the   *
                              * errqueue file. And for some reason *
                              * configure does not detect this.    *
                              * So, to simplify, we include here.  */
#include <linux/errqueue.h>
#include <linux/icmp.h>
#include <linux/icmpv6.h>
#endif

#define HAVE_UDP

/* SCTP support -- currently for UNIX platforms only: */
#undef HAVE_SCTP
#if defined(HAVE_SCTP_H)

#include <netinet/sctp.h>

/* SCTP Socket API Draft from version 11 on specifies that netinet/sctp.h must
   explicitly define HAVE_SCTP in case when SCTP is supported,  but Solaris 10
   still apparently uses Draft 10, and does not define that symbol, so we have
   to define it explicitly:
*/
#ifndef     HAVE_SCTP
#    define HAVE_SCTP
#endif

/* These changed in draft 11, so SOLARIS10 uses the old MSG_* */
#if ! HAVE_DECL_SCTP_UNORDERED
#     define    SCTP_UNORDERED  MSG_UNORDERED
#endif
#if ! HAVE_DECL_SCTP_ADDR_OVER
#     define    SCTP_ADDR_OVER  MSG_ADDR_OVER
#endif
#if ! HAVE_DECL_SCTP_ABORT
#     define    SCTP_ABORT      MSG_ABORT
#endif
#if ! HAVE_DECL_SCTP_EOF
#     define    SCTP_EOF        MSG_EOF
#endif

/* More Solaris 10 fixes: */
#if ! HAVE_DECL_SCTP_CLOSED && HAVE_DECL_SCTPS_IDLE
#    define SCTP_CLOSED SCTPS_IDLE
#    undef HAVE_DECL_SCTP_CLOSED
#    define HAVE_DECL_SCTP_CLOSED 1
#endif
#if ! HAVE_DECL_SCTP_BOUND && HAVE_DECL_SCTPS_BOUND
#    define SCTP_BOUND SCTPS_BOUND
#    undef HAVE_DECL_SCTP_BOUND
#    define HAVE_DECL_SCTP_BOUND 1
#endif
#if ! HAVE_DECL_SCTP_LISTEN && HAVE_DECL_SCTPS_LISTEN
#    define SCTP_LISTEN SCTPS_LISTEN
#    undef HAVE_DECL_SCTP_LISTEN
#    define HAVE_DECL_SCTP_LISTEN 1
#endif
#if ! HAVE_DECL_SCTP_COOKIE_WAIT && HAVE_DECL_SCTPS_COOKIE_WAIT
#    define SCTP_COOKIE_WAIT SCTPS_COOKIE_WAIT
#    undef HAVE_DECL_SCTP_COOKIE_WAIT
#    define HAVE_DECL_SCTP_COOKIE_WAIT 1
#endif
#if ! HAVE_DECL_SCTP_COOKIE_ECHOED && HAVE_DECL_SCTPS_COOKIE_ECHOED
#    define SCTP_COOKIE_ECHOED SCTPS_COOKIE_ECHOED
#    undef HAVE_DECL_SCTP_COOKIE_ECHOED
#    define HAVE_DECL_SCTP_COOKIE_ECHOED 1
#endif
#if ! HAVE_DECL_SCTP_ESTABLISHED && HAVE_DECL_SCTPS_ESTABLISHED
#    define SCTP_ESTABLISHED SCTPS_ESTABLISHED
#    undef HAVE_DECL_SCTP_ESTABLISHED
#    define HAVE_DECL_SCTP_ESTABLISHED 1
#endif
#if ! HAVE_DECL_SCTP_SHUTDOWN_PENDING && HAVE_DECL_SCTPS_SHUTDOWN_PENDING
#    define SCTP_SHUTDOWN_PENDING SCTPS_SHUTDOWN_PENDING
#    undef HAVE_DECL_SCTP_SHUTDOWN_PENDING
#    define HAVE_DECL_SCTP_SHUTDOWN_PENDING 1
#endif
#if ! HAVE_DECL_SCTP_SHUTDOWN_SENT && HAVE_DECL_SCTPS_SHUTDOWN_SENT
#    define SCTP_SHUTDOWN_SENT SCTPS_SHUTDOWN_SENT
#    undef HAVE_DECL_SCTP_SHUTDOWN_SENT
#    define HAVE_DECL_SCTP_SHUTDOWN_SENT 1
#endif
#if ! HAVE_DECL_SCTP_SHUTDOWN_RECEIVED && HAVE_DECL_SCTPS_SHUTDOWN_RECEIVED
#    define SCTP_SHUTDOWN_RECEIVED SCTPS_SHUTDOWN_RECEIVED
#    undef HAVE_DECL_SCTP_SHUTDOWN_RECEIVED
#    define HAVE_DECL_SCTP_SHUTDOWN_RECEIVED 1
#endif
#if ! HAVE_DECL_SCTP_SHUTDOWN_ACK_SENT && HAVE_DECL_SCTPS_SHUTDOWN_ACK_SENT
#    define SCTP_SHUTDOWN_ACK_SENT SCTPS_SHUTDOWN_ACK_SENT
#    undef HAVE_DECL_SCTP_SHUTDOWN_ACK_SENT
#    define HAVE_DECL_SCTP_SHUTDOWN_ACK_SENT 1
#endif
/* New spelling in lksctp 2.6.22 or maybe even earlier:
 *  adaption -> adaptation
 */
#if !defined(SCTP_ADAPTATION_LAYER) && defined (SCTP_ADAPTION_LAYER)
#     define SCTP_ADAPTATION_LAYER       SCTP_ADAPTION_LAYER
#     define SCTP_ADAPTATION_INDICATION  SCTP_ADAPTION_INDICATION
#     define sctp_adaptation_event       sctp_adaption_event
#     define sctp_setadaptation          sctp_setadaption
#     define sn_adaptation_event         sn_adaption_event
#     define sai_adaptation_ind          sai_adaption_ind
#     define ssb_adaptation_ind          ssb_adaption_ind
#     define sctp_adaptation_layer_event sctp_adaption_layer_event
#endif

/*
 * We *may* need this stuff later when we *fully* implement support for SCTP 
 *

#if defined(__GNUC__) && defined(HAVE_SCTP_BINDX)
static typeof(sctp_bindx) *esock_sctp_bindx = NULL;
#else
static int (*esock_sctp_bindx)
	(int sd, struct sockaddr *addrs, int addrcnt, int flags) = NULL;
#endif

#if defined(__GNUC__) && defined(HAVE_SCTP_PEELOFF)
static typeof(sctp_peeloff) *esock_sctp_peeloff = NULL;
#else
static int (*esock_sctp_peeloff)
        (int sd, sctp_assoc_t assoc_id) = NULL;
#endif

#if defined(__GNUC__) && defined(HAVE_SCTP_GETLADDRS)
static typeof(sctp_getladdrs) *esock_sctp_getladdrs = NULL;
#else
static int (*esock_sctp_getladdrs)
        (int sd, sctp_assoc_t assoc_id, struct sockaddr **ss) = NULL;
#endif

#if defined(__GNUC__) && defined(HAVE_SCTP_FREELADDRS)
static typeof(sctp_freeladdrs) *esock_sctp_freeladdrs = NULL;
#else
static void (*esock_sctp_freeladdrs)(struct sockaddr *addrs) = NULL;
#endif

#if defined(__GNUC__) && defined(HAVE_SCTP_GETPADDRS)
static typeof(sctp_getpaddrs) *esock_sctp_getpaddrs = NULL;
#else
static int (*esock_sctp_getpaddrs)
        (int sd, sctp_assoc_t assoc_id, struct sockaddr **ss) = NULL;
#endif

#if defined(__GNUC__) && defined(HAVE_SCTP_FREEPADDRS)
static typeof(sctp_freepaddrs) *esock_sctp_freepaddrs = NULL;
#else
static void (*esock_sctp_freepaddrs)(struct sockaddr *addrs) = NULL;
#endif

*/

#endif /* #if defined(HAVE_SCTP_H) */


#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif
#include "sys.h"

/* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *
 *                                                                        *
 * End of non-__WIN32__ section a.k.a UNIX section                        *
 *                                                                        *
 * ---------------------------------------------------------------------- */
#endif /* #ifdef __WIN32__  #else */



#include <erl_nif.h>

#include "socket_dbg.h"
#include "socket_tarray.h"
#include "socket_int.h"
#include "socket_util.h"
#include "prim_file_nif_dyncall.h"

#if defined(ERTS_INLINE)
#  define ESOCK_INLINE ERTS_INLINE
#else
#  if defined(__GNUC__)
#    define ESOCK_INLINE __inline__
#  elif defined(__WIN32__)
#    define ESOCK_INLINE __inline
#  else
#    define ESOCK_INLINE
#  endif
#endif


#if defined(SOL_IPV6) || defined(IPPROTO_IPV6)
#define HAVE_IPV6
#endif

/* Debug stuff... */
#define ESOCK_GLOBAL_DEBUG_DEFAULT FALSE
#define ESOCK_DEBUG_DEFAULT        FALSE

/* Counters and stuff (Don't know where to send this stuff anyway) */
#define ESOCK_NIF_IOW_DEFAULT FALSE


#ifdef __WIN32__

//#define INVALID_HANDLE        from Windows header file
//typedef void   *HANDLE        from Windows header file
//#define INVALID_SOCKET        from Windows header file
//typedef void   *SOCKET        from Windows header file
#define INVALID_EVENT NULL

#else

#define INVALID_HANDLE (-1)
typedef int HANDLE;
#define INVALID_SOCKET (-1)
typedef int SOCKET; /* A subset of HANDLE */
#define INVALID_EVENT INVALID_HANDLE

#endif


/* ==============================================================================
 * The ESOCK_IS_ERROR macro below is used for portability reasons.
 * While POSIX specifies that errors from socket-related system calls
 * should be indicated with a -1 return value, some users have experienced
 * non-Windows OS kernels that return negative values other than -1.
 * While one can argue that such kernels are technically broken, comparing
 * against values less than 0 covers their out-of-spec return values without
 * imposing incorrect semantics on systems that manage to correctly return -1
 * for errors, thus increasing Erlang's portability.
 */
#ifdef __WIN32__
#define ESOCK_IS_ERROR(val) ((val) == INVALID_SOCKET)
#else
#define ESOCK_IS_ERROR(val) ((val) < 0)
#endif


/* *** Misc macros and defines *** */

/* This macro exist on some (linux) platforms */
#if !defined(IPTOS_TOS_MASK)
#define IPTOS_TOS_MASK     0x1E
#endif
#if !defined(IPTOS_TOS)
#define IPTOS_TOS(tos)          ((tos)&IPTOS_TOS_MASK)
#endif


#if defined(TCP_CA_NAME_MAX)
#define ESOCK_OPT_TCP_CONGESTION_NAME_MAX TCP_CA_NAME_MAX
#else
/* This is really excessive, but just in case... */
#define ESOCK_OPT_TCP_CONGESTION_NAME_MAX 256
#endif


#if defined(TCP_CONGESTION) || defined(SO_BINDTODEVICE)
#define USE_GETOPT_STR_OPT
#define USE_SETOPT_STR_OPT
#endif



/* *** Socket state defs *** */

#define ESOCK_STATE_BOUND        0x0001 /* readState */
#define ESOCK_STATE_LISTENING    0x0002 /* readState */
#define ESOCK_STATE_ACCEPTING    0x0004 /* readState */
#define ESOCK_STATE_CONNECTING   0x0010 /* writeState */
#define ESOCK_STATE_CONNECTED    0x0020 /* writeState */

/* This is set in either readState or writeState
 * so it has to be read from both.
 * Means that the socket has been used in select,
 * so select_stop is required. */
#define ESOCK_STATE_SELECTED     0x0100 /* readState or writeState */

/* These are set in both readState and writeState
 * so they can be read from either. */
#define ESOCK_STATE_CLOSING      0x0200 /* readState and writeState */

#define ESOCK_STATE_CLOSED       0x0400 /* readState and writeState */
//
#define ESOCK_STATE_DTOR         0x8000

#define IS_CLOSED(st)                           \
    (((st) & ESOCK_STATE_CLOSED) != 0)

#define IS_CLOSING(st)                          \
    (((st) & ESOCK_STATE_CLOSING) != 0)

#define IS_OPEN(st)                                             \
    (((st) & (ESOCK_STATE_CLOSED | ESOCK_STATE_CLOSING)) == 0)

#define IS_SELECTED(d)                                                  \
    ((((d)->readState | (d)->writeState) & ESOCK_STATE_SELECTED) != 0)

    
#define ESOCK_GET_RESOURCE(ENV, REF, RES) \
    enif_get_resource((ENV), (REF), esocks, (RES))

#define ESOCK_RECV_BUFFER_COUNT_DEFAULT     0
#define ESOCK_RECV_BUFFER_SIZE_DEFAULT      8192
#define ESOCK_RECV_CTRL_BUFFER_SIZE_DEFAULT 1024
#define ESOCK_SEND_CTRL_BUFFER_SIZE_DEFAULT 1024

#define ESOCK_DESC_PATTERN_CREATED 0x03030303
#define ESOCK_DESC_PATTERN_DTOR    0xC0C0C0C0

/*
typedef union {
    struct {
        // 0 = not open, 1 = open
        unsigned int open:1;
        // 0 = not conn, 1 = connecting, 2 = connected
        unsigned int connect:2;
        // unsigned int connecting:1;
        // unsigned int connected:1;
        // 0 = not listen, 1 = listening, 2 = accepting
        unsigned int listen:2;
        // unsigned int listening:1;
        // unsigned int accepting:1;
        / * Room for more... * /
    } flags;
    unsigned int field; // Make it easy to reset all flags...
} SocketState;
*/

/*----------------------------------------------------------------------------
 * Interface constants.
 *
 * The set of elements should be the same as for the type
 * msg_flag() in socket.erl.
 */

static const struct msg_flag {
    int flag;
    ERL_NIF_TERM *name;
} msg_flags[] = {

    {
#ifdef MSG_CMSG_CLOEXEC
        MSG_CMSG_CLOEXEC,
#else
        0,
#endif
        &esock_atom_cmsg_cloexec},

    {
#ifdef MSG_CONFIRM
        MSG_CONFIRM,
#else
        0,
#endif
        &esock_atom_confirm},

    {
#ifdef MSG_CTRUNC
        MSG_CTRUNC,
#else
        0,
#endif
        &esock_atom_ctrunc},

    {
#ifdef MSG_DONTROUTE
        MSG_DONTROUTE,
#else
        0,
#endif
        &esock_atom_dontroute},

    {
#ifdef MSG_EOR
        MSG_EOR,
#else
        0,
#endif
        &esock_atom_eor},

    {
#ifdef MSG_ERRQUEUE
        MSG_ERRQUEUE,
#else
        0,
#endif
        &esock_atom_errqueue},

    {
#ifdef MSG_MORE
        MSG_MORE,
#else
        0,
#endif
        &esock_atom_more},

    {
#ifdef MSG_NOSIGNAL
        MSG_NOSIGNAL,
#else
        0,
#endif
        &esock_atom_nosignal},

    {
#ifdef MSG_OOB
        MSG_OOB,
#else
        0,
#endif
        &esock_atom_oob},

    {
#ifdef MSG_PEEK
        MSG_PEEK,
#else
        0,
#endif
        &esock_atom_peek},

    {
#ifdef MSG_TRUNC
        MSG_TRUNC,
#else
        0,
#endif
        &esock_atom_trunc}
};



/* level 'otp' options */
#define ESOCK_OPT_OTP_DEBUG        1001
#define ESOCK_OPT_OTP_IOW          1002
#define ESOCK_OPT_OTP_CTRL_PROC    1003
#define ESOCK_OPT_OTP_RCVBUF       1004
//#define ESOCK_OPT_OTP_SNDBUF       1005
#define ESOCK_OPT_OTP_RCVCTRLBUF   1006
#define ESOCK_OPT_OTP_SNDCTRLBUF   1007
#define ESOCK_OPT_OTP_FD           1008
#define ESOCK_OPT_OTP_META         1009
#define ESOCK_OPT_OTP_USE_REGISTRY 1010
/**/
#define ESOCK_OPT_OTP_DOMAIN       1999 // INTERNAL AND ONLY GET
#if 0
#define ESOCK_OPT_OTP_TYPE         1998 // INTERNAL AND ONLY GET
#define ESOCK_OPT_OTP_PROTOCOL     1997 // INTERNAL AND ONLY GET
#define ESOCK_OPT_OTP_DTP          1996 // INTERNAL AND ONLY GET
#endif


/*--------------------------------------------------------------------------*/


/* We should *eventually* use this instead of hard-coding the size (to 1) */
#define ESOCK_RECVMSG_IOVEC_SZ 1

#define ESOCK_CMD_DEBUG               0x0001
#define ESOCK_CMD_SOCKET_DEBUG        0x0002
#define ESOCK_CMD_USE_SOCKET_REGISTRY 0x0003


/* =================================================================== *
 *                                                                     *
 *                        Various esockmacros                          *
 *                                                                     *
 * =================================================================== */

/* Global socket debug */
#define SGDBG( proto )            ESOCK_DBG_PRINTF( data.dbg , proto )
/* Socket specific debug */
#define SSDBG( __D__ , proto )    ESOCK_DBG_PRINTF( (__D__)->dbg , proto )
#define SSDBG2( __DBG__ , proto ) ESOCK_DBG_PRINTF( (__DBG__) , proto )

#define ESOCK_CNT_INC( __E__, __D__, SF, ACNT, CNT, INC)                \
    do {                                                                \
        if (cnt_inc((CNT), (INC))) {					\
	  esock_send_wrap_msg((__E__), (__D__), (SF), (ACNT));		\
	}								\
    } while (0)


/* =================================================================== *
 *                                                                     *
 *                    Basic socket operations                          *
 *                                                                     *
 * =================================================================== */

#ifdef __WIN32__
/* ---------------------------------------------------------------------- *
 *                                                                        *
 * Start of __WIN32__ section                                             *
 *                                                                        *
 * vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv */

/* *** Windows macros *** */

#define sock_accept(s, addr, len) \
    make_noninheritable_handle(accept((s), (addr), (len)))
#define sock_bind(s, addr, len)        bind((s), (addr), (len))
#define sock_close(s)                  closesocket((s))
#define sock_close_event(e)            WSACloseEvent(e)
#define sock_connect(s, addr, len)     connect((s), (addr), (len))
#define sock_create_event(s)           WSACreateEvent()
#define sock_errno()                   WSAGetLastError()
#define sock_getopt(s,l,o,v,ln)        getsockopt((s),(l),(o),(v),(ln))
#define sock_htons(x)                  htons((x))
#define sock_htonl(x)                  htonl((x))
#define sock_listen(s, b)              listen((s), (b))
#define sock_name(s, addr, len)        getsockname((s), (addr), (len))
#define sock_ntohs(x)                  ntohs((x))
#define sock_open(domain, type, proto)                             \
    make_noninheritable_handle(socket((domain), (type), (proto)))
#define sock_peer(s, addr, len)    getpeername((s), (addr), (len))
#define sock_recv(s,buf,len,flag)  recv((s),(buf),(len),(flag))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
    recvfrom((s),(buf),(blen),(flag),(addr),(alen))
#define sock_send(s,buf,len,flag)      send((s),(buf),(len),(flag))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
    sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_setopt(s,l,o,v,ln)        setsockopt((s),(l),(o),(v),(ln))
#define sock_shutdown(s, how)          shutdown((s), (how))


#define SET_BLOCKING(s)            ioctlsocket(s, FIONBIO, &zero_value)
#define SET_NONBLOCKING(s)         ioctlsocket(s, FIONBIO, &one_value)
static unsigned long zero_value = 0;
static unsigned long one_value  = 1;


/* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *
 *                                                                        *
 * End of __WIN32__ section                                               *
 *                                                                        *
 * ---------------------------------------------------------------------- */
#else /* #ifdef __WIN32__ */
/* ---------------------------------------------------------------------- *
 *                                                                        *
 * Start of non-__WIN32__ section a.k.a UNIX section                      *
 *                                                                        *
 * vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv */


#ifdef HAS_ACCEPT4
// We have to figure out what the flags are...
#define sock_accept(s, addr, len)       accept4((s), (addr), (len), (SOCK_CLOEXEC))
#else
#define sock_accept(s, addr, len)       accept((s), (addr), (len))
#endif
#define sock_bind(s, addr, len)         bind((s), (addr), (len))
#define sock_close(s)                   close((s))
#define sock_close_event(e)             /* do nothing */
#define sock_connect(s, addr, len)      connect((s), (addr), (len))
#define sock_create_event(s)            (s) /* return file descriptor */
#define sock_errno()                    errno
#define sock_getopt(s,t,n,v,l)          getsockopt((s),(t),(n),(v),(l))
#define sock_htons(x)                   htons((x))
#define sock_htonl(x)                   htonl((x))
#define sock_listen(s, b)               listen((s), (b))
#define sock_name(s, addr, len)         getsockname((s), (addr), (len))
#define sock_ntohs(x)                   ntohs((x))
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))
#define sock_peer(s, addr, len)         getpeername((s), (addr), (len))
#define sock_recv(s,buf,len,flag)       recv((s),(buf),(len),(flag))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
    recvfrom((s),(buf),(blen),(flag),(addr),(alen))
#define sock_recvmsg(s,msghdr,flag)     recvmsg((s),(msghdr),(flag))
#define sock_send(s,buf,len,flag)       send((s), (buf), (len), (flag))
#define sock_sendmsg(s,msghdr,flag)     sendmsg((s),(msghdr),(flag))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_setopt(s,l,o,v,ln)         setsockopt((s),(l),(o),(v),(ln))
#define sock_shutdown(s, how)           shutdown((s), (how))


/* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *
 *                                                                        *
 * End of non-__WIN32__ section a.k.a UNIX section                        *
 *                                                                        *
 * ---------------------------------------------------------------------- */
#endif /* #ifdef __WIN32__  #else */


/* We can use the IPv4 def for this since the beginning
 * is the same for INET and INET6 */
#define which_address_port(sap)		     \
  ((((sap)->in4.sin_family == AF_INET) ||  \
    ((sap)->in4.sin_family == AF_INET6)) ? \
   ((sap)->in4.sin_port) : -1)


typedef struct {
    ErlNifMonitor mon;
    BOOLEAN_T     isActive;
} ESockMonitor;

typedef struct {
    ErlNifPid    pid; // PID of the requesting process
    ESockMonitor mon; // Monitor to the requesting process

    /* We need an environment for the copy of the ref we store here.
     * We will also use this environment for any messages we send
     * (with the ref in it). Such as the select message (used in the 
     * select call) or the abort message.
     */
    ErlNifEnv*   env;
    ERL_NIF_TERM ref; // The (unique) reference (ID) of the request
} ESockRequestor;

typedef struct{
    // Holding the socket level 'otp' option 'meta' term
    ErlNifEnv* env;
    ERL_NIF_TERM ref;
} ESockMeta;

typedef struct esock_request_queue_element {
    struct esock_request_queue_element* nextP;
    ESockRequestor                      data;
} ESockRequestQueueElement;

typedef struct {
    ESockRequestQueueElement* first;
    ESockRequestQueueElement* last;
} ESockRequestQueue;


/*** The point of this is primarily testing ***/
/*
#if defined(ESOCK_COUNTER_SIZE)
#undef ESOCK_COUNTER_SIZE
// #define ESOCK_COUNTER_SIZE 16
// #define ESOCK_COUNTER_SIZE 24
// #define ESOCK_COUNTER_SIZE 32
// #define ESOCK_COUNTER_SIZE 48
// #define ESOCK_COUNTER_SIZE 64

#endif
*/

#if ESOCK_COUNTER_SIZE == 16

typedef Uint16                   ESockCounter;
#define ESOCK_COUNTER_MAX        ((ESockCounter) 0xFFFF)
#define MKCNT(ENV, CNT)          MKUI((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%u"

#elif ESOCK_COUNTER_SIZE == 24

typedef Uint32                   ESockCounter;
#define ESOCK_COUNTER_MAX        ((ESockCounter) 0xFFFFFF)
#define MKCNT(ENV, CNT)          MKUI((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((ENV), (CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%lu"

#elif ESOCK_COUNTER_SIZE == 32

typedef Uint32 ESockCounter;
#define ESOCK_COUNTER_MAX        (~((ESockCounter) 0))
#define MKCNT(ENV, CNT)          MKUI((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((ENV), (CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%lu"

#elif ESOCK_COUNTER_SIZE == 48

typedef Uint64                   ESockCounter;
#define ESOCK_COUNTER_MAX        ((ESockCounter) 0xFFFFFFFFFFFF)
#define MKCNT(ENV, CNT)          MKUI64((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((ENV), (CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%llu"

#elif ESOCK_COUNTER_SIZE == 64

typedef Uint64                   ESockCounter;
#define ESOCK_COUNTER_MAX        (~((ESockCounter) 0))
#define MKCNT(ENV, CNT)          MKUI64((ENV), (CNT))
#define MKCT(ENV, TAG, CNT)      MKT2((ENV), (TAG), MKCNT((ENV), (CNT)))
#define ESOCK_COUNTER_FORMAT_STR "%llu"

#else

#error "Invalid counter size"

#endif

// static const ESockCounter esock_counter_max = ESOCK_COUNTER_MAX;

#ifdef HAVE_SENDFILE

typedef struct {
    ESockCounter       cnt;     // Calls to OS sendfile()
    ESockCounter       byteCnt; // Bytes sent with sendfile
    ESockCounter       fails;   // Failed sendfile operations
    ESockCounter       max;     // Largest sendfile operation
    ESockCounter       maxCnt;  // Counter for ="=
    ESockCounter       pkg;     // Sendfile chunks
    ESockCounter       pkgMax;  // Largest sendfile chunk
    ESockCounter       tries;   // Started sendfile operations
    ESockCounter       waits;   // Select's during sendfile
} ESockSendfileCounters;
static ESockSendfileCounters initESockSendfileCounters =
    {0, 0, 0, 0, 0, 0, 0, 0, 0};

#endif


typedef struct {
    /* 
     * +++ This is a way to, possibly, detect memory overrides "and stuff" +++
     *
     * We have two patterns. One is set when the descriptor is created
     * (allocated) and one is set when the descriptor is dtor'ed.
     */
    Uint32             pattern;

    /* +++ Stuff "about" the socket +++ */

    /* "Constant" - set when socket is created and never changed */
    int                domain;
    int                type;
    int                protocol;

    /* The state is partly for debugging, decisions are made often
     * based on other variables.  The state is divided in
     * a readState half and a writeState half that can be
     * OR:ed together to create the complete state.
     * The halves are locked by their corresponding lock.
     */

    /* +++ Write stuff +++ */
    ErlNifMutex*       writeMtx;
    /**/
    unsigned int       writeState; // For debugging
    ESockRequestor     currentWriter;
    ESockRequestor*    currentWriterP; // NULL or &currentWriter
    ESockRequestQueue  writersQ;
    ESockCounter       writePkgCnt;
    ESockCounter       writePkgMax;
    ESockCounter       writePkgMaxCnt;
    ESockCounter       writeByteCnt;
    ESockCounter       writeTries;
    ESockCounter       writeWaits;
    ESockCounter       writeFails;
#ifdef HAVE_SENDFILE
    HANDLE                 sendfileHandle;
    ESockSendfileCounters* sendfileCountersP;
#endif
    /* +++ Connector +++ */
    ESockRequestor     connector;
    ESockRequestor*    connectorP; // NULL or &connector
    /* +++ Config stuff +++ */
    size_t             wCtrlSz; // Write control buffer size
    ESockMeta          meta;    // Level 'otp' option 'meta' term

    /* +++ Read stuff +++ */
    ErlNifMutex*       readMtx;
    /**/
    unsigned int       readState; // For debugging
    ESockRequestor     currentReader;
    ESockRequestor*    currentReaderP; // NULL or &currentReader
    ESockRequestQueue  readersQ;
    ErlNifBinary       rbuffer;      // DO WE NEED THIS
    Uint32             readCapacity; // DO WE NEED THIS
    ESockCounter       readPkgCnt;
    ESockCounter       readPkgMax;
    ESockCounter       readPkgMaxCnt;
    ESockCounter       readByteCnt;
    ESockCounter       readTries;
    ESockCounter       readWaits;
    ESockCounter       readFails;
    /* +++ Accept stuff +++ */
    ESockRequestor     currentAcceptor;
    ESockRequestor*    currentAcceptorP; // NULL or &currentAcceptor
    ESockRequestQueue  acceptorsQ;
    ESockCounter       accSuccess;
    ESockCounter       accTries;
    ESockCounter       accWaits;
    ESockCounter       accFails;
    /* +++ Config stuff +++ */
    size_t             rBufSz;  // Read buffer size (when data length = 0)
    /* rNum and rNumCnt are used (together with rBufSz) when calling the recv 
     * function with the Length argument set to 0 (zero).
     * If rNum is 0 (zero), then rNumCnt is not used and only *one* read will
     * be done. Also, when get'ing the value of the option (rcvbuf) with 
     * getopt, the value will be reported as an integer. If the rNum has a 
     * value greater then 0 (zero), then it will instead be reported as {N, BufSz}.
     */
    unsigned int       rNum;    // recv: Number of reads using rBufSz
    unsigned int       rNumCnt; // recv: Current number of reads (so far)
    size_t             rCtrlSz; // Read control buffer size

    /* Locked by readMtx and writeMtx combined for writing,
     * which means only one of them is required for reading
     */
    /* +++ Close stuff +++ */
    ErlNifPid          closerPid;
    ESockMonitor       closerMon;
    ErlNifEnv*         closeEnv;
    ERL_NIF_TERM       closeRef;
    /* +++ Inform On (counter) Wrap +++ */
    BOOLEAN_T          iow;
    /* +++ Controller (owner) process +++ */
    ErlNifPid          ctrlPid;
    ESockMonitor       ctrlMon;
    /* +++ The actual socket +++ */
    SOCKET             sock;
    ErlNifEvent        event;
    SOCKET             origFD; // A 'socket' created from this FD
    BOOLEAN_T          closeOnClose; // Have we dup'ed or not
    /* +++ The dbg flag for SSDBG +++ */
    BOOLEAN_T          dbg;
    BOOLEAN_T          useReg;

    /* Lock order: readMtx, writeMtx, cntMtx
     */
} ESockDescriptor;


/* Global stuff.
 */
typedef struct {
    /* These are for debugging, testing and the like */
    // ERL_NIF_TERM version;
    // ERL_NIF_TERM buildDate;

    /* XXX Should be locked but too awkward and small gain */
    BOOLEAN_T    dbg;
    BOOLEAN_T    useReg;

    /* Registry stuff */
    ErlNifPid    regPid; /* Constant - not locked */

    /* IOV_MAX. Constant - not locked */
    int          iov_max;

    /* XXX
     * Should be locked but too awkward for no gain since it is not used yet
     */
    BOOLEAN_T    iow; // Where do we send this? Subscription?

    ErlNifMutex* protocolsMtx;

    ErlNifMutex* cntMtx; /* Locks the below */
    /* Its extreme overkill to have these counters be 64-bit,
     * but since the other counters are, it's much simpler to
     * let these be 64-bit also.
     */
    ESockCounter numSockets;
    ESockCounter numTypeStreams;
    ESockCounter numTypeDGrams;
    ESockCounter numTypeSeqPkgs;
    ESockCounter numDomainInet;
    ESockCounter numDomainInet6;
    ESockCounter numDomainLocal;
    ESockCounter numProtoIP;
    ESockCounter numProtoTCP;
    ESockCounter numProtoUDP;
    ESockCounter numProtoSCTP;
    //
    BOOLEAN_T    sockDbg;
} ESockData;


/* ----------------------------------------------------------------------
 *  F o r w a r d s
 * ----------------------------------------------------------------------
 */


extern char* erl_errno_id(int error); /* THIS IS JUST TEMPORARY??? */


/* All the nif "callback" functions for the socket API has
 * the exact same API:
 *
 * nif_<funcname>(ErlNifEnv*         env,
 *                int                argc,
 *                const ERL_NIF_TERM argv[]);
 *
 * So, to simplify, use some macro magic to define those.
 *
 * These are the functions making up the "official" API.
 * Basically, these functions does some preliminary checks and argument
 * extractions and then call the functions called 'n<funcname>', which 
 * does the actual work. Except for the info function.
 *
 * nif_info
 * nif_command
 * nif_supports
 * nif_open
 * nif_bind
 * nif_connect
 * nif_listen
 * nif_accept
 * nif_send
 * nif_sendto
 * nif_sendmsg
 * nif_sendfile
 * nif_recv
 * nif_recvfrom
 * nif_recvmsg
 * nif_close
 * nif_shutdown
 * nif_setopt
 * nif_getopt
 * nif_sockname
 * nif_peername
 * nif_finalize_close
 * nif_cancel
 */

#define ESOCK_NIF_FUNCS                             \
    ESOCK_NIF_FUNC_DEF(info);                       \
    ESOCK_NIF_FUNC_DEF(command);                    \
    ESOCK_NIF_FUNC_DEF(supports);                   \
    ESOCK_NIF_FUNC_DEF(open);                       \
    ESOCK_NIF_FUNC_DEF(bind);                       \
    ESOCK_NIF_FUNC_DEF(connect);                    \
    ESOCK_NIF_FUNC_DEF(listen);                     \
    ESOCK_NIF_FUNC_DEF(accept);                     \
    ESOCK_NIF_FUNC_DEF(send);                       \
    ESOCK_NIF_FUNC_DEF(sendto);                     \
    ESOCK_NIF_FUNC_DEF(sendmsg);                    \
    ESOCK_NIF_FUNC_DEF(recv);                       \
    ESOCK_NIF_FUNC_DEF(recvfrom);                   \
    ESOCK_NIF_FUNC_DEF(recvmsg);                    \
    ESOCK_NIF_FUNC_DEF(close);                      \
    ESOCK_NIF_FUNC_DEF(shutdown);                   \
    ESOCK_NIF_FUNC_DEF(setopt);                     \
    ESOCK_NIF_FUNC_DEF(getopt);                     \
    ESOCK_NIF_FUNC_DEF(sockname);                   \
    ESOCK_NIF_FUNC_DEF(peername);                   \
    ESOCK_NIF_FUNC_DEF(finalize_close);             \
    ESOCK_NIF_FUNC_DEF(cancel);

#define ESOCK_NIF_FUNC_DEF(F)                              \
    static ERL_NIF_TERM nif_##F(ErlNifEnv*         env,    \
                                int                argc,   \
                                const ERL_NIF_TERM argv[]);
ESOCK_NIF_FUNCS
#undef ESOCK_NIF_FUNC_DEF


#ifndef __WIN32__
/* ---------------------------------------------------------------------- *
 *                                                                        *
 *                                                                        *
 * Start of non-__WIN32__ section a.k.a UNIX section                      *
 *                                                                        *
 *                                                                        *
 * vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv */

/* And here comes the functions that does the actual work (for the most part) */

static BOOLEAN_T ecommand2command(ErlNifEnv*    env,
                                  ERL_NIF_TERM  ecommand,
                                  Uint16*       command,
                                  ERL_NIF_TERM* edata);
static ERL_NIF_TERM esock_command(ErlNifEnv*   env,
                                  Uint16       cmd,
                                  ERL_NIF_TERM ecdata);
static ERL_NIF_TERM esock_command_debug(ErlNifEnv*   env,
                                        ERL_NIF_TERM ecdata);
static ERL_NIF_TERM esock_command_socket_debug(ErlNifEnv*   env,
                                               ERL_NIF_TERM ecdata);
static ERL_NIF_TERM esock_command_use_socket_registry(ErlNifEnv*   env,
                                                      ERL_NIF_TERM ecdata);

static ERL_NIF_TERM esock_global_info(ErlNifEnv* env);
static ERL_NIF_TERM esock_socket_info(ErlNifEnv*       env,
                                      ESockDescriptor* descP);
static ERL_NIF_TERM esock_socket_info_domain(ErlNifEnv*       env,
                                             ESockDescriptor* descP);
static ERL_NIF_TERM esock_socket_info_type(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
static ERL_NIF_TERM esock_socket_info_counters(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
static ERL_NIF_TERM esock_socket_info_ctype(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
static ERL_NIF_TERM esock_socket_info_state(ErlNifEnv*   env,
					    unsigned int state);
#define ESOCK_SOCKET_INFO_REQ_FUNCS              \
    ESOCK_SOCKET_INFO_REQ_FUNC_DEF(readers);     \
    ESOCK_SOCKET_INFO_REQ_FUNC_DEF(writers);     \
    ESOCK_SOCKET_INFO_REQ_FUNC_DEF(acceptors);

#define ESOCK_SOCKET_INFO_REQ_FUNC_DEF(F)                               \
    static ERL_NIF_TERM esock_socket_info_##F(ErlNifEnv*         env,   \
                                              ESockDescriptor*   descP);
ESOCK_SOCKET_INFO_REQ_FUNCS
#undef ESOCK_SOCKET_INFO_REQ_FUNC_DEF

static ERL_NIF_TERM socket_info_reqs(ErlNifEnv*         env,
                                     ESockDescriptor*   descP,
                                     ESockRequestor*    currentRequestorP,
                                     ESockRequestQueue* q);

static ERL_NIF_TERM esock_supports_0(ErlNifEnv* env);
static ERL_NIF_TERM esock_supports_1(ErlNifEnv* env, ERL_NIF_TERM key);

static ERL_NIF_TERM esock_supports_msg_flags(ErlNifEnv* env);
static ERL_NIF_TERM esock_supports_protocols(ErlNifEnv* env);
static ERL_NIF_TERM esock_supports_options(ErlNifEnv* env);

static ERL_NIF_TERM esock_open2(ErlNifEnv*   env,
                                int          fd,
                                ERL_NIF_TERM eextra);
static BOOLEAN_T esock_open2_todup(ErlNifEnv*   env,
                                   ERL_NIF_TERM eextra);
static BOOLEAN_T esock_open2_get_domain(ErlNifEnv*   env,
                                        ERL_NIF_TERM eopts,
                                        int*         domain);
static BOOLEAN_T esock_open2_get_type(ErlNifEnv*   env,
                                      ERL_NIF_TERM eopt,
                                      int*         type);
static ERL_NIF_TERM esock_open4(ErlNifEnv*   env,
                                int          domain,
                                int          type,
                                int          protocol,
                                ERL_NIF_TERM eopts);
static BOOLEAN_T esock_open_is_debug(ErlNifEnv*   env,
                                     ERL_NIF_TERM eextra,
                                     BOOLEAN_T dflt);
static BOOLEAN_T esock_open_use_registry(ErlNifEnv*   env,
                                         ERL_NIF_TERM eextra,
                                         BOOLEAN_T dflt);
static BOOLEAN_T esock_open_which_domain(SOCKET sock,   int* domain);
static BOOLEAN_T esock_open_which_type(SOCKET sock,     int* type);
static BOOLEAN_T esock_open_which_protocol(SOCKET sock, int* proto);

static ERL_NIF_TERM esock_bind(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ESockAddress*    sockAddrP,
                               SOCKLEN_T        addrLen);
static ERL_NIF_TERM esock_connect(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     connRef,
                                  ESockAddress*    addrP,
                                  SOCKLEN_T        addrLen);
static ERL_NIF_TERM esock_listen(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              backlog);
static ERL_NIF_TERM esock_accept(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     ref);
static ERL_NIF_TERM esock_accept_listening_error(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 ERL_NIF_TERM     sockRef,
                                                 ERL_NIF_TERM     accRef,
                                                 ErlNifPid        caller,
                                                 int              save_errno);
static ERL_NIF_TERM esock_accept_listening_accept(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  SOCKET           accSock,
                                                  ErlNifPid        caller);
static ERL_NIF_TERM esock_accept_accepting_current(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     sockRef,
                                                   ERL_NIF_TERM     ref);
static ERL_NIF_TERM
esock_accept_accepting_current_accept(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      SOCKET           accSock);
static ERL_NIF_TERM esock_accept_accepting_current_error(ErlNifEnv*       env,
                                                         ESockDescriptor* descP,
                                                         ERL_NIF_TERM     sockRef,
                                                         ERL_NIF_TERM     opRef,
                                                         int              save_errno);
static ERL_NIF_TERM esock_accept_accepting_other(ErlNifEnv*       env,
						 ESockDescriptor* descP,
						 ERL_NIF_TERM     ref,
						 ErlNifPid        caller);
static ERL_NIF_TERM esock_accept_busy_retry(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     accRef,
                                            ErlNifPid*       pidP);
static BOOLEAN_T esock_accept_accepted(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       SOCKET           accSock,
                                       ErlNifPid        pid,
                                       ERL_NIF_TERM*    result);
static ERL_NIF_TERM esock_send(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     sendRef,
                               ErlNifBinary*    dataP,
                               int              flags);
static ERL_NIF_TERM esock_sendto(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     sendRef,
                                 ErlNifBinary*    dataP,
                                 int              flags,
                                 ESockAddress*    toAddrP,
                                 SOCKLEN_T        toAddrLen);
static ERL_NIF_TERM esock_sendmsg(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     sendRef,
                                  ERL_NIF_TERM     eMsg,
                                  int              flags,
                                  ERL_NIF_TERM     eIOV);

#ifdef HAVE_SENDFILE
static ERL_NIF_TERM
esock_sendfile_start(ErlNifEnv       *env,
                     ESockDescriptor *descP,
                     ERL_NIF_TERM     sockRef,
                     ERL_NIF_TERM     sendRef,
                     off_t            offset,
                     size_t           count,
                     ERL_NIF_TERM     fRef);
static ERL_NIF_TERM
esock_sendfile_cont(ErlNifEnv       *env,
                    ESockDescriptor *descP,
                    ERL_NIF_TERM     sockRef,
                    ERL_NIF_TERM     sendRef,
                    off_t            offset,
                    size_t           count);
static ERL_NIF_TERM
esock_sendfile_deferred_close(ErlNifEnv       *env,
                              ESockDescriptor *descP);
static int
esock_sendfile(ErlNifEnv       *env,
               ESockDescriptor *descP,
               ERL_NIF_TERM     sockRef,
               off_t            offset,
               size_t          *count,
               int             *errP);
static ERL_NIF_TERM
esock_sendfile_error(ErlNifEnv             *env,
                     ESockDescriptor       *descP,
                     ERL_NIF_TERM           sockRef,
                     ERL_NIF_TERM reason);
static ERL_NIF_TERM
esock_sendfile_errno(ErlNifEnv             *env,
                     ESockDescriptor       *descP,
                     ERL_NIF_TERM           sockRef,
                     int                    err);
static ERL_NIF_TERM
esock_sendfile_ok(ErlNifEnv       *env,
                  ESockDescriptor *descP,
                  ERL_NIF_TERM     sockRef,
                  size_t           count);
static ERL_NIF_TERM
esock_sendfile_select(ErlNifEnv       *env,
                      ESockDescriptor *descP,
                      ERL_NIF_TERM     sockRef,
                      ERL_NIF_TERM     sendRef,
                      size_t           count);
#endif // #ifdef HAVE_SENDFILE

static ERL_NIF_TERM esock_recv(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sendRef,
                               ERL_NIF_TERM     recvRef,
                               ssize_t          len,
                               int              flags);
static ERL_NIF_TERM esock_recvfrom(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     recvRef,
                                   ssize_t          bufSz,
                                   int              flags);
static ERL_NIF_TERM esock_recvmsg(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     recvRef,
                                  ssize_t          bufLen,
                                  ssize_t          ctrlLen,
                                  int              flags);
static ERL_NIF_TERM esock_close(ErlNifEnv*       env,
                                ESockDescriptor* descP);
static BOOLEAN_T esock_do_stop(ErlNifEnv* env,
                               ESockDescriptor* descP);
static ERL_NIF_TERM esock_shutdown(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              how);


/* Set OTP level options */
static ERL_NIF_TERM esock_setopt_otp(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              eOpt,
                                     ERL_NIF_TERM     eVal);
/* *** esock_setopt_otp_debug        ***
 * *** esock_setopt_otp_iow          ***
 * *** esock_setopt_otp_ctrl_proc    ***
 * *** esock_setopt_otp_rcvbuf       ***
 * *** esock_setopt_otp_rcvctrlbuf   ***
 * *** esock_setopt_otp_sndctrlbuf   ***
 * *** esock_setopt_otp_meta         ***
 * *** esock_setopt_otp_use_registry ***
 */
#define ESOCK_SETOPT_OTP_FUNCS               \
    ESOCK_SETOPT_OTP_FUNC_DEF(debug);        \
    ESOCK_SETOPT_OTP_FUNC_DEF(iow);          \
    ESOCK_SETOPT_OTP_FUNC_DEF(ctrl_proc);    \
    ESOCK_SETOPT_OTP_FUNC_DEF(rcvbuf);       \
    ESOCK_SETOPT_OTP_FUNC_DEF(rcvctrlbuf);   \
    ESOCK_SETOPT_OTP_FUNC_DEF(sndctrlbuf);   \
    ESOCK_SETOPT_OTP_FUNC_DEF(meta);	     \
    ESOCK_SETOPT_OTP_FUNC_DEF(use_registry);
#define ESOCK_SETOPT_OTP_FUNC_DEF(F)                                 \
    static ERL_NIF_TERM esock_setopt_otp_##F(ErlNifEnv*       env,   \
                                             ESockDescriptor* descP, \
                                             ERL_NIF_TERM     eVal)
ESOCK_SETOPT_OTP_FUNCS
#undef ESOCK_SETOPT_OTP_FUNC_DEF

/* Set native options */
static ERL_NIF_TERM esock_setopt_native(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              level,
                                        int              opt,
                                        ERL_NIF_TERM     eVal);
static ERL_NIF_TERM esock_setopt(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt,
                                 ERL_NIF_TERM     eVal);

/* *** Handling set of socket options for level = socket *** */

#if defined(SO_BINDTODEVICE)
static ERL_NIF_TERM esock_setopt_so_bindtodevice(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 int              level,
                                                 int              opt,
                                                 ERL_NIF_TERM     eVal);
#endif

#if defined(SO_LINGER)
static
ERL_NIF_TERM esock_setopt_linger(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt,
                                 ERL_NIF_TERM     eVal);
static
ERL_NIF_TERM esock_getopt_linger(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt);
#endif

#if defined(IP_MSFILTER) && defined(IP_MSFILTER_SIZE)
static ERL_NIF_TERM esock_setopt_msfilter(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              level,
                                          int              opt,
                                          ERL_NIF_TERM     eVal);
static BOOLEAN_T decode_msfilter_mode(ErlNifEnv*   env,
                                      ERL_NIF_TERM eVal,
                                      Uint32*      mode);
#endif
#if defined(IP_MTU_DISCOVER)
static ERL_NIF_TERM esock_setopt_ip_mtu_discover(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 int              level,
                                                 int              opt,
                                                 ERL_NIF_TERM     eVal);
#endif
#if defined(IP_MULTICAST_IF)
static ERL_NIF_TERM esock_setopt_multicast_if(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              level,
                                              int              opt,
                                              ERL_NIF_TERM     eVal);
#endif

#if defined(IP_TOS)
static ERL_NIF_TERM esock_setopt_tos(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              level,
                                     int              opt,
                                     ERL_NIF_TERM     eVal);
#endif

#if defined(IP_DROP_MEMBERSHIP) || defined(IP_ADD_MEMBERSHIP)
static
ERL_NIF_TERM esock_setopt_in_update_membership(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               int              level,
                                               int              opt,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(IP_ADD_SOURCE_MEMBERSHIP) || defined(IP_DROP_SOURCE_MEMBERSHIP) || defined(IP_BLOCK_SOURCE) || defined(IP_UNBLOCK_SOURCE)
static
ERL_NIF_TERM esock_setopt_in_update_source(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           int              level,
                                           int              opt,
                                           ERL_NIF_TERM     eVal);
#endif


/* *** Handling set of socket options for level = ipv6 *** */
#if defined(HAVE_IPV6)

#if defined(IPV6_ADDRFORM)
static ERL_NIF_TERM esock_setopt_addrform(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              level,
                                          int              opt,
                                          ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_MTU_DISCOVER)
static ERL_NIF_TERM esock_setopt_ipv6_mtu_discover(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   int              level,
                                                   int              opt,
                                                   ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_MULTICAST_HOPS)
static ERL_NIF_TERM esock_setopt_hops(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      int              opt,
                                      ERL_NIF_TERM     eVal);
#endif

#if defined(IPV6_ADD_MEMBERSHIP) || defined(IPV6_DROP_MEMBERSHIP)
static ERL_NIF_TERM
esock_setopt_in6_update_membership(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              opt,
                                   ERL_NIF_TERM     eVal);
#endif

#endif // defined(HAVE_IPV6)


#if defined(TCP_CONGESTION)
static ERL_NIF_TERM esock_setopt_tcp_congestion(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                int              level,
                                                int              opt,
                                                ERL_NIF_TERM     eVal);
#endif


#if defined(HAVE_SCTP)

#if defined(SCTP_ASSOCINFO)
static ERL_NIF_TERM esock_setopt_sctp_associnfo(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                int              level,
                                                int              opt,
                                                ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_EVENTS)
static ERL_NIF_TERM esock_setopt_sctp_events(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             int              level,
                                             int              opt,
                                             ERL_NIF_TERM     eVal);
static int esock_setopt_sctp_event(ErlNifEnv   *env,
                                   ERL_NIF_TERM eMap,
                                   ERL_NIF_TERM eKey,
                                   BOOLEAN_T   *failure);
#endif
#if defined(SCTP_INITMSG)
static ERL_NIF_TERM esock_setopt_sctp_initmsg(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              level,
                                              int              opt,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_RTOINFO)
static ERL_NIF_TERM esock_setopt_sctp_rtoinfo(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              level,
                                              int              opt,
                                              ERL_NIF_TERM     eVal);
#endif

#endif // defined(HAVE_SCTP)


static ERL_NIF_TERM esock_getopt_otp(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              eOpt);
/* *** esock_getopt_otp_debug        ***
 * *** esock_getopt_otp_iow          ***
 * *** esock_getopt_otp_ctrl_proc    ***
 * *** esock_getopt_otp_rcvbuf       ***
 * *** esock_getopt_otp_rcvctrlbuf   ***
 * *** esock_getopt_otp_sndctrlbuf   ***
 * *** esock_getopt_otp_fd           ***
 * *** esock_getopt_otp_meta         ***
 * *** esock_getopt_otp_use_registry ***
 * *** esock_getopt_otp_domain       ***
 * *** //esock_getopt_otp_type       ***
 * *** //esock_getopt_otp_protocol   ***
 * *** //esock_getopt_otp_dtp        ***
 */
#define ESOCK_GETOPT_OTP_FUNCS               \
    ESOCK_GETOPT_OTP_FUNC_DEF(debug);        \
    ESOCK_GETOPT_OTP_FUNC_DEF(iow);          \
    ESOCK_GETOPT_OTP_FUNC_DEF(ctrl_proc);    \
    ESOCK_GETOPT_OTP_FUNC_DEF(rcvbuf);       \
    ESOCK_GETOPT_OTP_FUNC_DEF(rcvctrlbuf);   \
    ESOCK_GETOPT_OTP_FUNC_DEF(sndctrlbuf);   \
    ESOCK_GETOPT_OTP_FUNC_DEF(fd);           \
    ESOCK_GETOPT_OTP_FUNC_DEF(meta);         \
    ESOCK_GETOPT_OTP_FUNC_DEF(use_registry); \
    ESOCK_GETOPT_OTP_FUNC_DEF(domain);
#if 0
    ESOCK_GETOPT_OTP_FUNC_DEF(type);         \
    ESOCK_GETOPT_OTP_FUNC_DEF(protocol);     \
    ESOCK_GETOPT_OTP_FUNC_DEF(dtp);
#endif
#define ESOCK_GETOPT_OTP_FUNC_DEF(F)                               \
    static ERL_NIF_TERM esock_getopt_otp_##F(ErlNifEnv*        env, \
                                             ESockDescriptor* descP)
ESOCK_GETOPT_OTP_FUNCS
#undef ESOCK_GETOPT_OTP_FUNC_DEF

static ERL_NIF_TERM esock_getopt_native(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              level,
                                        int              opt,
                                        ERL_NIF_TERM     valueSpec);
static ERL_NIF_TERM esock_getopt(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt);

#if defined(SO_BINDTODEVICE)
static ERL_NIF_TERM esock_getopt_so_bindtodevice(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 int              level,
                                                 int              opt);
#endif
#if defined(SO_DOMAIN)
static ERL_NIF_TERM esock_getopt_sock_domain(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             int              level,
                                             int              opt);
#endif

#if defined(SO_TYPE)
static
ERL_NIF_TERM esock_getopt_sock_type(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              level,
                                    int              opt);
#endif

#if defined(SO_PROTOCOL)
static
ERL_NIF_TERM esock_getopt_sock_protocol(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              level,
                                        int              opt);
#endif

#if defined(IP_MTU_DISCOVER)
static ERL_NIF_TERM esock_getopt_ip_mtu_discover(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              level,
                                              int              opt);
#endif
#if defined(IP_MULTICAST_IF)
static ERL_NIF_TERM esock_getopt_multicast_if(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              level,
                                              int              opt);
#endif
#if defined(IP_TOS)
static ERL_NIF_TERM esock_getopt_tos(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              level,
                                     int              opt);
#endif


#if defined(HAVE_IPV6)

#if defined(IPV6_MTU_DISCOVER)
static ERL_NIF_TERM esock_getopt_ipv6_mtu_discover(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   int              level,
                                                   int              opt);
#endif

#endif // defined(HAVE_IPV6)

#if defined(IP_PKTOPTIONS) || defined(IPV6_PKTOPTIONS)
static ERL_NIF_TERM esock_getopt_pktoptions(ErlNifEnv*       env,
					    ESockDescriptor* descP,
					    int              level,
					    int              opt);
#endif

#if defined(TCP_CONGESTION)
static ERL_NIF_TERM esock_getopt_tcp_congestion(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                int              level,
                                                int              opt);
#endif


#if defined(HAVE_SCTP)

#if defined(SCTP_ASSOCINFO)
static ERL_NIF_TERM esock_getopt_sctp_associnfo(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                int              level,
                                                int              opt);
#endif
#if defined(SCTP_INITMSG)
static ERL_NIF_TERM esock_getopt_sctp_initmsg(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              level,
                                              int              opt);
#endif
#if defined(SCTP_RTOINFO)
static ERL_NIF_TERM esock_getopt_sctp_rtoinfo(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              int              level,
                                              int              opt);
#endif

#endif // defined(HAVE_SCTP)


static ERL_NIF_TERM esock_sockname(ErlNifEnv*       env,
                                   ESockDescriptor* descP);
static ERL_NIF_TERM esock_peername(ErlNifEnv*       env,
                                   ESockDescriptor* descP);
static ERL_NIF_TERM esock_cancel(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     op,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     opRef);
static ERL_NIF_TERM esock_cancel_connect(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     opRef);
static ERL_NIF_TERM esock_cancel_accept(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     opRef);
static ERL_NIF_TERM esock_cancel_accept_current(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM esock_cancel_accept_waiting(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     opRef,
                                                const ErlNifPid* selfP);
static ERL_NIF_TERM esock_cancel_send(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     opRef);
static ERL_NIF_TERM esock_cancel_send_current(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM esock_cancel_send_waiting(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     opRef,
                                              const ErlNifPid* selfP);
static ERL_NIF_TERM esock_cancel_recv(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     opRef);
static ERL_NIF_TERM esock_cancel_recv_current(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM esock_cancel_recv_waiting(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     opRef,
                                              const ErlNifPid* selfP);
static ERL_NIF_TERM esock_cancel_read_select(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     opRef);
static ERL_NIF_TERM esock_cancel_write_select(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     opRef);
static ERL_NIF_TERM esock_cancel_mode_select(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     opRef,
                                             int              smode,
                                             int              rmode);

#if defined(USE_SETOPT_STR_OPT)
static ERL_NIF_TERM esock_setopt_str_opt(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt,
                                         int              max,
                                         ERL_NIF_TERM     eVal);
#endif
static ERL_NIF_TERM esock_setopt_bool_opt(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              level,
                                          int              opt,
                                          ERL_NIF_TERM     eVal);
static ERL_NIF_TERM esock_setopt_int_opt(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt,
                                         ERL_NIF_TERM     eVal);
#if (defined(SO_RCVTIMEO) || defined(SO_SNDTIMEO)) \
    && defined(ESOCK_USE_RCVSNDTIMEO)
static ERL_NIF_TERM esock_setopt_timeval_opt(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             int              level,
                                             int              opt,
                                             ERL_NIF_TERM     eVal);
#endif
static ERL_NIF_TERM esock_setopt_level_opt(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           int              level,
                                           int              opt,
                                           void*            optVal,
                                           socklen_t        optLen);

#if defined(USE_GETOPT_STR_OPT)
static ERL_NIF_TERM esock_getopt_str_opt(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt,
                                         int              max,
                                         BOOLEAN_T        stripNUL);
#endif
static ERL_NIF_TERM esock_getopt_bool_opt(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              level,
                                          int              opt);
static ERL_NIF_TERM esock_getopt_int_opt(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt);
static BOOLEAN_T esock_getopt_int(SOCKET sock,
                                  int    level,
                                  int    opt,
                                  int*   valP);
static ERL_NIF_TERM esock_getopt_size_opt(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              level,
                                          int              opt,
                                          SOCKOPTLEN_T     valueSz);
static ERL_NIF_TERM esock_getopt_bin_opt(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt,
                                         ErlNifBinary*    binP);
#if (defined(SO_RCVTIMEO) || defined(SO_SNDTIMEO)) \
    && defined(ESOCK_USE_RCVSNDTIMEO)
static ERL_NIF_TERM esock_getopt_timeval_opt(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             int              level,
                                             int              opt);
#endif



/* ------------------------------------------------------------------------
 * Socket option tables and handling
 */

struct ESockOpt
{
    int opt; // Option number

    // Function to set option
    ERL_NIF_TERM (*setopt)
    (ErlNifEnv *env, ESockDescriptor *descP,
     int level, int opt, ERL_NIF_TERM eVal);

    // Function to get option
    ERL_NIF_TERM (*getopt)
    (ErlNifEnv *env, ESockDescriptor *descP,
     int level, int opt);

    ERL_NIF_TERM *nameP; // Pointer to option name atom
};

// qsort and bsearch helper
static int cmpESockOpt(const void *vpa, const void *vpb) {
    struct ESockOpt *a, *b;
    a = (struct ESockOpt *) vpa;
    b = (struct ESockOpt *) vpb;
    return a->opt < b->opt ? -1 : (a->opt > b->opt ? 1 : 0);
}


/* SO_* options -------------------------------------------------------- */

static struct ESockOpt optLevelSocket[] =
    {
        {
#ifdef SO_ACCEPTCONN
            SO_ACCEPTCONN,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_acceptconn},

        {0, NULL, NULL, &esock_atom_acceptfilter},

        {
#ifdef SO_BINDTODEVICE
            SO_BINDTODEVICE,
            esock_setopt_so_bindtodevice, esock_getopt_so_bindtodevice,
#else
            0, NULL, NULL,
#endif
            &esock_atom_bindtodevice},

        {
#ifdef SO_BROADCAST
            SO_BROADCAST,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_broadcast},

        {0, NULL, NULL, &esock_atom_busy_poll},

        {
#ifdef SO_DEBUG
            SO_DEBUG,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_debug},

        {
#ifdef SO_DOMAIN
            SO_DOMAIN,
            NULL, esock_getopt_sock_domain,
#else
            0, NULL, NULL,
#endif
            &esock_atom_domain},

        {
#ifdef SO_DONTROUTE
            SO_DONTROUTE,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_dontroute},

        {0, NULL, NULL, &esock_atom_error},

        {
#ifdef SO_KEEPALIVE
            SO_KEEPALIVE,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_keepalive},

        {
#ifdef SO_LINGER
            SO_LINGER,
            esock_setopt_linger, esock_getopt_linger,
#else
            0, NULL, NULL,
#endif
            &esock_atom_linger},

        {0, NULL, NULL, &esock_atom_mark},

        {
#ifdef SO_OOBINLINE
            SO_OOBINLINE,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_oobinline},

        {
#ifdef SO_PASSCRED
            SO_PASSCRED,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_passcred},

        {
#ifdef SO_PEEK_OFF
            SO_PEEK_OFF,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_peek_off},

        {0, NULL, NULL, &esock_atom_peercred},

        {
#ifdef SO_PRIORITY
            SO_PRIORITY,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_priority},

        {
#ifdef SO_PROTOCOL
            SO_PROTOCOL,
            NULL, esock_getopt_sock_protocol,
#else
            0, NULL, NULL,
#endif
            &esock_atom_protocol},

        {
#ifdef SO_RCVBUF
            SO_RCVBUF,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_rcvbuf},

        {0, NULL, NULL, &esock_atom_rcvbufforce},

        {
#ifdef SO_RCVLOWAT
            SO_RCVLOWAT,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_rcvlowat},

        {
#if defined(SO_RCVTIMEO) && defined(ESOCK_USE_RCVSNDTIMEO)
            SO_RCVTIMEO,
            esock_setopt_timeval_opt, esock_getopt_timeval_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_rcvtimeo},

        {
#ifdef SO_REUSEADDR
            SO_REUSEADDR,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_reuseaddr},

        {
#ifdef SO_REUSEPORT
            SO_REUSEPORT,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_reuseport},

        {0, NULL, NULL, &esock_atom_rxq_ovfl},
        {0, NULL, NULL, &esock_atom_setfib},

        {
#ifdef SO_SNDBUF
            SO_SNDBUF,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_sndbuf},

        {0, NULL, NULL, &esock_atom_sndbufforce},

        {
#ifdef SO_SNDLOWAT
            SO_SNDLOWAT,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_sndlowat},

        {
#if defined(SO_SNDTIMEO) && defined(ESOCK_USE_RCVSNDTIMEO)
            SO_SNDTIMEO,
            esock_setopt_timeval_opt, esock_getopt_timeval_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_sndtimeo},

        {
#ifdef SO_TIMESTAMP
            SO_TIMESTAMP,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_timestamp},

        {
#ifdef SO_TYPE
            SO_TYPE,
            NULL, esock_getopt_sock_type,
#else
            0, NULL, NULL,
#endif
            &esock_atom_type}
    };


/* IP_* options -------------------------------------------------------- */

static struct ESockOpt optLevelIP[] =
    {
        {
#ifdef IP_ADD_MEMBERSHIP
            IP_ADD_MEMBERSHIP,
            esock_setopt_in_update_membership, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_add_membership},

        {
#ifdef IP_ADD_SOURCE_MEMBERSHIP
            IP_ADD_SOURCE_MEMBERSHIP,
            esock_setopt_in_update_source, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_add_source_membership},

        {
#ifdef IP_BLOCK_SOURCE
            IP_BLOCK_SOURCE,
            esock_setopt_in_update_source, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_block_source},

        {0, NULL, NULL, &esock_atom_dontfrag},

        {
#ifdef IP_DROP_MEMBERSHIP
            IP_DROP_MEMBERSHIP,
            esock_setopt_in_update_membership, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_drop_membership},

        {
#ifdef IP_DROP_SOURCE_MEMBERSHIP
            IP_DROP_SOURCE_MEMBERSHIP,
            esock_setopt_in_update_source, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_drop_source_membership},

        {
#ifdef IP_FREEBIND
            IP_FREEBIND,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_freebind},

        {
#ifdef IP_HDRINCL
            IP_HDRINCL,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_hdrincl},

        {
#ifdef IP_MINTTL
            IP_MINTTL,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_minttl},

        {
#if defined(IP_MSFILTER) && defined(IP_MSFILTER_SIZE)
            IP_MSFILTER,
            esock_setopt_msfilter, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_msfilter},

        {
#ifdef IP_MTU
            IP_MTU,
            NULL, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_mtu},

        {
#ifdef IP_MTU_DISCOVER
            IP_MTU_DISCOVER,
            esock_setopt_ip_mtu_discover, esock_getopt_ip_mtu_discover,
#else
            0, NULL, NULL,
#endif
            &esock_atom_mtu_discover},

        {
#ifdef IP_MULTICAST_ALL
            IP_MULTICAST_ALL,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_multicast_all},

        {
#ifdef IP_MULTICAST_IF
            IP_MULTICAST_IF,
            esock_setopt_multicast_if, esock_getopt_multicast_if,
#else
            0, NULL, NULL,
#endif
            &esock_atom_multicast_if},

        {
#ifdef IP_MULTICAST_LOOP
            IP_MULTICAST_LOOP,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_multicast_loop},

        {
#ifdef IP_MULTICAST_TTL
            IP_MULTICAST_TTL,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_multicast_ttl},

        {
#ifdef IP_NODEFRAG
            IP_NODEFRAG,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_nodefrag},

        {0, NULL, NULL, &esock_atom_options},

        {
#ifdef IP_PKTINFO
            IP_PKTINFO,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_pktinfo},

        {
#ifdef IP_PKTOPTIONS
            IP_PKTOPTIONS,
            NULL, esock_getopt_pktoptions,
#else
            0, NULL, NULL,
#endif
            &esock_atom_pktoptions},

        {
#ifdef IP_RECVDSTADDR
            IP_RECVDSTADDR,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvdstaddr},

        {
#ifdef IP_RECVERR
            IP_RECVERR,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recverr},

        {
#ifdef IP_RECVIF
            IP_RECVIF,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvif},

        {
#ifdef IP_RECVOPTS
            IP_RECVOPTS,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvopts},

        {
#ifdef IP_RECVORIGDSTADDR
            IP_RECVORIGDSTADDR,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvorigdstaddr},

        {
#ifdef IP_RECVTOS
            IP_RECVTOS,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvtos},

        {
#ifdef IP_RECVTTL
            IP_RECVTTL,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvttl},

        {
#ifdef IP_RETOPTS
            IP_RETOPTS,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_retopts},

        {
#ifdef IP_ROUTER_ALERT
            IP_ROUTER_ALERT,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_router_alert},

        {
#ifdef IP_SENDSRCADDR
            IP_SENDSRCADDR,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_sendsrcaddr},

        {
#ifdef IP_TOS
            IP_TOS,
            esock_setopt_tos, esock_getopt_tos,
#else
            0, NULL, NULL,
#endif
            &esock_atom_tos},

        {
#ifdef IP_TRANSPARENT
            IP_TRANSPARENT,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_transparent},

        {
#ifdef IP_TTL
            IP_TTL,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_ttl},

        {
#ifdef IP_UNBLOCK_SOURCE
            IP_UNBLOCK_SOURCE,
            esock_setopt_in_update_source, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_unblock_source}

    };

/* IPV6_* options ------------------------------------------------------ */

#ifdef HAVE_IPV6
static struct ESockOpt optLevelIPV6[] =
    {

        {
#ifdef IPV6_ADDRFORM
            IPV6_ADDRFORM,
            esock_setopt_addrform, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_addrform},

        {
#ifdef IPV6_ADD_MEMBERSHIP
            IPV6_ADD_MEMBERSHIP,
            esock_setopt_in6_update_membership, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_add_membership},

        {
#ifdef IPV6_AUTHHDR
            IPV6_AUTHHDR,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_authhdr},

        {0, NULL, NULL, &esock_atom_auth_level},
        {0, NULL, NULL, &esock_atom_checksum},

        {
#ifdef IPV6_DROP_MEMBERSHIP
            IPV6_DROP_MEMBERSHIP,
            esock_setopt_in6_update_membership, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_drop_membership},

        {
#if defined(IPV6_DSTOPTS)
            IPV6_DSTOPTS,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_dstopts},

        {0, NULL, NULL, &esock_atom_esp_network_level},
        {0, NULL, NULL, &esock_atom_esp_trans_level},
        {0, NULL, NULL, &esock_atom_faith},

        {
#ifdef IPV6_FLOWINFO
            IPV6_FLOWINFO,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_flowinfo},

        {
#ifdef IPV6_HOPLIMIT
            IPV6_HOPLIMIT,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_hoplimit},

        {
#ifdef IPV6_HOPOPTS
            IPV6_HOPOPTS,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_hopopts},

        {0, NULL, NULL, &esock_atom_ipcomp_level},
        {0, NULL, NULL, &esock_atom_join_group},
        {0, NULL, NULL, &esock_atom_leave_group},

        {
#ifdef IPV6_MTU
            IPV6_MTU,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_mtu},

        {
#ifdef IPV6_MTU_DISCOVER
            IPV6_MTU_DISCOVER,
            esock_setopt_ipv6_mtu_discover, esock_getopt_ipv6_mtu_discover,
#else
            0, NULL, NULL,
#endif
            &esock_atom_mtu_discover},

        {
#ifdef IPV6_MULTICAST_HOPS
            IPV6_MULTICAST_HOPS,
            esock_setopt_hops, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_multicast_hops},

        {
#ifdef IPV6_MULTICAST_IF
            IPV6_MULTICAST_IF,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_multicast_if},

        {
#ifdef IPV6_MULTICAST_LOOP
            IPV6_MULTICAST_LOOP,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_multicast_loop},

        {0, NULL, NULL, &esock_atom_portrange},

        {
#ifdef IPV6_PKTOPTIONS
            IPV6_PKTOPTIONS,
            NULL, esock_getopt_pktoptions,
#else
            0, NULL, NULL,
#endif
            &esock_atom_pktoptions},

        {
#ifdef IPV6_RECVERR
            IPV6_RECVERR,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recverr},

        {
#ifdef IPV6_RECVHOPLIMIT
            IPV6_RECVHOPLIMIT,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvhoplimit},

        {
#if defined(IPV6_RECVPKTINFO) || defined(IPV6_PKTINFO)
#if defined(IPV6_RECVPKTINFO)
                 IPV6_RECVPKTINFO,
#else
                 IPV6_PKTINFO,
#endif
                 esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvpktinfo},

        {
#ifdef IPV6_RECVTCLASS
            IPV6_RECVTCLASS,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_recvtclass},

        {
#ifdef IPV6_ROUTER_ALERT
            IPV6_ROUTER_ALERT,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_router_alert},

        {
#ifdef IPV6_RTHDR
            IPV6_RTHDR,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_rthdr},

        {
#ifdef IPV6_TCLASS
            IPV6_TCLASS,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_tclass},

        {
#ifdef IPV6_UNICAST_HOPS
            IPV6_UNICAST_HOPS,
            esock_setopt_hops, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_unicast_hops},

        {0, NULL, NULL, &esock_atom_use_min_mtu},

        {
#ifdef IPV6_V6ONLY
            IPV6_V6ONLY,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_v6only}

        };
#endif // #ifdef HAVE_IPV6


/* SCTP_* options ------------------------------------------------------ */

#ifdef HAVE_SCTP
static struct ESockOpt optLevelSCTP[] =
    {

        {0, NULL, NULL, &esock_atom_adaption_layer},

        {
#ifdef SCTP_ASSOCINFO
            SCTP_ASSOCINFO,
            esock_setopt_sctp_associnfo, esock_getopt_sctp_associnfo,
#else
            0, NULL, NULL,
#endif
            &esock_atom_associnfo},

        {0, NULL, NULL, &esock_atom_auth_active_key},
        {0, NULL, NULL, &esock_atom_auth_chunk},
        {0, NULL, NULL, &esock_atom_auth_delete_key},
        {0, NULL, NULL, &esock_atom_auth_key},

        {
#ifdef SCTP_AUTOCLOSE
            SCTP_AUTOCLOSE,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_autoclose},

        {0, NULL, NULL, &esock_atom_context},
        {0, NULL, NULL, &esock_atom_default_send_params},
        {0, NULL, NULL, &esock_atom_delayed_ack_time},

        {
#ifdef SCTP_DISABLE_FRAGMENTS
            SCTP_DISABLE_FRAGMENTS,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_disable_fragments},

        {0, NULL, NULL, &esock_atom_hmac_ident},

        {
#ifdef SCTP_EVENTS
            SCTP_EVENTS,
            esock_setopt_sctp_events, NULL,
#else
            0, NULL, NULL,
#endif
            &esock_atom_events},

        {0, NULL, NULL, &esock_atom_explicit_eor},
        {0, NULL, NULL, &esock_atom_fragment_interleave},
        {0, NULL, NULL, &esock_atom_get_peer_addr_info},

        {
#ifdef SCTP_INITMSG
            SCTP_INITMSG,
            esock_setopt_sctp_initmsg, esock_getopt_sctp_initmsg,
#else
            0, NULL, NULL,
#endif
            &esock_atom_initmsg},

        {0, NULL, NULL, &esock_atom_i_want_mapped_v4_addr},
        {0, NULL, NULL, &esock_atom_local_auth_chunks},

        {
#ifdef SCTP_MAXSEG
            SCTP_MAXSEG,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_maxseg},

        {0, NULL, NULL, &esock_atom_maxburst},

        {
#ifdef SCTP_NODELAY
            SCTP_NODELAY,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_nodelay},

        {0, NULL, NULL, &esock_atom_partial_delivery_point},
        {0, NULL, NULL, &esock_atom_peer_addr_params},
        {0, NULL, NULL, &esock_atom_peer_auth_chunks},
        {0, NULL, NULL, &esock_atom_primary_addr},
        {0, NULL, NULL, &esock_atom_reset_streams},

        {
#ifdef SCTP_RTOINFO
            SCTP_RTOINFO,
            esock_setopt_sctp_rtoinfo, esock_getopt_sctp_rtoinfo,
#else
            0, NULL, NULL,
#endif
            &esock_atom_rtoinfo},

        {0, NULL, NULL, &esock_atom_set_peer_primary_addr},
        {0, NULL, NULL, &esock_atom_status},
        {0, NULL, NULL, &esock_atom_use_ext_recvinfo}

    };
#endif // #ifdef HAVE_SCTP

/* TCP_* options ------------------------------------------------------- */

static struct ESockOpt optLevelTCP[] =
    {

        {
#ifdef TCP_CONGESTION
            TCP_CONGESTION,
            esock_setopt_tcp_congestion, esock_getopt_tcp_congestion,
#else
            0, NULL, NULL,
#endif
            &esock_atom_congestion},

        {
#ifdef TCP_CORK
            TCP_CORK,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_cork},

        {0, NULL, NULL, &esock_atom_info},
        {0, NULL, NULL, &esock_atom_keepcnt},
        {0, NULL, NULL, &esock_atom_keepidle},
        {0, NULL, NULL, &esock_atom_keepintvl},

        {
#ifdef TCP_MAXSEG
            TCP_MAXSEG,
            esock_setopt_int_opt, esock_getopt_int_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_maxseg},

        {0, NULL, NULL, &esock_atom_md5sig},

        {
#ifdef TCP_NODELAY
            TCP_NODELAY,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_nodelay},

        {0, NULL, NULL, &esock_atom_noopt},
        {0, NULL, NULL, &esock_atom_nopush},
        {0, NULL, NULL, &esock_atom_syncnt},
        {0, NULL, NULL, &esock_atom_user_timeout}

    };


/* UDP_* options ------------------------------------------------------- */

static struct ESockOpt optLevelUDP[] =
    {

        {
#ifdef UDP_CORK
            UDP_CORK,
            esock_setopt_bool_opt, esock_getopt_bool_opt,
#else
            0, NULL, NULL,
#endif
            &esock_atom_cork}

    };

/* Option levels table ------------------------------------------------- */

struct ESockOptLevel
{
    int level; // Level number

    size_t num; // Number of options

    struct ESockOpt *opts; // Options table
};

// qsort and bsearch helper
static int cmpESockOptLevel(const void *vpa, const void *vpb) {
    struct ESockOptLevel *a, *b;
    a = (struct ESockOptLevel*) vpa;
    b = (struct ESockOptLevel*) vpb;
    return a->level < b->level ? -1 : (a->level > b->level ? 1 : 0);
}

#define OPT_LEVEL(Level, Opts) {(Level), NUM(Opts), (Opts)}

/* Table --------------------------------------------------------------- */

static struct ESockOptLevel optLevels[] =
    {
        OPT_LEVEL(SOL_SOCKET, optLevelSocket),

#ifdef SOL_IP
        OPT_LEVEL(SOL_IP, optLevelIP),
#else
        OPT_LEVEL(IPPROTO_IP, optLevelIP),
#endif

#ifdef HAVE_IPV6
#ifdef SOL_IPV6
        OPT_LEVEL(SOL_IPV6, optLevelIPV6),
#else
        OPT_LEVEL(IPPROTO_IPV6, optLevelIPV6),
#endif
#endif // #ifdef HAVE_IPV6

#ifdef HAVE_SCTP
        OPT_LEVEL(IPPROTO_SCTP, optLevelSCTP),
#endif // #ifdef HAVE_SCTP

        OPT_LEVEL(IPPROTO_UDP, optLevelUDP),
        OPT_LEVEL(IPPROTO_TCP, optLevelTCP)
    };

#undef OPT_LEVEL

/* Tables init (sorting) ----------------------------------------------- */

#define ESOCK_SORT_TABLE(Array, Cmp)                            \
    qsort((Array), NUM(Array), sizeof(*(Array)), (Cmp))

static void initOpts(void) {
    ESOCK_SORT_TABLE(optLevelSocket, cmpESockOpt);
    ESOCK_SORT_TABLE(optLevelIP, cmpESockOpt);
#ifdef HAVE_IPV6
    ESOCK_SORT_TABLE(optLevelIPV6, cmpESockOpt);
#endif
#ifdef HAVE_SCTP
    ESOCK_SORT_TABLE(optLevelSCTP, cmpESockOpt);
#endif
    ESOCK_SORT_TABLE(optLevelTCP, cmpESockOpt);
    ESOCK_SORT_TABLE(optLevelUDP, cmpESockOpt);
    ESOCK_SORT_TABLE(optLevels, cmpESockOptLevel);
}

/* Option lookup in tables --------------------------------------------- */

static struct ESockOpt *lookupOpt(int level, int opt) {
    struct ESockOptLevel levelKey, *levelP;
    struct ESockOpt optKey;

    sys_memzero((char *) &levelKey, sizeof(levelKey));
    levelKey.level = level;
    levelP =
        bsearch(&levelKey, optLevels, NUM(optLevels), sizeof(*optLevels),
                cmpESockOptLevel);
    if (levelP == NULL)
        return NULL;

    sys_memzero((char *) &optKey, sizeof(optKey));
    optKey.opt = opt;
    return
        bsearch(&optKey, levelP->opts, levelP->num, sizeof(*levelP->opts),
                cmpESockOpt);
}

/* --------------------------------------------------------------------- */



static BOOLEAN_T send_check_writer(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     ref,
                                   ERL_NIF_TERM*    checkResult);
static ERL_NIF_TERM send_check_result(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ssize_t          send_result,
                                      ssize_t          dataSize,
                                      BOOLEAN_T        dataInTail,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     sendRef);
static ERL_NIF_TERM send_check_ok(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ssize_t          written,
                                  ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM send_check_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              saveErrno,
                                    ERL_NIF_TERM     sockRef);
static void send_error_waiting_writers(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     reason);
static ERL_NIF_TERM send_check_retry(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ssize_t          written,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     sendRef);
static BOOLEAN_T recv_check_reader(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     ref,
                                   ERL_NIF_TERM*    checkResult);
static void recv_init_current_reader(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     ref);
static void recv_update_current_reader(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef);
static void recv_error_current_reader(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     reason);
static ERL_NIF_TERM recv_check_result(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ssize_t          read,
                                      ssize_t          toRead,
                                      int              saveErrno,
                                      ErlNifBinary*    bufP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_full(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ssize_t          read,
                                    ssize_t          toRead,
                                    ErlNifBinary*    bufP,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_full_maybe_done(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ssize_t          read,
                                               ErlNifBinary*    bufP,
                                               ERL_NIF_TERM     sockRef,
                                               ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_full_done(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ssize_t          read,
                                         ErlNifBinary*    bufP,
                                         ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recv_check_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              saveErrno,
                                    ErlNifBinary*    buf1P,
                                    ErlNifBinary*    buf2P,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_fail_econnreset(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     sockRef,
                                               ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_partial(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ssize_t          read,
                                       ssize_t          toRead,
                                       ErlNifBinary*    bufP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_partial_done(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ssize_t          read,
                                            ErlNifBinary*    bufP,
                                            ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recv_check_partial_part(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ssize_t          read,
                                            ErlNifBinary*    bufP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_retry(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_fail_gen(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              saveErrno,
                                        ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recvfrom_check_result(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ssize_t          read,
                                          int              saveErrno,
                                          ErlNifBinary*    bufP,
                                          ESockAddress*    fromAddrP,
                                          SOCKLEN_T        fromAddrLen,
                                          ERL_NIF_TERM     sockRef,
                                          ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recvmsg_check_result(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ssize_t          read,
                                         int              saveErrno,
                                         struct msghdr*   msgHdrP,
                                         ErlNifBinary*    dataBufP,
                                         ErlNifBinary*    ctrlBufP,
                                         ERL_NIF_TERM     sockRef,
                                         ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recvmsg_check_msg(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ssize_t          read,
                                      struct msghdr*   msgHdrP,
                                      ErlNifBinary*    dataBufP,
                                      ErlNifBinary*    ctrlBufP,
                                      ERL_NIF_TERM     sockRef);

static ERL_NIF_TERM esock_finalize_close(ErlNifEnv*       env,
                                         ESockDescriptor* descP);
static int esock_close_socket(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              BOOLEAN_T        unlock);

static void encode_msg(ErlNifEnv*       env,
                       ESockDescriptor* descP,
                       ssize_t          read,
                       struct msghdr*   msgHdrP,
                       ErlNifBinary*    dataBufP,
                       ErlNifBinary*    ctrlBufP,
                       ERL_NIF_TERM*    eSockAddr);
static void encode_cmsgs(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ErlNifBinary*    cmsgBinP,
                         struct msghdr*   msgHdrP,
                         ERL_NIF_TERM*    eCMsg);
static BOOLEAN_T encode_cmsg(ErlNifEnv*     env,
                             int            level,
                             int            type,
                             unsigned char* dataP,
                             size_t         dataLen,
                             ERL_NIF_TERM*  eType,
                             ERL_NIF_TERM*  eData);
static BOOLEAN_T decode_cmsghdrs(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     eCMsg,
                                 char*            cmsgHdrBufP,
                                 size_t           cmsgHdrBufLen,
                                 size_t*          cmsgHdrBufUsed);
static BOOLEAN_T decode_cmsghdr(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     eCMsg,
                                char*            bufP,
                                size_t           rem,
                                size_t*          used);
static BOOLEAN_T decode_cmsghdr_value(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      ERL_NIF_TERM     eType,
                                      ERL_NIF_TERM     eValue,
                                      char*            dataP,
                                      size_t           dataLen,
                                      size_t*          dataUsedP);
static BOOLEAN_T decode_cmsghdr_data(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              level,
                                     ERL_NIF_TERM     eType,
                                     ERL_NIF_TERM     eData,
                                     char*            dataP,
                                     size_t           dataLen,
                                     size_t*          dataUsedP);
static void *init_cmsghdr(struct cmsghdr* cmsgP,
                          size_t          rem,
                          size_t          size,
                          size_t*         usedP);
static void encode_msg_flags(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              msgFlags,
                             ERL_NIF_TERM*    flags);
#if defined(IP_TOS)
static BOOLEAN_T decode_ip_tos(ErlNifEnv*   env,
                               ERL_NIF_TERM eVal,
                               int*         val);
#endif
#if defined(IP_MTU_DISCOVER)
static BOOLEAN_T decode_ip_pmtudisc(ErlNifEnv*   env,
                                    ERL_NIF_TERM eVal,
                                    int*         val);
#endif
#if defined(IP_MTU_DISCOVER)
static void encode_ip_pmtudisc(ErlNifEnv*    env,
                               int           val,
                               ERL_NIF_TERM* eVal);
#endif
#if defined(IPV6_MTU_DISCOVER)
static BOOLEAN_T decode_ipv6_pmtudisc(ErlNifEnv*   env,
                                      ERL_NIF_TERM eVal,
                                      int*         val);
#endif
#if defined(IPV6_MTU_DISCOVER)
static void encode_ipv6_pmtudisc(ErlNifEnv*    env,
                                 int           val,
                                 ERL_NIF_TERM* eVal);
#endif
#if defined(IPV6_MULTICAST_HOPS) || defined(IPV6_UNICAST_HOPS)
static
BOOLEAN_T decode_hops(ErlNifEnv *env, ERL_NIF_TERM eVal, int *val);
#endif

/*
static BOOLEAN_T decode_bool(ErlNifEnv*   env,
                             ERL_NIF_TERM eVal,
                             BOOLEAN_T*   val);
*/
// static void encode_bool(BOOLEAN_T val, ERL_NIF_TERM* eVal);
static ERL_NIF_TERM encode_ip_tos(ErlNifEnv* env, int val);

#if defined(SCTP_ASSOCINFO) || defined(SCTP_RTOINOFO)
static BOOLEAN_T decode_sctp_assoc_t(ErlNifEnv*    env,
                                     ERL_NIF_TERM  eVal,
                                     sctp_assoc_t* val);
static ERL_NIF_TERM encode_sctp_assoc_t(ErlNifEnv* env,
                                        sctp_assoc_t val);
#endif // #if defined(SCTP_ASSOCINFO) || defined(SCTP_RTOINOFO)

static void esock_stop_handle_current(ErlNifEnv*       env,
                                      const char*      role,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ESockRequestor*  reqP);
static void inform_waiting_procs(ErlNifEnv*         env,
                                 const char*        role,
                                 ESockDescriptor*   descP,
                                 ERL_NIF_TERM       sockRef,
                                 ESockRequestQueue* q,
                                 ERL_NIF_TERM       reason);

static int socket_setopt(int             sock,
                         int             level,
                         int             opt,
                         const void*     optVal,
                         const socklen_t optLen);

static BOOLEAN_T verify_is_connected(ESockDescriptor* descP, int* err);

static ESockDescriptor* alloc_descriptor(SOCKET sock, ErlNifEvent event);


static BOOLEAN_T ehow2how(ERL_NIF_TERM ehow, int* how);
#ifdef HAVE_SETNS
static BOOLEAN_T esock_open4_get_netns(ErlNifEnv*   env,
                                       ERL_NIF_TERM opts,
                                       char**       netns);
static BOOLEAN_T change_network_namespace(char* netns, int* cns, int* err);
static BOOLEAN_T restore_network_namespace(int ns, SOCKET sock, int* err);
#endif

static BOOLEAN_T cnt_inc(ESockCounter* cnt, ESockCounter inc);
static void      cnt_dec(ESockCounter* cnt, ESockCounter dec);

static void inc_socket(int domain, int type, int protocol);
static void dec_socket(int domain, int type, int protocol);



/* *** activate_next_acceptor ***
 * *** activate_next_writer   ***
 * *** activate_next_reader   ***
 *
 * All the activate-next functions for acceptor, writer and reader
 * have exactly the same API, so we apply some macro magic to simplify.
 * They simply operates on dufferent data structures.
 *
 */

#define ACTIVATE_NEXT_FUNCS_DEFS     \
    ACTIVATE_NEXT_FUNC_DEF(acceptor) \
    ACTIVATE_NEXT_FUNC_DEF(writer)   \
    ACTIVATE_NEXT_FUNC_DEF(reader)

#define ACTIVATE_NEXT_FUNC_DEF(F)                                 \
    static BOOLEAN_T activate_next_##F(ErlNifEnv*       env,      \
                                       ESockDescriptor* descP,    \
                                       ERL_NIF_TERM     sockRef);
ACTIVATE_NEXT_FUNCS_DEFS
#undef ACTIVATE_NEXT_FUNC_DEF

/* *** acceptor_search4pid | writer_search4pid | reader_search4pid ***
 * *** acceptor_push       | writer_push       | reader_push       ***
 * *** acceptor_pop        | writer_pop        | reader_pop        ***
 * *** acceptor_unqueue    | writer_unqueue    | reader_unqueue    ***
 *
 * All the queue operator functions (search4pid, push, pop
 * and unqueue) for acceptor, writer and reader has exactly
 * the same API, so we apply some macro magic to simplify.
 */

#define ESOCK_OPERATOR_FUNCS_DEFS      \
    ESOCK_OPERATOR_FUNCS_DEF(acceptor) \
    ESOCK_OPERATOR_FUNCS_DEF(writer)   \
    ESOCK_OPERATOR_FUNCS_DEF(reader)

#define ESOCK_OPERATOR_FUNCS_DEF(O)                            \
    static BOOLEAN_T O##_search4pid(ErlNifEnv*       env,      \
                                    ESockDescriptor* descP,    \
                                    ErlNifPid*       pid);     \
    static void O##_push(ErlNifEnv*       env,         \
                         ESockDescriptor* descP,               \
                         ErlNifPid        pid,                 \
                         ERL_NIF_TERM     ref);                \
    static BOOLEAN_T O##_pop(ErlNifEnv*       env,             \
                             ESockDescriptor* descP,           \
                             ESockRequestor*  reqP);           \
    static BOOLEAN_T O##_unqueue(ErlNifEnv*       env,         \
                                 ESockDescriptor* descP,       \
                                 ERL_NIF_TERM*    refP,        \
                                 const ErlNifPid* pidP);
ESOCK_OPERATOR_FUNCS_DEFS
#undef ESOCK_OPERATOR_FUNCS_DEF

static BOOLEAN_T requestor_pop(ESockRequestQueue* q,
                               ESockRequestor*    reqP);

static void requestor_init(ESockRequestor* reqP);
static void requestor_release(const char*      slogan,
                              ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ESockRequestor*  reqP);

static BOOLEAN_T qsearch4pid(ErlNifEnv*         env,
                             ESockRequestQueue* q,
                             ErlNifPid*         pid);
static void qpush(ESockRequestQueue*        q,
                  ESockRequestQueueElement* e);
static ESockRequestQueueElement* qpop(ESockRequestQueue* q);
static BOOLEAN_T qunqueue(ErlNifEnv*         env,
                          ESockDescriptor*   descP,
                          const char*        slogan,
                          ESockRequestQueue* q,
                          ERL_NIF_TERM*      refP,
                          const ErlNifPid*   pidP);

static int esock_monitor(const char*      slogan,
                         ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         const ErlNifPid* pid,
                         ESockMonitor*    mon);
static int esock_demonitor(const char*      slogan,
                           ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ESockMonitor*    monP);
static void esock_monitor_init(ESockMonitor* mon);
static ERL_NIF_TERM esock_make_monitor_term(ErlNifEnv*          env,
                                            const ESockMonitor* monP);
static BOOLEAN_T esock_monitor_eq(const ESockMonitor* monP,
                                  const ErlNifMonitor* mon);



static void esock_down_ctrl(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            const ErlNifPid* pidP);
static void esock_down_acceptor(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                const ErlNifPid* pidP,
                                const ErlNifMonitor* monP);
static void esock_down_writer(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     sockRef,
                              const ErlNifPid* pidP,
                              const ErlNifMonitor* monP);
static void esock_down_reader(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     sockRef,
                              const ErlNifPid* pidP,
                              const ErlNifMonitor* monP);

static void esock_send_reg_add_msg(ErlNifEnv*   env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM sockRef);
static void esock_send_reg_del_msg(ErlNifEnv*   env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM sockRef);

static void esock_send_wrap_msg(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     cnt);
static void esock_send_close_msg(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ErlNifPid*       pid);
#ifdef HAVE_SENDFILE
static void
esock_send_sendfile_deferred_close_msg(ErlNifEnv*       env,
                                       ESockDescriptor* descP);
#endif
static void esock_send_abort_msg(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ESockRequestor*  reqP,
                                 ERL_NIF_TERM     reason);
static BOOLEAN_T esock_send_msg(ErlNifEnv*   env,
                                ErlNifPid*   pid,
                                ERL_NIF_TERM msg,
                                ErlNifEnv*   msgEnv);

static ERL_NIF_TERM mk_reg_add_msg(ErlNifEnv*   env,
                                   ERL_NIF_TERM sockRef);
static ERL_NIF_TERM mk_reg_del_msg(ErlNifEnv*   env,
                                   ERL_NIF_TERM sockRef);
static ERL_NIF_TERM mk_reg_msg(ErlNifEnv*   env,
                               ERL_NIF_TERM tag,
                               ERL_NIF_TERM sockRef);
static ERL_NIF_TERM mk_abort_msg(ErlNifEnv*   env,
                                 ERL_NIF_TERM sockRef,
                                 ERL_NIF_TERM opRef,
                                 ERL_NIF_TERM reason);
static ERL_NIF_TERM mk_wrap_msg(ErlNifEnv*   env,
                                ERL_NIF_TERM sockRef,
                                ERL_NIF_TERM cnt);
static ERL_NIF_TERM mk_close_msg(ErlNifEnv*   env,
                                 ERL_NIF_TERM sockRef,
                                 ERL_NIF_TERM closeRef);
static ERL_NIF_TERM mk_select_msg(ErlNifEnv*   env,
                                  ERL_NIF_TERM sockRef,
                                  ERL_NIF_TERM selectRef);
static ERL_NIF_TERM mk_socket_msg(ErlNifEnv*   env,
                                  ERL_NIF_TERM sockRef,
                                  ERL_NIF_TERM tag,
                                  ERL_NIF_TERM info);
static ERL_NIF_TERM mk_socket(ErlNifEnv*   env,
                              ERL_NIF_TERM sockRef);

static int esock_select_read(ErlNifEnv*       env,
                             ErlNifEvent      event,
                             void*            obj,
                             const ErlNifPid* pidP,
                             ERL_NIF_TERM     sockRef,
                             ERL_NIF_TERM     selectRef);
static int esock_select_write(ErlNifEnv*       env,
                              ErlNifEvent      event,
                              void*            obj,
                              const ErlNifPid* pidP,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     selectRef);
static int esock_select_stop(ErlNifEnv*  env,
                             ErlNifEvent event,
                             void*       obj);
static int esock_select_cancel(ErlNifEnv*             env,
                               ErlNifEvent            event,
                               enum ErlNifSelectFlags mode,
                               void*                  obj);

static char* extract_debug_filename(ErlNifEnv*   env,
                                    ERL_NIF_TERM map);


/* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *
 *                                                                        *
 *                                                                        *
 * End of non-__WIN32__ section a.k.a UNIX section                        *
 *                                                                        *
 *                                                                        *
 * ---------------------------------------------------------------------- */
#endif // #ifndef __WIN32__


/*
#if defined(HAS_AF_LOCAL) || defined(SO_BINDTODEVICE)
static size_t my_strnlen(const char *s, size_t maxlen);
#endif
*/

static void esock_dtor(ErlNifEnv* env, void* obj);
static void esock_stop(ErlNifEnv* env,
                       void*      obj,
                       ErlNifEvent fd,
                       int        is_direct_call);
static void esock_down(ErlNifEnv*           env,
                       void*                obj,
                       const ErlNifPid*     pidP,
                       const ErlNifMonitor* monP);

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);



#if HAVE_IN6
#  if ! defined(HAVE_IN6ADDR_ANY) || ! HAVE_IN6ADDR_ANY
#    if HAVE_DECL_IN6ADDR_ANY_INIT
static const struct in6_addr in6addr_any = { { IN6ADDR_ANY_INIT } };
#    else
static const struct in6_addr in6addr_any =
    { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 } } };
#    endif /* HAVE_IN6ADDR_ANY_INIT */
#  endif /* ! HAVE_DECL_IN6ADDR_ANY */

#  if ! defined(HAVE_IN6ADDR_LOOPBACK) || ! HAVE_IN6ADDR_LOOPBACK
#    if HAVE_DECL_IN6ADDR_LOOPBACK_INIT
static const struct in6_addr in6addr_loopback =
    { { IN6ADDR_LOOPBACK_INIT } };
#    else
static const struct in6_addr in6addr_loopback =
    { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 } } };
#    endif /* HAVE_IN6ADDR_LOOPBACk_INIT */
#  endif /* ! HAVE_DECL_IN6ADDR_LOOPBACK */
#endif /* HAVE_IN6 */



/* *** Global atoms ***
 * Note that when an (global) atom is added here, it must also be added
 * in the socket_int.h file!
 */
#define GLOBAL_ATOMS                                   \
    GLOBAL_ATOM_DECL(abort);                           \
    GLOBAL_ATOM_DECL(accept);                          \
    GLOBAL_ATOM_DECL(acceptconn);                      \
    GLOBAL_ATOM_DECL(acceptfilter);                    \
    GLOBAL_ATOM_DECL(adaption_layer);                  \
    GLOBAL_ATOM_DECL(addr);                            \
    GLOBAL_ATOM_DECL(addrform);                        \
    GLOBAL_ATOM_DECL(add_membership);                  \
    GLOBAL_ATOM_DECL(add_source_membership);           \
    GLOBAL_ATOM_DECL(any);                             \
    GLOBAL_ATOM_DECL(associnfo);                       \
    GLOBAL_ATOM_DECL(authhdr);                         \
    GLOBAL_ATOM_DECL(auth_active_key);                 \
    GLOBAL_ATOM_DECL(auth_asconf);                     \
    GLOBAL_ATOM_DECL(auth_chunk);                      \
    GLOBAL_ATOM_DECL(auth_delete_key);                 \
    GLOBAL_ATOM_DECL(auth_key);                        \
    GLOBAL_ATOM_DECL(auth_level);                      \
    GLOBAL_ATOM_DECL(autoclose);                       \
    GLOBAL_ATOM_DECL(bad_data);                        \
    GLOBAL_ATOM_DECL(bindtodevice);                    \
    GLOBAL_ATOM_DECL(block_source);                    \
    GLOBAL_ATOM_DECL(broadcast);                       \
    GLOBAL_ATOM_DECL(busy_poll);                       \
    GLOBAL_ATOM_DECL(checksum);                        \
    GLOBAL_ATOM_DECL(close);                           \
    GLOBAL_ATOM_DECL(cmsg_cloexec);                    \
    GLOBAL_ATOM_DECL(command);                         \
    GLOBAL_ATOM_DECL(confirm);                         \
    GLOBAL_ATOM_DECL(congestion);                      \
    GLOBAL_ATOM_DECL(connect);                         \
    GLOBAL_ATOM_DECL(context);                         \
    GLOBAL_ATOM_DECL(cork);                            \
    GLOBAL_ATOM_DECL(credentials);                     \
    GLOBAL_ATOM_DECL(ctrl);                            \
    GLOBAL_ATOM_DECL(ctrunc);                          \
    GLOBAL_ATOM_DECL(data);                            \
    GLOBAL_ATOM_DECL(debug);                           \
    GLOBAL_ATOM_DECL(default);                         \
    GLOBAL_ATOM_DECL(default_send_params);             \
    GLOBAL_ATOM_DECL(delayed_ack_time);                \
    GLOBAL_ATOM_DECL(dgram);                           \
    GLOBAL_ATOM_DECL(disable_fragments);               \
    GLOBAL_ATOM_DECL(domain);                          \
    GLOBAL_ATOM_DECL(dontfrag);                        \
    GLOBAL_ATOM_DECL(dontroute);                       \
    GLOBAL_ATOM_DECL(drop_membership);                 \
    GLOBAL_ATOM_DECL(drop_source_membership);          \
    GLOBAL_ATOM_DECL(dstopts);                         \
    GLOBAL_ATOM_DECL(egp);                             \
    GLOBAL_ATOM_DECL(enotsup);                         \
    GLOBAL_ATOM_DECL(eor);                             \
    GLOBAL_ATOM_DECL(error);                           \
    GLOBAL_ATOM_DECL(errqueue);                        \
    GLOBAL_ATOM_DECL(esp_network_level);               \
    GLOBAL_ATOM_DECL(esp_trans_level);                 \
    GLOBAL_ATOM_DECL(events);                          \
    GLOBAL_ATOM_DECL(explicit_eor);                    \
    GLOBAL_ATOM_DECL(faith);                           \
    GLOBAL_ATOM_DECL(false);                           \
    GLOBAL_ATOM_DECL(family);                          \
    GLOBAL_ATOM_DECL(fastroute);                       \
    GLOBAL_ATOM_DECL(flags);                           \
    GLOBAL_ATOM_DECL(flowinfo);                        \
    GLOBAL_ATOM_DECL(fragment_interleave);             \
    GLOBAL_ATOM_DECL(freebind);                        \
    GLOBAL_ATOM_DECL(get_peer_addr_info);              \
    GLOBAL_ATOM_DECL(hatype);                          \
    GLOBAL_ATOM_DECL(hdrincl);                         \
    GLOBAL_ATOM_DECL(hmac_ident);                      \
    GLOBAL_ATOM_DECL(hoplimit);                        \
    GLOBAL_ATOM_DECL(hopopts);                         \
    GLOBAL_ATOM_DECL(host);                            \
    GLOBAL_ATOM_DECL(icmp);                            \
    GLOBAL_ATOM_DECL(icmp6);                           \
    GLOBAL_ATOM_DECL(ifindex);                         \
    GLOBAL_ATOM_DECL(igmp);                            \
    GLOBAL_ATOM_DECL(inet);                            \
    GLOBAL_ATOM_DECL(inet6);                           \
    GLOBAL_ATOM_DECL(info);                            \
    GLOBAL_ATOM_DECL(initmsg);                         \
    GLOBAL_ATOM_DECL(invalid);                         \
    GLOBAL_ATOM_DECL(integer_range);                   \
    GLOBAL_ATOM_DECL(iov);                             \
    GLOBAL_ATOM_DECL(ip);                              \
    GLOBAL_ATOM_DECL(ipcomp_level);                    \
    GLOBAL_ATOM_DECL(ipip);                            \
    GLOBAL_ATOM_DECL(ipv6);                            \
    GLOBAL_ATOM_DECL(i_want_mapped_v4_addr);           \
    GLOBAL_ATOM_DECL(join_group);                      \
    GLOBAL_ATOM_DECL(keepalive);                       \
    GLOBAL_ATOM_DECL(keepcnt);                         \
    GLOBAL_ATOM_DECL(keepidle);                        \
    GLOBAL_ATOM_DECL(keepintvl);                       \
    GLOBAL_ATOM_DECL(kernel);                          \
    GLOBAL_ATOM_DECL(leave_group);                     \
    GLOBAL_ATOM_DECL(level);                           \
    GLOBAL_ATOM_DECL(linger);                          \
    GLOBAL_ATOM_DECL(local);                           \
    GLOBAL_ATOM_DECL(local_auth_chunks);               \
    GLOBAL_ATOM_DECL(loopback);                        \
    GLOBAL_ATOM_DECL(lowdelay);                        \
    GLOBAL_ATOM_DECL(mark);                            \
    GLOBAL_ATOM_DECL(maxburst);                        \
    GLOBAL_ATOM_DECL(maxseg);                          \
    GLOBAL_ATOM_DECL(md5sig);                          \
    GLOBAL_ATOM_DECL(mincost);                         \
    GLOBAL_ATOM_DECL(minttl);                          \
    GLOBAL_ATOM_DECL(more);                            \
    GLOBAL_ATOM_DECL(msfilter);                        \
    GLOBAL_ATOM_DECL(mtu);                             \
    GLOBAL_ATOM_DECL(mtu_discover);                    \
    GLOBAL_ATOM_DECL(multicast);                       \
    GLOBAL_ATOM_DECL(multicast_all);                   \
    GLOBAL_ATOM_DECL(multicast_hops);                  \
    GLOBAL_ATOM_DECL(multicast_if);                    \
    GLOBAL_ATOM_DECL(multicast_loop);                  \
    GLOBAL_ATOM_DECL(multicast_ttl);                   \
    GLOBAL_ATOM_DECL(nodelay);                         \
    GLOBAL_ATOM_DECL(nodefrag);                        \
    GLOBAL_ATOM_DECL(noopt);                           \
    GLOBAL_ATOM_DECL(nopush);                          \
    GLOBAL_ATOM_DECL(nosignal);                        \
    GLOBAL_ATOM_DECL(not_found);                       \
    GLOBAL_ATOM_DECL(not_owner);                       \
    GLOBAL_ATOM_DECL(ok);                              \
    GLOBAL_ATOM_DECL(oob);                             \
    GLOBAL_ATOM_DECL(oobinline);                       \
    GLOBAL_ATOM_DECL(options);                         \
    GLOBAL_ATOM_DECL(origdstaddr);                     \
    GLOBAL_ATOM_DECL(otherhost);                       \
    GLOBAL_ATOM_DECL(outgoing);                        \
    GLOBAL_ATOM_DECL(packet);                          \
    GLOBAL_ATOM_DECL(partial_delivery_point);          \
    GLOBAL_ATOM_DECL(passcred);                        \
    GLOBAL_ATOM_DECL(path);                            \
    GLOBAL_ATOM_DECL(peek);                            \
    GLOBAL_ATOM_DECL(peek_off);                        \
    GLOBAL_ATOM_DECL(peer_addr_params);                \
    GLOBAL_ATOM_DECL(peer_auth_chunks);                \
    GLOBAL_ATOM_DECL(peercred);                        \
    GLOBAL_ATOM_DECL(pktinfo);                         \
    GLOBAL_ATOM_DECL(pktoptions);                      \
    GLOBAL_ATOM_DECL(pkttype);                         \
    GLOBAL_ATOM_DECL(port);                            \
    GLOBAL_ATOM_DECL(portrange);                       \
    GLOBAL_ATOM_DECL(primary_addr);                    \
    GLOBAL_ATOM_DECL(priority);                        \
    GLOBAL_ATOM_DECL(protocol);                        \
    GLOBAL_ATOM_DECL(raw);                             \
    GLOBAL_ATOM_DECL(rcvbuf);                          \
    GLOBAL_ATOM_DECL(rcvbufforce);                     \
    GLOBAL_ATOM_DECL(rcvlowat);                        \
    GLOBAL_ATOM_DECL(rcvtimeo);                        \
    GLOBAL_ATOM_DECL(rdm);                             \
    GLOBAL_ATOM_DECL(recv);                            \
    GLOBAL_ATOM_DECL(recvdstaddr);                     \
    GLOBAL_ATOM_DECL(recverr);                         \
    GLOBAL_ATOM_DECL(recvfrom);                        \
    GLOBAL_ATOM_DECL(recvhoplimit);                    \
    GLOBAL_ATOM_DECL(recvif);                          \
    GLOBAL_ATOM_DECL(recvmsg);                         \
    GLOBAL_ATOM_DECL(recvopts);                        \
    GLOBAL_ATOM_DECL(recvorigdstaddr);                 \
    GLOBAL_ATOM_DECL(recvpktinfo);                     \
    GLOBAL_ATOM_DECL(recvtclass);                      \
    GLOBAL_ATOM_DECL(recvtos);                         \
    GLOBAL_ATOM_DECL(recvttl);                         \
    GLOBAL_ATOM_DECL(reliability);                     \
    GLOBAL_ATOM_DECL(reset_streams);                   \
    GLOBAL_ATOM_DECL(retopts);                         \
    GLOBAL_ATOM_DECL(reuseaddr);                       \
    GLOBAL_ATOM_DECL(reuseport);                       \
    GLOBAL_ATOM_DECL(rights);                          \
    GLOBAL_ATOM_DECL(router_alert);                    \
    GLOBAL_ATOM_DECL(rthdr);                           \
    GLOBAL_ATOM_DECL(rtoinfo);                         \
    GLOBAL_ATOM_DECL(rxq_ovfl);                        \
    GLOBAL_ATOM_DECL(scope_id);                        \
    GLOBAL_ATOM_DECL(sctp);                            \
    GLOBAL_ATOM_DECL(sec);                             \
    GLOBAL_ATOM_DECL(select_failed);                   \
    GLOBAL_ATOM_DECL(select_sent);                     \
    GLOBAL_ATOM_DECL(send);                            \
    GLOBAL_ATOM_DECL(sendmsg);                         \
    GLOBAL_ATOM_DECL(sendsrcaddr);                     \
    GLOBAL_ATOM_DECL(sendto);                          \
    GLOBAL_ATOM_DECL(seqpacket);                       \
    GLOBAL_ATOM_DECL(setfib);                          \
    GLOBAL_ATOM_DECL(set_peer_primary_addr);           \
    GLOBAL_ATOM_DECL(socket);                          \
    GLOBAL_ATOM_DECL(sndbuf);                          \
    GLOBAL_ATOM_DECL(sndbufforce);                     \
    GLOBAL_ATOM_DECL(sndlowat);                        \
    GLOBAL_ATOM_DECL(sndtimeo);                        \
    GLOBAL_ATOM_DECL(spec_dst);                        \
    GLOBAL_ATOM_DECL(status);                          \
    GLOBAL_ATOM_DECL(stream);                          \
    GLOBAL_ATOM_DECL(syncnt);                          \
    GLOBAL_ATOM_DECL(tclass);                          \
    GLOBAL_ATOM_DECL(tcp);                             \
    GLOBAL_ATOM_DECL(throughput);                      \
    GLOBAL_ATOM_DECL(timestamp);                       \
    GLOBAL_ATOM_DECL(tos);                             \
    GLOBAL_ATOM_DECL(transparent);                     \
    GLOBAL_ATOM_DECL(true);                            \
    GLOBAL_ATOM_DECL(trunc);                           \
    GLOBAL_ATOM_DECL(ttl);                             \
    GLOBAL_ATOM_DECL(type);                            \
    GLOBAL_ATOM_DECL(udp);                             \
    GLOBAL_ATOM_DECL(unblock_source);                  \
    GLOBAL_ATOM_DECL(undefined);                       \
    GLOBAL_ATOM_DECL(unicast_hops);                    \
    GLOBAL_ATOM_DECL(usec);                            \
    GLOBAL_ATOM_DECL(user);                            \
    GLOBAL_ATOM_DECL(user_timeout);                    \
    GLOBAL_ATOM_DECL(use_ext_recvinfo);                \
    GLOBAL_ATOM_DECL(use_min_mtu);                     \
    GLOBAL_ATOM_DECL(v6only)


/* *** Global error reason atoms *** */
#define GLOBAL_ERROR_REASON_ATOMS   \
    GLOBAL_ATOM_DECL(eagain);       \
    GLOBAL_ATOM_DECL(einval)


#define GLOBAL_ATOM_DECL(A) ERL_NIF_TERM esock_atom_##A
GLOBAL_ATOMS;
GLOBAL_ERROR_REASON_ATOMS;
#undef GLOBAL_ATOM_DECL
ERL_NIF_TERM esock_atom_socket_tag; // This has a "special" name ('$socket')

/* *** Local atoms *** */
#define LOCAL_ATOMS                    \
    LOCAL_ATOM_DECL(accepting);	       \
    LOCAL_ATOM_DECL(acc_success);      \
    LOCAL_ATOM_DECL(acc_fails);        \
    LOCAL_ATOM_DECL(acc_tries);        \
    LOCAL_ATOM_DECL(acc_waits);        \
    LOCAL_ATOM_DECL(adaptation_layer); \
    LOCAL_ATOM_DECL(add);              \
    LOCAL_ATOM_DECL(addr_unreach);     \
    LOCAL_ATOM_DECL(address);          \
    LOCAL_ATOM_DECL(adm_prohibited);   \
    LOCAL_ATOM_DECL(already);          \
    LOCAL_ATOM_DECL(association);      \
    LOCAL_ATOM_DECL(assoc_id);         \
    LOCAL_ATOM_DECL(authentication);   \
    LOCAL_ATOM_DECL(boolean);          \
    LOCAL_ATOM_DECL(bound);	       \
    LOCAL_ATOM_DECL(bufsz);            \
    LOCAL_ATOM_DECL(close);            \
    LOCAL_ATOM_DECL(closed);           \
    LOCAL_ATOM_DECL(closing);          \
    LOCAL_ATOM_DECL(code);             \
    LOCAL_ATOM_DECL(connected);        \
    LOCAL_ATOM_DECL(connecting);       \
    LOCAL_ATOM_DECL(cookie_life);      \
    LOCAL_ATOM_DECL(counter_wrap);     \
    LOCAL_ATOM_DECL(counters);         \
    LOCAL_ATOM_DECL(ctype);            \
    LOCAL_ATOM_DECL(data_io);          \
    LOCAL_ATOM_DECL(data_size);        \
    LOCAL_ATOM_DECL(debug_filename);   \
    LOCAL_ATOM_DECL(del);              \
    LOCAL_ATOM_DECL(dest_unreach);     \
    LOCAL_ATOM_DECL(do);               \
    LOCAL_ATOM_DECL(dont);             \
    LOCAL_ATOM_DECL(dtor);             \
    LOCAL_ATOM_DECL(dup);              \
    LOCAL_ATOM_DECL(efile);            \
    LOCAL_ATOM_DECL(exclude);          \
    LOCAL_ATOM_DECL(false);            \
    LOCAL_ATOM_DECL(frag_needed);      \
    LOCAL_ATOM_DECL(host_unknown);     \
    LOCAL_ATOM_DECL(host_unreach);     \
    LOCAL_ATOM_DECL(how);              \
    LOCAL_ATOM_DECL(in4_sockaddr);     \
    LOCAL_ATOM_DECL(in6_sockaddr);     \
    LOCAL_ATOM_DECL(include);          \
    LOCAL_ATOM_DECL(initial);          \
    LOCAL_ATOM_DECL(interface);        \
    LOCAL_ATOM_DECL(integer);          \
    LOCAL_ATOM_DECL(iov_max);          \
    LOCAL_ATOM_DECL(iow);              \
    LOCAL_ATOM_DECL(listening);	       \
    LOCAL_ATOM_DECL(local_rwnd);       \
    LOCAL_ATOM_DECL(max);              \
    LOCAL_ATOM_DECL(max_attempts);     \
    LOCAL_ATOM_DECL(max_init_timeo);   \
    LOCAL_ATOM_DECL(max_instreams);    \
    LOCAL_ATOM_DECL(asocmaxrxt);       \
    LOCAL_ATOM_DECL(min);              \
    LOCAL_ATOM_DECL(missing);          \
    LOCAL_ATOM_DECL(mode);             \
    LOCAL_ATOM_DECL(msg);              \
    LOCAL_ATOM_DECL(msg_flags);        \
    LOCAL_ATOM_DECL(multiaddr);        \
    LOCAL_ATOM_DECL(net_unknown);      \
    LOCAL_ATOM_DECL(net_unreach);      \
    LOCAL_ATOM_DECL(netns);            \
    LOCAL_ATOM_DECL(none);             \
    LOCAL_ATOM_DECL(noroute);          \
    LOCAL_ATOM_DECL(not_neighbour);    \
    LOCAL_ATOM_DECL(null);             \
    LOCAL_ATOM_DECL(num_acceptors);    \
    LOCAL_ATOM_DECL(num_cnt_bits);     \
    LOCAL_ATOM_DECL(num_dinet);        \
    LOCAL_ATOM_DECL(num_dinet6);       \
    LOCAL_ATOM_DECL(num_dlocal);       \
    LOCAL_ATOM_DECL(num_outstreams);   \
    LOCAL_ATOM_DECL(number_peer_destinations); \
    LOCAL_ATOM_DECL(num_pip);          \
    LOCAL_ATOM_DECL(num_psctp);        \
    LOCAL_ATOM_DECL(num_ptcp);         \
    LOCAL_ATOM_DECL(num_pudp);         \
    LOCAL_ATOM_DECL(num_readers);      \
    LOCAL_ATOM_DECL(num_sockets);      \
    LOCAL_ATOM_DECL(num_tdgrams);      \
    LOCAL_ATOM_DECL(num_tseqpkgs);     \
    LOCAL_ATOM_DECL(num_tstreams);     \
    LOCAL_ATOM_DECL(num_writers);      \
    LOCAL_ATOM_DECL(offender);         \
    LOCAL_ATOM_DECL(onoff);            \
    LOCAL_ATOM_DECL(options);          \
    LOCAL_ATOM_DECL(origin);           \
    LOCAL_ATOM_DECL(otp);              \
    LOCAL_ATOM_DECL(otp_socket_option);\
    LOCAL_ATOM_DECL(owner);            \
    LOCAL_ATOM_DECL(partial_delivery); \
    LOCAL_ATOM_DECL(peer_error);       \
    LOCAL_ATOM_DECL(peer_rwnd);        \
    LOCAL_ATOM_DECL(pkt_toobig);       \
    LOCAL_ATOM_DECL(policy_fail);      \
    LOCAL_ATOM_DECL(port_unreach);     \
    LOCAL_ATOM_DECL(prim_file);        \
    LOCAL_ATOM_DECL(probe);            \
    LOCAL_ATOM_DECL(protocols);        \
    LOCAL_ATOM_DECL(rcvctrlbuf);       \
    LOCAL_ATOM_DECL(read);             \
    LOCAL_ATOM_DECL(read_byte);        \
    LOCAL_ATOM_DECL(read_fails);       \
    LOCAL_ATOM_DECL(read_pkg);         \
    LOCAL_ATOM_DECL(read_pkg_max);     \
    LOCAL_ATOM_DECL(read_tries);       \
    LOCAL_ATOM_DECL(read_waits);       \
    LOCAL_ATOM_DECL(read_write);       \
    LOCAL_ATOM_DECL(registry);         \
    LOCAL_ATOM_DECL(reject_route);     \
    LOCAL_ATOM_DECL(remote);           \
    LOCAL_ATOM_DECL(rstates);          \
    LOCAL_ATOM_DECL(select);           \
    LOCAL_ATOM_DECL(selected);         \
    LOCAL_ATOM_DECL(sender_dry);       \
    LOCAL_ATOM_DECL(send_failure);     \
    LOCAL_ATOM_DECL(sendfile);         \
    LOCAL_ATOM_DECL(sendfile_byte);    \
    LOCAL_ATOM_DECL(sendfile_deferred_close); \
    LOCAL_ATOM_DECL(sendfile_fails);   \
    LOCAL_ATOM_DECL(sendfile_max);     \
    LOCAL_ATOM_DECL(sendfile_pkg);     \
    LOCAL_ATOM_DECL(sendfile_pkg_max); \
    LOCAL_ATOM_DECL(sendfile_tries);   \
    LOCAL_ATOM_DECL(sendfile_waits);   \
    LOCAL_ATOM_DECL(shutdown);         \
    LOCAL_ATOM_DECL(slist);            \
    LOCAL_ATOM_DECL(sndctrlbuf);       \
    LOCAL_ATOM_DECL(sockaddr);         \
    LOCAL_ATOM_DECL(socket_debug);     \
    LOCAL_ATOM_DECL(socket_level);     \
    LOCAL_ATOM_DECL(socket_option);    \
    LOCAL_ATOM_DECL(sourceaddr);       \
    LOCAL_ATOM_DECL(state);            \
    LOCAL_ATOM_DECL(time_exceeded);    \
    LOCAL_ATOM_DECL(timeout);          \
    LOCAL_ATOM_DECL(true);             \
    LOCAL_ATOM_DECL(txstatus);         \
    LOCAL_ATOM_DECL(txtime);           \
    LOCAL_ATOM_DECL(use_registry);     \
    LOCAL_ATOM_DECL(value);            \
    LOCAL_ATOM_DECL(want);             \
    LOCAL_ATOM_DECL(write);            \
    LOCAL_ATOM_DECL(write_byte);       \
    LOCAL_ATOM_DECL(write_fails);      \
    LOCAL_ATOM_DECL(write_pkg);        \
    LOCAL_ATOM_DECL(write_pkg_max);    \
    LOCAL_ATOM_DECL(write_tries);      \
    LOCAL_ATOM_DECL(write_waits);      \
    LOCAL_ATOM_DECL(wstates);          \
    LOCAL_ATOM_DECL(zero);             \
    LOCAL_ATOM_DECL(zerocopy)

/* Local error reason atoms */
#define LOCAL_ERROR_REASON_ATOMS                \
    LOCAL_ATOM_DECL(select_read);               \
    LOCAL_ATOM_DECL(select_write)

#define LOCAL_ATOM_DECL(LA) static ERL_NIF_TERM atom_##LA
LOCAL_ATOMS;
LOCAL_ERROR_REASON_ATOMS;
#undef LOCAL_ATOM_DECL


/* *** Sockets *** */
static ErlNifResourceType*    esocks;
static ErlNifResourceTypeInit esockInit = {
   esock_dtor,
   esock_stop,
   (ErlNifResourceDown*) esock_down
};

// Initiated when the nif is loaded
static ESockData data;


/* These two (inline) functions are primarily intended for debugging,
 * that is, to make it easy to add debug printouts.
 */
static ESOCK_INLINE void esock_free_env(const char* slogan, ErlNifEnv* env)
{
    SGDBG( ("SOCKET", "env free - %s: 0x%lX\r\n", slogan, env) );
    // esock_dbg_printf("SOCK ENV", "free - %s: 0x%lX\r\n", slogan, env);

    if (env != NULL) enif_free_env(env);
}


static ESOCK_INLINE ErlNifEnv* esock_alloc_env(const char* slogan)
{
    ErlNifEnv* env;

    ESOCK_ASSERT( (env = enif_alloc_env()) != NULL);

    SGDBG( ("SOCKET", "env alloc - %s: 0x%lX\r\n", slogan, env) );
    // esock_dbg_printf("SOCK ENV", "alloc - %s: 0x%lX\r\n", slogan, env);

    return env;
}


/* ----------------------------------------------------------------------
 *  N I F   F u n c t i o n s
 * ----------------------------------------------------------------------
 *
 * Utility and admin functions:
 * ----------------------------
 * nif_info/0
 * nif_command/1
 * nif_supports/1
 *
 * The "proper" socket functions:
 * ------------------------------
 * nif_open(FD, Extra)
 * nif_open(Domain, Type, Protocol, Extra)
 * nif_bind(Sock, LocalAddr)
 * nif_connect(Sock, SockAddr)
 * nif_listen(Sock, Backlog)
 * nif_accept(LSock, Ref)
 * nif_send(Sock, SendRef, Data, Flags)
 * nif_sendto(Sock, SendRef, Data, Dest, Flags)
 * nif_sendmsg(Sock, SendRef, Msg, Flags)
 * nif_sendfile(Sock, SendRef, Offset, Count, InFileRef)
 * nif_sendfile(Sock, SendRef, Offset, Count)
 * nif_sendfile(Sock)
 * nif_recv(Sock, Length, Flags, RecvRef)
 * nif_recvfrom(Sock, RecvRef, BufSz, Flags)
 * nif_recvmsg(Sock, RecvRef, BufSz, CtrlSz, Flags)
 * nif_close(Sock)
 * nif_shutdown(Sock, How)
 * nif_sockname(Sock)
 * nif_peername(Sock)
 *
 * And some functions to manipulate and retrieve socket options:
 * -------------------------------------------------------------
 * nif_setopt/5
 * nif_getopt/3,4
 *
 * And some utility functions:
 * -------------------------------------------------------------
 *
 * And some socket admin functions:
 * -------------------------------------------------------------
 * nif_cancel(Sock, Ref)
 */


/* ----------------------------------------------------------------------
 * nif_info
 *
 * Description:
 * This is currently just a placeholder...
 */

static
ERL_NIF_TERM nif_info(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM info;
    ESockDescriptor* descP;

    SGDBG( ("SOCKET", "nif_info -> entry with %d args\r\n", argc) );

    if (argc == 0)
        return esock_global_info(env);

    ESOCK_ASSERT( argc == 1 );

    if (!ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }

    MLOCK(descP->readMtx);
    MLOCK(descP->writeMtx);

    SSDBG( descP, ("SOCKET", "nif_info(%T) {%d,0x%X} -> get socket info\r\n",
		   argv[0], descP->sock,
		   descP->readState | descP->writeState) );

    info = esock_socket_info(env, descP);

    SSDBG( descP, ("SOCKET", "nif_info(%T) -> get socket info done with"
		   "\r\n   info: %T"
		   "\r\n", argv[0], info) );

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    return info;
#endif
}


/*
 * This function return a property list containing "global" info.
 *
 * Note that we also include something in the counter list that is not
 * actually a counter, the num_cnt_bits. This is the "size" of each counter,
 * in number of bits: 16 | 24 | 32 | 48 | 64.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_global_info(ErlNifEnv* env)
{
    ERL_NIF_TERM
        numBits, numSockets, numTypeDGrams, numTypeStreams,
        numTypeSeqPkgs, numDomLocal, numDomInet, numDomInet6,
        numProtoIP, numProtoTCP, numProtoUDP, numProtoSCTP,
        sockDbg, iovMax, dbg, useReg, iow;

    MLOCK(data.cntMtx);
    numBits        = MKUI(env, ESOCK_COUNTER_SIZE);
    numSockets     = MKUI(env, data.numSockets);
    numTypeDGrams  = MKUI(env, data.numTypeDGrams);
    numTypeStreams = MKUI(env, data.numTypeStreams);
    numTypeSeqPkgs = MKUI(env, data.numTypeSeqPkgs);
    numDomLocal    = MKUI(env, data.numDomainLocal);
    numDomInet     = MKUI(env, data.numDomainInet);
    numDomInet6    = MKUI(env, data.numDomainInet6);
    numProtoIP     = MKUI(env, data.numProtoIP);
    numProtoTCP    = MKUI(env, data.numProtoTCP);
    numProtoUDP    = MKUI(env, data.numProtoUDP);
    numProtoSCTP   = MKUI(env, data.numProtoSCTP);
    sockDbg        = BOOL2ATOM(data.sockDbg);
    MUNLOCK(data.cntMtx);

    iovMax         = MKI(env,  data.iov_max);
    dbg            = BOOL2ATOM(data.dbg);
    useReg         = BOOL2ATOM(data.useReg);
    iow            = BOOL2ATOM(data.iow);

    {
        ERL_NIF_TERM gcntVals[] =
            {numBits,
             numSockets,
             numTypeDGrams, numTypeStreams, numTypeSeqPkgs,
             numDomLocal, numDomInet, numDomInet6,
             numProtoIP, numProtoTCP, numProtoUDP, numProtoSCTP};
        ERL_NIF_TERM gcntKeys[] =
            {atom_num_cnt_bits,
             atom_num_sockets,
             atom_num_tdgrams, atom_num_tstreams, atom_num_tseqpkgs,
             atom_num_dlocal, atom_num_dinet, atom_num_dinet6,
             atom_num_pip, atom_num_ptcp, atom_num_pudp, atom_num_psctp};
        unsigned int numGCntVals = NUM(gcntVals);
        unsigned int numGCntKeys = NUM(gcntKeys);
        ERL_NIF_TERM gcnt;

        ESOCK_ASSERT( numGCntKeys == numGCntVals );
        ESOCK_ASSERT( MKMA(env, gcntKeys, gcntVals, numGCntKeys, &gcnt) );

        {
            ERL_NIF_TERM
                keys[] = {esock_atom_debug,
                          atom_socket_debug,
                          atom_use_registry,
                          atom_iow,
                          atom_counters,
                          atom_iov_max},
                vals[] = {dbg,
                          sockDbg,
                          useReg,
                          iow,
                          gcnt,
                          iovMax},
                info;
            unsigned int
                numKeys = NUM(keys),
                numVals = NUM(vals);

            ESOCK_ASSERT( numKeys == numVals );
            ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &info) );

            return info;
        }
    }
}
#endif // #ifndef __WIN32__


/*
 * This function return a property *map*. The properties are:
 *    domain:    The domain of the socket
 *    type:      The type of the socket
 *    protocol:  The protocol of the socket
 *    owner:     Controlling process (owner) of the socket)
 *    rstates:   Socket read state(s)
 *    wstates:   Socket write state(s)
 *    counters:  A list of each socket counter and there current values
 *    readers:   The number of current and waiting readers
 *    writers:   The number of current and waiting writers
 *    acceptors: The number of current and waiting acceptors
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_socket_info(ErlNifEnv*       env,
                               ESockDescriptor* descP)
{
    ERL_NIF_TERM domain    = esock_socket_info_domain(env, descP);
    ERL_NIF_TERM type      = esock_socket_info_type(env, descP);
    ERL_NIF_TERM protocol  = MKI(env, descP->protocol);
    ERL_NIF_TERM ctrlPid   = MKPID(env, &descP->ctrlPid);
    ERL_NIF_TERM rstates   = esock_socket_info_state(env, descP->readState);
    ERL_NIF_TERM wstates   = esock_socket_info_state(env, descP->writeState);
    ERL_NIF_TERM ctype     = esock_socket_info_ctype(env, descP);
    ERL_NIF_TERM counters  = esock_socket_info_counters(env, descP);
    ERL_NIF_TERM readers   = esock_socket_info_readers(env, descP);
    ERL_NIF_TERM writers   = esock_socket_info_writers(env, descP);
    ERL_NIF_TERM acceptors = esock_socket_info_acceptors(env, descP);

    {
        ERL_NIF_TERM keys[]
            = {esock_atom_domain,
               esock_atom_type,
               esock_atom_protocol,
               atom_owner,
               atom_rstates,
               atom_wstates,
               atom_ctype,
               atom_counters,
               atom_num_readers,
               atom_num_writers,
               atom_num_acceptors};
        ERL_NIF_TERM vals[]
            = {domain,
               type,
               protocol,
               ctrlPid,
               rstates,
               wstates,
               ctype,
               counters,
               readers,
               writers,
               acceptors};
        ERL_NIF_TERM info;
        unsigned int numKeys  = NUM(keys);
        unsigned int numVals  = NUM(vals);

        SSDBG( descP, ("SOCKET", "esock_socket_info -> "
                       "\r\n   numKeys: %d"
                       "\r\n   numVals: %d"
                       "\r\n", numKeys, numVals) );

        ESOCK_ASSERT( numKeys == numVals );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &info) );

        return info;
    }
}
#endif // #ifndef __WIN32__


/*
 * Encode the socket domain
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_socket_info_domain(ErlNifEnv*       env,
                                      ESockDescriptor* descP)
{
    int          domain = descP->domain;
    ERL_NIF_TERM edomain;

    esock_encode_domain(env, domain, &edomain);
    return edomain;
}
#endif // #ifndef __WIN32__


/*
 * Encode the socket type
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_socket_info_type(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
    int          type = descP->type;
    ERL_NIF_TERM etype;

    esock_encode_type(env, type, &etype);

    return etype;
}
#endif // #ifndef __WIN32__


/*
 * Encode the socket "create type"
 * That is "show" how this socket was created:
 *
 *           normal | fromfd | {fromfd, integer()}
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_socket_info_ctype(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    ERL_NIF_TERM ctype;

    SSDBG( descP, ("SOCKET", "esock_socket_info_ctype {%d} -> entry with"
                   "\r\n   origFD:       %d"
                   "\r\n   closeOnClose: %s"
                   "\r\n", descP->sock,
                   descP->origFD, B2S(descP->closeOnClose)) );

    if (descP->origFD > 0) {
        /* Created from other FD */
        if (descP->closeOnClose) {
            /* We *have* dup'ed: {fromfd, integer()} */
            ctype = MKT2(env, MKA(env, "fromfd"), MKI(env, descP->origFD));
        } else {
            /* We have *not* dup'ed: fromfd */
            ctype = MKA(env, "fromfd");
        }
    } else {
        /* Normal socket */
        ctype = MKA(env, "normal");
    }

    SSDBG( descP, ("SOCKET", "esock_socket_info_ctype {%d} -> done:"
                   "\r\n   ctype: %T"
                   "\r\n", descP->sock, ctype) );

    return ctype;
}
#endif // #ifndef __WIN32__


/*
 * Encode the socket "state"
 * That is "show" how this socket was created:
 *
 *           normal | fromfd | {fromfd, integer()}
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_socket_info_state(ErlNifEnv*   env,
                                     unsigned int state)
{
    SocketTArray estate = TARRAY_CREATE(10);
    ERL_NIF_TERM estateList;


    if ((state & ESOCK_STATE_BOUND) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> bound"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_bound);
    }

    if ((state & ESOCK_STATE_LISTENING) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> listening"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_listening);
    }

    if ((state & ESOCK_STATE_ACCEPTING) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> accepting"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_accepting);
    }

    if ((state & ESOCK_STATE_CONNECTING) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> connecting"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_connecting);
    }

    if ((state & ESOCK_STATE_CONNECTED) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> connected"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_connected);
    }

    if ((state & ESOCK_STATE_SELECTED) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> selected"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_selected);
    }

    if ((state & ESOCK_STATE_CLOSING) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> closing"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_closing);
    }

    if ((state & ESOCK_STATE_CLOSED) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> closed"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_closed);
    }

    if ((state & ESOCK_STATE_DTOR) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> dror"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, atom_dtor);
    }

    TARRAY_TOLIST(estate, env, &estateList);

    return estateList;
}
#endif // #ifndef __WIN32__


/*
 * Collect all counters for a socket.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_socket_info_counters(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    ERL_NIF_TERM keys[] = {atom_read_byte,
                           atom_read_fails,
                           atom_read_pkg,
                           atom_read_pkg_max,
                           atom_read_tries,
                           atom_read_waits,
                           atom_write_byte,
                           atom_write_fails,
                           atom_write_pkg,
                           atom_write_pkg_max,
                           atom_write_tries,
                           atom_write_waits,
                           atom_acc_success,
                           atom_acc_fails,
                           atom_acc_tries,
                           atom_acc_waits};
    unsigned int numKeys = NUM(keys);
    ERL_NIF_TERM vals[] = {MKCNT(env, descP->readByteCnt),
                           MKCNT(env, descP->readFails),
                           MKCNT(env, descP->readPkgCnt),
                           MKCNT(env, descP->readPkgMax),
                           MKCNT(env, descP->readTries),
                           MKCNT(env, descP->readWaits),
                           MKCNT(env, descP->writeByteCnt),
                           MKCNT(env, descP->writeFails),
                           MKCNT(env, descP->writePkgCnt),
                           MKCNT(env, descP->writePkgMax),
                           MKCNT(env, descP->writeTries),
                           MKCNT(env, descP->writeWaits),
                           MKCNT(env, descP->accSuccess),
                           MKCNT(env, descP->accFails),
                           MKCNT(env, descP->accTries),
                           MKCNT(env, descP->accWaits)};
    unsigned int numVals = NUM(vals);
    ERL_NIF_TERM cnts;

    SSDBG( descP, ("SOCKET", "esock_socket_info_counters -> "
                   "\r\n   numKeys: %d"
                   "\r\n   numVals: %d"
                   "\r\n", numKeys, numVals) );

    ESOCK_ASSERT( numKeys == numVals );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &cnts) );

#ifdef HAVE_SENDFILE
    if (descP->sendfileCountersP != NULL) {
        ESockSendfileCounters *cP = descP->sendfileCountersP;
        ERL_NIF_TERM m,
            sfKeys[] =
            {atom_sendfile,
             atom_sendfile_byte,
             atom_sendfile_fails,
             atom_sendfile_max,
             atom_sendfile_pkg,
             atom_sendfile_pkg_max,
             atom_sendfile_tries,
             atom_sendfile_waits},
            sfVals[] =
            {MKUI(env, cP->cnt),
             MKUI(env, cP->byteCnt),
             MKUI(env, cP->fails),
             MKUI(env, cP->max),
             MKUI(env, cP->pkg),
             MKUI(env, cP->pkgMax),
             MKUI(env, cP->tries),
             MKUI(env, cP->waits)};
        size_t n, numSfKeys = NUM(sfKeys);

        ESOCK_ASSERT( numSfKeys == NUM(sfVals) );
        for (n = 0;  n < numSfKeys;  n++) {
            ESOCK_ASSERT( enif_make_map_put(env, cnts,
                                            sfKeys[n], sfVals[n],
                                            &m) );
            cnts = m;
        }
    }
#endif

    SSDBG( descP, ("SOCKET", "esock_socket_info_counters -> done with"
                   "\r\n   cnts: %T"
                   "\r\n", cnts) );

    return cnts;
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_command
 *
 * Description:
 * This function is intended to handle "various" commands. That is,
 * commands and operations that are not part of the socket API proper.
 * It's a map with two attributes command and (command) data:
 * #{command :: atom(), data :: term()}
 *
 * Currently it handles setting:
 *
 * Command                    Data
 * -------                    ----
 * (global) debug             boolean()
 * socket_debug               boolean()
 * use_registry               boolean()
 *
 */

static
ERL_NIF_TERM nif_command(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM ecmd, ecdata, result;
    Uint16       cmd;

    ESOCK_ASSERT( argc == 1 );

    SGDBG( ("SOCKET", "nif_command -> entry with %d args\r\n", argc) );

    if (! IS_MAP(env,  argv[0])) {
        return enif_make_badarg(env);
    }
    ecmd = argv[0];

    SGDBG( ("SOCKET", "nif_command -> "
            "\r\n   (e) command: %T"
            "\r\n", ecmd) );

    if (!ecommand2command(env, ecmd, &cmd, &ecdata)) {
        SGDBG( ("SOCKET", "nif_command -> invalid command\r\n") );
        return enif_make_badarg(env);
    }

    SGDBG( ("SOCKET", "nif_command -> "
            "\r\n   command:          %d"
            "\r\n   (e) command data: %T"
            "\r\n", cmd, ecdata) );

    result = esock_command(env, cmd, ecdata);

    SGDBG( ("SOCKET", "nif_command -> done with result: "
           "\r\n   %T"
           "\r\n", result) );

    return result;

#endif
}


#ifndef __WIN32__
static
ERL_NIF_TERM esock_command(ErlNifEnv* env, Uint16 cmd, ERL_NIF_TERM ecdata)
{
    ERL_NIF_TERM result;

    SGDBG( ("SOCKET", "esock_command -> entry with 0x%lX\r\n", cmd) );

    switch (cmd) {
    case ESOCK_CMD_DEBUG:
        result = esock_command_debug(env, ecdata);
        break;

    case ESOCK_CMD_SOCKET_DEBUG:
        result = esock_command_socket_debug(env, ecdata);
        break;

    case ESOCK_CMD_USE_SOCKET_REGISTRY:
        result = esock_command_use_socket_registry(env, ecdata);
        break;

    default:
        ESOCK_ASSERT(FALSE && "cmd is unknown");
        break;
    }

    return result;
}
#endif // #ifndef __WIN32__

#ifndef __WIN32__
static
ERL_NIF_TERM esock_command_debug(ErlNifEnv* env, ERL_NIF_TERM ecdata)
{
    ERL_NIF_TERM result;

    /* The data *should* be a boolean() */
    if (esock_decode_bool(ecdata, &data.dbg))
        result = esock_atom_ok;
    else
        result =
            esock_raise_invalid(env, MKT2(env, esock_atom_debug, ecdata));

    return result;
}
#endif // #ifndef __WIN32__

#ifndef __WIN32__
static
ERL_NIF_TERM esock_command_socket_debug(ErlNifEnv* env, ERL_NIF_TERM ecdata)
{
    BOOLEAN_T dbg;

    /* The data *should* be a boolean() */
    if (! esock_decode_bool(ecdata, &dbg))
        return
            esock_raise_invalid(env, MKT2(env, atom_socket_debug, ecdata));

    MLOCK(data.cntMtx);
    data.sockDbg = dbg;
    MUNLOCK(data.cntMtx);

    return esock_atom_ok;;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
ERL_NIF_TERM esock_command_use_socket_registry(ErlNifEnv*   env,
                                               ERL_NIF_TERM ecdata)
{
    BOOLEAN_T useReg = FALSE;

    /* The data *should* be a boolean() */
    if (! esock_decode_bool(ecdata, &useReg))
        return
            esock_raise_invalid(env, MKT2(env, atom_use_registry, ecdata));

    MLOCK(data.cntMtx);
    data.useReg = useReg;
    MUNLOCK(data.cntMtx);

    return esock_atom_ok;;
}
#endif




/* *** esock_socket_info_readers   ***
 * *** esock_socket_info_writers   ***
 * *** esock_socket_info_acceptors ***
 *
 * Calculate how many readers | writers | acceptors we have for this socket.
 * Current requestor + any waiting requestors (of the type).
 *
 */

#ifndef __WIN32__

#define ESOCK_INFO_REQ_FUNCS                                            \
    ESOCK_INFO_REQ_FUNC_DECL(readers,   currentReaderP,   readersQ)     \
    ESOCK_INFO_REQ_FUNC_DECL(writers,   currentWriterP,  writersQ)     \
    ESOCK_INFO_REQ_FUNC_DECL(acceptors, currentAcceptorP, acceptorsQ)

#define ESOCK_INFO_REQ_FUNC_DECL(F, CRP, Q)                             \
    static                                                              \
    ERL_NIF_TERM esock_socket_info_##F(ErlNifEnv*       env,            \
                                       ESockDescriptor* descP)          \
    {                                                                   \
        return socket_info_reqs(env, descP, descP->CRP, &descP->Q);     \
    }
ESOCK_INFO_REQ_FUNCS
#undef ESOCK_INFO_REQ_FUNC_DECL

static
ERL_NIF_TERM socket_info_reqs(ErlNifEnv*         env,
                              ESockDescriptor*   descP,
                              ESockRequestor*    currentRequestorP,
                              ESockRequestQueue* q)
{
    ESockRequestQueueElement* tmp;
    ERL_NIF_TERM              info;
    unsigned int              cnt = 0;

    if (currentRequestorP != NULL) {
        // We have an active requestor!
        cnt++;

        // And add all the waiting requestors
        tmp = q->first;
        while (tmp != NULL) {
            cnt++;
            tmp = tmp->nextP;
        }
    }

    info = MKUI(env, cnt);

    SSDBG( descP, ("SOCKET", "socket_info_reqs -> done with"
                   "\r\n   info: %T"
                   "\r\n", info) );

    return info;
}

#endif // #ifndef __WIN32__


/* ----------------------------------------------------------------------
 * nif_supports
 *
 * Description:
 * This function is intended to answer the question: "Is X supported?"
 * Currently three keys are "supported": options | sctp | ipv6
 * That results in a list of all *known options* (known by us) and if
 * the platform supports (OS) it or not.
 *
 * Key
 * ---
 * (arity 0)       [{sctp,      boolean()},
 *                  {ipv6,     boolean()},
 *                  {local,    boolean()},
 *                  {netns,    boolean()},
 *                  {sendfile, boolean()}]
 *
 * msg_flags       [{MsgFlag,              boolean()}]
 * protocols       [{[Protocol | Aliases], integer()}],
 * options         [{socket,               [{Opt, boolean()} | Opt]} |
 *                  {ProtoNum::integer(),  [{Opt, boolean()} | Opt]}]
 */

static
ERL_NIF_TERM nif_supports(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else

    SGDBG( ("SOCKET", "nif_supports -> entry with %d args\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if (argc == 0)
        return esock_supports_0(env);

    ESOCK_ASSERT( argc == 1 );

    return esock_supports_1(env, argv[0]);
#endif
}

/* esock_supports - what features do we support?
 *
 * This gives information about what features actually
 * work on the current platform.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_supports_0(ErlNifEnv* env)
{
    SocketTArray opts = TARRAY_CREATE(8);
    ERL_NIF_TERM is_supported, opts_list;

    SGDBG( ("SOCKET", "esock_supports_0 -> entry\r\n") );

#if defined(HAVE_SCTP)
    is_supported = esock_atom_true;
#else
    is_supported = esock_atom_false;
#endif
    TARRAY_ADD(opts, MKT2(env, esock_atom_sctp, is_supported));

    /* Is this (test) really sufficient for testing if we support IPv6? */
#if defined(HAVE_IPV6)
    is_supported = esock_atom_true;
#else
    is_supported = esock_atom_false;
#endif
    TARRAY_ADD(opts, MKT2(env, esock_atom_ipv6, is_supported));

#if defined(AF_LOCAL)
    is_supported = esock_atom_true;
#else
    is_supported = esock_atom_false;
#endif
    TARRAY_ADD(opts, MKT2(env, esock_atom_local, is_supported));

#if defined(HAVE_SETNS)
    is_supported = esock_atom_true;
#else
    is_supported = esock_atom_false;
#endif
    TARRAY_ADD(opts, MKT2(env, atom_netns, is_supported));

#if defined(HAVE_SENDFILE)
    is_supported = esock_atom_true;
#else
    is_supported = esock_atom_false;
#endif
    TARRAY_ADD(opts, MKT2(env, atom_sendfile, is_supported));

    TARRAY_TOLIST(opts, env, &opts_list);
    return opts_list;
}
#endif // #ifndef __WIN32__

#ifndef __WIN32__
static
ERL_NIF_TERM esock_supports_1(ErlNifEnv* env, ERL_NIF_TERM key)
{
    ERL_NIF_TERM result;

    SGDBG( ("SOCKET",
            "esock_supports_1 -> entry"
            "\r\n   key: %T"
            "\r\n", key) );

    if (COMPARE(key, atom_msg_flags) == 0)
        result = esock_supports_msg_flags(env);
    else if (COMPARE(key, atom_protocols) == 0)
        result = esock_supports_protocols(env);
    else if (COMPARE(key, atom_options) == 0)
        result = esock_supports_options(env);
    else
        result = MKEL(env);

    return result;
}
#endif // #ifndef __WIN32__



#ifndef __WIN32__

static ERL_NIF_TERM esock_supports_msg_flags(ErlNifEnv* env) {
    size_t n;
    ERL_NIF_TERM result;

    result = MKEL(env);
    for (n = 0;  n < NUM(msg_flags);  n++) {
        result =
            MKC(env,
                MKT2(env,
                     *(msg_flags[n].name),
                     MKI(env, msg_flags[n].flag)),
                result);
    }

    return result;
}

#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
ERL_NIF_TERM esock_supports_protocols(ErlNifEnv* env)
{
    ERL_NIF_TERM protocols;
    struct protoent *pe;
    int stayopen;

    protocols = MKEL(env);

#if defined(HAVE_GETPROTOENT) && \
    defined(HAVE_SETPROTOENT) && \
    defined(HAVE_ENDPROTOENT)

    MLOCK(data.protocolsMtx);
    stayopen = TRUE;
    setprotoent(stayopen);
    while ((pe = getprotoent()) != NULL) {
        ERL_NIF_TERM names;
        char **aliases;

        names = MKEL(env);
        for (aliases = pe->p_aliases;  *aliases != NULL;  aliases++)
            names = MKC(env, MKA(env, *aliases), names);
        names = MKC(env, MKA(env, pe->p_name), names);

        protocols =
            MKC(env, MKT2(env, names, MKI(env, pe->p_proto)), protocols);
    }
    endprotoent();
    MUNLOCK(data.protocolsMtx);

#endif

    /* Defaults for known protocols in case getprotoent()
     * does not work or does not exist.  Prepended to the list
     * so a subsequent maps:from_list/2 will take the default
     * only when there is nothing from getprotoent().
     */

    protocols =
        MKC(env,
            MKT2(env,
                 MKL1(env, esock_atom_ip),
                 MKI(env,
#ifdef SOL_IP
                     SOL_IP
#else
                     IPPROTO_IP
#endif
                     )),
            protocols);

#ifdef HAVE_IPV6
    protocols =
        MKC(env,
            MKT2(env,
                 MKL1(env, esock_atom_ipv6),
                 MKI(env,
#ifdef SOL_IPV6
                     SOL_IPV6
#else
                     IPPROTO_IPV6
#endif
                     )),
            protocols);
#endif

    protocols =
        MKC(env,
            MKT2(env, MKL1(env, esock_atom_tcp), MKI(env, IPPROTO_TCP)),
            protocols);

    protocols =
        MKC(env,
            MKT2(env, MKL1(env, esock_atom_udp), MKI(env, IPPROTO_UDP)),
            protocols);

#ifdef HAVE_SCTP
    protocols =
        MKC(env,
            MKT2(env, MKL1(env, esock_atom_sctp), MKI(env, IPPROTO_SCTP)),
            protocols);
#endif

    return protocols;
}
#endif // #ifndef __WIN32__



#ifndef __WIN32__

static
ERL_NIF_TERM esock_supports_options(ErlNifEnv* env)
{
    ERL_NIF_TERM levels;
    size_t n;

    levels = MKEL(env);

    for (n = 0;  n < NUM(optLevels);  n++) {
        ERL_NIF_TERM options;
        size_t m;
        struct ESockOptLevel *levelP;

        options = MKEL(env);
        levelP = optLevels + n;
        for (m = 0;  m < levelP->num;  m++) {
            struct ESockOpt *optP;

            optP = levelP->opts + m;
            if (optP->setopt == NULL && optP->getopt == NULL) {
                options = MKC(env, *optP->nameP, options);
            } else {
                options =
                    MKC(env,
                        MKT2(env, *optP->nameP, MKI(env, optP->opt)),
                        options);
            }
        }
        levels =
            MKC(env,
                MKT2(env,
                     esock_encode_level(env, levelP->level), options),
                levels);
    }

    return levels;
}

#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_open
 *
 * Description:
 * Create an endpoint for communication.
 * This function "exist" in two variants.
 * One with two args and onewith four.
 *
 * Arguments (2):
 * FD       - File Descriptor (of an already open socket).
 * Extra    - A map with extra options.
 *            The options are:
 *               [M] dup    - boolean() - Shall the fd be dup'ed or not.
 *               [O] bound  - boolean() - Is the fd already bound.
 *               [O] domain - domain()  - We may not be able to retrieve
 *                                        this on all platforms, and in
 *                                        *those* cases this *must* be
 *                                        provided.
 *               [O] type   - type()    - We may not be able to retrieve
 *                                        this on all platforms, and in
 *                                        *those* cases this *must* be
 *                                        provided.
 *               [O] proto - protocol() - We may not be able to retrieve
 *                                        this on all platforms, and in
 *                                        *those* cases this *must* be
 *                                        provided.
 *               [O] use_registry - boolean() - Shall we use the socket
 *                                        registry for this socket.
 * Arguments (4):
 * Domain   - The domain, for example 'inet'
 * Type     - Type of socket, for example 'stream'
 * Protocol - The protocol, for example 'tcp'
 * Extra    - A map with "obscure" options.
 *            Currently the only allowed option are:
 *               netns        - string()  - Network namespace.  *Only*
 *                                          allowed on linux!
 *               use_registry - boolean() - Shall we use the socket
 *                                          registry for this socket.
 *
 */
static
ERL_NIF_TERM nif_open(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result;

    SGDBG( ("SOCKET", "nif_open -> "
            "\r\n   argc: %d"
            "\r\n", argc) );

    if (argc == 2) {
        int          fd;
	ERL_NIF_TERM eopts;

	if (! GET_INT(env, argv[0], &fd)) {
            if (IS_INTEGER(env, argv[0]))
                return esock_make_error_integer_range(env, argv[0]);
            else
                return enif_make_badarg(env);
	}
	if (! IS_MAP(env,  argv[1])) {
	    return enif_make_badarg(env);
	}
	eopts = argv[1];

	SGDBG( ("SOCKET", "nif_open -> "
		"\r\n   FD:    %d"
		"\r\n   eopts: %T"
		"\r\n", fd, eopts) );

	MLOCK(data.cntMtx);
	result = esock_open2(env, fd, eopts);
	MUNLOCK(data.cntMtx);

    } else {
        ERL_NIF_TERM edomain, etype, eopts;
	int          domain, type, proto;

	ESOCK_ASSERT( argc == 4 );

	/* Extract arguments and perform preliminary validation */

	if (! GET_INT(env, argv[2], &proto)) {
            if (IS_INTEGER(env, argv[2]))
                return esock_make_error_integer_range(env, argv[2]);
            else
                return enif_make_badarg(env);
        }
	if (! IS_MAP(env,  argv[3])) {
	    return enif_make_badarg(env);
	}
	edomain = argv[0];
	etype = argv[1];
	eopts  = argv[3];

	SGDBG( ("SOCKET", "nif_open -> "
		"\r\n   edomain: %T"
		"\r\n   etype:   %T"
		"\r\n   proto:   %T"
		"\r\n   eopts:   %T"
		"\r\n", argv[0], argv[1], argv[2], eopts) );

	if (! esock_decode_domain(env, edomain, &domain)) {
	    SGDBG( ("SOCKET",
		    "nif_open -> invalid domain: %d\r\n", edomain) );
	    return esock_make_invalid(env, esock_atom_domain);
	}

	if (! esock_decode_type(env, etype, &type)) {
	    SGDBG( ("SOCKET",
		    "nif_open -> invalid type: %d\r\n", etype) );
	    return esock_make_invalid(env, esock_atom_type);
	}

	MLOCK(data.cntMtx);
	result = esock_open4(env, domain, type, proto, eopts);
	MUNLOCK(data.cntMtx);
    }

    SGDBG( ("SOCKET", "nif_open -> done with result: "
            "\r\n   %T"
            "\r\n", result) );

    return result;

#endif // #ifdef __WIN32__  #else
}


/* esock_open - create an endpoint (from an existing fd) for communication
 *
 * Assumes the input has been validated.
 *
 * Normally we want debugging on (individual) sockets to be controlled
 * by the sockets own debug flag. But since we don't even have a socket
 * yet, we must use the global debug flag.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_open2(ErlNifEnv*   env,
                         int          fd,
                         ERL_NIF_TERM eopts)
{
    BOOLEAN_T        dbg    = esock_open_is_debug(env, eopts, data.sockDbg);
    BOOLEAN_T        useReg = esock_open_use_registry(env, eopts, data.useReg);
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef;
    int              domain, type, protocol;
    int              save_errno = 0;
    BOOLEAN_T        closeOnClose;
    SOCKET           sock;
    ErlNifEvent      event;
    ErlNifPid        self;

    /* Keep track of the creator
     * This should not be a problem, but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    SSDBG2( dbg,
            ("SOCKET", "esock_open2 -> entry with"
             "\r\n   fd:    %d"
             "\r\n   eopts: %T"
             "\r\n", fd, eopts) );

    /*
     * Before we do anything else, we try to retrieve domain, type and protocol
     * This information is either present in the eopts map or if not we need
     * to "get" it from the system (getsockopt).
     * Note that its not possible to get all of these on all platoforms,
     * and in those cases the user *must* provide us with them (eopts).
     *
     * We try the system first (since its more reliable) and if that fails
     * we check the eopts map. If neither one works, we *give up*!
     */

    if (! esock_open_which_domain(fd, &domain)) {
        SSDBG2( dbg,
                ("SOCKET",
                 "esock_open2 -> failed get domain from system\r\n") );

        if (! esock_open2_get_domain(env, eopts, &domain)) {
            return esock_make_invalid(env, esock_atom_domain);
        }
    }

    if (! esock_open_which_type(fd, &type)) {
        SSDBG2( dbg,
                ("SOCKET", "esock_open2 -> failed get type from system\r\n") );

        if (! esock_open2_get_type(env, eopts, &type))
            return esock_make_invalid(env, esock_atom_type);
    }

    if (! esock_open_which_protocol(fd, &protocol)) {
        SSDBG2( dbg,
                ("SOCKET",
                 "esock_open2 -> failed get protocol from system\r\n") );

        if (! esock_extract_int_from_map(env, eopts,
                                        esock_atom_protocol, &protocol)) {
            SSDBG2( dbg,
                    ("SOCKET",
                     "esock_open2 -> trying protocol 0\r\n") );
            protocol = 0;
        }
    }


    SSDBG2( dbg,
            ("SOCKET", "esock_open2 -> "
             "\r\n   domain:   %d"
             "\r\n   type:     %d"
             "\r\n   protocol: %d"
             "\r\n", domain, type, protocol) );


    if (esock_open2_todup(env, eopts)) {
        /* We shall dup the socket */
        if (ESOCK_IS_ERROR(sock = dup(fd))) {
            save_errno = sock_errno();

            SSDBG2( dbg,
                    ("SOCKET",
                     "esock_open2 -> dup failed: %d\r\n",
                     save_errno) );

            return esock_make_error_errno(env, save_errno);
        }
        closeOnClose = TRUE;
    } else {
        sock         = fd;
        closeOnClose = FALSE;
    }

    event = sock;

    SET_NONBLOCKING(sock);

    /* Create and initiate the socket "descriptor" */
    descP               = alloc_descriptor(sock, event);
    descP->ctrlPid      = self;
    descP->domain       = domain;
    descP->type         = type;
    descP->protocol     = protocol;
    descP->closeOnClose = closeOnClose;
    descP->origFD       = fd;

    /* Check if we are already connected, if so change state */
    {
        ESockAddress remote;
        SOCKLEN_T    addrLen = sizeof(remote);
        sys_memzero((char *) &remote, addrLen);
        if (sock_peer(descP->sock,
                      (struct sockaddr*) &remote,
                      &addrLen) == 0) {
            SSDBG2( dbg, ("SOCKET", "esock_open2 -> connected\r\n") );
            descP->writeState |= ESOCK_STATE_CONNECTED;
        } else {
            SSDBG2( dbg, ("SOCKET", "esock_open2 -> not connected\r\n") );
        }
    }

    /* And create the 'socket' resource */
    sockRef = enif_make_resource(env, descP);
    enif_release_resource(descP);

    ESOCK_ASSERT( MONP("esock_open2 -> ctrl",
                       env, descP,
                       &descP->ctrlPid,
                       &descP->ctrlMon) == 0 );

    descP->dbg    = dbg;
    descP->useReg = useReg;
    inc_socket(domain, type, protocol);

    /* And finally (maybe) update the registry.
     * Shall we keep track of the fact that this socket is created elsewhere?
     */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, sockRef);

    SSDBG2( dbg,
            ("SOCKET", "esock_open2 -> done: %T\r\n", sockRef) );

    return esock_make_ok2(env, sockRef);
}
#endif // #ifndef __WIN32__


/* The eextra contains a boolean 'dup' key. Defaults to TRUE.
 */
#ifndef __WIN32__
static
BOOLEAN_T esock_open2_todup(ErlNifEnv* env, ERL_NIF_TERM eextra)
{
    return esock_get_bool_from_map(env, eextra, atom_dup, TRUE);
}
#endif // #ifndef __WIN32__

/* The eextra contains an integer 'domain' key.
 */
#ifndef __WIN32__
static
BOOLEAN_T esock_open2_get_domain(ErlNifEnv* env,
                                 ERL_NIF_TERM eopts, int* domain)
{
    ERL_NIF_TERM edomain;
    
    SGDBG( ("SOCKET", "esock_open2_get_domain -> entry with"
	    "\r\n   eopts: %T"
	    "\r\n", eopts) );

    if (!GET_MAP_VAL(env, eopts,
		     esock_atom_domain, &edomain))
      return FALSE;

    if (! esock_decode_domain(env, edomain, domain))
      return FALSE;

    return TRUE;
}
#endif // #ifndef __WIN32__

/* The eextra contains an integer 'type' key.
 */
#ifndef __WIN32__
static
BOOLEAN_T esock_open2_get_type(ErlNifEnv* env,
                               ERL_NIF_TERM eopts, int* type)
{
    ERL_NIF_TERM etype;

    SGDBG( ("SOCKET", "esock_open2_get_type -> entry with"
            "\r\n   eopts: %T"
            "\r\n", eopts) );

    if (! GET_MAP_VAL(env, eopts, esock_atom_type, &etype))
        return FALSE;

    if (! esock_decode_type(env, etype, type))
        return FALSE;

    return TRUE;
}
#endif // #ifndef __WIN32__


/* esock_open4 - create an endpoint for communication
 *
 * Assumes the input has been validated.
 *
 * Normally we want debugging on (individual) sockets to be controlled
 * by the sockets own debug flag. But since we don't even have a socket
 * yet, we must use the global debug flag.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_open4(ErlNifEnv*   env,
                         int          domain,
                         int          type,
                         int          protocol,
                         ERL_NIF_TERM eopts)
{
    BOOLEAN_T        dbg = esock_open_is_debug(env, eopts, data.sockDbg);
    BOOLEAN_T        useReg = esock_open_use_registry(env, eopts, data.useReg);
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef;
    int              proto = protocol, save_errno;
    SOCKET           sock;
    ErlNifEvent      event;
    char*            netns;
#ifdef HAVE_SETNS
    int              current_ns = 0;
#endif
    ErlNifPid        self;

    /* Keep track of the creator
     * This should not be a problem, but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    SSDBG2( dbg,
            ("SOCKET", "esock_open4 -> entry with"
             "\r\n   domain:   %d"
             "\r\n   type:     %d"
             "\r\n   protocol: %d"
             "\r\n   eopts:    %T"
             "\r\n", domain, type, protocol, eopts) );


#ifdef HAVE_SETNS
    if (esock_open4_get_netns(env, eopts, &netns)) {
        SGDBG( ("SOCKET", "nif_open -> namespace: %s\r\n", netns) );
    }
#else
    netns = NULL;
#endif


#ifdef HAVE_SETNS
    if ((netns != NULL) &&
        (! change_network_namespace(netns, &current_ns, &save_errno))) {
        FREE(netns);
        return esock_make_error_errno(env, save_errno);
    }
#endif

    if (ESOCK_IS_ERROR(sock = sock_open(domain, type, proto))) {
        if (netns != NULL) FREE(netns);
        return esock_make_error_errno(env, sock_errno());
    }

    SSDBG2( dbg, ("SOCKET", "esock_open -> open success: %d\r\n", sock) );


    /* NOTE that if the protocol = 0 (default) and the domain is not
     * local (AF_LOCAL) we need to explicitly get the protocol here!
     */
    
    if (proto == 0)
        (void) esock_open_which_protocol(sock, &proto);

#ifdef HAVE_SETNS
    if (netns != NULL) {
        FREE(netns);
        if (! restore_network_namespace(current_ns, sock, &save_errno))
            return esock_make_error_errno(env, save_errno);
    }
#endif


    if ((event = sock_create_event(sock)) == INVALID_EVENT) {
        save_errno = sock_errno();
        (void) sock_close(sock);
        return esock_make_error_errno(env, save_errno);
    }

    SSDBG2( dbg, ("SOCKET", "esock_open4 -> event success: %d\r\n", event) );

    SET_NONBLOCKING(sock);


    /* Create and initiate the socket "descriptor" */
    descP           = alloc_descriptor(sock, event);
    descP->ctrlPid  = self;
    descP->domain   = domain;
    descP->type     = type;
    descP->protocol = proto;

    sockRef = enif_make_resource(env, descP);
    enif_release_resource(descP);

    ESOCK_ASSERT( MONP("esock_open -> ctrl",
                       env, descP,
                       &descP->ctrlPid,
                       &descP->ctrlMon) == 0 );

    descP->dbg    = dbg;
    descP->useReg = useReg;
    inc_socket(domain, type, protocol);

    /* And finally (maybe) update the registry */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, sockRef);

    return esock_make_ok2(env, sockRef);
}
#endif // #ifndef __WIN32__

/* The eextra map "may" contain a boolean 'debug' key.
 */
#ifndef __WIN32__
static
BOOLEAN_T esock_open_is_debug(ErlNifEnv* env, ERL_NIF_TERM eextra,
                              BOOLEAN_T dflt)
{
    return esock_get_bool_from_map(env, eextra, esock_atom_debug, dflt);
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
BOOLEAN_T esock_open_use_registry(ErlNifEnv* env, ERL_NIF_TERM eextra,
                                  BOOLEAN_T dflt)
{
    return esock_get_bool_from_map(env, eextra, atom_use_registry, dflt);
}
#endif


#ifndef __WIN32__
static
BOOLEAN_T esock_open_which_domain(SOCKET sock, int* domain)
{
#if defined(SO_DOMAIN)
    if (esock_getopt_int(sock, SOL_SOCKET, SO_DOMAIN, domain))
        return TRUE;
#endif
    return FALSE;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
BOOLEAN_T esock_open_which_type(SOCKET sock, int* type)
{
#if defined(SO_TYPE)
    if (esock_getopt_int(sock, SOL_SOCKET, SO_TYPE, type))
        return TRUE;
#endif
    return FALSE;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
BOOLEAN_T esock_open_which_protocol(SOCKET sock, int* proto)
{
#if defined(SO_PROTOCOL)
    if (esock_getopt_int(sock, SOL_SOCKET, SO_PROTOCOL, proto))
        return TRUE;
#endif
    return FALSE;
}
#endif // #ifndef __WIN32__



#ifdef HAVE_SETNS


/* We should really have another API, so that we can return errno... */

/* *** change network namespace ***
 * Retreive the current namespace and set the new.
 * Return result and previous namespace if successfull.
 */
#ifndef __WIN32__
static
BOOLEAN_T change_network_namespace(char* netns, int* cns, int* err)
{
    int save_errno;
    int current_ns = 0;
    int new_ns     = 0;

    SGDBG( ("SOCKET", "change_network_namespace -> entry with"
            "\r\n   new ns: %s"
            "\r\n", netns) );

    current_ns = open("/proc/self/ns/net", O_RDONLY);
    if (ESOCK_IS_ERROR(current_ns)) {
        *err = sock_errno();
        return FALSE;
    }
    new_ns = open(netns, O_RDONLY);
    if (ESOCK_IS_ERROR(new_ns)) {
        save_errno = sock_errno();
        (void) close(current_ns);
        *err = save_errno;
        return FALSE;
    }
    if (setns(new_ns, CLONE_NEWNET) != 0) {
        save_errno = sock_errno();
        (void) close(new_ns);
        (void) close(current_ns);
        *err = save_errno;
        return FALSE;
    } else {
        (void) close(new_ns);
        *cns = current_ns;
        return TRUE;
    }
}
#endif // #ifndef __WIN32__


/* *** restore network namespace ***
 * Restore the previous namespace (see above).
 */
#ifndef __WIN32__
static
BOOLEAN_T restore_network_namespace(int ns, SOCKET sock, int* err)
{
    SGDBG( ("SOCKET", "restore_network_namespace -> entry with"
            "\r\n   ns: %d"
            "\r\n", ns) );

    if (setns(ns, CLONE_NEWNET) != 0) {
        /* XXX Failed to restore network namespace.
         * What to do? Tidy up and return an error...
         * Note that the thread now might still be in the namespace.
         * Can this even happen? Should the emulator be aborted?
         */
        int save_errno = sock_errno();
        (void) close(sock);
        (void) close(ns);
        *err = save_errno;
        return FALSE;
    } else {
        (void) close(ns);
        return TRUE;
    }
}
#endif // #ifndef __WIN32__


#endif // ifdef HAVE_SETNS



/* ----------------------------------------------------------------------
 * nif_bind
 *
 * Description:
 * Bind a name to a socket.
 *
 * Arguments:
 * [0] Socket (ref) - Points to the socket descriptor.
 * [1] LocalAddr    - Local address is a sockaddr map ( socket:sockaddr() ).
 */
static
ERL_NIF_TERM nif_bind(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     eSockAddr, ret;
    ESockAddress     sockAddr;
    SOCKLEN_T       addrLen;

    ESOCK_ASSERT( argc == 2 );

    SGDBG( ("SOCKET", "nif_bind -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }
    eSockAddr = argv[1];

    if (! esock_decode_sockaddr(env, eSockAddr, &sockAddr, &addrLen))
        return esock_make_invalid(env, atom_sockaddr);

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "nif_bind(%T) {%d,0x%X} ->"
            "\r\n   SockAddr: %T"
            "\r\n",
            argv[0], descP->sock, descP->readState,
            eSockAddr) );

    ret = esock_bind(env, descP, &sockAddr, addrLen);

    SSDBG( descP, ("SOCKET", "nif_bind(%T) -> done with"
                   "\r\n   ret: %T"
                   "\r\n", argv[0], ret) );

    MUNLOCK(descP->readMtx);

    return ret;
#endif // #ifdef __WIN32__  #else
}


#ifndef __WIN32__
static
ERL_NIF_TERM esock_bind(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ESockAddress*    sockAddrP,
                        SOCKLEN_T        addrLen)
{
    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);

    if (sock_bind(descP->sock, &sockAddrP->sa, addrLen) < 0) {
        return esock_make_error_errno(env, sock_errno());
    }

    descP->readState |= ESOCK_STATE_BOUND;

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_connect
 *
 * Description:
 * Initiate a connection on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Optional arguments:
 * ConnectRef   - Ref for the connection
 * SockAddr     - Socket Address of "remote" host.
 *                This is sockaddr(), which is either
 *                sockaddr_in4 or sockaddr_in6.
 */
static
ERL_NIF_TERM nif_connect(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     res, sockRef, connRef;
    ESockAddress     addr, *addrP;
    SOCKLEN_T        addrLen;

    ESOCK_ASSERT( argc >= 1 );

    SGDBG( ("SOCKET", "nif_connect -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    sockRef = argv[0];
    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP))
        return enif_make_badarg(env);

    if (argc == 3) {
        ERL_NIF_TERM eSockAddr = argv[2];

        connRef = argv[1];
        if (! enif_is_ref(env, connRef))
            return enif_make_badarg(env);

        if (! esock_decode_sockaddr(env, eSockAddr, &addr, &addrLen))
            return esock_make_invalid(env, atom_sockaddr);
        addrP = &addr;

        MLOCK(descP->writeMtx);

        SSDBG( descP,
               ("SOCKET", "nif_connect(%T), {%d0x%X} ->"
                "\r\n   ConnRef: %T"
                "\r\n   SockAddr: %T"
                "\r\n",
                sockRef, descP->sock, descP->writeState,
                connRef, eSockAddr) );
    } else {

        ESOCK_ASSERT( argc == 1 );

        connRef = esock_atom_undefined;
        addrP = NULL;
        addrLen = 0;

        MLOCK(descP->writeMtx);

        SSDBG( descP,
               ("SOCKET", "nif_connect(%T), {%d,0x%X} ->"
                "\r\n",
                sockRef, descP->sock, descP->writeState
                ) );
    }

    res = esock_connect(env, descP, sockRef, connRef, addrP, addrLen);

    SSDBG( descP, ("SOCKET", "nif_connect(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    MUNLOCK(descP->writeMtx);

    return res;

#endif // #ifdef __WIN32__  #else
}


#ifndef __WIN32__
static
ERL_NIF_TERM esock_connect(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     connRef,
                           ESockAddress*    addrP,
                           SOCKLEN_T        addrLen)
{
    int       save_errno;
    ErlNifPid self;

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    /*
     * Verify that we are in the proper state
     */

    if (! IS_OPEN(descP->writeState))
        return esock_make_error(env, atom_closed);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentWriterP != NULL)
        return esock_make_error_invalid(env, atom_state);

    if (descP->connectorP != NULL) {
        /* Connect in progress */

        if (COMPARE_PIDS(&self, &descP->connector.pid) != 0) {
	    /* Other process has connect in progress */
	    if (addrP != NULL) {
                return esock_make_error(env, atom_already);
	    } else {
	        /* This is a bad call sequence
		 * - connect without an address is only allowed
		 *   for the connecting process
		 */
	        return esock_raise_invalid(env, atom_state);
	    }
        }

        /* Finalize after received select message */

        requestor_release("esock_connect finalize -> connected",
                          env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;

        if (! verify_is_connected(descP, &save_errno)) {
            return esock_make_error_errno(env, save_errno);
        }

        descP->writeState |= ESOCK_STATE_CONNECTED;

        return esock_atom_ok;
    }

    /* No connect in progress */

    if (addrP == NULL)
        /* This is a bad call sequence
         * - connect without an address is only allowed when
         *   a connect is in progress, after getting the select message
         */
        return esock_raise_invalid(env, atom_state);

    /* Initial connect call, with address */

    if (sock_connect(descP->sock, (struct sockaddr*) addrP, addrLen) == 0) {
        /* Success already! */
        SSDBG( descP, ("SOCKET", "esock_connect {%d} -> connected\r\n",
                       descP->sock) );

        descP->writeState |= ESOCK_STATE_CONNECTED;

        return esock_atom_ok;
    }

    /* Connect returned error */
    save_errno = sock_errno();

    switch (save_errno) {

    case ERRNO_BLOCK:   /* Winsock2            */
    case EINPROGRESS:   /* Unix & OSE!!        */
        SSDBG( descP,
               ("SOCKET", "esock_connect {%d} -> would block => select\r\n",
                descP->sock) );
        {
            int sres;

            if ((sres =
                 esock_select_write(env, descP->sock, descP, NULL,
                                    sockRef, connRef)) < 0)
                return
                    enif_raise_exception(env,
                                         MKT2(env, atom_select_write,
                                              MKI(env, sres)));
            /* Initiate connector */
            descP->connector.pid = self;
            ESOCK_ASSERT( MONP("esock_connect -> conn",
                               env, descP,
                               &self, &descP->connector.mon) == 0 );
            descP->connector.env = esock_alloc_env("connector");
            descP->connector.ref = CP_TERM(descP->connector.env, connRef);
            descP->connectorP = &descP->connector;
            descP->writeState |=
                (ESOCK_STATE_CONNECTING | ESOCK_STATE_SELECTED);

            return atom_select;
        }
        break;

    default:
        SSDBG( descP,
               ("SOCKET", "esock_connect {%d} -> error: %d\r\n",
                descP->sock, save_errno) );

        return esock_make_error_errno(env, save_errno);

    } // switch(save_errno)
}
#endif // #ifndef __WIN32__



/* *** verify_is_connected ***
 * Check if a connection has been established.
 */
#ifndef __WIN32__
static
BOOLEAN_T verify_is_connected(ESockDescriptor* descP, int* err)
{
    /*
     * *** This is strange ***
     *
     * This *should* work on Windows NT too, but doesn't.
     * An bug in Winsock 2.0 for Windows NT?
     *
     * See "Unix Netwok Programming", "The Sockets Networking API",
     * W.R.Stevens, Volume 1, third edition, 16.4 Nonblocking 'connect',
     * before Interrupted 'connect' (p 412) for a discussion about
     * Unix portability and non blocking connect.
     */

    int error = 0;

#ifdef SO_ERROR
    if (! esock_getopt_int(descP->sock, SOL_SOCKET, SO_ERROR, &error)) {
        // Solaris does it this way according to W.R.Stevens
        error = sock_errno();
    }
#elif 1
    char buf[0];
    if (ESOCK_IS_ERROR(read(descP->sock, buf, sizeof(buf)))) {
        error = sock_errno();
    }
#else
    /* This variant probably returns wrong error value
     * ENOTCONN instead of the actual connect error
     */
    ESockAddress remote;
    SOCKLEN_T    addrLen = sizeof(remote);
    sys_memzero((char *) &remote, addrLen);
    if (sock_peer(descP->sock,
                  (struct sockaddr*) &remote, &addrLen)) < 0) {
        error = sock_errno();
    }
#endif

    if (error != 0) {
        *err = error;
        return FALSE;
    }
    return TRUE;
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_listen
 *
 * Description:
 * Listen for connections on a socket.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Backlog      - The maximum length to which the queue of pending
 *                connections for socket may grow.
 */
static
ERL_NIF_TERM nif_listen(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    int              backlog;
    ERL_NIF_TERM     ret;

    ESOCK_ASSERT( argc == 2 );

    SGDBG( ("SOCKET", "nif_listen -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }
    if (! GET_INT(env, argv[1], &backlog)) {
        if (IS_INTEGER(env, argv[1]))
            return esock_make_error_integer_range(env, argv[1]);
        else
            return enif_make_badarg(env);
    }

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "nif_listen(%T), {%d,0x%X} ->"
            "\r\n   backlog: %d"
            "\r\n",
            argv[0], descP->sock, descP->readState,
            backlog) );

    ret = esock_listen(env, descP, backlog);

    SSDBG( descP, ("SOCKET", "nif_listen(%T) -> done with"
                   "\r\n   ret: %T"
                   "\r\n", argv[0], ret) );

    MUNLOCK(descP->readMtx);

    return ret;
#endif // #ifdef __WIN32__  #else
}



#ifndef __WIN32__
static
ERL_NIF_TERM esock_listen(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          int              backlog)
{
    
    /*
     * Verify that we are in the proper state
     */

    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);

    /*
     * And attempt to make socket listening
     */
    
    if ((sock_listen(descP->sock, backlog)) < 0)
        return esock_make_error_errno(env, sock_errno());

    descP->readState |= ESOCK_STATE_LISTENING;

    return esock_atom_ok;

}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_accept
 *
 * Description:
 * Accept a connection on a socket.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Request ref  - Unique "id" of this request
 *                (used for the select, if none is in queue).
 */
static
ERL_NIF_TERM nif_accept(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else

    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, ref, res;

    ESOCK_ASSERT( argc == 2 );

    SGDBG( ("SOCKET", "nif_accept -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    sockRef = argv[0];
    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    ref = argv[1];

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "nif_accept%T), {%d,0x%X} ->"
            "\r\n   ReqRef:                %T"
            "\r\n   Current Acceptor addr: %p"
            "\r\n   Current Acceptor  pid: %T"
            "\r\n   Current Acceptor  mon: %T"
            "\r\n   Current Acceptor  env: 0x%lX"
            "\r\n   Current Acceptor  ref: %T"
            "\r\n",
            sockRef, descP->sock, descP->readState,
            ref,
            descP->currentAcceptorP,
            descP->currentAcceptor.pid,
            esock_make_monitor_term(env, &descP->currentAcceptor.mon),
            descP->currentAcceptor.env,
            descP->currentAcceptor.ref) );

    res = esock_accept(env, descP, sockRef, ref);

    SSDBG( descP, ("SOCKET", "nif_accept(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    MUNLOCK(descP->readMtx);

    return res;

#endif // #ifdef __WIN32__  #else
}


#ifndef __WIN32__
static
ERL_NIF_TERM esock_accept(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     accRef)
{
    ErlNifPid     caller;

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);

    /* Accept and Read uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentReaderP != NULL)
        return esock_make_error_invalid(env, atom_state);

    if (descP->currentAcceptorP == NULL) {
        SOCKET        accSock;

        /* We have no active acceptor (and therefore no acceptors in queue)
         */

        SSDBG( descP, ("SOCKET", "esock_accept {%d} -> try accept\r\n",
                       descP->sock) );

	ESOCK_CNT_INC(env, descP, sockRef, atom_acc_tries, &descP->accTries, 1);

        accSock = sock_accept(descP->sock, NULL, NULL);

        if (ESOCK_IS_ERROR(accSock)) {
            int           save_errno;

            save_errno = sock_errno();

            return esock_accept_listening_error(env, descP, sockRef,
                                                accRef, caller, save_errno);
        } else {
            /* We got an incoming connection */
            return
                esock_accept_listening_accept(env, descP, sockRef,
                                              accSock, caller);
        }
    } else {

        /* We have an active acceptor and possibly acceptors waiting in queue.
         * If the pid of the calling process is not the pid of the
	 * "current process", push the requester onto the (acceptor) queue.
         */

        SSDBG( descP, ("SOCKET", "esock_accept_accepting -> check: "
                       "is caller current acceptor:"
                       "\r\n   Caller:  %T"
                       "\r\n   Current: %T"
                       "\r\n", caller, descP->currentAcceptor.pid) );

        if (COMPARE_PIDS(&descP->currentAcceptor.pid, &caller) == 0) {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_accepting {%d} -> current acceptor\r\n",
                    descP->sock) );

            return esock_accept_accepting_current(env, descP, sockRef, accRef);

        } else {

            /* Not the "current acceptor", so (maybe) push onto queue */

            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_accepting {%d} -> *not* current acceptor\r\n",
                    descP->sock) );

            return esock_accept_accepting_other(env, descP, accRef, caller);
        }
    }
}
#endif // #ifndef __WIN32__

/* *** esock_accept_listening_error ***
 *
 * The accept call resultet in an error - handle it.
 * There are only two cases:
 * 1) BLOCK => Attempt a "retry"
 * 2) Other => Return the value (converted to an atom)
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_accept_listening_error(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     sockRef,
                                          ERL_NIF_TERM     accRef,
                                          ErlNifPid        caller,
                                          int              save_errno)
{
    ERL_NIF_TERM res;

    if (save_errno == ERRNO_BLOCK ||
        save_errno == EAGAIN) {

        /* *** Try again later *** */

        SSDBG( descP,
               ("SOCKET",
                "esock_accept_listening_error {%d} -> would block\r\n",
                descP->sock) );

	descP->currentAcceptor.pid = caller;
        ESOCK_ASSERT( MONP("esock_accept_listening -> current acceptor",
                           env, descP,
                           &descP->currentAcceptor.pid,
                           &descP->currentAcceptor.mon) == 0 );
        ESOCK_ASSERT( descP->currentAcceptor.env == NULL );
        descP->currentAcceptor.env = esock_alloc_env("current acceptor");
        descP->currentAcceptor.ref =
            CP_TERM(descP->currentAcceptor.env, accRef);
        descP->currentAcceptorP = &descP->currentAcceptor;
        res = esock_accept_busy_retry(env, descP, sockRef, accRef, NULL);
    } else {
        SSDBG( descP,
               ("SOCKET",
                "esock_accept_listening {%d} -> errno: %d\r\n",
                descP->sock, save_errno) );

        ESOCK_CNT_INC(env, descP, sockRef, atom_acc_fails, &descP->accFails, 1);

        res = esock_make_error_errno(env, save_errno);
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** esock_accept_listening_accept ***
 *
 * The accept call was successful (accepted) - handle the new connection.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_accept_listening_accept(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     sockRef,
                                           SOCKET           accSock,
                                           ErlNifPid        caller)
{
    ERL_NIF_TERM res;

    esock_accept_accepted(env, descP, sockRef, accSock, caller, &res);

    return res;
}
#endif // #ifndef __WIN32__


/* *** esock_accept_accepting_current ***
 * Handles when the current acceptor makes another attempt.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_accept_accepting_current(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     accRef)
{
    SOCKET        accSock;
    int           save_errno;
    ERL_NIF_TERM  res;

    SSDBG( descP,
           ("SOCKET",
            "esock_accept_accepting_current {%d} -> try accept\r\n",
            descP->sock) );

    ESOCK_CNT_INC(env, descP, sockRef, atom_acc_tries, &descP->accTries, 1);
	
    accSock = sock_accept(descP->sock, NULL, NULL);

    if (ESOCK_IS_ERROR(accSock)) {

        save_errno = sock_errno();

        res = esock_accept_accepting_current_error(env, descP, sockRef,
                                                   accRef, save_errno);
    } else {

        res = esock_accept_accepting_current_accept(env, descP, sockRef,
                                                    accSock);
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** esock_accept_accepting_current_accept ***
 * Handles when the current acceptor succeeded in its accept call -
 * handle the new connection.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_accept_accepting_current_accept(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     sockRef,
                                                   SOCKET           accSock)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET",
            "esock_accept_accepting_current_accept {%d}"
            "\r\n", descP->sock) );

    if (esock_accept_accepted(env, descP, sockRef, accSock,
                              descP->currentAcceptor.pid, &res)) {

        if (!activate_next_acceptor(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_accepting_current_accept {%d} ->"
                    " no more acceptors"
                    "\r\n", descP->sock) );

            descP->readState &= ~ESOCK_STATE_ACCEPTING;

            descP->currentAcceptorP = NULL;
        }

    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** esock_accept_accepting_current_error ***
 * The accept call of current acceptor resultet in an error - handle it.
 * There are only two cases:
 * 1) BLOCK => Attempt a "retry"
 * 2) Other => Return the value (converted to an atom)
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_accept_accepting_current_error(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     sockRef,
                                                  ERL_NIF_TERM     opRef,
                                                  int              save_errno)
{
    ERL_NIF_TERM   res, reason;

    if (save_errno == ERRNO_BLOCK ||
        save_errno == EAGAIN) {

        /*
         * Just try again, no real error, just a ghost trigger from poll,
         */

        SSDBG( descP,
               ("SOCKET",
                "esock_accept_accepting_current_error {%d} -> "
                "would block: try again\r\n", descP->sock) );

        ESOCK_CNT_INC(env, descP, sockRef, atom_acc_waits, &descP->accWaits, 1);

        res = esock_accept_busy_retry(env, descP, sockRef, opRef,
                                      &descP->currentAcceptor.pid);

    } else {
        ESockRequestor req;

        SSDBG( descP,
               ("SOCKET",
                "esock_accept_accepting_current_error {%d} -> "
                "error: %d\r\n", descP->sock, save_errno) );

        ESOCK_CNT_INC(env, descP, sockRef, atom_acc_fails, &descP->accFails, 1);

        requestor_release("esock_accept_accepting_current_error",
                          env, descP, &descP->currentAcceptor);

        reason = MKA(env, erl_errno_id(save_errno));
        res    = esock_make_error(env, reason);

        req.env = NULL;
        while (acceptor_pop(env, descP, &req)) {
            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_accepting_current_error {%d} -> abort %T\r\n",
                    descP->sock, req.pid) );

            esock_send_abort_msg(env, descP, sockRef, &req, reason);

            (void) DEMONP("esock_accept_accepting_current_error -> "
                          "pop'ed writer",
                          env, descP, &req.mon);
        }
        descP->currentAcceptorP = NULL;
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** esock_accept_accepting_other ***
 * Handles when the another acceptor makes an attempt, which
 * results (maybe) in the request beeing pushed onto the
 * acceptor queue.
 */
#ifndef __WIN32__
ERL_NIF_TERM
esock_accept_accepting_other(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ERL_NIF_TERM     ref,
                             ErlNifPid        caller)
{
    if (! acceptor_search4pid(env, descP, &caller)) {
        acceptor_push(env, descP, caller, ref);
	return atom_select;
    } else {
        /* Acceptor already in queue */
        return esock_raise_invalid(env, atom_state);
    }
}
#endif // #ifndef __WIN32__


/* *** esock_accept_busy_retry ***
 *
 * Perform a retry select. If successful, set nextState.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_accept_busy_retry(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     accRef,
                                     ErlNifPid*       pidP)
{
    int          sres;
    ERL_NIF_TERM res;

    if ((sres = esock_select_read(env, descP->sock, descP, pidP,
                                  sockRef, accRef)) < 0) {

        ESOCK_ASSERT( DEMONP("esock_accept_busy_retry - select failed",
                             env, descP, &descP->currentAcceptor.mon) == 0);
        /* It is very unlikely that a next acceptor will be able
         * to do anything succesful, but we will clean the queue
         */
        if (!activate_next_acceptor(env, descP, sockRef)) {
            SSDBG( descP,
                   ("SOCKET",
                    "esock_accept_busy_retry {%d} -> no more acceptors\r\n",
                    descP->sock) );

            descP->readState &= ~ESOCK_STATE_ACCEPTING;

            descP->currentAcceptorP = NULL;
        }

        res =
            enif_raise_exception(env,
                                 MKT2(env, atom_select_read,
                                      MKI(env, sres)));
    } else {
        descP->readState |=
            (ESOCK_STATE_ACCEPTING | ESOCK_STATE_SELECTED);
        res = atom_select;
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** esock_accept_accepted ***
 *
 * Generic function handling a successful accept.
 */
#ifndef __WIN32__
static
BOOLEAN_T esock_accept_accepted(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                SOCKET           accSock,
                                ErlNifPid        pid,
                                ERL_NIF_TERM*    result)
{
    ESockDescriptor* accDescP;
    ErlNifEvent      accEvent;
    ERL_NIF_TERM     accRef;
    int              save_errno;

    /*
     * We got one
     */

    ESOCK_CNT_INC(env, descP, sockRef, atom_acc_success, &descP->accSuccess, 1);

    if ((accEvent = sock_create_event(accSock)) == INVALID_EVENT) {
        save_errno = sock_errno();
        (void) sock_close(accSock);
        *result = esock_make_error_errno(env, save_errno);
        return FALSE;
    }

    accDescP           = alloc_descriptor(accSock, accEvent);
    accDescP->domain   = descP->domain;
    accDescP->type     = descP->type;
    accDescP->protocol = descP->protocol;

    MLOCK(descP->writeMtx);

    accDescP->rBufSz   = descP->rBufSz;  // Inherit buffer size
    accDescP->rNum     = descP->rNum;    // Inherit buffer uses
    accDescP->rNumCnt  = 0;
    accDescP->rCtrlSz  = descP->rCtrlSz; // Inherit buffer size
    accDescP->wCtrlSz  = descP->wCtrlSz; // Inherit buffer size
    accDescP->iow      = descP->iow;     // Inherit iow
    accDescP->dbg      = descP->dbg;     // Inherit debug flag
    accDescP->useReg   = descP->useReg;  // Inherit useReg flag
    inc_socket(accDescP->domain, accDescP->type, accDescP->protocol);

    accRef = enif_make_resource(env, accDescP);
    enif_release_resource(accDescP);

    accDescP->ctrlPid = pid;
    /* pid has actually been compared equal to self()
     * in this code path just a little while ago
     */
    ESOCK_ASSERT( MONP("esock_accept_accepted -> ctrl",
                       env, accDescP,
                       &accDescP->ctrlPid,
                       &accDescP->ctrlMon) == 0 );

    SET_NONBLOCKING(accDescP->sock);

    descP->writeState |= ESOCK_STATE_CONNECTED;

    MUNLOCK(descP->writeMtx);

    /* And finally (maybe) update the registry */
    if (descP->useReg) esock_send_reg_add_msg(env, descP, accRef);

    *result = esock_make_ok2(env, accRef);

    return TRUE;
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_send
 *
 * Description:
 * Send a message on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Bin          - The data to send as a binary()
 * Flags        - Send flags as an integer()
 * SendRef      - A unique id reference() for this (send) request.
 */

static
ERL_NIF_TERM nif_send(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, sendRef;
    ErlNifBinary     sndData;
    int              flags;
    ERL_NIF_TERM     res;

    ESOCK_ASSERT( argc == 4 );

    SGDBG( ("SOCKET", "nif_send -> entry with argc: %d\r\n", argc) );

    sockRef = argv[0]; // We need this in case we send in case we send abort
    sendRef = argv[3];

    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP)) {
        SGDBG( ("SOCKET", "nif_send -> get resource failed\r\n") );
        return enif_make_badarg(env);
    }

    /* Extract arguments and perform preliminary validation */

    if ((! enif_is_ref(env, sendRef)) ||
        (! GET_BIN(env, argv[1], &sndData))) {
        SSDBG( descP, ("SOCKET", "nif_send -> argv decode failed\r\n") );
        return enif_make_badarg(env);
    }
    if (! GET_INT(env, argv[2], &flags)) {
        SSDBG( descP, ("SOCKET", "nif_send -> argv decode failed\r\n") );
        if (IS_INTEGER(env, argv[2]))
            return esock_make_error_integer_range(env, argv[2]);
        else
            return enif_make_badarg(env);
    }
    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "nif_send(%T), {%d,0x%X} ->"
            "\r\n   SendRef:   %T"
            "\r\n   Data size: %u"
            "\r\n   flags:     0x%X"
            "\r\n",
            sockRef, descP->sock, descP->writeState,
            sendRef, sndData.size, flags) );

    /* We need to handle the case when another process tries
     * to write at the same time.
     * If the current write could not write its entire package
     * this time (resulting in an select). The write of the
     * other process must be made to wait until current
     * is done!
     */

    res = esock_send(env, descP, sockRef, sendRef, &sndData, flags);

    SSDBG( descP, ("SOCKET", "nif_send(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    MUNLOCK(descP->writeMtx);

    SGDBG( ("SOCKET", "nif_send -> done with result: "
            "\r\n   %T"
            "\r\n", res) );
    return res;

#endif // #ifdef __WIN32__  #else
}



/* *** esock_send ***
 *
 * Do the actual send.
 * Do some initial writer checks, do the actual send and then
 * analyze the result. If we are done, another writer may be
 * scheduled (if there is one in the writer queue).
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_send(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     sendRef,
                        ErlNifBinary*    sndDataP,
                        int              flags)
{
    ssize_t      send_result;
    ERL_NIF_TERM writerCheck;

    if (! IS_OPEN(descP->writeState))
        return esock_make_error(env, atom_closed);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, atom_state);

    send_result = (ssize_t) sndDataP->size;
    if ((size_t) send_result != sndDataP->size)
        return esock_make_error_invalid(env, atom_data_size);

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer  */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP, ("SOCKET", "esock_send {%d} -> writer check failed: "
                       "\r\n   %T\r\n", descP->sock, writerCheck) );
        return writerCheck;
    }
    
    ESOCK_CNT_INC(env, descP, sockRef, atom_write_tries, &descP->writeTries, 1);

    send_result =
        sock_send(descP->sock, sndDataP->data, sndDataP->size, flags);

    return send_check_result(env, descP,
                             send_result, sndDataP->size, FALSE,
                             sockRef, sendRef);

}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_sendto
 *
 * Description:
 * Send a message on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Bin          - The data to send as a binary()
 * Dest         - Destination (socket) address.
 * Flags        - Send flags as an integer().
 * SendRef      - A unique id reference() for this (send) request.
 */

static
ERL_NIF_TERM nif_sendto(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, sendRef;
    ErlNifBinary     sndData;
    int              flags;
    ERL_NIF_TERM     eSockAddr;
    ESockAddress     remoteAddr;
    SOCKLEN_T        remoteAddrLen;
    ERL_NIF_TERM     res;

    ESOCK_ASSERT( argc == 5 );

    SGDBG( ("SOCKET", "nif_sendto -> entry with argc: %d\r\n", argc) );

    sockRef   = argv[0]; // We need this in case we send abort (to the caller)
    sendRef   = argv[4];

    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP)) {
        SGDBG( ("SOCKET", "nif_sendto -> get resource failed\r\n") );
        return enif_make_badarg(env);
    }

    /* Extract arguments and perform preliminary validation */

    if ((! enif_is_ref(env, sendRef)) ||
        (! GET_BIN(env, argv[1], &sndData))) {
        SSDBG( descP, ("SOCKET", "nif_sendto -> argv decode failed\r\n") );
        return enif_make_badarg(env);
    }
    if (! GET_INT(env, argv[3], &flags)) {
        SSDBG( descP, ("SOCKET", "nif_sendto -> argv decode failed\r\n") );
        if (IS_INTEGER(env, argv[3]))
            return esock_make_error_integer_range(env, argv[3]);
        else
            return enif_make_badarg(env);
    }
    eSockAddr = argv[2];
    if (! esock_decode_sockaddr(env, eSockAddr,
                                &remoteAddr,
                                &remoteAddrLen)) {
        SSDBG( descP,
               ("SOCKET",
                "nif_sendto(%T), {%d} -> sockaddr decode failed \r\n",
                sockRef, descP->sock) );

        return esock_make_invalid(env, atom_sockaddr);
    }

    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "nif_sendto(%T), {%d,0x%X} ->"
            "\r\n   sendRef:   %T"
            "\r\n   Data size: %u"
            "\r\n   eSockAddr: %T"
            "\r\n   flags:     0x%X"
            "\r\n",
            sockRef, descP->sock, descP->readState,
            sendRef, sndData.size, eSockAddr, flags) );

    res = esock_sendto(env, descP, sockRef, sendRef, &sndData, flags,
                       &remoteAddr, remoteAddrLen);

    SSDBG( descP, ("SOCKET", "nif_sendto(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    MUNLOCK(descP->writeMtx);

    return res;

#endif // if defined(__WIN32__)
}


#ifndef __WIN32__
static
ERL_NIF_TERM esock_sendto(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     sendRef,
                          ErlNifBinary*    dataP,
                          int              flags,
                          ESockAddress*    toAddrP,
                          SOCKLEN_T        toAddrLen)
{
    ssize_t      sendto_result;
    ERL_NIF_TERM writerCheck;

    if (! IS_OPEN(descP->writeState))
        return esock_make_error(env, atom_closed);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, atom_state);

    sendto_result = (ssize_t) dataP->size;
    if ((size_t) sendto_result != dataP->size)
        return esock_make_error_invalid(env, atom_data_size);

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer  */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP, ("SOCKET", "esock_sendto {%d} -> writer check failed: "
                       "\r\n   %T\r\n", descP->sock, writerCheck) );
        return writerCheck;
    }
    
    ESOCK_CNT_INC(env, descP, sockRef, atom_write_tries, &descP->writeTries, 1);

    if (toAddrP != NULL) {
        sendto_result =
            sock_sendto(descP->sock,
                        dataP->data, dataP->size, flags,
                        &toAddrP->sa, toAddrLen);
    } else {
        sendto_result =
            sock_sendto(descP->sock,
                        dataP->data, dataP->size, flags,
                        NULL, 0);
    }

    return send_check_result(env, descP, sendto_result, dataP->size, FALSE,
                             sockRef, sendRef);
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_sendmsg
 *
 * Description:
 * Send a message on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Msg          - Message - map() with data and (maybe) control and dest
 * Flags        - Send flags as an integer().
 * SendRef      - A unique id reference() for this (send) request.
 */

static
ERL_NIF_TERM nif_sendmsg(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM     res, sockRef, sendRef, eMsg, eIOV;
    ESockDescriptor* descP;
    int              flags;

    ESOCK_ASSERT( argc == 5 );

    SGDBG( ("SOCKET", "nif_sendmsg -> entry with argc: %d\r\n", argc) );

    sockRef = argv[0]; // We need this in case we send abort (to the caller)
    eMsg    = argv[1];
    sendRef = argv[3];
    eIOV    = argv[4];

    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP)) {
        SGDBG( ("SOCKET", "nif_sendmsg -> get resource failed\r\n") );
        return enif_make_badarg(env);
    }

    /* Extract arguments and perform preliminary validation */

    if ((! enif_is_ref(env, sendRef)) ||
        (! IS_MAP(env, eMsg))) {
        SSDBG( descP, ("SOCKET", "nif_sendmsg -> argv decode failed\r\n") );
        return enif_make_badarg(env);
    }
    if (! GET_INT(env, argv[2], &flags)) {
        if (IS_INTEGER(env, argv[2]))
            return esock_make_error_integer_range(env, argv[2]);
        else
            return enif_make_badarg(env);
    }

    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "nif_sendmsg(%T), {%d,0x%X} ->"
            "\r\n   SendRef:   %T"
            "\r\n   flags:     0x%X"
            "\r\n",
            sockRef, descP->sock, descP->writeState,
            sendRef, flags) );

    res = esock_sendmsg(env, descP, sockRef, sendRef, eMsg, flags, eIOV);

    MUNLOCK(descP->writeMtx);

    SSDBG( descP, ("SOCKET", "nif_sendmsg(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    return res;

#endif // #ifdef __WIN32__  #else
}


#ifndef __WIN32__
static
ERL_NIF_TERM esock_sendmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     sendRef,
                           ERL_NIF_TERM     eMsg,
                           int              flags,
                           ERL_NIF_TERM     eIOV)
{
    ERL_NIF_TERM  res, eAddr, eCtrl;
    ESockAddress  addr;
    struct msghdr msgHdr;
    ErlNifIOVec  *iovec = NULL;
    char*         ctrlBuf;
    size_t        ctrlBufLen,  ctrlBufUsed;
    ssize_t       dataSize,    sendmsg_result;
    ERL_NIF_TERM  writerCheck, tail;

    if (! IS_OPEN(descP->writeState))
        return esock_make_error(env, atom_closed);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, atom_state);

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer  */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP,
               ("SOCKET", "esock_sendmsg {%d} -> writer check failed: "
                "\r\n   %T\r\n", descP->sock, writerCheck) );
        return writerCheck;
    }

    /* Initiate the .name and .namelen fields depending on if
     * we have an address or not
     */
    if (! GET_MAP_VAL(env, eMsg, esock_atom_addr, &eAddr)) {

        SSDBG( descP, ("SOCKET",
                       "esock_sendmsg {%d} -> no address\r\n", descP->sock) );

        msgHdr.msg_name    = NULL;
        msgHdr.msg_namelen = 0;
    } else {
        msgHdr.msg_name    = (void*) &addr;
        msgHdr.msg_namelen = sizeof(addr);
        sys_memzero((char *) msgHdr.msg_name, msgHdr.msg_namelen);

        SSDBG( descP, ("SOCKET", "esock_sendmsg {%d} ->"
                       "\r\n   address: %T"
                       "\r\n", descP->sock, eAddr) );

        if (! esock_decode_sockaddr(env, eAddr,
                                    msgHdr.msg_name,
                                    &msgHdr.msg_namelen)) {
            SSDBG( descP, ("SOCKET",
                           "esock_sendmsg {%d} -> invalid address\r\n",
                           descP->sock) );
            return esock_make_invalid(env, esock_atom_addr);
        }
    }

    /* Extract the *mandatory* 'iov', which must be an erlang:iovec(),
     * from which we take at most IOV_MAX binaries
     */
    if ((! enif_inspect_iovec(NULL, data.iov_max, eIOV, &tail, &iovec))) {
        SSDBG( descP, ("SOCKET",
                       "esock_sendmsg {%d} -> not an iov\r\n",
                       descP->sock) );

        return esock_make_invalid(env, esock_atom_iov);
    }

    SSDBG( descP, ("SOCKET", "esock_sendmsg {%d} ->"
                   "\r\n   iovcnt: %lu"
                   "\r\n   tail:   %s"
                   "\r\n", descP->sock,
                   (unsigned long) iovec->iovcnt,
                   B2S(! enif_is_empty_list(env, tail))) );

    /* We now have an allocated iovec */

    eCtrl             = esock_atom_undefined;
    ctrlBufLen        = 0;
    ctrlBuf           = NULL;

    if (iovec->iovcnt > data.iov_max) {
        if (descP->type == SOCK_STREAM) {
            iovec->iovcnt = data.iov_max;
        } else {
            /* We can not send the whole packet in one sendmsg() call */
            SSDBG( descP, ("SOCKET",
                           "esock_sendmsg {%d} -> iovcnt > iov_max\r\n",
                           descP->sock) );
            res = esock_make_invalid(env, esock_atom_iov);
            goto done_free_iovec;
        }
    }

    dataSize = 0;
    {
        ERL_NIF_TERM h,   t;
        ErlNifBinary bin;
        size_t       i;

        /* Find out if there is remaining data in the tail.
         * Skip empty binaries otherwise break.
         * If 'tail' after loop exit is the empty list
         * there was no more data.  Otherwise there is more
         * data or the 'iov' is invalid.
         */
        for (;;) {
            if (enif_get_list_cell(env, tail, &h, &t) &&
                enif_inspect_binary(env, h, &bin) &&
                (bin.size == 0)) {
                tail = t;
                continue;
            } else
                break;
        }

        if ((! enif_is_empty_list(env, tail)) &&
            (descP->type != SOCK_STREAM)) {
            /* We can not send the whole packet in one sendmsg() call */
            SSDBG( descP, ("SOCKET",
                           "esock_sendmsg {%d} -> invalid tail\r\n",
                           descP->sock) );
            res = esock_make_invalid(env, esock_atom_iov);
            goto done_free_iovec;
        }

        /* Calculate the data size */

        for (i = 0;  i < iovec->iovcnt;  i++) {
            size_t len = iovec->iov[i].iov_len;
            dataSize += len;
            if (dataSize < len) {
                /* Overflow */
                SSDBG( descP, ("SOCKET", "esock_sendmsg {%d} -> Overflow"
                               "\r\n   i:         %lu"
                               "\r\n   len:       %lu"
                               "\r\n   dataSize:  %ld"
                               "\r\n", descP->sock, (unsigned long) i,
                               (unsigned long) len, (long) dataSize) );
                res = esock_make_invalid(env, esock_atom_iov);
                goto done_free_iovec;
            }
        }
    }
    SSDBG( descP,
           ("SOCKET",
            "esock_sendmsg {%d} ->"
            "\r\n   iov length: %lu"
            "\r\n   data size:  %u"
            "\r\n",
            descP->sock,
            (unsigned long) iovec->iovcnt, (long) dataSize) );

    msgHdr.msg_iovlen = iovec->iovcnt;
    msgHdr.msg_iov    = iovec->iov;

    /* Extract the *optional* 'ctrl' */
    if (GET_MAP_VAL(env, eMsg, esock_atom_ctrl, &eCtrl)) {
        ctrlBufLen = descP->wCtrlSz;
        ctrlBuf    = (char*) MALLOC(ctrlBufLen);
        ESOCK_ASSERT( ctrlBuf != NULL );
    }
    SSDBG( descP, ("SOCKET", "esock_sendmsg {%d} -> optional ctrl: "
                   "\r\n   ctrlBuf:    %p"
                   "\r\n   ctrlBufLen: %lu"
                   "\r\n   eCtrl:      %T"
                   "\r\n", descP->sock,
                   ctrlBuf, (unsigned long) ctrlBufLen, eCtrl) );

    /* Decode the ctrl and initiate that part of the msghdr.
     */
    if (ctrlBuf != NULL) {
        if (! decode_cmsghdrs(env, descP,
                              eCtrl,
                              ctrlBuf, ctrlBufLen, &ctrlBufUsed)) {
            SSDBG( descP, ("SOCKET",
                           "esock_sendmsg {%d} -> invalid ctrl\r\n",
                           descP->sock) );
            res = esock_make_invalid(env, esock_atom_ctrl);
            goto done_free_iovec;
        }
    } else {
        ctrlBufUsed = 0;
    }
    msgHdr.msg_control    = ctrlBuf;
    msgHdr.msg_controllen = ctrlBufUsed;

    /* The msg_flags field is not used when sending,
     * but zero it just in case */
    msgHdr.msg_flags      = 0;

    ESOCK_CNT_INC(env, descP, sockRef, atom_write_tries, &descP->writeTries, 1);

    /* And now, try to send the message */
    sendmsg_result = sock_sendmsg(descP->sock, &msgHdr, flags);

    res = send_check_result(env, descP, sendmsg_result, dataSize,
                            (! enif_is_empty_list(env, tail)),
                            sockRef, sendRef);

 done_free_iovec:
    enif_free_iovec(iovec);
    if (ctrlBuf != NULL) FREE(ctrlBuf);

    SSDBG( descP,
           ("SOCKET", "esock_sendmsg {%d} ->"
            "\r\n   %T\r\n", descP->sock, res) );
    return res;
}
#endif // #ifndef __WIN32__



#ifdef FOOBAR

/* ----------------------------------------------------------------------
 * nif_writev / nif_sendv
 *
 * Description:
 * Send a message (vector) on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * SendRef      - A unique id for this (send) request.
 * Data         - A vector of binaries
 * Flags        - Send flags.
 */

static
ERL_NIF_TERM nwritev(ErlNifEnv*       env,
                     ESockDescriptor* descP,
                     ERL_NIF_TERM     sendRef,
                     ERL_NIF_TERM     data)
{
    ERL_NIF_TERM tail;
    ErlNifIOVec  vec;
    ErlNifIOVec* iovec = &vec;
    SysIOVec*    sysiovec;
    int          save_errno;
    int          iovcnt, n;

    if (! enif_inspect_iovec(env, MAX_VSZ, data, &tail, &iovec))
        return enif_make_badarg(env);

    if (enif_ioq_size(descP->outQ) > 0) {
        /* If the I/O queue contains data we enqueue the iovec
         * and then peek the data to write out of the queue.
         */
        if (!enif_ioq_enqv(q, iovec, 0))
            return -3;

        sysiovec = enif_ioq_peek(descP->outQ, &iovcnt);

    } else {
        /* If the I/O queue is empty we skip the trip through it. */
        iovcnt   = iovec->iovcnt;
        sysiovec = iovec->iov;
    }

    /* Attempt to write the data */
    n = writev(fd, sysiovec, iovcnt);
    saved_errno = errno;

    if (enif_ioq_size(descP->outQ) == 0) {
        /* If the I/O queue was initially empty we enqueue any
           remaining data into the queue for writing later. */
        if (n >= 0 && !enif_ioq_enqv(descP->outQ, iovec, n))
            return -3;
    } else {
        /* Dequeue any data that was written from the queue. */
        if (n > 0 && !enif_ioq_deq(descP->outQ, n, NULL))
            return -4;
    }
    /* return n, which is either number of bytes written or -1 if
       some error happened */
    errno = saved_errno;
    return n;
}

#endif // #ifdef FOOBAR



/* ----------------------------------------------------------------------
 * nif_sendfile/1,4,5
 *
 * Description:
 * Send a file on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 *
 * SendRef      - A unique id reference() for this (send) request.
 *
 * Offset       - File offset to start from.
 * Count        - The number of bytes to send.
 *
 * InFileRef    - A file NIF resource.
 */

static ERL_NIF_TERM
nif_sendfile(ErlNifEnv*         env,
             int                argc,
             const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__) || !defined(HAVE_SENDFILE)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor       *descP;
    ERL_NIF_TERM           sockRef, res;

    SGDBG( ("SOCKET", "nif_sendfile -> entry with argc: %d\r\n", argc) );

    if (argc < 1) {
        SGDBG( ("SOCKET", "nif_sendfile -> argc < 1\r\n") );
        return enif_make_badarg(env);
    }
    sockRef = argv[0];
    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) (&descP))) {
        SGDBG( ("SOCKET", "nif_sendfile -> get resource failed\r\n") );
        return enif_make_badarg(env);
    }

    if (argc < 2) { // argc == 1

        MLOCK(descP->writeMtx);

        SSDBG( descP,
               ("SOCKET", "nif_sendfile(%T), {%d,%d,0x%X} ->"
                "\r\n",
                sockRef,
                descP->sock, descP->sendfileHandle, descP->writeState) );

        res = esock_sendfile_deferred_close(env, descP);

    } else {
        ERL_NIF_TERM sendRef;
        ErlNifSInt64 offset64;
        ErlNifUInt64 count64u;
        off_t        offset;
        size_t       count;
        BOOLEAN_T    a2ok;

        ESOCK_ASSERT( argc >= 4 );

        sendRef = argv[1];
        if ((! enif_is_ref(env, sendRef))) {
            SSDBG( descP,
                   ("SOCKET", "nif_sendfile -> argv[1] decode failed\r\n") );
            return enif_make_badarg(env);
        }

        if ((! (a2ok = GET_INT64(env, argv[2], &offset64))) ||
            (! GET_UINT64(env, argv[3], &count64u))) {
            if ((! IS_INTEGER(env, argv[3])) ||
                (! IS_INTEGER(env, argv[3])))
                return enif_make_badarg(env);
            if (! a2ok)
                return esock_make_error_integer_range(env, argv[2]);
            else
                return esock_make_error_integer_range(env, argv[3]);
        }
        offset = (off_t) offset64;
        if (offset64 != (ErlNifSInt64) offset)
            return esock_make_error_integer_range(env, argv[2]);
        count = (size_t) count64u;
        if (count64u != (ErlNifUInt64) count)
            return esock_make_error_integer_range(env, argv[3]);

        if (argc == 4) {

            MLOCK(descP->writeMtx);

            SSDBG( descP,
                   ("SOCKET", "nif_sendfile(%T), {%d,0x%X} ->"
                    "\r\n   sendRef: %T"
                    "\r\n   offset:  %ld"
                    "\r\n   count:   %ld"
                    "\r\n",
                    sockRef, descP->sock, descP->readState,
                    sendRef, (long) offset, (long) count) );

            res =
                esock_sendfile_cont(env, descP, sockRef, sendRef,
                                    offset, count);
        } else {
            ERL_NIF_TERM  fRef;

            ESOCK_ASSERT( argc == 5 );

            fRef = argv[4];
            if ((! enif_is_ref(env, fRef)))
                return enif_make_badarg(env);

            MLOCK(descP->writeMtx);

            SSDBG( descP,
                   ("SOCKET", "nif_sendfile(%T), {%d,0x%X} ->"
                    "\r\n   sendRef: %T"
                    "\r\n   offset:  %ld"
                "\r\n   count:   %ld"
                    "\r\n   fRef:    %T"
                    "\r\n",
                    sockRef, descP->sock, descP->readState,
                    sendRef, (long) offset, (long) count, fRef) );

            res =
                esock_sendfile_start(env, descP, sockRef, sendRef,
                                     offset, count, fRef);
        }
    }

    SSDBG( descP, ("SOCKET", "nif_sendfile(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    MUNLOCK(descP->writeMtx);

    return res;

#endif // #if defined(__WIN32__) || !defined(HAVE_SENDFILE)
}

#ifndef __WIN32__
#ifdef HAVE_SENDFILE

/* Start a sendfile() operation
 */
static ERL_NIF_TERM
esock_sendfile_start(ErlNifEnv       *env,
                     ESockDescriptor *descP,
                     ERL_NIF_TERM     sockRef,
                     ERL_NIF_TERM     sendRef,
                     off_t            offset,
                     size_t           count,
                     ERL_NIF_TERM     fRef) {
    ERL_NIF_TERM           writerCheck;
    ssize_t                res;
    int                    err;

    SSDBG( descP, ("SOCKET",
                   "esock_sendfile_start(%T) {%d} -> sendRef: %T\r\n"
                   "   fRef:   %T\r\n"
                   "   offset: %lu\r\n"
                   "   count:  %lu\r\n",
                   sockRef, descP->sock, sendRef,
                   fRef, (unsigned long) offset, (unsigned long) count) );

    if (! IS_OPEN(descP->writeState)) {
        return esock_make_error(env, atom_closed);
    }

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL) {
        return esock_make_error_invalid(env, atom_state);
    }

    /* Ensure that we either have no current writer or we are it,
     * or enqueue this process if there is a current writer
     */
    if (! send_check_writer(env, descP, sendRef, &writerCheck)) {
        SSDBG( descP, ("SOCKET",
                       "esock_sendfile_start {%d} -> writer check failed: "
                       "\r\n   %T\r\n", descP->sock, writerCheck) );

        /* Returns 'select' if current process got enqueued,
         * or exception invalid state if current process already
         * was enqueued
         */
        return writerCheck;
    }

    if (descP->sendfileHandle != INVALID_HANDLE)
        return esock_make_error_invalid(env, atom_state);

    /* Get a dup:ed file handle from prim_file_nif
     * through a NIF dyncall
     */
    {
        struct prim_file_nif_dyncall_dup dc_dup;

        dc_dup.op = prim_file_nif_dyncall_dup;
        dc_dup.result = EINVAL; // should not be needed

        /* Request the handle */
        if (enif_dynamic_resource_call(env,
                                       atom_prim_file, atom_efile, fRef,
                                       &dc_dup)
            != 0) {
            return
                esock_sendfile_error(env, descP, sockRef,
                                     MKT2(env, esock_atom_invalid,
                                          atom_efile));
        }
        if (dc_dup.result != 0) {
            return
                esock_sendfile_errno(env, descP, sockRef, dc_dup.result);
        }
        descP->sendfileHandle = dc_dup.handle;
    }

    SSDBG( descP, ("SOCKET",
                   "esock_sendfile_start(%T) {%d} -> sendRef: %T\r\n"
                   "   sendfileHandle: %d\r\n",
                   sockRef, descP->sock, sendRef,
                   descP->sendfileHandle) );

    if (descP->sendfileCountersP == NULL) {
        descP->sendfileCountersP = MALLOC(sizeof(ESockSendfileCounters));
        *descP->sendfileCountersP = initESockSendfileCounters;
    }

    ESOCK_CNT_INC(env, descP, sockRef,
                  atom_sendfile_tries, &descP->sendfileCountersP->tries, 1);
    descP->sendfileCountersP->maxCnt = 0;

    res = esock_sendfile(env, descP, sockRef, offset, &count, &err);

    if (res < 0) { // Terminal error

        (void) close(descP->sendfileHandle);
        descP->sendfileHandle = INVALID_HANDLE;

        return esock_sendfile_errno(env, descP, sockRef, err);

    } else if (res > 0) { // Retry by select

        if (descP->currentWriterP == NULL) {
            int mon_res;

            /* Register writer as current */
            ESOCK_ASSERT( enif_self(env, &descP->currentWriter.pid) != NULL );
            mon_res =
                MONP("sendfile -> current writer",
                     env, descP,
                     &descP->currentWriter.pid,
                     &descP->currentWriter.mon);
            ESOCK_ASSERT( mon_res >= 0 );

            if (mon_res > 0) {
                /* Caller died already, can happen for dirty NIFs */

                (void) close(descP->sendfileHandle);
                descP->sendfileHandle = INVALID_HANDLE;

                return
                    esock_sendfile_error(env, descP, sockRef,
                                         MKT2(env,
                                              esock_atom_invalid,
                                              esock_atom_not_owner));
            }
            ESOCK_ASSERT( descP->currentWriter.env == NULL );
            descP->currentWriter.env = esock_alloc_env("current-writer");
            descP->currentWriter.ref =
                CP_TERM(descP->currentWriter.env, sendRef);
            descP->currentWriterP = &descP->currentWriter;
        }
        // else current writer is already registered by requestor_pop()

        return esock_sendfile_select(env, descP, sockRef, sendRef, count);

    } else { // res == 0: Done
        return esock_sendfile_ok(env, descP, sockRef, count);
    }
}

/* Continue an ongoing sendfile operation
 */
static ERL_NIF_TERM
esock_sendfile_cont(ErlNifEnv       *env,
                    ESockDescriptor *descP,
                    ERL_NIF_TERM     sockRef,
                    ERL_NIF_TERM     sendRef,
                    off_t            offset,
                    size_t           count) {
    ErlNifPid              caller;
    ssize_t                res;
    int                    err;

    SSDBG( descP, ("SOCKET",
                   "esock_sendfile_cont(%T) {%d} -> sendRef: %T\r\n",
                   sockRef, descP->sock, sendRef) );

    if (! IS_OPEN(descP->writeState))
        return esock_make_error(env, atom_closed);

    /* Connect and Write uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->connectorP != NULL)
        return esock_make_error_invalid(env, atom_state);

    /* Verify that this process has a sendfile operation in progress */
    ESOCK_ASSERT( enif_self(env, &caller) != NULL );
    if ((descP->currentWriterP == NULL) ||
        (descP->sendfileHandle == INVALID_HANDLE) ||
        (COMPARE_PIDS(&descP->currentWriter.pid, &caller) != 0)) {
        //
        return esock_raise_invalid(env, atom_state);
    }

    res = esock_sendfile(env, descP, sockRef, offset, &count, &err);

    if (res < 0) { // Terminal error

        (void) close(descP->sendfileHandle);
        descP->sendfileHandle = INVALID_HANDLE;

        return esock_sendfile_errno(env, descP, sockRef, err);

     } else if (res > 0) { // Retry by select

        /* Overwrite current writer registration */
        enif_clear_env(descP->currentWriter.env);
        descP->currentWriter.ref =
            CP_TERM(descP->currentWriter.env, sendRef);

        return esock_sendfile_select(env, descP, sockRef, sendRef, count);

    } else { // res == 0: Done
        return esock_sendfile_ok(env, descP, sockRef, count);
    }
}

/* Deferred close of the dup:ed file descriptor
 */
static ERL_NIF_TERM
esock_sendfile_deferred_close(ErlNifEnv       *env,
                              ESockDescriptor *descP) {
    if (descP->sendfileHandle == INVALID_HANDLE)
        return esock_make_error_invalid(env, atom_state);

    (void) close(descP->sendfileHandle);
    descP->sendfileHandle = INVALID_HANDLE;

    return esock_atom_ok;
}

/* Platform independent sendfile() function
 *
 * Return < 0  for terminal error
 *        0    for done
 *        > 0  for retry with select
 */
static int
esock_sendfile(ErlNifEnv       *env,
               ESockDescriptor *descP,
               ERL_NIF_TERM     sockRef,
               off_t            offset,
               size_t          *countP,
               int             *errP) {

    size_t pkgSize = 0; // Total sent in this call

    SSDBG( descP, ("SOCKET",
                   "esock_sendfile(%T) {%d,%d}\r\n",
                   sockRef, descP->sock, descP->sendfileHandle) );

    for (;;) {
        size_t  chunk_size = (size_t) 0x20000000UL; // 0.5 GB
        size_t  bytes_sent;
        ssize_t res;
        int     error;

        /* *countP == 0 means send the whole file - use chunk size */
        if ((*countP > 0) && (*countP < chunk_size))
            chunk_size = *countP;

        {
            /* Platform dependent code:
             *   update and check offset, set and check bytes_sent, and
             *       set res to >= 0 and error to 0, or
             *       set res to < 0 and error to sock_errno()
             */
#if defined (__linux__)

            off_t prev_offset;

            prev_offset = offset;
            res =
                sendfile(descP->sock, descP->sendfileHandle,
                         &offset, chunk_size);
            error = (res < 0) ? sock_errno() : 0;

            ESOCK_ASSERT( offset >= prev_offset );
            ESOCK_ASSERT( (off_t) chunk_size >= (offset - prev_offset) );
            bytes_sent = (size_t) (offset - prev_offset);

            SSDBG( descP,
                   ("SOCKET",
                    "esock_sendfile(%T) {%d,%d}"
                    "\r\n   res:         %d"
                    "\r\n   bytes_sent:  %lu"
                    "\r\n   error:       %d"
                    "\r\n",
                    sockRef, descP->sock, descP->sendfileHandle,
                    res, (unsigned long) bytes_sent, error) );

#elif defined(__FreeBSD__) || defined(__DragonFly__) || defined(__DARWIN__)

            off_t sbytes;

#if defined(__DARWIN__)
            sbytes = (off_t) chunk_size;
            res = (ssize_t)
                sendfile(descP->sendfileHandle, descP->sock, offset,
                         &sbytes, NULL, 0);
#else
            sbytes = 0;
            res = (ssize_t)
                sendfile(descP->sendfileHandle, descP->sock, offset,
                         chunk_size, NULL, &sbytes, 0);
#endif
            error = (res < 0) ? sock_errno() : 0;

            /* For an error return, we do not dare trust that sbytes is set
             * unless the error is ERRNO_BLOCK or EINTR
             * - the man page is to vague
             */
            if ((res < 0) && (error != ERRNO_BLOCK) && (error != EINTR)) {
                sbytes = 0;
            } else {
                ESOCK_ASSERT( sbytes >= 0 );
                ESOCK_ASSERT( (off_t) chunk_size >= sbytes );
                ESOCK_ASSERT( offset + sbytes >= offset );
                offset += sbytes;
            }
            bytes_sent = (size_t) sbytes;

            SSDBG( descP,
                   ("SOCKET",
                    "esock_sendfile(%T) {%d,%d}"
                    "\r\n   res:         %d"
                    "\r\n   bytes_sent:  %lu"
                    "\r\n   error:       %d"
                    "\r\n",
                    sockRef, descP->sock, descP->sendfileHandle,
                    res, (unsigned long) bytes_sent, error) );

#elif defined(__sun) && defined(__SVR4) && defined(HAVE_SENDFILEV)

            sendfilevec_t sfvec[1];

            sfvec[0].sfv_fd = descP->sendfileHandle;
            sfvec[0].sfv_flag = 0;
            sfvec[0].sfv_off = offset;
            sfvec[0].sfv_len = chunk_size;

            res = sendfilev(descP->sock, sfvec, NUM(sfvec), &bytes_sent);
            error = (res < 0) ? sock_errno() : 0;

            SSDBG( descP,
                   ("SOCKET",
                    "esock_sendfile(%T) {%d,%d}"
                    "\r\n   res:         %d"
                    "\r\n   bytes_sent:  %lu"
                    "\r\n   error:       %d"
                    "\r\n",
                    sockRef, descP->sock, descP->sendfileHandle,
                    res, (unsigned long) bytes_sent, error) );

            if ((res < 0) && (error == EINVAL)) {
                /* On e.b SunOS 5.10 using sfv_len > file size
                 * lands here - we regard this as a succesful send.
                 * All other causes for EINVAL are avoided,
                 * except for .sfv_fd not seekable, which would
                 * give bytes_sent == 0 that we would interpret
                 * as end of file, which is kind of true.
                 */
                res = 0;
            }
            ESOCK_ASSERT( chunk_size >= bytes_sent );
            ESOCK_ASSERT( offset + bytes_sent >= offset );
            offset += bytes_sent;

#else
#error "Unsupported sendfile syscall; update configure test."
#endif

            ESOCK_CNT_INC(env, descP, sockRef,
                          atom_sendfile, &descP->sendfileCountersP->cnt, 1);

            if (bytes_sent != 0) {

                pkgSize += bytes_sent;

                ESOCK_CNT_INC(env, descP, sockRef,
                              atom_sendfile_pkg,
                              &descP->sendfileCountersP->pkg,
                              1);
                ESOCK_CNT_INC(env, descP, sockRef,
                              atom_sendfile_byte,
                              &descP->sendfileCountersP->byteCnt,
                              bytes_sent);

                if (pkgSize > descP->sendfileCountersP->pkgMax)
                    descP->sendfileCountersP->pkgMax = pkgSize;
                if ((descP->sendfileCountersP->maxCnt += bytes_sent)
                    > descP->sendfileCountersP->max)
                    descP->sendfileCountersP->max =
                        descP->sendfileCountersP->maxCnt;
            }

            /* *countP == 0 means send whole file */
            if (*countP > 0) {

                *countP -= bytes_sent;

                if (*countP == 0) { // All sent
                    *countP = pkgSize;
                    return 0;
                }
            }

            if (res < 0) {
                if (error == ERRNO_BLOCK) {
                    *countP = pkgSize;
                    return 1;
                }
                if (error == EINTR)
                    continue;
                *errP = error;
                return -1;
            }

            if (bytes_sent == 0) { // End of input file
                *countP = pkgSize;
                return 0;
            }
        }
    } // for (;;)
}

static ERL_NIF_TERM
esock_sendfile_errno(ErlNifEnv             *env,
                     ESockDescriptor       *descP,
                     ERL_NIF_TERM           sockRef,
                     int                    err) {
    ERL_NIF_TERM reason;

    reason = MKA(env, erl_errno_id(err));
    return esock_sendfile_error(env, descP, sockRef, reason);
}

static ERL_NIF_TERM
esock_sendfile_error(ErlNifEnv             *env,
                     ESockDescriptor       *descP,
                     ERL_NIF_TERM           sockRef,
                     ERL_NIF_TERM           reason) {

    if (descP->sendfileCountersP == NULL) {
        descP->sendfileCountersP = MALLOC(sizeof(ESockSendfileCounters));
        *descP->sendfileCountersP = initESockSendfileCounters;
    }

    ESOCK_CNT_INC(env, descP, sockRef,
                  atom_sendfile_fails,
                  &descP->sendfileCountersP->fails, 1);

    SSDBG( descP, ("SOCKET",
                   "esock_sendfile_error(%T) {%d} -> error: %T\r\n",
                   sockRef, descP->sock, reason) );

    /* XXX Should we have special treatment for EINVAL,
     * such as to only fail current operation and activate
     * the next from the queue?
     */

    if (descP->currentWriterP != NULL) {

        (void) DEMONP("esock_sendfile_error",
                      env, descP, &descP->currentWriter.mon);

        /* Fail all queued writers */
        requestor_release("esock_sendfile_error",
                          env, descP, &descP->currentWriter);
        send_error_waiting_writers(env, descP, sockRef, reason);
        descP->currentWriterP = NULL;

    }

    return esock_make_error(env, reason);
}

static ERL_NIF_TERM
esock_sendfile_select(ErlNifEnv       *env,
                      ESockDescriptor *descP,
                      ERL_NIF_TERM     sockRef,
                      ERL_NIF_TERM     sendRef,
                      size_t           count) {
    int sres;

    /* Select write for this process */
    sres =
        esock_select_write(env, descP->sock, descP, NULL, sockRef, sendRef);
    if (sres < 0) {
        ERL_NIF_TERM reason;

        /* Internal select error */
        (void) DEMONP("esock_sendfile_select - failed",
                      env, descP, &descP->currentWriter.mon);

        /* Fail all queued writers */
        reason = MKT2(env, atom_select_write, MKI(env, sres));
        requestor_release("esock_sendfile_select_fail",
                          env, descP, &descP->currentWriter);
        send_error_waiting_writers(env, descP, sockRef, reason);
        descP->currentWriterP = NULL;

        (void) close(descP->sendfileHandle);
        descP->sendfileHandle = INVALID_HANDLE;

        return enif_raise_exception(env, reason);

    } else {
        ErlNifUInt64 bytes_sent;

        SSDBG( descP,
               ("SOCKET", "esock_sendfile_select(%T) {%d} -> "
                "sendRef (%T)\r\n"
                "count:  %lu\r\n",
                sockRef, descP->sock, sendRef, (unsigned long) count) );

        ESOCK_CNT_INC(env, descP, sockRef,
                      atom_sendfile_waits,
                      &descP->sendfileCountersP->waits,
                      1);
        descP->writeState |= ESOCK_STATE_SELECTED;
        bytes_sent = (ErlNifUInt64) count;

        return MKT2(env, atom_select, MKUI64(env, bytes_sent));
    }
}

static ERL_NIF_TERM
esock_sendfile_ok(ErlNifEnv       *env,
                  ESockDescriptor *descP,
                  ERL_NIF_TERM     sockRef,
                  size_t           count) {
    ErlNifUInt64 bytes_sent64u;

    SSDBG( descP,
           ("SOCKET", "esock_sendfile_ok(%T) {%d} -> "
            "everything written (%lu) - done\r\n",
            sockRef, descP->sock, (unsigned long) count) );

    if (descP->currentWriterP != NULL) {

        (void) DEMONP("esock_sendfile_ok -> current writer",
                      env, descP, &descP->currentWriter.mon);

        /*
         * Ok, this write is done maybe activate the next (if any)
         */
        if (! activate_next_writer(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_sendfile_ok(%T) {%d} -> no more writers\r\n",
                    sockRef, descP->sock) );

            descP->currentWriterP = NULL;
        }
    }

    descP->writePkgMaxCnt = 0;
    bytes_sent64u = (ErlNifUInt64) count;

    (void) close(descP->sendfileHandle);
    descP->sendfileHandle = INVALID_HANDLE;

    return esock_make_ok2(env, MKUI64(env, bytes_sent64u));
}

#endif // #ifdef HAVE_SENDFILE
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_recv
 *
 * Description:
 * Receive a message on a socket.
 * Normally used only on a connected socket!
 * If we are trying to read > 0 bytes, then that is what we do.
 * But if we have specified 0 bytes, then we want to read
 * whatever is in the buffers (everything it got).
 *
 * Arguments:
 * Socket (ref) - NIF resource reference() to the socket descriptor.
 * Length       - The number of bytes to receive; integer().
 * Flags        - Receive flags; integer().
 * RecvRef      - A unique reference() id for this (send) request | 'poll'
 */

static
ERL_NIF_TERM nif_recv(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, recvRef;
    ErlNifUInt64     elen;
    ssize_t          len; /* ssize_t due to the return type of recv() */
    int              flags;
    ERL_NIF_TERM     res;
    BOOLEAN_T        a1ok;

    ESOCK_ASSERT( argc == 4 );

    sockRef = argv[0]; // We need this in case we send abort (to the caller)
    recvRef = argv[3];

    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    if ((! enif_is_ref(env, recvRef)) &&
        (COMPARE(recvRef, atom_zero) != 0)) {
        return enif_make_badarg(env);
    }
    if ((! (a1ok = GET_UINT64(env, argv[1], &elen))) ||
        (! GET_INT(env, argv[2], &flags))) {
        if ((! IS_INTEGER(env, argv[1])) ||
            (! IS_INTEGER(env, argv[2])))
            return enif_make_badarg(env);

        if (! a1ok)
            return esock_make_error_integer_range(env, argv[1]);
        return
            esock_make_error_integer_range(env, argv[2]);
    }
    len = (ssize_t) elen;
    if (elen != (ErlNifUInt64) len)
        return esock_make_error_integer_range(env, elen);

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "nif_recv(%T), {%d,0x%X} ->"
            "\r\n   recvRef: %T"
            "\r\n   len:     %ld"
            "\r\n   flags:   0x%X"
            "\r\n",
            sockRef, descP->sock, descP->readState,
            recvRef, (long) len, flags) );

    /* We need to handle the case when another process tries
     * to receive at the same time.
     * If the current recv could not read its entire package
     * this time (resulting in an select). The read of the
     * other process must be made to wait until current
     * is done!
     */

    res = esock_recv(env, descP, sockRef, recvRef, len, flags);

    SSDBG( descP, ("SOCKET", "nif_recv(%T) -> done"
                   "\r\n", sockRef) );

    MUNLOCK(descP->readMtx);

    return res;

#endif // #ifdef __WIN32__  #else
}


/* The (read) buffer handling should be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_recv(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     recvRef,
                        ssize_t          len,
                        int              flags)
{
    ssize_t      read;
    ErlNifBinary buf;
    ERL_NIF_TERM readerCheck;
    int          save_errno;
    size_t       bufSz = (len != 0 ? len : descP->rBufSz);

    SSDBG( descP, ("SOCKET", "esock_recv {%d} -> entry with"
                   "\r\n   count,size: (%ld:%u:%lu)"
                   "\r\n", descP->sock,
                   (long) len, descP->rNumCnt, (unsigned long) bufSz) );

    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);

    /* Accept and Read uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentAcceptorP != NULL)
        return esock_make_error_invalid(env, atom_state);

    /* Ensure that we either have no current reader or that we are it,
     * or enqueue this process if there is a current reader */
    if (! recv_check_reader(env, descP, recvRef, &readerCheck)) {
        SSDBG( descP,
               ("SOCKET", "esock_recv {%d} -> reader check failed: "
                "\r\n   %T\r\n", descP->sock, readerCheck) );
        return readerCheck;
    }

    /* Allocate a buffer:
     * Either as much as we want to read or (if zero (0)) use the "default"
     * size (what has been configured).
     */
    ESOCK_ASSERT( ALLOC_BIN(bufSz, &buf) );

    // If it fails (read = -1), we need errno...
    SSDBG( descP, ("SOCKET", "esock_recv {%d} -> try read (%lu)\r\n",
                   descP->sock, (unsigned long) buf.size) );

    ESOCK_CNT_INC(env, descP, sockRef, atom_read_tries, &descP->readTries, 1);

    read = sock_recv(descP->sock, buf.data, buf.size, flags);
    if (ESOCK_IS_ERROR(read)) {
        save_errno = sock_errno();
    } else {
        save_errno = 0; // The value does not actually matter in this case
    }

    SSDBG( descP, ("SOCKET",
                   "esock_recv {%d} -> read: %ld (%d)\r\n",
                   descP->sock, (long) read, save_errno) );

    return recv_check_result(env, descP, read, len, save_errno,
                             &buf, sockRef, recvRef);
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_recvfrom
 *
 * Description:
 * Receive a message on a socket.
 * Normally used only on a (un-) connected socket!
 * If a buffer size = 0 is specified, then the we will use the default
 * buffer size for this socket (whatever has been configured).
 *
 * Arguments:
 * Socket (ref) - NIF resource reference() to the socket descriptor.
 * BufSz        - integer() ize of the buffer
 *                into which we put the received message.
 * Flags        - Receive flags; integer().
 * RecvRef      - A unique reference() id for this recv request.
 *
 * <KOLLA>
 *
 * How do we handle if the peek flag is set? We need to basically keep
 * track of if we expect any data from the read. Regardless of the
 * number of bytes we try to read.
 *
 * </KOLLA>
 */

static
ERL_NIF_TERM nif_recvfrom(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, recvRef;
    ErlNifUInt64     elen;
    ssize_t          len; /* ssize_t due to the return type of recvfrom() */
    int              flags;
    ERL_NIF_TERM     res;
    BOOLEAN_T        a1ok;

    ESOCK_ASSERT( argc == 4 );

    SGDBG( ("SOCKET", "nif_recvfrom -> entry with argc: %d\r\n", argc) );

    sockRef = argv[0]; // We need this in case we send abort (to the caller)
    recvRef = argv[3];

    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    /* Extract arguments and perform preliminary validation */

    if ((! enif_is_ref(env, recvRef)) &&
        (COMPARE(recvRef, atom_zero) != 0)) {
        return enif_make_badarg(env);
    }

    if ((! (a1ok = GET_UINT64(env, argv[1], &elen))) ||
        (! GET_INT(env, argv[2], &flags))) {
        if ((! IS_INTEGER(env, argv[1])) ||
            (! IS_INTEGER(env, argv[2])))
            return enif_make_badarg(env);

        if (! a1ok)
            return esock_make_error_integer_range(env, argv[1]);
        return
            esock_make_error_integer_range(env, argv[2]);
    }
    len = (ssize_t) elen;
    if (elen != (ErlNifUInt64) len)
        return esock_make_error_integer_range(env, elen);

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "nif_recvfrom(%T), {%d,0x%X} ->"
            "\r\n   recvRef: %T"
            "\r\n   len:     %ld"
            "\r\n   flags:   0x%X"
            "\r\n",
            sockRef, descP->sock, descP->readState,
            recvRef, (long) len, flags) );

    /* <KOLLA>
     * We need to handle the case when another process tries
     * to receive at the same time.
     * If the current recv could not read its entire package
     * this time (resulting in an select). The read of the
     * other process must be made to wait until current
     * is done!
     * Basically, we need a read queue!
     *
     * A 'reading' field (boolean), which is set if we did
     * not manage to read the entire message and reset every
     * time we do.
     * </KOLLA>
     */

    res = esock_recvfrom(env, descP, sockRef, recvRef, len, flags);

    SSDBG( descP, ("SOCKET", "nif_recvfrom(%T) -> done"
                   "\r\n", sockRef) );

    MUNLOCK(descP->readMtx);

    return res;
#endif // #ifdef __WIN32__  #else
}


/* The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_recvfrom(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     recvRef,
                            ssize_t          len,
                            int              flags)
{
    ESockAddress  fromAddr;
    SOCKLEN_T     addrLen;
    ssize_t       read;
    int           save_errno;
    ErlNifBinary  buf;
    ERL_NIF_TERM  readerCheck;
    size_t        bufSz = (len != 0 ? len : descP->rBufSz);

    SSDBG( descP, ("SOCKET", "esock_recvfrom {%d} -> entry with"
                   "\r\n   bufSz: %d"
                   "\r\n", descP->sock, bufSz) );

    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);

    /* Accept and Read uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentAcceptorP != NULL)
        return esock_make_error_invalid(env, atom_state);

    /* Ensure that we either have no current reader or that we are it,
     * or enqueue this process if there is a current reader */
    if (! recv_check_reader(env, descP, recvRef, &readerCheck)) {
        SSDBG( descP,
               ("SOCKET", "esock_recv {%d} -> reader check failed: "
                "\r\n   %T\r\n", descP->sock, readerCheck) );
        return readerCheck;
    }

    /* Allocate a buffer:
     * Either as much as we want to read or (if zero (0)) use the "default"
     * size (what has been configured).
     */
    ESOCK_ASSERT( ALLOC_BIN(bufSz, &buf) );

    ESOCK_CNT_INC(env, descP, sockRef, atom_read_tries, &descP->readTries, 1);

    addrLen = sizeof(fromAddr);
    sys_memzero((char*) &fromAddr, addrLen);

    read = sock_recvfrom(descP->sock, buf.data, buf.size, flags,
                         &fromAddr.sa, &addrLen);
    if (ESOCK_IS_ERROR(read))
        save_errno = sock_errno();
    else
        save_errno = 0; // The value does not actually matter in this case

    return recvfrom_check_result(env, descP, read, save_errno,
                                 &buf, &fromAddr, addrLen,
                                 sockRef, recvRef);
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_recvmsg
 *
 * Description:
 * Receive a message on a socket.
 * Normally used only on a (un-) connected socket!
 * If a buffer size = 0 is specified, then we will use the default
 * buffer size for this socket (whatever has been configured).
 * If ctrl (buffer) size = 0 is specified, then the default ctrl
 * (buffer) size is used (1024).
 *
 * Arguments:
 * Socket (ref) - NIF resource reference() to the socket descriptor.
 * BufSz        - Size of the buffer into which we put the received message;
 *                integer().
 * CtrlSz       - Size of the ctrl (buffer) into which we put the received
 *                ancillary data; integer().
 * Flags        - Receive flags; integer().
 * RecvRef      - A unique reference() id for this (send) request.
 *
 * <KOLLA>
 *
 * How do we handle if the peek flag is set? We need to basically keep
 * track of if we expect any data from the read. Regardless of the
 * number of bytes we try to read.
 *
 * </KOLLA>
 */

static
ERL_NIF_TERM nif_recvmsg(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, recvRef;
    ErlNifUInt64     eBufSz,  eCtrlSz;
    ssize_t          bufSz,   ctrlSz;
    int              flags;
    ERL_NIF_TERM     res;
    BOOLEAN_T        a1ok, a2ok;

    ESOCK_ASSERT( argc == 5 );

    SGDBG( ("SOCKET", "nif_recvmsg -> entry with argc: %d\r\n", argc) );

    sockRef = argv[0]; // We need this in case we send abort (to the caller)
    recvRef = argv[4];

    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    /* Extract arguments and perform preliminary validation */

    if ((! enif_is_ref(env, recvRef)) &&
        (COMPARE(recvRef, atom_zero) != 0)) {
        return enif_make_badarg(env);
    }

    if ((! (a1ok = GET_UINT64(env, argv[1], &eBufSz))) ||
        (! (a2ok = GET_UINT64(env, argv[2], &eCtrlSz))) ||
        (! GET_INT(env, argv[3], &flags))) {
        if ((! IS_INTEGER(env, argv[1])) ||
            (! IS_INTEGER(env, argv[2])) ||
            (! IS_INTEGER(env, argv[3])))
            return enif_make_badarg(env);

        if (! a1ok)
            return esock_make_error_integer_range(env, argv[1]);
        if (! a2ok)
            return esock_make_error_integer_range(env, argv[2]);
        return
            esock_make_error_integer_range(env, argv[3]);
    }

    bufSz  = (ssize_t) eBufSz;
    if (eBufSz  != (ErlNifUInt64) bufSz)
        return esock_make_error_integer_range(env, eBufSz);

    ctrlSz = (ssize_t) eCtrlSz;
    if (eCtrlSz != (ErlNifUInt64) ctrlSz)
        return esock_make_error_integer_range(env, eCtrlSz);

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "nif_recvmsg(%T), {%d,0x%X} ->"
            "\r\n   recvRef: %T"
            "\r\n   bufSz:   %ld"
            "\r\n   ctrlSz:  %ld"
            "\r\n   flags:   0x%X"
            "\r\n",
            sockRef, descP->sock, descP->readState,
            recvRef, (long) bufSz, (long) ctrlSz, flags) );

    /* <KOLLA>
     *
     * We need to handle the case when another process tries
     * to receive at the same time.
     * If the current recv could not read its entire package
     * this time (resulting in an select). The read of the
     * other process must be made to wait until current
     * is done!
     * Basically, we need a read queue!
     *
     * A 'reading' field (boolean), which is set if we did
     * not manage to read the entire message and reset every
     * time we do.
     *
     * </KOLLA>
     */

    res = esock_recvmsg(env, descP, sockRef, recvRef, bufSz, ctrlSz, flags);

    SSDBG( descP, ("SOCKET", "nif_recvmsg(%T) -> done"
                   "\r\n", sockRef) );

    MUNLOCK(descP->readMtx);

    return res;
#endif // #ifdef __WIN32__  #else
}


/* The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_recvmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     recvRef,
                           ssize_t          bufLen,
                           ssize_t          ctrlLen,
                           int              flags)
{
    SOCKLEN_T     addrLen;
    ssize_t       read;
    int           save_errno;
    size_t        bufSz  = (bufLen  != 0 ? bufLen  : descP->rBufSz);
    size_t        ctrlSz = (ctrlLen != 0 ? ctrlLen : descP->rCtrlSz);
    struct msghdr msgHdr;
    struct iovec  iov[1];  // Shall we always use 1?
    ErlNifBinary  data[1]; // Shall we always use 1?
    ErlNifBinary  ctrl;
    ERL_NIF_TERM  readerCheck;
    ESockAddress  addr;

    SSDBG( descP, ("SOCKET", "esock_recvmsg {%d} -> entry with"
                   "\r\n   bufSz:  %lu (%ld)"
                   "\r\n   ctrlSz: %ld (%ld)"
                   "\r\n", descP->sock,
                   (unsigned long) bufSz, (long) bufLen,
                   (unsigned long) ctrlSz, (long) ctrlLen) );

    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);

    /* Accept and Read uses the same select flag
     * so they can not be simultaneous
     */
    if (descP->currentAcceptorP != NULL)
        return esock_make_error_invalid(env, atom_state);

    /* Ensure that we either have no current reader or that we are it,
     * or enqueue this process if there is a current reader */
    if (! recv_check_reader(env, descP, recvRef, &readerCheck)) {
        SSDBG( descP,
               ("SOCKET", "esock_recv {%d} -> reader check failed: "
                "\r\n   %T\r\n", descP->sock, readerCheck) );
        return readerCheck;
    }

    /*
    for (i = 0; i < sizeof(buf); i++) {
        ESOCK_ASSERT( ALLOC_BIN(bifSz, &buf[i]) );
        iov[i].iov_base = buf[i].data;
        iov[i].iov_len  = buf[i].size;
    }
    */

    /* Allocate the (msg) data buffer:
     */
    ESOCK_ASSERT( ALLOC_BIN(bufSz, &data[0]) );

    /* Allocate the ctrl (buffer):
     */
    ESOCK_ASSERT( ALLOC_BIN(ctrlSz, &ctrl) );

    ESOCK_CNT_INC(env, descP, sockRef, atom_read_tries, &descP->readTries, 1);

    addrLen = sizeof(addr);
    sys_memzero((char*) &addr,   addrLen);
    sys_memzero((char*) &msgHdr, sizeof(msgHdr));

    iov[0].iov_base = data[0].data;
    iov[0].iov_len  = data[0].size;

    msgHdr.msg_name       = &addr;
    msgHdr.msg_namelen    = addrLen;
    msgHdr.msg_iov        = iov;
    msgHdr.msg_iovlen     = 1; // Should use a constant or calculate...
    msgHdr.msg_control    = ctrl.data;
    msgHdr.msg_controllen = ctrl.size;

    read = sock_recvmsg(descP->sock, &msgHdr, flags);
    if (ESOCK_IS_ERROR(read))
        save_errno = sock_errno();
    else
        save_errno = 0; // The value does not actually matter in this case

    return recvmsg_check_result(env, descP, read, save_errno,
                                &msgHdr,
                                data,  // Needed for iov encode
                                &ctrl, // Needed for ctrl header encode
                                sockRef, recvRef);
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_close
 *
 * Description:
 * Close a (socket) file descriptor.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 */

static
ERL_NIF_TERM nif_close(ErlNifEnv*         env,
                       int                argc,
                       const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM res;

    ESOCK_ASSERT( argc == 1 );

    SGDBG( ("SOCKET", "nif_close -> entry with argc: %d\r\n", argc) );

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }

    MLOCK(descP->readMtx);
    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "nif_close(%T), {%d,0x%X}\r\n",
            argv[0], descP->sock, descP->readState) );

    res = esock_close(env, descP);

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    SSDBG( descP, ("SOCKET", "nif_close(%T) -> done"
                   "\r\n   res: %T"
                   "\r\n", argv[0], res) );

    return res;
#endif // #ifdef __WIN32__  #else
}


#ifndef __WIN32__
static
ERL_NIF_TERM esock_close(ErlNifEnv*       env,
                         ESockDescriptor* descP)
{
    if (! IS_OPEN(descP->readState)) {
        /* A bit of cheeting; maybe not closed yet - do we need a queue? */
        return esock_make_error(env, atom_closed);
    }

    /* Store the PID of the caller,
     * since we need to inform it when we
     * (that is, the stop callback function)
     * completes.
     */
    ESOCK_ASSERT( enif_self(env, &descP->closerPid) != NULL );

    /* If the caller is not the owner; monitor the caller,
     * since we should complete this operation even if the caller dies
     * (for whatever reason).
     */
    if (COMPARE_PIDS(&descP->closerPid, &descP->ctrlPid) != 0) {

        ESOCK_ASSERT( MONP("esock_close_check -> closer",
                           env, descP,
                           &descP->closerPid,
                           &descP->closerMon) == 0 );
    }

    /* Prepare for closing the socket */
    descP->readState  |= ESOCK_STATE_CLOSING;
    descP->writeState |= ESOCK_STATE_CLOSING;
    if (esock_do_stop(env, descP)) {
        // esock_stop() has been scheduled - wait for it
        SSDBG( descP,
               ("SOCKET", "esock_close {%d} -> stop was scheduled\r\n",
                descP->sock) );

        // Create closeRef for the close msg that esock_stop() will send
        descP->closeEnv = esock_alloc_env("esock_close_do - close-env");
        descP->closeRef = MKREF(descP->closeEnv);

        return esock_make_ok2(env, CP_TERM(env, descP->closeRef));
    } else {
        // The socket may be closed - tell caller to finalize
        SSDBG( descP,
               ("SOCKET",
                "esock_close {%d} -> stop was called\r\n",
                descP->sock) );

        return esock_atom_ok;
    }
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
/* Prepare for close - return whether stop is scheduled
 */
static
BOOLEAN_T esock_do_stop(ErlNifEnv* env,
                        ESockDescriptor* descP) {
    BOOLEAN_T    ret;
    int          sres;
    ERL_NIF_TERM sockRef;

    sockRef = enif_make_resource(env, descP);

    if (IS_SELECTED(descP)) {
        ESOCK_ASSERT( (sres = esock_select_stop(env, descP->sock, descP))
                      >= 0 );
        if ((sres & ERL_NIF_SELECT_STOP_CALLED) != 0) {
            /* The socket is no longer known by the select machinery
             * - it may be closed
             */
            ret = FALSE;
        } else {
            ESOCK_ASSERT( (sres & ERL_NIF_SELECT_STOP_SCHEDULED) != 0 );
            /* esock_stop() is scheduled
             * - socket may be removed by esock_stop() or later
             */
            ret = TRUE;
        }
    } else {
        sres = 0;
        /* The socket has never been used in the select machinery
         * - it may be closed
         */
        ret = FALSE;
    }

    /* +++++++ Current and waiting Writers +++++++ */

    if (descP->currentWriterP != NULL) {

        /* We have a current Writer; was it deselected?
         */

        if (sres & ERL_NIF_SELECT_WRITE_CANCELLED) {

            /* The current Writer will not get a select message
             * - send it an abort message
             */

            esock_stop_handle_current(env,
                                      "writer",
                                      descP, sockRef, &descP->currentWriter);
        }

        /* Inform the waiting Writers (in the same way) */

        SSDBG( descP,
               ("SOCKET",
                "esock_do_stop {%d} -> handle waiting writer(s)\r\n",
                descP->sock) );

        inform_waiting_procs(env, "writer",
                             descP, sockRef, &descP->writersQ, atom_closed);

        descP->currentWriterP = NULL;
    }

    /* +++++++ Connector +++++++
     * Note that there should not be Writers and a Connector
     * at the same time so the check for if the
     * current Writer/Connecter was deselected is only correct
     * under that assumption
     */

    if (descP->connectorP != NULL) {

        /* We have a Connector; was it deselected?
         */

        if (sres & ERL_NIF_SELECT_WRITE_CANCELLED) {

            /* The Connector will not get a select message
             * - send it an abort message
             */

            esock_stop_handle_current(env,
                                      "connector",
                                      descP, sockRef, &descP->connector);
        }

        descP->connectorP = NULL;
    }

    /* +++++++ Current and waiting Readers +++++++ */

    if (descP->currentReaderP != NULL) {

        /* We have a current Reader; was it deselected?
         */

        if (sres & ERL_NIF_SELECT_READ_CANCELLED) {

            /* The current Reader will not get a select message
             * - send it an abort message
             */

            esock_stop_handle_current(env,
                                      "reader",
                                      descP, sockRef, &descP->currentReader);
        }

        /* Inform the Readers (in the same way) */

        SSDBG( descP,
               ("SOCKET",
                "esock_do_stop {%d} -> handle waiting reader(s)\r\n",
                descP->sock) );

        inform_waiting_procs(env, "writer",
                             descP, sockRef, &descP->readersQ, atom_closed);

        descP->currentReaderP = NULL;
    }

    /* +++++++ Current and waiting Acceptors +++++++
     *
     * Note that there should not be Readers and Acceptors
     * at the same time so the check for if the
     * current Reader/Acceptor was deselected is only correct
     * under that assumption
     */

    if (descP->currentAcceptorP != NULL) {

        /* We have a current Acceptor; was it deselected?
         */

        if (sres & ERL_NIF_SELECT_READ_CANCELLED) {

            /* The current Acceptor will not get a select message
             * - send it an abort message
             */

            esock_stop_handle_current(env,
                                      "acceptor",
                                      descP, sockRef, &descP->currentAcceptor);
        }

        /* Inform the waiting Acceptor (in the same way) */

        SSDBG( descP,
               ("SOCKET",
                "esock_do_stop {%d} -> handle waiting acceptors(s)\r\n",
                descP->sock) );

        inform_waiting_procs(env, "acceptor",
                             descP, sockRef, &descP->acceptorsQ, atom_closed);

        descP->currentAcceptorP = NULL;
    }

    return ret;
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_finalize_close
 *
 * Description:
 * Perform the actual socket close!
 * Note that this function is executed in a dirty scheduler.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 */
static
ERL_NIF_TERM nif_finalize_close(ErlNifEnv*         env,
                                int                argc,
                                const ERL_NIF_TERM argv[])
{
    ESockDescriptor* descP;
    ERL_NIF_TERM result;
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else

    /* Extract arguments and perform preliminary validation */

    ESOCK_ASSERT( argc == 1 );

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }

    MLOCK(descP->readMtx);
    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "nif_finalize_close(%T), {%d,0x%X}\r\n",
            argv[0], descP->sock, descP->readState) );

    result = esock_finalize_close(env, descP);

    SSDBG( descP, ("SOCKET", "nif_finalize_close(%T) -> done with"
                   "\r\n   result: %T"
                   "\r\n", argv[0], result) );

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    return result;
#endif // #ifdef __WIN32__  #else
}


/* *** esock_finalize_close ***
 * Perform the final step in the socket close.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_finalize_close(ErlNifEnv*       env,
                                  ESockDescriptor* descP)
{
    int err;
    ErlNifPid self;
#ifdef HAVE_SENDFILE
    HANDLE sendfileHandle;
#endif

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    if (IS_CLOSED(descP->readState))
        return esock_make_error(env, atom_closed);

    if (! IS_CLOSING(descP->readState)) {
        // esock_close() has not been called
        return esock_raise_invalid(env, atom_state);
    }

    if (IS_SELECTED(descP) && (descP->closeEnv != NULL)) {
        // esock_stop() is scheduled but has not been called
        return esock_raise_invalid(env, atom_state);
    }

    if (COMPARE_PIDS(&descP->closerPid, &self) != 0) {
        // This process is not the closer
        return esock_raise_invalid(env, atom_state);
    }

    // Close the socket

    /* Stop monitoring the closer.
     * Demonitoring may fail since this is a dirty NIF
     * - the caller may have died already.
     */
    enif_set_pid_undefined(&descP->closerPid);
    if (descP->closerMon.isActive) {
        (void) DEMONP("esock_finalize_close -> closer",
                      env, descP, &descP->closerMon);
    }

    /* Stop monitoring the owner */
    enif_set_pid_undefined(&descP->ctrlPid);
    (void) DEMONP("esock_finalize_close -> ctrl",
                  env, descP, &descP->ctrlMon);
    /* Not impossible to still get a esock_down() call from a
     * just triggered owner monitor down
     */

#ifdef HAVE_SENDFILE
    sendfileHandle = descP->sendfileHandle;
    descP->sendfileHandle = INVALID_HANDLE;
#endif

    /* This nif is executed in a dirty scheduler just so that
     * it can "hang" (whith minumum effect on the VM) while the
     * kernel writes our buffers. IF we have set the linger option
     * for this ({true, integer() > 0}). For this to work we must
     * be blocking...
     */
    SET_BLOCKING(descP->sock);
    err = esock_close_socket(env, descP, TRUE);

#ifdef HAVE_SENDFILE
    if (sendfileHandle != INVALID_HANDLE) {
        (void) close(descP->sendfileHandle);
    }
#endif

    if (err != 0) {
        if (err == ERRNO_BLOCK) {
            /* Not all data in the buffers where sent,
             * make sure the caller gets this.
             */
            return esock_make_error(env, atom_timeout);
        } else {
            return esock_make_error_errno(env, err);
        }
    }

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static int esock_close_socket(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              BOOLEAN_T        unlock) {
    int          err      = 0;
    SOCKET       sock     = descP->sock;
    ERL_NIF_TERM sockRef;

    /* This code follows Linux's advice to assume that after calling
     * close(2), the file descriptor may be reused, so assuming
     * that it can be used for anything such as retrying
     * to close is bad behaviour, although odd platforms
     * such as HP-UX requires a retry after EINTR
     */

    /* First update the state so no other thread will try
     * to close the socket, then we will close it,
     * possibly when being scheduled in during
     * finalize_close
     */
    descP->sock        = INVALID_SOCKET;
    descP->event       = INVALID_EVENT;
    descP->readState  |= ESOCK_STATE_CLOSED;
    descP->writeState |= ESOCK_STATE_CLOSED;
    dec_socket(descP->domain, descP->type, descP->protocol);

    /* +++++++ Clear the meta option +++++++ */
    enif_clear_env(descP->meta.env);
    descP->meta.ref = esock_atom_undefined;

    sock_close_event(descP->event);
    if (descP->closeOnClose) {
        if (unlock) {
            MUNLOCK(descP->writeMtx);
            MUNLOCK(descP->readMtx);
        }
        if (sock_close(sock) != 0)
            err = sock_errno();
        if (unlock) {
            MLOCK(descP->readMtx);
            MLOCK(descP->writeMtx);
        }
    }

    if (err != 0) {
        SSDBG( descP,
               ("SOCKET", "esock_close_socket {%d} -> %d\r\n",
                sock, err) );
    }

    /* (maybe) Update the registry */
    if (descP->useReg) {
        sockRef = enif_make_resource(env, descP);
        esock_send_reg_del_msg(env, descP, sockRef);
    }

    return err;
}
#endif // #ifndef __WIN32__


/* ----------------------------------------------------------------------
 * nif_shutdown
 *
 * Description:
 * Disable sends and/or receives on a socket.
 *
 * Arguments:
 * [0] Socket (ref) - Points to the socket descriptor.
 * [1] How          - What will be shutdown.
 */

static
ERL_NIF_TERM nif_shutdown(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     ehow, res;
    int              how;

    ESOCK_ASSERT( argc == 2 );

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }
    ehow = argv[1];

    if (! ehow2how(ehow, &how))
        return esock_raise_invalid(env,
                                   MKT2(env, atom_how, ehow));

    MLOCK(descP->readMtx);
    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "nif_shutdown(%T), {%d,0x%X} ->"
            "\r\n   how: %d"
            "\r\n",
            argv[0], descP->sock, descP->readState | descP->writeState,
            how) );

    res = esock_shutdown(env, descP, how);

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    SSDBG( descP, ("SOCKET", "nif_shutdown(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", argv[0], res) );

    return res;
#endif // #ifdef __WIN32__  #else
}



#ifndef __WIN32__
static
ERL_NIF_TERM esock_shutdown(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            int              how)
{
    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);

    if (sock_shutdown(descP->sock, how) == 0)
        return esock_atom_ok;
    else
        return esock_make_error_errno(env, sock_errno());
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_setopt
 *
 * Description:
 * Set socket option.
 * It is possible to use a native mode value where we do not use
 * any assumption about how to encode the value but instead
 * use the value's type to select encoding.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Level        - Level of the socket option.
 * Opt          - The socket option.
 * Value        - Value of the socket option.
 * NativeValue  - If 0 Value type has to match our encoding function,
 *                if not 0 type selects encoding.
 */

static
ERL_NIF_TERM nif_setopt(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP = NULL;
    int              level, opt, nativeValue;
    ERL_NIF_TERM     eVal;

    ESOCK_ASSERT( argc == 5 );

    SGDBG( ("SOCKET", "nif_setopt -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) ||
        (! GET_INT(env, argv[4], &nativeValue))) {
        //
        SGDBG( ("SOCKET", "nif_setopt -> failed initial arg check\r\n") );
        return enif_make_badarg(env);
    }
    if (! GET_INT(env, argv[2], &opt)) {
        SSDBG( descP,
               ("SOCKET", "nif_setopt -> failed initial arg check\r\n") );
        if (! IS_INTEGER(env, argv[2]))
            return enif_make_badarg(env);
        else
            return esock_make_error_integer_range(env, argv[2]);
    }
    eVal = argv[3];

    if (esock_decode_level(env, argv[1], &level)) {
        if (nativeValue == 0)
            return esock_setopt(env, descP, level, opt, eVal);
        else
            return esock_setopt_native(env, descP, level, opt, eVal);
    }

    if (COMPARE(argv[1], atom_otp) == 0) {
        if (nativeValue == 0) {
            return esock_setopt_otp(env, descP, opt, eVal);
        } else {
            SSDBG( descP, ("SOCKET", "nif_setopt -> failed arg check\r\n") );
            return enif_make_badarg(env);
        }
    }

    SGDBG( ("SOCKET", "nif_setopt -> failed arg check\r\n") );

    if (IS_INTEGER(env, argv[1]))
        return esock_make_error_integer_range(env, argv[1]);
    else
        return enif_make_badarg(env);
#endif // #ifdef __WIN32__  #else
}


/* esock_setopt_otp - Handle OTP (level) options
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              eOpt,
                              ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
    case ESOCK_OPT_OTP_DEBUG:
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);
        result = esock_setopt_otp_debug(env, descP, eVal);
        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_IOW:
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);
        result = esock_setopt_otp_iow(env, descP, eVal);
        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_CTRL_PROC:
        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);
        result = esock_setopt_otp_ctrl_proc(env, descP, eVal);
        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_RCVBUF:
        MLOCK(descP->readMtx);
        result = esock_setopt_otp_rcvbuf(env, descP, eVal);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_RCVCTRLBUF:
        MLOCK(descP->readMtx);
        result = esock_setopt_otp_rcvctrlbuf(env, descP, eVal);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_SNDCTRLBUF:
        MLOCK(descP->writeMtx);
        result = esock_setopt_otp_sndctrlbuf(env, descP, eVal);
        MUNLOCK(descP->writeMtx);
        break;

    case ESOCK_OPT_OTP_META:
        MLOCK(descP->writeMtx);
        result = esock_setopt_otp_meta(env, descP, eVal);
        MUNLOCK(descP->writeMtx);
        break;

    case ESOCK_OPT_OTP_USE_REGISTRY:
        MLOCK(descP->writeMtx);
        result = esock_setopt_otp_use_registry(env, descP, eVal);
        MUNLOCK(descP->writeMtx);
        break;

    default:
        MLOCK(descP->writeMtx);
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp {%d} -> invalid with"
                "\r\n   eOpt: %d"
                "\r\n   eVal: %T"
                "\r\n", descP->sock, eOpt, eVal) );
        MUNLOCK(descP->writeMtx);

        /* This is an internal error - prim_inet gave us junk */
        result =
            esock_raise_invalid(env,
                                MKT2(env,
                                     atom_otp_socket_option,
                                     MKI(env, eOpt)));
        break;
    }

    return result;
}
#endif // #ifndef __WIN32__



/* esock_setopt_otp_debug - Handle the OTP (level) debug options
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp_debug(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_debug {%d} -> closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    if (! esock_decode_bool(eVal, &descP->dbg))
        return esock_make_invalid(env, atom_value);

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_debug {%d} -> ok"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* esock_setopt_otp_iow - Handle the OTP (level) iow options
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp_iow(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     eVal)
{
    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_iow {%d} -> closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    if (! esock_decode_bool(eVal, &descP->iow))
      return esock_make_invalid(env, atom_value);

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_iow {%d} -> ok"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* esock_setopt_otp_ctrl_proc - Handle the OTP (level)
 * controlling_process options
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp_ctrl_proc(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    ErlNifPid     caller, newCtrlPid;
    int           xres;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_ctrl_proc {%d} -> entry"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_ctrl_proc {%d} -> closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    /* Ensure that caller is (current) controlling process */
    ESOCK_ASSERT( enif_self(env, &caller) != NULL );
    if (COMPARE_PIDS(&descP->ctrlPid, &caller) != 0) {
        SSDBG( descP, ("SOCKET",
                       "esock_setopt_otp_ctrl_proc -> not owner (%T)\r\n",
                       descP->ctrlPid) );
        return esock_make_error_invalid(env, esock_atom_not_owner);
    }

    /* Ensure that the new controller is a local process */
    if (!GET_LPID(env, eVal, &newCtrlPid)) {
        esock_warning_msg("Failed get pid of new controlling process\r\n");
        return esock_make_invalid(env, atom_value);
    }

    if ((xres = DEMONP("esock_setopt_otp_ctrl_proc -> (old) ctrl",
                       env, descP, &descP->ctrlMon)) != 0) {
        /* There is a legitimate reason for this is; the current
         * process was just killed from a different thread
         */
        esock_warning_msg("Failed demonitor (%d) "
                          "old controlling process %T (%T)\r\n",
                          xres, descP->ctrlPid, descP->ctrlMon);
    }

    descP->ctrlPid = newCtrlPid;

    if ((xres =
         MONP("esock_setopt_otp_ctrl_proc -> (new) ctrl",
                     env, descP, &descP->ctrlPid, &descP->ctrlMon)) != 0) {

        ESOCK_ASSERT( 0 < xres );
        /* Indicates that we do not have a DOWN callback,
         * which is preposterous
         */

        /* We know newCtrlPid is not 'undefined' so
         * it must be dead already
         * - pretend the controlling process change went well
         * and then the monitor went down
         */
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_ctrl_proc {%d} -> DOWN"
                "\r\n   xres: %d"
                "\r\n", descP->sock, xres) );

        enif_set_pid_undefined(&descP->ctrlPid);

        esock_down_ctrl(env, descP, &newCtrlPid);

        descP->readState  |= ESOCK_STATE_CLOSING;
        descP->writeState |= ESOCK_STATE_CLOSING;

    } else {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_ctrl_proc {%d} -> ok"
                "\r\n", descP->sock) );
    }

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* esock_setopt_otp_rcvbuf - Handle the OTP (level) rcvbuf option
 * The (otp) rcvbuf option is provided as:
 *
 *       BufSz :: default | pos_integer() |
 *           {N :: pos_integer(), Sz :: default | pos_integer()}
 *
 * Where N is the max number of reads.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp_rcvbuf(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
    const ERL_NIF_TERM* t;   // The array of the elements of the tuple
    int                 tsz; // The size of the tuple - should be 2
    unsigned int        n;
    size_t              bufSz;
    ssize_t             z;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_rcvbuf {%d} -> entry"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_rcvbuf {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    if (esock_decode_bufsz(env,
                           eVal,
                           ESOCK_RECV_BUFFER_SIZE_DEFAULT,
                           &bufSz)) {
        n = 0; // Reported as an integer buffer size by getopt
    } else {
        if ((! GET_TUPLE(env, eVal, &tsz, &t)) ||
            (tsz != 2) ||
            (! GET_UINT(env, t[0], &n)) ||
            (n == 0) ||
            (! esock_decode_bufsz(env, t[1],
                                 ESOCK_RECV_BUFFER_SIZE_DEFAULT,
                                  &bufSz))) {
            SSDBG( descP,
                   ("SOCKET",
                    "esock_setopt_otp_rcvbuf {%d} -> done invalid\r\n",
                descP->sock) );
            return esock_make_invalid(env, atom_value);
        }
    }
    // We do not want a buffer size that does not fit in ssize_t
    z = bufSz;
    if (bufSz != (size_t) z)
        return esock_make_invalid(env, atom_value);

    descP->rNum   = n;
    descP->rBufSz = bufSz;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_rcvbuf {%d} -> ok"
            "\r\n", descP->sock) );

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* esock_setopt_otp_rcvctrlbuf - Handle the OTP (level) rcvctrlbuf option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp_rcvctrlbuf(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eVal)
{
    size_t val;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_recvctrlbuf {%d} -> entry"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_rcvctrlbuf {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    if (! esock_decode_bufsz(env,
                             eVal,
                             ESOCK_RECV_CTRL_BUFFER_SIZE_DEFAULT,
                             &val)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_setopt_otp_rcvctrlbuf {%d} -> done invalid\r\n",
                descP->sock) );
        return esock_make_invalid(env, atom_value);
    }

    descP->rCtrlSz = val;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_rcvctrlbuf {%d} -> ok"
            "\r\n", descP->sock) );

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* esock_setopt_otp_sndctrlbuf - Handle the OTP (level) sndctrlbuf option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp_sndctrlbuf(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eVal)
{
    size_t val;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_sndvctrlbuf {%d} -> entry"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_sndctrlbuf {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    if (! esock_decode_bufsz(env,
                             eVal,
                             ESOCK_SEND_CTRL_BUFFER_SIZE_DEFAULT,
                             &val)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_setopt_otp_sndctrlbuf {%d} -> done invalid\r\n",
                descP->sock) );
        return esock_make_invalid(env, atom_value);
    }

    descP->wCtrlSz = val;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_sndctrlbuf {%d} -> ok"
            "\r\n", descP->sock) );

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* esock_setopt_otp_meta - Handle the OTP (level) meta options
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp_meta(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     eVal)
{
    ErlNifPid caller;

    ESOCK_ASSERT( enif_self(env, &caller) != NULL );

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_meta {%d} -> entry"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_meta {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    if (COMPARE_PIDS(&descP->ctrlPid, &caller) != 0) {
        SSDBG( descP, ("SOCKET",
                       "esock_setopt_otp_meta -> not owner (%T)\r\n",
                       descP->ctrlPid) );
        return esock_make_error_invalid(env, esock_atom_not_owner);
    }

    enif_clear_env(descP->meta.env);
    descP->meta.ref = CP_TERM(descP->meta.env, eVal);

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_meta {%d} -> ok"
            "\r\n", descP->sock) );

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* esock_setopt_otp_use_registry - Handle the OTP (level) use_registry option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_otp_use_registry(ErlNifEnv*       env,
					   ESockDescriptor* descP,
					   ERL_NIF_TERM     eVal)
{
    BOOLEAN_T useReg = FALSE;

    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_otp_use_registry {%d} -> closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    if (! esock_decode_bool(eVal, &useReg))
      return esock_make_invalid(env, atom_value);

    /* We only allow turning this on! */
    if (! useReg)
        return esock_make_invalid(env, atom_value);

    if (!descP->useReg) {
      ERL_NIF_TERM sockRef = enif_make_resource(env, descP);

      descP->useReg = useReg;
      esock_send_reg_add_msg(env, descP, sockRef);
    }

    SSDBG( descP,
           ("SOCKET", "esock_setopt_otp_use_registry {%d} -> ok"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* The option has *not* been encoded. Instead it has been provided
 * in "native mode" (value is a binary, an integer or a boolean).
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_native(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt,
                                 ERL_NIF_TERM     eVal)
{
    ErlNifBinary binary;
    int          integer;
    BOOLEAN_T    boolean;
    ERL_NIF_TERM result;

    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "esock_setopt_native {%d} -> entry"
            "\r\n   level: %d"
            "\r\n   opt:   %d"
            "\r\n   eVal: %T"
            "\r\n", descP->sock,
            level, opt, eVal) );

    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_native {%d} -> done closed\r\n",
                descP->sock) );

        MUNLOCK(descP->writeMtx);
        return esock_make_error(env, atom_closed);
    }

    if (GET_BIN(env, eVal, &binary)) {
        result = esock_setopt_level_opt(env, descP, level, opt,
                                        binary.data, binary.size);
    } else if (GET_INT(env, eVal, &integer)) {
        result = esock_setopt_level_opt(env, descP, level, opt,
                                        &integer, sizeof(integer));
    } else if (esock_decode_bool(eVal, &boolean)) {
        integer = boolean ? 1 : 0;
        result = esock_setopt_level_opt(env, descP, level, opt,
                                        &integer, sizeof(integer));
    } else {
        result = esock_make_error_invalid(env, atom_value);
    }

    SSDBG( descP,
           ("SOCKET", "esock_setopt_native {%d} -> done when"
            "\r\n   result: %T"
            "\r\n", descP->sock, result) );

    MUNLOCK(descP->writeMtx);
    return result;
}
#endif // #ifndef __WIN32__


/* esock_setopt - A "proper" level (option) has been specified,
 * and we have an value of known encoding
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          int              level,
                          int              opt,
                          ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;
    const struct ESockOpt *optP;

    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "esock_setopt {%d} -> entry with"
            "\r\n   level:       %d"
            "\r\n   opt:        %d"
            "\r\n   eVal:        %T"
            "\r\n", descP->sock, level, opt, eVal) );

    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt {%d} -> done closed\r\n",
                descP->sock) );

        MUNLOCK(descP->writeMtx);
        return esock_make_error(env, atom_closed);
    }

    optP = lookupOpt(level, opt);

    if (optP == NULL) {

        result = esock_make_invalid(env, atom_socket_option);

        SSDBG( descP,
               ("SOCKET",
                "esock_setopt {%d} -> unknown option\r\n",
                descP->sock) );

    } else if (optP->setopt == NULL) {

        result = esock_make_invalid(env, atom_socket_option);

        SSDBG( descP,
               ("SOCKET",
                "esock_setopt {%d} -> opt not settable\r\n",
                descP->sock) );

    } else {

        result = (optP->setopt)(env, descP, level, opt, eVal);

        SSDBG( descP,
               ("SOCKET", "esock_setopt {%d} -> done when"
                "\r\n   result: %T"
                "\r\n", descP->sock, result) );
    }

    MUNLOCK(descP->writeMtx);
    return result;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(SO_BINDTODEVICE)
static
ERL_NIF_TERM esock_setopt_so_bindtodevice(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              level,
                                       int              opt,
                                       ERL_NIF_TERM     eVal)
{
    return esock_setopt_str_opt(env, descP, level, opt, IFNAMSIZ, eVal);
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(SO_LINGER)
static
ERL_NIF_TERM esock_setopt_linger(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt,
                                 ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM  eOnOff, eLinger;
    BOOLEAN_T onOff;
    struct linger val;

    sys_memzero(&val, sizeof(val));

    if ((! GET_MAP_VAL(env, eVal, atom_onoff, &eOnOff)) ||
        (! GET_MAP_VAL(env, eVal, esock_atom_linger, &eLinger))) {

        if (COMPARE(eVal, esock_atom_abort) == 0) {
            val.l_onoff = 1;
            val.l_linger = 0;
            return esock_setopt_level_opt(env, descP, level, opt,
                                          &val, sizeof(val));
        } else
            return esock_make_invalid(env, atom_value);
    }

    if ((! esock_decode_bool(eOnOff, &onOff)) ||
        (! GET_INT(env, eLinger, &val.l_linger)) ||
        (val.l_linger < 0)) {
        return esock_make_invalid(env, atom_value);
    }
    val.l_onoff = onOff ? 1 : 0;

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &val, sizeof(val));
}
#endif
#endif // #ifndef __WIN32__



#ifndef __WIN32__
#if defined(IP_MSFILTER) && defined(IP_MSFILTER_SIZE)

/* esock_setopt_msfilter - Level IP MSFILTER option
 *
 * The value can be *either* the atom 'null' or a map of type ip_msfilter().
 */
static
ERL_NIF_TERM esock_setopt_msfilter(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              opt,
                                   ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    if (COMPARE(eVal, atom_null) == 0) {
        return
            esock_setopt_level_opt(env, descP, level, opt, NULL, 0);
    } else {
        struct ip_msfilter* msfP;
        Uint32              msfSz;
        ERL_NIF_TERM        eMultiAddr, eInterface, eFMode, eSList, elem, tail;
        unsigned int        slistLen, idx;

        if ((! GET_MAP_VAL(env, eVal, atom_multiaddr, &eMultiAddr)) ||
            (! GET_MAP_VAL(env, eVal, atom_interface, &eInterface)) ||
            (! GET_MAP_VAL(env, eVal, atom_mode, &eFMode)) ||
            (! GET_MAP_VAL(env, eVal, atom_slist, &eSList)))
            goto invalid;

        /* We start (decoding) with the slist, since without it we don't
         * really know how much (memory) to allocate.
         */
        if (! GET_LIST_LEN(env, eSList, &slistLen))
            goto invalid;

        msfSz = IP_MSFILTER_SIZE(slistLen);
        msfP  = MALLOC(msfSz);
        ESOCK_ASSERT( msfP != NULL );

        if ((! esock_decode_in_addr(env, eMultiAddr,
                                    &msfP->imsf_multiaddr)) ||
            (! esock_decode_in_addr(env, eInterface,
                                    &msfP->imsf_interface)) ||
            (! decode_msfilter_mode(env, eFMode,
                                    (Uint32*) &msfP->imsf_fmode)))
            goto free_invalid;

        /* And finally, extract the source addresses */
        msfP->imsf_numsrc = slistLen;
        for (idx = 0; idx < slistLen; idx++) {
            ESOCK_ASSERT( GET_LIST_ELEM(env, eSList, &elem, &tail) );
            if (! esock_decode_in_addr(env, elem,
                                       &msfP->imsf_slist[idx]))
                goto free_invalid;
            eSList = tail;
        }

        /* And now, finally, set the option */
        result = esock_setopt_level_opt(env, descP, level, opt,
                                        msfP, msfSz);

        FREE(msfP);
        return result;

    free_invalid:
        FREE(msfP);
    invalid:
        return esock_make_invalid(env, atom_value);
    }

}

static
BOOLEAN_T decode_msfilter_mode(ErlNifEnv*   env,
                                  ERL_NIF_TERM eVal,
                                  Uint32*      mode)
{
    BOOLEAN_T result;

    if (COMPARE(eVal, atom_include) == 0) {
        *mode  = MCAST_INCLUDE;
        result = TRUE;
    } else if (COMPARE(eVal, atom_exclude) == 0) {
        *mode  = MCAST_EXCLUDE;
        result = TRUE;
    } else {
        result = FALSE;
    }

    return result;
}

#endif // #if defined(IP_MSFILTER) && defined(IP_MSFILTER_SIZE)
#endif // #ifndef __WIN32__


/* esock_setopt_ip_mtu_discover - Level IP MTU_DISCOVER option
 *
 * The value is an atom of the type ip_pmtudisc().
 */
#ifndef __WIN32__
#if defined(IP_MTU_DISCOVER)
static
ERL_NIF_TERM esock_setopt_ip_mtu_discover(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              level,
                                          int              opt,
                                          ERL_NIF_TERM     eVal)
{
    int            val;

    if (! decode_ip_pmtudisc(env, eVal, &val))
        return esock_make_invalid(env, atom_value);
    else
        return esock_setopt_level_opt(env, descP, level, opt,
                                      &val, sizeof(val));
}
#endif // #if defined(IP_MTU_DISCOVER)
#endif // #ifndef __WIN32__



/* esock_setopt_multicast_if - Level IP MULTICAST_IF option
 *
 * The value is either the atom 'any' or a 4-tuple.
 */
#ifndef __WIN32__
#if defined(IP_MULTICAST_IF)
static
ERL_NIF_TERM esock_setopt_multicast_if(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              level,
                                       int              opt,
                                       ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM   result;
    struct in_addr ifAddr;

    if (! esock_decode_in_addr(env, eVal, &ifAddr)) {
        result = esock_make_invalid(env, atom_value);
    } else {
        result =
            esock_setopt_level_opt(env, descP, level, opt,
                                   &ifAddr, sizeof(ifAddr));
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__

/* esock_setopt_tos - Level IP TOS option
 */
#ifndef __WIN32__
#if defined(IP_TOS)
static
ERL_NIF_TERM esock_setopt_tos(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              level,
                              int              opt,
                              ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;
    int          val;

    if (decode_ip_tos(env, eVal, &val)) {
        result =
            esock_setopt_level_opt(env, descP, level, opt,
                                   &val, sizeof(val));
    } else {
        result = esock_make_invalid(env, atom_value);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__




/* The value is a map with two attributes: multiaddr and interface.
 * The attribute 'multiaddr' is always a 4-tuple (IPv4 address).
 * The attribute 'interface' is either the atom 'any' or a 4-tuple
 * (IPv4 address).
 */
#ifndef __WIN32__
#if defined(IP_ADD_MEMBERSHIP) || defined(IP_DROP_MEMBERSHIP)
static
ERL_NIF_TERM esock_setopt_in_update_membership(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               int              level,
                                               int              opt,
                                               ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM   eMultiAddr, eInterface;
    struct ip_mreq mreq;

    if (! GET_MAP_VAL(env, eVal, atom_multiaddr, &eMultiAddr)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_in_update_membership -> "
                "failed get multiaddr (map) attribute\r\n") );
        goto invalid;
    }

    if (! GET_MAP_VAL(env, eVal, atom_interface, &eInterface)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_in_update_membership -> "
                "failed get interface (map) attribute\r\n") );
        goto invalid;
    }

    if (! esock_decode_in_addr(env,
                               eMultiAddr,
                               &mreq.imr_multiaddr)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_in_update_membership -> "
                "failed decode multiaddr %T\r\n", eMultiAddr) );
        goto invalid;
    }

    if (! esock_decode_in_addr(env,
                               eInterface,
                               &mreq.imr_interface)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_in_update_membership -> "
                "failed decode interface %T\r\n", eInterface) );
        goto invalid;
    }

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &mreq, sizeof(mreq));

 invalid:
    return esock_make_invalid(env, atom_value);
}
#endif
#endif // #ifndef __WIN32__


/* The value is a map with three attributes: multiaddr, interface and
 * sourceaddr.
 * The attribute 'multiaddr' is always a 4-tuple (IPv4 address).
 * The attribute 'interface' is always a 4-tuple (IPv4 address).
 * The attribute 'sourceaddr' is always a 4-tuple (IPv4 address).
 * (IPv4 address).
 */
#ifndef __WIN32__
#if defined(IP_ADD_SOURCE_MEMBERSHIP) ||  \
    defined(IP_DROP_SOURCE_MEMBERSHIP) || \
    defined(IP_BLOCK_SOURCE) ||           \
    defined(IP_UNBLOCK_SOURCE)
static
ERL_NIF_TERM esock_setopt_in_update_source(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           int              level,
                                           int              opt,
                                           ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM          eMultiAddr, eInterface, eSourceAddr;
    struct ip_mreq_source mreq;

    if ((! GET_MAP_VAL(env, eVal, atom_multiaddr, &eMultiAddr)) ||
        (! GET_MAP_VAL(env, eVal, atom_interface, &eInterface)) ||
        (! GET_MAP_VAL(env, eVal, atom_sourceaddr, &eSourceAddr)) ||
        (! esock_decode_in_addr(env,
                                eMultiAddr,
                                &mreq.imr_multiaddr)) ||
        (! esock_decode_in_addr(env,
                                eInterface,
                                &mreq.imr_interface)) ||
        (! esock_decode_in_addr(env,
                                eSourceAddr,
                                &mreq.imr_sourceaddr)))
        goto invalid;

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &mreq, sizeof(mreq));
 invalid:
    return esock_make_invalid(env, atom_value);
}
#endif
#endif // #ifndef __WIN32__



#if defined(HAVE_IPV6)



#ifndef __WIN32__
#if defined(IPV6_ADDRFORM)
static
ERL_NIF_TERM esock_setopt_addrform(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              opt,
                                   ERL_NIF_TERM     eVal)
{
    int domain;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_addrform -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    if (! esock_decode_domain(env, eVal, &domain))
        return esock_make_invalid(env, atom_value);

    SSDBG( descP, ("SOCKET",
                   "esock_setopt_addrform -> try set opt to %d\r\n",
                   domain) );
    
    return esock_setopt_level_opt(env, descP, level, opt,
                                  &domain, sizeof(domain));
}
#endif
#endif // #ifndef __WIN32__



/* esock_setopt_ipv6_mtu_discover - Level IPv6 MTU_DISCOVER option
 *
 * The value is an atom of the type ipv6_pmtudisc().
 */
#ifndef __WIN32__
#if defined(IPV6_MTU_DISCOVER)
static
ERL_NIF_TERM esock_setopt_ipv6_mtu_discover(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            int              level,
                                            int              opt,
                                            ERL_NIF_TERM     eVal)
{
    int           val;

    if (! decode_ipv6_pmtudisc(env, eVal, &val))
        return esock_make_invalid(env, atom_value);

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &val, sizeof(val));
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(IPV6_MULTICAST_HOPS)
static
ERL_NIF_TERM esock_setopt_hops(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               int              level,
                               int              opt,
                               ERL_NIF_TERM     eVal)
{
    int hops;

    if (! decode_hops(env, eVal, &hops))
        return esock_make_invalid(env, atom_value);

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &hops, sizeof(hops));
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(IPV6_ADD_MEMBERSHIP) || defined(IPV6_DROP_MEMBERSHIP)
static
ERL_NIF_TERM esock_setopt_in6_update_membership(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                int              level,
                                                int              opt,
                                                ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM     eMultiAddr, eInterface;
    struct ipv6_mreq mreq;

    if (! GET_MAP_VAL(env, eVal, atom_multiaddr, &eMultiAddr)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_in6_update_membership -> "
                "failed get multiaddr (map) attribute\r\n") );
        goto invalid;
    }

    if (! GET_MAP_VAL(env, eVal, atom_interface, &eInterface)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_in6_update_membership -> "
                "failed get interface (map) attribute\r\n") );
        goto invalid;
    }

    if (! esock_decode_in6_addr(env,
                                eMultiAddr,
                                &mreq.ipv6mr_multiaddr)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_in6_update_membership -> "
                "failed decode multiaddr %T\r\n", eMultiAddr) );
        goto invalid;
    }

    if (! GET_UINT(env, eInterface, &mreq.ipv6mr_interface)) {
        SSDBG( descP,
               ("SOCKET", "esock_setopt_in6_update_membership -> "
                "failed decode interface %T\r\n", eInterface) );
        goto invalid;
    }

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &mreq, sizeof(mreq));

 invalid:
    return esock_make_invalid(env, atom_value);
}
#endif
#endif // #ifndef __WIN32__


#endif // defined(HAVE_IPV6)




/* esock_setopt_tcp_congestion - Level TCP CONGESTION option
 */
#ifndef __WIN32__
#if defined(TCP_CONGESTION)
static
ERL_NIF_TERM esock_setopt_tcp_congestion(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt,
                                         ERL_NIF_TERM     eVal)
{
    int max = ESOCK_OPT_TCP_CONGESTION_NAME_MAX+1;

    return esock_setopt_str_opt(env, descP, level, opt, max, eVal);
}
#endif
#endif // #ifndef __WIN32__



#if defined(HAVE_SCTP)



/* esock_setopt_sctp_associnfo - Level SCTP ASSOCINFO option
 */
#ifndef __WIN32__
#if defined(SCTP_ASSOCINFO)
static
ERL_NIF_TERM esock_setopt_sctp_associnfo(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt,
                                         ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM            eAssocId, eMaxRxt, eNumPeerDests;
    ERL_NIF_TERM            ePeerRWND, eLocalRWND, eCookieLife;
    struct sctp_assocparams assocParams;
    unsigned int            ui;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_sctp_associnfo -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    // It must be a map
    if (! IS_MAP(env, eVal))
        goto invalid;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_associnfo -> extract attributes\r\n") );

    if ((! GET_MAP_VAL(env, eVal, atom_assoc_id,       &eAssocId))   ||
        (! GET_MAP_VAL(env, eVal, atom_asocmaxrxt,     &eMaxRxt))    ||
        (! GET_MAP_VAL(env, eVal, atom_number_peer_destinations,
                       &eNumPeerDests))                              ||
        (! GET_MAP_VAL(env, eVal, atom_peer_rwnd,      &ePeerRWND))  ||
        (! GET_MAP_VAL(env, eVal, atom_local_rwnd,     &eLocalRWND)) ||
        (! GET_MAP_VAL(env, eVal, atom_cookie_life,    &eCookieLife)))
        goto invalid;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_associnfo -> decode attributes\r\n") );

    if (! decode_sctp_assoc_t(env, eAssocId, &assocParams.sasoc_assoc_id))
        goto invalid;

    /*
     * We should really make sure this is ok in erlang (to ensure that
     * the values (max-rxt and num-peer-dests) fits in 16-bits).
     * The value should be a 16-bit unsigned int...
     * Both sasoc_asocmaxrxt and sasoc_number_peer_destinations.
     */

    if (! GET_UINT(env, eMaxRxt, &ui))
        goto invalid;
    assocParams.sasoc_asocmaxrxt = (Uint16) ui;

    if (! GET_UINT(env, eNumPeerDests, &ui))
        goto invalid;
    assocParams.sasoc_number_peer_destinations = (Uint16) ui;

    if (! GET_UINT(env, ePeerRWND, &ui))
        goto invalid;
    assocParams.sasoc_peer_rwnd = (Uint32) ui;

    if (! GET_UINT(env, eLocalRWND, &ui))
        goto invalid;
    assocParams.sasoc_local_rwnd = (Uint32) ui;

    if (! GET_UINT(env, eCookieLife, &ui))
        goto invalid;
    assocParams.sasoc_cookie_life = (Uint32) ui;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_associnfo -> set associnfo option\r\n") );

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &assocParams, sizeof(assocParams));

 invalid:
    return esock_make_invalid(env, atom_value);
}
#endif
#endif // #ifndef __WIN32__


/* esock_setopt_sctp_events - Level SCTP EVENTS option
 */
#ifndef __WIN32__
#if defined(SCTP_EVENTS)
static
ERL_NIF_TERM esock_setopt_sctp_events(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      int              opt,
                                      ERL_NIF_TERM     eVal)
{
    struct    sctp_event_subscribe events;
    BOOLEAN_T error;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_sctp_events {%d} -> entry with"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    // It must be a map
    if (! IS_MAP(env, eVal))
        goto invalid;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_events {%d} -> decode attributes\r\n",
            descP->sock) );

    error = FALSE;

    events.sctp_data_io_event =
        esock_setopt_sctp_event(env, eVal, atom_data_io, &error);
    events.sctp_association_event =
        esock_setopt_sctp_event(env, eVal, atom_association, &error);
    events.sctp_address_event =
        esock_setopt_sctp_event(env, eVal, atom_address, &error);
    events.sctp_send_failure_event =
        esock_setopt_sctp_event(env, eVal, atom_send_failure, &error);
    events.sctp_peer_error_event =
        esock_setopt_sctp_event(env, eVal, atom_peer_error, &error);
    events.sctp_shutdown_event =
        esock_setopt_sctp_event(env, eVal, atom_shutdown, &error);
    events.sctp_partial_delivery_event =
        esock_setopt_sctp_event(env, eVal, atom_partial_delivery, &error);
    events.sctp_adaptation_layer_event =
        esock_setopt_sctp_event(env, eVal, atom_adaptation_layer, &error);

#if defined(HAVE_STRUCT_SCTP_EVENT_SUBSCRIBE_SCTP_AUTHENTICATION_EVENT)
    events.sctp_authentication_event =
        esock_setopt_sctp_event(env, eVal, atom_authentication, &error);
#endif

#if defined(HAVE_STRUCT_SCTP_EVENT_SUBSCRIBE_SCTP_SENDER_DRY_EVENT)
    events.sctp_sender_dry_event =
        esock_setopt_sctp_event(env, eVal, atom_sender_dry, &error);
#endif

    if (error) {
        goto invalid;
    } else {
        ERL_NIF_TERM result;

        result = esock_setopt_level_opt(env, descP, level, opt,
                                        &events, sizeof(events));
        SSDBG( descP,
               ("SOCKET",
                "esock_setopt_sctp_events {%d} -> set events -> %T\r\n",
                descP->sock, result) );

        return result;
    }

 invalid:
    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_events {%d} -> invalid\r\n",
            descP->sock) );

    return esock_make_invalid(env, atom_value);
}

/* Return the value to make use of automatic type casting.
 * Set *error if something goes wrong.
 */
static int esock_setopt_sctp_event(ErlNifEnv   *env,
                                   ERL_NIF_TERM eMap,
                                   ERL_NIF_TERM eKey,
                                   BOOLEAN_T   *error)
{
    ERL_NIF_TERM eVal;
    BOOLEAN_T    val;

    if (GET_MAP_VAL(env, eMap, eKey, &eVal))
        if (esock_decode_bool(eVal, &val))
            return (int) val;

    *error = TRUE;
    return 0;
}
#endif
#endif // #ifndef __WIN32__


/* esock_setopt_sctp_initmsg - Level SCTP INITMSG option
 */
#ifndef __WIN32__
#if defined(SCTP_INITMSG)
static
ERL_NIF_TERM esock_setopt_sctp_initmsg(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              level,
                                       int              opt,
                                       ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM        eNumOut, eMaxIn, eMaxAttempts, eMaxInitTO;
    struct sctp_initmsg initMsg;
    unsigned int        tmp;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_sctp_initmsg -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    // It must be a map
    if (! IS_MAP(env, eVal))
        goto invalid;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_initmsg -> extract attributes\r\n") );

    if ((! GET_MAP_VAL(env, eVal, atom_num_outstreams, &eNumOut)) ||
        (! GET_MAP_VAL(env, eVal, atom_max_instreams,  &eMaxIn)) ||
        (! GET_MAP_VAL(env, eVal, atom_max_attempts,   &eMaxAttempts)) ||
        (! GET_MAP_VAL(env, eVal, atom_max_init_timeo, &eMaxInitTO)))
        goto invalid;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_initmsg -> decode attributes\r\n") );

    if (! GET_UINT(env, eNumOut, &tmp))
        goto invalid;
    initMsg.sinit_num_ostreams = (Uint16) tmp;

    if (! GET_UINT(env, eMaxIn, &tmp))
        goto invalid;
    initMsg.sinit_max_instreams = (Uint16) tmp;

    if (! GET_UINT(env, eMaxAttempts, &tmp))
        goto invalid;
    initMsg.sinit_max_attempts = (Uint16) tmp;

    if (! GET_UINT(env, eMaxInitTO, &tmp))
        goto invalid;
    initMsg.sinit_max_init_timeo = (Uint16) tmp;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_initmsg -> set initmsg option\r\n") );

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &initMsg, sizeof(initMsg));

 invalid:
    return esock_make_invalid(env, atom_value);
}
#endif
#endif // #ifndef __WIN32__


/* esock_setopt_sctp_rtoinfo - Level SCTP RTOINFO option
 */
#ifndef __WIN32__
#if defined(SCTP_RTOINFO)
static
ERL_NIF_TERM esock_setopt_sctp_rtoinfo(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              level,
                                       int              opt,
                                       ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM        eAssocId, eInitial, eMax, eMin;
    struct sctp_rtoinfo rtoInfo;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_sctp_rtoinfo -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    // It must be a map
    if (! IS_MAP(env, eVal))
        goto invalid;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_rtoinfo -> extract attributes\r\n") );

    if ((! GET_MAP_VAL(env, eVal, atom_assoc_id, &eAssocId)) ||
        (! GET_MAP_VAL(env, eVal, atom_initial,  &eInitial)) ||
        (! GET_MAP_VAL(env, eVal, atom_max,      &eMax)) ||
        (! GET_MAP_VAL(env, eVal, atom_min,      &eMin)))
        goto invalid;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_rtoinfo -> decode attributes\r\n") );

    if (! decode_sctp_assoc_t(env, eAssocId, &rtoInfo.srto_assoc_id))
        goto invalid;

    if ((! GET_UINT(env, eInitial, &rtoInfo.srto_initial)) ||
        (! GET_UINT(env, eMax, &rtoInfo.srto_max)) ||
        (! GET_UINT(env, eMin, &rtoInfo.srto_min)))
        goto invalid;

    SSDBG( descP,
           ("SOCKET",
            "esock_setopt_sctp_rtoinfo -> set associnfo option\r\n") );

    return esock_setopt_level_opt(env, descP, level, opt,
                                  &rtoInfo, sizeof(rtoInfo));

 invalid:
    return esock_make_invalid(env, atom_value);
}
#endif
#endif // #ifndef __WIN32__



#endif // defined(HAVE_SCTP)




/* esock_setopt_bool_opt - set an option that has an (integer) bool value
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_bool_opt(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              opt,
                                   ERL_NIF_TERM     eVal)
{
    BOOLEAN_T    val;
    int          ival;

    if (! esock_decode_bool(eVal, &val))
        return esock_make_invalid(env, atom_value);

    ival = (val) ? 1 : 0;
    return esock_setopt_level_opt(env, descP, level, opt,
                                  &ival, sizeof(ival));
}
#endif // #ifndef __WIN32__


/* esock_setopt_int_opt - set an option that has an integer value
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_setopt_int_opt(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              level,
                                  int              opt,
                                  ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;
    int          val;

    if (GET_INT(env, eVal, &val)) {
        result =
            esock_setopt_level_opt(env, descP, level, opt,
                                   &val, sizeof(val));
    } else {
        result = esock_make_invalid(env, atom_value);
    }
    return result;
}
#endif // #ifndef __WIN32__


/* esock_setopt_str_opt - set an option that has an string value
 */
#ifndef __WIN32__
#if defined(USE_SETOPT_STR_OPT)
static
ERL_NIF_TERM esock_setopt_str_opt(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              level,
                                  int              opt,
                                  int              max,
                                  ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;
    int          optLen;
    char*        val = MALLOC(max);

    ESOCK_ASSERT( val != NULL );

    if ((optLen = GET_STR(env, eVal, val, max)) > 0) {
        optLen--;

        result =
            esock_setopt_level_opt(env, descP, level, opt,
                                   val, optLen);
    } else {
        result = esock_make_invalid(env, atom_value);
    }

    FREE(val);

    return result;
}
#endif
#endif // #ifndef __WIN32__


/* esock_setopt_timeval_opt - set an option that has an (timeval) bool value
 */
#ifndef __WIN32__
#if (defined(SO_RCVTIMEO) || defined(SO_SNDTIMEO)) \
    && defined(ESOCK_USE_RCVSNDTIMEO)
static
ERL_NIF_TERM esock_setopt_timeval_opt(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      int              opt,
                                      ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM   result;
    struct timeval timeVal;

    SSDBG( descP,
           ("SOCKET", "esock_setopt_timeval_opt -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    if (! esock_decode_timeval(env, eVal, &timeVal))
        return esock_make_invalid(env, atom_value);

    SSDBG( descP,
           ("SOCKET", "esock_setopt_timeval_opt -> set timeval option\r\n") );

    result =
        esock_setopt_level_opt(env, descP, level, opt,
                               &timeVal, sizeof(timeVal));

    SSDBG( descP,
           ("SOCKET", "esock_setopt_timeval_opt -> done with"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;

}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static ERL_NIF_TERM esock_setopt_level_opt(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           int              level,
                                           int              opt,
                                           void*            optVal,
                                           socklen_t        optLen)
{
    if (socket_setopt(descP->sock, level, opt, optVal, optLen))
        return esock_make_error_errno(env, sock_errno());
    else
        return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* +++ socket_setopt +++
 *
 * <Per H @ Tail-f>
 * The original code here had problems that possibly
 * only occur if you abuse it for non-INET sockets, but anyway:
 * a) If the getsockopt for SO_PRIORITY or IP_TOS failed, the actual
 *    requested setsockopt was never even attempted.
 * b) If {get,set}sockopt for one of IP_TOS and SO_PRIORITY failed,
 *    but ditto for the other worked and that was actually the requested
 *    option, failure was still reported to erlang.
 * </Per H @ Tail-f>
 *
 * <PaN>
 * The relations between SO_PRIORITY, TOS and other options
 * is not what you (or at least I) would expect...:
 * If TOS is set after priority, priority is zeroed.
 * If any other option is set after tos, tos might be zeroed.
 * Therefore, save tos and priority. If something else is set,
 * restore both after setting, if  tos is set, restore only
 * prio and if prio is set restore none... All to keep the
 * user feeling socket options are independent.
 * </PaN>
 */
#ifndef __WIN32__
static
int socket_setopt(int sock, int level, int opt,
                  const void* optVal, const socklen_t optLen)
{
    int res;

#if  defined(IP_TOS) && defined(SOL_IP) && defined(SO_PRIORITY)
    int          tmpIValPRIO = 0;
    int          tmpIValTOS = 0;
    int          resPRIO;
    int          resTOS;
    SOCKOPTLEN_T tmpArgSzPRIO = sizeof(tmpIValPRIO);
    SOCKOPTLEN_T tmpArgSzTOS  = sizeof(tmpIValTOS);

    resPRIO = sock_getopt(sock, SOL_SOCKET, SO_PRIORITY,
                           &tmpIValPRIO, &tmpArgSzPRIO);
    resTOS  = sock_getopt(sock, SOL_IP, IP_TOS,
                          &tmpIValTOS, &tmpArgSzTOS);

    res = sock_setopt(sock, level, opt, optVal, optLen);
    if (res == 0) {

        /* Ok, now we *maybe* need to "maybe" restore PRIO and TOS...
         * maybe, possibly, ...
         */

        if (opt != SO_PRIORITY) {
	    if ((opt != IP_TOS) && (resTOS == 0)) {
		resTOS = sock_setopt(sock, SOL_IP, IP_TOS,
                                     (void *) &tmpIValTOS,
                                     tmpArgSzTOS);
                res = resTOS;
            }
	    if ((res == 0) && (resPRIO == 0)) {
		resPRIO = sock_setopt(sock, SOL_SOCKET, SO_PRIORITY,
                                      &tmpIValPRIO,
                                      tmpArgSzPRIO);

                /* Some kernels set a SO_PRIORITY by default
                 * that you are not permitted to reset,
                 * silently ignore this error condition.
                 */

                if ((resPRIO != 0) && (sock_errno() == EPERM)) {
                    res = 0;
                } else {
                    res = resPRIO;
		}
	    }
	}
    }

#else

    res = sock_setopt(sock, level, opt, optVal, optLen);

#endif

    return res;
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_getopt
 *
 * Description:
 * Get socket option.
 * Its possible to use a ValueSpec to select
 * how the value should be decoded.
 *
 * Arguments:
 * Socket      (ref) - Points to the socket descriptor.
 * Level       (int) - Protocol level, encoded or native
 * Opt         (int) - Option, encoded or native
 * ValueSpec  (term) - How to decode the value [optional]
 */

static
ERL_NIF_TERM nif_getopt(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    int              level, opt;

    ESOCK_ASSERT( (argc == 3) || (argc == 4) );

    SGDBG( ("SOCKET", "nif_getopt -> entry with argc: %d\r\n", argc) );

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        SGDBG( ("SOCKET", "nif_getopt -> failed initial args check\r\n") );
        return enif_make_badarg(env);
    }

    if (! GET_INT(env, argv[2], &opt)) {
        SSDBG( descP,
               ("SOCKET", "nif_getopt -> failed initial args check\r\n") );
        if (! IS_INTEGER(env, argv[2]))
            return enif_make_badarg(env);
        else
            return esock_make_error_integer_range(env, argv[2]);
    }

    if (esock_decode_level(env, argv[1], &level)) {
        if (argc == 4) {
            ERL_NIF_TERM valueSpec = argv[3];
            return esock_getopt_native(env, descP, level, opt, valueSpec);
        } else {
            return esock_getopt(env, descP, level, opt);
        }
    }

    if ((COMPARE(argv[1], atom_otp) == 0) &&
        (argc == 3)) {
        return esock_getopt_otp(env, descP, opt) ;
    }

    SGDBG( ("SOCKET", "nif_getopt -> failed args check\r\n") );
    if (IS_INTEGER(env, argv[1]))
        return esock_make_error_integer_range(env, argv[1]);
    else
        return enif_make_badarg(env);

#endif // #ifdef __WIN32__  #else
}



/* esock_getopt_otp - Handle OTP (level) options
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              eOpt)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp -> entry with"
            "\r\n   eOpt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
    case ESOCK_OPT_OTP_DEBUG:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_debug(env, descP);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_IOW:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_iow(env, descP);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_CTRL_PROC:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_ctrl_proc(env, descP);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_RCVBUF:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_rcvbuf(env, descP);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_RCVCTRLBUF:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_rcvctrlbuf(env, descP);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_SNDCTRLBUF:
        MLOCK(descP->writeMtx);
        result = esock_getopt_otp_sndctrlbuf(env, descP);
        MUNLOCK(descP->writeMtx);
        break;

    case ESOCK_OPT_OTP_FD:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_fd(env, descP);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_META:
        MLOCK(descP->writeMtx);
        result = esock_getopt_otp_meta(env, descP);
        MUNLOCK(descP->writeMtx);
        break;

    case ESOCK_OPT_OTP_USE_REGISTRY:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_use_registry(env, descP);
        MUNLOCK(descP->readMtx);
        break;

        /* *** INTERNAL *** */
    case ESOCK_OPT_OTP_DOMAIN:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_domain(env, descP);
        MUNLOCK(descP->readMtx);
        break;

#if 0
    case ESOCK_OPT_OTP_TYPE:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_type(env, descP);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_PROTOCOL:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_protocol(env, descP);
        MUNLOCK(descP->readMtx);
        break;

    case ESOCK_OPT_OTP_DTP:
        MLOCK(descP->readMtx);
        result = esock_getopt_otp_dtp(env, descP);
        MUNLOCK(descP->readMtx);
        break;
#endif

    default:
        MLOCK(descP->readMtx);
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp {%d} -> invalid with"
                "\r\n   eOpt: %d"
                "\r\n", descP->sock, eOpt) );
        MUNLOCK(descP->readMtx);

        /* This is an internal error - prim_inet gave us junk */
        result =
            esock_raise_invalid(env,
                                MKT2(env,
                                     atom_otp_socket_option,
                                     MKI(env, eOpt)));
        break;
    }

    return result;
}
#endif // #ifndef __WIN32__

/* esock_getopt_otp_debug - Handle the OTP (level) debug option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_debug(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_debug {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    eVal = esock_encode_bool(descP->dbg);

    return esock_make_ok2(env, eVal);
}
#endif // #ifndef __WIN32__

/* esock_getopt_otp_iow - Handle the OTP (level) iow option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_iow(ErlNifEnv*       env,
                                  ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_iow {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    eVal = esock_encode_bool(descP->iow);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_iow {%d} ->"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_make_ok2(env, eVal);
}
#endif // #ifndef __WIN32__


/* esock_getopt_otp_ctrl_proc - Handle the OTP (level) controlling_process option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_ctrl_proc(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_getopt_otp_ctrl_proc {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    eVal = MKPID(env, &descP->ctrlPid);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_ctrlProc {%d} ->"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_make_ok2(env, eVal);
}
#endif // #ifndef __WIN32__


/* esock_getopt_otp_rcvbuf - Handle the OTP (level) rcvbuf option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_rcvbuf(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_rcvbuf {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    if (descP->rNum == 0) {
        eVal = MKUL(env, (unsigned long) descP->rBufSz);
    } else {
        eVal = MKT2(env,
                    MKI(env, descP->rNum),
                    MKUL(env, (unsigned long) descP->rBufSz));
    }

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_rcvbuf {%d} ->"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_make_ok2(env, eVal);
}
#endif // #ifndef __WIN32__


/* esock_getopt_otp_rcvctrlbuf - Handle the OTP (level) rcvctrlbuf option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_rcvctrlbuf(ErlNifEnv*       env,
                                         ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_getopt_otp_rcvctrlbuf {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    eVal = MKUL(env, (unsigned long) descP->rCtrlSz);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_rcvctrlbuf {%d} ->"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_make_ok2(env, eVal);
}
#endif // #ifndef __WIN32__


/* esock_getopt_otp_sndctrlbuf - Handle the OTP (level) sndctrlbuf option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_sndctrlbuf(ErlNifEnv*       env,
                                         ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_getopt_otp_sndctrlbuf {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    eVal = MKUL(env, (unsigned long) descP->wCtrlSz);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_sndctrlbuf {%d} ->"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_make_ok2(env, eVal);
}
#endif // #ifndef __WIN32__


/* esock_getopt_otp_fd - Handle the OTP (level) fd option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_fd(ErlNifEnv*       env,
                                 ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_debug {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    eVal = MKI(env, descP->sock);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_fd {%d} ->"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_make_ok2(env, eVal);
}
#endif // #ifndef __WIN32__


/* esock_getopt_otp_meta - Handle the OTP (level) meta option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_meta(ErlNifEnv*       env,
                                   ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (! IS_OPEN(descP->writeState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_meta {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    eVal = CP_TERM(env, descP->meta.ref);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_meta {%d} ->"
            "\r\n   eVal: %T"
            "\r\n", descP->sock, eVal) );

    return esock_make_ok2(env, eVal);
}
#endif // #ifndef __WIN32__


/* esock_getopt_otp_use_registry - Handle the OTP (level) use_registry option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_use_registry(ErlNifEnv*       env,
                                           ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal = esock_encode_bool(descP->useReg);

    return esock_make_ok2(env, eVal);
}
#endif


/*
 * esock_getopt_otp_domain - Handle the OTP (level) domain option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_domain(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    ERL_NIF_TERM domain, result;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_domain {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    esock_encode_domain(env, descP->domain, &domain);
    result = esock_make_ok2(env, domain);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_domain {%d} ->"
            "\r\n   result: %T"
            "\r\n", descP->sock, result) );

    return result;
}
#endif // #ifndef __WIN32__



#if 0

/*
 * esock_getopt_otp_type - Handle the OTP (level) type options.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_type(ErlNifEnv*       env,
                                   ESockDescriptor* descP)
{
    ERL_NIF_TERM type, result;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_type {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    esock_encode_type(env, descP->type, &type);
    result = esock_make_ok2(env, type);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_type {%d} ->"
            "\r\n   result: %T"
            "\r\n", descP->sock, result) );

    return result;
}
#endif // #ifndef __WIN32__


/*
 * esock_getopt_otp_protocol - Handle the OTP (level) protocol options.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_protocol(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    ERL_NIF_TERM protocol, result;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_protocol {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    protocol = MKI(env, descP->protocol);
    result = esock_make_ok2(env, protocol);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_protocol {%d} ->"
            "\r\n   result: %T"
            "\r\n", descP->sock, result) );

    return result;
}
#endif // #ifndef __WIN32__


/*
 * esock_getopt_otp_dtp - Handle the OTP (level) type options.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_otp_dtp(ErlNifEnv*       env,
                                   ESockDescriptor* descP)
{
    ERL_NIF_TERM domain, type, protocol, dtp, result;

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_otp_dtp {%d} -> done closed\r\n",
                descP->sock) );
        return esock_make_error(env, atom_closed);
    }

    esock_encode_domain(env, descP->domain, &domain);
    esock_encode_type(env, descP->type, &type);
    protocol = MKI(env, descP->protocol);
    dtp = MKT3(env, domain, type, protocol);
    result = esock_make_ok2(env, dtp);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_otp_dtp {%d} ->"
            "\r\n   result: %T"
            "\r\n", descP->sock, result) );

    return result;
}
#endif // #ifndef __WIN32__


#endif // #if 0


/* How to decode the value is specified with valueSpec
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_native(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt,
                                 ERL_NIF_TERM     valueSpec)
{
    ERL_NIF_TERM result;
    SOCKOPTLEN_T valueSz;
    int          sz;
    ErlNifBinary bin;

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "esock_getopt_native {%d} -> entry"
            "\r\n   level: %d"
            "\r\n   opt: %d"
            "\r\n   valueSpec: %T"
            "\r\n", descP->sock,
            level, opt, valueSpec) );

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_native {%d} -> done closed\r\n",
                descP->sock) );
        MUNLOCK(descP->readMtx);
        return esock_make_error(env, atom_closed);
    }

    /* <KOLLA>
     * We could make it possible to specify more types,
     * such as string, NUL terminated or not, etc...
     * </KOLLA>
     */

    if (GET_INT(env, valueSpec, &sz)) {
        valueSz = (SOCKOPTLEN_T) sz;
        if ((int) valueSz == sz) {
            SSDBG( descP,
                   ("SOCKET", "esock_getopt_native {%d} -> binary size"
                    "\r\n   valueSz: %d"
                    "\r\n", descP->sock, sz) );
            result =
                esock_getopt_size_opt(env, descP, level, opt, valueSz);
        } else {
            result = esock_make_invalid(env, atom_value);
        }
    } else if (COMPARE(valueSpec, atom_integer) == 0) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_native {%d} -> integer"
                "\r\n", descP->sock) );
        result = esock_getopt_int_opt(env, descP, level, opt);
    } else if (COMPARE(valueSpec, atom_boolean) == 0) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_native {%d} -> boolean"
                "\r\n", descP->sock) );
        result = esock_getopt_bool_opt(env, descP, level, opt);
    } else if (enif_inspect_binary(env, valueSpec, &bin)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt_native {%d} -> binary"
                "\r\n   size: %lu"
                "\r\n", descP->sock, (unsigned long) bin.size) );
        result = esock_getopt_bin_opt(env, descP, level, opt, &bin);
    } else {
        result = esock_make_invalid(env, atom_value);
    }

    SSDBG( descP,
           ("SOCKET", "esock_getopt_native {%d} -> done when"
            "\r\n   result: %T"
            "\r\n", descP->sock, result) );

    MUNLOCK(descP->readMtx);
    return result;
}
#endif // #ifndef __WIN32__


/* esock_getopt - An option that we know how to decode
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          int              level,
                          int              opt)
{
    ERL_NIF_TERM result;
    const struct ESockOpt *optP;

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "esock_getopt {%d} -> entry with"
            "\r\n   level:       %d"
            "\r\n   opt:        %d"
            "\r\n", descP->sock, level, opt) );

    if (! IS_OPEN(descP->readState)) {
        SSDBG( descP,
               ("SOCKET", "esock_getopt {%d} -> done closed\r\n",
                descP->sock) );
        MUNLOCK(descP->readMtx);
        return esock_make_error(env, atom_closed);
    }

    optP = lookupOpt(level, opt);

    if (optP == NULL) {

        result = esock_make_invalid(env, atom_socket_option);

        SSDBG( descP,
               ("SOCKET", "esock_getopt {%d} -> unknown option\r\n",
                descP->sock) );

    } else if (optP->getopt == NULL) {

        result = esock_make_invalid(env, atom_socket_option);

        SSDBG( descP,
               ("SOCKET", "esock_getopt {%d} -> opt not gettable\r\n",
                descP->sock) );

    } else {

        result = (optP->getopt)(env, descP, level, opt);

        SSDBG( descP,
               ("SOCKET", "esock_getopt {%d} -> done when"
                "\r\n   result: %T"
                "\r\n", descP->sock, result) );
    }

    MUNLOCK(descP->readMtx);
    return result;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(SO_BINDTODEVICE)
static
ERL_NIF_TERM esock_getopt_so_bindtodevice(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              level,
                                          int              opt)
{
    return esock_getopt_str_opt(env, descP, level, opt, IFNAMSIZ+1, FALSE);
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(SO_DOMAIN)
static
ERL_NIF_TERM esock_getopt_sock_domain(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      int              opt)
{
    int val;
    ERL_NIF_TERM result;

    if (! esock_getopt_int(descP->sock, level, opt, &val)) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM domain;
        esock_encode_domain(env, val, &domain);
        result = esock_make_ok2(env, domain);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(SO_LINGER)
static
ERL_NIF_TERM esock_getopt_linger(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt)
{
    ERL_NIF_TERM  result;
    struct linger val;
    SOCKOPTLEN_T  valSz = sizeof(val);
    int           res;

    sys_memzero((void *) &val, sizeof(val));

    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM
            lOnOff = ((val.l_onoff != 0) ? atom_true : atom_false),
            lSecs  = MKI(env, val.l_linger),
            keys[] = {atom_onoff, esock_atom_linger},
            vals[] = {lOnOff, lSecs},
            linger;
        size_t numKeys = NUM(keys);

        ESOCK_ASSERT( numKeys == NUM(vals) );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &linger) );

        result = esock_make_ok2(env, linger);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__



#ifndef __WIN32__
#if defined(SO_TYPE)
static
ERL_NIF_TERM esock_getopt_sock_type(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              level,
                                    int              opt)
{
    ERL_NIF_TERM result;
    int          val;

    if (! esock_getopt_int(descP->sock, level, opt, &val)) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM type;
        esock_encode_type(env, val, &type);
        result = esock_make_ok2(env, type);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(SO_PROTOCOL)
static
ERL_NIF_TERM esock_getopt_sock_protocol(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              level,
                                        int              opt)
{
    ERL_NIF_TERM result;
    int          val;

    if (! esock_getopt_int(descP->sock, level, opt, &val)) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM protocol;

        protocol =
#ifdef AF_LOCAL
            /* For AF_LOCAL, the protocols table value for 0 is wrong */
            (val == 0) && (descP->domain == AF_LOCAL) ?
            esock_atom_default :
            /* It is correct for AF_INET and hopefully for AF_INET6,
             * but for future additions it is an open question
             */
#endif
            MKI(env, val);

        result = esock_make_ok2(env, protocol);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
/* esock_getopt_ip_mtu_discover - Level IP MTU_DISCOVER option
 */
#if defined(IP_MTU_DISCOVER)
static
ERL_NIF_TERM esock_getopt_ip_mtu_discover(ErlNifEnv*       env,
					  ESockDescriptor* descP,
					  int              level,
					  int              opt)
{
    ERL_NIF_TERM result;
    int          mtuDisc;

    if (! esock_getopt_int(descP->sock, level, opt, &mtuDisc)) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eMtuDisc;
        encode_ip_pmtudisc(env, mtuDisc, &eMtuDisc);
        result = esock_make_ok2(env, eMtuDisc);
    }

    return result;

}
#endif
#endif // #ifndef __WIN32__


/* esock_getopt_multicast_if - Level IP MULTICAST_IF option
 */
#ifndef __WIN32__
#if defined(IP_MULTICAST_IF)
static
ERL_NIF_TERM esock_getopt_multicast_if(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              level,
                                       int              opt)
{
    ERL_NIF_TERM   result;
    ERL_NIF_TERM   eAddr;
    struct in_addr ifAddr;
    SOCKOPTLEN_T   ifAddrSz = sizeof(ifAddr);
    int            res;

    sys_memzero((void *) &ifAddr, ifAddrSz);

    res = sock_getopt(descP->sock, level, opt, &ifAddr, &ifAddrSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        esock_encode_in_addr(env, &ifAddr, &eAddr);
        result = esock_make_ok2(env, eAddr);
    }

    return result;

}
#endif
#endif // #ifndef __WIN32__


/* esock_getopt_tos - Level IP TOS option
 */
#ifndef __WIN32__
#if defined(IP_TOS)
static
ERL_NIF_TERM esock_getopt_tos(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              level,
                              int              opt)
{
    ERL_NIF_TERM result;
    int          val = 0;

    if (! esock_getopt_int(descP->sock, level, opt, &val)) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        result = esock_make_ok2(env, encode_ip_tos(env, val));
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__


#if defined(HAVE_IPV6)


/* esock_getopt_ipv6_mtu_discover - Level IPv6 MTU_DISCOVER option
 */
#ifndef __WIN32__
#if defined(IPV6_MTU_DISCOVER)
static
ERL_NIF_TERM esock_getopt_ipv6_mtu_discover(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            int              level,
                                            int              opt)
{
    ERL_NIF_TERM  result;
    int           mtuDisc;

    if (! esock_getopt_int(descP->sock, level, opt, &mtuDisc)) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eMtuDisc;
        encode_ipv6_pmtudisc(env, mtuDisc, &eMtuDisc);
        result = esock_make_ok2(env, eMtuDisc);
    }

    return result;

}
#endif
#endif // #ifndef __WIN32__


#endif // defined(HAVE_IPV6)



/* esock_getopt_tcp_congestion - Level TCP CONGESTION option
 */
#ifndef __WIN32__
#if defined(TCP_CONGESTION)
static
ERL_NIF_TERM esock_getopt_tcp_congestion(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt)
{
    int max = ESOCK_OPT_TCP_CONGESTION_NAME_MAX+1;

    return esock_getopt_str_opt(env, descP, level, opt, max, TRUE);
}
#endif
#endif // #ifndef __WIN32__



#if defined(HAVE_SCTP)



/* esock_getopt_sctp_associnfo - Level SCTP ASSOCINFO option
 *
 * <KOLLA>
 *
 * We should really specify which association this relates to,
 * as it is now we get assoc-id = 0. If this socket is an
 * association (and not an endpoint) then it will have an
 * assoc id. But since the sctp support at present is "limited",
 * we leave it for now.
 * What do we do if this is an endpoint? Invalid op? Or just leave
 * it for the OS?
 *
 * </KOLLA>
 */
#ifndef __WIN32__
#if defined(SCTP_ASSOCINFO)
static
ERL_NIF_TERM esock_getopt_sctp_associnfo(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              level,
                                         int              opt)
{
    ERL_NIF_TERM            result;
    struct sctp_assocparams val;
    SOCKOPTLEN_T            valSz = sizeof(val);
    int                     res;

    sys_memzero((char*) &val, valSz);
    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result =  esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eAssocParams;
        ERL_NIF_TERM keys[]  = {atom_assoc_id,
                                atom_asocmaxrxt,
                                atom_number_peer_destinations,
                                atom_peer_rwnd,
                                atom_local_rwnd,
                                atom_cookie_life};
        ERL_NIF_TERM vals[]  = {encode_sctp_assoc_t(env, val.sasoc_assoc_id),
                                MKUI(env, val.sasoc_asocmaxrxt),
                                MKUI(env, val.sasoc_number_peer_destinations),
                                MKUI(env, val.sasoc_peer_rwnd),
                                MKUI(env, val.sasoc_local_rwnd),
                                MKUI(env, val.sasoc_cookie_life)};
        size_t numKeys        = NUM(keys);

        ESOCK_ASSERT( numKeys == NUM(vals) );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &eAssocParams) );
        result = esock_make_ok2(env, eAssocParams);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__




/* esock_getopt_sctp_initmsg - Level SCTP INITMSG option
 *
 */
#ifndef __WIN32__
#if defined(SCTP_INITMSG)
static
ERL_NIF_TERM esock_getopt_sctp_initmsg(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              level,
                                       int              opt)
{
    ERL_NIF_TERM        result;
    struct sctp_initmsg val;
    SOCKOPTLEN_T        valSz = sizeof(val);
    int                 res;

    sys_memzero((char*) &val, valSz);
    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eInitMsg;
        ERL_NIF_TERM keys[]  = {atom_num_outstreams, atom_max_instreams,
                                atom_max_attempts, atom_max_init_timeo};
        ERL_NIF_TERM vals[]  = {MKUI(env, val.sinit_num_ostreams),
                                MKUI(env, val.sinit_max_instreams),
                                MKUI(env, val.sinit_max_attempts),
                                MKUI(env, val.sinit_max_init_timeo)};
        unsigned int numKeys = NUM(keys);
        unsigned int numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &eInitMsg) );
        result = esock_make_ok2(env, eInitMsg);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__


/* esock_getopt_sctp_rtoinfo - Level SCTP ASSOCINFO option
 *
 * <KOLLA>
 *
 * We should really specify which association this relates to,
 * as it is now we get assoc-id = 0. If this socket is an
 * association (and not an endpoint) then it will have an
 * assoc id (we can assume). But since the sctp support at
 * present is "limited", we leave it for now.
 * What do we do if this is an endpoint? Invalid op?
 *
 * </KOLLA>
 */
#ifndef __WIN32__
#if defined(SCTP_RTOINFO)
static
ERL_NIF_TERM esock_getopt_sctp_rtoinfo(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              level,
                                       int              opt)
{
    ERL_NIF_TERM        result;
    struct sctp_rtoinfo val;
    SOCKOPTLEN_T        valSz = sizeof(val);
    int                 res;

    sys_memzero((char*) &val, valSz);
    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eRTOInfo;
        ERL_NIF_TERM keys[]  = {atom_assoc_id, atom_initial, atom_max, atom_min};
        ERL_NIF_TERM vals[]  = {encode_sctp_assoc_t(env, val.srto_assoc_id),
                                MKUI(env, val.srto_initial),
                                MKUI(env, val.srto_max),
                                MKUI(env, val.srto_min)};
        unsigned int numKeys = NUM(keys);
        unsigned int numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &eRTOInfo) );
        result = esock_make_ok2(env, eRTOInfo);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__



#endif // defined(HAVE_SCTP)



/* esock_getopt_bool_opt - get an (integer) bool option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_bool_opt(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              opt)
{
    ERL_NIF_TERM result;
    int          val = 0;

    if (! esock_getopt_int(descP->sock, level, opt, &val)) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM bval = ((val) ? atom_true : atom_false);

        result = esock_make_ok2(env, bval);
    }
    return result;
}
#endif // #ifndef __WIN32__


/* esock_getopt_int_opt - get an integer option
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_int_opt(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              level,
                                  int              opt)
{
    int val;

    if (! esock_getopt_int(descP->sock, level, opt, &val))
        return esock_make_error_errno(env, sock_errno());

    return esock_make_ok2(env, MKI(env, val));
}
#endif // #ifndef __WIN32__



/* esock_getopt_int - get an integer option
 */
#ifndef __WIN32__
static
BOOLEAN_T esock_getopt_int(SOCKET           sock,
                           int              level,
                           int              opt,
                           int*             valP)
{
    int          val = 0;
    SOCKOPTLEN_T valSz = sizeof(val);

    if (sock_getopt(sock, level, opt, &val, &valSz) != 0)
        return FALSE;

    *valP = val;
    return TRUE;
}
#endif // #ifndef __WIN32__



#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_size_opt(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              opt,
                                   SOCKOPTLEN_T     valueSz)
{
    ERL_NIF_TERM result;
    int          res;

    if (valueSz == 0) {
        res = sock_getopt(descP->sock, level, opt, NULL, NULL);
        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;
    } else {
        SOCKOPTLEN_T vsz = valueSz;
        ErlNifBinary val;

        ESOCK_ASSERT( ALLOC_BIN(vsz, &val) );
        sys_memzero(val.data, val.size);
        res = sock_getopt(descP->sock, level, opt, val.data, &vsz);
        if (res != 0) {
            result = esock_make_error_errno(env, sock_errno());
            FREE_BIN(&val);
        } else {

            /* Did we use all of the buffer? */
            if (vsz == val.size) {
                result = esock_make_ok2(env, MKBIN(env, &val));

            } else {

                ERL_NIF_TERM tmp;

                tmp = MKBIN(env, &val);
                tmp = MKSBIN(env, tmp, 0, vsz);

                result = esock_make_ok2(env, tmp);
            }
        }
    }

    return result;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
ERL_NIF_TERM esock_getopt_bin_opt(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              opt,
                                   ErlNifBinary*    binP)
{
    ERL_NIF_TERM result;
    int          res;
    SOCKOPTLEN_T vsz;
    ErlNifBinary val;

    vsz = (SOCKOPTLEN_T) binP->size;
    if (SZT(vsz) != binP->size) {
        result = esock_make_error_invalid(env, atom_data_size);
    } else {
        ESOCK_ASSERT( ALLOC_BIN(vsz, &val) );
        sys_memcpy(val.data, binP->data, vsz);
        res = sock_getopt(descP->sock, level, opt, val.data, &vsz);
        if (res != 0) {
            result = esock_make_error_errno(env, sock_errno());
            FREE_BIN(&val);
        } else {

            /* Did we use all of the buffer? */
            if (vsz == val.size) {
                result = esock_make_ok2(env, MKBIN(env, &val));

            } else {

                ERL_NIF_TERM tmp;

                tmp = MKBIN(env, &val);
                tmp = MKSBIN(env, tmp, 0, vsz);

                result = esock_make_ok2(env, tmp);
            }
        }
    }

    return result;
}
#endif // #ifndef __WIN32__


/* esock_getopt_timeval_opt - get an timeval option
 */
#ifndef __WIN32__
#if (defined(SO_RCVTIMEO) || defined(SO_SNDTIMEO)) \
    && defined(ESOCK_USE_RCVSNDTIMEO)
static
ERL_NIF_TERM esock_getopt_timeval_opt(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      int              opt)
{
    ERL_NIF_TERM   result;
    struct timeval val;
    SOCKOPTLEN_T   valSz = sizeof(val);
    int            res;

    sys_memzero((char*) &val, valSz);
    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eTimeVal;

        esock_encode_timeval(env, &val, &eTimeVal);
        result = esock_make_ok2(env, eTimeVal);
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(IP_PKTOPTIONS) || defined(IPV6_PKTOPTIONS)

/* Calculate CMSG_NXTHDR without having a struct msghdr*.
 * CMSG_LEN only caters for alignment for start of data.
 * To get how much to advance we need to use CMSG_SPACE
 * on the payload length.  To get the payload length we
 * take the calculated cmsg->cmsg_len and subtract the
 * header length.  To get the header length we use
 * the pointer difference from the cmsg start pointer
 * to the CMSG_DATA(cmsg) pointer.
 *
 * Some platforms (seen on ppc Linux 2.6.29-3.ydl61.3)
 * may return 0 as the cmsg_len if the cmsg is to be ignored.
 */
#define LEN_CMSG_DATA(__CMSG__)                                             \
    ((__CMSG__)->cmsg_len < sizeof (struct cmsghdr) ? 0 :                   \
     (__CMSG__)->cmsg_len - ((char*)CMSG_DATA(__CMSG__) - (char*)(__CMSG__)))
#define NEXT_CMSG_HDR(__CMSG__)                                              \
    ((struct cmsghdr*)(((char*)(__CMSG__)) + CMSG_SPACE(LEN_CMSG_DATA(__CMSG__))))

static
ERL_NIF_TERM esock_getopt_pktoptions(ErlNifEnv*       env,
				     ESockDescriptor* descP,
				     int              level,
				     int              opt)
{
  ERL_NIF_TERM result, ePktOpts;
  int          res;
  ErlNifBinary cmsgs;
  SOCKOPTLEN_T sz       = (SOCKOPTLEN_T) descP->rCtrlSz;
  SocketTArray cmsghdrs = TARRAY_CREATE(16);
  ERL_NIF_TERM ctrlBuf;

  ESOCK_ASSERT( ALLOC_BIN(sz, &cmsgs) );

  sys_memzero(cmsgs.data, cmsgs.size);
  sz  = cmsgs.size; // Make no assumption about the size
  res = sock_getopt(descP->sock, level, opt, cmsgs.data, &sz);

  if (res != 0) {
    result = esock_make_error_errno(env, sock_errno());
  } else {
    struct cmsghdr* currentP;
    struct cmsghdr* endOfBuf;

    ctrlBuf = MKBIN(env, &cmsgs); // The *entire* binary

    for (endOfBuf = (struct cmsghdr*)(cmsgs.data + cmsgs.size),
	 currentP = (struct cmsghdr*)(cmsgs.data);
	 (currentP != NULL) && (currentP < endOfBuf);
	 currentP = NEXT_CMSG_HDR(currentP)) {
      unsigned char* dataP   = UCHARP(CMSG_DATA(currentP));
      size_t         dataPos = dataP - cmsgs.data;
      size_t         dataLen = (UCHARP(currentP) + currentP->cmsg_len) - dataP;

      SSDBG( descP,
	     ("SOCKET", "esock_getopt_pktoptions {%d} -> cmsg header data: "
	      "\r\n   currentP: 0x%lX"
	      "\r\n         level: %d"
	      "\r\n         data:  %d"
	      "\r\n         len:   %d [0x%lX]"
	      "\r\n   dataP:    0x%lX"
	      "\r\n   dataPos:  %d"
	      "\r\n   dataLen:  %d [0x%lX]"
	      "\r\n", descP->sock,
	      currentP,
	      currentP->cmsg_level,
	      currentP->cmsg_type,
	      currentP->cmsg_len, currentP->cmsg_len,
	      dataP,
	      dataPos,
	      dataLen, dataLen) );

      /*
       * Check that this is within the allocated buffer...
       * The 'next control message header' is a bit adhoc,
       * so this check is a bit...
       */
      if ((dataPos > cmsgs.size) ||
	  (dataLen > cmsgs.size) ||
	  ((dataPos + dataLen) > ((size_t)endOfBuf))) {
	break;
      } else {
	ERL_NIF_TERM cmsgHdr;
	ERL_NIF_TERM keys[] = {esock_atom_level,
			       esock_atom_type,
			       esock_atom_data,
			       atom_value};
	ERL_NIF_TERM vals[NUM(keys)];
	size_t       numKeys = NUM(keys);
	BOOLEAN_T    haveValue;
    
	vals[0] = esock_encode_level(env, currentP->cmsg_level);
	vals[2] = MKSBIN(env, ctrlBuf, dataPos, dataLen);

	haveValue = encode_cmsg(env,
				currentP->cmsg_level,
				currentP->cmsg_type,
				dataP, dataLen, &vals[1], &vals[3]);

	SSDBG( descP,
	       ("SOCKET", "esock_getopt_pktoptions {%d} -> "
		"\r\n   %T: %T"
		"\r\n   %T: %T"
		"\r\n   %T: %T"
		"\r\n", descP->sock,
		keys[0], vals[0], keys[1], vals[1], keys[2], vals[2]) );

	if (haveValue) {
	  SSDBG( descP,
		 ("SOCKET", "esock_getopt_pktoptions {%d} -> "
		  "\r\n   %T: %T"
		  "\r\n", descP->sock, keys[3], vals[3]) );
	}

	/* Guard agains cut-and-paste errors */
	ESOCK_ASSERT( numKeys == NUM(vals) );

	/* Make control message header map */
	ESOCK_ASSERT( MKMA(env, keys, vals,
			   numKeys - (haveValue ? 0 : 1), &cmsgHdr) );

	SSDBG( descP,
	       ("SOCKET", "esock_getopt_pktoptions {%d} -> header processed: "
		"\r\n   %T"
		"\r\n", descP->sock, cmsgHdr) );

	/* And finally add it to the list... */
	TARRAY_ADD(cmsghdrs, cmsgHdr);	    
      }
    }

    SSDBG( descP,
           ("SOCKET", "esock_getopt_pktoptions {%d} -> "
	    "cmsg headers processed when"
            "\r\n   TArray Size: %d"
            "\r\n", descP->sock, TARRAY_SZ(cmsghdrs)) );

    /* The tarray is populated - convert it to a list */
    TARRAY_TOLIST(cmsghdrs, env, &ePktOpts);

    result = esock_make_ok2(env, ePktOpts);
  }

  FREE_BIN(&cmsgs);

  return result;
}
#endif
#endif // #ifndef __WIN32__




/* esock_getopt_str_opt - get a string option
 *
 * We provide the max size of the string. This is the
 * size of the buffer we allocate for the value.
 * The actual size of the (read) value will be communicated
 * in the valSz variable.
 */
#ifndef __WIN32__
#if defined(USE_GETOPT_STR_OPT)
static
ERL_NIF_TERM esock_getopt_str_opt(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              level,
                                  int              opt,
                                  int              max,
                                  BOOLEAN_T        stripNUL)
{
    ERL_NIF_TERM result;
    char*        val   = MALLOC(max);
    SOCKOPTLEN_T valSz = max;
    int          res;

    ESOCK_ASSERT( val != NULL );

    sys_memzero(val, max);
    res = sock_getopt(descP->sock, level, opt, val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        if (stripNUL &&
            valSz > 0 &&
            val[valSz - 1] == '\0') valSz--;

        result = esock_make_ok2(env, MKSL(env, val, valSz));
    }
    FREE(val);

    return result;
}
#endif // if defined(USE_GETOPT_STR_OPT)
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_sockname - get socket name
 *
 * Description:
 * Returns the current address to which the socket is bound.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 */

static
ERL_NIF_TERM nif_sockname(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     res;

    ESOCK_ASSERT( argc == 1 );

    SGDBG( ("SOCKET", "nif_sockname -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "nif_sockname(%T) {%d}"
            "\r\n", argv[0], descP->sock) );

    res = esock_sockname(env, descP);

    SSDBG( descP,
           ("SOCKET", "nif_sockname(%T) {%d} -> done with res = %T\r\n",
            argv[0], descP->sock, res) );

    MUNLOCK(descP->readMtx);

    return res;
#endif // #ifdef __WIN32__  #else
}



#ifndef __WIN32__
static
ERL_NIF_TERM esock_sockname(ErlNifEnv*       env,
                            ESockDescriptor* descP)
{
    ESockAddress  sa;
    ESockAddress* saP = &sa;
    SOCKLEN_T     sz  = sizeof(ESockAddress);

    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);
    
    sys_memzero((char*) saP, sz);
    if (sock_name(descP->sock, (struct sockaddr*) saP, &sz) < 0) {
        return esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM esa;

        esock_encode_sockaddr(env, saP, sz, &esa);
        return esock_make_ok2(env, esa);
    }
}
#endif // #ifndef __WIN32__




/* ----------------------------------------------------------------------
 * nif_peername - get name of the connected peer socket
 *
 * Description:
 * Returns the address of the peer connected to the socket.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 */

static
ERL_NIF_TERM nif_peername(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     res;

    ESOCK_ASSERT( argc == 1 );

    SGDBG( ("SOCKET", "nif_peername -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "nif_peername(%T) {%d}"
            "\r\n", argv[0], descP->sock) );

    res = esock_peername(env, descP);

    SSDBG( descP,
           ("SOCKET", "nif_peername(%T) {%d} -> done with res = %T\r\n",
            argv[0], descP->sock, res) );

    MUNLOCK(descP->readMtx);

    return res;
#endif // #ifdef __WIN32__  #else
}



#ifndef __WIN32__
static
ERL_NIF_TERM esock_peername(ErlNifEnv*       env,
                            ESockDescriptor* descP)
{
    ESockAddress  sa;
    ESockAddress* saP = &sa;
    SOCKLEN_T     sz  = sizeof(ESockAddress);

    if (! IS_OPEN(descP->readState))
        return esock_make_error(env, atom_closed);

    sys_memzero((char*) saP, sz);
    if (sock_peer(descP->sock, (struct sockaddr*) saP, &sz) < 0) {
        return esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM esa;

        esock_encode_sockaddr(env, saP, sz, &esa);
        return esock_make_ok2(env, esa);
    }
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 * nif_cancel
 *
 * Description:
 * Cancel a previous select!
 *
 * Arguments:
 * Socket    (ref)  - Points to the socket descriptor.
 * Operation (atom) - What kind of operation (accept, send, ...) is to be cancelled
 * Ref       (ref)  - Unique id for the operation
 */
static
ERL_NIF_TERM nif_cancel(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     op, sockRef, opRef;

    ESOCK_ASSERT( argc == 3 );

    SGDBG( ("SOCKET", "nif_cancel -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    sockRef = argv[0];
    if (! ESOCK_GET_RESOURCE(env, sockRef, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    op    = argv[1];
    opRef = argv[2];
    if ((! IS_ATOM(env, op)) ||
        (! enif_is_ref(env, opRef))) {
        return enif_make_badarg(env);
    }

    return esock_cancel(env, descP, op, sockRef, opRef);

#endif // #ifdef __WIN32__  #else
}


#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     op,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     opRef)
{
    int cmp;

    /* <KOLLA>
     *
     * Do we really need all these variants? Should it not be enough with: 
     *
     *     connect | accept | send | recv
     *
     * </KOLLA>
     */

    /* Hand crafted binary search */
    if ((cmp = COMPARE(op, esock_atom_recvmsg)) == 0)
        return esock_cancel_recv(env, descP, sockRef, opRef);
    if (cmp < 0) {
        if ((cmp = COMPARE(op, esock_atom_recv)) == 0)
            return esock_cancel_recv(env, descP, sockRef, opRef);
        if (cmp < 0) {
            if (COMPARE(op, esock_atom_connect) == 0)
                return esock_cancel_connect(env, descP, opRef);
            if (COMPARE(op, esock_atom_accept) == 0)
                return esock_cancel_accept(env, descP, sockRef, opRef);
        } else {
            if (COMPARE(op, esock_atom_recvfrom) == 0)
                return esock_cancel_recv(env, descP, sockRef, opRef);
        }
    } else {
        if ((cmp = COMPARE(op, esock_atom_sendmsg)) == 0)
            return esock_cancel_send(env, descP, sockRef, opRef);
        if (cmp < 0) {
            if (COMPARE(op, esock_atom_send) == 0)
                return esock_cancel_send(env, descP, sockRef, opRef);
            if (COMPARE(op, atom_sendfile) == 0)
                return esock_cancel_send(env, descP, sockRef, opRef);
        } else {
            if (COMPARE(op, esock_atom_sendto) == 0)
                return esock_cancel_send(env, descP, sockRef, opRef);
        }
    }

    {
        ERL_NIF_TERM result;
        const char *reason;

        MLOCK(descP->readMtx);
        MLOCK(descP->writeMtx);

        if (! IS_OPEN(descP->readState)) {
            result = esock_make_error(env, atom_closed);
            reason = "closed";
        } else {
            result = enif_make_badarg(env);
            reason = "badarg";
        }

        SSDBG( descP,
               ("SOCKET", "esock_cancel(%T), {%d,0x%X} -> %s"
                "\r\n",
                sockRef,  descP->sock,
                descP->readState | descP->writeState, reason) );

        MUNLOCK(descP->writeMtx);
        MUNLOCK(descP->readMtx);

        return result;
    }
}
#endif // #ifndef __WIN32__



/* *** esock_cancel_connect ***
 *
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_connect(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;
    ErlNifPid self;

    ESOCK_ASSERT( enif_self(env, &self) != NULL );

    MLOCK(descP->writeMtx);

    if (! IS_OPEN(descP->writeState)) {

        res = esock_make_error(env, atom_closed);

    } else if ((descP->connectorP == NULL) ||
               (COMPARE_PIDS(&self, &descP->connector.pid) != 0) ||
               (COMPARE(opRef, descP->connector.ref) != 0)) {

        res = esock_atom_not_found;

    } else {

        res = esock_cancel_write_select(env, descP, opRef);
        requestor_release("esock_cancel_connect",
                          env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;
    }

    SSDBG( descP,
           ("SOCKET",
            "esock_cancel_connect {%d,0x%X} ->"
            "\r\n   opRef: %T"
            "\r\n   res: %T"
            "\r\n",
            descP->sock, descP->writeState,
            opRef, res) );

    MUNLOCK(descP->writeMtx);

    return res;
}
#endif // #ifndef __WIN32__


/* *** esock_cancel_accept ***
 *
 * We have two different cases:
 *   *) Its the current acceptor
 *      Cancel the select!
 *      We need to activate one of the waiting acceptors.
 *   *) Its one of the acceptors ("waiting") in the queue
 *      Simply remove the acceptor from the queue.
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_accept(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET",
            "esock_cancel_accept(%T), {%d,0x%X} ->"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n",
            sockRef,  descP->sock, descP->readState,
            opRef,
            ((descP->currentAcceptorP == NULL)
             ? "without acceptor" : "with acceptor")) );

    if (! IS_OPEN(descP->readState)) {

        res = esock_make_error(env, atom_closed);

    } else if (descP->currentAcceptorP == NULL) {

        res = esock_atom_not_found;

    } else {
        ErlNifPid self;

        ESOCK_ASSERT( enif_self(env, &self) != NULL );

        if (COMPARE_PIDS(&self, &descP->currentAcceptor.pid) == 0) {
            if (COMPARE(opRef, descP->currentAcceptor.ref) == 0)
                res = esock_cancel_accept_current(env, descP, sockRef);
            else
                res = esock_atom_not_found;
        } else {
            res = esock_cancel_accept_waiting(env, descP, opRef, &self);
        }
    }

    SSDBG( descP,
           ("SOCKET", "esock_cancel_accept(%T) -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, res) );

    MUNLOCK(descP->readMtx);

    return res;
}
#endif // #ifndef __WIN32__


/* The current acceptor process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the acceptor queue).
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_accept_current(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    ESOCK_ASSERT( DEMONP("esock_cancel_accept_current -> current acceptor",
                         env, descP, &descP->currentAcceptor.mon) == 0);
    res = esock_cancel_read_select(env, descP, descP->currentAcceptor.ref);

    SSDBG( descP,
           ("SOCKET",
            "esock_cancel_accept_current(%T) {%d} -> cancel res: %T"
            "\r\n", sockRef, descP->sock, res) );

    if (!activate_next_acceptor(env, descP, sockRef)) {

        SSDBG( descP,
               ("SOCKET",
                "esock_cancel_accept_current(%T) {%d} -> "
                "no more acceptors\r\n",
                sockRef, descP->sock) );

        descP->readState &= ~ESOCK_STATE_ACCEPTING;

        descP->currentAcceptorP = NULL;
    }

    return res;
}
#endif // #ifndef __WIN32__


/* These processes have not performed a select, so we can simply
 * remove them from the acceptor queue.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_accept_waiting(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     opRef,
                                         const ErlNifPid* selfP)
{
    /* unqueue request from (acceptor) queue */

    if (acceptor_unqueue(env, descP, &opRef, selfP)) {
        return esock_atom_ok;
    } else {
        return esock_atom_not_found;
    }
}
#endif // #ifndef __WIN32__



/* *** esock_cancel_send ***
 *
 * Cancel a send operation.
 * Its either the current writer or one of the waiting writers.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_send(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET",
            "esock_cancel_send(%T), {%d,0x%X} -> entry with"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n",
            sockRef,  descP->sock, descP->writeState,
            opRef,
            ((descP->currentWriterP == NULL)
             ? "without writer" : "with writer")) );

    if (! IS_OPEN(descP->writeState)) {

        res = esock_make_error(env, atom_closed);

    } else if (descP->currentWriterP == NULL) {

        res = esock_atom_not_found;

    } else {
        ErlNifPid self;

        ESOCK_ASSERT( enif_self(env, &self) != NULL );

        if (COMPARE_PIDS(&self, &descP->currentWriter.pid) == 0) {
            if (COMPARE(opRef, descP->currentWriter.ref) == 0)
                res = esock_cancel_send_current(env, descP, sockRef);
            else
                res = esock_atom_not_found;
        } else {
            res = esock_cancel_send_waiting(env, descP, opRef, &self);
        }
    }

    SSDBG( descP,
           ("SOCKET", "esock_cancel_send(%T) {%d} -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, descP->sock, res) );

    MUNLOCK(descP->writeMtx);

    return res;
}
#endif // #ifndef __WIN32__



/* The current writer process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the writer queue).
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_send_current(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    ESOCK_ASSERT( DEMONP("esock_cancel_send_current -> current writer",
                         env, descP, &descP->currentWriter.mon) == 0);
    res = esock_cancel_write_select(env, descP, descP->currentWriter.ref);

    SSDBG( descP,
           ("SOCKET", "esock_cancel_send_current(%T) {%d} -> cancel res: %T"
            "\r\n", sockRef, descP->sock, res) );

    if (!activate_next_writer(env, descP, sockRef)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_cancel_send_current(%T) {%d} -> no more writers"
                "\r\n", sockRef, descP->sock) );

        descP->currentWriterP = NULL;
    }

    return res;
}
#endif // #ifndef __WIN32__


/* These processes have not performed a select, so we can simply
 * remove them from the writer queue.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_send_waiting(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     opRef,
                                       const ErlNifPid* selfP)
{
    /* unqueue request from (writer) queue */

    if (writer_unqueue(env, descP, &opRef, selfP)) {
        return esock_atom_ok;
    } else {
        return esock_atom_not_found;
    }
}
#endif // #ifndef __WIN32__



/* *** esock_cancel_recv ***
 *
 * Cancel a read operation.
 * Its either the current reader or one of the waiting readers.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_recv(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET",
            "esock_cancel_recv(%T), {%d,0x%X} -> entry with"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n",
            sockRef,  descP->sock, descP->readState,
            opRef,
            ((descP->currentReaderP == NULL)
             ? "without reader" : "with reader")) );

    if (! IS_OPEN(descP->readState)) {

        res = esock_make_error(env, atom_closed);

    } else if (descP->currentReaderP == NULL) {

        res =  esock_atom_not_found;

    } else {
        ErlNifPid self;

        ESOCK_ASSERT( enif_self(env, &self) != NULL );

        if (COMPARE_PIDS(&self, &descP->currentReader.pid) == 0) {
            if (COMPARE(opRef, descP->currentReader.ref) == 0)
                res = esock_cancel_recv_current(env, descP, sockRef);
            else
                res =  esock_atom_not_found;
        } else {
            res = esock_cancel_recv_waiting(env, descP, opRef, &self);
        }
    }

    SSDBG( descP,
           ("SOCKET", "esock_cancel_recv(%T) {%d} -> done with result:"
            "\r\n   %T"
            "\r\n", sockRef, descP->sock, res) );

    MUNLOCK(descP->readMtx);

    return res;
}
#endif // #ifndef __WIN32__


/* The current reader process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the reader queue).
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_recv_current(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    ESOCK_ASSERT( DEMONP("esock_cancel_recv_current -> current reader",
                         env, descP, &descP->currentReader.mon) == 0);
    res = esock_cancel_read_select(env, descP, descP->currentReader.ref);

    SSDBG( descP,
           ("SOCKET", "esock_cancel_recv_current(%T) {%d} -> cancel res: %T"
            "\r\n", sockRef, descP->sock, res) );

    if (! activate_next_reader(env, descP, sockRef)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_cancel_recv_current(%T) {%d} -> no more readers"
                "\r\n", sockRef, descP->sock) );

        descP->currentReaderP = NULL;
    }

    return res;
}
#endif // #ifndef __WIN32__


/* These processes have not performed a select, so we can simply
 * remove them from the reader queue.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_recv_waiting(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     opRef,
                                       const ErlNifPid* selfP)
{
    /* unqueue request from (reader) queue */

    if (reader_unqueue(env, descP, &opRef, selfP)) {
        return esock_atom_ok;
    } else {
        return esock_atom_not_found;
    }
}
#endif // #ifndef __WIN32__



#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_read_select(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     opRef)
{
    return esock_cancel_mode_select(env, descP, opRef,
                                    ERL_NIF_SELECT_READ,
                                    ERL_NIF_SELECT_READ_CANCELLED);
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_write_select(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     opRef)
{
    return esock_cancel_mode_select(env, descP, opRef,
                                    ERL_NIF_SELECT_WRITE,
                                    ERL_NIF_SELECT_WRITE_CANCELLED);
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
ERL_NIF_TERM esock_cancel_mode_select(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     opRef,
                                      int              smode,
                                      int              rmode)
{
    /* Assumes cancelling only one mode */

    int selectRes = esock_select_cancel(env, descP->sock, smode, descP);

    if (selectRes >= 0) {
        /* Success */
        if ((selectRes & rmode) != 0) {
            /* Was cancelled */
            return esock_atom_ok;
        } else {
            /* Has already sent the message */
            return esock_atom_select_sent;
        }
    } else {
        /* Stopped? */
        SSDBG( descP,
               ("SOCKET",
                "esock_cancel_mode_select {%d} -> failed: %d (0x%lX)"
                "\r\n", descP->sock, selectRes, selectRes) );

        return esock_atom_not_found;
    }
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 *  U t i l i t y   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* *** send_check_writer ***
 *
 * Checks if we have a current writer and if that is us.
 * If not (current writer), then we must be made to wait
 * for our turn. This is done by pushing us unto the writer queue.
 */
#ifndef __WIN32__
static
BOOLEAN_T send_check_writer(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     ref,
                            ERL_NIF_TERM*    checkResult)
{
    if (descP->currentWriterP != NULL) {
        ErlNifPid caller;
        
        ESOCK_ASSERT( enif_self(env, &caller) != NULL );

        if (COMPARE_PIDS(&descP->currentWriter.pid, &caller) != 0) {
            /* Not the "current writer", so (maybe) push onto queue */

            SSDBG( descP,
                   ("SOCKET",
                    "send_check_writer {%d} -> not (current) writer"
                    "\r\n   ref: %T"
                    "\r\n", descP->sock, ref) );

            if (! writer_search4pid(env, descP, &caller)) {
                writer_push(env, descP, caller, ref);
                *checkResult = atom_select;
            } else {
                /* Writer already in queue */
                *checkResult = esock_raise_invalid(env, atom_state);
            }
            
            SSDBG( descP,
                   ("SOCKET",
                    "send_check_writer {%d} -> queue (push) result: %T\r\n"
                    "\r\n   ref: %T"
                    "\r\n", descP->sock, *checkResult, ref) );
            
            return FALSE;
        }
    }

    // Does not actually matter in this case, but ...
    *checkResult = esock_atom_ok;

    return TRUE;
}
#endif // #ifndef __WIN32__


/* *** send_check_result ***
 *
 * Check the result of a socket send (send, sendto and sendmsg) call.
 * If a "complete" send has been made, the next (waiting) writer will be 
 * scheduled (if there is one).
 * If we did not manage to send the entire package, make another select,
 * so that we can be informed when we can make another try (to send the rest),
 * and return with the amount we actually managed to send (its up to the caller
 * (that is the erlang code) to figure out hust much is left to send).
 * If the write fail, we give up and return with the appropriate error code.
 *
 * What about the remaining writers!!
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM send_check_result(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ssize_t          send_result,
                               ssize_t          dataSize,
                               BOOLEAN_T        dataInTail,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     sendRef)
{
    ERL_NIF_TERM res;
    BOOLEAN_T    send_error;
    int          err;

    send_error = ESOCK_IS_ERROR(send_result);
    err = send_error ? sock_errno() : 0;

    SSDBG( descP,
           ("SOCKET", "send_check_result(%T) {%d} -> entry with"
            "\r\n   send_result:  %ld"
            "\r\n   dataSize:     %ld"
            "\r\n   err:          %d"
            "\r\n   sendRef:      %T"
            "\r\n", sockRef, descP->sock,
            (long) send_result, (long) dataSize, err, sendRef) );

    if (send_error) {
        /* Some kind of send failure - check what kind */
        if ((err != EAGAIN) && (err != EINTR)) {
            res = send_check_fail(env, descP, err, sockRef);
        } else {
            /* Ok, try again later */

            SSDBG( descP,
                   ("SOCKET",
                    "send_check_result(%T) {%d} -> try again"
                    "\r\n", sockRef, descP->sock) );

            res = send_check_retry(env, descP, -1, sockRef, sendRef);
        }
    } else {
        ssize_t written = send_result;
        ESOCK_ASSERT( dataSize >= written );

        if (written < dataSize) {
            /* Not the entire package */
            SSDBG( descP,
                   ("SOCKET",
                    "send_check_result(%T) {%d} -> "
                    "not entire package written (%d of %d)"
                    "\r\n", sockRef, descP->sock,
                    written, dataSize) );

            res = send_check_retry(env, descP, written, sockRef, sendRef);
        } else if (dataInTail) {
            /* Not the entire package */
            SSDBG( descP,
                   ("SOCKET",
                    "send_check_result(%T) {%d} -> "
                    "not entire package written (%d but data in tail)"
                    "\r\n", sockRef, descP->sock,
                    written) );

            res =
                send_check_retry(env, descP, written, sockRef,
                                 esock_atom_iov);
        } else {
            res = send_check_ok(env, descP, written, sockRef);
        }
    }

    SSDBG( descP,
           ("SOCKET",
            "send_check_result(%T) {%d} -> done:"
            "\r\n   res: %T"
            "\r\n", sockRef, descP->sock,
            res) );

    return res;
}
#endif // #ifndef __WIN32__


/* *** send_check_ok ***
 *
 * Processing done upon successful send.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM send_check_ok(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ssize_t          written,
                           ERL_NIF_TERM     sockRef)
{
    ESOCK_CNT_INC(env, descP, sockRef,
                  atom_write_pkg, &descP->writePkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef,
                  atom_write_byte, &descP->writeByteCnt, written);
    descP->writePkgMaxCnt += written;
    if (descP->writePkgMaxCnt > descP->writePkgMax)
        descP->writePkgMax = descP->writePkgMaxCnt;
    descP->writePkgMaxCnt = 0;

    SSDBG( descP,
           ("SOCKET", "send_check_ok(%T) {%d} -> "
            "everything written (%ld) - done\r\n",
            sockRef, descP->sock, written) );

    if (descP->currentWriterP != NULL) {
        ESOCK_ASSERT( DEMONP("send_check_ok -> current writer",
                             env, descP, &descP->currentWriter.mon) == 0);
    }
    /*
     * Ok, this write is done maybe activate the next (if any)
     */
    if (!activate_next_writer(env, descP, sockRef)) {

        SSDBG( descP,
               ("SOCKET", "send_check_ok(%T) {%d} -> no more writers\r\n",
                sockRef, descP->sock) );

        descP->currentWriterP = NULL;
    }

    return esock_atom_ok;
}
#endif // #ifndef __WIN32__


/* *** send_check_failure ***
 *
 * Processing done upon failed send.
 * An actual failure - we (and everyone waiting) give up.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM send_check_fail(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              saveErrno,
                             ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM reason;

    ESOCK_CNT_INC(env, descP, sockRef, atom_write_fails, &descP->writeFails, 1);

    SSDBG( descP, ("SOCKET", "send_check_fail(%T) {%d} -> error: %d\r\n",
                   sockRef, descP->sock, saveErrno) );

    reason = MKA(env, erl_errno_id(saveErrno));

    if (saveErrno != EINVAL) {

        /*
         * We assume that anything other then einval (invalid input)
         * is basically fatal (=> all waiting sends are aborted)
         */

        if (descP->currentWriterP != NULL) {

            requestor_release("send_check_fail",
                              env, descP, &descP->currentWriter);

            send_error_waiting_writers(env, descP, sockRef, reason);

            descP->currentWriterP = NULL;
        }
    }
    return esock_make_error(env, reason);
}
#endif // #ifndef __WIN32__


/* *** send_error_waiting_writers ***
 *
 * Process all waiting writers when a fatal error has occured.
 * All waiting writers will be "aborted", that is a
 * nif_abort message will be sent (with ref and reason).
 */
#ifndef __WIN32__
static
void send_error_waiting_writers(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     reason)
{
    ESockRequestor req;

    req.env = NULL; /* read by writer_pop before free */
    while (writer_pop(env, descP, &req)) {
        SSDBG( descP,
               ("SOCKET",
                "send_error_waiting_writers(%T) {%d} -> abort"
                "\r\n   pid:    %T"
                "\r\n   reason: %T"
                "\r\n",
                sockRef, descP->sock, &req.pid, reason) );

        esock_send_abort_msg(env, descP, sockRef, &req, reason);

        (void) DEMONP("send_error_waiting_writers -> pop'ed writer",
                      env, descP, &req.mon);
    }
}
#endif // #ifndef __WIN32__


/* *** send_check_retry ***
 *
 * Processing done upon uncomplete or blocked send.
 *
 * We failed to write the *entire* packet (anything less
 * then size of the packet, which is 0 <= written < sizeof
 * packet, so schedule the rest for later.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM send_check_retry(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ssize_t          written,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     sendRef)
{
    int          sres;
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET",
            "send_check_retry(%T) {%d} -> %ld"
            "\r\n", sockRef, descP->sock, (long) written) );

    if (written >= 0) {
        descP->writePkgMaxCnt += written;

        if (descP->type != SOCK_STREAM) {
            /* Partial write for packet oriented socket
             * - done with packet
             */
            if (descP->writePkgMaxCnt > descP->writePkgMax)
                descP->writePkgMax = descP->writePkgMaxCnt;
            descP->writePkgMaxCnt = 0;

            ESOCK_CNT_INC(env, descP, sockRef,
                          atom_write_pkg, &descP->writePkgCnt, 1);
            ESOCK_CNT_INC(env, descP, sockRef,
                          atom_write_byte, &descP->writeByteCnt, written);

            if (descP->currentWriterP != NULL) {
                ESOCK_ASSERT( DEMONP("send_check_retry -> current writer",
                                     env, descP,
                                     &descP->currentWriter.mon) == 0);
            }
            /*
             * Ok, this write is done maybe activate the next (if any)
             */
            if (!activate_next_writer(env, descP, sockRef)) {

                SSDBG( descP,
                       ("SOCKET",
                        "send_check_retry(%T) {%d} -> no more writers\r\n",
                        sockRef, descP->sock) );

                descP->currentWriterP = NULL;
            }

            return esock_make_ok2(env, MKI64(env, written));
        } /* else partial write for stream socket */
    } /* else send would have blocked */

    /* Register this process as current writer */

    if (descP->currentWriterP == NULL) {
        /* Register writer as current */

        ESOCK_ASSERT( enif_self(env, &descP->currentWriter.pid) != NULL );
        ESOCK_ASSERT( MONP("send_check_retry -> current writer",
                           env, descP,
                           &descP->currentWriter.pid,
                           &descP->currentWriter.mon) == 0 );
        ESOCK_ASSERT( descP->currentWriter.env == NULL );

        descP->currentWriter.env = esock_alloc_env("current-writer");
        descP->currentWriter.ref =
            CP_TERM(descP->currentWriter.env, sendRef);
        descP->currentWriterP = &descP->currentWriter;
    } else {
        /* Overwrite current writer registration */
        enif_clear_env(descP->currentWriter.env);
        descP->currentWriter.ref = CP_TERM(descP->currentWriter.env, sendRef);
    }

    if (COMPARE(sendRef, esock_atom_iov) == 0) {
        ESOCK_ASSERT( written >= 0 );
        /* IOV iteration - do not select */
        return MKT2(env, esock_atom_iov, MKI64(env, written));
    }

    /* Select write for this process */

    sres = esock_select_write(env, descP->sock, descP, NULL, sockRef, sendRef);

    if (sres < 0) {
        ERL_NIF_TERM reason;

        /* Internal select error */
        ESOCK_ASSERT( DEMONP("send_check_retry - select error",
                             env, descP, &descP->currentWriter.mon) == 0);

        /* Fail all queued writers */
        reason = MKT2(env, atom_select_write, MKI(env, sres));
        requestor_release("send_check_retry - select error",
                          env, descP, &descP->currentWriter);
        send_error_waiting_writers(env, descP, sockRef, reason);
        descP->currentWriterP = NULL;

        res =
            enif_raise_exception(env,
                                 MKT2(env, atom_select_write,
                                      MKI(env, sres)));

    } else {
        ESOCK_CNT_INC(env, descP, sockRef, atom_write_waits,
                      &descP->writeWaits, 1);

        descP->writeState |= ESOCK_STATE_SELECTED;

        if (written >= 0) {
            /* Partial write success */
            res = MKT2(env, atom_select, MKI64(env, written));
        } else {
            /* No write - try again */
            res = atom_select;
        }
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** recv_check_reader ***
 *
 * Checks if we have a current reader and if that is us. If not,
 * then we must be made to wait for our turn. This is done by pushing
 * us unto the reader queue.
 * Note that we do *not* actually initiate the currentReader structure
 * here, since we do not actually know yet if we need to! We do that in
 * the [recv|recvfrom|recvmsg]_check_result function.
 */
#ifndef __WIN32__
static
BOOLEAN_T recv_check_reader(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     ref,
                            ERL_NIF_TERM*    checkResult)
{
    if (descP->currentReaderP != NULL) {
        ErlNifPid caller;
        
        ESOCK_ASSERT( enif_self(env, &caller) != NULL );

        if (COMPARE_PIDS(&descP->currentReader.pid, &caller) != 0) {
            /* Not the "current reader", so (maybe) push onto queue */

            SSDBG( descP,
                   ("SOCKET",
                    "recv_check_reader {%d} -> not (current) reader"
                    "\r\n   ref: %T"
                    "\r\n", descP->sock, ref) );

            if (! reader_search4pid(env, descP, &caller)) {
                if (COMPARE(ref, atom_zero) == 0)
                    goto done_ok;
                reader_push(env, descP, caller, ref);
                *checkResult = atom_select;
            } else {
                /* Reader already in queue */
                *checkResult = esock_raise_invalid(env, atom_state);
            }
            
            SSDBG( descP,
                   ("SOCKET",
                    "recv_check_reader {%d} -> queue (push) result: %T\r\n",
                    descP->sock, *checkResult) );

            return FALSE;
        }
    }

 done_ok:
    // Does not actually matter in this case, but ...
    *checkResult = esock_atom_ok;
    return TRUE;
}
#endif // #ifndef __WIN32__


/* *** recv_init_current_reader ***
 *
 * Initiate (maybe) the currentReader structure of the descriptor.
 * Including monitoring the calling process.
 */
#ifndef __WIN32__
static
void recv_init_current_reader(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     recvRef)
{
    if (descP->currentReaderP == NULL) {

        ESOCK_ASSERT( enif_self(env, &descP->currentReader.pid) != NULL );

        ESOCK_ASSERT( MONP("recv_init_current_reader -> current reader",
                           env, descP,
                           &descP->currentReader.pid,
                           &descP->currentReader.mon) == 0);
        ESOCK_ASSERT(!descP->currentReader.env);

        descP->currentReader.env = esock_alloc_env("current-reader");
        descP->currentReader.ref =
            CP_TERM(descP->currentReader.env, recvRef);
        descP->currentReaderP = &descP->currentReader;
    } else {

        /*
         * This is a retry:
         * We have done, for instance, recv(Sock, X), but only received Y < X.
         * We then call recv again with size = X-Y. So, we then get a new ref.
         * 
         * Make use of the existing environment
         */

        enif_clear_env(descP->currentReader.env);
        descP->currentReader.ref = CP_TERM(descP->currentReader.env, recvRef);
    }
}
#endif // #ifndef __WIN32__


/* *** recv_update_current_reader ***
 *
 * Demonitors the current reader process and pop's the reader queue.
 * If there is a waiting (reader) process, then it will be assigned
 * as the new current reader and a new (read) select will be done.
 */
#ifndef __WIN32__
static void
recv_update_current_reader(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef)
{
    if (descP->currentReaderP != NULL) {
        
        ESOCK_ASSERT( DEMONP("recv_update_current_reader",
                             env, descP, &descP->currentReader.mon) == 0);

        if (! activate_next_reader(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "recv_update_current_reader(%T) {%d} -> no more readers\r\n",
                    sockRef, descP->sock) );

            descP->currentReaderP = NULL;
        }
    }
}
#endif // #ifndef __WIN32__


/* *** recv_error_current_reader ***
 *
 * Process the current reader and any waiting readers
 * when a read (fatal) error has occured.
 * All waiting readers will be "aborted", that is a 
 * nif_abort message will be sent (with ref and reason).
 */
#ifndef __WIN32__
static
void recv_error_current_reader(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     reason)
{
    if (descP->currentReaderP != NULL) {
        ESockRequestor req;

        requestor_release("recv_error_current_reader",
                          env, descP, &descP->currentReader);

        req.env = NULL; /* read by reader_pop before free */
        while (reader_pop(env, descP, &req)) {

            SSDBG( descP,
                   ("SOCKET", "recv_error_current_reader(%T) {%d} -> abort"
                    "\r\n   pid:   %T"
                    "\r\n   reason %T"
                    "\r\n", sockRef, descP->sock,
                    req.pid, reason) );

            esock_send_abort_msg(env, descP, sockRef, &req, reason);

            ESOCK_ASSERT( DEMONP("recv_error_current_reader -> pop'ed reader",
                                 env, descP, &req.mon) == 0);
        }

        descP->currentReaderP = NULL;
    }
}
#endif // #ifndef __WIN32__


/* *** recv_check_result ***
 *
 * Process the result of a call to recv.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_result(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ssize_t          read,
                               ssize_t          toRead,
                               int              saveErrno,
                               ErlNifBinary*    bufP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET", "recv_check_result(%T) {%d} -> entry with"
            "\r\n   read:      %ld"
            "\r\n   toRead:    %ld"
            "\r\n   saveErrno: %d"
            "\r\n   recvRef:   %T"
            "\r\n", sockRef, descP->sock,
            (long) read, (long) toRead, saveErrno, recvRef) );


    /* <KOLLA>
     *
     * We need to handle read = 0 for other type(s) (DGRAM) when
     * its actually valid to read 0 bytes.
     *
     * </KOLLA>
     */
    
    if ((read == 0) && (descP->type == SOCK_STREAM)) {
        ERL_NIF_TERM reason = atom_closed;
        res = esock_make_error(env, reason);
        
        ESOCK_CNT_INC(env, descP, sockRef,
                      atom_read_fails, &descP->readFails, 1);

        /*
         * When a stream socket peer has performed an orderly shutdown,
         * the return value will be 0 (the traditional "end-of-file" return).
         *
         * *We* do never actually try to read 0 bytes!
         *
         * We must also notify any waiting readers!
         */

        recv_error_current_reader(env, descP, sockRef, reason);

        FREE_BIN(bufP);

    } else {
    
        /* There is a special case: If the provided 'to read' value is
         * zero (0) (only for type =/= stream).
         * That means that we read as much as we can, using the default
         * read buffer size.
         */

        if (bufP->size == read) {

            /* +++ We filled the buffer +++ */

            SSDBG( descP,
                   ("SOCKET",
                    "recv_check_result(%T) {%d} -> [%lu] filled the buffer\r\n",
                    sockRef, descP->sock, (unsigned long) bufP->size) );

            res = recv_check_full(env, descP, read, toRead, bufP,
                                  sockRef, recvRef);

        } else if (read < 0) {

            /* +++ Error handling +++ */

            res = recv_check_fail(env, descP, saveErrno, bufP, NULL,
                                  sockRef, recvRef);

        } else {

            /* +++ We did not fill the buffer +++ */

            SSDBG( descP,
                   ("SOCKET",
                    "recv_check_result(%T) {%d} -> [%lu] "
                    "did not fill the buffer (%ld)\r\n",
                    sockRef, descP->sock, (unsigned long) bufP->size,
                    (long) read) );

            res = recv_check_partial(env, descP, read, toRead, bufP,
                                     sockRef, recvRef);
        }
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** recv_check_full ***
 *
 * This function is called if we filled the allocated buffer.
 * But are we done yet?
 *
 * toRead = 0 means: Give me everything you have => maybe
 * toRead > 0 means: Yes
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_full(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ssize_t          read,
                             ssize_t          toRead,
                             ErlNifBinary*    bufP,
                             ERL_NIF_TERM     sockRef,
                             ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    if ((toRead == 0) &&
        (descP->type == SOCK_STREAM)) {

        /* +++ Give us everything you have got =>     *
         *     (maybe) needs to continue          +++ */

        /* Send up each chunk of data for each of the read
         * and let the erlang code assemble it: {more, Bin}
         * (when complete it should return {ok, Bin}).
         * We need to read atleast one more time to be sure if its
         * done...
         *
         * Also, we need to check if the rNumCnt has reached its max (rNum),
         * in which case we will assume the read to be done!
         */

        SSDBG( descP,
               ("SOCKET", "recv_check_full(%T) {%d} -> shall we continue reading?"
                "\r\n   read:    %ld"
                "\r\n   rNum:    %u"
                "\r\n   rNumCnt: %u"
                "\r\n", sockRef, descP->sock,
                (unsigned long) read, descP->rNum, descP->rNumCnt) );

        res = recv_check_full_maybe_done(env, descP, read, bufP,
                                         sockRef, recvRef);

    } else {

        /* +++ We got exactly as much as we requested => We are done +++ */

        SSDBG( descP,
               ("SOCKET",
                "recv_check_full(%T) {%d} -> [%ld] "
                "we got exactly what we could fit\r\n",
                sockRef, descP->sock, (long) toRead) );

        res = recv_check_full_done(env, descP, read, bufP, sockRef);

    }

    return res;

}
#endif // #ifndef __WIN32__


/* *** recv_check_full_maybe_done ***
 *
 * Send up each chunk of data for each of the read
 * and let the erlang code assemble it: {more, Bin}
 * (when complete it should return {ok, Bin}).
 * We need to read at least one more time to be sure if its
 * done...
 *
 * Also, we need to check if the rNumCnt has reached its max (rNum),
 * in which case we will assume the read to be done!
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_full_maybe_done(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ssize_t          read,
                                        ErlNifBinary*    bufP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     recvRef)
{
    ESOCK_CNT_INC(env, descP, sockRef,
                  atom_read_byte, &descP->readByteCnt, read);
    descP->readPkgMaxCnt += read;

    descP->rNumCnt++;
    if (descP->rNumCnt >= descP->rNum) {

        descP->rNumCnt = 0;

        ESOCK_CNT_INC(env, descP, sockRef,
                      atom_read_pkg, &descP->readPkgCnt, 1);
        if (descP->readPkgMaxCnt > descP->readPkgMax)
            descP->readPkgMax = descP->readPkgMaxCnt;
        descP->readPkgMaxCnt = 0;

        recv_update_current_reader(env, descP, sockRef);

        /* This transfers "ownership" of the *allocated* binary to an
         * erlang term (no need for an explicit free).
         */

        return esock_make_ok2(env, MKBIN(env, bufP));

    }

    /* Yes, we *do* need to continue reading */

    recv_init_current_reader(env, descP, recvRef);

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */

    SSDBG( descP,
           ("SOCKET",
            "recv_check_full_maybe_done(%T) {%d} -> [%lu] "
            "we are done for now - read more\r\n",
            sockRef, descP->sock, (unsigned long)bufP->size) );

    return MKT2(env, esock_atom_more, MKBIN(env, bufP));
}
#endif // #ifndef __WIN32__


/* *** recv_check_full_done ***
 *
 * A successful recv and we filled the buffer.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_full_done(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ssize_t          read,
                                  ErlNifBinary*    bufP,
                                  ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM data;

    ESOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef, atom_read_byte,
                  &descP->readByteCnt, read);

    descP->readPkgMaxCnt += read;
    if (descP->readPkgMaxCnt > descP->readPkgMax)
        descP->readPkgMax = descP->readPkgMaxCnt;
    descP->readPkgMaxCnt = 0;

    recv_update_current_reader(env, descP, sockRef);

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(env, bufP);

    return esock_make_ok2(env, data);
}
#endif // #ifndef __WIN32__


/* *** recv_check_fail ***
 *
 * Handle recv failure.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_fail(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              saveErrno,
                             ErlNifBinary*    buf1P,
                             ErlNifBinary*    buf2P,
                             ERL_NIF_TERM     sockRef,
                             ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    FREE_BIN(buf1P);
    if (buf2P != NULL) FREE_BIN(buf2P);

    if (saveErrno == ECONNRESET)  {

        /* +++ Oops - closed +++ */

        SSDBG( descP,
               ("SOCKET",
                "recv_check_fail(%T) {%d} -> econnreset: closed"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, recvRef) );

        // This is a bit overkill (to count here), but just in case...
        ESOCK_CNT_INC(env, descP, sockRef, atom_read_fails,
                      &descP->readFails, 1);

        res = recv_check_fail_econnreset(env, descP, sockRef, recvRef);

    } else if ((saveErrno == ERRNO_BLOCK) ||
               (saveErrno == EAGAIN)) {

        SSDBG( descP,
               ("SOCKET",
                "recv_check_fail(%T) {%d} -> eagain"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, recvRef) );

        if (COMPARE(recvRef, atom_zero) == 0)
            res = esock_atom_ok;
        else
            res = recv_check_retry(env, descP, sockRef, recvRef);

    } else {

        SSDBG( descP,
               ("SOCKET",
                "recv_check_fail(%T) {%d} -> errno: %d\r\n"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, saveErrno, recvRef) );

        ESOCK_CNT_INC(env, descP, sockRef, atom_read_fails,
                      &descP->readFails, 1);

        res = recv_check_fail_gen(env, descP, saveErrno, sockRef);
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** recv_check_fail_econnreset ***
 *
 * We detected that the socket was closed wile reading.
 * Inform current and waiting readers.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_fail_econnreset(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM reason = MKA(env, erl_errno_id(ECONNRESET));
    ERL_NIF_TERM res = esock_make_error(env, reason);

    /* <KOLLA>
     *
     * IF THE CURRENT PROCESS IS *NOT* THE CONTROLLING
     * PROCESS, WE NEED TO INFORM IT!!!
     *
     * ALL WAITING PROCESSES MUST ALSO GET THE ERROR!!
     * HANDLED BY THE STOP (CALLBACK) FUNCTION?
     *
     * SINCE THIS IS A REMOTE CLOSE, WE DON'T NEED TO WAIT
     * FOR OUTPUT TO BE WRITTEN (NO ONE WILL READ), JUST
     * ABORT THE SOCKET REGARDLESS OF LINGER???
     *
     * </KOLLA>
     */

    recv_error_current_reader(env, descP, sockRef, reason);

    return res;
}
#endif // #ifndef __WIN32__


/* *** recv_check_retry ***
 *
 * The recv call would have blocked, so retry.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_retry(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     recvRef)
{
    int          sres;
    ERL_NIF_TERM res;

    descP->rNumCnt = 0;
    recv_init_current_reader(env, descP, recvRef);

    SSDBG( descP,
           ("SOCKET",
            "recv_check_retry(%T) {%d} -> SELECT for more"
            "\r\n   recvRef: %T"
            "\r\n", sockRef, descP->sock, recvRef) );

    if ((sres = esock_select_read(env, descP->sock, descP, NULL,
                                  sockRef, recvRef)) < 0) {
        /* Unlikely that any next reader will have better luck,
         * but why not give them a shot - the queue will be cleared
         */
        recv_update_current_reader(env, descP, sockRef);

        res =
            enif_raise_exception(env,
                                 MKT2(env, atom_select_read,
                                      MKI(env, sres)));
    } else {
        descP->readState |= ESOCK_STATE_SELECTED;
        res = atom_select;
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** recv_check_fail_gen ***
 *
 * The recv call had a "general" failure.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_fail_gen(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              saveErrno,
                                 ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM reason = MKA(env, erl_errno_id(saveErrno));

    recv_error_current_reader(env, descP, sockRef, reason);

    return esock_make_error(env, reason);
}
#endif // #ifndef __WIN32__


/* *** recv_check_partial ***
 *
 * Handle a sucessful recv which only partly filled the specified buffer.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_partial(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ssize_t          read,
                                ssize_t          toRead,
                                ErlNifBinary*    bufP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    if ((toRead == 0) ||
        (descP->type != SOCK_STREAM) ||
        (COMPARE(recvRef, atom_zero) == 0)) {

        /* +++ We got it all, but since we      +++
         * +++ did not fill the buffer, we      +++
         * +++ must split it into a sub-binary. +++
         */

        SSDBG( descP,
               ("SOCKET",
                "recv_check_partial(%T) {%d} -> [%ld] split buffer"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, (long) toRead,
                recvRef) );

        res = recv_check_partial_done(env, descP, read, bufP, sockRef);

    } else {
        /* A stream socket with specified read size
         * and not a polling read, we got a partial read
         * - return a select result to initiate a retry
         */

        SSDBG( descP,
               ("SOCKET",
                "recv_check_partial(%T) {%d} -> [%ld]"
                " only part of message - expect more"
                "\r\n   recvRef: %T"
                "\r\n", sockRef, descP->sock, (long) toRead,
                recvRef) );

        res =
            recv_check_partial_part(env, descP, read,
                                    bufP, sockRef, recvRef);
    }

    return res;
}
#endif // #ifndef __WIN32__


/* *** recv_check_partial_done ***
 *
 * A successful but only partial recv, which fulfilled the required read.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_partial_done(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ssize_t          read,
                                     ErlNifBinary*    bufP,
                                     ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM data;

    descP->rNumCnt = 0;
    ESOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef, atom_read_byte,
                  &descP->readByteCnt, read);

    descP->readPkgMaxCnt += read;
    if (descP->readPkgMaxCnt > descP->readPkgMax)
        descP->readPkgMax = descP->readPkgMaxCnt;
    descP->readPkgMaxCnt = 0;

    recv_update_current_reader(env, descP, sockRef);

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(env, bufP);
    data = MKSBIN(env, data, 0, read);

    SSDBG( descP,
           ("SOCKET", "recv_check_partial_done(%T) {%d} -> [%ld] done\r\n",
            sockRef, descP->sock, (long) read) );

    return esock_make_ok2(env, data);
}
#endif // #ifndef __WIN32__


/* *** recv_check_partial_part ***
 *
 * A successful but only partial recv, which only partly fulfilled
 * the required read.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recv_check_partial_part(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ssize_t          read,
                                     ErlNifBinary*    bufP,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;
    int          sres;

    ESOCK_CNT_INC(env, descP, sockRef, atom_read_byte,
                  &descP->readByteCnt, read);

    recv_init_current_reader(env, descP, recvRef);

    /* SELECT for more data */

    sres = esock_select_read(env, descP->sock, descP, NULL,
                             sockRef, recvRef);
    if (sres < 0) {
        /* Unlikely that any next reader will have better luck,
         * but why not give them a shot - the queue will be cleared
         */
        recv_update_current_reader(env, descP, sockRef);

        res =
            enif_raise_exception(env,
                                 MKT2(env, atom_select_read,
                                      MKI(env, sres)));
    } else {
        ERL_NIF_TERM data;

        descP->readState |= ESOCK_STATE_SELECTED;
	data = MKBIN(env, bufP);
	data = MKSBIN(env, data, 0, read);
	res = MKT2(env, atom_select, data);
    }

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    return res;
}
#endif // #ifndef __WIN32__




/* The recvfrom function delivers one (1) message. If our buffer
 * is too small, the message will be truncated. So, regardless
 * if we filled the buffer or not, we have got what we are going
 * to get regarding this message.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recvfrom_check_result(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ssize_t          read,
                                   int              saveErrno,
                                   ErlNifBinary*    bufP,
                                   ESockAddress*    fromAddrP,
                                   SOCKLEN_T        fromAddrLen,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM data, res;

    SSDBG( descP,
           ("SOCKET", "recvfrom_check_result(%T) {%d} -> entry with"
            "\r\n   read:      %ld"
            "\r\n   saveErrno: %d"
            "\r\n   recvRef:   %T"
            "\r\n", sockRef, descP->sock,
            (long) read, saveErrno, recvRef) );

    /* <KOLLA>
     *
     * We need to handle read = 0 for non_stream socket type(s) when
     * its actually valid to read 0 bytes.
     *
     * </KOLLA>
     */

    if ((read == 0) && (descP->type == SOCK_STREAM)) {

        /*
         * When a stream socket peer has performed an orderly shutdown,
         * the return value will be 0 (the traditional "end-of-file" return).
         *
         * *We* do never actually try to read 0 bytes!
         */

        ESOCK_CNT_INC(env, descP, sockRef,
                      atom_read_fails, &descP->readFails, 1);

        FREE_BIN(bufP);

        return esock_make_error(env, atom_closed);
    }

    if (read < 0) {

        /* +++ Error handling +++ */

        res = recv_check_fail(env, descP, saveErrno, bufP, NULL,
                              sockRef, recvRef);

    } else {

        /* +++ We sucessfully got a message - time to encode the address +++ */

        ERL_NIF_TERM eSockAddr;

        esock_encode_sockaddr(env,
                              fromAddrP, fromAddrLen,
                              &eSockAddr);

        if (read == bufP->size) {

            data = MKBIN(env, bufP);

        } else {

            /* +++ We got a chunk of data but +++
             * +++ since we did not fill the  +++
             * +++ buffer, we must split it   +++
             * +++ into a sub-binary.         +++
             */

            data = MKBIN(env, bufP);
            data = MKSBIN(env, data, 0, read);
        }

        ESOCK_CNT_INC(env, descP, sockRef, atom_read_pkg,
                      &descP->readPkgCnt, 1);
        ESOCK_CNT_INC(env, descP, sockRef, atom_read_byte,
                      &descP->readByteCnt, read);

        recv_update_current_reader(env, descP, sockRef);
        
        res = esock_make_ok2(env, MKT2(env, eSockAddr, data));

    }

    return res;

}
#endif // #ifndef __WIN32__



/* *** recvmsg_check_result ***
 *
 * The recvmsg function delivers one (1) message. If our buffer
 * is to small, the message will be truncated. So, regardless
 * if we filled the buffer or not, we have got what we are going
 * to get regarding this message.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recvmsg_check_result(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ssize_t          read,
                                  int              saveErrno,
                                  struct msghdr*   msgHdrP,
                                  ErlNifBinary*    dataBufP,
                                  ErlNifBinary*    ctrlBufP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET", "recvmsg_check_result(%T) {%d} -> entry with"
            "\r\n   read:      %ld"
            "\r\n   saveErrno: %d"
            "\r\n   recvRef:   %T"
            "\r\n", sockRef, descP->sock,
            (long) read, saveErrno, recvRef) );


    /* <KOLLA>
     *
     * We need to handle read = 0 for non_stream socket type(s) when
     * its actually valid to read 0 bytes.
     *
     * </KOLLA>
     */

    if ((read == 0) && (descP->type == SOCK_STREAM)) {
        
        /*
         * When a stream socket peer has performed an orderly shutdown,
         * the return value will be 0 (the traditional "end-of-file" return).
         *
         * *We* do never actually try to read 0 bytes!
         */

        ESOCK_CNT_INC(env, descP, sockRef,
                      atom_read_fails, &descP->readFails, 1);

        FREE_BIN(dataBufP); FREE_BIN(ctrlBufP);

        return esock_make_error(env, atom_closed);
    }


    if (read < 0) {

        /* +++ Error handling +++ */

        res = recv_check_fail(env, descP, saveErrno, dataBufP, ctrlBufP,
                              sockRef, recvRef);

    } else {

        /* +++ We sucessfully got a message - time to encode it +++ */

        res = recvmsg_check_msg(env, descP, read, msgHdrP,
                                dataBufP, ctrlBufP, sockRef);

    }

    return res;

}
#endif // #ifndef __WIN32__


/* *** recvmsg_check_msg ***
 *
 * We successfully read one message. Time to process.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM recvmsg_check_msg(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ssize_t          read,
                               struct msghdr*   msgHdrP,
                               ErlNifBinary*    dataBufP,
                               ErlNifBinary*    ctrlBufP,
                               ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM eMsg;

    /*
     * <KOLLA>
     *
     * The return value of recvmsg is the *total* number of bytes
     * that where successfully read. This data has been put into
     * the *IO vector*.
     *
     * </KOLLA>
     */

    encode_msg(env, descP,
               read, msgHdrP, dataBufP, ctrlBufP,
               &eMsg);

    SSDBG( descP,
           ("SOCKET", "recvmsg_check_result(%T) {%d} -> ok\r\n",
            sockRef, descP->sock) );

    ESOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);
    ESOCK_CNT_INC(env, descP, sockRef, atom_read_byte,
                  &descP->readByteCnt, read);

    recv_update_current_reader(env, descP, sockRef);

    return esock_make_ok2(env, eMsg);
}
#endif // #ifndef __WIN32__


/* +++ encode_msg +++
 *
 * Encode a msg() (recvmsg). In erlang its represented as
 * a map, which has a specific set of attributes:
 *
 *     addr (source address) - sockaddr()
 *     iov                   - [binary()]
 *     ctrl                  - [cmsg()]
 *     flags                 - msg_flags()
 */
#ifndef __WIN32__
static
void encode_msg(ErlNifEnv*       env,
                ESockDescriptor* descP,
                ssize_t          read,
                struct msghdr*   msgHdrP,
                ErlNifBinary*    dataBufP,
                ErlNifBinary*    ctrlBufP,
                ERL_NIF_TERM*    eSockAddr)
{
    ERL_NIF_TERM addr, iov, ctrl, flags;

    SSDBG( descP,
           ("SOCKET", "encode_msg {%d} -> entry with"
            "\r\n   read: %ld"
            "\r\n", descP->sock, (long) read) );

    /* The address is not used if we are connected (unless, maybe,
     * family is 'local'), so check (length = 0) before we try to encodel
     */
    if (msgHdrP->msg_namelen != 0) {
        esock_encode_sockaddr(env,
                              (ESockAddress*) msgHdrP->msg_name,
                              msgHdrP->msg_namelen,
                              &addr);
    } else {
        addr = esock_atom_undefined;
    }

    SSDBG( descP,
           ("SOCKET", "encode_msg {%d} -> encode iov"
            "\r\n   msg_iovlen: %lu"
            "\r\n",
            descP->sock,
            (unsigned long) msgHdrP->msg_iovlen) );

    esock_encode_iov(env, read,
                     msgHdrP->msg_iov, msgHdrP->msg_iovlen, dataBufP,
                     &iov);

    SSDBG( descP,
           ("SOCKET",
            "encode_msg {%d} -> try encode cmsgs\r\n",
            descP->sock) );

    encode_cmsgs(env, descP, ctrlBufP, msgHdrP, &ctrl);

    SSDBG( descP,
           ("SOCKET",
            "encode_msg {%d} -> try encode flags\r\n",
            descP->sock) );

    encode_msg_flags(env, descP, msgHdrP->msg_flags, &flags);

    SSDBG( descP,
           ("SOCKET", "encode_msg {%d} -> components encoded:"
            "\r\n   addr:  %T"
            "\r\n   ctrl:  %T"
            "\r\n   flags: %T"
            "\r\n", descP->sock, addr, ctrl, flags) );

    {
        ERL_NIF_TERM keys[]  = {esock_atom_iov,
                                esock_atom_ctrl,
                                esock_atom_flags,
                                esock_atom_addr};
        ERL_NIF_TERM vals[]  = {iov, ctrl, flags, addr};
        size_t       numKeys = NUM(keys);
        
        ESOCK_ASSERT( numKeys == NUM(vals) );
        
        SSDBG( descP,
               ("SOCKET",
                "encode_msg {%d} -> create map\r\n",
                descP->sock) );

        if (msgHdrP->msg_namelen == 0)
            numKeys--; // No addr
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, eSockAddr) );

        SSDBG( descP,
               ("SOCKET",
                "encode_msg {%d}-> map encoded\r\n",
                descP->sock) );
    }

    SSDBG( descP,
           ("SOCKET", "encode_msg {%d} -> done\r\n", descP->sock) );
}
#endif // #ifndef __WIN32__



/* +++ encode_cmsgs +++
 *
 * Encode a list of cmsg(). There can be 0 or more cmsghdr "blocks".
 *
 * Our "problem" is that we have no idea how many control messages
 * we have.
 *
 * The cmsgHdrP arguments points to the start of the control data buffer,
 * an actual binary. Its the only way to create sub-binaries. So, what we
 * need to continue processing this is to turn that into an binary erlang 
 * term (which can then in turn be turned into sub-binaries).
 *
 * We need the cmsgBufP (even though cmsgHdrP points to it) to be able
 * to create sub-binaries (one for each cmsg hdr).
 *
 * The TArray (term array) is created with the size of 128, which should
 * be enough. But if its not, then it will be automatically realloc'ed during
 * add. Once we are done adding hdr's to it, we convert the tarray to a list.
 */
#ifndef __WIN32__
static
void encode_cmsgs(ErlNifEnv*       env,
                  ESockDescriptor* descP,
                  ErlNifBinary*    cmsgBinP,
                  struct msghdr*   msgHdrP,
                  ERL_NIF_TERM*    eCMsg)
{
    ERL_NIF_TERM    ctrlBuf  = MKBIN(env, cmsgBinP); // The *entire* binary
    SocketTArray    cmsghdrs = TARRAY_CREATE(128);
    struct cmsghdr* firstP   = CMSG_FIRSTHDR(msgHdrP);
    struct cmsghdr* currentP;
    
    SSDBG( descP, ("SOCKET", "encode_cmsgs {%d} -> entry when"
                   "\r\n   msg ctrl len:  %d"
                   "\r\n   (ctrl) firstP: 0x%lX"
                   "\r\n", descP->sock,
                   msgHdrP->msg_controllen, firstP) );

    for (currentP = firstP;
         /*
          * In *old* versions of darwin, the CMSG_FIRSTHDR does not
          * check the msg_controllen, so we do it here.
          * We should really test this stuff during configure,
          * but for now, this will have to do.
          */
#if defined(__DARWIN__)
         (msgHdrP->msg_controllen >= sizeof(struct cmsghdr)) &&
             (currentP != NULL);
#else
         (currentP != NULL);
#endif
         currentP = CMSG_NXTHDR(msgHdrP, currentP)) {

        SSDBG( descP,
               ("SOCKET", "encode_cmsgs {%d} -> process cmsg header when"
                "\r\n   TArray Size: %d"
                "\r\n", descP->sock, TARRAY_SZ(cmsghdrs)) );

        /* MUST check this since on Linux the returned "cmsg" may actually
         * go too far!
         */
        if (((CHARP(currentP) + currentP->cmsg_len) - CHARP(firstP)) >
            msgHdrP->msg_controllen) {

            /* Ouch, fatal error - give up 
             * We assume we cannot trust any data if this is wrong.
             */

            SSDBG( descP,
                   ("SOCKET", "encode_cmsgs {%d} -> check failed when: "
                    "\r\n   currentP:           0x%lX"
                    "\r\n   (current) cmsg_len: %d"
                    "\r\n   firstP:             0x%lX"
                    "\r\n   =>                  %d"
                    "\r\n   msg ctrl len:       %d"
                    "\r\n", descP->sock,
                    CHARP(currentP), currentP->cmsg_len, CHARP(firstP),
                    (CHARP(currentP) + currentP->cmsg_len) - CHARP(firstP),
                    msgHdrP->msg_controllen) );

            TARRAY_ADD(cmsghdrs, esock_atom_bad_data);
            break;
            
        } else {
            unsigned char* dataP   = UCHARP(CMSG_DATA(currentP));
            size_t         dataPos = dataP - cmsgBinP->data;
            size_t         dataLen =
                (UCHARP(currentP) + currentP->cmsg_len) - dataP;
            ERL_NIF_TERM
                cmsgHdr,
                keys[]  =
                {esock_atom_level,
                 esock_atom_type,
                 esock_atom_data,
                 atom_value},
                vals[NUM(keys)];
            size_t numKeys = NUM(keys);
            BOOLEAN_T have_value;

            SSDBG( descP,
                   ("SOCKET", "encode_cmsgs {%d} -> cmsg header data: "
                    "\r\n   dataPos: %d"
                    "\r\n   dataLen: %d"
                    "\r\n", descP->sock, dataPos, dataLen) );

            vals[0] = esock_encode_level(env, currentP->cmsg_level);
            vals[2] = MKSBIN(env, ctrlBuf, dataPos, dataLen);
            have_value =
                encode_cmsg(env,
                            currentP->cmsg_level,
                            currentP->cmsg_type,
                            dataP, dataLen, &vals[1], &vals[3]);

            SSDBG( descP,
                   ("SOCKET", "encode_cmsgs {%d} -> "
                    "\r\n   %T: %T"
                    "\r\n   %T: %T"
                    "\r\n   %T: %T"
                    "\r\n", descP->sock,
                    keys[0], vals[0], keys[1], vals[1], keys[2], vals[2]) );
            if (have_value)
                SSDBG( descP,
                       ("SOCKET", "encode_cmsgs {%d} -> "
                        "\r\n   %T: %T"
                        "\r\n", descP->sock, keys[3], vals[3]) );

            /* Guard agains cut-and-paste errors */
            ESOCK_ASSERT( numKeys == NUM(vals) );
            ESOCK_ASSERT( MKMA(env, keys, vals,
                               numKeys - (have_value ? 0 : 1), &cmsgHdr) );

            /* And finally add it to the list... */
            TARRAY_ADD(cmsghdrs, cmsgHdr);
        }
    }

    SSDBG( descP,
           ("SOCKET", "encode_cmsgs {%d} -> cmsg headers processed when"
            "\r\n   TArray Size: %d"
            "\r\n", descP->sock, TARRAY_SZ(cmsghdrs)) );

    /* The tarray is populated - convert it to a list */
    TARRAY_TOLIST(cmsghdrs, env, eCMsg);
}
#endif // #ifndef __WIN32__



#ifndef __WIN32__
#ifdef SCM_TIMESTAMP
static
BOOLEAN_T esock_cmsg_encode_timeval(ErlNifEnv     *env,
                                    unsigned char *data,
                                    size_t         dataLen,
                                    ERL_NIF_TERM  *eResult) {
    struct timeval* timeP = (struct timeval *) data;

    if (dataLen < sizeof(*timeP))
        return FALSE;

    esock_encode_timeval(env, timeP, eResult);
    return TRUE;
}

static BOOLEAN_T esock_cmsg_decode_timeval(ErlNifEnv *env,
                                           ERL_NIF_TERM eValue,
                                           struct cmsghdr *cmsgP,
                                           size_t rem,
                                           size_t *usedP)
{
    struct timeval time, *timeP;

    if (! esock_decode_timeval(env, eValue, &time))
        return FALSE;

    if ((timeP = init_cmsghdr(cmsgP, rem, sizeof(*timeP), usedP)) == NULL)
        return FALSE;

    *timeP = time;
    return TRUE;
}
#endif
#endif


#ifndef __WIN32__
#if defined(IP_TOS) || defined(IP_RECVTOS)
static
BOOLEAN_T esock_cmsg_encode_ip_tos(ErlNifEnv     *env,
                                   unsigned char *data,
                                   size_t         dataLen,
                                   ERL_NIF_TERM  *eResult)
{
    unsigned char tos;

    if (dataLen < sizeof(tos))
        return FALSE;

    tos = *data;

    *eResult = encode_ip_tos(env, tos);
    return TRUE;
}

static BOOLEAN_T esock_cmsg_decode_ip_tos(ErlNifEnv *env,
                                          ERL_NIF_TERM eValue,
                                          struct cmsghdr *cmsgP,
                                          size_t rem,
                                          size_t *usedP)
{
    int tos, *tosP;

    if (! decode_ip_tos(env, eValue, &tos))
        return FALSE;

    if ((tosP = init_cmsghdr(cmsgP, rem, sizeof(*tosP), usedP)) == NULL)
        return FALSE;

    *tosP = tos;
    return TRUE;
}
#endif // #ifdef IP_TOS
#endif // #ifdef __WIN32__

#ifndef __WIN32__
#if defined(IP_TTL) || \
    defined(IPV6_HOPLIMIT) || \
    defined(IPV6_TCLASS) || defined(IPV6_RECVTCLASS)
static
BOOLEAN_T esock_cmsg_encode_int(ErlNifEnv     *env,
                                unsigned char *data,
                                size_t         dataLen,
                                ERL_NIF_TERM  *eResult) {
    int value;

    if (dataLen < sizeof(value))
        return FALSE;

    value = *((int *) data);
    *eResult = MKI(env, value);
    return TRUE;
}

static BOOLEAN_T esock_cmsg_decode_int(ErlNifEnv *env,
                                       ERL_NIF_TERM eValue,
                                       struct cmsghdr *cmsgP,
                                       size_t rem,
                                       size_t *usedP)
{
    int value, *valueP;

    if (! GET_INT(env, eValue, &value))
        return FALSE;

    if ((valueP = init_cmsghdr(cmsgP, rem, sizeof(*valueP), usedP)) == NULL)
        return FALSE;

    *valueP = value;
    return TRUE;
}
#endif
#endif


#ifndef __WIN32__
static BOOLEAN_T esock_cmsg_decode_bool(ErlNifEnv *env,
                                        ERL_NIF_TERM eValue,
                                        struct cmsghdr *cmsgP,
                                        size_t rem,
                                        size_t *usedP)
{
    BOOLEAN_T v;
    int *valueP;

    if (! esock_decode_bool(eValue, &v))
        return FALSE;

    if ((valueP = init_cmsghdr(cmsgP, rem, sizeof(*valueP), usedP)) == NULL)
        return FALSE;

    *valueP = v? 1 : 0;
    return TRUE;
}
#endif


#ifndef __WIN32__
#ifdef IP_RECVTTL
static
BOOLEAN_T esock_cmsg_encode_uchar(ErlNifEnv     *env,
                                  unsigned char *data,
                                  size_t         dataLen,
                                  ERL_NIF_TERM  *eResult) {
    unsigned char value;

    if (dataLen < sizeof(value))
        return FALSE;

    value = *data;
    *eResult = MKUI(env, value);
    return TRUE;
}
#endif
#endif

#ifndef __WIN32__
#ifdef IP_PKTINFO
static
BOOLEAN_T esock_cmsg_encode_in_pktinfo(ErlNifEnv     *env,
                                       unsigned char *data,
                                       size_t         dataLen,
                                       ERL_NIF_TERM  *eResult) {
    struct in_pktinfo* pktInfoP = (struct in_pktinfo*) data;
    ERL_NIF_TERM       ifIndex;
    ERL_NIF_TERM       specDst, addr;

    if (dataLen < sizeof(*pktInfoP))
        return FALSE;

    ifIndex  = MKUI(env, pktInfoP->ipi_ifindex);
    esock_encode_in_addr(env, &pktInfoP->ipi_spec_dst, &specDst);
    esock_encode_in_addr(env, &pktInfoP->ipi_addr, &addr);

    {
        ERL_NIF_TERM keys[] = {esock_atom_ifindex,
                               esock_atom_spec_dst,
                               esock_atom_addr};
        ERL_NIF_TERM vals[] = {ifIndex, specDst, addr};
        unsigned int numKeys = NUM(keys);
        unsigned int numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, eResult) );
    }
    return TRUE;
}
#endif
#endif

#ifndef __WIN32__
#ifdef IP_ORIGDSTADDR
static
BOOLEAN_T esock_cmsg_encode_sockaddr(ErlNifEnv     *env,
                                     unsigned char *data,
                                     size_t         dataLen,
                                     ERL_NIF_TERM  *eResult) {
    SOCKLEN_T addrLen = (SOCKLEN_T) dataLen;

    if (addrLen != dataLen)
        return FALSE;

    esock_encode_sockaddr(env,
                          (ESockAddress*) data,
                          addrLen,
                          eResult);
    return TRUE;
}
#endif
#endif

#ifndef __WIN32__
#ifdef HAVE_LINUX_ERRQUEUE_H
#if defined(IP_RECVERR) || defined(IPV6_RECVERR)
/* +++ encode_cmsg_encode_recverr +++
 *
 * Encode the extended socker error in the data part of the cmsg().
 *
 */
static
BOOLEAN_T esock_cmsg_encode_recverr(ErlNifEnv                *env,
                                    unsigned char            *data,
                                    size_t                    dataLen,
                                    ERL_NIF_TERM             *eCMsgData)
{
    struct sock_extended_err *sock_err = (struct sock_extended_err *) data;
    struct sockaddr *offender;
    BOOLEAN_T        have_offender = FALSE;
    ERL_NIF_TERM
        ee_errno, ee_origin, ee_type, ee_code, ee_info, ee_data,
        eSockAddr;

    if (dataLen < sizeof(*sock_err))
        return FALSE;

    offender = SO_EE_OFFENDER(sock_err);
    ee_errno = MKA(env, erl_errno_id(sock_err->ee_errno));
    ee_info = MKI(env, sock_err->ee_info);
    ee_data = MKI(env, sock_err->ee_data);

    switch (sock_err->ee_origin) {
#if defined(SO_EE_ORIGIN_NONE)
    case SO_EE_ORIGIN_NONE:
        ee_origin = atom_none;
        ee_type = MKI(env, sock_err->ee_type);
        ee_code = MKI(env, sock_err->ee_code);
        break;
#endif

#if defined(SO_EE_ORIGIN_LOCAL)
    case SO_EE_ORIGIN_LOCAL:
        ee_origin = esock_atom_local;
        ee_type = MKI(env, sock_err->ee_type);
        ee_code = MKI(env, sock_err->ee_code);
        break;
#endif

#if defined(SO_EE_ORIGIN_ICMP)
    case SO_EE_ORIGIN_ICMP:
        ee_origin = esock_atom_icmp;
        switch (sock_err->ee_type) {

#if defined(ICMP_DEST_UNREACH)
        case ICMP_DEST_UNREACH:
            ee_type   = atom_dest_unreach;
            switch (sock_err->ee_code) {

#if defined(ICMP_NET_UNREACH)
            case ICMP_NET_UNREACH:
                ee_code = atom_net_unreach;
                break;
#endif

#if defined(ICMP_HOST_UNREACH)
            case ICMP_HOST_UNREACH:
                ee_code = atom_host_unreach;
                break;
#endif

#if defined(ICMP_PORT_UNREACH)
            case ICMP_PORT_UNREACH:
                ee_code = atom_port_unreach;
                break;
#endif

#if defined(ICMP_FRAG_NEEDED)
            case ICMP_FRAG_NEEDED:
                ee_code = atom_frag_needed;
                break;
#endif

#if defined(ICMP_NET_UNKNOWN)
            case ICMP_NET_UNKNOWN:
                ee_code = atom_net_unknown;
                break;
#endif

#if defined(ICMP_HOST_UNKNOWN)
            case ICMP_HOST_UNKNOWN:
                ee_code = atom_host_unknown;
                break;
#endif

            default:
                ee_code = MKI(env, sock_err->ee_code);
                break;
            }
            break;
#endif // ICMP_DEST_UNREACH

#if defined(ICMP_TIME_EXCEEDED)
        case ICMP_TIME_EXCEEDED:
            ee_type = atom_time_exceeded;
            ee_code = MKI(env, sock_err->ee_code);
            break;
#endif

        default:
            ee_type = MKI(env, sock_err->ee_type);
            ee_code = MKI(env, sock_err->ee_code);
            break;
        }
        break;
#endif // SO_EE_ORIGIN_ICMP

#if defined(SO_EE_ORIGIN_ICMP6)
    case SO_EE_ORIGIN_ICMP6:
        ee_origin = esock_atom_icmp6;
        switch (sock_err->ee_type) {

#if defined(ICMPV6_DEST_UNREACH)
        case ICMPV6_DEST_UNREACH:
            ee_type = atom_dest_unreach;
            switch (sock_err->ee_code) {

#if defined(ICMPV6_NOROUTE)
            case ICMPV6_NOROUTE:
                ee_code = atom_noroute;
                break;
#endif
#if defined(ICMPV6_ADM_PROHIBITED)
            case ICMPV6_ADM_PROHIBITED:
                ee_code = atom_adm_prohibited;
                break;
#endif

#if defined(ICMPV6_NOT_NEIGHBOUR)
            case ICMPV6_NOT_NEIGHBOUR:
                ee_code = atom_not_neighbour;
                break;
#endif

#if defined(ICMPV6_ADDR_UNREACH)
            case ICMPV6_ADDR_UNREACH:
                ee_code = atom_addr_unreach;
                break;
#endif

#if defined(ICMPV6_PORT_UNREACH)
            case ICMPV6_PORT_UNREACH:
                ee_code = atom_port_unreach;
                break;
#endif

#if defined(ICMPV6_POLICY_FAIL)
            case ICMPV6_POLICY_FAIL:
                ee_code = atom_policy_fail;
                break;
#endif

#if defined(ICMPV6_REJECT_ROUTE)
            case ICMPV6_REJECT_ROUTE:
                ee_code = atom_reject_route;
                break;
#endif

            default:
                ee_code = MKI(env, sock_err->ee_code);
                break;
            }
            break;
#endif // ICMPV6_DEST_UNREACH

#if defined(ICMPV6_PKT_TOOBIG)
        case ICMPV6_PKT_TOOBIG:
            ee_type = atom_pkt_toobig;
            ee_code = MKI(env, sock_err->ee_code);
            break;
#endif

#if defined(ICMPV6_TIME_EXCEED)
        case ICMPV6_TIME_EXCEED:
            ee_type = atom_time_exceeded;
            ee_code = MKI(env, sock_err->ee_code);
            break;
#endif

        default:
            ee_type = MKI(env, sock_err->ee_type);
            ee_code = MKI(env, sock_err->ee_code);
            break;
        }
        break;
#endif // SO_EE_ORIGIN_ICMP6

#if defined(SO_EE_ORIGIN_TXSTATUS)
    case SO_EE_ORIGIN_TXSTATUS:
        ee_origin = atom_txstatus;
        ee_type   = MKI(env, sock_err->ee_type);
        ee_code   = MKI(env, sock_err->ee_code);
        break;
#endif

#if defined(SO_EE_ORIGIN_ZEROCOPY)
    case SO_EE_ORIGIN_ZEROCOPY:
        ee_origin = atom_zerocopy;
        ee_type   = MKI(env, sock_err->ee_type);
        ee_code   = MKI(env, sock_err->ee_code);
        break;
#endif

#if defined(SO_EE_ORIGIN_TXTIME)
    case SO_EE_ORIGIN_TXTIME:
        ee_origin = atom_txtime;
        ee_type   = MKI(env, sock_err->ee_type);
        ee_code   = MKI(env, sock_err->ee_code);
        break;
#endif

    default:
        ee_origin = MKI(env, sock_err->ee_origin);
        ee_type   = MKI(env, sock_err->ee_type);
        ee_code   = MKI(env, sock_err->ee_code);
        break;
    }

    have_offender = CHARP(sock_err) + dataLen > CHARP(offender);
    if (have_offender) {
        esock_encode_sockaddr(env,
                              (ESockAddress *)offender,
                              (CHARP(sock_err) + dataLen ) - CHARP(offender),
                              &eSockAddr);
    } else {
        eSockAddr = esock_atom_undefined;
    }

    {
        ERL_NIF_TERM keys[] = {esock_atom_error,
                               atom_origin,
                               esock_atom_type,
                               atom_code,
                               esock_atom_info,
                               esock_atom_data,
                               atom_offender};
        ERL_NIF_TERM vals[] = {ee_errno,
                               ee_origin,
                               ee_type,
                               ee_code,
                               ee_info,
                               ee_data,
                               eSockAddr};
        unsigned int numKeys = NUM(keys);
        unsigned int numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );
        if (! have_offender) numKeys--;
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, eCMsgData) );
    }
    return TRUE;
}
#endif // #if defined(IP_RECVERR) || defined(IPV6_RECVERR)
#endif // #ifdef HAVE_LINUX_ERRQUEUE_H
#endif // #ifndef __WIN32__

#ifndef __WIN32__
#ifdef IPV6_PKTINFO
static
BOOLEAN_T esock_cmsg_encode_in6_pktinfo(ErlNifEnv     *env,
                                        unsigned char *data,
                                        size_t         dataLen,
                                        ERL_NIF_TERM  *eResult) {
    struct in6_pktinfo* pktInfoP = (struct in6_pktinfo*) data;
    ERL_NIF_TERM        ifIndex, addr;

    if (dataLen < sizeof(*pktInfoP))
        return FALSE;
    ifIndex  = MKI(env, pktInfoP->ipi6_ifindex);
    esock_encode_in6_addr(env, &pktInfoP->ipi6_addr, &addr);
    {
        ERL_NIF_TERM keys[]  = {esock_atom_addr, esock_atom_ifindex};
        ERL_NIF_TERM vals[]  = {addr, ifIndex};
        unsigned int numKeys = NUM(keys);
        unsigned int numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, eResult) );
    }
    return TRUE;
}
#endif
#endif



#ifndef __WIN32__

struct ESockCmsgSpec {
    int type; // Message type

    // Function to encode into erlang term
    BOOLEAN_T (* encode)
    (ErlNifEnv *env, unsigned char *data, size_t dataLen,
     ERL_NIF_TERM *eResult);

    // Function to decode from erlang term
    BOOLEAN_T (* decode)
    (ErlNifEnv *env, ERL_NIF_TERM eValue,
     struct cmsghdr *cmsgP, size_t rem, size_t *usedP);

    ERL_NIF_TERM *nameP; // Pointer to option name atom
};

static int cmpESockCmsgSpec(const void *vpa, const void *vpb) {
    struct ESockCmsgSpec *a, *b;
    a = (struct ESockCmsgSpec *) vpa;
    b = (struct ESockCmsgSpec *) vpb;
    return COMPARE(*(a->nameP), *(b->nameP));
}

static struct ESockCmsgSpec
    cmsgLevelSocket[] =
    {
#if defined(SCM_CREDENTIALS)
        {SCM_CREDENTIALS, NULL, NULL,
         &esock_atom_credentials},
#elif defined(SCM_CREDS)
        {SCM_CREDS, NULL, NULL,
         &esock_atom_credentials},
#endif

#if defined(SCM_RIGHTS)
        {SCM_RIGHTS, NULL, NULL,
         &esock_atom_rights},
#endif

#if defined(SCM_TIMESTAMP)
        {SCM_TIMESTAMP,
         &esock_cmsg_encode_timeval, esock_cmsg_decode_timeval,
         &esock_atom_timestamp},
#endif
    },

    cmsgLevelIP[] =
    {
#if defined(IP_TOS)
        {IP_TOS, esock_cmsg_encode_ip_tos, esock_cmsg_decode_ip_tos,
         &esock_atom_tos},
#endif

#if defined(IP_TTL)
        {IP_TTL, esock_cmsg_encode_int, esock_cmsg_decode_int,
         &esock_atom_ttl},
#endif

#if defined(IP_RECVTTL)
        {IP_RECVTTL, esock_cmsg_encode_uchar, NULL,
         &esock_atom_recvttl},
#endif

#if defined(IP_PKTINFO)
        {IP_PKTINFO, esock_cmsg_encode_in_pktinfo, NULL,
         &esock_atom_pktinfo},
#endif

#if defined(IP_ORIGDSTADDR)
        {IP_ORIGDSTADDR, esock_cmsg_encode_sockaddr, NULL,
         &esock_atom_origdstaddr},
#endif

#if defined(IP_RECVTOS)
        {IP_RECVTOS, esock_cmsg_encode_ip_tos, NULL,
         &esock_atom_recvtos},
#endif

#if defined(IP_RECVERR)
        {IP_RECVERR,
#if defined(HAVE_LINUX_ERRQUEUE_H)
         esock_cmsg_encode_recverr,
#else
         NULL,
#endif
         NULL,
         &esock_atom_recverr},
#endif
    };

#ifdef HAVE_IPV6
static struct ESockCmsgSpec cmsgLevelIPv6[] =
    {
#if defined(IPV6_PKTINFO)
        {IPV6_PKTINFO, esock_cmsg_encode_in6_pktinfo, NULL,
         &esock_atom_pktinfo},
#endif

#if defined(IPV6_HOPLIMIT)
        {IPV6_HOPLIMIT, esock_cmsg_encode_int, esock_cmsg_decode_int,
         &esock_atom_hoplimit},
#endif

#if defined(IPV6_TCLASS)
        {IPV6_TCLASS, esock_cmsg_encode_int, esock_cmsg_decode_int,
         &esock_atom_tclass},
#endif

#if defined(IPV6_RECVTCLASS)
        {IPV6_RECVTCLASS, esock_cmsg_encode_int, NULL,
         &esock_atom_recvtclass},
#endif

#if defined(IPV6_RECVERR)
        {IPV6_RECVERR,
#if defined(HAVE_LINUX_ERRQUEUE_H)
         esock_cmsg_encode_recverr,
#else
         NULL,
#endif
         NULL,
         &esock_atom_recverr},
#endif
    };
#endif // #ifdef HAVE_IPV6

static void initCmsgTables(void) {
    ESOCK_SORT_TABLE(cmsgLevelSocket, cmpESockCmsgSpec);
    ESOCK_SORT_TABLE(cmsgLevelIP, cmpESockCmsgSpec);
#ifdef HAVE_IPV6
    ESOCK_SORT_TABLE(cmsgLevelIPv6, cmpESockCmsgSpec);
#endif
}

static struct ESockCmsgSpec *lookupCmsgTable(int level, size_t *num) {
    switch (level) {

    case SOL_SOCKET:
        *num = NUM(cmsgLevelSocket);
        return cmsgLevelSocket;

#ifdef SOL_IP
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        *num = NUM(cmsgLevelIP);
        return cmsgLevelIP;

#ifdef HAVE_IPV6
#ifdef SOL_IPV6
    case SOL_IPV6:
#else
    case IPPROTO_IPV6:
#endif
        *num = NUM(cmsgLevelIPv6);
        return cmsgLevelIPv6;
#endif

    default:
        return NULL;
    }
}

static struct ESockCmsgSpec *lookupCmsgSpec(struct ESockCmsgSpec *table,
                                            size_t num,
                                            ERL_NIF_TERM eType) {
    struct ESockCmsgSpec key;

    sys_memzero(CHARP(&key), sizeof(key));
    key.nameP = &eType;
    return bsearch(&key, table, num, sizeof(*table), cmpESockCmsgSpec);
}

#endif // #ifdef __WIN32__



#ifndef __WIN32__
static
BOOLEAN_T encode_cmsg(ErlNifEnv*     env,
                      int            level,
                      int            type,
                      unsigned char* dataP,
                      size_t         dataLen,
                      ERL_NIF_TERM*  eType,
                      ERL_NIF_TERM*  eData) {
    const struct ESockCmsgSpec *cmsgTable;
    size_t num;

    if ((cmsgTable = lookupCmsgTable(level, &num)) != NULL) {
        size_t n;

        /* Linear search for type number in level table
         */
        for (n = 0;  n < num;  n++) {
            if (cmsgTable[n].type == type) {
                /* Found the type number in the level table;
                 * return the symbolic type (atom)
                 * and try to encode the data
                 */

                *eType = *cmsgTable[n].nameP;

                if (cmsgTable[n].encode != NULL)
                    return cmsgTable[n].encode(env, dataP, dataLen, eData);
                else
                    return FALSE;
            }
        }
    }
    /* No level table, or unknown type number in the table;
     * just return the type number as an erlang integer
     */


    *eType = MKI(env, type);
    return FALSE;
}
#endif


#ifndef __WIN32__
static
BOOLEAN_T decode_cmsghdr_value(ErlNifEnv*   env,
                               ESockDescriptor* descP,
                               int          level,
                               ERL_NIF_TERM eType,
                               ERL_NIF_TERM eValue,
                               char*        bufP,
                               size_t       rem,
                               size_t*      usedP)
{
    int type;
    struct cmsghdr *cmsgP = (struct cmsghdr *) bufP;
    struct ESockCmsgSpec *cmsgTable;
    struct ESockCmsgSpec *cmsgSpecP = NULL;
    size_t num = 0;

    SSDBG( descP,
           ("SOCKET",
            "decode_cmsghdr_value {%d} -> entry  \r\n"
            "   eType:  %T\r\n"
            "   eValue: %T\r\n",
            descP->sock, eType, eValue) );

    // We have decode functions only for symbolic (atom) types
    if (! IS_ATOM(env, eType)) {
        SSDBG( descP,
               ("SOCKET",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   eType not an atom\r\n",
                descP->sock) );
        return FALSE;
    }

    /* Try to look up the symbolic type
     */
    if (((cmsgTable = lookupCmsgTable(level, &num)) == NULL) ||
        ((cmsgSpecP = lookupCmsgSpec(cmsgTable, num, eType)) == NULL) ||
        (cmsgSpecP->decode == NULL)) {
        /* We found no table for this level,
         * we found no symbolic type in the level table,
         * or no decode function for this type
         */

        SSDBG( descP,
               ("SOCKET",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   cmsgTable:  %p\r\n"
                "   cmsgSpecP:  %p\r\n",
                descP->sock, cmsgTable, cmsgSpecP) );
        return FALSE;
    }

    if (! cmsgSpecP->decode(env, eValue, cmsgP, rem, usedP)) {
        // Decode function failed
        SSDBG( descP,
               ("SOCKET",
                "decode_cmsghdr_value {%d} -> FALSE:\r\n"
                "   decode function failed\r\n",
                descP->sock) );
        return FALSE;
    }

    // Succesful decode

    type = cmsgSpecP->type;

    SSDBG( descP,
           ("SOCKET",
            "decode_cmsghdr_value {%d} -> TRUE:\r\n"
            "   level:   %d\r\n"
            "   type:    %d\r\n",
            "   *usedP:  %lu\r\n",
            descP->sock, level, type, (unsigned long) *usedP) );

    cmsgP->cmsg_level = level;
    cmsgP->cmsg_type = type;
    return TRUE;
}
#endif

#ifndef __WIN32__
static
BOOLEAN_T decode_cmsghdr_data(ErlNifEnv*   env,
                              ESockDescriptor* descP,
                              int          level,
                              ERL_NIF_TERM eType,
                              ERL_NIF_TERM eData,
                              char*        bufP,
                              size_t       rem,
                              size_t*      usedP)
{
    int type;
    ErlNifBinary bin;
    struct cmsghdr *cmsgP = (struct cmsghdr *) bufP;
    struct ESockCmsgSpec *cmsgSpecP = NULL;

    SSDBG( descP,
           ("SOCKET",
            "decode_cmsghdr_data {%d} -> entry  \r\n"
            "   eType: %T\r\n"
            "   eData: %T\r\n",
            descP->sock, eType, eData) );

    // Decode Type
    if (! GET_INT(env, eType, &type)) {
        struct ESockCmsgSpec *cmsgTable = NULL;
        size_t num = 0;

        /* Try to look up the symbolic (atom) type
         */
        if ((! IS_ATOM(env, eType)) ||
            ((cmsgTable = lookupCmsgTable(level, &num)) == NULL) ||
            ((cmsgSpecP = lookupCmsgSpec(cmsgTable, num, eType)) == NULL)) {
            /* Type was not an atom,
             * we found no table for this level,
             * or we found no symbolic type in the level table
             */

            SSDBG( descP,
                   ("SOCKET",
                    "decode_cmsghdr_data {%d} -> FALSE:\r\n"
                    "   cmsgTable:  %p\r\n"
                    "   cmsgSpecP:  %p\r\n",
                    descP->sock, cmsgTable, cmsgSpecP) );
            return FALSE;
        }

        type = cmsgSpecP->type;
    }

    // Decode Data
    if (GET_BIN(env, eData, &bin)) {
        void *p;

        p = init_cmsghdr(cmsgP, rem, bin.size, usedP);
        if (p == NULL) {
            /* No room for the data
             */

            SSDBG( descP,
                   ("SOCKET",
                    "decode_cmsghdr_data {%d} -> FALSE:\r\n"
                    "   rem:      %lu\r\n"
                    "   bin.size: %lu\r\n",
                    descP->sock,
                    (unsigned long) rem,
                    (unsigned long) bin.size) );
            return FALSE;
        }

        // Copy the binary data
        sys_memcpy(p, bin.data, bin.size);

    } else if ((! esock_cmsg_decode_int(env, eData, cmsgP, rem, usedP)) &&
               (! esock_cmsg_decode_bool(env, eData, cmsgP, rem, usedP))) {
        SSDBG( descP,
               ("SOCKET",
                "decode_cmsghdr_data {%d} -> FALSE\r\n",
                descP->sock) );
        return FALSE;
    }

    // Succesful decode

    SSDBG( descP,
           ("SOCKET",
            "decode_cmsghdr_data {%d} -> TRUE:\r\n"
            "   level:   %d\r\n"
            "   type:    %d\r\n"
            "   *usedP:  %lu\r\n",
            descP->sock, level, type, (unsigned long) *usedP) );

    cmsgP->cmsg_level = level;
    cmsgP->cmsg_type = type;
    return TRUE;
}
#endif

/* Clear the CMSG space and init the ->cmsg_len member,
 * return the position for the data, and the total used space
 */
#ifndef __WIN32__
static void *init_cmsghdr(struct cmsghdr *cmsgP,
                          size_t rem,  // Remaining space
                          size_t size, // Size of data
                          size_t *usedP)
{
    size_t space = CMSG_SPACE(size);

    if (rem < space)
        return NULL; // Not enough space

    sys_memzero(cmsgP, space);
    cmsgP->cmsg_len = CMSG_LEN(size);

    *usedP = space;
    return CMSG_DATA(cmsgP);
}
#endif


/* +++ decode_cmsghdrs +++
 *
 * Decode a list of cmsg(). There can be 0 or more "blocks".
 *
 * Each element can either be a (erlang) map that needs to be decoded,
 * or a (erlang) binary that just needs to be appended to the control
 * buffer.
 *
 * Our "problem" is that we have no idea much memory we actually need.
 *
 */
#ifndef __WIN32__
static
BOOLEAN_T decode_cmsghdrs(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     eCMsg,
                          char*            cmsgHdrBufP,
                          size_t           cmsgHdrBufLen,
                          size_t*          cmsgHdrBufUsed)
{
    ERL_NIF_TERM elem, tail, list;
    char*        bufP;
    size_t       rem, used, totUsed = 0;
    unsigned int len;
    int          i;

    SSDBG( descP, ("SOCKET", "decode_cmsghdrs {%d} -> entry with"
                   "\r\n   eCMsg:      %T"
                   "\r\n   cmsgHdrBufP:   0x%lX"
                   "\r\n   cmsgHdrBufLen: %d"
                   "\r\n", descP->sock,
                   eCMsg, cmsgHdrBufP, cmsgHdrBufLen) );

    if (! GET_LIST_LEN(env, eCMsg, &len))
        return FALSE;

    SSDBG( descP,
           ("SOCKET",
            "decode_cmsghdrs {%d} -> list length: %d\r\n",
            descP->sock, len) );

    for (i = 0, list = eCMsg, rem  = cmsgHdrBufLen, bufP = cmsgHdrBufP;
         i < len; i++) {
            
        SSDBG( descP, ("SOCKET", "decode_cmsghdrs {%d} -> process elem %d:"
                       "\r\n   (buffer) rem:     %u"
                       "\r\n   (buffer) totUsed: %u"
                       "\r\n", descP->sock, i, rem, totUsed) );

        /* Extract the (current) head of the (cmsg hdr) list */
        if (! GET_LIST_ELEM(env, list, &elem, &tail))
            return FALSE;
            
        used = 0; // Just in case...
        if (! decode_cmsghdr(env, descP, elem, bufP, rem, &used))
            return FALSE;

        bufP     = CHARP( ULONG(bufP) + used );
        rem      = SZT( rem - used );
        list     = tail;
        totUsed += used;

    }

    *cmsgHdrBufUsed = totUsed;

    SSDBG( descP, ("SOCKET", "decode_cmsghdrs {%d} -> done"
                   "\r\n   all %u ctrl headers processed"
                   "\r\n   totUsed = %lu\r\n",
                   descP->sock, len, (unsigned long) totUsed) );

    return TRUE;
}
#endif // #ifndef __WIN32__


/* +++ decode_cmsghdr +++
 *
 * Decode one cmsg(). Put the "result" into the buffer and advance the
 * pointer (of the buffer) afterwards. Also update 'rem' accordingly.
 * But before the actual decode, make sure that there is enough room in 
 * the buffer for the cmsg header (sizeof(*hdr) < rem).
 *
 * The eCMsg should be a map with three fields:
 *
 *     level :: socket | protocol() | integer()
 *     type  :: atom() | integer()
 *                                What values are valid depend on the level
 *     data  :: binary() | integer() | boolean()
 *                                The type of the data depends on
 *     or                         level and type, but can be a binary,
 *                                which means that the data is already coded.
 *     value :: term()            Which is a term matching the decode function
 */
#ifndef __WIN32__
static
BOOLEAN_T decode_cmsghdr(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ERL_NIF_TERM     eCMsg,
                         char*            bufP,
                         size_t           rem,
                         size_t*          used)
{
    ERL_NIF_TERM eLevel, eType, eData, eValue;
    int          level;

    SSDBG( descP, ("SOCKET", "decode_cmsghdr {%d} -> entry with"
                   "\r\n   eCMsg: %T"
                   "\r\n", descP->sock, eCMsg) );

    // Get 'level' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_level, &eLevel))
        return FALSE;
    SSDBG( descP, ("SOCKET", "decode_cmsghdr {%d} -> eLevel: %T"
                   "\r\n", descP->sock, eLevel) );

    // Get 'type' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_type, &eType))
        return FALSE;
    SSDBG( descP, ("SOCKET", "decode_cmsghdr {%d} -> eType:  %T"
                   "\r\n", descP->sock, eType) );

    // Decode Level
    if (! esock_decode_level(env, eLevel, &level))
        return FALSE;
    SSDBG( descP, ("SOCKET", "decode_cmsghdr {%d}-> level:  %d\r\n",
                   descP->sock, level) );

    // Get 'data' field
    if (! GET_MAP_VAL(env, eCMsg, esock_atom_data, &eData)) {

        // Get 'value' field
        if (! GET_MAP_VAL(env, eCMsg, atom_value, &eValue))
            return FALSE;
        SSDBG( descP, ("SOCKET", "decode_cmsghdr {%d} -> eValue:  %T"
                   "\r\n", descP->sock, eValue) );

        // Decode Value
        if (! decode_cmsghdr_value(env, descP, level, eType, eValue,
                                   bufP, rem, used))
            return FALSE;

    } else {

        // Verify no 'value' field
        if (GET_MAP_VAL(env, eCMsg, atom_value, &eValue))
            return FALSE;

        SSDBG( descP, ("SOCKET", "decode_cmsghdr {%d} -> eData:  %T"
                   "\r\n", descP->sock, eData) );

        // Decode Data
        if (! decode_cmsghdr_data(env, descP, level, eType, eData,
                                  bufP, rem, used))
            return FALSE;
    }

    SSDBG( descP, ("SOCKET", "decode_cmsghdr {%d}-> used:  %lu\r\n",
                   descP->sock, (unsigned long) *used) );

    return TRUE;
}
#endif // #ifndef __WIN32__



/* +++ encode_msg_flags +++
 *
 * Encode a list of msg_flag().
 *
 */
#ifndef __WIN32__
static
void encode_msg_flags(ErlNifEnv*       env,
                      ESockDescriptor* descP,
                      int              msgFlags,
                      ERL_NIF_TERM*    flags)
{
    SSDBG( descP,
           ("SOCKET", "encode_msg_flags {%d} -> entry with"
            "\r\n   msgFlags: %d (0x%lX)"
            "\r\n", descP->sock, msgFlags, msgFlags) );

    if (msgFlags == 0) {
        *flags = MKEL(env);
    } else {
        size_t n;
        SocketTArray ta = TARRAY_CREATE(10); // Just to be on the safe side

        for (n = 0;  n < NUM(msg_flags);  n++) {
            int f = msg_flags[n].flag;
            if ((f != 0) && ((msgFlags & f) == f)) {
                msgFlags &= ~f;
                TARRAY_ADD(ta, *(msg_flags[n].name));
            }
            if (msgFlags == 0) goto done;
        }
        /* Append remaining flags as an integer */
        if (msgFlags != 0)
            TARRAY_ADD(ta, MKI(env, msgFlags));

    done:
        SSDBG( descP,
               ("SOCKET", "encode_msg_flags {%d} -> flags processed when"
                "\r\n   TArray size: %d"
                "\r\n", descP->sock, TARRAY_SZ(ta)) );

        TARRAY_TOLIST(ta, env, flags);
    }
}
#endif // #ifndef __WIN32__


/* +++ decode the ip socket option TOS +++
 * The (ip) option can be provide in two ways:
 *
 *           atom() | integer()
 *
 * When its an atom it can have the values:
 *
 *       lowdelay |  throughput | reliability | mincost
 *
 */
#ifndef __WIN32__
#if defined(IP_TOS)
static
BOOLEAN_T decode_ip_tos(ErlNifEnv* env, ERL_NIF_TERM eVal, int* val)
{
    BOOLEAN_T result = FALSE;

    if (IS_ATOM(env, eVal)) {

        if (COMPARE(eVal, esock_atom_lowdelay) == 0) {
            *val   = IPTOS_LOWDELAY;
            result = TRUE;
        } else if (COMPARE(eVal, esock_atom_throughput) == 0) {
            *val   = IPTOS_THROUGHPUT;
            result = TRUE;
        } else if (COMPARE(eVal, esock_atom_reliability) == 0) {
            *val   = IPTOS_RELIABILITY;
            result = TRUE;
#if defined(IPTOS_MINCOST)
        } else if (COMPARE(eVal, esock_atom_mincost) == 0) {
            *val   = IPTOS_MINCOST;
            result = TRUE;
#endif
        } else {
            *val   = -1;
            result = FALSE;
        }
            
    } else if (IS_NUM(env, eVal)) {

        if (GET_INT(env, eVal, val)) {
            result = TRUE;
        } else {
            *val   = -1;
            result = FALSE;
        }

    } else {
        *val   = -1;
        result = FALSE;
    }

    return result;
}
#endif
#endif // #ifndef __WIN32__


/* +++ decode the ip socket option MTU_DISCOVER +++
 * The (ip) option can be provide in two ways:
 *
 *           atom() | integer()
 *
 * When it's an atom it can have the values:
 *
 *       want | dont | do | probe
 *
 */
#ifndef __WIN32__
#if defined(IP_MTU_DISCOVER)
static
BOOLEAN_T decode_ip_pmtudisc(ErlNifEnv* env, ERL_NIF_TERM eVal, int* val)
{
    if (IS_ATOM(env, eVal)) {

        if (COMPARE(eVal, atom_want) == 0) {
            *val = IP_PMTUDISC_WANT;
        } else if (COMPARE(eVal, atom_dont) == 0) {
            *val = IP_PMTUDISC_DONT;
        } else if (COMPARE(eVal, atom_do) == 0) {
            *val = IP_PMTUDISC_DO;
#if defined(IP_PMTUDISC_PROBE)
        } else if (COMPARE(eVal, atom_probe) == 0) {
            *val = IP_PMTUDISC_PROBE;
#endif
        } else {
            return FALSE;
        }

    } else if (! GET_INT(env, eVal, val)) {
        return FALSE;
    }

    return TRUE;
}
#endif
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(IPV6_MULTICAST_HOPS) || defined(IPV6_UNICAST_HOPS)
static
BOOLEAN_T decode_hops(ErlNifEnv *env, ERL_NIF_TERM eVal, int *val) {
    int hops;

    if (! GET_INT(env, eVal, &hops)) {
        if (COMPARE(eVal, esock_atom_default) == 0) {
            *val = -1;
            return TRUE;
        }
        return FALSE;
    }
    if (hops < 0 || 255 < hops)
        return FALSE;

    *val = hops;
    return TRUE;
}
#endif
#endif // #ifndef __WIN32__


/* +++ decode the ipv6 socket option MTU_DISCOVER +++
 * The (ip) option can be provide in two ways:
 *
 *           atom() | integer()
 *
 * When its an atom it can have the values:
 *
 *       want | dont | do | probe
 *
 */
#ifndef __WIN32__
#if defined(IPV6_MTU_DISCOVER)
static
BOOLEAN_T decode_ipv6_pmtudisc(ErlNifEnv* env, ERL_NIF_TERM eVal, int* val)
{
    if (IS_ATOM(env, eVal)) {

        if (COMPARE(eVal, atom_want) == 0) {
            *val = IPV6_PMTUDISC_WANT;
        } else if (COMPARE(eVal, atom_dont) == 0) {
            *val = IPV6_PMTUDISC_DONT;
        } else if (COMPARE(eVal, atom_do) == 0) {
            *val = IPV6_PMTUDISC_DO;
#if defined(IPV6_PMTUDISC_PROBE)
        } else if (COMPARE(eVal, atom_probe) == 0) {
            *val = IPV6_PMTUDISC_PROBE;
#endif
        } else {
            return FALSE;
        }

    } else if (! GET_INT(env, eVal, val)) {
        return FALSE;
    }

    return TRUE;
}
#endif
#endif // #ifndef __WIN32__


/* +++ encode the ip socket option MTU_DISCOVER +++
 * The (ip) option can be provide in two ways:
 *
 *           atom() | integer()
 *
 * If its one of the "known" values, it will be an atom:
 *
 *       want | dont | do | probe
 *
 */
#ifndef __WIN32__
#if defined(IP_MTU_DISCOVER)
static
void encode_ip_pmtudisc(ErlNifEnv* env, int val, ERL_NIF_TERM* eVal)
{
    switch (val) {
    case IP_PMTUDISC_WANT:
        *eVal = atom_want;
        break;

    case IP_PMTUDISC_DONT:
        *eVal = atom_dont;
        break;

    case IP_PMTUDISC_DO:
        *eVal = atom_do;
        break;

#if defined(IP_PMTUDISC_PROBE)
    case IP_PMTUDISC_PROBE:
        *eVal = atom_probe;
        break;
#endif

    default:
        *eVal = MKI(env, val);
        break;
    }

    return;
}
#endif
#endif // #ifndef __WIN32__


/* +++ encode the ipv6 socket option MTU_DISCOVER +++
 * The (ipv6) option can be provide in two ways:
 *
 *           atom() | integer()
 *
 * If its one of the "known" values, it will be an atom:
 *
 *       want | dont | do | probe
 *
 */
#ifndef __WIN32__
#if defined(IPV6_MTU_DISCOVER)
static
void encode_ipv6_pmtudisc(ErlNifEnv* env, int val, ERL_NIF_TERM* eVal)
{
    switch (val) {
    case IPV6_PMTUDISC_WANT:
        *eVal = atom_want;
        break;

    case IPV6_PMTUDISC_DONT:
        *eVal = atom_dont;
        break;

    case IPV6_PMTUDISC_DO:
        *eVal = atom_do;
        break;

#if defined(IPV6_PMTUDISC_PROBE)
    case IPV6_PMTUDISC_PROBE:
        *eVal = atom_probe;
        break;
#endif

    default:
        *eVal = MKI(env, val);
        break;
    }

    return;
}
#endif
#endif // #ifndef __WIN32__


/* +++ encode the ip socket option tos +++
 * The (ip) option can be provide as:
 *
 *       lowdelay |  throughput | reliability | mincost | integer()
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM encode_ip_tos(ErlNifEnv* env, int val)
{
    ERL_NIF_TERM result;

    switch (IPTOS_TOS(val)) {
    case IPTOS_LOWDELAY:
        result = esock_atom_lowdelay;
        break;

    case IPTOS_THROUGHPUT:
        result = esock_atom_throughput;
        break;

    case IPTOS_RELIABILITY:
        result = esock_atom_reliability;
        break;

#if defined(IPTOS_MINCOST)
    case IPTOS_MINCOST:
        result = esock_atom_mincost;
        break;
#endif

    default:
        result = MKI(env, val);
        break;
    }

    return result;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
#if defined(SCTP_ASSOCINFO) || defined(SCTP_RTOINOFO)

static
BOOLEAN_T decode_sctp_assoc_t(ErlNifEnv* env,
                              ERL_NIF_TERM eVal,
                              sctp_assoc_t* val)
{
    sctp_assoc_t assoc_id;
    int i;
    unsigned int ui;

    /* Ensure that the assoc_id fits whether it is signed or unsigned
     */
    if (GET_INT(env, eVal, &i)) {
        assoc_id = (sctp_assoc_t) i;
        if ((int) assoc_id == i) {
            *val = assoc_id;
            return TRUE;
        }
    } else if (GET_UINT(env, eVal, &ui)) {
        assoc_id = (sctp_assoc_t) ui;
        if ((unsigned int) assoc_id == ui) {
            *val = assoc_id;
            return TRUE;
        }
    }

    return FALSE;
}

static
ERL_NIF_TERM encode_sctp_assoc_t(ErlNifEnv* env, sctp_assoc_t val)
{
    unsigned int ui;

    ui = (unsigned int) val;
    if ((sctp_assoc_t) ui == val)
        return MKUI(env, ui);
    else
        return MKI(env, val);
}

#endif // #if defined(SCTP_ASSOCINFO) || defined(SCTP_RTOINOFO)
#endif // #ifdef __WIN32__


/* *** alloc_descriptor ***
 *
 * Allocate and perform basic initialization of a socket descriptor.
 *
 */
#ifndef __WIN32__
static
ESockDescriptor* alloc_descriptor(SOCKET sock, ErlNifEvent event)
{
    ESockDescriptor* descP;
    char buf[64]; /* Buffer used for building the mutex name */

    ESOCK_ASSERT( (descP =
                   enif_alloc_resource(esocks, sizeof(ESockDescriptor)))
                  != NULL );

    descP->pattern = ESOCK_DESC_PATTERN_CREATED;

    requestor_init(&descP->connector);
    descP->connectorP = NULL;

    sprintf(buf, "esock.w[%d]", sock);
    descP->writeMtx       = MCREATE(buf);
    descP->writeState     = 0;
    requestor_init(&descP->currentWriter);
    descP->currentWriterP = NULL; // currentWriter not used
    descP->writersQ.first = NULL;
    descP->writersQ.last  = NULL;

    descP->writePkgCnt     = 0;
    descP->writePkgMax     = 0;
    descP->writePkgMaxCnt  = 0;
    descP->writeByteCnt    = 0;
    descP->writeTries      = 0;
    descP->writeWaits      = 0;
    descP->writeFails      = 0;

#ifdef HAVE_SENDFILE
    descP->sendfileHandle      = INVALID_HANDLE;
    descP->sendfileCountersP = NULL;
#endif

    sprintf(buf, "esock.r[%d]", sock);
    descP->readMtx        = MCREATE(buf);
    descP->readState      = 0;
    requestor_init(&descP->currentReader);
    descP->currentReaderP = NULL; // currentReader not used
    descP->readersQ.first = NULL;
    descP->readersQ.last  = NULL;

    descP->readPkgCnt     = 0;
    descP->readPkgMax     = 0;
    descP->readPkgMaxCnt  = 0;
    descP->readByteCnt    = 0;
    descP->readTries      = 0;
    descP->readWaits      = 0;
    descP->readFails      = 0;

    sprintf(buf, "esock.acc[%d]", sock);
    requestor_init(&descP->currentAcceptor);
    descP->currentAcceptorP = NULL; // currentAcceptor not used
    descP->acceptorsQ.first = NULL;
    descP->acceptorsQ.last  = NULL;
    descP->accSuccess       = 0;
    descP->accFails         = 0;
    descP->accTries         = 0;
    descP->accWaits         = 0;

    sprintf(buf, "esock.close[%d]", sock);
    descP->closeEnv         = NULL;
    descP->closeRef         = esock_atom_undefined;
    enif_set_pid_undefined(&descP->closerPid);
    MON_INIT(&descP->closerMon);

    sprintf(buf, "esock.cfg[%d]", sock);
    descP->rBufSz           = ESOCK_RECV_BUFFER_SIZE_DEFAULT;
    descP->rNum             = ESOCK_RECV_BUFFER_COUNT_DEFAULT;
    descP->rNumCnt          = 0;
    descP->rCtrlSz          = ESOCK_RECV_CTRL_BUFFER_SIZE_DEFAULT;
    descP->wCtrlSz          = ESOCK_SEND_CTRL_BUFFER_SIZE_DEFAULT;
    descP->iow              = FALSE;
    descP->dbg              = ESOCK_DEBUG_DEFAULT; // Overwritten by caller
    descP->useReg           = ESOCK_CMD_USE_SOCKET_REGISTRY; // Overwritten by caller
    descP->meta.env         = esock_alloc_env("alloc_descriptor - "
                                              "meta-env");
    descP->meta.ref         = esock_atom_undefined;

    descP->sock             = sock;
    descP->event            = event;
    descP->origFD           = INVALID_SOCKET;
    descP->closeOnClose     = TRUE;

    enif_set_pid_undefined(&descP->ctrlPid);
    MON_INIT(&descP->ctrlMon);

    return descP;
}
#endif // #ifndef __WIN32__


/* Decrement counters for when a socket is closed
 */
#ifndef __WIN32__
static
void dec_socket(int domain, int type, int protocol)
{
    MLOCK(data.cntMtx);

    cnt_dec(&data.numSockets, 1);

    /* *** Domain counter *** */
    if (domain == AF_INET)
        cnt_dec(&data.numDomainInet, 1);
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (domain == AF_INET6)
        cnt_dec(&data.numDomainInet6, 1);
#endif
#ifdef HAS_AF_LOCAL
    else if (domain == AF_LOCAL)
        cnt_dec(&data.numDomainInet6, 1);
#endif

    /* *** Type counter *** */
    if (type == SOCK_STREAM)
        cnt_dec(&data.numTypeStreams, 1);
    else if (type == SOCK_DGRAM)
        cnt_dec(&data.numTypeDGrams, 1);
#ifdef SOCK_SEQPACKET
    else if (type == SOCK_SEQPACKET)
        cnt_dec(&data.numTypeSeqPkgs, 1);
#endif

    /* *** Protocol counter *** */
    if (protocol == IPPROTO_IP)
        cnt_dec(&data.numProtoIP, 1);
    else if (protocol == IPPROTO_TCP)
        cnt_dec(&data.numProtoTCP, 1);
    else if (protocol == IPPROTO_UDP)
        cnt_dec(&data.numProtoUDP, 1);
#if defined(HAVE_SCTP)
    else if (protocol == IPPROTO_SCTP)
        cnt_dec(&data.numProtoSCTP, 1);
#endif

    MUNLOCK(data.cntMtx);
}
#endif // #ifndef __WIN32__


/* Increment counters for when a socket is opened
 */
#ifndef __WIN32__
static
void inc_socket(int domain, int type, int protocol)
{
    cnt_inc(&data.numSockets, 1);
    
    /* *** Domain counter *** */
    if (domain == AF_INET)
        cnt_inc(&data.numDomainInet, 1);
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (domain == AF_INET6)
        cnt_inc(&data.numDomainInet6, 1);
#endif
#ifdef HAS_AF_LOCAL
    else if (domain == AF_LOCAL)
        cnt_inc(&data.numDomainInet6, 1);
#endif

    /* *** Type counter *** */
    if (type == SOCK_STREAM)
        cnt_inc(&data.numTypeStreams, 1);
    else if (type == SOCK_DGRAM)
        cnt_inc(&data.numTypeDGrams, 1);
#ifdef SOCK_SEQPACKET
    else if (type == SOCK_SEQPACKET)
        cnt_inc(&data.numTypeSeqPkgs, 1);
#endif

    /* *** Protocol counter *** */
    if (protocol == IPPROTO_IP)
        cnt_inc(&data.numProtoIP, 1);
    else if (protocol == IPPROTO_TCP)
        cnt_inc(&data.numProtoTCP, 1);
    else if (protocol == IPPROTO_UDP)
        cnt_inc(&data.numProtoUDP, 1);
#if defined(HAVE_SCTP)
    else if (protocol == IPPROTO_SCTP)
        cnt_inc(&data.numProtoSCTP, 1);
#endif
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 *  D e c o d e / E n c o d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

#ifndef __WIN32__
#ifdef HAVE_SETNS
/* esock_open4_get_netns - extract the netns field from the opts map
 */
static
BOOLEAN_T esock_open4_get_netns(ErlNifEnv* env, ERL_NIF_TERM opts, char** netns)
{
    ERL_NIF_TERM val;
    ErlNifBinary bin;
    char*        buf;

    /* The currently only supported extra option is: netns */
    if (!GET_MAP_VAL(env, opts, atom_netns, &val)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    /* The value should be a binary file name */
    if (! enif_inspect_binary(env, val, &bin)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    ESOCK_ASSERT( (buf = MALLOC(bin.size+1)) != NULL );

    sys_memcpy(buf, bin.data, bin.size);
    buf[bin.size] = '\0';
    *netns = buf;
    return TRUE;
}
#endif
#endif // #ifndef __WIN32__


/* ehow2how - convert internal (erlang) "shutdown how" to
 * (proper) "shutdown how"
 */
#ifndef __WIN32__
static
BOOLEAN_T ehow2how(ERL_NIF_TERM ehow, int* how)
{
    int cmp;

    cmp = COMPARE(ehow, atom_read_write);
    if (cmp == 0)
        *how = SHUT_RDWR;
    else if (cmp < 0) {
        if (COMPARE(ehow, atom_read) == 0)
            *how = SHUT_RD;
        else
            return FALSE;
    } else {
        if (COMPARE(ehow, atom_write) == 0)
            *how = SHUT_WR;
        else
            return FALSE;
    }

    return TRUE;
}
#endif // #ifndef __WIN32__



/* ecommand2command - convert erlang command to "native" command (and data)
 */
#ifndef __WIN32__
static
BOOLEAN_T ecommand2command(ErlNifEnv*    env,
                           ERL_NIF_TERM  ecommand,
                           Uint16*       command,
                           ERL_NIF_TERM* edata)
{
    ERL_NIF_TERM ecmd;

    if (!IS_MAP(env, ecommand)) {
        SGDBG( ("SOCKET", "ecommand2command -> (e)command not a map\r\n") );
        return FALSE;
    }

    /* Get the command value, and transform into integer
     * (might as well do that, since theer is no point in
     *  extracting the data if command is invalid).
     */
    if (!GET_MAP_VAL(env, ecommand, esock_atom_command, &ecmd)) {
        SGDBG( ("SOCKET", "ecommand2command -> command attribute not found\r\n") );
        return FALSE;
    }
    if (COMPARE(ecmd, esock_atom_debug) == 0) {
        *command = ESOCK_CMD_DEBUG;
    } else if (COMPARE(ecmd, atom_socket_debug) == 0) {
        *command = ESOCK_CMD_SOCKET_DEBUG;
    } else if (COMPARE(ecmd, atom_use_registry) == 0) {
        *command = ESOCK_CMD_USE_SOCKET_REGISTRY;
    } else {
        SGDBG( ("SOCKET", "ecommand2command -> unknown command %T\r\n", ecmd) );
        return FALSE;
    }

    /* Get the command data value, we do *not* convert it to 
     * the native form (here) since it may "in theory" be complex.
     */
    if (!GET_MAP_VAL(env, ecommand, esock_atom_data, edata)) {
        SGDBG( ("SOCKET", "ecommand2command -> (command) data not found\r\n") );
        return FALSE;
    }

    return TRUE;
}
#endif // #ifndef __WIN32__




#ifndef __WIN32__
#ifdef HAS_AF_LOCAL
/* strnlen doesn't exist everywhere */
/*
static
size_t my_strnlen(const char *s, size_t maxlen)
{
    size_t i = 0;
    while (i < maxlen && s[i] != '\0')
        i++;
    return i;
}
*/
#endif
#endif // #ifndef __WIN32__




/* ===========================================================================
 *
 *                   Socket Registry message functions
 *
 * ===========================================================================
 */

/* Send a (socket) add message to the socket registry process.
 * We know that this process *is* alive since the VM would
 * terminate otherwise, so there is no need to test if
 * the sending fails.
 */
#ifndef __WIN32__
static
void esock_send_reg_add_msg(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM msg = mk_reg_add_msg(env, sockRef);

    if (! esock_send_msg(env, &data.regPid, msg, NULL)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_send_reg_add_msg(%T) {%d} failed ->"
                "\r\n   regPid: %T"
                "\r\n",
                sockRef, descP->sock, MKPID(env, &data.regPid)) );
    }
}
#endif // #ifndef __WIN32__



/* Send a (socket) del message to the socket registry process.
 * We know that this process *is* alive since the VM would
 * terminate otherwise, so there is no need to test if
 * the sending fails.
 */
#ifndef __WIN32__
static
void esock_send_reg_del_msg(ErlNifEnv*   env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM sockRef)
{
    ERL_NIF_TERM msg = mk_reg_del_msg(env, sockRef);

    if (! esock_send_msg(env, &data.regPid, msg, NULL)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_send_reg_del_msg(%T) {%d} failed ->"
                "\r\n   regPid: %T"
                "\r\n",
                sockRef, descP->sock, MKPID(env, &data.regPid)) );
    }
}
#endif // #ifndef __WIN32__




/* ===========================================================================
 *
 *                   Socket user message functions
 *
 * ===========================================================================
 */

/* Send an counter wrap message to the controlling process:
 * A message in the form:
 *
 *     {'$socket', Socket, counter_wrap, Counter :: atom()}
 *
 * This message will only be sent if the iow (Inform On Wrap) is TRUE.
 */
#ifndef __WIN32__
static
void esock_send_wrap_msg(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         ERL_NIF_TERM     sockRef,
                         ERL_NIF_TERM     cnt)
{
    ERL_NIF_TERM msg = mk_wrap_msg(env, sockRef, cnt);
    
    if (! esock_send_msg(env, &descP->ctrlPid, msg, NULL)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_send_wrap_msg(%T) {%d} failed ->"
                "\r\n   ctrlPid: %T"
                "\r\n   cnt:     %T"
                "\r\n",
                sockRef, descP->sock, MKPID(env, &descP->ctrlPid), cnt) );
    }
}
#endif // #ifndef __WIN32__


/* Send an close message to the specified process:
 * A message in the form:
 *
 *     {'$socket', Socket, close, CloseRef}
 *
 * This message is for processes that is waiting in the
 * erlang API (close-) function for the socket to be "closed"
 * (actually that the 'stop' callback function has been called).
 */
#ifndef __WIN32__
static
void esock_send_close_msg(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ErlNifPid*       pid)
{
    ERL_NIF_TERM sockRef, msg;

    sockRef = enif_make_resource(descP->closeEnv, descP);
    msg     = mk_close_msg(descP->closeEnv, sockRef, descP->closeRef);

    if (! esock_send_msg(env, pid, msg, descP->closeEnv)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_send_close_msg(%T) {%d} failed ->"
                "\r\n   pid:      %T"
                "\r\n   closeRef: %T"
                "\r\n",
                sockRef, descP->sock, MKPID(env, pid), descP->closeRef) );
    }
}
#ifdef HAVE_SENDFILE
static void
esock_send_sendfile_deferred_close_msg(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    ERL_NIF_TERM sockRef, msg;
    ErlNifPid   *pid;

    pid = &data.regPid;
    sockRef = enif_make_resource(env, descP);
    msg = mk_reg_msg(env, atom_sendfile_deferred_close, sockRef);

    /* If this send should fail we have leaked a file descriptor
     * (intolerable), and if we try to close it here, on a regular
     * scheduler, it might hang "forever" due to e.g NFS
     * (out of the question), so terminating the VM
     * is the only viable option
     */
    ESOCK_ASSERT( esock_send_msg(env, pid, msg, NULL) );
}
#endif // #ifdef HAVE_SENDFILE
#endif // #ifndef __WIN32__


/* Send an abort message to the specified process:
 * A message in the form:
 *
 *     {'$socket', Socket, abort, {RecvRef, Reason}}
 *
 * This message is for processes that is waiting in the
 * erlang API functions for a select message.
 */
#ifndef __WIN32__
static
void esock_send_abort_msg(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ESockRequestor*  reqP,
                          ERL_NIF_TERM     reason)



                          
{
    ERL_NIF_TERM msg;

    msg =
        mk_abort_msg(reqP->env,
                     /* sockRef not in env so copy */
                     CP_TERM(reqP->env, sockRef), reqP->ref, reason);

    if (! esock_send_msg(env, &reqP->pid, msg, reqP->env)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_send_abort_msg(%T) {%d} failed ->"
                "\r\n   pid: %T"
                "\r\n",
                sockRef, descP->sock, MKPID(env, &reqP->pid)) );
    }
    reqP->env = NULL;
}
#endif // #ifndef __WIN32__


/* Send a message to the specified process.
 */
#ifndef __WIN32__
static
BOOLEAN_T esock_send_msg(ErlNifEnv*   env,
                         ErlNifPid*   pid,
                         ERL_NIF_TERM msg,
                         ErlNifEnv*   msgEnv)
{
    int res = enif_send(env, pid, msgEnv, msg);
    esock_free_env("esock_msg_send - msg-env", msgEnv);

    return !!res;
}
#endif // #ifndef __WIN32__



/* *** mk_reg_add_msg ***
 *
 * Construct a socket add message for the socket registry.
 *
 *         {'$socket', add, Socket}
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_reg_add_msg(ErlNifEnv*   env,
                            ERL_NIF_TERM sockRef)
{
    return mk_reg_msg(env, atom_add, sockRef);
}
#endif // #ifndef __WIN32__


/* *** mk_reg_del_msg ***
 *
 * Construct a socket del message for the socket registry.
 *
 *         {'$socket', del, Socket}
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_reg_del_msg(ErlNifEnv*   env,
                            ERL_NIF_TERM sockRef)
{
    return mk_reg_msg(env, atom_del, sockRef);
}
#endif // #ifndef __WIN32__


/* *** mk_reg_msg ***
 *
 * Construct a general message for the socket registry.
 * Tag is (at this time) either the atom 'add' or the atom 'del'.
 *
 *         {'$socket', Tag, Socket}
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_reg_msg(ErlNifEnv*   env,
                        ERL_NIF_TERM tag,
                        ERL_NIF_TERM sockRef)
{
    ERL_NIF_TERM socket = mk_socket(env, sockRef);

    return MKT3(env, esock_atom_socket_tag, tag, socket);
}
#endif // #ifndef __WIN32__


/* *** mk_abort_msg ***
 *
 * Create the abort message, which has the following form:
 *
 *     {'$socket', Socket, abort, {OpRef, Reason}}
 *
 * This message is for processes that are waiting in the
 * erlang API functions for a select (or this) message.
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_abort_msg(ErlNifEnv*   env,
                          ERL_NIF_TERM sockRef,
                          ERL_NIF_TERM opRef,
                          ERL_NIF_TERM reason)
{
    ERL_NIF_TERM info = MKT2(env, opRef, reason);
    
    return mk_socket_msg(env, sockRef, esock_atom_abort, info);
}
#endif // #ifndef __WIN32__


/* *** mk_wrap_msg ***
 *
 * Construct a counter wrap (socket) message. It has the form: 
 *
 *         {'$socket', Socket, counter_wrap, Counter}
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_wrap_msg(ErlNifEnv*   env,
                         ERL_NIF_TERM sockRef,
                         ERL_NIF_TERM cnt)
{
    return mk_socket_msg(env, sockRef, atom_counter_wrap, cnt);
}
#endif // #ifndef __WIN32__


/* *** mk_close_msg ***
 *
 * Construct a close (socket) message. It has the form: 
 *
 *         {'$socket', Socket, close, closeRef}
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_close_msg(ErlNifEnv*   env,
                          ERL_NIF_TERM sockRef,
                          ERL_NIF_TERM closeRef)
{
    return mk_socket_msg(env, sockRef, esock_atom_close, closeRef);
}
#endif // #ifndef __WIN32__


/* *** mk_select_msg ***
 *
 * Construct a select (socket) message. It has the form: 
 *
 *         {'$socket', Socket, select, selectRef}
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_select_msg(ErlNifEnv*   env,
                           ERL_NIF_TERM sockRef,
                           ERL_NIF_TERM selectRef)
{
    return mk_socket_msg(env, sockRef, atom_select, selectRef);
}
#endif // #ifndef __WIN32__


/* *** mk_socket_msg ***
 *
 * Construct the socket message:
 *
 *         {'$socket', Socket, Tag, Info}
 *
 * Socket :: socket:socket()
 * Tag    :: atom()
 * Info   :: term()
 *
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_socket_msg(ErlNifEnv*   env,
                           ERL_NIF_TERM sockRef,
                           ERL_NIF_TERM tag,
                           ERL_NIF_TERM info)
{
    ERL_NIF_TERM socket = mk_socket(env, sockRef);

    return MKT4(env, esock_atom_socket_tag, socket, tag, info);
}
#endif // #ifndef __WIN32__


/* *** mk_socket ***
 *
 * Simple utility function that construct the socket tuple:
 *
 *     socket:socket() :: {'$socket', SockRef :: reference()}
 */
#ifndef __WIN32__
static
ERL_NIF_TERM mk_socket(ErlNifEnv*   env,
                       ERL_NIF_TERM sockRef)
{
    return MKT2(env, esock_atom_socket_tag, sockRef);
}
#endif // #ifndef __WIN32__

                              
/* ----------------------------------------------------------------------
 *  S e l e c t   W r a p p e r   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* *** esock_select_read ***
 *
 * Perform a read select. When the select is triggered, a 'select'
 * message (see mk_select_msg) will be sent.
 *
 * There are two ways to handle the select message:
 * 1) Create "your own" environment and create the message using it
 *    and then pass it on to the select function.
 * 2) Or, to create the message using any available environment,
 *    and then pass a NULL pointer to the select function.
 *    This will have the effect that the select function will 
 *    create its own environment and then copy the message to it.
 * We choose the second alternative.
 */
#ifndef __WIN32__
static
int esock_select_read(ErlNifEnv*       env,
                      ErlNifEvent      event,     // The file descriptor
                      void*            obj,       // The socket descriptor object
                      const ErlNifPid* pidP,      // Destination
                      ERL_NIF_TERM     sockRef,   // Socket
                      ERL_NIF_TERM     selectRef) // "ID" of the operation
{
    ERL_NIF_TERM selectMsg = mk_select_msg(env, sockRef, selectRef);

    return enif_select_read(env, event, obj, pidP, selectMsg, NULL);

}
#endif // #ifndef __WIN32__


/* *** esock_select_write ***
 *
 * Perform a write select. When the select is triggered, a 'select'
 * message (see mk_select_msg) will be sent.
 * The sockRef is copied to the msgEnv when the socket message is created,
 * so no need to do that here, but the selectRef needs to be copied.
 */
#ifndef __WIN32__
static
int esock_select_write(ErlNifEnv*       env,
                       ErlNifEvent      event,     // The file descriptor
                       void*            obj,       // The socket descriptor
                       const ErlNifPid* pidP,       // Destination
                       ERL_NIF_TERM     sockRef,   // Socket
                       ERL_NIF_TERM     selectRef) // "ID" of the operation
{
    ERL_NIF_TERM selectMsg = mk_select_msg(env, sockRef, selectRef);

    return enif_select_write(env, event, obj, pidP, selectMsg, NULL);
}
#endif // #ifndef __WIN32__


/* *** esock_select_stop ***
 *
 * WARNING: enif_select may call esock_stop directly
 * in which case deadlock is avoided by esock_stop that checks
 * if it got a direct call and then does not lock readMtx and writeMtx.
 *
 * So readMtx and writeMtx are supposed to be locked
 * when this function is called.
 */
#ifndef __WIN32__
static
int esock_select_stop(ErlNifEnv*  env,
                      ErlNifEvent event,
                      void*       obj)
{
    return enif_select(env, event, (ERL_NIF_SELECT_STOP), obj, NULL,
                       esock_atom_undefined);
}
#endif // #ifndef __WIN32__

#ifndef __WIN32__
static
int esock_select_cancel(ErlNifEnv*             env,
                        ErlNifEvent            event,
                        enum ErlNifSelectFlags mode,
                        void*                  obj)
{
    return enif_select(env, event, (ERL_NIF_SELECT_CANCEL | mode), obj, NULL,
                       esock_atom_undefined);
}
#endif // #ifndef __WIN32__


/* ----------------------------------------------------------------------
 *  A c t i v a t e   N e x t   ( o p e r a t o r )   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* *** activate_next_acceptor ***
 * *** activate_next_writer   ***
 * *** activate_next_reader   ***
 *
 * This functions pops the requestors queue and then selects until it 
 * manages to successfully activate a requestor or the queue is empty.
 * Return value indicates if a new requestor was activated or not.
 */

#ifndef __WIN32__

#define ACTIVATE_NEXT_FUNCS                                               \
    ACTIVATE_NEXT_FUNC_DECL(acceptor, read,  currentAcceptor, acceptorsQ) \
    ACTIVATE_NEXT_FUNC_DECL(writer,   write, currentWriter,   writersQ)   \
    ACTIVATE_NEXT_FUNC_DECL(reader,   read,  currentReader,   readersQ)

#define ACTIVATE_NEXT_FUNC_DECL(F, S, R, Q)                  \
    static                                                   \
    BOOLEAN_T activate_next_##F(ErlNifEnv*       env,        \
                                ESockDescriptor* descP,      \
                                ERL_NIF_TERM     sockRef)    \
    {                                                        \
        BOOLEAN_T          popped, activated;                \
        int                sres;                             \
        ERL_NIF_TERM       reason;                           \
        ESockRequestor*    reqP = &descP->R;                 \
        ESockRequestQueue* q    = &descP->Q;                 \
                                                             \
        popped = FALSE;                                      \
        do {                                                 \
                                                             \
            if (requestor_pop(q, reqP)) {                    \
                                                             \
                /* There was another one */                  \
                                                             \
                SSDBG( descP,                                           \
                       ("SOCKET",                                       \
                        "activate_next_" #F "(%T) {%d} ->"              \
                        " new (active) requestor: "                     \
                        "\r\n   pid: %T"                                \
                        "\r\n   ref: %T"                                \
                        "\r\n", sockRef, descP->sock,                   \
                        reqP->pid, reqP->ref) );                        \
                                                                        \
                /* We need to copy req ref to 'env' */                  \
                if ((sres =                                             \
                     esock_select_##S(env, descP->sock, descP,          \
                                      &reqP->pid, sockRef,              \
                                      CP_TERM(env, reqP->ref))) < 0) {  \
                                                                        \
                    /* We need to inform this process, reqP->pid,  */   \
                    /* that we failed to select, so we don't leave */   \
                    /* it hanging.                                 */   \
                    /* => send abort                               */   \
                                                                        \
                    reason = MKT2(env,                                  \
                                  esock_atom_select_failed,             \
                                  MKI(env, sres));                      \
                    esock_send_abort_msg(env, descP, sockRef,           \
                                         reqP, reason);                 \
                                                                        \
                } else {                                                \
                                                                        \
                    descP->S##State |= ESOCK_STATE_SELECTED;            \
                                                                        \
                    /* Success: New requestor selected */               \
                    popped    = TRUE;                                   \
                    activated = TRUE;                                   \
                                                                        \
                }                                                       \
                                                                        \
            } else {                                                    \
                                                                        \
                SSDBG( descP,                                           \
                       ("SOCKET",                                       \
                        "activate_next_" #F "(%T) {%d} ->"              \
                        " no more requestors\r\n",                      \
                        sockRef, descP->sock) );                        \
                                                                        \
                popped    = TRUE;                                       \
                activated = FALSE;                                      \
            }                                                           \
                                                                        \
        } while (!popped);                                              \
                                                                        \
        SSDBG( descP,                                                   \
               ("SOCKET", "activate_next_" #F "(%T) {%d} -> "           \
                "done with %s\r\n",                                     \
                sockRef, descP->sock, B2S(activated)) );                \
                                                                        \
        return activated;                                               \
    }
ACTIVATE_NEXT_FUNCS
#undef ACTIVATE_NEXT_FUNC_DECL

#endif // #ifndef __WIN32__


/* ----------------------------------------------------------------------
 *  R e q u e s t o r   Q u e u e   F u n c t i o n s
 * ----------------------------------------------------------------------
 *
 * Since each of these functions (search4pid, push, pop and unqueue
 * are virtually identical for acceptors, writers and readers, 
 * we make use of set of declaration macros.
 */

#ifndef __WIN32__

/* *** acceptor_search4pid ***
 * *** writer_search4pid   ***
 * *** reader_search4pid   ***
 *
 * Search for a pid in the requestor (acceptor, writer, or reader) queue.
 *
 */

#define REQ_SEARCH4PID_FUNCS                       \
    REQ_SEARCH4PID_FUNC_DECL(acceptor, acceptorsQ) \
    REQ_SEARCH4PID_FUNC_DECL(writer,   writersQ)   \
    REQ_SEARCH4PID_FUNC_DECL(reader,   readersQ)

#define REQ_SEARCH4PID_FUNC_DECL(F, Q)                 \
    static                                             \
    BOOLEAN_T F##_search4pid(ErlNifEnv*       env,     \
                             ESockDescriptor* descP,   \
                             ErlNifPid*       pid)     \
    {                                                  \
        return qsearch4pid(env, &descP->Q, pid);       \
    }
REQ_SEARCH4PID_FUNCS
#undef REQ_SEARCH4PID_FUNC_DECL

#endif // #ifndef __WIN32__



/* *** acceptor_push ***
 * *** writer_push   ***
 * *** reader_push   ***
 *
 * Push a requestor (acceptor, writer, or reader) onto its queue.
 * This happens when we already have a current request (of its type).
 *
 */

#ifndef __WIN32__

#define REQ_PUSH_FUNCS                       \
    REQ_PUSH_FUNC_DECL(acceptor, acceptorsQ) \
    REQ_PUSH_FUNC_DECL(writer,   writersQ)   \
    REQ_PUSH_FUNC_DECL(reader,   readersQ)

#define REQ_PUSH_FUNC_DECL(F, Q)                                        \
    static                                                              \
    void F##_push(ErlNifEnv*       env,                                 \
                  ESockDescriptor* descP,                               \
                  ErlNifPid        pid, /* self() */                    \
                  ERL_NIF_TERM     ref)                                 \
    {                                                                   \
        ESockRequestQueueElement *e;                                    \
        ESockRequestor           *reqP;                                 \
                                                                        \
        ESOCK_ASSERT( (e = MALLOC(sizeof(ESockRequestQueueElement)))    \
                      != NULL );                                        \
        reqP = &e->data;                                                \
        reqP->pid = pid;                                                \
        ESOCK_ASSERT( MONP("reader_push -> " #F " request",             \
                           env, descP, &pid, &reqP->mon) == 0 );        \
        reqP->env = esock_alloc_env(#F "_push");                        \
        reqP->ref = CP_TERM(reqP->env, ref);                            \
                                                                        \
        qpush(&descP->Q, e);                                            \
    }
REQ_PUSH_FUNCS
#undef REQ_PUSH_FUNC_DECL

#endif // #ifndef __WIN32__



/* *** acceptor_pop ***
 * *** writer_pop   ***
 * *** reader_pop   ***
 *
 * Pop a requestor (acceptor, writer, or reader) from its queue.
 *
 */

#ifndef __WIN32__

#define REQ_POP_FUNCS                       \
    REQ_POP_FUNC_DECL(acceptor, acceptorsQ) \
    REQ_POP_FUNC_DECL(writer,   writersQ)   \
    REQ_POP_FUNC_DECL(reader,   readersQ)

#define REQ_POP_FUNC_DECL(F, Q)                \
    static                                     \
    BOOLEAN_T F##_pop(ErlNifEnv*       env,    \
                      ESockDescriptor* descP,  \
                      ESockRequestor*  reqP)   \
    {                                          \
        return requestor_pop(&descP->Q, reqP); \
    }
REQ_POP_FUNCS
#undef REQ_POP_FUNC_DECL

#endif // #ifndef __WIN32__



/* *** acceptor_unqueue ***
 * *** writer_unqueue   ***
 * *** reader_unqueue   ***
 *
 * Remove a requestor (acceptor, writer, or reader) from its queue.
 *
 */

#ifndef __WIN32__

#define REQ_UNQUEUE_FUNCS                       \
    REQ_UNQUEUE_FUNC_DECL(acceptor, acceptorsQ) \
    REQ_UNQUEUE_FUNC_DECL(writer,   writersQ)   \
    REQ_UNQUEUE_FUNC_DECL(reader,   readersQ)

#define REQ_UNQUEUE_FUNC_DECL(F, Q)                             \
    static                                                      \
    BOOLEAN_T F##_unqueue(ErlNifEnv*       env,                 \
                          ESockDescriptor* descP,               \
                          ERL_NIF_TERM*    refP,                \
                          const ErlNifPid* pidP)                \
    {                                                           \
        return qunqueue(env, descP, "qunqueue -> waiting " #F,  \
                        &descP->Q, refP, pidP);                 \
    }
REQ_UNQUEUE_FUNCS
#undef REQ_UNQUEUE_FUNC_DECL

#endif // #ifndef __WIN32__



/* *** requestor pop ***
 *
 * Pop an requestor from its queue.
 */

#ifndef __WIN32__

static
BOOLEAN_T requestor_pop(ESockRequestQueue* q,
                        ESockRequestor*    reqP)
{
    ESockRequestQueueElement* e = qpop(q);

    esock_free_env("requestor_pop", reqP->env);

    if (e != NULL) {
        reqP->pid = e->data.pid;
        reqP->mon = e->data.mon;
        reqP->env = e->data.env;
        reqP->ref = e->data.ref;
        FREE(e);
        return TRUE;
    } else {
        /* Queue was empty */
        requestor_init(reqP);
        return FALSE;
    }
    
}

static void requestor_init(ESockRequestor* reqP) {
    enif_set_pid_undefined(&reqP->pid);
    MON_INIT(&reqP->mon);
    reqP->env = NULL;
    reqP->ref = esock_atom_undefined;
}

static void requestor_release(const char*      slogan,
                              ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ESockRequestor*  reqP) {

    enif_set_pid_undefined(&reqP->pid);
    (void) DEMONP(slogan, env, descP, &reqP->mon);
    esock_free_env(slogan, reqP->env);
    reqP->env = NULL;
    reqP->ref = esock_atom_undefined;
}
#endif // #ifndef __WIN32__



#ifndef __WIN32__

static
BOOLEAN_T qsearch4pid(ErlNifEnv*         env,
                      ESockRequestQueue* q,
                      ErlNifPid*         pid)
{
    ESockRequestQueueElement* tmp = q->first;
    
    while (tmp != NULL) {
        if (COMPARE_PIDS(&tmp->data.pid, pid) == 0)
            return TRUE;
        else
            tmp = tmp->nextP;
    }

    return FALSE;
}

static
void qpush(ESockRequestQueue*        q,
           ESockRequestQueueElement* e)
{
    if (q->first != NULL) {
        q->last->nextP = e;
        q->last        = e;
        e->nextP       = NULL;
    } else {
        q->first = e;
        q->last  = e;
        e->nextP = NULL;
    }
}

static
ESockRequestQueueElement* qpop(ESockRequestQueue* q)
{
    ESockRequestQueueElement* e = q->first;
    
    if (e != NULL) {
        /* Atleast one element in the queue */
        if (e == q->last) {
            /* Only one element in the queue */
            q->first = q->last = NULL;
        } else {
            /* More than one element in the queue */
            q->first = e->nextP;
        }
    }
    
    return e;
}
#endif // #ifndef __WIN32__



#ifndef __WIN32__
static
BOOLEAN_T qunqueue(ErlNifEnv*         env,
                   ESockDescriptor*   descP,
                   const char*        slogan,
                   ESockRequestQueue* q,
                   ERL_NIF_TERM*      refP,
                   const ErlNifPid*   pidP)
{
    ESockRequestQueueElement* e = q->first;
    ESockRequestQueueElement* p = NULL;

    /* Check if it was one of the waiting acceptor processes */
    while (e != NULL) {
        if (COMPARE_PIDS(&e->data.pid, pidP) == 0) {
            if ((refP != NULL) && (COMPARE(e->data.ref, *refP) != 0))
                return FALSE;

            /* We have a match */

            (void) DEMONP(slogan, env, descP, &e->data.mon);
            
            if (p != NULL) {
                /* Not the first, but could be the last */
                if (q->last == e) {
                    q->last  = p;
                    p->nextP = NULL;                    
                } else {
                    p->nextP = e->nextP;
                }
                    
            } else {
                /* The first and could also be the last */
                if (q->last == e) {
                    q->last  = NULL;
                    q->first = NULL;
                } else {
                    q->first = e->nextP;
                }
            }

            esock_free_env("qunqueue", e->data.env);
            FREE(e);

            return TRUE;
        }

        /* Try next */
        p = e;
        e = e->nextP;
    }

    return FALSE;
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 *  C o u n t e r   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

#ifndef __WIN32__

static
BOOLEAN_T cnt_inc(ESockCounter* cnt, ESockCounter inc)
{
    BOOLEAN_T    wrap;
    ESockCounter max     = ESOCK_COUNTER_MAX;
    ESockCounter current = *cnt;

    if ((max - inc) >= current) {
      *cnt += inc;
      wrap  = FALSE;
    } else {
      *cnt = inc - (max - current) - 1;
      wrap = TRUE;
    }

    return (wrap);
}

static
void cnt_dec(ESockCounter* cnt, ESockCounter dec)
{
    ESockCounter current = *cnt;

    if (dec > current)
        *cnt = 0; // The counter cannot be < 0 so this is the best we can do...
    else
        *cnt -= dec;

    return;
}

#endif // #ifndef __WIN32__




/* ----------------------------------------------------------------------
 *  M o n i t o r   W r a p p e r   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

#ifndef __WIN32__

static
int esock_monitor(const char*      slogan,
                  ErlNifEnv*       env,
                  ESockDescriptor* descP,
                  const ErlNifPid* pid,
                  ESockMonitor*    monP)
{
    int res;

    SSDBG( descP, ("SOCKET",
                   "esock_monitor {%d} [%T] %s: try monitor\r\n",
                   descP->sock, esock_self(env), slogan) );

    res = enif_monitor_process(env, descP, pid, &monP->mon);

    if (res != 0) {
        monP->isActive = FALSE;

        SSDBG( descP,
               ("SOCKET",
                "esock_monitor {%d} [%T] %s: monitor failed: %d\r\n",
                descP->sock, esock_self(env), slogan, res) );
    } else {
        monP->isActive = TRUE;

        SSDBG( descP,
               ("SOCKET",
                "esock_monitor {%d} [%T] %s: monitor ok: %T\r\n",
                descP->sock, esock_self(env), slogan,
                esock_make_monitor_term(env, monP)) );
    }

    return res;
}

static
int esock_demonitor(const char*      slogan,
                    ErlNifEnv*       env,
                    ESockDescriptor* descP,
                    ESockMonitor*    monP)
{
    int res;

    if (! monP->isActive)
        return 1;

    SSDBG( descP, ("SOCKET",
                   "esock_demonitor {%d} [%T] %s: try demonitor %T\r\n",
                   descP->sock, esock_self(env), slogan,
                   esock_make_monitor_term(env, monP)) );

    res = enif_demonitor_process(env, descP, &monP->mon);
    esock_monitor_init(monP);

    if (res != 0) {
        SSDBG( descP,
               ("SOCKET",
                "esock_demonitor {%d}[%T] %s: demonitor failed: %d\r\n",
                descP->sock, esock_self(env), slogan, res) );
    }

    return res;
}

static
void esock_monitor_init(ESockMonitor* monP)
{
    monP->isActive = FALSE;
}

static
ERL_NIF_TERM esock_make_monitor_term(ErlNifEnv* env, const ESockMonitor* monP)
{
    if (monP->isActive)
        return enif_make_monitor_term(env, &monP->mon);
    else
        return esock_atom_undefined;
}

static BOOLEAN_T esock_monitor_eq(const ESockMonitor* monP,
                                  const ErlNifMonitor* mon) {
    if (monP->isActive)
        return enif_compare_monitors(&monP->mon, mon) == 0;
    else
        return FALSE;
}

#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 *  C a l l b a c k   F u n c t i o n s
 * ----------------------------------------------------------------------
 */


#ifndef __WIN32__
static void free_request_queue(ESockRequestQueue* q)
{
    while (q->first) {
        ESockRequestQueueElement* free_me = q->first;
        q->first = free_me->nextP;
        esock_free_env("dtor", free_me->data.env);
        FREE(free_me);
    }
}
#endif // #ifndef __WIN32__

/* =========================================================================
 * esock_dtor - Callback function for resource destructor
 *
 */
static
void esock_dtor(ErlNifEnv* env, void* obj)
{
#ifndef __WIN32__    
  ESockDescriptor* descP = (ESockDescriptor*) obj;

  MLOCK(descP->readMtx);
  MLOCK(descP->writeMtx);

  SGDBG( ("SOCKET", "dtor {%d,0x%X}\r\n",
          descP->sock, descP->readState | descP->writeState) );

  if (IS_SELECTED(descP)) {
      /* We have used the socket in the select machinery,
       * so we must have closed it properly to get here
       */
      ESOCK_ASSERT( IS_CLOSED(descP->readState) );
      ESOCK_ASSERT( IS_CLOSED(descP->writeState) );
      ESOCK_ASSERT( descP->sock == INVALID_SOCKET );
  } else {
      /* The socket is only opened, should be safe to close nonblocking */
      (void) sock_close(descP->sock);
      descP->sock = INVALID_SOCKET;
  }

  SGDBG( ("SOCKET", "dtor -> set state and pattern\r\n") );
  descP->readState  |= (ESOCK_STATE_DTOR | ESOCK_STATE_CLOSED);
  descP->writeState |= (ESOCK_STATE_DTOR | ESOCK_STATE_CLOSED);
  descP->pattern     = (ESOCK_DESC_PATTERN_DTOR | ESOCK_STATE_CLOSED);

  esock_free_env("dtor reader", descP->currentReader.env);
  descP->currentReader.env = NULL;

  esock_free_env("dtor writer", descP->currentWriter.env);
  descP->currentWriter.env = NULL;

  esock_free_env("dtor acceptor", descP->currentAcceptor.env);
  descP->currentAcceptor.env = NULL;

  SGDBG( ("SOCKET", "dtor -> try free readers request queue\r\n") );
  free_request_queue(&descP->readersQ);

  SGDBG( ("SOCKET", "dtor -> try free writers request queue\r\n") );
  free_request_queue(&descP->writersQ);

  SGDBG( ("SOCKET", "dtor -> try free acceptors request queue\r\n") );
  free_request_queue(&descP->acceptorsQ);

#ifdef HAVE_SENDFILE
  ESOCK_ASSERT( descP->sendfileHandle == INVALID_HANDLE );
  if (descP->sendfileCountersP != NULL) {
      FREE(descP->sendfileCountersP);
      descP->sendfileCountersP = NULL;
  }
#endif

  esock_free_env("dtor close env", descP->closeEnv);
  descP->closeEnv = NULL;

  esock_free_env("dtor meta env", descP->meta.env);
  descP->meta.env = NULL;

  MUNLOCK(descP->writeMtx);
  MUNLOCK(descP->readMtx);

  SGDBG( ("SOCKET", "dtor -> try destroy read mutex\r\n") );
  MDESTROY(descP->readMtx);  descP->readMtx  = NULL;

  SGDBG( ("SOCKET", "dtor -> try destroy write mutex\r\n") );
  MDESTROY(descP->writeMtx); descP->writeMtx = NULL;

  SGDBG( ("SOCKET", "dtor -> done\r\n") );
#endif // #ifndef __WIN32__
}


/* =========================================================================
 * esock_stop - Callback function for resource stop
 *
 * When the socket is stopped, we need to inform:
 *
 *     * the controlling process
 *     * the current writer and any waiting writers
 *     * the current reader and any waiting readers
 *     * the current acceptor and any waiting acceptor
 *
 * Also, make sure no process gets the message twice
 * (in case it is, for instance, both controlling process
 * and a writer).
 *
 */
static
void esock_stop(ErlNifEnv* env, void* obj, ErlNifEvent fd, int is_direct_call)
{
#ifndef __WIN32__
    ESockDescriptor* descP = (ESockDescriptor*) obj;

    if (is_direct_call) {
        return; // Nothing to do, caller gets ERL_NIF_SELECT_STOP_CALLED
    }

    // This is a scheduled call, caller gets ERL_NIF_SELECT_STOP_SCHEDULED
    MLOCK(descP->readMtx);
    MLOCK(descP->writeMtx);
    
    SSDBG( descP, ("SOCKET", "esock_stop {%d/%d} -> when %s"
                   "\r\n   ctrlPid:      %T"
                   "\r\n   closerPid:    %T"
                   "\r\ncounters:"
                   "\r\n   writePkgCnt:      %lu"
                   "\r\n   writePkgMax:      %lu"
                   "\r\n   writeByteCnt:     %lu"
                   "\r\n   writeTries:       %lu"
                   "\r\n   writeWaits:       %lu"
                   "\r\n   writeFails:       %lu"
                   "\r\n   readPkgCnt:       %lu"
                   "\r\n   readPkgMax:       %lu"
                   "\r\n   readByteCnt:      %lu"
                   "\r\n   readTries:        %lu"
                   "\r\n   readWaits:        %lu"
                   "\r\n   accSuccess:       %lu"
                   "\r\n   accTries:         %lu"
                   "\r\n   accWaits:         %lu"
                   "\r\n   accFails:         %lu"
                   "\r\n",
                   descP->sock, fd,
                   (is_direct_call) ? "called" : "scheduled",
                   descP->ctrlPid,
                   descP->closerPid,
                   //
                   (unsigned long) descP->writePkgCnt,
                   (unsigned long) descP->writePkgMax,
                   (unsigned long) descP->writeByteCnt,
                   (unsigned long) descP->writeTries,
                   (unsigned long) descP->writeWaits,
                   (unsigned long) descP->writeFails,
                   (unsigned long) descP->readPkgCnt,
                   (unsigned long) descP->readPkgMax,
                   (unsigned long) descP->readByteCnt,
                   (unsigned long) descP->readTries,
                   (unsigned long) descP->readWaits,
		   //
		   (unsigned long) descP->accSuccess,
                   (unsigned long) descP->accTries,
                   (unsigned long) descP->accWaits,
		   (unsigned long) descP->accFails) );

#ifdef HAVE_SENDFILE
    if (descP->sendfileCountersP != NULL) {
        ESockSendfileCounters *cP = descP->sendfileCountersP;

        SSDBG( descP, ("SOCKET", "esock_stop {%d/%d} ->"
                       "\r\nsendfileCounters:"
                       "\r\n   cnt:      %lu"
                       "\r\n   byteCnt:  %lu"
                       "\r\n   fails:    %lu"
                       "\r\n   max:      %lu"
                       "\r\n   pkg:      %lu"
                       "\r\n   pkgMax    %lu"
                       "\r\n   tries:    %lu"
                       "\r\n   waits:    %lu"
                       "\r\n",
                       descP->sock, fd,
                       (unsigned long) cP->cnt,
                       (unsigned long) cP->byteCnt,
                       (unsigned long) cP->fails,
                       (unsigned long) cP->max,
                       (unsigned long) cP->pkg,
                       (unsigned long) cP->pkgMax,
                       (unsigned long) cP->tries,
                       (unsigned long) cP->waits) );
    }
#endif

    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *
     *           Inform waiting Closer, or close socket
     *
     * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */

    if (! enif_is_pid_undefined(&descP->closerPid)) {
        /* We have a waiting closer process after nif_close()
         * - send message to trigger nif_finalize_close()
         */

        SSDBG( descP,
               ("SOCKET",
                "esock_stop {%d/%d} -> send close msg to %T\r\n",
                descP->sock, fd, MKPID(env, &descP->closerPid)) );

        esock_send_close_msg(env, descP, &descP->closerPid);
        /* Message send frees closeEnv */
        descP->closeEnv = NULL;
        descP->closeRef = esock_atom_undefined;
    } else {
        int err;

        /* We do not have a closer process
         * - have to do an unclean (non blocking) close */

#ifdef HAVE_SENDFILE
        if (descP->sendfileHandle != INVALID_HANDLE)
            esock_send_sendfile_deferred_close_msg(env, descP);
#endif

        err = esock_close_socket(env, descP, FALSE);

        if (err != 0)
            esock_warning_msg("Failed closing socket without "
                              "closer process: "
                              "\r\n   Controlling Process: %T"
                              "\r\n   Descriptor:          %d"
                              "\r\n   Errno:               %d (%T)"
                              "\r\n",
                              descP->ctrlPid, descP->sock,
                              err, MKA(env, erl_errno_id(err)));
    }

    SSDBG( descP,
           ("SOCKET",
            "esock_stop {%d/%d} -> done\r\n",
            descP->sock, fd) );

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);
#endif // #ifndef __WIN32__
}



/* *** esock_stop_handle_current ***
 *
 * Handle current requestor (reader, writer or acceptor) during
 * socket stop.
 */
#ifndef __WIN32__
static
void esock_stop_handle_current(ErlNifEnv*       env,
                               const char*      role,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ESockRequestor*  reqP)
{
    (void) DEMONP("esock_stop_handle_current", env, descP, &reqP->mon);

    SSDBG( descP, ("SOCKET",
                   "esock_stop_handle_current {%d} ->"
                   " send abort message to current %s %T %T\r\n",
                   descP->sock, role, reqP->pid, reqP->ref) );

    esock_send_abort_msg(env, descP, sockRef, reqP, atom_closed);

    enif_set_pid_undefined(&reqP->pid);
    reqP->ref = esock_atom_undefined;
}
#endif // #ifndef __WIN32__



/* This function traverse the queue and sends the specified
 * nif_abort message with the specified reason to each member,
 * and empty the queue.
 */
#ifndef __WIN32__
static
void inform_waiting_procs(ErlNifEnv*         env,
                          const char*        role,
                          ESockDescriptor*   descP,
                          ERL_NIF_TERM       sockRef,
                          ESockRequestQueue* q,
                          ERL_NIF_TERM       reason)
{
    ESockRequestQueueElement* currentP = q->first;
    ESockRequestQueueElement* nextP;

    SSDBG( descP,
           ("SOCKET",
            "inform_waiting_procs -> handle waiting %s(s)\r\n", role) );

    while (currentP != NULL) {

        /* <KOLLA>
         *
         * Should we inform anyone if we fail to demonitor?
         * NOT SURE WHAT THAT WOULD REPRESENT AND IT IS NOT
         * IMPORTANT IN *THIS* CASE, BUT IT'S A FUNDAMENTAL OP...
         *
         * </KOLLA>
         */

        SSDBG( descP,
               ("SOCKET",
                "inform_waiting_procs(%T) {%d} -> "
                "send abort message to waiting %s %T\r\n",
                sockRef, descP->sock,
                role, currentP->data.pid) );

        esock_send_abort_msg(env, descP, sockRef, &currentP->data, reason);

        (void) DEMONP("inform_waiting_procs -> current 'request'",
                      env, descP, &currentP->data.mon);

        nextP = currentP->nextP;
        FREE(currentP);
        currentP = nextP;
    }

    q->first = NULL;
    q->last  = NULL;
}
#endif // #ifndef __WIN32__


/* =========================================================================
 * esock_down - Callback function for resource down (monitored processes)
 *
 */
static
void esock_down(ErlNifEnv*           env,
                void*                obj,
                const ErlNifPid*     pidP,
                const ErlNifMonitor* monP)
{
#ifndef __WIN32__
    ESockDescriptor* descP = (ESockDescriptor*) obj;

    MLOCK(descP->readMtx);
    MLOCK(descP->writeMtx);

    SSDBG( descP, ("SOCKET", "esock_down {%d} -> entry with"
                   "\r\n   pid:   %T"
                   "\r\n   Close: %s (%s)"
                   "\r\n",
                   descP->sock, MKPID(env, pidP),
                   B2S(IS_CLOSED(descP->readState)),
                   B2S(IS_CLOSING(descP->readState))) );

    if (COMPARE_PIDS(&descP->closerPid, pidP) == 0) {

        /* The closer process went down
         * - it will not call nif_finalize_close
         */

        enif_set_pid_undefined(&descP->closerPid);

        if (MON_EQ(&descP->closerMon, monP)) {
            MON_INIT(&descP->closerMon);

            SSDBG( descP,
                   ("SOCKET",
                    "esock_down {%d} -> closer process exit\r\n",
                    descP->sock) );

        } else {
            // The owner is the closer so we used its monitor

            ESOCK_ASSERT( MON_EQ(&descP->ctrlMon, monP) );
            MON_INIT(&descP->ctrlMon);
            enif_set_pid_undefined(&descP->ctrlPid);

            SSDBG( descP,
                   ("SOCKET",
                    "esock_down {%d} -> closer controlling process exit\r\n",
                    descP->sock) );
        }

        /* Since the closer went down there was one,
         * hence esock_close() must have run or scheduled esock_stop(),
         * or the socket has never been selected upon
         */

        if (descP->closeEnv == NULL) {
            int err;

            /* Since there is no closeEnv,
             * esock_close() did not schedule esock_stop()
             * and is about to call esock_finalize_close() but died,
             * or esock_stop() has run, sent close_msg to the closer
             * and cleared ->closeEnv but the closer died
             * - we have to do an unclean (non blocking) socket close here
             */

#ifdef HAVE_SENDFILE
            if (descP->sendfileHandle != INVALID_HANDLE)
                esock_send_sendfile_deferred_close_msg(env, descP);
#endif

            err = esock_close_socket(env, descP, FALSE);
            if (err != 0)
                esock_warning_msg("Failed closing socket for terminating "
                                  "closer process: "
                                  "\r\n   Closer Process: %T"
                                  "\r\n   Descriptor:     %d"
                                  "\r\n   Errno:          %d (%T)"
                                  "\r\n",
                                  MKPID(env, pidP), descP->sock,
                                  err, MKA(env, erl_errno_id(err)));
        } else {
            /* Since there is a closeEnv esock_stop() has not run yet
             * - when it finds that there is no closer process
             *   it will close the socket and ignore the close_msg
             */
            esock_free_env("esock_down - close-env", descP->closeEnv);
            descP->closeEnv = NULL;
            descP->closeRef = esock_atom_undefined;
        }

    } else if (MON_EQ(&descP->ctrlMon, monP)) {
        MON_INIT(&descP->ctrlMon);
        /* The owner went down */
        enif_set_pid_undefined(&descP->ctrlPid);

        if (IS_OPEN(descP->readState)) {
            SSDBG( descP,
                   ("SOCKET",
                    "esock_down {%d} -> controller process exit"
                    "\r\n   initiate close\r\n",
                    descP->sock) );

            esock_down_ctrl(env, descP, pidP);

            descP->readState  |= ESOCK_STATE_CLOSING;
            descP->writeState |= ESOCK_STATE_CLOSING;
        } else {
            SSDBG( descP,
                   ("SOCKET",
                    "esock_down {%d} -> controller process exit"
                    "\r\n   already closed or closing\r\n",
                    descP->sock) );
        }

    } else if (descP->connectorP != NULL &&
               MON_EQ(&descP->connector.mon, monP)) {
        MON_INIT(&descP->connector.mon);

        SSDBG( descP,
               ("SOCKET",
                "esock_down {%d} -> connector process exit\r\n",
                descP->sock) );

        /* connectorP is only set during connection.
         * Forget all about the ongoing connection.
         * We might end up connected, but the process that initiated
         * the connection has died and will never know
         */

        requestor_release("esock_down->connector",
                          env, descP, &descP->connector);
        descP->connectorP = NULL;
        descP->writeState &= ~ESOCK_STATE_CONNECTING;

    } else {
        ERL_NIF_TERM     sockRef;

        /* check all operation queue(s): acceptor, writer and reader.
         *
         * Is it really any point in doing this if the socket is closed?
         *
         */

        sockRef = enif_make_resource(env, descP);

        if (IS_CLOSED(descP->readState)) {
            SSDBG( descP,
                   ("SOCKET",
                    "esock_down(%T) {%d} -> stray down: %T\r\n",
                    sockRef, descP->sock, pidP) );
        } else {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_down(%T) {%d} -> other process term\r\n",
                    sockRef, descP->sock) );

            if (descP->currentReaderP != NULL)
                esock_down_reader(env, descP, sockRef, pidP, monP);
            if (descP->currentAcceptorP != NULL)
                esock_down_acceptor(env, descP, sockRef, pidP, monP);
            if (descP->currentWriterP != NULL)
                esock_down_writer(env, descP, sockRef, pidP, monP);
        }
    }

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    SSDBG( descP, ("SOCKET", "esock_down -> done\r\n") );

#endif // #ifndef __WIN32__
}



/* *** esock_down_ctrl ***
 *
 * Stop after a downed controller
 *
 */
#ifndef __WIN32__
static
void esock_down_ctrl(ErlNifEnv*           env,
                     ESockDescriptor*     descP,
                     const ErlNifPid*     pidP)
{
    SSDBG( descP,
           ("SOCKET", "esock_down_ctrl {%d} ->"
            "\r\n   Pid: %T"
            "\r\n", descP->sock, MKPID(env, pidP)) );

    if (esock_do_stop(env, descP)) {
        /* esock_stop() is scheduled
         * - it has to close the socket
         */
        SSDBG( descP,
               ("SOCKET", "esock_down_ctrl {%d} -> stop was scheduled\r\n",
                descP->sock) );
    } else {
        int err;

        /* Socket is not in the select machinery
         * so esock_stop() will not be called
         * - we have to do an unclean (non blocking) socket close here
         */

#ifdef HAVE_SENDFILE
        if (descP->sendfileHandle != INVALID_HANDLE)
            esock_send_sendfile_deferred_close_msg(env, descP);
#endif

        err = esock_close_socket(env, descP, FALSE);
        if (err != 0)
            esock_warning_msg("Failed closing socket for terminating "
                              "owner process: "
                              "\r\n   Owner Process:  %T"
                              "\r\n   Descriptor:     %d"
                              "\r\n   Errno:          %d (%T)"
                              "\r\n",
                              MKPID(env, pidP), descP->sock,
                              err, MKA(env, erl_errno_id(err)));
    }
}
#endif // #ifndef __WIN32__



/* *** esock_down_acceptor ***
 *
 * Check and then handle a downed acceptor process.
 *
 */
#ifndef __WIN32__
static
void esock_down_acceptor(ErlNifEnv*           env,
                         ESockDescriptor*     descP,
                         ERL_NIF_TERM         sockRef,
                         const ErlNifPid*     pidP,
                         const ErlNifMonitor* monP)
{
    if (MON_EQ(&descP->currentAcceptor.mon, monP)) {
        MON_INIT(&descP->currentAcceptor.mon);
        
        SSDBG( descP,
               ("SOCKET",
                "esock_down_acceptor(%T) {%d} -> "
                "current acceptor - try activate next\r\n",
                sockRef, descP->sock) );
        
        if (!activate_next_acceptor(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_down_acceptor(%T) {%d} -> no more writers\r\n",
                    sockRef, descP->sock) );

            descP->readState &= ~ESOCK_STATE_ACCEPTING;

            descP->currentAcceptorP = NULL;
        }

    } else {
        
        /* Maybe unqueue one of the waiting acceptors */
        
        SSDBG( descP,
               ("SOCKET",
                "esock_down_acceptor(%T) {%d} -> "
                "not current acceptor - maybe a waiting acceptor\r\n",
                sockRef, descP->sock) );
        
        acceptor_unqueue(env, descP, NULL, pidP);
    }
}
#endif // #ifndef __WIN32__


/* *** esock_down_writer ***
 *
 * Check and then handle a downed writer process.
 *
 */
#ifndef __WIN32__
static
void esock_down_writer(ErlNifEnv*           env,
                       ESockDescriptor*     descP,
                       ERL_NIF_TERM         sockRef,
                       const ErlNifPid*     pidP,
                       const ErlNifMonitor* monP)
{
    if (MON_EQ(&descP->currentWriter.mon, monP)) {
        MON_INIT(&descP->currentWriter.mon);
        
        SSDBG( descP,
               ("SOCKET",
                "esock_down_writer(%T) {%d} -> "
                "current writer - try activate next\r\n",
                sockRef, descP->sock) );
        
        if (!activate_next_writer(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_down_writer(%T) {%d} -> no active writer\r\n",
                    sockRef, descP->sock) );

            descP->currentWriterP = NULL;
        }
        
    } else {
        
        /* Maybe unqueue one of the waiting writer(s) */
        
        SSDBG( descP,
               ("SOCKET",
                "esock_down_writer(%T) {%d} -> "
                "not current writer - maybe a waiting writer\r\n",
                sockRef, descP->sock) );
        
        writer_unqueue(env, descP, NULL, pidP);
    }
}
#endif // #ifndef __WIN32__




/* *** esock_down_reader ***
 *
 * Check and then handle a downed reader process.
 *
 */
#ifndef __WIN32__
static
void esock_down_reader(ErlNifEnv*           env,
                       ESockDescriptor*     descP,
                       ERL_NIF_TERM         sockRef,
                       const ErlNifPid*     pidP,
                       const ErlNifMonitor* monP)
{
    if (MON_EQ(&descP->currentReader.mon, monP)) {
        MON_INIT(&descP->currentReader.mon);
        
        SSDBG( descP,
               ("SOCKET",
                "esock_down_reader(%T) {%d} -> "
                "current reader - try activate next\r\n",
                sockRef, descP->sock) );
        
        if (! activate_next_reader(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "esock_down_reader(%T) {%d} -> no more readers\r\n",
                    sockRef, descP->sock) );

            descP->currentReaderP = NULL;
        }

    } else {
        
        /* Maybe unqueue one of the waiting reader(s) */
        
        SSDBG( descP,
               ("SOCKET",
                "esock_down_reader(%T) {%d} -> "
                "not current reader - maybe a waiting reader\r\n",
                sockRef, descP->sock) );
        
        reader_unqueue(env, descP, NULL, pidP);
    }
}
#endif // #ifndef __WIN32__



/* ----------------------------------------------------------------------
 *  L o a d / u n l o a d / u p g r a d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

static
ErlNifFunc esock_funcs[] =
{
    // Some utility and support functions
    {"nif_info",                0, nif_info, 0},
    {"nif_info",                1, nif_info, 0},
    {"nif_supports",            0, nif_supports, 0},
    {"nif_supports",            1, nif_supports, 0},
    {"nif_command",             1, nif_command, 0},

    // The proper "socket" interface
    {"nif_open",                2, nif_open, 0},
    {"nif_open",                4, nif_open, 0},
    {"nif_bind",                2, nif_bind, 0},
    {"nif_connect",             1, nif_connect, 0},
    {"nif_connect",             3, nif_connect, 0},
    {"nif_listen",              2, nif_listen, 0},
    {"nif_accept",              2, nif_accept, 0},
    {"nif_send",                4, nif_send, 0},
    {"nif_sendto",              5, nif_sendto, 0},
    {"nif_sendmsg",             5, nif_sendmsg, 0},
    {"nif_sendfile",            5, nif_sendfile, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_sendfile",            4, nif_sendfile, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_sendfile",            1, nif_sendfile, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_recv",                4, nif_recv, 0},
    {"nif_recvfrom",            4, nif_recvfrom, 0},
    {"nif_recvmsg",             5, nif_recvmsg, 0},
    {"nif_close",               1, nif_close, 0},
    {"nif_shutdown",            2, nif_shutdown, 0},
    {"nif_setopt",              5, nif_setopt, 0},
    {"nif_getopt",              3, nif_getopt, 0},
    {"nif_getopt",              4, nif_getopt, 0},
    {"nif_sockname",            1, nif_sockname, 0},
    {"nif_peername",            1, nif_peername, 0},

    /* Misc utility functions */

    /* "Extra" functions to "complete" the socket interface.
     * For instance, the function nif_finalize_close
     * is called after the close *select* has "completed".
     */
    {"nif_cancel",              3, nif_cancel, 0},
    {"nif_finalize_close",      1, nif_finalize_close, ERL_NIF_DIRTY_JOB_IO_BOUND}
};


#ifndef __WIN32__
static
char* extract_debug_filename(ErlNifEnv*   env,
			     ERL_NIF_TERM map)
{
    /* See the functions above */
    ERL_NIF_TERM val;
    ErlNifBinary bin;
    char *filename;
    
    if (! GET_MAP_VAL(env, map, atom_debug_filename, &val))
        return NULL;

    if (! enif_inspect_binary(env, val, &bin))
        return NULL;

    ESOCK_ASSERT( (filename = MALLOC(bin.size + 1)) != NULL );

    sys_memcpy(filename, bin.data, bin.size);
    filename[bin.size] = '\0';
    return filename;
}
#endif // #ifndef __WIN32__



/* =======================================================================
 * load_info - A map of misc info (e.g global debug)
 */

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    /* +++ Local atoms and error reason atoms +++ */
#define LOCAL_ATOM_DECL(A) atom_##A = MKA(env, #A)
    LOCAL_ATOMS;
    LOCAL_ERROR_REASON_ATOMS;
#undef LOCAL_ATOM_DECL

    /* Global atom(s) and error reason atom(s) */
#define GLOBAL_ATOM_DECL(A) esock_atom_##A = MKA(env, #A)
    GLOBAL_ATOMS;
    GLOBAL_ERROR_REASON_ATOMS;
#undef GLOBAL_ATOM_DECL

    esock_atom_socket_tag = MKA(env, "$socket");

#ifndef __WIN32__

    if (! esock_extract_pid_from_map(env, load_info,
                                     atom_registry,
                                     &data.regPid)) {
        enif_set_pid_undefined(&data.regPid);
        return 1; // Failure - no registry pid
    }

    data.useReg =
        esock_get_bool_from_map(env, load_info,
                                atom_use_registry,
                                ESOCK_CMD_USE_SOCKET_REGISTRY);

    data.iow =
        esock_get_bool_from_map(env, load_info,
                                atom_iow,
                                ESOCK_NIF_IOW_DEFAULT);

    {
        char *debug_filename;

        debug_filename = extract_debug_filename(env, load_info);
        
        if (esock_dbg_init(debug_filename)) {
            // Pick up early debug flags only if debug_filename is ok

            data.dbg =
                esock_get_bool_from_map(env, load_info,
                                        esock_atom_debug,
                                        ESOCK_GLOBAL_DEBUG_DEFAULT);
            data.sockDbg =
                esock_get_bool_from_map(env, load_info,
                                        atom_socket_debug,
                                        ESOCK_DEBUG_DEFAULT);
        }

        if (debug_filename != NULL)
            FREE(debug_filename);
    }

    data.protocolsMtx = MCREATE("esock.protocols");

    /* +++ Global Counters +++ */
    data.cntMtx         = MCREATE("esock.gcnt");
    data.numSockets     = 0;
    data.numTypeDGrams  = 0;
    data.numTypeStreams = 0;
    data.numTypeSeqPkgs = 0;
    data.numDomainLocal = 0;
    data.numDomainInet  = 0;
    data.numDomainInet6 = 0;
    data.numProtoIP     = 0;
    data.numProtoTCP    = 0;
    data.numProtoUDP    = 0;
    data.numProtoSCTP   = 0;

    initOpts();
    initCmsgTables();

    data.iov_max =
#if defined(NO_SYSCONF) || (! defined(_SC_IOV_MAX))
#   ifdef IOV_MAX
        IOV_MAX
#   else
        16
#   endif
#else
        sysconf(_SC_IOV_MAX)
#endif
        ;
    ESOCK_ASSERT( data.iov_max > 0 );

#endif // #ifndef __WIN32__

    esocks = enif_open_resource_type_x(env,
                                       "sockets",
                                       &esockInit,
                                       ERL_NIF_RT_CREATE,
                                       NULL);
    return esocks != NULL ?
        0: // Success
        1; // Failure
}

/*
 * MODULE:  socket (the erlang API/interface module)
 * funcs:   esock_funcs (defines the API of this nif)
 * load:    on_load (load this nif)
 * upgrade: NULL (not used)
 * NULL:    THIS IS NOT USED
 * unload:  NULL (not used)
 */
ERL_NIF_INIT(prim_socket, esock_funcs, on_load, NULL, NULL, NULL)
